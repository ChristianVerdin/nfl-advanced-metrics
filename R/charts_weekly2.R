# Week by week, continued (W5, W7, W9, W10): roles, division races, defenses by position, penalties.

# ---- W5: whose role is growing ----

chart_w5_usage_risers <- function(n_show = 12, recent = 3, min_rise = 0.15) {
  if (LASTW < 6) { message("W5: needs 6+ weeks"); return(invisible(NULL)) }
  grp <- c(RB = "RB", HB = "RB", FB = "RB", WR = "WR", TE = "TE")
  sn <- data_rds("snaps") |> filter(position %in% names(grp)) |> mutate(pos = grp[position]) |>
    left_join(w_pfr_gsis(), by = "pfr_player_id")
  gi <- data_rds("schedules") |>
    { \(s) bind_rows(s |> transmute(team = home_team, week), s |> transmute(team = away_team, week)) }() |>
    arrange(team, week) |> group_by(team) |> mutate(gi = row_number(), last = gi > max(gi) - recent) |> ungroup()
  played <- sn |> filter(offense_snaps > 0) |> inner_join(gi, by = c("team", "week"))
  # targets and carries per player-game, as shares of the team's
  ffo <- data_rds("ff_opportunity") |> filter(season == as.character(SEASON), week %in% WEEKS) |>
    group_by(game_id, posteam) |> mutate(tt = sum(rec_attempt, na.rm = TRUE), tc = sum(rush_attempt, na.rm = TRUE)) |> ungroup() |>
    transmute(gsis_id = player_id, team = posteam, week, tgt = rec_attempt / tt, car = rush_attempt / tc)
  played <- played |> left_join(ffo, by = c("gsis_id", "team", "week"))
  per <- played |> group_by(team, pfr_player_id, gsis_id, player, pos, last) |>
    summarise(g = n(), snap = mean(offense_pct), tgt = mean(tgt, na.rm = TRUE), car = mean(car, na.rm = TRUE), .groups = "drop")
  wide <- per |> pivot_wider(id_cols = c(team, pfr_player_id, gsis_id, player, pos), names_from = last,
                             values_from = c(g, snap, tgt, car)) |>
    rename(g0 = g_FALSE, g1 = g_TRUE, s0 = snap_FALSE, s1 = snap_TRUE, t0 = tgt_FALSE, t1 = tgt_TRUE, c0 = car_FALSE, c1 = car_TRUE) |>
    filter(coalesce(g0, 0L) >= 2, coalesce(g1, 0L) >= 2) |> mutate(rise = s1 - s0)
  # "filling in": a same-position teammate who out-snapped him before missed 2+ of the team's last games (out or gone)
  last_g <- gi |> filter(last) |> select(team, week)
  missed <- wide |> select(team, mate = pfr_player_id, pos, ms0 = s0) |>
    bind_rows(per |> filter(!last, g >= 2) |> anti_join(wide, by = c("team", "pfr_player_id")) |>
                transmute(team, mate = pfr_player_id, pos, ms0 = snap)) |>
    left_join(played |> filter(last) |> count(team, mate = pfr_player_id, name = "g_last"), by = c("team", "mate")) |>
    filter(coalesce(g_last, 0L) <= recent - 2)
  d <- wide |> filter(rise >= min_rise) |> slice_max(rise, n = n_show, with_ties = FALSE)
  d$filling <- vapply(seq_len(nrow(d)), function(i) {
    any(missed$team == d$team[i] & missed$pos == d$pos[i] & missed$ms0 > d$s0[i] & missed$mate != d$pfr_player_id[i])
  }, logical(1))
  d <- d |> arrange(rise) |> mutate(key = factor(pfr_player_id, pfr_player_id))
  d$img <- sprite_for(d$gsis_id, d$team)
  share <- function(x) ifelse(is.na(x), "-", pct(x))
  d$lab <- paste0(d$player, " (", d$team, " ", d$pos, ")   ",
                  ifelse(d$pos == "RB", paste0("carries ", share(d$c0), " to ", share(d$c1)),
                         paste0("targets ", share(d$t0), " to ", share(d$t1))))
  top <- d |> slice_max(rise, n = 1, with_ties = FALSE)
  k <- sum(d$filling)
  title <- paste0(top$player, " has played ", pct(top$s1), " of snaps over the last ", recent, " games, up from ", pct(top$s0),
                  "; ", k, " of these ", nrow(d), " risers are filling in for a teammate")
  FILL <- ORANGE; EARN <- GREEN
  W <- 0.042
  gg <- ggplot(d, aes(y = key)) +
    geom_segment(aes(x = s0, xend = s1, yend = key, color = filling), linewidth = 2, lineend = "round", alpha = 0.55) +
    geom_point(aes(x = s0), shape = 21, fill = BG, color = INK, size = 3, stroke = 0.9) +
    geom_point(aes(x = s1, color = filling), size = 3.6) +
    ggpath::geom_from_path(aes(x = -0.07, y = key, path = img), width = HEAD_W(W)) +
    geom_label(aes(x = s1 + 0.025, label = lab), hjust = 0, size = 3.1, family = "RobotoNFL", color = INK, fill = BG,
               linewidth = 0, label.padding = unit(0.12, "lines")) +
    scale_color_manual(values = c(`TRUE` = FILL, `FALSE` = EARN)) +
    scale_x_continuous(labels = function(x) pct(x), breaks = seq(0, 1, 0.2), limits = c(-0.11, 1.75), expand = expansion(0)) +
    coord_cartesian(clip = "off") +
    labs(title = title,
         subtitle = paste0("Share of offensive snaps: open dot = earlier games, dot = last ", recent, ". ",
                           kw("Filling in", FILL), " for a teammate who is out or gone, or ", kw("earned", EARN)),
         x = "Share of offensive snaps (games played)", y = NULL,
         caption = caption(paste0("RB, WR, TE with 2+ games in each stretch; rises of ", pct(min_rise), "+"),
                           credit = HEADS)) +
    theme_nfl_axis(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
  save_chart(gg, "W5_usage_risers", w = 12, h = 2.6 + 0.42 * nrow(d))
}

# ---- W7: division races, games over .500 after each week ----

chart_w7_division_races <- function() {
  tm <- data_rds("teams")
  s <- data_rds("schedules")
  res <- bind_rows(s |> transmute(team = home_team, week, d = sign(home_score - away_score)),
                   s |> transmute(team = away_team, week, d = sign(away_score - home_score)))
  d <- expand.grid(team = tm$team_abbr, week = 0:LASTW, stringsAsFactors = FALSE) |>
    left_join(res, by = c("team", "week")) |> mutate(d = coalesce(d, 0)) |>
    arrange(team, week) |> group_by(team) |> mutate(over = cumsum(d)) |> ungroup() |>
    left_join(tm |> select(team = team_abbr, div = team_division, col = team_color, col2 = team_color2), by = "team") |>
    mutate(col = on_bg(col, col2))
  wl <- res |> group_by(team) |> summarise(w = sum(d > 0), l = sum(d < 0), t = sum(d == 0), .groups = "drop") |>
    left_join(tm |> select(team = team_abbr, div = team_division), by = "team") |>
    group_by(div) |> mutate(gb = ((max(w - l) - (w - l))) / 2) |> ungroup()
  races <- wl |> group_by(div) |> arrange(gb, .by_group = TRUE) |>
    summarise(close = sum(gb <= 1), lead = sort(gb)[2], leader = team[1], .groups = "drop")
  tight <- races |> arrange(desc(close), lead) |> slice(1)
  wide <- races |> slice_max(lead, n = 1, with_ties = FALSE)
  title <- paste0("After Week ", LASTW, ": the ", tight$div, " has ", tight$close, " teams within a game of first; the ",
                  w_nick(wide$leader), " lead the ", wide$div, " by ", ifelse(wide$lead %% 1 == 0, wide$lead, number(wide$lead, 0.1)), " games")
  title <- sub(" by 1 games$", " by one game", title)
  ends <- d |> filter(week == LASTW) |> group_by(div, over) |> mutate(k = row_number() - 1) |> ungroup() |>
    mutate(x = LASTW + 0.8 + k * 1.25)
  gg <- ggplot(d, aes(week, over, group = team)) +
    geom_hline(yintercept = 0, color = MUTED, linewidth = 0.35) +
    geom_step(aes(color = I(col)), linewidth = 1.1, alpha = 0.9, direction = "hv") +
    geom_nfl_logos(data = ends, aes(x = x, y = over, team_abbr = team), width = 0.062) +
    facet_wrap(~div, nrow = 2) +
    scale_x_continuous(breaks = seq(0, LASTW, 3), expand = expansion(mult = c(0.02, 0.26))) +
    scale_y_continuous(breaks = pretty_breaks(6), labels = function(x) ifelse(x > 0, paste0("+", x), x)) +
    labs(title = title,
         subtitle = paste0("Wins minus losses after each week (0 = .500), by division. A flat stretch is a bye; a tie counts as zero"),
         x = "Week", y = "Games over .500",
         caption = caption("Within a game: games behind (difference in wins minus losses, halved) of 1 or less")) +
    theme_nfl()
  save_chart(gg, "W7_division_races", w = 12, h = 7.6)
}

# ---- W9: which positions each defense gives up the most to ----

chart_w9_defense_vs_position <- function(k = 40) {
  grp <- c(WR = "WR", TE = "TE", RB = "RB", FB = "RB")
  t <- data_rds("pbp") |> filter(pass_attempt == 1, sack == 0, !is.na(receiver_player_id), !is.na(epa)) |>
    left_join(data_rds("players") |> select(receiver_player_id = gsis_id, position), by = "receiver_player_id") |>
    filter(position %in% names(grp)) |> mutate(pos = factor(grp[position], c("WR", "TE", "RB")))
  lg <- t |> group_by(pos) |> summarise(lg = mean(epa), .groups = "drop")
  d <- t |> group_by(team = defteam, pos) |> summarise(n = n(), epa = sum(epa), .groups = "drop") |>
    group_by(team) |> mutate(mix = n / sum(n)) |> ungroup() |>
    left_join(lg, by = "pos") |> mutate(adj = (epa + k * lg) / (n + k), rel = adj - lg)
  ov <- t |> group_by(team = defteam) |> summarise(all = mean(epa), .groups = "drop") |> arrange(desc(all)) |>
    mutate(team = factor(team, rev(team)))
  d$team <- factor(d$team, levels(ov$team))
  worst <- d |> slice_max(rel, n = 1, with_ties = FALSE)
  word <- c(WR = "wide receivers", TE = "tight ends", RB = "running backs")
  title <- paste0("Passes to ", word[as.character(worst$pos)], " against the ", w_nick(as.character(worst$team)), " gain ",
                  signed(worst$adj), " EPA per target, the most of any defense against any position")
  lim <- max(abs(d$rel))
  gg <- ggplot(d, aes(pos, team)) +
    geom_tile(aes(fill = rel), color = BG, linewidth = 0.9) +
    geom_text(aes(label = signed(adj), color = abs(rel) > lim * 0.55), size = 3.1, family = "RobotoNFL", fontface = "bold",
              nudge_x = -0.12) +
    geom_text(aes(label = pct(mix), color = abs(rel) > lim * 0.55), size = 2.6, family = "RobotoNFL", hjust = 0, nudge_x = 0.06,
              alpha = 0.8) +
    scale_fill_gradient2(low = BLUE, mid = MID, high = WARM, midpoint = 0, limits = c(-lim, lim)) +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = INK)) +
    geom_nfl_logos(data = ov, aes(x = 0.22, y = team, team_abbr = as.character(team)), height = 0.024, inherit.aes = FALSE) +
    scale_x_discrete(position = "top", labels = c(WR = "To wide receivers", TE = "To tight ends", RB = "To running backs"),
                     expand = expansion(add = c(1.05, 0.5))) +
    labs(title = title,
         subtitle = paste0("EPA per target allowed (shrunk toward average): ", kw("more than average", WARM), ", ", kw("less", BLUE),
                           ". Small number: share of targets. Sorted by all targets"),
         x = NULL, y = NULL,
         caption = caption(paste0("Shrinkage: ", k, " league-average targets added to each cell; fullbacks count as running backs"))) +
    theme_nfl_axis(axis.text.y = element_blank(), panel.grid.major = element_blank(),
                   axis.text.x.top = element_text(face = "bold", color = INK, size = rel(1)))
  save_chart(gg, "W9_defense_vs_position", w = 12, h = 11)
}

# ---- W10: penalties, before the snap and after it ----

W_PRESNAP <- c("False Start", "Delay of Game", "Encroachment", "Neutral Zone Infraction", "Defensive Offside", "Illegal Formation",
               "Illegal Shift", "Illegal Motion", "Offensive Offside", "Illegal Substitution", "Defensive Too Many Men on Field",
               "Offensive Too Many Men on Field", "Defensive Delay of Game")

chart_w10_self_inflicted <- function() {
  p <- data_rds("pbp")
  games <- data_rds("schedules") |>
    { \(s) bind_rows(s |> transmute(team = home_team, game_id), s |> transmute(team = away_team, game_id)) }() |>
    count(team, name = "g")
  pen <- p |> filter(penalty == 1, !is.na(penalty_team), coalesce(penalty_yards, 0) > 0) |>
    mutate(when = ifelse(penalty_type %in% W_PRESNAP, "pre", "live"),
           gift = coalesce(first_down_penalty, 0) == 1 & penalty_team == defteam)
  d <- pen |> group_by(team = penalty_team, when) |> summarise(yds = sum(penalty_yards), .groups = "drop") |>
    left_join(games, by = "team") |> mutate(ypg = yds / g, when = factor(when, c("live", "pre")))
  tot <- pen |> group_by(team = penalty_team) |> summarise(yds = sum(penalty_yards), fd = sum(gift), n = n(), .groups = "drop") |>
    left_join(games, by = "team") |> mutate(ypg = yds / g, fdpg = fd / g, npg = n / g) |> arrange(ypg) |>
    mutate(team = factor(team, team))
  d$team <- factor(d$team, levels(tot$team))
  pre <- d |> filter(when == "pre") |> select(team, pre = ypg)
  top <- tot |> slice_max(ypg, n = 1, with_ties = FALSE) |> left_join(pre, by = "team")
  bot <- tot |> slice_min(ypg, n = 1, with_ties = FALSE)
  title <- paste0("The ", w_nick(as.character(top$team)), " give away ", number(top$ypg, 0.1), " penalty yards a game, ",
                  number(top$pre, 0.1), " of them before the snap; the ", w_nick(as.character(bot$team)), " ", number(bot$ypg, 0.1))
  xcol <- max(tot$ypg) * 1.2; xlogo <- -max(tot$ypg) * 0.03
  gg <- ggplot(d, aes(x = ypg, y = team, fill = when)) +
    geom_col(width = 0.72) +
    geom_text(data = tot, aes(x = ypg + 0.8, y = team, label = number(ypg, 0.1)), inherit.aes = FALSE, hjust = 0, size = 3.1,
              family = "RobotoNFL", fontface = "bold", color = INK) +
    geom_text(data = tot, aes(x = xcol, y = team, label = number(fdpg, 0.1)), inherit.aes = FALSE, hjust = 0.5, size = 3.1,
              family = "RobotoNFL", color = INK) +
    annotate("text", x = xcol, y = nrow(tot) + 1.1, label = "First downs\ngiven a game", size = 3, family = "RobotoNFL",
             fontface = "bold", color = INK, lineheight = 0.9, vjust = 0) +
    scale_fill_manual(values = c(pre = ORANGE, live = if (DARK) "#7b8794" else "#5b6670")) +
    geom_nfl_logos(data = tot, aes(x = xlogo, y = team, team_abbr = as.character(team)), height = 0.026, inherit.aes = FALSE) +
    scale_x_continuous(expand = expansion(mult = c(0.06, 0.06)), breaks = pretty_breaks(6)(c(0, max(tot$ypg)))) +
    scale_y_discrete(expand = expansion(add = c(0.6, 2.2))) +
    coord_cartesian(clip = "off") +
    labs(title = title,
         subtitle = paste0("Penalty yards per game, ", kw("before the snap", ORANGE), " or ",
                           kw("during the play", if (DARK) "#9aa5b1" else "#5b6670"), ". Right: first downs from defensive fouls"),
         x = "Penalty yards per game", y = NULL,
         caption = caption("Accepted penalties only. Before the snap: false starts, offside, delay of game and the like")) +
    theme_nfl_axis(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
  save_chart(gg, "W10_self_inflicted", w = 12, h = 10.5)
}
