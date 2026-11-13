# Week by week (W1-W8): rebuilt every week from the latest snapshot. The week is in each title; the file names stay the
# same from week to week. Helpers here start with w_ because this file is sourced after the others.

w_nick <- function(a) { tm <- data_rds("teams"); tm$team_nick[match(a, tm$team_abbr)] }
w_names <- function() { pf <- data_rds("players_full") |> filter(!is.na(gsis_id)); setNames(pf$display_name, pf$gsis_id) }

# one row per team-game: points for and against, offense EPA per play, the opponent's offense EPA per play, and the gap
w_team_games <- function(p = data_rds("pbp"), s = data_rds("schedules")) {
  off <- plays_epa(p) |> group_by(game_id, team = posteam) |> summarise(off = mean(epa), .groups = "drop")
  bind_rows(s |> transmute(game_id, week, team = home_team, opp = away_team, pf = home_score, pa = away_score),
            s |> transmute(game_id, week, team = away_team, opp = home_team, pf = away_score, pa = home_score)) |>
    left_join(off, by = c("game_id", "team")) |>
    left_join(off |> rename(opp = team, opp_off = off), by = c("game_id", "opp")) |>
    mutate(net = off - opp_off)
}

# share of decided games won by the side with the lower EPA per play, 2018-2025
w_outplayed_rate <- function() {
  h <- data_rds("pbp_2018_2025") |> filter(season_type == "REG") |> plays_epa() |>
    group_by(game_id, team = posteam) |> summarise(off = mean(epa), .groups = "drop")
  s <- data_rds("schedules_all") |> filter(game_type == "REG", season < SEASON, !is.na(result), result != 0) |>
    transmute(game_id, win = ifelse(result > 0, home_team, away_team), los = ifelse(result > 0, away_team, home_team))
  d <- s |> inner_join(h |> rename(win = team, w = off), by = c("game_id", "win")) |>
    inner_join(h |> rename(los = team, l = off), by = c("game_id", "los"))
  list(rate = mean(d$w < d$l), n = nrow(d))
}

chart_w1_week_in_one_picture <- function() {
  g <- w_team_games() |> filter(pf > pa) |> transmute(game_id, week, win = team, los = opp, pf, pa, margin = pf - pa, gap = net)
  wk <- g |> filter(week == LASTW); rest <- g |> filter(week < LASTW)
  out <- wk |> filter(gap < 0) |> arrange(gap)
  hist <- w_outplayed_rate()
  title <- if (nrow(out) == 0) {
    paste0("Week ", LASTW, ": all ", nrow(wk), " winners moved the ball better than the team they beat")
  } else if (nrow(out) == 1) {
    paste0("Week ", LASTW, ": the ", w_nick(out$win), " beat the ", w_nick(out$los), " while adding ", number(-out$gap, 0.01),
           " fewer points per play")
  } else {
    paste0("Week ", LASTW, ": ", nrow(out), " of ", nrow(wk), " winners moved the ball worse than the team they beat, the ",
           w_nick(out$win[1]), " by the most")
  }
  xr <- range(c(g$gap, 0)); yr <- c(0, max(g$margin)); ytop <- yr[2] * 1.04
  # each logo with its label underneath is pushed clear of the others; a thin line leads back to the true point
  wk <- cbind(wk, spread_xy(wk$gap, wk$margin, rep(0.085, nrow(wk)), xr, yr, aspect = 12 / 7.4, pad = 1.05))
  gg <- ggplot(g, aes(gap, margin)) +
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, fill = RED, alpha = 0.07) +
    geom_vline(xintercept = 0, color = INK, linewidth = 0.4) +
    geom_point(data = rest, color = GRAY, size = 2.3, alpha = 0.55) +
    geom_segment(data = wk, aes(xend = xs, yend = ys), color = GRAY, linewidth = 0.35) +
    geom_point(data = wk, color = INK, size = 1.3) +
    geom_nfl_logos(data = wk, aes(x = xs, y = ys, team_abbr = win), width = 0.038) +
    geom_text(data = wk, aes(x = xs, y = ys - diff(yr) * 0.06, label = paste0("beat ", los, " ", pf, "-", pa)), vjust = 1,
              size = 3, family = "RobotoNFL", color = INK) +
    annotate("text", x = xr[1], y = ytop, label = "Won while\nmoving the\nball worse", hjust = 0, vjust = 1, color = RED,
             family = "RobotoNFL", fontface = "bold", size = 3.6, lineheight = 0.95) +
    annotate("text", x = xr[2], y = yr[1] + diff(yr) * 0.02, label = "Moved the ball\nbetter and won", hjust = 1, vjust = 0,
             color = GREEN, family = "RobotoNFL", fontface = "bold", size = 3.6, lineheight = 0.95) +
    scale_x_continuous(labels = function(x) signed(x, 0.1), breaks = pretty_breaks(8), expand = expansion(mult = 0.05)) +
    scale_y_continuous(breaks = pretty_breaks(7), expand = expansion(mult = c(0.03, 0.02))) +
    labs(title = title,
         subtitle = paste0("Each winner's EPA per play (points added per play) minus the loser's, vs the final margin. ",
                           "<b>Week ", LASTW, "</b> logos, earlier weeks gray"),
         x = "Winner's EPA per play minus the loser's", y = "Winning margin (points)",
         caption = caption(paste0("Won while moving the ball worse: ", sum(g$gap < 0), " of ", nrow(g), " decided games in ", SEASON,
                                  " (", pct(mean(g$gap < 0), 0.1), "), ", pct(hist$rate, 0.1), " in 2018-25"))) +
    theme_nfl()
  save_chart(gg, "W1_week_in_one_picture", w = 12, h = 7.4)
}

# ---- W2: the plays that moved win probability most ----

w_play_text <- function(d, nm) {
  n <- function(id, ab) coalesce(unname(nm[id]), ab)
  yd <- function(y) paste0(y, if (all(abs(y) == 1)) " yard" else " yards")
  with(d, case_when(
    two_point_attempt == 1 ~ paste0("Two-point try ", ifelse(two_point_conv_result %in% "success", "is good", "fails")),
    field_goal_attempt == 1 ~ paste0(kick_distance, "-yard field goal ",
                                     c(made = "is good", missed = "is missed", blocked = "is blocked")[field_goal_result]),
    interception == 1 ~ paste0("Interception by ", n(interception_player_id, interception_player_name),
                               ifelse(return_touchdown %in% 1, ", returned for a touchdown", "")),
    fumble_lost == 1 & sack == 1 ~ paste0("Strip sack of ", n(passer_player_id, passer_player_name), ", recovered by ",
                                          n(fumble_recovery_1_player_id, fumble_recovery_1_player_name)),
    fumble_lost == 1 ~ paste0("Fumble by ", n(fumbled_1_player_id, fumbled_1_player_name), ", recovered by ",
                              n(fumble_recovery_1_player_id, fumble_recovery_1_player_name)),
    sack == 1 ~ paste0("Sack of ", n(passer_player_id, passer_player_name), " by ", n(sack_player_id, sack_player_name)),
    complete_pass == 1 ~ paste0(ifelse(touchdown == 1, "Touchdown catch by ", "Catch by "), n(receiver_player_id, receiver_player_name),
                                ", ", yd(yards_gained), " (from ", n(passer_player_id, passer_player_name), ")"),
    pass_attempt == 1 ~ paste0("Incomplete pass", ifelse(fourth_down_failed %in% 1, " on fourth down", ""), " from ",
                               n(passer_player_id, passer_player_name)),
    !is.na(rusher_player_id) ~ paste0(ifelse(touchdown == 1, "Touchdown run by ", "Run by "), n(rusher_player_id, rusher_player_name),
                                      ", ", yd(yards_gained), ifelse(fourth_down_failed %in% 1, ", stopped on fourth down", "")),
    TRUE ~ substr(sub("^\\([^)]*\\)\\s*", "", desc), 1, 70)
  ))
}

chart_w2_biggest_plays <- function(n = 10, per_game = 2) {
  nm <- w_names()
  p <- data_rds("pbp") |> filter(week == LASTW, !is.na(wpa), play_type != "no_play") |>
    mutate(swing = abs(wpa), bt = ifelse(wpa > 0, posteam, defteam), opp = ifelse(wpa > 0, defteam, posteam)) |>
    group_by(game_id) |> slice_max(swing, n = per_game, with_ties = FALSE) |> ungroup() |>
    slice_max(swing, n = n, with_ties = FALSE)
  p <- p |> mutate(
    hero = case_when(
      interception == 1 ~ interception_player_id,
      fumble_lost == 1 ~ coalesce(fumble_recovery_1_player_id, forced_fumble_player_1_player_id),
      field_goal_attempt == 1 & bt == posteam ~ kicker_player_id,
      field_goal_attempt == 1 ~ blocked_player_id,
      sack == 1 & bt == defteam ~ coalesce(sack_player_id, half_sack_1_player_id),
      bt == posteam & complete_pass == 1 ~ receiver_player_id,
      bt == posteam & two_point_attempt != 1 ~ coalesce(rusher_player_id, passer_player_id),
      TRUE ~ NA_character_),
    what = w_play_text(p, nm),
    late = qtr >= 5 | (qtr == 4 & quarter_seconds_remaining <= 120),
    bs = ifelse(bt == posteam, posteam_score, defteam_score), os = ifelse(bt == posteam, defteam_score, posteam_score),
    state = ifelse(bs > os, "up", ifelse(bs < os, "down", "tied")),
    ctx = paste0(ifelse(qtr >= 5, "OT", paste0("Q", qtr)), " ", sub("^0", "", time), ", ", bt, " ", state, " ", bs, "-", os,
                 " vs ", opp, " before the play")) |>
    arrange(desc(swing)) |> mutate(rank = row_number())
  p$img <- NA_character_
  has <- !is.na(p$hero)
  if (any(has)) p$img[has] <- sprite_for(p$hero[has], p$bt[has])
  tm <- data_rds("teams")
  p$col <- on_bg(tm$team_color[match(p$bt, tm$team_abbr)], tm$team_color2[match(p$bt, tm$team_abbr)])
  p$lab <- paste0("<b>", p$what, "</b><br><span style='color:", MUTED, "'>", p$ctx, "</span>")
  mx <- max(p$swing) * 100
  k <- sum(p$late)
  title <- paste0("Week ", LASTW, ": ", if (k == 0) "none" else k, " of the ", nrow(p),
                  " biggest win-probability swings came in the last two minutes or overtime")
  W <- 0.055
  gg <- ggplot(p, aes(y = rank)) +
    geom_col(aes(x = swing * 100, fill = I(col)), orientation = "y", width = 0.62, alpha = 0.9) +
    geom_text(aes(x = swing * 100 + mx * 0.015, label = paste0("+", round(swing * 100), "%")), hjust = 0, size = 3.4,
              family = "RobotoNFL", fontface = "bold", color = INK) +
    ggtext::geom_richtext(aes(x = -mx * 0.11, label = lab), hjust = 1, size = 3.3, family = "RobotoNFL", color = INK,
                          fill = NA, label.color = NA, lineheight = 1.15) +
    ggpath::geom_from_path(data = p |> filter(!is.na(img)), aes(x = -mx * 0.05, y = rank, path = img), width = HEAD_W(W)) +
    geom_nfl_logos(data = p |> filter(is.na(img)), aes(x = -mx * 0.05, y = rank, team_abbr = bt), width = W * 0.8) +
    scale_y_reverse(breaks = NULL, expand = expansion(add = 0.6)) +
    scale_x_continuous(limits = c(-mx * 1.15, mx * 1.12), breaks = pretty_breaks(5)(c(0, mx)), labels = function(x) paste0(x, "%"),
                       expand = expansion(0)) +
    coord_cartesian(clip = "off") +
    labs(title = title,
         subtitle = paste0("How much one play moved a team's chance of winning (nflfastR model), at most ", per_game,
                           " per game. Headshot: the player who made it"),
         x = NULL, y = NULL,
         caption = caption("Win probability: nflfastR model; a logo where no one player made the play", credit = HEADS)) +
    theme_nfl_axis(panel.grid.major.y = element_blank(), axis.text.x = element_text(color = AXIS_TEXT))
  save_chart(gg, "W2_biggest_plays", w = 12, h = 8.2)
}

# ---- W3: share of game time leading, tied, trailing ----

w_game_clock <- function(p = data_rds("pbp")) {
  p |> mutate(.r = row_number()) |>
    filter(!is.na(qtr), !is.na(quarter_seconds_remaining)) |>
    group_by(game_id) |> arrange(.r, .by_group = TRUE) |>
    mutate(t = ifelse(qtr <= 4, (3600 - game_seconds_remaining) / 60, 60 + (qtr - 5) * 10 + (600 - quarter_seconds_remaining) / 60),
           t = cummax(coalesce(t, 0)),
           hs = cummax(coalesce(total_home_score, 0)), as = cummax(coalesce(total_away_score, 0)),
           dt = coalesce(lead(t), t) - t) |>
    summarise(week = first(week), home = first(home_team), away = first(away_team),
              h_lead = sum(dt[hs > as]), tie = sum(dt[hs == as]), a_lead = sum(dt[hs < as]), .groups = "drop") |>
    mutate(total = h_lead + tie + a_lead)
}

chart_w3_game_control <- function(low = 0.1) {
  gc <- w_game_clock()
  s <- data_rds("schedules") |> select(game_id, home_score, away_score)
  d <- bind_rows(gc |> transmute(game_id, week, team = home, lead = h_lead, tie, trail = a_lead, total),
                 gc |> transmute(game_id, week, team = away, lead = a_lead, tie, trail = h_lead, total)) |>
    left_join(s, by = "game_id") |>
    left_join(gc |> select(game_id, home), by = "game_id") |>
    mutate(pf = ifelse(team == home, home_score, away_score), pa = ifelse(team == home, away_score, home_score),
           res = ifelse(pf > pa, "W", ifelse(pf < pa, "L", "T")), net = (lead - trail) / total)
  tot <- d |> group_by(team) |> summarise(lead = sum(lead) / sum(total), tie = sum(tie) / sum(total), trail = sum(trail) / sum(total),
                                         .groups = "drop") |> arrange(lead) |> mutate(team = factor(team, team))
  d$team <- factor(d$team, levels(tot$team))
  top <- tot |> slice_max(lead, n = 1); bot <- tot |> slice_min(lead, n = 1)
  steals <- d |> filter(res == "W", lead / total < low)
  bars <- tot |> pivot_longer(c(lead, tie, trail), names_to = "state") |>
    mutate(state = factor(state, c("trail", "tie", "lead")))
  pa <- ggplot(bars, aes(x = value, y = team, fill = state)) +
    geom_col(width = 0.72) +
    geom_text(data = tot, aes(x = 0.015, y = team, label = pct(lead)), inherit.aes = FALSE, hjust = 0, size = 3,
              family = "RobotoNFL", fontface = "bold", color = "white") +
    scale_fill_manual(values = c(lead = GREEN, tie = if (DARK) "#4a5058" else "#c9cdd2", trail = RED)) +
    geom_nfl_logos(data = tot, aes(x = -0.045, y = team, team_abbr = as.character(team)), height = 0.026, inherit.aes = FALSE) +
    scale_x_continuous(labels = function(x) pct(x), breaks = c(0, 0.5, 1), expand = expansion(mult = c(0.09, 0))) +
    labs(x = "Share of game time, season", y = NULL) +
    theme_nfl_axis(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
  pb <- ggplot(d, aes(x = week, y = team)) +
    geom_tile(aes(fill = net), color = BG, linewidth = 0.8, alpha = 0.9) +
    geom_text(aes(label = res), size = 2.9, family = "RobotoNFL", fontface = "bold",
              color = ifelse(abs(d$net) > 0.45, "white", INK)) +
    geom_tile(data = steals, fill = NA, color = INK, linewidth = 0.9) +
    scale_fill_gradient2(low = RED, mid = if (DARK) "#2a2f35" else "#ececec", high = GREEN, midpoint = 0, limits = c(-1, 1)) +
    scale_x_continuous(breaks = WEEKS, labels = paste0("Wk ", WEEKS), position = "top", expand = expansion(0)) +
    labs(x = NULL, y = NULL) +
    theme_nfl_axis(axis.text.y = element_blank(), panel.grid.major = element_blank())
  title <- paste0("The ", w_nick(as.character(top$team)), " have led for ", pct(top$lead), " of their game time this season; the ",
                  w_nick(as.character(bot$team)), " for ", pct(bot$lead))
  g <- patchwork::wrap_plots(pa, pb, widths = c(1, 1.35)) +
    patchwork::plot_annotation(
      title = wrap_text(title, 72),
      subtitle = paste0("Share of game time ", kw("leading", GREEN), ", tied or ", kw("trailing", RED),
                        ". Right: each game, by week (W/L). Boxed: won after leading under ", pct(low), " of the game"),
      caption = caption(paste0(nrow(steals), " such wins so far; game time runs from kickoff to the final play, overtime included")),
      theme = theme_nfl())
  save_chart(g, "W3_game_control", w = 12, h = 10.5)
}

# ---- W4: next week's games, unit against unit ----

w_unit_ranks <- function(p = data_rds("pbp")) {
  e <- plays_epa(p) |> mutate(unit = ifelse(pass == 1, "pass", "run"))
  list(
    off = e |> group_by(team = posteam, unit) |> summarise(v = mean(epa), .groups = "drop") |>
      group_by(unit) |> mutate(o_rank = rank(-v, ties.method = "first")) |> ungroup(),
    def = e |> group_by(team = defteam, unit) |> summarise(v = mean(epa), .groups = "drop") |>
      group_by(unit) |> mutate(d_rank = rank(v, ties.method = "first")) |> ungroup())
}

chart_w4_next_week_mismatches <- function() {
  nw <- LASTW + 1
  s <- data_rds("schedules_all") |> filter(season == SEASON, game_type == "REG", week == nw) |> arrange(gameday, gametime, game_id)
  if (!nrow(s)) { message("W4: no games after Week ", LASTW); return(invisible(NULL)) }
  r <- w_unit_ranks()
  s$row <- seq_len(nrow(s))
  xs <- c(a_pass = 1, a_run = 2, h_pass = 3.5, h_run = 4.5)
  m <- bind_rows(
    tibble(row = s$row, off = s$away_team, def = s$home_team, unit = "pass", col = "a_pass"),
    tibble(row = s$row, off = s$away_team, def = s$home_team, unit = "run", col = "a_run"),
    tibble(row = s$row, off = s$home_team, def = s$away_team, unit = "pass", col = "h_pass"),
    tibble(row = s$row, off = s$home_team, def = s$away_team, unit = "run", col = "h_run")) |>
    left_join(r$off |> select(off = team, unit, o_rank), by = c("off", "unit")) |>
    left_join(r$def |> select(def = team, unit, d_rank), by = c("def", "unit")) |>
    mutate(gap = d_rank - o_rank, x = xs[col], lab = paste0(ordinal(o_rank), " vs ", ordinal(d_rank)))
  best <- m |> group_by(row) |> slice_max(abs(gap), n = 1, with_ties = FALSE) |> ungroup()
  top <- m |> slice_max(gap, n = 1, with_ties = FALSE)
  unit_word <- c(pass = "passing", run = "running")
  title <- paste0("Week ", nw, "'s biggest mismatch: the ", w_nick(top$off), "' ", unit_word[top$unit], " offense (",
                  ordinal(top$o_rank), ") against the ", w_nick(top$def), "' ", top$unit, " defense (", ordinal(top$d_rank), ")")
  lg <- bind_rows(tibble(row = s$row, x = 0.2, team = s$away_team), tibble(row = s$row, x = 5.3, team = s$home_team))
  gg <- ggplot(m, aes(x, row)) +
    geom_tile(aes(fill = gap), width = 0.94, height = 0.86, alpha = 0.92) +
    geom_tile(data = best, fill = NA, color = INK, width = 0.94, height = 0.86, linewidth = 1.1) +
    geom_text(aes(label = lab, color = abs(gap) >= 18), size = 3.3, family = "RobotoNFL", fontface = "bold") +
    geom_nfl_logos(data = lg, aes(x = x, y = row, team_abbr = team), height = 0.62 / nrow(s), inherit.aes = FALSE) +
    annotate("text", x = 2.75, y = s$row, label = "at", size = 3.6, family = "RobotoNFL", color = MUTED) +
    scale_fill_gradient2(low = WARM, mid = MID, high = BLUE, midpoint = 0, limits = c(-31, 31)) +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = INK)) +
    scale_x_continuous(breaks = unname(xs), labels = c("Away pass\noffense", "Away run\noffense", "Home pass\noffense", "Home run\noffense"),
                       position = "top", limits = c(-0.2, 5.7), expand = expansion(0)) +
    scale_y_reverse(breaks = NULL, expand = expansion(add = 0.55)) +
    labs(title = title,
         subtitle = paste0("Offense rank vs the facing defense's rank (EPA per play, ", wk_span(1, LASTW), "): ",
                           kw("offense ahead", BLUE), ", ", kw("defense ahead", WARM), ". Box: biggest gap per game"),
         x = NULL, y = NULL,
         caption = caption(paste0("Ranks 1-32, 1st = best; pass plays include sacks and scrambles"))) +
    theme_nfl_axis(panel.grid.major = element_blank(), axis.text.x.top = element_text(face = "bold", color = INK, lineheight = 0.95))
  save_chart(gg, "W4_next_week_mismatches", w = 12, h = 2.4 + 0.58 * nrow(s))
}

# ---- W6: starters who did not play ----

w_pfr_gsis <- function() {
  data_rds("players_full") |> filter(!is.na(pfr_id), !is.na(gsis_id)) |> distinct(pfr_id, .keep_all = TRUE) |>
    select(pfr_player_id = pfr_id, gsis_id)
}

# Starters for each team-week: the top n on offense and the top n on defense by average snap share in the team's previous k
# games (games a player appeared in). Returns the starters who then did not play, with a reason.
w_starters_missing <- function(k = 3, n = 11) {
  sn <- data_rds("snaps") |> left_join(w_pfr_gsis(), by = "pfr_player_id")
  played <- sn |> filter(offense_snaps + defense_snaps + st_snaps > 0)
  gi <- data_rds("schedules") |>
    { \(s) bind_rows(s |> transmute(team = home_team, week), s |> transmute(team = away_team, week)) }() |>
    arrange(team, week) |> group_by(team) |> mutate(gi = row_number()) |> ungroup()
  win <- gi |> filter(gi >= 2) |> rename(t_week = week, t_gi = gi) |>
    inner_join(played |> inner_join(gi, by = c("team", "week")), by = "team", relationship = "many-to-many") |>
    filter(gi < t_gi, gi >= t_gi - k)
  st <- win |> group_by(team, t_week, pfr_player_id, gsis_id, player, position) |>
    summarise(o = mean(offense_pct), d = mean(defense_pct), .groups = "drop")
  starters <- bind_rows(st |> group_by(team, t_week) |> slice_max(o, n = n, with_ties = FALSE),
                        st |> group_by(team, t_week) |> slice_max(d, n = n, with_ties = FALSE)) |>
    ungroup() |> distinct(team, t_week, pfr_player_id, .keep_all = TRUE)
  miss <- starters |> anti_join(played |> select(team, t_week = week, pfr_player_id), by = c("team", "t_week", "pfr_player_id"))
  rw <- data_rds("rosters_weekly") |> select(t_week = week, gsis_id, rw_team = team, status) |> distinct(t_week, gsis_id, .keep_all = TRUE)
  inj <- data_rds("injuries") |> select(t_week = week, gsis_id, report_status) |> distinct(t_week, gsis_id, .keep_all = TRUE)
  miss |> left_join(rw, by = c("t_week", "gsis_id")) |> left_join(inj, by = c("t_week", "gsis_id")) |>
    mutate(reason = case_when(
      is.na(rw_team) | rw_team != team | status %in% c("CUT", "TRD", "TRT", "RET") ~ "gone",
      status == "RES" ~ "reserve",
      report_status %in% c("Out", "Doubtful") ~ "out",
      report_status == "Questionable" ~ "questionable",
      TRUE ~ "other")) |>
    filter(reason != "gone") |> rename(week = t_week)
}

chart_w6_starters_missing <- function() {
  m <- w_starters_missing()
  cols <- c(reserve = RED, out = ORANGE, questionable = if (DARK) "#e0b33a" else "#c99a06", other = GRAY)
  m$reason <- factor(m$reason, names(cols))
  tot <- m |> count(team, name = "n") |> right_join(tibble(team = data_rds("teams")$team_abbr), by = "team") |>
    mutate(n = coalesce(n, 0L)) |> arrange(n, team) |> mutate(team = factor(team, team))
  by_r <- m |> count(team, reason) |> mutate(team = factor(team, levels(tot$team)))
  wk <- m |> count(team, week) |>
    right_join(data_rds("schedules") |> { \(s) bind_rows(s |> transmute(team = home_team, week), s |> transmute(team = away_team, week)) }() |>
                 filter(week >= 2), by = c("team", "week")) |>
    mutate(n = coalesce(n, 0L), team = factor(team, levels(tot$team)))
  top <- tot |> slice_max(n, n = 1, with_ties = FALSE); bot <- tot |> slice_min(n, n = 1, with_ties = FALSE)
  byes <- expand.grid(team = levels(tot$team), week = 2:LASTW, stringsAsFactors = FALSE) |>
    anti_join(wk |> mutate(team = as.character(team)), by = c("team", "week")) |>
    mutate(team = factor(team, levels(tot$team)))
  pa <- ggplot(wk, aes(week, team)) +
    geom_tile(aes(fill = n), color = BG, linewidth = 0.8) +
    geom_text(data = byes, aes(label = "bye"), size = 2.5, family = "RobotoNFL", color = MUTED) +
    geom_text(data = wk |> filter(n > 0), aes(label = n, color = n >= 4), size = 3, family = "RobotoNFL", fontface = "bold") +
    scale_fill_gradient(low = if (DARK) "#22272c" else "#efefef", high = if (DARK) "#d1495b" else "#b2182b", limits = c(0, NA)) +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = INK)) +
    geom_nfl_logos(data = tot, aes(x = 1.05, y = team, team_abbr = as.character(team)), height = 0.026, inherit.aes = FALSE) +
    scale_x_continuous(breaks = 2:LASTW, labels = paste0("Wk ", 2:LASTW), position = "top", expand = expansion(add = c(0.95, 0))) +
    labs(x = NULL, y = NULL) +
    theme_nfl_axis(axis.text.y = element_blank(), panel.grid.major = element_blank())
  pb <- ggplot(by_r, aes(x = n, y = team, fill = reason)) +
    geom_col(width = 0.72, position = position_stack(reverse = TRUE)) +
    geom_text(data = tot, aes(x = n + 0.4, y = team, label = n), inherit.aes = FALSE, hjust = 0, size = 3.1,
              family = "RobotoNFL", fontface = "bold", color = INK) +
    scale_fill_manual(values = cols) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12)), breaks = pretty_breaks(4)) +
    labs(x = "Starter-games missed, season", y = NULL) +
    theme_nfl_axis(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
  title <- paste0("The ", w_nick(as.character(top$team)), " have been missing a starter ", top$n, " times since Week 2, the most; the ",
                  w_nick(as.character(bot$team)), " ", bot$n, " times")
  g <- patchwork::wrap_plots(pa, pb, widths = c(1.5, 1)) +
    patchwork::plot_annotation(
      title = wrap_text(title, 72),
      subtitle = paste0("Starters who did not play, by week and by reason: ", kw("reserve list", RED), ", ", kw("ruled out", ORANGE),
                        ", ", kw("questionable, then inactive", cols[["questionable"]]), ", ", kw("not on the injury report", GRAY)),
      caption = caption("Starter: top 11 each side by snap share over the prior 3 games; trades and releases left out"),
      theme = theme_nfl())
  save_chart(g, "W6_starters_missing", w = 12, h = 10.5)
}

# ---- W8: each team's quarterback this week against his own season ----

chart_w8_qb_form <- function(min_db = 12, min_prior = 60) {
  gm <- data_rds("pbp") |> filter(qb_dropback == 1, !is.na(epa), !is.na(id)) |>
    group_by(week, team = posteam, id) |> summarise(db = n(), epa = sum(epa), .groups = "drop")
  this <- gm |> group_by(week, team) |> slice_max(db, n = 1, with_ties = FALSE) |> ungroup() |>
    filter(week == LASTW, db >= min_db)
  prior <- gm |> filter(week < LASTW, id %in% this$id) |> group_by(id) |>
    summarise(pdb = sum(db), avg = sum(epa) / sum(db),
              best = suppressWarnings(max((epa / db)[db >= min_db])), worst = suppressWarnings(min((epa / db)[db >= min_db])),
              .groups = "drop")
  d <- this |> inner_join(prior, by = "id") |> filter(pdb >= min_prior) |>
    left_join(data_rds("players") |> select(id = gsis_id, name), by = "id") |>
    mutate(now = epa / db, chg = now - avg) |> arrange(chg) |> mutate(id = factor(id, id))
  up <- d |> slice_max(chg, n = 1, with_ties = FALSE); dn <- d |> slice_min(chg, n = 1, with_ties = FALSE)
  phrase <- function(r, dir) {
    if (dir > 0 && r$now > r$best) "his best game of the season"
    else if (dir < 0 && r$now < r$worst) "his worst"
    else paste0(signed(r$chg), " EPA per dropback against his average")
  }
  title <- paste0("Week ", LASTW, ": ", up$name, " had ", phrase(up, 1), "; ", dn$name, ", ", phrase(dn, -1))
  d$better <- d$chg > 0
  gg <- ggplot(d, aes(y = id)) +
    geom_vline(xintercept = 0, color = MUTED, linewidth = 0.3) +
    geom_segment(aes(x = avg, xend = now, yend = id, color = better), linewidth = 1.8, lineend = "round", alpha = 0.55) +
    geom_point(aes(x = avg), shape = 21, fill = BG, color = INK, size = 3, stroke = 0.9) +
    geom_point(aes(x = now, color = better), size = 3.6) +
    geom_label(aes(x = pmax(now, avg) + 0.035, label = paste0(name, "  ", signed(now), " (", signed(chg), ")")), hjust = 0,
               size = 3.1, family = "RobotoNFL", color = INK, fill = BG, linewidth = 0, label.padding = unit(0.12, "lines")) +
    scale_color_manual(values = c(`TRUE` = GREEN, `FALSE` = RED)) +
    scale_x_continuous(labels = function(x) signed(x, 0.1), breaks = pretty_breaks(8), expand = expansion(mult = c(0.03, 0.32))) +
    labs(title = title,
         subtitle = paste0("Open dot: his EPA per dropback before Week ", LASTW, ". Dot: Week ", LASTW, ", ", kw("better", GREEN), " or ",
                           kw("worse", RED), ". Each team's main passer (", min_prior, "+ earlier dropbacks)"),
         x = paste("EPA per dropback (points added per pass play, sacks and scrambles included)"), y = NULL,
         caption = caption(paste0("Main passer: most dropbacks for his team in Week ", LASTW, " (", min_db, "+)"), credit = HEADS)) +
    theme_nfl_axis(axis.text.y = element_nfl_headshot(size = 1.15), panel.grid.major.y = element_blank())
  save_chart(gg, "W8_qb_form", w = 12, h = 2.6 + 0.3 * nrow(d))
}
