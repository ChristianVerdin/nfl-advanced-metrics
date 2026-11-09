# Chicago Bears / Caleb Williams showcase. Every title is computed from the data, so it stays true on a rerun.

bears_col <- function() c(navy = "#0B162A", orange = "#E64100")

qb_season <- function(d, min_db) {
  d |> filter(qb_dropback == 1, !is.na(epa), !is.na(id)) |>
    group_by(id) |>
    summarise(db = n(), epa = mean(epa), cpoe = mean(cpoe, na.rm = TRUE), .groups = "drop") |>
    filter(db >= min_db) |>
    mutate(rank_epa = rank(-epa, ties.method = "min"), rank_cpoe = rank(-cpoe, ties.method = "min"), n = n())
}

chart_caleb_leap <- function() {
  h <- data_rds("pbp_2018_2025") |> filter(season_type == "REG")
  seasons <- bind_rows(
    qb_season(h |> filter(season == 2024), 200) |> mutate(season = "2024\nrookie"),
    qb_season(h |> filter(season == 2025), 200) |> mutate(season = "2025\nyear 1 with Ben Johnson"),
    qb_season(data_rds("pbp"), 150) |> mutate(season = paste0("2026\nthrough Week ", LASTW))
  ) |> mutate(season = factor(season, levels = unique(season)))
  long <- seasons |> pivot_longer(c(epa, cpoe), names_to = "metric", values_to = "value") |>
    mutate(metric = factor(ifelse(metric == "epa", "EPA per dropback", "Completion % over expected (CPOE)"),
                           levels = c("EPA per dropback", "Completion % over expected (CPOE)")),
           rank = ifelse(metric == "EPA per dropback", rank_epa, rank_cpoe))
  cw <- long |> filter(id == CALEB) |> left_join(sprites(), by = c("id" = "gsis_id"))
  r <- cw |> filter(metric == "EPA per dropback") |> arrange(season)
  c26 <- cw |> filter(metric != "EPA per dropback") |> arrange(season)
  set.seed(7)
  g <- ggplot(long, aes(season, value)) +
    geom_hline(yintercept = 0, color = INK, linewidth = 0.35) +
    geom_jitter(data = long |> filter(id != CALEB), width = 0.16, height = 0, size = 2.3, alpha = 0.35, color = "#5c6b7a") +
    geom_line(data = cw, aes(group = 1), color = bears_col()[["orange"]], linewidth = 1.2) +
    ggpath::geom_from_path(data = cw, aes(path = sprite), width = 0.13) +
    geom_label(data = cw, aes(label = paste0(ordinal(rank), " of ", n)), nudge_x = 0.33, family = "RobotoNFL",
               fontface = "bold", size = 3.6, color = bears_col()[["navy"]], fill = "white") +
    facet_wrap(~metric, scales = "free_y") +
    scale_y_continuous(labels = function(v) if (max(abs(v), na.rm = TRUE) > 1.5) paste0(signed(v, 1), "%") else signed(v),
                       breaks = pretty_breaks(6), expand = expansion(mult = 0.12)) +
    labs(
      title = paste0("Caleb Williams went from ", ordinal(r$rank[1]), " to ", ordinal(r$rank[2]), " to ",
                     ordinal(r$rank[3]), " in EPA per dropback"),
      subtitle = paste0("Gray dots: every qualifying QB each season. The 2026 sample is ", r$db[3], " dropbacks in 5 starts"),
      x = NULL, y = NULL,
      caption = caption(span = paste0("2024-2025 and 2026 ", wk_span(1, LASTW)), credit = HEADS)) +
    theme_nfl() + theme(strip.text = element_text(size = 13, hjust = 0), panel.grid.major.x = element_blank())
  save_chart(g, "B1_caleb_williams_leap", w = 13, h = 7.6)
}

starter_ids <- function() read.csv(file.path(ROOT, "assets", "headshots", "manifest.csv")) |>
  filter(group == "Starting QBs") |> pull(gsis_id)

chart_caleb_vs_starters <- function(n_heads = 9) {
  ids <- starter_ids()
  all_st <- qb_season(data_rds("pbp"), 1) |> filter(id %in% ids)
  d <- all_st |> filter(db >= 100) |> left_join(sprites(), by = c("id" = "gsis_id")) |>
    mutate(me = id == CALEB, rank_epa = rank(-epa, ties.method = "min"), rank_cpoe = rank(-cpoe, ties.method = "min"))
  cw <- d |> filter(me)
  heads <- d |> filter(rank_epa <= n_heads + 1) |> mutate(w = ifelse(me, 0.1, 0.06))
  heads <- cbind(heads, spread_xy(heads$cpoe, heads$epa, heads$w, range(d$cpoe), range(d$epa), aspect = 13 / 8.4))
  both <- d |> filter(!me, epa > cw$epa, cpoe > cw$cpoe)
  pl <- data_rds("players")
  better <- pl$name[match(both$id, pl$gsis_id)]
  title <- if (nrow(both) == 0) "No current starter is both more efficient and more accurate than Caleb Williams" else
    paste0("Only ", paste(better, collapse = " and "), if (nrow(both) == 1) " is" else " are",
           " both more efficient and more accurate than Caleb Williams")
  g <- ggplot(d, aes(cpoe, epa)) +
    geom_mean_lines(aes(x0 = cpoe, y0 = epa), color = "black", linetype = "dashed", alpha = 0.4, linewidth = 0.5) +
    annotate("rect", xmin = cw$cpoe, xmax = Inf, ymin = cw$epa, ymax = Inf, fill = ORANGE, alpha = 0.07) +
    annotate("text", x = Inf, y = Inf, label = "better than Caleb on both", hjust = 1.05, vjust = 1.6, family = "RobotoNFL",
             fontface = "bold", size = 3.5, color = ORANGE) +
    geom_point(data = d |> filter(!me, rank_epa > n_heads), color = GRAY, size = 3, alpha = 0.55) +
    geom_segment(data = heads, aes(x = cpoe, y = epa, xend = xs, yend = ys), color = "#9a9a9a", linewidth = 0.4) +
    geom_point(data = heads, aes(cpoe, epa), color = INK, size = 1.3) +
    ggpath::geom_from_path(data = heads |> filter(!me), aes(x = xs, y = ys, path = sprite), width = 0.06) +
    ggpath::geom_from_path(data = heads |> filter(me), aes(x = xs, y = ys, path = sprite), width = 0.1) +
    scale_x_continuous(labels = function(v) paste0(signed(v, 1), "%"), breaks = pretty_breaks(7), expand = expansion(mult = 0.08)) +
    scale_y_continuous(labels = function(v) signed(v), breaks = pretty_breaks(7), expand = expansion(mult = 0.12)) +
    labs(
      title = title,
      subtitle = paste0("Starting QBs with 100+ dropbacks: ", kw("Caleb", ORANGE), " (", ordinal(cw$rank_epa), " in EPA, ",
                        ordinal(cw$rank_cpoe), " in CPOE), headshots for the next ", n_heads, ", gray dots for the rest"),
      x = "Completion % over expected (CPOE)", y = "EPA per dropback", caption = caption(credit = HEADS)
    ) +
    theme_nfl()
  save_chart(g, "B2_caleb_vs_starting_qbs", w = 13, h = 8.4)
}

caleb_games <- function(p) {
  p |> filter(posteam == "CHI", qb_dropback == 1, !is.na(id)) |> count(game_id, id) |>
    group_by(game_id) |> slice_max(n, n = 1, with_ties = FALSE) |> ungroup() |> filter(id == CALEB) |> pull(game_id)
}

chart_caleb_targets <- function() {
  p <- data_rds("pbp"); pl <- data_rds("players")
  gms <- caleb_games(p)
  t <- p |> filter(game_id %in% gms, passer_player_id == CALEB, pass_attempt == 1, sack == 0, !is.na(receiver_player_id))
  d <- t |> group_by(gsis_id = receiver_player_id) |>
    summarise(tgt = n(), rec = sum(complete_pass), yds = sum(yards_gained * complete_pass, na.rm = TRUE),
              epa = mean(epa, na.rm = TRUE), ay = sum(air_yards, na.rm = TRUE), .groups = "drop") |>
    mutate(share = tgt / sum(tgt), ay_share = ay / sum(ay)) |>
    filter(tgt >= 4) |>
    left_join(pl |> select(gsis_id, name, position), by = "gsis_id") |> left_join(sprites(), by = "gsis_id") |>
    arrange(desc(share)) |> mutate(gsis_id = factor(gsis_id, levels = rev(gsis_id)))
  top <- d |> slice_head(n = 3)
  eff <- d |> filter(tgt >= 10) |> slice_max(epa, n = 1)
  g <- ggplot(d, aes(share, gsis_id)) +
    geom_col(aes(fill = gsis_id == eff$gsis_id), width = 0.62) +
    scale_fill_manual(values = c(`TRUE` = ORANGE, `FALSE` = NAVY), guide = "none") +
    geom_text(aes(x = share + 0.006, label = paste0(name, "   ", pct(share), " of targets  ·  ", rec, "/", tgt, " for ", yds,
                                                    " yds  ·  ", signed(epa), " EPA per target")),
              hjust = 0, family = "RobotoNFL", fontface = "bold", size = 3.5, color = INK) +
    ggpath::geom_from_path(aes(x = -0.02, path = circle), width = 0.05) +
    scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(-0.04, max(d$share) * 2.45),
                       breaks = seq(0, 0.3, 0.05), expand = c(0, 0)) +
    labs(
      title = paste0("Caleb Williams keeps everyone involved, and ", eff$name, " makes the most of it"),
      subtitle = paste0("Every Bear with 4+ targets from Caleb in his ", length(gms), " starts"),
      x = "Share of Caleb's targets", y = NULL, caption = caption(credit = HEADS)
    ) +
    theme_nfl() + theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
  save_chart(g, "B3_caleb_targets", w = 13, h = 7.6)
}

chart_caleb_depth <- function() {
  p <- data_rds("pbp")
  att <- p |> filter(pass_attempt == 1, sack == 0, !is.na(air_yards), !is.na(cpoe)) |>
    mutate(depth = cut(air_yards, c(-Inf, -0.5, 9.5, 19.5, Inf),
                       labels = c("Behind the line", "Short (0-9 yds)", "Intermediate (10-19)", "Deep (20+)")))
  cw <- att |> filter(passer_player_id == CALEB) |> group_by(depth) |>
    summarise(n = n(), cpoe = mean(cpoe), comp = mean(complete_pass), .groups = "drop") |> mutate(who = "Caleb Williams")
  lg <- att |> group_by(depth) |> summarise(n = n(), cpoe = mean(cpoe), comp = mean(complete_pass), .groups = "drop") |>
    mutate(who = "League")
  d <- bind_rows(cw, lg)
  best <- cw |> slice_max(cpoe, n = 1)
  deep <- cw |> filter(depth == "Deep (20+)")
  s <- sprites() |> filter(gsis_id == CALEB)
  g <- ggplot(d, aes(depth, comp, fill = who)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    geom_text(aes(label = paste0(pct(comp), "\n(", n, ")")), position = position_dodge(width = 0.75), vjust = -0.25,
              family = "RobotoNFL", fontface = "bold", size = 3.4, lineheight = 0.9) +
    geom_label(data = cw, aes(y = comp / 2, label = paste0(signed(cpoe, 0.1), "%\nvs expected")), fill = "white",
               position = position_nudge(x = -0.19), family = "RobotoNFL", fontface = "bold", size = 3.2, color = bears_col()[["orange"]],
               lineheight = 0.9) +
    ggpath::geom_from_path(data = s, aes(x = 4.25, y = 0.9, path = sprite), width = 0.11, inherit.aes = FALSE) +
    scale_fill_manual(values = c("Caleb Williams" = ORANGE, "League" = GRAY), name = NULL) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.05), expand = c(0, 0)) +
    labs(
      title = paste0("Caleb Williams beats expected completion ", if (all(cw$cpoe > 0)) "at every depth" else "at most depths",
                     ", most of all ", tolower(sub(" \\(.*", "", best$depth)), " (", signed(best$cpoe, 0.1), "%)"),
      subtitle = paste0("Completion rate by depth of throw: ", kw("Caleb Williams", ORANGE), " vs the ", kw("league", GRAY), " (attempts in brackets)"),
      x = NULL, y = "Completion rate", caption = caption("Expected completion: nflfastR model", credit = HEADS)) +
    theme_nfl() + theme(panel.grid.major.x = element_blank())
  save_chart(g, "B4_caleb_by_depth", w = 12.5, h = 7.4)
}

chart_bears_backfield <- function() {
  p <- data_rds("pbp"); pl <- data_rds("players")
  r <- p |> filter(posteam == "CHI", rush == 1, !is.na(rusher_player_id), !is.na(epa)) |>
    left_join(pl |> select(rusher_player_id = gsis_id, position), by = "rusher_player_id") |> filter(position %in% c("RB", "FB"))
  d <- r |> group_by(gsis_id = rusher_player_id) |>
    summarise(carries = n(), ypc = mean(yards_gained, na.rm = TRUE), succ = mean(success, na.rm = TRUE),
              epa = mean(epa), expl = mean(yards_gained >= 10, na.rm = TRUE), .groups = "drop") |>
    mutate(share = carries / sum(carries)) |> slice_max(carries, n = 2) |>
    left_join(pl |> select(gsis_id, name), by = "gsis_id") |> left_join(sprites(), by = "gsis_id")
  long <- d |> select(gsis_id, name, sprite, `Carry share` = share, `Success rate` = succ, `10+ yard runs` = expl,
                      `Yards per carry` = ypc, `EPA per carry` = epa) |>
    pivot_longer(-c(gsis_id, name, sprite), names_to = "metric") |>
    mutate(metric = factor(metric, levels = c("Carry share", "Success rate", "10+ yard runs", "Yards per carry", "EPA per carry")),
           lab = ifelse(metric %in% c("Yards per carry"), number(value, 0.1), ifelse(metric == "EPA per carry", signed(value), pct(value))),
           who = last_name(name))
  lead <- d |> slice_max(carries, n = 1); other <- d |> slice_min(carries, n = 1)
  eff <- d |> slice_max(epa, n = 1); noeff <- d |> slice_min(epa, n = 1)
  title <- if (eff$gsis_id != lead$gsis_id) paste0(last_name(lead$name), " gets the carries; ", last_name(eff$name), " does more with them") else
    paste0(lead$name, " gets the carries and makes the most of them")
  long <- long |> mutate(sprite = factor(sprite, levels = c(other$sprite, lead$sprite)))
  g <- ggplot(long, aes(value, sprite, fill = who)) +
    geom_col(width = 0.6) +
    geom_vline(xintercept = 0, color = INK, linewidth = 0.35) +
    geom_text(aes(label = lab, hjust = ifelse(value >= 0, -0.15, 1.15)), family = "RobotoNFL", fontface = "bold", size = 3.8) +
    facet_grid(~metric, scales = "free_x") +
    scale_fill_manual(values = setNames(c(bears_col()[["navy"]], bears_col()[["orange"]]), c(last_name(lead$name), last_name(other$name)))) +
    scale_x_continuous(expand = expansion(mult = c(0.45, 0.45)), labels = NULL) +
    labs(
      title = title,
      subtitle = paste0("Chicago's top two running backs, 2026 ", wk_span(1, LASTW)),
      x = NULL, y = NULL, caption = caption("Success rate: share of runs with positive EPA", credit = HEADS)) +
    theme_nfl_axis(panel.grid.major = element_blank(), axis.text.y = ggpath::element_path(size = 3.2),
                   strip.text = element_text(size = 12, face = "bold"))
  save_chart(g, "B5_bears_backfield", w = 13, h = 5.6)
}

chart_bears_games <- function() {
  p <- data_rds("pbp"); sc <- data_rds("schedules")
  st <- p |> filter(posteam == "CHI", qb_dropback == 1, !is.na(id)) |> count(game_id, week, id) |>
    group_by(game_id) |> slice_max(n, n = 1, with_ties = FALSE) |> ungroup() |>
    left_join(data_rds("players") |> select(id = gsis_id, qb = name), by = "id")
  off <- plays_epa(p) |> filter(posteam == "CHI") |> group_by(game_id) |> summarise(epa = mean(epa), .groups = "drop")
  g1 <- sc |> filter(home_team == "CHI" | away_team == "CHI") |>
    transmute(game_id, week, opp = ifelse(home_team == "CHI", away_team, home_team),
              pf = ifelse(home_team == "CHI", home_score, away_score), pa = ifelse(home_team == "CHI", away_score, home_score),
              where = ifelse(home_team == "CHI", "vs", "at")) |>
    left_join(st |> select(game_id, qb), by = "game_id") |> left_join(off, by = "game_id") |>
    mutate(caleb = qb == "Caleb Williams", res = paste0(ifelse(pf > pa, "W ", "L "), pf, "-", pa))
  w <- g1 |> filter(caleb); wo <- g1 |> filter(!caleb)
  g <- ggplot(g1, aes(week, epa)) +
    geom_hline(yintercept = 0, color = INK, linewidth = 0.4) +
    geom_col(aes(fill = caleb), width = 0.62) +
    geom_nfl_logos(aes(y = ifelse(epa >= 0, epa + 0.06, epa - 0.06), team_abbr = opp), width = 0.055) +
    geom_text(aes(y = ifelse(epa >= 0, epa + 0.135, epa - 0.135), label = res), family = "RobotoNFL", fontface = "bold", size = 3.6) +
    scale_fill_manual(values = c(`TRUE` = ORANGE, `FALSE` = GRAY)) +
    scale_x_continuous(breaks = g1$week, labels = paste0("Wk ", g1$week, "\n", g1$where, " ", g1$opp, "\n", last_name(g1$qb))) +
    scale_y_continuous(labels = function(v) signed(v), breaks = pretty_breaks(6),
                       limits = c(min(0, min(g1$epa)) - 0.2, max(g1$epa) + 0.2)) +
    labs(
      title = paste0("The Bears are ", sum(w$pf > w$pa), "-", sum(w$pf < w$pa), " when Caleb Williams starts, scoring ",
                     number(mean(w$pf), 0.1), " a game (", number(mean(wo$pf), 0.1), " without him)"),
      subtitle = paste0("Chicago's offensive EPA per play each game: ", kw("Williams started", ORANGE), ", ", kw(paste(unique(last_name(wo$qb)), collapse = " or "), GRAY), " started"),
      x = NULL, y = "Offensive EPA per play", caption = caption()
    ) +
    theme_nfl() + theme(panel.grid.major.x = element_blank(), axis.text.x = element_text(lineheight = 1.1))
  save_chart(g, "B6_bears_game_by_game", w = 13, h = 7.2)
}

chart_caleb_sacks <- function() {
  h <- data_rds("pbp_2018_2025") |> filter(season_type == "REG", season >= 2024)
  rate <- function(d, m) d |> filter(qb_dropback == 1, !is.na(id)) |> group_by(season, id) |>
    summarise(db = n(), sk = sum(sack == 1, na.rm = TRUE), .groups = "drop") |> filter(db >= m) |>
    group_by(season) |> mutate(rate = sk / db, rk = rank(rate, ties.method = "min"), n = n()) |> ungroup()
  d <- bind_rows(rate(h, 200), rate(data_rds("pbp") |> mutate(season = SEASON), 150)) |>
    mutate(lab = c("2024" = "2024\nrookie", "2025" = "2025\nyear 1 with Ben Johnson", "2026" = paste0("2026\nthrough Week ", LASTW))[as.character(season)])
  d$lab <- factor(d$lab, levels = unique(d$lab[order(d$season)]))
  cw <- d |> filter(id == CALEB) |> left_join(sprites(), by = c("id" = "gsis_id")) |> arrange(season)
  lg <- d |> group_by(lab) |> summarise(lg = sum(sk) / sum(db), .groups = "drop")
  set.seed(11)
  g <- ggplot(d, aes(lab, rate)) +
    geom_jitter(data = d |> filter(id != CALEB), width = 0.16, height = 0, size = 2.3, alpha = 0.35, color = "#5c6b7a") +
    geom_errorbar(data = lg, aes(x = lab, ymin = lg, ymax = lg), width = 0.42, color = MUTED, linetype = "dashed", inherit.aes = FALSE) +
    geom_line(data = cw, aes(group = 1), color = bears_col()[["orange"]], linewidth = 1.2) +
    ggpath::geom_from_path(data = cw, aes(path = sprite), width = 0.11) +
    geom_label(data = cw, aes(label = paste0(sk, " sacks, ", pct(rate, 0.1), "\n", ordinal(rk), " of ", n)), nudge_x = 0.34,
               family = "RobotoNFL", fontface = "bold", size = 3.5, color = bears_col()[["navy"]], fill = "white", lineheight = 0.95) +
    scale_y_reverse(labels = percent_format(accuracy = 1), breaks = pretty_breaks(6), expand = expansion(mult = 0.12)) +
    labs(
      title = paste0("Caleb Williams cut his sack rate from ", pct(cw$rate[1], 0.1), " as a rookie to ", pct(cw$rate[2], 0.1),
                     " in 2025; it is back to ", pct(cw$rate[3], 0.1), " this season"),
      subtitle = "Sacks per dropback. Gray dots: every qualifying QB; dashes: league rate. Fewer is better, so the axis runs down",
      x = NULL, y = "Sacked on this share of dropbacks",
      caption = caption("Sacks also reflect protection", span = paste0("2024-2025 and 2026 ", wk_span(1, LASTW)), credit = HEADS)) +
    theme_nfl() + theme(panel.grid.major.x = element_blank())
  save_chart(g, "B7_caleb_sack_rate", w = 12.5, h = 7.4)
}

chart_bears_third_down <- function() {
  h <- data_rds("pbp_2018_2025") |> filter(season_type == "REG")
  x <- bind_rows(h, data_rds("pbp") |> mutate(season = SEASON)) |> filter(down == 3, pass == 1 | rush == 1, !is.na(posteam))
  all <- x |> group_by(season, team = posteam) |> summarise(conv = mean(yards_gained >= ydstogo, na.rm = TRUE), .groups = "drop") |>
    group_by(season) |> mutate(rk = rank(-conv, ties.method = "min"), lg = mean(conv)) |> ungroup()
  chi <- all |> filter(team == "CHI") |> arrange(season) |>
    mutate(valley = line_label_pos(conv)$type == "valley", ny = ifelse(valley, -0.006, 0.006), vj = ifelse(valley, 1, 0))
  eras <- data.frame(xmin = c(2017.5, 2021.5, 2024.5), xmax = c(2021.5, 2024.5, SEASON + 0.5),
                     lab = c("Matt Nagy", "Matt Eberflus", "Ben Johnson"), fill = c("#eef0f3", "#e6e9ee", "#fbe3d6"))
  a <- chi |> filter(season == 2024); b <- chi |> filter(season == 2025)
  g <- ggplot(chi, aes(season, conv)) +
    geom_rect(data = eras, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill), inherit.aes = FALSE) +
    geom_text(data = eras, aes(x = (xmin + xmax) / 2, y = Inf, label = lab), vjust = 1.6, family = "RobotoNFL", fontface = "bold",
              size = 3.8, color = MUTED, inherit.aes = FALSE) +
    scale_fill_identity() +
    geom_line(data = all |> distinct(season, lg), aes(season, lg), linetype = "dashed", color = MUTED) +
    geom_line(color = bears_col()[["navy"]], linewidth = 1.3) +
    geom_point(aes(color = season >= 2025), size = 4.5) +
    geom_text(aes(y = conv + ny, label = paste0(pct(conv), "\n", ordinal(rk)), vjust = vj), family = "RobotoNFL", fontface = "bold",
              size = 3.3, lineheight = 0.9) +
    scale_color_manual(values = c(`TRUE` = bears_col()[["orange"]], `FALSE` = bears_col()[["navy"]])) +
    scale_x_continuous(breaks = 2018:SEASON, labels = function(v) ifelse(v == SEASON, paste0(v, "*"), v)) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0.28, 0.47)) +
    labs(
      title = paste0("Under Ben Johnson, the Bears went from ", ordinal(a$rk), " to ", ordinal(b$rk), " on third down"),
      subtitle = paste0("Third-down conversion rate and league rank by season; dashed = league average. *", SEASON, " = Weeks ", min(WEEKS), "-", max(WEEKS)),
      x = NULL, y = "Third-down conversion rate",
      caption = caption("Eberflus was replaced by interim coach Thomas Brown in Nov. 2024", span = paste0("2018-", SEASON), credit = NULL)) +
    theme_nfl() + theme(panel.grid.major.x = element_blank())
  save_chart(g, "B8_bears_third_down_by_coach", w = 12.5, h = 7)
}

chart_bears_defense <- function() {
  p <- data_rds("pbp"); sc <- data_rds("schedules")
  d <- p |> filter(pass == 1 | rush == 1, !is.na(defteam), !is.na(epa))
  pa <- bind_rows(sc |> transmute(team = home_team, pa = away_score), sc |> transmute(team = away_team, pa = home_score)) |>
    group_by(team) |> summarise(pa = mean(pa), .groups = "drop")
  m <- d |> group_by(team = defteam) |>
    summarise(`Third-down conversions allowed` = mean((yards_gained >= ydstogo)[down == 3], na.rm = TRUE),
              `Success rate allowed` = mean(success, na.rm = TRUE), `EPA per play allowed` = mean(epa),
              `Yards per play allowed` = mean(yards_gained, na.rm = TRUE), `Plays of 20+ yards allowed` = mean(yards_gained >= 20, na.rm = TRUE),
              .groups = "drop") |>
    left_join(pa |> rename(`Points allowed per game` = pa), by = "team") |>
    pivot_longer(-team, names_to = "metric") |> group_by(metric) |>
    mutate(rank = rank(value, ties.method = "min"), lg = mean(value)) |> ungroup()
  chi <- m |> filter(team == "CHI") |>
    mutate(lab = ifelse(metric %in% c("Yards per play allowed", "Points allowed per game"), number(value, 0.1),
                        ifelse(metric == "EPA per play allowed", signed(value), pct(value, 0.1))),
           lglab = ifelse(metric %in% c("Yards per play allowed", "Points allowed per game"), number(lg, 0.1),
                          ifelse(metric == "EPA per play allowed", signed(lg), pct(lg, 0.1))),
           metric = reorder(metric, -rank))
  best <- chi |> slice_min(rank, n = 1); worst <- chi |> slice_max(rank, n = 1)
  g <- ggplot(chi, aes(33 - rank, metric)) +
    geom_vline(xintercept = 16.5, linetype = "dashed", color = MUTED) +
    geom_segment(aes(x = 0, xend = 33 - rank, yend = metric), color = "#c9ced6", linewidth = 2.2) +
    geom_point(aes(color = rank <= 10), size = 8) +
    geom_text(aes(label = rank), color = "white", family = "RobotoNFL", fontface = "bold", size = 3.6) +
    geom_text(aes(x = 33.5, label = paste0(lab, "  (league ", lglab, ")")), hjust = 0, family = "RobotoNFL", size = 3.6, color = INK) +
    annotate("text", x = 16.5, y = 0.4, label = "league middle", family = "RobotoNFL", size = 3, color = MUTED, vjust = 0) +
    scale_color_manual(values = c(`TRUE` = bears_col()[["orange"]], `FALSE` = bears_col()[["navy"]])) +
    scale_x_continuous(limits = c(0, 46), breaks = c(1, 8, 16, 24, 32), labels = function(v) ordinal(33 - v),
                       expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    labs(
      title = paste0("The Bears defense gives up yards and big plays, then wins the down that matters: ", ordinal(best$rank),
                     " in ", tolower(best$metric), ", ", ordinal(worst$rank), " in ", tolower(worst$metric)),
      subtitle = "Chicago's defensive rank among 32 teams (1st = best), with the value and the league average",
      x = "Rank (1st = best, to the right)", y = NULL, caption = caption()
    ) +
    theme_nfl() + theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(face = "bold", size = 12))
  save_chart(g, "B9_bears_defense_profile", w = 13, h = 6.6)
}
