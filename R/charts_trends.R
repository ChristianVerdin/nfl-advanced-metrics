# How teams have trended across the 2026 season: growth and decline between periods. Net EPA per play = offensive EPA
# per play minus defensive EPA per play allowed (passes and runs). Not adjusted for opponents; byes mean teams played 3 or 4
# games in each half.

period_epa <- function(p, periods) {
  e <- plays_epa(p) |> mutate(period = periods(week))
  off <- e |> group_by(team = posteam, period) |> summarise(off = mean(epa), op = n(), .groups = "drop")
  def <- e |> group_by(team = defteam, period) |> summarise(def = mean(epa), dp = n(), .groups = "drop")
  inner_join(off, def, by = c("team", "period")) |> mutate(net = off - def)
}

halves <- function(w) ifelse(w <= 4, "Weeks 1-4", "Weeks 5-8")
nick <- function(a) { tm <- data_rds("teams"); tm$team_nick[match(a, tm$team_abbr)] }

trend_halves <- function() {
  period_epa(data_rds("pbp"), halves) |>
    select(team, period, off, def, net) |>
    pivot_wider(names_from = period, values_from = c(off, def, net)) |>
    rename(off1 = `off_Weeks 1-4`, off2 = `off_Weeks 5-8`, def1 = `def_Weeks 1-4`, def2 = `def_Weeks 5-8`,
           net1 = `net_Weeks 1-4`, net2 = `net_Weeks 5-8`) |>
    mutate(chg = net2 - net1)
}

chart_trend_dumbbell <- function() {
  d <- trend_halves() |> mutate(team = reorder(team, chg), dir = ifelse(chg >= 0, "up", "down"))
  up <- d |> slice_max(chg, n = 1); dn <- d |> slice_min(chg, n = 1)
  n_up <- sum(d$chg > 0)
  g <- ggplot(d, aes(y = team)) +
    geom_vline(xintercept = 0, color = INK, linewidth = 0.35) +
    geom_segment(aes(x = net1, xend = net2, yend = team, color = dir), linewidth = 1.6,
                 arrow = arrow(length = unit(0.12, "inches"), type = "closed")) +
    geom_point(aes(x = net1), size = 2.6, color = "#9aa6b2") +
    geom_text(aes(x = pmax(net1, net2) + 0.03, label = signed(chg)), hjust = 0, family = "RobotoNFL", fontface = "bold",
              size = 3.1, color = INK) +
    scale_color_manual(values = c(up = "#1a7f4b", down = "#c0392b")) +
    scale_x_continuous(labels = function(v) signed(v), breaks = pretty_breaks(7), expand = expansion(mult = c(0.04, 0.1))) +
    labs(
      title = paste0("The ", nick(up$team), " improved the most from the first four weeks to the next four (", signed(up$chg),
                     "); the ", nick(dn$team), " fell the furthest (", signed(dn$chg), ")"),
      subtitle = paste0("Net EPA per play (offense minus defense allowed) in Weeks 1-4 (gray dot) and Weeks 5-8 (arrow head). ",
                        n_up, " of 32 teams improved. Sorted by the change, shown at the right. Not adjusted for opponents."),
      x = "Net EPA per play", y = NULL, caption = caption()
    ) +
    theme_nfl_axis(axis.text.y = element_nfl_logo(size = 0.75), panel.grid.major.y = element_blank())
  save_chart(g, "T1_trend_first_vs_second_four_weeks", w = 11.5, h = 12)
}

chart_trend_arrows <- function() {
  d <- trend_halves()
  both_up <- d |> filter(off2 > off1, def2 < def1); both_dn <- d |> filter(off2 < off1, def2 > def1)
  d <- d |> mutate(kind = ifelse(team %in% both_up$team, "both_up", ifelse(team %in% both_dn$team, "both_dn", "mixed")))
  g <- ggplot(d) +
    geom_vline(xintercept = mean(c(d$off1, d$off2)), linetype = "dashed", color = MUTED, alpha = 0.6) +
    geom_hline(yintercept = mean(c(d$def1, d$def2)), linetype = "dashed", color = MUTED, alpha = 0.6) +
    geom_segment(data = d |> filter(kind == "mixed"), aes(x = off1, y = def1, xend = off2, yend = def2),
                 color = "#c9ced6", linewidth = 0.6, arrow = arrow(length = unit(0.07, "inches"), type = "closed")) +
    geom_segment(data = d |> filter(kind != "mixed"), aes(x = off1, y = def1, xend = off2, yend = def2, color = kind),
                 linewidth = 1.2, arrow = arrow(length = unit(0.11, "inches"), type = "closed")) +
    geom_point(data = d |> filter(kind != "mixed"), aes(off1, def1, color = kind), size = 2.2) +
    geom_nfl_logos(data = d |> filter(kind == "mixed"), aes(x = off2, y = def2, team_abbr = team), width = 0.03, alpha = 0.35) +
    geom_nfl_logos(data = d |> filter(kind != "mixed"), aes(x = off2, y = def2, team_abbr = team), width = 0.045) +
    annotate("text", x = Inf, y = -Inf, label = "better offense, better defense", hjust = 1.05, vjust = 1.6,
             family = "RobotoNFL", fontface = "bold", size = 3.6, color = "#1a7f4b") +
    annotate("text", x = -Inf, y = Inf, label = "worse on both sides", hjust = -0.05, vjust = -0.8,
             family = "RobotoNFL", fontface = "bold", size = 3.6, color = "#c0392b") +
    scale_color_manual(values = c(both_up = "#1a7f4b", both_dn = "#c0392b", mixed = "#9aa6b2")) +
    scale_x_continuous(labels = function(v) signed(v), breaks = pretty_breaks(7)) +
    scale_y_reverse(labels = function(v) signed(v), breaks = pretty_breaks(7)) +
    coord_cartesian(clip = "off") +
    labs(
      title = paste0(nrow(both_up), " teams got better on both sides of the ball after Week 4; ", nrow(both_dn),
                     " got worse on both"),
      subtitle = paste0("Each arrow runs from a team's Weeks 1-4 position (gray dot) to Weeks 5-8 (logo). Up and to the right is better. ",
                        "Green: improved on offense and defense (", paste(nick(both_up$team), collapse = ", "), "). Red: declined on both (",
                        paste(nick(both_dn$team), collapse = ", "), "). Faded: better on one side, worse on the other."),
      x = "Offense: EPA per play", y = "Defense: EPA per play allowed (lower is better)", caption = caption()
    ) +
    theme_nfl()
  save_chart(g, "T2_trend_offense_defense_arrows", w = 13, h = 9)
}

chart_trend_small_multiples <- function() {
  tm <- data_rds("teams")
  e <- plays_epa(data_rds("pbp"))
  off <- e |> group_by(team = posteam, week) |> summarise(off = mean(epa), .groups = "drop")
  def <- e |> group_by(team = defteam, week) |> summarise(def = mean(epa), .groups = "drop")
  d <- inner_join(off, def, by = c("team", "week")) |> mutate(net = off - def) |> arrange(team, week) |>
    group_by(team) |> mutate(roll = (net + lag(net, default = NA) + lag(net, 2, default = NA)) / 3,
                             roll = ifelse(is.na(roll), cummean(net), roll)) |> ungroup()
  ord <- tm |> arrange(team_conf, team_division, team_abbr) |> pull(team_abbr)
  d <- d |> mutate(team = factor(team, levels = ord))
  last3 <- d |> group_by(team) |> slice_max(week, n = 1) |> ungroup()
  first3 <- d |> group_by(team) |> slice_min(week, n = 3) |> summarise(f = mean(net), .groups = "drop")
  tr <- last3 |> left_join(first3, by = "team") |> mutate(chg = roll - f)
  g <- ggplot(d, aes(week, net)) +
    geom_hline(yintercept = 0, color = "#9a9a9a", linewidth = 0.3) +
    geom_col(aes(fill = net >= 0), width = 0.7, alpha = 0.55) +
    geom_line(aes(y = roll), color = INK, linewidth = 0.8) +
    geom_point(data = last3, aes(y = roll, color = tr$chg[match(team, tr$team)] >= 0), size = 2.2) +
    facet_wrap(~team, ncol = 8) +
    scale_fill_manual(values = c(`TRUE` = "#1a7f4b", `FALSE` = "#c0392b")) +
    scale_color_manual(values = c(`TRUE` = "#1a7f4b", `FALSE` = "#c0392b")) +
    scale_x_continuous(breaks = c(1, 4, 8)) +
    scale_y_continuous(breaks = c(-0.5, 0, 0.5), labels = function(v) signed(v, 0.1)) +
    labs(
      title = "Every team's season so far, game by game",
      subtitle = paste0("Bars: net EPA per play in each game (green above zero). Line: three-game rolling average. ",
                        "The end dot is green if a team's last three games beat its first three. Grouped by division, AFC then NFC; gaps are byes."),
      x = "Week", y = "Net EPA per play", caption = caption()
    ) +
    theme_nfl_axis(strip.text = element_nfl_logo(size = 0.9), panel.spacing.x = unit(10, "pt"), panel.spacing.y = unit(8, "pt"),
                   axis.text = element_text(size = 8, color = "#666666"), panel.grid.major.x = element_blank())
  save_chart(g, "T3_every_team_game_by_game", w = 14, h = 10)
}

chart_trend_bump <- function(k = 4) {
  thirds <- function(w) ifelse(w <= 3, "Weeks 1-3", ifelse(w <= 6, "Weeks 4-6", "Weeks 7-8"))
  d <- period_epa(data_rds("pbp"), thirds) |> group_by(period) |> mutate(rank = rank(-net, ties.method = "first")) |> ungroup() |>
    mutate(x = match(period, c("Weeks 1-3", "Weeks 4-6", "Weeks 7-8")))
  mv <- d |> select(team, x, rank) |> pivot_wider(names_from = x, values_from = rank, names_prefix = "r") |> mutate(mv = r1 - r3)
  risers <- mv |> slice_max(mv, n = k, with_ties = FALSE); fallers <- mv |> slice_min(mv, n = k, with_ties = FALSE)
  hl <- c(risers$team, fallers$team)
  d <- d |> mutate(hl = team %in% hl, kind = ifelse(team %in% risers$team, "rise", ifelse(team %in% fallers$team, "fall", "other")))
  ends <- d |> filter(hl, x %in% c(1, 3)) |> mutate(xl = ifelse(x == 1, 0.86, 3.14))
  g <- ggplot(d, aes(x, rank, group = team)) +
    geom_line(data = d |> filter(!hl), color = "#dfe3e8", linewidth = 0.8) +
    geom_line(data = d |> filter(hl), aes(color = kind), linewidth = 1.8, alpha = 0.9) +
    geom_point(data = d |> filter(hl), aes(color = kind), size = 3) +
    geom_nfl_logos(data = ends, aes(x = xl, team_abbr = team), width = 0.034) +
    scale_color_manual(values = c(rise = "#1a7f4b", fall = "#c0392b")) +
    scale_y_reverse(breaks = c(1, 8, 16, 24, 32), labels = function(v) ordinal(v), expand = expansion(add = 0.8)) +
    scale_x_continuous(breaks = 1:3, labels = c("Weeks 1-3", "Weeks 4-6", "Weeks 7-8"), limits = c(0.75, 3.25)) +
    labs(
      title = paste0("The biggest climbs: ", paste0(nick(risers$team), " (", ordinal(risers$r1), " to ", ordinal(risers$r3), ")", collapse = ", ")),
      subtitle = paste0("Net EPA per play rank (1st = best) in three stretches of the season. Green: the ", k, " biggest risers. Red: the ", k,
                        " biggest fallers (", paste0(nick(fallers$team), " ", ordinal(fallers$r1), " to ", ordinal(fallers$r3), collapse = ", "), ")."),
      x = NULL, y = "Rank", caption = caption("Weeks 7-8 is two games or fewer per team, so it moves the most.")
    ) +
    theme_nfl() + theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "#ececec"),
                        axis.text.x = element_text(face = "bold", size = 12))
  save_chart(g, "T4_rank_bump_chart", w = 12, h = 10)
}
