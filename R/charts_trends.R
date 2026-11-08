# How teams have trended across the 2026 season: growth and decline between periods. Net EPA per play = offensive EPA
# per play minus defensive EPA per play allowed (passes and runs). Not adjusted for opponents; byes mean teams played 3 or 4
# games in each half.

period_epa <- function(p, periods) {
  e <- plays_epa(p) |> mutate(period = periods(week))
  off <- e |> group_by(team = posteam, period) |> summarise(off = mean(epa), op = n(), .groups = "drop")
  def <- e |> group_by(team = defteam, period) |> summarise(def = mean(epa), dp = n(), .groups = "drop")
  inner_join(off, def, by = c("team", "period")) |> mutate(net = off - def)
}

halves <- function(w) ifelse(w <= HALF, "a", "b")   # first half of the weeks so far vs the rest
nick <- function(a) { tm <- data_rds("teams"); tm$team_nick[match(a, tm$team_abbr)] }

trend_halves <- function() {
  period_epa(data_rds("pbp"), halves) |>
    select(team, period, off, def, net) |>
    pivot_wider(names_from = period, values_from = c(off, def, net)) |>
    rename(off1 = off_a, off2 = off_b, def1 = def_a, def2 = def_b, net1 = net_a, net2 = net_b) |>
    mutate(chg = net2 - net1)
}

chart_trend_dumbbell <- function(k = 8) {
  all <- trend_halves()
  n_up <- sum(all$chg > 0)
  d <- bind_rows(all |> slice_max(chg, n = k), all |> slice_min(chg, n = k)) |>
    mutate(team = reorder(team, chg), dir = ifelse(chg >= 0, "up", "down"))
  up <- d |> slice_max(chg, n = 1); dn <- d |> slice_min(chg, n = 1)
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
      subtitle = paste0("Net EPA per play, ", wk_span(1, HALF), " (dot) to ", wk_span(HALF + 1, LASTW), " (arrow): the ", k, " ", kw("biggest risers", GREEN), " and ", kw("fallers", RED)),
      x = "Net EPA per play", y = NULL, caption = caption(paste0("Net EPA = offense minus defense allowed, not opponent-adjusted. All 32: ", n_up, " improved, ", 32 - n_up, " declined"))) +
    theme_nfl_axis(axis.text.y = element_nfl_logo(size = 0.9), panel.grid.major.y = element_blank())
  save_chart(g, "T1_trend_first_vs_second_four_weeks", w = 12, h = 8.4)
}

chart_trend_arrows <- function(k = 8) {
  d <- trend_halves() |> mutate(net_now = off2 - def2)
  best <- d |> slice_max(net_now, n = k) |> mutate(panel = paste0("THE BEST ", k, " RIGHT NOW"))
  worst <- d |> slice_min(net_now, n = k) |> mutate(panel = paste0("THE WORST ", k, " RIGHT NOW"))
  d2 <- bind_rows(best, worst) |> mutate(panel = factor(panel, levels = c(best$panel[1], worst$panel[1])),
                                         dir = ifelse(chg >= 0, "up", "down"))
  rose <- best |> filter(chg > 0); fell <- worst |> filter(chg < 0)
  g <- ggplot(d2) +
    geom_segment(aes(x = off1, y = def1, xend = off2, yend = def2, color = dir), linewidth = 1.1,
                 arrow = arrow(length = unit(0.1, "inches"), type = "closed")) +
    geom_point(aes(off1, def1, color = dir), size = 2.2) +
    geom_nfl_logos(aes(x = off2, y = def2, team_abbr = team), width = 0.085) +
    facet_wrap(~panel, scales = "free") +
    scale_color_manual(values = c(up = GREEN, down = RED)) +
    scale_x_continuous(labels = function(v) signed(v), breaks = pretty_breaks(4), expand = expansion(mult = 0.15)) +
    scale_y_reverse(labels = function(v) signed(v), breaks = pretty_breaks(4), expand = expansion(mult = 0.15)) +
    labs(
      title = paste0(length(rose$team), " of today's top ", k, " climbed there after Week ", HALF, "; ", length(fell$team), " of the bottom ", k, " fell there"),
      subtitle = paste0("Offense vs defense EPA per play: ", wk_span(1, HALF), " (dot) to ", wk_span(HALF + 1, LASTW), " (logo), ", kw("improved", GREEN), " or ",
                        kw("declined", RED), ". Up and right is better"),
      x = "Offense: EPA per play", y = "Defense: EPA allowed per play",
      caption = caption(paste0("Best and worst by net EPA per play in ", wk_span(HALF + 1, LASTW), "; each panel has its own scale"))
    ) +
    theme_nfl() + theme(strip.text = element_text(size = 13, hjust = 0, face = "bold"))
  save_chart(g, "T2_trend_offense_defense_arrows", w = 13, h = 7.6)
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
      subtitle = paste0("Net EPA per play each game (bars) and three-game average (line). End dot: ", kw("trending up", GREEN), " or ", kw("down", RED), " vs the first three games"),
      x = "Week", y = "Net EPA per play", caption = caption()
    ) +
    theme_nfl_axis(strip.text = element_nfl_logo(size = 0.9), panel.spacing.x = unit(10, "pt"), panel.spacing.y = unit(8, "pt"),
                   axis.text = element_text(size = 8, color = "#666666"), panel.grid.major.x = element_blank())
  save_chart(g, "T3_every_team_game_by_game", w = 14, h = 10)
}

chart_trend_bump <- function(k = 4) {
  labs3 <- c(wk_span(1, B3[1]), wk_span(B3[1] + 1, B3[2]), wk_span(B3[2] + 1, LASTW))
  thirds <- function(w) ifelse(w <= B3[1], labs3[1], ifelse(w <= B3[2], labs3[2], labs3[3]))
  d <- period_epa(data_rds("pbp"), thirds) |> group_by(period) |> mutate(rank = rank(-net, ties.method = "first")) |> ungroup() |>
    mutate(x = match(period, labs3))
  mv <- d |> select(team, x, rank) |> pivot_wider(names_from = x, values_from = rank, names_prefix = "r") |> mutate(mv = r1 - r3)
  risers <- mv |> slice_max(mv, n = k, with_ties = FALSE); fallers <- mv |> slice_min(mv, n = k, with_ties = FALSE)
  hl <- d |> filter(team %in% c(risers$team, fallers$team)) |>
    mutate(kind = ifelse(team %in% risers$team, "rise", "fall"),
           team = factor(team, levels = c(risers$team, fallers$team)))
  g <- ggplot(hl, aes(x, rank)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 8.5, fill = GREEN, alpha = 0.06) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 24.5, ymax = 32.5, fill = RED, alpha = 0.06) +
    geom_line(aes(color = kind), linewidth = 1.6) +
    geom_point(aes(color = kind), size = 7.5) +
    geom_text(aes(label = rank), color = "white", family = "RobotoNFL", fontface = "bold", size = 3.2) +
    facet_wrap(~team, nrow = 2) +
    scale_color_manual(values = c(rise = GREEN, fall = RED)) +
    scale_y_reverse(limits = c(33, 0), breaks = c(1, 8, 16, 24, 32), labels = function(v) ordinal(v)) +
    scale_x_continuous(breaks = 1:3, labels = sub("Weeks ", "Wk ", sub("Week ", "Wk ", labs3)), limits = c(0.7, 3.3)) +
    labs(
      title = paste0(and_list(nick(risers$team)), " climbed; ", and_list(nick(fallers$team)), " fell"),
      subtitle = paste0("Net EPA per play rank in three stretches of the season (1st = best): the ", k, " ", kw("biggest risers", GREEN),
                        " and ", kw("biggest fallers", RED)),
      x = NULL, y = "Rank", caption = caption(paste0("Shaded: top 8 and bottom 8. ", labs3[3], " is the shortest stretch"))
    ) +
    theme_nfl_axis(strip.text = element_nfl_logo(size = 1.1), panel.grid.major.x = element_blank(),
                   axis.text.x = element_text(size = 9.5, color = "#555555"), panel.spacing.x = unit(14, "pt"))
  save_chart(g, "T4_rank_bump_chart", w = 12.5, h = 8)
}

