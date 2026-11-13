# League patterns across seasons, from nflverse data: the kickoff rule's effect on field position, touchdown regression,
# and how much the first weeks predict. Titles are
# computed, so they restate whatever the data says.

yard_line <- function(own) ifelse(own <= 50, paste0("own ", round(own)), paste0("opponent's ", round(100 - own)))

kickoff_drives <- function(d) {
  ko <- d |> filter(kickoff_attempt == 1, !is.na(fixed_drive), !is.na(posteam)) |> distinct(game_id, season, posteam, fixed_drive)
  sc <- d |> filter(pass == 1 | rush == 1, !is.na(yardline_100)) |>
    semi_join(ko, by = c("game_id", "posteam", "fixed_drive"))
  sc |> group_by(game_id, season, posteam, fixed_drive) |>
    summarise(start = 100 - first(yardline_100), best = 100 - min(yardline_100), .groups = "drop") |>
    mutate(cross = best > 50)
}

chart_kickoff_rule <- function() {
  h <- data_rds("pbp_2018_2025") |> filter(season_type == "REG")
  cur <- data_rds("pbp") |> mutate(season = SEASON)
  kd <- bind_rows(kickoff_drives(h), kickoff_drives(cur))
  s <- kd |> group_by(season) |> summarise(start = mean(start), cross = mean(cross), n = n(), .groups = "drop")
  before <- s |> filter(season <= 2023); after <- s |> filter(season == max(season))
  long <- s |> pivot_longer(c(start, cross), names_to = "m") |>
    mutate(m = factor(ifelse(m == "start", "Average starting yard line after a kickoff", "Kickoff drives that cross midfield"),
                      levels = c("Average starting yard line after a kickoff", "Kickoff drives that cross midfield")),
           lab = ifelse(m == "Kickoff drives that cross midfield", pct(value), yard_line(value)),
           era = ifelse(season >= 2024, "after", "before")) |>
    arrange(m, season) |> group_by(m) |> mutate(line_label_pos(value)) |> ungroup()
  rules <- data.frame(x = c(2023.5, 2024.5))
  g <- ggplot(long, aes(season, value)) +
    annotate("rect", xmin = 2023.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#f2b84b", alpha = 0.12) +
    geom_vline(data = rules, aes(xintercept = x), linetype = "dashed", color = MUTED) +
    geom_line(color = INK, linewidth = 1) +
    geom_point(aes(color = era), size = 3.6) +
    geom_text(aes(label = lab, hjust = hj, vjust = vj), family = "RobotoNFL", fontface = "bold", size = 3.2) +
    facet_wrap(~m, scales = "free_y") +
    scale_color_manual(values = c(before = "#7a8794", after = "#d9822b")) +
    scale_x_continuous(breaks = 2018:SEASON, labels = function(x) ifelse(x == SEASON, paste0(x, "*"), x)) +
    scale_y_continuous(expand = expansion(mult = c(0.25, 0.18)), labels = NULL) +
    labs(
      title = paste0("The new kickoff gave every offense about ", round(after$start - mean(before$start)),
                     " free yards: drives now start at the ", round(after$start), ", not the ", round(mean(before$start))),
      subtitle = paste0("Drives that began with a kickoff, 2018-", SEASON, ". Dashed lines: the 2024 and 2025 kickoff rule changes. *", SEASON, " = Weeks ", min(WEEKS), "-", max(WEEKS)),
      x = NULL, y = NULL, caption = caption(span = paste0("2018-", SEASON), credit = NULL)
    ) +
    theme_nfl() + theme(panel.grid.major.x = element_blank(), strip.text = element_text(size = 12, hjust = 0))
  save_chart(g, "P1_kickoff_rule_field_position", w = 13, h = 6.8)
}

td_halves <- function(ffo) {
  ffo |> filter(position != "QB", !is.na(player_id)) |>
    mutate(td = coalesce(rec_touchdown, 0) + coalesce(rush_touchdown, 0),
           xtd = coalesce(rec_touchdown_exp, 0) + coalesce(rush_touchdown_exp, 0),
           half = ifelse(week <= 8, "h1", "h2")) |>
    group_by(season, player_id, half) |>
    summarise(g = n(), td = sum(td), xtd = sum(xtd), .groups = "drop") |>
    pivot_wider(names_from = half, values_from = c(g, td, xtd)) |>
    filter(g_h1 >= 5, g_h2 >= 5, xtd_h1 >= 1) |>
    mutate(over1 = (td_h1 - xtd_h1) / g_h1, over2 = (td_h2 - xtd_h2) / g_h2,
           x1 = xtd_h1 / g_h1, t1 = td_h1 / g_h1, t2 = td_h2 / g_h2)
}

chart_td_regression <- function() {
  d <- td_halves(data_rds("ff_opportunity") |> filter(season <= 2025, week <= 18))
  r_over <- cor(d$over1, d$over2); r_xtd <- cor(d$x1, d$t2); r_td <- cor(d$t1, d$t2)
  p1 <- paste0("Luck: touchdowns above expected, per game\nr = ", number(r_over, 0.01))
  p2 <- paste0("Opportunity: expected TDs, then actual TDs, per game\nr = ", number(r_xtd, 0.01))
  long <- bind_rows(d |> transmute(x = over1, y = over2, panel = p1), d |> transmute(x = x1, y = t2, panel = p2)) |>
    mutate(panel = factor(panel, levels = c(p1, p2)))
  g <- ggplot(long, aes(x, y)) +
    geom_hline(yintercept = 0, color = "#cfcfcf") + geom_vline(xintercept = 0, color = "#cfcfcf") +
    geom_point(alpha = 0.22, size = 1.6, color = "#3b5b7a") +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#d9822b", linewidth = 1.3) +
    facet_wrap(~panel, scales = "free") +
    scale_x_continuous(labels = function(v) number(v, 0.1)) + scale_y_continuous(labels = function(v) number(v, 0.1)) +
    labs(
      title = "Touchdown luck doesn't carry over. Touchdown opportunity does.",
      subtitle = paste0("Running backs, receivers and tight ends, 2018-2025: Weeks 1-8 vs Weeks 9-18 of the same season (", comma(nrow(d)), " player-seasons)"),
      x = "Weeks 1-8", y = "Weeks 9-18",
      caption = caption("Expected touchdowns: nflverse ff_opportunity model", span = "2018-2025", credit = NULL)) +
    theme_nfl() + theme(strip.text = element_text(size = 12, hjust = 0, lineheight = 1.1))
  save_chart(g, "P2_touchdown_regression", w = 13, h = 7)
}

td_outliers_2026 <- function(n = 8) {
  data_rds("ff_opportunity") |> filter(season == SEASON, week %in% WEEKS, position != "QB") |>
    group_by(gsis_id = player_id) |>
    summarise(team = last(posteam), name = last(full_name), td = sum(coalesce(rec_touchdown, 0) + coalesce(rush_touchdown, 0)),
              xtd = sum(coalesce(rec_touchdown_exp, 0) + coalesce(rush_touchdown_exp, 0)), .groups = "drop") |>
    filter(xtd >= 2) |> mutate(diff = td - xtd) |>
    arrange(desc(diff)) |> (\(x) bind_rows(head(x, n), tail(x, n)))()
}

chart_td_outliers_2026 <- function() {
  d <- td_outliers_2026() |> left_join(sprites(), by = "gsis_id") |>
    mutate(gsis_id = reorder(gsis_id, diff), side = ifelse(diff > 0, "over", "under"))
  top <- d |> slice_max(diff, n = 1); bot <- d |> slice_min(diff, n = 1)
  g <- ggplot(d, aes(diff, gsis_id)) +
    geom_col(aes(fill = side), width = 0.68) +
    geom_vline(xintercept = 0, color = INK, linewidth = 0.4) +
    geom_text(aes(x = ifelse(diff > 0, diff + 0.12, diff - 0.12), hjust = ifelse(diff > 0, 0, 1),
                  label = paste0(name, "  ", td, " TD on ", number(xtd, 0.1), " expected")),
              family = "RobotoNFL", fontface = "bold", size = 3.3) +
    ggpath::geom_from_path(aes(x = ifelse(diff > 0, -0.3, 0.3), path = circle), width = 0.03) +
    scale_fill_manual(values = c(over = "#d9822b", under = "#3b5b7a")) +
    scale_x_continuous(labels = function(v) signed(v, 0.1), breaks = pretty_breaks(8), expand = expansion(mult = 0.42)) +
    labs(
      title = paste0("Touchdowns are running ahead of opportunity for ", top$name, " (", signed(top$diff, 0.1), ") and behind it for ",
                     bot$name, " (", signed(bot$diff, 0.1), ")"),
      subtitle = "Touchdowns minus expected touchdowns, players with 2+ expected. Since 2018, gaps like these have faded",
      x = "Touchdowns above (or below) expected", y = NULL,
      caption = caption("Expected touchdowns: nflverse ff_opportunity model", credit = HEADS)) +
    theme_nfl() + theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
  save_chart(g, "P2b_touchdown_outliers_2026", w = 13, h = 8.2)
}

chart_early_weeks <- function() {
  h <- data_rds("pbp_2018_2025") |> filter(season_type == "REG", !is.na(epa), pass == 1 | rush == 1, !is.na(posteam))
  net <- function(x) {
    o <- x |> group_by(season, team = posteam) |> summarise(o = mean(epa), .groups = "drop")
    dd <- x |> group_by(season, team = defteam) |> summarise(d = mean(epa), .groups = "drop")
    inner_join(o, dd, by = c("season", "team")) |> transmute(season, team, net = o - d)
  }
  d <- inner_join(net(h |> filter(week <= 3)) |> rename(early = net), net(h |> filter(week >= 4)) |> rename(rest = net),
                  by = c("season", "team"))
  r <- cor(d$early, d$rest); fit <- coef(lm(rest ~ early, d))
  last_s <- max(d$season)
  hl <- d |> filter(season == last_s)
  mover <- hl |> mutate(drop = early - rest) |> slice_max(drop, n = 1)
  g <- ggplot(d, aes(early, rest)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = MUTED) +
    geom_point(data = d |> filter(season != last_s), color = "#9aa6b2", alpha = 0.45, size = 1.8) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#d9822b", linewidth = 1.2) +
    geom_nfl_logos(data = hl, aes(team_abbr = team), width = 0.04, alpha = 0.95) +
    annotate("label", x = 0.2, y = fit[1] + fit[2] * 0.2 - 0.09, label = paste0("a +0.20 start projects to ", signed(fit[1] + fit[2] * 0.2), " the rest of the way"),
             family = "RobotoNFL", fontface = "bold", size = 3.4, color = "#b5651d", fill = "white", hjust = 0) +
    annotate("text", x = max(d$rest) * 1.02, y = max(d$rest) * 1.02, label = "dotted: if Weeks 1-3 told the whole story", hjust = 1.05,
             vjust = 0, family = "RobotoNFL", size = 3.2, color = MUTED) +
    scale_x_continuous(labels = function(v) signed(v, 0.01)) + scale_y_continuous(labels = function(v) signed(v, 0.01)) +
    coord_cartesian(ylim = range(d$rest) * 1.08) +
    labs(
      title = paste0("Three weeks of football explain only ", pct(r^2), " of how a team plays the rest of the season"),
      subtitle = paste0("Net EPA per play, Weeks 1-3 vs Weeks 4-18, 2018-", last_s, " (", nrow(d), " team-seasons; logos = ", last_s, "), r = ", number(r, 0.01)),
      x = "Net EPA per play, Weeks 1-3", y = "Net EPA per play, Weeks 4-18",
      caption = caption(span = paste0("2018-", last_s))
    ) +
    theme_nfl()
  save_chart(g, "P3_early_weeks_vs_rest", w = 12.5, h = 8)
}

# P4: passing by wind, outdoor games 2018-2026, against indoor games. Wind is the listed game-time wind; 2026 games
# from Week 5 on have none listed yet and drop out.
chart_wind_passing <- function() {
  cols <- c("game_id", "season", "season_type", "qb_dropback", "pass_attempt", "sack", "air_yards", "epa")
  p <- bind_rows(data_rds("pbp_2018_2025")[, cols], data_rds("pbp")[, cols]) |>
    filter(season_type == "REG", qb_dropback == 1, !is.na(epa))
  levs <- c("Indoors", "0-5 mph", "6-10 mph", "11-15 mph", "16+ mph")
  d <- p |> left_join(data_rds("schedules_all") |> select(game_id, roof, wind), by = "game_id") |>
    mutate(bucket = case_when(
      roof %in% c("dome", "closed") ~ "Indoors",
      roof %in% c("outdoors", "open") & !is.na(wind) ~ as.character(cut(wind, c(-Inf, 5, 10, 15, Inf), labels = levs[-1])),
      TRUE ~ NA_character_)) |>
    filter(!is.na(bucket)) |> mutate(bucket = factor(bucket, levs))
  s <- d |> group_by(bucket) |>
    summarise(games = n_distinct(game_id),
              share = mean(air_yards[pass_attempt == 1 & sack == 0 & !is.na(air_yards)] >= 20),
              deep = mean(epa[pass_attempt == 1 & sack == 0 & !is.na(air_yards) & air_yards >= 20]),
              epa = mean(epa), .groups = "drop")
  calm <- s |> filter(bucket == "0-5 mph"); gust <- s |> filter(bucket == "16+ mph")
  title <- paste0("In 16+ mph wind, a throw 20+ yards downfield is worth ", number(gust$deep, 0.01), " EPA, ",
                  pct(1 - gust$deep / calm$deep), " less than in calm air (", number(calm$deep, 0.01), ")")
  m <- s |> pivot_longer(c(epa, deep), names_to = "measure") |>
    mutate(measure = factor(measure, c("epa", "deep"), c("EPA per dropback, all", "EPA per throw 20+ yards downfield")),
           lab = signed(value))
  ramp <- if (DARK) c("#6b7480", "#9cc3e6", "#6aa6dc", "#3f86cc", "#1f66b0") else c("#9aa3ad", "#b7d3ec", "#7fb0dc", "#3f86c4", "#1b5a9a")
  g <- ggplot(m, aes(bucket, value, fill = bucket)) +
    geom_col(width = 0.68) +
    geom_text(aes(label = lab, vjust = ifelse(value >= 0, -0.5, 1.4)), size = 3.6, family = "RobotoNFL", fontface = "bold",
              color = INK) +
    geom_hline(yintercept = 0, color = MUTED, linewidth = 0.4) +
    facet_wrap(~measure, scales = "free_y") +
    scale_fill_manual(values = setNames(ramp, levs)) +
    scale_x_discrete(labels = paste0(levs, "\n", number(s$games, big.mark = ","), " games")) +
    scale_y_continuous(expand = expansion(mult = c(0.12, 0.15)), labels = NULL) +
    labs(title = title,
         subtitle = paste0("Listed wind at kickoff, outdoor and open-roof games, against <b>indoor</b> games. Deep throws stay ",
                           pct(min(s$share)), "-", pct(max(s$share)), " of throws in every wind"),
         x = NULL, y = NULL,
         caption = caption(paste0(SEASON, " games from Week 5 on have no listed wind yet and are left out"),
                           span = paste0("2018-", SEASON, " regular seasons"), credit = NULL)) +
    theme_nfl_axis(panel.grid.major = element_blank(), strip.text = element_text(face = "bold", size = rel(1.1)))
  save_chart(g, "P4_wind_and_passing", w = 12, h = 6.75)
}
