# Receiver, runner and quarterback-distribution charts (headshots).

targets <- function() {
  data_rds("pbp") |>
    filter(pass_attempt == 1, sack == 0, !is.na(receiver_player_id), !is.na(posteam)) |>
    mutate(air_yards = coalesce(air_yards, 0))
}

# per player-game targets, with the team's totals in that game
player_games <- function(tg) {
  team_game <- tg |> group_by(posteam, game_id) |>
    summarise(team_tgt = n(), team_ay = sum(air_yards), .groups = "drop")
  tg |> group_by(receiver_player_id, posteam, game_id) |>
    summarise(tgt = n(), ay = sum(air_yards), .groups = "drop") |>
    left_join(team_game, by = c("posteam", "game_id"))
}

chart_target_share <- function(min_tgt = 40, n_heads = 22, n_labels = 22) {
  pl <- data_rds("players")
  tg <- targets()
  pbp_names <- tg |> count(receiver_player_id, receiver_player_name) |> group_by(id = receiver_player_id) |>
    slice_max(n, n = 1, with_ties = FALSE) |> ungroup() |> select(id, pbp_name = receiver_player_name)
  pg <- player_games(tg)
  d <- pg |> group_by(id = receiver_player_id) |>
    summarise(team = main_team(posteam), games = n_distinct(game_id), tgt = sum(tgt),
              ts = sum(tgt) / sum(team_tgt), ays = sum(ay) / sum(team_ay), .groups = "drop") |>
    filter(tgt >= min_tgt) |>
    mutate(wopr = 1.5 * ts + 0.7 * ays) |>
    left_join(pl |> select(id = gsis_id, name), by = "id") |>
    left_join(pbp_names, by = "id") |>
    mutate(label = ifelse(is.na(name), sub("^.*\\.", "", pbp_name), last_name(name)),
           rank = rank(-wopr, ties.method = "first"))
  heads <- d |> filter(rank <= n_heads)
  g <- ggplot(d, aes(ts, ays)) +
    geom_mean_lines(aes(x0 = ts, y0 = ays), color = "black", linetype = "dashed", alpha = 0.4, linewidth = 0.5) +
    geom_point(data = d |> filter(rank > n_heads), aes(color = team), size = 2.6, alpha = 0.75) +
    scale_color_nfl(type = "primary") +
    geom_nfl_headshots(data = heads, aes(player_gsis = id), height = 0.068) +
    geom_text_repel(data = d |> filter(rank <= n_labels), aes(label = label), size = 3, family = "RobotoNFL",
                    fontface = "bold", color = INK, point.size = 16, box.padding = 0.25, min.segment.length = 0.4,
                    segment.color = "#9a9a9a", segment.size = 0.3, max.overlaps = Inf, seed = 2026) +
    scale_x_continuous(labels = percent_format(accuracy = 1), breaks = pretty_breaks(7), expand = expansion(mult = 0.05)) +
    scale_y_continuous(labels = percent_format(accuracy = 1), breaks = pretty_breaks(7), expand = expansion(mult = 0.08)) +
    labs(
      title = "Who's the focal point of the passing game?",
      subtitle = paste0("Share of team targets vs share of team air yards, in games each player was targeted. Minimum ",
                        min_tgt, " targets.\nHeadshots: the top ", n_heads,
                        " by weighted opportunity (WOPR); dots are everyone else, in their team's color."),
      x = "Target share", y = "Air-yards share",
      caption = caption("WOPR = 1.5 x target share + 0.7 x air-yards share (Josh Hermsmeyer's weighted opportunity rating).")
    ) +
    theme_nfl()
  save_chart(g, "06_target_share_vs_air_yards", w = 13, h = 8.2)
}

chart_q1_target_share <- function(min_games = 5, min_q1 = 10, top = 15) {
  pl <- data_rds("players"); tm <- data_rds("teams")
  tg <- targets()
  pg <- player_games(tg)
  team_q1 <- tg |> filter(qtr == 1) |> count(posteam, game_id, name = "team_q1")
  q1p <- tg |> filter(qtr == 1) |> count(receiver_player_id, posteam, game_id, name = "q1")
  d <- pg |> select(receiver_player_id, posteam, game_id) |>
    left_join(q1p, by = c("receiver_player_id", "posteam", "game_id")) |>
    left_join(team_q1, by = c("posteam", "game_id")) |>
    mutate(q1 = coalesce(q1, 0L), team_q1 = coalesce(team_q1, 0L)) |>
    group_by(id = receiver_player_id) |>
    summarise(team = main_team(posteam), games = n(), q1 = sum(q1), share = sum(q1) / sum(team_q1), .groups = "drop") |>
    filter(games >= min_games, q1 >= min_q1) |>
    slice_max(share, n = top, with_ties = FALSE) |>
    left_join(pl |> select(id = gsis_id, name, position), by = "id") |>
    left_join(tm |> select(team = team_abbr, team_color, team_color2), by = "team") |>
    mutate(id = reorder(id, share),
           lab = paste0(name, "  ", percent(share, 1), "  (", q1, " targets, ", games, " games)"))
  g <- ggplot(d, aes(share, id)) +
    geom_col(aes(fill = team_color), width = 0.72) +
    geom_text(aes(x = 0.006, label = lab), hjust = 0, color = "white", family = "RobotoNFL", fontface = "bold", size = 3.6) +
    geom_nfl_logos(aes(x = share + 0.016, team_abbr = team), height = 0.05) +
    scale_fill_identity() +
    scale_x_continuous(labels = percent_format(accuracy = 1), breaks = pretty_breaks(6),
                       expand = expansion(mult = c(0, 0.08))) +
    labs(
      title = "Who gets fed early?",
      subtitle = paste0("Share of the team's 1st-quarter targets, in games each player was targeted. Top ", top,
                        "; minimum ", min_games, " games and ", min_q1, " 1st-quarter targets."),
      x = "Share of team 1st-quarter targets", y = NULL,
      caption = caption()
    ) +
    theme_nfl_axis(axis.text.y = element_nfl_headshot(size = 1.35), panel.grid.major.y = element_blank())
  save_chart(g, "07_q1_target_share_leaders", w = 12, h = 8)
}

chart_qb_air_yards_ridges <- function(top = 16) {
  pl <- data_rds("players"); tm <- data_rds("teams")
  pa <- data_rds("pbp") |> filter(pass_attempt == 1, sack == 0, !is.na(air_yards), !is.na(passer_player_id))
  keep <- pa |> count(passer_player_id, sort = TRUE) |> slice_head(n = top)
  d <- pa |> filter(passer_player_id %in% keep$passer_player_id) |>
    group_by(passer_player_id) |> mutate(team = main_team(posteam), adot = mean(air_yards)) |> ungroup() |>
    left_join(tm |> select(team = team_abbr, team_color), by = "team") |>
    mutate(passer_player_id = reorder(passer_player_id, adot))
  lab <- d |> distinct(passer_player_id, adot) |>
    left_join(pl |> select(passer_player_id = gsis_id, name), by = "passer_player_id")
  meansd <- function(x, ...) c(mean(x) - sd(x), mean(x), mean(x) + sd(x))
  g <- ggplot(d, aes(air_yards, passer_player_id)) +
    geom_density_ridges(aes(fill = team_color), quantile_lines = TRUE, quantile_fun = meansd, color = "white",
                        alpha = 0.9, scale = 1.5, rel_min_height = 0.005, linewidth = 0.4) +
    geom_text(data = lab, aes(x = 52, y = passer_player_id, label = paste0(last_name(name), "  ", number(adot, 0.1), " yds")),
              hjust = 1, vjust = -0.6, family = "RobotoNFL", fontface = "bold", size = 3.3, color = INK, inherit.aes = FALSE) +
    scale_fill_identity() +
    scale_x_continuous(limits = c(-12, 52), breaks = seq(-10, 50, 10), labels = function(x) paste0(x, " yds")) +
    scale_y_discrete(expand = expansion(add = c(0.3, 1.6))) +
    labs(
      title = "How far downfield does each quarterback throw?",
      subtitle = paste0("Air yards on every pass attempt for the ", top,
                        " quarterbacks with the most attempts, sorted by average depth (shown at right).\n",
                        "White lines mark the average and one standard deviation either side."),
      x = "Air yards (distance past the line of scrimmage)", y = NULL,
      caption = caption()
    ) +
    theme_nfl_axis(axis.text.y = element_nfl_headshot(size = 1.25), panel.grid.major.y = element_blank())
  save_chart(g, "08_qb_air_yards_ridges", w = 12, h = 9.5)
}

chart_ngs_ryoe <- function(min_att = 60) {
  tm <- data_rds("teams")
  d <- data_rds("ngs_rushing") |>
    filter(week %in% WEEKS, !is.na(rush_yards_over_expected)) |>
    mutate(team_abbr = clean_team_abbrs(team_abbr)) |>
    group_by(id = player_gsis_id) |>
    summarise(name = last(player_display_name), team = last(team_abbr), att = sum(rush_attempts),
              ryoe = sum(rush_yards_over_expected), .groups = "drop") |>
    filter(att >= min_att) |>
    mutate(per = ryoe / att) |>
    left_join(tm |> select(team = team_abbr, team_color), by = "team") |>
    mutate(id = reorder(id, per))
  g <- ggplot(d, aes(per, id)) +
    geom_col(aes(fill = team_color), width = 0.72) +
    geom_vline(xintercept = 0, color = INK, linewidth = 0.5) +
    geom_text(aes(x = ifelse(per >= 0, per + 0.04, per - 0.04), hjust = ifelse(per >= 0, 0, 1),
                  label = paste0(name, "  ", number(per, 0.01, style_positive = "plus"))),
              family = "RobotoNFL", fontface = "bold", size = 3.1, color = INK) +
    scale_fill_identity() +
    scale_x_continuous(labels = label_number(accuracy = 0.1, style_positive = "plus", suffix = " yds"),
                       breaks = pretty_breaks(7), expand = expansion(mult = 0.32)) +
    labs(
      title = "Who's creating yards on the ground?",
      subtitle = paste0("Rushing yards over expected per carry, from Next Gen Stats player tracking. Minimum ", min_att,
                        " carries.\nExpected yards account for blockers, defenders and their speed and position at handoff."),
      x = "Rushing yards over expected per carry", y = NULL,
      caption = caption("Rushing yards over expected: NFL Next Gen Stats, via nflverse.")
    ) +
    theme_nfl_axis(axis.text.y = element_nfl_headshot(size = 1.05), panel.grid.major.y = element_blank())
  save_chart(g, "13_rushing_yards_over_expected", w = 12, h = 10)
}
