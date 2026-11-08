# Quarterback charts (headshots).

last_name <- function(x) sub("^\\S+\\s+", "", x)

main_team <- function(x) names(sort(table(x), decreasing = TRUE))[1]

chart_qb_epa_cpoe <- function(min_db = 150, n_top = 6, n_bottom = 4) {
  pl <- data_rds("players")
  d <- data_rds("pbp") |>
    filter(qb_dropback == 1, !is.na(epa), !is.na(id)) |>
    group_by(id) |>
    summarise(dropbacks = n(), epa = mean(epa), cpoe = mean(cpoe, na.rm = TRUE),
              team = main_team(posteam), .groups = "drop") |>
    filter(dropbacks >= min_db) |>
    left_join(pl |> select(id = gsis_id, name), by = "id") |>
    mutate(label = last_name(name), r = rank(-epa))
  show <- d |> filter(r <= n_top | r > nrow(d) - n_bottom) |> left_join(sprites(), by = c("id" = "gsis_id"))
  top <- d |> arrange(r) |> slice_head(n = 3)
  W <- 0.075
  show <- cbind(show, spread_xy(show$cpoe, show$epa, rep(W, nrow(show)), range(d$cpoe), range(d$epa), aspect = 13 / 8.2))
  g <- ggplot(d, aes(cpoe, epa)) +
    geom_mean_lines(aes(x0 = cpoe, y0 = epa), color = "black", linetype = "dashed", alpha = 0.4, linewidth = 0.5) +
    geom_point(data = d |> filter(!id %in% show$id), color = GRAY, size = 3, alpha = 0.55) +
    geom_segment(data = show, aes(x = cpoe, y = epa, xend = xs, yend = ys), color = "#9a9a9a", linewidth = 0.4) +
    geom_point(data = show, aes(cpoe, epa), color = INK, size = 1.3) +
    ggpath::geom_from_path(data = show, aes(x = xs, y = ys, path = sprite), width = W) +
    scale_x_continuous(labels = function(v) paste0(signed(v, 1), "%"), breaks = pretty_breaks(7), expand = expansion(mult = 0.08)) +
    scale_y_continuous(labels = function(v) signed(v), breaks = pretty_breaks(7), expand = expansion(mult = 0.12)) +
    labs(
      title = paste0(paste(last_name(top$name[1:2]), collapse = ", "), " and ", last_name(top$name[3]),
                     " lead the league in EPA per dropback"),
      subtitle = paste0("EPA per dropback vs completion % over expected, ", min_db, "+ dropbacks. Headshots: the top ",
                        n_top, " and bottom ", n_bottom, "; dashed lines = averages"),
      x = "Completion % over expected (CPOE)", y = "EPA per dropback",
      caption = caption("EPA and CPOE: nflfastR models", credit = HEADS)
    ) +
    theme_nfl()
  save_chart(g, "04_qb_epa_vs_cpoe", w = 13, h = 8.2)
}

chart_qb_third_down <- function(min_att = 15) {
  pl <- data_rds("players")
  tm <- data_rds("teams")
  d <- data_rds("pbp") |>
    filter(down == 3, between(ydstogo, 5, 10), pass_attempt == 1, sack == 0,
           !is.na(air_yards), !is.na(passer_player_id)) |>
    group_by(passer_player_id) |>
    summarise(att = n(), agg = mean(air_yards >= ydstogo), team = main_team(posteam), .groups = "drop") |>
    filter(att >= min_att) |>
    left_join(pl |> select(passer_player_id = gsis_id, name), by = "passer_player_id") |>
    left_join(tm |> select(team = team_abbr, team_color, team_color2), by = "team") |>
    mutate(label = last_name(name), passer_player_id = reorder(passer_player_id, -agg))
  avg <- mean(d$agg)
  g <- ggplot(d, aes(passer_player_id, agg)) +
    geom_col(aes(fill = team_color, color = team_color2), width = 0.78, linewidth = 0.7) +
    geom_hline(yintercept = avg, linetype = "dashed", color = INK, alpha = 0.6, linewidth = 0.5) +
    with_outer_glow(
      geom_text(aes(y = agg - 0.015, label = label), angle = 90, hjust = 1, color = "white",
                family = "RobotoNFL", fontface = "bold", size = 3.3),
      colour = "black", sigma = 4, expand = 1.2
    ) +
    annotate("text", x = Inf, y = avg + 0.012, label = paste0("Average ", percent(avg, 1)),
             hjust = 1, vjust = 0, family = "RobotoNFL", size = 3.4, color = MUTED) +
    scale_fill_identity() + scale_color_identity() +
    scale_y_continuous(labels = percent_format(accuracy = 1), breaks = pretty_breaks(6), expand = c(0, 0),
                       limits = c(0, max(d$agg) * 1.06)) +
    labs(
      title = "Who throws past the sticks on 3rd down?",
      subtitle = paste0("Share of 3rd-and-5-to-10 throws at or past the first-down marker, ", min_att, "+ attempts, sacks excluded"),
      x = NULL, y = "Thrown at or past the sticks",
      caption = caption()) +
    theme_nfl_axis(axis.text.x = element_nfl_headshot(size = 0.95), panel.grid.major.x = element_blank())
  save_chart(g, "05_qb_third_down_aggressiveness", w = 13, h = 7.2)
}
