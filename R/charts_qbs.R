# Quarterback charts (headshots).

last_name <- function(x) sub("^\\S+\\s+", "", x)

main_team <- function(x) names(sort(table(x), decreasing = TRUE))[1]

chart_qb_epa_cpoe <- function(min_db = 150) {
  pl <- data_rds("players")
  d <- data_rds("pbp") |>
    filter(qb_dropback == 1, !is.na(epa), !is.na(id)) |>
    group_by(id) |>
    summarise(dropbacks = n(), epa = mean(epa), cpoe = mean(cpoe, na.rm = TRUE),
              team = main_team(posteam), .groups = "drop") |>
    filter(dropbacks >= min_db) |>
    left_join(pl |> select(id = gsis_id, name), by = "id") |>
    mutate(label = last_name(name))
  g <- ggplot(d, aes(cpoe, epa)) +
    geom_mean_lines(aes(x0 = cpoe, y0 = epa), color = "black", linetype = "dashed", alpha = 0.45, linewidth = 0.5) +
    geom_nfl_headshots(aes(player_gsis = id), height = 0.08) +
    geom_text_repel(aes(label = label), size = 3, family = "RobotoNFL", fontface = "bold", color = INK,
                    point.size = 17, box.padding = 0.25, min.segment.length = 0.4, segment.color = "#9a9a9a",
                    segment.size = 0.3, max.overlaps = Inf, seed = 2026) +
    scale_x_continuous(labels = label_number(accuracy = 1, suffix = "%", style_positive = "plus"),
                       breaks = pretty_breaks(7), expand = expansion(mult = 0.07)) +
    scale_y_continuous(labels = label_number(accuracy = 0.01, style_positive = "plus"),
                       breaks = pretty_breaks(7), expand = expansion(mult = 0.1)) +
    labs(
      title = "Efficient and accurate: the 2026 quarterback landscape",
      subtitle = paste0("EPA per dropback vs completion percentage over expected (CPOE). Minimum ", min_db,
                        " dropbacks; dropbacks include sacks and scrambles.\nDashed lines mark the averages of the ",
                        nrow(d), " quarterbacks shown."),
      x = "CPOE: completion percentage over expected", y = "EPA per dropback",
      caption = caption("CPOE compares each completion with the nflfastR expected-completion model.")
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
      subtitle = paste0("Share of 3rd-and-5-to-10 pass attempts thrown at or beyond the first-down marker. Minimum ",
                        min_att, " attempts."),
      x = NULL, y = "Thrown at or past the sticks",
      caption = caption("Air yards = how far past the line of scrimmage the ball was thrown; sacks excluded.")
    ) +
    theme_nfl_axis(axis.text.x = element_nfl_headshot(size = 0.95), panel.grid.major.x = element_blank())
  save_chart(g, "05_qb_third_down_aggressiveness", w = 13, h = 7.2)
}
