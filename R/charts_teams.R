# Team-level charts (logos). Each function builds one chart from data/ and saves it to out/.

plays_epa <- function(pbp) pbp |> filter(!is.na(epa), !is.na(posteam), pass == 1 | rush == 1)

team_epa <- function() {
  p <- plays_epa(data_rds("pbp"))
  off <- p |> group_by(team = posteam) |> summarise(off_epa = mean(epa), .groups = "drop")
  def <- p |> group_by(team = defteam) |> summarise(def_epa = mean(epa), .groups = "drop")
  inner_join(off, def, by = "team") |> mutate(net = off_epa - def_epa)
}

chart_epa_landscape <- function() {
  d <- team_epa()
  px <- diff(range(d$off_epa)) * 0.10
  py <- diff(range(d$def_epa)) * 0.13
  x0 <- min(d$off_epa) - px; x1 <- max(d$off_epa) + px
  y0 <- min(d$def_epa) - py; y1 <- max(d$def_epa) + py
  quad <- tibble(
    x = c(x1, x0, x1, x0), y = c(y0, y0, y1, y1),
    hjust = c(1, 0, 1, 0), vjust = c(1, 1, 0, 0),
    label = c("Good offense, good defense", "Good defense, struggling offense",
              "Good offense, leaky defense", "Struggling on both sides"),
    col = c("#1a7f4b", MUTED, MUTED, "#b3261e")
  )
  g <- ggplot(d, aes(off_epa, def_epa)) +
    geom_mean_lines(aes(x0 = off_epa, y0 = def_epa), color = "black", linetype = "dashed", alpha = 0.45, linewidth = 0.5) +
    geom_nfl_logos(aes(team_abbr = team), width = 0.05, alpha = 0.95) +
    geom_text(data = quad, aes(x, y, label = label, hjust = hjust, vjust = vjust, color = col),
              family = "RobotoNFL", fontface = "bold", size = 4.2, inherit.aes = FALSE) +
    scale_color_identity() +
    scale_x_continuous(labels = label_number(accuracy = 0.01, style_positive = "plus"), breaks = pretty_breaks(6),
                       expand = expansion(mult = 0.02)) +
    scale_y_reverse(labels = label_number(accuracy = 0.01, style_positive = "plus"), breaks = pretty_breaks(6),
                    expand = expansion(mult = 0.02)) +
    labs(
      title = "Who's good on both sides of the ball?",
      subtitle = paste0("Expected points added (EPA) per play on offense and allowed on defense, passes and runs. ",
                        "Up and to the right is good.\nDashed lines mark the league average."),
      x = "Offense: EPA per play (higher is better)",
      y = "Defense: EPA per play allowed (lower is better)",
      caption = caption("EPA measures how much each play changes a team's expected points, from the nflfastR model.")
    ) +
    theme_nfl()
  save_chart(g, "01_team_epa_landscape")
}

chart_team_tiers <- function() {
  d <- team_epa() |> arrange(desc(net))
  set.seed(2026)
  km <- kmeans(d$net, centers = 5, nstart = 200)
  ord <- order(km$centers, decreasing = TRUE)
  d <- d |>
    mutate(tier_no = match(km$cluster, ord)) |>
    group_by(tier_no) |> mutate(tier_rank = row_number()) |> ungroup()
  g <- nfl_team_tiers(
    d |> transmute(tier_no, team_abbr = team, tier_rank),
    title = "2026 team tiers after eight weeks",
    subtitle = paste0("Net EPA per play (offense minus defense allowed), grouped with k-means. ",
                      "Best first within each tier."),
    caption = caption(),
    tier_desc = c("1" = "Top tier", "2" = "Above average", "3" = "Average", "4" = "Below average", "5" = "Bottom tier"),
    width = 0.072
  ) +
    theme(
      text = element_text(family = "RobotoNFL", color = INK),
      plot.title = element_text(family = "RobotoNFLBlack", size = 22, hjust = 0, color = INK),
      plot.subtitle = element_text(size = 13, color = MUTED, hjust = 0),
      plot.caption = element_text(size = 9, color = "#6b6b6b", hjust = 0, lineheight = 1.2),
      axis.text.y = element_text(family = "RobotoNFL", face = "bold", size = 13, color = INK),
      plot.background = element_rect(fill = BG, color = NA),
      panel.background = element_rect(fill = BG, color = NA)
    )
  save_chart(g, "02_team_tiers", w = 12, h = 7.5)
}

chart_proe <- function() {
  d <- data_rds("pbp") |>
    filter(down %in% 1:2, !is.na(pass_oe), !is.na(posteam), between(wp, 0.2, 0.8), half_seconds_remaining > 120) |>
    group_by(team = posteam) |> summarise(proe = mean(pass_oe), plays = n(), .groups = "drop") |>
    mutate(team = reorder(team, proe), off = ifelse(proe >= 0, 1, -1) * diff(range(proe)) * 0.045)
  g <- ggplot(d, aes(proe, team)) +
    geom_col(aes(fill = team), width = 0.66) +
    geom_vline(xintercept = 0, color = INK, linewidth = 0.5) +
    geom_nfl_logos(aes(x = proe + off, team_abbr = team), height = 0.048) +
    scale_fill_nfl(alpha = 0.9) +
    scale_x_continuous(labels = label_number(accuracy = 1, suffix = "%", style_positive = "plus"),
                       breaks = pretty_breaks(8), expand = expansion(mult = 0.1)) +
    labs(
      title = "Who throws more than the situation calls for?",
      subtitle = paste0("Early-down pass rate over expected: how often a team drops back to pass compared with the nflfastR ",
                        "expected-pass model.\n1st and 2nd down, win probability 20-80%, outside the last two minutes of each half."),
      x = "Dropback rate over expected (percentage points)", y = NULL,
      caption = caption("Expected-pass model: nflfastR xpass, from down, distance, field position, score, time and win probability.")
    ) +
    theme_nfl() +
    theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
  save_chart(g, "03_early_down_proe", w = 12, h = 8.5)
}
