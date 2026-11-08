# A gt leaderboard with headshots, logos, color scales and weekly sparklines. Writes HTML; scripts/shoot.py
# turns it into a PNG.

chart_qb_table <- function(top = 20) {
  pl <- data_rds("players")
  db <- data_rds("pbp") |> filter(qb_dropback == 1, !is.na(epa), !is.na(id))
  att <- data_rds("pbp") |> filter(pass_attempt == 1, sack == 0, !is.na(air_yards), !is.na(passer_player_id)) |>
    group_by(id = passer_player_id) |> summarise(adot = mean(air_yards), .groups = "drop")
  weekly <- db |> group_by(id, week) |> summarise(e = mean(epa), .groups = "drop") |>
    arrange(week) |> group_by(id) |> summarise(trend = list(e), .groups = "drop")
  d <- db |> group_by(id) |>
    summarise(team = main_team(posteam), dropbacks = n(), epa = mean(epa), success = mean(success, na.rm = TRUE),
              cpoe = mean(cpoe, na.rm = TRUE), sack = mean(sack == 1, na.rm = TRUE), .groups = "drop") |>
    slice_max(dropbacks, n = top, with_ties = FALSE) |>
    left_join(att, by = "id") |> left_join(weekly, by = "id") |>
    left_join(pl |> select(id = gsis_id, name), by = "id") |>
    arrange(desc(epa)) |>
    mutate(rank = row_number(), headshot = id) |>
    select(rank, headshot, name, team, dropbacks, epa, success, cpoe, adot, sack, trend)
  tbl <- gt::gt(d) |>
    gt::tab_header(
      title = gt::md("**Quarterback efficiency board**"),
      subtitle = paste0("The ", top, " quarterbacks with the most dropbacks, ranked by EPA per dropback. ", SPAN, ".")
    ) |>
    nflplotR::gt_nfl_headshots(columns = "headshot", height = 34) |>
    nflplotR::gt_nfl_logos(columns = "team", height = 26) |>
    gt::fmt_number(columns = c("epa"), decimals = 2, force_sign = TRUE) |>
    gt::fmt_percent(columns = c("success", "sack"), decimals = 0) |>
    gt::fmt_number(columns = c("cpoe"), decimals = 1, force_sign = TRUE, pattern = "{x}%") |>
    gt::fmt_number(columns = c("adot"), decimals = 1) |>
    gt::data_color(columns = c("epa", "cpoe", "success"), method = "numeric",
                   palette = c("#c0392b", "#f7f7f7", "#1a7f4b")) |>
    gt::data_color(columns = "sack", method = "numeric", palette = c("#1a7f4b", "#f7f7f7", "#c0392b")) |>
    gtExtras::gt_plt_sparkline(trend, type = "ref_median", same_limit = TRUE, label = FALSE,
                               palette = c("#1b1b1b", "#1b1b1b", "#c0392b", "#1a7f4b", "#9a9a9a")) |>
    gt::cols_label(rank = "", headshot = "", name = "Quarterback", team = "Team", dropbacks = "Dropbacks",
                   epa = "EPA / dropback", success = "Success rate", cpoe = "CPOE", adot = "Avg depth",
                   sack = "Sack rate", trend = "Week by week") |>
    gt::cols_align("center", columns = -c("name")) |>
    gt::tab_source_note(gt::md(paste0(
      "EPA and success rate from the nflfastR models; CPOE = completion percentage over expected; avg depth = air yards per attempt. ",
      "Week by week: EPA per dropback in each game (red dot = low, green dot = high, line = median).<br>",
      "Data: nflverse via nflreadr. Logos and headshots via nflplotR. Table built with gt and gtExtras."))) |>
    gt::opt_table_font(font = list(gt::google_font("Roboto"), "Helvetica", "Arial")) |>
    gt::tab_options(
      table.background.color = BG, heading.align = "left", heading.title.font.size = 26,
      heading.subtitle.font.size = 14, column_labels.font.weight = "bold", column_labels.font.size = 13,
      table.font.size = 15, data_row.padding = 3, source_notes.font.size = 11,
      table.border.top.color = BG, column_labels.border.bottom.color = INK, table_body.hlines.color = "#e2e2e2"
    )
  path <- file.path(OUT, "14_qb_efficiency_board.html")
  gt::gtsave(tbl, path)
  message("wrote ", path)
}
