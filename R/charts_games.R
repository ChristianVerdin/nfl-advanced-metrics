# Drive, week, game-flow, scheme and scoring charts (logos).

chart_explosive_td_drives <- function() {
  tm <- data_rds("teams")
  d <- data_rds("pbp") |>
    filter(!is.na(fixed_drive), !is.na(posteam), pass == 1 | rush == 1) |>
    group_by(game_id, posteam, fixed_drive) |>
    summarise(result = first(fixed_drive_result), big = max(yards_gained, na.rm = TRUE), .groups = "drop") |>
    filter(result == "Touchdown") |>
    group_by(team = posteam) |>
    summarise(drives = n(), big = sum(big >= 20), share = big / drives, .groups = "drop") |>
    left_join(tm |> select(team = team_abbr, team_color), by = "team") |>
    mutate(team = reorder(team, share))
  avg <- sum(d$big) / sum(d$drives)
  g <- ggplot(d, aes(share, team)) +
    geom_col(aes(fill = team_color), width = 0.7, alpha = 0.92) +
    geom_vline(xintercept = avg, linetype = "dashed", color = INK, alpha = 0.6) +
    geom_text(aes(x = share - 0.01, label = paste0(big, " of ", drives)), hjust = 1, color = "white",
              family = "RobotoNFL", fontface = "bold", size = 3.1) +
    annotate("text", x = avg + 0.008, y = 1.2, label = paste0("League ", percent(avg, 1)), hjust = 0,
             family = "RobotoNFL", size = 3.4, color = MUTED) +
    scale_fill_identity() +
    scale_x_continuous(labels = percent_format(accuracy = 1), breaks = pretty_breaks(6), expand = expansion(mult = c(0, 0.04))) +
    labs(
      title = "Who needs a big play to score?",
      subtitle = "Share of each team's touchdown drives that included a gain of 20+ yards (counts on the bars)",
      x = "Touchdown drives with a 20+ yard play", y = NULL,
      caption = caption()) +
    theme_nfl_axis(axis.text.y = element_nfl_logo(size = 0.8), panel.grid.major.y = element_blank())
  save_chart(g, "09_big_play_td_drives", w = 11, h = 11)
}

chart_division_weekly <- function(division = "NFC North") {
  tm <- data_rds("teams")
  div <- tm |> filter(team_division == division)
  wk <- plays_epa(data_rds("pbp")) |>
    filter(posteam %in% div$team_abbr) |>
    group_by(team = posteam, week) |> summarise(epa_sum = sum(epa), n = n(), .groups = "drop") |>
    arrange(team, week) |> group_by(team) |>
    mutate(epa = cumsum(epa_sum) / cumsum(n)) |> ungroup()
  # each team's most readable color: the primary unless it is near-black, then the secondary
  dark <- function(hex) { v <- col2rgb(hex); sum(v * c(0.299, 0.587, 0.114)) < 60 }
  cols <- div |> mutate(col = ifelse(vapply(team_color, dark, logical(1)), team_color2, team_color))
  cols <- setNames(cols$col, cols$team_abbr)
  last_pt <- wk |> filter(!is.na(epa)) |> group_by(team) |> slice_max(week, n = 1) |> ungroup() |> arrange(epa) |> mutate(ly = epa)
  gap <- diff(range(wk$epa, na.rm = TRUE)) * 0.11   # end logos at least a logo's height apart, in rank order
  for (i in seq_len(nrow(last_pt))[-1]) last_pt$ly[i] <- max(last_pt$ly[i], last_pt$ly[i - 1] + gap)
  g <- ggplot(wk, aes(week, epa, color = team, group = team)) +
    geom_hline(yintercept = 0, color = INK, linewidth = 0.4) +
    geom_line(linewidth = 1.6, na.rm = TRUE) +
    geom_point(size = 4.2, color = BG, na.rm = TRUE) +
    geom_point(size = 2.6, na.rm = TRUE) +
    geom_segment(data = last_pt, aes(x = week + 0.08, xend = week + 0.2, y = epa, yend = ly), linewidth = 0.5) +
    geom_nfl_logos(data = last_pt, mapping = aes(x = week + 0.42, y = ly, team_abbr = team), width = 0.05,
                   inherit.aes = FALSE) +
    scale_color_manual(values = cols) +
    scale_x_continuous(breaks = WEEKS, labels = paste("Wk", WEEKS), limits = c(min(WEEKS) - 0.2, max(WEEKS) + 0.6)) +
    scale_y_continuous(labels = label_number(accuracy = 0.01, style_positive = "plus"), breaks = pretty_breaks(7)) +
    labs(
      title = paste0("How each ", division, " offense has trended"),
      subtitle = "Season-to-date offensive EPA per play after each game; a skipped week is a bye",
      x = NULL, y = "EPA per play, season to date",
      caption = caption()
    ) +
    theme_nfl()
  save_chart(g, paste0("10_", tolower(gsub(" ", "_", division)), "_weekly_epa"))
}

chart_comeback <- function() {
  tm <- data_rds("teams"); sc <- data_rds("schedules")
  p <- data_rds("pbp") |> filter(!is.na(home_wp), !is.na(game_seconds_remaining))
  res <- sc |> transmute(game_id, final_margin = result)
  mins <- p |> select(game_id, home_wp) |> left_join(res, by = "game_id") |> filter(final_margin != 0) |>
    mutate(win_wp = ifelse(final_margin > 0, home_wp, 1 - home_wp)) |>
    group_by(game_id) |> summarise(min_wp = min(win_wp), .groups = "drop") |> arrange(min_wp)
  gid <- mins$game_id[1]
  g1 <- p |> filter(game_id == gid) |>
    mutate(t = ifelse(qtr <= 4, (3600 - game_seconds_remaining) / 60, 60 + (600 - quarter_seconds_remaining) / 60)) |>
    arrange(t)
  info <- sc |> filter(game_id == gid)
  home <- info$home_team; away <- info$away_team
  hc <- tm$team_color[tm$team_abbr == home]; ac <- tm$team_color[tm$team_abbr == away]
  winner <- ifelse(info$result > 0, home, away)
  low <- g1 |> mutate(win_wp = if (winner == home) home_wp else 1 - home_wp) |> slice_min(win_wp, n = 1, with_ties = FALSE)
  tmax <- max(60, ceiling(max(g1$t)))
  nick <- function(a) tm$team_nick[tm$team_abbr == a]
  g <- ggplot(g1, aes(t, home_wp)) +
    geom_ribbon(aes(ymin = 0.5, ymax = pmax(home_wp, 0.5)), fill = hc, alpha = 0.85) +
    geom_ribbon(aes(ymin = pmin(home_wp, 0.5), ymax = 0.5), fill = ac, alpha = 0.85) +
    geom_line(linewidth = 0.5, color = INK) +
    geom_hline(yintercept = 0.5, color = INK, linewidth = 0.4) +
    geom_vline(xintercept = c(15, 30, 45, if (tmax > 60) 60), color = "#bdbdbd", linetype = "dotted") +
    annotate("point", x = low$t, y = low$home_wp, size = 3.5, color = INK) +
    annotate("label", x = low$t, y = ifelse(low$home_wp > 0.5, low$home_wp - 0.14, low$home_wp + 0.14),
             label = paste0(nick(winner), " win probability: ", percent(low$win_wp, 0.1), "\nQ", low$qtr, " ",
                            low$time),
             family = "RobotoNFL", size = 3.4, fill = "white") +
    geom_nfl_logos(data = data.frame(x = c(2.2, 2.2), y = c(0.9, 0.1), team = c(home, away)),
                   aes(x = x, y = y, team_abbr = team), width = 0.055, inherit.aes = FALSE) +
    scale_x_continuous(breaks = c(0, 15, 30, 45, 60, if (tmax > 60) tmax), labels = c("Kickoff", "Q2", "Half", "Q4", "End",
                       if (tmax > 60) "OT"), limits = c(0, tmax), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1), expand = expansion(add = 0.03),
                       labels = c(paste(away, "100%"), paste(away, "75%"), "50/50", paste(home, "75%"), paste(home, "100%"))) +
    labs(
      title = paste0("The comeback of the season so far: ", nick(winner), " ",
                     max(info$home_score, info$away_score), ", ",
                     ifelse(winner == home, nick(away), nick(home)), " ", min(info$home_score, info$away_score)),
      subtitle = paste0(away, " at ", home, ", Week ", info$week, ": ", kw(nick(home), hc), " chances above the line, ", kw(nick(away), ac), " below"),
      x = NULL, y = "Win probability",
      caption = caption("Win probability: nflfastR model")) +
    theme_nfl()
  save_chart(g, "11_comeback_win_probability")
}

chart_play_action <- function() {
  f <- data_rds("ftn") |> transmute(game_id = nflverse_game_id, play_id = as.numeric(nflverse_play_id), is_play_action)
  db <- data_rds("pbp") |> filter(qb_dropback == 1, !is.na(epa), !is.na(posteam)) |>
    mutate(play_id = as.numeric(play_id)) |>
    inner_join(f, by = c("game_id", "play_id"))
  d <- db |> group_by(team = posteam) |>
    summarise(pa_rate = mean(is_play_action), epa_pa = mean(epa[is_play_action]), epa_no = mean(epa[!is_play_action]),
              n_pa = sum(is_play_action), .groups = "drop")
  g <- ggplot(d, aes(pa_rate, epa_pa)) +
    geom_mean_lines(aes(x0 = pa_rate, y0 = epa_pa), color = "black", linetype = "dashed", alpha = 0.45, linewidth = 0.5) +
    geom_nfl_logos(aes(team_abbr = team), width = 0.052, alpha = 0.95) +
    scale_x_continuous(labels = percent_format(accuracy = 1), breaks = pretty_breaks(7), expand = expansion(mult = 0.06)) +
    scale_y_continuous(labels = label_number(accuracy = 0.01, style_positive = "plus"), breaks = pretty_breaks(7),
                       expand = expansion(mult = 0.08)) +
    labs(
      title = "Who leans on play action, and does it work?",
      subtitle = "Play-action rate vs EPA per play-action dropback; dashed lines = league averages",
      x = "Play-action rate (share of dropbacks)", y = "EPA per play-action dropback",
      caption = caption("Play-action charting: FTN Data via nflverse (CC BY-SA 4.0)")) +
    theme_nfl()
  save_chart(g, "12_play_action_rate_vs_epa")
}

quarter_points <- function() {
  sc <- data_rds("schedules")
  q <- data_rds("pbp") |>
    select(game_id, qtr, home_team, away_team, total_home_score, total_away_score) |>
    group_by(game_id) |>
    mutate(h = cummax(coalesce(total_home_score, 0)), a = cummax(coalesce(total_away_score, 0))) |>
    group_by(game_id, home_team, away_team, qtr) |> summarise(h = last(h), a = last(a), .groups = "drop") |>
    group_by(game_id) |> arrange(qtr, .by_group = TRUE) |>
    mutate(hp = h - lag(h, default = 0), ap = a - lag(a, default = 0)) |> ungroup()
  check <- q |> group_by(game_id) |> summarise(h = sum(hp), a = sum(ap)) |>
    inner_join(sc |> select(game_id, home_score, away_score), by = "game_id")
  stopifnot(all(check$h == check$home_score), all(check$a == check$away_score))
  bind_rows(q |> transmute(team = home_team, game_id, qtr, pts = hp), q |> transmute(team = away_team, game_id, qtr, pts = ap))
}

chart_quarter_heatmap <- function() {
  qp <- quarter_points()
  games <- qp |> distinct(team, game_id) |> count(team, name = "g")
  d <- qp |> filter(qtr <= 4) |> group_by(team, qtr) |> summarise(pts = sum(pts), .groups = "drop") |>
    left_join(games, by = "team") |> mutate(ppg = pts / g, q = paste0("Q", qtr))
  ord <- d |> filter(qtr == 1) |> arrange(ppg) |> pull(team)
  d <- d |> mutate(team = factor(team, levels = ord))
  g <- ggplot(d, aes(q, team, fill = ppg)) +
    geom_tile(color = BG, linewidth = 1.2) +
    geom_text(aes(label = number(ppg, 0.1), color = ppg > quantile(ppg, 0.7)), family = "RobotoNFL", fontface = "bold", size = 3.5) +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = INK), guide = "none") +
    scale_fill_gradient(low = "#eef3f8", high = "#0b4f8a", guide = "none") +
    scale_x_discrete(position = "top") +
    labs(
      title = "When does each team score?",
      subtitle = "Points per game in each quarter, sorted by 1st-quarter scoring (overtime left out)",
      x = NULL, y = NULL, caption = caption()
    ) +
    theme_nfl_axis(axis.text.y = element_nfl_logo(size = 0.95), panel.grid.major = element_blank(),
                   axis.text.x.top = element_text(face = "bold", size = 13))
  save_chart(g, "15_quarter_scoring_heatmap", w = 8.5, h = 12)
}
