# Record vs performance, opponent adjustment and the road ahead (16-18).


team_records <- function() {
  s <- data_rds("schedules")
  bind_rows(s |> transmute(team = home_team, pf = home_score, pa = away_score),
            s |> transmute(team = away_team, pf = away_score, pa = home_score)) |>
    group_by(team) |>
    summarise(g = n(), w = sum(pf > pa), l = sum(pf < pa), t = sum(pf == pa), pf = sum(pf), pa = sum(pa), .groups = "drop") |>
    mutate(wins = w + t / 2, rec = paste0(w, "-", l, ifelse(t > 0, paste0("-", t), "")))
}

chart_pythagorean <- function(expo = 2.37) {
  tm <- data_rds("teams")
  d <- team_records() |>
    mutate(xw = g * pf^expo / (pf^expo + pa^expo), diff = wins - xw) |>
    left_join(tm |> select(team = team_abbr, team_nick), by = "team") |>
    arrange(diff) |> mutate(team = factor(team, team), ahead = diff >= 0)
  hi <- d |> slice_max(diff, n = 1, with_ties = FALSE); lo <- d |> slice_min(diff, n = 1, with_ties = FALSE)
  g <- ggplot(d, aes(y = team)) +
    geom_segment(aes(x = xw, xend = wins, yend = team, color = ahead), linewidth = 1.6, lineend = "round") +
    geom_point(aes(x = xw), shape = 21, fill = BG, color = INK, size = 2.6, stroke = 0.8) +
    geom_nfl_logos(aes(x = wins, team_abbr = team), height = 0.034) +
    geom_text(aes(x = pmax(wins, xw) + 0.28, label = paste0(rec, "   ", signed(diff, 0.1))), hjust = 0, size = 3.1,
              family = "RobotoNFL", color = INK) +
    scale_color_manual(values = c(`TRUE` = GREEN, `FALSE` = RED)) +
    scale_x_continuous(breaks = 0:max(d$g), expand = expansion(mult = c(0.03, 0.1))) +
    labs(
      title = paste0("The ", hi$team_nick, " have ", number(hi$diff, 0.1), " more wins than their points say; the ",
                     lo$team_nick, " have ", number(-lo$diff, 0.1), " fewer"),
      subtitle = paste0("Open dot: wins expected from points scored and allowed. Logo: actual wins. ",
                        kw("Ahead of their points", GREEN), " or ", kw("behind", RED)),
      x = "Wins", y = NULL,
      caption = caption(paste0("Expected wins = games x PF^", expo, " / (PF^", expo, " + PA^", expo, "); a tie counts as half a win"))) +
    theme_nfl_axis(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
  save_chart(g, "16_wins_vs_point_differential", w = 12, h = 10)
}

# Opponent-adjusted EPA per play: one lightly penalised (ridge) regression over every team-game, EPA per play = league + offense +
# opponent's defense + home field, weighted by plays. Returns raw and adjusted offense, defense and net per team.
adjusted_epa <- function(lambda = 0.1) {
  p <- plays_epa(data_rds("pbp"))
  gm <- p |> group_by(game_id, off = posteam, def = defteam, home = as.integer(posteam == home_team)) |>
    summarise(epa = mean(epa), n = n(), .groups = "drop")
  tms <- sort(unique(gm$off)); k <- length(tms)
  X <- cbind(1, gm$home - mean(gm$home), outer(gm$off, tms, "==") * 1, outer(gm$def, tms, "==") * 1)
  pen <- diag(c(0, 0, rep(lambda, 2 * k))) * mean(gm$n)
  b <- solve(t(X) %*% (X * gm$n) + pen, t(X) %*% (gm$n * gm$epa))
  raw <- inner_join(p |> group_by(team = posteam) |> summarise(off_raw = mean(epa), .groups = "drop"),
                    p |> group_by(team = defteam) |> summarise(def_raw = mean(epa), .groups = "drop"), by = "team")
  tibble(team = tms, off_adj = b[1] + b[3:(k + 2)], def_adj = b[1] + b[(k + 3):(2 * k + 2)]) |>
    inner_join(raw, by = "team") |>
    mutate(net_raw = off_raw - def_raw, net_adj = off_adj - def_adj,
           rank_raw = rank(-net_raw, ties.method = "first"), rank_adj = rank(-net_adj, ties.method = "first"))
}

chart_opponent_adjusted <- function() {
  tm <- data_rds("teams")
  d <- adjusted_epa() |> left_join(tm |> select(team = team_abbr, team_nick), by = "team") |>
    arrange(net_adj) |> mutate(team = factor(team, team), up = net_adj >= net_raw, move = rank_raw - rank_adj)
  hi <- d |> slice_max(move, n = 1, with_ties = FALSE); lo <- d |> slice_min(move, n = 1, with_ties = FALSE)
  off <- diff(range(c(d$net_raw, d$net_adj))) * 0.035
  g <- ggplot(d, aes(y = team)) +
    geom_vline(xintercept = 0, color = "#b5b5b5", linewidth = 0.4) +
    geom_segment(aes(x = net_raw, xend = net_adj, yend = team, color = up), linewidth = 1.1,
                 arrow = arrow(length = unit(0.09, "inches"), type = "closed")) +
    geom_point(aes(x = net_raw), shape = 21, fill = BG, color = INK, size = 2.2, stroke = 0.7) +
    geom_nfl_logos(aes(x = ifelse(up, net_adj + off, net_adj - off), team_abbr = team), height = 0.034) +
    geom_text(aes(x = ifelse(up, net_raw - off * 0.6, net_raw + off * 0.6), hjust = ifelse(up, 1, 0),
                  label = ifelse(move == 0, ordinal(rank_adj), paste0(ordinal(rank_raw), " to ", ordinal(rank_adj)))),
              size = 2.9, family = "RobotoNFL", color = MUTED) +
    scale_color_manual(values = c(`TRUE` = BLUE, `FALSE` = WARM)) +
    scale_x_continuous(labels = label_number(accuracy = 0.01, style_positive = "plus"), breaks = pretty_breaks(8),
                       expand = expansion(mult = 0.07)) +
    labs(
      title = paste0("Adjust for who they played and the ", hi$team_nick, " climb from ", ordinal(hi$rank_raw), " to ",
                     ordinal(hi$rank_adj), "; the ", lo$team_nick, " drop from ", ordinal(lo$rank_raw), " to ", ordinal(lo$rank_adj)),
      subtitle = paste0("Net EPA per play: open dot is raw, the arrow ends at the opponent-adjusted value. ",
                        kw("Tougher slate than it looked", BLUE), " or ", kw("softer", WARM)),
      x = "Net EPA per play (offense minus defense allowed)", y = NULL,
      caption = caption("Adjusted: ridge regression of each team-game's EPA per play on offense, opposing defense and home field")) +
    theme_nfl_axis(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
  save_chart(g, "17_opponent_adjusted_epa", w = 12, h = 10)
}

chart_remaining_schedule <- function() {
  tm <- data_rds("teams")
  r <- adjusted_epa() |> select(opp = team, rating = net_adj)
  s <- readRDS(file.path(DATA, "schedules_all.rds")) |> filter(season == SEASON, game_type == "REG", week > LASTW)
  d <- bind_rows(s |> transmute(week, team = home_team, opp = away_team, away = FALSE),
                 s |> transmute(week, team = away_team, opp = home_team, away = TRUE)) |>
    left_join(r, by = "opp")
  avg <- d |> group_by(team) |> summarise(sos = mean(rating), n = n(), .groups = "drop") |>
    arrange(sos) |> mutate(rk = rank(-sos, ties.method = "first"))
  d <- d |> mutate(team = factor(team, avg$team))
  avg <- avg |> mutate(team = factor(team, team)) |> left_join(tm |> select(team = team_abbr, team_nick), by = "team")
  hi <- avg |> slice_max(sos, n = 1); lo <- avg |> slice_min(sos, n = 1)
  wks <- sort(unique(d$week)); lim <- max(abs(d$rating))
  g <- ggplot(d, aes(week, team)) +
    geom_tile(aes(fill = rating), color = BG, linewidth = 1.1) +
    geom_nfl_logos(aes(team_abbr = opp), height = 0.024) +
    geom_text(data = d |> filter(away), aes(x = week - 0.36, label = "@"), size = 2.6, family = "RobotoNFL", color = INK) +
    geom_nfl_logos(data = avg, aes(x = min(wks) - 1.15, team_abbr = team), height = 0.028) +
    geom_text(data = avg, aes(x = max(wks) + 0.65, label = paste0(signed(sos, 0.01), "  (", ordinal(rk), ")")), hjust = 0, size = 3.1,
              family = "RobotoNFL", fontface = "bold", color = INK) +
    annotate("text", x = max(wks) + 0.65, y = nrow(avg) + 0.9, label = "Avg opponent", hjust = 0, size = 3.1,
             family = "RobotoNFL", fontface = "bold", color = MUTED) +
    scale_fill_gradient2(low = BLUE, mid = MID, high = WARM, midpoint = 0, limits = c(-lim, lim)) +
    scale_x_continuous(breaks = wks, labels = paste("Wk", wks), position = "top",
                       limits = c(min(wks) - 1.5, max(wks) + 2.2), expand = expansion(0)) +
    scale_y_discrete(expand = expansion(add = c(0.6, 1.3))) +
    labs(
      title = paste0("The ", hi$team_nick, " have the hardest road left; the ", lo$team_nick, " have the easiest"),
      subtitle = paste0("Every game after Week ", LASTW, ", shaded by opponent strength: ",
                        kw("strong opponent", WARM), ", ", kw("weak opponent", BLUE), ". @ = road game, blank = bye"),
      x = NULL, y = NULL,
      caption = caption("Opponent strength: opponent-adjusted net EPA per play (chart 17)")) +
    theme_nfl_axis(axis.text.y = element_blank(), panel.grid.major = element_blank(),
                   axis.text.x.top = element_text(face = "bold"))
  save_chart(g, "18_remaining_schedule", w = 12, h = 12)
}
