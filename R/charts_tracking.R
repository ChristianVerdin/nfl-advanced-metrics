# Player tracking and charting (25-29): Next Gen Stats time to throw and separation, interception luck (FTN),
# rookie playing time, kickers against expectation.

# sprite path per player, made on demand for anyone outside the headshot groups
sprite_for <- function(ids, teams) {
  sp <- sprites()
  vapply(seq_along(ids), function(i) {
    j <- match(ids[i], sp$gsis_id); f <- sp$sprite[j]
    if (is.na(f) || !file.exists(f)) { e <- ensure_sprite(ids[i], teams[i]); if (DARK) e$circle else e$sprite }
    else if (DARK) sp$circle[j] else f   # on dark the name arc (team colour) is unreadable: plain circle + a text label
  }, character(1))
}

# on a dark background the headshot is the plain circle with the last name printed underneath
HEAD_W <- function(W) if (DARK) W * 0.66 else W
head_names <- function(heads, W, yspan, aspect) {
  if (!DARK) return(NULL)
  geom_text(data = heads, aes(x = xs, y = ys - yspan * W * aspect * 0.43, label = toupper(last_name(name))), size = 2.7,
            family = "RobotoNFL", fontface = "bold", color = INK, inherit.aes = FALSE)
}

chart_time_to_throw <- function(min_att = 150) {
  d <- data_rds("ngs_passing") |> mutate(team = clean_team_abbrs(team_abbr)) |>
    group_by(id = player_gsis_id) |>
    summarise(name = last(player_display_name), team = last(team), att = sum(attempts),
              ttt = weighted.mean(avg_time_to_throw, attempts), ay = weighted.mean(avg_intended_air_yards, attempts), .groups = "drop") |>
    filter(att >= min_att)
  slow <- d |> slice_max(ttt, n = 1); fast <- d |> slice_min(ttt, n = 1)
  pick <- unique(c(slow$id, fast$id, (d |> slice_max(ttt, n = 3))$id, (d |> slice_min(ttt, n = 3))$id,
                   (d |> slice_max(ay, n = 2))$id, (d |> slice_min(ay, n = 2))$id))
  heads <- d |> filter(id %in% pick); heads$sprite <- sprite_for(heads$id, heads$team)
  W <- 0.075
  heads <- cbind(heads, spread_xy(heads$ttt, heads$ay, rep(W, nrow(heads)), range(d$ttt), range(d$ay), aspect = 12 / 7.6))
  g <- ggplot(d, aes(ttt, ay)) +
    geom_mean_lines(aes(x0 = ttt, y0 = ay), color = INK, linetype = "dashed", alpha = 0.4, linewidth = 0.5) +
    geom_point(data = d |> filter(!id %in% pick), color = GRAY, size = 2.8, alpha = 0.6) +
    geom_text_repel(data = d |> filter(!id %in% pick), aes(label = last_name(name)), size = 3, family = "RobotoNFL",
                    color = MUTED, seed = 7, box.padding = 0.25, min.segment.length = 1, max.overlaps = 30) +
    geom_segment(data = heads, aes(xend = xs, yend = ys), color = "#9a9a9a", linewidth = 0.4) +
    geom_point(data = heads, color = INK, size = 1.3) +
    ggpath::geom_from_path(data = heads, aes(x = xs, y = ys, path = sprite), width = HEAD_W(W)) +
    head_names(heads, W, diff(range(d$ay)) * 1.2, 12 / 7.6 * 1.1) +
    scale_x_continuous(labels = label_number(accuracy = 0.1, suffix = "s"), breaks = pretty_breaks(7), expand = expansion(mult = 0.08)) +
    scale_y_continuous(labels = label_number(accuracy = 1, suffix = " yds"), breaks = pretty_breaks(6), expand = expansion(mult = 0.1)) +
    labs(
      title = paste0(slow$name, " holds the ball ", number(slow$ttt, 0.01), " seconds a throw; ", fast$name, " gets it out in ",
                     number(fast$ttt, 0.01)),
      subtitle = paste0("Average time to throw vs average depth of target, quarterbacks with ", min_att,
                        "+ attempts. Headshots: the extremes on either measure"),
      x = "Average time to throw", y = "Average intended air yards",
      caption = caption("Time to throw and air yards: NFL Next Gen Stats", credit = HEADS)) +
    theme_nfl()
  save_chart(g, "25_time_to_throw", w = 12, h = 7.6)
}

chart_int_luck <- function(top = 24) {
  j <- data_rds("pbp") |> filter(pass_attempt == 1, sack == 0, !is.na(passer_player_id)) |>
    inner_join(data_rds("ftn") |> select(game_id = nflverse_game_id, play_id = nflverse_play_id, worthy = is_interception_worthy),
               by = c("game_id", "play_id"))
  d <- j |> group_by(id = passer_player_id) |>
    summarise(team = main_team(posteam), att = n(), worthy = sum(worthy), ints = sum(interception), .groups = "drop") |>
    slice_max(att, n = top, with_ties = FALSE) |>
    left_join(data_rds("players") |> select(id = gsis_id, name), by = "id") |>
    mutate(gap = worthy - ints) |> arrange(gap, desc(ints)) |> mutate(id = factor(id, id))
  lucky <- d |> slice_max(gap, n = 1, with_ties = FALSE); un <- d |> slice_min(gap, n = 1, with_ties = FALSE)
  g <- ggplot(d, aes(y = id)) +
    geom_segment(aes(x = ints, xend = worthy, yend = id), color = "#b9c0c8", linewidth = 2.2, lineend = "round") +
    geom_point(aes(x = worthy), shape = 21, fill = BG, color = INK, size = 3.4, stroke = 0.9) +
    geom_point(aes(x = ints), color = RED, size = 3.4) +
    geom_text(aes(x = pmax(worthy, ints) + 0.5, label = paste0(name, "   ", ints, " INT, ", worthy, " worthy")), hjust = 0,
              size = 3.1, family = "RobotoNFL", color = INK) +
    scale_x_continuous(breaks = pretty_breaks(8), expand = expansion(mult = c(0.03, 0.3))) +
    labs(
      title = paste0(lucky$name, " has thrown ", lucky$worthy, " interception-worthy passes and been picked ",
                     if (lucky$ints == 1) "once" else paste(lucky$ints, "times"), "; ", un$name, ": ", un$worthy, " and ", un$ints),
      subtitle = paste0(kw("Interceptions", RED), " vs <b>interception-worthy throws</b> (open dot), the ", top,
                        " busiest passers. Top of the chart: defenders dropped the most"),
      x = "Passes", y = NULL,
      caption = caption("Interception-worthy throws: FTN Data charting via nflverse", credit = HEADS)) +
    theme_nfl_axis(axis.text.y = element_nfl_headshot(size = 1.15), panel.grid.major.y = element_blank())
  save_chart(g, "26_interception_luck", w = 12, h = 10)
}

chart_rookie_snaps <- function(n_lab = 7) {
  NAVYB <- if (DARK) "#6aa5e0" else "#1f4e79"
  sn <- data_rds("snaps")
  tg <- sn |> distinct(team, game_id) |> count(team, name = "tg")
  ps <- sn |> group_by(pfr_player_id) |>
    summarise(team = main_team(team), share_sum = sum(pmax(offense_pct, defense_pct)), .groups = "drop") |>
    left_join(tg, by = "team") |> mutate(share = pmin(1, share_sum / tg))
  d <- data_rds("draft_picks") |> filter(season == SEASON) |>
    left_join(ps, by = "pfr_player_id") |>
    mutate(share = coalesce(share, 0), side = ifelse(side == "D", "Defense", ifelse(side == "O", "Offense", "Specialist")),
           team = coalesce(team.y, clean_team_abbrs(team.x)), rd = factor(round))
  half <- d |> group_by(round) |> summarise(n = n(), half = sum(share >= 0.5), .groups = "drop")
  day3 <- sum(half$half[half$round >= 4])
  lab <- bind_rows(d |> filter(round >= 3) |> slice_max(share, n = n_lab, with_ties = FALSE),
                   d |> filter(round == 1) |> slice_min(share, n = 2, with_ties = FALSE))
  g <- ggplot(d, aes(rd, share)) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = INK, linewidth = 0.4) +
    ggbeeswarm::geom_quasirandom(aes(color = side), size = 2.9, alpha = 0.85, width = 0.36, stroke = 0) +
    geom_text_repel(data = lab, aes(label = paste0(pfr_player_name, ", ", team)), size = 3, family = "RobotoNFL", color = INK,
                    seed = 3, box.padding = 0.5, point.padding = 0.3, min.segment.length = 0, segment.color = "#9a9a9a",
                    segment.size = 0.3) +
    geom_text(data = half, aes(x = factor(round), y = 1.09, label = paste0(half, " of ", n)), size = 3.6, family = "RobotoNFL",
              fontface = "bold", color = INK) +
    annotate("text", x = 0.42, y = 1.09, label = "Half the snaps or more:", hjust = 1, size = 3.2, family = "RobotoNFL", color = MUTED) +
    scale_color_manual(values = c(Offense = NAVYB, Defense = ORANGE, Specialist = GRAY)) +
    scale_x_discrete(labels = function(x) paste("Round", x), expand = expansion(add = c(1.5, 0.6))) +
    scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 1, 0.25), limits = c(0, 1.12)) +
    labs(
      title = paste0(half$half[1], " of ", half$n[1], " first-rounders play at least half their team's snaps; so do ", day3,
                     " players drafted in rounds 4-7"),
      subtitle = paste0("Every player drafted in ", SEASON, ": average share of the team's ", kw("offensive", NAVYB), " or ",
                        kw("defensive", ORANGE), " snaps per team game (0% when he did not play)"),
      x = NULL, y = "Share of team snaps per game",
      caption = caption("Snap counts and draft results: Pro Football Reference via nflverse", credit = NULL)) +
    theme_nfl_axis(panel.grid.major.x = element_blank(), axis.text.x = element_text(face = "bold"))
  save_chart(g, "27_rookie_playing_time", w = 13, h = 7.6)
}

chart_kickers <- function(min_att = 10) {
  k <- data_rds("pbp") |> filter(field_goal_attempt == 1, !is.na(fg_prob), !is.na(kicker_player_id)) |>
    mutate(made = field_goal_result == "made")
  d <- k |> group_by(id = kicker_player_id) |>
    summarise(name = last(kicker_player_name), team = last(posteam), att = n(), made = sum(made), exp = sum(fg_prob),
              long = max(kick_distance[field_goal_result == "made"], -Inf), .groups = "drop") |>
    filter(att >= min_att) |> mutate(poe = 3 * (made - exp)) |>
    left_join(data_rds("players") |> select(id = gsis_id, full = name), by = "id") |>
    left_join(data_rds("teams") |> select(team = team_abbr, team_color, team_color2), by = "team") |>
    mutate(name = coalesce(full, name), team_color = on_bg(team_color, team_color2)) |> arrange(poe) |> mutate(id = factor(id, id))
  hi <- tail(d, 1); lo <- head(d, 1); off <- diff(range(d$poe)) * 0.02
  g <- ggplot(d, aes(poe, id)) +
    geom_vline(xintercept = 0, color = INK, linewidth = 0.5) +
    geom_segment(aes(x = 0, xend = poe, yend = id, color = team_color), linewidth = 1.5) +
    geom_point(aes(color = team_color), size = 3.6) +
    geom_text(aes(x = ifelse(poe >= 0, poe + off, poe - off), hjust = ifelse(poe >= 0, 0, 1),
                  label = paste0(name, "  ", made, "/", att, ", long ", long)), size = 3.05, family = "RobotoNFL", color = INK) +
    scale_color_identity() +
    scale_x_continuous(labels = label_number(accuracy = 1, style_positive = "plus"), breaks = pretty_breaks(8),
                       expand = expansion(mult = 0.3)) +
    labs(
      title = paste0(hi$name, " has been worth ", number(hi$poe, 0.1), " points more than an average kicker; ", lo$name,
                     " has cost ", number(-lo$poe, 0.1)),
      subtitle = paste0("Field goal points over expected: 3 x (makes minus the make probability of each kick). ", min_att, "+ attempts"),
      x = "Field goal points over expected", y = NULL,
      caption = caption("Make probability by distance and conditions: nflfastR model", credit = HEADS)) +
    theme_nfl_axis(axis.text.y = element_nfl_headshot(size = 1.0), panel.grid.major.y = element_blank())
  save_chart(g, "28_kickers_over_expected", w = 12, h = 10.5)
}

chart_separation <- function(min_tgt = 35, n_each = 3) {
  d <- data_rds("ngs_receiving") |> mutate(team = clean_team_abbrs(team_abbr)) |>
    group_by(id = player_gsis_id) |>
    summarise(name = last(player_display_name), pos = last(player_position), team = last(team), tgt = sum(targets), rec = sum(receptions),
              sep = weighted.mean(avg_separation, targets), yac = weighted.mean(avg_yac_above_expectation, receptions, na.rm = TRUE),
              .groups = "drop") |>
    filter(tgt >= min_tgt, !is.na(yac))
  s1 <- d |> slice_max(sep, n = 1); y1 <- d |> slice_max(yac, n = 1)
  pick <- unique(c((d |> slice_max(sep, n = n_each))$id, (d |> slice_max(yac, n = n_each))$id,
                   (d |> slice_min(sep, n = 1))$id, (d |> slice_min(yac, n = 1))$id))
  heads <- d |> filter(id %in% pick); heads$sprite <- sprite_for(heads$id, heads$team)
  W <- 0.075
  heads <- cbind(heads, spread_xy(heads$sep, heads$yac, rep(W, nrow(heads)), range(d$sep), range(d$yac), aspect = 12 / 7.6))
  g <- ggplot(d, aes(sep, yac)) +
    geom_hline(yintercept = 0, color = INK, linewidth = 0.4) +
    geom_vline(xintercept = weighted.mean(d$sep, d$tgt), color = INK, linetype = "dashed", alpha = 0.4, linewidth = 0.5) +
    geom_point(data = d |> filter(!id %in% pick), aes(shape = pos == "TE"), color = GRAY, size = 2.6, alpha = 0.6) +
    geom_segment(data = heads, aes(xend = xs, yend = ys), color = "#9a9a9a", linewidth = 0.4) +
    geom_point(data = heads, color = INK, size = 1.3) +
    ggpath::geom_from_path(data = heads, aes(x = xs, y = ys, path = sprite), width = HEAD_W(W)) +
    head_names(heads, W, diff(range(d$yac)) * 1.2, 12 / 7.6 * 1.1) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17)) +
    scale_x_continuous(labels = label_number(accuracy = 0.1, suffix = " yds"), breaks = pretty_breaks(7), expand = expansion(mult = 0.08)) +
    scale_y_continuous(labels = label_number(accuracy = 0.1, style_positive = "plus", suffix = " yds"), breaks = pretty_breaks(7),
                       expand = expansion(mult = 0.1)) +
    labs(
      title = if (s1$id == y1$id) paste0(s1$name, " is the most open target (", number(s1$sep, 0.1),
                                         " yards) and the best after the catch (", signed(y1$yac, 0.1), " per catch)")
              else paste0(s1$name, " gets the most separation (", number(s1$sep, 0.1), " yards); ", y1$name,
                          " adds the most after the catch (", signed(y1$yac, 0.1), ")"),
      subtitle = paste0("Average separation at the catch point vs yards after catch over expected per reception, ", min_tgt,
                        "+ targets. Triangles: tight ends"),
      x = "Average separation from the nearest defender", y = "Yards after catch over expected, per reception",
      caption = caption("Separation and expected yards after catch: NFL Next Gen Stats", credit = HEADS)) +
    theme_nfl()
  save_chart(g, "29_separation_vs_yac", w = 12, h = 7.6)
}
