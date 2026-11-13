# League context (30-32): 2026 against 2018-2025 through the same week, every team's rank on twelve measures,
# and the season-to-date EPA landscape animated week by week.

chart_league_trends <- function() {
  cols <- c("season", "season_type", "week", "play_type", "pass", "rush", "qb_dropback", "pass_attempt", "sack", "air_yards",
            "down", "yards_gained", "epa", "posteam")
  p <- bind_rows(readRDS(file.path(DATA, "pbp_2018_2025.rds"))[, cols], data_rds("pbp")[, cols]) |>
    filter(season_type == "REG", week <= LASTW)
  sc <- readRDS(file.path(DATA, "schedules_all.rds")) |> filter(game_type == "REG", week <= LASTW, !is.na(result)) |>
    group_by(season) |> summarise(v = mean((home_score + away_score) / 2), .groups = "drop")
  sm <- function(d, name) d |> transmute(season, v, metric = name)
  sn <- p |> filter(pass == 1 | rush == 1, !is.na(epa), !is.na(posteam))
  fmt <- list(
    "Points per team per game" = function(x) number(x, 0.1), "Yards per play" = function(x) number(x, 0.01),
    "Dropback rate" = function(x) pct(x, 0.1), "Average depth of target" = function(x) paste0(number(x, 0.1), " yds"),
    "Sack rate" = function(x) pct(x, 0.1), "Fourth-down go rate" = function(x) pct(x, 0.1))
  d <- bind_rows(
    sm(sc, names(fmt)[1]),
    sm(sn |> group_by(season) |> summarise(v = mean(yards_gained, na.rm = TRUE)), names(fmt)[2]),
    sm(sn |> group_by(season) |> summarise(v = mean(qb_dropback)), names(fmt)[3]),
    sm(p |> filter(pass_attempt == 1, sack == 0, !is.na(air_yards)) |> group_by(season) |> summarise(v = mean(air_yards)), names(fmt)[4]),
    sm(sn |> filter(qb_dropback == 1) |> group_by(season) |> summarise(v = mean(sack)), names(fmt)[5]),
    sm(p |> filter(down == 4, play_type %in% c("pass", "run", "punt", "field_goal")) |> group_by(season) |>
         summarise(v = mean(play_type %in% c("pass", "run"))), names(fmt)[6])) |>
    mutate(metric = factor(metric, names(fmt)))
  d$lab <- mapply(function(m, v) fmt[[m]](v), as.character(d$metric), d$v)
  ns <- length(unique(d$season))
  st <- d |> group_by(metric) |>
    summarise(now = v[season == SEASON], avg = mean(v[season < SEASON]), z = (now - avg) / sd(v[season < SEASON]),
              from_top = rank(-v)[season == SEASON], from_bottom = rank(v)[season == SEASON], .groups = "drop") |>
    mutate(pos = pmin(from_top, from_bottom), high = from_top <= from_bottom)
  # the headline is the measure where this season sits closest to an extreme of the period
  top <- st |> arrange(pos, desc(abs(z))) |> slice(1)
  f <- fmt[[as.character(top$metric)]]
  place <- paste0(if (top$pos > 1) paste0(ordinal(top$pos), "-") else "", if (top$high) "highest" else "lowest")
  g <- ggplot(d, aes(season, v)) +
    geom_hline(data = st, aes(yintercept = avg), linetype = "dashed", color = "#9a9a9a", linewidth = 0.4) +
    geom_line(color = GRAY, linewidth = 0.9) +
    geom_point(data = d |> filter(season < SEASON), color = GRAY, size = 2.2) +
    geom_point(data = d |> filter(season == SEASON), color = ORANGE, size = 4) +
    geom_text(data = d |> filter(season == SEASON), aes(label = lab), color = ORANGE, fontface = "bold", family = "RobotoNFL",
              size = 4.2, hjust = -0.28, vjust = 0.4) +
    facet_wrap(~metric, scales = "free_y", ncol = 3) +
    scale_x_continuous(breaks = seq(2018, SEASON, 2), expand = expansion(mult = c(0.06, 0.3))) +
    scale_y_continuous(breaks = pretty_breaks(4), expand = expansion(mult = 0.22),
                       labels = function(x) if (max(x, na.rm = TRUE) < 1) pct(x, 0.1) else number(x, 0.1)) +
    labs(
      title = paste0(top$metric, ": ", f(top$now), " through Week ", LASTW, ", the ", place, " of the last ", ns,
                     " seasons (2018-", SEASON - 1, " average: ", f(top$avg), ")"),
      subtitle = paste0("Each season through Week ", LASTW, " only. ", kw(SEASON, ORANGE), " against 2018-", SEASON - 1,
                        "; dashed line: the average of those seasons"),
      x = NULL, y = NULL,
      caption = caption(span = paste0("regular seasons 2018-", SEASON, ", Weeks 1-", LASTW), credit = NULL)) +
    theme_nfl_axis(strip.text = element_text(face = "bold", size = 12, hjust = 0), panel.spacing = unit(1.4, "lines"),
                   panel.grid.major.x = element_blank())
  save_chart(g, "30_league_trends_since_2018", w = 13, h = 7.6)
}

team_measures <- function() {
  p <- plays_epa(data_rds("pbp")) |> mutate(expl = (pass == 1 & yards_gained >= 20) | (rush == 1 & yards_gained >= 10))
  third <- data_rds("pbp") |> filter(down == 3, pass == 1 | rush == 1, !is.na(posteam)) |>
    mutate(conv = first_down == 1 | touchdown == 1)
  dr <- data_rds("pbp") |> filter(!is.na(posteam), !is.na(fixed_drive_result), !is.na(yardline_100)) |>
    group_by(game_id, fixed_drive, posteam, defteam) |>
    summarise(rz = any(yardline_100 <= 20 & (pass == 1 | rush == 1)), td = first(fixed_drive_result) == "Touchdown", .groups = "drop") |>
    filter(rz)
  to <- data_rds("pbp") |> filter(!is.na(posteam)) |>
    mutate(to = coalesce(interception, 0) + coalesce(fumble_lost, 0))
  gms <- p |> distinct(game_id, posteam) |> count(team = posteam, name = "g")
  side <- function(by) {
    k <- function(d) d |> rename(team = all_of(by))
    k(p) |> group_by(team) |> summarise(epa = mean(epa), success = mean(success, na.rm = TRUE), explosive = mean(expl), .groups = "drop") |>
      left_join(k(third) |> group_by(team) |> summarise(third = mean(conv), .groups = "drop"), by = "team") |>
      left_join(k(dr) |> group_by(team) |> summarise(redzone = mean(td), .groups = "drop"), by = "team") |>
      left_join(k(to) |> group_by(team) |> summarise(to = sum(to), .groups = "drop"), by = "team") |>
      left_join(gms, by = "team") |> mutate(to = to / g) |> select(-g)
  }
  labs <- c(epa = "EPA\nper play", success = "Success\nrate", explosive = "Explosive\nplays", third = "Third\ndown",
            redzone = "Red zone\nTD rate", to = "Turnovers\nper game")
  o <- side("posteam") |> pivot_longer(-team) |> mutate(side = "Offense", good = ifelse(name == "to", -value, value))
  f <- side("defteam") |> pivot_longer(-team) |> mutate(side = "Defense", good = ifelse(name == "to", value, -value))
  bind_rows(o, f) |> group_by(side, name) |> mutate(rank = rank(-good, ties.method = "min")) |> ungroup() |>
    mutate(measure = factor(labs[name], labs), side = factor(side, c("Offense", "Defense")))
}

chart_team_fingerprint <- function() {
  d <- team_measures()
  ord <- d |> group_by(team) |> summarise(avg = mean(rank), top10 = sum(rank <= 10), bot10 = sum(rank >= 23), .groups = "drop") |>
    arrange(desc(avg)) |> left_join(data_rds("teams") |> select(team = team_abbr, team_nick), by = "team")
  d <- d |> mutate(team = factor(team, ord$team))
  best <- ord |> slice_max(top10, n = 1, with_ties = FALSE); worst <- ord |> slice_max(bot10, n = 1, with_ties = FALSE)
  nm <- n_distinct(d$name) * 2
  g <- ggplot(d, aes(measure, team)) +
    geom_tile(aes(fill = rank), color = BG, linewidth = 1) +
    geom_text(aes(label = rank, color = abs(rank - 16.5) > 10), size = 3.1, family = "RobotoNFL", fontface = "bold") +
    geom_nfl_logos(data = ord |> mutate(team = factor(team, team), side = factor("Offense", c("Offense", "Defense"))),
                   aes(x = 0.2, y = team, team_abbr = team), height = 0.021, inherit.aes = FALSE) +
    facet_grid(~side, scales = "free_x") +
    scale_fill_gradient2(low = BLUE, mid = MID, high = WARM, midpoint = 16.5) +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = INK)) +
    scale_x_discrete(position = "top", expand = expansion(add = c(1.05, 0))) +
    labs(
      title = paste0("The ", best$team_nick, " rank in the top 10 on ", best$top10, " of ", nm, " measures; the ", worst$team_nick,
                     " are in the bottom 10 on ", worst$bot10),
      subtitle = paste0("League rank on six offensive and six defensive measures: ", kw("1st is best", BLUE), ", ", kw("32nd is worst", WARM),
                        ". Teams sorted by average rank"),
      x = NULL, y = NULL,
      caption = caption("Explosive: 20+ yard passes, 10+ yard runs. Turnovers: giveaways (offense), takeaways (defense)")) +
    theme_nfl_axis(axis.text.y = element_blank(), panel.grid.major = element_blank(),
                   axis.text.x.top = element_text(face = "bold", size = 9.5, lineheight = 0.95),
                   strip.text = element_text(face = "bold", size = 14), strip.placement = "outside",
                   panel.spacing = unit(1.6, "lines"))
  save_chart(g, "31_team_rank_grid", w = 12, h = 12)
}

# Animated GIF: the offense/defense landscape of chart 01, season to date after each week. Frames are drawn one by
# one and stitched with magick; positions between weeks are eased with tweenr.
chart_epa_race <- function(from = 3, steps = 8, hold = 6) {
  p <- plays_epa(data_rds("pbp"))
  cum <- bind_rows(lapply(from:LASTW, function(w) {
    q <- p |> filter(week <= w)
    inner_join(q |> group_by(team = posteam) |> summarise(off = mean(epa), .groups = "drop"),
               q |> group_by(team = defteam) |> summarise(def = mean(epa), .groups = "drop"), by = "team") |> mutate(week = w)
  })) |> mutate(net = off - def)
  mv <- inner_join(cum |> filter(week == from), cum |> filter(week == LASTW), by = "team") |> mutate(ch = net.y - net.x) |>
    left_join(data_rds("teams") |> select(team = team_abbr, team_nick), by = "team")
  up <- mv |> slice_max(ch, n = 1); dn <- mv |> slice_min(ch, n = 1)
  xl <- range(cum$off) + c(-1, 1) * diff(range(cum$off)) * 0.06; yl <- range(cum$def) + c(-1, 1) * diff(range(cum$def)) * 0.08
  ix <- diff(xl) * 0.012; iy <- diff(yl) * 0.02
  ease <- tweenr::tween(c(0, 1), steps + 1, ease = "cubic-in-out")[[1]]
  frames <- list(); wks <- from:LASTW
  for (i in seq_along(wks)) {
    a <- cum |> filter(week == wks[i]) |> arrange(team)
    frames <- c(frames, rep(list(a), hold))
    if (i < length(wks)) {
      b <- cum |> filter(week == wks[i + 1]) |> arrange(team)
      for (t in ease[-c(1, length(ease))]) frames <- c(frames, list(mutate(a, off = off + (b$off - off) * t, def = def + (b$def - def) * t)))
    }
  }
  frames <- c(frames, rep(list(frames[[length(frames)]]), hold * 3))
  dir <- file.path(tempdir(), "epa_race"); unlink(dir, recursive = TRUE); dir.create(dir)
  files <- character(length(frames))
  for (k in seq_along(frames)) {
    f <- frames[[k]]
    g <- ggplot(f, aes(off, def)) +
      geom_hline(yintercept = 0, color = INK, linetype = "dashed", alpha = 0.4, linewidth = 0.5) +
      geom_vline(xintercept = 0, color = INK, linetype = "dashed", alpha = 0.4, linewidth = 0.5) +
      annotate("text", x = xl[2] - ix, y = yl[1] + iy, label = "Good offense, good defense", hjust = 1, vjust = 1, color = GREEN,
               family = "RobotoNFL", fontface = "bold", size = 4.2) +
      annotate("text", x = xl[1] + ix, y = yl[2] - iy, label = "Struggling on both sides", hjust = 0, vjust = 0, color = RED,
               family = "RobotoNFL", fontface = "bold", size = 4.2) +
      annotate("text", x = xl[2] - ix, y = yl[2] - iy, label = paste("Through Week", f$week[1]), hjust = 1, vjust = 0, color = if (DARK) "#3a4047" else "#c9c9c9",
               family = "RobotoNFLBlack", size = 13) +
      geom_nfl_logos(aes(team_abbr = team), width = 0.05, alpha = 0.95) +
      scale_x_continuous(limits = xl, labels = label_number(accuracy = 0.01, style_positive = "plus"), breaks = pretty_breaks(6), expand = expansion(0)) +
      scale_y_reverse(limits = rev(yl), labels = label_number(accuracy = 0.01, style_positive = "plus"), breaks = pretty_breaks(6), expand = expansion(0)) +
      labs(title = paste0("Since Week ", from, " the ", up$team_nick, " have climbed the most and the ", dn$team_nick, " have fallen the furthest"),
           subtitle = "Season-to-date EPA per play on offense and allowed on defense, after each week. Up and to the right is good.",
           x = "Offense: EPA per play (higher is better)", y = "Defense: EPA per play allowed (lower is better)",
           caption = caption("EPA = expected points added (nflfastR)")) +
      theme_nfl()
    g <- g + labs(title = wrap_text(get_labs(g)$title, 72))
    files[k] <- file.path(dir, sprintf("f%03d.png", k))
    ggsave(files[k], g, width = 12, height = 6.75, dpi = 90, device = ragg::agg_png, bg = BG)
  }
  out <- file.path(OUT, "32_epa_landscape_by_week.gif")
  im <- magick::image_read(files)
  magick::image_write(magick::image_animate(im, fps = 10, optimize = TRUE), out)
  message("wrote ", out, " (", length(files), " frames, ", round(file.size(out) / 1e6, 1), " MB)")
}
