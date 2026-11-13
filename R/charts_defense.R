# Pass rush and the passing map (23-24).

chart_pressure_blitz <- function() {
  db <- data_rds("pbp") |> filter(qb_dropback == 1, !is.na(defteam)) |> count(team = defteam, name = "db")
  d <- data_rds("pfr_pass") |> group_by(team = opponent) |>
    summarise(press = sum(times_pressured, na.rm = TRUE), blitz = sum(times_blitzed, na.rm = TRUE), .groups = "drop") |>
    inner_join(db, by = "team") |> mutate(press = press / db, blitz = blitz / db) |>
    left_join(data_rds("teams") |> select(team = team_abbr, team_nick), by = "team")
  top <- d |> slice_max(press, n = 1); most <- d |> slice_max(blitz, n = 1)
  # most pressure for the blitzing done, judged against the trend among the other teams
  rest <- d |> filter(team != most$team); rest$resid <- resid(lm(press ~ blitz, rest)); four <- rest |> slice_max(resid, n = 1)
  # a team far off the blitz scale is drawn at the right edge with its real numbers, so the other 31 stay readable
  edge <- max(rest$blitz) + diff(range(rest$blitz)) * 0.3
  far <- d |> filter(blitz > edge) |> mutate(real = blitz, blitz = edge)
  d <- d |> mutate(blitz = pmin(blitz, edge))
  px <- diff(range(d$blitz)) * 0.08; py <- diff(range(d$press)) * 0.1
  quad <- tibble(x = c(min(d$blitz) - px, max(d$blitz) + px, min(d$blitz) - px, max(d$blitz) + px),
                 y = c(max(d$press) + py, max(d$press) + py, min(d$press) - py, min(d$press) - py),
                 hjust = c(0, 1, 0, 1), vjust = c(1, 1, 0, 0),
                 label = c("Pressure without blitzing", "", "Sit back, no pressure", "Blitz and still no pressure"),
                 col = c(GREEN, MUTED, MUTED, RED))
  g <- ggplot(d, aes(blitz, press)) +
    geom_mean_lines(data = rest, aes(x0 = blitz, y0 = press), color = INK, linetype = "dashed", alpha = 0.45, linewidth = 0.5) +
    geom_nfl_logos(aes(team_abbr = team), width = 0.05, alpha = 0.95) +
    geom_text(data = far, aes(x = blitz - diff(range(d$blitz)) * 0.04, label = paste0("blitz rate ", pct(real), ",\noff the scale \u2192")),
              hjust = 1, size = 3.3, lineheight = 0.95, family = "RobotoNFL", fontface = "bold", color = INK) +
    geom_text(data = quad, aes(x, y, label = label, hjust = hjust, vjust = vjust, color = col), family = "RobotoNFL",
              fontface = "bold", size = 4.2, inherit.aes = FALSE) +
    scale_color_identity() +
    scale_x_continuous(labels = percent_format(accuracy = 1), breaks = pretty_breaks(7), expand = expansion(mult = 0.02)) +
    scale_y_continuous(labels = percent_format(accuracy = 1), breaks = pretty_breaks(7), expand = expansion(mult = 0.02)) +
    labs(
      title = if (top$team == most$team)
        paste0("The ", top$team_nick, " blitz on ", pct(top$blitz), " of dropbacks and get the most pressure (", pct(top$press),
               "); the ", four$team_nick, " get the most for how little they blitz")
      else paste0("The ", top$team_nick, " get the most pressure (", pct(top$press), " of dropbacks); the ", most$team_nick,
                  " blitz the most (", pct(most$blitz), ")"),
      subtitle = paste0("Blitz rate vs pressure rate on opponent dropbacks. The ", four$team_nick, ": pressure on ", pct(four$press),
                        ", a blitz on ", pct(four$blitz)),
      x = "Blitz rate (share of opponent dropbacks)", y = "Pressure rate (share of opponent dropbacks)",
      caption = caption("Pressures and blitzes: Pro Football Reference advanced stats via nflverse. Dashed lines: averages")) +
    theme_nfl()
  save_chart(g, "23_pressure_vs_blitz")
}

chart_pass_maps <- function() {
  pa <- data_rds("pbp") |> filter(pass_attempt == 1, sack == 0, !is.na(air_yards), !is.na(pass_location), !is.na(posteam)) |>
    mutate(depth = cut(air_yards, c(-Inf, 0, 9, 19, Inf), labels = c("Behind the line", "1-9", "10-19", "20+")),
           side = factor(pass_location, c("left", "middle", "right"), c("L", "M", "R")))
  lg <- pa |> count(depth, side) |> mutate(lg = n / sum(n)) |> select(-n)
  d <- pa |> count(team = posteam, depth, side) |> complete(team, depth, side, fill = list(n = 0)) |>
    group_by(team) |> mutate(share = n / sum(n)) |> ungroup() |> left_join(lg, by = c("depth", "side")) |>
    mutate(diff = share - lg)
  deep <- pa |> group_by(team = posteam) |> summarise(deep = mean(air_yards >= 20), .groups = "drop") |>
    left_join(data_rds("teams") |> select(team = team_abbr, team_nick), by = "team") |> arrange(desc(deep))
  d <- d |> mutate(team = factor(team, deep$team))
  hi <- head(deep, 1); lo <- tail(deep, 1); lim <- max(abs(d$diff))
  g <- ggplot(d, aes(side, depth)) +
    geom_tile(aes(fill = diff), color = BG, linewidth = 0.8) +
    geom_text(aes(label = number(share * 100, 1)), size = 2.7, family = "RobotoNFL", color = INK) +
    geom_nfl_logos(data = d |> distinct(team), aes(x = 2, y = 5.1, team_abbr = team), height = 0.2, inherit.aes = FALSE) +
    facet_wrap(~team, ncol = 8) +
    scale_fill_gradient2(low = BLUE, mid = MID, high = WARM, midpoint = 0, limits = c(-lim, lim)) +
    scale_y_discrete(labels = c("Behind", "1-9", "10-19", "20+"), expand = expansion(add = c(0.5, 1.45))) +
    labs(
      title = paste0("The ", hi$team_nick, " throw 20+ yards downfield on ", pct(hi$deep), " of passes; the ", lo$team_nick, " on ",
                     pct(lo$deep)),
      subtitle = paste0("Share of each offense's passes (%) by depth and side of the field: ", kw("more than the league", WARM), " or ",
                        kw("less", BLUE), ". Teams sorted by deep-pass rate"),
      x = "Side of the field (left, middle, right)", y = "Air yards",
      caption = caption(paste0(comma(nrow(pa)), " charted pass attempts"))) +
    theme_nfl_axis(strip.text = element_blank(), panel.grid.major = element_blank(),
                   axis.text = element_text(size = 7.5, color = MUTED), panel.spacing = unit(0.9, "lines"))
  save_chart(g, "24_passing_maps", w = 14, h = 9.6)
}
