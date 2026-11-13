# Situational football (19-22): the red zone, fourth-and-short, how drives end, pre-snap motion.

team_nicks <- function() data_rds("teams") |> select(team = team_abbr, team_nick)

chart_red_zone <- function() {
  dr <- data_rds("pbp") |> filter(!is.na(posteam), !is.na(fixed_drive_result), !is.na(yardline_100)) |>
    group_by(game_id, fixed_drive, posteam, defteam) |>
    summarise(rz = any(yardline_100 <= 20 & (pass == 1 | rush == 1)), td = first(fixed_drive_result) == "Touchdown", .groups = "drop") |>
    filter(rz)
  d <- inner_join(dr |> group_by(team = posteam) |> summarise(off = mean(td), off_n = n(), .groups = "drop"),
                  dr |> group_by(team = defteam) |> summarise(def = mean(td), def_n = n(), .groups = "drop"), by = "team") |>
    left_join(team_nicks(), by = "team")
  bo <- d |> slice_max(off, n = 1, with_ties = FALSE); bd <- d |> slice_min(def, n = 1, with_ties = FALSE)
  px <- diff(range(d$off)) * 0.08; py <- diff(range(d$def)) * 0.1
  quad <- tibble(x = c(max(d$off) + px, min(d$off) - px, max(d$off) + px, min(d$off) - px),
                 y = c(min(d$def) - py, min(d$def) - py, max(d$def) + py, max(d$def) + py),
                 hjust = c(1, 0, 1, 0), vjust = c(1, 1, 0, 0),
                 label = c("Finish drives, deny them", "Deny them, settle for three", "Finish drives, give them back", "Lose both ends"),
                 col = c(GREEN, MUTED, MUTED, RED))
  g <- ggplot(d, aes(off, def)) +
    geom_mean_lines(aes(x0 = off, y0 = def), color = INK, linetype = "dashed", alpha = 0.45, linewidth = 0.5) +
    geom_nfl_logos(aes(team_abbr = team), width = 0.05, alpha = 0.95) +
    geom_text(data = quad, aes(x, y, label = label, hjust = hjust, vjust = vjust, color = col), family = "RobotoNFL",
              fontface = "bold", size = 4.2, inherit.aes = FALSE) +
    scale_color_identity() +
    scale_x_continuous(labels = percent_format(accuracy = 1), breaks = pretty_breaks(7), expand = expansion(mult = 0.02)) +
    scale_y_reverse(labels = percent_format(accuracy = 1), breaks = pretty_breaks(7), expand = expansion(mult = 0.02)) +
    labs(
      title = paste0("Inside the 20: the ", bo$team_nick, " score a touchdown on ", pct(bo$off), " of trips; the ",
                     bd$team_nick, " allow one on ", pct(bd$def)),
      subtitle = "Share of red-zone drives (a snap at or inside the opponent's 20) that end in a touchdown. Up and to the right is good.",
      x = "Offense: touchdown rate in the red zone", y = "Defense: touchdown rate allowed in the red zone",
      caption = caption(paste0(nrow(dr), " red-zone drives"))) +
    theme_nfl()
  save_chart(g, "19_red_zone_touchdown_rate")
}

chart_fourth_down <- function(max_togo = 3, own_side = 60) {
  tm <- data_rds("teams")
  d4 <- data_rds("pbp") |>
    filter(down == 4, ydstogo <= max_togo, yardline_100 <= own_side, qtr <= 3, !is.na(posteam),
           play_type %in% c("pass", "run", "punt", "field_goal")) |>
    mutate(go = play_type %in% c("pass", "run"), conv = go & (first_down == 1 | touchdown == 1))
  d <- d4 |> group_by(team = posteam) |> summarise(n = n(), go_n = sum(go), conv = sum(conv), rate = mean(go), .groups = "drop") |>
    left_join(tm |> select(team = team_abbr, team_nick, team_color, team_color2), by = "team") |>
    mutate(team_color = on_bg(team_color, team_color2)) |> arrange(rate, go_n) |> mutate(team = factor(team, team))
  # ties on the rate go to the bigger sample, and every team tied on both is named
  hi <- d |> filter(rate == max(rate)) |> filter(n == max(n)); lo <- d |> filter(rate == min(rate)) |> filter(n == max(n))
  lg <- mean(d4$go)
  g <- ggplot(d, aes(rate, team)) +
    geom_col(aes(fill = team_color), width = 0.68) +
    geom_vline(xintercept = lg, linetype = "dashed", color = INK, linewidth = 0.4) +
    annotate("text", x = lg + 0.008, y = nrow(d) + 0.85, label = paste0("League ", pct(lg)), hjust = 0, size = 3.3,
             family = "RobotoNFL", fontface = "bold", color = INK) +
    geom_nfl_logos(aes(x = rate + 0.022, team_abbr = team), height = 0.03) +
    geom_label(aes(x = rate + 0.042, label = paste0(pct(rate), "   ", go_n, " of ", n, ", ", conv, " converted")), hjust = 0,
               size = 3, family = "RobotoNFL", color = INK, fill = BG, linewidth = 0, label.padding = unit(0.12, "lines")) +
    scale_y_discrete(expand = expansion(add = c(0.6, 1.4))) +
    scale_fill_identity() +
    scale_x_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 1, 0.1), limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.2))) +
    labs(
      title = paste0("Fourth and short: the ", and_list(rev(hi$team_nick)), " have gone for it on ", hi$go_n[1], " of ", hi$n[1],
                     "; the ", and_list(rev(lo$team_nick)), " on ", lo$go_n[1], " of ", lo$n[1]),
      subtitle = paste0("Share of 4th-and-", max_togo, "-or-less snaps past their own ", 100 - own_side,
                        " where the offense ran a play instead of kicking. Quarters 1-3"),
      x = "Go-for-it rate", y = NULL,
      caption = caption("Runs and passes count as going for it; punts and field goal tries as kicking")) +
    theme_nfl_axis(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
  save_chart(g, "20_fourth_and_short", w = 12, h = 10)
}

chart_drive_outcomes <- function() {
  lv <- c("Touchdown", "Field goal", "Punt", "Downs or missed kick", "Turnover")
  cols <- if (DARK) c(Touchdown = "#2a9d63", `Field goal` = "#2f6b4c", Punt = "#3a4047", `Downs or missed kick` = "#8a5a33", Turnover = "#d0503f")
          else c(Touchdown = "#1a7f4b", `Field goal` = "#8fcfa8", Punt = "#c9ced4", `Downs or missed kick` = "#f0b38a", Turnover = "#c0392b")
  dr <- data_rds("pbp") |> filter(!is.na(posteam), !is.na(fixed_drive_result)) |>
    distinct(game_id, fixed_drive, posteam, res = fixed_drive_result) |>
    filter(res != "End of half") |>
    mutate(out = case_when(res == "Touchdown" ~ lv[1], res == "Field goal" ~ lv[2], res == "Punt" ~ lv[3],
                           res %in% c("Turnover on downs", "Missed field goal") ~ lv[4], TRUE ~ lv[5]))
  d <- dr |> count(team = posteam, out) |> group_by(team) |> mutate(share = n / sum(n), drives = sum(n)) |> ungroup() |>
    mutate(out = factor(out, rev(lv)))
  ord <- d |> filter(out == "Touchdown") |> arrange(share)
  d <- d |> mutate(team = factor(team, ord$team)) |> arrange(team, desc(out)) |>
    group_by(team) |> mutate(mid = cumsum(share) - share / 2) |> ungroup()
  top <- ord |> slice_max(share, n = 1) |> left_join(team_nicks(), by = "team")
  bot <- ord |> slice_min(share, n = 1) |> left_join(team_nicks(), by = "team")
  g <- ggplot(d, aes(share, team)) +
    geom_col(aes(fill = out), width = 0.74, color = BG, linewidth = 0.5) +
    geom_text(data = d |> filter(share >= 0.06, out %in% c("Touchdown", "Turnover")), aes(x = mid, label = pct(share)),
              color = "white", size = 3, family = "RobotoNFL", fontface = "bold") +
    geom_text(data = d |> filter(share >= 0.06, !out %in% c("Touchdown", "Turnover")), aes(x = mid, label = pct(share)),
              color = INK, size = 2.8, family = "RobotoNFL") +
    geom_nfl_logos(data = ord, aes(x = -0.022, y = team, team_abbr = team), height = 0.026, inherit.aes = FALSE) +
    scale_fill_manual(values = cols) +
    scale_x_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 1, 0.2), expand = expansion(mult = c(0.045, 0.01))) +
    labs(
      title = paste0("The ", top$team_nick, " score a touchdown on ", pct(top$share), " of drives; the ", bot$team_nick,
                     " on ", pct(bot$share)),
      subtitle = paste0("How every drive ends: ", kw("touchdown", cols[1]), ", ", kw("field goal", "#4fa873"), ", ",
                        kw("punt", "#7d8791"), ", ", kw("downs or missed kick", "#d9813f"), ", ", kw("turnover", cols[5]),
                        ". Sorted by touchdown share"),
      x = "Share of drives", y = NULL,
      caption = caption(paste0(nrow(dr), " drives; kneel-downs that run out a half are left out. Turnover includes safeties"))) +
    theme_nfl_axis(axis.text.y = element_blank(), panel.grid.major = element_blank())
  save_chart(g, "21_how_drives_end", w = 12, h = 10)
}

chart_motion <- function() {
  j <- plays_epa(data_rds("pbp")) |>
    inner_join(data_rds("ftn") |> select(game_id = nflverse_game_id, play_id = nflverse_play_id, is_motion), by = c("game_id", "play_id")) |>
    filter(qb_kneel == 0, qb_spike == 0)
  d <- j |> group_by(team = posteam) |>
    summarise(rate = mean(is_motion), with = mean(epa[is_motion]), without = mean(epa[!is_motion]), .groups = "drop") |>
    mutate(gain = with - without) |> left_join(team_nicks(), by = "team")
  hi <- d |> slice_max(rate, n = 1); best <- d |> slice_max(gain, n = 1)
  lg <- j |> group_by(is_motion) |> summarise(epa = mean(epa))
  g <- ggplot(d, aes(rate, gain)) +
    geom_hline(yintercept = 0, color = INK, linewidth = 0.45) +
    geom_vline(xintercept = mean(j$is_motion), color = INK, linetype = "dashed", alpha = 0.45, linewidth = 0.5) +
    geom_nfl_logos(aes(team_abbr = team), width = 0.05, alpha = 0.95) +
    annotate("text", x = min(d$rate), y = max(d$gain) * 1.08, label = "Better with motion", hjust = 0, family = "RobotoNFL",
             fontface = "bold", size = 4.2, color = GREEN) +
    annotate("text", x = max(d$rate), y = min(d$gain) * 1.08, label = "Better without it", hjust = 1, family = "RobotoNFL",
             fontface = "bold", size = 4.2, color = RED) +
    scale_x_continuous(labels = percent_format(accuracy = 1), breaks = pretty_breaks(7), expand = expansion(mult = 0.05)) +
    scale_y_continuous(labels = label_number(accuracy = 0.05, style_positive = "plus"), breaks = pretty_breaks(7),
                       expand = expansion(mult = 0.08)) +
    labs(
      title = paste0("The ", hi$team_nick, " send a man in motion on ", pct(hi$rate), " of snaps; it pays most for the ",
                     best$team_nick, " (", signed(best$gain), " EPA per play)"),
      subtitle = paste0("Pre-snap motion rate vs EPA per play with motion minus without. League: ",
                        signed(lg$epa[lg$is_motion]), " with, ", signed(lg$epa[!lg$is_motion]), " without"),
      x = "Share of offensive snaps with pre-snap motion", y = "EPA per play: with motion minus without",
      caption = caption("Motion: FTN Data charting via nflverse")) +
    theme_nfl()
  save_chart(g, "22_pre_snap_motion")
}
