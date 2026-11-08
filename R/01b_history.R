# Multi-season context for pattern charts: 2018-2025 play-by-play (slim columns), expected-touchdown data
# (nflverse ff_opportunity) for 2018-2026, and 2026 snap counts. Saved to data/ like 01_data.R.

source("R/00_setup.R")

keep <- c("game_id", "season", "season_type", "week", "posteam", "defteam", "home_team", "away_team", "play_id",
          "qtr", "down", "ydstogo", "yardline_100", "drive", "fixed_drive", "fixed_drive_result", "drive_start_yard_line",
          "drive_play_count", "play_type", "pass", "rush", "qb_dropback", "pass_attempt", "sack", "complete_pass",
          "air_yards", "yards_gained", "epa", "success", "cpoe", "wp", "id", "name", "passer_player_id",
          "receiver_player_id", "rusher_player_id", "touchdown", "kickoff_attempt", "touchback", "return_yards",
          "total_home_score", "total_away_score", "interception", "fumble_lost", "pass_touchdown", "rush_touchdown")
hist <- load_pbp(2018:2025)
hist <- hist[, intersect(keep, names(hist))]
saveRDS(hist, file.path(DATA, "pbp_2018_2025.rds"))
message("pbp 2018-2025: ", nrow(hist), " rows")

ffo <- load_ff_opportunity(seasons = 2018:SEASON, stat_type = "weekly")
saveRDS(ffo, file.path(DATA, "ff_opportunity.rds"))
message("ff_opportunity: ", nrow(ffo), " rows, seasons ", paste(range(ffo$season), collapse = "-"))

snaps <- load_snap_counts(SEASON) |> filter(game_type == "REG", week %in% WEEKS)
saveRDS(snaps, file.path(DATA, "snaps.rds"))
saveRDS(load_schedules(2018:SEASON), file.path(DATA, "schedules_all.rds"))
message("snaps 2026: ", nrow(snaps), " rows")
