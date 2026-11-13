# Week-by-week tables for the W charts: official injury reports, weekly roster status (active, inactive, reserve)
# and a fresh nflverse player table (it maps snap-count ids to gsis ids). Saved to data/ like 01_data.R.

source("R/00_setup.R")

inj <- load_injuries(SEASON) |> filter(game_type == "REG")
save_rds(inj, "injuries")
message("injuries: ", nrow(inj), " rows, weeks ", paste(range(inj$week), collapse = "-"))

rw <- load_rosters_weekly(SEASON) |> filter(game_type == "REG")
save_rds(rw, "rosters_weekly")
message("rosters_weekly: ", nrow(rw), " rows, weeks ", paste(range(rw$week), collapse = "-"))

save_rds(load_players(), "players_full")
message("players_full refreshed")

check_clean()
