# Extra nflverse tables for the 16-32 charts: Next Gen Stats passing and receiving, Pro Football Reference advanced
# stats (pressures, blitzes), weekly player stats, draft results and rosters. Saved to data/ like 01_data.R.

source("R/00_setup.R")

grab <- function(name, expr) {
  d <- tryCatch(expr, error = function(e) { message("FAILED ", name, ": ", conditionMessage(e)); NULL })
  if (!is.null(d)) { saveRDS(d, file.path(DATA, paste0(name, ".rds"))); message(name, ": ", nrow(d), " rows") }
}
reg <- function(d) d |> filter(season_type == "REG", week %in% WEEKS)

grab("ngs_passing", load_nextgen_stats(SEASON, stat_type = "passing") |> reg())
grab("ngs_receiving", load_nextgen_stats(SEASON, stat_type = "receiving") |> reg())
for (s in c("pass", "rush", "rec", "def")) {
  grab(paste0("pfr_", s), load_pfr_advstats(SEASON, stat_type = s, summary_level = "week") |>
         filter(game_type == "REG", week %in% WEEKS))
}
grab("player_stats", load_player_stats(SEASON) |> reg())
grab("team_stats", load_team_stats(SEASON) |> reg())
grab("draft_picks", load_draft_picks())
grab("rosters", load_rosters(SEASON))
