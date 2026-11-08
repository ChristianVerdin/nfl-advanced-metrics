# Pulls one snapshot of every nflverse table the charts use and saves it to data/.
# Charts read only these files, so a rerun of the charts is reproducible offline.

source("R/00_setup.R")

sched <- load_schedules(SEASON) |> filter(game_type == "REG")
done <- sched |> group_by(week) |> summarise(final = all(!is.na(result)), .groups = "drop") |> arrange(week)
last_full <- max(c(0, done$week[cumprod(done$final) == 1]))
stopifnot(last_full >= 1)
set_weeks(1:last_full)
message("weeks with every game final: 1-", last_full)
pbp_all <- load_pbp(SEASON)
pbp <- pbp_all |> filter(season_type == "REG", week %in% WEEKS)

games <- pbp |> distinct(game_id, week)
final_check <- load_schedules(SEASON) |>
  filter(game_type == "REG", week %in% WEEKS) |>
  summarise(scheduled = n(), final = sum(!is.na(result)))
stopifnot(final_check$scheduled == final_check$final, nrow(games) == final_check$final)

saveRDS(pbp, file.path(DATA, "pbp.rds"))
saveRDS(load_schedules(SEASON) |> filter(game_type == "REG", week %in% WEEKS), file.path(DATA, "schedules.rds"))
saveRDS(teams(), file.path(DATA, "teams.rds"))
saveRDS(players(), file.path(DATA, "players.rds"))
saveRDS(load_nextgen_stats(SEASON, stat_type = "rushing") |> filter(season_type == "REG", week %in% WEEKS),
        file.path(DATA, "ngs_rushing.rds"))
saveRDS(load_ftn_charting(SEASON) |> filter(week %in% WEEKS), file.path(DATA, "ftn.rds"))

stamp <- data.frame(
  pulled_at = format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
  season = SEASON, weeks = paste(range(WEEKS), collapse = "-"), games = nrow(games), plays = nrow(pbp),
  nflreadr = as.character(packageVersion("nflreadr")), nflplotR = as.character(packageVersion("nflplotR")),
  ggplot2 = as.character(packageVersion("ggplot2")), R = paste(R.version$major, R.version$minor, sep = ".")
)
write.csv(stamp, file.path(DATA, "SNAPSHOT.csv"), row.names = FALSE)
print(stamp)
