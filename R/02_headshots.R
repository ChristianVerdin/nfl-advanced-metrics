# Headshot "sprites" for chosen player groups: the Advanced Headshots Method from Congelio's book (section 4.15),
# rebuilt on current packages.
#   book                                   now
#   cropcircles::circle_crop(url)          face-centred square pre-crop (magick), then circle_crop() with its own
#                                          border_size / border_colour / bg_fill (cropcircles >= 0.2.4), replacing
#                                          the book's hand-drawn white-disc border() function
#   ggimage::geom_image(image_fun=border)  ggpath::geom_from_path (the nflverse image geom)
#   geomtextpath name arc                  same idea, name in the team colour
# Outputs (assets/headshots/): raw/<id>.png (NFL.com original), circle/<id>.png, sprite/<id>.png (circle + name arc),
# manifest.csv, and one contact sheet per group in out/headshots/ to check every crop by eye.

source("R/00_setup.R")
suppressPackageStartupMessages({ library(magick); library(cropcircles); library(geomtextpath); library(ggpath) })

HS <- file.path(ROOT, "assets", "headshots")
for (d in c("raw", "circle", "sprite")) dir.create(file.path(HS, d), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT, "headshots"), showWarnings = FALSE)

pf <- file.path(DATA, "players_full.rds")
if (!file.exists(pf)) saveRDS(load_players(), pf)
plf <- readRDS(pf) |> filter(!is.na(gsis_id)) |>
  transmute(gsis_id, name = display_name, position, headshot, pfr_id)
tm <- data_rds("teams")
p <- data_rds("pbp")

# ---- groups ---------------------------------------------------------------------------------------------------
TOP <- c(WR = 32, RB = 24, TE = 16)

starting_qbs <- function() {
  db <- p |> filter(qb_dropback == 1, !is.na(id))
  last_game <- db |> group_by(posteam) |> filter(week == max(week)) |> ungroup()
  last_game |> count(team = posteam, gsis_id = id) |> group_by(team) |> slice_max(n, n = 1, with_ties = FALSE) |>
    ungroup() |> transmute(group = "Starting QBs", team, gsis_id, stat = "started the team's latest game")
}

rec_yards <- p |> filter(complete_pass == 1, !is.na(receiver_player_id)) |>
  group_by(gsis_id = receiver_player_id) |>
  summarise(team = names(sort(table(posteam), TRUE))[1], rec = sum(yards_gained, na.rm = TRUE), .groups = "drop")
rush_yards <- p |> filter(rush == 1, !is.na(rusher_player_id)) |>
  group_by(gsis_id = rusher_player_id) |>
  summarise(team_r = names(sort(table(posteam), TRUE))[1], rush = sum(yards_gained, na.rm = TRUE), .groups = "drop")

top_by_position <- function(pos, n, label) {
  full_join(rec_yards, rush_yards, by = "gsis_id") |>
    mutate(team = coalesce(team, team_r), rec = coalesce(rec, 0), rush = coalesce(rush, 0),
           yards = if (pos == "RB") rec + rush else rec) |>
    inner_join(plf |> filter(position %in% if (pos == "RB") c("RB", "FB") else pos) |> select(gsis_id), by = "gsis_id") |>
    slice_max(yards, n = n, with_ties = FALSE) |>
    transmute(group = label, team, gsis_id,
              stat = paste0(comma(yards), if (pos == "RB") " scrimmage yds" else " receiving yds"))
}

bears_offense <- function() {
  sn <- data_rds("snaps") |> filter(team == "CHI", position %in% c("QB", "RB", "FB", "WR", "TE"), offense_snaps > 0) |>
    group_by(pfr_id = pfr_player_id) |> summarise(snaps = sum(offense_snaps), .groups = "drop") |>
    inner_join(plf |> filter(!is.na(pfr_id)) |> select(pfr_id, gsis_id), by = "pfr_id")
  sn |> transmute(group = "Bears offense", team = "CHI", gsis_id, stat = paste0(comma(snaps), " offensive snaps"))
}

td_outliers <- function(n = 8) {
  readRDS(file.path(DATA, "ff_opportunity.rds")) |> filter(season == SEASON, week %in% WEEKS, position != "QB") |>
    group_by(gsis_id = player_id) |>
    summarise(team = last(posteam), td = sum(coalesce(rec_touchdown, 0) + coalesce(rush_touchdown, 0)),
              xtd = sum(coalesce(rec_touchdown_exp, 0) + coalesce(rush_touchdown_exp, 0)), .groups = "drop") |>
    filter(xtd >= 2) |> mutate(diff = td - xtd) |> arrange(desc(diff)) |>
    (\(x) bind_rows(head(x, n), tail(x, n)))() |>
    transmute(group = "2026 TD outliers", team, gsis_id, stat = paste0(td, " TD on ", round(xtd, 1), " expected"))
}

roster <- bind_rows(starting_qbs(), top_by_position("WR", TOP[["WR"]], "Top WRs"),
                    top_by_position("RB", TOP[["RB"]], "Top RBs"), top_by_position("TE", TOP[["TE"]], "Top TEs"),
                    bears_offense(), td_outliers()) |>
  left_join(plf, by = "gsis_id") |>
  left_join(tm |> select(team = team_abbr, team_color, team_color2), by = "team")

# ---- images ---------------------------------------------------------------------------------------------------
lum <- function(hex) vapply(hex, function(h) sum(col2rgb(h) * c(0.299, 0.587, 0.114)), numeric(1))
# primary colour unless it is near-black (CHI navy, LV and PIT black): then the secondary (orange, silver, gold)
ring_col <- function(c1, c2) ifelse(lum(c1) < 25 & lum(c2) < 190, c2, c1)
# names stay readable on a light background: a light ring gets its name in the dark primary
name_col <- function(ring, c1) ifelse(lum(ring) > 160, c1, ring)

fetch_raw <- function(id, url) {
  f <- file.path(HS, "raw", paste0(id, ".png"))
  if (!file.exists(f) && !is.na(url)) tryCatch(image_write(image_read(url), f, format = "png"),
                                                error = function(e) message("no headshot for ", id, ": ", conditionMessage(e)))
  if (file.exists(f)) f else NA_character_
}

# NFL.com headshots are 3400x2450 with the face centred horizontally at ~40% of the height. A plain centre crop
# keeps the chest and clips the hair, so take a square of 80% of the height starting 4% from the top first.
face_square <- function(raw, id) {
  im <- image_read(raw); i <- image_info(im)
  s <- round(0.80 * i$height); x0 <- round((i$width - s) / 2); y0 <- round(0.04 * i$height)
  f <- file.path(tempdir(), paste0(id, "_sq.png"))
  image_write(image_scale(image_crop(im, geometry_area(s, s, x0, y0)), "600x600"), f, format = "png")
  f
}

placeholder <- function(id, name, col) {
  ini <- paste(substr(strsplit(name, " ")[[1]], 1, 1), collapse = "")
  f <- file.path(tempdir(), paste0(id, "_ph.png"))
  g <- ggplot() + annotate("text", x = 0, y = 0, label = ini, family = "RobotoNFLBlack", size = 60, color = col) +
    xlim(-1, 1) + ylim(-1, 1) + theme_void()
  ggsave(f, g, width = 2, height = 2, dpi = 300, device = ragg::agg_png, bg = "white")
  f
}

make_circle <- function(id, raw, name, ring) {
  out <- file.path(HS, "circle", paste0(id, ".png"))
  src <- if (!is.na(raw)) face_square(raw, id) else placeholder(id, name, ring)
  circle_crop(src, to = out, border_size = 14, border_colour = ring, bg_fill = "white")
  out
}

make_sprite <- function(id, circle, label, col) {
  # left-to-right arc: a right-to-left path makes geomtextpath flip the text upright, which also flips which
  # side counts as "above", and the name lands inside the ring
  t <- seq(0.95, 0.05, length.out = 120) * pi
  arc <- data.frame(x = 0.86 * cos(t), y = 0.86 * sin(t))
  size <- min(6.4, 60 / nchar(label))
  g <- ggplot() +
    geom_from_path(aes(x = 0, y = 0, path = circle), width = 0.66) +
    geom_textpath(data = arc, aes(x, y, label = toupper(label)), linecolor = NA, color = col, size = size,
                  fontface = "bold", family = "RobotoNFL", vjust = -0.1, hjust = 0.5, text_only = TRUE) +
    scale_x_continuous(limits = c(-1.25, 1.25)) + scale_y_continuous(limits = c(-1.25, 1.25)) +
    coord_equal() + theme_void()
  out <- file.path(HS, "sprite", paste0(id, ".png"))
  ggsave(out, g, width = 2, height = 2, dpi = 300, device = ragg::agg_png, bg = "transparent")
  out
}

players_u <- roster |> distinct(gsis_id, .keep_all = TRUE) |>
  mutate(ring = ring_col(team_color, team_color2), ncol = name_col(ring, team_color), label = last_name(name))
players_u$raw <- mapply(fetch_raw, players_u$gsis_id, players_u$headshot)
players_u$circle <- mapply(make_circle, players_u$gsis_id, players_u$raw, players_u$name, players_u$ring)
players_u$sprite <- mapply(make_sprite, players_u$gsis_id, players_u$circle, players_u$label, players_u$ncol)

manifest <- roster |> select(group, team, gsis_id, name, position, stat, headshot_url = headshot) |>
  left_join(players_u |> select(gsis_id, has_photo = raw, circle, sprite), by = "gsis_id") |>
  mutate(has_photo = !is.na(has_photo), circle = sub(paste0(ROOT, "/"), "", circle), sprite = sub(paste0(ROOT, "/"), "", sprite))
write.csv(manifest, file.path(HS, "manifest.csv"), row.names = FALSE)

contact_sheet <- function(files, per_row = 8, px = 220, pad = 8) {
  ims <- lapply(files, function(f) image_border(image_background(image_scale(image_read(f), paste0(px, "x", px)), BG),
                                                BG, paste0(pad, "x", pad)))
  rows <- split(ims, ceiling(seq_along(ims) / per_row))
  blank <- image_blank(px + 2 * pad, px + 2 * pad, BG)
  rows <- lapply(rows, function(r) { while (length(r) < per_row) r <- c(r, list(blank)); image_append(do.call(c, r)) })
  image_background(image_append(do.call(c, rows), stack = TRUE), BG)
}

for (g in unique(manifest$group)) {
  m <- manifest |> filter(group == g)
  f <- file.path(OUT, "headshots", paste0("contact_", gsub(" ", "_", tolower(g)), ".png"))
  image_write(contact_sheet(file.path(ROOT, m$sprite)), f, format = "png")
  message(g, ": ", nrow(m), " players, ", sum(!m$has_photo), " without a photo -> ", f)
}
