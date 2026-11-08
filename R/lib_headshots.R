# Headshot sprite helpers (Congelio 4.15 method on current packages). Sourced by 02_headshots.R and the live scripts.
suppressPackageStartupMessages({ library(magick); library(cropcircles); library(geomtextpath); library(ggpath) })
HS <- file.path(ROOT, "assets", "headshots")
for (d in c("raw", "circle", "sprite")) dir.create(file.path(HS, d), recursive = TRUE, showWarnings = FALSE)

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


# make (or reuse) the circle and sprite for one player; returns list(circle, sprite)
ensure_sprite <- function(gsis_id, team) {
  sp <- file.path(HS, "sprite", paste0(gsis_id, ".png")); ci <- file.path(HS, "circle", paste0(gsis_id, ".png"))
  if (file.exists(sp) && file.exists(ci)) return(list(circle = ci, sprite = sp))
  pf <- file.path(DATA, "players_full.rds")
  if (!file.exists(pf)) saveRDS(load_players(), pf)
  pl <- readRDS(pf) |> filter(gsis_id == !!gsis_id)
  tm <- data_rds("teams") |> filter(team_abbr == team)
  ring <- ring_col(tm$team_color, tm$team_color2); ncol <- name_col(ring, tm$team_color)
  raw <- fetch_raw(gsis_id, pl$headshot[1])
  ci <- make_circle(gsis_id, raw, pl$display_name[1], ring)
  sp <- make_sprite(gsis_id, ci, last_name(pl$display_name[1]), ncol)
  list(circle = ci, sprite = sp)
}
