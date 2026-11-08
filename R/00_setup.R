# Shared setup: packages, fonts, theme and helpers for every chart.
# Run everything from the project root with the nfl_viz conda env:
#   Rscript R/run_all.R   (see README.md for the environment)

suppressPackageStartupMessages({
  library(nflreadr)
  library(nflplotR)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(ggrepel)
  library(ggridges)
  library(ggfx)
  library(scales)
  library(systemfonts)
})

ROOT <- normalizePath(".")
DATA <- file.path(ROOT, "data")
OUT <- file.path(ROOT, "out")
dir.create(DATA, showWarnings = FALSE)
dir.create(OUT, showWarnings = FALSE)

SEASON <- 2026
WEEKS <- 1:8
SPAN <- sprintf("%d regular season, Weeks %d-%d", SEASON, min(WEEKS), max(WEEKS))

options(nflplotR.cache = "filesystem", nflreadr.verbose = FALSE)

font_dir <- file.path(ROOT, "fonts")
register_font(
  name = "RobotoNFL",
  plain = file.path(font_dir, "Roboto-Regular.ttf"),
  bold = file.path(font_dir, "Roboto-Bold.ttf"),
  italic = file.path(font_dir, "Roboto-Italic.ttf"),
  bolditalic = file.path(font_dir, "Roboto-BoldItalic.ttf")
)
register_font(name = "RobotoNFLBlack", plain = file.path(font_dir, "Roboto-Black.ttf"))

BG <- "#f7f7f7"
INK <- "#1b1b1b"
MUTED <- "#5c5c5c"

# Deliberately NOT a complete theme (no theme_minimal()/theme_grey() call): nflplotR/ggpath axis images
# (element_nfl_logo, element_nfl_headshot) are dropped when the plot carries a complete theme under
# ggplot2 4.0.3 / nflplotR 1.7.1 / ggpath 1.1.2, with `+` and with `%+replace%` alike. A partial theme()
# layered on ggplot2's default keeps them; Congelio's book theme is built the same way.
theme_nfl <- function(base_size = 13) {
  theme(
    text = element_text(family = "RobotoNFL", size = base_size, color = INK),
    plot.background = element_rect(fill = BG, color = NA),
    panel.background = element_rect(fill = BG, color = NA),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#dedede", linewidth = 0.35),
    axis.ticks = element_blank(),
    axis.title = element_text(face = "bold", size = rel(0.95)),
    axis.text = element_text(color = "#333333", size = rel(0.9)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(family = "RobotoNFLBlack", size = rel(1.75), hjust = 0, margin = margin(b = 4)),
    plot.subtitle = element_text(size = rel(1.0), color = MUTED, hjust = 0, lineheight = 1.15, margin = margin(b = 14)),
    plot.caption = element_text(size = rel(0.7), color = "#6b6b6b", hjust = 0, lineheight = 1.2, margin = margin(t = 12)),
    plot.margin = margin(20, 26, 14, 20),
    legend.position = "none",
    legend.background = element_rect(fill = BG, color = NA),
    legend.key = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )
}

theme_nfl_axis <- function(..., base_size = 13) theme_nfl(base_size) + theme(...)

HEADS <- "Headshots: NFL.com via nflverse, circle-cropped with cropcircles after the Advanced Headshots Method in Congelio's book (4.15)."
LOGOS <- "Team logos and headshots via nflplotR."

caption <- function(..., span = SPAN, credit = LOGOS) {
  paste(c(..., paste0("Data: nflverse via nflreadr (", span, ")."), credit), collapse = "\n")
}

# wrap the title and subtitle to the chart width (characters per inch measured on these fonts and sizes)
# balanced: a paragraph that needs n lines is wrapped at about 1/n of its length, so no line is a lone word
wrap_text <- function(x, width) {
  para <- strsplit(x, "\n")[[1]]
  paste(unlist(lapply(para, function(p) {
    n <- ceiling(nchar(p) / width)
    strwrap(p, width = if (n <= 1) width else min(width, ceiling(nchar(p) / n) + 4))
  })), collapse = "\n")
}

save_chart <- function(p, name, w = 12, h = 6.75, dpi = 300) {
  lb <- tryCatch(get_labs(p), error = function(e) list())
  if (is.character(lb$subtitle)) p <- p + labs(subtitle = wrap_text(lb$subtitle, floor(w * 11)))
  if (is.character(lb$title)) p <- p + labs(title = wrap_text(lb$title, floor(w * 6.6)))
  path <- file.path(OUT, paste0(name, ".png"))
  ggsave(path, p, width = w, height = h, dpi = dpi, device = ragg::agg_png, bg = BG)
  message("wrote ", path)
  invisible(path)
}

teams <- function() {
  load_teams(current = TRUE) |>
    select(team_abbr, team_name, team_nick, team_color, team_color2, team_division, team_conf)
}

# nflverse player names, keyed by gsis id (newest name wins, per the name-matching rule)
players <- function() {
  load_players() |>
    filter(!is.na(gsis_id)) |>
    transmute(gsis_id, name = display_name, short = coalesce(short_name, display_name), position)
}

data_rds <- function(name) readRDS(file.path(DATA, paste0(name, ".rds")))

last_name <- function(x) sub("^\\S+\\s+", "", x)
main_team <- function(x) names(sort(table(x), decreasing = TRUE))[1]

# headshot sprites built by R/02_headshots.R (circle crop + team ring + name arc), keyed by gsis id
sprites <- function() {
  read.csv(file.path(ROOT, "assets", "headshots", "manifest.csv")) |>
    distinct(gsis_id, .keep_all = TRUE) |>
    transmute(gsis_id, sprite = file.path(ROOT, sprite), circle = file.path(ROOT, circle))
}
ordinal <- function(n) paste0(n, ifelse(n %% 100 %in% 11:13, "th", c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1]))
pct <- function(x, acc = 1) percent(x, accuracy = acc)
signed <- function(x, acc = 0.01) number(x, accuracy = acc, style_positive = "plus")
CALEB <- "00-0039918"
