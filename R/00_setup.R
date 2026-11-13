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
  library(ggtext)
})

ROOT <- normalizePath(".")
DATA <- file.path(ROOT, "data")
# NFL_VIZ_THEME=dark renders every chart on a dark background into out_dark/ (the default light set stays in out/)
DARK <- identical(Sys.getenv("NFL_VIZ_THEME"), "dark")
OUT <- file.path(ROOT, if (DARK) "out_dark" else "out")
dir.create(DATA, showWarnings = FALSE)
dir.create(OUT, showWarnings = FALSE)

# Columns no chart uses can be listed (HIDE_COLS) in an optional, untracked R/local_guard.R. They are dropped
# whenever a table is saved to or read from data/, and check_clean() stops a data script if any are left.
HIDE_COLS <- character()
if (file.exists(file.path(ROOT, "R", "local_guard.R"))) source(file.path(ROOT, "R", "local_guard.R"))
clean_cols <- function(d) {
  if (!is.data.frame(d) || !length(HIDE_COLS)) return(d)
  d[, setdiff(names(d), HIDE_COLS), drop = FALSE]
}
save_rds <- function(d, name) saveRDS(clean_cols(d), file.path(DATA, paste0(name, ".rds")))
check_clean <- function() {
  if (!length(HIDE_COLS)) return(invisible(TRUE))
  for (f in list.files(DATA, pattern = "[.]rds$", full.names = TRUE)) {
    d <- readRDS(f)
    if (is.data.frame(d) && any(names(d) %in% HIDE_COLS)) stop("hidden columns left in ", basename(f))
  }
  message("check_clean: no hidden columns in data/")
  invisible(TRUE)
}

SEASON <- 2026
# Weeks covered = the snapshot's (R/01_data.R picks the last week whose games are all final); 1:8 before any pull
.snap <- file.path(DATA, "SNAPSHOT.csv")
WEEKS <- if (file.exists(.snap)) { r <- as.integer(strsplit(read.csv(.snap)$weeks, "-")[[1]]); r[1]:r[length(r)] } else 1:8
set_weeks <- function(w) {
  WEEKS <<- w; LASTW <<- max(w)
  SPAN <<- sprintf("%d regular season, Weeks %d-%d", SEASON, min(w), LASTW)
  HALF <<- ceiling(LASTW / 2); B3 <<- c(ceiling(LASTW / 3), ceiling(2 * LASTW / 3))
}
wk_span <- function(a, b) if (a == b) paste("Week", a) else paste0("Weeks ", a, "-", b)
set_weeks(WEEKS)

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

BG <- if (DARK) "#14171a" else "#f7f7f7"
INK <- if (DARK) "#ececea" else "#1b1b1b"
MUTED <- if (DARK) "#a4a9b0" else "#5c5c5c"
GRID <- if (DARK) "#2c3136" else "#dedede"
AXIS_TEXT <- if (DARK) "#c4c8cc" else "#333333"

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
    panel.grid.major = element_line(color = GRID, linewidth = 0.35),
    axis.ticks = element_blank(),
    axis.title = element_text(face = "bold", size = rel(0.95)),
    axis.text = element_text(color = AXIS_TEXT, size = rel(0.9)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(family = "RobotoNFLBlack", size = rel(1.75), hjust = 0, margin = margin(b = 4)),
    plot.subtitle = element_markdown(size = rel(1.05), color = MUTED, hjust = 0, lineheight = 1.2, margin = margin(b = 16)),
    plot.caption = element_text(size = rel(0.68), color = "#7a7a7a", hjust = 0, margin = margin(t = 12)),
    plot.margin = margin(20, 26, 14, 20),
    legend.position = "none",
    legend.background = element_rect(fill = BG, color = NA),
    legend.key = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", color = INK)
  )
}

theme_nfl_axis <- function(..., base_size = 13) theme_nfl(base_size) + theme(...)

HEADS <- "Headshots: NFL.com via nflverse, circle-cropped (Congelio 4.15 method)"
LOGOS <- "Logos and headshots via nflplotR"

# one line: an optional short note, the data credit, the image credit
caption <- function(note = NULL, span = SPAN, credit = LOGOS) {
  paste(c(note, paste0("Data: nflverse via nflreadr, ", span), credit), collapse = "   \u00b7   ")
}

GREEN <- if (DARK) "#3fbf7f" else "#1a7f4b"; RED <- if (DARK) "#ee6352" else "#c0392b"
ORANGE <- if (DARK) "#ff6a2b" else "#E64100"; NAVY <- "#0B162A"; GRAY <- "#8a96a3"
# diverging pair and its neutral midpoint (charts 17, 18, 24, 31)
BLUE <- if (DARK) "#4f97e0" else "#2166ac"; WARM <- if (DARK) "#e5773f" else "#c4501b"; MID <- if (DARK) "#2a2f35" else "#ececec"
# a team colour that stays visible on the background: on dark, a near-black primary gives way to the secondary
on_bg <- function(col, alt = NA) {
  if (!DARK) return(col)
  l <- function(h) vapply(h, function(x) if (is.na(x)) 0 else sum(col2rgb(x) * c(0.299, 0.587, 0.114)), numeric(1))
  ifelse(l(col) >= 50, col, ifelse(l(alt) >= 50, alt, "#9aa3ad"))
}
# On dark, a one-colour navy logo disappears: redraw it as a lighter silhouette on top of itself.
if (DARK) {
  DARK_LOGOS <- c(NYG = "#7da2f2")
  geom_nfl_logos <- function(mapping = NULL, data = NULL, ...) {
    only <- function(d) d[rlang::eval_tidy(mapping$team_abbr, d) %in% names(DARK_LOGOS), , drop = FALSE]
    top <- list(...); top$alpha <- NULL   # ggpath ignores colour when alpha is set
    list(nflplotR::geom_nfl_logos(mapping = mapping, data = data, ...),
         do.call(nflplotR::geom_nfl_logos, c(list(mapping = mapping, data = if (is.null(data)) only else only(data),
                                                  colour = DARK_LOGOS[[1]]), top)))
  }
}
# a coloured, bold word for subtitles: the subtitle doubles as the legend
kw <- function(text, col) sprintf("<b style='color:%s'>%s</b>", col, text)

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
  if (is.character(lb$title)) p <- p + labs(title = wrap_text(lb$title, floor(w * 6.0)))
  if (is.character(lb$subtitle)) {
    plain <- gsub("<[^>]+>", "", lb$subtitle)
    if (nchar(plain) > w * 10.5) message("SUBTITLE TOO LONG (", nchar(plain), " > ", floor(w * 10.5), "): ", name)
  }
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

data_rds <- function(name) clean_cols(readRDS(file.path(DATA, paste0(name, ".rds"))))
plays_epa <- function(pbp) pbp |> filter(!is.na(epa), !is.na(posteam), pass == 1 | rush == 1)

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
and_list <- function(x) if (length(x) <= 1) x else paste0(paste(head(x, -1), collapse = ", "), " and ", tail(x, 1))

# Push overlapping headshots apart. Positions are normalised to the plotted ranges (height scaled by the panel aspect),
# and two images of widths wi, wj (fractions of panel width) must sit at least (wi + wj) / 2 * pad apart. Returns the
# shifted centres; draw a thin segment from each true point to its image.
# Where a point label goes on a line chart so the line never runs through it: below a valley, upper-left on a rise,
# upper-right on a fall, above a peak or an end. y must be in x order (one series).
line_label_pos <- function(y) {
  n <- length(y); prev <- c(NA, y[-n]); nxt <- c(y[-1], NA); mid <- !is.na(prev) & !is.na(nxt)
  type <- ifelse(mid & y < prev & y < nxt, "valley", ifelse(mid & prev < y & y < nxt, "rise",
          ifelse(mid & prev > y & y > nxt, "fall", "peak")))
  data.frame(type = type, hj = c(valley = 0.5, rise = 1.15, fall = -0.15, peak = 0.5)[type],
             vj = c(valley = 1.9, rise = -0.5, fall = -0.5, peak = -1.1)[type], row.names = NULL)
}

spread_xy <- function(x, y, w, xlim, ylim, aspect = 1.6, pad = 1.2, iter = 500) {
  u <- (x - xlim[1]) / diff(xlim); v <- (y - ylim[1]) / diff(ylim) / aspect
  n <- length(u)
  for (k in seq_len(iter)) {
    moved <- FALSE
    for (i in seq_len(n - 1)) for (j in (i + 1):n) {
      need <- (w[i] + w[j]) / 2 * pad
      dx <- u[j] - u[i]; dy <- v[j] - v[i]; dist <- sqrt(dx^2 + dy^2)
      if (dist < need) {
        if (dist < 1e-9) { dx <- 1e-3; dy <- 1e-3; dist <- sqrt(2e-6) }
        push <- (need - dist) / 2
        u[i] <- u[i] - dx / dist * push; v[i] <- v[i] - dy / dist * push
        u[j] <- u[j] + dx / dist * push; v[j] <- v[j] + dy / dist * push
        moved <- TRUE
      }
    }
    if (!moved) break
  }
  data.frame(xs = xlim[1] + u * diff(xlim), ys = ylim[1] + v * aspect * diff(ylim))
}
