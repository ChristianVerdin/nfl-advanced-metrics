# Builds every chart (or the ones named on the command line) from the data/ snapshot.
#   Rscript R/run_all.R                 # all charts
#   Rscript R/run_all.R epa_landscape   # one chart

source("R/00_setup.R")
for (f in list.files("R", pattern = "^charts_.*\\.R$", full.names = TRUE)) source(f)

fns <- ls(pattern = "^chart_")
want <- commandArgs(trailingOnly = TRUE)
if (length(want)) fns <- intersect(fns, paste0("chart_", want))
for (f in fns) {
  message("== ", f)
  tryCatch(get(f)(), error = function(e) message("FAILED ", f, ": ", conditionMessage(e)))
}
