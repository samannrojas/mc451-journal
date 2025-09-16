# Rewrites the "chapters" section of _quarto.yml to include entries/ in order.
# If files are named like week_02.qmd, week_03.qmd, they are sorted numerically by week.
# Otherwise, falls back to alphabetical.
update_chapters <- function(qyml = "_quarto.yml") {
  if (!file.exists(qyml)) stop("Cannot find _quarto.yml", call. = FALSE)
  y <- readLines(qyml, warn = FALSE)

  beg <- grep("^\\s*# BEGIN AUTO\\s*$", y)
  end <- grep("^\\s*# END AUTO\\s*$", y)
  if (length(beg) != 1 || length(end) != 1 || end <= beg) {
    stop("Auto-managed markers not found or out of order in _quarto.yml", call. = FALSE)
  }

  files <- list.files("entries", pattern = "\\.qmd$", full.names = TRUE)
  if (length(files)) {
    base <- basename(files)
    # try to parse week numbers from "week_XX.qmd"
    wk <- suppressWarnings(as.integer(sub("^week_([0-9]+)\\.qmd$", "\\1", tolower(base))))
    if (all(!is.na(wk))) {
      ord <- order(wk)
      base <- base[ord]
    } else {
      base <- sort(base)
    }
    rel <- file.path("entries", base)
    lines <- paste0("    - ", rel)
  } else {
    rel <- character()
    lines <- "    # (no entries yet)"
  }

  y_new <- c(y[1:beg], lines, y[end:length(y)])
  writeLines(y_new, qyml)
  invisible(rel)
}

if (sys.nframe() == 0L) update_chapters()
