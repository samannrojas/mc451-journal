new_journal_entry(week = 3)new_journal_entry <- function(
	week,
	date = Sys.Date(),
	template = "entry_template.qmd",
	prompts_file = NULL
) {
	# 0) Helpers ---------------------------------------------------------------
	trim <- function(x) sub("\\s+$", "", sub("^\\s+", "", x))
	# remove one pair of matching outer quotes if present: "…", ‘…’, “…” or '…'
	trim_outer_quotes <- function(x) {
		x <- trim(x)
		if ((startsWith(x, "\"") && endsWith(x, "\"")) ||
				(startsWith(x, "“") && endsWith(x, "”")) ||
				(startsWith(x, "‘") && endsWith(x, "’")) ||
				(startsWith(x, "'") && endsWith(x, "'"))) {
			return(substring(x, 2L, nchar(x) - 1L))
		}
		x
	}
	# YAML single-quoted scalar (escape ' by doubling)
	yaml_sq <- function(x) paste0("'", gsub("'", "''", gsub("\r", "", x, fixed = TRUE), fixed = TRUE), "'")

	# 1) Locate prompts --------------------------------------------------------
	if (is.null(prompts_file)) {
		if (file.exists("prompts_mc451.csv")) prompts_file <- "prompts_mc451.csv"
		if (file.exists("prompts_mc501.csv")) prompts_file <- "prompts_mc501.csv"
	}
	if (is.null(prompts_file) || !file.exists(prompts_file)) {
		stop("Could not locate prompts file (prompts_mc451.csv or prompts_mc501.csv).", call. = FALSE)
	}

	pdat <- tryCatch(utils::read.csv(prompts_file, stringsAsFactors = FALSE), error = function(e) NULL)
	if (is.null(pdat) || !all(c("week", "prompt") %in% names(pdat))) {
		stop("Prompts file must have at least columns: week, prompt", call. = FALSE)
	}

	week <- as.integer(week)
	week_prompts <- pdat[pdat$week == week, , drop = FALSE]
	if (nrow(week_prompts) != 3) {
		stop(sprintf("Week %d must have exactly 3 prompts in %s (found %d).",
								 week, basename(prompts_file), nrow(week_prompts)), call. = FALSE)
	}

	# 2) Derive course + word range -------------------------------------------
	course <- if (grepl("mc501", basename(prompts_file), ignore.case = TRUE)) "mc501" else "mc451"
	word_min <- if (course == "mc501") 450L else 250L
	word_max <- if (course == "mc501") 500L else 300L

	# 3) Prepare prompts (strip outer quotes once) ----------------------------
	p1 <- trim_outer_quotes(week_prompts$prompt[1])
	p2 <- trim_outer_quotes(week_prompts$prompt[2])
	p3 <- trim_outer_quotes(week_prompts$prompt[3])

	# 4) Read template and split YAML/header ----------------------------------
	if (!file.exists(template)) stop("Cannot find entry_template.qmd in this folder.", call. = FALSE)
	lines <- readLines(template, warn = FALSE)

	# Find the YAML front matter between the first two '---' lines
	dash_idx <- which(trim(lines) == "---")
	if (length(dash_idx) < 2) stop("Template is missing YAML front matter fences '---'.", call. = FALSE)
	head_start <- dash_idx[1]
	head_end   <- dash_idx[2]

	body <- lines[(head_end + 1L):length(lines)]

	# 5) Build a fresh YAML header (no regex edits) ---------------------------
	day <- as.character(as.Date(date))
	header <- c(
		"---",
		sprintf('title: "%s"', day),
		"format:",
		"  html: default",
		"  pdf: default",
		"params:",
		sprintf('  course: "%s"', course),
		sprintf("  word_min: %d", word_min),
		sprintf("  word_max: %d", word_max),
		sprintf("  p1: %s", yaml_sq(p1)),
		sprintf("  p2: %s", yaml_sq(p2)),
		sprintf("  p3: %s", yaml_sq(p3)),
		"---"
	)

	# 6) Write entry -----------------------------------------------------------
	out_dir <- "entries"
	if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
	out <- file.path(out_dir, paste0(day, ".qmd"))

	writeLines(c(header, body), out, useBytes = TRUE)
	message("Created: ", out)

	# 7) Update the book TOC ---------------------------------------------------
	updater <- file.path("scripts", "update_chapters.R")
	if (file.exists(updater)) {
		source(updater)
		if (exists("update_chapters", mode = "function")) {
			update_chapters("_quarto.yml")
		} else {
			warning("update_chapters() not found after sourcing update_chapters.R; skipping TOC update.", call. = FALSE)
		}
	} else {
		warning("scripts/update_chapters.R not found; skipping TOC update.", call. = FALSE)
	}

	invisible(out)
}

new_journal_entry(week = 6# Run directly: prompt when sourced interactively
if (sys.nframe() == 0L) {
	pf <- if (file.exists("prompts_mc451.csv")) "prompts_mc451.csv"
	else if (file.exists("prompts_mc501.csv")) "prompts_mc501.csv"
	else NULL
	if (is.null(pf)) stop("Could not find prompts_mc451.csv or prompts_mc501.csv in the current folder.", call. = FALSE)
	cat("Enter the week number (2–14): ")
	wk <- suppressWarnings(as.integer(readLines(con = stdin(), n = 1L)))
	if (is.na(wk)) stop("Invalid week number.", call. = FALSE)
	new_journal_entry(week = wk, prompts_file = pf)
}
