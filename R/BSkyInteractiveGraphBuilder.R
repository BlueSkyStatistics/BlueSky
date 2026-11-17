##############################################################################
# BlueSky Graph Builder - macOS diagnostics + hardened subprocess launcher
#
# Drop-in replacement for BSkyGraphBuilderSubprocLaunchInternal() and
# BSkyGraphBuilderSubprocLaunchMain() in BSkyInteractiveGraphBuilder.R.
# BSkyGraphBuilderInternalCore() is NOT changed.
#
# What changes vs. the future()/multisession version:
#   1. Runs the Shiny server in its own R process (callr::r_bg), so it no longer
#      shares plan() / the 2 multisession workers with the R-help HTTP server.
#   2. Passes the parent's .libPaths() to the child explicitly.
#   3. The child never launches the browser (launch.browser = FALSE). The PARENT
#      opens it after confirming the port is listening (browseURL, then
#      /usr/bin/open on macOS as a fallback).
#   4. Child stdout/stderr go to a log file; if the child dies or never starts
#      listening, the last log lines are printed instead of a false
#      "launched" message.
#   5. Falls back to a free random port if 2468 is taken.
#   6. supervise = TRUE: child is killed when the BlueSky R session ends, so no
#      orphaned server keeps holding the port.
#
# Requires: callr (install.packages("callr")), httpuv (installed with shiny).
##############################################################################


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
.bsky_gb_required_pkgs <- c("shiny", "ggplot2", "ggthemes", "dplyr", "plotly",
                            "scales", "shinyFiles", "shinyjs", "httpuv", "gridExtra")

# TRUE if nothing is bound to the port
.bsky_gb_port_is_free <- function(port) {
	tryCatch({ s <- serverSocket(port); close(s); TRUE }, error = function(e) FALSE)
}

# TRUE if something accepts connections on 127.0.0.1:port
.bsky_gb_port_is_listening <- function(port) {
	con <- tryCatch(suppressWarnings(
		socketConnection("127.0.0.1", port, open = "r+", blocking = TRUE, timeout = 1)),
		error = function(e) NULL)
	if (is.null(con)) return(FALSE)
	close(con)
	TRUE
}

# Evaluate expr. When quiet = TRUE, hide warnings, package start-up chatter,
# Shiny's "Warning: Error in ..." traces and console cat() noise for the
# duration of expr only; everything is restored on exit (even on error).
.bsky_gb_quietly <- function(expr, quiet = TRUE) {
	if (!isTRUE(quiet)) return(expr)
	nullcon <- file(nullfile(), open = "w")
	sink(nullcon, type = "output")
	sink(nullcon, type = "message")
	on.exit({ sink(type = "message"); sink(type = "output"); close(nullcon) }, add = TRUE)
	suppressWarnings(suppressPackageStartupMessages(suppressMessages(expr)))
}

.bsky_gb_open_browser <- function(url) {
	ok <- tryCatch({ utils::browseURL(url); TRUE }, error = function(e) FALSE)
	if (!ok && identical(Sys.info()[["sysname"]], "Darwin")) {
		ok <- tryCatch(system2("/usr/bin/open", shQuote(url)) == 0, error = function(e) FALSE)
	}
	if (!ok) message("Could not open a browser automatically. Open this URL manually: ", url)
	invisible(ok)
}


# Save the open (non-empty) datasets to an .RData snapshot for the Graph Builder.
# Returns the file path, or NULL (with a message) if there is nothing to plot.
.bsky_gb_save_open_datasets <- function(tempDatasetRDataFilePath = c()) {
	valid_datasets <- Filter(function(df_name) {
		df <- get(df_name, envir = .GlobalEnv)
		if (!is.data.frame(df)) return(FALSE)
		all_empty <- sapply(df, function(col) {
			if (is.character(col)) all(is.na(col) | col == "") else all(is.na(col))
		})
		!all(all_empty)
	}, uadatasets$name)

	if (length(valid_datasets) == 0) {
		message("No valid datasets to launch the Graph Builder i.e. all open datasets are either all empty or NAs: ",
				paste0(uadatasets$name, collapse = ', '))
		return(NULL)
	}
	invalid_datasets <- setdiff(uadatasets$name, valid_datasets)
	if (length(invalid_datasets) > 0) {
		message("The following datasets (either all empty or all NAs) were excluded from the Graph Builder:\n",
				paste(invalid_datasets, collapse = ", "))
	}
	save_tmp_dataset_file <- if (length(tempDatasetRDataFilePath) == 0)
		paste0(tempdir(), '/bsky_open_dataframes.RData') else tempDatasetRDataFilePath
	save(list = valid_datasets, file = save_tmp_dataset_file, envir = .GlobalEnv)
	save_tmp_dataset_file
}


# ---------------------------------------------------------------------------
# Diagnostics: run this on the Mac, from the BlueSky R console, and send the
# output. It shows what a FRESH Rscript worker (what future::multisession
# starts) sees compared to the BlueSky main R session.
# ---------------------------------------------------------------------------
BSkyGraphBuilderDiagnose <- function(port = 2468) {
	cat("=== MAIN BlueSky R session ===\n")
	cat("R              :", R.version.string, "|", R.version$platform, "\n")
	cat("R.home()       :", R.home(), "\n")
	cat("Rscript exists :", file.exists(file.path(R.home("bin"), "Rscript")), "\n")
	cat("interactive()  :", interactive(), "\n")
	cat("options(browser):", format(getOption("browser")), "\n")
	cat("Port", port, "free   :", .bsky_gb_port_is_free(port), "\n")
	if (requireNamespace("future", quietly = TRUE)) {
		cat("future plan    :", paste(class(future::plan()), collapse = ", "),
			"| workers:", future::nbrOfWorkers(), "\n")
	}
	cat(".libPaths():\n"); print(.libPaths())
	main_missing <- .bsky_gb_required_pkgs[!vapply(.bsky_gb_required_pkgs,
		requireNamespace, logical(1), quietly = TRUE)]
	cat("Packages missing in MAIN session:",
		if (length(main_missing)) paste(main_missing, collapse = ", ") else "none", "\n\n")

	lib_out <- tempfile()
	script  <- tempfile(fileext = ".R")
	writeLines(c(
		sprintf("writeLines(.libPaths(), %s)", dQuote(lib_out, FALSE)),
		'cat("R              :", R.version.string, "\\n")',
		'cat("interactive()  :", interactive(), "\\n")',
		'cat("options(browser):", format(getOption("browser")), "\\n")',
		'cat(".libPaths():\\n"); print(.libPaths())',
		'for (v in c("R_HOME","R_LIBS","R_LIBS_USER","R_BROWSER","LANG","LC_ALL","TMPDIR","HOME"))',
		'  cat(sprintf("%-12s = %s\\n", v, Sys.getenv(v, "<unset>")))',
		'cat("PATH =", Sys.getenv("PATH"), "\\n")',
		'cat("/usr/bin/open exists:", file.exists("/usr/bin/open"), "\\n")',
		sprintf("pk <- c(%s)", paste(dQuote(.bsky_gb_required_pkgs, FALSE), collapse = ", ")),
		'for (p in pk) cat(sprintf("%-12s %s\\n", p, if (requireNamespace(p, quietly = TRUE)) as.character(packageVersion(p)) else "** MISSING **"))'
	), script)

	out <- tryCatch(
		system2(file.path(R.home("bin"), "Rscript"), shQuote(script), stdout = TRUE, stderr = TRUE),
		error = function(e) paste("Could not start Rscript:", conditionMessage(e)))
	cat("=== FRESH Rscript worker (what a multisession worker sees) ===\n")
	cat(out, sep = "\n")

	if (file.exists(lib_out)) {
		worker_libs <- readLines(lib_out, warn = FALSE)
		only_main <- setdiff(normalizePath(.libPaths(), mustWork = FALSE),
		                     normalizePath(worker_libs, mustWork = FALSE))
		cat("\nLibrary paths in MAIN but NOT visible to a fresh worker:",
			if (length(only_main)) "" else "none\n", sep = " ")
		if (length(only_main)) print(only_main)
	}
	invisible(NULL)
}


# ---------------------------------------------------------------------------
# Stop a running Graph Builder subprocess (if any)
# ---------------------------------------------------------------------------
BSkyGraphBuilderSubprocStop <- function() {
	if (exists("bsky_graphBuilder_subprocess_handle", envir = .GlobalEnv, inherits = FALSE)) {
		h <- get("bsky_graphBuilder_subprocess_handle", envir = .GlobalEnv)
		if (inherits(h, "process") && h$is_alive()) h$kill()
		rm("bsky_graphBuilder_subprocess_handle", envir = .GlobalEnv)
	}
	invisible(NULL)
}


# ---------------------------------------------------------------------------
# Launcher
# ---------------------------------------------------------------------------
BSkyGraphBuilderSubprocLaunchInternal <- function(tempDatasetRDataFilePath = c(), port_to_use = NULL,
		graph_area_height = 600,
		graphBuilderTitle = c("BlueSky Statistics Interactive Graph Builder"),
		open_browser = TRUE, startup_timeout_secs = 60) {

	if (!requireNamespace("callr", quietly = TRUE)) {
		message("The 'callr' package is required to launch the Graph Builder. Run: install.packages('callr')")
		return(invisible(NULL))
	}

	save_tmp_dataset_file <- .bsky_gb_save_open_datasets(tempDatasetRDataFilePath)
	if (is.null(save_tmp_dataset_file)) return(invisible(NULL))

	# ---- Already running? ----
	if (exists("bsky_graphBuilder_subprocess_handle", envir = .GlobalEnv, inherits = FALSE)) {
		old <- get("bsky_graphBuilder_subprocess_handle", envir = .GlobalEnv)
		if (inherits(old, "process") && old$is_alive()) {
			message("There is a BlueSky Statistics Graph Builder already running",
				if (exists("bsky_graphBuilder_url", envir = .GlobalEnv)) paste0(" at ", get("bsky_graphBuilder_url", envir = .GlobalEnv)) else "",
				". Close and quit that first before launching another one.")
			return(invisible(NULL))
		}
	}

	# ---- Port: preferred one if free, otherwise a random free one ----
	preferred <- if (is.null(port_to_use)) 2468L else as.integer(port_to_use)
	port <- if (.bsky_gb_port_is_free(preferred)) preferred else httpuv::randomPort()
	if (port != preferred) message("Port ", preferred, " is in use; using port ", port, " instead.")

	log_file <- file.path(tempdir(), "bsky_graphbuilder_subprocess.log")
	if (file.exists(log_file)) unlink(log_file)

	# Make the core function self-contained for serialization to the child
	core_fn <- BSkyGraphBuilderInternalCore
	environment(core_fn) <- globalenv()

	proc <- tryCatch(
		callr::r_bg(
			func = function(core, datafile, port, height, title) {
				app <- core(tempDatasetRDataFilePath = datafile,
							graph_area_height = height,
							graphBuilderTitle = title)
				# Browser is opened by the parent process, not from here
				shiny::runApp(app, host = "127.0.0.1", port = port, launch.browser = FALSE)
			},
			args = list(core = core_fn, datafile = save_tmp_dataset_file, port = port,
						height = graph_area_height, title = graphBuilderTitle),
			libpath   = .libPaths(),   # child sees the same packages as BlueSky
			stdout    = log_file,
			stderr    = "2>&1",
			supervise = TRUE           # child dies with the BlueSky R session
		),
		error = function(e) {
			message("Error launching BlueSky Statistics Graph Builder: ", conditionMessage(e))
			NULL
		})
	if (is.null(proc)) return(invisible(NULL))

	# ---- Wait until the server accepts connections (or the child dies) ----
	ready <- FALSE
	deadline <- Sys.time() + startup_timeout_secs
	while (Sys.time() < deadline) {
		if (!proc$is_alive()) break
		if (.bsky_gb_port_is_listening(port)) { ready <- TRUE; break }
		Sys.sleep(0.25)
	}

	if (!ready) {
		still_alive <- proc$is_alive()
		if (still_alive) proc$kill()
		log_tail <- if (file.exists(log_file)) paste(utils::tail(readLines(log_file, warn = FALSE), 30), collapse = "\n") else "(no log written)"
		message("BlueSky Statistics Graph Builder failed to start (",
				if (still_alive) paste0("no response within ", startup_timeout_secs, "s") else "subprocess exited",
				").\nLog file: ", log_file, "\n--- last log lines ---\n", log_tail)
		return(invisible(NULL))
	}

	url <- sprintf("http://127.0.0.1:%d", port)
	assign("bsky_graphBuilder_subprocess_handle", proc, envir = .GlobalEnv)
	assign("bsky_graphBuilder_url", url, envir = .GlobalEnv)

	if (isTRUE(open_browser)) .bsky_gb_open_browser(url)
	cat(sprintf("BlueSky Statistics Graph Builder app launched in a browser. See %s in your browser.\n", url))
	invisible(url)
}


##############################################################################
# R HTML help server in a callr child process (replaces the future-based
# BSkyGetRHelpParallelHTTPServerPortNumberHTMLHelp). Same signature and return value.
# Polls for the port file instead of a fixed Sys.sleep(1).
##############################################################################
BSkyGetRHelpParallelHTTPServerPortNumberHTMLHelp <- function(restartHelpServer = FALSE, stopHelpServer = FALSE,
		startup_timeout_secs = 30) {
	port_number <- 0

	if (BSkyGetRHelpHTTPServer() != "R_parallel") return(invisible(0))

	kill_existing <- function() {
		if (exists("bsky_help_server_handle", envir = .GlobalEnv, inherits = FALSE)) {
			h <- get("bsky_help_server_handle", envir = .GlobalEnv)
			if (inherits(h, "process") && h$is_alive()) h$kill()
			rm("bsky_help_server_handle", envir = .GlobalEnv)
		}
		if (exists("uadatasets.sk")) uadatasets.sk$BSkyParallelHelpHTTPServerPortNumber <- 0
	}

	if (isTRUE(stopHelpServer)) { kill_existing(); return(invisible(0)) }
	if (isTRUE(restartHelpServer)) kill_existing()

	# Already running and alive?
	if (exists("bsky_help_server_handle", envir = .GlobalEnv, inherits = FALSE) &&
		exists("uadatasets.sk") && exists("BSkyParallelHelpHTTPServerPortNumber", envir = uadatasets.sk)) {
		h <- get("bsky_help_server_handle", envir = .GlobalEnv)
		if (inherits(h, "process") && h$is_alive() &&
			as.numeric(uadatasets.sk$BSkyParallelHelpHTTPServerPortNumber) > 0) {
			return(invisible(uadatasets.sk$BSkyParallelHelpHTTPServerPortNumber))
		}
	}

	if (!requireNamespace("callr", quietly = TRUE)) {
		message("The 'callr' package is required for the R help server. Run: install.packages('callr')")
		return(invisible(0))
	}

	port_file <- tempfile("bsky_help_port_")
	log_file  <- file.path(tempdir(), "bsky_help_server_subprocess.log")
	sleep_secs <- BSkyGetHelpProcessSleepSeconds()

	proc <- tryCatch(
		callr::r_bg(
			func = function(port_file, sleep_secs) {
				options(help_type = "html")
				p <- tools::startDynamicHelp(NA)
				writeLines(as.character(p), port_file)
				# Keep this process alive; Sys.sleep services the HTTP server on Unix
				repeat Sys.sleep(sleep_secs)
			},
			args = list(port_file = port_file, sleep_secs = sleep_secs),
			libpath = .libPaths(), stdout = log_file, stderr = "2>&1", supervise = TRUE),
		error = function(e) { message("Could not start R help server: ", conditionMessage(e)); NULL })
	if (is.null(proc)) return(invisible(0))

	deadline <- Sys.time() + startup_timeout_secs
	while (Sys.time() < deadline && proc$is_alive() && !file.exists(port_file)) Sys.sleep(0.2)

	if (file.exists(port_file)) {
		port_number <- suppressWarnings(as.numeric(readLines(port_file, warn = FALSE)[1]))
		if (is.na(port_number)) port_number <- 0
	}
	if (port_number == 0) {
		if (proc$is_alive()) proc$kill()
		log_tail <- if (file.exists(log_file)) paste(utils::tail(readLines(log_file, warn = FALSE), 20), collapse = "\n") else "(no log)"
		message("R help server failed to start. Log: ", log_file, "\n", log_tail)
		return(invisible(0))
	}

	assign("bsky_help_server_handle", proc, envir = .GlobalEnv)
	if (exists("uadatasets.sk")) uadatasets.sk$BSkyParallelHelpHTTPServerPortNumber <- port_number
	invisible(port_number)
}


##############################################################################
# Fallback: run the Graph Builder IN the BlueSky R process (no subprocess).
# BlueSky is blocked until the user closes the Graph Builder (Close & Quit
# button or closing the browser tab). Needs no callr/future/Rscript.
##############################################################################
BSkyGraphBuilderBlockingLaunch <- function(tempDatasetRDataFilePath = c(), port_to_use = NULL,
		graph_area_height = 600,
		graphBuilderTitle = c("BlueSky Statistics Interactive Graph Builder"),
		open_browser = TRUE, show_warnings = FALSE) {

	save_tmp_dataset_file <- .bsky_gb_save_open_datasets(tempDatasetRDataFilePath)
	if (is.null(save_tmp_dataset_file)) return(invisible(NULL))

	preferred <- if (is.null(port_to_use)) 2468L else as.integer(port_to_use)
	port <- if (.bsky_gb_port_is_free(preferred)) preferred else httpuv::randomPort()
	if (port != preferred) message("Port ", preferred, " is in use; using port ", port, " instead.")

	quiet <- !isTRUE(show_warnings)

	# The Core loads the .RData into .GlobalEnv; remember what was there so the
	# snapshot copies don't linger in the BlueSky session afterwards.
	before <- ls(envir = .GlobalEnv, all.names = TRUE)

	app <- .bsky_gb_quietly(
		BSkyGraphBuilderInternalCore(tempDatasetRDataFilePath = save_tmp_dataset_file,
									 graph_area_height = graph_area_height,
									 graphBuilderTitle = graphBuilderTitle),
		quiet = quiet)

	cat(sprintf("BlueSky Statistics Graph Builder running at http://127.0.0.1:%d (BlueSky is paused until you close it).\n", port))
	on.exit({
		added <- setdiff(ls(envir = .GlobalEnv, all.names = TRUE), before)
		added <- added[added == "bsky_already_loaded_df_names"]   # only remove our own marker
		if (length(added)) rm(list = added, envir = .GlobalEnv)
	}, add = TRUE)

	tryCatch(
		.bsky_gb_quietly(
			shiny::runApp(app, host = "127.0.0.1", port = port,
				launch.browser = if (isTRUE(open_browser)) function(url) .bsky_gb_open_browser(url) else FALSE),
			quiet = quiet),
		error = function(e) message("Graph Builder error: ", conditionMessage(e)))

	cat("BlueSky Statistics Graph Builder closed.\n")
	invisible(NULL)
}


BSkyGraphBuilderSubprocLaunchMain <- function(tempDatasetRDataFilePath = c(), port_to_use = NULL,
		graph_area_height = 600,
		graphBuilderTitle = c("BlueSky Statistics Interactive Graph Builder"),
		open_browser = TRUE, launch_as_subprocess = TRUE, show_warnings = FALSE) {
	if (isTRUE(launch_as_subprocess)) {
		# Subprocess output already goes to a log file, so show_warnings has no
		# console effect here; the log is still available for troubleshooting.
		BSkyGraphBuilderSubprocLaunchInternal(tempDatasetRDataFilePath = tempDatasetRDataFilePath,
			port_to_use = port_to_use, graph_area_height = graph_area_height,
			graphBuilderTitle = graphBuilderTitle, open_browser = open_browser)
	} else {
		BSkyGraphBuilderBlockingLaunch(tempDatasetRDataFilePath = tempDatasetRDataFilePath,
			port_to_use = port_to_use, graph_area_height = graph_area_height,
			graphBuilderTitle = graphBuilderTitle, open_browser = open_browser,
			show_warnings = show_warnings)
	}
}
