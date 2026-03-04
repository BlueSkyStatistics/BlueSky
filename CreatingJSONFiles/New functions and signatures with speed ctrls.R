`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

build_exact_name_hint_json <- function(
  out_file = "r_hints_exact.json",
  include_description = TRUE,

  # speed controls
  max_functions_total = Inf,     # e.g. 100
  max_functions_per_pkg = Inf,   # e.g. 50
  packages = NULL,              # NULL = all installed, or c("stats","utils")
  seed = 1
) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Please install jsonlite: install.packages('jsonlite')")
  }

  .trim <- function(x) {
    x <- gsub("[\r\n\t]+", " ", x)
    x <- gsub("\\s{2,}", " ", x)
    trimws(x)
  }

  # Signature from formals() (no "NULL" ever)
  .signature_from_formals <- function(fn_name, fn_obj) {
    fmls <- tryCatch(formals(fn_obj), error = function(e) NULL)
    if (is.null(fmls)) return(paste0(fn_name, "(...)"))

    arg_strs <- mapply(function(arg_name, val) {
      if (is.name(val)) {
        dv <- deparse(val)
        if (identical(dv, "")) arg_name else paste0(arg_name, " = ", dv)
      } else if (is.null(val)) {
        arg_name
      } else {
        paste0(arg_name, " = ", paste(deparse(val), collapse = ""))
      }
    }, names(fmls), fmls, USE.NAMES = FALSE)

    paste0(fn_name, "(", paste(arg_strs, collapse = ", "), ")")
  }

  # Doc extraction using YOUR proven-working Rd_tag approach
  .get_function_doc_summary <- function(func_name, pkg_name = NULL) {
    if (!include_description) return(list(title = "", description = ""))

    # 1) Try package-specific help (more precise)
    h <- NULL
    if (!is.null(pkg_name)) {
      h <- suppressWarnings(try(utils::help(func_name, package = pkg_name), silent = TRUE))
      if (inherits(h, "try-error") || length(h) == 0) h <- NULL
    }

    # 2) Fallback: global help (your old behavior)
    if (is.null(h)) {
      h <- suppressWarnings(try(utils::help(func_name), silent = TRUE))
      if (inherits(h, "try-error") || length(h) == 0) {
        return(list(title = "", description = ""))
      }
    }

    rd <- suppressWarnings(try(utils:::.getHelpFile(h), silent = TRUE))
    if (inherits(rd, "try-error") || is.null(rd)) return(list(title = "", description = ""))

    title <- ""
    description <- ""

    for (item in rd) {
      tag <- attr(item, "Rd_tag")
      if (identical(tag, "\\title")) {
        title <- .trim(paste(unlist(item), collapse = ""))
      } else if (identical(tag, "\\description")) {
        raw_text <- paste(unlist(item), collapse = "")
        # first paragraph-ish
        desc1 <- strsplit(raw_text, "\n\n", fixed = TRUE)[[1]][1]
        desc1 <- gsub("\n", " ", desc1)
        description <- .trim(desc1)
      }
    }

    list(title = title %||% "", description = description %||% "")
  }

  # Packages
  pkgs <- if (is.null(packages)) rownames(utils::installed.packages()) else unique(packages)

  # Build candidate list (pkg, exported function)
  candidates <- do.call(rbind, lapply(pkgs, function(pkg) {
    ns <- suppressWarnings(try(loadNamespace(pkg), silent = TRUE))
    if (inherits(ns, "try-error") || is.null(ns)) return(NULL)

    exports <- suppressWarnings(try(getNamespaceExports(pkg), silent = TRUE))
    if (inherits(exports, "try-error") || !length(exports)) return(NULL)

    # optional per-package cap
    if (is.finite(max_functions_per_pkg) && length(exports) > max_functions_per_pkg) {
      set.seed(seed + sum(utf8ToInt(pkg)))
      exports <- sample(exports, max_functions_per_pkg)
    }

    data.frame(pkg = pkg, nm = exports, stringsAsFactors = FALSE)
  }))

  if (is.null(candidates) || nrow(candidates) == 0) {
    stop("No exported functions found for selected packages.")
  }

  # optional global cap
  if (is.finite(max_functions_total) && nrow(candidates) > max_functions_total) {
    set.seed(seed)
    candidates <- candidates[sample(seq_len(nrow(candidates)), max_functions_total), , drop = FALSE]
  }

  by_name <- list()

  for (i in seq_len(nrow(candidates))) {
    pkg <- candidates$pkg[i]
    nm  <- candidates$nm[i]

    # skip special/internal operators like your old code
    if (grepl("^[\\$%\\[\\.]", nm)) next

    obj <- suppressWarnings(try(getExportedValue(pkg, nm), silent = TRUE))
    if (inherits(obj, "try-error") || !is.function(obj)) next

    doc <- .get_function_doc_summary(nm, pkg_name = pkg)

    entry <- list(
      name = nm,
      package = pkg,
      title = doc$title %||% "",
      description = doc$description %||% "",
      signature = .signature_from_formals(nm, obj)
    )

    if (is.null(by_name[[nm]])) by_name[[nm]] <- list(entry)
    else by_name[[nm]] <- c(by_name[[nm]], list(entry))
  }

  jsonlite::write_json(
    by_name,
    path = out_file,
    auto_unbox = TRUE,
    pretty = FALSE,
    null = "null"
  )

  invisible(by_name)
}
