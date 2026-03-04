get_all_function_signatures_name_values <- function(choice = c("function_names", "signatures")) {
  choice <- match.arg(choice)
  all_output <- list()

  pkg_names <- rownames(installed.packages())

  for (pkg in pkg_names) {
    # Try loading the namespace without attaching it
    ns <- tryCatch(loadNamespace(pkg), error = function(e) NULL)
    if (is.null(ns)) next

    result <- get_signatures_from_env_returns_name_values(
      env = ns,
      choice = choice,
      pkg_name = pkg
    )

    all_output <- c(all_output, result)

    # Unload the namespace if it wasn’t already loaded
    if (!(pkg %in% loadedNamespaces())) {
      tryCatch(unloadNamespace(pkg), error = function(e) NULL)
    }
  }

  return(all_output)
}

get_signatures_from_env_returns_name_values <- function(env, choice = c("function_names", "signatures"), pkg_name) {
  choice <- match.arg(choice)
  signatures <- list()

  exported_fns <- tryCatch(getNamespaceExports(pkg_name), error = function(e) character(0))

  for (fn in exported_fns) {
    # Skip special/internal operators or functions
    if (grepl("^[\\$%\\[\\.]", fn)) {
      next
    }

    obj <- tryCatch(get(fn, envir = env), error = function(e) NULL)
    if (is.function(obj)) {
      args <- tryCatch(formals(obj), error = function(e) NULL)
      if (!is.null(args)) {
        arg_strs <- mapply(function(arg_name, val) {
          if (is.name(val)) {
            if (identical(deparse(val), "")) {
              return(arg_name)
            } else {
              return(paste0(arg_name, " = ", deparse(val)))
            }
          } else if (is.null(val)) {
            return(arg_name)
          } else {
            return(paste0(arg_name, " = ", paste(deparse(val), collapse = "")))
          }
        }, names(args), args, USE.NAMES = FALSE)

        signature <- paste0(fn, "(", paste(arg_strs, collapse = ", "), ")")

        # Try to get documentation
        doc <- tryCatch(get_function_doc_summary(fn), error = function(e) list(title = NULL, description = NULL))

        # Build the entry
        entry <- list(
          name = fn,
          package = pkg_name,
          title = doc$title,
          description = doc$description
        )

        if (choice == "signatures") {
          entry$signature <- signature
        }

        signatures[[fn]] <- entry
      }
    }
  }

  return(signatures)
}

get_function_doc_summary <- function(func_name) {
  # Get the help topic
  h <- utils::help(func_name)
  if (length(h) == 0) {
    stop("Help file not found for function: ", func_name)
  }
 
  # Get the Rd file object
  rd <- utils:::.getHelpFile(h)
 
  # Extract Title and Description
  title <- NULL
  description <- NULL
 
  for (item in rd) {
    if (attr(item, "Rd_tag") == "\\title") {
      title <- paste(unlist(item), collapse = "")
    } else if (attr(item, "Rd_tag") == "\\description") {
      # Combine and clean the description text
      raw_text <- paste(unlist(item), collapse = "")
      # Extract the first paragraph
      description <- strsplit(raw_text, "\n\n")[[1]][1]
      description <- gsub("\n", " ", description)
    }
  }
 
  list(
    title = title,
    description = description
  )
}
