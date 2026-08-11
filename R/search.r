.bskyReplaceInit <- function() {
  if (!exists('.bskyRE', envir = globalenv(), inherits = FALSE)) {
    e <- new.env(parent = emptyenv())
    e$tokens <- list()
    e$order  <- character(0)
    e$loadId <- list()
    assign('.bskyRE', e, envir = globalenv())
  }
  get('.bskyRE', envir = globalenv())
}

.bskyReplaceLoadId <- function(datasetName) {
  e <- .bskyReplaceInit()
  id <- e$loadId[[datasetName]]
  if (is.null(id)) { id <- 1L; e$loadId[[datasetName]] <- id }
  id
}

.bskyReplaceClear <- function(datasetName) {
  e <- .bskyReplaceInit()
  cur <- e$loadId[[datasetName]]
  if (is.null(cur)) cur <- 1L
  e$loadId[[datasetName]] <- cur + 1L
  if (length(e$tokens)) {
    keep <- vapply(e$tokens, function(x) !identical(x$dataset, datasetName), logical(1))
    e$tokens <- e$tokens[keep]
    e$order  <- e$order[e$order %in% names(e$tokens)]
  }
  invisible(TRUE)
}

.bskyReplaceApplyAll <- function(datasetName, rows, cols, replacement, token,
                                  maxStack = 20L, term = NULL, mode = "whole", ignoreCase = TRUE) {
  e  <- .bskyReplaceInit()
  df <- get(datasetName, envir = globalenv())
  ucols <- unique(cols)
  # Partial mode substitutes only the matched text within each cell's existing value
  # (literal, case sensitivity per ignoreCase -- mirrors BSkySearchDataset's own matching),
  # leaving the rest of the cell's content intact. Whole mode (default) keeps the prior
  # overwrite, which is unconditional and does not depend on term/ignoreCase.
  partial <- identical(mode, "partial") && !is.null(term) && nzchar(term)
  pre <- list(); post <- list(); colsChanged <- FALSE; total <- 0L
  for (j in ucols) {
    jkey <- as.character(j)
    rws  <- rows[cols == j]
    col  <- df[[j]]
    substVals <- function() {
      vapply(as.character(col)[rws], function(v) gsub(term, replacement, v,
             ignore.case = ignoreCase, fixed = TRUE), character(1), USE.NAMES = FALSE)
    }
    if (is.factor(col)) {
      newCol <- col
      newVals <- if (partial) substVals() else rep(replacement, length(rws))
      addLevels <- setdiff(unique(newVals), levels(newCol))
      if (length(addLevels)) levels(newCol) <- c(levels(newCol), addLevels)
      newCol[rws] <- newVals
      pre[[jkey]]  <- list(whole = col)
      post[[jkey]] <- list(whole = newCol)
      colsChanged  <- TRUE
    } else {
      if (partial) {
        repl <- substVals()
        # gsub() always returns character. If every substituted value still parses as a
        # number (matching whole-mode's own numeric coercion below), convert back and keep
        # the column numeric -- otherwise a plain "replace part of the text" on a numeric
        # column would needlessly flip its type, forcing a full column/grid rebuild on the
        # frontend (which resets scroll position and has caused undo-history mismatches).
        # is.na(num) vs is.na(repl) tells them apart: identical NA positions means every
        # non-NA substitution parsed cleanly, so any NAs present were already NA in `col`.
        if (is.numeric(col)) {
          num <- suppressWarnings(as.numeric(repl))
          if (identical(is.na(num), is.na(repl))) {
            if (is.integer(col) && all(is.na(num) | (num == round(num) & abs(num) <= .Machine$integer.max))) {
              repl <- as.integer(num)
            } else {
              repl <- num
            }
          }
        }
      } else {
        repl <- replacement
        if (is.numeric(col)) {
          num <- suppressWarnings(as.numeric(replacement))
          if (!is.na(num)) {
            if (is.integer(col) && num == round(num) && abs(num) <= .Machine$integer.max) {
              repl <- as.integer(num)
            } else {
              repl <- num
            }
          }
        }
      }
      newCol <- col; newCol[rws] <- repl
      if (!identical(class(newCol), class(col))) {
        pre[[jkey]]  <- list(whole = col)
        post[[jkey]] <- list(whole = newCol)
        colsChanged  <- TRUE
      } else {
        pre[[jkey]]  <- list(rows = rws, vals = col[rws])
        post[[jkey]] <- list(rows = rws, vals = newCol[rws])
      }
    }
    df[[j]] <- newCol
    total <- total + length(rws)
  }
  assign(datasetName, df, envir = globalenv())
  e$tokens[[token]] <- list(dataset = datasetName, pre = pre, post = post,
                            colsChanged = colsChanged,
                            loadId = .bskyReplaceLoadId(datasetName))
  e$order <- c(e$order, token)
  dsTok <- e$order[vapply(e$order,
             function(tk) identical(e$tokens[[tk]]$dataset, datasetName), logical(1))]
  if (length(dsTok) > maxStack) {
    drop <- dsTok[seq_len(length(dsTok) - maxStack)]
    for (tk in drop) e$tokens[[tk]] <- NULL
    e$order <- e$order[!(e$order %in% drop)]
  }
  list(token = token, replaced = total, colsChanged = colsChanged)
}

.bskyReplaceRestore <- function(token, which) {
  e <- .bskyReplaceInit()
  entry <- e$tokens[[token]]
  if (is.null(entry)) return(list(ok = FALSE, reason = 'notoken', colsChanged = FALSE))
  if (!identical(entry$loadId, .bskyReplaceLoadId(entry$dataset)))
    return(list(ok = FALSE, reason = 'staleload', colsChanged = FALSE))
  df   <- get(entry$dataset, envir = globalenv())
  snap <- if (identical(which, 'pre')) entry$pre else entry$post
  for (jkey in names(snap)) {
    j <- as.integer(jkey); s <- snap[[jkey]]
    if (!is.null(s$whole)) df[[j]] <- s$whole
    else df[[j]][s$rows] <- s$vals
  }
  assign(entry$dataset, df, envir = globalenv())
  list(ok = TRUE, dataset = entry$dataset, colsChanged = entry$colsChanged)
}