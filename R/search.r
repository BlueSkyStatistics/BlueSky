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

.bskyReplaceApplyAll <- function(datasetName, rows, cols, replacement, token, maxStack = 20L) {
  e  <- .bskyReplaceInit()
  df <- get(datasetName, envir = globalenv())
  ucols <- unique(cols)
  pre <- list(); post <- list(); colsChanged <- FALSE; total <- 0L
  for (j in ucols) {
    jkey <- as.character(j)
    rws  <- rows[cols == j]
    col  <- df[[j]]
    if (is.factor(col)) {
      newCol <- col
      if (!(replacement %in% levels(newCol)))
        levels(newCol) <- c(levels(newCol), replacement)
      newCol[rws] <- replacement
      pre[[jkey]]  <- list(whole = col)
      post[[jkey]] <- list(whole = newCol)
      colsChanged  <- TRUE
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