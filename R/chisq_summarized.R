BSky_ChiSquare_Association_Summarized <- function(
  data,
  table_cols,
  row_label_col = NULL,

  # checkboxes (any combination)
  show_counts = TRUE,
  show_margins = TRUE,
  show_expected = FALSE,
  show_contrib = FALSE,
  show_raw_residuals = FALSE,
  show_std_residuals = FALSE,
  show_adj_residuals = FALSE
) {
  if (is.null(table_cols) || length(table_cols) < 2) {
    stop("Select at least 2 columns containing the two-way table counts.")
  }

  # Extract counts and coerce to numeric
  counts_df <- data[, table_cols, drop = FALSE]
  for (nm in names(counts_df)) counts_df[[nm]] <- as.numeric(counts_df[[nm]])

  if (anyNA(counts_df)) stop("Selected table columns contain non-numeric or missing values.")
  if (any(counts_df < 0)) stop("Counts must be non-negative.")

  obs <- as.matrix(counts_df)
  colnames(obs) <- table_cols

  # Row labels
  rnames <- as.character(seq_len(nrow(obs)))
  if (!is.null(row_label_col) && row_label_col %in% names(data)) {
    rlab <- as.character(data[[row_label_col]])
    if (!all(is.na(rlab) | rlab == "")) rnames <- rlab
  }
  rownames(obs) <- rnames

  # Pearson chi-square (no continuity correction)
  pearson <- suppressWarnings(chisq.test(obs, correct = FALSE))
  expct <- pearson$expected
  df_chi <- unname(pearson$parameter)
  pearson_stat <- unname(pearson$statistic)
  pearson_p <- unname(pearson$p.value)

  # Likelihood ratio (G^2)
  nz <- obs > 0
  G2 <- 2 * sum(obs[nz] * log(obs[nz] / expct[nz]))
  lr_p <- pchisq(G2, df = df_chi, lower.tail = FALSE)

  # Diagnostics
  raw_resid <- obs - expct
  std_resid <- raw_resid / sqrt(expct)

  N <- sum(obs)
  row_prop <- rowSums(obs) / N
  col_prop <- colSums(obs) / N
  denom_adj <- sqrt(expct * (1 - row_prop) %o% (1 - col_prop))
  adj_resid <- raw_resid / denom_adj

  contrib <- (raw_resid^2) / expct

  # ----- Build content “row types” in EXACT requested order -----
  row_types <- list()

  if (isTRUE(show_counts)) {
    row_types <- c(row_types, list(list(
      key = "Count",
      label = "Count",
      formatter = function(x) formatC(x, format = "f", digits = 0),
      getter = function(i, j) obs[i, j]
    )))
  }
  if (isTRUE(show_expected)) {
    row_types <- c(row_types, list(list(
      key = "Expected",
      label = "Expected count",
      formatter = function(x) formatC(x, format = "f", digits = 2),
      getter = function(i, j) expct[i, j]
    )))
  }
  if (isTRUE(show_contrib)) {
    row_types <- c(row_types, list(list(
      key = "Contribution",
      label = "Contribution to Chi-square",
      formatter = function(x) formatC(x, format = "f", digits = 4),
      getter = function(i, j) contrib[i, j]
    )))
  }
  if (isTRUE(show_raw_residuals)) {
    row_types <- c(row_types, list(list(
      key = "RawResidual",
      label = "Raw residual (O - E)",
      formatter = function(x) formatC(x, format = "f", digits = 2),
      getter = function(i, j) raw_resid[i, j]
    )))
  }
  if (isTRUE(show_std_residuals)) {
    row_types <- c(row_types, list(list(
      key = "StdResidual",
      label = "Standardized residual",
      formatter = function(x) formatC(x, format = "f", digits = 3),
      getter = function(i, j) std_resid[i, j]
    )))
  }
  if (isTRUE(show_adj_residuals)) {
    row_types <- c(row_types, list(list(
      key = "AdjResidual",
      label = "Adjusted residual",
      formatter = function(x) formatC(x, format = "f", digits = 3),
      getter = function(i, j) adj_resid[i, j]
    )))
  }

  if (length(row_types) == 0) {
    stop("Select at least one cell content checkbox (Counts/Expected/Contribution/Residuals).")
  }

  # Decide which stacked row gets margins
  margin_row_key <- if (isTRUE(show_counts)) "Count" else row_types[[1]]$key

  # ----- Build stacked table -----
  out_cols <- if (isTRUE(show_margins)) c(table_cols, "All") else table_cols

  blocks <- lapply(seq_len(nrow(obs)), function(i) {
    block <- data.frame(matrix("", nrow = length(row_types), ncol = length(out_cols)),
                        stringsAsFactors = FALSE)
    colnames(block) <- out_cols

    rn <- vapply(row_types, function(rt) paste(rnames[i], rt$key, sep = " - "), character(1))
    rownames(block) <- rn

    for (k in seq_along(row_types)) {
      rt <- row_types[[k]]
      for (j in seq_len(ncol(obs))) {
        block[k, j] <- rt$formatter(rt$getter(i, j))
      }
    }

    if (isTRUE(show_margins)) {
      idx_margin <- which(vapply(row_types, function(rt) rt$key == margin_row_key, logical(1)))
      if (length(idx_margin) == 1) {
        block[idx_margin, "All"] <- formatC(sum(obs[i, ]), format = "f", digits = 0)
      }
    }

    block
  })

  stacked <- do.call(rbind, blocks)

  if (isTRUE(show_margins)) {
    all_block <- data.frame(matrix("", nrow = length(row_types), ncol = length(out_cols)),
                            stringsAsFactors = FALSE)
    colnames(all_block) <- out_cols
    rownames(all_block) <- vapply(row_types, function(rt) paste("All", rt$key, sep = " - "), character(1))

    idx_margin <- which(vapply(row_types, function(rt) rt$key == margin_row_key, logical(1)))
    if (length(idx_margin) == 1) {
      all_block[idx_margin, table_cols] <- formatC(colSums(obs), format = "f", digits = 0)
      all_block[idx_margin, "All"] <- formatC(sum(obs), format = "f", digits = 0)
    }

    stacked <- rbind(stacked, all_block)
  }

  # -------- FIXED FOOTNOTE SECTION --------
  foot_lines <- c(
    paste0(
      "Cell contents are displayed as separate rows in this order: ",
      paste(vapply(row_types, `[[`, "", "label"), collapse = "; ")
    )
  )

  if (isTRUE(show_margins)) {
    foot_lines <- c(
      foot_lines,
      paste0("Margins (row/column totals) are shown on the '", margin_row_key, "' rows only.")
    )
  }

  # Must be a single string separated by newline
  attr(stacked, "BSkyFootnote_BSkySplit") <- paste(foot_lines, collapse = "\n")
  # ----------------------------------------

  test_tbl <- data.frame(
    Test = c("Pearson", "Likelihood Ratio"),
    ChiSquare = c(pearson_stat, G2),
    DF = c(df_chi, df_chi),
    PValue = c(pearson_p, lr_p),
    check.names = FALSE
  )

  list(
    CombinedTable = stacked,
    ChiSquareTests = test_tbl
  )
}
