# ============================================================
# BSky_one_sample_variance
# Updates:
# 1) Wide headers: "Method (Variable: mpg)" etc.
# 2) Long headers: "Method (Variable: mpg, where cyl = 4)" etc.
# 3) Method text: "sigma: standard deviation of variable: mpg ..." (wide/long)
# ============================================================

BSky_fmt_plain <- function(x, digits = BSkyGetDecimalDigitSetting()) {
  if (is.na(x)) return(NA_character_)
  formatC(round(x, digits = digits), format = "f", digits = digits)
}

BSky_make_hypothesis_statements <- function(hypoth_type, hypoth_value, alt_hypothesis) {
  if (hypoth_type == "sd") {
    h0 <- paste0("H0: sigma = ", hypoth_value)
    h1 <- switch(
      alt_hypothesis,
      "two.sided" = paste0("H1: sigma != ", hypoth_value),
      "greater"   = paste0("H1: sigma > ", hypoth_value),
      "less"      = paste0("H1: sigma < ", hypoth_value),
      stop("alt_hypothesis must be one of: 'two.sided', 'greater', 'less'")
    )
  } else if (hypoth_type == "var") {
    h0 <- paste0("H0: sigma^2 = ", hypoth_value)
    h1 <- switch(
      alt_hypothesis,
      "two.sided" = paste0("H1: sigma^2 != ", hypoth_value),
      "greater"   = paste0("H1: sigma^2 > ", hypoth_value),
      "less"      = paste0("H1: sigma^2 < ", hypoth_value),
      stop("alt_hypothesis must be one of: 'two.sided', 'greater', 'less'")
    )
  } else stop("hypoth_type must be one of: 'sd', 'var'")
  c(h0 = h0, h1 = h1)
}

BSky_chisq_one_sample <- function(x, hypoth_type, hypoth_value, conf_level, alt_hypothesis) {
  x <- x[!is.na(x)]
  n <- length(x)
  df <- n - 1

  if (n < 2) {
    return(list(
      n = n, df = df,
      sd = NA_real_, var = NA_real_,
      stat = NA_real_, p = NA_real_,
      ci_sd = c(NA_real_, NA_real_),
      ci_var = c(NA_real_, NA_real_)
    ))
  }

  s <- sd(x)
  v <- s^2

  v0 <- if (hypoth_type == "sd") (hypoth_value^2) else hypoth_value
  if (!is.numeric(v0) || length(v0) != 1 || v0 <= 0) stop("hypoth_value must be > 0")

  chi <- df * v / v0

  if (alt_hypothesis == "two.sided") {
    p <- 2 * min(pchisq(chi, df), 1 - pchisq(chi, df))
  } else if (alt_hypothesis == "greater") {
    p <- 1 - pchisq(chi, df)
  } else if (alt_hypothesis == "less") {
    p <- pchisq(chi, df)
  } else stop("alt_hypothesis must be one of: 'two.sided', 'greater', 'less'")

  alpha <- 1 - conf_level
  ci_var_low  <- (df * v) / qchisq(1 - alpha/2, df)
  ci_var_high <- (df * v) / qchisq(alpha/2, df)

  list(
    n = n, df = df,
    sd = s, var = v,
    stat = chi, p = p,
    ci_sd = sqrt(c(ci_var_low, ci_var_high)),
    ci_var = c(ci_var_low, ci_var_high)
  )
}

BSky_bonett_sd_ci <- function(x, alpha = 0.05) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2) return(c(NA_real_, NA_real_))

  s <- sd(x)

  if (n > 5) {
    trim_prop <- 1 / (2 * sqrt(n - 4))
    m_trim <- mean(x, trim = trim_prop)
  } else {
    m_trim <- mean(x)
  }

  numerator <- n * sum((x - m_trim)^4)
  denom <- (sum((x - mean(x))^2))^2
  gamma4_hat <- numerator / denom

  z <- qnorm(1 - alpha/2)
  c_const <- n / (n - z)
  se <- c_const * sqrt((gamma4_hat - (n - 3) / n) / (n - 1))

  log_variance <- log(c_const * s^2)
  lower_var <- exp(log_variance - z * se)
  upper_var <- exp(log_variance + z * se)

  c(sqrt(lower_var), sqrt(upper_var))
}

BSky_bonett_one_sd <- function(x, sigma0, alpha = 0.05, ut = TRUE, uc = TRUE, ag = TRUE) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (!is.numeric(sigma0) || length(sigma0) != 1 || sigma0 <= 0) {
    stop("sigma0 must be a single positive number (hypothesized SD).")
  }
  if (n < 2) {
    return(data.frame(
      n = n, sample_sd = NA_real_, sigma0 = sigma0,
      Z_abs = NA_real_, pval = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  s <- sd(x)

  if (ut) {
    trim_prop <- if (n > 5) 1/(2*sqrt(n - 4)) else 0
    m <- mean(x, trim = trim_prop)
  } else {
    m <- mean(x)
  }

  M4 <- sum((x - m)^4)
  denom_kurt <- (sum((x - mean(x))^2))^2
  if (denom_kurt <= 0) {
    return(data.frame(
      n = n, sample_sd = s, sigma0 = sigma0,
      Z_abs = NA_real_, pval = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  gamma_hat <- n * M4 / denom_kurt
  g <- if (ag) (n - 3)/n else 0

  if (uc) {
    z_alpha2 <- qnorm(1 - alpha/2)
    c_const <- n / (n - z_alpha2)
  } else c_const <- 1

  log_term <- log(c_const * s^2) - log(sigma0^2)
  se2 <- (gamma_hat - g) / (n - 1)
  se <- sqrt(abs(se2))

  Z_signed <- log_term / se
  Z_abs <- abs(Z_signed)

  data.frame(
    n = n, sample_sd = s, sigma0 = sigma0,
    gamma_hat = gamma_hat, g = g, c_const = c_const,
    se2 = se2, se = se,
    Z_signed = Z_signed, Z_abs = Z_abs,
    p_two_sided = 2 * (1 - pnorm(Z_abs)),
    stringsAsFactors = FALSE
  )
}

BSky_bonett_p_adjust_alt <- function(Z_signed, alt_hypothesis) {
  if (is.na(Z_signed)) return(NA_real_)
  if (alt_hypothesis == "two.sided") {
    return(2 * (1 - pnorm(abs(Z_signed))))
  } else if (alt_hypothesis == "greater") {
    return(1 - pnorm(Z_signed))
  } else if (alt_hypothesis == "less") {
    return(pnorm(Z_signed))
  }
  stop("alt_hypothesis must be one of: 'two.sided', 'greater', 'less'")
}

# NOTE: new arg "context_label" controls the Method text line and the header labels.
BSky_run_one_sample <- function(
  x,
  context_label,   # e.g. "variable: mpg" OR "variable: mpg where cyl = 4"
  conf_level,
  alt_hypothesis,
  hypoth_type,
  hypoth_value
) {
  x <- x[!is.na(x)]
  n <- length(x)

  method_mat <- matrix(
    c(
      paste0("sigma: standard deviation of ", context_label),  # <-- CHANGED
      "The Bonett method is valid for any continuous distribution.",
      "The chi-square method is valid only for the normal distribution."
    ),
    ncol = 1, byrow = TRUE
  )
  rownames(method_mat) <- c(" ", "  ", "   ")
  colnames(method_mat) <- "Method"

  hs <- BSky_make_hypothesis_statements(hypoth_type, hypoth_value, alt_hypothesis)

  if (n < 2) {
    desc_df <- data.frame(
      N = n, StDev = NA_real_, Variance = NA_real_,
      CI_Lower_Bonett = NA_real_, CI_Upper_Bonett = NA_real_,
      CI_Lower_ChiSquare = NA_real_, CI_Upper_ChiSquare = NA_real_,
      stringsAsFactors = FALSE
    )

    test_stmt_df <- data.frame(
      Test = c("Null hypothesis", "Alternative hypothesis"),
      Statement = c(hs["h0"], hs["h1"]),
      stringsAsFactors = FALSE
    )

    test_res_df <- data.frame(
      Method = c("Bonett", "Chi-Square"),
      Statistic = c(NA_real_, NA_real_),
      DF = c(NA_integer_, NA_integer_),
      P_Value = c(NA_real_, NA_real_),
      stringsAsFactors = FALSE
    )

    return(list(method = method_mat, descriptive = desc_df, test_statements = test_stmt_df, test_results = test_res_df))
  }

  alpha <- 1 - conf_level
  bon_ci <- BSky_bonett_sd_ci(x, alpha = alpha)

  chi_res <- BSky_chisq_one_sample(x, hypoth_type, hypoth_value, conf_level, alt_hypothesis)

  desc_df <- data.frame(
    N = chi_res$n,
    StDev = chi_res$sd,
    Variance = chi_res$var,
    CI_Lower_Bonett = bon_ci[1],
    CI_Upper_Bonett = bon_ci[2],
    CI_Lower_ChiSquare = chi_res$ci_sd[1],
    CI_Upper_ChiSquare = chi_res$ci_sd[2],
    stringsAsFactors = FALSE
  )

  test_stmt_df <- data.frame(
    Test = c("Null hypothesis", "Alternative hypothesis"),
    Statement = c(hs["h0"], hs["h1"]),
    stringsAsFactors = FALSE
  )

  sigma0 <- if (hypoth_type == "sd") hypoth_value else sqrt(hypoth_value)

  bon_res <- BSky_bonett_one_sd(x, sigma0 = sigma0, alpha = alpha)
  bon_p <- BSky_bonett_p_adjust_alt(bon_res$Z_signed[1], alt_hypothesis)

  test_res_df <- data.frame(
    Method = c("Bonett", "Chi-Square"),
    Statistic = c(abs(bon_res$Z_signed[1]), chi_res$stat),
    DF = c(NA_integer_, chi_res$df),
    P_Value = c(bon_p, chi_res$p),
    stringsAsFactors = FALSE
  )

  list(method = method_mat, descriptive = desc_df, test_statements = test_stmt_df, test_results = test_res_df)
}

BSky_one_sample_variance <- function(
  data_name,
  data_format = c("wide", "long"),
  cols_to_test = NULL,
  response_col = NULL,
  group_col = NULL,
  conf_level = 0.95,
  alt_hypothesis = c("two.sided", "greater", "less"),
  hypoth_type = c("sd", "var"),
  hypoth_value
) {
  data_format <- match.arg(data_format)
  alt_hypothesis <- match.arg(alt_hypothesis)
  hypoth_type <- match.arg(hypoth_type)

  if (!is.numeric(hypoth_value) || length(hypoth_value) != 1 || hypoth_value <= 0) {
    stop("hypoth_value must be a single positive number.")
  }

  data <- get(data_name, envir = .GlobalEnv)

  # store each sample as list item with:
  #  - x
  #  - header_suffix (what goes inside "(...)")
  samples_list <- list()

  if (data_format == "wide") {
    if (is.null(cols_to_test) || length(cols_to_test) < 1) {
      stop("For wide format, provide cols_to_test (one or more numeric variables).")
    }
    cols_to_test <- as.character(cols_to_test)

    for (nm in cols_to_test) {
      if (!nm %in% names(data)) stop(paste("Column not found:", nm))
      x <- data[[nm]]
      x <- x[!is.na(x)]
      samples_list[[nm]] <- list(
        x = x,
        header_suffix = paste0("Variable: ", nm),
        context_label = paste0("variable: ", nm)
      )
    }

  } else {
    if (is.null(response_col) || is.null(group_col)) {
      stop("For long format, you must provide a response variable and a grouping variable.")
    }
    response_col <- as.character(response_col)[1]
    group_col <- as.character(group_col)

    cols_needed <- c(response_col, group_col)
    data2 <- data %>% tidyr::drop_na(any_of(cols_needed))

    # combined group
    data2 <- data2 %>%
      dplyr::mutate(.bsky_group = interaction(!!!rlang::syms(group_col), sep = "_", drop = TRUE))

    split_list <- split(data2[[response_col]], data2$.bsky_group)
    split_list <- lapply(split_list, function(v) v[!is.na(v)])

    # create labels like: "Variable: mpg, where cyl = 4"
    # If multiple grouping vars, interaction label is "4_A" etc; we keep it as-is.
    for (gname in names(split_list)) {
      # pretty "where ..." piece:
      # If ONE grouping var, it's: where <group_col> = <level>
      # If MULTIPLE grouping vars, we show: where <group_col1,group_col2> = <interaction-level>
      where_txt <- if (length(group_col) == 1) {
        paste0("where ", group_col[1], " = ", gname)
      } else {
        paste0("where ", paste(group_col, collapse = ", "), " = ", gname)
      }

      key <- paste0(response_col, " | ", gname)

      samples_list[[key]] <- list(
        x = split_list[[gname]],
        header_suffix = paste0("Variable: ", response_col, ", ", where_txt),
        context_label = paste0("variable: ", response_col, " ", where_txt)
      )
    }
  }

  all_results <- list()

  for (nm in names(samples_list)) {
    x <- samples_list[[nm]]$x
    header_suffix <- samples_list[[nm]]$header_suffix
    context_label <- samples_list[[nm]]$context_label

    res <- BSky_run_one_sample(
      x = x,
      context_label = context_label,
      conf_level = conf_level,
      alt_hypothesis = alt_hypothesis,
      hypoth_type = hypoth_type,
      hypoth_value = hypoth_value
    )

    # ---- Table 1 ----
    BSkyFormat(res$method, singleTableOutputHeader = paste0("Method (", header_suffix, ")"))

    # ---- Table 2 ----
    desc_out <- res$descriptive
    colnames(desc_out) <- c(
      "N", "StDev", "Variance",
      "95% CI for sigma using Bonett (Lower)", "95% CI for sigma using Bonett (Upper)",
      "95% CI for sigma using Chi-Square (Lower)", "95% CI for sigma using Chi-Square (Upper)"
    )
    BSkyFormat(
      desc_out,
      singleTableOutputHeader = paste0("Descriptive Statistics (", header_suffix, ")"),
      decimalDigitsRounding = BSkyGetDecimalDigitSetting()
    )

    # ---- Table 3 ----
    BSkyFormat(res$test_statements, singleTableOutputHeader = paste0("Test (", header_suffix, ")"))

    # ---- Table 4 ----
    test_out <- res$test_results
    test_out$P_Value <- as.numeric(test_out$P_Value)
    BSkyFormat(
      test_out,
      singleTableOutputHeader = paste0("Test Results (", header_suffix, ")"),
      decimalDigitsRounding = BSkyGetDecimalDigitSetting()
    )

    all_results[[nm]] <- res
  }

  invisible(all_results)
}
