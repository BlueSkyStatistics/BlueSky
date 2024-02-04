
print.lavaan.fitMeasures_aar <- function (x, ..., nd = 3L, add.h0 = TRUE)
{
    names.x <- names(x)
    scaled.flag <- "chisq.scaled" %in% names.x
    num.format <- paste("%", max(8L, nd + 5L), ".", nd, "f",
        sep = "")
    if (add.h0 && "chisq" %in% names.x) {
        #cat("\nModel Test User Model:\n\n")
        c1 <- c2 <- c3 <- character(0L)
        c1 <- c(c1, "Test statistic")
        c2 <- c(c2, sprintf(num.format, x["chisq"]))
        c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format, x["chisq.scaled"]),
            ""))
        c1 <- c(c1, "Degrees of freedom")
        c2 <- c(c2, x["df"])
        c3 <- c(c3, ifelse(scaled.flag, ifelse(x["df.scaled"]%%1 ==
            0, x["df.scaled"], sprintf(num.format, x["df.scaled"])),
            ""))
        c1 <- c(c1, "P-value")
        c2 <- c(c2, sprintf(num.format, x["pvalue"]))
        c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format, x["pvalue.scaled"]),
            ""))
        if (scaled.flag && "chisq.scaling.factor" %in% names.x) {
            c1 <- c(c1, "Scaling correction factor")
            c2 <- c(c2, "")
            c3 <- c(c3, sprintf(num.format, x["chisq.scaling.factor"]))
        }
        c1 <- format(c1, width = 35L)
        c2 <- format(c2, width = 16L + max(0, (nd - 3L)) * 4L,
            justify = "right")
        c3 <- format(c3, width = 8L + nd, justify = "right")
        if (scaled.flag) {
            M <- cbind(c1, c2, c3, deparse.level = 0)
        }
        else {
            M <- cbind(c1, c2, deparse.level = 0)
        }
        colnames(M) <- rep("", ncol(M))
        rownames(M) <- rep(" ", nrow(M))
		BSkyFormat(M, singleTableOutputHeader= "Model Test User Model")
        #write.table(M, row.names = TRUE, col.names = FALSE, quote = FALSE)
    }
    if ("baseline.chisq" %in% names.x) {
        #cat("\nModel Test Baseline Model:\n\n")
        c1 <- c2 <- c3 <- character(0L)
        c1 <- c(c1, "Test statistic")
        c2 <- c(c2, sprintf(num.format, x["baseline.chisq"]))
        c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format, x["baseline.chisq.scaled"]),
            ""))
        c1 <- c(c1, "Degrees of freedom")
        c2 <- c(c2, x["baseline.df"])
        c3 <- c(c3, ifelse(scaled.flag, ifelse(x["baseline.df.scaled"]%%1 ==
            0, x["baseline.df.scaled"], sprintf(num.format, x["baseline.df.scaled"])),
            ""))
        c1 <- c(c1, "P-value")
        c2 <- c(c2, sprintf(num.format, x["baseline.pvalue"]))
        c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format, x["baseline.pvalue.scaled"]),
            ""))
        if (scaled.flag && "baseline.chisq.scaling.factor" %in%
            names.x) {
            c1 <- c(c1, "Scaling correction factor")
            c2 <- c(c2, "")
            c3 <- c(c3, sprintf(num.format, x["baseline.chisq.scaling.factor"]))
        }
        c1 <- format(c1, width = 35L)
        c2 <- format(c2, width = 16L + max(0, (nd - 3L)) * 4L,
            justify = "right")
        c3 <- format(c3, width = 8L + nd, justify = "right")
        if (scaled.flag) {
            M <- cbind(c1, c2, c3, deparse.level = 0)
        }
        else {
            M <- cbind(c1, c2, deparse.level = 0)
        }
        colnames(M) <- rep("", ncol(M))
        rownames(M) <- rep(" ", nrow(M))
        #write.table(M, row.names = TRUE, col.names = FALSE, quote = FALSE)
		BSkyFormat(M, singleTableOutputHeader= "Model Test Baseline Model")
		
    }
    if (any(c("cfi", "tli", "nnfi", "rfi", "nfi", "ifi", "rni",
        "pnfi") %in% names.x)) {
        #cat("\nUser Model versus Baseline Model:\n\n")
        c1 <- c2 <- c3 <- character(0L)
        if ("cfi" %in% names.x) {
            c1 <- c(c1, "Comparative Fit Index (CFI)")
            c2 <- c(c2, sprintf(num.format, x["cfi"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["cfi.scaled"]), ""))
        }
        if ("tli" %in% names.x) {
            c1 <- c(c1, "Tucker-Lewis Index (TLI)")
            c2 <- c(c2, sprintf(num.format, x["tli"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["tli.scaled"]), ""))
        }
        if ("cfi.robust" %in% names.x) {
            c1 <- c(c1, "")
            c2 <- c(c2, "")
            c3 <- c(c3, "")
            c1 <- c(c1, "Robust Comparative Fit Index (CFI)")
            if (scaled.flag) {
                c2 <- c(c2, "")
                c3 <- c(c3, sprintf(num.format, x["cfi.robust"]))
            }
            else {
                c2 <- c(c2, sprintf(num.format, x["cfi.robust"]))
                c3 <- c(c3, "")
            }
        }
        if ("tli.robust" %in% names.x) {
            c1 <- c(c1, "Robust Tucker-Lewis Index (TLI)")
            if (scaled.flag) {
                c2 <- c(c2, "")
                c3 <- c(c3, sprintf(num.format, x["tli.robust"]))
            }
            else {
                c2 <- c(c2, sprintf(num.format, x["tli.robust"]))
                c3 <- c(c3, "")
            }
        }
        if ("nnfi" %in% names.x) {
            c1 <- c(c1, "Bentler-Bonett Non-normed Fit Index (NNFI)")
            c2 <- c(c2, sprintf(num.format, x["nnfi"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["nnfi.robust"]), ""))
        }
        if ("nfi" %in% names.x) {
            c1 <- c(c1, "Bentler-Bonett Normed Fit Index (NFI)")
            c2 <- c(c2, sprintf(num.format, x["nfi"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["nfi.scaled"]), ""))
        }
        if ("pnfi" %in% names.x) {
            c1 <- c(c1, "Parsimony Normed Fit Index (PNFI)")
            c2 <- c(c2, sprintf(num.format, x["pnfi"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["pnfi.scaled"]), ""))
        }
        if ("rfi" %in% names.x) {
            c1 <- c(c1, "Bollen's Relative Fit Index (RFI)")
            c2 <- c(c2, sprintf(num.format, x["rfi"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["rfi.scaled"]), ""))
        }
        if ("ifi" %in% names.x) {
            c1 <- c(c1, "Bollen's Incremental Fit Index (IFI)")
            c2 <- c(c2, sprintf(num.format, x["ifi"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["ifi.scaled"]), ""))
        }
        if ("rni" %in% names.x) {
            c1 <- c(c1, "Relative Noncentrality Index (RNI)")
            c2 <- c(c2, sprintf(num.format, x["rni"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["rni.robust"]), ""))
        }
        c1 <- format(c1, width = 43L)
        c2 <- format(c2, width = 8L + max(0, (nd - 3L)) * 4L,
            justify = "right")
        c3 <- format(c3, width = 8L + nd, justify = "right")
        if (scaled.flag) {
            M <- cbind(c1, c2, c3, deparse.level = 0)
        }
        else {
            M <- cbind(c1, c2, deparse.level = 0)
        }
        colnames(M) <- rep("", ncol(M))
        rownames(M) <- rep(" ", nrow(M))
       #write.table(M, row.names = TRUE, col.names = FALSE, quote = FALSE)
	   BSkyFormat(M, singleTableOutputHeader= "Fit Indices")
		
    }
    if ("logl" %in% names.x) {
       #cat("\nLoglikelihood and Information Criteria:\n\n")
        c1 <- c2 <- c3 <- character(0L)
        c1 <- c(c1, "Loglikelihood user model (H0)")
        c2 <- c(c2, sprintf(num.format, x["logl"]))
        c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format, x["logl"]),
            ""))
        if (!is.na(x["scaling.factor.h0"])) {
            c1 <- c(c1, "Scaling correction factor")
            c2 <- c(c2, sprintf("  %10s", ""))
            c3 <- c(c3, sprintf(num.format, x["scaling.factor.h0"]))
            c1 <- c(c1, "    for the MLR correction")
            c2 <- c(c2, "")
            c3 <- c(c3, "")
        }
        if ("unrestricted.logl" %in% names.x) {
            c1 <- c(c1, "Loglikelihood unrestricted model (H1)")
            c2 <- c(c2, sprintf(num.format, x["unrestricted.logl"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["unrestricted.logl"]), ""))
            if (!is.na(x["scaling.factor.h1"])) {
                c1 <- c(c1, "Scaling correction factor")
                c2 <- c(c2, sprintf("  %10s", ""))
                c3 <- c(c3, sprintf(num.format, x["scaling.factor.h1"]))
                c1 <- c(c1, "    for the MLR correction")
                c2 <- c(c2, "")
                c3 <- c(c3, "")
            }
        }
        c1 <- c(c1, "")
        c2 <- c(c2, "")
        c3 <- c(c3, "")
        c1 <- c(c1, "Akaike (AIC)")
        c2 <- c(c2, sprintf(num.format, x["aic"]))
        c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format, x["aic"]),
            ""))
        c1 <- c(c1, "Bayesian (BIC)")
        c2 <- c(c2, sprintf(num.format, x["bic"]))
        c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format, x["bic"]),
            ""))
        if (!is.na(x["bic2"])) {
            c1 <- c(c1, "Sample-size adjusted Bayesian (SABIC)")
            c2 <- c(c2, sprintf(num.format, x["bic2"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["bic2"]), ""))
        }
        c1 <- format(c1, width = 39L)
        c2 <- format(c2, width = 12L + max(0, (nd - 3L)) * 4L,
            justify = "right")
        c3 <- format(c3, width = 8L + nd, justify = "right")
        if (scaled.flag) {
            M <- cbind(c1, c2, c3, deparse.level = 0)
        }
        else {
            M <- cbind(c1, c2, deparse.level = 0)
        }
        colnames(M) <- rep("", ncol(M))
        rownames(M) <- rep(" ", nrow(M))
       #write.table(M, row.names = TRUE, col.names = FALSE, quote = FALSE)
	   BSkyFormat(M, singleTableOutputHeader= "Loglikelihood and Information Criteria")
		
    }
    if ("rmsea" %in% names.x) {
        #cat("\nRoot Mean Square Error of Approximation:\n\n")
        c1 <- c2 <- c3 <- character(0L)
        c1 <- c(c1, "RMSEA")
        c2 <- c(c2, sprintf(num.format, x["rmsea"]))
        c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format, x["rmsea.scaled"]),
            ""))
        ci.level <- NULL
        if ("rmsea.ci.level" %in% names.x) {
            ci.level <- x["rmsea.ci.level"]
        }
        if ("rmsea.ci.lower" %in% names.x) {
            if (is.null(ci.level)) {
                c1 <- c(c1, "Confidence interval - lower")
            }
            else {
                c1 <- c(c1, paste0(sprintf("%2d", round(ci.level *
                  100)), " Percent confidence interval - lower"))
            }
            c2 <- c(c2, sprintf(num.format, x["rmsea.ci.lower"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["rmsea.ci.lower.scaled"]), ""))
            if (is.null(ci.level)) {
                c1 <- c(c1, "Confidence interval - upper")
            }
            else {
                c1 <- c(c1, paste0(sprintf("%2d", round(ci.level *
                  100)), " Percent confidence interval - upper"))
            }
            c2 <- c(c2, sprintf(num.format, x["rmsea.ci.upper"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["rmsea.ci.upper.scaled"]), ""))
        }
        rmsea.close.h0 <- NULL
        if ("rmsea.close.h0" %in% names.x) {
            rmsea.close.h0 <- x["rmsea.close.h0"]
        }
        rmsea.notclose.h0 <- NULL
        if ("rmsea.notclose.h0" %in% names.x) {
            rmsea.notclose.h0 <- x["rmsea.notclose.h0"]
        }
        if ("rmsea.pvalue" %in% names.x) {
            if (is.null(rmsea.close.h0)) {
                c1 <- c(c1, "P-value H_0: RMSEA <= 0.05")
            }
            else {
                c1 <- c(c1, paste0("P-value H_0: RMSEA <= ",
                  sprintf("%4.3f", rmsea.close.h0)))
            }
            c2 <- c(c2, sprintf(num.format, x["rmsea.pvalue"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["rmsea.pvalue.scaled"]), ""))
        }
        if ("rmsea.notclose.pvalue" %in% names.x) {
            if (is.null(rmsea.notclose.h0)) {
                c1 <- c(c1, "P-value H_0: RMSEA >= 0.080")
            }
            else {
                c1 <- c(c1, paste0("P-value H_0: RMSEA >= ",
                  sprintf("%4.3f", rmsea.notclose.h0)))
            }
            c2 <- c(c2, sprintf(num.format, x["rmsea.notclose.pvalue"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["rmsea.notclose.pvalue.scaled"]), ""))
        }
        if ("rmsea.robust" %in% names.x) {
            c1 <- c(c1, "")
            c2 <- c(c2, "")
            c3 <- c(c3, "")
            c1 <- c(c1, "Robust RMSEA")
            if (scaled.flag) {
                c2 <- c(c2, "")
                c3 <- c(c3, sprintf(num.format, x["rmsea.robust"]))
            }
            else {
                c2 <- c(c2, sprintf(num.format, x["rmsea.robust"]))
                c3 <- c(c3, "")
            }
        }
        if ("rmsea.ci.lower.robust" %in% names.x) {
            if (is.null(ci.level)) {
                c1 <- c(c1, "Confidence interval - lower")
            }
            else {
                c1 <- c(c1, paste0(sprintf("%2d", round(ci.level *
                  100)), " Percent confidence interval - lower"))
            }
            if (scaled.flag) {
                c2 <- c(c2, "")
                c3 <- c(c3, sprintf(num.format, x["rmsea.ci.lower.robust"]))
            }
            else {
                c2 <- c(c2, sprintf(num.format, x["rmsea.ci.lower.robust"]))
                c3 <- c(c3, "")
            }
            if (is.null(ci.level)) {
                c1 <- c(c1, "Confidence interval - upper")
            }
            else {
                c1 <- c(c1, paste0(sprintf("%2d", round(ci.level *
                  100)), " Percent confidence interval - upper"))
            }
            if (scaled.flag) {
                c2 <- c(c2, "")
                c3 <- c(c3, sprintf(num.format, x["rmsea.ci.upper.robust"]))
            }
            else {
                c2 <- c(c2, sprintf(num.format, x["rmsea.ci.upper.robust"]))
                c3 <- c(c3, "")
            }
        }
        if ("rmsea.pvalue.robust" %in% names.x) {
            if (is.null(rmsea.close.h0)) {
                c1 <- c(c1, "P-value H_0: Robust RMSEA <= 0.05")
            }
            else {
                c1 <- c(c1, paste0("P-value H_0: Robust RMSEA <= ",
                  sprintf("%4.3f", rmsea.close.h0)))
            }
            if (scaled.flag) {
                c2 <- c(c2, "")
                c3 <- c(c3, sprintf(num.format, x["rmsea.pvalue.robust"]))
            }
            else {
                c2 <- c(c2, sprintf(num.format, x["rmsea.pvalue.robust"]))
                c3 <- c(c3, "")
            }
        }
        if ("rmsea.notclose.pvalue.robust" %in% names.x) {
            if (is.null(rmsea.notclose.h0)) {
                c1 <- c(c1, "P-value H_0: Robust RMSEA >= 0.080")
            }
            else {
                c1 <- c(c1, paste0("P-value H_0: Robust RMSEA >= ",
                  sprintf("%4.3f", rmsea.notclose.h0)))
            }
            if (scaled.flag) {
                c2 <- c(c2, "")
                c3 <- c(c3, sprintf(num.format, x["rmsea.notclose.pvalue.robust"]))
            }
            else {
                c2 <- c(c2, sprintf(num.format, x["rmsea.notclose.pvalue.robust"]))
                c3 <- c(c3, "")
            }
        }
        c1 <- format(c1, width = 43L)
        c2 <- format(c2, width = 8L + max(0, (nd - 3L)) * 4L,
            justify = "right")
        c3 <- format(c3, width = 8L + nd, justify = "right")
        if (scaled.flag) {
            M <- cbind(c1, c2, c3, deparse.level = 0)
        }
        else {
            M <- cbind(c1, c2, deparse.level = 0)
        }
        colnames(M) <- rep("", ncol(M))
        rownames(M) <- rep(" ", nrow(M))
        #write.table(M, row.names = TRUE, col.names = FALSE, quote = FALSE)
		BSkyFormat(M, singleTableOutputHeader= "Root Mean Square Error of Approximation")
		
    }
    if (any(c("rmr", "srmr") %in% names.x) && !"srmr_within" %in%
        names.x) {
        #cat("\nStandardized Root Mean Square Residual:\n\n")
        c1 <- c2 <- c3 <- character(0L)
        if ("rmr" %in% names.x) {
            c1 <- c(c1, "RMR")
            c2 <- c(c2, sprintf(num.format, x["rmr"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["rmr"]), ""))
        }
        if ("rmr_nomean" %in% names.x) {
            c1 <- c(c1, "RMR (No Mean)")
            c2 <- c(c2, sprintf(num.format, x["rmr_nomean"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["rmr_nomean"]), ""))
        }
        if ("srmr" %in% names.x) {
            c1 <- c(c1, "SRMR")
            c2 <- c(c2, sprintf(num.format, x["srmr"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["srmr"]), ""))
        }
        if ("srmr_nomean" %in% names.x) {
            c1 <- c(c1, "SRMR (No Mean)")
            c2 <- c(c2, sprintf(num.format, x["srmr_nomean"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["srmr_nomean"]), ""))
        }
        c1 <- format(c1, width = 43L)
        c2 <- format(c2, width = 8L + max(0, (nd - 3L)) * 4L,
            justify = "right")
        c3 <- format(c3, width = 8L + nd, justify = "right")
        if (scaled.flag) {
            M <- cbind(c1, c2, c3, deparse.level = 0)
        }
        else {
            M <- cbind(c1, c2, deparse.level = 0)
        }
        colnames(M) <- rep("", ncol(M))
        rownames(M) <- rep(" ", nrow(M))
        #write.table(M, row.names = TRUE, col.names = FALSE, quote = FALSE)
		BSkyFormat(M, singleTableOutputHeader= "Standardized Root Mean Square Residual")
		#cat("\nStandardized Root Mean Square Residual:\n\n")
    }
    if (any(c("srmr_within", "srmr_between") %in% names.x)) {
        #cat("\nStandardized Root Mean Square Residual (corr metric):\n\n")
        c1 <- c2 <- c3 <- character(0L)
        if ("srmr_within" %in% names.x) {
            c1 <- c(c1, "SRMR (within covariance matrix)")
            c2 <- c(c2, sprintf(num.format, x["srmr_within"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["srmr_within"]), ""))
        }
        if ("srmr_between" %in% names.x) {
            c1 <- c(c1, "SRMR (between covariance matrix)")
            c2 <- c(c2, sprintf(num.format, x["srmr_between"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["srmr_between"]), ""))
        }
        c1 <- format(c1, width = 43L)
        c2 <- format(c2, width = 8L + max(0, (nd - 3L)) * 4L,
            justify = "right")
        c3 <- format(c3, width = 8L + nd, justify = "right")
        if (scaled.flag) {
            M <- cbind(c1, c2, c3, deparse.level = 0)
        }
        else {
            M <- cbind(c1, c2, deparse.level = 0)
        }
        colnames(M) <- rep("", ncol(M))
        rownames(M) <- rep(" ", nrow(M))
		#cat("\nStandardized Root Mean Square Residual (corr metric):\n\n")
		BSkyFormat(M, singleTableOutputHeader= "Standardized Root Mean Square Residual (corr metric)")
        #write.table(M, row.names = TRUE, col.names = FALSE, quote = FALSE)
    }
    if ("wrmr" %in% names.x) {
        #cat("\nWeighted Root Mean Square Residual:\n\n")
        c1 <- c2 <- c3 <- character(0L)
        if ("wrmr" %in% names.x) {
            c1 <- c(c1, "WRMR")
            c2 <- c(c2, sprintf(num.format, x["wrmr"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["wrmr"]), ""))
        }
        c1 <- format(c1, width = 43L)
        c2 <- format(c2, width = 8L + max(0, (nd - 3L)) * 4L,
            justify = "right")
        c3 <- format(c3, width = 8L + nd, justify = "right")
        if (scaled.flag) {
            M <- cbind(c1, c2, c3, deparse.level = 0)
        }
        else {
            M <- cbind(c1, c2, deparse.level = 0)
        }
        colnames(M) <- rep("", ncol(M))
        rownames(M) <- rep(" ", nrow(M))
		#cat("\nWeighted Root Mean Square Residual:\n\n")
		BSkyFormat(M, singleTableOutputHeader= "Weighted Root Mean Square Residual")
        #write.table(M, row.names = TRUE, col.names = FALSE, quote = FALSE)
    }
    if (any(c("cn_05", "cn_01", "gfi", "agfi", "pgfi", "mfi") %in%
        names.x)) {
        #cat("\nOther Fit Indices:\n\n")
        c1 <- c2 <- c3 <- character(0L)
        if ("cn_05" %in% names.x) {
            c1 <- c(c1, "Hoelter Critical N (CN) alpha = 0.05")
            c2 <- c(c2, sprintf(num.format, x["cn_05"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["cn_05"]), ""))
        }
        if ("cn_01" %in% names.x) {
            c1 <- c(c1, "Hoelter Critical N (CN) alpha = 0.01")
            c2 <- c(c2, sprintf(num.format, x["cn_01"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["cn_01"]), ""))
        }
        if (any(c("cn_05", "cn_01") %in% names.x)) {
            c1 <- c(c1, "")
            c2 <- c(c2, "")
            c3 <- c(c3, "")
        }
        if ("gfi" %in% names.x) {
            c1 <- c(c1, "Goodness of Fit Index (GFI)")
            c2 <- c(c2, sprintf(num.format, x["gfi"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["gfi"]), ""))
        }
        if ("agfi" %in% names.x) {
            c1 <- c(c1, "Adjusted Goodness of Fit Index (AGFI)")
            c2 <- c(c2, sprintf(num.format, x["agfi"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["agfi"]), ""))
        }
        if ("pgfi" %in% names.x) {
            c1 <- c(c1, "Parsimony Goodness of Fit Index (PGFI)")
            c2 <- c(c2, sprintf(num.format, x["pgfi"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["pgfi"]), ""))
        }
        if (any(c("gfi", "agfi", "pgfi") %in% names.x)) {
            c1 <- c(c1, "")
            c2 <- c(c2, "")
            c3 <- c(c3, "")
        }
        if ("mfi" %in% names.x) {
            c1 <- c(c1, "McDonald Fit Index (MFI)")
            c2 <- c(c2, sprintf(num.format, x["mfi"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["mfi"]), ""))
        }
        if ("mfi" %in% names.x) {
            c1 <- c(c1, "")
            c2 <- c(c2, "")
            c3 <- c(c3, "")
        }
        if ("ecvi" %in% names.x) {
            c1 <- c(c1, "Expected Cross-Validation Index (ECVI)")
            c2 <- c(c2, sprintf(num.format, x["ecvi"]))
            c3 <- c(c3, ifelse(scaled.flag, sprintf(num.format,
                x["ecvi"]), ""))
        }
        c1 <- format(c1, width = 43L)
        c2 <- format(c2, width = 8L + max(0, (nd - 3L)) * 4L,
            justify = "right")
        c3 <- format(c3, width = 8L + nd, justify = "right")
        if (scaled.flag) {
            M <- cbind(c1, c2, c3, deparse.level = 0)
        }
        else {
            M <- cbind(c1, c2, deparse.level = 0)
        }
        colnames(M) <- rep("", ncol(M))
        rownames(M) <- rep(" ", nrow(M))
        #write.table(M, row.names = TRUE, col.names = FALSE, quote = FALSE)
		#cat("\nOther Fit Indices:\n\n")
		BSkyFormat(M, singleTableOutputHeader= "Other Fit Indices")
    }
    invisible(x)
}
