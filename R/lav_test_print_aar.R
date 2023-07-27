
lav_test_print_aar <- function (object, nd = 3L)
{
    TEST <- object
    if (is.null(TEST) || length(TEST) == 0L || !is.list(TEST)) {
        return(character(0L))
    }
    if (TEST[[1]]$test == "none") {
        return(character(0L))
    }
    info <- attr(object, "info")
    ngroups <- info$ngroups
    group.label <- info$group.label
    information <- info$information
    h1.information <- info$h1.information
    observed.information <- info$observed.information
    num.format <- paste("%", max(8L, nd + 5L), ".", nd, "f",
        sep = "")
   # cat("Model Test User Model:\n")
    has.no.scaling <- unname(sapply((lapply(TEST, "[[", "scaling.factor")),
        is.null))
    robust.idx <- which(!has.no.scaling)
    non.robust.idx <- which(has.no.scaling)
    scaled.idx <- 1L
    if (length(robust.idx) > 0L) {
        scaled.idx <- which(names(TEST) == TEST[[robust.idx[1]]]$scaled.test)
        if (length(scaled.idx) == 0L) {
            scaled.idx <- 1L
        }
        non.robust.idx <- non.robust.idx[-scaled.idx]
    }
    BLOCKS <- c(robust.idx, non.robust.idx)
    nBlocks <- length(BLOCKS)
    for (block in BLOCKS) {
        if (length(robust.idx) > 0L && block %in% robust.idx) {
            twocolumn <- TRUE
        }
        else {
            twocolumn <- FALSE
        }
        if (!twocolumn) {
            c1 <- c2 <- c3 <- character(0L)
            if (!is.null(TEST[[block]]$label)) {
                c1 <- c(c1, TEST[[block]]$label)
                c2 <- c(c2, "")
                c3 <- c(c3, "")
            }
            if (is.na(TEST[[block]]$df) || TEST[[block]]$df ==
                0L) {
                c1 <- c(c1, c("Test statistic", "Degrees of freedom"))
                c2 <- c(c2, c(sprintf(num.format, TEST[[block]]$stat),
                  ifelse(TEST[[block]]$df%%1 == 0, TEST[[block]]$df,
                    sprintf(num.format, TEST[[block]]$df))))
                c3 <- c(c3, c("", ""))
            }
            else {
                PLABEL <- "P-value"
                if (!is.null(TEST[[block]]$refdistr)) {
                  if (TEST[[block]]$refdistr == "chisq") {
                    PLABEL <- "P-value (Chi-square)"
                  }
                  else if (TEST[[block]]$refdistr == "unknown") {
                    PLABEL <- "P-value (Unknown)"
                  }
                  else if (TEST[[block]]$refdistr == "bootstrap") {
                    PLABEL <- "P-value (Bollen-Stine bootstrap)"
                  }
                }
                c1 <- c(c1, c("Test statistic", "Degrees of freedom",
                  PLABEL))
                c2 <- c(c2, c(sprintf(num.format, TEST[[block]]$stat),
                  ifelse(TEST[[block]]$df%%1 == 0, TEST[[block]]$df,
                    sprintf(num.format, TEST[[block]]$df)), sprintf(num.format,
                    TEST[[block]]$pvalue)))
                c3 <- c(c3, c("", "", ""))
            }
        }
        else {
            c1 <- c2 <- c3 <- character(0L)
            if (!is.null(TEST[[scaled.idx]]$label)) {
                c1 <- c(c1, TEST[[scaled.idx]]$label)
                c2 <- c(c2, "")
                c3 <- c(c3, "")
            }
            if (is.na(TEST[[block]]$df) || TEST[[block]]$df ==
                0L) {
                c1 <- c(c1, c("Test Statistic", "Degrees of freedom"))
                c2 <- c(c2, c(sprintf(num.format, TEST[[scaled.idx]]$stat),
                  ifelse(TEST[[scaled.idx]]$df%%1 == 0, TEST[[scaled.idx]]$df,
                    sprintf(num.format, TEST[[scaled.idx]]$df))))
                c3 <- c(c3, c(sprintf(num.format, TEST[[block]]$stat),
                  ifelse(TEST[[block]]$df%%1 == 0, TEST[[block]]$df,
                    sprintf(num.format, TEST[[block]]$df))))
            }
            else {
                if (!is.null(TEST[[scaled.idx]]$refdistr)) {
                  if (TEST[[scaled.idx]]$refdistr == "chisq") {
                    PLABEL <- "P-value (Chi-square)"
                  }
                  else if (TEST[[scaled.idx]]$refdistr == "unknown") {
                    PLABEL <- "P-value (Unknown)"
                  }
                  else {
                    PLABEL <- "P-value"
                  }
                }
                c1 <- c(c1, c("Test Statistic", "Degrees of freedom",
                  PLABEL, "Scaling correction factor"))
                c2 <- c(c2, c(sprintf(num.format, TEST[[scaled.idx]]$stat),
                  ifelse(TEST[[scaled.idx]]$df%%1 == 0, TEST[[scaled.idx]]$df,
                    sprintf(num.format, TEST[[scaled.idx]]$df)),
                  sprintf(num.format, TEST[[scaled.idx]]$pvalue),
                  ""))
                c3 <- c(c3, c(sprintf(num.format, TEST[[block]]$stat),
                  ifelse(TEST[[block]]$df%%1 == 0, TEST[[block]]$df,
                    sprintf(num.format, TEST[[block]]$df)), sprintf(num.format,
                    TEST[[block]]$pvalue), sprintf(num.format,
                    TEST[[block]]$scaling.factor)))
                if (TEST[[block]]$test == "scaled.shifted") {
                  if (ngroups == 1L || length(TEST[[block]]$shift.parameter) ==
                    1L) {
                    c1 <- c(c1, "Shift parameter")
                    c2 <- c(c2, "")
                    c3 <- c(c3, sprintf(num.format, TEST[[block]]$shift.parameter))
                  }
                  else {
                    c1 <- c(c1, "Shift parameter for each group:")
                    c2 <- c(c2, "")
                    c3 <- c(c3, "")
                    for (g in 1:ngroups) {
                      c1 <- c(c1, sprintf("    %-38s", group.label[[g]]))
                      c2 <- c(c2, "")
                      c3 <- c(c3, sprintf(num.format, TEST[[block]]$shift.parameter[g]))
                    }
                  }
                }
                c1 <- c(c1, paste("  ", TEST[[block]]$label,
                  sep = ""))
                c2 <- c(c2, "")
                c3 <- c(c3, "")
            }
        }
        if (twocolumn && block == BLOCKS[1]) {
            c1 <- c("", c1)
            c2 <- c("Standard", c2)
            c3 <- c("Scaled", c3)
        }
        else {
            c1 <- c("", c1)
            c2 <- c("", c2)
            c3 <- c("", c3)
        }
        if (length(information) > 1L && information[1] != information[2]) {
            c1 <- c(c1, "Information")
            tmp.txt <- information[2]
            c2 <- c(c2, paste(toupper(substring(tmp.txt, 1, 1)),
                substring(tmp.txt, 2), sep = ""))
            c3 <- c(c3, "")
        }
        if (length(h1.information) > 1L && h1.information[1] !=
            h1.information[2]) {
            c1 <- c(c1, "Information saturated (h1) model")
            tmp.txt <- h1.information[2]
            c2 <- c(c2, paste(toupper(substring(tmp.txt, 1, 1)),
                substring(tmp.txt, 2), sep = ""))
            c3 <- c(c3, "")
        }
        if (length(observed.information) > 1L && information[2] ==
            "observed" && (observed.information[1] != observed.information[2])) {
            c1 <- c(c1, "Observed information based on")
            tmp.txt <- observed.information[2]
            c2 <- c(c2, paste(toupper(substring(tmp.txt, 1, 1)),
                substring(tmp.txt, 2), sep = ""))
            c3 <- c(c3, "")
        }
        c1 <- format(c1, width = 43L)
        c2 <- format(c2, width = 8L + max(0, (nd - 3L)) * 4L,
            justify = "right")
        c3 <- format(c3, width = 8L + nd, justify = "right")
        if (twocolumn) {
            M <- cbind(c1, c2, c3, deparse.level = 0)
        }
        else {
            M <- cbind(c1, c2, deparse.level = 0)
        }
        colnames(M) <- rep("", ncol(M))
        rownames(M) <- rep(" ", nrow(M))
        #write.table(M, row.names = TRUE, col.names = FALSE, quote = FALSE)
		BSkyFormat(M, singleTableOutputHeader= "Model Test User Model")
		
        ngroups <- ngroups
        if (ngroups > 1L) {
            c1 <- c2 <- c3 <- character(ngroups)
            for (g in 1:ngroups) {
                tmp <- sprintf("  %-40s", group.label[[g]])
                c1[g] <- format(tmp, width = 43L)
                if (!twocolumn) {
                  tmp <- sprintf(num.format, TEST[[block]]$stat.group[g])
                  c2[g] <- format(tmp, width = 8L + max(0, (nd -
                    3L)) * 4L, justify = "right")
                }
                else {
                  tmp <- sprintf(num.format, TEST[[scaled.idx]]$stat.group[g])
                  c2[g] <- format(tmp, width = 8L + max(0, (nd -
                    3L)) * 4L, justify = "right")
                  tmp <- sprintf(num.format, TEST[[block]]$stat.group[g])
                  c3[g] <- format(tmp, width = 8L + nd, justify = "right")
                }
            }
            if (twocolumn) {
                M <- cbind(c1, c2, c3, deparse.level = 0)
            }
            else {
                M <- cbind(c1, c2, deparse.level = 0)
            }
            colnames(M) <- rep("", ncol(M))
            rownames(M) <- rep(" ", nrow(M))
            #cat("  Test statistic for each group:\n")
            #write.table(M, row.names = TRUE, col.names = FALSE,quote = FALSE)
			BSkyFormat(M, singleTableOutputHeader= "Test statistic for each group")
        }
    }
}