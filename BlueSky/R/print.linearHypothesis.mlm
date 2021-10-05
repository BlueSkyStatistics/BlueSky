
BSkyprint.linearHypothesis.mlm <-function (x, SSP = TRUE, SSPE = SSP, digits = getOption("digits"),
    ...)
{
    test <- x$test
    if (!is.null(x$P) && SSP) {
        P <- x$P
        #cat("\n Response transformation matrix:\n")
		tableHeader1="Response transformation matrix"
        attr(P, "assign") <- NULL
        attr(P, "contrasts") <- NULL
        #print(P, digits = digits)
		class (P) <-"matrix"
		table_list  <- list(P)
		table_list_names = tableHeader1
		
    }
    if (SSP) {
        #cat("\nSum of squares and products for the hypothesis:\n")
		tableHeader2 = "Sum of squares and products for the hypothesis:"
		class(x$SSPH) <-"matrix"
		table_list = c(table_list, list(x$SSPH))
        #print(x$SSPH, digits = digits)
		table_list_names = c(table_list_names, tableHeader2)
    }
    if (SSPE) {
        #cat("\nSum of squares and products for error:\n")
		tableHeader3 = "Sum of squares and products for error:"
        #print(x$SSPE, digits = digits)
		class(x$SSPE) <-"matrix"
		table_list = c(table_list, list(x$SSPE))
		table_list_names = c(table_list_names, tableHeader3)
    }
    if ((!is.null(x$singular)) && x$singular) {
        warning("the error SSP matrix is singular; multivariate tests are unavailable")
        return(invisible(x))
    }
    SSPE.qr <- qr(x$SSPE)
    eigs <- Re(eigen(qr.coef(SSPE.qr, x$SSPH), symmetric = FALSE)$values)
    tests <- matrix(NA, 4, 4)
    rownames(tests) <- c("Pillai", "Wilks", "Hotelling-Lawley",
        "Roy")
    if ("Pillai" %in% test)
        tests[1, 1:4] <- car:::Pillai(eigs, x$df, x$df.residual)
    if ("Wilks" %in% test)
        tests[2, 1:4] <- car:::Wilks(eigs, x$df, x$df.residual)
    if ("Hotelling-Lawley" %in% test)
        tests[3, 1:4] <- car:::HL(eigs, x$df, x$df.residual)
    if ("Roy" %in% test)
        tests[4, 1:4] <- car:::Roy(eigs, x$df, x$df.residual)
    tests <- na.omit(tests)
    ok <- tests[, 2] >= 0 & tests[, 3] > 0 & tests[, 4] > 0
    ok <- !is.na(ok) & ok
    tests <- cbind(x$df, tests, pf(tests[ok, 2], tests[ok, 3],
        tests[ok, 4], lower.tail = FALSE))
    colnames(tests) <- c("Df", "test stat", "approx F", "num Df",
        "den Df", "Pr(>F)")
	#Original had class anova that I removed, original is commented below
    # tests <- structure(as.data.frame(tests), heading = paste("\nMultivariate Test",
        # if (nrow(tests) > 1)
            # "s", ": ", x$title, sep = ""), class = c("anova",
        # "data.frame"))
	#Here is the revised
	tests <- structure(as.data.frame(tests), heading = paste("\nMultivariate Test",
        if (nrow(tests) > 1)
            "s", ": ", x$title, sep = ""), class = c("data.frame"))
	tableHeader4 = paste("Multivariate Test",
        if (nrow(tests) > 1)
            "s", ": ", x$title, sep = "")
    table_list = c(table_list, list(tests))
	table_list_names = c(table_list_names, tableHeader4)
	#print(tests, digits = digits)
	names(table_list) = table_list_names
    invisible( table_list)
}