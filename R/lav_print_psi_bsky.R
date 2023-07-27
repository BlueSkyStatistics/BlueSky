
lav_print_psi_bsky <-function (x, nd = 3L, alpha.level = 0.01, x.se = NULL)
{
    y <- unclass(x)
    y <- format(round(y, nd), width = 3L + nd, justify = "right")
    colnames(y) <- format(colnames(y), width = 3L + nd, justify = "right")
    if (!is.null(x.se) && !any(is.na(x.se))) {
        colNAMES <- colnames(y)
        rowNAMES <- rownames(y)
        x.se[x.se < sqrt(.Machine$double.eps)] <- 1
        zstat <- x/x.se
        z.cutoff <- qnorm(1 - (alpha.level/2))
        zstat.string <- ifelse(abs(zstat) > z.cutoff, "*", " ")
        y <- matrix(paste(y, zstat.string, sep = ""), nrow(y),
            ncol(y))
        colnames(y) <- colNAMES
        rownames(y) <- rowNAMES
    }
    ll <- upper.tri(x, diag = FALSE)
    y[ll] <- ""
    #print(y, quote = FALSE)
	BSkyFormat(y, singleTableOutputHeader= "PSI")
}