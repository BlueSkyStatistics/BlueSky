
lav_print_loadings_aar <- function (x, nd = 3L, cutoff = 0.3, dot.cutoff = 0.1, alpha.level = 0.01,
    resvar = NULL, x.se = NULL)
{
    y <- unclass(x)
    y <- format(round(y, nd), width = 3L + nd, justify = "right")
    colnames(y) <- format(colnames(y), width = 3L + nd, justify = "right")
    dot.string <- format(".", width = 3L + nd, justify = "right")
    empty.string <- format(" ", width = 3L + nd)
    if (dot.cutoff < cutoff) {
        y[abs(x) < cutoff & abs(x) > dot.cutoff] <- dot.string
    }
    y[abs(x) < min(dot.cutoff, cutoff)] <- empty.string
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
    if (!is.null(resvar)) {
        NAMES <- colnames(y)
        y <- cbind(y, format(round(cbind(resvar, 1 - resvar),
            nd), width = 12L + nd, justify = "right"))
        resvar.names <- format(c("unique.var", "communalities"),
            width = 12L + nd, justify = "right")
        colnames(y) <- c(NAMES, resvar.names)
    }
    print(y, quote = FALSE)
	BSkyFormat(y, singleTableOutputHeader= "Loadings")
}