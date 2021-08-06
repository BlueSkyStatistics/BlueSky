BSkyPrintTukeyHSD <-function (x, digits = getOption("digits"), ...) 
{
   # cat("  Tukey multiple comparisons of means\n")
BSkyFormat("Tukey multiple comparisons of means")
  #  cat("    ", format(100 * attr(x, "conf.level"), 2), "% family-wise confidence level\n", sep = "")
line1=paste(format(100 * attr(x, "conf.level"), 2), "% family-wise confidence level", sep = "")
BSkyFormat(line1)
    if (attr(x, "ordered")) 
       # cat("    factor levels have been ordered\n")
BSkyFormat("factor levels have been ordered")
   # cat("\nFit: ", deparse(attr(x, "orig.call"), 500L), "\n\n",  sep = "")
  line2 =paste("Fit: ", deparse(attr(x, "orig.call"), 500L),   sep = "")
BSkyFormat(line2)
    xx <- unclass(x)
    attr(xx, "orig.call") <- attr(xx, "conf.level") <- attr(xx, 
        "ordered") <- NULL
    xx[] <- lapply(xx, function(z, digits) {
        z[, "p adj"] <- round(z[, "p adj"], digits)
        z
    }, digits = digits)
  #  print.default(xx, digits, ...)
#BSkyFormat(as.matrix(xx))
BSkyFormat(xx)
    invisible(x)
}



BSkyTukeyHSD <-function (x, which = seq_along(tabs), ordered = FALSE, conf.level = 0.95, 
    ...) 
{
    mm <- model.tables(x, "means")
    if (is.null(mm$n)) 
        stop("no factors in the fitted model")
    tabs <- mm$tables[-1L]
    tabs <- tabs[which]
    nn <- mm$n[names(tabs)]
    nn_na <- is.na(nn)
    if (all(nn_na)) 
        stop("'which' specified no factors")
    if (any(nn_na)) {
        warning("'which' specified some non-factors which will be dropped")
        tabs <- tabs[!nn_na]
        nn <- nn[!nn_na]
    }
    out <- setNames(vector("list", length(tabs)), names(tabs))
    MSE <- sum(x$residuals^2)/x$df.residual
    for (nm in names(tabs)) {
        tab <- tabs[[nm]]
        means <- as.vector(tab)
        nms <- if (length(dim(tab)) > 1L) {
            dn <- dimnames(tab)
            apply(do.call("expand.grid", dn), 1L, paste, collapse = ":")
        }
        else names(tab)
        n <- nn[[nm]]
        if (length(n) < length(means)) 
            n <- rep.int(n, length(means))
        if (as.logical(ordered)) {
            ord <- order(means)
            means <- means[ord]
            n <- n[ord]
            if (!is.null(nms)) 
                nms <- nms[ord]
        }
        center <- outer(means, means, "-")
        keep <- lower.tri(center)
        center <- center[keep]
        width <- qtukey(conf.level, length(means), x$df.residual) * 
            sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep]
        est <- center/(sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep])
        pvals <- ptukey(abs(est), length(means), x$df.residual, 
            lower.tail = FALSE)
        dnames <- list(NULL, c("diff", "lwr", "upr", "p adj"))
        if (!is.null(nms)) 
            dnames[[1L]] <- outer(nms, nms, paste, sep = "-")[keep]
        out[[nm]] <- array(c(center, center - width, center + 
            width, pvals), c(length(width), 4L), dnames)
    }
    class(out) <- c("TukeyHSD", "multicomp")
    attr(out, "orig.call") <- x$call
    attr(out, "conf.level") <- conf.level
    attr(out, "ordered") <- ordered
    BSkyPrintTukeyHSD(out)
}