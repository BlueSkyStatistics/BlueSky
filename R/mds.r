BSkyprint.dist <-function (x, diag = NULL, upper = NULL, digits = getOption("digits"),
    justify = "none", right = TRUE, ...)
{
    if (length(x)) {
        if (is.null(diag))
            diag <- attr(x, "Diag") %||% FALSE
        if (is.null(upper))
            upper <- attr(x, "Upper") %||% FALSE
        m <- as.matrix(x)
        cf <- format(m, digits = digits, justify = justify)
        if (!upper)
            cf[row(cf) < col(cf)] <- ""
        if (!diag)
            cf[row(cf) == col(cf)] <- ""
        if (diag || upper)
          {
            BSkyFormat(cf, singleTableOutputHeader ="Distance matrix")
          }
        else
          {
          cf <- cf[-1, -attr(x, "Size"), drop = FALSE]
           BSkyFormat(cf, singleTableOutputHeader ="Distance matrix")
          }
    }
    else {
        cat(data.class(x), "(0)\n", sep = "")
    }
    invisible(x)
}