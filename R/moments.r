rk.describe.alternative <-function (x)
{
    res <- ""
    if (!is.null(x$alternative)) {
        if (!is.null(x$null.value)) {
            if (length(x$null.value) == 1) {
                alt.char <- switch(x$alternative, two.sided = "not equal to",
                  less = "less than", greater = "greater than")
                res <- paste("true", names(x$null.value), "is",
                  alt.char, x$null.value)
            }
            else {
                res <- paste(x$alternative, "\nnull values:\n",
                  x$null.value)
            }
        }
        else {
            res <- (x$alternative)
        }
    }
    res
}