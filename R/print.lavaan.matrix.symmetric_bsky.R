print.lavaan.matrix.symmetric_bsky	<- function (x, ..., nd = 3L, shift = 0L, diag.na.dot = TRUE, message)
{
    x <- as.matrix(x)
    y <- x
    y <- unclass(y)
    attributes(y)[c("header", "footer")] <- NULL
    ll <- lower.tri(x, diag = TRUE)
    y[ll] <- format(round(x[ll], digits = nd))
    y[!ll] <- ""
    if (diag.na.dot) {
        diag.idx <- lav_matrix_diag_idx(ncol(x))
        tmp <- x[diag.idx]
        if (all(is.na(tmp))) {
            y[diag.idx] <- paste(strrep(" ", nd + 2L), ".", sep = "")
        }
    }
    if (!is.null(colnames(x))) {
        colnames(y) <- abbreviate(colnames(x), minlength = nd +
            3L)
    }
    if (shift > 0L) {
        empty.string <- rep(strrep(x = " ", times = shift), times = nrow(x))
        if (!is.null(rownames(x))) {
            rownames(y) <- paste(empty.string, rownames(x), sep = "")
        }
        else {
            rownames(y) <- empty.string
        }
    }
    if (!is.null(attr(x, "header"))) {
        cat("\n", attr(x, "header"), "\n\n", sep = "")
    }
    #print(y, ..., quote = FALSE, right = TRUE)
	
    if (!is.null(attr(x, "footer"))) {
        #cat("\n", attr(x, "footer"), "\n\n", sep = "")
      	footer =attr(x, "footer")
      attr(y, "BSkyFootnote_footer") = footer
	  
    }
	BSkyFormat(y, singleTableOutputHeader=message)
    invisible(x)
}