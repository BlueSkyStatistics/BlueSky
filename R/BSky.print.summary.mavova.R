#For class = summary.manova
BSky.print.summary.manova <-function (x, digits = getOption("digits"), ...)
{
    table_list =NULL
	table_list_names =NULL
	if (length(stats <- x$stats)) {
        table_list <- BSky.print.anova(stats)
    }
    else {
        cat("No error degrees of freedom\n\n")
		#print(data.frame(Df = x$Df, row.names = x$row.names))
		attr(x$Df, "BSkyFootnote_BSky.print.summary.manova_1") <-"No error degrees of freedom"
		table_list = c(table_list, x$Df)
		names(table_list) = "MANOVA summary"
	}
	#invisible(x)
    invisible(table_list)
}


BSky.print.anova <- function (x, digits = max(getOption("digits") - 2L, 3L), signif.stars = getOption("show.signif.stars"),
    ...)
{
    table_list =NULL
	table_list_names =NULL
	if (!is.null(heading <- attr(x, "heading")))
        cat(heading, sep = "\n")
    nc <- dim(x)[2L]
    if (is.null(cn <- colnames(x)))
        stop("'anova' object must have colnames")
    has.P <- grepl("^(P|Pr)\\(", cn[nc])
    zap.i <- 1L:(if (has.P)
        nc - 1
    else nc)
    i <- which(substr(cn, 2, 7) == " value")
    i <- c(i, which(!is.na(match(cn, c("F", "Cp", "Chisq")))))
    if (length(i))
        zap.i <- zap.i[!(zap.i %in% i)]
    tst.i <- i
    if (length(i <- grep("Df$", cn)))
        zap.i <- zap.i[!(zap.i %in% i)]
    # printCoefmat(x, digits = digits, signif.stars = signif.stars,
        # has.Pvalue = has.P, P.values = has.P, cs.ind = NULL,
        # zap.ind = zap.i, tst.ind = tst.i, na.print = "", ...)
    
	#invisible(x)
	class(x) <-"matrix"
	table_list = c(table_list, list(x))
		names(table_list) = "Manova summary"
		 invisible(table_list)
}


#For class = "summary.aov" "listof"

BSky.summary.aov  <- function (x)
{
	table_list =NULL
	table_list_names =NULL
	if (!is.null(x))
	{
	noofitems=length(x)
	for ( i in 1:noofitems)
		{
			temp = as.data.frame(x[[i]])
			table_list = c(table_list, list(temp))
			table_list_names = c(table_list_names, names(x)[i])
		}
	names(table_list) = table_list_names
	return(table_list)
	}
}

	