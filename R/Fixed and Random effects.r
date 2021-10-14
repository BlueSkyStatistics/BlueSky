### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Mixed Models
#'
#' @description Print model summary
#'
#' @param x 
#' @param digits 
#' @param correlation 
#' @param symbolic.cor 
#' @param signif.stars 
#' @param ranef.comp 
#' @param show.resids 
#'
#' @return
#'
#' @examples
BSkyprint.summary.merMod  <-function (x, digits = max(3, getOption("digits") - 3), correlation = NULL, 
    symbolic.cor = FALSE, signif.stars = getOption("show.signif.stars"), 
    ranef.comp = c("Variance", "Std.Dev."), show.resids = TRUE, 
    ...) 
{
     obj1 <-.prt.methTit(x$methTitle, x$objClass)
    obj2 <-.prt.family(x)
    obj3<- .prt.call(x$call)
    #cat("\n")
    obj4<-.prt.aictab(x$AICtab)
   # cat("\n")
    if (show.resids) 
    {
        BSky.prt.resids(x$residuals, digits = digits)
    }
    obj5 <-BSky.prt.VC(varcor =x$varcor, digits = digits, useScale = x$useScale, 
        comp = ranef.comp)
   obj6<- .prt.grps(x$ngrps, nobs = x$devcomp$dims[["n"]])
    p <- nrow(x$coefficients)
    if (p > 0) {
        #cat("\nFixed effects:\n")
        #printCoefmat(x$coefficients, digits = digits, signif.stars = signif.stars)
		 BSkyFormat(x$coefficients,singleTableOutputHeader="Fixed Effects")
        hasCor <- !is.null(VC <- x$vcov) && !is.null(VC@factors$correlation)
        if (is.null(correlation)) {
            cor.max <- getOption("lme4.summary.cor.max")
            correlation <- hasCor && p <= cor.max
            if (!correlation && p > cor.max) {
                nam <- deparse(substitute(x))
                if (length(nam) > 1 || nchar(nam) >= 32) 
                {
                  nam <- "...."
                }   
                message(sprintf(paste("\nCorrelation matrix not shown by default, as p = %d > %d.", 
                  "Use print(%s, correlation=TRUE)  or", "    vcov(%s)        if you need it\n", 
                  sep = "\n"), p, cor.max, nam, nam))
            }
        }
        else if (!is.logical(correlation)) 
            {
            stop("'correlation' must be NULL or logical")
            }
        if (correlation) {
            if (is.null(VC)) {
                VC <- vcov(x, correlation = TRUE)
            }               
            corF <- VC@factors$correlation
            if (is.null(corF)) {
                message("\nCorrelation of fixed effects could have been required in summary()")
                corF <- cov2cor(VC)
            }
            p <- ncol(corF)

            if (p > 1) {
                rn <- rownames(x$coefficients)
                rns <- abbreviate(rn, minlength = 11)
                #cat("\nCorrelation of Fixed Effects:\n")
                if (is.logical(symbolic.cor) && symbolic.cor) {
                  corf <- as(corF, "matrix")
                  dimnames(corf) <- list(rns, abbreviate(rn, 
                    minlength = 1, strict = TRUE))
                    corf =symnum(corf)
                    BSkyFormat(corf,singleTableOutputHeader="Correlation of Fixed Effects:")
                 # print(symnum(corf))
                    #help(symnum)
                }
                else {
                  corf <- matrix(format(round(corF@x, 3), nsmall = 3), 
                    ncol = p, dimnames = list(rns, abbreviate(rn, 
                      minlength = 6)))
                  corf[!lower.tri(corf)] <- ""
                     BSkyFormat(corf[, c(-p)], singleTableOutputHeader="Correlation of Fixed Effects:")
                 # print(corf[-1, -p, drop = FALSE], quote = FALSE)
                }
            }
        }

    }
    if (length(x$fitMsgs) && any(nchar(x$fitMsgs) > 0)) {
        cat("fit warnings:\n")
        writeLines(x$fitMsgs)
    }
    .prt.warn(x$optinfo, summary = FALSE)
    invisible(x)
}


BSky.prt.resids <-function (resids, digits, title = "Scaled residuals:", ...)
{
   # cat(title, "\n")
    rq <- setNames(zapsmall(quantile(resids, na.rm = TRUE), digits + 
        1L), c("Min", "1Q", "Median", "3Q", "Max"))
   #print(rq, digits = digits)
    BSkyFormat(rq, singleTableOutputHeader="Scaled Residuals")
    cat("\n")
}

#################################
BSky.prt.VC <-function (varcor, digits, comp, formatter = format, ...)
{
    if (missing(comp)) {
        fVC <- formatVC(varcor, digits = digits, formatter = formatter)
    }
    else {
        fVC = formatVC(varcor, digits = digits, formatter = formatter, 
            comp = comp)
    }
    #colnames(fVC) <- NULL
    #rownames(fVC) <- NULL
    BSkyFormat(fVC, singleTableOutputHeader = "Random Effects")
    invisible(fVC)
}