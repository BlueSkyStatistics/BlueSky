
bin.var <-function (x, bins = 4, method = c("intervals", "proportions",  "natural"), labels = FALSE) 
{
    method <- match.arg(method)
    if (length(x) < bins) {
        stop(gettextRcmdr("The number of bins exceeds the number of data values"))
    }
    x <- if (method == "intervals") 
        cut(x, bins, labels = labels)
    else if (method == "proportions") 
        cut(x, quantile(x, probs = seq(0, 1, 1/bins), na.rm = TRUE), 
            include.lowest = TRUE, labels = labels)
    else {
        xx <- na.omit(x)
        breaks <- c(-Inf, tapply(xx, KMeans(xx, bins)$cluster, 
            max))
        cut(x, breaks, labels = labels)
    }
    as.factor(x)
}

BSkyStandardizeVars.old <-function(vars,prefix, datasetname, excludeEnvPrefix=FALSE)
{
	len =length(vars)
	
 #23Jan2016 Adding logic to add .Global if its not there
	if(!excludeEnvPrefix )
	{
		charcount <- nchar(datasetname)
		if(charcount > 11) # .GlobalEnv$
		{
			if(substr(datasetname, 1,11)!= '.GlobalEnv$')
			{
				datasetname <- paste(".GlobalEnv$",datasetname, sep='')
			}
		}
		else
		{
			datasetname <- paste(".GlobalEnv$",datasetname, sep='')
		}
	}	
	
	.Z <- eval(parse(text = paste("scale(", datasetname, "[,", paste(deparse(vars),collapse=''), "])")))
	#.Z<-eval(parse(text=paste("scale(", datasetname,"[,",deparse(vars,width.cutoff=499),"])")))

	for (i in 1:len)
	{
		eval(parse(text=paste(datasetname, "$",prefix,vars[i],"<-",".Z[,i]",sep=""))) # <<- changed to <- by Anil
		i=i+1
	}
}

#Aaron changed 27Jun2018
BSkyStandardizeVars.old2 <-function (vars, stingToPrefixOrSuffix, prefixOrSuffix ,datasetname, excludeEnvPrefix = FALSE) 
{
    len = length(vars)
    if (!excludeEnvPrefix) {
        charcount <- nchar(datasetname)
        if (charcount > 11) {
            if (substr(datasetname, 1, 11) != ".GlobalEnv$") {
                datasetname <- paste(".GlobalEnv$", datasetname, 
                  sep = "")
            }
        }
        else {
            datasetname <- paste(".GlobalEnv$", datasetname, 
                sep = "")
        }
    }
    if (prefixOrSuffix =="Prefix")
    {       
    .Z <- eval(parse(text = paste("scale(", datasetname, "[,", 
        paste(deparse(vars), collapse = ""), "])")))
    for (i in 1:len) {
        eval(parse(text = paste(datasetname, "$", stingToPrefixOrSuffix, vars[i], 
            "<-", ".Z[,i]", sep = "")))
        i = i + 1
    }
    }
    else 
    {
         .Z <- eval(parse(text = paste("scale(", datasetname, "[,", 
        paste(deparse(vars), collapse = ""), "])")))
    for (i in 1:len) {
        eval(parse(text = paste(datasetname, "$",  vars[i],  stingToPrefixOrSuffix,
            "<-", ".Z[,i]", sep = "")))
        i = i + 1
    }
    }
}


#Aaron changed 10Dec2018. For Daniel
### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Standardize variable(s)
#'
#' @description Standardizes variables (z scores).  The standardized values are stored in new variables with either the prefix or suffix of the original variables. The option is provide to center and/or scale.
#' Note: We will not convert from numeric to factor. When  a numeric is recoded, it will remain a numeric, when a factor variable is recoded it will remain a factor.
#' 
#' @param vars: One or more variables to standardize. Only numeric variables (not factors) supported.
#' @param center:  If center is TRUE then centering is done by subtracting the column means (omitting NAs) of x from their corresponding columns, and if center is FALSE, no centering is done.
#' @param scale: If scale is TRUE then scaling is done by dividing the (centered) columns of x by their standard deviations if center is TRUE, and the root mean square otherwise. If scale is FALSE, no scaling is done.
#' @param stingToPrefixOrSuffix:  A character string that specifies the prefixor suffix  to use for the new standardized variables( i.e. new columns in the dataset).
#' @param prefixOrSuffix: specify if you want a prefix or a suffix
#' @param datasetname: The dataset/dataframe name
#'
#' @return
#'
#' @examples
BSkyStandardizeVars <-function (vars, center,scale, stingToPrefixOrSuffix, prefixOrSuffix, datasetname, excludeEnvPrefix = FALSE) 
{
    len = length(vars)
    if (!excludeEnvPrefix) {
        charcount <- nchar(datasetname)
        if (charcount > 11) {
            if (substr(datasetname, 1, 11) != ".GlobalEnv$") {
                datasetname <- paste(".GlobalEnv$", datasetname, 
                  sep = "")
            }
        }
        else {
            datasetname <- paste(".GlobalEnv$", datasetname, 
                sep = "")
        }
    }
    if (prefixOrSuffix == "Prefix") {
        .Z <- eval(parse(text = paste("scale(", datasetname, 
            "[,", paste(deparse(vars), collapse = ""), "], center=" ,toString(center), ", scale =", toString(scale), ")")))
        for (i in 1:len) {
            eval(parse(text = paste(datasetname, "$", stingToPrefixOrSuffix, "_",
                vars[i], "<-", ".Z[,i]", sep = "")))
            i = i + 1
        }
    }
    else {
        .Z <- eval(parse(text = paste("scale(", datasetname, 
            "[,", paste(deparse(vars), collapse = ""), "], center=" ,toString(center), ", scale =", toString(scale), ")")))
        for (i in 1:len) {
            eval(parse(text = paste(datasetname, "$", vars[i], "_",
                stingToPrefixOrSuffix, "<-", ".Z[,i]", sep = "")))
            i = i + 1
        }
    }
}



KMeans <- function (x, centers, iter.max = 10, num.seeds = 10) 
{
    if (mode(x) == "numeric") 
        x <- data.frame(new.x = x)
    KM <- kmeans(x = x, centers = centers, iter.max = iter.max)
    for (i in 2:num.seeds) {
        newKM <- kmeans(x = x, centers = centers, iter.max = iter.max)
        if (sum(newKM$withinss) < sum(KM$withinss)) {
            KM <- newKM
        }
    }
    KM$tot.withinss <- sum(KM$withinss)
    xmean <- apply(x, 2, mean)
    centers <- rbind(KM$centers, xmean)
    bss1 <- as.matrix(dist(centers)^2)
    KM$betweenss <- sum(as.vector(bss1[nrow(bss1), ]) * c(KM$size, 
        0))
    return(KM)
}
