BSkyReadTransactions.old <- function (datasetname, format = c("basket", "single"), sep = "", cols = NULL, 
    rm.duplicates = FALSE, quote = "\"'", skip = 0, encoding = "unknown") 
{
    format <- match.arg(format)
    if (format == "basket") {
        #data <- lapply(readLines(file, encoding = encoding), FUN = function(l) scan(text = l, what = "character", sep = sep, quote = quote, quiet = TRUE, encoding = encoding))
		data <- list()
		rowcount <- nrow(Dataset4)
for(i in 1:rowcount)
{
        data[i] <- lapply(as.character(Dataset4[i,1]), FUN = function(l) scan(text = l, what = "character", sep = ",",quiet = TRUE, encoding = encoding))
}		
        if (skip > 0) 
            data <- data[-(1:skip)]
        if (!is.null(cols)) {
            if (!(is(cols, "numeric") && (length(cols) == 1))) 
                stop("'cols' must be a numeric scalar for 'basket'.")
            cols <- as(cols, "integer")
            names(data) <- sapply(data, "[", cols)
            data <- lapply(data, "[", -cols)
        }
        data <- lapply(data, function(x) gsub("^\\s*|\\s*$", 
            "", x))
        data <- lapply(data, function(x) x[nchar(x) > 0])
        if (rm.duplicates) 
            data <- .rm.duplicates(data)
        return(as(data, "transactions"))
    }
    if (is(cols, "character") && (length(cols) == 2)) {
        #colnames <- scan(file = file, what = "", sep = sep, quote = quote, quiet = TRUE, skip = skip, nlines = 1)
		colnames = names(Dataset2)
        cols <- match(cols, colnames)
        if (any(is.na(cols))) 
            stop("'cols' does not match 2 entries in header of file.")
        skip <- skip + 1
    }
    if (!(is(cols, "numeric") && (length(cols) == 2))) 
        stop("'cols' must be a numeric or character vector of length 2 for 'single'.")
    cols <- as(cols, "integer")
    what <- vector("list", length = max(cols))
    what[cols] <- ""
	entries <- list()
	entries[[1]] <- as.character( Dataset2$id)
	entries[[2]] <- Dataset2$name
    #entries <- scan(file = file, sep = sep, quote = quote, what = what, flush = TRUE, quiet = TRUE, skip = skip)
    entries <- split(entries[[cols[2]]], entries[[cols[1]]])
    if (rm.duplicates) 
        entries <- .rm.duplicates(entries)
    return(as(entries, "transactions"))
}

## to support column name in param datasetname, we are writing a wrapper to change col name to data frame.
## passing Dataset$colname should work better than Dataset[1] because Dataset[1] may become a tibble (usually when col is referred from CSV)
# BSky.Read.Transactions <- function (datasetname, format = c("basket", "single"), sep = "", cols = NULL, 
    # rm.duplicates = FALSE, quote = "\"'", skip = 0, encoding = "unknown") 
# {
	# BSkyTempDF = NULL ##clean it before
	# BSkyTempDF <- eval(parse(text=paste('as.data.frame(',datasetname,')', sep='')))
	# ##calling the original function that only takes dataframes
	# BSkyTempTrans <- BSkyReadTransactions(datasetname='BSkyTempDF', format =format, sep = sep, cols = cols, 
    # rm.duplicates = rm.duplicates, quote = quote, skip = skip, encoding = encoding) 
	# BSkyTempDF = NULL ##clean it after
	# return(BSkyTempTrans)
# }


## read.transactions() from arules
BSkyReadTransactions <- function (datasetname, format = c("basket", "single"), sep = "", cols = NULL, 
    rm.duplicates = FALSE, quote = "\"'", skip = 0, encoding = "unknown") 
{


#### number of rows ####
#noOfRows = eval(parse(text=paste('NROW(',datasetname,')', sep=''))) ## this syntax works but not needed right now.

## to support column name in param datasetname, we are writing a wrapper to change col name to data frame.
## passing Dataset$colname should work better than Dataset[1] because Dataset[1] may become a tibble (usually when col is referred from CSV)
BSkyTempDF = NULL ##clean it before and also at the end of this function
#BSkyTempDF <- eval(parse(text=paste('as.data.frame(',datasetname,'[2:noOfRows])', sep=''))) ##no need to do 2:noOfRows. We are not 
# reading file anymore. so we are safe. We just want to pass one col and col header is already not part of the data-row. In text file though
# if you directly pass it to create transaction, the first row ('Items') will also be considered as a transaction (only in bakket format).
BSkyTempDF <- eval(parse(text=paste('as.data.frame(',datasetname,')', sep=''))) 
datasetname='BSkyTempDF'
# cat("\nFirst: ")
# print(BSkyTempDF[1])
# cat("\nSecond: ")
# print(BSkyTempDF[2])
# cat("\nLast: ")
# print(BSkyTempDF[noOfRows])

    format <- match.arg(format)
    if (format == "basket") {
        #data <- lapply(readLines(file, encoding = encoding), FUN = function(l) scan(text = l, what = "character", sep = sep, quote = quote, quiet = TRUE, encoding = encoding))
		data <- list()
		rowcount <- eval(parse(text=paste('nrow(',datasetname,')', sep='' )))
for(i in 1:rowcount)
{
 data[i] <- eval(parse(text=paste( 'lapply(as.character(',datasetname,'[',i,',1]), FUN = function(l) scan(text = l, what = "character", sep = sep,quiet = TRUE, encoding = encoding))', sep='')))
}		
        if (skip > 0) 
            data <- data[-(1:skip)]
        if (!is.null(cols)) {
            if (!(is(cols, "numeric") && (length(cols) == 1))) 
                stop("'cols' must be a numeric scalar for 'basket'.")
            cols <- as(cols, "integer")
            names(data) <- sapply(data, "[", cols)
            data <- lapply(data, "[", -cols)
        }
        data <- lapply(data, function(x) gsub("^\\s*|\\s*$", 
            "", x))
        data <- lapply(data, function(x) x[nchar(x) > 0])
        if (rm.duplicates) 
            data <- .rm.duplicates(data)
        return(as(data, "transactions"))
    }
    if (is(cols, "character") && (length(cols) == 2)) {
        #colnames <- scan(file = file, what = "", sep = sep, quote = quote, quiet = TRUE, skip = skip, nlines = 1)
		colnames = eval(parse(text=paste('names(',datasetname,')', sep='' )))
        cols <- match(cols, colnames)
        if (any(is.na(cols))) 
            stop("'cols' does not match 2 entries in header of file.")
        skip <- skip + 1
    }
    if (!(is(cols, "numeric") && (length(cols) == 2))) 
        stop("'cols' must be a numeric or character vector of length 2 for 'single'.")
    cols <- as(cols, "integer")
    what <- vector("list", length = max(cols))
    what[cols] <- ""
	entries <- list()
	entries[[1]] <- eval(parse(text=paste('as.character(',datasetname,'[[1]])', sep='')))
	entries[[2]] <- eval(parse(text=paste('as.character(',datasetname,'[[2]])', sep='')))
    #entries <- scan(file = file, sep = sep, quote = quote, what = what, flush = TRUE, quiet = TRUE, skip = skip)
    entries <- split(entries[[cols[2]]], entries[[cols[1]]])
    if (rm.duplicates) 
        entries <- .rm.duplicates(entries)
		
BSkyTempDF = NULL ##clean it after		
    return(as(entries, "transactions"))
}

## read.transactions() from arules
## this code has got as.data.frame ### it is not working if 'Dataset$colname' is passed for datasetname param
BSkyReadTransactions.not <- function (datasetname, format = c("basket", "single"), sep = "", cols = NULL, 
    rm.duplicates = FALSE, quote = "\"'", skip = 0, encoding = "unknown") 
{
    format <- match.arg(format)
    if (format == "basket") {
        #data <- lapply(readLines(file, encoding = encoding), FUN = function(l) scan(text = l, what = "character", sep = sep, quote = quote, quiet = TRUE, encoding = encoding))
		data <- list()
		rowcount <- eval(parse(text=paste('nrow(as.data.frame(',datasetname,'))', sep='' )))
for(i in 1:rowcount)
{
        data[i] <- eval(parse(text=paste( 'lapply(as.character(as.data.frame(',datasetname,')[',i,',1]), FUN = function(l) scan(text = l, what = "character", sep = ",",quiet = TRUE, encoding = encoding))', sep='')))
}		
        if (skip > 0) 
            data <- data[-(1:skip)]
        if (!is.null(cols)) {
            if (!(is(cols, "numeric") && (length(cols) == 1))) 
                stop("'cols' must be a numeric scalar for 'basket'.")
            cols <- as(cols, "integer")
            names(data) <- sapply(data, "[", cols)
            data <- lapply(data, "[", -cols)
        }
        data <- lapply(data, function(x) gsub("^\\s*|\\s*$", 
            "", x))
        data <- lapply(data, function(x) x[nchar(x) > 0])
        if (rm.duplicates) 
            data <- .rm.duplicates(data)
        return(as(data, "transactions"))
    }
    if (is(cols, "character") && (length(cols) == 2)) {
        #colnames <- scan(file = file, what = "", sep = sep, quote = quote, quiet = TRUE, skip = skip, nlines = 1)
		colnames = eval(parse(text=paste('names(as.data.frame(',datasetname,'))', sep='' )))
        cols <- match(cols, colnames)
        if (any(is.na(cols))) 
            stop("'cols' does not match 2 entries in header of file.")
        skip <- skip + 1
    }
    if (!(is(cols, "numeric") && (length(cols) == 2))) 
        stop("'cols' must be a numeric or character vector of length 2 for 'single'.")
    cols <- as(cols, "integer")
    what <- vector("list", length = max(cols))
    what[cols] <- ""
	entries <- list()
	entries[[1]] <- eval(parse(text=paste('as.character(as.data.frame(',datasetname,')[[1]])', sep='')))
	entries[[2]] <- eval(parse(text=paste('as.character(as.data.frame(',datasetname,')[[2]])', sep='')))
    #entries <- scan(file = file, sep = sep, quote = quote, what = what, flush = TRUE, quiet = TRUE, skip = skip)
    entries <- split(entries[[cols[2]]], entries[[cols[1]]])
    if (rm.duplicates) 
        entries <- .rm.duplicates(entries)
    return(as(entries, "transactions"))
}


###following 3 are not in use
GetDataFormat <- function(datasetname)
{
	datatype=character(0)
	colcount = eval(parse(text=paste("ncol(",datasetname,")", sep='')))

	## Basket data format has 1 col while single data format has 2.
	if(colcount==1)
	{
		#another check for comma separated values
		rowdata = eval(parse(text=paste("as.data.frame(",datasetname,"[,1]", sep='')))
		#check for separator in one row.
		if(true)#separator found)
		{
			datatype = "basket"  #single line transaction
		}
	}
	else if( colcount==2)
	{
		if(false)#condition met)
		{
		datatype="single" #multi line tansaction
		}
	}
	else
	{
		datatype="invalid" #data type not supported for market basket analysis
	}
}

#does the current dataset contains data in basket format
preReq.isBasketFormat<- function(datasetname)
{
	result = GetDataFormat(datasetname)
	if(result=="basket")
	{
		msg="Success"
	}
	else
	{
		msg="Error: Current dataset does not contain data in basket format"
	}
}

#does the current dataset contains data in single format
preReq.isSingleFormat <- function(datasetname)
{
	result = GetDataFormat(datasetname)
	if(result=="single")
	{
		msg="Success"
	}
	else
	{
		msg="Error: Current dataset does not contain data in single format"
	}
}