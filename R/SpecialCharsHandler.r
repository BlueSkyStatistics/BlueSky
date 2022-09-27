#generate unique col name for the columns those does not have any name( happens with excel)
# maxval is the  maximum counter value. Counter is postfix to the new col name (eg Var1, 1 is the counter here). 
# So counter can go upto maxval (ie 50000 is default below)
GenerateUniqueColName <- function(datasetname, maxval=50000)
{
	## This only works if dataset is already loaded : datasetname <- BSkyValidateDataset(datasetname)
	# datasetname <- paste(".GlobalEnv$", datasetname, sep = "")
	# print(datasetname)
	logflag=FALSE
	eval(parse(text=paste('objexists <- exists("',datasetname,'", envir=.GlobalEnv)',sep=''))) #dataset object exists
	# print(objexists)
	datasetname <- paste(".GlobalEnv$", datasetname, sep = "")
	eval(parse(text=paste('isdataframe <- is.data.frame(',datasetname,')'))) #object is data.frame
	# print(isdataframe)
	eval(parse(text=paste('hascolumns <- ncol(',datasetname,') > 0'))) #20Oct2016 data.frame has cols
	# print(hascolumns)
	eval(parse(text=paste('hasrows <- nrow(',datasetname,') > 0'))) #20Oct2016 data.frame has rows
	# print(hasrows)
	if(objexists && isdataframe && hascolumns)#20Oct2016 Added hascolumns because for loop below executes for colcout=0.
	{
		eval(parse(text=paste('colcount <- ncol(',datasetname,')' )))
		for(i in 1 : colcount) #FINDING:this loop steps in positive(i++) or negative(i--) direction. So it will always execute for colcount=0 too
		{ 
			#cat("\ni=")
			#print(i)
			if(logflag)
			{
				cat('\n-----------------------------------------------------------------')
				cat("\nOldName = ")
				print( eval(parse(text=paste('names(',datasetname,'[i])',sep=''))))
			}
			#remove leading and trailing whitespace from a col name
			eval(parse(text=paste('dscolname = gsub("(^[[:space:]]+|[[:space:]]+$)", "", names(',datasetname,'[i]))' )))
			# eval(parse(text=paste('dscolname = RemoveNonASCIIchars(dscolname)',sep=''))) ##sanitize while loading
			
			# print(nchar(dscolname))
			# print(dscolname)
			if( nchar(dscolname) < 1 || is.na(dscolname)) # if col name is blank. generate new name for it
			{
				colnewname=''
				#generate unique col name
				for(j in 1 : maxval)
				{
					colnewname <- paste('Var',j,sep='')
					eval(parse(text=paste('colexists <- colnewname %in% names(',datasetname,')'))) #colname exists in the dataset
					#print(colexists)
					if(colexists) #(colnewname %in% names(datasetname))
					{
						next
					}
					else
					{
						break
					}
				}
				#assign new name to the col that had a blank name
				eval(parse(text=paste('names(',datasetname,')[i] <- colnewname')))
				#cat(colnewname,':')
				if(logflag)
				{
					cat(" :: NewName = ")
					print(eval(parse(text=paste('names(',datasetname,'[i])',sep=''))))
					cat('\n-----------------------------------------------------------------')
				}				
			}
			else # if colname was not blank. It might had spaces(leading/trailing)
			{
				eval(parse(text=paste('names(',datasetname,')[i] <- dscolname'))) # colname without leading/ trailing spaces assigned back
				#cat(dscolname,':')
			}
		}
		eval(parse(text=paste('allcolnames <- names(',datasetname,')')))
		invisible(allcolnames)
	}
}



## replace special characters
ReplaceSplChrsAndPrefixXForDigitInBegining <- function(anystring)
{
	BSkyFunctionInit()
	
	BSkyErrMsg = paste("ReplaceSplChrsAndDigitInBegining: Error replace special characters : ",sep="")
	BSkyWarnMsg = paste("ReplaceSplChrsAndDigitInBegining: Warning  replace special characters : ",sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)

	originstr = anystring #for comparison
	logflag=FALSE
	require(stringr) # for str_replace_all()
	require(gdata) #for startsWith()
	
	if(logflag)
	{
		cat('\n___________________________________________________________________\n')
		cat('\nCol name before replacing spl char: ')
		print(anystring)
	}
	#First replace special characters with underscores (_)
	anystring <-  stringr::str_replace_all(anystring, '[^[:alnum:]]', '_')
	#anystring <-  base::make.names(anystring, unique=TRUE)
	if(logflag)
	{
		cat('\nCol name after replacing spl char: ')
		print(anystring)
	}
	#Now if there is any digit in the begining of the anystring then prefix  'X'
		##14Jun2017 res = grep(TRUE, gdata::startsWith(anystring, c(1,2,3,4,5,6,7,8,9,0)) )
		res = grep(TRUE, BSky.startsWith(anystring, c(1,2,3,4,5,6,7,8,9,0)) )##14Jun2017

	if( length(res) >0 ) # a digit is present at the first index position. Prefix  X.
	{
		anystring <- paste('X',anystring,sep='')
	}
	
	res = grep(TRUE, BSky.startsWith(anystring, c("_")) )##14Jun2017

	if( length(res) >0 ) # a digit is present at the first index position. Prefix  X.
	{
		anystring <- paste('X',anystring,sep='')
	}
	
	if(logflag)
	{
		cat('\nCol name after replacing first digit: ')
		print(anystring)
		cat('\n___________________________________________________________________')
	}
	if(anystring!=originstr)
	{
		message = paste("NOTE: The variable name ",originstr, " has one or more special characters that have been replaced by _ ")
		cat(message)
	}
	BSkyFunctionWrapUp()
	
	invisible(anystring)
}

#x = 'a'; charToRaw(x)
#iconv('Kaç_yýldýr', to="ASCII//TRANSLIT")
# res = RemoveNonASCIIchars('Kaç_yýldýr', FALSE)
RemoveNonASCIIchars <- function(nonAsciiStr, use_iconv=TRUE)
{
	newstring=""
	tempvector = c()
	if(use_iconv)
	{
		# newstring = iconv(nonAsciiStr, to="ASCII//TRANSLIT")
		newstring = iconv(nonAsciiStr, to="ASCII",sub = "")
	}
	else
	{
		len = nchar(nonAsciiStr)
		nonAsciiStrsplit <- strsplit(nonAsciiStr, "")[[1]]
		cat("\nnonASCIIsplit:\n")
		print(nonAsciiStrsplit)

		for (achar in nonAsciiStrsplit) 
		{
			asciiCode=as.integer(charToRaw(achar))
			if(asciiCode>127)
			{
				tempvector=c(tempvector, '_')
			}
			else
			{
				tempvector=c(tempvector, achar)
			}
		}
		cat("\nnewvec:\n")
		print(tempvector)

		newstring = paste(tempvector,collapse='')
		
		# cat("\nnewstring:\n")
		# print(newstring)		
	}
	return(invisible(newstring))
}

#BSkySanitizeDataset('Turnudep_data_reprex')
BSkySanitizeDataset <- function(datasetname, use_iconv=TRUE)
{
	logflag=FALSE
	eval(parse(text=paste('objexists <- exists("',datasetname,'", envir=.GlobalEnv)',sep=''))) #dataset object exists
	# print(objexists)
	datasetname <- paste(".GlobalEnv$", datasetname, sep = "")
	eval(parse(text=paste('isdataframe <- is.data.frame(',datasetname,')'))) #object is data.frame
	# print(isdataframe)
	eval(parse(text=paste('hascolumns <- ncol(',datasetname,') > 0'))) #data.frame has cols
	# print(hascolumns)
	eval(parse(text=paste('hasrows <- nrow(',datasetname,') > 0'))) #data.frame has rows
	# print(hasrows)
	if(objexists && isdataframe && hascolumns)#20Oct2016 Added hascolumns because for loop below executes for colcout=0.
	{
		eval(parse(text=paste('colcount <- ncol(',datasetname,')' )))
		for(i in 1 : colcount) #FINDING:this loop steps in positive(i++) or negative(i--) direction. So it will always execute for colcount=0 too
		{ 
			#cat("\ni=")
			#print(i)
			if(logflag)
			{
				cat('\n-----------------------------------------------------------------')
				cat("\nOldName = ")
				print( eval(parse(text=paste('names(',datasetname,'[i])',sep=''))))
			}
			#remove leading and trailing whitespace from a col name
			eval(parse(text=paste('dscolname = gsub("(^[[:space:]]+|[[:space:]]+$)", "", names(',datasetname,'[i]))' )))
			eval(parse(text=paste('dscolname = RemoveNonASCIIchars(dscolname)',sep=''))) ##sanitize column name
			
			# print(nchar(dscolname))
			# print(dscolname)
			if( nchar(dscolname) < 1 || is.na(dscolname)) # if col name is blank. generate new name for it
			{
				colnewname=''
				#generate unique col name
				for(j in 1 : maxval)
				{
					colnewname <- paste('Var',j,sep='')
					eval(parse(text=paste('colexists <- colnewname %in% names(',datasetname,')'))) #colname exists in the dataset
					#print(colexists)
					if(colexists) #(colnewname %in% names(datasetname))
					{
						next
					}
					else
					{
						break
					}
				}
				#assign new name to the col that had a blank name
				eval(parse(text=paste('names(',datasetname,')[i] <- colnewname')))
				#cat(colnewname,':')
				if(logflag)
				{
					cat(" :: NewName = ")
					print(eval(parse(text=paste('names(',datasetname,'[i])',sep=''))))
					cat('\n-----------------------------------------------------------------')
				}				
			}
			else # if colname was not blank. It might had spaces(leading/trailing)
			{
				eval(parse(text=paste('names(',datasetname,')[i] <- dscolname'))) # colname without leading/ trailing spaces assigned back
				#cat(dscolname,':')
			}
		}
		eval(parse(text=paste('allcolnames <- names(',datasetname,')')))
		invisible(allcolnames)
	}
}

# returns string w/o leading whitespace
bsky.trim.leading <- function (x)  sub("^\\s+", "", x)

# returns string w/o trailing whitespace
bsky.trim.trailing <- function (x) sub("\\s+$", "", x)

# returns string w/o leading or trailing whitespace
bsky.trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## 14 Jun 2017
##replacement for gdata::startsWith(). Ver 2.17 worked well but 2.18.0 failed.
BSky.startsWith <- function (str, pattern, trim = FALSE, ignore.case = FALSE) 
{
    if (trim) 
        str <- trim(str)
    if (ignore.case) {
        str <- toupper(str)
        pattern <- toupper(pattern)
    }
    substr(str, start = 1, stop = nchar(pattern)) == pattern
}