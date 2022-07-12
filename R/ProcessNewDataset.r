# https://rdrr.io/cran/janitor/man/remove_empty.html
#
# https://stats.stackexchange.com/questions/9067/removing-empty-columns-from-a-dataset
# 
# https://stackoverflow.com/questions/6437164/removing-empty-rows-of-a-data-file-in-r
#
# https://stackoverflow.com/questions/17672649/r-remove-multiple-empty-columns-of-character-variables
#
# This function remove empty rows and columns from a new dataset.
# 

BSkyProcessNewDataset <-function(datasetName, NAstrings = c("NA"), stringAsFactor=TRUE, excludechars=c("", NA))
{
	BSkyFunctionInit()

	if( eval(parse(text=paste('(!exists("',datasetName,'") || is.null(',datasetName,'))',sep='' ))))
	{
		msg = paste(datasetName," does not exist in BSkyProcessNewDataset function")
		warning(msg)
		BSkyFunctionWrapUp()
		return(invisible(BSkyReturnStructure()))
	}
	BSkySetCurrentDatasetName(datasetName)
	
	BSkyErrMsg = paste("BSkyProcessNewDataset: Error in Loading empty dataset : ", "DataSetName :", datasetName," ", sep="")
	BSkyWarnMsg = paste("BSkyProcessNewDataset: Warning in Loading empty dataset : ", "DataSetName :", datasetName," ", sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	datasetname <- datasetName
	tryCatch(
		{
		withCallingHandlers(
		{
			
			#### Method 1 ## We remove all empty rows and cols from a dataset ##
			if(FALSE) #not in use
			{
				#find how many rows and cols will remain after removing empty ones.
				finalrows = eval(parse(text=paste('nrow(',datasetname,'[!apply(is.na(',datasetname,') | ',datasetname,' == "", 1, all),])', sep='')))
				#finalcols = eval(parse(text=paste('ncol(', datasetname,'[!apply(',datasetname,', 2, function(var) length(unique(var)) == 1)])',sep='')))
				finalcols = eval(parse(text=paste('ncol(', datasetname,'[!sapply(',datasetname,',function(x) all(x == ""))])',sep='')))


				if(finalrows > 0 && finalcols > 0)
				{
					## remove empty rows or rows having all NAs
					eval(parse(text=paste(datasetname, '<<-',datasetname,'[!apply(is.na(',datasetname,') | ',datasetname,' == "", 1, all),]', sep='')))
					
					## remove empty columns.
					## This does not seem to work if less than 2x2 matrix is created
					#eval(parse(text=paste(datasetname, '<<-', datasetname,'[!apply(',datasetname,', 2, function(var) length(unique(var)) == 1)]',sep='')))
					## Another way
					#Dataset6 = Dataset6[!sapply(Dataset6, function(x) all(x == ""))]
					eval(parse(text=paste(datasetname, '<<-', datasetname,'[!sapply(',datasetname,',function(x) all(x == ""))]',sep='')))
				}
				# OR use janitor R pkg and remove empty row as well as column in one shot
				# require(janitor)
				# eval(parse(text=paste(datasetname, '<<- remove_empty(',datasetname,'which=c("rows","cols"))',sep='')))
			}
			
			if(TRUE) #This block removes empty rows from the bottom(moving up) and empty cols from the right(moving left)
			{
				# max non-empty row index and max non-empty col index, will be taken as the last cell of our data.frame.
				# So, now we need to remove all rows and columns beyond this cell index
				# We allow user to have empty row or empty col before the last cell of the data.frame
				
				# 'which' starts counting col by col. So first col has 30 values, second col first cell is index 31 and 3rd
				# col will start at index 61, 4th col starts with 91. 5th col starts with 121.
				#
				#Get all non-empty cell indexes
				allidxnotNA = eval(parse(text=paste('which(',datasetname,'!="")')))
				if(length(allidxnotNA) > 0)
				{
					#find max of all the indexes in allidxnotNA
					idx = max(allidxnotNA) 

					r=eval(parse(text=paste('nrow(',datasetname,')',sep='')))
					col=eval(parse(text=paste('ncol(',datasetname,')',sep='')))
					
					lastcellcolidx = ceiling(idx / r)
					
					#finding row index is not very straight forward
					# in allidxnotNA find if there is an index that is 
					# completely divisible by data.frame row count 'r'
					# if so then the last non empty row index is equal 
					# to the row count of the data.frame.
					remainderzeroIndexes = which( allidxnotNA %% r == 0) # list of indexes completely divisible by row-count 'r'
					if( length(remainderzeroIndexes) > 0 )#if any such index found then max row index = row-count 'r'
					{
						lastcellrowidx = r
					}
					else #if last row was empty then the max remainder found below should be the last non empty row
					{
						lastcellrowidx = max(allidxnotNA %% r)
					}

					#get current col names. This will be only used if tehre is just one column
					eval(parse(text=paste('colnames = names(',datasetname,')[1]',sep='')))
					
					#remove empty rows and empty col beyond the 'last cell'.
					eval(parse(text=paste(datasetname, '<<- as.data.frame(', datasetname,'[1:lastcellrowidx, 1:lastcellcolidx])',sep='')))
					
					#if there is just one colum then the name of the col will be var1. We always create dataset from top left cell.
					if(lastcellcolidx == 1)
					{
						eval(parse(text=paste('names(',datasetname,')=colnames',sep='')))
					}


					#Converting character col type to appropriate data type
					#Given a character vector, it attempts to convert it to logical, integer, numeric or complex, and failing 
					#that converts it to factor unless as.is = TRUE. So if we pass as.is=TRUE data will not convert.
					#The first type that can accept all the non-missing values is chosen. 
					#Vectors which are entirely missing values are converted to logical, since NA is primarily logical. 
					#
					#require(utils); auto <- type.convert(mtcars)

					## 11Jul2022 fint type.convert below was converting all columns based on the data that col contains. We dont want that.
					## Sanjay gave me the type.convert with lapply and now we only convert 'character' cols and leave remaing columns as is.
					#eval(parse(text=paste(datasetname, '<<- utils::type.convert(x=', datasetname,',as.is =', !stringAsFactor,')',sep=''))) 
					### abc[] = lapply( abc, function(x) {if(is.character(x)) type.convert(as.character(x), as.is = TRUE) else x} )
					eval(parse(text=paste(datasetname,'[] <<- lapply(',datasetname,', function(x) {if(is.character(x)) utils::type.convert(as.character(x), as.is = TRUE) else x} )',sep='')))

					#08Jun2021 This does not work. some issue with parameter na.strings.
					#R Err Msg : invalid 'na.strings' argument  in function:  type.convert.default(x[[i]], ...)
					#eval(parse(text=paste(datasetname, '<<- utils::type.convert(x=', datasetname,', na.strings = ',NAstrings,',as.is =', !stringAsFactor,')',sep=''))) 
					
					#Based on discussion with Aaron, we decided to convert factors back to characters
					# if(!stringAsFactor)
					# {
							# colcount = eval(parse(text=paste('ncol(',datasetname,')')))
							# for(i in 1:colcount)
							# {
								# coluname = eval(parse(text=paste('colnames(',datasetname,')[',i,']')))
								# colclass = eval(parse(text=paste('class(',datasetname,'$',coluname,')')))
								
								# if(colclass == "factor")
								# {
									# eval(parse(text=paste(datasetname,'$',coluname,'<<- as.character(',datasetname,'$',coluname,')',sep='' )))
								# }
							# }
					# }

						colcount = eval(parse(text=paste('ncol(',datasetname,')')))
						for(i in 1:colcount)
						{
							coluname = eval(parse(text=paste('colnames(',datasetname,')[',i,']')))
							
							## if col class is factor it may have a blank level because of the blank cell in between (in the grid). This blank level must be dropped.
							colclass = eval(parse(text=paste('class(',datasetname,'$',coluname,')')))
							#cat("col class:")
							#print(colclass)
							if("factor" %in% colclass)
							{
								eval(parse(text=paste(datasetname,'$',coluname,' <<- factor(x=',datasetname,'$',coluname,',  exclude = excludechars)', sep='')))
								#eval(parse(text=paste('print(levels(',datasetname,'$',coluname,'))',sep='')))
							}				
							
							###creating missing value attribute, which is dataset level att.
							colmisatt <- eval(parse(text=paste(coluname,'<-list(',coluname,'=list(type="none", value=""))')))
							# print(colmisatt)
							if(i>1)
								eval(parse(text=paste('attr(',datasetname,',"misvals") <<- c(attr(',datasetname,',"misvals"), ',colmisatt,')')))
							else
								eval(parse(text=paste('attr(',datasetname,',"misvals") <<- c(',colmisatt,')')))
							# cat("done!@")
						}

						#cat("\nCreating Extra attributes for new DS. ")
						eval(parse(text=paste('attr(',datasetname,',"maxfactor") <<-', bskymaxfactors)))
						UAcreateExtraAttributes(datasetname, "RDATA")
				}
			}
		},
		
		warning = UAwarnHandlerFn

		) # end of withCallingHandlers for catching warnings and continuing execution		
		},
		error = UAerrHandlerFn,
		
		silent =TRUE		
	)

	    	if(BSkyLocalErrorFound() == TRUE)
    	{
		
			# cat("Error caught in UAloadDataset \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level load dataset function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
			# cat("Warning caught in UAloadDataset \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		# cat("\nWrapup now")
		BSkyFunctionWrapUp()

		return(invisible(BSkyReturnStructure()))
}


# return TRUE if Dataset is empty
BSkyIsEmptyDataset <- function(datasetName)
{
	datasetname <- datasetName
	isEmpty=TRUE
		#find how many rows and cols will remain after removing empty ones.
	#OLD# finalrows = eval(parse(text=paste('nrow(',datasetname,'[!apply(is.na(',datasetname,') | ',datasetname,' == "", 1, all),])', sep='')))
	
	#!is.na is added to find a col that has more than one unique value that is not NA. 
	#If col has all NAs means there is no value in that col and the col is empty so this column should not be counted in ncol
	#finalcols = ncol(Dataset1[!sapply(Dataset1, function(x) all(x == "" || is.na(x)))])
	#OLD# finalcols = eval(parse(text=paste('ncol(', datasetname,'[!apply(',datasetname,', 2, function(var) length(unique(var)) == 1)])',sep='')))#original statement
	
	rowcount = eval(parse(text=paste('nrow(',datasetname,')',sep='')))
	colcount = eval(parse(text=paste('ncol(',datasetname,')',sep='')))
	
	if(rowcount > 0 && colcount > 0)
	{
		for(ridx in 1: rowcount)
		{
			for(cidx in 1: colcount)
			{
				isnonEmpty = eval(parse(text=paste("!is.na(",datasetname,"[ridx, cidx ]) &&",datasetname,"[ridx, cidx ] !=''",sep='')))
				if (isnonEmpty) 
				{
					isEmpty=FALSE
					break;
				}
			}
		}
	}


	#if(finalrows > 0 && finalcols > 0)
	#{
	#	isEmpty=FALSE
	#}
	return(invisible(isEmpty))
}


#after running analysis on an empty dataset that was Processed(empty rows/cols dropped) we need to put
#back empty cells. Note that analysis may add (or remove) cols, so add as much to make final rows=30 and cols=6
#If for any reason we already have rows=30 and cols=6 after analysis then we should not be adding 
#any more empty rows(or discuss with team)
BSkyPutEmptyCellsBack <-function (datasetName, defaultRows = 80, defaultCols = 16)
{

    datasetname <- datasetName
    rowcount = eval(parse(text = paste("nrow(", datasetname,
        ")", sep = "")))
    colcount = eval(parse(text = paste("ncol(", datasetname,
        ")", sep = "")))
    addRows = defaultRows - rowcount
    addCols = defaultCols - colcount

	##11Jul2022 this mutate code was converting all cols back to character
    # require(dplyr)
    # eval(parse(text = paste(datasetname, " <<- ", datasetname,
    #     " %>%  dplyr::mutate(across(everything(), as.character))",
    #     sep = "")))
    if (addCols > 0) {
        for (i in 1:addCols) {
            newcol <- rep("", rowcount)
            eval(parse(text = paste(datasetname, "[ , ncol(",
                datasetname, ") + 1] <<- newcol", sep = "")))
            eval(parse(text = paste("colnames(", datasetname,
                ")[ncol(", datasetname, ")] <<- paste0(\"X\", ",
                (colcount + i), ")", sep = "")))
        }
    }
    colcount = eval(parse(text = paste("ncol(", datasetname,
        ")", sep = "")))
    if (addRows > 0) {
        for (i in 1:addRows) {
			##11Jul2022 this code is not good enough if dataset cols are non-character class.
            # newrow <- rep("", colcount)
            # eval(parse(text = paste(datasetname, "[nrow(", datasetname,
            #     ")+1, ] <<- newrow", sep = "")))
			##11Jul2022 following call will generate correct class for each col.
			#require(tibble)
			eval(parse(text = paste(datasetname,' <<- ',datasetname,' %>% tibble::add_row()', sep='')))
        }
    }
	# cat("Exiting BSkyPutEmptyCellsBack");
	return(invisible())
}