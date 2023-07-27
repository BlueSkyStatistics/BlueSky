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
# BSkyBlankDSColNameClassList <<- list()
# blankDScolumnnames <<- NULL
# blankDSrowcount <<- NULL
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
	# datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	datasetname <- paste('.GlobalEnv$',datasetName, sep='')
	lastcellcolidx = 0
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
				BSkyBlankDSColNameClassList <<- list()
				BSkyBlankDSColNameLevelsList <<- list()
				blankDScolumnnames <<-NULL
				blankDSrowcount <<- eval(parse(text=paste('nrow(',datasetname,')',sep='')))
				blankDScolremoved <<- 0 #no. of empty cols deleted from the say Dataset1
				#28Jun2023 backup: save all col-names and col-class in a temp BSkyColNameClassList variable
				blankDScolumnnames <<- eval(parse(text=paste('colnames(',datasetname,')',sep='')))
				for(colname in blankDScolumnnames) # save column name with the class
				{
					columnclass = eval(parse(text=paste('class(',datasetname,'$',colname,')',sep='')))
					eval(parse(text=paste('BSkyBlankDSColNameClassList$',colname,' <<- columnclass' ,sep='')))
					if("factor" %in% columnclass)
					{
						columnlevels = eval(parse(text=paste('levels(',datasetname,'$',colname,')',sep='')))
						eval(parse(text=paste('BSkyBlankDSColNameLevelsList$',colname,' <<- columnlevels' ,sep='')))
					}
				}

				# print(BSkyBlankDSColNameClassList)
				# print(blankDScolumnnames)
				# print(blankDSrowcount)
				
				#19Jul23 Ross want us to keep the empty cols on the right of the boundary cell if it is renamed (X5 -> gender, but is empty col)
				# And say in this scenario the 3rd col is renamed to X3 -> weight and has values.
				# hint: create lastcellcolidx based on the name changed from original (X5 in above example). lastcellrowidx will remain as it is now.
				keepEmptyRtCols = TRUE #this flag enables disables this capability.
				if(keepEmptyRtCols)
				{
					defaultColNames = c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9', 'X10', 'X11', 'X12', 'X13', 'X14', 'X15')
					for(i in 15:1)
					{
						if(!(blankDScolumnnames[i] %in% defaultColNames) )
						{
							lastcellcolidx = i
							break
						}
						
					}
				}
				

				# max non-empty row index and max non-empty col index, will be taken as the last cell of our data.frame.
				# So, now we need to remove all rows and columns beyond this cell index
				# We allow user to have empty row or empty col before the last cell of the data.frame
				
				# 'which' starts counting col by col. So first col has 30 values, second col first cell is index 31 and 3rd
				# col will start at index 61, 4th col starts with 91. 5th col starts with 121.
				#
				#Get all non-empty cell indexes. Since following does no work well with Date/POSIX col. we will make a copy
				# of a dataset and make all cols character so that we can find the filled cell in a copy dataset.
				#29Jun2023  allidxnotNA = eval(parse(text=paste('which(',datasetname,'!="")')))

				# BSkyTeMpDs235 <- as.data.frame(lapply(datasetname, as.character), stringsAsFactors = FALSE)
				eval(parse(text=paste('BSkyTeMpDs235 <- as.data.frame(lapply(',datasetname,', as.character), stringsAsFactors = FALSE)', sep='' )))
				allidxnotNA = eval(parse(text=paste('which(BSkyTeMpDs235 !="")')))


				rm(BSkyTeMpDs235) #clean temp copy of the dataset

				if(length(allidxnotNA) > 0 || lastcellcolidx > 0)#this 'if' executes either there is data in the grid or column name is changed.
				{
					r=eval(parse(text=paste('nrow(',datasetname,')',sep='')))
					maxCols=eval(parse(text=paste('ncol(',datasetname,')',sep='')))# col count when the dataset was not cleaned (not processed)
					lastcellrowidx = r
					if(length(allidxnotNA) > 0)## this if block will execute only if there is any data in the grid
					{
						#find max of all the indexes in allidxnotNA
						idx = max(allidxnotNA) 
						lastcellcolidx2 = ceiling(idx / r) ## this gives us the last col in which user atleast filled one cell
						if(lastcellcolidx2 > lastcellcolidx) ##pick the higher column index
						{
							lastcellcolidx = lastcellcolidx2
						}

						##27Jun2023 we also need to find if beyond last cell user renamed any col from 'Xn' to something else(e.g. X4 to 'age').
						
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
					}
					
					#get current col names. This will be only used if there is just one column
					Colnames = eval(parse(text=paste('colnames(',datasetname,')[1]',sep=''))) # saving one col name only
					#cat(Colnames)
					#remove empty rows and empty col beyond the 'last cell'.
					eval(parse(text=paste(datasetname, '<- as.data.frame(', datasetname,'[1:',lastcellrowidx,', 1:',lastcellcolidx,'])',sep='')))

					#if there is just one colum then the name of the col will be var1. We always create dataset from top left cell.
					if(lastcellcolidx == 1)
					{
						# print(eval(parse(text=paste('colnames(',datasetname,')'))))
						eval(parse(text=paste('colnames(',datasetname,') <- Colnames',sep=''))) # restoring one col name only
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
					eval(parse(text=paste(datasetname,'[] <- lapply(',datasetname,', function(x) {if(is.character(x)) utils::type.convert(as.character(x), as.is = TRUE) else x} )',sep='')))

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
						colmlevels = c()
						colcount = eval(parse(text=paste('ncol(',datasetname,')')))
						for(i in 1:colcount)
						{
							colmlevels = c()
							coluname = eval(parse(text=paste('colnames(',datasetname,')[',i,']')))

							## if col class is factor it may have a blank level because of the blank cell in between (in the grid). This blank level must be dropped.
							colclass = eval(parse(text=paste('class(',datasetname,'$',coluname,')')))
							#cat("col class:")
							#print(colclass)
							if("factor" %in% colclass)
							{
								colmlevels = eval(parse(text=paste('BSkyBlankDSColNameLevelsList$',coluname, sep='')))
							}
							
							
							if("ordered" %in% colclass)
							{
								eval(parse(text=paste(datasetname,'$',coluname,' <- ordered(x=',datasetname,'$',coluname,',  exclude = excludechars)', sep='')))
								#eval(parse(text=paste('print(levels(',datasetname,'$',coluname,'))',sep='')))
								
								#19Jul23 restore factor levels too
								eval(parse(text=paste('levels(',datasetname,'$',coluname,') <- colmlevels',sep='')))
							}	
							else if("factor" %in% colclass)
							{
								eval(parse(text=paste(datasetname,'$',coluname,' <- factor(x=',datasetname,'$',coluname,',  exclude = excludechars)', sep='')))
								#eval(parse(text=paste('print(levels(',datasetname,'$',coluname,'))',sep='')))
								
								#19Jul23 restore factor levels too
								eval(parse(text=paste('levels(',datasetname,'$',coluname,') <- colmlevels',sep='')))
							}							
							
							#19Jul23 utils::type.convert() above seems to convert the empty 'character' cols to 'logical'. I think we should convert it back to character
							# But we will only convert the logical col class to character if they were character before utils::type.convert()
							fixChar2Logical=TRUE #this flag enables disables this capability.
							if(fixChar2Logical)
							{
								colmclass = eval(parse(text=paste('BSkyBlankDSColNameClassList$',coluname, sep=''))) 
								if("logical" %in% colclass && 'character' %in% colmclass)
								{
									#eval(parse(text = paste('class(',datasetname,'$',coluname,') <- BSkyBlankDSColNameClassList[',i,']', sep='')))
									eval(parse(text = paste('class(',datasetname,'$',coluname,') <- colmclass', sep='')))
								}
							}
							
							###creating missing value attribute, which is dataset level att.
							colmisatt <- eval(parse(text=paste(coluname,'<-list(',coluname,'=list(type="none", value=""))')))
							# print(colmisatt)
							if(i>1)
								eval(parse(text=paste('attr(',datasetname,',"misvals") <- c(attr(',datasetname,',"misvals"), ',colmisatt,')')))
							else
								eval(parse(text=paste('attr(',datasetname,',"misvals") <- c(',colmisatt,')')))
							# cat("done!@")
						}

						#cat("\nCreating Extra attributes for new DS. ")
						eval(parse(text=paste('attr(',datasetname,',"maxfactor") <-', bskymaxfactors)))
						eval(parse(text=paste('attr(',datasetname,',"processDS") <- TRUE')))
						UAcreateExtraAttributes(datasetname, "RDATA")
						blankDScolremoved <<- maxCols - colcount ##maxcols=15 and colcount is col in subDS after cleanup
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

### 28Jun-05Jul2023 updated: say sub-dataset has 2 cols filled. We add all the previously removed rows to these 2 cols. Then we add previously removed cols with 
### vlaues as NAs or ""(for char only) and proper class and levels if "factor" col.
### This will help us to not introduce unwanted NAs. 
### In old code, we were adding removed cols first and then adding the rows to all cols, which was filling the whole dataset blank cells with NAs and then
### next time cleanup was not working while saving(or running analysis) and the whole dataset starts taking part in SAVE or analysis even though grid looks
### empty except 2 cols that were filled.
BSkyPutEmptyCellsBack <-function (datasetName, defaultRows = 80, defaultCols = 15)
{
	# print("Showing backed up values:")
	# print(BSkyBlankDSColNameClassList)
	# print(blankDScolumnnames)
	# print(blankDSrowcount)
	# print("End-of Showing backed up values:")
	datasetname <- paste('.GlobalEnv$',datasetName, sep='')
    # datasetname <- datasetName
    rowcount = eval(parse(text = paste("nrow(", datasetname, ")", sep = "")))
    colcount = eval(parse(text = paste("ncol(", datasetname, ")", sep = "")))
	subDScolumnnames <- eval(parse(text=paste('colnames(',datasetname,')',sep='')))
	#we should not use the following beacause maybe user deleted 20 empty rows before analysis still have some. We should not stick to 80 rows
    #28Jun2023 addRows = defaultRows - rowcount
    #28Jun2023 addCols = defaultCols - colcount

	addRows = blankDSrowcount - rowcount
	addCols = blankDScolremoved #add as many cols as many you deleted #length(blankDScolumnnames) - colcount
	# allBSkyVarNames = c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9', 'X10', 'X11', 'X12', 'X13', 'X14', 'X15')
	subDScolnames = eval(parse(text=paste('colnames(',datasetname,')',sep=''))) ##col names of the sub-dataset (in which empty cells were removed)
	##11Jul2022 this mutate code was converting all cols back to character
    # require(dplyr)
    # eval(parse(text = paste(datasetname, " <<- ", datasetname,
    #     " %>%  dplyr::mutate(across(everything(), as.character))",
    #     sep = "")))

	#Either add rows first or col first modify code accordingly. Better to add cols last because you can restore 'class' properly
	#otherwise weired things happens and class is not what it was before cleanup (before process-blank-dataset)

    if (addRows > 0) { ## Add rows first

		##create emptyRowdata that will contain "" only for "character" and NA for other classes
		emptyRowdata <- rep("", colcount)
		for(colidx in 1: colcount)
		{
			# colmclass = eval(parse(text=paste('BSkyBlankDSColNameClassList[[',colidx,']]', sep='')))# colclass should not come from old original (full) dataset
			colmclass = eval(parse(text = paste('class(',datasetname,'[[',colidx,']])', sep='')))# col-class should come from sub-DS because new row is being added to sub-DS
			if( !("character" %in% colmclass))
			{
				emptyRowdata[colidx] = NA
			}
		}
		# print("insert:")
		# print(emptyRowdata)

        for (i in rowcount+1:addRows) {
			# emptyRowdata <- rep("", rowcount)
			#29Jun2023  eval(parse(text = paste(datasetname,' <- ',datasetname,' %>% tibble::add_row()', sep=''))) #this inserts NAs in blank cells so we need to remove them below
			eval(parse(text = paste(datasetname,'[',i,',] <- emptyRowdata', sep='')))
        }
    }

	emptySupported = c("character") #, "numeric", "integer", "Date", "POSIXct", "POSIXt")
	colmlevels=""
    if (addCols > 0) {
		rowcount = blankDSrowcount
		for( cname in blankDScolumnnames)
		{
			colmclass = eval(parse(text=paste('BSkyBlankDSColNameClassList$',cname, sep=''))) 
			if("factor" %in% colmclass)
			{
				colmlevels = eval(parse(text=paste('BSkyBlankDSColNameLevelsList$',cname, sep='')))
			}
			
			if(!(cname %in% subDScolnames)) ## append columns that are not present in sub-dataset
			{
				# print(eval(parse(text=paste('BSkyBlankDSColNameClassList$',cname, sep=''))))
				# print(paste0(cname,":", colmclass))##from backup

				if( any(emptySupported %in% colmclass) ) # if any from emptySupported class is in colmclass
				{
					emptyColdata <- rep("", rowcount)
				}
				else 
				{
					emptyColdata <- rep(NA, rowcount)
				}
				# print(emptyColdata)
				eval(parse(text = paste(datasetname, "[ , ncol(", datasetname, ") + 1] <- emptyColdata", sep = ""))) ## append a col

				eval(parse(text = paste("colnames(", datasetname, ")[ncol(", datasetname, ")] <- cname", sep = ""))) ## assign name to the appended col

				# print(eval(parse(text = paste('class(',datasetname,'$',cname,')', sep='')))) #existing class in sub-dataset

				 ## assign the same class that was there before cleanup. Restoring class is imp b'coz user might have empty col renamed and converted to say factor
				 if('ordered' %in% colmclass)
				 {
					eval(parse(text = paste(datasetname,'$',cname,' <- ordered(',datasetname,'$',cname,')', sep='')))
					#we can add levels in line above but for easy readablity we have a separate line below
					eval(parse(text=paste('levels(',datasetname,'$',cname,') <- colmlevels',sep='')))
				 }
				 else if ('factor' %in% colmclass)
				 {
					eval(parse(text = paste(datasetname,'$',cname,' <- factor(',datasetname,'$',cname,')', sep='')))
					#we can add levels in line above but for easy readablity we have a separate line below
					eval(parse(text=paste('levels(',datasetname,'$',cname,') <- colmlevels',sep='')))					
				 }
				 else 
				 {
					eval(parse(text = paste('class(',datasetname,'$',cname,') <- colmclass', sep='')))
				 }

				# print("New class:")
				# print(eval(parse(text = paste('class(',datasetname,'$',cname,')', sep=''))))
				
			}
			else ## existing col in sub-dataset may have a different class during analysis execution we need to put back what it was just before analysis
			{
				# print(paste0(cname,":", colmclass)) ##from backup
				if('ordered' %in% colmclass)
				{
					eval(parse(text = paste(datasetname,'$',cname,' <- ordered(',datasetname,'$',cname,')', sep='')))
					#we can add levels in line above but for easy readablity we have a separate line below
					eval(parse(text=paste('levels(',datasetname,'$',cname,') <- colmlevels',sep='')))					
				}
				else if ('factor' %in% colmclass)
				{
					eval(parse(text = paste(datasetname,'$',cname,' <- factor(',datasetname,'$',cname,')', sep='')))
					#we can add levels in line above but for easy readablity we have a separate line below
					eval(parse(text=paste('levels(',datasetname,'$',cname,') <- colmlevels',sep='')))					
				}
				else 
				{
					eval(parse(text = paste('class(',datasetname,'$',cname,') <- colmclass', sep='')))
				}
				# print("Class:")
				# print(eval(parse(text = paste('class(',datasetname,'$',cname,')', sep=''))))

			}
		}

    }
    # colcount = eval(parse(text = paste("ncol(", datasetname, ")", sep = "")))

	# cat("Exiting BSkyPutEmptyCellsBack");
	return(invisible())
}