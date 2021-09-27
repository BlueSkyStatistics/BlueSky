######## list of methods in this file; in sequence (few may not be ready and unused ) ###########
# BSkySortDataframe
# BSkyComputeExpression
# BSkyRecode
# ColNametoTitleOrObject
# BSky.Activate.Dataset
#################################################################################################

## Sort Dataframe in R , on specified columns ##
BSkySortDataframe <- function(colNames, sortorder="ASC", dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkySortDataframe: Error in Sorting : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNames, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkySortDataframe: Warning in Sorting : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNames, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{

		withCallingHandlers(
		{
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
		
			if(!is.null(datasetname))
			{		
				##for each col in colNames set col_level split prop, use UAsetColLevelSplit
				#sortcolnames <- paste("uadatasets$lst[[DataSetIndex]]$", colNames,  sep = "", collapse=',')
				# for(i in 1:length(colNames))
				# {
					# if(i==1 || i==length(colNames))
					# {
						# sortcolnames = sortcolnames + "uadatasets$lst[[DataSetIndex]]$"+colNames[i]
					# }
					# else
					# {
						# sortcolnames = sortcolnames + "uadatasets$lst[[DataSetIndex]]$"+colNames[i]+","
					# }
					# cat("Order:",sortcolnames)
				# }
				#cat("Order:",sortcolnames)
				### Standard R side Sort Function 'order' is used to sort dataframe ##
				#uadatasets$lst[[DataSetIndex]] <- uadatasets$lst[[DataSetIndex]][order(sortcolnames, decreasing=descending),]
				# uadatasets$lst[[DataSetIndex]] <- eval(parse(text=paste('uadatasets$lst[[DataSetIndex]][order(',paste('uadatasets$lst[[DataSetIndex]]$', colNames,  sep = '', collapse=','), ', decreasing=descending),]', sep="")))
				
				#descending = !ascending
				#if(!(is.logical(descending)))
					#descending=FALSE
				if(sortorder == "ASC") #sort ascending
					descending = FALSE
				else
					descending = TRUE
				sortcolnames <- paste(datasetname,'$',colNames,  sep = '', collapse=',')
				eval(parse(text=paste(datasetname,' <- ',datasetname,'[order(',sortcolnames, ', decreasing=',descending,'),]', sep="")))
				#chaned <<- to <-
			}
			else
			{  # paste(datasetname,' <- ',datasetname,'[order(',sortcolnames, ', decreasing=',descending,'),]', sep="")
				# cat("\nError: Cannot sort Dataframe. Dataset name or index not found\n")
				BSkyErrMsg =paste("BSkySortDataframe: Cannot sort Dataframe. Dataset name or index not found."," Dataset Name:", datasetname)
				warning("BSkySortDataframe: Cannot sort Dataframe. Dataset name or index not found.")
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
    		# if anything needs to be checked or print etc
			# Error is already handled and logged
			# Whereever the error occured execution cannot continue
			# control will be passed to the error handler function
			# and then the control will come out ogf the Try-Catch block 
			
			# cat("Error caught in BSkySortDataframe \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level sort function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkySortDataframe \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(BSkyReturnStructure()))
}

BSkyComputeExpression <- function(targetvar, numexpression, condition, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyComputeExpression: Error in Compute : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(targetvar, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyComputeExpression: Warning in Compute : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(targetvar, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{

		withCallingHandlers(
		{
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{		
				targetcolindex <- UAgetIndexOfColInDataSet(datasetname,targetvar)
				totalcols <- eval(parse(text=paste('ncol(',datasetname,')')))
				#cat("\nCol index:",targetcolindex)
				if(targetcolindex < 0)
				{
					targetcolindex <- totalcols + 1
				}			
				#cat("\nNew Col index:",targetcolindex)
				#cat("\nOriginal Expr :", numexpression)
				#modify expression by replacing col names (tg0) with full-colnames(uadatasets$lst[[1]]$tg0
				numexpression <- ColNametoTitleOrObject( numexpression , datasetname)
				
				#cat("\nNew Expr :", numexpression,"\n")
				
				#uadatasets$lst[[1]][6] <- eval(parse(text=paste(" uadatasets$lst[[1]][4] + 3")))
				eval(parse(text=paste(datasetname,'[',targetcolindex,'] <- ', numexpression))) #changed <<- to <-
				
				#name new col. If old then also renaming will not effect.
				eval(parse(text=paste('colnames(',datasetname,')[',targetcolindex,'] <- targetvar'))) #changed <<- to <-
			}
			else
			{
				# cat("\nError: Cannot compute. Dataset name or index not found\n")
				BSkyErrMsg =paste("BSkyComputeExpression: Cannot compute. Dataset name or index not found."," Dataset Name:", datasetname)
				warning("BSkyComputeExpression: Cannot compute. Dataset name or index not found.")
			}

		},
		
		warning = UAwarnHandlerFn

		) # end of withCallingHandlers for catching warnings and continuing execution		

		},
		error = UAerrHandlerFn,
		
		silent =TRUE		
	)
	
		#cat("\nDone Computing!")
		if(BSkyLocalErrorFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# Error is already handled and logged
			# Whereever the error occured execution cannot continue
			# control will be passed to the error handler function
			# and then the control will come out ogf the Try-Catch block 
			
			# cat("Error caught in BSkyComputeExpression \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level BSkyComputeExpression function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkyComputeExpression \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level BSkyComputeExpression function\n")
		return(invisible(BSkyReturnStructure()))
}


##### Recode specified columns ##
### 'colNames' is existing (old) colnames
### 'newColNames' is new colnames corressponding to old ones those will hold the new values after recode. Old col will retain their old values in this case
### 'OldNewVals' is set of which old value should be replaced with what new value.
### 'NewCol' FALSE means new columns are not generated instead old col will have new values based on 'OldNewVals'. if TRUE then 'newColNames' must have new names
##		 - corressponding to each column name given in 'colNames'
###	dataSetNamesOrIndex is Dataset name ( or index can also be provided)
##### OldNewVals must have value like this:
### '\"Male\"=\"Man\" ; \"Female\"=3'
### two sets separated by semi colon. Strings enclosed in double quotes, hence backslash used with double quotes. Numeric without quotes.
BSkyRecode.old <- function(colNames, newColNames='', OldNewVals, NewCol=FALSE, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyRecode: Error in Compute : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNames, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyRecode: Warning in Compute : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNames, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{

		withCallingHandlers(
		{
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
		
			if(!is.null(datasetname))
			{		
				if(!NewCol) ##If there are no new cols, overwrite values in old col
				{
					newColNames <- colNames  ## old col will also beahave as new cols
				}
				
				##Length must match. if not, that means, lesser number of new or old col names are provided
				if(length(colNames) == length(newColNames))
				{
					totalcols <- length(colNames)
					#cat("\nTotal Cols to recode :",totalcols)
					## replacing  comma(,) with semi-colon (;)
					OldNewVals <- gsub(",",";",OldNewVals,fixed=TRUE)
					for( i in 1:totalcols)
					{
						# eval(parse(text=paste('uadatasets$lst[[DataSetIndex]]$',newColNames[i], '<- recode(uadatasets$lst[[DataSetIndex]]$',colNames[i],', \'',OldNewVals,'\', as.factor.result=TRUE)')))
						#eval(parse(text=paste(datasetname,'$',newColNames[i], '<- recode(',datasetname,'$',colNames[i],', \'',OldNewVals,'\', as.factor.result=TRUE)')))
						##16Feb2017  Either use car::recode or Recode. Because Himsc pkg also has recode() function.
						eval(parse(text=paste(datasetname,'$',newColNames[i], '<- car::recode(',datasetname,'$',colNames[i],', \'',OldNewVals,'\', )')))						
						#changed <<- to <- in above line for .GlobalEnv locked error
					}
					
					#cat("\nDone Recode :\n")
				}
				else
				{
					# cat("\nError: Cannot Recode. Number of old col name(s) does not match with number of new col name(s)\n")
					BSkyErrMsg =paste("BSkyRecode: Cannot Recode. Number of old col name(s) does not match with number of new col name(s).")
					warning("BSkyRecode: Cannot Recode. Number of old col name(s) does not match with number of new col name(s).")
				}
			}
			else
			{
				# cat("\nError: Cannot Recode. Dataset name or index not found\n")
				BSkyErrMsg =paste("BSkyRecode:  Cannot Recode. Dataset name or index not found."," Dataset Name:", datasetname)
				warning("BSkyRecode:  Cannot Recode. Dataset name or index not found.")
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
    		# if anything needs to be checked or print etc
			# Error is already handled and logged
			# Whereever the error occured execution cannot continue
			# control will be passed to the error handler function
			# and then the control will come out ogf the Try-Catch block 
			
			# cat("Error caught in BSkyRecode \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level BSkyRecode function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkyRecode \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level Reode function\n")
		invisible(BSkyReturnStructure())
}


# Aaron fixed 28Jun2018
BSkyRecode <-function (colNames, newColNames = "", OldNewVals, NewCol = FALSE, prefixOrSuffix='',prefixOrSuffixString='',makeNumeric=FALSE,dontMakeFactor=FALSE,dataSetNameOrIndex) 
{
    BSkyFunctionInit()
    BSkySetCurrentDatasetName(dataSetNameOrIndex)
    BSkyErrMsg = paste("BSkyRecode: Error in Compute : ", "DataSetName :", 
        dataSetNameOrIndex, " ", "Variable Name List :", paste(colNames, 
            collapse = ","), sep = "")
    BSkyWarnMsg = paste("BSkyRecode: Warning in Compute : ", 
        "DataSetName :", dataSetNameOrIndex, " ", "Variable Name List :", 
        paste(colNames, collapse = ","), sep = "")
    BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
    tryCatch({
        withCallingHandlers({
            datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
            if (!is.null(datasetname)) {
                if ( prefixOrSuffix ==""  ) 
                {
                 if (!NewCol)
                  {
                  newColNames <- colNames
                  }
                if (length(colNames) == length(newColNames)) {
                  totalcols <- length(colNames)
                  #OldNewVals <- gsub(",", ";", OldNewVals, fixed = TRUE)
                  for (i in 1:totalcols) 
				  {
					if (makeNumeric)
					{
					eval(parse(text = paste(datasetname, "$", 
                      newColNames[i], "<- car::recode(", datasetname, 
                      "$", colNames[i], ", '", OldNewVals, "',as.numeric=TRUE ), sep="")))
					}
					else if (dontMakeFactor) {
					eval(parse(text = paste(datasetname, "$", 
                      newColNames[i], "<- car::recode(", datasetname, 
                      "$", colNames[i], ", '", OldNewVals, "',as.factor=FALSE ), sep="")))
					
					
					}
					else
					{
                    eval(parse(text = paste(datasetname,
                          "$", newColNames[i], "<- car::recode(",
                          datasetname, "$", colNames[i], ", '",
                          OldNewVals, "')", sep="")))
					}
                  }
                }
            else {
                  BSkyErrMsg = paste("BSkyRecode: Cannot Recode. Number of old col name(s) does not match with number of new col name(s).")
                  warning("BSkyRecode: Cannot Recode. Number of old col name(s) does not match with number of new col name(s).")
                }
                } #Added by Aaron
                
            if (prefixOrSuffix =="suffix")
            {
                totalcols <- length(colNames)
                # OldNewVals <- gsub(",", ";", OldNewVals, fixed = TRUE)
                for (i in 1:totalcols) {
				
				
					if (makeNumeric)
					{
					eval(parse(text = paste(datasetname, "$", 
                      colNames[i],prefixOrSuffixString, "<- car::recode(", datasetname, 
                      "$", colNames[i], ", '", OldNewVals, "',as.numeric= TRUE )",sep="")))
					
					}
					
					else if (dontMakeFactor)
					{
					eval(parse(text = paste(datasetname, "$", 
                      colNames[i],prefixOrSuffixString, "<- car::recode(", datasetname, 
                      "$", colNames[i], ", '", OldNewVals, "',as.factor =FALSE )",sep="")))
					
					}
					else 
					{
					
                    eval(parse(text = paste(datasetname, "$", 
                      colNames[i],prefixOrSuffixString, "<- car::recode(", datasetname, 
                      "$", colNames[i], ", '", OldNewVals, "' )",sep="")))
					}
				  
				  
				  
				  
				  }

            }
            
              if (prefixOrSuffix =="prefix")
            {
                totalcols <- length(colNames)
                # OldNewVals <- gsub(",", ";", OldNewVals, fixed = TRUE)
                for (i in 1:totalcols) {
				
					if (dontMakeFactor)
					{
					 eval(parse(text = paste(datasetname, "$", prefixOrSuffixString, 
                      colNames[i], "<- car::recode(", datasetname, 
                      "$", colNames[i], ", '", OldNewVals, "', as.factor=FALSE)" ,sep=""     )))
					
					}
					else if (makeNumeric)
					{
					 eval(parse(text = paste(datasetname, "$", prefixOrSuffixString, 
                      colNames[i], "<- car::recode(", datasetname, 
                      "$", colNames[i], ", '", OldNewVals, "', as.numeric=TRUE)" ,sep=""     )))
					
					}
					else {
					
					
					
                    eval(parse(text = paste(datasetname, "$", prefixOrSuffixString, 
                      colNames[i], "<- car::recode(", datasetname, 
                      "$", colNames[i], ", '", OldNewVals, "', )" ,sep=""     )))
					  }
                  }

            }
  
            }
            else {
                BSkyErrMsg = paste("BSkyRecode:  Cannot Recode. Dataset name or index not found.", 
                  " Dataset Name:", datasetname)
                warning("BSkyRecode:  Cannot Recode. Dataset name or index not found.")
            }
        }, warning = UAwarnHandlerFn)
    }, error = UAerrHandlerFn, silent = TRUE)
    if (BSkyLocalErrorFound() == TRUE) {
    }
    if (BSkyLocalWarningFound() == TRUE) {
        BSkyLocalWarningFlagsReset()
    }
    BSkyFunctionWrapUp()
    invisible(BSkyReturnStructure())
}




### Converts Col names to object if they are not enclosed within double or single quotes ###
## eg.. 
## aa<-ColNametoTitleOrObject(expressionstring="this c(tg0) and c('tg0') withtg0 \"tg00\" but tg00 more+tg1+tg2*wgt1" , dataSetNameOrIndex='Dataset1')
ColNametoTitleOrObject <- function( expressionstring , dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("ColNametoTitleOrObject: Error converting col name to title/object : ", "DataSetName :", dataSetNameOrIndex," ", "Expression  :", paste(expressionstring, collapse = ","),sep="")
	BSkyWarnMsg = paste("ColNametoTitleOrObject: Warning converting col name to title/object : ", "DataSetName :", dataSetNameOrIndex," ", "Expression :", paste(expressionstring, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{		
				totalcols <- eval(parse(text=paste('ncol(',datasetname,')')))

				# cat("\nOriginal Expr :", expressionstring)

				allcolnames <- eval(parse(text=paste('colnames(',datasetname,')')))
				## Adding spaces in begining and end of the expression ##
				expressionstring <- paste(' ',expressionstring,' ',sep="")
				
				## Adding spaces around special charaters
				#expressionstring<-'pop+10*3+popi'
				operators <- c('\\+', '-', '\\*', '/', '%%', '\\^','->','<-', '>=','<=','==','!=','>','<','!','&','\\|','~','\\$',':','\\(','\\)','\\{','\\}')
				totalopr <- length(operators)
				for(j in 1:totalopr)
				{
					pat <- paste(operators[j], sep="") # colname+nonAlphanumeric "($|[^a-zA-Z])"    "($|[^*+/-])"
					replacewith <- paste(" ",operators[j]," ",sep="")
					#cat("\n",pat,"\t\t",replacewith)
					expressionstring <- gsub(pattern=pat, replacement=replacewith, x=expressionstring, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
					#cat("\n",expressionstring)
				}
				
								#modify expression by replacing col names (tg0) with full-colnames(uadatasets$lst[[1]]$tg0
				### Add logic for "var" to "var" (both ' and "). Check syntax guide for vaid var name
				for( i in 1:totalcols)
				{
					#[^a-zA-Z0-9]*popi[^a-zA-Z0-9]*
					#"[^'\"a-zA-Z0-9_]",allcolnames[i],"[^a-zA-Z0-9_'\"]"
					pat <- paste("[^'\"a-zA-Z0-9_]",allcolnames[i],"[^a-zA-Z0-9_'\"]","", sep="") # colname+nonAlphanumeric "($|[^a-zA-Z])"    "($|[^*+/-])"
					replacewith <- paste(datasetname,'$',allcolnames[i],sep="")
					#cat("\nPatt :", pat)
					#cat("\nRep :", replacewith)
					
					
					# #grepl("('[\t\n\r\f\v ]*tg0[\t\n\r\f\v ]*')|(\"[\t\n\r\f\v ]*tg0[\t\n\r\f\v ]*\")", c("this has      \"tg0 \"   and", "second   ' tg0 '  ok"))
					# ## if colname is enclosed in single or double quotes then dont convert it to object, instead let it be string ##
					# colpat <- paste("('[\t\n\r\f\v ]*",allcolnames[i],"[\t\n\r\f\v ]*')|(\"[\t\n\r\f\v ]*",allcolnames[i],"[\t\n\r\f\v ]*\")","", sep="")
					# searchresult <- grepl(colpat, expressionstring)
					# searchlen <- length(searchresult)
					# foundvar <- false  # foundvar will be true if
					# for(j in 1:searchlen)
					# {
						# if(searchresult[j])
						# {
							
						# }
					# }
					
					expressionstring <- gsub(pattern=pat, replacement=replacewith, x=expressionstring, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
				}
				
				#cat("\nNew Expr :", expressionstring,"\n")
				#-return(expressionstring)

			}
			else
			{
				# cat("\nError: Col name to Object or Title Conversion\n")
				BSkyErrMsg =paste("ColNametoTitleOrObject:  Col name to Object or Title Conversion.")
				warning("ColNametoTitleOrObject:  Col name to Object or Title Conversion.")
			}
	
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(expressionstring)) #
}


## if Dataset is open, makes it active and return true else returns false##
BSky.Activate.Dataset <- function(DatasetAlias)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(DatasetAlias)
	
	BSkyErrMsg = paste("BSky.Activate.Dataset: Error activating dataset : ", "DataSetName :", DatasetAlias,sep="")
	BSkyWarnMsg = paste("BSky.Activate.Dataset: Warning activating dataset : ", "DataSetName :", DatasetAlias,sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	activatesuccess <- FALSE
	tryCatch(
		{

		withCallingHandlers(
		{
			dataSetNameOrIndex = DatasetAlias
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{		
				##use attach to activate dataset ###
				eval(parse(text=paste('attach(',datasetname,')')))
				activatesuccess <- TRUE #-return(TRUE);
			}
			else
			{
				# cat("\nError: Cannot Activate Dataset. Dataset not found\n")
				BSkyErrMsg =paste("Cannot Activate Dataset. Dataset not found.")
				warning("Cannot Activate Dataset. Dataset not found.")
				### We can write some logic here to load dataset###
				### or
				### we can return false which would mean that dataset is not open
				activatesuccess <- FALSE #-return(FALSE);
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
    		# if anything needs to be checked or print etc
			# Error is already handled and logged
			# Whereever the error occured execution cannot continue
			# control will be passed to the error handler function
			# and then the control will come out ogf the Try-Catch block 
			
			# cat("Error caught in BSky.Activate.Dataset \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level sort function\n")
	if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSky.Activate.Dataset \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function UAsetColMeasure
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(BSkyReturnStructure(list(extra=c(activatesuccess)))))  #
}