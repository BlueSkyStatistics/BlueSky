######## list of methods in this file; in sequence (few may not be ready and unused ) ###########
# UAaddNewRow (no logic inside)
# BSkyEditDatagrid
# BSkyAddNewDatagridRow
# BSkyRemoveDatagridRow
# BSkyAddVarRow
# BSkyDatasetLevelAttributeBackup
# BSkyDatasetLevelAttributeRestore
# BSkySetAttributes  EMPTY
# BSkyRemoveVarRow
# BSkyRemoveMultipleVarRows
# BSkyChangeLevels
# BSkyAddLevels
# BSkyChangeMissings  EMPTY
#################################################################################################

#check and delete UAaddNewRow
UAaddNewRow<-function(dataSetNameOrIndex)
{

	tryCatch(
		{

		withCallingHandlers(
		{
			#cat("\nUAaddNewRow:Finding DataSet \n")		
DataSetIndex <- BSkyValidateDataset(dataSetNameOrIndex)

			if(DataSetIndex > 0)
			{		
				####put your code here

			}
			else
			{
				# cat("\nError: Dataset name or index not found\n")
				BSkyErrMsg =paste("UAaddNewRow: Dataset name or index not found."," Dataset Name:", dataSetNameOrIndex)
				warning("UAaddNewRow: Dataset name or index not found.")
			}
		},
		
		warning = UAwarnHandlerFn

		) # end of withCallingHandlers for catching warnings and continuing execution		

		},
		error = UAerrHandlerFn,
		
		silent =TRUE		
	)	
}



#############################################################################################################
#Function: BSkyEditDatagrid(colname, colceldata, rowdata, rowindex=0, dataSetNameOrIndex, rdateformat)										
#              																								
#Parameters: 
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		rowindex					- Row index, which is changed in data grid.
#									  This will also act as a col index for data grid where the new col is to be inserted
#		rowdata(future use)			- rowdata is a list of values(data). Example
#										rowdata <- list(agecat="Under 21",gender="Male",accid=1234,pop=112233)
#		colname						- name of col whose value is given. Rest col will have NAs. Later use can edit.
#		coldata						- data for the colname for new record(row)
#		colceldata					- cell value/data for the col cell in existing row.
#       rdateformat					- the format in r e.g. %m/%d/%Y		
#																											
#Description:
#		Edit exisiting row in data grid.
#																											
#Example: BSkyEditDatagrid(colname="pop", colceldata="212343", rowindex=0, dataSetNameOrIndex="mydf")		
# class 'numeric' na is NA while class 'character'/'factor' na is <NA> in the UI grid																						
#																											
#############################################################################################################
BSkyEditDatagrid<-function(colname, colceldata=NA, rowdata=NA, rowindex=0, dataSetNameOrIndex, rdateformat='')
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyEditDatagrid: Error in edit grid data : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colname, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyEditDatagrid: Warning in edit grid data : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colname, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{
	
		withCallingHandlers(
		{
	rowlist <- ""
	##cat("\nFinding DataSet \n")		
	datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	
	#set rowindex
	if(rowindex >= 0)
	     rowindex <- rowindex + 1 # UI is 0 based while in R here is 1 based. So, index 0 in UI means 1 in R
	else
		rowindex <- eval(parse(text=paste('nrow(',datasetname,') + 1' ))) #if rowindex -ve then make rowindex greater than total rows
		
	#if dataset index is valid and rowindex is valid. rowindex should be not more that total rows.
	if(!is.null(datasetname))
	{	
		#Find is colname exists
# colIndex<-BSkyValidateColumn(DataSetIndex, colname)
	colIndex <- BSkyValidateColumn(datasetname, colname)	
		#### Processing colceldata  ####	##Put following checks everywhere
		if(colIndex > 0)
		{

					classOfCol =eval(parse(text=paste(  "class(", datasetname, "$",colname, ")")))
					if("numeric" %in% classOfCol)
					{			
						#cat("\nEdit Numeric cell.")
						 #cat(paste(datasetname,"[,",colIndex,"][[",rowindex,"]] <<- as.numeric(",colceldata,")"))
						if(!is.na(suppressWarnings(as.numeric(colceldata))) || is.na(colceldata)){
							eval(parse(text=paste(datasetname,"[",rowindex,",",colIndex,"]  <- as.numeric(",colceldata,")")))#. <<- to <-
						}
						else {
							cat(paste0("\nValue entered ", colceldata," - is invalid type for the dataset column of type ",classOfCol,"\n"))
						}
						# for factor: uadatasets$lst[[1]][,2][[2]]<- "Male" or NA
						# cat("\nTesting2.")
					}
					else if("integer" %in% classOfCol)
					{			
						if(!is.na(suppressWarnings(as.integer(colceldata)))  || is.na(colceldata)){
							eval(parse(text=paste(datasetname,"[",rowindex,",",colIndex,"]  <- as.integer(",colceldata,")")))#. <<- to <-
						}
						else {
							cat(paste0("\nValue entered ", colceldata," - is invalid type for the dataset column of type ",classOfCol,"\n"))
						}						
					}		
					else if("character" %in% classOfCol)
					{			
						eval(parse(text=paste(datasetname,"[",rowindex,",",colIndex,"]  <- as.character(","colceldata",")")))#. <<- to <-
					}	
					else if("POSIXct" %in% classOfCol || "POSIXlt" %in% classOfCol || "Date" %in% classOfCol)
					{			
						dtformat=rdateformat
						#if(classOfCol =="Date")
						#{
						#	dtformat='%Y-%m-%d'
						#}
						if(is.na(dtformat) || dtformat=='')
						{
							dtformat = eval(parse(text=paste('attr(',datasetname,'[[',colIndex,']],"DateFormat")')))
							if(is.null(dtformat) || dtformat=="")
								dtformat='%Y-%m-%d'  # we may want to change this
						}
						coltzone = eval(parse(text=paste('attr(',datasetname,'[[',colIndex,']],"tzone")')))
						if(is.null(coltzone) || coltzone=="")
							coltzone = Sys.timezone()

						if(is.na(colceldata))#this is needed to set NA in the date column if user wants to do so
						{
							eval(parse(text=paste(datasetname,"[",rowindex,",",colIndex,"] <- NA")))
						}
						else
						{
							## Check if date is valid and is in the right format
							validStrDate = BSkyIsDateValid(stringDate=colceldata, dateFormat=dtformat, coltzone=coltzone)
							if(!is.na(validStrDate) && validStrDate != '')
							{
								colceldata = validStrDate
							
							#cat("\nEdit Date cell.")
							 #cat(paste(datasetname,"[,",colIndex,"][[",rowindex,"]] <<- as.numeric(",colceldata,")"))
							 
							 ## use following when UIgrid column is of type DateTime.
							 ## (we may have to match the date format withUIgrid format)
							 #  stdt <- strptime(colceldata,format='%m/%d/%Y %H:%M:%S')
							 ## use following instead of above line
							 ## if the date column in the UI grid is string and has yyyy-MM-dd HH:mm:ss format
							 stdt <- strptime(colceldata,format=dtformat, tz=coltzone)
							 #cat("\nStrp:")
							 #print(stdt)
							 posdt <- as.POSIXct(stdt, tz=coltzone)
							 #cat("\nAs POSIXct:")
							 #print(posdt)
							#eval(parse(text=paste(datasetname,"[,",colIndex,"][[",rowindex,"]] <<- ",posdt,"")))
							eval(parse(text=paste(datasetname,"[",rowindex,",",colIndex,"] <- posdt")))#. <<- to <-
							#cat("\nEdit Executed")
							#eval(parse(text=paste('print(',datasetname,"[",rowindex,",",colIndex,"]")))
							# for factor: uadatasets$lst[[1]][,2][[2]]<- "Male" or NA
							# cat("\nTesting2.")
							}
						}
					}
					else if("logical" %in% classOfCol)
					{
						valid=true
						if(is.na(colceldata) )
						{
							colceldata = NA
						}
						else if( base::toupper(colceldata)=="TRUE" || colceldata==TRUE) #string as well as logical comparison
						{
							colceldata = TRUE
						}
						else if( base::toupper(colceldata) == "FALSE"  || colceldata==FALSE)
						{
							colceldata = FALSE
						}
						else
						{
							valid = false
							colceldata = NA
						}

						if(valid){ #!is.na(suppressWarnings(as.logical(colceldata))) ){
							eval(parse(text=paste(datasetname,"[",rowindex,",",colIndex,"]  <- (",colceldata,")", sep='')))
						}
						else {
							cat(paste0("\nValue entered ", colceldata," - is invalid type for the dataset column of type ",classOfCol,"\n"))
						}
					}				
					else
					{			
						if(is.na(colceldata) ) #if it is NA then you sent NA and not 'NA'
						{
							eval(parse(text=paste(datasetname,"[",rowindex,",",colIndex,"]  <- ",colceldata, sep='')))
						}	
						else
						{						
							#cat("\nEdit Character cell.")
							#cat(paste(datasetname,"[,",colIndex,"][[",rowindex,"]] <<- (",colceldata,")"))
							eval(parse(text=paste(datasetname,"[",rowindex,",",colIndex,"]  <- ('",colceldata,"')", sep='')))#. <<- to <-
							# for factor: uadatasets$lst[[1]][,2][[2]]<- "Male" or NA
							# cat("\nTesting2.")
						}
					}
		}

		#-return("")#result.commandstring
	}#if dataset index is valid and existing row index is valid
	else
	{
		# cat("\nDataset doesnot exists. or invalid rowindex\n")
		BSkyErrMsg =paste("BSkyEditDatagrid: Dataset doesnot exists. or invalid rowindex."," Dataset Name:", dataSetNameOrIndex)
		warning("BSkyEditDatagrid: Dataset doesnot exists. or invalid rowindex.")
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
			
			# cat("Error caught in BSkyEditDatagrid \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level edit grid data function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkyEditDatagrid \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function BSkyEditDatagrid
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level edit grid data function\n")
		return(invisible(BSkyReturnStructure()))
}


BSkyMultipleEditDataGrid <- function (startRow = 2, startCol = 1, noOfRows = 4, noOfCols = 3,
    data = NA, dataSetNameOrIndex = "mtcars")
{
  
  	deciCh = '.'
	groupingChar = ''

	##reading global value for decimal and display markers
	decimarker = BSkyGetDecimalMarker()
	dispmarker = BSkyGetDisplayMarker()
	globalmarkersset = FALSE
	if(decimarker!="" || dispmarker!="")
	{
		globalmarkersset = TRUE
	}

	if(decimarker!="")
	{
		deciCh = decimarker
	}

	if(dispmarker!="")
	{
		groupingChar = dispmarker
	}

	if(!globalmarkersset)
	{
		localeRes = Sys.getlocale("LC_COLLATE")
		if(localeRes == "German_Italy.1252" ||
		localeRes == "German_Liechtenstein.1252" ||
		localeRes == "German_Luxembourg.1252" ||
		localeRes == "German_Austria.1252" ||
		localeRes == "German_Switzerland.1252" ||
		localeRes == "German_Germany.1252" || 
		localeRes == "German_Belgium.1252")
		{
			
			deciCh = ','
			groupingChar = '.'
		} 
	}


  # Get system information
	sys_info <- Sys.info()
	
	# Check the operating system
	os_type <- sys_info['sysname']
	
		 # if (!is.na(data))
		# {
			 # validData =TRUE
		 # }
	 
	#  if (length(data) ==1 && is.na(data))
	if (is.null(data) || (is.null(dim(data)) && length(data) == 1 && is.na(data)))
	{
	validData =FALSE
	} else {
	validData =TRUE
	}
	
	
	# if (length(data) ==1 && is.na(data))
	if (is.null(data) || (is.null(dim(data)) && length(data) == 1 && is.na(data)))
	{

	if (os_type == "Windows") 
	{
	#print("The operating system is Windows.")
	# library(clipr, quietly = TRUE)
	if(!BSkyGetLibLoadMsgPrintSetting())
	{
		suppressPackageStartupMessages(
			suppressMessages(
				suppressWarnings(
					library(clipr, quietly = TRUE)
				)
			)
		)
	}
	# Read clipboard content
	clipboard_content <- read_clip()
	# Check if the clipboard is empty
	if (is.null(clipboard_content) || length(clipboard_content)==0 || all(clipboard_content == "") ){
	print("The clipboard is still being prepared or is empty. Please wait a few seconds and retry the paste or try copying again before pasting.")
	return(invisible("The clipboard is empty."))
	} else 
	{
	#print("The clipboard is not empty.")
	# tabular_data <- read.delim("clipboard", header = FALSE, stringsAsFactors = FALSE) ## num becomes string "50.000,50"
	delimiter =BSkyGetDelimMarker()
	templocale = locale(decimal_mark = deciCh, grouping_mark = groupingChar)
	## clipboard data is tab separated
	#tabular_data = readr::read_delim(readr::clipboard(), col_names = FALSE, locale =  templocale) #delim = '\t', 
	
	tabular_data =NULL
tabular_data <- tryCatch({
                  if (delimiter == "") {
				   readr::read_delim(readr::clipboard(), col_names = FALSE,
                      delim = "\t", locale = templocale, skip_empty_rows = FALSE, show_col_types = FALSE)
                   
                  }
                  else {
                    readr::read_delim(readr::clipboard(), col_names = FALSE,
                      locale = templocale, delim = delimiter, show_col_types = FALSE)
                  }
                }, error = function(e) {
                  tryCatch({
                     readr::read_delim(readr::clipboard(), col_names = FALSE,
                      locale = templocale, skip_empty_rows = FALSE, show_col_types = FALSE)
                  }, error = function(e2) {
                   
                    tryCatch({
                    readr::read_delim(readr::clipboard(), col_names = FALSE,
                      delim = "\n", locale = templocale, skip_empty_rows = FALSE, show_col_types = FALSE)
                  }, error = function(e2) {
                   
                    "error"
                   
                   
                   
                  })  
                  })
                })

if ("character" %in% class(tabular_data))
{
    if (tabular_data =="error")
	{
	return (invisible("The paste failed as we could not guess the character that separates variables/rows from your data, please examine the data you want to paste and manually specify the delimited in the Triple dot>Settings>Misc. We have already tried the newline character i.e. \\n without success."))
	}
}
if (base::is.null(tabular_data))
{
    return (invisible("Something went wrong with realing from the clipboard and a NULL was returned"))
}	
	
	## run for each column to get desired encoded data
	tabular_data <- data.frame(lapply(tabular_data, function(x) iconv(x)))

	noOfRows =nrow(tabular_data)
	noOfCols =ncol(tabular_data)
	data = as.character(t(as.matrix(tabular_data)))
	validData =TRUE
	}
  
	} else if (os_type == "Darwin") {

	clipboard_content <- tryCatch({
	readLines(pipe("pbpaste"), warn = FALSE)
	}, error = function(e) {
  NULL
	})

# Check if the clipboard is empty
if (is.null(clipboard_content) || length(clipboard_content) == 0 || all(clipboard_content == "")) {
  print("The clipboard is empty.")
  return(invisible("The clipboard is empty."))
} else {
#   tabular_data <- read.delim(pipe("pbpaste"), header = FALSE, stringsAsFactors = FALSE)

	clipboardEncoding = ""  ## get it from clipboard somehow.
	templocale = locale(decimal_mark = deciCh, grouping_mark = groupingChar)
	## clipboard data is tab separated
	tabular_data = readr::read_delim(readr::clipboard(), col_names = FALSE, locale =  templocale, show_col_types = FALSE) #delim = '\t',
	## run for each column to get desired encoded data
	tabular_data <- data.frame(lapply(tabular_data, function(x) iconv(x)))
	
	noOfRows =nrow(tabular_data)
	noOfCols =ncol(tabular_data)
	data = as.character(t(as.matrix(tabular_data)))
  	validData =TRUE
}

  #print("The operating system is macOS.")
   
} 

}
  
  
  # #Handle single cell, we call BSkyEditDatagrid
  # tabular_data <- read.delim("clipboard", header = FALSE, stringsAsFactors = FALSE)
  
  # tabular_data <- read.delim(pipe("pbpaste"), header = FALSE, stringsAsFactors = FALSE)
  if (validData)
  {
  #data = as.character(as.matrix(tabular_data))
  
  if (length(data) == 1) {
        colname = eval(parse(text = paste("names(", dataSetNameOrIndex,
            ")", "[", startCol, "]", sep = "")))
        if (data == "" || data == "<NA>") {
            BSkyEditDatagrid(colname = colname, rowindex = startRow -
                1, dataSetNameOrIndex = dataSetNameOrIndex)
        }
        else {
            BSkyEditDatagrid(colname = colname, colceldata = data,
                rowindex = startRow - 1, dataSetNameOrIndex = dataSetNameOrIndex)
        }
     return(invisible())  
    }
  
  totalDatasetCols = eval(parse(text = paste("ncol(", dataSetNameOrIndex, ")")))
  newColumnBaseName ="var"
  newColumnSuffix = 1
  for (i in 1:noOfCols) { 
	# Adding new columns
      if (startCol > totalDatasetCols)
      {
        newColName =paste(newColumnBaseName, startCol, sep="")
        eval(parse(text = paste(".GlobalEnv$", dataSetNameOrIndex,
                  "[,c(", deparse(newColName), ")]", "<- NA", sep = "")))
        
        eval(parse(text = paste(".GlobalEnv$", dataSetNameOrIndex,
                  "[,c(", startCol, ")]", "<- as.numeric(.GlobalEnv$",
                  dataSetNameOrIndex, "[, c(", deparse(newColName), ")])",
                  sep = "")))
		newColumnSuffix = newColumnSuffix +1

      }
      
      
        every_column <- data[seq(i, length(data), by = noOfCols)]
        classOfVariable <- eval(parse(text = paste("class(",
            dataSetNameOrIndex, "[,", startCol, "])")))
        if ("numeric" %in% classOfVariable || "integer" %in%
            classOfVariable) {
			
			 every_column_temp <- every_column 
			 every_column_na_removed = every_column[!is.na(every_column)]
            empty_string_count <- sum(nchar(every_column_na_removed) ==
                0 )
            every_column = suppressWarnings(as.numeric(every_column))
              empty_numeric_count <- sum(is.na(every_column) ==
                TRUE)
			
                    	
          
          
            if (empty_string_count != empty_numeric_count) {
                every_column <- every_column_temp
                eval(parse(text = paste(".GlobalEnv$", dataSetNameOrIndex,
                  "[,", startCol, "]", "<- as.character(.GlobalEnv$",
                  dataSetNameOrIndex, "[, ", startCol, "])",
                  sep = "")))
            }
            end_position <- startRow + noOfRows - 1
            eval(parse(text = paste(".GlobalEnv$", dataSetNameOrIndex,
                "[startRow:end_position, ", startCol, "] <- every_column",
                sep = "")))
            startCol = startCol + 1
        }
        else if ("factor" %in% classOfVariable || "ordered" %in%
            classOfVariable) {
			every_column[every_column ==""] =NA
            every_column = as.factor(every_column)
            levelsInPastedData = levels(every_column)
            levelsInDestinationColumn = eval(parse(text = paste("levels(",
                dataSetNameOrIndex, "[,", startCol, "])", sep = "")))
            for (element in levelsInPastedData) {
                if (!(element %in% levelsInDestinationColumn)) {
                  eval(parse(text = paste(".GlobalEnv$", dataSetNameOrIndex,
                    "[,", startCol, "]", " <- factor(.GlobalEnv$",
                    dataSetNameOrIndex, "[,", startCol, "],",
                    "levels = c(levels(.GlobalEnv$", dataSetNameOrIndex,
                    "[,", startCol, "]),", deparse(element),
                    "))", sep = "")))
                }
            }
            end_position <- startRow + noOfRows - 1
            eval(parse(text = paste(".GlobalEnv$", dataSetNameOrIndex,
                "[startRow:end_position, ", startCol, "] <- every_column",
                sep = "")))
            startCol = startCol + 1
        }
        else if ("character" %in% classOfVariable) {
            end_position <- startRow + noOfRows - 1
            eval(parse(text = paste(".GlobalEnv$", dataSetNameOrIndex,
                "[startRow:end_position, ", startCol, "] <- every_column",
                sep = "")))
            startCol = startCol + 1
        }
        else if ("Date" %in% classOfVariable || "POSIXct" %in%
            classOfVariable || "logical" %in% classOfVariable) {
            eval(parse(text = paste(".GlobalEnv$", dataSetNameOrIndex,
                "[,", startCol, "]", "<- as.character(.GlobalEnv$",
                dataSetNameOrIndex, "[, ", startCol, "])", sep = "")))
            end_position <- startRow + noOfRows - 1
            eval(parse(text = paste(".GlobalEnv$", dataSetNameOrIndex,
                "[startRow:end_position, ", startCol, "] <- every_column",
                sep = "")))
            startCol = startCol + 1
        }
		}
	}
}


#Adding a row in data grid
# rowindex = -1 means add as  a last row in data grid
#  rowdata <- list(agecat="Under 21",gender="Male",accid=1234,pop=112233)
#############################################################################################################
#Function: BSkyAddNewDatagridRow(colname, colceldata, rowdata, rowindex=0, dataSetNameOrIndex)										
#              																								
#Parameters: 
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		rowindex					- Row index, where this new row should be inserted in data grid.
#									  This will also act as a col index for data grid where the new col is to be inserted
#		rowdata(future use)			- rowdata is a list of values(data). Example
#										rowdata <- list(agecat="Under 21",gender="Male",accid=1234,pop=112233)
#		colname						- name of col whose values is given. Rest col will have NAs. Later use can edit.
#		coldata						- data for the colname for new record(row)
#		colceldata					- cell value/data for the col cell in new row.
#																											
#Description:
#		Add new row to data grid.
#																											
#Example: BSkyAddNewDatagridRow(colname="pop", colceldata="212343", rowindex=0, dataSetNameOrIndex="mydf")																								
#																											
#############################################################################################################
BSkyAddNewDatagridRow <- function(colname, colceldata, rowdata=NA, rowindex=0, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	#cat("\nAdding new Data row\n")
	#cat(rowdata)
	BSkyErrMsg = paste("BSkyAddNewDatagridRow: Error in Add Row in Grid : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colname, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyAddNewDatagridRow: Warning in Add Row in Grid : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colname, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{
	
		withCallingHandlers(
		{

	rowlist <- c()  ## "" was here earlier 09Aug2013
	#cat("\nFinding DataSet \n")		
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

	#set rowindex
	if(rowindex >= 0)
	     rowindex <- rowindex + 1 # UI is 0 based while in R here is 1 based. So, index 0 in UI means 1 in R
	else
		rowindex <- eval(parse(text=paste('nrow(',datasetname,')' )))#if rowindex -ve then edit last row.
	 #cat("\nRow Index:",rowindex)	
	#if dataset index is valid
	#if(DataSetIndex <= length(uadatasets$lst) && DataSetIndex >0)##Put these checks everywhere
	if(!is.null(datasetname))
	{	
		#Find is colname exists
# colIndex<-BSkyValidateColumn(DataSetIndex, colname)
colIndex <- BSkyValidateColumn(datasetname, colname)	
	 #cat("\nCol Index:",colIndex,"\n")
		#### Processing colceldata  ####	##Put following checks everywhere
		#if(colIndex <= length(uadatasets$lst[[DataSetIndex]]) && colIndex > 0)#Col exisits
		if(colIndex > 0)
		{
			# As per UI datagrid requirement colceldata should be numeric always.
			if(!is.na(as.numeric(colceldata)))
			{
				colceldata=as.numeric(colceldata)# converting "2" to 2
			}
			else
			{
				colceldata <- eval(parse(text=paste('which(levels(',datasetname,'[[',colIndex,']]) == colceldata)', sep='')))
				if(length(colceldata) == 0)
				{
					colceldata = NA
					cat("\nError: colceldata is non-numeric!\n")
					BSkyErrMsg =paste("BSkyAddNewDatagridRow: colceldata is non-numeric!"," Col Name:", colname)
					warning("BSkyAddNewDatagridRow: colceldata is non-numeric!")
				}
				
			}
			# cat("\ncolceldata is ",colceldata,"\n")	
			## Find length(no.) of levels. If its greater than 0 then its factor. OR we can directly do is.factor()
			# length(levels(datasetname[,colIndex]))>0
			nooflevels <- eval(parse(text=paste('length(levels(',datasetname,'[,colIndex]))',sep='')))
			isfac <- eval(parse(text=paste('is.factor(',datasetname,'[,colIndex])', sep='')))
			if(!is.na(colceldata) && isfac)#if true colIndex is factor and non-NA
			{
				if(colceldata <= nooflevels)
				{
					## Now get correct level coressponding to colceldata (only for factor cols)
					## basically converting numeric value of level to coressponding string.
					## eg.. convert 1 to "Male"
					colceldata <- eval(parse(text=paste('levels(',datasetname,'[,colIndex])[colceldata]',sep='')))
				}
				else
				{
					# cat("\nError: Not a valid factor level! Converting to NA.\n")
					BSkyWarnMsg =paste("BSkyAddNewDatagridRow: Not a valid factor level! Converting to NA.!"," Col Name:", colname)
					warning("BSkyAddNewDatagridRow: Not a valid factor level! Converting to NA.!")
					colceldata <- NA
				}
				# cat("\nColCelData is ",colceldata,"\n")	
			}
			else #non-factor col ##no need  I guess
			{
				# cat("\nNo code here \n")	
				BSkyErrMsg =paste("BSkyAddNewDatagridRow: No code in this section!"," Dataset Name:", dataSetNameOrIndex)
				warning("BSkyAddNewDatagridRow: No code in this section!")
			}
		}
		
		# get all columns
		col.names <- UAgetColNames(dataSetNameOrIndex)
		idx = 1;
		# for each find data type and put the default value
		for( cn in col.names)
		{
			 #cat("\nLoop cols:",cn,"\n")
			##colprop <- UAgetColProp(cn, Type.as.Class=FALSE, DataSetIndex)
			#defaultval <- eval( parse(text= paste("rowdata$",cn)) )
			# cat("\nDf:",defaultval)
			#if(is.character(defaultval))
	 			#eval( parse(text= paste(cn, "<-", 'defaultval')) )
			#else
				#eval( parse(text= paste(cn, "<-", defaultval)) )
			#cat("\n")
			
			if(length(rowdata)== length(col.names)) # if rowdata is provided for 1 complete row
			{
				eval( parse(text= paste(cn, "<- rowdata[idx]")) ) #value for each col
			}
			else if(cn == colname) # if rowdata is provided for just 1 col of the row.
			{
				eval( parse(text= paste(cn, "<- colceldata")) ) #value for first column only
			}			
			else # if no data for any of the column. Fill with NA or NaN
			{
				eval( parse(text= paste(cn, "<- NA")) ) #for rest of the columns
			}
			
			rowlist <- c(rowlist, cn)
			idx = idx + 1
		}
		## Since factor columns are loosing their attributes after new row addition. So we need to save a copy
		## and put it back after new row is added
		#### backup attributes of all factor cols ####
		# cat("\n")
		#print("Rec:")
		# print(rowlist)
		# cat("\n.....!#")
		#Create Command
		pre <- "data.frame("
		mid <- ""
		totcols <- length(rowlist)
		i <- 1
		for( rl in rowlist)
		{
			#newrow <- eval(parse(text=paste("data.frame(",rowlist[1],",",rowlist[2],")")))
			mid <- paste(mid,rl)
			if(i < totcols)
			{
				mid <- paste(mid, ",")
			}
			i <- i + 1
		}
		post <- ")"

		
		
		
#Dataset1[6,] = c(555,777)

#Dataset1[7,] = c(5555,7777)

#attributes(Dataset1)

#Dataset1 = rbind(Dataset1, c(444,666) )

#Dataset1 = rbind(Dataset1[1:3,], c(999,333), Dataset1[4:8,] )
		
		
		
		
		# create a record
		newrow <- eval(parse(text=paste(pre,mid,post))) ####OK###Till here###
		#cat("\n New Row \n"); print(newrow) ; cat("\n")
		totrows <- eval(parse(text=paste('nrow(',datasetname,')')))
		# add to the dataset at given index. Index should not be 1 more than the max rows currently.
		if(rowindex < 1 || rowindex > totrows ) # add as a last row
		{
		#cat("\nInsert at last location\n")
			# uadatasets$lst[[DataSetIndex]] <- rbind(uadatasets$lst[[DataSetIndex]], newrow)	##using rbind
			#eval(parse(text=paste(datasetname,'[',totrows+1,',] <<- newrow', sep='' ))) #overwrites old record with new one for given row index.
			eval(parse(text=paste(datasetname,' <- rbind(',datasetname,', ',deparse(rowdata, width.cutoff=500),' )',sep='' )))#. <<- to <-
			
		}
		else if(rowindex>1 && rowindex < totrows)# insert in the middle
		{
		#cat("\nInsert in the middle\n")
		# use deparse to stop this this repeating on each col value in newrow thus stopping it from executing it total col number of times.
		# paste(datasetname,' <<- rbind(',datasetname,'[1:',(rowindex-1),',],',deparse(c(4,5)),', ',datasetname,'[',rowindex,':',totrows,',] )', sep='')

			# eval(parse(text=paste( datasetname,' <<- rbind(',datasetname,'[1:(rowindex-1),],', newrow,', ',datasetname,'[(rowindex+1):totrows,] )', sep='')))
			eval(parse(text=paste(datasetname,' <- rbind(',datasetname,'[1:',rowindex-1,',],',deparse(rowdata, width.cutoff=500),', ',datasetname,'[',rowindex,':',totrows,',] )', sep='')))#. <<- to <-
		}
		else # insert as first
 		{
		       #cat("\nInsert at first location\n")
			   

	#cat(rowdata)
			# ####### Method 1 ##### Insert Record making a copy of dataset  ##########
			# temp <- eval(parse(text=paste(datasetname)))## making a copy
			# totalrows <- nrow(temp) ## <- totrows
			# #eval(parse(text=paste(datasetname,'[rowindex,] <<- newrow', sep=''))) ### overwrite record at rowindex
			# eval(parse(text=paste( datasetname <<- rbind(datasetname[1:(rowindex-1),], newrow, datasetname[(rowindex+1):totrows,] ), sep='')))
			
			# cat(text=paste(datasetname,' <<- rbind(deparse(newrow),',datasetname,'[1:',totrows,',] )', sep=''))
			# cat("\n")
			# cat(text= paste(datasetname,' <<- rbind(',deparse(newrow),',',datasetname,'[1:',totrows,',] )', sep=''))
			# cat("\n")
			# cat(text=paste(datasetname,' <<- rbind(deparse(',newrow,'),',datasetname,'[1:',totrows,',] )', sep=''))
			# cat("\nPrinting Text\n")
			
			# text=paste(datasetname,' <<- rbind(',paste("c(33,666)"),',',datasetname,'[1:',totrows,',] )', sep='')
			# print(text)

			eval(parse(text=paste(datasetname,' <- rbind(',deparse(rowdata, width.cutoff=500),',',datasetname,'[1:',totrows,',] )', sep='')))#. <<- to <-
	
			# ### Now copy remaing records from temp to regular dataset  ###
			# nextrowindex <- rowindex + 1
			# newrowcount <- totalrows + 1
			# #eval(parse(text=paste( paste(datasetname,'[nextrowindex:newrowcount,]',sep=''),' <<- ',paste('temp[rowindex:totalrows,]',sep='') )))
			# eval(parse(text=paste( paste(datasetname,'[nextrowindex:newrowcount,]',sep=''),' <<- ',paste('temp[rowindex:totalrows,]',sep='') )))
			# temp <- NULL ### removing temp memory
			
			####### Method 2 ##### Insert Record using rbind. We lose attributes  ##########
			#if(rowindex == 1)
			#{
				#uadatasets$lst[[DataSetIndex]] <- rbind(newrow,uadatasets$lst[[DataSetIndex]]) 
			#}
			#else
			#{
				#preindex <- rowindex - 1 
				############# test is properly for large datasets. ##########
				#uadatasets$lst[[DataSetIndex]] <- rbind(uadatasets$lst[[DataSetIndex]][1:preindex,], newrow, uadatasets$lst[[DataSetIndex]][rowindex:nrow(uadatasets$lst[[DataSetIndex]]),])
				##renumbering
				#row.names(uadatasets$lst[[DataSetIndex]]) <- 1:nrow(uadatasets$lst[[DataSetIndex]])	
			#}
		}

		#### put back attributes of all factor cols ####
		#-return("")#result.commandstring
	}#if dataset index is valid
	else
	{
		# cat("\nDataset doesnot exists.\n")
		BSkyErrMsg =paste("BSkyAddNewDatagridRow: Dataset doesnot exists"," Dataset Name:", dataSetNameOrIndex)
		warning("BSkyAddNewDatagridRow: Dataset doesnot exists")
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
			
			# cat("Error caught in BSkyAddNewDatagridRow \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level add row in grid function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkyAddNewDatagridRow \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level add row in grid function\n")
		return(BSkyReturnStructure())
}
 
BSkyAddNewDatagridRowAR <- function(rowdata=NA, rowindex=0, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	#cat("\nAdding new Data row\n")
	BSkyErrMsg = paste("BSkyAddNewDatagridRow: Error in Add Row in Grid : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", sep="")
	BSkyWarnMsg = paste("BSkyAddNewDatagridRow: Warning in Add Row in Grid : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{
	
		withCallingHandlers(
		{
			
			rowlist <- c()  
			datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
			totrows <- eval(parse(text=paste('nrow(',datasetname,')')))
			if(rowindex >= 0)
			{
				rowindex <- rowindex + 1 # UI is 0 based while in R here is 1 based. So, index 0 in UI means 1 in R
			}
			else
			{
				rowindex <- eval(parse(text=paste('nrow(',datasetname,')' )))#if rowindex -ve then edit last row.
			}
			#cat("\nRow Index:",rowindex)	
			#if dataset index is valid
			#if(DataSetIndex <= length(uadatasets$lst) && DataSetIndex >0)##Put these checks everywhere
			if(!is.null(datasetname))
			{	
				#backup all col attrs in BSkyblankDSallColAttr
				backupAllColAttr(datasetname)		

				col.names <- UAgetColNames(dataSetNameOrIndex)
				idx = 1;
				# for each find data type and put the default value
				for( cn in col.names)
				{
					 #
					 #cat("\nLoop cols:",cn,"\n")
					 #print(rowdata[idx])
					 
					##colprop <- UAgetColProp(cn, Type.as.Class=FALSE, DataSetIndex)
					#defaultval <- eval( parse(text= paste("rowdata$",cn)) )
					# cat("\nDf:",defaultval)
					#if(is.character(defaultval))
	 					#eval( parse(text= paste(cn, "<-", 'defaultval')) )
					#else
						#eval( parse(text= paste(cn, "<-", defaultval)) )
					#cat("\n")
					
					#classOfCol =eval(parse(text=paste(datasetname, "$",cn)))
					classOfCol =eval(parse(text=paste(  "class(", datasetname, "$",cn, ")")))
					#cat("--Class col\n")
					#cat(classOfCol)
					if(classOfCol =="numeric")
					{
						#cat("--Numeric col\n")
						
						#01Feb2018  #### Similar fix is done to all such 'if's below but detailed comments are added to 1st one only ###
						# '!is.na(rowdata[idx])' is added as a fix for new row not adding from Datagrid "Click here to add new row"
						# The reason being 'rowdata[idx]' was NA. So the condition [ NA != "" ] resulted in NA and not TRUE/FALSE
						# and 'if' condition crashed because it needs expression/condition to result in either TRUE or FALSE. 
						# To fix we had to add is.na part so now whole 'if' expression will always produce either TRUE or FALSE.
						if (!is.na(rowdata[idx]) && rowdata[idx] !="")
						{
							eval( parse(text= paste(cn, "<- as.numeric(rowdata[idx])")) ) #value for each col
						}
						else
						{
							#cat("\nMaking it NA")
							eval( parse(text= paste(cn, "<- NA")) )
						}
					}
					else if(classOfCol =="POSIXct" || classOfCol =="POSIXlt" || classOfCol =="Date")
					{
						#cat("\nEdit Date cell while adding row.")
						stdt <- strptime(rowdata[idx],format='%m/%d/%Y %H:%M:%S')
						posdt <- as.POSIXct(stdt)
						#print(posdt)
						
						#01Feb2018 
						if (!is.na(rowdata[idx]) && rowdata[idx] !="")
						{
						#cat("\nputting POSXct")
							eval( parse(text= paste(cn, "<- posdt")) ) #value for date col
						}
						else
						{
						#cat("\nputting NA for POSXct")
							eval( parse(text= paste(cn, "<- NA")) )
						}
						#cat("\n")
						#print(cn)
					}					
					else if (classOfCol =="character") 
					{
						#01Feb2018 
						if (!is.na(rowdata[idx]) && rowdata[idx] !="")
						{
							eval( parse(text= paste(cn, "<- rowdata[idx]")) )
						}
						else
						{
							eval( parse(text= paste(cn, "<- NA")) )
						}
					}
					else if (classOfCol =="factor")#28Jun2016 Added as.factor, otherwise adding a row was changing factor to character
					{
						#01Feb2018 
						if (!is.na(rowdata[idx]) && rowdata[idx] !="")
						{
							eval( parse(text= paste(cn, "<- as.factor(rowdata[idx])")) )
						}
						else
						{
							eval( parse(text= paste(cn, "<- as.factor(NA)")) )
						}
					}					
					else
					{
						#01Feb2018 
						if (!is.na(rowdata[idx]) && rowdata[idx] !="")
						{
							eval( parse(text= paste(cn, "<- rowdata[idx]")) )
						}
						else
						{
							eval( parse(text= paste(cn, "<- NA")) )
						}					
					}
			
					#cat("\nCN\t: ")
					#print(cn)
					#print(eval(parse(text=paste(cn))) )
				rowlist <- c(rowlist, cn)
				idx = idx + 1
			} #End of for
		#cat("\nRowList: ")
		#print(rowlist)
		pre <- "data.frame("
		mid <- ""
		totcols <- length(rowlist)
		i <- 1
		for( rl in rowlist)
		{
			#newrow <- eval(parse(text=paste("data.frame(",rowlist[1],",",rowlist[2],")")))
			mid <- paste(mid,rl)
			if(i < totcols)
			{
				mid <- paste(mid, ",")
			}
			i <- i + 1
		}
		post <- ")"

		newrow <- eval(parse(text=paste(pre,mid,post))) ####OK###Till here###
		#cat("\nNow Adding new row\n")
		#print(newrow)
		#cat("\nClass of the dataset col 3 before rbind:")
		#print(eval(parse(text=paste('class(',datasetname,'[[3]])', sep=''))))
		if(rowindex >= totrows) # add at last
		{
			#cat("\nInsert at last")
			eval(parse(text=paste(datasetname,' <- rbind(',datasetname,",", "newrow",")")))#. <<- to <-
			#cat("\nInserted at last")
		}
		else if(rowindex > 1 && rowindex < totrows) # insert in middle
		{
			#cat("\nInsert at middle")
			# Dataset1 <- rbind( Dataset1[1:5,], newrow, Dataset1[6:18,])
			# eval(parse(text=paste(datasetname,' <<- rbind(',datasetname,'[1:',rowindex-1,',],',deparse(rowdata, width.cutoff=500),', ',datasetname,'[',rowindex,':',totrows,',] )', sep='')))
			rowbefore <- rowindex - 1
			#cat("\nExecuting:\n")
			#cat(paste(datasetname,"<<- rbind(",datasetname,"[1:",rowbefore,",]", ", newrow, ", datasetname,"[",rowindex,":",totrows,",])"))
			eval(parse(text=paste(datasetname,"<- rbind(",datasetname,"[1:",rowbefore,",]", ", newrow, ", datasetname,"[",rowindex,":",totrows,",])") ))#. <<- to <-
			#cat("\nInserted at middle")
		}
		else # insert at first location
		{
			#cat("\nInsert at first")
			# Dataset1 <- rbind( newrow, Dataset1[1:18,])
			#cat("\nExecuting first:\n")
			#cat(paste(datasetname," <<- rbind(", "newrow",",", datasetname,")"))
			eval(parse(text=paste(datasetname," <- rbind(", "newrow",",", datasetname,")")))#. <<- to <-
			#Adds 3 rows :  eval(parse(text=paste(datasetname," <- rbind(", newrow,",", datasetname,")")))#. <<- to <-
			#cat("\nInserted at first")
		}
		
		#cat("\nClass of the dataset col 3 After adding row :")
		#print(eval(parse(text=paste('class(',datasetname,'[[3]])', sep=''))))
		#cat("\n")
		
			#restore all col attrs from BSkyblankDSallColAttr
			restoreAllColAttr(datasetname)
		}#End of if
} ,#End of with calling handlers
		warning = UAwarnHandlerFn
	)
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
			
			# cat("Error caught in BSkyAddNewDatagridRow \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level add row in grid function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkyAddNewDatagridRow \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level add row in grid function\n")
		return(BSkyReturnStructure())	
	
}
 
 
#############################################################################################################
#Function: BSkyRemoveDatagridRow(rowindex=0, dataSetNameOrIndex)										
#              																								
#Parameters: 
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		rowindex					- Row index, which is to be removed from data grid.
#																											
#Description:
#		Add new row to data grid.
#																											
#Example: BSkyRemoveDatagridRow(rowdata="average", rowindex=0, dataSetNameOrIndex="mydf")																								
#																											
#############################################################################################################
BSkyRemoveDatagridRow <- function(rowindex=0, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyRemoveDatagridRow: Error in Remove Row from grid : ", "DataSetName :", dataSetNameOrIndex," ", "Row index :", paste(rowindex, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyRemoveDatagridRow: Warning in Remove Row from grid : ", "DataSetName :", dataSetNameOrIndex," ", "Row index :", paste(rowindex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{
	
		withCallingHandlers(
		{
	##cat("\nFinding DataSet \n")		
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	totrows <- eval(parse(text=paste('nrow(',datasetname,')' )))
	totcols <- eval(parse(text=paste('ncol(',datasetname,')' )))
	#set rowindex
	if(rowindex >= 0)
	     rowindex <- rowindex + 1 # UI is 0 based while in R here is 1 based. So, index 0 in UI means 1 in R
	else
		rowindex <- totrows + 1#if rowindex -ve then make rowindex greater than total rows
	
	#if dataset index is valid and rowindex is valid. rowindex should be not more that total rows.
	if(!is.null(datasetname) && rowindex <= totrows)
	{	
		#backup all col attrs in BSkyblankDSallColAttr
		backupAllColAttr(datasetname)

		if(totcols==1)
		{
			bkupcolnames = eval(parse(text=paste('names(',datasetname,')',sep='')))
			eval(parse(text=paste(datasetname,' <- as.data.frame(',datasetname,'[-rowindex,], stringsAsFactors = FALSE)' )))#. <<- to <-
			eval(parse(text=paste('names(',datasetname,') <- c(bkupcolnames)',sep='')))
		}
		else
		{
			eval(parse(text=paste(datasetname,' <- ',datasetname,'[-rowindex,]' )))
		}

		#restore all col attrs from BSkyblankDSallColAttr
		restoreAllColAttr(datasetname)

	}#if dataset index is valid and existing row index is valid
	else
	{
		# cat("\nDataset doesnot exists. or invalid rowindex\n")
		BSkyErrMsg =paste("BSkyRemoveDatagridRow: Dataset doesnot exists. or invalid rowindex"," Dataset Name:", dataSetNameOrIndex)
		warning("BSkyRemoveDatagridRow: Dataset doesnot exists. or invalid rowindex")
	}
	#-return("")#result.commandstring
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
			
			# cat("Error caught in BSkyRemoveDatagridRow \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level remove grid row function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkyRemoveDatagridRow \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level remove grid row function\n")
		return(BSkyReturnStructure())
}




#############################################################################################################
#Function: BSkyAddVarRow(newcolname, datagridcolval, rowindex=0, dataSetNameOrIndex)	This does not have datatype											
#              																								
#Parameters: 
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		rowindex					- Row index, where this new variable should be inserted in var grid.
#									This will also act as a col index for data grid where the new col is to be inserted
#		datagridcolval				- data for the new col in datagrid view.
#		newcolname					- new col name that is to be added as new row in var grid and as new col in datagrid
#																											
#Description:
#		Add new Variable to Var grid and make corressponding cahanges to data grid(ie.. add new col there)
#																											
#Example: BSkyAddVarRow(newcolname="average", datagridcolval=25, rowindex=0, dataSetNameOrIndex="mydf")																								
#																											
#############################################################################################################

BSkyAddVarRow <-function (newcolname, rdatatype, datagridcolval, newcolindex = 0, DateFormat ="%y/%m/%d",
    dataSetNameOrIndex)
{
    BSkyFunctionInit()
    BSkySetCurrentDatasetName(dataSetNameOrIndex)
    BSkyErrMsg = paste("BSkyAddVarRow: Error in Add Var Row : ",
        "DataSetName :", dataSetNameOrIndex, " ", "Variable Name List :",
        paste(newcolname, collapse = ","), sep = "")
    BSkyWarnMsg = paste("BSkyAddVarRow: Warning in Add Var Row : ",
        "DataSetName :", dataSetNameOrIndex, " ", "Variable Name List :",
        paste(newcolname, collapse = ","), sep = "")
    BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
    tryCatch({
        withCallingHandlers(
{
            if (!is.na(as.numeric(datagridcolval))) {
                colval = as.numeric(datagridcolval)
            }
            else {
                colval = datagridcolval
            }
            if (datagridcolval == ".") {
                colval = ""
            }
            props <- c("Name", "Type", "Label", "Levels", "Missing",
                "Align", "Measure", "Split", "Width", "Decimals",
                "Columns", "Role","DateFormat")
				#Added by Aaron 06/25/2020 passed string below
            if (rdatatype == "character") {
                pval <- list(newcolname, rdatatype, "",
                  NULL, "none", "Left", "String", FALSE, 4, 0,
                  8, "Input", "")
            }
            else if (rdatatype == "double") {
                pval <- list(newcolname, rdatatype, "",
                  NULL, "none", "Left", "Scale", FALSE, 4, 0, 8,
                  "Input", "")
            }
			 else if (rdatatype == "POSIXct") {
                pval <- list(newcolname, rdatatype, "",
                  NULL, "none", "Left", "Date", FALSE, 4, 0, 8,
                  "Input", DateFormat)
            }
			else if (rdatatype == "factor")
			{
			pval <- list(newcolname, rdatatype, "",
							  "", "none", "Left", "Nominal", FALSE, 4, 0, 8,
							  "Input", "")
			}
			#Added by Aaron 06/25/2020 passed Ordinal below
			else if (rdatatype == "ordered")
			{
			pval <- list(newcolname, rdatatype, "",
							  "", "none", "Left", "Ordinal", FALSE, 4, 0, 8,
							  "Input", "")
			}
            rowlist <- ""
            datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
            if (!is.null(datasetname)) {
                colIndex <- BSkyValidateColumn(datasetname, newcolname)
#Added by Aaron 06/23/2020 This is the case of a new column name
                if (colIndex < 1) {
                  newcolmisatt <- eval(parse(text = paste(newcolname,
                    "<-list(", newcolname, "=list(type=\"none\", value=\"\"))")))
                  newvarlbls <- c()
                  varlabels <- eval(parse(text = paste("attr(",
                    datasetname, ",\"variable.labels\")")))
                  misvals <- eval(parse(text = paste("attr(",
                    datasetname, ",\"misvals\")")))
                  oldcolscount <- eval(parse(text = paste("ncol(",
                    datasetname, ")")))
                  cols <- oldcolscount + 1
                  vlbls <- eval(parse(text = paste("c(", newcolname,
                    " = newcolname)")))
                  ##-#-##BkupObj <- BSkyDatasetLevelAttributeBackup(datasetname)
				eval(parse(text=paste('dsallattr <- attributes(',datasetname,')', sep='')))#fix for design
				#except names and row.names backup all attributes. Because names and row.name will get modified.
				eval(parse(text=paste('bkupattr <- dsallattr[!(names(dsallattr) %in% c("names", "row.names"))]',sep='')))#fix for design
 ##Added by Aaron 06/23/2020
 #newcolindex is passed to the function and indicated the position to insert
                  if ((newcolindex > 0) || (newcolindex <= cols)) {
                    n <- newcolindex
                    max <- oldcolscount
                    eval(parse(text = paste("oldcolnames <- names(",
                      datasetname, ")", sep = "")))
                    strasfact = FALSE
                    if (rdatatype == "double") {
                      newcol <- eval(parse(text = paste("c(rep( c(0), nrow(",
                        datasetname, ")))", sep = "")))
                    }
					
					 if (rdatatype == "POSIXct") {
                       newcol <- eval(parse(text = paste("as.POSIXct(c(rep( strptime(c(Sys.time()), format=DateFormat), nrow(",
                        datasetname, "))))", sep = "")))
                    }
                    else  if (rdatatype == "factor"){
                      strasfact = TRUE
                      newcol <- eval(parse(text = paste("factor(c(rep( c(NA), nrow(",
                        datasetname, "))))", sep = "")))
                    }
					 ##Added by Aaron 06/23/2020
					 #changed c(rep( c(NA) with rep (c('')
				else if (rdatatype == "character")
				{
				newcol <- eval(parse(text = paste("c(rep( c(''), nrow(",
										datasetname, ")))", sep = "")))
				}
                    if (n == (max + 1)) {
                      newcolnames = c(oldcolnames[1:(n - 1)],
                        newcolname)
                      newvarlbls = c(varlabels[1:(n - 1)], vlbls)
                      eval(parse(text = paste(datasetname, " <- cbind(",
                        datasetname, ", newcol, stringsAsFactors=",
                        strasfact, ")", sep = "")))
                    }
                    else if (n == 1) {
                      cat("\nInsert as first col\n")
                      newcolnames = c(newcolname, oldcolnames[1:max])
                      newvarlbls = c(vlbls, varlabels[1:max])
                      eval(parse(text = paste(datasetname, " <- cbind(",
                        datasetname, ",newcol, stringsAsFactors=",
                        strasfact, ")[, c((", max, "+1), 1:",
                        max, " )]", sep = "")))
                    }
                    else {
                      newcolnames = c(oldcolnames[1:(n - 1)],
                        newcolname, oldcolnames[n:max])
                      newvarlbls = c(varlabels[1:(n - 1)], vlbls,
                        varlabels[n:max])
                      eval(parse(text = paste(datasetname, " <- cbind(",
                        datasetname, ", newcol, stringsAsFactors=",
                        strasfact, ")[, c(1:(", n, "-1), (",
                        max, "+1), ", n, ":", max, " )]", sep = "")))
                    }

					#eval(parse(text=paste('attributes(',datasetname,') = bkupattr', sep='')))# this is not req as 
					#we have new restore just before 'else' below
					
                    eval(parse(text = paste("names(", datasetname,") <- c(newcolnames)", sep = "")))
                  }
                  i = 1
                  for (prop in props) {
					print(prop)
                    UAsetColProperties(dataSetNameOrIndex = datasetname,
                      colNameOrIndex = newcolname, propertyName = prop,
                      propertyValue = pval[[i]], mistype = "none",
                      newOrder = c())
                    i = i + 1
                  }
                  eval(parse(text = paste("attr(", datasetname,
                    ",\"variable.labels\") <- newvarlbls")))
                  eval(parse(text = paste("attr(", datasetname,
                    ",\"misvals\") <- c(attr(", datasetname,
                    ",\"misvals\")[1:", (cols - 1), "], ", newcolmisatt,
                    ")", sep = "")))
                  ##-#-##BSkyDatasetLevelAttributeRestore(datasetname, BkupObj)
				eval(parse(text=paste('attributes(',datasetname,') = c(attributes(',datasetname,'), bkupattr)', sep='')))#fix for design
                }
                else {
                  BSkyErrMsg = paste("BSkyAddVarRow: Col with that name already exists...",
                    " Dataset Name:", dataSetNameOrIndex)
                  warning("BSkyAddVarRow: Col with that name already exists...")
                }
            }
            else {
                BSkyErrMsg = paste("BSkyAddVarRow: Dataset not found!...",
                  " Dataset Name:", dataSetNameOrIndex)
                warning("BSkyAddVarRow: Dataset not found!...")
            }
        }, warning = UAwarnHandlerFn)
    }, error = UAerrHandlerFn, silent = TRUE)
    if (BSkyLocalErrorFound() == TRUE) {
        BSkyLocalErrorFlagsReset()
    }
    if (BSkyLocalWarningFound() == TRUE) {
        BSkyLocalWarningFlagsReset()
    }
    BSkyFunctionWrapUp()
    return(invisible(BSkyReturnStructure2()))
}



BSkyDatasetLevelAttributeBackup <- function(datasetname)
{
			#cat("\nStart Bakcup process")
			retobj <- list()
			#retobj$varlbls <- eval(parse(text=paste('attr(',datasetname,',"variable.labels")',)))
			retobj$split <- eval(parse(text=paste('attr(',datasetname,',"split")')))
			#cat("\nsplit stored.\t")
			retobj$splitcolnames <- eval(parse(text=paste('attr(',datasetname,',"splitcolumnnames")')))
			#cat("\nsplitcolumnnames stored.\t")
			retobj$splitcolindexes <- eval(parse(text=paste('attr(',datasetname,',"splitcolumnindex")')))
			#cat("\nsplitcolumnindex stored.\t")
			## Add 2 more attribute "filetype" and "slice" (differentiate slice from spss dataset)
			retobj$filetype <- eval(parse(text=paste('attr(',datasetname,',"filetype")')))
			#cat("\nfiletype stored.\t")
			retobj$slice <- eval(parse(text=paste('attr(',datasetname,',"slice")')))
			#cat("\nslice stored.\t")

			### New Attribute for missings  at Dataset Level###
			retobj$misvals <- eval(parse(text=paste('attr(',datasetname,',"misvals")')))
			#cat("\nmisvals stored.\t")
			retobj$maxfactor <- eval(parse(text=paste('attr(',datasetname,',"maxfactor")')))
			#cat("\nmaxfactor stored.\t")

			### design class related attributes ###

			return(retobj)
}

BSkyDatasetLevelAttributeRestore <- function(datasetname, retobj)
{
			#cat("\nStart Restore process")
			#cat(retobj$split)
			#eval(parse(text=paste('attr(',datasetname,',"variable.labels") <<- ',retobj$varlbls)))
			eval(parse(text=paste('attr(',datasetname,',"split") <- retobj$split'))) #. <<- to <-
			#cat("\nsplit set.\t")
			#cat(retobj$splitcolnames)
			eval(parse(text=paste('attr(',datasetname,',"splitcolumnnames") <- retobj$splitcolnames')))#. <<- to <-
			#cat("\nsplitcolumnnames set.\t")
			eval(parse(text=paste('attr(',datasetname,',"splitcolumnindex") <- retobj$splitcolindexes')))#. <<- to <-
			#cat("\nsplitcolumnindex set.\t")
			## Add 2 more attribute "filetype" and "slice" (differentiate slice from spss dataset)
			eval(parse(text=paste('attr(',datasetname,',"filetype") <- retobj$filetype')))#. <<- to <-
			#cat("\nfiletype set.\t")
			eval(parse(text=paste('attr(',datasetname,',"slice") <- retobj$slice')))#. <<- to <-
			#cat("\nslice set.\t")

			### New Attribute for missings  at Dataset Level###
			eval(parse(text=paste('attr(',datasetname,',"misvals") <- retobj$misvals')))#. <<- to <-
			#cat("\nmisvals set.\t")
			eval(parse(text=paste('attr(',datasetname,',"maxfactor") <- retobj$maxfactor')))#. <<- to <-
			#cat("\nmaxfactor set.\t")
}

BSkySetAttributes<-function(startindex, endindex)
{
	
}

 

#############################################################################################################
#Function: BSkyRemoveVarRow(delcolname, dataSetNameOrIndex)												
#              																								
#Parameters: 
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		delcolname					- col name that is to be removed from datagrid
#																											
#Description:
#		Removes row from var grid (ie.. removes col form data grid)
#																											
#Example: BSkyRemoveVarRow(delcolname="average", dataSetNameOrIndex="mydf")																								
#																											
#############################################################################################################
BSkyRemoveVarRow <- function(delcolname, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyRemoveVarRow: Error in Remove Var Row : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(delcolname, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyRemoveVarRow: Warning in Remove Var Row : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(delcolname, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{
	
		withCallingHandlers(
		{
	#M1 :- mydata[substr(rownames(mydata),1,1)!="U",] 

	#cat("\nFinding DataSet \n")		
			datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
			if(!is.null(datasetname))
			{	
# colIndex<-BSkyValidateColumn(DataSetIndex, delcolname)
colIndex <- BSkyValidateColumn(datasetname, delcolname)	
		rowindex <- colIndex
		#Old col will have index greater than 0
		if(colIndex > 0)#col found.
		{
			# cat("\nCol found...")
			# remove col from the dataset.
			# rowindex in vargrid is actually colindex in datagrid
			
			colcount <- eval(parse(text=paste('ncol(',datasetname,')'))) ## orginal col count before deleting a col
			#M2: removing by assigning to NULL
			#04Aug2016 eval(parse(text=paste(datasetname,'[',colIndex,'] <- NULL',sep='')))#. <<- to <-
			eval(parse(text=paste(datasetname,'<-', datasetname,'[-',colIndex,']',sep='')))# easy way to remove col.
			
			## Also fix variable.labels 09Aug2013
			remainingLabels <- c()
			for(i in 1:colcount) 
			{
				if(i!=colIndex) ## except removed col label, rest are saved and restored later
				{
					onelabel <- eval(parse(text=paste('attr(',datasetname,',"variable.labels")[',i,']',sep='')))
					# cat("\nOne Lbl:",onelabel)
					remainingLabels <- c(remainingLabels, onelabel)
				}
			}
			eval(parse(text=paste('attr(',datasetname,',"variable.labels") <- remainingLabels',sep='')))    ## restoring labels.#. <<- to <-
			# eval(parse(text=paste('attr(',datasetname,', "variable.labels")'))) #attr(Dataset1, "variable.labels")
			#M3: using cbind
			#numcol <- ncol(uadatasets$lst[[DataSetIndex]])
			#if(rowindex == 1) 
			#{
				#uadatasets$lst[[DataSetIndex]] <- cbind(uadatasets$lst[[DataSetIndex]][,2:numcol])	
			#}
			#else
 			#{
				#if(rowindex == numcol)
				#{
					#numcol = numcol -1
					#uadatasets$lst[[DataSetIndex]] <- cbind(uadatasets$lst[[DataSetIndex]][,1:numcol])
				#}
				#else
				#{
					#preindex <- rowindex - 1
					#nextindex <- rowindex + 1
					#uadatasets$lst[[DataSetIndex]] <- cbind(uadatasets$lst[[DataSetIndex]][,1:preindex],uadatasets$lst[[DataSetIndex]][,nextindex:numcol])
				#}
			#}
		}#col found
		else
		{
			# cat("\nCol with that name doesn't exists...\n")
			BSkyErrMsg =paste("BSkyRemoveVarRow: Col with that name doesn't exists..."," Col Name:", delcolname)
			warning("BSkyRemoveVarRow: Col with that name doesn't exists...")
		}
	}#if dataset index is valid	(ie.. not found)
	else
	{
		# cat("\nDataset not found!...\n")
		BSkyErrMsg =paste("BSkyRemoveVarRow: Dataset not found."," Dataset Name:", dataSetNameOrIndex)
		warning("BSkyRemoveVarRow: Dataset not found.")
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
			
			# cat("Error caught in BSkyRemoveVarRow \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		# cat("\nWARNING:: top level remove var row function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkyRemoveVarRow \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		# cat("Returning return structure from this top level remove var row function\n")
		invisible(BSkyReturnStructure())
}

#04Aug2016 Deletes multiple variable/columns of a dataset

BSkyRemoveMultipleVarRows <- function(delcolnames, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyRemoveMultipleVarRows: Error in Remove Var Row : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(delcolnames, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyRemoveMultipleVarRows: Warning in Remove Var Row : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(delcolnames, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{
	
		withCallingHandlers(
		{
	#M1 :- mydata[substr(rownames(mydata),1,1)!="U",] 

	#cat("\nFinding DataSet \n")		
			datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
			if(!is.null(datasetname))
			{	
			no.of.cols = length(delcolnames)
			delindexes=character(0)
			tempidx = character(0)
			for(i in 1 : no.of.cols)
			{
				colIndex <- BSkyValidateColumn(datasetname, delcolnames[i])	
				if(i==1)
				{
					delindexes <- paste(delindexes,'-',colIndex,sep='')# no comma before minus
					tempidx <- paste(tempidx,colIndex,sep='')## used in variable labels below
				}
				else
				{
					delindexes <- paste(delindexes,',-',colIndex,sep='')
					tempidx <- paste(tempidx,',',colIndex,sep='')
				}
			}
			delindexes <- paste('c(',delindexes,')',sep='')
			vec <- eval(parse(text=paste('c(',tempidx,')')))
			#cat('DelColIdxs')
			#print(delindexes)
			#cat('Vector')
			#print(vec)			
		rowindex <- colIndex
		#Old col will have index greater than 0
		if(colIndex > 0)#col found.
		{
			# cat("\nCol found...")
			# remove col from the dataset.
			# rowindex in vargrid is actually colindex in datagrid
			
			colcount <- eval(parse(text=paste('ncol(',datasetname,')'))) ## orginal col count before deleting a col
			#M2: removing by assigning to NULL
			#04Aug2016 eval(parse(text=paste(datasetname,'[',colIndex,'] <- NULL',sep='')))#. <<- to <-
			eval(parse(text=paste(datasetname,'<-', datasetname,'[',delindexes,']',sep='')))# easy way to remove col.
			
			## Also fix variable.labels 09Aug2013
			remainingLabels <- c()
			for(i in 1:colcount) 
			{
				if(!(i %in% vec)) ## except removed col labels, rest are saved and restored later
				{
					onelabel <- eval(parse(text=paste('attr(',datasetname,',"variable.labels")[',i,']',sep='')))
					# cat("\nOne Lbl:",onelabel)
					remainingLabels <- c(remainingLabels, onelabel)
				}
			}
			eval(parse(text=paste('attr(',datasetname,',"variable.labels") <- remainingLabels',sep='')))    ## restoring labels.#. <<- to <-
			# eval(parse(text=paste('attr(',datasetname,', "variable.labels")'))) #attr(Dataset1, "variable.labels")
			#M3: using cbind
			#numcol <- ncol(uadatasets$lst[[DataSetIndex]])
			#if(rowindex == 1) 
			#{
				#uadatasets$lst[[DataSetIndex]] <- cbind(uadatasets$lst[[DataSetIndex]][,2:numcol])	
			#}
			#else
 			#{
				#if(rowindex == numcol)
				#{
					#numcol = numcol -1
					#uadatasets$lst[[DataSetIndex]] <- cbind(uadatasets$lst[[DataSetIndex]][,1:numcol])
				#}
				#else
				#{
					#preindex <- rowindex - 1
					#nextindex <- rowindex + 1
					#uadatasets$lst[[DataSetIndex]] <- cbind(uadatasets$lst[[DataSetIndex]][,1:preindex],uadatasets$lst[[DataSetIndex]][,nextindex:numcol])
				#}
			#}
		}#col found
		else
		{
			# cat("\nCol with that name doesn't exists...\n")
			BSkyErrMsg =paste("BSkyRemoveMultipleVarRows: Col with that name doesn't exists..."," Col Name:", delcolnames)
			warning("BSkyRemoveMultipleVarRows: Col with that name doesn't exists...")
		}
	}#if dataset index is valid	(ie.. not found)
	else
	{
		# cat("\nDataset not found!...\n")
		BSkyErrMsg =paste("BSkyRemoveMultipleVarRows: Dataset not found."," Dataset Name:", dataSetNameOrIndex)
		warning("BSkyRemoveMultipleVarRows: Dataset not found.")
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
			
			# cat("Error caught in BSkyRemoveVarRow \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		# cat("\nWARNING:: top level remove var row function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkyRemoveVarRow \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		# cat("Returning return structure from this top level remove var row function\n")
		return(invisible(BSkyReturnStructure()))
}




#############################################################################################################
#Function: BSkyChangeLevels(delcolname, dataSetNameOrIndex)												
#              																								
#Parameters: 
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colNameOrIndex				- col name or index whose levels need to be changed
#		newLevels					- new values of levels for cols those are factor type
#																											
#Description:
#		Changes the levels of factor col
#																											
#Example: BSkyChangeLevels(colNameOrIndex=2, newLevels=c("A","B"), dataSetNameOrIndex="mydf")																								
#																											
#############################################################################################################
BSkyChangeLevels <- function(colNameOrIndex, oldLevels, newLevels, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyChangeLevels: Error in Change Levels : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(newLevels, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyChangeLevels: Warning in Change Levels : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(newLevels, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{
	
		withCallingHandlers(
		{
	#1 check if col is factor. It should be then only we can add levels to it
	#2 add new levels. levels(uadatasets$lst[[1]]$colname)<- c("l1","l2","l3")
	##cat("\nFinding DataSet \n")		
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	#if dataset index is valid
			if(!is.null(datasetname))
			{		
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)	
		#Error: dataSetName and colname not found
		if(colIndex > 0)
		{
			#oldlevelcount <- eval(parse(text=paste('length(levels(',datasetname,'[,colIndex]))', sep='')))
			#oldlevelcount <- eval(parse(text=paste('length(levels(',datasetname,'[[colIndex]]))', sep='')))
			
			#isfac <- eval(parse(text=paste('is.factor(',datasetname,'[,colIndex])',sep='' )))#does not work on CSV files properly
			isfac <- eval(parse(text=paste('is.factor(',datasetname,'[[colIndex]])',sep='' )))
			
			if(isfac)
			{
					#uadatasets$lst[[DataSetIndex]][,colIndex][ uadatasets$lst[[DataSetIndex]][,colIndex] == c("Male") ] <- NA
					#library(gdata)
					#drop.levels(uadatasets$lst[[DataSetIndex]][,colIndex])
					
					#levels(uadatasets$lst[[DataSetIndex]][,colIndex]) <- newLevels
					#uadatasets$lst[[1]][,2]<-factor(uadatasets$lst[[1]][,2], levels=c("Female", "EMale"), labels = c("Female", "Murd"))					
					bskyattlist <- BSkyAttributesBackup(colIndex, datasetname)
					#eval(parse(text=paste(datasetname,'[,colIndex] <<- factor((',datasetname,'[,colIndex]),oldLevels, newLevels)', sep='')))#. <<- to <-
					eval(parse(text=paste(datasetname,'[[colIndex]] <- factor((',datasetname,'[[colIndex]]),oldLevels, newLevels)', sep='')))
					
					#factor(uadatasets$lst[[2]][,1], levels=c("Under 21", "26-30",""), labels=c("Under age","26-30", "D"))## Change / Del / Add. All in one
					
					#cat('\nNew Levels: ')
					#cat(levels(uadatasets$lst[[DataSetIndex]][,colIndex]))
					BSkyAttributesRestore(colIndex, bskyattlist, datasetname)
					#cat('\nNew Levels: ')
					#cat(levels(uadatasets$lst[[DataSetIndex]][,colIndex]))
					#-return("")
			}
		}
		else
		{
			BSkyErrMsg =paste("BSkyChangeLevels: Col not found."," Col Name:", colNameOrIndex)
			warning("BSkyChangeLevels: Col not found.")
		}
	}
	else
	{
		BSkyErrMsg =paste("BSkyChangeLevels: Dataset not found."," Dataset Name:", dataSetNameOrIndex)
		warning("BSkyChangeLevels: Dataset not found.")
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
			
			# cat("Error caught in BSkyChangeLevels \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level change levels function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkyChangeLevels \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level change levels function\n")
		return(invisible(BSkyReturnStructure()))
}


BSkyAddLevels <- function(colNameOrIndex, newLevels, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyAddLevels: Error in Change Levels : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(newLevels, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyAddLevels: Warning in Change Levels : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(newLevels, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{
	
		withCallingHandlers(
		{
	#1 check if col is factor. It should be then only we can add levels to it
	#2 add new levels. levels(uadatasets$lst[[1]]$colname)<- c("l1","l2","l3")
	##cat("\nFinding DataSet \n")		
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	#if dataset index is valid
			if(!is.null(datasetname))
			{		
colIndex <- BSkyValidateColumn(datasetname, colNameOrIndex)	
		#Error: dataSetName and colname not found
		if(colIndex > 0)
		{
			oldlevelcount <- eval(parse(text=paste('length(levels(',datasetname,'[,colIndex]))', sep='')))
			isfac <- eval(parse(text=paste('is.factor(',datasetname,'[,colIndex])',sep='' )))
			if(isfac)
			{
					bskyattlist <- BSkyAttributesBackup(colIndex, datasetname)
					eval(parse(text=paste('levels(',datasetname,'[,colIndex]) <- c(levels(',datasetname,'[,colIndex]), newLevels)', sep='')))#. <<- to <-
					BSkyAttributesRestore(colIndex, bskyattlist, datasetname)
			}
		}
		else
		{
			BSkyErrMsg =paste("BSkyAddLevels: Col not found."," Col Name:", colNameOrIndex)
			warning("BSkyAddLevels: Col not found.")
		}
	}
	else
	{
		BSkyErrMsg =paste("BSkyAddLevels: Dataset not found."," Dataset Name:", dataSetNameOrIndex)
		warning("BSkyAddLevels: Dataset not found.")
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
			
			# cat("Error caught in BSkyAddLevels \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level change levels function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkyAddLevels \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level change levels function\n")
		return(invisible(BSkyReturnStructure()))
}

#############################################################################################################
#Function: BSkyChangeMissings(delcolname, newMissing, dataSetNameOrIndex)												
#              																								
#Parameters: 
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colNameOrIndex				- col name or index whose levels need to be changed
#		newMissing					- new values of levels for cols those are factor type
#																											
#Description:
#		Changes the levels of factor col
#																											
#Example: BSkyChangeMissings(colNameOrIndex=2, newLevels=c("A","B"), dataSetNameOrIndex="mydf")																								
#																											
#############################################################################################################
BSkyChangeMissings <- function()
{
		tryCatch(
		{
	
		withCallingHandlers(
		{

#############################

		},
		
		warning = UAwarnHandlerFn

		) # end of withCallingHandlers for catching warnings and continuing execution		
		},
		error = UAerrHandlerFn,
		
		silent =TRUE		
	)
}


BSkyIsDateValid <- function(stringDate, dateFormat="%Y-%m-%d %H:%M:%S", coltzone="")
{
	validStrDate = ""
	isDateValid=FALSE
	islongDate=FALSE
	msg = ""
	
	if(grepl(":", stringDate) )
	{
		islongDate = TRUE
	} 

	islongFromat=FALSE
	if(grepl(":", dateFormat) )
	{
		islongFromat = TRUE
	} 

	# dtfrmtmsg = "YYYY-MM-DD"
	# if(islongFromat)
	# {
		# dtfrmtmsg = "YYYY-MM-DD HH:MM:SS"
	# }
	dtfrmtmsg = dateFormat
	
	msg = paste("Error:Invalid date entered: Use correct date format ", dtfrmtmsg, " and enter correct date", sep='')

	if(islongDate && islongFromat) #check if long date matches to long format for valid date
	{
		suppressWarnings({
			stdt <- strptime(stringDate,format=dateFormat, tz=coltzone)
			posdt <- as.POSIXct(stdt, tz=coltzone)
			if(!is.na(posdt))
			{
				isDateValid = TRUE
				validStrDate = stringDate
			}
		})
	}
	if(!islongDate && islongFromat) #cshort date with long format mean user remove time part of the date
	{
		suppressWarnings({
			stdt <- strptime(stringDate,format="%Y-%m-%d", tz=coltzone)
			posdt <- as.POSIXct(stdt, tz=coltzone)
			if(!is.na(posdt))
			{
				isDateValid = TRUE
				if(isDateValid) ## date is valid but the user removed the time part so add time 00:00:00
				{
					validStrDate = paste(stringDate," 00:00:00", sep='')
				}				
			}

		})
	}	
	else if (!islongDate && !islongFromat) #short date and short date format. Check valid date
	{
		suppressWarnings({
			stdt <- strptime(stringDate,format=dateFormat, tz=coltzone)
			posdt <- as.POSIXct(stdt, tz=coltzone)
			if(!is.na(posdt))
			{
				isDateValid = TRUE
				validStrDate = stringDate
			} 
		})
	}

	if(!isDateValid)
	{
		print(msg)
	}
	return(validStrDate)
}