######## list of methods in this file; in sequence (few may not be ready and unused ) ###########
# UAreadExcel
# UAwriteExcel
#################################################################################################


###################################################################################################################
#Function: UAreadExcel(excelfilename, datasetname, sheetname, replace=FALSE, xlsx=FALSE)								
#              																			
#Parameters:  
#		excelfilename			- full path filename of Excel(.xls or .xlsx) file on disk. Also set xlsx flag.
#		datasetname     		- new refrence name for later use   
#		sheetname				- Existing wroksheet name thats in the excel which we want to load  
#		replace 				- TRUE if you want to overwrite exisiting dataset in UA memory space     	
#		xlsx					- TRUE if the file is .xlsx file (MS Excel2007) format. FALSE for .xls format
#																						
#Description:	
#		Reads .xls or .xlsx file from disk and loads in UA memory space. You can refer this dataset later 
#		with provided datasetname
#																						
#Example: UAreadExcel("C:/Data/test.xls", "myxls", "Sheet1", replace=FALSE, xlsx=FALSE)																			
###################################################################################################################
UAreadExcel <- function(excelfilename, datasetname, sheetname, replace=FALSE, xlsx=FALSE, character.to.factor=FALSE, colNames=TRUE)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetname)
	
	BSkyErrMsg = paste("UAreadExcel: Error reading Excel file : ", "DataSetName :", datasetname," ", "Excel filename  :", paste(excelfilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAreadExcel: Warning reading Excel file : ", "DataSetName :", datasetname," ", "Excel filename :", paste(excelfilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success=-1
	##loading the dbf file from disk to uadatasets array
	# New Dataset name is only added if you want to replace existing, Or you will check that it should not already present
	curidx <- UAgetIndexOfDataSet(datasetname) # current index of dataset if its already loaded ( before )
	#cat("\nCur Index of Excel:",curidx)
	if((replace == TRUE) || (replace == FALSE && curidx == -1))
	{
		# if i>0 then datasetname already exists && you want to replace it.
		if(replace == TRUE && curidx > 0)
		{
			# Delete the existing dataset from the global list first. 
			# if dataset already exists, its index is in i.
			# uadatasets$lst[i]<-NULL
			#UAcloseDataset(datasetname)
			BSkyCloseForReloading(datasetname)
			#cat("DS Closed")			
		}

		######### Using RODBC package ########starts###
		# #library(RODBC)
		# channel<-NULL
		# if(xlsx)
		# {
			# channel <- odbcConnectExcel2007(excelfilename, readOnly=FALSE)
		# }
		# else
		# {
			# channel <- odbcConnectExcel(excelfilename, readOnly=FALSE)
		# }
		# # uadatasets$lst[[DataSetIndex]] <- sqlFetch(channel, sheetname)
						
		# # Now add new dataset.
		# # uadatasets$lst <- c(uadatasets$lst, list(sqlFetch(channel, sheetname)))
		# #eval( parse(text=paste(datasetname,' <<- sqlFetch(channel, \'',sheetname,'\')', sep=''  )))
		# eval( parse(text=paste(datasetname,' <<- sqlQuery(channel, \'select * from [',sheetname,'$]\')', sep=''  )))
		# odbcCloseAll()
		######### Using RODBC package ########ends####
		
		#R command to open data file
		corecommand = paste('readxl::read_excel(path=\'',excelfilename,'\',sheet=\'',sheetname,'\', col_names=',colNames,')', sep='')

		#reset global error-warning flag
		eval(parse(text="bsky_opencommand_execution_an_exception_occured = FALSE"), envir=globalenv())
		
		# eval( parse(text=paste('bskytempx <<- NULL',sep='')))## 03Aug2016 Clean x
		
		#trying to open the datafile
		tryCatch({		
			withCallingHandlers({		
				# eval( parse(text=paste('bskytempx <<- as.data.frame(',corecommand,')', sep=''  )))

				#03Aug2016 change "character" cols to "factor" ## This take more time to load in grid. 
				#Dataset(with many character cols loan.csv) which was taking 27sec earlier, took 1min:45sec to load when "character" changed to "factor"
				if(character.to.factor) 
				{
					eval( parse(text=paste('bskytempx <<- NULL',sep='')))## 03Aug2016 Clean x
					######### Using readxl package ########		datasetname <- read_excel(path, sheet = 1)
					eval( parse(text=paste('bskytempx <<- as.data.frame(',corecommand,')', sep=''  ))) # use with lapply

					#generate column names if colNames is FALSE
					if(!colNames)
					{
						colscount = eval(parse(text=paste('ncol(bskytempx)', sep='' ))) 
						newcolnames = paste("X", c(1:colscount), sep='')
						eval(parse(text=paste('names(bskytempx) = newcolnames', sep='' )))
					}
					# cat("\nNo global:\n")
					# eval(parse(text=paste('objexists <- exists("bskytempx", envir=.GlobalEnv)',sep=''))) #dataset object exists
					# print(objexists)

					# cat("\nGlobal:\n")
					# eval(parse(text=paste('objexists <- exists(".GlobalEnv$bskytempx")',sep=''))) #dataset object exists
					# print(objexists)					
					#at this point we need to assign some column name(Var1, Var2) to the column those do not have any col-names.
					GenerateUniqueColName('bskytempx')

					##27May2022 it is found that if there is a numeric col that contains numbers as characters (e.e '24') then  this col will be 
					## loaded as a factor col in the grid. If we make this col numeric (from the grid) the actual data is replace by factor levels
					## 1,2,3 etc. To fix this issue we must first conver the character-numbers ('24') to numbers and then load this in the grid. Now
					## since numeric col contains actual numbers, they should not be converted to factors.
					#mtcars[] = lapply(mtcars, function(x) type.convert(as.character(x), as.is = TRUE))
					
					#eval(parse(text=paste('bskytempx <<- lapply(bskytempx, function(x) type.convert(x, as.is=TRUE))',sep='')))
					##15Jun2022 Line above was converting dates to character which in second eval were turning into factor

					eval(parse(text=paste('bskytempx <<- lapply(bskytempx, function(x){if(!("POSIXct" %in% class(x)) && !("Date" %in% class(x)) && !("POSIXt" %in% class(x))) type.convert(x, as.is=TRUE) else x})',sep='')))

					#following line : empty col name replaced by something like C..NA..NA..NA
					eval(parse(text=paste('.GlobalEnv$',datasetname,' <- as.data.frame( lapply (bskytempx, function(y) {if("character" %in% class(y)) y = factor(y) else y} ) )',sep='' )))
				}
				else
				{
					eval( parse(text=paste('.GlobalEnv$',datasetname,' <- as.data.frame( ',corecommand,')', sep=''  ))) 

					#generate column names if colNames is FALSE
					if(!colNames)
					{
						colscount = eval(parse(text=paste('ncol(.GlobalEnv$',datasetname,')', sep='' ))) 
						newcolnames = paste("X", c(1:colscount), sep='')
						eval(parse(text=paste('names(.GlobalEnv$',datasetname,') = newcolnames', sep='' )))
					}					

					#at this point we need to assign some column name(Var1, Var2) to the column those do not have any col-names.
					GenerateUniqueColName(datasetname)		
				}						
				
			}, warning = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
		}, error = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)		
		
		if(bsky_opencommand_execution_an_exception_occured == FALSE)## Success
		{
			colscount = eval(parse(text=paste('ncol(.GlobalEnv$',datasetname,')', sep='' ))) 
			if(colscount == 0)## Success in open but no the sheet is mepty
			{
				cat("\nError: Empty sheet.\n") 
				print(corecommand) #no need to print this
				eval(parse(text=paste('.GlobalEnv$',datasetname,'=NULL', sep='' ))) #cleanup
				# eval(parse(text="bsky_opencommand_execution_an_exception_occured = TRUE"), envir=globalenv())
				success = -1;
			}
			else
			{
				success = 0
				## maybe return 0 for success
				cat("\nSuccessfully opened using:\n") 
				print(corecommand) #no need to print this
			}

		}
		else ## Failure
		{
			cat("\nError: Can't open file\n") 
			# cat("\n\nCommand executed:\n")
			print(corecommand)
			## gracefully report error to the app layer about the issue so it does not keep waiting. 
			## maybe return -1 for failure
			success = -1;
		}	
		##03Aug2016 following block was used earlier. Now the if-else above is used. 
		## FALSE in 'if' below is added to not execute that code.
		## Ealier, the following block was with 'if' and without curly brackets
		if(FALSE)
		{
			eval( parse(text=paste('x <<- NULL',sep='')))## 03Aug2016 Clean x
			######### Using readxl package ########		datasetname <- read_excel(path, sheet = 1)
			#eval( parse(text=paste(datasetname,' <<- as.data.frame( read_excel(path=\'',excelfilename,'\',sheet=\'',sheetname,'\'))', sep=''  )))# Working
			###A###require(openxlsx)
			###A###eval( parse(text=paste(datasetname,' <<- as.data.frame( read.xlsx(xlsxFile=\'',excelfilename,'\'))', sep=''  ))) # using openxlsx
			
			eval( parse(text=paste('x <<- as.data.frame( read_excel(path=\'',excelfilename,'\',sheet=\'',sheetname,'\'))', sep=''  )))


			#at this point we need to assign some column name(Var1, Var2) to the column those do not have any col-names.
			GenerateUniqueColName('x')
			
			#following line : empty col name replaced by something like C..NA..NA..NA
			eval(parse(text=paste(datasetname,' <<- as.data.frame( lapply (x, function(y) {if("character" %in% class(y)) y = factor(y) else y} ) )' )))
			#as.data.frame( lapply (x, function(y) {if(class(y) == "character") y = factor(y) else y} ) )
		}
		
		if(success==0)
		{
			## 03Sep2016 We had to put following code because hadley's was not handling the special characters
			#Now replace special chars with underscore and prefix 'X' if there is digit in the first index  of colum name.
			colcount <- eval(parse(text=paste('length(names(.GlobalEnv$',datasetname,'))', sep='' ))) #colcount = length(names(datasetname))
			#cat('Count=')
			#cat(colcount)
			#ptm= proc.time()
			for( i in 1:colcount)
			{
				eval(parse(text=paste('names(','.GlobalEnv$',datasetname,')[',i,']  <- ReplaceSplChrsAndPrefixXForDigitInBegining(names(.GlobalEnv$',datasetname,')[',i,'])', sep='')))
			}
			
			# set the name (Which is passed as an input parameter to this function)
			# to the newly created data frame within the global list
			# names(uadatasets$lst)[length(uadatasets$lst)]<-datasetname
			uadatasets$name <- c(uadatasets$name, datasetname)
			#cat("\nLoaded Excel dataset :", datasetname)					
			
			# if(replace == FALSE)
			# {						
				# #for old code compatibility put same list in 'name' also
				# uadatasets$name <- c(uadatasets$name, datasetname)
			# }	
						
			# DataSetIndex <- UAgetIndexOfDataSet(datasetname)
			ftype = "XLS"
			if(xlsx)
			{
				ftype = "XLSX"
			}


			#Creating extra attributes at column level
			UAcreateExtraAttributes(datasetname, ftype)		
					#cat("\nFixing UTC")
					#MakeAllDateColUTC(datasetname)
					#cat("\nUTC Fixed")		
		}
	}
	else
	{
		BSkyErrMsg =paste("UAreadExcel: Dataset with the same name already on the global list."," Dataset Name:", datasetname)
		warning("UAreadExcel: Dataset with the same name already on the global list ")
	}				
	BSkyFunctionWrapUp()
	return(success)
}

###################################################################################################################
#Function: UAwriteExcel(excelfilename, dataSetNameOrIndex, sheetname, xlsx=FALSE)								
#              																			
#Parameters:  
#		excelfilename			- full path filename of new .xls or .xlsx file to which particular dataset element 
#								  of uadatasets$lst is to be written. Also set xlsx flag accordingly.
#		dataSetNameOrIndex     	- Dataset Name or Index (of uadatasets$lst array) which is to be 
#								  written to disk file as new Excel file.    
#		sheetname				- New wroksheet name to which you want to write the dataset from UA memory space (sheet name = table name )
#		row.names				- 
#		col.names				-
#		xlsx 					- TRUE if the file is .xlsx file (MS Excel2007) format. FALSE for .xls format    																
#																						
#Description:	
#		Write UA memory space dataset to a .xls or .xlsx file format
#																						
#Example: UAwriteExcel("C:/Data/new.xls", "dietstudy", "FirstSheet", xlsx=FALSE)																		
###################################################################################################################
UAwriteExcel <- function(excelfilename, dataSetNameOrIndex, sheetname, row.names = TRUE, col.names = TRUE, xlsx=FALSE)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAwriteExcel: Error writing Excel file : ", "DataSetName :", dataSetNameOrIndex," ", "Excel filename  :", paste(excelfilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAwriteExcel: Warning writing Excel file : ", "DataSetName :", dataSetNameOrIndex," ", "Excel filename :", paste(excelfilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success=-1
	datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

	if(!is.null(datasetname))
	{		
			
		#The openxlsx package provides a write option and it also uses Rcpp to work 
		#so it is much much faster than the xlsx package.
		# library(RODBC)
		# channel<-NULL
		# if(xlsx)
		# {
			# channel <- odbcConnectExcel2007(excelfilename, readOnly=FALSE)
		# }
		# else
		# {
			# channel <- odbcConnectExcel(excelfilename, readOnly=FALSE)
		# }			

		# #channel is a handle that points to excel file
		# # sqlSave(channel, uadatasets$lst[[1]],tablename="Sheet1")
		# eval(parse(text=paste('sqlSave(channel, ',datasetname,', tablename=sheetname, rownames = row.names, colnames = col.names)')))
		# odbcCloseAll()
					
		#a. openxlsx : looks good choice so far
		#b. xlsx : documentation says :"per-formance for very large data.frame may be an issue"
		#c. gdata : reads Excel but saves as "fixed width format"
		#d. XLConnect : reads Excel: load workbook > createsheet > writeWorksheet >saveWorkBook (looks like does not saves data.frame but only workbook)				

		#using openxlsx
		require(openxlsx)
		## formatting
		#options("openxlsx.borderColour" = "#4F80BD") ## set default border colour
		#write.xlsx(iris, file = "writeXLSX1.xlsx", colNames = TRUE, borders = "columns")
		#write.xlsx(iris, file = "writeXLSX2.xlsx", colNames = TRUE, borders = "surrounding")
		#hs <- createStyle(textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize=12,fontName="Arial Narrow", fgFill = "#4F80BD")
		#write.xlsx(iris, file = "writeXLSX3.xlsx", colNames = TRUE, borders = "rows", headerStyle = hs)
		
		#Works well but No SheetName. Was in use before 07Jul2016
		#eval(parse(text=paste('write.xlsx(',datasetname,', file="',excelfilename,'", colNames=TRUE)',sep='')))
		#With SheetName
		corecommand = paste('openxlsx::write.xlsx(',datasetname,', file="',excelfilename,'", colNames=TRUE, sheetName="',sheetname,'", overwrite=TRUE)', sep='')
		#reset global error-warning flag
		eval(parse(text="bsky_opencommand_execution_an_exception_occured = FALSE"), envir=globalenv())		
		#trying to save the datafile
		tryCatch({
		
				withCallingHandlers({
					eval(parse(text=corecommand))
				}, warning = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
				}, error = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
		
		if(bsky_opencommand_execution_an_exception_occured == FALSE)## Success
		{
			success = 0
			## maybe return 0 for success
			# cat("\nSuccessfully saved\n") 
			# print(corecommand) #no need to print this
		}
		else ## Failure
		{
			cat("\nError: Can't save file\n") 
			# cat("\n\nCommand executed:\n")
			print(corecommand)
			## gracefully report error to the app layer about the issue so it does not keep waiting. 
			## maybe return -1 for failure
			success = -1;
		}
		## following was in use before enclosing it in tryCatch above
		#eval(parse(text=paste('write.xlsx(',datasetname,', file="',excelfilename,'", colNames=TRUE, sheetName="',sheetname,'", overwrite=TRUE)',sep='')))
	
	}
	else
	{
		# cat("Error: Cannot write Excel. Dataset name or index not found")
		BSkyErrMsg =paste("UAwriteExcel: Cannot write Excel. Dataset name or index not found."," Dataset Name:", dataSetNameOrIndex)
		warning("UAwriteExcel: Cannot write Excel. Dataset name or index not found.")
	}			
	BSkyFunctionWrapUp()
	return(success)
}

GetTableList <- function(excelfilename, xlsx=FALSE)
{
	BSkyFunctionInit()
	#BSkySetCurrentDatasetName(datasetname)
	
	BSkyErrMsg = paste("GetTableList: Error reading table list : ",sep="")
	BSkyWarnMsg = paste("GetTableList: Warning reading table list : ",sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	# success=-1
	# tablelist = NULL
					
	#reset global error-warning flag
	# eval(parse(text="bsky_opencommand_execution_an_exception_occured = FALSE"), envir=globalenv())	
	
	#trying to open the datafile
	# tryCatch({		
		# withCallingHandlers({					
				tablelist <- excel_sheets(excelfilename)
		# }, warning = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
	# }, error = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)	
	
	# if(bsky_opencommand_execution_an_exception_occured == FALSE)## Success
	# {
		# success = 0
		# cat("\nSuccessfully fetched sheets list:\n") 
		# print(corecommand) #no need to print this
	# }
	# else ## Failure
	# {
		# cat("\nError: Can't fetch worksheets\n") 
		# print(corecommand)
		# success = -1;
	# }				
	BSkyFunctionWrapUp()
	# invisible(tablelist$TABLE_NAME)
	invisible(tablelist)
}



# ##################### RODBC help #################

# channel2 <- odbcDriverConnect(paste("DRIVER=Microsoft Excel Driver (*.xls)",
									# "DBQ=F:/SPSS/dietstudy.xls",
									# "ReadOnly=False", 
									# sep = ";")
							# )
							
# channel2 <- odbcDriverConnect(paste("DRIVER=Microsoft Excel Driver (*.xls *.xlsx, *.xlsm, *.xlsb)",
									# "DBQ=F:/SPSS/dietstudy.xls",
									# "ReadOnly=False", 
									# sep = ";")
							# )							
							
# channel2 <- odbcDriverConnect(paste("DRIVER=Microsoft Excel Driver (*.xls *.xlsx, *.xlsm, *.xlsb)"))	


# library(RODBC)
# channel <- odbcConnectExcel2007("F:/SPSS/xl2010.xlsx", readOnly=FALSE)
# sqlTables(channel)
# sheet1<-sqlFetch(channel, "Sheet1")
# sheet1$Age[5]<-200
# sqlSave(channel,sheet1, "My")
# odbcCloseAll()

# ################################# MS Excel Help ###############
# 1.If you are the user of an application, consult your application documentation for details on how 
# to use the appropriate driver.
# 2.If you are an application developer using OLEDB, set the Provider argument of the ConnectionString
# property to 'Microsoft.ACE.OLEDB.12.0'
# If you are connecting to Microsoft Office Excel data, add Excel 12.0 to the Extended Properties
 # of the OLEDB connection string.
# 3.If you are application developer using ODBC to connect to Microsoft Office Access data, 
# set the Connection String to Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=path to mdb/accdb file
# 4.If you are application developer using ODBC to connect to Microsoft Office Excel data, 
# set the Connection String to Driver={Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb)};DBQ=path to xls/xlsx/xlsm/xlsb file						
