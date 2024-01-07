#UI layer will call functions from this file. And the subfunctions are called from these functions

######## list of methods in this file; in sequence (few may not be ready and unused ) ###########
# BSkyOpenNewDataset
# BSkyloadDataset
# BSkyReloadDataset
# BSkysaveDataset
# BSkycloseDataset
# BSkyCloseForReloading
# UAgetAllData (not complete)
# UAgetColProp
# UAsetColProp
# BSkygetIndexesOfCols
# UAgetDataByIndex (not complete)
# UAgetMissing
# UAsetMissing
# BSkyGetDatasetSplitInfo
# UAgetDataframeSplit
# BSkySetDataFrameSplit
# UAsetDataframeSplit
# UAremoveDataframeSplit
# BSkyOutputTestDriver
#################################################################################################

# BSkyOpenNewDataset('ds1')
#' Creates a blank dataset.
#'
#' @param datasetName is the dataset object name which points to the blank dataset that got created.
#' @export 
#' @examples
#' BSkyOpenNewDataset( 'mydata' ) # create new blank datagrid. Does not loads it in UI datagrid.
#' BSkyLoadRefreshDataframe(mydata) # to load the new dataset in the BlueSky Statistics UI datagrid
BSkyOpenNewDataset <-function(datasetName ,noOfRows=30,noOfCols=6) # rows=80 and cols=50 for electron
{
BSkyFunctionInit()
BSkySetCurrentDatasetName(datasetName)
BSkyErrMsg = paste("BSkyOpenNewDataset: Error in Loading empty dataset : ", "DataSetName :", datasetName," ", sep="")
BSkyWarnMsg = paste("BSkyOpenNewDataset: Warning in Loading empty dataset : ", "DataSetName :", datasetName," ", sep="")
BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
datasetname <- datasetName
tryCatch(
{
withCallingHandlers(
{
#find extension
#extn = File.sav
if(TRUE)##filetype=="SPSS")
{
## Old code.
# var1 <- c(NA, NA)
# var2 <- c(NA, NA)
# var3 <- c(NA, NA)
# var4 <- c(NA, NA)
# var5 <- c(NA, NA)
# var6 <- c(NA, NA)

## putting NaN here shows NA in the grid but they are not NA. So converting any var to factor gives us a factor level NaN.
# var1 <- c(rep(NaN,15 )) ##c(NA, NA)
# var2 <- c(rep(NaN,15 )) ##c(NA, NA)
# var3 <- c(rep(NaN,15 )) ##c(NA, NA)
# var4 <- c(rep(NaN,15 )) ##c(NA, NA)
# var5 <- c(rep(NaN,15 )) ##c(NA, NA)
# var6 <- c(rep(NaN,15 )) ##c(NA, NA)


   ## putting 0 here shows 0 in the datagrid. They are of class numeric.
# var1 <- c(rep(0,15 ))
# var2 <- c(rep(0,15 ))
# var3 <- c(rep(0,15 ))
# var4 <- c(rep(0,15 ))
# var5 <- c(rep(0,15 ))
# var6 <- c(rep(0,15 ))
   ## putting 0 here shows 0 in the datagrid. They are of class numeric.
# var1 <- c(rep(NA,15 ))
# var2 <- c(rep(NA,15 ))
# var3 <- c(rep(NA,15 ))
# var4 <- c(rep(NA,15 ))
# var5 <- c(rep(NA,15 ))
# var6 <- c(rep(NA,15 ))

   ## 23Sep2019 Making all cells blank. Blanks turn into NA
# var1 <- c(rep("",30 ))
# var2 <- c(rep("",30 ))
# var3 <- c(rep("",30 ))
# var4 <- c(rep("",30 ))
# var5 <- c(rep("",30 ))
# var6 <- c(rep("",30 ))
## 23Sep2019 Making all cells blank. Blanks turn into NA
# DF = data.frame(A=c("","",""), B=c("","",""))

## putting NA makes vars logical and datagrid shows them as 'True'
# var1 <- c(NA, NA,NA, NA,NA, NA,NA, NA)
# var2 <- c(NA, NA,NA, NA,NA, NA,NA, NA)
# var3 <- c(NA, NA,NA, NA,NA, NA,NA, NA)
# var4 <- c(NA, NA,NA, NA,NA, NA,NA, NA)
# var5 <- c(NA, NA,NA, NA,NA, NA,NA, NA)
# var6 <- c(NA, NA,NA, NA,NA, NA,NA, NA)

## putting first value zero (and rest NAs) then the whole col
# becomes numeric and shows the same in the datagrid
# var1 <- c(0, NA,NA, NA,NA, NA,NA, NA)
# var2 <- c(0, NA,NA, NA,NA, NA,NA, NA)
# var3 <- c(0, NA,NA, NA,NA, NA,NA, NA)
# var4 <- c(0, NA,NA, NA,NA, NA,NA, NA)
# var5 <- c(0, NA,NA, NA,NA, NA,NA, NA)
# var6 <- c(0, NA,NA, NA,NA, NA,NA, NA)
#df <- data.frame(a,b)
#eval( parse(text=paste(datasetname,' <<- data.frame(var1=as.character(var1),var2=as.character(var2), var3=as.character(var3), var4=as.character(var4), var5=as.character(var5), var6=as.character(var6), stringsAsFactors =FALSE)',sep='')))
# eval( parse(text=paste(datasetname,' <<- data.frame(var1=as.numeric(var1),var2=as.numeric(var2), var3=as.numeric(var3), var4=as.numeric(var4), var5=as.numeric(var5), var6=as.numeric(var6), stringsAsFactors =FALSE)',sep='')))
#eval( parse(text=paste('class(',datasetname,'$a) <<- \'character\'',sep='')))
#eval( parse(text=paste('class(',datasetname,'$b) <<-  \'character\'',sep='')))
eval(parse(text =paste (datasetName, "<<-data.frame(matrix(data = ", "rep (''," , "noOfRows*noOfCols), ncol =", noOfCols , ",nrow =", noOfRows, "),stringsAsFactors = FALSE)", sep="",collapse="")))
uadatasets$name <- c(uadatasets$name, datasetname)
}
varlbls <- c(var1="var1", var2="var2", var3="var3", var4="var4", var5="var5", var6="var6")
#cat("\nLoaded dataset :", datasetname)
DataSetIndex <- UAgetIndexOfDataSet(datasetname)
#cat("\nDatasetIndex =", DataSetIndex)
#eval(parse(text=paste('attr(',datasetname,',"variable.labels") <<- varlbls')))
#cat("\nVar lables set")
eval(parse(text=paste('attr(',datasetname,',"split") <<- c(FALSE)')))
#cat("\nsplit set.\t")
eval(parse(text=paste('attr(',datasetname,',"splitcolumnnames") <<- c("")')))
#cat("splitcolumnnames set.\t")
eval(parse(text=paste('attr(',datasetname,',"splitcolumnindex") <<- c("")')))
#cat("splitcolumnindex set.\t")
## Add 2 more attribute "filetype" and "slice" (differentiate slice from spss dataset)
#eval(parse(text=paste('attr(',datasetname,',"filetype") <<- c("NEW")')))
#cat("filetype set to NEW.\t")
eval(parse(text=paste('attr(',datasetname,',"slice") <<- c(FALSE)')))
#cat("slice set.\t")

### New Attribute for missings  at Dataset Level###
eval(parse(text=paste('attr(',datasetname,',"misvals") <<- NULL')))

###11Jul2022  New Attribute to know if dataset needs precessing using type.convert or not. Blank dataset that we create will always need precessing ###
eval(parse(text=paste('attr(',datasetname,',"processDS") <<- TRUE')))

# cat("misval set.\t")
if(TRUE) ##is.null(missingval) )##no missing values. then set all col to "none"
{
# cat("\ncreating Missing attr.")
colcount = eval(parse(text=paste('ncol(',datasetname,')')))
for(i in 1:colcount)
{
coluname = eval(parse(text=paste('colnames(',datasetname,')[',i,']')))
###creating missing value attribute, which is dataset level att.
colmisatt <- eval(parse(text=paste(coluname,'<-list(',coluname,'=list(type="none", value=""))')))
# print(colmisatt)
if(i>1)
eval(parse(text=paste('attr(',datasetname,',"misvals") <<- c(attr(',datasetname,',"misvals"), ',colmisatt,')')))
else
eval(parse(text=paste('attr(',datasetname,',"misvals") <<- c(',colmisatt,')')))
# cat("done!@")
}
}
#cat("\nCreating Extra attributes for new DS. ")
eval(parse(text=paste('attr(',datasetname,',"maxfactor") <<-', bskymaxfactors)))
UAcreateExtraAttributes(datasetname, "RDATA")
########## Following should not execute if any kind of error occur in code above #######
### Dataset already in place, now find index and do following ###
DataSetIndex <- UAgetIndexOfDataSet(datasetname)
####uadatasets$fullpath[DataSetIndex] <- fullpathfilename
#cat("\nlist of DS :")
#cat(uadatasets$name)

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
#cat("\nlist of DS finally:")
#cat(uadatasets$name)
#print(BSkyReturnStructure())
# cat("Returning return structure from this top level load dataset function\n")
return(invisible(BSkyReturnStructure()))
}


BSkyEmpty <-function(datasetName ,noOfRows,noOfCols)
{
 eval(parse(text =paste (datasetName, "<<-data.frame(matrix(data = ", "rep (''," , "noOfRows*noOfCols), ncol =", noOfCols , ",nrow =", noOfRows, "),stringsAsFactors = FALSE)", sep="",collapse=""))) 
}

###################################################################################################################
# Detail : This function can be used to load diff file formats in UA memory space
# fullpathfilename -> Full path filename(drive directory and filename) of an existing file to load the UA memory space
# datasetName -> any Name that you want as a refrence for this dataset
# worksheetName -> provide this parameter only if Excel file is to be read.
# replace_ds   -> Pass TRUE if you want to overwrite the exisitng dataset in UA memory space
# csvHeader    -> Should be TRUE if the first line of CSV file contains column name and not data.
# filetype   -> Diff file formats. Currently supports:- SPSS, XLS, XLSX, CSV, DBF, RDATA [This is case sensitive
# 				right now, provide exactly what the current support list shows]
###################################################################################################################
#' @title Load dataset
#'
#' @description 
#' loads a dataset into R by reading data from a disk file. When this function is executed from BlueSky R Command Editor
#' it loads the dataset in the UI datagrid.
#' [supported file types : .SAV, .XLSX, .XLS, .CSV, .SAS7BDAT, .DTA, .RDATA, .DBF, and .DAT]
#'
#' @name BSkyloadDataset
#' @param fullpathfilename is a full path filename of a disk file, that is to be loaded into R. Use forward slash as a path separator.
#' @param filetype is one of the ("SPSS", "SAS7BDAT", "DTA", "XLS", "XLSX", "CSV", "DBF", "RDATA" and "DAT").
#' @param worksheetName is the name of the worksheet. Used only when reading Excel file type. 
#' @param datasetName is a name of the object in R that corresponds to the dataset you have loaded. 
#'
#' Parameters replace_ds, load.missing, csvHeader are for internal use.
#' @export 
#' @examples
#' fullpathfilename <- 'C:/BlueSky Statistics/Sample Datasets/Sample R Datasets(.rdata)/caranalysis.RData'
#' BSkyloadDataset(fullpathfilename=fullpathfilename, filetype <- 'RDATA', datasetName <- 'rdataset')
#' BSkyLoadRefresh(rdataset)
#'
#' fullpathfilename <- 'C:/BlueSky Statistics/Sample Datasets/Excel/sample.xls'
#' BSkyloadDataset(fullpathfilename=fullpathfilename, filetype <- 'XLS', worksheetName = 'Sheet1', datasetName <- 'exceldata')
#' BSkyLoadRefresh(exceldata)
BSkyloadDataset <-function(fullpathfilename,  filetype, worksheetName=NULL, replace_ds=FALSE, 
load.missing = FALSE, csvHeader=TRUE,character.to.factor=FALSE, isBasketData=FALSE, trimSPSStrailing=FALSE, sepChar=',', deciChar='.', datasetName, encoding=NULL, maxFactor=BSkyGetMaxFactor(), skip = 0 )
{

	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetName)
	
	BSkyErrMsg = paste("BSkyloadDataset: Error in Loading dataset : ", "DataSetName :", datasetName," ", "Filepath :", paste(fullpathfilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyloadDataset: Warning in Loading dataset : ", "DataSetName :", datasetName," ", "Filepath :", paste(fullpathfilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	datasetname <- datasetName
	success=-1
	eval(parse(text="bsky_opencommand_execution_an_exception_occured = FALSE"), envir=globalenv())
	# # # tryCatch(
		# # # {
		# # # withCallingHandlers(
		# # # {
			#cat("Top Level - Start Loading Dataset:",datasetname)
			#print(Sys.time())
			#find extension
			#extn = File.sav
			if(filetype=="SPSS"){
				success = UAloadSPSSinDataFrame(fullpathfilename, datasetname, replace=replace_ds, loadMissingValue=load.missing, trimSPSStrailing=trimSPSStrailing, encoding=encoding) 
			}
			if(filetype=="SAS7BDAT"){
				success = BSkyLoadSASinDataFrame(fullpathfilename, datasetname, replace=replace_ds) 
			}	
			if(filetype=="DTA"){
				success = BSkyReadStata(fullpathfilename, datasetname, replace=replace_ds) 
			}			
			else if(filetype == "XLS"){
				success = UAreadExcel(fullpathfilename, datasetname, worksheetName, replace=replace_ds, xlsx=FALSE, character.to.factor=character.to.factor, colNames=csvHeader, skip = skip)
			}
			else if(filetype == "XLSX"){
				success = UAreadExcel(fullpathfilename, datasetname, worksheetName, replace=replace_ds, xlsx=TRUE, character.to.factor=character.to.factor, colNames=csvHeader, skip = skip)
			}			
			else if(isBasketData && (filetype == "CSV" || filetype == "DAT" || filetype == "TXT") ) #if its mkt basket data in a flat file having extension csv, txt or data
			{
					# For Basket data (transaction in one line) some separator will be used
					# but to load that data we must ignore that separator if we want to read
					# each line in a separate row of a single column(say Items)
					# User data file can contain a separator from a vast range of characters 
					# but not the vertical tab character. We want our sepChar to not match to any
					# separator available to user. Appropriate candidate is vertical tab.
					sepChar='\v' #vertical tab should never be found as a separator in user data file.
				success = BSkyLoadDATinDataFrame(fullpathfilename, datasetname, replace=replace_ds, Header=csvHeader, sepCh=sepChar, deciCh=deciChar)			
			}
			else if(filetype == "CSV" || filetype == "TXT"){

				success = UAreadCSV(fullpathfilename, datasetname, Header=csvHeader, replace=replace_ds, character.to.factor=character.to.factor, sepCh=sepChar, deciCh=deciChar)
			}
			else if(filetype == "DBF"){
				success = UAreadDBF(fullpathfilename, datasetname,replace=replace_ds)
			}
			else if(filetype == "RDATA" || filetype == "RDA"){ ##this block only runs when BSkyReloadDataset() is run on RData with single dataframe object.
			## so load.refresh is needed below.

				#31May2018 UAreadRObj(fullpathfilename, datasetname, replace=replace_ds)
				#BSky.GetDataframeObjNames(RObjFilename=fullpathfilename, ReturnObjectName='BSkyCurrentRObj')
				#BSky.LoadRefresh.Dataframe(dframe)
				
				#19Oct2019 above 2 line does not seem to work so code below should just work fine for single dataset RDATA file
				success = eval(parse(text=paste('UAreadRObj(RObjfileName="',fullpathfilename,'", datasetname="',datasetname,'", replace=TRUE)',sep='')))
			}			
			else if(filetype=="DAT"){
				# success = BSkyLoadDATinDataFrame(fullpathfilename, datasetname, replace=replace_ds, Header=csvHeader, sepCh=sepChar, deciCh=deciChar) 
				success = UAreadCSV(fullpathfilename, datasetname, Header=csvHeader, replace=replace_ds, character.to.factor=character.to.factor, sepCh=sepChar, deciCh=deciChar)				
			}					
			else if(filetype == "PSV" || filetype == "TSV" || filetype == "CSVY" || filetype == "ZSAV" || filetype == "XPT" ||
			filetype == "POR" || filetype == "RDS" || filetype == "REC" || filetype == "MTP" || filetype == "SYD" || filetype == "ARFF" || 
			filetype == "DIF" || filetype == "FWF" || filetype == "GZ" || filetype == "PARQUET" || filetype == "WF1" || filetype == "FEATHER" ||
			filetype == "FST" || filetype == "JSON" || filetype == "MAT" || filetype == "ODS" || filetype == "HTML" || filetype == "XML" ||
			filetype == "YML" || filetype == "PZFX"){
				success = BSkyReadWithRio(fullpathfilename, datasetname, replace=replace_ds) 
			}			
			#cat("Top Level - Finished Loading Dataset:",datasetname)
			#print(Sys.time())
			# if ( isUniqueColumns(datasetname) )
			# {
				########## Following should not execute if any kind of error occur in code above #######
				### Dataset already in place, now find index and do following ###
				DataSetIndex <- UAgetIndexOfDataSet(datasetname)
				uadatasets$fullpath[DataSetIndex] <- fullpathfilename
				#print(eval(parse(text=paste('colnames(',datasetname,')'))))
				replacePeriodWithUnderscore(datasetname)	
				#print(eval(parse(text=paste('colnames(',datasetname,')'))))
			# }
			# else
			# {
				# cat("\nStopping!")
				# #BSkyRemoveRefreshDataframe(datasetname)
				# stop("The dataset has duplicate column names.")
				# cat("\nStopped!")
			# }
		# # # },
		
		# # # warning = UAwarnHandlerFn

		# # # ) # end of withCallingHandlers for catching warnings and continuing execution		
		# # # },
		# # # error = UAerrHandlerFn,
		
		# # # silent =TRUE		
	# # # )

	    	if(BSkyLocalErrorFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# Error is already handled and logged
			# Whereever the error occured execution cannot continue
			# control will be passed to the error handler function
			# and then the control will come out ogf the Try-Catch block 
			
			# cat("Error caught in UAloadDataset \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level load dataset function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAloadDataset \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		
		# if maxFactor = -1 then we do not convert factor col to character
		# if maxFactor is a positive integer and factor columns has levels more than maxFactor 
		# we convert this col to character.
		#
		## 11Jul2023 : The logic above is old. We need new rules: 
		## Ross asked for 1 and 2: 
		## 1: no char to factor conversion if the UI setting ("Control the creation of factor variables") is unchecked. 
		## that means when maxFactor == -1 then we should load all chars as chars (no conversion to factor)
		##
		## 2: When user setting is checked and a positive value is provided, the factor columns having levels count
		## more than the maxFactor will be converted to character. While the remaining factors having levels count
		## lower than the maxFactor will remain factors.
		## 
		## 3: to complete the set I think we can add one more rule. if maxFactor == 0, then we should keep all the
		## factors as factors (no convertion from factor to character)
		##
		## I found, when Hadley's loads Excel or CSV, it loads characters as characters. We make
		## these columns factor so that we can have some control over it using the maxFactor.		
		## When we load (Excel, CSV, DAT and SAS) we are forcing char to factor conversion. Later based on the
		## maxFactor value we are executing the following block of code to convert (all, none or a few) factor columns
		## to character. The reason why we load dataset and converted the character columns to factor is probably
		## because we need levels count in the following block. Maybe making them factor is an easy way to find out 
		## how many distince values a character column have.
		##
		
		### We load these files (Excel, CSV, DAT and SAS) converting char to factor so only these files should go 
		### under this processing to convert factor back to characters based on maxFactor value.
		allowedFileFormats = c("XLSX","XLS","CSV","DAT","SAS7BDAT")

		if(success == 0 && maxFactor != 0 && (filetype %in% allowedFileFormats)) ## if file opened successfully
		{
			colcount = eval(parse(text=paste('ncol(.GlobalEnv$',datasetname,')')))
			for(i in 1:colcount)
			{
				coluname = eval(parse(text=paste('colnames(.GlobalEnv$',datasetname,')[',i,']',sep='')))
				colclass = eval(parse(text=paste('class(.GlobalEnv$',datasetname,'$',coluname,')',sep='')))

				if("factor" %in% colclass)
				{
					lvlcount = eval(parse(text=paste('length(levels(.GlobalEnv$',datasetname,'$',coluname,'))',sep='')))
					if(lvlcount > maxFactor)
					{
						eval(parse(text=paste('.GlobalEnv$',datasetname,'$',coluname,'<- as.character(.GlobalEnv$',datasetname,'$',coluname,')',sep='' )))
					}
				}
			}
		}
		# cat("\nWrapup now")
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		# cat("Returning return structure from this top level load dataset function\n")
		# # # return(invisible(BSkyReturnStructure2()))
		return(success)
}

#Reloads data along with attributes. If you modified data and want to get original data back(reload from disk).
#For CSV and EXCEL this may not work because of extra parameter required. Once proper values are provided to those it should work.
## Loads dataset from disk file.
BSkyReloadDataset<-function(fullpathfilename,  filetype, sheetname=NULL, csvHeader=TRUE, loaddataonly=FALSE,isBasketData=FALSE, sepChar=',', deciChar='.', datasetname)
{
	#cat("\nReloading Dataset...");
	#print(loaddataonly)
	#cat("\n");
	
	#if someone runs realod on dataset that is reduced few rows and then loaddataonly=TRUE,
	#other attributes will not be created and there will be problem loading variable grid. 
	if(FALSE) #(loaddataonly) ## reloads data only#
	{
			## Dataset level attributes are backed up ( Do backup column level attribute too for each column )
			bkupattrs <- eval(parse(text=paste('attributes(',datasetname,')')))
			#cat('\nDataset opened with  name\n',datasetname)
			if(filetype=="SPSS"){
				eval( parse(text=paste(datasetname,' <<- read.spss(file=\'',fullpathfilename,'\',use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)',sep='')))
			}
			else if(filetype == "XLS"){
				channel<-NULL
				channel <- odbcConnectExcel(fullpathfilename, readOnly=FALSE)
				eval( parse(text=paste(datasetname,' <<- sqlQuery(channel, \'select * from [',sheetname,'$]\')', sep=''  )))
				odbcCloseAll()
			}
			else if(filetype == "XLSX"){
				channel<-NULL
				channel <- odbcConnectExcel2007(fullpathfilename, readOnly=FALSE)
				eval( parse(text=paste(datasetname,' <<- sqlQuery(channel, \'select * from [',sheetname,'$]\')', sep=''  )))
				odbcCloseAll()
			}			
			else if(filetype == "CSV"){
				eval( parse(text=paste(datasetname,' <<- read.csv(\'',fullpathfilename,'\', header=Header)',sep='')))
			}
			else if(filetype == "DBF"){
				eval( parse(text=paste(datasetname,' <<- read.dbf(\'',fullpathfilename,'\')', sep='' )))
			}
			else if(filetype == "RDATA" || filetype == "RDA"){
					### Following structure is already created when saving a file. While load robj file these are filled automatically ####
					UAObj <- NULL
					UAObj$info <- NULL
					UAObj$obj <- NULL
					# load(file = fullpathfilename)#, uadatasets$lst[Newindex])
					# #Check signature. If rdata file is created using BSky software then only open it
					# bskysig <- UAObj$info
					# if(!is.null(bskysig) && bskysig$ver==1.2 && bskysig$name=="BSky")
					# {
						# eval( parse(text=paste( datasetname,' <<- UAObj$obj' , sep=''   )))
					# }				
					
					## above code commented
					## 31May 2018 loading RData file that has single dataset only and no other object.
					## it could be of UAObj format or just one data.frame saved in RData file.
					#load(file = fullpathfilename, envir = .GlobalEnv)
					
					#17Oct2019
					eval(parse(text=paste('UAreadRObj(RObjfileName="',fullpathfilename,'", datasetname="',datasetname,'", replace=TRUE)',sep='')))
			}			
			else  if(filetype == "TXT"){
			}	
			###restoring Dataset level attributes (Do restore column level attributes for each column too)
			eval(parse(text=paste('attributes(',datasetname,') <<- bkupattrs')))
	}
	else ## reloads everything, data attributes etc.
	{
		BSkyloadDataset(fullpathfilename,  filetype, worksheetName=sheetname, replace_ds=TRUE, csvHeader=csvHeader, isBasketData=isBasketData, sepChar=sepChar, deciChar=deciChar,datasetName=datasetname )
	}
	return(invisible(NULL))
}


BSkysaveAsDataset <-function(fullpathfilename,  filetype, Rownames = TRUE, Colnames = FALSE, newWorksheetName=NULL,factor2char=TRUE, dataSetNameOrIndex, processit=TRUE)
{
	processDS = FALSE   ## do not process datasets
	eval(parse(text=paste('attrlist = names(attributes(',dataSetNameOrIndex,'))',sep='')))
	if("processDS" %in% attrlist )
	{
		eval(parse(text=paste('processDS = attributes(',dataSetNameOrIndex,')$processDS',sep='')))
	}
	  
	if(processDS) ## (processit). processit not needed. it was coming from JS while prcessDS is a dataset atribute.
	{
		BSkyProcessNewDataset(datasetName=dataSetNameOrIndex, NAstrings = c("NA"), stringAsFactor=TRUE)
	}
	success =  BSkysaveDataset(fullpathfilename=fullpathfilename,  filetype=filetype, Rownames = Rownames, Colnames = Colnames, newWorksheetName=newWorksheetName,factor2char=factor2char, dataSetNameOrIndex=dataSetNameOrIndex)
	# cat("\nSuccess = ")
	# cat(success)
	if(success == 0)
	{
		BSkycloseDataset(dataSetNameOrIndex)
	}
	else #saveas failed and we did not close the dataset. So we can load it back but we first remove 
	{ #entry from uadatasets$name as load-refresh will add entry back in uadatasets$name
		BSkyRemoveEntry(dataSetNameOrIndex)
	}
	return(invisible(success))
}


#remove entry from uadatasets$name so that loadrefresh can add it back from memory dataset
BSkyRemoveEntry <-function(dataSetNameOrIndex)
{
	idx = UAgetIndexOfDataSet(dataSetNameOrIndex) 
	#cat('\nNo .GlobalEnv: Index of the dataset:\n')
	#print(idx)
	
	if(idx > 0) #19Oct2016 do not use this --> if(length(idx) > 0)
	{
		uadatasets$name[idx] <- "" ## making the closed dataset blank. So index of existsing open dataset will not change.
		dscount =  length(uadatasets$name)
		#cat('\nTotal DS\n')
		#print(dscount)
		oldds <- uadatasets$name
		uadatasets$name <- NULL
		for(i in 1 : dscount)
		{
			#cat("\n", oldds[i], "\t", nchar(oldds[i]))
			if(nchar(oldds[i]) > 0)
			{
				uadatasets$name <- c(uadatasets$name, oldds[i])
				#cat("\n+++", oldds[i])
			}
		}

	}
}


###################################################################################################################
# fullpathfilename -> Full path filename(drive directory and filename) of an existing file to load the UA memory space
# dataSetNameOrIndex -> dataset name thats already given to dataset while reading/loading. Or index of dataset
# rownames			-> For Excel only. Serial number for each record
# colnames			-> For Excel only.
# newWorksheetName -> For Excel only (filetype must be XLS or XLSX).New Worksheet name where dataset is to be saved
# 					  
# fact3char			- For DBF only, Factor col are written in character if TRUE. Else they will contain integer 
# filetype -> Case sensitive only supports SPSS, XLS, XLSX, DBF, CSV, RDATA
###################################################################################################################

### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Save a memory dataset to a file
#'
#' @description saves a BlueSky Statistics UI datagrid loaded dataset to the supported file formats.
#' [supported file formats : .SAV, .SAS7BDAT, .DTA, .XLSX, .CSV, .RDATA, and .DBF]
#'
#' @param fullpathfilename is a full path filename of a disk file to which will dataset will be written. Use forward slash as path separator.
#' @param filetype is the file format you want to save to. It must be one of the ("XLSX", "CSV", "DBF", "RDATA", "SAV", "SAS7BDAT", "DTA").
#' @param newWorksheetName is the name of the excel worksheet. Used only when saving to an Excel file format.
#' @param dataSetNameOrIndex is a name of the dataset that already loaded in the BlueSky Statistics UI datagrid. Use single quotes around dataset name.
#'
#' @examples
#' df <-data.frame(A=c(1,2,3), B=c(4,5,6), C=c(6,7,8))
#' BSkyLoadRefresh('df') #dataframe must be loaded in the grid before you try to save it to different formats.
#' BSkysaveDataset("D:/mytestdata.rdata", filetype="RDATA", dataSetNameOrIndex='df')
BSkysaveDataset <-function(fullpathfilename,  filetype, Rownames = TRUE, Colnames = FALSE, newWorksheetName=NULL,factor2char=TRUE, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkysaveDataset: Error in Saving dataset : ", "DataSetName :", dataSetNameOrIndex," ", "Filename :", paste(fullpathfilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkysaveDataset: Warning in Saving dataset : ", "DataSetName :", dataSetNameOrIndex," ", "Filename :", paste(fullpathfilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success=-1
	tryCatch(
		{
		withCallingHandlers(
		{
			## before saving to a file we must turn-off processDS attribute so that when we load this
			## dataset (.RData) later it should not run the empty dataset processing on the file based dataset.
			## because processDS = FALSE in saved file, we do not do any empty row/col cleaning.
			eval(parse(text=paste('attr(',dataSetNameOrIndex,',"processDS") <<- FALSE')))
			#find extension
			#extn = File.sav
			if(filetype=="SAV"){
				success = BSkywriteSAV(fullpathfilename, dataSetNameOrIndex)  #requires rio R package
			}
			if(filetype=="SAS7BDAT"){
				success = BSkyWriteSas(fullpathfilename, dataSetNameOrIndex)  #requires haven R package
			}
			if(filetype=="DTA"){
				success = BSkyWriteStata(fullpathfilename, dataSetNameOrIndex)  #requires haven R package
			}			
			else if(filetype == "XLS"){
				success = UAwriteExcel(fullpathfilename, dataSetNameOrIndex, newWorksheetName, row.names = Rownames, col.names = Colnames, xlsx=FALSE)
			}
			else if(filetype == "XLSX"){
				success = UAwriteExcel(fullpathfilename, dataSetNameOrIndex, newWorksheetName, row.names = Rownames, col.names = Colnames, xlsx=TRUE)
			}			
			else if(filetype == "CSV" || filetype == "TXT"){
				success = UAwriteCSV(fullpathfilename, dataSetNameOrIndex)
			}
			else if(filetype == "DBF"){
				success = UAwriteDBF(fullpathfilename, dataSetNameOrIndex, fact2char=factor2char)
			}
			else if(filetype == "RDATA" || filetype == "RDA"){
				success = UAwriteRObj(fullpathfilename,dataSetNameOrIndex)
			}	
			else if(filetype == "PSV" || filetype == "TSV" || filetype == "CSVY" || filetype == "ZSAV" || filetype == "XPT" ||
			filetype == "POR" || filetype == "RDS" || filetype == "REC" || filetype == "MTP" || filetype == "SYD" || filetype == "ARFF" || 
			filetype == "DIF" || filetype == "FWF" || filetype == "GZ" || filetype == "PARQUET" || filetype == "WF1" || filetype == "FEATHER" ||
			filetype == "FST" || filetype == "JSON" || filetype == "MAT" || filetype == "ODS" || filetype == "HTML" || filetype == "XML" ||
			filetype == "YML" || filetype == "PZFX"){
				success = BSkyWriteWithRio(fullpathfilename, dataSetNameOrIndex) 
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
			
			#cat("Error caught in BSkysaveDataset \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level save dataset function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			#cat("Warning caught in BSkysaveDataset \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level save dataset function\n")
		# # # return(invisible(BSkyReturnStructure()))
		return(success)
}

# "UAsaveDataset('F:/SPSS/d2.xls', 'Dataset1',  newWorksheetName='Sheet1', filetype='XLS')"


###################################################################################################################
# dataSetNameOrIndex -> dataset name thats already assigned to the dataset while reading/loading. Or index of the dataset
# This dataset name could be in-memory dataframe (created on the fly). Or it could be, dataframe created by loading (say SPSS) file from disk
# Dataframe is in memory, the difference is how its created.
###################################################################################################################

#' BSkycloseDataset closes a dataset, removing it from the R.
#' @name BSkycloseDataset
#' @param dataSetNameOrIndex is the name of the dataset that is to be closed.
#' @export 
#' @examples
#' df <-data.frame(A=c(1,2,3), B=c(4,5,6), C=c(6,7,8))
#' BSkycloseDataset(df)

BSkycloseDataset<-function(dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkycloseDataset: Error in Closing Dataset : ", "DataSetName :", dataSetNameOrIndex,sep="")
	BSkyWarnMsg = paste("BSkycloseDataset: Warning in Closing Dataset : ", "DataSetName :", dataSetNameOrIndex,sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	tryCatch(
		{
			
		withCallingHandlers(
		{
			datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{
				#Setting DataSetIndex location in uadatasets to NULL, to remove it.
				eval(parse(text=paste(datasetname, '<- NULL')))
				#cat("\nDataset closed by setting it to null\n")
				## Also need to remove this name from uadatasets$name list
				# uadatasets$name <- names(uadatasets$lst)
				
				#19Oct2016 This call is just for testing. And can also be replaced for the next UAgetIndexOfDataSet call
				# idx = UAgetIndexOfDataSet(datasetname)
				# cat('\n.GlobalEnv passed : Index of the dataset:\n')
				# print(idx)
				
				#19Oct2016 only pass dataset name (eg.. 'Dataset1') and not '.GlobalEnv$Dataset1'
				#20Oct2016 Since a fix has been made to UAgetIndexOfDataSet so you can pass 'Dataset1' or '.GlobalEnv$Dataset1'
				#The code commented above uses '.GlobalEnv$Dataset1' and works fine too.
				idx = UAgetIndexOfDataSet(dataSetNameOrIndex) 
				#cat('\nNo .GlobalEnv: Index of the dataset:\n')
				#print(idx)
				
				if(idx > 0) #19Oct2016 do not use this --> if(length(idx) > 0)
				{
					uadatasets$name[idx] <- "" ## making the closed dataset blank. So index of existsing open dataset will not change.
					dscount =  length(uadatasets$name)
					#cat('\nTotal DS\n')
					#print(dscount)
					oldds <- uadatasets$name
					uadatasets$name <- NULL
					for(i in 1 : dscount)
					{
						#cat("\n", oldds[i], "\t", nchar(oldds[i]))
						if(nchar(oldds[i]) > 0)
						{
							uadatasets$name <- c(uadatasets$name, oldds[i])
							#cat("\n+++", oldds[i])
						}
					}

				}
				#-return(NULL)
				eval(parse(text=paste('remove(',dataSetNameOrIndex, ', envir=.GlobalEnv)')))
			}
			else
			{
				BSkyErrMsg =paste("UAcloseDataset: Cannot close dataset. Dataset name or index not found."," Dataset Name:", dataSetNameOrIndex)
				# cat("\nError: Cannot close dataset. Dataset name or index not found\n")
				warning("UAcloseDataset: Cannot close dataset. Dataset name or index not found.")
				#-return(NULL)
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
			
			# cat("Error caught in UAcloseDataset \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level close dataset function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAcloseDataset \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level close dataset function\n")
		return(invisible(BSkyReturnStructure()))
}

# closes dataset with name 'datasetname'. Following function only closes and does not perform reopening.
# ReOpen logic should reside in the parent function within which following function is called (to first close and then reopen).
BSkyCloseForReloading<-function(datasetname)
{
			if(!is.null(datasetname))
			{
				#Setting DataSetIndex location in uadatasets to NULL, to remove it.
				eval(parse(text=paste(datasetname, '<- NULL')))
				#cat("\nDataset closed by setting it to null\n")
				## Also need to remove this name from uadatasets$name list
				# uadatasets$name <- names(uadatasets$lst)
				
				idx = UAgetIndexOfDataSet(datasetname)
				if(length(idx) > 0)
				{
					uadatasets$name[idx] <- "" ## making the closed dataset blank. So index of existsing open dataset will not change.
					dscount =  length(uadatasets$name)
					oldds <- uadatasets$name
					uadatasets$name <- NULL
					for(i in 1 : dscount)
					{
						#cat("\n", oldds[i], "\t", nchar(oldds[i]))
						if(nchar(oldds[i]) > 0)
						{
							uadatasets$name <- c(uadatasets$name, oldds[i])
							#cat("\n+++", oldds[i])
						}
					}
				}
				eval(parse(text=paste('remove(',datasetname, ', envir=.GlobalEnv)')))
			}
}

#datasetName must be a string
BSkyCheckNullDataset <- function(datasetNameStr)
{
	success=-1
	if(exists(datasetNameStr, envir=.GlobalEnv) && !(eval(parse(text=paste("is.null(",datasetNameStr,")")))))
	{
		uadatasets.sk$currentDatasetname = datasetNameStr
		success=0
	}
	else
	{
		idx = which(datasetNameStr == uadatasets$name)
		if(length(idx) > 0)
		{
			uadatasets$name = uadatasets$name[-idx]
			if(uadatasets.sk$currentDatasetname == datasetNameStr)
			{
				uadatasets.sk$currentDatasetname = ""
			}
		}
	}
	# cat("\n Success:\n")
	# print(success)
	return(invisible(success))
}

# not complete
UAgetAllData<-function()
{
	BSkyFunctionInit()
	#BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAgetAllData: Error in get all data : ",  sep="")
	BSkyWarnMsg = paste("UAgetAllData: Warning in get all data : ",  sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{
		withCallingHandlers(
		{
			DataSetIndex <- BSkyValidateDataset(dataSetNameOrIndex)		
			filetype <- attr(uadatasets$lst[[DataSetIndex]],"filetype")
			#cat("\nFile Typ:",filetype,"\n")	
			
			if(filetype=="SPSS"){
					
			}
			else if(filetype == "XLS"){
			}
			else if(filetype == "XLSX"){
			}			
			else if(filetype == "CSV"){
			}
			else if(filetype == "DBF"){
			}
			else if(filetype == "RDATA" || filetype == "RDA"){
			}			
			else if(filetype == "TXT"){
			}			
			else{
				warning("UAgetAllData: File type not supported.")
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
			
			# cat("Error caught in UAgetAllData \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level get all data function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAgetAllData \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level get all data function\n")
		#return(BSkyReturnStructure())	
}

###################################################################################################################
# dataSetNameOrIndex -> dataset name thats already given to dataset while reading/loading. Or index of dataset
# colNameOrIndex     -> Name/index of the column whose properties you need. eg.. "gender"
# propname			 -> Name of the property whose value is needed. For 'All' all the properties will be returned
###################################################################################################################
UAgetColProp<-function(colNameOrIndex, propname='All', Type.as.Class=FALSE, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAgetColProp: Error in Compute : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAgetColProp: Warning in Compute : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	prop <- list()
	tryCatch(
		{

		withCallingHandlers(
		{
			if(propname == 'All')
			{
				prop<-UAgetColProperties(dataSetNameOrIndex, colNameOrIndex, asClass=Type.as.Class)
				 #cat("\nCol Properties:\n")
				 #print(prop)
				 #cat("\n")
			}
			else
			{
				prop<-UAgetColProperties(dataSetNameOrIndex, colNameOrIndex, asClass=Type.as.Class)
				#cat("\nAll Properties:",prop[[2]],"\n")
				prop <- eval(parse(text = paste('prop$', propname, sep = '', collapse=',' )))
				#cat("\nSingle Property:",prop,"\n")
				# eval(parse(text = paste(datasetName,'$', datasetVariableList[i])))
			}
			#-return(prop)
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
			
			# cat("Error caught in UAgetColProp \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level get col property function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAgetColProp \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function UAgetColProp
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level get col property function\n")
		return(invisible(BSkyReturnStructure(list(extra=c(prop)))))  
}


UAsetColProp<-function(colNameOrIndex, propertyName, propertyValue, mistype="none", newOrder=c(), dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetColProp: Error in setting col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetColProp: Warning in setting col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success <- -1
	tryCatch(
		{
	
		withCallingHandlers(
		{
			datasetname <- BSkyValidateDataset(dataSetNameOrIndex)	
			
			if(!is.null(datasetname))
			{		
				# filetype <- attr(uadatasets$lst[[DataSetIndex]],"filetype")
				# cat("\nFTyp:",filetype,"\n")
			    UAsetColProperties(dataSetNameOrIndex, colNameOrIndex, propertyName, propertyValue, mistype, newOrder)
				success <- 1 #- return (1)
			}
			else
			{
				BSkyErrMsg =paste("UAsetColProp: Cannot set col property. Dataset name or index not found."," Dataset Name:", dataSetNameOrIndex)
				# cat("\nError: Cannot set col property. Dataset name or index not found\n")
				warning("UAsetColProp: Cannot set col property. Dataset name or index not found.")
				success <- -1 #return (-1)
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
			
			# cat("Error caught in UAsetColProp \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level set clo properties function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAsetColProp \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level set clo properties function\n")
		return(invisible(BSkyReturnStructure())) #return(success)
}

###################################################################################################################
# dataSetNameOrIndex -> dataset name thats already given to dataset while reading/loading. Or index of dataset
# colNames -> name of the columns whose indexes you need. eg..  c("age", "gender")
###################################################################################################################
BSkygetIndexesOfCols<-function(colNames, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkygetIndexesOfCols: Error in get cols indexes : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNames, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkygetIndexesOfCols: Warning in get cols indexes : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNames, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	indexes <- -1
	tryCatch(
		{
		
		withCallingHandlers(
		{
	
			indexes<-UAgetIndexsOfColsInDataSet(dataSetNameOrIndex,colNames)
			#- return(indexes)
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
			
			# cat("Error caught in BSkygetIndexesOfCols \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level get cols indexes function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkygetIndexesOfCols \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level get cols indexes function\n")
		return(invisible(indexes)) #return(BSkyReturnStructure(indexes))
}

# not comlpete
UAgetDataByIndex<-function(filetype)
{
	BSkyFunctionInit()
	#BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAgetDataByIndex: Error in get data by index : ", "DataSetName :", filetype," ", "Variable Name List :", paste(filetype, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAgetDataByIndex: Warning in get data by index : ", "DataSetName :", filetype," ", "Variable Name List :", paste(filetype, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{
					
		withCallingHandlers(
		{
			datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{		
				filetype <- eval(parse(text=paste('attr(',datasetname,',"filetype")',sep='')))
				#cat("\nFTyp:",filetype,"\n")
				
				if(filetype=="SPSS"){
						# indexes<-UAgetIndexsOfColsInDataSet(dataSetNameOrIndex,colNames)
				}
				else if(filetype == "XLS"){
				}
				else if(filetype == "XLSX"){
				}	
				else if(filetype == "CSV"){
				}
				else if(filetype == "DBF"){
				}
				else if(filetype == "RDATA" || filetype == "RDA"){
				}
				else if(filetype == "TXT"){
				}

			}
			else
			{
				# cat("\nError: Cannot get data by index. Dataset name or index not found\n")
				warning("UAgetDataByIndex: Cannot get data by index. Dataset name or index not found.")
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
			
			# cat("Error caught in UAgetDataByIndex \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level get data by index function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAgetDataByIndex \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level get data by index function\n")
		return(invisible(BSkyReturnStructure()))
}

###################################################################################################################
# dataSetNameOrIndex -> dataset name thats already given to dataset while reading/loading. Or index of dataset
# colNameOrIndex     -> Name/index of the columns whose missing values you need. eg..   "age"
###################################################################################################################
UAgetMissing<-function( colNameOrIndex, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAgetMissing: Error in get missing : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAgetMissing: Warning in get missing : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	miss <- ""
	tryCatch(
		{

		withCallingHandlers(
		{

			miss<-UAgetColMissing(dataSetNameOrIndex, colNameOrIndex)
			#- return(miss)
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
			
			# cat("Error caught in UAgetMissing \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level get Missing function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAgetMissing \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level get Missing function\n")
		return(invisible(BSkyReturnStructure(list(extra=c(miss))))) #return(miss)
}

###################################################################################################################
# dataSetNameOrIndex -> dataset name thats already given to dataset while reading/loading. Or index of dataset
# colNameOrIndex     -> Name/index of the columns whose missing values you want to set. eg..   "age"
# missvals -> vector representing missing values. eg.. c(100, 200,300)
###################################################################################################################
UAsetMissing<-function( colNameOrIndex, missvals=NULL, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetMissing: Error in setting missing : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetMissing: Warning in setting missing : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	tryCatch(
		{
		withCallingHandlers(
		{
		
			UAsetColMissing(dataSetNameOrIndex, colNameOrIndex, misvals=missvals)	
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
			
			# cat("Error caught in UAsetMissing \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		# cat("\nWARNING:: top level set missing function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAsetMissing \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level set missing function\n")
		return(invisible(BSkyReturnStructure()))
}



##22Aug2021 simplified BSky func to get the split column list. It will return empty character array if there is no split.
BSkyGetDatasetSplitInfo <- function(datasetNameStr)
{

	splitVarList = c()

	if(exists(datasetNameStr, envir = .GlobalEnv) &&  !(eval(parse(text=paste("is.null(",datasetNameStr,")")))))
	{
		#cat("B4 Split Info:")
		splitInfo = UAgetDataframeSplit(datasetNameStr)
		#cat(datasetNameStr)
		#cat("Split Info:")
		#print(splitInfo)
		if(!is.null(splitInfo) && length(splitInfo) > 0 && !is.null(splitInfo$DFsplit) && splitInfo$DFsplit == TRUE)
		{
			splitVarList = splitInfo$DFsplitcolnames
		}
	}
	else 
	{
		idx = which(datasetNameStr == uadatasets$name)
		if(length(idx) > 0)
		{
			uadatasets$name = uadatasets$name[-idx]
			if(uadatasets.sk$currentDatasetname == datasetNameStr)
			{
				uadatasets.sk$currentDatasetname = ""
			}
		}
	}
	#cat("\nreturn from BSkyGetDatasetSplitInfo\n")
	#print(splitVarList)
	return(invisible(splitVarList))
}

#################################  NOT IN USE ###### (better check)  ############################################################################
# dataSetNameOrIndex -> dataset name thats already given to dataset while reading/loading. Or index of dataset
###################################################################################################################
UAgetDataframeSplit<-function(dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAgetDataframeSplit: Error in getting dataframe split : ", "DataSetName :", dataSetNameOrIndex," ", sep="")
	BSkyWarnMsg = paste("UAgetDataframeSplit: Warning in getting dataframe split : ", "DataSetName :", dataSetNameOrIndex," ",sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	splitprop <-""
	tryCatch(
		{
	
		withCallingHandlers(
		{

			splitprop<-UAgetDataframeSplitProp(dataSetNameOrIndex)	
			#- return(splitprop)
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
			
			# cat("Error caught in UAgetDataframeSplit \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level get dataframe split function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAgetDataframeSplit \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level get dataframe split function\n")
		return(invisible(splitprop)) # return(BSkyReturnStructure(splitprop))
}

### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Set or remove split
#'
#' @description Splits the data into groups based on the factors selected, once the dataset is split, the analysis you select is performed independently for each split. For example if you run a crosstabulation analysis or a hypothesis test, this analysis is performed independently for each split (the output of the analysis is also generated separately for each split). 
#' OR
#' Removes the split on the dataset. Splits, see Data>Group By>Set Split splits the data into groups based on the factors selected, once the dataset is split, the analysis you select is performed independently for each split. For example if you run a crosstabulation analysis or a hypothesis test, this analysis is performed independently for each split (the output of the analysis is also generated separately for each split). 
#' 
#' @param col.names These are the column names/variable names that you want to split the dataset by, e.g. col.names =c("var1", "var2"). To reset the split, set to c().
#' @param datasetnameorindex this is the name of the index.​
#' @param removeall.splits TRUE splits are removed, FALSE splits are added.
#'
#' @return
#'
#' @examples
BSkySetDataFrameSplit <- function(col.names, datasetnameorindex, removeall.splits=FALSE)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetnameorindex)
	
	BSkyErrMsg = paste("BSkySetDataFrameSplit: Error in setting datafram split : ", "DataSetName :", datasetnameorindex," ", "Variable Name List :", paste(col.names, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkySetDataFrameSplit: Warning in setting datafram split : ", "DataSetName :", datasetnameorindex," ", "Variable Name List :", paste(col.names, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	tryCatch(
		{
	
		withCallingHandlers(
		{
 if(!is.na(col.names) && length(col.names) > 0) #not NA and there is alteast one col name.
 {
	 #cat("\nSetting-Split:\n")
	# UAsetDataframeSplit(col.names, datasetnameorindex)
	UAsetDataframeSplitProp(dataSetNameOrIndex=datasetnameorindex, colNames=col.names)
}
 else
 {
	 #cat("\n Removing-Split:\n")
	# UAremoveDataframeSplit( col.names, removeall.splits=TRUE, datasetnameorindex)
	if(removeall.splits || is.null(col.names) || is.na(col.names) || length(col.names)==0)
		removeallsplits = TRUE
	UAremoveDataframeSplitProp(dataSetNameOrIndex=datasetnameorindex, colNames=col.names, removeAllSplits=removeallsplits)
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
			
			# cat("Error caught in BSkySetDataFrameSplit \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level set dataframe split function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkySetDataFrameSplit \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level set dataframe split function\n")
		invisible(BSkyReturnStructure(bskyAdditionalTableList = NA))
}

###################################################################################################################
# dataSetNameOrIndex -> dataset name thats already given to dataset while reading/loading. Or index of dataset
# colName_s  -> one or more column names for those you want to set split. Sequence is important. 
# 				eg.. c("gender", "tg1", "wgt2")
###################################################################################################################
UAsetDataframeSplit<-function(col.names, datasetnameorindex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetnameorindex)
	
	BSkyErrMsg = paste("UAsetDataframeSplit: Error setting dataframe split : ", "DataSetName :", datasetnameorindex," ", "Variable  :", paste(col.names, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetDataframeSplit: Warning setting dataframe split : ", "DataSetName :", datasetnameorindex," ", "Variable :", paste(col.names, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
		tryCatch(
		{

		withCallingHandlers(
		{
			UAsetDataframeSplitProp(dataSetNameOrIndex=datasetnameorindex, colNames=col.names)
			#print(attributes(accidents)$split)
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
			
			# cat("Error caught in UAsetDataframeSplit \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level sort function\n")
	if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAsetDataframeSplit \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function UAsetColMeasure
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		invisible(BSkyReturnStructure()) #		
}


###################################################################################################################
# dataSetNameOrIndex -> dataset name thats already given to dataset while reading/loading. Or index of dataset
# colName_s  -> one or more column names for those you want to set split. Sequence is important. 
# 				eg.. c("gender", "tg1", "wgt2")
###################################################################################################################
UAremoveDataframeSplit<-function( col.names, removeall.splits=FALSE, datasetnameorindex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetnameorindex)
	
	BSkyErrMsg = paste("UAremoveDataframeSplit: Error  : ", "DataSetName :", datasetnameorindex," ", "Variable  :", paste(col.names, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAremoveDataframeSplit: Warning  : ", "DataSetName :", datasetnameorindex," ", "Variable :", paste(col.names, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	tryCatch(
		{
	
		withCallingHandlers(
		{
			UAremoveDataframeSplitProp(dataSetNameOrIndex=datasetnameorindex, colNames=col.names, removeAllSplits=removeall.splits)
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
			
			# cat("Error caught in UAremoveDataframeSplit \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level sort function\n")
	if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAremoveDataframeSplit \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function UAsetColMeasure
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(BSkyReturnStructure())) #
}


###################################################################################################################
#Empty function for testing output templates
###################################################################################################################
BSkyOutputTestDriver<-function(colNameOrIndex=NA,dataSetNameOrIndex=NA)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkyOutputTestDriver: Error output test driver : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkyOutputTestDriver: Warning output test driver : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	tryCatch(
		{

		withCallingHandlers(
		{
			cat("\nEmpty Function Executed!\n")
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
			
			# cat("Error caught in BSkyOutputTestDriver \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level sort function\n")
	if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkyOutputTestDriver \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function UAsetColMeasure
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(BSkyReturnStructure()))#
}


#starttime =Sys.time()
#test =uaonesmt.test(vars,mu=0,conf.level=0.95,datasetname,missing=1)
#time =Sys.time() -starttime
#test
#time

#UAloadDataset("F:/BlueSky/Projects/SPSS/dietstudy.sav",  filetype="SPSS", worksheetName=NULL, replace_ds=FALSE, csvHeader=TRUE, datasetName="diet" )