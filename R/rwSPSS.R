######## list of methods in this file; in sequence (few may not be ready and unused ) ###########
# UAloadSPSSinDataFrame
# UAcreateExtraAttributes
# BSkygetMissing
# UAgetSPSSVariableView_Label
# UAsetSPSSColNames
# UAaddSPSSColumn
# UAsetSPSSColType    (not complete)
# UAsetSPSSColLevels (not complete)
# UAsetColasFactor  (not complete)
# UAsetSPSSColMeasure  (not complete)
# UAsetSPSSColData  (not complete)
# UAsetSPSSColProperties (not complete)
#################################################################################################


#############################################################################################################
#Function: UAloadSPSSinDataFrame(SPSSfileName, dataSetName,replace)						
#              																							
#Parameters: 
#		SPSSfileName		- SPSS file name which is to be loaded 
#							 ( provide full path  drive, directory and filename)
#		dataSetName			- Reference name given to the dataset where user loaded the SPSS file.
#		replace				- If TRUE, it will replace existing data set of same name, if found.            																					
#		loadMissingValue	- If TRUE, read.spss() runs twice so that missing value can be read. 																									
#Description:	
#		Loads SPSS file in UA memory space. dataSetName is reference name specified by user for 
#		later use of this dataset
#																											
#Example: UAloadSPSSinDataFrame("C:\\satisf.sav", "mydataset",TRUE)																								
#############################################################################################################
# OpenStatus=-1
UAloadSPSSinDataFrame <- function(SPSSfileName, datasetname, replace=FALSE, loadMissingValue=FALSE, trimSPSStrailing=FALSE, max.factor=Inf, usehaven=TRUE, encoding=NULL)
{
	success=0
	if(usehaven)
	{
		success = UAloadSPSSinDataFrame.haven(SPSSfileName=SPSSfileName, datasetname=datasetname, replace=replace, loadMissingValue=loadMissingValue, trimSPSStrailing=trimSPSStrailing, max.factor=max.factor, encoding=encoding)
		if(success!=0)
		{
			# OpenStatus <<- success
			# e <- simpleError("test error")
			# stop(e)
		}
	}
	else 
	{
		UAloadSPSSinDataFrame.foreign(SPSSfileName=SPSSfileName, datasetname=datasetname, replace=replace, loadMissingValue=loadMissingValue, trimSPSStrailing=trimSPSStrailing, max.factor=max.factor)
	}
	# OpenStatus=-1
	return(success)
}

UAloadSPSSinDataFrame.foreign <- function(SPSSfileName, datasetname, replace=FALSE, loadMissingValue=FALSE, trimSPSStrailing=FALSE, max.factor=Inf)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetname)
	
	BSkyErrMsg = paste("UAloadSPSSinDataFrame: Error loading SPSS file : ", "DataSetName :", datasetname," ", "SPSS filepath & filename  :", paste(SPSSfileName, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAloadSPSSinDataFrame: Warning loading SPSS file : ", "DataSetName :", datasetname," ", "SPSS filepath & filename   :", paste(SPSSfileName, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	#Error: SPSS filename and dataset name cannot be blank
	# if( (SPSSfileName == "") || (dataSetName == "") )
	# {
		# # set error messages and error codes
		 # uadatasets$errmsg = "SPSS filename or dataset name not provided."
		 # uadatasets$errorfn = -1
		 # # return(-1);
	# }		
	# else
	# {
		#loading necessary package if already not loaded.
		#we should unload it after use, so that we keep R env in original state
		# if(length(which("package:foreign" == search())) == 0)
		# {
			# library(foreign)
			# library(data.table)
		# }
			
		 #cat("SPSS loading...","\n")

		# New Dataset name is only added if you want to replace existing, Or you will check that it should not already be present
		curidx <- UAgetIndexOfDataSet(datasetname) # current index of dataset if its already loaded ( before )
		if((replace == TRUE) || (replace == FALSE && curidx == -1))
		{
			# if i>0 then dataSetName already exists && you want to replace it.
			if(replace == TRUE && curidx > 0)
			{
				# Delete the existing dataset from the global list first. 
				# if dataset already exists, its index is in curidx.
				#cat("\nDataset already exists, found at:",curidx)
				#cat("\nClosing exisiting one and Loading new dataset: ",datasetname)
				# uadatasets$lst[curidx]<-NULL
				#UAcloseDataset(datasetname)
				BSkyCloseForReloading(datasetname)
				#cat("DS Closed")
			}
			#cat("SPSS loading 1st time:",datasetname)
			#print(Sys.time())
			
############## RCommander Way of reading SPSS file #######
### read.spss("D:/BlueSky/Projects/Xtras_Required/Data_Files/Cars.sav", use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)							

missingval <- NULL
			########## Read SPSS keeping to.data.frame=FALSE to get attribute "missings" #########
			if(loadMissingValue) #21Apr2014. Load Missing only when C# flag is ON
			{
temp <- read.spss(file=SPSSfileName,use.value.labels=TRUE, to.data.frame=FALSE,use.missings=FALSE)
#cat("SPSS loaded First time:",datasetname)
#print(Sys.time())
missingval <- attr(temp, "missings")
			 #cat("\nMising Val:")
			 #print(missingval)
			# cat("\n temp to null \n")							
temp <- NULL
			}
			#cat("\nTotal Datasets, before loading new one :",length(uadatasets$name))							
			# Now add new dataset. use.value.labels = TRUE will give factor values else it will be blank on CS var grid.
			# uadatasets$lst <- c(uadatasets$lst, list(read.spss(file=SPSSfileName,use.value.labels=TRUE, to.data.frame=TRUE,use.missings=FALSE)))
			#cat("SPSS loading 2nd time:",datasetname)
			#print(Sys.time())							
			eval( parse(text=paste('.GlobalEnv$',datasetname,' <- read.spss(file=\'',SPSSfileName,'\',use.value.labels=TRUE, max.value.labels=',max.factor,', to.data.frame=TRUE)',sep='')))
			#cat("SPSS loaded Sec time:",datasetname)
			#print(Sys.time())
# ## Another copy of dataset. 
# dsname <- datasetname #BSkyFilename2Datasetname( BSkyGetFilenameOrPath(SPSSfileName)	)
# cat("\nGenerated Datasetname :",dsname)
# #eval( parse(text=paste(dsname, '<<- read.spss(file=SPSSfileName,use.value.labels=TRUE, to.data.frame=TRUE,use.missings=FALSE)' ) ) )
			# set the name (Which is passed as an input parameter to this function)
			# to the newly created data frame within the global list

			#logic to remove extra space from factor/character variables of a SPSS file
			if(trimSPSStrailing)
			{
				colcount = eval(parse(text=paste('ncol(',datasetname,')')))
				for(i in 1:colcount)
				{
					coluname = eval(parse(text=paste('colnames(',datasetname,')[',i,']')))
					colclass = eval(parse(text=paste('class(',datasetname,'$',coluname,')')))
					
					if(colclass == "factor" || colclass=="character")
					{
						eval(parse(text=paste('.GlobalEnv$',datasetname,'$',coluname,'<- str_trim(string=',datasetname,'$',coluname,',side ="both" )',sep='')))
					}
					if(colclass == "factor")
					{
						eval(parse(text=paste('.GlobalEnv$',datasetname,'$',coluname,'<- as.factor(',datasetname,'$',coluname,')',sep='' )))
					}
				}
				
				####Following two lines can be used in place of above for(). May be logic of two line below can be merged into one line. Not Sure.
				###
				## remove space from 'character' type varaibles/column. This also converts col to factor not sure why.
				#eval(parse(text=paste(datasetname,' <<- as.data.frame( lapply (',datasetname,', function(y) {if(class(y) == "character") y = str_trim(string=y,side="both") else y} ) )' )))
				## remove space from 'character' type varaibles/column. We have as.factor() here to convert col back to factor.
				#eval(parse(text=paste(datasetname,' <<- as.data.frame( lapply (',datasetname,', function(y) {if(class(y) == "factor") y = as.factor(str_trim(string=y,side="both")) else y} ) )' )))
			}
			# names(uadatasets$name)[length(uadatasets$lst)]<-datasetname	
			uadatasets$name <- c(uadatasets$name, datasetname)

			#cat("\nLoaded dataset :", datasetname)
			DataSetIndex <- UAgetIndexOfDataSet(datasetname)
			#cat("\nDatasetIndex =", DataSetIndex)
				### New Attribute for missings  at Dataset Level###
				#####if( is.null(missingval))
				#####{
					#eval(parse(text=paste('attr(',datasetname,',"misvals") <<- NULL')))
					#####eval(parse(text=paste('setattr(',datasetname,', "misvals", NULL )' ))) # for NULL value attribute may not be created
				#####}
				#####else
				#####{
					#eval(parse(text=paste('attr(',datasetname,',"misvals") <<- missingval')))
					#eval(parse(text=paste('attributes(',datasetname,')$misvals <<- missingval'))) ## working fine. may be slow. not sure
					#####eval(parse(text=paste('setattr(',datasetname,', "misvals", missingval )' ))) ## working fine.
					
					#print(paste('attr(',datasetname,', "misvals")'))
				#####}
				 #cat("misval set.\t")
				 #print(is.null(missingval))
				 if(loadMissingValue)# load missing only when flag is on. No need to create blank missing values for each col.
				 {
				if(is.null(missingval) || missingval=="" )##no missing values. then set all col to "none"
				{
					#cat("\ncreating Missing attr.")
					#print(Sys.time())
					colmisattr <- list()
					colcount = eval(parse(text=paste('ncol(',datasetname,')')))
					for(i in 1:colcount)
					{
						coluname = eval(parse(text=paste('colnames(',datasetname,')[',i,']')))
						###creating missing value attribute, which is dataset level att.
						#####colmisatt <- eval(parse(text=paste(coluname,'<-list(',coluname,'=list(type="none", value=""))')))
						#####colmisattr	<- c(colmisattr,	colmisatt)	
						
						#print(colmisatt)
						# if(i>1)
						# {
							# eval(parse(text=paste('attr(',datasetname,',"misvals") <<- c(attr(',datasetname,',"misvals"), ',colmisatt,')')))
						# }
						# else
						# {
							# eval(parse(text=paste('attr(',datasetname,',"misvals") <<- c(',colmisatt,')')))
						# }
						# cat("done!@")
						
						#23Oct2015
						#move setattr here beacuse appears like it does not have capablity to set complex attributes
						# So the misvals attribute setting after this loop comes in here.
						colmisatt <- eval(parse(text=paste('list(type="none", value="")')))
						eval(parse(text=paste('setattr(','.GlobalEnv$',datasetname,', "misvals_',coluname,'",  colmisatt )',sep='' )))#attr for Dataset$colname
					}

					#####print(colmisatt)
					#####cat("1 done!@\n")
					#####print(colmisattr)
					# eval(parse(text=paste('attr(',datasetname,',"misvals") <<- c(attr(',datasetname,',"misvals"), ',colmisattr,')')))
					#eval(parse(text=paste('attr(',datasetname,',"misvals") <<- c(colmisattr)')))
	#####eval(parse(text=paste('setattr(',datasetname,', "misvals", ', colmisattr,' )' )))
					#####cat("2 done!@\n")
					#####print(eval(parse(text=paste('attr(',datasetname,',"misvals")'))))
					#print(Sys.time())
				}	
				else #if SPSS file already has missing values. Set them in new misvals_COLNAME attribute(dataset level)
				{
					#cat("\nmissing val not null!\n")
					
					colmisattr <- list()
					colcount = eval(parse(text=paste('ncol(',datasetname,')')))
					for(i in 1:colcount)
					{
					#cat("Col idx:")
					#cat(i)
					#cat("\n")
						colIndex = i
						coluname = eval(parse(text=paste('colnames(','.GlobalEnv$',datasetname,')[',i,']')))
						#colexists <- eval(parse(text=paste('names(',datasetname,'[',colIndex,']) %in% missingval',sep='')))
						colexists <- eval(parse(text=paste('coluname %in% names(missingval)',sep='')))
						#print(colexists)
						if (colexists)##Checking if colname exists in attribute
						{ 
							#cat("\nMissing Col Found...\n")
							spsscolmisatt <- eval(parse(text=paste('missingval[',colIndex,']',sep=''))) # $type and $value
							mistype <- eval(parse(text=paste('missingval[[',colIndex,']]$type',sep='')))
							#print(mistype)
							if(is.null(mistype) || mistype == "" || mistype == "none")
							{
								#cat("A")
								colmisatt <- eval(parse(text=paste('list(type="none", value="")')))
								#cat("B")
							}
							else
							{
								#cat("C")
								misvals <- eval(parse(text=paste('missingval[[',colIndex,']]$value',sep='')))
								#cat("D")
								colmisatt <- eval(parse(text=paste('list(type=mistype, value=misvals)')))
								#colmisatt <- eval(parse(text=paste('list(type="',mistype,'", value=c(',misvals,')')))
								#eval(parse(text=paste('list(type=mistype, value=misvals)' )))
								#print(misvals)
							}
							#cat("E")
							
						}
						else #some col those do not have missing attributes, create defaults for them
						{
						#cat("F")
							colmisatt <- eval(parse(text=paste('list(type="none", value="")')))
						#eval(parse(text=paste('setattr(',datasetname,', "misvals_',coluname,'",  colmisatt )',sep='' )))#attr for Dataset$colname						
						}
#cat("G")
						#colmisatt <- eval(parse(text=paste('list(type="none", value="")')))
						eval(parse(text=paste('setattr(','.GlobalEnv$',datasetname,', "misvals_',coluname,'",  colmisatt )',sep='' )))#attr for Dataset$colname
						#cat("H")
					}					
				}
				}# load missing only when flag is ON.
				
				### OR you can have attribute per col level ## For that need to loop missingval
				### missingval$type and missingval$value for each col
				### attr(uadatasets$lst[[1]][[1]],"misvals")$type <- "none"
				### attr(uadatasets$lst[[1]][[1]],"misvals")$value <- c(1,2,3)
				
				# if(replace == FALSE)
				# {						
					# #for old code compatibility put same list in 'name' also
					# #uadatasets$name <- c(uadatasets$name, datasetname)
					# ##following is better than above line
					
					 # uadatasets$name <- names(uadatasets$lst)
				# }	
				#cat("Extra Attributes.\t")
				#Creating extra attributes at column level
				UAcreateExtraAttributes(datasetname, "SPSS")
				#cat("Extra Attribute set.\t")
				# ll<-length(uadatasets$lst)
				# cat("Len of Dataset:",ll,"\n")
				#return(length(uadatasets$lst))
				
				#increment the 'index' to point to last dataset that was loaded
				#eval( parse(text=paste(dsname, '<<- uadatasets$lst[[DataSetIndex]]' ) ) )
				
				
				#################  TEMP FIX FOR LOST ATTRIBUTES ###########
				# eval( parse(text=paste(dsname, '<<- uadatasets$lst[[',DataSetIndex,']]' ) ) )

		}
		else
		{
			# Error condition - dataset with the same name already on the global list or
			# dataFrameObj is not a data frame type object
			warning("UAloadSPSSinDataFrame: Dataset with the same name already on the global list ")
			##warnmsg <- "Dataset with the same name already on the global list "
			##oendtime=date()
			##endtime=proc.time()
			##time=endtime #-starttime
			##uawritelog(type="Warning",functionname="UAloadSPSSinDataFrame",functcommand=NA,uberfunct="UAloadDataset",endtime =oendtime,usertime=NA, 
			##systemtime=NA,elapsed=NA,message =NA,uamessage =warnmsg,callfn=NA)							
			# return(-1)
		}
					# }
#cat("\nLoading SPSS Done!\n")		
		BSkyFunctionWrapUp()	
	#return(BSkyReturnStructure())
}

##uses haven for reading SPSS files
UAloadSPSSinDataFrame.haven <- function(SPSSfileName, datasetname, replace=FALSE, loadMissingValue=FALSE, trimSPSStrailing=FALSE, max.factor=Inf, encoding=NULL)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetname)
	
	BSkyErrMsg = paste("UAloadSPSSinDataFrame: Error loading SPSS file : ", "DataSetName :", datasetname," ", "SPSS filepath & filename  :", paste(SPSSfileName, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAloadSPSSinDataFrame: Warning loading SPSS file : ", "DataSetName :", datasetname," ", "SPSS filepath & filename   :", paste(SPSSfileName, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success = -1;
		# New Dataset name is only added if you want to replace existing, Or you will check that it should not already be present
		curidx <- UAgetIndexOfDataSet(datasetname) # current index of dataset if its already loaded ( before )
		if((replace == TRUE) || (replace == FALSE && curidx == -1))
		{
			# if i>0 then dataSetName already exists && you want to replace it.
			if(replace == TRUE && curidx > 0)
			{
				# Delete the existing dataset from the global list first. 
				# if dataset already exists, its index is in curidx.
				BSkyCloseForReloading(datasetname)
				#cat("DS Closed")
			}

			missingval <- NULL
			# cat("\nBefore Try Catch\n")
			corecommand=c()
			#R command to open data file (SPSS)
			if(is.null(encoding))
			{
				corecommand = paste('haven::read_sav(file=\'',SPSSfileName,'\')',sep='')
				# opendatafilecmd = paste('.GlobalEnv$',datasetname,' <- as.data.frame( read_sav(file=\'',SPSSfileName,'\'))',sep='')
			}
			else{
				corecommand = paste('haven::read_sav(file=\'',SPSSfileName,'\', encoding=\'',encoding,'\')', sep='')
				# opendatafilecmd = paste('.GlobalEnv$',datasetname,' <- as.data.frame( read_sav(file=\'',SPSSfileName,'\', encoding=\'',encoding,'\'))',sep='')
			}
			opendatafilecmd = paste('.GlobalEnv$',datasetname,' <- as.data.frame( ',corecommand,')', sep='')
			#reset global error-warning flag
			eval(parse(text="bsky_opencommand_execution_an_exception_occured = FALSE"), envir=globalenv())
			#trying to open the datafile
			tryCatch({
			
					withCallingHandlers({
						eval(parse(text = opendatafilecmd))
					}, warning = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
			}, error = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
			
			if(bsky_opencommand_execution_an_exception_occured == FALSE)## Success
			{
				success = 0
				## maybe return 0 for success
				cat("\nSuccessfully opened using:\n") 
				print(corecommand) #no need to print this
			}
			else ## Failure
			{
				print(paste('Current system encoding: cp',l10n_info()$codepage,sep=''))
				# print(paste('SysEncoding: ',l10n_info()$codepage,sep=''))
				cat("\nError opening file:\n") 
				# cat("\n\nCommand executed:\n")
				print(corecommand)
				## gracefully report error to the app layer about the issue so it does not keep waiting. 
				## maybe return -1 for failure
				success = -1;
			}			

			##following line was in use before(11Nov2021) above tryCatch
			# eval( parse(text=paste('.GlobalEnv$',datasetname,' <- as.data.frame( read_sav(file=\'',SPSSfileName,'\',))',sep='')))
			if(success == 0)
			{
			colcount = eval(parse(text=paste('ncol(',datasetname,')')))
			for(i in 1:colcount)
			{
				coluname = eval(parse(text=paste('colnames(',datasetname,')[',i,']')))
				colclass = eval(parse(text=paste('class(',datasetname,'$',coluname,')')))

				if(colclass[1] == "haven_labelled")##"haven_labelled" "vctrs_vctr"     "double"
				{
					eval(parse(text=paste('.GlobalEnv$',datasetname,'$',coluname,'<- as_factor(',datasetname,'$',coluname,')',sep='' )))
				}
			}
			uadatasets$name <- c(uadatasets$name, datasetname)

			#cat("\nLoaded dataset :", datasetname)
			DataSetIndex <- UAgetIndexOfDataSet(datasetname)
				 if(loadMissingValue)# load missing only when flag is on. No need to create blank missing values for each col.
				 {
				if(is.null(missingval) || missingval=="" )##no missing values. then set all col to "none"
				{
					colmisattr <- list()
					colcount = eval(parse(text=paste('ncol(',datasetname,')')))
					for(i in 1:colcount)
					{
						coluname = eval(parse(text=paste('colnames(',datasetname,')[',i,']')))
						colmisatt <- eval(parse(text=paste('list(type="none", value="")')))
						eval(parse(text=paste('setattr(','.GlobalEnv$',datasetname,', "misvals_',coluname,'",  colmisatt )',sep='' )))#attr for 
					}
				}	
				else #if SPSS file already has missing values. Set them in new misvals_COLNAME attribute(dataset level)
				{
					colmisattr <- list()
					colcount = eval(parse(text=paste('ncol(',datasetname,')')))
					for(i in 1:colcount)
					{
						colIndex = i
						coluname = eval(parse(text=paste('colnames(','.GlobalEnv$',datasetname,')[',i,']')))
						colexists <- eval(parse(text=paste('coluname %in% names(missingval)',sep='')))
						if (colexists)##Checking if colname exists in attribute
						{ 
							spsscolmisatt <- eval(parse(text=paste('missingval[',colIndex,']',sep=''))) # $type and $value
							mistype <- eval(parse(text=paste('missingval[[',colIndex,']]$type',sep='')))
							if(is.null(mistype) || mistype == "" || mistype == "none")
							{
								colmisatt <- eval(parse(text=paste('list(type="none", value="")')))
							}
							else
							{
								misvals <- eval(parse(text=paste('missingval[[',colIndex,']]$value',sep='')))
								colmisatt <- eval(parse(text=paste('list(type=mistype, value=misvals)')))
							}
						}
						else #some col those do not have missing attributes, create defaults for them
						{
							colmisatt <- eval(parse(text=paste('list(type="none", value="")')))
						}
						eval(parse(text=paste('setattr(','.GlobalEnv$',datasetname,', "misvals_',coluname,'",  colmisatt )',sep='' )))#attr for 
					}					
				}
			}# load missing only when flag is ON.
				
			UAcreateExtraAttributes(datasetname, "SPSS")
			}
		}
		else
		{
			warning("UAloadSPSSinDataFrame: Dataset with the same name already on the global list ")
		}
					# }
		#cat("\nLoading SPSS Done!\n")		
		BSkyFunctionWrapUp()	
		return(success)
		#return(BSkyReturnStructure())
}

BSkyOpenDatafileCommandErrWarnHandler <- function(m)
{
	#print(str(m))

	if("error" %in% attr(m, "class")) ##quit
	{
		eval(parse(text="bsky_opencommand_execution_an_exception_occured = TRUE"), envir=globalenv())
		cat("\n")
		message("Error: ", as.character(m$message))
		
		#print(sys.calls()) #to print the stack trace - not very helpful 
	}
	else if("warning" %in% attr(m, "class"))
	{
		message("Warning: ", as.character(m$message))
	}
	else
	{
		message("Msg: ", as.character(m$message))
	}
}

# WRITE ROLLBACK FUNCTION, IF CREATEATRRIBUTE FAILS THEN U "may" NEED TO ROLLBACK AND REMOVE THE CURRENT LATEST LOADED DATASET FOR WHICH ATTIBUTES FAILED TO CREATE
# internal function called from UAloadSPSSinDataFrame, to create following extra attributes
# coldesc, usermissing, missingvalues, split
UAcreateExtraAttributes <- function(dataSetNameOrIndex, filetype, usehaven=TRUE) 
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAcreateExtraAttributes: Error creating extra attributes : ", "DataSetName :", dataSetNameOrIndex," ", "Extra attributes : coldesc, usermissing, missingvalues, split", sep="")
	BSkyWarnMsg = paste("UAcreateExtraAttributes: Warning creating extra attributes : ", "DataSetName :", dataSetNameOrIndex," ", "Extra attributes : coldesc, usermissing, missingvalues, split",sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	#cat("\nPrinting Data View NAMES Col..\t",dataSetNameOrIndex,"\n")
	datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	# uadatasets$lst[[DataSetIndex]][[colIndex]]
	
	#datasetname <- paste(".GlobalEnv$", datasetname1, sep='')
	
	#### Creatting some Dataset level attributes  #####
	eval(parse(text=paste('setattr(',datasetname,', "split", c(FALSE) )' )))
	#cat("\nsplit set.\t")
	eval(parse(text=paste('setattr(',datasetname,', "splitcolumnnames", c("") )' )))
	#cat("splitcolumnnames set.\t")
	eval(parse(text=paste('setattr(',datasetname,', "splitcolumnindex", c("") )' )))
	#cat("splitcolumnindex set.\t")
	eval(parse(text=paste('setattr(',datasetname,', "filetype", filetype)' )))
	#cat("filetype set.\t", filetype)
	eval(parse(text=paste('setattr(',datasetname,', "slice", c(FALSE) )' )))
	#cat("slice set.\t")
	eval(parse(text=paste('setattr(',datasetname,', "maxfactor",', bskymaxfactors,' )' )))			
	#cat("\nMaxfactor set")

	noOfCols = eval(parse(text=paste('ncol(',datasetname,')')))
	#cat("\nNo. of Cols:",noOfCols)
	#cat("\nCreating Extra Attr fo all cols :",datasetname)
	#print(Sys.time())			 
	for(colIndex in 1:noOfCols)
	{
		label <- ""  #putting colname here instead of blank would be better. 
		#filetype <- eval(parse(text=paste('attr(',datasetname,',"filetype")')))
		#cat("\nFile Type:",filetype,"\n")
		if(filetype == "SPSS" && !usehaven)# should only be used if foreign is used for opening SPSS
		{
			colName = eval(parse(text=paste('colnames(',datasetname,')[',colIndex,']',sep='')))
			label = UAgetSPSSVariableView_Label(datasetname,colName)
		}
		if(filetype == "SAS")
		{
		}

		## few more attributes those should be not created when loading RDATA file. 
		## Because RDATA already have them (as we created RDATA files by SaveAs)  but other formats dont have ##
		if(filetype != "RDATA" && filetype != "RDA")
		{
			eval(parse(text=paste('setattr(',datasetname,'[,',colIndex,'], "coldesc", label)' ,sep='')))
			eval(parse(text=paste('setattr(',datasetname,'[,',colIndex,'], "usermissing", c(FALSE) )' , sep='')))
			eval(parse(text=paste('setattr(',datasetname,'[,',colIndex,'], "missingvalues", NULL )' , sep='' )))
			eval(parse(text=paste('setattr(',datasetname,'[,',colIndex,'], "split", c(FALSE) )' ,sep='')))
			eval(parse(text=paste('setattr(',datasetname,'[,',colIndex,'], "levelLabels", c("") )', sep='' )))		
		
			eval(parse(text=paste('setattr(',datasetname,'[,',colIndex,'], "width",c(4) )' ,sep='')))
			eval(parse(text=paste('setattr(',datasetname,'[,',colIndex,'], "decimals",c(0))' , sep='')))
			eval(parse(text=paste('setattr(',datasetname,'[,',colIndex,'], "columns",c(8) )' , sep='' )))
			eval(parse(text=paste('setattr(',datasetname,'[,',colIndex,'], "align",c("Left") )' ,sep='')))
			eval(parse(text=paste('setattr(',datasetname,'[,',colIndex,'], "measure",c("Scale"))', sep='' )))
			eval(parse(text=paste('setattr(',datasetname,'[,',colIndex,'], "role", c("Input") )', sep='' )))
		}
		#cat("\nCol Index:",colIndex)
		#cat("\nCreating Extra Attr :",datasetname)
		#print(Sys.time())
	}
	#cat("Extra Attr Created:",datasetname)
	#print(Sys.time())	
	# return(dataView_Names)
	# cat("\nReturning From here")
	BSkyFunctionWrapUp()	
}


BSkygetMissing<-function(dataSetNameOrIndex, colName)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkygetMissing: Error getting Missing : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colName, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkygetMissing: Warning getting Missing : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colName, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	missingv <- list()
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{		
				#Error : dataSetName not found
				#colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				colIndex<-BSkyValidateColumn(datasetname, colName)
				if(colIndex > 0){
					missing.type <- eval(parse(text=paste('attr(',datasetname,',"missings")[[',colIndex,']][[1]]',sep='')))
					if(missing.type != "none")
					{
						missing.values <- eval(parse(text=paste('attr(',datasetname,',"missings")[[',colIndex,']][[2]]',sep='')))
					}
					else
					{
						missing.values <- ""
					}
					missingv <- list(type = missing.type, value = missing.values)
					#- return(missingv)
				}			
				else
				{
					# cat("\nError: Cannot get SPSS Missing. Col not found\n")
					BSkyErrMsg =paste("BSkygetMissing: Cannot get SPSS Missing. Col not found."," Col Name:", colName)
					warning("BSkygetMissing: Cannot get SPSS Missing. Col not found.")
				}
			}
			else
			{
				# cat("\nError: Cannot get SPSS Missing. Dataset name or index not found\n")
				BSkyErrMsg =paste("BSkygetMissing: Cannot get SPSS Missing. Dataset name or index not found."," Dataset Name:", datasetname)
				warning("BSkygetMissing:  Cannot get SPSS Missing. Dataset name or index not found.")
			}		
			BSkyFunctionWrapUp()		
	return(invisible(missingv))
}




#############################################################################################################
#Function: UAgetSPSSVariableView_Label(dataSetNameOrIndex, colName)										
#              																			
#Parameters:  
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colName     				- Name of the column whose Label is needed         																
#																						
#Description:	
#		Returns LABEL column entry of SPSS variable view for specific column.																		
#																						
#Example: UAgetSPSSVariableView_Label("mydataset","city")																			
#############################################################################################################
UAgetSPSSVariableView_Label <- function(dataSetNameOrIndex, colName)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAgetSPSSVariableView_Label: Error getting col description : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colName, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAgetSPSSVariableView_Label: Warning getting col description : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colName, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	dataView_Label <- ""
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{		
			#cat("Label start:",datasetname)
			#print(Sys.time())
				#Error : dataSetName not found
				#colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				colIndex<-BSkyValidateColumn(datasetname, colName)
				nooflables <- eval(parse(text=paste('length(attr(',datasetname,',"variable.label"))' )))
				if(colIndex > 0 && nooflables>=colIndex){
					dataView_Label <- eval(parse(text=paste('attr(',datasetname,',"variable.label")[[',colIndex,']]',sep='')))
					#-return(dataView_Label)
				}			
				else
				{
					# cat("\nError: Cannot get SPSS Lable. Col not found\n")
					BSkyErrMsg =paste("UAgetSPSSVariableView_Label: Cannot get SPSS Lable. Col not found."," Col Name:", colName)
					warning("UAgetSPSSVariableView_Label: Cannot get SPSS Lable. Col not found.")
				}
			#cat("Label end:",datasetname)
			#print(Sys.time())
			}
			else
			{
				# cat("\nError: Cannot get SPSS Lable. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAgetSPSSVariableView_Label: Cannot get SPSS Lable. Dataset name or index not found."," Dataset Name:", datasetname)
				warning("UAgetSPSSVariableView_Label: Cannot get SPSS Lable. Dataset name or index not found.")
			}			
		BSkyFunctionWrapUp()
return(invisible(dataView_Label))
}


#############################################################################################################
#Function: 	UAsetSPSSColNames(dataSetNameOrIndex)											
#              																			
#Parameters: 
#		dataSetNameOrIndex			 - dataset name that was given when SPSS file was loaded in memory
#																						
#Description:
#		Set new name for any of the existing col.
#																						
#Example: UAsetSPSSColNames("mydataset")												
#																						
#############################################################################################################
UAsetSPSSColNames <- function(dataSetNameOrIndex, colName, newColName)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetSPSSColNames: Error in Setting Col Names : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colName,newColName, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetSPSSColNames: Warning in Setting Col Names : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colName,newColName, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
			#cat("\nPrinting Data View NAMES Col..\n")
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)

			if(!is.null(datasetname))
			{		
				#Error: Dataset not found
				#colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)	
				colIndex<-BSkyValidateColumn(datasetname, colName)
				if(colIndex > 0)
				{
					#cat("\nFound at index..\n")
					#cat(DataSetIndex)
					eval(parse(text=paste('colnames(',datasetname,')[',colIndex,'] <- ',newColName,')',sep='')))
					#cat("\nDisplaying now..\n")
					#cat(dataView_Names)
					#cat("\n*** End ***\n")

					# return(dataView_Names)
				}
				else
				{
					# cat("\nError: Cannot set SPSS Col Names. Col not found\n")
					BSkyErrMsg =paste("UAsetSPSSColNames: Cannot set SPSS Col Names. Col not found."," Col Name:", colName)
					warning("UAsetSPSSColNames: Cannot set SPSS Col Names. Col not found.")
				}				
			}
			else
			{
				# cat("\nError: Cannot set SPSS Col Names. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAsetSPSSColNames: Cannot set SPSS Col Names. Dataset name or index not found."," Dataset Name:", datasetname)
				warning("UAsetSPSSColNames: Cannot set SPSS Col Names. Dataset name or index not found.")
			}			

		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level UAsetSPSSColNames function\n")
		return(invisible(BSkyReturnStructure()))
}


#Adding a new Column . not complete
UAaddSPSSColumn <- function(dataSetNameOrIndex, newcolName)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAaddSPSSColumn: Error in Add Column : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(newcolName, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAaddSPSSColumn: Warning in Add Column : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(newcolName, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{

		withCallingHandlers(
		{
			#cat("\nPrinting Data View NAMES Col..\n")
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
			if(!is.null(datasetname))
			{		
				#Error: Dataset not found
				#colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)	
				colIndex<-BSkyValidateColumn(datasetname, colName)
				if(colIndex > 0)
				{
					#cat("\nFound at index..\n")
					#cat(DataSetIndex)
					colnames(datasetname)[colIndex] <- c(newColName)	 ###### USE EVAL HERE WHEN LOGIC IS READY
					# eval(parse(text=paste('
					
					#cat("\nDisplaying now..\n")
					#cat(dataView_Names)
					#cat("\n*** End ***\n")

					# return(dataView_Names)
				}
				else
				{
					# cat("\nError: Cannot add SPSS col. Col not found\n")
					BSkyErrMsg =paste("UAaddSPSSColumn: Cannot add SPSS col. Col not found."," Col Name:", newcolName)
					warning("UAaddSPSSColumn: Cannot add SPSS col. Col not found.")
				}				
			}
			else
			{
				# cat("\nError: Cannot add SPSS col. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAaddSPSSColumn: Cannot add SPSS col. Dataset name or index not found."," Dataset Name:", datasetname)
				warning("UAaddSPSSColumn: Cannot add SPSS col. Dataset name or index not found.")
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
			
			# cat("Error caught in UAaddSPSSColumn \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level add col function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAaddSPSSColumn \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level add col function\n")
		return(invisible(BSkyReturnStructure()))
}

#not complete
#############################################################################################################
#Function: UAsetSPSSColType(dataSetNameOrIndex,colName)										
#              																			
#Parameters:   
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory
#		colName     				- Name of the column whose Type is needed
#																						
#Description:	
#		Returns TYPE column entry of SPSS variable view for specific column.
#																						
#Example: UAsetSPSSColType("mydataset", "age")													
#																						
#############################################################################################################
UAsetSPSSColType <- function(dataSetNameOrIndex,colName)# not done
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetSPSSColType: Error in Col Type : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colName, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetSPSSColType: Warning in Col Type : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colName, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
			
			if(!is.null(datasetname))
			{		
				#Error: Dataset not found
				#colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)	
				colIndex<-BSkyValidateColumn(datasetname, colName)
				if(colIndex > 0){
					colType <- eval(parse(text=paste('class(',datasetname,'[[',colIndex,']])',sep='')))
				}			
				else
				{
					# cat("\nError: Cannot set SPSS Col Type. Col not found\n")
					BSkyErrMsg =paste("UAsetSPSSColType: Cannot set SPSS Col Type. Col not found."," Col Name:", colName)
					warning("UAsetSPSSColType: Cannot set SPSS Col Type. Col not found.")
				}
			}
			else
			{
				# cat("\nError: Cannot set SPSS Col Type. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAsetSPSSColType: Cannot set SPSS Col Type. Dataset name or index not found."," Dataset Name:", dataSetNameOrIndex)
				warning("UAsetSPSSColType: Cannot set SPSS Col Type. Dataset name or index not found.")
			}			

	#-return(colType)
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level set col type function\n")
		 return(invisible(colType)) #return(BSkyReturnStructure(colType))
}


#############################################################################################################
#Function: UAsetSPSSColLevels(dataSetNameOrIndex, colName)									
#              																			
#Parameters:  
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colName     				- Name of the column whose Levels are needed              																
#																						
#Description:	
#		To add the new level to old list of levels or if Replace is TRUE, new level(s) will replace old list completely
#																						
#Example: UAsetSPSSColLevels("mydataset","gender")																			
#																						
#############################################################################################################
UAsetSPSSColLevels <- function(dataSetNameOrIndex, colName, newLevel, Replace=FALSE)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetSPSSColLevels: Error in Set col Levels : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colName,newLevel, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetSPSSColLevels: Warning in Set col Levels : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colName,newLevel, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
		
			if(!is.null(datasetname))
			{		
				#Error: dataSetName not found
				#colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				colIndex<-BSkyValidateColumn(datasetname, colName)
				if(colIndex > 0){
					isfactor <- eval(parse(text=paste('is.factor(',datasetname,'[[',colIndex,']]',sep='')))
					if(isfactor) 
					{
						if(Replace == FALSE)
						{			
							eval(parse(text=paste('levels(',datasetname,'[[',colIndex,']]) <- c(levels(',datasetname,'[[',colIndex,']]), c(',newLevel,'))',sep='')))
						}
						else
						{
							eval(parse(text=paste('levels(',datasetname,'[[',colIndex,']]) <-  c(',newLevel,')',sep='')))
						}
					}
					else
					{
						# cat("\nCol is not factor type\n")
						warning("UAsetSPSSColLevels: Col is not factor type.")
					}
					# return(colLevels)
				}		
				else
				{
					# cat("\nError: Cannot set Col levels. Col not found\n")
					BSkyErrMsg =paste("UAsetSPSSColLevels: Cannot set Col levels. Col not found."," Col Name:", colName)
					warning("UAsetSPSSColLevels: Cannot set Col levels. Col not found.")
				}
			}
			else
			{
				# cat("\nError: Cannot set Col levels. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAsetSPSSColLevels: Cannot set Col levels. Dataset name or index not found."," Dataset Name:", datasetname)
				warning("UAsetSPSSColLevels: Cannot set Col levels. Dataset name or index not found.")
			}		

		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level set col level function\n")
		#return(BSkyReturnStructure())
}

#not complete
#set any non factor column to type "factor"
UAsetColasFactor <- function(dataSetNameOrIndex, colName)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetColasFactor: Error in set col as factor : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colName, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetColasFactor: Warning in se col as factor : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colName, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{
		withCallingHandlers(
		{
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
		
			if(!is.null(datasetname))
			{		
				#Error: dataSetName not found
				#colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				colIndex<-BSkyValidateColumn(datasetname, colName)
				if(colIndex > 0){
					eval(parse(text=paste(,datasetname,'[',colIndex,'] <-factor(',datasetname,'[',colIndex,'])',sep='')))
					# class(uadatasets$lst[[DataSetIndex]][[colIndex]]) <-  "factor"
					# Error in class(uadatasets$lst[[1]][[10]]) <- "factor" : 
					# adding class "factor" to an invalid object
					# return(colLevels)

				}		
				else
				{
					# cat("\nError: Cannot set Col as Factor. Col not found\n")
					BSkyErrMsg =paste("UAsetColasFactor: Cannot set Col as Factor. Col not found."," Col Name:", colName)
					warning("UAsetColasFactor: Cannot set Col as Factor. Col not found.")
				}

			}
			else
			{
				# cat("\nError: Cannot set Col as Factor. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAsetColasFactor: Cannot set Col as Factor. Dataset name or index not found."," Dataset Name:", datasetname)
				warning("UAsetColasFactor: Cannot set Col as Factor. Dataset name or index not found.")
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
			
			# cat("Error caught in UAsetColasFactor \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level set col as factor function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAsetColasFactor \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level set col as factor function\n")
		return(invisible(BSkyReturnStructure()))
}


#not complete
#############################################################################################################
#Function: UAsetSPSSColMeasure(dataSetNameOrIndex, colName)														
#              																			
#Parameters:        
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colName     				- Name of the column whose Measure values are needed          																
#																						
#Description:	
#		Returns the entry from MEASURE column of SPSS variable view for a specific column																		
#																						
#Example: UAsetSPSSColMeasure("mydataset","gender")																			
#																						
#############################################################################################################
UAsetSPSSColMeasure <- function(dataSetNameOrIndex, colName)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetSPSSColMeasure: Error  : ", "DataSetName :", dataSetNameOrIndex," ", "Variable  :", paste(colName, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetSPSSColMeasure: Warning  : ", "DataSetName :", dataSetNameOrIndex," ", "Variable :", paste(colName, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	tryCatch(
		{

		withCallingHandlers(
		{
#################### WRITE MAIN LOGIC HERE ################
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
			
			# cat("Error caught in UAsetSPSSColMeasure \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level sort function\n")
	if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAsetSPSSColMeasure \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function UAsetColMeasure
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level sort function\n")
		return(invisible(BSkyReturnStructure())) #
}



#not complete
#############################################################################################################
#Function: UAsetSPSSColData(dataSetNameOrIndex, colName)											
#              																			
#Parameters:     
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colName     				- Name of the column whose data is needed          																
#																						
#Description:
#		Returns the complete data that belongs to a specific column																			
#																						
#Example: UAsetSPSSColData("mydataset","gender")																			
#																						
#############################################################################################################
UAsetSPSSColData <- function(dataSetNameOrIndex, colName)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetSPSSColData: Error in set col data : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colName, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetSPSSColData: Warning in set col data : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colName, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{

		withCallingHandlers(
		{
			##cat("Finding DataSet \n")		
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
		
			if(!is.null(datasetname))
			{		
				##cat("Found dataset index:",DataSetIndex,"\n")
				#Error: dataSetName not found
				#colIndex <- UAgetIndexOfColInDataSet(DataSetIndex,colName)
				colIndex<-BSkyValidateColumn(datasetname, colName)
				##cat(colIndex,"\n:-) ")
				if(colIndex > 0){
					col.int.vector <- eval(parse(text=paste('unclass(',datasetname,'[[',colIndex,']])',sep='')))
					#-return(col.int.vector)
				}
				else
				{
					# cat("\nError: Cannot set SPSS Col Data. Col not found\n")
					BSkyErrMsg =paste("UAsetSPSSColData: Cannot set SPSS Col Data. Col not found."," Col Name:", colName)
					warning("UAsetSPSSColData: Cannot set SPSS Col Data. Col not found.")
				}					
			}
			else
			{
				# cat("\nError: Cannot set SPSS Col Data. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAsetSPSSColData: Cannot set SPSS Col Data. Dataset name or index not found."," Dataset Name:", datasetname)
				warning("UAsetSPSSColData: Cannot set SPSS Col Data. Dataset name or index not found.")
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
			
			# cat("Error caught in UAsetSPSSColData \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level set col data function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAsetSPSSColData \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level set col data function\n")
		return(invisible(BSkyReturnStructure(list(extra=c(col.int.vector)))))  #return(col.int.vector)
}

#not complete
#############################################################################################################
#Function: UAsetSPSSColProperties(dataSetNameOrIndex, colname)										
#              																			
#Parameters:        
#		dataSetNameOrIndex			- dataset name that was given when SPSS file was loaded in memory  
#		colName     				- Name of the column whose propeties are needed       																
#																						
#Description:
#		Returns all column properties of a specific column																		
#																						
#Example: UAsetSPSSColProperties("mydataset","gender")																			
#																						
#############################################################################################################
UAsetSPSSColProperties <- function(dataSetNameOrIndex, colName, colType, colLabel, colLevel, colMissing, colMeasure)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAsetSPSSColProperties: Error in set col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colName, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAsetSPSSColProperties: Warning in set col properties : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colName, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	tryCatch(
		{

		withCallingHandlers(
		{
			#Error: dataSetName and colname not found
			#  colType, colLabel, colMeasure
			UAsetSPSSColNames(dataSetNameOrIndex, colName)
			#cat("\nName:",colName)
			
			#set col type
			colType <- UAsetSPSSColType(dataSetNameOrIndex, colName)
			#cat("\nType:",colType)
			
			#set col label
			#UAsetColDesc(dataSetNameOrIndex, colName, colDesc)
			#cat("\nLabel:",colLabel)
			
			#set col values, factor part
			# colValues_factor <- UAsetSPSSVariableView_Factor(dataSetNameOrIndex, colname)
			#cat("\nFactor:",colValues_factor)
			
			#set col values, level part
			UAsetSPSSColLevels(dataSetNameOrIndex, colName, colLevel, Replace=FALSE)
			#cat("\nLevels:",colValues_level)
			
			#set col missing
			UAsetColMissing(dataSetNameOrIndex, colName, colMissing)
			#cat("\nMissing:",colMissing)
			
			#set col measure
			colMeasure <- list()
			#cat("\nMeasure:",colMeasure)
			#cat("\n\n")
			#Create two col
			##propCol1 <- c( "Name", "Type", "Label", "Factor", "Level", "Missing", "Measure" )
			##propCol2 <- c(colName, colType, colLabel, list(colValues_factor), list(colValues_level), colMissing, colMeasure)

			#Create an object with two col
			# return(colProperties)
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
			
			# cat("Error caught in UAsetSPSSColProperties \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level set col properties function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in UAsetSPSSColProperties \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level set col properties function\n")
		return(invisible(BSkyReturnStructure()))
}


## Write to SPSS .sav format using rio R package
BSkywriteSAV <- function(savfilename, dataSetNameOrIndex)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("BSkywriteSAV: Error writing SAV file : ", "DataSetName :", dataSetNameOrIndex," ", "SAV filename  :", paste(savfilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSkywriteSAV: Warning writing SAV file : ", "DataSetName :", dataSetNameOrIndex," ", "SAV filename  :", paste(savfilename, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success=-1
			##dataset saved to SAV file is from uadatasets
	datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
			
	if(!is.null(datasetname))
	{		
		require(rio)
		# eval(parse(text=paste('export(',datasetname,', savfilename)')))
		#above line was in use before adding tryCatch below
		
		corecommand = paste('rio::export(',datasetname,', savfilename)')
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
			cat("\nError saving file:\n") 
			# cat("\n\nCommand executed:\n")
			print(corecommand)
			## gracefully report error to the app layer about the issue so it does not keep waiting. 
			## maybe return -1 for failure
			success = -1;
		}		
		
	}
	else
	{
		BSkyErrMsg =paste("BSkywriteSAV:  Cannot write SAV. Dataset name or index not found."," Dataset Name:", dataSetNameOrIndex)
		# cat("\nError: Cannot write CSV. Dataset name or index not found\n")
		warning("BSkywriteSAV:  Cannot write SAV. Dataset name or index not found.")
	}			
		BSkyFunctionWrapUp()	
				
	return(success)
}


