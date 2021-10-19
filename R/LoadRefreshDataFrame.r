#For loading and refreshing in memory dataframe. (not a disk file dataframe)
# dframe is the dataframe name and is a string.
BSky.LoadRefresh.Dataframe <- function(dframe)
{
	#print(environment())
	#print( parent.frame())
	#print(globalenv())
	#print("Summ Exists")
	#print(exists("summary", envir = parent.frame(), inherits = FALSE))
	#print(class(summary))


	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dframe)
	success <- NULL
	logflag=FALSE
	#print("BSky Load Refresh starts..")
	#print(eval( parse(text=paste('class(',dframe,')',sep=''))))#.GlobalEnv$
	#BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	curidx <- UAgetIndexOfDataSet(dframe) # current index of dataset if its already loaded ( before )
	filetype = "RDATA" #default
	#print("BSky Load Refresh..")
	#cat("\nCur Index :",curidx)
	
	############
	# 20-Jan-2021 Sanjay and I (Anil) Concluded that following code is unnecessary because it is not executing the 'if' condition
	# anyway and that is why the old code (#30Dec2020) happens to work fine.
	# The new code (#20Jan2021) appear like a logical approach bu then it convertes dframe from string to actual dataframe which
	# in turn makes rest of the calls to fail (becuase all that code expects dframe to be a string.
	# Here is an example that we tried
	#
		#df = data.frame(A=c(1,2,3), B=c(4,5,6), C=c(11,22,33))
		# tbdf = as_tibble(df)
		# class(tbdf)
		# class(tbdf) = c("tbl_df") #make tbdf a list

		# df2 = as.data.frame(tbdf)
		# class(df2)

		# dframe2= "tbdf"
		# dfstr= "df"

		# dfclass = eval( parse(text=paste('class(.GlobalEnv$',dfstr,')',sep='')))
		# eval(parse(text=paste('hascolumns <- ncol(.GlobalEnv$',dframe2,') > 0')))

		# ## [Aggregate to data set]
		# require(dplyr);
		# #Aggregate the dataset
		# test<-mtcars %>%
			 # dplyr::group_by(transmission,region) %>%
			 # dplyr::summarize(mean_mpg=mean(mpg,na.rm =TRUE))
		# class(test)
		# is.data.frame(test)
		# dframe='test'
		# dfclass = class(dframe)
		# tbl_df_index = which(dfclass == 'tbl_df')
		# length(tbl_df_index) > 0
		# eval( parse(text=paste('is.data.frame(',dframe,')',sep='')))
		# eval(parse(text=paste('hascolumns <- ncol(.GlobalEnv$',dframe,') > 0')))
		# dframe = as.data.frame(dframe)
	#
	#We may have to come back and fix the following commented code if our dframe class is only tbl_df and does not contain data.frame class.
	# # #if dframe class is tbl_df then we convert it to data.frame type
	 # # dfclass = class(dframe) #30Dec2020. This does not look right. dframe is supposed to be string. Fixed, line below
	 # # #20Jan2021 dfclass = eval( parse(text=paste('class(.GlobalEnv$',dframe,')',sep='')))
	 # # tbl_df_index = which(dfclass == 'tbl_df')
	 # # if( length(tbl_df_index) > 0)#length is empty if tbl_df is not found
	 # # {
		 # # dframe = as.data.frame(dframe)#30Dec2020. This does not look right. dframe is supposed to be string. Fixed, line below
		# # # #20Jan2021 dframe = eval( parse(text=paste('as.data.frame(.GlobalEnv$',dframe,')',sep='')))
	 # # }
	############
		
	#### CHANGE THIS TO ls()
	#if(eval( parse(text=paste('exists("dframe")',sep=''))))#exists("dframe")) 
	# # # if(eval( parse(text=paste('exists("',dframe,'", where=globalenv())',sep=''))))#exists(dframe))	in any env
	# # # {
		# # # if(eval( parse(text=paste('is.data.frame(',dframe,')',sep='')))) ##
		# # # {
			# # # ####29Jan2021  we do not need if check because we care making a local copy of the dataset
			# # # # if(!eval( parse(text=paste('exists("',dframe,'", where=globalenv(), inherits=FALSE)',sep=''))))
			# # # # {
				# # # eval( parse(text=paste('.GlobalEnv$',dframe,' <- as.data.frame(',dframe,')',sep=''))) # making a copy of the dataset that may be in some package. This is will also take care of "tibble"
			# # # # }
		# # # }
	# # # }
	
	### 01Feb2021 Whole if block above can be replaced by following commented line.
	BSkyCheckIfTibble(dataframeName=dframe) # converts to data.frame only if dataframe has tbl_df class
	
	#print("Step5")
	if(eval( parse(text=paste('exists("',dframe,'", where=globalenv(), inherits=FALSE)',sep=''))))#exists(dframe))
	{
		#cat("\nDataframe exists\n Is it a DataFrame : ")
		#print(ls())
		varenv <- eval( parse(text=paste('getEnv(dframe)',sep='')))
		#print(varenv)
		#print(eval( parse(text=paste('is.data.frame(.GlobalEnv$',dframe,')',sep='')))) #.GlobalEnv$
		#cat("\n")
		# if(eval( parse(text=paste('is.data.frame(',dframe,')',sep='')),envir=globalenv()))
		if(eval( parse(text=paste('is.data.frame(.GlobalEnv$',dframe,')',sep='')))) #.GlobalEnv$
		{
			#print("HasCol and Has row")
			eval(parse(text=paste('hascolumns <- ncol(.GlobalEnv$',dframe,') > 0'))) #20Oct2016 data.frame has cols
			#print("A")
			#print(hascolumns)
			eval(parse(text=paste('hasrows <- nrow(.GlobalEnv$',dframe,') > 0'))) #20Oct2016 data.frame has rows
			#print("B")
			#print(hasrows)
			if(hascolumns && hasrows)#20Oct2016 'if' condition added for the code below. And a full 'else' block is also added for this 'if'
			{
				#at this point we need to assign some column name(Var1, Var2) to the column those do not have any col-names.
				GenerateUniqueColName(paste('.GlobalEnv$',dframe,sep='') )#its string so no need to enclose in single quotes
				
				#Now replace special chars with underscore and prefix 'X' if there is digit in the first index  of colum name.
				colcount <- eval(parse(text=paste('length(names(.GlobalEnv$',dframe,'))' ))) #colcount = length(names(datasetname))
				#cat('Count=')
				#cat(colcount)
				#ptm= proc.time()
				for( i in 1:colcount)
				{
					if(logflag)
					{
						cat('\n-----------------------------------------------------------------')
						cat("\nSpl Chr:OldName = ")
						print(eval(parse(text=paste('names(.GlobalEnv$',dframe,')[',i,']',sep=''))))
					}			
					eval(parse(text=paste('names(.GlobalEnv$',dframe,')[',i,']  <- ReplaceSplChrsAndPrefixXForDigitInBegining(names(.GlobalEnv$',dframe,')[',i,'])', sep='')))
					#names(datasetname)[i] = ReplaceSplChrsAndPrefixXForDigitInBegining(names(datasetname)[i])
					
					if(logflag)
					{
						cat("\nSpl Chr removed ColName = ")
						print(eval(parse(text=paste('names(.GlobalEnv$',dframe,')[',i,']',sep=''))))
						cat('\n-----------------------------------------------------------------')
					}				
				}			
				
				
				#cat("\nIts Dataframe\n")
				#eval( parse(text=paste(dframe,' <<- dframe',sep='')))
				if(curidx == -1) # if not exists in list
				{
					#cat("\n adding to uadatasets$name:")
					uadatasets$name <- c(uadatasets$name, dframe)
					#cat("\n added to uadatasets$name:")

				}
				else
				{
					#cat("\nCreating filetype attribute\n")
					filetype <- eval(parse(text=paste('attr(.GlobalEnv$',dframe,',"filetype")',sep=''))) ## this may not work. because dframe is new object. may not have attributes.
					#bskymaxfactors <- eval(parse(text=paste('attr(',datasetname,',"maxfactor")',sep='')))
				}
				
				#cat("\n Creating Attrs for DS:")
				#eval(parse(text=paste('attributes(',dframe,') <<- c(attributes(',dframe,'), list(split = c(FALSE), splitcolumnnames = c(), splitcolumnindex = c(), filetype = c("CSV"), slice = c(FALSE), align = c("Left"), maxfactor= c(bskymaxfactors)))', sep='')))	
				if(is.null(filetype) || nchar(filetype)==0)
					filetype = "RDATA"
				#cat("File Type:", filetype)
				
				#eval(parse(text=paste('uadatasets$lst$',dframe,' <- .GlobalEnv$',dframe,sep='')))
				#cat("\n added to uadatasets$lst$dfname:")				

				UAcreateExtraAttributes(dframe, filetype)
				#cat("\n Finished Creating Attrs for DS:\n")
				success = "SUCCESS"
				#print(eval(parse(text=paste('colnames(',dframe,')'))))
				replacePeriodWithUnderscore(dframe)
				#print(eval(parse(text=paste('colnames(.GlobalEnv$',dframe,')'))))
				
				#cat("\nFixing UTC")
				#MakeAllDateColUTC(dframe)
				#cat("\nUTC Fixed")
			}
			else
			{
				cat("Dataframe object does not have row(s) or column(s)!\n")
				BSkyErrMsg =paste("BSky.LoadRefresh.Dataframe: Dataset has no row(s) or column(s)"," Dataset Name:", dframe)
				warning("BSky.LoadRefresh.Dataframe: Dataset has no row(s) or column(s) ")			
			}
		}
		else
		{
			cat("Object is not data frame object");
			warning("BSky.LoadRefresh.Dataframe: Object is not data frame object ")
		}
	}
	else
	{
		cat("Dataframe object does not exists!")
		BSkyErrMsg =paste("BSky.LoadRefresh.Dataframe: Dataset with the same name already on the global list"," Dataset Name:", dframe)
		warning("BSky.LoadRefresh.Dataframe: Dataset with the same name already on the global list ")
	}

	BSkyFunctionWrapUp()
	invisible(success)
}

#Closes a Dataframe, Makes it NULL
BSky.RemoveRefresh.Dataframe <- function(dframe)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dframe)

	#BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	#curidx <- UAgetIndexOfDataSet(dframe) # current index of dataset if its already loaded ( before )
	#cat("\nCur Index of CSV:",curidx)

	if(eval( parse(text=paste('exists("dframe")',sep=''))))#exists(dframe)) 
	{
		if(eval( parse(text=paste('is.null("dframe")',sep='')))) #Already closed from elsewhere
		{
			#Just remove external attributes
		}
		else if(eval( parse(text=paste('is.data.frame(',dframe,')',sep='')))) # not closed. Close it from here
		{
			UAcloseDataset(dframe)
			#remove external attributes
		}
		else
		{
			cat("Object is not data frame object");
		}
	}
	else
	{
		cat("Dataframe object does not exists!")
		BSkyErrMsg =paste("BSky.RemoveRefresh.Dataframe: Dataset with the same name already on the global list"," Dataset Name:", dframe)
		warning("BSky.RemoveRefresh.Dataframe: Dataset with the same name already on the global list ")
	}

	BSkyFunctionWrapUp()
	
}

BSkyLoadRefreshDataframe2 <- function(dframe)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dframe)
	success <- NULL
	curidx <- UAgetIndexOfDataSet(dframe) # current index of dataset if its already loaded ( before )
	filetype = "RDATA" #default
	
	#### CHANGE THIS TO ls()
	if(eval( parse(text=paste('exists("',dframe,'", where=globalenv())',sep=''))))#exists(dframe))
	{
		varenv <- eval( parse(text=paste('getEnv(dframe)',sep='')))
		#print(varenv)
		#print(eval( parse(text=paste('is.data.frame(.GlobalEnv$',dframe,')',sep='')))) #.GlobalEnv$
		#cat("\n")
		# if(eval( parse(text=paste('is.data.frame(',dframe,')',sep='')),envir=globalenv()))
		if(eval( parse(text=paste('is.data.frame(.GlobalEnv$',dframe,')',sep='')))) #.GlobalEnv$
		{
			eval(parse(text=paste('uadatasets$lst$',dframe,' <- .GlobalEnv$',dframe,sep='')))
			#cat("\n added to uadatasets$lst$dfname:")				

			#UAcreateExtraAttributes(dframe, filetype)
			#cat("\n Finished Creating Attrs for DS:\n")
			success = "SUCCESS"
			#print(eval(parse(text=paste('colnames(',dframe,')'))))
			#replacePeriodWithUnderscore(dframe)
			#print(eval(parse(text=paste('colnames(.GlobalEnv$',dframe,')'))))
		}
		else
		{
			cat("Object is not data frame object");
			warning("BSky.LoadRefresh.Dataframe: Object is not data frame object ")
		}
	}
	else
	{
		cat("Dataframe object does not exists!")
		BSkyErrMsg =paste("BSky.LoadRefresh.Dataframe: Dataset with the same name already on the global list"," Dataset Name:", dframe)
		warning("BSky.LoadRefresh.Dataframe: Dataset with the same name already on the global list ")
	}

	BSkyFunctionWrapUp()
	invisible(success)
}

# dframe must be a name (a string) rather than an actual dataframe object
##Refreshes values in C1Datagrid, if dataset/dataframe was modified from syntax.
#Basically loads/refreshes the grid from memory(dataset/dataframe). Not by reading disk file from disk.

# BSkyLoadRefreshDataframe loads the specified dataframe in the BlueSky Statistics UI datagrid. 
# If dataframe is already loaded in the UI datagrid then it refresh it for any changes done.
#
# To use BSkyLoadRefreshDataframe you must run it from within the BlueSky Statistics application.
#
# @name BSkyLoadRefreshDataframe
# @param dframe is the dataframe object(do not use quotes around the object)
# @param load.dataframe is provided so that you can selectively refresh or not refresh the UI datagrid. 
# This parameter is defined in the dialog to make the decision, whether to refresh the UI datagrid or not.
# Default value is TRUE.
# @export 
# @examples
# df <-data.frame(A=c(1,2,3), B=c(4,5,6), C=c(6,7,8))
# BSkyLoadRefreshDataframe(df)
BSkyLoadRefreshDataframe <- function(dframe, load.dataframe = TRUE)
{
	#if(load.dataframe)
	#{
		#invisible(dframe)
	#}
	#else
	#{
		#invisible(NULL)
	#}
	BSkyLoadRefresh(dframe, load.dataframe);
}


### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Loading a dataframe in the BlueSky Statistics UI datagrid
#'
#' @description loads the specified dataframe in the BlueSky Statistics UI datagrid. 
#'
#' @param bskyDatasetName is the dataframe name (use quotes around the name)
#' @param load.dataframe is provided so that you can selectively refresh or not refresh the UI datagrid. 
#' This parameter is defined in the dialog to make the decision, whether to refresh the UI datagrid or not. Default value is TRUE.
#'
#'
#' @examples 
#' df <-data.frame(A=c(1,2,3), B=c(4,5,6), C=c(6,7,8))
#' BSkyLoadRefresh('df')
BSkyLoadRefresh <- function (bskyDatasetName, load.dataframe = TRUE, isRmarkdownOutputOn = BSkyIsRmarkdownOutputOn())## change this to a string parameter from a dataset object 
{
	isdataframe=FALSE
	isPkgLoaded = FALSE
	isexists = FALSE
	error = FALSE
	pkgname = c()
	dsname = c()
	pkgEnv = c()
	ischar = is.character(bskyDatasetName) 
	if(ischar)## is argument a character. If not we print error in the sink and exit
	{
		hasPkgname = grepl("::", bskyDatasetName)
		
		if(hasPkgname)#package name is passed
		{
			arr = strsplit(bskyDatasetName, "::")
			pkgname = arr[[1]][1]
			dsname = arr[[1]][2]
			
			isPkgLoaded = pkgname %in% (.packages())
			
			if(!isPkgLoaded)# if packge is not loaded
			{
				cat("\n") # forcing a new line in case someone created a cat() without a trailing new line
				cat("Package ")
				cat(pkgname)
				cat(" not loaded. Use library(packageName) to load the package.")
				error=TRUE
				invisible()				
			}
			
			pkgEnv = eval(parse(text=paste('as.environment("package:',pkgname,'")', sep='')))
			
			isexists = eval(parse(text=paste('exists("',dsname,'", where=pkgEnv, inherits=FALSE)', sep='' )))# DS exists in the pkg.
			
			if(isexists)
			{
				#check if data.frame
				isdataframe = eval(parse(text=paste('"data.frame" %in% c(class(',pkgname,'::',dsname,'))',sep='')))
				
				if(isdataframe)
				{
					eval( parse(text=paste('.GlobalEnv$',dsname,' <- ',pkgname,'::',dsname,sep='')))##make a copy in globalEnv
					#dsname = 'mtcars'
					bskyDatasetName=dsname #overwrite bskyDatasetName as it may still have 'datasets::mtcars'
				}
			}
			else
			{
				cat("\n") # forcing a new line in case someone created a cat() without a trailing new line
				cat("Dataset ")
				cat(dsname)
				cat(" is not found in the package ")
				cat(pkgname)	
				error= TRUE
				invisible()
			}
			
		}
		else#package name is not passed so, whatever is passed should be considered as a datasetname in global env.
		{
			inGlobalEnv = eval( parse(text=paste('exists("',bskyDatasetName,'", where=globalenv(), inherits=FALSE)',sep='')))
			# if dataset is not found in the Global environment then we should not go searching it 
			# because same named datasets can be found in multiple packages. And user may get confused if we load
			# the first one we found. So we simply error out. User is supposed to pass package name to avoid ambiguity
			if(inGlobalEnv)
			{
				isdataframe = eval(parse(text=paste('"data.frame" %in% c(class(',bskyDatasetName,'))',sep='')))
				
				if ( !isdataframe ) #not a data.frame and the object exists
				{
					cat("\n") # forcing a new line in case someone created a cat() without a trailing new line
					cat("Syntax error (BSkyLoadRefresh expects the name of the data.frame object as a character string) or the object referenced is not an object of class data.frame")
					cat("\nCorrect syntax is:")
					cat("\n\tBSkyLoadRefresh('dataframe-name') #load an object of class data.frame")
					cat("\n\tBSkyLoadRefresh('packageName::dataframe-name') #load an object of class data.frame from a specific R package")
					error= TRUE
					invisible()
				}
				
			}
			else
			{
				isexists = eval(parse(text=paste('exists("'  ,bskyDatasetName,'")', sep='' )))# DS exists in the pkg.
				if (isexists)
				{
					eval( parse(text=paste('.GlobalEnv$',bskyDatasetName,' <- ',bskyDatasetName,sep='')))##make a copy in globalEnv
					#dsname = 'mtcars'
					isdataframe = eval(parse(text=paste('"data.frame" %in% c(class(',bskyDatasetName,'))',sep='')))
					
					if ( !isdataframe ) #not a data.frame and the object exists
					{
						cat("\n") # forcing a new line in case someone created a cat() without a trailing new line
						cat("Syntax error (BSkyLoadRefresh expects the name of the data.frame object as a character string) or the object referenced is not an object of class data.frame")
						cat("\nCorrect syntax is:")
						cat("\n\tBSkyLoadRefresh('dataframe-name') #load an object of class data.frame")
						cat("\n\tBSkyLoadRefresh('packageName::dataframe-name') #load an object of class data.frame from a specific R package")
						error= TRUE
						invisible()
					}
					
				}
				else
				{
					cat("\n") # forcing a new line in case someone created a cat() without a trailing new line
					cat("Dataset ")
					cat(bskyDatasetName)
					cat(" cannot be found. Please check the spelling of the dataset name or load the package containing the dataset")
					error= TRUE
					invisible()
				
				}
				
			}
			
		}
	}
	
	
	if(load.dataframe && !error)
	{
		# first check the whether the dataset exsits in the uadatasets list - if so no processing of the dataset is needed - Anil needs to verify this 
		# if the dataset name does not exist in the uadatasets, then do all prep (attrbutes, variables, etc) the dataset and add to the uadatasets
		
		#Following block is added by Anil. 28Nov2020
		#curidx <- UAgetIndexOfDataSet(bskyDatasetName) #strip out .GlobalEnv$ from the bskyDatasetName and then looks in to uadatasets$name
		#if(curidx == -1)
		#{
			#BSky.LoadRefresh.Dataframe(bskyDatasetName)#This is what we call from C# behind the scenes if the memory dataframe is a new one.
		#}
		
		##get dataframe object from the dataframename
		#dframeobj = eval( parse(text=paste('as.data.frame(.GlobalEnv$',bskyDatasetName,')',sep='')))
		##we can put dframeobj object=c(dframeobj)in the Sanjay's code below or we can just put the name object=c(bskyDatasetName)
		##if we put object then python gets the object directly. 
		##if we put name then python code must know that its a name and get object of that name from R memory and then process it.

		# if((exists("uadatasets.sk") && exists("BSkyKableFormatting", env=uadatasets.sk) && uadatasets.sk$BSkyKableFormatting == FALSE))
		# {
			# doKableFormatting = FALSE
		# }
		# else
		# {
			# doKableFormatting = TRUE
			
			# if((exists("uadatasets.sk") && exists("BSkyRmarkdownFormatting", env=uadatasets.sk) && uadatasets.sk$BSkyRmarkdownFormatting == FALSE))
			# {
				# doRmarkdownFormatting = FALSE
			# }
			# else
			# {
				# doRmarkdownFormatting = TRUE
			# }
			
			# isRmarkdownOutputOn = doRmarkdownFormatting
		# }
		
		# after the above processing do the following
		if(isRmarkdownOutputOn == FALSE)
		{
			cat("\n") # forcing a new line in case someone created a cat() without a trailing new line
			print("BSkyDataGridRefresh")
		}
		###################################################################################
		#11/24/20 - The following code is for creating an in memory queue instead of a sync file to signal the new (Python, etc) application tier
		####################################################################################
		
		if(!exists("holdBSkyFormatObjectNew", env=uadatasets.sk) || is.null(uadatasets.sk$holdBSkyFormatObjectNew))
		{
			uadatasets.sk$holdBSkyFormatObjectNew = list(list(type=c("BSkyDataGridRefresh"), object=c(bskyDatasetName)))
		}
		else
		{
			uadatasets.sk$holdBSkyFormatObjectNew = c(uadatasets.sk$holdBSkyFormatObjectNew, list(list(type=c("BSkyDataGridRefresh"), object=c(bskyDatasetName))))
		}
		
		###### 23 Jan 2021 ### Addin dataset in uadataset$name  ######
		datasetname <- BSkyValidateDataset(bskyDatasetName)
		if(is.null(datasetname))
		{
			uadatasets$name <- c(uadatasets$name, bskyDatasetName)
		}
		UAcreateExtraAttributes(bskyDatasetName, "RDATA")

	}
}

BSkyRemoveRefreshDataframe <- function(dframe)
{
	BSky.RemoveRefresh.Dataframe (dframe)
}
