############################################################
#Documentation of how drop.unused.levels in crosstable works
############################################################
#generating a subset of a dataset with factors completely missing in the layers of the cross tab
#this means the factors in the layers have several levels that are complete missing in the crosstab geberated
#The dataset is below
#UAloadDataset("c:/aaron/datasets/SPSSSamples/satisf1 to test splits split dataset with min rows.sav",datasetName="satisfsplits",filetype="SPSS") 
#tempdataset=subset(uadatasets$lst[[24]], uadatasets$lst[[24]][,1]==2)
#uatab <-xtabs(~store + overall + regular + contact,data=tempdataset,drop.unused.levels = FALSE)
#All the dimensions are correctly represented
#dim(uatab) #Values are 5 5 5 3
#Now lets take an example where the dimensions are not represented
#uatab <-xtabs(~store + overall + regular + contact,data=tempdataset,drop.unused.levels = TRUE)
#All the dimensions are correctly represented
#dim(uatab) #Values are 2 3 3 2


############################################################
#Things that need to be done
############################################################
#Test case of a dimension < 2. See code by search for "Valerie"
#Test cases for splits on multiple variables e.g. gender and regular
#DONE Need to return Vishal the levels of the columns that we are returning values for 
#Error handling with multiple rows and column variables
#ERROR handling to reflect the specific row variable, column variable and split
#The significance of varindex with the cartlevel and multiple row variables and column variabels
#The chi sq test in SPSS displays footers, 6 cells 100% have expected count less than 5, the minimum expected count is .43
#We need to understand the significance of displaying the above in the footer and the statistican should assist with this


#################################################################
#Cross tabs
#################################################################
#The arguments
# x -An array of row variables
#y -An array of column variables
# layers -An array of variables that the cross tab is to be layered by
#expected -These are the expected counts
#prop.r -Enable/disable row proportions
#prop.c -Enable/disable row proportions
#prop.t -Enable/disable total proportions ???
#prop.chisq -???
#Chisq -Perform a Chi sq test
#fisher -Perform a Fisher test
#Mcnemar -Perform a mcnemar test
#resid -Display residuals
#sresid -Display standard residuals
#aresid -Display ???
#missing.include=TRUE, ????
#dnn = NULL
#datasetname -Name of the dataset


#Return values
#The 1st return value tells us whether there is a split or not (1=Split, 0=No split)
#The 2nd return value tells us whether there is an error or not (-1 if there is an error, 0 if there is no error)
#The 3rd return value gives us the number of errors
#The 4th return value tells us how many warnings there are
#The 5th return value gives us all the log entries
#The 6th return value gives us the summary of the function
#The 7th return value if there is no split returns the data associated with the crosstab
#The 8th return value, if there is no split, returns the results of the chisq test
#The 9th variable, if there is no splits tells us what to display i.e. what variables should we display the results for
#and the variables for which we should display nothing as there are errors and warnings
#Note: we will display the errors and warnings at the top of the table
#If there is a split,???


# Analysis> Crosstab
# Last modified 10/7/2021
#Last modified 10/17/2021
### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Crosstab
#'
#' @description Creates crosstab with row, column and layer variables and optionally displays the following
#' Expected counts
#' Row and column percentages
#' Unstandardized, standardized and adjusted residuals
#' Chisq with odds ratio, McNemar and Fisher statistics
#'
#' @param x row variable
#' @param y column variable
#' @param layers one or more variables for layers
#' @param weights a numeric variable containing the frequency weights
#' @param datasetname Name of the dataset from which x,y and layers (variables) are chosen
#' @param chisq  if TRUE generates chi-square table
#' @param prop.r Row percentages are produced if this is TRUE
#' @param prop.c Column percentages are generated if this is TRUE
#' @param resid if TRUE, unstandardized residual are generated
#' @param sresid if TRUE, standardized residuals are generated
#' @param expected Expected counts are generated if this is TRUE
#' @param asresid  if TRUE, adjusted residuals are generated
#'
#' @return A list with the results
#'
#' @examples BSky_Multiway_Cross_Tab = BSkyCrossTable(x=c('manufact'),y=c('model'),
#' layers=c('type'),datasetname='Dataset2',
#' chisq = FALSE,prop.r=FALSE,prop.c=FALSE,
#' resid=FALSE,sresid=FALSE,expected=FALSE,
#' asresid=FALSE)
#' BSkyFormat(BSky_Multiway_Cross_Tab)
BSkyCrossTable<- function(data = NULL, x=NA, y=NA,layers=NA, weight=NA, digits=3, max.width = 5, expected=FALSE, prop.r=FALSE, prop.c=FALSE,
           prop.t=FALSE, prop.chisq=FALSE, chisq = FALSE, fisher=FALSE, mcnemar=FALSE,
           resid=FALSE, sresid=FALSE, asresid=FALSE,
           missing.include=TRUE, dnn = NULL, datasetname = NULL, bSkyHandleSplit = TRUE, long_table = FALSE, debug=FALSE)
           
{
	
	# Must be called within uber or any sub function as the very fist executable statement/function call
	# This is to gather the call parameters passed. This is needed within
	# warning and error handler to log the calling details in logfile
	BSkyFunctionInit()
	
	orig_datasetname = datasetname  
	
	datasetname_passed = c("")
	
	if(!is.null(data))
	{
		if(class(data)[1] != "character")
		{
			dataset_name_str = deparse(substitute(data))
			
			if(dataset_name_str == ".")
			{
				datasetname_passed = dataset_name_str
				dataset_name_str = "data" 
			}
			
			#print(head(data))
		}
		else
		{
			dataset_name_str = data
			data = eval(parse(text=data), envir = globalenv())
		}
		
		datasetname = dataset_name_str
	}
	else if(length(datasetname) == 0)
	{
		return(invisible(NULL))
	}
	else if(BSkyIsRmarkdownOutputOn() == TRUE)
	{
		# For Rstudio to work correctly when data parameter is NULL (i.e. not used with %>%)
		# data  is null but datasetname has the dataset name
		# BSkyLoadRefresh is needed to load the dataset in ua dataset list global obj
		# for BSKy functions e.g. crosstab, ind sample and one sample to work in RStudio 
		if(!exists("name", envir = uadatasets) || !(datasetname %in% uadatasets$name))
		{
			BSkyLoadRefresh(datasetname, load.UIgrid = FALSE)
		}
	}
	
	
	if(is.na(x) && is.na(y))
	{
		datasetObj = eval(parse(text=datasetname))
		col_names = dimnames(datasetObj)[[2]]
		
		if(length(col_names) < 2)
		{
			return(invisible(NULL))
		}
		else
		{
			x = col_names[1]
			y = col_names[2]
			
			if(is.na(layers) && length(col_names) > 2)
			{
				if(is.na(weight))
				{
					layers = col_names[3:length(col_names)]
				}
				else 
				{
					layers = col_names[3:(length(col_names)-1)]
				}
			}
		}
	}
	
	
	if(datasetname_passed == ".")
	{
		if(length(orig_datasetname) == 0)
		{
			#temp_pipe_dataset_name = tempfile(pattern = "pipe_data_", tmpdir = "")
			#temp_pipe_dataset_name = substr(temp_pipe_dataset_name, 2, nchar(temp_pipe_dataset_name))
			temp_pipe_dataset_name = "bsky_piped_temp_dataset"
			eval(parse(text= paste(temp_pipe_dataset_name, "<<- data"))) #, envir = globalenv())
			BSkyLoadRefresh(temp_pipe_dataset_name, load.UIgrid = FALSE)
			 
			datasetname = temp_pipe_dataset_name
			
			#print(head(eval(parse(text= paste(temp_pipe_dataset_name)), envir = globalenv())))
			#print(datasetname)
			# print(head(data))
			
			# return()
		}
		else
		{
			datasetname = orig_datasetname
		}

		BSkySetCurrentDatasetName(datasetname, setDatasetIndex ="y")
		bSkyDatasetname = BSkyGetDatasetName(datasetname)
		datasetNameOrDatasetGlobalIndex = datasetname
		#bSkyHandleSplit = FALSE
		
		if(is.null(dnn))
		{
			dnn = NA
		}
		replace_uasummary_7 = paste( "BSkyCrossTable(",
									"asresid=c(", asresid, "),",
									"bSkyHandleSplit=c(", bSkyHandleSplit, "),",
									"chisq=c(",chisq, "),",
									"datasetname=c('",datasetname, "'),",
									"digits=c(",digits, "),",
									"dnn=",dnn, ",",
									"expected=c(", expected, "),",
									"fisher=c(", fisher, "),",
									"layers=c(",paste(layers, collapse=","), "),",
									"max.width=c(", max.width,  "),",
									"mcnemar=c(", mcnemar, "),",
									"missing.include=c(", missing.include, "),", 
									"prop.c=c(", prop.c, "),", 
									"prop.chisq=c(", prop.chisq, "),", 
									"prop.r=c(", prop.r, "),", 
									"prop.t=c(", prop.t, "),", 
									"resid=c(", resid, "),", 
									"sresid=c(", sresid, "),", 
									"weight=c(", weight, "),", 
									"x=c('",x, "' ),", 
									"y=c('", y,"' ) )", sep="")
		#print(replace_uasummary_7)
		#cat("\n",replace_uasummary_7, "\n")
	}
	else
	{
		datasetname = orig_datasetname
		
		#This is just to set the context to the current dataset name
		#This also copies the dataset to uadatasets$lst for backward compatibility
		BSkySetCurrentDatasetName(datasetname, setDatasetIndex ="y")
		bSkyDatasetname = BSkyGetDatasetName(datasetname)
	}
	
	
	#Added by Aaron 02/19/2021
	ua.CST <-NULL
	#if (layers=="") varNamesOrVarGlobalIndices=c(x,y)
	#else varNamesOrVarGlobalIndices=c(x,y)
	
	if (length(layers)==1)
	{
		if (is.na(layers))
		{
		  if (is.na(weight))
		  varNamesOrVarGlobalIndices=c(x,y)
		  else varNamesOrVarGlobalIndices=c(x,y,weight)
		    
		}
		else 
		  {
		    if (is.na(weight))
		    varNamesOrVarGlobalIndices =c(x,y,layers)
		    else 
		      varNamesOrVarGlobalIndices =c(x,y,weight,layers)
		  }
	}
	else 
	{
		if (is.na(weight))
			varNamesOrVarGlobalIndices =c(x,y,layers)
		else varNamesOrVarGlobalIndices =c(x,y,weight,layers)
	}
	
	
	if(datasetname_passed == ".")
	{
		bSkyVarnames = varNamesOrVarGlobalIndices
	}
	else
	{
		#varNamesOrVarGlobalIndices=c(x,y,layers)
		bSkyVarnames = BSkyGetVarNames(varNamesOrVarGlobalIndices, datasetname)
	}
    
 	#The global used to store the results of the tables returned by thecrosstab. 
	# Typically, there is one Retstructure for every table that needs to be displayed
	# In the case of splits, the retstructure is initialized for every split 
	uadatasets$retstructure <-list()
    layers =rev(layers)	
    #used to track the number of tables returned
    nooftablestodis =0
	uaorilevels=NULL
	# As retstructure is re-initilaized after every split, uaStatResults stores the retstructures across splits
    # uaStatResults is the added to the final list of results returned. All items in uaStatResults are added 
    # at position 8 onwards of the final return structure        		
    uaStatResults =list()
	
	
	BSkyErrMsg =paste("Error in CrossTable"," Dataset Name:", datasetname, " Row variables: ",paste(x, collapse = ",")," Column variables:",paste(y, collapse = ",")," Layers:", paste(layers, collapse = ","))
	BSkyWarnMsg =paste("Warning in CrossTable"," Dataset Name:", datasetname, " Row variables: ",paste(x, collapse = ",")," Column variables:",paste(y, collapse = ",")," Layers:", paste(layers, collapse = ","))
		
    
	BSkyStoreApplicationWarnErrMsg( BSkyWarnMsg,BSkyErrMsg)	
	bskyListCrosstabs=rev(expand.grid(y,x))
	bskyNoofCrosstabs=nrow(bskyListCrosstabs)
	
	
		tryCatch(
				{
    				withCallingHandlers(
    				{
					# Calculate #of iterations needed to perform for each dataset segments based on the split condition
					# If there is no split condition then the it will only loop through once i.e. compute analysis only
					# once for the entire dataset
					
					#warning("this is a custom warning!")
					#warning("this is a custom warning2!")
					
					if(bSkyHandleSplit == TRUE)
					{
      					bSkySplitIterationCount = BSkyComputeSplitdataset(bSkyVarnames, bSkyDatasetname)
						# if there is no split, uaSplitIterationCount = 1, otherwise usSplitIterationCount = cartesian product count
      					for (bSkySplitIterationCounter in 1:bSkySplitIterationCount)
						{
							bSkyDatasetSliceIndex = BSkyGetNextDatasetSliceIndex(bSkyVarnames, bSkyDatasetname)
							bSkyGlobalDataSliceIndexToWorkOn = bSkyDatasetSliceIndex$datasetSliceIndex
							bSkyVariableColumnIndicesOnDataSlice = bSkyDatasetSliceIndex$varIndex
							if (bSkySplitIterationCount > 1)
							{
								# BSkyStoreApplicationWarnErrMsg() can be used anytime to store Application level messages
								# so that it can be logged in the return structure for BSky application to display on the 
								# Output window UI. Here the App level message is set for every split iteration so that 
								# messages can be more specific to pint point what segment of the dataset has encounterd 
								# error or warning
								#Construct Application level warning message that will be logged within Warning and error handler function
								#This ensures that appropriate error messages are listed in the logs that indicate for which split the 
								#error or warning is recorded
								BSkyErrMsgwithSplit = paste(BSkyErrMsg, "Current Factors, if there is Split :",paste(BSkyComputeCurrentVarNamesAndFactorValues(bSkyDatasetname), collapse = ","),sep=" * ")
								BSkyWarnMsgWithSplit = paste(BSkyWarnMsg, "Current Factors, if there is Split :",paste(BSkyComputeCurrentVarNamesAndFactorValues(bSkyDatasetname), collapse = ","),sep=" * ")
								BSkyStoreApplicationWarnErrMsg(BSkyWarnMsgWithSplit, BSkyErrMsgwithSplit)
							}
						
							#07/13/2013 Aaron
							#Handling multiple row and column variables
							#For each split dataset, we create cross tabs for all the rows and column variables
							#if no split, we simply create a cross tab for all row and column variables
							#We generate a separate cross tab for each row in the cartestian product of the row and column variables
							#bskyNoofCrosstabs lists the total number of rows in the cartesian product
							p=1
							for (p in 1:bskyNoofCrosstabs)
							{
								#uadatasets$retstructure[[1]]$metadatatable[[3]]=NULL
								x=	as.character(bskyListCrosstabs[p,1])
								y=	as.character(bskyListCrosstabs[p,2])
								#Code to get the original levels of the row variable so that we can handle NAs for missing levels
							
								xindex =BSkygetIndexesOfCols(x, bSkyGlobalDataSliceIndexToWorkOn)
								#Index of the column variables
								uaorilevels=levels(uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]][,xindex])
											
								# 07/10/2013 -Not sure that we are doing anything with the column variables
								#Tracks the column names of the column variables that we are returning 
								#We need to track the column variables as the xtabs function will drop unused levels of the column variables that
								#are not represented in the data
								BSkyColLevels =NULL
								#warning("yahoo")
								#warning("Forced warning msg")														
    							BSkyColLevels=uaCrossTablegen(uaorilevels,x,y,weight,layers,digits, max.width, expected, prop.r, prop.c,
									prop.t, prop.chisq, chisq, fisher, mcnemar,resid, sresid, asresid,missing.include
									, dnn,index=bSkyGlobalDataSliceIndexToWorkOn)
									
									# Incrementally build the return structure for each iteration (i.e. based on dataset split)
									# with the tables generated from the statistical analysis performed in the above 
									# function i.e. uaonesample(). The following function will store the tables in a global struture
									# and return it to the application UI at the end of this top level function i.e. bsky.one.sm.t.test()
								BSkyBuildReturnTableStructure(varNamesOrVarGlobalIndices, datasetname,  OutputDataTableListIfPassed=NA)
               				}
      					
          				}
				}
				else
					{
							bSkyGlobalDatasetIndexToWorkOn = BSkyGetDatasetGlobalIndex(datasetNameOrDatasetGlobalIndex)

							bSkyVariableColumnIndicesOnDataset = BSkyGetVarGlobalIndices(varNamesOrVarGlobalIndices, datasetNameOrDatasetGlobalIndex)
							
							p=1
							for (p in 1:bskyNoofCrosstabs)
							{
								#uadatasets$retstructure[[1]]$metadatatable[[3]]=NULL
								x=	as.character(bskyListCrosstabs[p,1])
								y=	as.character(bskyListCrosstabs[p,2])
								#Code to get the original levels of the row variable so that we can handle NAs for missing levels
							
								xindex =BSkygetIndexesOfCols(x, bskyGlobalDataSliceIndexToWorkOn)
								#Index of the column variables
								uaorilevels=levels(uadatasets$lst[[bskyGlobalDataSliceIndexToWorkOn]][,xindex])
											
								# 07/10/2013 -Not sure that we are doing anything with the column variables
								#Tracks the column names of the column variables that we are returning 
								#We need to track the column variables as the xtabs function will drop unused levels of the column variables that
								#are not represented in the data
								BSkyColLevels =NULL
								#warning("yahoo")
							
							
							#OutputResultTablesList = uaonesample(uaVariableColumnIndexOnDataSlice, mu,conf.level,uaGlobalDataSliceIndexToWorkOn, missing)
								#uaonesample(bSkyVariableColumnIndicesOnDataset, mu,conf.level,bSkyGlobalDatasetIndexToWorkOn, missing)
							
							BSkyColLevels=uaCrossTablegen(uaorilevels,x,y,weight,layers,digits, max.width, expected, prop.r, prop.c,
									prop.t, prop.chisq, chisq, fisher, mcnemar,resid, sresid, asresid,missing.include
									, dnn,index=bSkyGlobalDatasetIndexToWorkOn)
							
							
							# cat("\nVarnames=\t")
							# print(bSkyVarnames)
							BSkyBuildReturnTableStructure(bSkyVarnames, bSkyDatasetname, OutputDataTableListIfPassed=NA)
							}
					}
          	  #cat ("\n Printing uaresults structure containing umat, uamatdisp etc \n")
          	  #print( uaStatResults)
    			},
    				warning = UAwarnHandlerFn
    									) # end of withCallingHandlers for catching warnings and continuing execution
    			},
    		error = UAerrHandlerFn,
    		silent =TRUE
    			)#end of try catch
 
    ############################################################################################################
    # Do other stuff whatever necessary coming out of the try catch block before returning from this UA function
    ############################################################################################################
    
    if( BSkyGlobalErrorFound() == TRUE)
    {
    	# if anything needs to be checked or print etc
			# Error is already handled and logged
			# Whereever the error occured execution cannot continue
			# control will be passed to the error handler function
			# and then the control will come out ogf the Try-Catch block 
			
			# cat("error caught in bsky.one.sm.t.test or anywhere in any sub function calls\n")
		BSkyBuildReturnTableStructure(varNamesOrVarGlobalIndices, datasetname,  OutputDataTableListIfPassed=NA)
    }
    
    if(BSkyGlobalWarningFound() == TRUE)
    {
    	#if any warning encountered in the above try catch block
    }
    
    #######################################################################################
    #Function to call at the end to close out logfiles. 
    #######################################################################################
	BSkyFunctionWrapUp()
	# Function BSKYReturnStructure creates the final retun structure i.e. splits or not, error or not, the number of errors
	# the number of warnings, the log, the summary of the function, the number of tables to display
	# Basically the first 7 elements of the list
	
	bsky_return_structure = BSkyReturnStructure2()
	
	if(datasetname_passed == ".")
	{
		bsky_return_structure$uasummary[[7]] = replace_uasummary_7
	}
	
	if(debug == TRUE)
	{
		return(invisible(bsky_return_structure))
	}
	
	#return(invisible(bsky_return_structure))
	#return(bsky_return_structure)
	table_list = BSkyFormatBSkyCrossTable(bsky_return_structure, long_table = long_table)
	#table_list = table_list$tables[1:(table_list$nooftables -1)]

	if(BSkyIsRmarkdownOutputOn() == TRUE)
	{
		return(noquote(table_list))
	}
	else
	{
		return(invisible(table_list))
	}
}






################################################################
#HANDLE THE CASE WHERE THERE ARE NO ENTRIES FOR STORE1, STORE2, STORE3, STORE4, STORE5 IN A SINGLE 
#TABLE E.G. REGULAR =BIWEEKLY, CONTACT=FREQUENTLY, GENDER =MALE

#asresid is not working. Code is bombing out at line below

#if (asresid) 
#      		 ua.ASR <- (ua.CST$observed - ua.CST$expected)/sqrt(ua.CST$expected *  ((1 - ua.RS/ua.GT) %*% uaretstructure$t(1 - ua.CS/ua.GT)))

# 07/13/2013 Aaron
# Here we call xtabs to create the table structure containing the counts. 
# Once the table structure containing the counts is created, we then call function 
#ua.MultDimCrossTab (uaorilevels,uatab, ua.2D.crossTab.fun.name="ua.2D.crosstab", starting.dim.pos =3, format="SPSS", prop.r=prop.r, prop.c=prop.c, prop.t=prop.t, prop.chisq=prop.chisq, missing.include=missing.include,resid=resid, sresid=sresid, asresid=asresid,expected=expected,chisq=chisq,mcnemar=mcnemar)	
#ua.MultDimCrossTab handles the calculation of row percents, column percents.....
#ua.MultDimCrossTab calls "ua.2D.crosstab" for the counts that represented a unique layer tuple

uaCrossTablegen<-function(uaorilevels,x, y,weight, layers,digits, max.width, expected, prop.r, prop.c,
           prop.t, prop.chisq, chisq, fisher, mcnemar,
           resid, sresid, asresid,
           missing.include,
           dnn = NULL,index)
{
	### Sanjays new template
	
    #uaFunctionInit()
	BSkyFunctionInit()
	
	#Tracks the column names of the column variables that we are returning 
	#We need to track the column variables as the xtabs function will drop unused levels of the column variables that
	#are not represented in the data
	BSkyColLevels =NULL
	#Handling the case of an empty dataset
	if (nrow(uadatasets$lst[[index]])==0 )
	{
		#uaFunctionWrapUp(callParamDetails)
		uawritelog(type="Error", BSkyMessage ="CrossTable cannot be run as the dataset is empty");
		#This takes care of a case of an unhandled warning that may have made an entry in the return structure, before
		#the return structure was created
		#If there was a unhandled warning that occured before uadatasets$retstructure[[1]] was initialized in the Crosstable function
		#, we would have to display it, as a result we don't want to initialize uadatasets$retstructure[[1]]
		#Case of no global warning that occured before uaCrossTablegen was called
		if (length(uadatasets$retstructure)==0)
		{
			uadatasets$retstructure[[1]]<-list()
			uadatasets$retstructure[[1]]$type="table"
			uadatasets$retstructure[[1]]$metadata="yes"
			uadatasets$retstructure[[1]]$nometadatatables=1
			uadatasets$retstructure[[1]]$metadatatabletype=c("normal")
			uadatasets$retstructure[[1]]$metadatatable=list()
			uadatasets$retstructure[[1]]$metadatatable[[1]]=data.frame(varIndex=NA,severityType=-2,varName=NA,dataTableRow=NA,startCol=NA,endCol=NA,BSkyMsg="Crosstab cannot be run as the dataset is empty",Rmsg=NA)
			uadatasets$retstructure[[1]]$datatable=NULL
		}
		else
		{
			bskyNoofTables=length(uadatasets$retstructure)
			uadatasets$retstructure[[1]]$type="table"
			uadatasets$retstructure[[1]]$nometadatatables=1
			uadatasets$retstructure[[1]]$metadatatabletype="normal"	
			uadatasets$retstructure[[1]]$datatable=NULL
			uadatasets$retstructure[[bskyNoofTables]]$metadatatable[[1]]=rbind(uadatasets$retstructure[[bskyNoofTables]]$metadatatable[[1]],data.frame(varIndex=NA,severityType=-2,varName=NA,dataTableRow=NA,startCol=NA,endCol=NA,BSkyMsg="Crosstab cannot be run as the dataset is empty",RMsg=NA))
			
			#bskyNoofTables=length(uadatasets$retstructure)
			#uadatasets$retstructure[[bskyNoofTables]]$metadatatable[[1]]=rbind(uadatasets$retstructure[[bskyNoofTables]]$metadatatable[[1]],data.frame(varIndex=NA,type=-2,varName=NA,dataTableRow=NA,startCol=NA,endCol=NA,BSkyMsg="Cross Table cannot be run as the dataset is empty",Rmsg=NA))
		}
		uadatasets$error =-1;
		uadatasets$errorindex =uadatasets$errorindex +1
		# every ua sub function must call the following function to write the log
		#uaFunctionWrapUp("uaCrossTablegen")
		BSkyFunctionWrapUp()
		return(TRUE)			
	}
		
	#This takes care of a case of an unhandled warning that may have made an entry in the return structure, before
	#the return structure was created
	#If there was a unhandled warning that occured before uadatasets$retstructure[[1]] was initialized in the Crosstable function
	#, we would have to display it, as a result we don't want to initialize uadatasets$retstructure[[1]]
	
	#Case of no global warning that occured before uaCrossTablegen was called
	if (length(uadatasets$retstructure)==0)
	{
		uadatasets$retstructure[[1]]<-list()
		uadatasets$retstructure[[1]]$type="table"
		uadatasets$retstructure[[1]]$metadata="yes"
		uadatasets$retstructure[[1]]$nometadatatables=3
		uadatasets$retstructure[[1]]$metadatatabletype=c("crosstab")
		uadatasets$retstructure[[1]]$metadatatable=list()
		uadatasets$retstructure[[1]]$metadatatable[[1]]=data.frame()
		uadatasets$retstructure[[1]]$datatable=NULL
		uadatasets$retstructure[[1]]$metadatatable[[2]]=matrix(nrow=0,ncol=2)
	}
	else
	# Case where the dataset is not empty and a warning has already occured
	{
		uadatasets$retstructure[[1]]$type="table"
		uadatasets$retstructure[[1]]$nometadatatables=3
		uadatasets$retstructure[[1]]$metadatatabletype=c("crosstab")	
		uadatasets$retstructure[[1]]$datatable=NULL
		uadatasets$retstructure[[1]]$metadatatable[[2]]=matrix(nrow=0,ncol=2)
	}
	#Note, this line was inserted on 03/10/2012 as we are generating the 2nd table only where chisq=TRUE
	if (chisq ==TRUE||fisher==TRUE|| mcnemar==TRUE)
	{
		uadatasets$retstructure[[2]]<-list()
		uadatasets$retstructure[[2]]$type="table"
		uadatasets$retstructure[[2]]$metadata="yes"
		uadatasets$retstructure[[2]]$nometadatatables=2
		uadatasets$retstructure[[2]]$metadatatabletype=c("normal")
		uadatasets$retstructure[[2]]$metadatatable=list()
		uadatasets$retstructure[[2]]$metadatatable[[1]]=data.frame()
		uadatasets$retstructure[[2]]$datatable=NULL
	#uadatasets$retstructure[[2]]$metadatatable[[2]]=matrix()
	}
	#uavars =c(x,y,layers)
	#if (layers=="") uavars=c(x,y)
	#else uavars =c(x,y,layers)
	
	
	
	
	
	
	if (length(layers)==1)
	{
	  if (is.na(layers))uavars=c(x,y)
	  else uavars =c(x,y,layers)
	}
	else uavars =c(x,y,layers)
	
	
	
	if (is.na(weight))
	{
	  
	  uaform <- as.formula(paste(" ~ ", paste(uavars, collapse= "+")))
	}
	else
	{
	  uaform <- as.formula(paste(weight," ~ ", paste(uavars, collapse= "+")))
	}
	
	#uaform <- as.formula(paste(" ~ ", paste(uavars, collapse= "+")))
	#Aaron, I changed this back to true on 10/07/2012
	#This was because in the past missing levels were removed from the rows and the columns
	#it was very difficult to create uamatdis correctly as levels would be missing and I would have to communicate the missing values to Vishal
	#So moved to a model where I would pass vishal the levels and the counts for that level
		
	#Aaron changed this to False from True on 10/05/2011
	#this was because of a level in any dimension that is not used in the cross tab is automatically removed.
	#consider store and overall with regular, contact and gender in the layers
	#if regurlar has 5 levels and only 3 are used, then we will not see the tables for the 2 missing levels
	#This is an issue when the missing level is in the layers and not in the row and column of the crosstab
	# SEE DOCUMENTATION OF HOW DROPPED UNUSED LEVELS WORKS ABOVE WITH EXAMPLE i HAVE CREATED
 	uatab <-xtabs(formula=uaform,data=uadatasets$lst[[index]],drop.unused.levels = TRUE)
	BSkyColLevels=dimnames(uatab)[[2]]
	
	#Added by Aaron 04/21/2015
	uadatasets$retstructure[[1]]$columnNames =BSkyColLevels
	
	#bskycartlevels= rev(expand.grid(x,y))
	uatab <-ua.MultDimCrossTab (uaorilevels,uatab, ua.2D.crossTab.fun.name="ua.2D.crosstab", starting.dim.pos =3, format="SPSS", prop.r=prop.r, prop.c=prop.c, prop.t=prop.t, prop.chisq=prop.chisq, missing.include=missing.include,resid=resid, sresid=sresid, asresid=asresid,expected=expected,chisq=chisq,mcnemar=mcnemar,fisher=fisher)	
	
	
	#The levels that we are returning values for are repeated in both the crosstab and the results of the statistics
	#We hence copy the
	#Note, this line was inserted on 03/10/2012 as we are generating the 2nd table only where chisq=TRUE
	if (chisq ==TRUE|| fisher==TRUE||mcnemar==TRUE)
	uadatasets$retstructure[[2]]$metadatatable[[2]]=uadatasets$retstructure[[1]]$metadatatable[[3]]
	
	#uaFunctionWrapUp("uaCrossTablegen")
	BSkyFunctionWrapUp()
	return(BSkyColLevels)
}	



uacreatuamatdis <-function(t,uaindofmisslevels,uaorilevels,prop.r,prop.c,expected,resid,sresid,asresid, oriprop.chisq ,orichisq,actualexpected, orifisher, orimcnemar ,oriresid,orisresid ,oriasresid)
{
	# every ua sub function must call the following two 
    # initislization call at the begining of the function as follows 
    #callParamDetails = uaLogFunArgsDetails()
    #uaFunctionInit(callParamDetails)
	#uaFunctionInit()
	BSkyFunctionInit()
	j=1
	k=1
	len=length(uaorilevels)
	uanoofrows =nrow(t)
	uanoofcols=ncol(t)
	uatemprow =rep(0,uanoofcols)
	uatempcol =rep(0,uanoofrows)
	uatempindex =NULL
	rowid=1
	
	for (i in 1:len)
	{
		#Code below checks for a factor that is completely missing in the data e.g if store has levels store1, store2, store3, 
		#store 4, store5 and store2 is completely missing from the entire dataset
		#Here we store a 0 in each row of uamatdis corresponding to the count and expected, prop.r, prop.c, resid, sresid and asresid if selected
		#Note that if the dimensions of the table are less than 2, we set expected, chisq, resid, sresid, asresid to FALSE
		#the Original values that the function was called with are stored in orichisq, actualexpected, oriresid, orisresid, oriasresid
		#So to protect us from the fact that resid =FALSE but oriresid =TRUE, we need to check the original values
		#and signal to Anil that these values are ignored or will not be displayed in datatable by adding a 0 to uamatdis even through resid is FALSE
		
			if (i %in% uaindofmisslevels[k] )
			{
				uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
				k=k+1
				rowid=rowid+1
				if (actualexpected) 
				{
				uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
				rowid=rowid+1
				}
							
				
				if (prop.r) 
				{
				uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
				rowid=rowid+1
				}
				#else if (prop.r !=oriprop.r)
				#{
					#uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
					#rowid=rowid+1
				#}
				if (prop.c) 
				{
				uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
				rowid=rowid+1
				}
				#else if (prop.c !=oriprop.c)
				#{
					#uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
					#rowid=rowid+1
				#}
			
			
				if (oriresid) 
				{
				uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
				rowid=rowid+1
				}
				#else if (resid !=oriresid)
				#{
					#uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
					#rowid=rowid+1
				#}
				if (orisresid) 
				{
				uadatasets$retstructure[[1]]$metadatatable[[2]]=rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
				rowid=rowid+1
				}
				#else if (sresid !=orisresid)
				#{
					#uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
					#rowid=rowid+1
				#}
			
				#Added by Aaron 10/12/2014
				if (oriasresid) 
				{
				uadatasets$retstructure[[1]]$metadatatable[[2]]=rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
				rowid=rowid+1
				}
				#else if (asresid !=oriasresid)
				#{
				#uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
				#rowid=rowid+1
				#}
						
			
			}
				
		#The code below checks for rows with all 0s. All entries will be 0 when the sum is zero
		#When there is a row with all 0s,  we remove that datarow from the datatable. The prop.r, prop.c
 		#expected, residuals are also not displayed in the data table.WE need to inform Anil by
		#setting that row to 0 in uamatdis.
		##Note that if the dimensions of the table are less than 2, we set expected, chisq, resid, sresid, asresid to FALSE
		#the Original values that the function was called with are stored in orichisq, actualexpected, oriresid, orisresid, oriasresid
		#So to protect us from the fact that resid =FALSE but oriresid =TRUE, we need to check the original values
		#and signal to Anil that these values are ignored or will not be displayed in datatable by adding a 0 to uamatdis even through resid is FALSE
		else if (sum(t[j,]) ==0)
 		{
			j=j+1
			uatempindex =c(uatempindex, i)
			uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
			rowid=rowid+1			
			if (actualexpected) 
			{
			uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
			rowid=rowid+1
			}
			if (prop.r) 
			{
			uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
			rowid=rowid+1
			}
			if (prop.c) 
			{
			uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
			rowid=rowid+1
			}
			if (oriresid) 
			{
			uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
			rowid=rowid+1
			}
			if (orisresid) 
			{
			uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
			rowid=rowid+1
			}
			#Added by Aaron 10/12/2014
				if (oriasresid) 
				{
				uadatasets$retstructure[[1]]$metadatatable[[2]]=rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
				rowid=rowid+1
				}
			#else if (asresid !=oriasresid)
				#{
				#uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
				#rowid=rowid+1
				#}
		
		}
	
		#The case below removes the totals from the display if the column sums are 0
		#This would mean all rows are 0, hence we would not display the table
		#Since all the rows are omitted from the display, so should be with the totals
						
		else 
		{
			#uadatasets$uamatdis =rbind(uadatasets$uamatdis, c(dimnames(t)[[1]][i], 1))			
			uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 1))
			rowid=rowid+1
			if (actualexpected) 
			{
				uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 1))
				rowid=rowid+1
			}
			# This is the case when one of the dimensions are less than 2. Here expected =FALSE (we set 
			#expected to FALSE, "actual expected" the original value is TRUE
			#So in scenario above we set uamatdis to 0 as the datatable does not contained a value for expected
			#else if (expected!=actualexpected)
			#{
				#uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
				#rowid=rowid+1
			#}
		
			if (prop.r) 
			{
			uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 1))
			rowid=rowid+1
			}
			
			
			if (prop.c) 
			{
			uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 1))
			rowid=rowid+1
			}
			
		
		
			if (oriresid) 
			{
			uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 1))
			rowid=rowid+1
			}
			#else if (resid !=oriresid)
			#{
				#uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
				#rowid=rowid+1
			#}
		
		
			if (orisresid) 
			{
			uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 1))
			rowid=rowid+1
			}
			#else if (sresid !=orisresid)
			#{
				#uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
				#rowid=rowid+1
			#}
		
		
			#Added by Aaron 10/12/2014
				if (oriasresid) 
				{
				uadatasets$retstructure[[1]]$metadatatable[[2]]=rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 1))
				rowid=rowid+1
				}
				#else if (asresid !=oriasresid)
				#{
				#uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
				#rowid=rowid+1
				#}
			j=j+1
		
		}
		
	} #closes the for loop


	#Handling the sitiation where there may be several levels 
	#Code below checks for a factor that is completely missing in the data, that is  e.g if store has levels store1, store2, store3, 
	#store 4, store5 and store4 and store 5 are completely missing from the entire dataset
	#BASICALLY WE ARE LOOKING AT FACTORS THAT ARE COMPLETELY MISSING THAT ARE LEFT OUT OF XTABS
	#THE CODE BELOW HANDLES STORE4 AND STORE5
	#Aaron 03/08/2015
	#What about a column that is completely missing from the dataset
	
	#Code below handles uamatdis for totals
	
	if (sum(colSums(t,na.rm=TRUE)) ==0)
	{
		uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
		rowid=rowid+1
		if (actualexpected) 
		{
			uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
			rowid=rowid+1
		}		
		if (prop.r) 
		{
			uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
			rowid=rowid+1
		}
		if (prop.c) 
		{
			uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
			rowid=rowid+1
		}
	}
	else
	{
		uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 1))					
		rowid=rowid+1
		if (actualexpected) 
		{
			uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 1))
			rowid=rowid+1
		}
		#else if (expected != actualexpected)
		#{
			#uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 0))
			#rowid=rowid+1
		#}
	
	
		if (prop.r) 
		{
			uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 1))
			rowid=rowid+1
		}
		
	
		if (prop.c ) 
		{
		uadatasets$retstructure[[1]]$metadatatable[[2]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[2]], c(rowid, 1))
		rowid=rowid+1
		}
		
	
	}
	#uaFunctionWrapUp("uacreatuamatdis")
	BSkyFunctionWrapUp()
	return(uatempindex)		
}

#This function prepares the subset of the table from xtabs for running the 2 d cross tab function
#In order for the statistical tests to work, all the rows and columns on the table where the counts are zero, need to be made zero
#The table required for calculating the percents require all the rows with zeros removed but all the cols with zero are needed
#uaretstructure$t ->all rows with 0s removed, all cols with 0s set to NA
	#uaretstructure$tpercents ->all rows with only 0s removed. Cols with only 0s are not set to NA
	#uaretstructure$tforstats -> table with only columns that are not zero and rows that are not zero

uapreptableforstats <-function(t,uatempindex)
{
	#callParamDetails = uaLogFunArgsDetails()
	#	uaFunctionInit()
		BSkyFunctionInit()
	
	#Deleting all the rows in the table that contain 0
	if (!is.null(uatempindex))	t = t[-uatempindex,]
	#This handles the case where only 1 row is non zero. In this case we need to convert a numeric to a matrix
	#if(class(t)=="numeric")t =matrix(t,nrow=1)
	
	if (length(class(t))==1)
	{
	  #19Jan2017 Added "integer" condition in following.
	  #without that 'if' was failing and further the crosstab was failing.
	  #From Cars.csv put 'origin' in 1st, 'year' in 2nd and 'cylinder' in 3rd box, you will see an error, if
	  # 'integer' condition is not present in following 'if'
		if(class(t)=="numeric" || class(t)=="integer")t =matrix(t,nrow=1)
	}
	# The code below creates a table to be returned that will be used to calculate row percents and column percents
	tpercents=t
	#The code below creates a matrix that will be used to compute the statistics
	uanoofcols=ncol(t)
	j=1
	uatforstats =matrix(nrow =nrow(t),ncol=0)	
	for (i in 1:uanoofcols)
	{
	if (sum(t[,i]) ==0)
 		{
			t[,i] =NA
		}
		else 
		{
			uatforstats =cbind(uatforstats,t[,i])
		}	
	}
	#uaFunctionWrapUp("uapreptableforstats")
	BSkyFunctionWrapUp()
	return(list(t=t,uatforstats=uatforstats,tpercents=tpercents))			
}









###########################################################################
#Creates the crosstabs without the totals
###########################################################################

newcreateglobalcrosstab<-function(xtab.dim.orig, crosstab.2D.return.obj,expected, 
    prop.r , prop.c, prop.t, prop.chisq , chisq, fisher, mcnemar, resid, 
    sresid, asresid,oriprop.chisq ,orichisq,actualexpected, orifisher, orimcnemar ,oriresid,orisresid ,oriasresid, missing.include,...)
{
	#callParamDetails = uaLogFunArgsDetails()
	#uaFunctionInit()
		BSkyFunctionInit()
	
	uatablestats =crosstab.2D.return.obj$crosstab.2D.return.obj
	uanorows=nrow(uatablestats$table)
	uanocols=ncol(uatablestats$table)
	#uadatasets$uacrosstabres<-rbind(uadatasets$uacrosstabres,
	#rbind( cbind(crosstab.2D.return.obj$crosstab.2D.return.obj$table,
	#Total =crosstab.2D.return.obj$crosstab.2D.return.obj$row.sum),
	#c(crosstab.2D.return.obj$crosstab.2D.return.obj$col.sum,crosstab.2D.return.obj$crosstab.2D.return.obj$grand.total)))	
	
	if (prop.r)
	{
		uaprop.row =uatablestats$prop.row*100
		uaprop.rowtotal=100
	}
	else 
	{
		uaprop.row =NULL
		#This is for the second row (Under count)
		uaprop.rowpercent=NULL
		uaprop.rowtotal=NULL
	}
	if (prop.c)
	{
		uaprop.col =uatablestats$prop.col*100
		#This is for the column totals at the bottom of the table that are always 100
		# uaprop.colpercent which is the column percents is initialized below in the for loop
		# Coltotal represents the column percents in the totals of all the rows
		#uaprop.coltotal=rep(100,uanocols)	
		uaprop.coltotal <-NULL
	}
	else 
	{
		uaprop.col =NULL
		#This is for the 3rd row, under the counts 
		uaprop.colpercent=NULL
		uaprop.coltotal=NULL
	}
	
	if (resid )
	{
		uaresid=uatablestats$resid
		uaresid =cbind(uaresid,NA)
	}
	else
	{
		uaresid=NULL
	}
	
	if (sresid)
	{
		uasresid=uatablestats$sresid
		uasresid =cbind(uasresid,NA)
	}
	else
	{
		uasresid=NULL
	}
	
	
	
	if (asresid )
	{
		uaasresid=uatablestats$asresid
		uaasresid =cbind(uaasresid,NA)
	}
	else
	{
		uaasresid=NULL
	}
	
	if (expected )
	{
		uaexpected =uatablestats$CSTexpected
		uaexpected =cbind(uaexpected,rowSums(uaexpected,na.rm=TRUE))
		uaexpectedtot =colSums(uaexpected,na.rm=TRUE)
		uaexpectedtot[uaexpectedtot==0]<-NA 
	}
	else
	{
		uaexpected=NULL
		uaexpectedtot=NULL
	}

	if (chisq |orichisq)
	{
			#This code handes the case where any of the dimensions of the table of counts are less than 2.
			#As the chi sq test is not run in the ua.2D.crosstab function, we set the rows in crostabstats to NA so that the 
			#corresponding warning can be displayed
		if (is.null(uatablestats$chisq$statistic))uadatasets$retstructure[[2]]$datatable=rbind(uadatasets$retstructure[[2]]$datatable, c( NA,NA,NA,NA,NA,NA))
		else uadatasets$retstructure[[2]]$datatable=rbind(uadatasets$retstructure[[2]]$datatable, c( uatablestats$chisq$statistic,uatablestats$chisq$parameter,uatablestats$chisq$p.value,uatablestats$OR,uatablestats$lower.OR, uatablestats$upper.OR))
	}
	
	if (mcnemar| orimcnemar)
	{
		if (is.null(uatablestats$mcnemar$statistic))uadatasets$retstructure[[2]]$datatable=rbind(uadatasets$retstructure[[2]]$datatable, c( NA,NA,NA,NA,NA,NA))
		else
		uadatasets$retstructure[[2]]$datatable=rbind(uadatasets$retstructure[[2]]$datatable, c( uatablestats$mcnemar$statistic,uatablestats$mcnemar$parameter,uatablestats$mcnemar$p.value,NA,NA,NA))
	}
	if (fisher| orifisher)
	{
		if (is.null(uatablestats$fisher.ts$p.value))uadatasets$retstructure[[2]]$datatable=rbind(uadatasets$retstructure[[2]]$datatable, c( NA,NA,NA,NA,NA,NA))
		else
		uadatasets$retstructure[[2]]$datatable=rbind(uadatasets$retstructure[[2]]$datatable, c( NA, NA, uatablestats$fisher.ts$p.value,uatablestats$fisher.ts$estimate,uatablestats$fisher.ts$conf.int[1],uatablestats$fisher.ts$conf.int[2] ))
		#uadatasets$retstructure[[2]]$datatable=rbind(uadatasets$retstructure[[2]]$datatable, c( NA,NA,NA,NA,NA,NA))
	}
	
	#Here we are combining the row counts with the row and column percents
	for ( i in 1:uanorows)
	{
		#uaprop.totalcol are the column percents
		if (prop.c) uaprop.colpercent=(uatablestats$row.sum[i]/sum(uatablestats$row.sum[1:uanorows]))*100
		
		uadatasets$retstructure[[1]]$datatable =rbind(uadatasets$retstructure[[1]]$datatable, 
			#This corresponds to the counts
			c(uatablestats$table[i,],uatablestats$row.sum[i]),
			#This corresponds to expected counts
			#c(uaexpected[i,],sum(uaexpected[i,],na.rm=TRUE)),
			c(uaexpected[i,]),
			#This is for the 2nd row which corresponds to row percents
			c(uaprop.row[i,],uaprop.rowtotal),
			#This corresponds to the 3rd row or column percents
			c(uaprop.col[i,],uaprop.colpercent),
			#This corresponds to residuals
			c(uaresid[i,]),c(uasresid[i,]),c(uaasresid[i,])
			)
	}

	
	#The code below constructs the total across all the rows
	#if prop.r is TRUE, uaprop.coltotal below has the correct value
	if (prop.r)   
	{
		uaprop.rowtotal =(uatablestats$col.sum/uatablestats$grand.total)*100
		uaprop.rowtotal[uaprop.rowtotal==0]<-NA 
		uaprop.rowtotal <-c(uaprop.rowtotal,100)
	}
	if (prop.c )   
	{
		uatablestats$col.sum[uatablestats$col.sum==0]<-NA
		uaprop.coltotal[uatablestats$col.sum==0]<-NA
		uaprop.coltotal[uatablestats$col.sum!=0]<-100
		uaprop.coltotal <-c(uaprop.coltotal,100)
	}
	#The code below ensures that if the sum of any column =0, we set it to NA instead of 0
	#uatablestats$col.sum <-
	uatablestats$col.sum[uatablestats$col.sum==0]<-NA
	uadatasets$retstructure[[1]]$datatable =rbind(uadatasets$retstructure[[1]]$datatable, c(uatablestats$col.sum,uatablestats$grand.total),c(uaexpectedtot),c(uaprop.rowtotal),
			c(uaprop.coltotal))
	#uaFunctionWrapUp(callParamDetails)
	#Code for total of expected count
	#if (expected)
	#{
	#	uadatasets$uacrosstabres =rbind(uadatasets$uacrosstabres, colSums(uaexpected))
#}
	#uaFunctionWrapUp("newcreateglobalcrosstab")
		BSkyFunctionWrapUp()
}

















#The 2D crosstable returns 0 in the row percents for entries that are NA. We need to set 0 to NA


uahandlenainpercents <-function(oritable,percenttable)
{
		BSkyFunctionInit()
	#callParamDetails = uaLogFunArgsDetails()
	#uaFunctionInit(callParamDetails)
	noofrows=nrow(oritable)
	noofcols=ncol(percenttable)	
	for (i in 1:noofrows)
	{
		for (j in 1:noofcols)
		{
			if (is.na(oritable[i,j])) percenttable[i,j]=NA
		}
	}
	#uaFunctionWrapUp(callParamDetails)
		BSkyFunctionWrapUp()
	return(percenttable)
}



#ua.MultDimCrossTab handles the calculation of row percents, column percents.....
#ua.MultDimCrossTab calls "ua.2D.crosstab" for the counts that represented a unique layer tuple

ua.MultDimCrossTab<-function (uaorilevels,t,  ua.2D.crossTab.fun.name="ua.2D.crosstab",  starting.dim.pos =3, digits = 3, max.width = 5, expected = FALSE, 
    prop.r, prop.c, prop.t, prop.chisq , 
    chisq = TRUE, fisher = FALSE, mcnemar = FALSE, resid = FALSE, 
    sresid = FALSE, asresid = FALSE, missing.include = TRUE, 
    format = NULL, dnn = NULL, ...) 
{
	
	BSkyFunctionInit()
	xtab.dim.len<-length(dim(t))
	factorvals=NULL
	
		oriprop.t=prop.t
		oriprop.chisq=prop.chisq
		orichisq=chisq
		
		actualexpected=expected
		orifisher=fisher
		orimcnemar=mcnemar
		oriresid=resid
		orisresid=sresid
		oriasresid=asresid
	
	#this is the case when every table in t has one row and one column. In this situation
	#every table returned by t[,,1],t[,,2] has a single row and column and the table passed to ua.2D.crosstan
	#has a single value and hence newlevels=dimnames(t)[[1]] returns null
	#we use storeOriLevelOfFirstVarIfOneByOneTable to store the orifinal levels of the first dimension
	# See code below to recreate
	#crosst<-NULL
#crosst <-BSkyCrossTable(x=c('cylinder'),y=c('origin'),layer=c('year'),datasetname="forcross",chisq = TRUE,prop.r=TRUE,prop.c=TRUE, resid=FALSE, sresid=TRUE, expected=FALSE)
#crosst
	
	storeOriLevelOfFirstVarIfOneByOneTable =dimnames(t)[[1]];
	xtab.dim.orig<-dim(t)
	#we are generating values in the crosstab for
	#This will be used by Vishal to determine what levels we are generating values in the crosstab,
	#This is because drop.unused.levels =TRUE removes all levels that are not used
	#we need to communicate to Vishal what levels we are generating values for
	
	# Aaron 08/31/2013
	# This is the case where there is no layer information added
	# We directly call ua.2D.crosstab and set 
	
	
	if (xtab.dim.len ==2)
	{
		uadatasets$retstructure[[1]]$metadatatable[[3]]=NA
		# Aaron 08/31/2013
		# I have created an object containing a list named crosstab.2D.return.obj for compatibility 
		# to ensure we can use functions like newcreateglobalcrosstab
		crosstab.2D.return.obj<-list( crosstab.2D.return.obj = list())
		
		noNonEmptyTables =0
		
		## Aaron 08/31/2013
		# We call out directly to ua.2D.crosstab instead of using the broker as we are not interested in creating the layer.name.value
		#property. This is used to populate uadatasets$retstructure[[1]]$metadatatable[[3]]. However as the layers are non-existant
		# we don't need this
		
		crosstab.2D.return.obj$crosstab.2D.return.obj <-ua.2D.crosstab(uaorilevels,t,  digits = 3, max.width = 5, expected , 
		prop.r , prop.c, prop.t , prop.chisq , 
		chisq , fisher , mcnemar , resid , 
		sresid , asresid ,oriprop.chisq ,orichisq,actualexpected, orifisher, orimcnemar ,oriresid,orisresid ,oriasresid,layerinfo="" ,missing.include, 
		format , dnn, factorvals,noNonEmptyTables,storeOriLevelOfFirstVarIfOneByOneTable) 
	
		noNonEmptyTables=crosstab.2D.return.obj$crosstab.2D.return.obj$noNonEmptyTables
		if (!crosstab.2D.return.obj$crosstab.2D.return.obj$tableAllZeroCounts)
		{
			crosstab.2D.return.obj$crosstab.2D.return.obj$prop.col <-uahandlenainpercents(crosstab.2D.return.obj$crosstab.2D.return.obj$table,crosstab.2D.return.obj$crosstab.2D.return.obj$prop.col)
			crosstab.2D.return.obj$crosstab.2D.return.obj$prop.row <-uahandlenainpercents(crosstab.2D.return.obj$crosstab.2D.return.obj$table,crosstab.2D.return.obj$crosstab.2D.return.obj$prop.row)
			###############Code added to handle the totals and building the crosstab in memory
			#xtab.dim.loop.tracking
			newcreateglobalcrosstab(xtab.dim.orig,crosstab.2D.return.obj, expected, 
			prop.r , prop.c, prop.t, prop.chisq , chisq, fisher, mcnemar, resid, 
			sresid, asresid, oriprop.chisq ,orichisq,actualexpected, orifisher, orimcnemar ,oriresid,orisresid ,oriasresid, missing.include,... )
				
		}
		BSkyFunctionWrapUp()
		return
	}
	#callParamDetails = uaLogFunArgsDetails()
	#uaFunctionInit()
	xtab.dim.len<-length(dim(t))
	factorvals=NULL
	#we are generating values in the crosstab for
	#This will be used by Vishal to determine what levels we are generating values in the crosstab,
	#This is because drop.unused.levels =TRUE removes all levels that are not used
	#we need to communicate to Vishal what levels we are generating values for
	uadatasets$retstructure[[1]]$metadatatable[[3]]=matrix(nrow=0,ncol=(xtab.dim.len-2))
	xtab.dim.orig<-dim(t)
	#I am using this variable to pass to the function that creates the crosstab with the proportions and totals
	uatemp1 <-xtab.dim.orig
	xtab.dim.loop.tracking<-dim(t)
	xtab.dim.loop.counter<-dim(t)
	total.num.2D.table.calls<-1
	starting.dimension.pos <- starting.dim.pos 
	
	#1st set of return values. just one time no looping
	return.result.set <-list( table.name = deparse(substitute(t)), class.of.table = class(t), 
				table.dim.length =  xtab.dim.len, table.dim.level.size = xtab.dim.orig,
				table.dim.names = dimnames(t), table.row.names = rownames(t), table.col.names = colnames(t),
				formulas = attr(t, "call"), result.list.of.objects <-list("") )
	
	
	# return.result.set$result.objects
	# func.name<-ua.2D.crossTab.fun.name
	# cat("Cross Tab Function Name1: ", deparse(substitute(ua.2D.crossTab.fun.name)), " and ", func.name, "\n" )
	
	if (xtab.dim.len > 2 && all(dim(t)!=0) )
	{	
		for ( i in starting.dimension.pos:xtab.dim.len)
		{
			total.num.2D.table.calls<- total.num.2D.table.calls * xtab.dim.orig[i]
			xtab.dim.loop.tracking[i]<- (xtab.dim.orig[i] +1) - xtab.dim.loop.counter[i]  
			# cat("i =", i, "total.num.2D.table.calls = ", total.num.2D.table.calls, "xtab.dim.loop.tracking[",i,"] = ", xtab.dim.loop.tracking[i], "\n")
		}
		#Aaron 10/27/2014
		#cat("Xtable Name : ", deparse(substitute(t)), "\n" )
		#cat("xtab.dim.len=", xtab.dim.len, "\n")
		#cat("xtab.dim=", xtab.dim.orig, "\n")
		#cat("total.num.2D.Cross Table.calls (2D Function Name:", ua.2D.crossTab.fun.name,"( ) )", "= ", total.num.2D.table.calls, "\n\n")

		#ua.2D.crosstab.broker<-function (ua.2D.crossTab.fun.name, xtab.table.index, t, y, digits, max.width, expected, 
    	#					prop.r, prop.c, prop.t, prop.chisq, 
    	#					chisq, fisher, mcnemar, resid, 
    	#					sresid, asresid, missing.include, 
    	#					format, dnn, ...) 
		ua.2D.crosstab.broker<-function (ua.2D.crossTab.fun.name, uaorilevels,xtab.table.index, t, digits, max.width, expected, 
    						prop.r, prop.c, prop.t, prop.chisq, 
    						chisq, fisher, mcnemar, resid, 
    						sresid, asresid,  oriprop.chisq ,orichisq,actualexpected, orifisher, orimcnemar ,oriresid,orisresid ,oriasresid,missing.include, 
    						format, dnn,factorval,noNonEmptyTables, storeOriLevelOfFirstVarIfOneByOneTable,...)
		{
			# cat("Cross Tab Function Name3: ", deparse(substitute(ua.2D.crossTab.fun.name)), " and ", ua.2D.crossTab.fun.name, " and ", func.name, "\n" )
			#callParamDetails = uaLogFunArgsDetails()
			#uaFunctionInit(callParamDetails)
	
			if(starting.dimension.pos ==1)
			{
				index <- c(xtab.table.index[starting.dimension.pos])
				index <- c(index, ",")
				m <- 1
			}
			else
			{
				index<-c("")
				for(m in 1:(starting.dimension.pos-1) )
				{
					if (index[1] == "")
					 	index[1]<-c(",")
					else
						index <- c(index, ",")
				}
			}

			m<- m + 1
			for (n in m:xtab.dim.len)
			{
				index <- c( index, xtab.table.index[n])
				if ( n != xtab.dim.len)
					index <- c(index, ",")
			}
			
			#args <- as.list(match.call())
			#cat("index for table", eval(deparse(substitute(t)), envir=parent.frame()), "[", index, "]", "\n") 
			# cat("\nCurrent index for the table being processed :: ", deparse(substitute(t)), "[", index, "]", "\n") 

#cat("\n","===========================================================================================", "\n") 
			
			
first.time=1
			
			for ( i in length(xtab.table.index):starting.dimension.pos)
			{
				#cat( names(dimnames(t)[i]), " = " , "\"", dimnames(t)[[i]][xtab.table.index[i]], "\"", " and  ")

				if(first.time == 1)
				{
					first.time = 0
					#We are creating a list
					layer.name.value = list(Layer.Name = names(dimnames(t)[i]), Layer.Value = dimnames(t)[[i]][xtab.table.index[i]])
				}
				else
				{
					#We are creating a list
					layer.name.value = c(layer.name.value , list(Layer.Name = names(dimnames(t)[i]), Layer.Value = dimnames(t)[[i]][xtab.table.index[i]]))
				}
			}
#cat( "\n","==========================================================================================", "\n\n") 
			
			#10/07/2011 The code below generates a matrix of all the levels that
			#we are generating values in the crosstab for
			#Tried to generate a dataframe, challenge with adding the second row as I could not specify the
			#column names, as the number of columns were variable
			#The rbind on the second row was failing
			
			q=2
			bskylen=length(layer.name.value)
			bskytemp=NULL
			while (q <= bskylen)
			{
				bskytemp=c(bskytemp,layer.name.value[[q]])
				q=q+2
			}
			#cat(bskytemp)
			uadatasets$retstructure[[1]]$metadatatable[[3]]=rbind(uadatasets$retstructure[[1]]$metadatatable[[3]],bskytemp)
			
			
			#Construct dynamically the call to the 2D object
			#Here we are getting back all the Statistics from the CrossTable
			factorvals=paste(index, collapse=" ")	
			
			storeOriLevelOfFirstVarIfOneByOneTable=dimnames(t)[[1]]
			
			if (class(eval(parse (text=paste(deparse(substitute(t)), "[", paste(index, collapse=" "),"]")))) =="numeric")
			{
				noOfColumns =dim(t)[2]
				crosstab.2D.return.obj <- eval (parse (text= paste(
						#a<- paste(
								ua.2D.crossTab.fun.name, 
								# "CrossTable",
								"(",
								"uaorilevels =",deparse(uaorilevels,width.cutoff=499),",","matrix(",
								deparse(substitute(t)), 
								"[",
								paste(index, collapse=" "),
								"]",",ncol =",noOfColumns,")",
								",",
								paste(
								"digits =", digits, ",",
								"max.width =", max.width, ",",
								"expected =", expected, ",",
    								"prop.r =", prop.r, ",",
								"prop.c =", prop.c, ",",
								"prop.t =", prop.t, ",",
								"prop.chisq =", prop.chisq, ",",
    								"chisq =", chisq, ",",
								"fisher =", fisher, ",",
								"mcnemar =", mcnemar, ",",
								"resid =", resid, ",",
    								"sresid =", sresid, ",",
								"asresid =", asresid, ",",
								"actualexpected =", actualexpected, ",",
    								"oriprop.t =", oriprop.t, ",",
								"oriprop.chisq =", oriprop.chisq, ",",
    								"orichisq =", orichisq, ",",
								"orifisher =", orifisher, ",",
								"orimcnemar =", orimcnemar, ",",
								"oriresid =", oriresid, ",",
    								"orisresid =", orisresid, ",",
								"oriasresid =", oriasresid, ",",
								"layerinfo=", deparse(bskytemp,width.cutoff=499 ),",",
								"missing.include =", missing.include, ",",
    								paste("format = \"", format, "\"", sep="", collapse=""),  ",",
								"dnn =", dnn, ",",
								paste("factorvals=","\"",factorvals,"\""),",","noNonEmptyTables =", noNonEmptyTables,",","storeOriLevelOfFirstVarIfOneByOneTable=",deparse(storeOriLevelOfFirstVarIfOneByOneTable,width.cutoff=499),",",
								" ...", 
								collapse=" "
 								),
								")"
							)
					)
				)
				#cat(bskytemp)
}

			
			
			else
			{
			
						
 				crosstab.2D.return.obj <- eval (parse (text= paste(
						#a<- paste(
								ua.2D.crossTab.fun.name, 
								# "CrossTable",
								"(",
								"uaorilevels =",deparse(uaorilevels,width.cutoff=499),",","as.matrix(",
								deparse(substitute(t)), 
								"[",
								paste(index, collapse=" "),
								"]",")",
								",",
								paste(
								"digits =", digits, ",",
								"max.width =", max.width, ",",
								"expected =", expected, ",",
    								"prop.r =", prop.r, ",",
								"prop.c =", prop.c, ",",
								"prop.t =", prop.t, ",",
								"prop.chisq =", prop.chisq, ",",
    								"chisq =", chisq, ",",
								"fisher =", fisher, ",",
								"mcnemar =", mcnemar, ",",
								"resid =", resid, ",",
    								"sresid =", sresid, ",",
								"asresid =", asresid, ",",
								"actualexpected =", actualexpected, ",",
    								"oriprop.t =", oriprop.t, ",",
								"oriprop.chisq =", oriprop.chisq, ",",
    								"orichisq =", orichisq, ",",
								"orifisher =", orifisher, ",",
								"orimcnemar =", orimcnemar, ",",
								"oriresid =", oriresid, ",",
    								"orisresid =", orisresid, ",",
								"oriasresid =", oriasresid, ",",
								"layerinfo=", deparse(bskytemp,width.cutoff=499 ),",",
								"missing.include =", missing.include, ",",
    								paste("format = \"", format, "\"", sep="", collapse=""),  ",",
								"dnn =", dnn, ",",
								paste("factorvals=","\"",factorvals,"\""),",","noNonEmptyTables =", noNonEmptyTables,",","storeOriLevelOfFirstVarIfOneByOneTable=",deparse(storeOriLevelOfFirstVarIfOneByOneTable,width.cutoff=499),",",
								" ...", 
								collapse=" "
 								),
								")"
							)
					)
				)
				#cat(bskytemp)
			}
			# cat("Function Call:", "\n", a, "\n")
			# cat("\n", "return list for the cross tab index [", index, "]", names(return.list), "\n")
			
			#Aaron This is the layer 
			return.list<-list(layer.name.value = layer.name.value, crosstab.2D.return.obj = crosstab.2D.return.obj)
			#Aaron This is only for 1 iteration
			#This id is 4000
			#uaFunctionWrapUp(callParamDetails)
			invisible(return.list)
		} # End of ua.2D.crosstab.broker<-function 
		
		# Valerie
		
		
		
		
		#if (any(dim(t) < 2)) {
        		#prop.chisq <- chisq <- expected <- fisher <- mcnemar <- resid <-sresid <-aresid <-FALSE
    	#}
		
		#This variable is introduced to track the number of tables for which we can calculate accurate
		#statistics from. The challenge is with tables with zero counts, we cannot calculate accurate
		#statistics from these tables and since the counts are all zeros, we ignore these tables in
 		#uadatasets$crosstabres and the uadatasets$crosstabstats. However for error handling, especially in the case of a table 
 		#where there is a single positive count and all other counts are 0's, we don't run the chisq as it is invalid to run such a test
		# as the dimensions of the row variables is less than 2. We need to handle the footer 
		#for the statistics results (uadatasets$crosstabstats) stating why we have not calculated the chi square, 
		#In order to insert the footer in the appropriate position, we need to track the valid rows in uadatasets$crosstabstats
		#This is exactly what noNonEmptyTables=1 handles
		noNonEmptyTables=0
		for (k in 1:total.num.2D.table.calls)
		{
			if(xtab.dim.loop.counter[xtab.dim.len] >0)
			{
				####Aaron 10/27/2014
				#cat("\n Current location for 2D cross tab",xtab.dim.loop.tracking)
				uatemp <-xtab.dim.loop.tracking
				crosstab.2D.return.obj <- ua.2D.crosstab.broker(ua.2D.crossTab.fun.name, uaorilevels,xtab.dim.loop.tracking, t, digits, max.width, expected, 
   						prop.r, prop.c, prop.t, prop.chisq, 
    						chisq, fisher, mcnemar, resid, 
    						sresid, asresid, oriprop.chisq ,orichisq,actualexpected, orifisher, orimcnemar ,oriresid,orisresid ,oriasresid,missing.include, 
    						format, dnn, factorvals,noNonEmptyTables,storeOriLevelOfFirstVarIfOneByOneTable,...) 
				#crosstab.2D.return.obj$crosstab.2D.return.obj$tableAllZeroCounts=TRUE when all rows of the table t are 0s i.e. ALL counts are 0s
				#In this situation there is no need to construct the crosstab as we ignore the entire table from the display, uamatdis is correctly set to ignore all rows for this table
				#In this situation there is no need to call uahandlenainpercents as we are not adding to crosstabres
				
				noNonEmptyTables=crosstab.2D.return.obj$crosstab.2D.return.obj$noNonEmptyTables
				if (!crosstab.2D.return.obj$crosstab.2D.return.obj$tableAllZeroCounts)
				{
				crosstab.2D.return.obj$crosstab.2D.return.obj$prop.col <-uahandlenainpercents(crosstab.2D.return.obj$crosstab.2D.return.obj$table,crosstab.2D.return.obj$crosstab.2D.return.obj$prop.col)
				crosstab.2D.return.obj$crosstab.2D.return.obj$prop.row <-uahandlenainpercents(crosstab.2D.return.obj$crosstab.2D.return.obj$table,crosstab.2D.return.obj$crosstab.2D.return.obj$prop.row)
				
				###############Code added to handle the totals and building the crosstab in memory
				#xtab.dim.loop.tracking
				newcreateglobalcrosstab(xtab.dim.orig,crosstab.2D.return.obj, expected, 
				prop.r , prop.c, prop.t, prop.chisq , chisq, fisher, mcnemar, resid, 
				sresid, asresid, oriprop.chisq ,orichisq,actualexpected, orifisher, orimcnemar ,oriresid,orisresid ,oriasresid,missing.include,... )
				
			}
				else
				#lets just delete the row instructing Vishal to display the counts for the levels for which the crosstab is all 0s
				{
					uadatasets$retstructure[[1]]$metadatatable[[3]]=uadatasets$retstructure[[1]]$metadatatable[[3]][-nrow(uadatasets$retstructure[[1]]$metadatatable[[3]]),]
					
				}
			
				#This is the code that handles the totals
				#fincrosstab(crosstab.2D.return.obj,xtab.dim.loop.tracking,xtab.dim.orig)
				###############
 				
				#id is 4000
				#return.result.set$result.list.of.objects <- c(return.result.set$result.list.of.objects, crosstab.2D.result.obj = crosstab.2D.return.obj)
				
				#return.result.set$result.list.of.objects[[k]]=crosstab.2D.return.obj
				#k is the iteration number
				#K goes from 1 to 10
				#return.result.set$result.list.of.objects
				
				table.dim <- starting.dimension.pos 
				xtab.dim.loop.counter[table.dim] <- xtab.dim.loop.counter[table.dim] -1
				aarontrackinner <-xtab.dim.loop.tracking
				
				xtab.dim.loop.tracking[table.dim]<- (xtab.dim.orig[table.dim] +1) - xtab.dim.loop.counter[table.dim]  
				#cat ("\n Ha ha xtab.dim.loop.counter", xtab.dim.loop.counter)

				if( table.dim != xtab.dim.len  && xtab.dim.loop.counter[table.dim] == 0)
				{
					#cat ("\n This is the inner most loop xtab.dim.loop.counter", xtab.dim.loop.counter)
					#cat ("\n This is the inner most loop xtab.dim.loop.tracking", xtab.dim.loop.tracking)
					#cat ("\n This is the inner most loop xtab.dim.loop.tracking", aarontrackinner)
					xtab.dim.loop.counter[table.dim] <- xtab.dim.orig[table.dim] 
					xtab.dim.loop.tracking[table.dim]<- (xtab.dim.orig[table.dim] +1) - xtab.dim.loop.counter[table.dim]
					

					repeat
					{	
						table.dim <- table.dim + 1

						xtab.dim.loop.counter[table.dim] <- xtab.dim.loop.counter[table.dim] -1
						aarontrack <-xtab.dim.loop.tracking
						
						xtab.dim.loop.tracking[table.dim]<- xtab.dim.orig[table.dim] +1 - xtab.dim.loop.counter[table.dim]
						#if (xtab.dim.loop.counter[table.dim] != 0) cat("\n Tracking ",xtab.dim.loop.tracking)

						if( table.dim != xtab.dim.len  && xtab.dim.loop.counter[table.dim] == 0)
						{
							
							#cat ("\nAaron xtab.dim.loop.counter", xtab.dim.loop.counter)
						    #cat ("\nAaron xtab.dim.loop.tracking", xtab.dim.loop.tracking)
							#cat ("\nAaron xtab.dim.loop.tracking", aarontrack)
							xtab.dim.loop.counter[table.dim] <- xtab.dim.orig[table.dim]
							xtab.dim.loop.tracking[table.dim]<- (xtab.dim.orig[table.dim] +1) - xtab.dim.loop.counter[table.dim] 
							
						}
						else
						{
							break
						}
					}
				}
			}
		}
#Insert the code here, this is the grand of the grand total
}
	#uaFunctionWrapUp("ua.MultDimCrossTab")
	BSkyFunctionWrapUp()
	invisible(TRUE)
}




		ua.2D.crosstab<-function (uaorilevels,t,  digits = 3, max.width = 5, expected = FALSE, 
			prop.r = TRUE, prop.c = TRUE, prop.t = TRUE, prop.chisq = TRUE, 
			chisq = FALSE, fisher = FALSE, mcnemar = FALSE, resid = FALSE, 
			sresid = FALSE, asresid = FALSE,oriprop.chisq ,orichisq,actualexpected, orifisher, orimcnemar ,oriresid,orisresid ,oriasresid,layerinfo="",missing.include = FALSE, 
			format = NULL, dnn = NULL, factorvals=NULL,noNonEmptyTables,storeOriLevelOfFirstVarIfOneByOneTable, ...) 
		{
			#callParamDetails = uaLogFunArgsDetails()
			#uaFunctionInit()
				BSkyFunctionInit()
			#Used to store the original value of expected
			oriexpected=NULL
			#Added by Aaron to handle uamatdis and clean up the table so that statistics can be run on it
			#Holds the index of all the rows that contain all 0s
			ua.crosstab.row.size<-nrow(t)
			ua.crosstab.col.size<-ncol(t)
			#cat(layerinfo)
			uatempindex=NULL
			#uadatasets$uacrosstabres=NULL
			oddsRatio=NA
			
			#Added by Aaron 02/19/2021
			ua.CST <-NULL
			
			#orilevels=levels(uadatasets$lst[[13]]$store)
			#These are all the dimensions in the table minus the unused levels 
			
			newlevels=dimnames(t)[[1]]
			#this is the case when every table in t has one row and one column. In this situation
			#every table returned by t[,,1],t[,,2] has a single row and column and the table passed to ua.2D.crosstan
			#has a single value and hence newlevels=dimnames(t)[[1]] returns null
			#we use storeOriLevelOfFirstVarIfOneByOneTable to store the orifinal levels of the first dimension
			# See code below to recreate
			#crosst<-NULL
		#crosst <-BSkyCrossTable(x=c('cylinder'),y=c('origin'),layer=c('year'),datasetname="forcross",chisq = TRUE,prop.r=TRUE,prop.c=TRUE, resid=FALSE, sresid=TRUE, expected=FALSE)
		#crosst
			if (is.null(newlevels)) 
				newlevels=storeOriLevelOfFirstVarIfOneByOneTable
			uaindofmisslevels= which (uaorilevels %in% newlevels ==FALSE)
			
			
			#Code below returns the index of all the rows with all zeros
			uanoofrows =nrow(t)
			l=1
			for (l in 1:uanoofrows)
			if (sum(t[l,]) ==0)	uatempindex =c(uatempindex, l)
			
			#This function prepares the subset of the table from xtabs for running the 2 d cross tab function
			#In order for the statistical tests to work, all the rows on the table where the counts are zero, need to be removed
			#For displaying, all the columns with zero need to be set to NA. We return the following
			#uaretstructure$t ->all rows with 0s removed, all cols with 0s set to NA
			#uaretstructure$tpercents ->all rows with only 0s removed. Cols with only 0s are not set to NA
			#uaretstructure$tforstats -> table with only columns that are not zero and rows that are not zero
			#The table required for calculating the percents require all the rows with zeros removed but all the cols with zero are needed

			uaretstructure=	uapreptableforstats(t,uatempindex)
			
			#######
			#added by Aaron 08/25
			#######
			##This is the case where all the counts ate 0
			##we don't display these entries
			ua.CSTexpected<-array(c(NA),dim=c(ua.crosstab.row.size,ua.crosstab.col.size))
			ua.prop.chisq<-array(c(NA),dim=c(ua.crosstab.row.size,ua.crosstab.col.size))
			ua.prop.r<-array(c(NA),dim=c(ua.crosstab.row.size,ua.crosstab.col.size+1))
			ua.prop.c<-array(c(NA),dim=c(ua.crosstab.row.size,ua.crosstab.col.size))
			ua.prop.t<-array(c(NA),dim=c(ua.crosstab.row.size,ua.crosstab.col.size))
			ua.resid<-array(c(NA),dim=c(ua.crosstab.row.size,ua.crosstab.col.size))
			ua.sresid<-array(c(NA),dim=c(ua.crosstab.row.size,ua.crosstab.col.size))
			ua.asresid<-array(c(NA),dim=c(ua.crosstab.row.size,ua.crosstab.col.size))

			#ua.CT<-list(NA) #return list object
			#row percents
			if (nrow(uaretstructure$t)==0) 
			{
				#You need to remember that although uaretstructure$t has no rows, the counts table t 
				#is a table that is non empty has has all 0s and the counts in all the rows are os
				#uamatdis handles this appropriately
				
				#I have commented this as if the table of counts is all zeros, we are not displaying
				#uatempindex=uacreatuamatdis(t,uaindofmisslevels,uaorilevels,prop.r,prop.c,expected,resid,sresid)
				ua.CT =list(noNonEmptyTables=noNonEmptyTables, tableAllZeroCounts=TRUE)
				#uaFunctionWrapUp("ua.2D.crosstab")
				BSkyFunctionWrapUp()
				return(ua.CT)
			}
			noNonEmptyTables=noNonEmptyTables+1
				if (any(dim(uaretstructure$uatforstats) < 2)) 
				{
					#The original code is below
					#prop.c <- prop.r <- prop.chisq <- chisq <- expected <- fisher <- mcnemar <- FALSE
					#the revised code so that we still display row and column percents
					
					if (orichisq)
					{
						BSkyfootermsg ="Chi sq test cannot be run as row or column variables are constants (the number of levels in the row or column variables are less than 2)";
					uadatasets$retstructure[[2]]$metadatatable[[1]] =rbind(uadatasets$retstructure[[2]]$metadatatable[[1]],data.frame(varIndex=NA,type=2,varName=NA,dataTableRow=noNonEmptyTables,startCol=NA,endCol=NA,BSkyMsg=BSkyfootermsg,RMsg= NA))
					}
					if (expected)
					{
						#uadatasets$uamatdis1 =rbind(uamatdis,c(NA,"-3",NA,NA,NA,NA,"Expected values cannot be calculated as the dataset is empty", NA))
						#uawritelog(type="Error", uberfunct="uaonesmt.test",uamessage ="Chi sq test cannot be run as the dataset is empty");
						
						ua.CSTexpected =uaretstructure$t
						#Saving the original value of expected, this is because we have to set the flag to off so that the code in section A
						#does not get executed, we will set to original value of true later
						oriexpected=expected
						expected=FALSE
					}
					prop.chisq <- chisq <-  fisher <- mcnemar<-asresid <-resid<-sresid<- FALSE
				}
				if (any(dim(uaretstructure$uatforstats) != 2))
				{
					if (orimcnemar)
					{
						#BSkyfootermsg =sprintf("Mcnemar test cannot be run as %s and %s are constants",names(dimnames(t))[1],names(dimnames(t))[2])
						#BSkyfootermsg =paste("Mcnemar test cannot be run as %s and %s are constants",)
						#BSkyfootermsg ="McNemar test cannot be run as row variable and column variables don't create a 2x2 table";
						
						if (layerinfo =="")
						BSkyfootermsg <-"McNemar test cannot be run as row variables and column variables don't create a 2x2 table"
						else
						BSkyfootermsg <-paste ("McNemar test cannot be run as row variables and column variables don't create a 2x2 table for the following values in the layer variables:-", paste (layerinfo, collapse=","))
						uadatasets$retstructure[[2]]$metadatatable[[1]] =rbind(uadatasets$retstructure[[2]]$metadatatable[[1]],data.frame(varIndex=NA,type=2,varName=NA,dataTableRow=noNonEmptyTables,startCol=NA,endCol=NA,BSkyMsg=BSkyfootermsg,RMsg= NA))
					}
					if (orifisher)
					{
						# BSkyfootermsg =sprintf("Mcnemar test cannot be run as %s and %s are constants",names(dimnames(t))[1],names(dimnames(t))[2])
						#BSkyfootermsg ="Fisher test cannot be run as row variable  and column variable  don't create a 2x2 table";
						
						if (layerinfo=="")
						BSkyfootermsg <-"Fisher test cannot be run as row variables and column variables don't create a 2x2 table"
						else
						BSkyfootermsg <-paste ("Fisher test cannot be run as row variables and column variables don't create a 2x2 table for the following values in the layer variables:-", paste (layerinfo, collapse=","))
						uadatasets$retstructure[[2]]$metadatatable[[1]] =rbind(uadatasets$retstructure[[2]]$metadatatable[[1]],data.frame(varIndex=NA,type=2,varName=NA,dataTableRow=noNonEmptyTables,startCol=NA,endCol=NA,BSkyMsg=BSkyfootermsg,RMsg= NA))
					}
					fisher <- mcnemar<-FALSE
				}	
				
				
			

			ua.crosstab.row.size<-nrow(uaretstructure$t)
			ua.crosstab.col.size<-ncol(uaretstructure$t)

			#it was here below
			ua.CPR <- prop.table(uaretstructure$tpercent, 1)
			#column percents
			ua.CPC <- prop.table(uaretstructure$tpercent, 2)
				
			ua.CPT <- prop.table(uaretstructure$tpercent)
			ua.GT <- sum(uaretstructure$t,na.rm=TRUE)
			ua.RS <- rowSums(uaretstructure$t,na.rm=TRUE)
			ua.CS <- colSums(uaretstructure$t,na.rm=TRUE)
			
			if (chisq) {
					 if (all(dim(uaretstructure$uatforstats) == 2))
					 {
					 
								if (any(uaretstructure$uatforstats==0))
								{
									oddsRatio =NA
									#warning ("Odds ratio could not be computed as one of the counts was 0")
									if(layerinfo!="")
									{
									msg = paste("Odds ratio could not be computed as one of the counts was 0, for layer ", layerinfo)
									}
									else
									{
									msg="Odds ratio could not be computed as one of the counts was 0";
									}
						#BSkyfootermsg ="Odds ratio could not be computed as one of the counts was 0";
					uadatasets$retstructure[[2]]$metadatatable[[1]] =rbind(uadatasets$retstructure[[2]]$metadatatable[[1]],data.frame(varIndex=NA,type=2,varName=NA,dataTableRow=noNonEmptyTables,startCol=3,endCol=6,BSkyMsg=msg,RMsg= NA))									
								}
								else
								{
									ua.CSTc <- chisq.test(uaretstructure$uatforstats, correct = TRUE)
									b=uaretstructure$uatforstats
									b[2,]=uaretstructure$uatforstats[1,]
									b[1,]=uaretstructure$uatforstats[2,]

									oddsRatio =BSkyorrr(b,verbose=FALSE)
								}
					}
						ua.CST <- suppressWarnings(chisq.test(uaretstructure$uatforstats, correct = FALSE))
						
						#Added by Aaron 07/27/2019
						
						
						
				}
			else if (prop.chisq | sresid|asresid|expected| resid){
				ua.CST <- suppressWarnings(chisq.test(uaretstructure$uatforstats, correct = FALSE))
			}

			if (fisher) {
						ua.FTt <- fisher.test(uaretstructure$uatforstats, alternative = "two.sided")
						 if (all(dim(uaretstructure$uatforstats) == 2)) {
								ua.FTl <- fisher.test(uaretstructure$uatforstats, alternative = "less")
								ua.FTg <- fisher.test(uaretstructure$uatforstats, alternative = "greater")
				}
					}

			if (mcnemar) {
				#Commented by Aaron 10/21/2018
				#ua.McN <- mcnemar.test(uaretstructure$uatforstats, correct = FALSE)
				 if (all(dim(uaretstructure$uatforstats) == 2)) 
							ua.McNc <- mcnemar.test(uaretstructure$uatforstats, correct = TRUE)
					}

			if (asresid) 
					{
						#Note the t below is transpose
						ua.CSnew <- colSums(uaretstructure$uatforstats,na.rm=TRUE)
						ua.ASR <- (ua.CST$observed - ua.CST$expected)/sqrt(ua.CST$expected *  ((1 - ua.RS/ua.GT) %*% t(1 - ua.CSnew/ua.GT)))
						
								
					}
							
							
							
			#section A section A
			# The original code was ua.CSTexpected[i,k] <-ua.CST$expected[i,]
			#This did not work as we have stripped out all the cols with 0s to calculate the CHI square
			#Note: ua.CST$expected is computed from the CHI square
			#ua.CSTexpected expects all the colums now ever the CHI square is calculated with the reduced column set.
			#The code below inserts the correct expected values into ua.CSTexpected while honoring the NA (columns that have all zeros)
			for(i in 1:nrow(uaretstructure$t)) {
							if (expected) 
							{
								uatemp<-ua.CST$expected[i,]
								k=1
								j=1
								while (k <=length(ua.CSTexpected[i,]))
								{
									if (!is.na(uaretstructure$t[i,k]))	
									{
										ua.CSTexpected[i,k]<- uatemp[j]
										j=j+1
									}
								k=k+1
								}
								
							}
						 if (prop.chisq) 
								ua.prop.chisq[i,]<-((ua.CST$expected[i, ] - t[i, ])^2)/ua.CST$expected[i,] 
						if (prop.r) 
								ua.prop.r[i,]<-c(ua.CPR[i, ] * 100, 100 * ua.RS[i]/ua.GT)
						if (prop.c) 
							ua.prop.c[i,]<-ua.CPC[i, ] * 100
						if (prop.t) 
								ua.prop.t[i,]<-ua.CPT[i, ] * 100
								
								
			# The original code was ua.resid[i,k] <-ua.CST$observed[i, ] - ua.CST$expected[i,]  
			#This did not work as we have stripped out all the cols with 0s to calculate the CHI square
			#Note: ua.CST$expected and ua.CST$observed is computed from the CHI square
			#ua.CSTexpected expects all the colums now ever the CHI square is calculated with the reduced column set.
			#The code below inserts the correct expected values into ua.CSTexpected while honoring the NA (columns that have all zeros)
						if (resid) 
						{
							uatemp<-ua.CST$observed[i, ] - ua.CST$expected[i,] 
							k=1
							j=1
							while (k <=length(ua.resid[i,]))
								{
									if (!is.na(uaretstructure$t[i,k]))	
									{
										ua.resid[i,k]<- uatemp[j]
										j=j+1
									}
								k=k+1
								}
						}
			# The original code was ua.sresid[i,k] <-ua.CST$residual[i, ] 
			#This did not work as we have stripped out all the cols with 0s to calculate the CHI square
			#Note: ua.CST$residual[i, ] is computed from the CHI square
			#ua.CSTexpected expects all the colums now ever the CHI square is calculated with the reduced column set.
			#The code below inserts the correct expected values into ua.CSTexpected while honoring the NA (columns that have all zeros)
						if (sresid) 
						{
							uatemp <-ua.CST$residual[i, ]
							k=1
							j=1
							while (k <=length(ua.sresid[i,]))
								{
									if (!is.na(uaretstructure$t[i,k]))	
									{
										ua.sresid[i,k]<- uatemp[j]
										j=j+1
									}
								k=k+1
								}
						}

					
						if (asresid) 
						{
							
							#ua.asresid[i,]<-ua.ASR[i, ]
							
							#uatemp <-ua.CST$residual[i, ]
							uatemp <-ua.ASR[i,]
							k=1
							j=1
							while (k <=length(ua.asresid[i,]))
								{
									if (!is.na(uaretstructure$t[i,k]))	
									{
										ua.asresid[i,k]<- uatemp[j]
										j=j+1
									}
								k=k+1
								}
						}
			} 	
			
			###Here is where we build the return object
			OR=NA
			lower.OR=NA
			upper.OR=NA
			
			ua.CT <- list(table = uaretstructure$t, prop.row = ua.CPR, prop.col = ua.CPC, prop.tbl = ua.CPT,noNonEmptyTables=noNonEmptyTables, tableAllZeroCounts=FALSE)
			ua.CT <- c(ua.CT, list(grand.total=ua.GT, row.sum=ua.RS, col.sum=ua.CS))
			ua.CT <- c( ua.CT, list(CSTexpected= ua.CSTexpected,  prop.chisq=ua.prop.chisq, resid=ua.resid,  sresid1= ua.sresid,  asresid=ua.asresid))

					if (any(chisq, fisher, mcnemar)) {	
						if (all(dim(uaretstructure$t) == 2)) {
								if (chisq) 
									{
									#Modified by Aaron on 09/25/2011. Commented line below
									#ua.CT <- c(ua.CT, list(chisq = ua.CST, chisq.corr =  ))
									
									##Added by Aaron 07/27/2019
									
									if (!is.na(oddsRatio))
										{
											ua.CT <- c(ua.CT, list(chisq = ua.CST, OR = oddsRatio, lower.OR =attr(oddsRatio, "lower.OR", exact=TRUE), upper.OR =attr(oddsRatio, "upper.OR", exact=TRUE) ))
										}
									else
										{
											ua.CT <- c(ua.CT, list(chisq = ua.CST, OR = NA, lower.OR =NA, upper.OR =NA ))
										}
			
									}
									
							if (fisher) 
									 ua.CT <- c(ua.CT, list(fisher.ts = ua.FTt, fisher.tl = ua.FTl, fisher.gt = ua.FTg))
								if (mcnemar)
									ua.CT <- c(ua.CT, list(mcnemar = ua.McNc))
									#ua.CT <- c(ua.CT, list(mcnemar = ua.McN, mcnemar.corr = ua.McNc))
						}
						else {
							 
							
							 if (chisq) 
									ua.CT <- c(ua.CT, list(chisq = ua.CST))
								#We ran a fisher test only if all Dimensions =2
								if (fisher)
								{
									if (layerinfo=="") warning("Fisher test cannot be run as a 2x2 table is not generated")
									else
									warning(paste("Fisher test cannot be run as a 2x2 table is not generated for the following values in the layer variables:-", paste (layerinfo, collapse=", ")))
								}
								#	ua.CT <- c(ua.CT, list(fisher.ts = ua.FTt))
								#Commented by Aaron 10/21/2018
								#McNemar does not work under the condition that its not a 2 by 2 table
								if (mcnemar) 
								{
								if (layerinfo=="") warning("McNemar test cannot be run, McNemar's test requires a 2x2 table ")
								
								else
									warning(paste("McNemar test cannot be run as a 2x2 table is not generated for the following values in the layer variables:-", paste (layerinfo, collapse=", ")))
								
								}
								#ua.CT <- c(ua.CT, list(mcnemar = ua.McN))
									
						}
					}
			#The function below creates the display matrix telling us which rows and columns to display in the output
			#It also checks for all the rows that contain 0 in every column. These rows need to be deleted and the
			#counts, row percents and column percents need to be hidden	
			#we are creating the display matrix at the end because the CHI square can fail. If it fails we cannot calculate
			#expected counts, redisuals a	nd std residuals even if the user has requested it and we need to adjust accordingly
			
			#uatempindex=uacreatuamatdis(t,uaindofmisslevels,uaorilevels,prop.r,prop.c,expected,resid, sresid, asresid)
			
			if (!is.null(oriexpected))expected =TRUE
			
			uatempindex=uacreatuamatdis(t,uaindofmisslevels,uaorilevels,prop.r,prop.c,expected,resid,sresid,asresid,oriprop.chisq ,orichisq,actualexpected, orifisher, orimcnemar ,oriresid,orisresid ,oriasresid)
			#uaFunctionWrapUp(callParamDetails)
			#uaFunctionWrapUp("ua.2D.crosstab")
				BSkyFunctionWrapUp()
			invisible(ua.CT)
		}



