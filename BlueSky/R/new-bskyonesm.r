######## list of methods in this file; in sequence (few may not be ready and unused ) ###########
# bsky.one.sm.t.test.oldest
# bsky.one.sm.t.test.old
# BSkyOneSmTTest
#################################################################################################
bsky.one.sm.t.test.oldest<- function(vars, mu=0,conf.level=0.95,datasetname, missing=0)
{
      # Must be called within uber or any sub function as the fist function call
      # This is to gather the call parameters passed. This is needed within
      # warning and error handler to log the calling details in logfile
    	#callParamDetails = uaLogFunArgsDetails()
    	# print( callParamDetails )
    	
      # To be called only once from the uber UA function
    	# This function should not be called form any sub function that uber UA function may call
    	# Subfuction will call another function -  uaFunctionInit()
    	#uaGlobalInit()

    	uaGlobalInit()
    	
    	 #used to track the number of tables returned
      nooftablestodis =0
      
       uadatasets$retstructure <-list()
     
       # To build up the list of stat result tables returned by ua sub function                  
    	 uaStatResults =list()
    	
    	BSkyErrMsg =paste("Error in One Sample t.test"," Dataset Name:", datasetname, "Variables:",paste (vars,collapse=","))
		BSkyWarnMsg =paste("Warning in One Sample t.test"," Dataset Name:", datasetname, "Variables:",paste (vars,collapse=","))
	
    	tryCatch(
    		{
    			withCallingHandlers(
    			{
      				uaSplitIterationCount = BSkyComputeSplitdataset(vars, datasetname)
					#cat("in 1 sm t test", uaSplitIterationCount )
      
      				# if there is no split, usSplitIterationCount = 1, otherwise usSplitIterationCount = cart product count
      				for (j in 1:uaSplitIterationCount)
					{
						uaDatasetSliceIndex = BSkyGetNextDatasetSliceIndex(vars, datasetname)
        					
        				index = uaDatasetSliceIndex$datasetSliceIndex
        				# print(uadatasets$lst[index])
        					
						uavarindex = uaDatasetSliceIndex$varIndex
						# print(uavarindex)
                  
						if( uaIsDatasetSplit(datasetname) == TRUE)
						{ 
						 # cat("Current factor names and values : \n", BSkyComputeCurrentVarNamesAndFactorValues(j, datasetname), "\n\n")
						}
                  
						# Construct Application level warning message that will be logged within Warning handler function
						# uaWarnmsgdis = sprintf("<construct wahtever application level warning msg needed> such as function name, veriable names etc")
						# Construct Application level error message that will be logged within error handler function
   					    # uaErrmsgdis = sprintf("<construct wahtever application level error msg needed> such as function name, veriable names etc")
						# msg <- paste (msg, paste(y, sep=" ", collapse = " "))  
    		          
						# Construct Application level message as one single string - so that 
						# if there is an error or warnign generated, the error and warning 
						# handler can print this msg string in the log
						# Constructing a single string is tricky becuse of the variable of vector type
						# Those vector variable need to be flattened out first before pasting
						# with our string  e.g. paste(vars, collapse = " ")
    		          
						#uaWarnErrMsgdis = ""
						#uaWarnErrMsgdis = paste(uaWarnErrMsgdis, paste("Dataset Name :", datasetname))
						#uaWarnErrMsgdis = paste(uaWarnErrMsgdis, sep=" * ", "Variables :")
						#uaWarnErrMsgdis = paste(uaWarnErrMsgdis, paste(vars, collapse = ","))
						#uaWarnErrMsgdis = paste(uaWarnErrMsgdis, sep=" * ", "Current Factors, if there is Split :")
						#uaWarnErrMsgdis = paste(uaWarnErrMsgdis, paste(BSkyComputeCurrentVarNamesAndFactorValues(j, datasetname), collapse = ","))
						#uaWarnErrMsgdis = paste(uaWarnErrMsgdis, sep=" * ", paste(uaGetCurrentSplitLevel(),collapse = ","))
                  
						if (uaSplitIterationCount>1)
        				{
        						BSkyErrMsg = paste(BSkyErrMsg, "Current Factors, if there is Split :",paste(BSkyComputeCurrentVarNamesAndFactorValues(j, datasetname), collapse = ","),sep=" * ")
        						BSkyWarnMsg = paste(BSkyWarnMsg, "Current Factors, if there is Split :",paste(BSkyComputeCurrentVarNamesAndFactorValues(j, datasetname), collapse = ","),sep=" * ")
        				}
						uaStoreApplicationWarnErrMsg(BSkyErrMsg,BSkyWarnMsg)
						# print(uaWarnErrMsgdis)
                  
						#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
						### Example - Do Stat stuff that this function is supposed to do
						#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
						#warning("Forced warning msg in uber function")
						#if(j == 1 || j == 2)
						{
						   #tt= "test"
						   #as.numeric(tt)
						   #uaTestSubfuncWithHandler(uavarindex, mu,conf.level,index, missing)
						   #uaTestSubfuncNoHandler(uavarindex, mu,conf.level,index, missing)
						}
						#inducing an error or warning to test the error handler
						#warning("Forced warning msg")
						#forced error
						#a <- b + 1
						#if(j == 3)   a <- bbb + 1
      				    #cat ("dataset slice index ", index, "\n")
      				    #cat ("variable index for the slice datasets : ", uavarindex, "\n")
      				    #cat (" data set passed to usonesample \n")
      				    #print (uadatasets$lst[index])
						
                	#cat("\n uavarindex = ", uavarindex,"\tindex=",index,"\n")
						# The call to the actual Stat Analytic function
						uaonesample(uavarindex, mu,conf.level,index, missing)

						#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
						# Fill out the uamat and datatable for each split iteration in the return result section
						#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
						# for testing only
						#uatemp = list(NULL)
						#uatemp[[1]]$type="table"
						#numoftablestructures =  length(uatemp)
               	  
						numoftablestructures =  length(uadatasets$retstructure)
		# cat("\n Aaron's No. of Tables = ", numoftablestructures,"\n")
		# print(uadatasets$retstructure)
		# cat("\n###########################\n")						
						for (k in 1:numoftablestructures)
						{
							uadatasets$retstructure[[k]]$cartlevel = BSkyComputeCurrentVarNamesAndFactorValues(j, datasetname)
							uadatasets$retstructure[[k]]$varindex = BSkygetIndexesOfCols(vars, datasetname)
							uaStatResults <- c(uaStatResults, list(uadatasets$retstructure[[k]]))
							#This variable calculates the number of tables as there could be empty datasets for splits 
							#where there is no data. In the case of an empty dataset, we are only returning a single 
							#table per split
							nooftablestodis=nooftablestodis+1
						}
						
						#reinitializing the return structure to preapare for the next split iteration if there is a split
				        uadatasets$retstructure <-list()  
						
					} # End of the split iteration loop
          	  
					#cat ("\n Printing uaresults structure containing umat, uamatdisp etc \n")
					#print( uaStatResults)
    			},
    			warning = UAwarnHandlerFn
    			) # end of withCallingHandlers for catching warnings and continuing execution
    		},
    		error = UAerrHandlerFn,
    		silent =TRUE
    	)
    
    	############################################################################################################
    	# Do other stuff whatever necessary coming out of the try catch block before returning from this UA function
    	############################################################################################################
    
    	if( uaGlobalErrorFound() == TRUE)
    	{
			#if any error trapped by the above uber try catch block add the uamatdisp to the end of the 
			# return table structure
			#uaStatResults <- c(uaStatResults, list(WriteLatestUberErrorInReturnTables()))
		
			#If there is an untrapped error, we add the error message to the normal metadata table of the return structure that was
			#most recently created.
			#Each return structure corresponds to a table to be displayed.
			#If a return structure has not been created, we will create a return stucture. This is because we will always return
			# a single table and the corresponding return structure to the front end (application).
			#Untrapped errors are always critical errors with type -2
			#Untrapped errors are added to the metadata table of the last return structure although there are not necessarily 
			#associated with that return structure and the associated table.
			#This means we can have a situation where you are running a t.test on 3 variables, tg0, tg1,tg2 
			#The t.test runs correctly and the datatable is populated.
			#As there are no errors with the datatable, hence there are no entries in the meta data table corresponding to the rows 
			#and columns with that datatable.
			#Now After we have created the return structure for the t.test we run into an untrapped error before the 
			#return structure of the next table is created (Lets assume after the t.test, we run a correlation test on tg0, tg1, tg2.
			#The untrapped error is added to the existing return structure, for the t.test.
			#The application neeeds to handle this appropriately so that the critical untrapped error is displayed below the 
			#results of the t.test. 
			
			WriteLatestUberErrorInReturnTables()
			numoftablestructures =  length(uadatasets$retstructure)
		#cat("\n Aarons Global Error No. of tables = ", numoftablestructures,"\n")
		#print(uadatasets$retstructure)
		#cat("\n###########################\n")
			for (k in 1:numoftablestructures)
			{
				uadatasets$retstructure[[k]]$cartlevel = BSkyComputeCurrentVarNamesAndFactorValues(j, datasetname)
				uadatasets$retstructure[[k]]$varindex = BSkygetIndexesOfCols(vars, datasetname)
				uaStatResults <- c(uaStatResults, list(uadatasets$retstructure[[k]]))
				#This variable calculates the number of tables as there could be empty datasets for splits 
				#where there is no data. In the case of an empty dataset, we are only returning a single 
				#table per split
				nooftablestodis=nooftablestodis+1
			}
			#nooftablestodis = nooftablestodis + 1
			#print ( uaStatResults )
		}
    
    	if(uaGlobalWarningFound() == TRUE)
    	{
    		#if any warning encountered in the above try catch block
    	}
    
    	#######################################################################################
    	#Function to call at the end to close out logfiles. This function should not be called
    	#from any other sub function. This is only for uber function. Sub function has different
    	#function - uaFunctionWrapUp()
    	#######################################################################################
    	uaGlobalFunctionWrapUp()

    	####################################################################################
    	#Preparing the return structure
    	####################################################################################
    	
    	#initialize the list and auto fill from index 1 thru 6 location
    	uaReturnListtoUI <-uaRetInitialize()
    
    	#The 7th return value tells us now many tables we are going to display in the output
    	uaReturnListtoUI <- c(uaReturnListtoUI, nooftables = list(nooftablestodis))
    
    	## Should the uamat and data table be added here to the uaReturnListtoUI from uaresults
    	# uaReturnListtoUI <- c( uaReturnListtoUI, statOuputTables = uaresults)
        uaallstatresults <- list() #anil      
		#Sanjay, the code here can be made more efficient. 	  
		for (i in 1:nooftablestodis)
		{
   	      #Anil uaReturnListtoUI <- c( uaReturnListtoUI, list(uaStatResults[[i]]))
		  uaallstatresults <- c( uaallstatresults, list(uaStatResults[[i]])) #Anil
		}
		uaReturnListtoUI <- c( uaReturnListtoUI, tables = list(uaallstatresults))#Anil
		#cat("\n Printing uber function return result structure \n")
		# print(uaReturnListtoUI)
      
    	invisible(uaReturnListtoUI)
	}


bsky.one.sm.t.test.old<- function(vars, mu=0,conf.level=0.95,datasetname, missing=0)
{
		# Must be called within uber or any sub function as the very fist executable statement/function call
		# This is to gather the call parameters passed. This is needed within
		# warning and error handler to log the calling details in logfile
		BSkyFunctionInit()

		#This is just to set the context to the current dataset name
		BSkySetCurrentDatasetName(datasetname, setDatasetIndex ="y")
    	
		# The following is to set some application level messages(not R's own native meesage) 
		# in case there is error or warning gets generated
    	BSkyErrMsg =paste("Error in bsky.one.sm.t.test"," Dataset Name:", datasetname, "Variables:",paste (vars,collapse=","))
		BSkyWarnMsg =paste("Warning in bsky.one.sm.t.test"," Dataset Name:", datasetname, "Variables:",paste (vars,collapse=","))
		BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
    	tryCatch(
    		{
    			withCallingHandlers(
    			{
					# Calculate #of iterations needed to perform for each dataset segments based on the split condition
					# If there is no split condition then the it will only loop through once i.e. compute analysis only
					# once for the entire dataset
      				uaSplitIterationCount = BSkyComputeSplitdataset(vars, datasetname)
					# cat("In bsky.one.sm.t.test: Split iteration Number (if 0 means no split): ", uaSplitIterationCount )
              			 # cat("\nNo of DS in LST before for loop \n")
						 # print(length(uadatasets$lst))
      				# if there is no split, usSplitIterationCount = 1, otherwise usSplitIterationCount = cart product count
      				for (uaSplitIterationCounter in 1:uaSplitIterationCount)
					{
					     # cat("\nNo of DS in LST Before slicing inside 'for' loop \n")
						 # print(length(uadatasets$lst))
						 
						uaDatasetSliceIndex = BSkyGetNextDatasetSliceIndex(vars, datasetname)
        					
        				uaGlobalDataSliceIndexToWorkOn = uaDatasetSliceIndex$datasetSliceIndex
        				
        				# cat("\nNo of DS in LST After slicing, inside for loop \n")
						# print(length(uadatasets$lst))
						uaVariableColumnIndexOnDataSlice = uaDatasetSliceIndex$varIndex
						#print(uaVariableColumnIndexOnDataSlice)
							#cat("\nCurrent Split:\n")
							#print(paste(BSkyComputeCurrentVarNamesAndFactorValues(datasetname), collapse = ","))
							#cat("\n")                  
							#print(uadatasets$lst[[uaGlobalDataSliceIndexToWorkOn]])
							#cat("\n")
						#if( uaIsDatasetSplit(datasetname) == TRUE)
						#{ 
						# 	cat("Current factor names and values : \n", BSkyComputeCurrentVarNamesAndFactorValues(splitIterationCounter, datasetname), "\n\n")
						#}
    		          
						# Construct Application level message as one single string - so that 
						# if there is an error or warnign generated, the error and warning 
						# handler can print this msg string in the log
						# Constructing a single string is tricky becuse of the variable of vector type
						# Those vector variable need to be flattened out first before pasting
						# with our string  e.g. paste(vars, collapse = " ")
                  
						##if(uaDatasetSliceIndex$globalSplitIterationCounter > 1)  
						if (uaSplitIterationCount > 1)##old
        				{
							# BSkyStoreApplicationWarnErrMsg() can be used anytime to store Application level messages
							# so that it can be logged in the return structure for BSky application to display on the 
							# Output window UI. Here the App level message is set for every split iteration so that 
							# messages can be more specific to pint point what segment of the dataset has encounterd 
							# error or warning
							# BSkyErrMsg = paste(BSkyErrMsg, "Current Factors, if there is Split :",paste(BSkyComputeCurrentVarNamesAndFactorValues(uaSplitIterationCounter, datasetname), collapse = ","),sep=" * ")
							# BSkyWarnMsg = paste(BSkyWarnMsg, "Current Factors, if there is Split :",paste(BSkyComputeCurrentVarNamesAndFactorValues(uaSplitIterationCounter, datasetname), collapse = ","),sep=" * ")

							BSkyErrMsg = paste(BSkyErrMsg, "Current Factors, if there is Split :",paste(BSkyComputeCurrentVarNamesAndFactorValues( datasetname), collapse = ","),sep=" * ")
							BSkyWarnMsg = paste(BSkyWarnMsg, "Current Factors, if there is Split :",paste(BSkyComputeCurrentVarNamesAndFactorValues( datasetname), collapse = ","),sep=" * ")
							BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
							# print(BSkyWarnMsg)
						}
                  
						#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
						# Do Stat stuff that this function is supposed to do	
						# e.g. calling the uaonesample() to actually do the work for
						# one sample t test analysis for each segment of the dataset
						# based on the split setting
						#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
                	
						# The call to the actual Stat Analytic function
						#cat("\n uaVariableColumnIndexOnDataSlice = ", uaVariableColumnIndexOnDataSlice,"\tuaGlobalDataSliceIndexToWorkOn=",uaGlobalDataSliceIndexToWorkOn,"\n")
						uaonesample(uaVariableColumnIndexOnDataSlice, mu,conf.level,uaGlobalDataSliceIndexToWorkOn, missing)
						
						# uaonesample(uavarindex, mu,conf.level,index, missing)
						# If the anlytical subfunction i.e. uaonesample() does not use global variable uadatasets$retstructure
						# to stack up the tables generated from the analytical stat computation, then the subfunction can return
						# a list structure of all the tables generated in this split iteration (as shown below)
						
						#OutputResultTablesList = uaonesample(uaVariableColumnIndexOnDataSlice, mu,conf.level,uaGlobalDataSliceIndexToWorkOn, missing)
						
						# Incrementally build the return structure for each iteration (i.e. based on dataset split)
						# with the tables generated from the statistical analysis performed in the above 
						# function i.e. uaonesample(). The following function will store the tables in a global struture
						# and return it to the application UI at the end of this top level function i.e. bsky.one.sm.t.test()
						warning("One Sample Top Level :: Warning raised by Anil, for testing. ");
						#BSkyBuildReturnTableStructure(vars, datasetname, uaSplitIterationCounter, OutputDataTableListIfPassed=OutputResultTablesList)
						##old BSkyBuildReturnTableStructure(vars, datasetname, uaSplitIterationCounter, OutputDataTableListIfPassed=NA)
						BSkyBuildReturnTableStructure(vars, datasetname, OutputDataTableListIfPassed=NA)
						
					} # End of the split iteration loop
    			},
    			warning = UAwarnHandlerFn
    			) # end of withCallingHandlers for catching warnings and continuing execution
    		},
    		error = UAerrHandlerFn,
    		silent =TRUE
    	)
    
    	##########################################################################################################################
    	# Do other stuff whatever necessary coming out of the try catch block before returning from this BSky Analytical function#
    	##########################################################################################################################
    
    	if(BSkyGlobalErrorFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# Error is already handled and logged
			# Whereever the error occured execution cannot continue
			# control will be passed to the error handler function
			# and then the control will come out ogf the Try-Catch block 
			
			# cat("error caught in bsky.one.sm.t.test or anywhere in any sub function calls\n")
    	}
		
		if(BSkyGlobalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("warning caught in bsky.one.sm.t.test or anywhere in any sub function calls\n")
    	}
   	
    	#######################################################################################
    	#Function to call at the end to close out logfiles. This function should not be called
    	#from any other sub function. This is only for uber function. Sub function has different
    	#function - BSkyFunctionWrapUp()
    	#######################################################################################
    	BSkyFunctionWrapUp()

    	#invisible( BSkyReturnStructure(bskyAdditionalTableList = NA) ) ### ORIGINAL CODE
		
		      ########## Create a a list of 2-3 tables and pass them to test in UI to see all types of tables ######### ANIL
			  df <- data.frame(A=c(3,6,4), B=c("Aa","Bb","Cc"))
			  mdat <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol=3)
			  Amat = matrix( c(2, 4, 3, 1, 5, 7), nrow=2, ncol=3, byrow = TRUE)
			  dimnames(Amat) = list( c("row1", "row2"),c("col1", "col2", "col3"))
		invisible( BSkyReturnStructure(bskyAdditionalTableList = list(c("BSky", "Analytics"), df, mdat, Amat) ) ) ## TEST CODE
	}

	
BSkyOneSmTTest.old<- function(varNamesOrVarGlobalIndices, mu=0,conf.level=0.95,datasetNameOrDatasetGlobalIndex, missing=0, bSkyHandleSplit = TRUE)
{
		BSkyFunctionInit()
		
		
		
	#cat("\nBSkyOneSmTTest PROC: ")
	#print(uadatasets.sk$rproc)
	#cat("\nCLASS ")
	#print( class(uadatasets.sk$rproc) )
	#cat("\n varNamesOrVarGlobalIndices :", varNamesOrVarGlobalIndices, "\n")

#### START UASUMMARY[[7]] FIX #### 03/04 Jun 2017
### Following is a fix for uasummary[[7]] which contains the original top level function call. But somehow it was not taking more than 5 variables
### for Multi-var T.Tests (both independent and one sample), so we hade to do some string manipulation to get rights string there.
### UASUMMARY[[7]] is used for templated dialogs. The command is read from here and is used to feed values to the invisible dialog(both Multi.var)
### since uasummary was not containing more than 5 vars, the output only showed 5 var. After following fix I have tested adding 173 vars from Daniel's
### dataset and all 173 got displayed in output for (Multi var t.tests)
	
pos = regexpr('varNamesOrVarGlobalIndices', uadatasets.sk$rproc)

newstr = substr(uadatasets.sk$rproc, 1,pos[1]+26)

y = paste("c('",paste(varNamesOrVarGlobalIndices, sep=',',collapse="','"), "')", sep='')

finalstr = paste(newstr, y, ')', sep='')
	
uadatasets.sk$rproc = finalstr
#### END UASUMMARY[[7]] FIX #### 
	
	#cat("\nBSkyOneSmTTest Reassigned PROC: ")
	#print(uadatasets.sk$rproc)			
		
		
		
		#print("call stack:")
		#print(uadatasets.sk$callstack )
		#print("call stack index:")
		#print(uadatasets.sk$callstackindex)
		#print("errors:")
    #print(uadatasets.sk$totalerrors )
	#print("warnings")
    #print(uadatasets.sk$totalwarnings )		
	#print("now start")
		#cat('\n datasetname: ',datasetNameOrDatasetGlobalIndex)
		BSkySetCurrentDatasetName(datasetNameOrDatasetGlobalIndex, setDatasetIndex ="y")
		bSkyDatasetname = BSkyGetDatasetName(datasetNameOrDatasetGlobalIndex)
		#cat('\n Datasetname: ',bSkyDatasetname)
		bSkyVarnames = BSkyGetVarNames(varNamesOrVarGlobalIndices, datasetNameOrDatasetGlobalIndex)
		#cat("\nGetVarnames=\t")
		#print(bSkyVarnames)
    	BSkyErrMsg =paste("Error in One Sample T Test"," Dataset Name:", bSkyDatasetname, "Variables:",paste (bSkyVarnames,collapse=","))
		BSkyWarnMsg =paste("Warning in One Sample T Test"," Dataset Name:", bSkyDatasetname, "Variables:",paste (bSkyVarnames,collapse=","))
		BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
    	tryCatch(
    		{
    			withCallingHandlers(
    			{
				#cat('\n1BSky1sm: ',uadatasets.sk$currentDatasetname)
					if(bSkyHandleSplit == TRUE)
					{
						bSkySplitIterationCount = BSkyComputeSplitdataset(bSkyVarnames, bSkyDatasetname)
							#cat('\n2BSky1sm: ',uadatasets.sk$currentDatasetname)
						for (bSkySplitIterationCounter in 1:bSkySplitIterationCount)
						{
							bSkyDatasetSliceIndex = BSkyGetNextDatasetSliceIndex(bSkyVarnames, bSkyDatasetname)
							#cat('\n3BSky1sm: ',uadatasets.sk$currentDatasetname)
							bSkyGlobalDataSliceIndexToWorkOn = bSkyDatasetSliceIndex$datasetSliceIndex
							#cat('\n4BSky1sm: ',uadatasets.sk$currentDatasetname)
							bSkyVariableColumnIndicesOnDataSlice = bSkyDatasetSliceIndex$varIndex
					  #cat('\n5BSky1sm: ',uadatasets.sk$currentDatasetname)
							if (bSkySplitIterationCount > 1)
							{
							#cat('\n5.1BSky1sm: ',uadatasets.sk$currentDatasetname)
								BSkyErrMsgwithSplit = paste(BSkyErrMsg, "Current Factors, if there is Split :",paste(BSkyComputeCurrentVarNamesAndFactorValues(bSkyDatasetname), collapse = ","),sep=" * ")
								BSkyWarnMsgWithSplit = paste(BSkyWarnMsg, "Current Factors, if there is Split :",paste(BSkyComputeCurrentVarNamesAndFactorValues(bSkyDatasetname), collapse = ","),sep=" * ")
								BSkyStoreApplicationWarnErrMsg(BSkyWarnMsgWithSplit, BSkyErrMsgwithSplit)
							#cat('\n5.2BSky1sm: ',uadatasets.sk$currentDatasetname)	
							}
							
							#OutputResultTablesList = uaonesample(uaVariableColumnIndexOnDataSlice, mu,conf.level,uaGlobalDataSliceIndexToWorkOn, missing)
							#cat('\n6BSky1sm: ',uadatasets.sk$currentDatasetname)
							uaonesample(bSkyVariableColumnIndicesOnDataSlice, mu,conf.level, bSkyGlobalDataSliceIndexToWorkOn, missing)
							#cat('\n7BSky1sm: ',uadatasets.sk$currentDatasetname)
							BSkyBuildReturnTableStructure(bSkyVarnames, bSkyDatasetname, OutputDataTableListIfPassed=NA)
							#cat('\n8BSky1sm: ',uadatasets.sk$currentDatasetname)
						
						} # End of the split iteration loop
					}
					else
					{
							bSkyGlobalDatasetIndexToWorkOn = BSkyGetDatasetGlobalIndex(datasetNameOrDatasetGlobalIndex)

							bSkyVariableColumnIndicesOnDataset = BSkyGetVarGlobalIndices(varNamesOrVarGlobalIndices, datasetNameOrDatasetGlobalIndex)

							#OutputResultTablesList = uaonesample(uaVariableColumnIndexOnDataSlice, mu,conf.level,uaGlobalDataSliceIndexToWorkOn, missing)
							uaonesample(bSkyVariableColumnIndicesOnDataset, mu,conf.level,bSkyGlobalDatasetIndexToWorkOn, missing)
							#cat("\nVarnames=\t")
							#print(bSkyVarnames)
							BSkyBuildReturnTableStructure(bSkyVarnames, bSkyDatasetname, OutputDataTableListIfPassed=NA)
							
					}
    			},
    			warning = UAwarnHandlerFn
    			) # end of withCallingHandlers for catching warnings and continuing execution
    		},
    		error = UAerrHandlerFn,
    		silent =TRUE
    	)
    
    	##########################################################################################################################
    	# Do other stuff whatever necessary coming out of the try catch block before returning from this BSky Analytical function#
    	##########################################################################################################################
    
    	if(BSkyGlobalErrorFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# Error is already handled and logged
			# Whereever the error occured execution cannot continue
			# control will be passed to the error handler function
			# and then the control will come out ogf the Try-Catch block 
			BSkyBuildReturnTableStructure(bSkyVarnames, bSkyDatasetname, OutputDataTableListIfPassed=NA)
			# cat("error caught in bsky.one.sm.t.test or anywhere in any sub function calls\n")
    	}
		
		if(BSkyGlobalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued

			# cat("warning caught in bsky.one.sm.t.test or anywhere in any sub function calls\n")
    	}
   	
	#cat('\n Current Dataset: ',uadatasets.sk$currentDatasetname)
    	#######################################################################################
    	#Function to call at the end to close out logfiles. This function should not be called
    	#from any other sub function. This is only for uber function. Sub function has different
    	#function - BSkyFunctionWrapUp()
    	#######################################################################################
    	BSkyFunctionWrapUp()
		      ########## Create a a list of 2-3 tables and pass them to test in UI to see all types of tables ######### ANIL
			   #df <- data.frame(A=c(3,6,4), B=c("Aa","Bb","Cc"))
			   #mdat <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol=3)
			   #Amat = matrix( c(2, 4, 3, 1, 5, 7), nrow=2, ncol=3, byrow = TRUE)
			   #dimnames(Amat) = list( c("row1", "row2"),c("col1", "col2", "col3"))
			#invisible( BSkyReturnStructure(bskyAdditionalTableList = list(c("BSky", "Analytics"), df, mdat, Amat) ) ) ## TEST CODE
		
    	invisible( BSkyReturnStructure(bskyAdditionalTableList = NA) ) ### ORIGINAL CODE
	}
	
#30Jul2018	
# Changes to BSkyOneSmTTest,  uaonesample, uaonesamttest to support alternative

#08Oct2021
BSkyOneSmTTest <-function (data = NULL, varNamesOrVarGlobalIndices = NULL, mu = 0, conf.level = 0.95, alternative = 'two.sided',
    datasetNameOrDatasetGlobalIndex = NULL, missing = 0, bSkyHandleSplit = TRUE, 
	cohens_d=FALSE, cohensd_correction=FALSE, hedges_g =FALSE, hedgesg_correction=FALSE, glass_d=FALSE, glassd_correction=FALSE) 
{
    BSkyFunctionInit()
	#print(match.call())
	
	datasetname_passed = c("")
	
	stripped_data = data
	
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
		
		datasetNameOrDatasetGlobalIndex = dataset_name_str
	}
	else if(length(datasetNameOrDatasetGlobalIndex) == 0)
	{
		return(invisible(NULL))
	}
	
	
	if(!is.null(data)) # No group by for one sample t-test if(length(group) == 0 && !is.null(data)) - but filer out grouping variable if passed 
	{
		group_by_col_names = names(as.data.frame(attr(data, "groups")))
		
		if(length(group_by_col_names) > 0) 
		{
			group_by_col_names = group_by_col_names[1:(length(group_by_col_names) - 1)] # dropping the ".rows" col names from dplyr tibble table 
			
			#selected_group_by_col_names_list = paste(paste(group_by_col_names, "=", dataset_name_str, 
			#											"$", group_by_col_names, sep = ""), sep = "", collapse = ",")
			
			#group = eval(parse(text = paste("list(", selected_group_by_col_names_list, ")", sep = "")))
			group = group_by_col_names
			
			#print(selected_group_by_col_names_list)								
			#print(group)
			
			#stripped_data = data[c(length(group_by_col_names): ncol(data))] # group_by_col_names includes columns and additional .rows column
			stripped_data = data[, !(names(data) %in% c(group_by_col_names))] # group_by_col_names includes columns and additional .rows column
		}
	}
	
	
	if(length(varNamesOrVarGlobalIndices) == 0 && !is.null(data))
	{
		varNamesOrVarGlobalIndices = dimnames(stripped_data)[[2]]
		
		#selected_col_names_list = paste(paste(col_names, "=", dataset_name_str, 
		#												"$", col_names, sep = ""), sep = "", collapse = ",")
			
		#datasetColumnObjects = eval(parse(text = paste("list(", 
		#												selected_col_names_list, ")", sep = "")))
	}
	
	
	if(datasetname_passed == ".")
	{
		#temp_pipe_dataset_name = tempfile(pattern = "pipe_data_", tmpdir = "")
		#temp_pipe_dataset_name = substr(temp_pipe_dataset_name, 2, nchar(temp_pipe_dataset_name))
		temp_pipe_dataset_name = "bsky_piped_temp_dataset"
		eval(parse(text= paste(temp_pipe_dataset_name, "<<- data"))) #, envir = globalenv())
		BSkyLoadRefresh(temp_pipe_dataset_name)
		 
		datasetname = temp_pipe_dataset_name

		BSkySetCurrentDatasetName(datasetname, setDatasetIndex ="y")
		bSkyDatasetname = BSkyGetDatasetName(datasetname)
		datasetNameOrDatasetGlobalIndex = datasetname
		#bSkyHandleSplit = FALSE
	
			replace_uasummary_7 =	paste("BSkyOneSmTTest(",
									"alternative=c(", alternative, "),", 
									"bSkyHandleSplit=c(", bSkyHandleSplit, "),", 
									"conf.level=c(", conf.level, "),", 
									"mu=c(", mu, "),", 
									"datasetNameOrDatasetGlobalIndex=c('", datasetNameOrDatasetGlobalIndex, "'),",
									"cohens_d=c(",cohens_d, "),", 
									"cohensd_correction=c(",cohensd_correction, "),", 									
									"glass_d=c(", glass_d, "),", 
									"glassd_correction=c(", glassd_correction, "),", 
									"hedges_g=c(",hedges_g, "),", 
									"hedgesg_correction=c(", hedgesg_correction, "),", 
									"missing=c(",missing, "),", 
									"varNamesOrVarGlobalIndices=c(",paste(varNamesOrVarGlobalIndices,collapse=","),"))", sep="")
							
		#print(replace_uasummary_7)
		#cat("\n",replace_uasummary_7, "\n")
		#return 
	}
	else
	{
		#This is just to set the context to the current dataset name
		#This also copies the dataset to uadatasets$lst for backward compatibility
		BSkySetCurrentDatasetName(datasetNameOrDatasetGlobalIndex, setDatasetIndex ="y")
		bSkyDatasetname = BSkyGetDatasetName(datasetNameOrDatasetGlobalIndex)
	}
	
	
    pos = regexpr("varNamesOrVarGlobalIndices", uadatasets.sk$rproc)
    newstr = substr(uadatasets.sk$rproc, 1, pos[1] + 26)
    y = paste("c('", paste(varNamesOrVarGlobalIndices, sep = ",", 
        collapse = "','"), "')", sep = "")
    finalstr = paste(newstr, y, ")", sep = "")
    uadatasets.sk$rproc = finalstr
    
	#BSkySetCurrentDatasetName(datasetNameOrDatasetGlobalIndex, setDatasetIndex = "y")
    #bSkyDatasetname = BSkyGetDatasetName(datasetNameOrDatasetGlobalIndex)
    
	bSkyVarnames = BSkyGetVarNames(varNamesOrVarGlobalIndices, 
        datasetNameOrDatasetGlobalIndex)
    BSkyErrMsg = paste("Error in One Sample T Test", " Dataset Name:", 
        bSkyDatasetname, "Variables:", paste(bSkyVarnames, collapse = ","))
    BSkyWarnMsg = paste("Warning in One Sample T Test", " Dataset Name:", 
        bSkyDatasetname, "Variables:", paste(bSkyVarnames, collapse = ","))
    BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
    tryCatch({
        withCallingHandlers({
            if (bSkyHandleSplit == TRUE) {
                bSkySplitIterationCount = BSkyComputeSplitdataset(bSkyVarnames, 
                  bSkyDatasetname)
                for (bSkySplitIterationCounter in 1:bSkySplitIterationCount) {
                  bSkyDatasetSliceIndex = BSkyGetNextDatasetSliceIndex(bSkyVarnames, 
                    bSkyDatasetname)
                  bSkyGlobalDataSliceIndexToWorkOn = bSkyDatasetSliceIndex$datasetSliceIndex
                  bSkyVariableColumnIndicesOnDataSlice = bSkyDatasetSliceIndex$varIndex
                  if (bSkySplitIterationCount > 1) {
                    BSkyErrMsgwithSplit = paste(BSkyErrMsg, "Current Factors, if there is Split :", 
                      paste(BSkyComputeCurrentVarNamesAndFactorValues(bSkyDatasetname), 
                        collapse = ","), sep = " * ")
                    BSkyWarnMsgWithSplit = paste(BSkyWarnMsg, 
                      "Current Factors, if there is Split :", 
                      paste(BSkyComputeCurrentVarNamesAndFactorValues(bSkyDatasetname), 
                        collapse = ","), sep = " * ")
                    BSkyStoreApplicationWarnErrMsg(BSkyWarnMsgWithSplit, 
                      BSkyErrMsgwithSplit)
                  }
                  uaonesample(bSkyVariableColumnIndicesOnDataSlice, 
                    mu, conf.level, alternative ,cohens_d=cohens_d, cohensd_correction=cohensd_correction,hedges_g =hedges_g,hedgesg_correction=hedgesg_correction,glass_d=glass_d, glassd_correction=glassd_correction,bSkyGlobalDataSliceIndexToWorkOn, 
                    missing)
                  BSkyBuildReturnTableStructure(bSkyVarnames, 
                    bSkyDatasetname, OutputDataTableListIfPassed = NA)
                }
            }
            else {
                bSkyGlobalDatasetIndexToWorkOn = BSkyGetDatasetGlobalIndex(datasetNameOrDatasetGlobalIndex)
                bSkyVariableColumnIndicesOnDataset = BSkyGetVarGlobalIndices(varNamesOrVarGlobalIndices, 
                  datasetNameOrDatasetGlobalIndex)
                uaonesample(bSkyVariableColumnIndicesOnDataset, 
                  mu, conf.level, alternative,cohens_d=cohens_d, cohensd_correction=cohensd_correction,hedges_g =hedges_g,hedgesg_correction=hedgesg_correction,glass_d=glass_d, glassd_correction=glassd_correction,bSkyGlobalDatasetIndexToWorkOn, 
                  missing)
                BSkyBuildReturnTableStructure(bSkyVarnames, bSkyDatasetname, 
                  OutputDataTableListIfPassed = NA)
            }
        }, warning = UAwarnHandlerFn)
    }, error = UAerrHandlerFn, silent = TRUE)
    if (BSkyGlobalErrorFound() == TRUE) {
        BSkyBuildReturnTableStructure(bSkyVarnames, bSkyDatasetname, 
            OutputDataTableListIfPassed = NA)
    }
    if (BSkyGlobalWarningFound() == TRUE) {
    }
    BSkyFunctionWrapUp()
	
	bsky_return_structure = BSkyReturnStructure2()
	
	if(datasetname_passed == ".")
	{
		bsky_return_structure$uasummary[[7]] = replace_uasummary_7
	}
	
	#print("Returning from one sample")
    invisible(bsky_return_structure)
}

