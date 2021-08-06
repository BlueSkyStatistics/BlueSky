TestErrors <- function()
{
	
	# Must be called within uber or any sub function as the very fist executable statement/function call
	# This is to gather the call parameters passed. This is needed within
	# warning and error handler to log the calling details in logfile
	BSkyFunctionInit()
	#This is just to set the context to the current dataset name
	#This also copies the dataset to uadatasets$lst for backward compatibility
	
	#BSkySetCurrentDatasetName(datasetname, setDatasetIndex ="y")
	#bSkyDatasetname = BSkyGetDatasetName(datasetname)
	
	
	uadatasets$retstructure <-list()

    #used to track the number of tables returned
    #nooftablestodis =0

	# As retstructure is re-initilaized after every split, uaStatResults stores the retstructures across splits
    # uaStatResults is the added to the final list of results returned. All items in uaStatResults are added 
    # at position 8 onwards of the final return structure        		
    #uaStatResults =list()
	
	
	BSkyErrMsg =paste("Error in testerrors"," Dataset Name:")#, datasetname)
	BSkyWarnMsg =paste("Warning in testerrors"," Dataset Name:")#, datasetname)
		
    
	BSkyStoreApplicationWarnErrMsg( BSkyWarnMsg,BSkyErrMsg)	

	
		tryCatch(
				{
    				withCallingHandlers(
    				{
					# Calculate #of iterations needed to perform for each dataset segments based on the split condition
					# If there is no split condition then the it will only loop through once i.e. compute analysis only
					# once for the entire dataset
					
					
					cat("\nBefore Calling Sub\n")
					#Call Sub function that will throw some error
					res <- TestErrorsSub()
					cat("\nAfter Calling Sub\n")

					#For error before DF
					#a = b+3
					
					cat("\nBefore Creating and adding DF\n")
					#For returning a dataframe
					df <- data.frame(A=c(21,24,32), B=c(46,57,68), C=c(68,87,89))
					BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = df)
					cat("\nAfter Creating and Adding DF\n")
					
					cat("\nNow causing error\n")
					#For error After DF
					a = b+3

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
    cat("\nOut of Try-Catch\n")
    if( BSkyGlobalErrorFound() == TRUE)
    {
    	# if anything needs to be checked or print etc
			# Error is already handled and logged
			# Whereever the error occured execution cannot continue
			# control will be passed to the error handler function
			# and then the control will come out ogf the Try-Catch block 
			
			# cat("error caught in bsky.one.sm.t.test or anywhere in any sub function calls\n")
		#BSkyBuildReturnTableStructure(varNamesOrVarGlobalIndices, datasetname,  OutputDataTableListIfPassed=NA)
    }
    
    if(BSkyGlobalWarningFound() == TRUE)
    {
    	#if any warning encountered in the above try catch block
    }
    
    #######################################################################################
    #Function to call at the end to close out logfiles. 
    #######################################################################################
	cat("\nBefore Wrapup from  main function\n")
	BSkyFunctionWrapUp()
	cat("\nWrapup from  main function\n")
	# Function BSKYReturnStructure creates the final retun structure i.e. splits or not, error or not, the number of errors
	# the number of warnings, the log, the summary of the function, the number of tables to display
	# Basically the first 7 elements of the list
	invisible( BSkyReturnStructure())
}

TestErrorsSub <- function()
{
	
	# Must be called within uber or any sub function as the very fist executable statement/function call
	# This is to gather the call parameters passed. This is needed within
	# warning and error handler to log the calling details in logfile
	BSkyFunctionInit()
	#This is just to set the context to the current dataset name
	#This also copies the dataset to uadatasets$lst for backward compatibility
	
	#BSkySetCurrentDatasetName(datasetname, setDatasetIndex ="y")
	#bSkyDatasetname = BSkyGetDatasetName(datasetname)
	
	
	#uadatasets$retstructure <-list()

    #used to track the number of tables returned
    #nooftablestodis =0

	# As retstructure is re-initilaized after every split, uaStatResults stores the retstructures across splits
    # uaStatResults is the added to the final list of results returned. All items in uaStatResults are added 
    # at position 8 onwards of the final return structure        		
    #uaStatResults =list()
	
	
	BSkyErrMsg =paste("Error in TestErrorsSub"," Dataset Name:")#, datasetname)
	BSkyWarnMsg =paste("Warning in TestErrorsSub"," Dataset Name:")#, datasetname)
		
    
	BSkyStoreApplicationWarnErrMsg( BSkyWarnMsg,BSkyErrMsg)	

	
		tryCatch(
				{
    				withCallingHandlers(
    				{
					# Calculate #of iterations needed to perform for each dataset segments based on the split condition
					# If there is no split condition then the it will only loop through once i.e. compute analysis only
					# once for the entire dataset
					
					#For error Before
					#a = b+3
					
					#For returning a dataframe
					df <- data.frame(A=c(1,2,3), B=c(4,5,6), C=c(6,7,89))
					BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = df,  singleTableOutputHeader="Table Name")

					cat("\nAdded DF to return structure.\nCausing error in Sub function\n")
					#For error Afterwards
					a = b+3

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
		#BSkyBuildReturnTableStructure(varNamesOrVarGlobalIndices, datasetname,  OutputDataTableListIfPassed=NA)
    }
    
    if(BSkyGlobalWarningFound() == TRUE)
    {
    	#if any warning encountered in the above try catch block
    }
    
    #######################################################################################
    #Function to call at the end to close out logfiles. 
    #######################################################################################
	BSkyFunctionWrapUp()
	cat("\nWrapup from  sub function\n")
	# Function BSKYReturnStructure creates the final retun structure i.e. splits or not, error or not, the number of errors
	# the number of warnings, the log, the summary of the function, the number of tables to display
	# Basically the first 7 elements of the list
	#invisible( BSkyReturnStructure()) ###CALL THIS ONLY FROM TOP LEVEL FUNCTION AND NEVER FROM SUB-FUNCTION.
}