 # # # # # # # # # # bskyReturnObj = BSkyTwoWayFrequency("Dataset1",  Dataset1$agecat,  Dataset1$gender)

BSkyTwoWayFrequency <- function(datasetNameString,  datasetColumnObj1, datasetColumnObj2, cellPercent = FALSE, rowPercent = FALSE, colPercent = FALSE, bSkyHandleSplit = TRUE)
{
		BSkyFunctionInit()
		
		#This is just to set the context to the current dataset name
		BSkySetCurrentDatasetName(datasetNameString)
	
    	tryCatch(
    		{
    			withCallingHandlers(
    			{	 
					# 2-Way Frequency Table
					mytable <- table(datasetColumnObj1, datasetColumnObj2) # here dataset column1 will be rows, dataset column2 will be columns 		
					#BSkyBuildReturnTableStructure(OutputDataTableListIfPassed=list(mytable))

					#tempTable = margin.table(mytable, 1) # datasetColumnObj1 frequencies 
					#BSkyBuildReturnTableStructure(OutputDataTableListIfPassed=list(tempTable))
					
					#tempTable = margin.table(mytable, 2) # datasetColumnObj2 frequencies 
					#BSkyBuildReturnTableStructure(OutputDataTableListIfPassed=list(tempTable))
					
					if(cellPercent )
					{
						tempTable = prop.table(mytable) # cell percentages
						##create
						BSkyBuildReturnTableStructure(OutputDataTableListIfPassed=list(tempTable))
					}
					
					if(rowPercent)
					{
						tempTable = prop.table(mytable, 1) # row percentages
						BSkyBuildReturnTableStructure(OutputDataTableListIfPassed=list(tempTable))
					}
					
					if(colPercent )
					{
						tempTable = prop.table(mytable, 2) # column percentages 
						BSkyBuildReturnTableStructure(OutputDataTableListIfPassed=list(tempTable))
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

    	invisible( BSkyReturnStructure() ) ###say one more added here
	}
	
