## storeSurvey=data.frame(store = c('s1', 's2','s3','s2','s1','s2','s3','s1'), clerkGender =c('m','m','f','f','f','m','f','f'), newEmployee=c('y','n','y','y','n','n','n','y'))
## bskyReturnObj = bsky.example.2WayFrequency.no.split("storeSurvey",  storeSurvey$store,  storeSurvey$clerkGender)
### bskyReturnObj = bsky.example.2WayFrequency.no.split("storeSurvey",  vars = c(store,  clerkGender) )
## BSkyFormat(bskyReturnObj)



 # # # # # # # # # # bskyReturnObj = bsky.example.2WayFrequency.no.split1("Dataset1",  Dataset1$agecat,  Dataset1$gender)

bsky.example.2WayFrequency.no.split1<- function(datasetNameString,  datasetColumnObj1, datasetColumnObj2, cellPercent = 'N', rowPercent = 'N', colPercent = 'N', bSkyHandleSplit = TRUE)
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
					BSkyBuildReturnTableStructure(OutputDataTableListIfPassed=list(mytable))

					tempTable = margin.table(mytable, 1) # datasetColumnObj1 frequencies 
					BSkyBuildReturnTableStructure(OutputDataTableListIfPassed=list(tempTable))
					
					tempTable = margin.table(mytable, 2) # datasetColumnObj2 frequencies 
					BSkyBuildReturnTableStructure(OutputDataTableListIfPassed=list(tempTable))
					
					if(cellPercent != 'N')
					{
						tempTable = prop.table(mytable) # cell percentages
						BSkyBuildReturnTableStructure(OutputDataTableListIfPassed=list(tempTable))
					}
					
					if(rowPercent != 'N')
					{
						tempTable = prop.table(mytable, 1) # row percentages
						BSkyBuildReturnTableStructure(OutputDataTableListIfPassed=list(tempTable))
					}
					
					if(colPercent != 'N')
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

    	invisible( BSkyReturnStructure() ) 
	}
	
	
	# bskyReturnObj = bsky.example.2WayFrequency.no.split2("Dataset1",  varsNameStrings=c('agecat', 'gender'), cellPercent = 'Y', rowPercent = 'Y', colPercent = 'Y')
bsky.example.2WayFrequency.no.split2<- function(datasetNameString,  varsNameStrings, cellPercent = 'N', rowPercent = 'N', colPercent = 'N', bSkyHandleSplit = TRUE)
{
		BSkyFunctionInit()
		
		#This is just to set the context to the current dataset name
		BSkySetCurrentDatasetName(datasetNameString)
	
    	tryCatch(
    		{
    			withCallingHandlers(
    			{	 
				    # cat("Hello\n")
					colObjects <- list()
					len <- length(varsNameStrings)
					if(len > 0)
					{
						for(i in 1:len)
						{
							colObjects[[i]] <- eval(parse(text=paste(datasetNameString,'$',varsNameStrings[i], sep='')))
						}
					}
					# 2-Way Frequency Table
					mytable <- table(colObjects[[1]], colObjects[[2]]) # here dataset column1 will be rows, dataset column2 will be columns 		
					BSkyBuildReturnTableStructure(OutputDataTableListIfPassed=list(mytable))

					tempTable = margin.table(mytable, 1) # datasetColumnObj1 frequencies 
					BSkyBuildReturnTableStructure(OutputDataTableListIfPassed=list(tempTable))
					
					tempTable = margin.table(mytable, 2) # datasetColumnObj2 frequencies 
					BSkyBuildReturnTableStructure(OutputDataTableListIfPassed=list(tempTable))
					
					if(cellPercent != 'N')
					{
						tempTable = prop.table(mytable) # cell percentages
						BSkyBuildReturnTableStructure(OutputDataTableListIfPassed=list(tempTable))
					}
					
					if(rowPercent != 'N')
					{
						tempTable = prop.table(mytable, 1) # row percentages
						BSkyBuildReturnTableStructure(OutputDataTableListIfPassed=list(tempTable))
					}
					
					if(colPercent != 'N')
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

    	invisible( BSkyReturnStructure() ) 
	}
		