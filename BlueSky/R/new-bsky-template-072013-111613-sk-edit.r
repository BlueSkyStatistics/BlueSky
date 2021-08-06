######## list of methods in this file; in sequence (few may not be ready and unused ) ###########
# BSkyBatchCommand
# BSkyIsInBatchCommandMode
# uaLogFunArgsDetails3
# uaLogFunArgsDetails2
# uaLogFunArgsDetailsErr
# uaLogFunArgsDetails
# uaLogFunArgsDetailsInExceptionHandler
# UAwarnHandlerFn
# UAerrHandlerFn
# uaGlobalErrorFound
# BSkyGlobalErrorFound
# uaGlobalWarningFound
# BSkyGlobalWarningFound
# uaErrorFound
# BSkyLocalErrorFound
# uaWarningFound
# BSkyLocalWarningFound
# uaErrorFlagsReset
# uaWarningFlagsReset
# BSkyLocalErrorFlagsReset
# BSkyLocalWarningFlagsReset
# uaStoreApplicationWarnErrMsg
# BSkyStoreApplicationWarnErrMsg
# uaGetStoredApplicationWarnErrMsg
# BSkyGetStoredApplicationWarnErrMsg
# uaGlobalInit
# BSkyFunctionInit
# BSkyBatchFunctionModeResetForUberFunction
# uaGlobalFunctionWrapUp
# BSkyFunctionWrapUp
# BSkyBuildReturnTableStructure
# BSkyReturnStructure
# uaFunctionInit
# uaFunctionWrapUp
# uaprocdesc
# uagetlevels
# uagetsplitcriteria
# uagetsplitinfo
# BSkyComputeSplitdataset.old 
# BSkyComputeSplitdataset
# BSkySetCurrentDatasetName.old
# BSkySetCurrentDatasetName
# BSkyGetDatasetName
# BSkyGetVarNames
# BSkyGetDatasetGlobalIndex
# BSkyGetVarGlobalIndices
# uaGetCurrentDatabaseName
# BSkyGetNextDatasetSliceIndex.old 
# BSkyGetNextDatasetSliceIndex
# BSkyComputeCurrentVarNamesAndFactorValues
# uaIsDatasetSplit
# uaRetInitialize
# uaGetUberFunctionName
# uaGetUberFunctionParam
# uaGetCurrentSplitLevel
# WriteLatestUberErrorInReturnTables
# uaTestSubfuncNoHandler
# uaTestSubfuncWithHandler
# uaTestSubSubfuncNoHandler
# uawritelog
# uastartlog
# ualogcommand
# uaretlog

#################################################################################################

BSkyBatchCommand <- function(begin = 0) #begin = 1 means the begining of the batch command block; 0 means end of batch command block
{
	if(begin == 1)
	{
		BSkyFunctionInit()
		uadatasets.sk$BSkyBatchCommandModeON = TRUE
	}
	else
	{
		BSkyFunctionWrapUp() #################### It must be called otherwise things go really bad ###################
		uadatasets.sk$BSkyBatchCommandModeON = FALSE
	}
}

BSkyIsInBatchCommandMode <- function()  ## to check if batch command is still ON
{
	#if(uadatasets.sk$uaTempUberFunctionName == "BSkyBatchCommand")
	if(uadatasets.sk$BSkyBatchCommandModeON == TRUE)
		return (TRUE)
	else
		return (FALSE)
}


uaLogFunArgsDetails3<-function(frameNum)
{
      fnCall<-sys.call()
    	fnName<-as.character(fnCall)[1]
    	#cat("sys Frame Num: 0", "FuncName :", fnName, "\n")
    	
      for(i in 1:frameNum)
      {
          fnCall<-sys.call(-i)
        	fnName<-as.character(fnCall)[1]
        	#cat("sys Frame Num:", i, "FuncName :", fnName, "\n")
      }
      
      return(list(FuntionCallMade = "", RuntimeParamValues = "", NearestTryCatchFnName = "") )
}

uaLogFunArgsDetails2<-function(frameNum)
{
	fnCall<-sys.call(-1)
	fnName<-as.character(fnCall)[1]
	cat("sys -1 :", fnName, "\n")
	
	fnCall<-sys.call(-2)
	fnName<-as.character(fnCall)[1]
	cat("sys -2 :", fnName, "\n")

	fnCall<-sys.call(-3)
	fnName<-as.character(fnCall)[1]
	cat("sys -3 :", fnName, "\n")

	fnCall<-sys.call(-4)
	fnName<-as.character(fnCall)[1]
	cat("sys -4 :", fnName, "\n")

	fnCall<-sys.call(-5)
	fnName<-as.character(fnCall)[1]
	cat("sys -5 :", fnName, "\n")
	
	fnCall<-sys.call(-6)
	fnName<-as.character(fnCall)[1]
	cat("sys -6 :", fnName, "\n")

	fnCall<-sys.call(-7)
	fnName<-as.character(fnCall)[1]
	cat("sys -7 :", fnName, "\n")

	fnCall<-sys.call(-8)
	fnName<-as.character(fnCall)[1]
	cat("sys -8 :", fnName, "\n")

	fnCall<-sys.call(-9)
	fnName<-as.character(fnCall)[1]
	cat("sys -9 :", fnName, "\n")

	fnCall<-sys.call(-10)
	fnName<-as.character(fnCall)[1]
	cat("sys -10 :", fnName, "\n")

	fnCall<-sys.call(-11)
	fnName<-as.character(fnCall)[1]
	cat("sys -11 :", fnName, "\n")

	fnCall<-sys.call(-12)
	fnName<-as.character(fnCall)[1]
	cat("sys -12 :", fnName, "\n")

	fnCall<-sys.call(-13)
	fnName<-as.character(fnCall)[1]
	cat("sys -13 :", fnName, "\n")

	fnCall<-sys.call(-14)
	fnName<-as.character(fnCall)[1]
	cat("sys -14 :", fnName, "\n")

	fnCall<-sys.call(-15)
	fnName<-as.character(fnCall)[1]
	cat("sys -15 :", fnName, "\n")

	x<- pmatch("tryCatch", fnName)
	y <- pmatch("TryCatch", fnName)

	if( !is.na(x) || !is.na(y) )
	{
		fnCall<-sys.call(-16)
		fnName<-as.character(fnCall)[1]
		cat("sys -16 :", fnName, "\n")

		fnCall<-sys.call(-17)
		fnName<-as.character(fnCall)[1]
		cat("sys -17 :", fnName, "\n")

		fnCall<-sys.call(-18)
		fnName<-as.character(fnCall)[1]
		cat("sys -18 :", fnName, "\n")

		fnCall<-sys.call(-19)
		fnName<-as.character(fnCall)[1]
		cat("sys -19 :", fnName, "\n")

		fnCall<-sys.call(-20)
		fnName<-as.character(fnCall)[1]
		cat("sys -20 :", fnName, "\n")

		fnCall<-sys.call(-21)
		fnName<-as.character(fnCall)[1]
		cat("sys -21 :", fnName, "\n")

		fnCall<-sys.call(-22)
		fnName<-as.character(fnCall)[1]
		cat("sys -22 :", fnName, "\n")
	}
}


uaLogFunArgsDetailsErr<-function(frameNum)
{
	
	fnCall<-sys.call()
	fnName<-as.character(fnCall)[1]
	cat("sys () :", fnName, "\n")

	fnCall<-sys.call(0)
	fnName<-as.character(fnCall)[1]
	cat("sys 0 :", fnName, "\n")

	fnCall<-sys.call(-1)
	fnName<-as.character(fnCall)[1]
	cat("sys -1 :", fnName, "\n")
	#print(fnCall)
	
	fnCall<-sys.call(-2)
	fnName<-as.character(fnCall)[1]
	cat("sys -2 :", fnName, "\n")

	fnCall<-sys.call(-3)
	fnName<-as.character(fnCall)[1]
	cat("sys -3 :", fnName, "\n")

	fnCall<-sys.call(-4)
	fnName<-as.character(fnCall)[1]
	cat("sys -4 :", fnName, "\n")

	fnCall<-sys.call(-5)
	fnName<-as.character(fnCall)[1]
	cat("sys -5 :", fnName, "\n")
	
	fnCall<-sys.call(-6)
	fnName<-as.character(fnCall)[1]
	cat("sys -6 :", fnName, "\n")

	fnCall<-sys.call(-7)
	fnName<-as.character(fnCall)[1]
	cat("sys -7 :", fnName, "\n")

	fnCall<-sys.call(-8)
	fnName<-as.character(fnCall)[1]
	cat("sys -8 :", fnName, "\n")

	
	#x<- pmatch("tryCatch", fnName)
	#y <- pmatch("TryCatch", fnName)
				

	#if( !is.na(x) || !is.na(y) )
	{

		fnCall<-sys.call(-9)
	fnName<-as.character(fnCall)[1]
	cat("sys -9 :", fnName, "\n")

	fnCall<-sys.call(-10)
	fnName<-as.character(fnCall)[1]
	cat("sys -10 :", fnName, "\n")

	fnCall<-sys.call(-11)
	fnName<-as.character(fnCall)[1]
	cat("sys -11 :", fnName, "\n")

	fnCall<-sys.call(-12)
	fnName<-as.character(fnCall)[1]
	cat("sys -12 :", fnName, "\n")

	fnCall<-sys.call(-13)
	fnName<-as.character(fnCall)[1]
	cat("sys -13 :", fnName, "\n")

	fnCall<-sys.call(-14)
	fnName<-as.character(fnCall)[1]
	cat("sys -14 :", fnName, "\n")

	fnCall<-sys.call(-15)
	fnName<-as.character(fnCall)[1]
	cat("sys -15 :", fnName, "\n")


		fnCall<-sys.call(-16)
		fnName<-as.character(fnCall)[1]
		cat("sys -16 :", fnName, "\n")

		fnCall<-sys.call(-17)
		fnName<-as.character(fnCall)[1]
		cat("sys -17 :", fnName, "\n")

		#fnCall<-sys.call(-18)
		#fnName<-as.character(fnCall)[1]
		#cat("sys -18 :", fnName, "\n")

		#fnCall<-sys.call(-19)
		#fnName<-as.character(fnCall)[1]
		#cat("sys -19 :", fnName, "\n")

		#fnCall<-sys.call(-20)
		#fnName<-as.character(fnCall)[1]
		#cat("sys -20 :", fnName, "\n")

		#fnCall<-sys.call(-21)
		#fnName<-as.character(fnCall)[1]
		#cat("sys -21 :", fnName, "\n")

		#fnCall<-sys.call(-22)
		#fnName<-as.character(fnCall)[1]
		#cat("sys -22 :", fnName, "\n")
	}
}

##############################################################################
# Orginal call logging function to log call details and call parameter details
# Fixed a bug for a "list" parameter with no name
##############################################################################
uaLogFunArgsDetails.old<-function()
{
    	#fnCall<-sys.call(-1)
    	fnCall<-sys.call(-2)
    	
    	fnName<-as.character(fnCall)[1]
    	
    	fnSignatureRunTime<-paste(fnName,"(", " ", sep="")
    	
    	
    	#print(fnCall)
    	#fnCallInputParamValues<-capture.output(print(ls.str(parent.frame(5)),give.head = FALSE))
    	#fnCallInputParamValues<-gsub('"', "", fnCallInputParamValues)
    
    	#fnCallInputParamObjs <- ls(parent.frame())
    	fnCallInputParamObjs <- ls(sys.frame(-2))
    	
    	#print( fnCallInputParamObjs )
    	
    	len <- length(fnCallInputParamObjs)
    	
    	for(i in 1:len)
    	{
    		#print(fnCallInputParamObjs[i])
    
    		#paramObjClass1<-class(paramObj<-get(fnCallInputParamObjs[i], envir=parent.frame()))
    		paramObjClass1<-class(paramObj<-get(fnCallInputParamObjs[i], envir=sys.frame(-2)))
    		
    		if(length(paramObjClass1) > 1) 
  		  {
  		        # this check is necessary for "xtab" and "table" class of object which return both vaues for class()
  		        paramObjClass = paramObjClass1[1]
		    }
		    else
		    {
		          paramObjClass = paramObjClass1
		    }
    		
    		#print(paramObjClass)
    		
    		if(paramObjClass == "NULL")
    		{
    			if(i>1 && i<=len)
    			{
    				fnSignatureRunTime<-paste(fnSignatureRunTime,","," ",sep="")
    			}
    
    			fnSignatureRunTime<-paste(fnSignatureRunTime,fnCallInputParamObjs[i],"=","NULL",sep="")
    
    			#print(fnSignatureRunTime)
    		}
    		else if(paramObjClass == "xtabs")
    		{
    			if(i>1 && i<=len)
    			{
    				fnSignatureRunTime<-paste(fnSignatureRunTime,","," ",sep="")
    			}
    
    			fnSignatureRunTime<-paste(fnSignatureRunTime,fnCallInputParamObjs[i],"=",deparse(attr(paramObj,"call")),sep="")
    
    			#print(fnSignatureRunTime)
    		}
    		else if(paramObjClass == "numeric" || paramObjClass == "logical")
    		{
    			if(i>1 && i<=len)
    			{
    				fnSignatureRunTime<-paste(fnSignatureRunTime,","," ",sep="")
    			}
    
    			fnSignatureRunTime<-paste(fnSignatureRunTime,fnCallInputParamObjs[i],"=","c","(",sep="")
    			objLen<-length(paramObj)
    
    			for(j in 1:objLen)
    			{
    				if(j > 1)
    				{
    					fnSignatureRunTime<-paste(fnSignatureRunTime,",")
    				}
    
    				fnSignatureRunTime<-paste(fnSignatureRunTime,paramObj[j],sep="")
    			}
    
    			fnSignatureRunTime<-paste(fnSignatureRunTime,")")
    			
    			
    			#print(fnSignatureRunTime)
    		}
    		else if(paramObjClass == "character")
    		{
    			if(i>1 && i<=len)
    			{
    				fnSignatureRunTime<-paste(fnSignatureRunTime,","," ",sep="")
    			}
    
    			fnSignatureRunTime<-paste(fnSignatureRunTime,fnCallInputParamObjs[i],"=","c","(",sep="")
    			objLen<-length(paramObj)
    
    			for(j in 1:objLen)
    			{
    				if(j > 1)
    				{
    					fnSignatureRunTime<-paste(fnSignatureRunTime,",")
    				}
    
    				fnSignatureRunTime<-paste(fnSignatureRunTime,"'", paramObj[j],"'",sep="")
    			}
    
    			fnSignatureRunTime<-paste(fnSignatureRunTime,")")
    
    			#print(fnSignatureRunTime)
    		}
    		else if(paramObjClass == "list" || paramObjClass == "data.frame")
    		{
    			if(i>1 && i<=len)
    			{
    				fnSignatureRunTime<-paste(fnSignatureRunTime,","," ",sep="")
    			}
    			
    			if(paramObjClass == "list")
    			{
    
    				fnSignatureRunTime<-paste(fnSignatureRunTime,fnCallInputParamObjs[i],"=","list","(",sep="")
    			}
    			else #paramObjClass == "data.frame"
    			{
    
    				fnSignatureRunTime<-paste(fnSignatureRunTime,fnCallInputParamObjs[i],"=","data.frame","(",sep="")
    			}
    
    			objLen<-length(paramObj)
    			
    			for(k in 1:objLen)
    			{
    				listItemObjClass<-class(paramObj[[k]])
    
    				#print(listItemObjClass)
    		
    				if(listItemObjClass == "numeric" || listItemObjClass == "logical")
    				{
    					if(k>1 && k<=len)
    					{
    						fnSignatureRunTime<-paste(fnSignatureRunTime,","," ",sep="")
    					}
    					
    					if(length(names(paramObj[k])) >0 && names(paramObj[k]) != "")
    					{
    						fnSignatureRunTime<-paste(fnSignatureRunTime,names(paramObj[k]),"=",sep="")
    					}
    
    					fnSignatureRunTime<-paste(fnSignatureRunTime,"c","(",sep="")
    					itemLen<-length(paramObj[[k]])
    
    					for(j in 1:itemLen)
    					{
    						if(j > 1)
    						{
    							fnSignatureRunTime<-paste(fnSignatureRunTime,",")
    						}
    
    						fnSignatureRunTime<-paste(fnSignatureRunTime,paramObj[[k]][j],sep="")
    					}
    
    					fnSignatureRunTime<-paste(fnSignatureRunTime,")")
    			
    					#print(fnSignatureRunTime)
    				}
    				else if(listItemObjClass == "character" ||listItemObjClass == "factor") #factor for data.frame
    				{
    					if(k>1 && k<=len)
    					{
    						fnSignatureRunTime<-paste(fnSignatureRunTime,","," ",sep="")
    					}
    					
    					if(length(names(paramObj[k])) >0 && names(paramObj[k]) != "")
    					{
    						fnSignatureRunTime<-paste(fnSignatureRunTime,names(paramObj[k]),"=",sep="")
    					}
    
    					fnSignatureRunTime<-paste(fnSignatureRunTime,"c","(",sep="")
    					itemLen<-length(paramObj[[k]])
    
    					for(j in 1:itemLen)
    					{
    						if(j > 1)
    						{
    							fnSignatureRunTime<-paste(fnSignatureRunTime,",")
    						}
    
    						fnSignatureRunTime<-paste(fnSignatureRunTime,"'",paramObj[[k]][j],"'",sep="")
    					}
    
    					fnSignatureRunTime<-paste(fnSignatureRunTime,")")
    
    					#print(fnSignatureRunTime)
    				}
    				else # Error list item type not processed by this function at this time
    				{
    				}	
    			}
    
    			fnSignatureRunTime<-paste(fnSignatureRunTime,")")
    		}
    		else # Error class type not processed by this function at this time
    		{
    		}
    	}
    	
    	fnSignatureRunTime<-paste(fnSignatureRunTime," ", ")", sep="")
    
    	# Each element in the following List is a character vector object
    	# First character vector captures how the function was called
    	# Second character vector captures function call with inline expansion of the call parameters
    
    	callDetails<- list(FuntionCallMade = fnCall, RuntimeParamValues = fnSignatureRunTime)
    	
    	# "callDetails" list with function param details can be written to a log file at this point
    
    	invisible(callDetails)
}

uaLogFunArgsDetails<-function()
{
    	#fnCall<-sys.call(-1)
    	fnCall<-sys.call(-2)
    	
    	fnName<-as.character(fnCall)[1]
    	
    	fnSignatureRunTime<-paste(fnName,"(", " ", sep="")
    	
    	
    	#print(fnCall)
    	#fnCallInputParamValues<-capture.output(print(ls.str(parent.frame(5)),give.head = FALSE))
    	#fnCallInputParamValues<-gsub('"', "", fnCallInputParamValues)
    
    	#fnCallInputParamObjs <- ls(parent.frame())
    	fnCallInputParamObjs <- ls(sys.frame(-2))
    	
    	#print( fnCallInputParamObjs )
    	
    	len <- length(fnCallInputParamObjs)
		
		#31May2015 this is to avoid the crash in the BSkyFunctionInit() when any BSky analytics function is called without parameter
		 if(len ==0)
		 {
			fnSignatureRunTime<-paste(fnName,"(", ")", sep="")
			return(callDetails<- list(FuntionCallMade = fnCall, RuntimeParamValues = fnSignatureRunTime))
		 }		
		 
		restrictNumberOfItems = 5
		restrictItemlength = 5
		
    	
    	for(i in 1:len)
    	{
    		#print(fnCallInputParamObjs[i])
    
    		#paramObjClass1<-class(paramObj<-get(fnCallInputParamObjs[i], envir=parent.frame()))
    		paramObjClass1<-class(paramObj<-get(fnCallInputParamObjs[i], envir=sys.frame(-2)))
    		
    		if(length(paramObjClass1) > 1) 
  		    {
  		        # this check is necessary for "xtab" and "table" class of object which return both vaues for class()
  		        paramObjClass = paramObjClass1[1]
		    }
		    else
		    {
		          paramObjClass = paramObjClass1
		    }
    		
    		#print(paramObjClass)
    		
    		if(paramObjClass == "NULL")
    		{
    			if(i>1 && i<=len)
    			{
    				fnSignatureRunTime<-paste(fnSignatureRunTime,","," ",sep="")
    			}
    
    			fnSignatureRunTime<-paste(fnSignatureRunTime,fnCallInputParamObjs[i],"=","NULL",sep="")
    
    			#print(fnSignatureRunTime)
    		}
    		else if(paramObjClass == "xtabs")
    		{
    			if(i>1 && i<=len)
    			{
    				fnSignatureRunTime<-paste(fnSignatureRunTime,","," ",sep="")
    			}
    
    			fnSignatureRunTime<-paste(fnSignatureRunTime,fnCallInputParamObjs[i],"=",deparse(attr(paramObj,"call")),sep="")
    
    			#print(fnSignatureRunTime)
    		}
    		else if(paramObjClass == "numeric" || paramObjClass == "logical")
    		{
    			if(i>1 && i<=len)
    			{
    				fnSignatureRunTime<-paste(fnSignatureRunTime,","," ",sep="")
    			}
    
    			fnSignatureRunTime<-paste(fnSignatureRunTime,fnCallInputParamObjs[i],"=","c","(",sep="")
    			
				objLen<-length(paramObj)
				
				if(objLen > restrictItemlength) objLen = restrictItemlength
    
    			for(j in 1:objLen)
    			{
    				if(j > 1)
    				{
    					fnSignatureRunTime<-paste(fnSignatureRunTime,",")
    				}
    
    				fnSignatureRunTime<-paste(fnSignatureRunTime,paramObj[j],sep="")
    			}
    
    			fnSignatureRunTime<-paste(fnSignatureRunTime,")")
    			
    			
    			#print(fnSignatureRunTime)
    		}
    		else if(paramObjClass == "character")
    		{
    			if(i>1 && i<=len)
    			{
    				fnSignatureRunTime<-paste(fnSignatureRunTime,","," ",sep="")
    			}
    
    			fnSignatureRunTime<-paste(fnSignatureRunTime,fnCallInputParamObjs[i],"=","c","(",sep="")
    			
				objLen<-length(paramObj)
				
				if(objLen > restrictItemlength) objLen = restrictItemlength
    
    			for(j in 1:objLen)
    			{
    				if(j > 1)
    				{
    					fnSignatureRunTime<-paste(fnSignatureRunTime,",")
    				}
    
    				fnSignatureRunTime<-paste(fnSignatureRunTime,"'", paramObj[j],"'",sep="")
    			}
    
    			fnSignatureRunTime<-paste(fnSignatureRunTime,")")
    
    			#print(fnSignatureRunTime)
    		}
    		else if(paramObjClass == "list" || paramObjClass == "data.frame")
    		{
    			if(i>1 && i<=len)
    			{
    				fnSignatureRunTime<-paste(fnSignatureRunTime,","," ",sep="")
    			}
    			
    			if(paramObjClass == "list")
    			{
    
    				fnSignatureRunTime<-paste(fnSignatureRunTime,fnCallInputParamObjs[i],"=","list","(",sep="")
    			}
    			else #paramObjClass == "data.frame"
    			{
    
    				fnSignatureRunTime<-paste(fnSignatureRunTime,fnCallInputParamObjs[i],"=","data.frame","(",sep="")
    			}
    
    			objLen<-length(paramObj)
				
				if(objLen > restrictNumberOfItems) objLen = restrictNumberOfItems
    			
    			for(k in 1:objLen)
    			{
    				listItemObjClass<-class(paramObj[[k]])
    
    				#print(listItemObjClass)
    		
    				if(listItemObjClass == "numeric" || listItemObjClass == "logical")
    				{
    					if(k>1 && k<=len)
    					{
    						fnSignatureRunTime<-paste(fnSignatureRunTime,","," ",sep="")
    					}
    					
    					if(length(names(paramObj[k])) >0 && names(paramObj[k]) != "")
    					{
    						fnSignatureRunTime<-paste(fnSignatureRunTime,names(paramObj[k]),"=",sep="")
    					}
    
    					fnSignatureRunTime<-paste(fnSignatureRunTime,"c","(",sep="")
    					itemLen<-length(paramObj[[k]])
						
						if(itemLen > restrictItemlength) itemLen = restrictItemlength
    
    					for(j in 1:itemLen)
    					{
    						if(j > 1)
    						{
    							fnSignatureRunTime<-paste(fnSignatureRunTime,",")
    						}
    
    						fnSignatureRunTime<-paste(fnSignatureRunTime,paramObj[[k]][j],sep="")
    					}
    
    					fnSignatureRunTime<-paste(fnSignatureRunTime,")")
    			
    					#print(fnSignatureRunTime)
    				}
    				else if(listItemObjClass == "character" ||listItemObjClass == "factor") #factor for data.frame
    				{
    					if(k>1 && k<=len)
    					{
    						fnSignatureRunTime<-paste(fnSignatureRunTime,","," ",sep="")
    					}
    					
    					if(length(names(paramObj[k])) >0 && names(paramObj[k]) != "")
    					{
    						fnSignatureRunTime<-paste(fnSignatureRunTime,names(paramObj[k]),"=",sep="")
    					}
    
    					fnSignatureRunTime<-paste(fnSignatureRunTime,"c","(",sep="")
    					itemLen<-length(paramObj[[k]])
    
						if(itemLen > restrictItemlength) itemLen = restrictItemlength
						
    					for(j in 1:itemLen)
    					{
    						if(j > 1)
    						{
    							fnSignatureRunTime<-paste(fnSignatureRunTime,",")
    						}
    
    						fnSignatureRunTime<-paste(fnSignatureRunTime,"'",paramObj[[k]][j],"'",sep="")
    					}
    
    					fnSignatureRunTime<-paste(fnSignatureRunTime,")")
    
    					#print(fnSignatureRunTime)
    				}
    				else # Error list item type not processed by this function at this time
    				{
    				}	
    			}
    
    			fnSignatureRunTime<-paste(fnSignatureRunTime,")")
    		}
    		else # Error class type not processed by this function at this time
    		{
    		}
    	}
    	
    	fnSignatureRunTime<-paste(fnSignatureRunTime," ", ")", sep="")
    
    	# Each element in the following List is a character vector object
    	# First character vector captures how the function was called
    	# Second character vector captures function call with inline expansion of the call parameters
    
    	callDetails<- list(FuntionCallMade = fnCall, RuntimeParamValues = fnSignatureRunTime)
    	
    	# "callDetails" list with function param details can be written to a log file at this point
    
    	invisible(callDetails)
}

#########################################################
# This is the call logging routine designed to be
# called from within error and warning handler for
# a Try-Catch block
#########################################################
uaLogFunArgsDetailsInExceptionHandler<-function(exceptionType, frameNum, offendingFunction = NA)
{
		callingFnName = ""
    	callingFnNameOneLevelUp = ""
    	fnSignatureRunTime =  ""
    	fnSignatureRunTimeOneLevelUp =  ""   
		runtimeParamValuesOneLevelUp = ""
		nearestUpperLevelTryCatchFunctionName = ""
		fnSignatureRunTimeTryCatchFunction = ""
		assumeCallStackDepth = 0
		
		#cat("Frame Number :-",frameNum,"\n")
    	fnSignatureRunTime<-list()
    	#print(  offendingFunction )
    	
    	#callingFnName = ""
		
		
		#cat("\n=============================================\n")
		#cat("In uaLogFunArgsDetailsInExceptionHandler() - Printing R Stack - Exception Type: ", exceptionType, "\n")
		
		#Calculating how deep is current R system level function call Stack
		#At the end of this for loop assumeCallStackDepth will contain the current depth (# of levels) of R stack
		for(assumeCallStackDepth in 2:100)
		{
			#cat("stack index: ", assumeCallStackDepth)
			stackfnCall<-sys.call(-assumeCallStackDepth)
			if(!is.null(stackfnCall))
			{
				stackfnName<-as.character(stackfnCall)[1]
				#cat(" and Stack Function Name: ",stackfnName,"\n")
			}
			else
			{
				break
			}
		}
		#cat("=============================================\n")
    	
		# frame number 15 for warning and 8 for error. For error (not for warning) to get to the function
		# which trapped the error exception
    	fnCall<-sys.call(-frameNum)
    	fnName<-as.character(fnCall)[1]
    	nearestUpperLevelTryCatchFunctionName = fnName
    	
		#cat("++++++++++++++++++\n")
    	#cat("In uaLogFunArgsDetailsInExceptionHandler (Frame 15 for warn and 8 for error) Frame Number :-",frameNum," ", "and Functiona Name: ",fnName, "\n")
		#cat("Printing fnCall<-sys.call(-frameNum)\n")
		#print(fnCall)
		#cat("++++++++++++++++++\n")
		
		#function name for one level up
		#callingFn <-sys.call(-(frameNum + 1))
		#callingFnName <- as.character(callingFn)[1]
    	
		#cat("In uaLogFunArgsDetailsInExceptionHandler - Ofending Function 1: \n")
		#print(offendingFunction)
		#cat("In uaLogFunArgsDetailsInExceptionHandler - offending function name - does it not start with withCallingHandlers? \n")
		#print(is.na(pmatch("withCallingHandlers", offendingFunction)))
		
    	if( exceptionType == "ERR")
    	{
			if(!is.na(offendingFunction) && offendingFunction != "NULL" && is.na(pmatch("withCallingHandlers", offendingFunction)))
			{
				correctFunctionCallFormat = gsub("\"","'",offendingFunction, fixed=TRUE)
				#cat("In uaLogFunArgsDetailsInExceptionHandler - Ofending Function 2: \n")
				#print(correctFunctionCallFormat )
    	   
				functionName =  strsplit(correctFunctionCallFormat,"\\(")
				#cat("In uaLogFunArgsDetailsInExceptionHandler - Ofending Function 3: \n")
				#print(functionName)
				
				#cat("In uaLogFunArgsDetailsInExceptionHandler - Ofending Function param 4: \n")
				runtimeFunctionParam =   correctFunctionCallFormat
			}
			else
			{
				# use the nearest function name that contains the try-catch block and the one caught the
				# exception
				#functionName = nearestUpperLevelTryCatchFunctionName
				#runtimeFunctionParam = fnCall
				
				correctFunctionCallFormat = gsub("\"","'",fnCall, fixed=TRUE)
				#cat("In uaLogFunArgsDetailsInExceptionHandler (withCallingHandlers?)- Ofending Function 5: \n")
				#print(  correctFunctionCallFormat )
    	   
				functionName =  strsplit(correctFunctionCallFormat,"\\(")
				#cat("In uaLogFunArgsDetailsInExceptionHandler (withCallingHandlers?)- Ofending Function 6: \n")
				#print(functionName)
    	   
				runtimeFunctionParam =   correctFunctionCallFormat
				#cat("In uaLogFunArgsDetailsInExceptionHandler (withCallingHandlers?)- Ofending Function param 7: \n")
				#print(runtimeFunctionParam)
			}
    	   
			if(uadatasets.sk$callStackIndex > 0)
			{
			   #cat ("In uaLogFunArgsDetailsInExceptionHandler uadatasets.sk$callStackIndex is : ", uadatasets.sk$callStackIndex, "\n")
			   for(k in  uadatasets.sk$callStackIndex:1)
			   {
					if(uadatasets.sk$callStack[[k]]$FunctionName == nearestUpperLevelTryCatchFunctionName)
					{
					  fnSignatureRunTimeTryCatchFunction = uadatasets.sk$callStack[[k]]$FunctionParam
					  break
					}
				}
    	   
				for(k in  uadatasets.sk$callStackIndex:1)
				{
					#cat("In uaLogFunArgsDetailsInExceptionHandler printing call stack and oofending function name \n")
					#print( uadatasets.sk$callStack[[k]]$FunctionName )
					#print ( functionName[[1]][1] )
					
					if(uadatasets.sk$callStack[[k]]$FunctionName == functionName[[1]][1])
					{
					  runtimeFunctionParam = uadatasets.sk$callStack[[k]]$FunctionParam
					  
					  # This may not be accurate for one level up function details. This depends on whether
					  # every subfuction called the uaFunctionInit() to create the call stack array correctly
					  if(uadatasets.sk$callStackIndex > 2)
					  {
						callingFnNameOneLevelUp = uadatasets.sk$callStack[[k-1]]$FunctionName
						runtimeParamValuesOneLevelUp = uadatasets.sk$callStack[[k-1]]$FunctionParam
					  }
					  
					  # This is not needed the enxt clean up for loop will take care of this
					  #uadatasets.sk$callStack[k] <- NULL
					  #uadatasets.sk$callStackIndex = uadatasets.sk$callStackIndex - 1
					  
					  break
					}
				}
				
				if(callingFnNameOneLevelUp == "")
				{
					callingFnNameOneLevelUp = uadatasets.sk$callStack[[uadatasets.sk$callStackIndex]]$FunctionName
					runtimeParamValuesOneLevelUp = uadatasets.sk$callStack[[uadatasets.sk$callStackIndex]]$FunctionParam
				}
			}
				
      		#clean up the stack from the bottom of the call stack till the nearest try-catch function
      		#if(uadatasets.sk$callStackIndex > 0)
      		#{
			#	for(k in  uadatasets.sk$callStackIndex:1)
			#	{
					#print( uadatasets.sk$callStack[[k]]$FunctionName )
					#print ( functionName[[1]][1] )
			#		if(uadatasets.sk$callStack[[k]]$FunctionName == nearestUpperLevelTryCatchFunctionName)
			#		{
			#		  break
			#		}
			#		else
			#		{
			#			uadatasets.sk$callStack[k] <- NULL
			#			uadatasets.sk$callStackIndex = uadatasets.sk$callStackIndex - 1
			#		}
			#	}
			#}
      		
   		   if(length(runtimeFunctionParam) == 0) runtimeFunctionParam = ""
   		   if(length(nearestUpperLevelTryCatchFunctionName) == 0) nearestUpperLevelTryCatchFunctionName = ""
   		   #if(length(callingFnName) == 0) callingFnName = ""
   		   
    	   #callDetails<- list(FuntionCallMade = functionName[[1]][1], RuntimeParamValues = runtimeFunctionParam, NearestTryCatchFnName =nearestUpperLevelTryCatchFunctionName, CallingFnName = callingFnName)
    	   callDetails<- list(FuntionCallMade = functionName[[1]][1], RuntimeParamValues = runtimeFunctionParam, CallingFnNameOneLevelUp = callingFnNameOneLevelUp, RuntimeParamValuesOneLevelUp = runtimeParamValuesOneLevelUp, NearestTryCatchFnName =nearestUpperLevelTryCatchFunctionName, RunTimeParamValuesTryCatchFunction = fnSignatureRunTimeTryCatchFunction)
		   
		   #cat("&&&&&&&&&&&&&&&&&&&&&&&&\n")
		   #cat ("In uaLogFunArgsDetailsInExceptionHandler printing the return structure before reurning to error handler\n")
		   #print(callDetails)
		   #cat("&&&&&&&&&&&&&&&&&&&&&&&&\n")
		   
    	   return(callDetails)
    	}
    	
    	if ( exceptionType == "WARN")
    	{
    	    callingFnName = ""
    	    callingFnNameOneLevelUp = ""
    	    fnSignatureRunTime =  ""
    	    fnSignatureRunTimeOneLevelUp =  ""
			runtimeParamValuesOneLevelUp = ""
           
			nearestUpperLevelTryCatchFunctionName = ""
			fnSignatureRunTimeTryCatchFunction = ""
           
            i = 1
			# Actual R stack depth is already calculated at the begining of this function
			#assumeCallStackDepth = 18
			while (i < assumeCallStackDepth )
			{
      	       stackfnCall<-sys.call(-i)
      	       stackfnName<-as.character(stackfnCall)[1]
      	       #cat ("first Loop :", stackfnName, "\n")
      	       #x<- pmatch(".signalSimpleWarning", stackfnName)
      	       #if(!is.na(x))
      	       if(stackfnName == ".signalSimpleWarning" || !is.na(pmatch("signal", stackfnName)))
      	       {
      	           break;
      	       }
      	       i = i + 1
			}
      	   
			i = i + 1
		   
			if( i < assumeCallStackDepth )
		    {
				stackfnCall<-sys.call(-i)
				stackfnName<-as.character(stackfnCall)[1]
			}
      	   
      	   if( stackfnName != "withCallingHandlers" )
      	   {
				callingFnName = stackfnName
        	     
				i = i + 1
        	     
				while (i < assumeCallStackDepth )
				{
          	       stackfnCall<-sys.call(-i)
          	       stackfnName<-as.character(stackfnCall)[1]
          	       #cat ("Second Loop :", stackfnName, "\n")
          	       
				   #if( stackfnName == "withCallingHandlers" || !is.na(pmatch("BSky", stackfnName)) || !is.na(pmatch("UA", stackfnName)) || !is.na(pmatch("Ua", stackfnName)) )
          	       if(stackfnName == "withCallingHandlers" || is.na(pmatch("eval", stackfnName)))
				   {
          	           break;
          	       }
          	       i = i + 1
				}
				
				if( i < assumeCallStackDepth && stackfnName != "withCallingHandlers" )
				{
      	           callingFnNameOneLevelUp =  stackfnName
				}
				
				while (i < assumeCallStackDepth )
				{
          	       stackfnCall<-sys.call(-i)
          	       stackfnName<-as.character(stackfnCall)[1]
          	       #cat ("Third Loop :", stackfnName, "\n")
          	       if( stackfnName == "withCallingHandlers" )
          	       {
          	           break;
          	       }
          	       i = i + 1
          	   }
      	   }
      	   
      	   i = i + 1
      	   
      	   while (i < assumeCallStackDepth )
      	   {
      	       stackfnCall<-sys.call(-i)
      	       stackfnName<-as.character(stackfnCall)[1]
      	       #cat ("Fourth Loop :", stackfnName, "\n")
      	       
               x<- pmatch("tryCatch", stackfnName)
               y <- pmatch("doTryCatch", stackfnName)

               if( is.na(x) && is.na(y))
      	       {
      	           break;
      	       }
      	       i = i + 1
      	   }
      	   
      	   if (i < assumeCallStackDepth )
      	   {
      	       nearestUpperLevelTryCatchFunctionName = stackfnName
			   
      	       if(callingFnName == "")
      	       {
      	         callingFnName =  stackfnName 
      	       }
			   
			   if(callingFnNameOneLevelUp == "")
      	       {
      	         callingFnNameOneLevelUp =  stackfnName 
      	       }
      	   }
		   
		   #cat("In uaLogFunArgsDetailsInExceptionHandler for Warning - Ofending Functionname and the stack function name : \n")
		   #print(callingFnName)
		   #print(stackfnName)
		   
		   #Not needed - Check this logic for callingFnNameOneLevelUp later by running some more tests
      	   #if(callingFnNameOneLevelUp == "")
		   #{
		   #	callingFnNameOneLevelUp = nearestUpperLevelTryCatchFunctionName
		   #}
		   
		   if( uadatasets.sk$callStackIndex > 0)
		   {
			   for(k in  uadatasets.sk$callStackIndex:1)
				{
					#print( uadatasets.sk$callStack[[k]]$FunctionName )
					#print ( functionName[[1]][1] )
					if(uadatasets.sk$callStack[[k]]$FunctionName == callingFnName)
					{
						fnSignatureRunTime = uadatasets.sk$callStack[[k]]$FunctionParam
					}
					
					if(uadatasets.sk$callStack[[k]]$FunctionName == callingFnNameOneLevelUp)
					{
						fnSignatureRunTimeOneLevelUp = uadatasets.sk$callStack[[k]]$FunctionParam
					}
					
					if(uadatasets.sk$callStack[[k]]$FunctionName == nearestUpperLevelTryCatchFunctionName)
					{
						fnSignatureRunTimeTryCatchFunction = uadatasets.sk$callStack[[k]]$FunctionParam
					}    
				} 
			}
        		
        	callDetails<- list(FuntionCallMade = callingFnName, RuntimeParamValues = fnSignatureRunTime, CallingFnNameOneLevelUp = callingFnNameOneLevelUp, RuntimeParamValuesOneLevelUp = fnSignatureRunTimeOneLevelUp, NearestTryCatchFnName = nearestUpperLevelTryCatchFunctionName, RunTimeParamValuesTryCatchFunction = fnSignatureRunTimeTryCatchFunction)
            
			#cat("$$$$$$$$$$$$$$$$$$$$$$$$$$\n")
			#cat ("In uaLogFunArgsDetailsInExceptionHandler printing the return structure before reurning to warning handler\n")
			#print(callDetails)
			#cat("$$$$$$$$$$$$$$$$$$$$$$$$$$\n")
            
			#return(callDetails)
      }
}


UAwarnHandlerFn <- function(ex) 
{
		#cat("in warning handler :\n")
		#print(ex)
		
		FuntionCallMadeStackIndex = 0
		CallingFnNameOneLevelUpStackIndex = 0
		NearestTryCatchFnNameStackIndex = 0

		if(is.null(ex)) 
		{
			ex <- warnings();
  		}
  		
  		#cat("\nPrinting callfn from =conditionCall(ex) in warning handler\n")
  		#callfn =conditionCall(ex)
		#print(callfn)
      
		#uaLogFunArgsDetails2(15)
      
		#The  R warning message
		uarwarnmsg =conditionMessage(ex)
		
		#cat("\n R Warn Msg: ",uarwarnmsg, "\n")
		prefix = "R Warn Msg :"
		uarwarnmsg = paste(prefix, uarwarnmsg)
      
  		# We need to go back 15 stack frames to retrieve the calling singnature for 
		# the function that generated the warning exception
		#functionCallDeails<-uaLogFunArgsDetails(15) 
		#functionCallDeails<-uaLogFunArgsDetails3(25)
		
		# functionCallDeails contains
		# functionName= functionCallDeails$FuntionCallMade , 
		# functionCommand = functcommand, 
		# uberFunct = uadatasets.sk$uaTempUberFunctionName,  
		# nearestTryCatch = functionCallDeails$NearestTryCatchFnName, 
		# runTimeParamValuesTryCatchFunction = functionCallDeails$RunTimeParamValuesTryCatchFunction
		# callingFnNameOneLevelUp = functionCallDeails$CallingFnNameOneLevelUp, 
		# runTimeParamValuesOneLevelUp = functionCallDeails$RuntimeParamValuesOneLevelUp, 
		
		#cat("=======================\n")
		functionCallDeails<-uaLogFunArgsDetailsInExceptionHandler("WARN", 15)
		#cat("Returned structure functionCallDeails from uaLogFunArgsDetailsInExceptionHandler(WARN 15)\n")
		#print(functionCallDeails)
		#cat("=======================\n")
      
		#functionCallDeails<-uaLogFunArgsDetailsInExceptionHandler("WARN", 15)
		#print(functionCallDeails)
			
		uadatasets.sk$totalWarnings = uadatasets.sk$totalWarnings+1
			
		#if(as.character(functionCallDeails$FuntionCallMade)[1] !=uadatasets.sk$uaTempUberFunctionName && !is.na(pmatch("withCallingHandlers", deparse(callfn))) )
		if(functionCallDeails$NearestTryCatchFnName != uadatasets.sk$uaTempUberFunctionName)
		{
			#cat ("I am in sub function warning handler\n")
          
			#To keep track of warnings in sub function
			uadatasets$warning =-1;
			uadatasets$uawarnfn=-1

			#uadatasets$uawarnmsgdis =This is a string
			#uadatasets$uarwarnmsg -This is a vector of strings
          
    		#keeps track of the total number of warnings
    		uadatasets$warnindex=uadatasets$warnindex+1;
    			
    		#Handling multiple R warning msgs
		    #uadatasets$uarwarnmsg =c(uadatasets$uarwarnmsg,uarwarnmsg );

			#Application level (not R system level) custom warning messages
			#uadatasets$uawarnmsgdis =c(uadatasets$uawarnmsgdis, uadatasets.sk$uaTempWarnmsgdis)
			#appMsgDisp =  uadatasets$uawarnmsgdis
			#appMsgDisp = paste(deparse(uadatasets$uawarnmsgdis), collapse = " ")
			
			#appMsgDisp =  uadatasets.sk$uaTempWarnmsgdis
			
			#Application level (not R system level) custom warning messages
			if(uadatasets.sk$callStackIndex > 0)
			{
				for(k in  uadatasets.sk$callStackIndex:1)
				{
					#if(uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$FuntionCallMade || uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$CallingFnNameOneLevelUp || uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$NearestTryCatchFnName)
					#{
						#list(FunctionName = functionName, FunctionParam = callDetails$RuntimeParamValues, BSkyErrMsg="", BSkyWarnMsg="", ErrFound=0, WarnFound=0 )
					#	appMsgDisp = paste(deparse(uadatasets.sk$callStack[[k]]$BSkyWarnMsg), collapse = " ")
					#	uadatasets.sk$callStack[[k]]$WarnFound=1
					#}
					
					#if(uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$NearestTryCatchFnName)
					#{
						#list(FunctionName = functionName, FunctionParam = callDetails$RuntimeParamValues, BSkyErrMsg="", BSkyWarnMsg="", ErrFound=0, WarnFound=0 )
						#appMsgDisp = paste(deparse(uadatasets.sk$callStack[[k]]$BSkyErrMsg), collapse = " ")
					#	uadatasets.sk$callStack[[k]]$WarnFound=1
					#	break
					#}
									
					if(uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$FuntionCallMade)
					{	
						FuntionCallMadeStackIndex = k
					}
					
					if(uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$CallingFnNameOneLevelUp)
					{
						CallingFnNameOneLevelUpStackIndex = k
					}
					
					if(uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$NearestTryCatchFnName)
					{
						NearestTryCatchFnNameStackIndex = k
					}
				}
					
				if(FuntionCallMadeStackIndex >0)
				{
					appMsgStackIndex = FuntionCallMadeStackIndex
				}
				else if(CallingFnNameOneLevelUpStackIndex > 0)
				{
					appMsgStackIndex = CallingFnNameOneLevelUpStackIndex
				}
				else if(NearestTryCatchFnNameStackIndex > 0)
				{
					appMsgStackIndex = NearestTryCatchFnNameStackIndex 
				}
				else # this is just a safety net to catch all - it should not enter this else at all
				{
					appMsgStackIndex = NearestTryCatchFnNameStackIndex 
				}
					
				appMsgDisp = paste(deparse(uadatasets.sk$callStack[[appMsgStackIndex]]$BSkyWarnMsg), collapse = " ")
				uadatasets.sk$callStack[[appMsgStackIndex]]$WarnFound=1
				uadatasets.sk$callStack[[NearestTryCatchFnNameStackIndex]]$WarnFound=1
			}
			
    			
			# Newly added on 05/27/13 to store all warning messages either from Uber function or any sub function
			# in an array with the uadataset.sk global variable - this uadataset.sk will get reset once uber function
			# completes (uber function wrap up call
			
			#Handling multiple R warning msgs
			#uadatasets.sk$uberUaRwarnmsg =c(uadatasets.sk$uberUaRwarnmsg,uarwarnmsg )

			#Application level (not R system level) custom warning messages
    		uadatasets.sk$uberUaAppwarnmsgdis =c(uadatasets.sk$uberUaAppwarnmsgdis, appMsgDisp)
			
    		# As it can have several warning messages on a stat function, the list of variables that generated the warning, and the messages
			# uadatasets$uawarnvar =c(uadatasets$uawarnvar,uadatasets$uaTempWarnvar)
			# This should come from the global application level message store
			# Warning handler has no access to the var list
		} 
		else
		{
			#Catching untrapped warnings
			#cat ("I am in uber function warning handler\n")
			
			#========================================================
			#Newly inroduced on 05/26/13 to keep track of warnings generated in 
			#ubber function just like it has been used earlier in sub functions
			uadatasets$warning =-1;
			uadatasets$uawarnfn=-1

			#uadatasets$uawarnmsgdis =This is a string
			#uadatasets$uarwarnmsg -This is a vector of strings
          
    		#keeps track of the total number of warnings
    		uadatasets$warnindex=uadatasets$warnindex+1;
    			
    		#Handling multiple R warning msgs
		    #uadatasets$uarwarnmsg =c(uadatasets$uarwarnmsg,uarwarnmsg );
			#==========================================================
			  
			if(uadatasets.sk$uaTempUberFunctionName ==functionCallDeails$FuntionCallMade || uadatasets.sk$uaTempUberFunctionName == functionCallDeails$NearestTryCatchFnName)
			{
				uadatasets.sk$totalUberWarnings = uadatasets.sk$totalUberWarnings+1
			}
			
			#Handling multiple R warning msgs
			#if(functionCallDeails$FuntionCallMade == uadatasets.sk$uaTempUberFunctionName)
			#{
				#uadatasets.sk$uberUaRwarnmsg =c(uadatasets.sk$uberUaRwarnmsg,uarwarnmsg );
			#}

			#Application level (not R system level) custom warning messages
			if(uadatasets.sk$callStackIndex > 0)
			{
				for(k in  uadatasets.sk$callStackIndex:1)
				{
					#if(uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$FuntionCallMade || uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$CallingFnNameOneLevelUp || uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$NearestTryCatchFnName)
					#{
						#list(FunctionName = functionName, FunctionParam = callDetails$RuntimeParamValues, BSkyErrMsg="", BSkyWarnMsg="", ErrFound=0, WarnFound=0 )
					#	appMsgDisp = paste(deparse(uadatasets.sk$callStack[[k]]$BSkyWarnMsg), collapse = " ")
					#	uadatasets.sk$callStack[[k]]$WarnFound=1
					#}
					
					#if(uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$NearestTryCatchFnName)
					#{
						#list(FunctionName = functionName, FunctionParam = callDetails$RuntimeParamValues, BSkyErrMsg="", BSkyWarnMsg="", ErrFound=0, WarnFound=0 )
						#appMsgDisp = paste(deparse(uadatasets.sk$callStack[[k]]$BSkyErrMsg), collapse = " ")
					#	uadatasets.sk$callStack[[k]]$WarnFound=1
					#	break
					#}
									
					if(uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$FuntionCallMade)
					{	
						FuntionCallMadeStackIndex = k
					}
					
					if(uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$CallingFnNameOneLevelUp)
					{
						CallingFnNameOneLevelUpStackIndex = k
					}
					
					if(uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$NearestTryCatchFnName)
					{
						NearestTryCatchFnNameStackIndex = k
					}
				}
					
				if(FuntionCallMadeStackIndex >0)
				{
					appMsgStackIndex = FuntionCallMadeStackIndex
				}
				else if(CallingFnNameOneLevelUpStackIndex > 0)
				{
					appMsgStackIndex = CallingFnNameOneLevelUpStackIndex
				}
				else if(NearestTryCatchFnNameStackIndex > 0)
				{
					appMsgStackIndex = NearestTryCatchFnNameStackIndex 
				}
				else # this is just a safety net to catch all - it should not enter this else at all
				{
					appMsgStackIndex = NearestTryCatchFnNameStackIndex 
				}
					
				appMsgDisp = paste(deparse(uadatasets.sk$callStack[[appMsgStackIndex]]$BSkyWarnMsg), collapse = " ")
				uadatasets.sk$callStack[[appMsgStackIndex]]$WarnFound=1
				uadatasets.sk$callStack[[NearestTryCatchFnNameStackIndex]]$WarnFound=1
			}
			
    		#uadatasets.sk$uberUaAppwarnmsgdis =c(uadatasets.sk$uberUaAppwarnmsgdis, uadatasets.sk$uaTempWarnmsgdis)
			uadatasets.sk$uberUaAppwarnmsgdis =c(uadatasets.sk$uberUaAppwarnmsgdis, appMsgDisp)
    		#appMsgDisp =   uadatasets.sk$uaTempWarnmsgdis
    			
    		#Aaron, add your code here, use parameters to uawritelog
  			#Aaron, this code was added to accomodate the fact that a warning may have been generated before the return structure
  			#was created
  			#bskyNoofTables=length(uadatasets$retstructure)
  			#if (bskyNoofTables==0)
  			#{
			#	uadatasets$retstructure[[1]]<-list()
			#	#Aaron: We don't know if this is a table or graph, this will be filled in later
			#	#uadatasets$retstructure[[1]]$type="table"
			#	uadatasets$retstructure[[1]]$metadata="yes"
			#	#Aaron: We don't know if this is a return structure for a t.test or a crosstab, this will be filled in later
			#	#uadatasets$retstructure[[1]]$nometadatatables=1
			#	#uadatasets$retstructure[[1]]$metadatatabletype="normal"
			#	uadatasets$retstructure[[1]]$metadatatable=list()
			#	uadatasets$retstructure[[1]]$metadatatable[[1]]=data.frame(varIndex=NA,type=1,varName=NA,dataTableRow=NA,startCol=NA,endCol=NA,BSkyMsg=appMsgDisp,RMsg=uarwarnmsg)					
  			#}
  			#else
  			#{
			#	uadatasets$retstructure[[bskyNoofTables]]$metadatatable[[1]]=rbind(uadatasets$retstructure[[bskyNoofTables]]$metadatatable[[1]],data.frame(varIndex=NA,type=1,varName=NA,dataTableRow=NA,startCol=NA,endCol=NA,BSkyMsg=appMsgDisp,RMsg=uarwarnmsg))
  			#}		
		}
			
		#uawritelog <-function(type=NA,functionname=NA,functcommand=NA,uberfunct=NA,
		#                      starttime=NA,endtime =NA,usertime=NA, systemtime=NA,totalcputime =NA,elapsed=NA,initNcells=NA,
		#                      initVcells=NA,initTotal=NA, finNcells=NA, finVcells=NA, finTotal=NA,
		#                      maxNcells=NA,maxVcells=NA,maxTotal=NA,
		#                      message =NA,uamessage =NA,callfn=NA)

		# Writing the warning to the log	
		# uawritelog(type="Warning", functionname= as.character(functionCallDeails$FuntionCallMade)[1] , uberfunct=uadatasets.sk$uaTempUberFunctionName, functcommand =functionCallDeails$RuntimeParamValues, message =uarwarnmsg, uamessage =appMsgDisp);
		# uadatasets.sk$currentVarNamesAndFactorValues 
		# uadatasets.sk$currentDatasetname
		#cat("\n", "appWarnMsgDisp : ",  appMsgDisp, "\n")
		#cat("Runtime param len :", length( functionCallDeails$RuntimeParamValues), " Runtime param values :", functionCallDeails$RuntimeParamValues, "\n")
      
		if (length (functionCallDeails$RuntimeParamValues)==0 || (length (functionCallDeails$RuntimeParamValues)== 1 && functionCallDeails$RuntimeParamValues[[1]]=="" )) 
		{
			functcommand =NA
			uarwarnmsg = paste(uarwarnmsg, " in function : ", functionCallDeails$FuntionCallMade,"()")
		}
		else 
		{
			functcommand =functionCallDeails$RuntimeParamValues
			#uarwarnmsg = paste(uarwarnmsg, " in function2 : ", ",",functcommand,",","length :", length (functionCallDeails$RuntimeParamValues))
			uarwarnmsg = paste(uarwarnmsg, " in function : ",functcommand)
		}
		
		#Handling multiple R warning msgs
		uadatasets$uarwarnmsg =c(uadatasets$uarwarnmsg,uarwarnmsg );
		
		#Handling multiple R warning msgs
		#if(functionCallDeails$FuntionCallMade == uadatasets.sk$uaTempUberFunctionName)
		#{
			uadatasets.sk$uberUaRwarnmsg =c(uadatasets.sk$uberUaRwarnmsg,uarwarnmsg );
		#}
      
		# uawritelog(type="Warning", datasetName=uadatasets.sk$currentDatasetname, splitLevel=uadatasets.sk$currentVarNamesAndFactorValues,  functionName= functionCallDeails$FuntionCallMade , functionCommand =functionCallDeails$RuntimeParamValues, uberFunct=uadatasets.sk$uaTempUberFunctionName, RMessage =uarwarnmsg, BSkyMessage =appMsgDisp, nearestTryCatch = functionCallDeails$NearestTryCatchFnName, callingFnOneLevelUp = functionCallDeails$CallingFnName)
		uawritelog(type="Warning", datasetName=uadatasets.sk$currentDatasetname, splitLevel=uadatasets.sk$currentVarNamesAndFactorValues,  functionName= functionCallDeails$FuntionCallMade , functionCommand =functcommand, uberFunct=uadatasets.sk$uaTempUberFunctionName, RMessage =uarwarnmsg, BSkyMessage =appMsgDisp, nearestTryCatch = functionCallDeails$NearestTryCatchFnName, callingFnNameOneLevelUp = functionCallDeails$CallingFnNameOneLevelUp, runTimeParamValuesOneLevelUp = functionCallDeails$RuntimeParamValuesOneLevelUp, runTimeParamValuesTryCatchFunction = functionCallDeails$RunTimeParamValuesTryCatchFunction)
			
		# if it is a warning, continue the execution from the point where the warning was generated after the 
		# warning handler returns to main body of the execution
		invokeRestart("muffleWarning")
}

UAerrHandlerFn <- function(ex) 
{
	  #cat("printing ex in UAerrHandlerFn(ex) in Error Handler: Debug Msg - Remove this print statement later\n")
	  #print(ex)
	  #cat("\n")
	  
		FuntionCallMadeStackIndex = 0
		CallingFnNameOneLevelUpStackIndex = 0
		NearestTryCatchFnNameStackIndex = 0
	  
	  callfn =conditionCall(ex)
	  
	  #cat("Printing callfn =conditionCall(ex) in Error Handler \n")
	  #print(callfn)
	  #cat("\n")
	  
	  # sometime callfn returns more than one string fragment for the functiona name/signature
	  callSignature = paste(deparse(callfn), collapse = "")

	  #cat("Printing the call signature in Error Handler after massaging callfn returned from conditionCall(ex)\n")
	  #print(callSignature)
	  #cat("\n")
	  
	  #uaLogFunArgsDetailsErr(8)
	  
		#The  R error message
		uarerrmsg=conditionMessage(ex)
		#cat("\n R Err Msg to be commented out later in Error handler: ",uarerrmsg, "\n")
		prefix = "R Err Msg :"
		uarerrmsg = paste(prefix, uarerrmsg)
	
		uadatasets.sk$totalErrors = uadatasets.sk$totalErrors+1
			
		# We need to go back 8 stack frames to retrieve the calling singnature for 
		# the function that generated the Error exception
		# functionCallDeails<-uaLogFunArgsDetails2(8) 
	
		# functionCallDeails contains
		# functionName= functionCallDeails$FuntionCallMade , 
		# functionCommand = functcommand, 
		# uberFunct = uadatasets.sk$uaTempUberFunctionName,  
		# nearestTryCatch = functionCallDeails$NearestTryCatchFnName, 
		# runTimeParamValuesTryCatchFunction = functionCallDeails$RunTimeParamValuesTryCatchFunction
		# callingFnNameOneLevelUp = functionCallDeails$CallingFnNameOneLevelUp, 
		# runTimeParamValuesOneLevelUp = functionCallDeails$RuntimeParamValuesOneLevelUp, 
		
		#cat("=======\n")
		#cat("in Error Handler - before calling uaLogFunArgsDetailsInExceptionHandler(ERR, 8, callSignature)\n")
		#print(callSignature)
		functionCallDeails<-uaLogFunArgsDetailsInExceptionHandler("ERR", 8, callSignature)
		#cat("Returned structure functionCallDeails from uaLogFunArgsDetailsInExceptionHandler(ERR 8 callSignature)\n")
		#print(functionCallDeails)
		#cat("=======\n")
		#cat("in Error Handler - Printing call stack\n")
		#print(uadatasets.sk$callStack)
		#cat("=======\n")
		
		if(functionCallDeails$NearestTryCatchFnName != uadatasets.sk$uaTempUberFunctionName)
		{
			#cat ("I am in sub function error handler\n")
          
			#to keep track of errors generated in sub function
    		uadatasets$error =-1;
    		uadatasets$errorfn=-1
    
    		#keeps track of the total number of errors in the sub function
    		uadatasets$errorindex =uadatasets$errorindex +1
    			
    		#uadatasets$uaerrmsgdis-This is a string
			#uadatasets$uarerrmsg -This is a string 
          
    		#Handling multiple R error msgs for stat functions that will iterate through different variables
    		# uadatasets$uarerrmsg =c(uadatasets$uarerrmsg,uarerrmsg);
			# Actually in a sub function one one error will be processed after that the control has to
			# come out of the Try-Catch block. Unlike Warnings, for error there is no restart from the
			# point where the error has occured
    		uadatasets$uarerrmsg = uarerrmsg
    
    		#Application level (not R system level) custom eoor messages
    		#appErrMsgDisp = uadatasets$uaerrmsgdis 
			# Not sure whether there is function right now to set uadatasets$uaerrmsgdis from a sub function
			# Do we just use the Uber function message storage or should we create a new structure within
			# the call stack structure to store messages for each sub function?
    		#appErrMsgDisp = paste(deparse(uadatasets$uaerrmsgdis), collapse = " ")
			
			# Get the UA or Bsky sub functiona name to clean up the call stack
			
			if(uadatasets.sk$callStackIndex > 0)
			{
				for(k in  uadatasets.sk$callStackIndex:1)
				{				
					if(uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$FuntionCallMade)
					{	
						FuntionCallMadeStackIndex = k
					}
					
					if(uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$CallingFnNameOneLevelUp)
					{
						CallingFnNameOneLevelUpStackIndex = k
					}
					
					if(uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$NearestTryCatchFnName)
					{
						NearestTryCatchFnNameStackIndex = k
					}
				}
				
				if(FuntionCallMadeStackIndex >0)
				{
					appMsgStackIndex = FuntionCallMadeStackIndex
				}
				else if(CallingFnNameOneLevelUpStackIndex > 0)
				{
					appMsgStackIndex = CallingFnNameOneLevelUpStackIndex
				}
				else if(NearestTryCatchFnNameStackIndex > 0)
				{
					appMsgStackIndex = NearestTryCatchFnNameStackIndex 
				}
				else # this is just a safety net to catch all - it should not enter this else at all
				{
					appMsgStackIndex = NearestTryCatchFnNameStackIndex 
				}
				
				appMsgDisp = paste(deparse(uadatasets.sk$callStack[[appMsgStackIndex]]$BSkyErrMsg), collapse = " ")
				uadatasets.sk$callStack[[appMsgStackIndex]]$ErrFound=1
				uadatasets.sk$callStack[[NearestTryCatchFnNameStackIndex]]$ErrFound=1
			}
    
    		# As it can have several error messages on a stat function, the list of variables that generated the error, and the messages
    		# uadatasets$uaerrnvar =c(uadatasets$uaerrvar,uadatasets$uaTempErrvar)

			# Newly added on 05/27/13 to store all error messages euther from Uber function or any sub function
			# in an array with the uadataset global variable - this uadatset.sk will get reset once uber function
			# completes (uber function wrap up call
		    #uadatasets.sk$uberUaRerrmsg =c(uadatasets.sk$uberUaRerrmsg,uarerrmsg );
			
    		#Application level (not R system level) custom warning messages
    		uadatasets.sk$uberUaApperrmsgdis =c(uadatasets.sk$uberUaApperrmsgdis, appMsgDisp)
		}
		else
		{
			#cat ("I am in uber function error handler\n")
			
			#========================================================
			#Newly inroduced on 05/26/13 to keep track of errors generated in 
			#ubber function just like it has been used earlier in sub functions
			uadatasets$error =-1;
    		uadatasets$errorfn=-1
    
    		#keeps track of the total number of errors
    		uadatasets$errorindex =uadatasets$errorindex +1
    			
    		#uadatasets$uaerrmsgdis-This is a string
			#uadatasets$uarerrmsg -This is a string 
          
    		#Handling multiple R error msgs for stat functions that will iterate through different variables
    		# uadatasets$uarerrmsg =c(uadatasets$uarerrmsg,uarerrmsg);
    		uadatasets$uarerrmsg = uarerrmsg
			#========================================================
			
			if(uadatasets.sk$uaTempUberFunctionName ==functionCallDeails$FuntionCallMade || uadatasets.sk$uaTempUberFunctionName == functionCallDeails$NearestTryCatchFnName)
			{
				uadatasets.sk$totalUberErrors = uadatasets.sk$totalUberErrors+1
    		}
			
    		#appErrMsgDisp =   uadatasets.sk$uaTempErrmsgdis
			
			if(uadatasets.sk$callStackIndex > 0)
			{
				for(k in  uadatasets.sk$callStackIndex:1)
				{
					#cat("Printing uadatasets.sk$callStack[[k]]$FunctionName ", uadatasets.sk$callStack[[k]]$FunctionName, "and functionCallDeails$FuntionCallMade ", functionCallDeails$FuntionCallMade,"\n")
					
					#if(uadatasets.sk$callStack[[k]]$FunctionName == uadatasets.sk$uaTempUberFunctionName)
					#if(uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$FuntionCallMade || uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$CallingFnNameOneLevelUp || uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$NearestTryCatchFnName)
					#{
						
						#list(FunctionName = functionName, FunctionParam = callDetails$RuntimeParamValues, BSkyErrMsg="", BSkyWarnMsg="", ErrFound=0, WarnFound=0 )
						#appMsgDisp = paste(deparse(uadatasets.sk$callStack[[k]]$BSkyErrMsg), collapse = " ")
						#uadatasets.sk$callStack[[k]]$ErrFound=1
					#}
					
					#if(uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$NearestTryCatchFnName)
					#{
						
						#list(FunctionName = functionName, FunctionParam = callDetails$RuntimeParamValues, BSkyErrMsg="", BSkyWarnMsg="", ErrFound=0, WarnFound=0 )
						#appMsgDisp = paste(deparse(uadatasets.sk$callStack[[k]]$BSkyErrMsg), collapse = " ")
					#	uadatasets.sk$callStack[[k]]$ErrFound=1
					#	break
					#}
					
					for(k in  uadatasets.sk$callStackIndex:1)
					{				
						if(uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$FuntionCallMade)
						{	
							FuntionCallMadeStackIndex = k
						}
						
						if(uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$CallingFnNameOneLevelUp)
						{
							CallingFnNameOneLevelUpStackIndex = k
						}
						
						if(uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$NearestTryCatchFnName)
						{
							NearestTryCatchFnNameStackIndex = k
						}
					}
					
					if(FuntionCallMadeStackIndex >0)
					{
						appMsgStackIndex = FuntionCallMadeStackIndex
					}
					else if(CallingFnNameOneLevelUpStackIndex > 0)
					{
						appMsgStackIndex = CallingFnNameOneLevelUpStackIndex
					}
					else if(NearestTryCatchFnNameStackIndex > 0)
					{
						appMsgStackIndex = NearestTryCatchFnNameStackIndex 
					}
					else # this is just a safety net to catch all - it should not enter this else at all
					{
						appMsgStackIndex = NearestTryCatchFnNameStackIndex 
					}
					
					appMsgDisp = paste(deparse(uadatasets.sk$callStack[[appMsgStackIndex]]$BSkyErrMsg), collapse = " ")
					uadatasets.sk$callStack[[appMsgStackIndex]]$ErrFound=1
					uadatasets.sk$callStack[[NearestTryCatchFnNameStackIndex]]$ErrFound=1
				}
			}
			
    		#Application level (not R system level) custom warning messages
    		#uadatasets.sk$uberUaApperrmsgdis =c(uadatasets.sk$uberUaApperrmsgdis, uadatasets.sk$uaTempErrmsgdis)
			uadatasets.sk$uberUaApperrmsgdis =c(uadatasets.sk$uberUaApperrmsgdis, appMsgDisp)
			
			#Aaron, add your code here, use parameters to uawritelog
  			#Aaron, this code was added to accomodate the fact that a warning may have been generated before the return structure
  			#was created
  			#bskyNoofTables=length(uadatasets$retstructure)
  			#if (bskyNoofTables==0)
  			#{
			#	uadatasets$retstructure[[1]]<-list()
			#	#Aaron: We don't know if this is a table or graph, this will be filled in later
			#	#uadatasets$retstructure[[1]]$type="table"
			#	uadatasets$retstructure[[1]]$metadata="yes"
			#	#Aaron: We don't know if this is a return structure for a t.test or a crosstab, this will be filled in later
			#	#uadatasets$retstructure[[1]]$nometadatatables=1
			#	#uadatasets$retstructure[[1]]$metadatatabletype="normal"
			#	uadatasets$retstructure[[1]]$metadatatable=list()
			#	uadatasets$retstructure[[1]]$metadatatable[[1]]=data.frame(varIndex=NA,type=1,varName=NA,dataTableRow=NA,startCol=NA,endCol=NA,BSkyMsg=appErrMsgDisp,RMsg=uarerrmsg)					
  			#}
  			#else
  			#{
  			#	uadatasets$retstructure[[bskyNoofTables]]$metadatatable[[1]]=rbind(uadatasets$retstructure[[bskyNoofTables]]$metadatatable[[1]],data.frame(varIndex=NA,type=1,varName=NA,dataTableRow=NA,startCol=NA,endCol=NA,BSkyMsg=appErrMsgDisp,RMsg=uarerrmsg))
  			#}		
		}
		
		if (length (functionCallDeails$RuntimeParamValues)==0 || (length (functionCallDeails$RuntimeParamValues)== 1 && functionCallDeails$RuntimeParamValues[[1]]=="" )) 
		{
			functcommand =NA
			uarerrmsg = paste(uarerrmsg, " in function : ", functionCallDeails$FuntionCallMade,"()")
		}
		else 
		{
			functcommand =functionCallDeails$RuntimeParamValues
			uarerrmsg = paste(uarerrmsg, " in function: ", functcommand)
		}
		
		if(uadatasets.sk$callStackIndex >0 && FuntionCallMadeStackIndex == 0)
		{ 
			uarerrmsg = paste(uarerrmsg, " which is possibly called from function: ", uadatasets.sk$callStack[[uadatasets.sk$callStackIndex]]$FunctionParam)
		}
		
		#added on 05/27/13 to store all error messages euther from Uber function or any sub function
		# in an array with the uadataset global variable - this uadatset.sk will get reset once uber function
		# completes (uber function wrap up call
		    
		#Handling multiple R error msgs
		uadatasets.sk$uberUaRerrmsg =c(uadatasets.sk$uberUaRerrmsg,uarerrmsg );
		
		#clean up the stack from the bottom of the call stack till the nearest try-catch function
		if(uadatasets.sk$callStackIndex > 0)
		{
			for(k in  uadatasets.sk$callStackIndex:1)
			{
				#print( uadatasets.sk$callStack[[k]]$FunctionName )
				if(uadatasets.sk$callStack[[k]]$FunctionName == functionCallDeails$NearestTryCatchFnName)
				{
				  break
				}
				else
				{
					uadatasets.sk$callStack[k] <- NULL
					uadatasets.sk$callStackIndex = uadatasets.sk$callStackIndex - 1
				}
			}
		}
			
		#Writing the error to the log	
		# uawritelog(type="Error", functionname= uadatasets$uaTempFunctionName[uadatasets$callStackIndex], uberfunct=uadatasets$uaTempUberFunctionName, message =uarerrmsg, callfn =conditionCall(ex), uamessage =uadatasets$uaTempErrmsgdis);
		#Writing the error to the log	
		#uawritelog(type="Error", functionname= as.character(functionCallDeails$FuntionCallMade)[1] , uberfunct=uadatasets.sk$uaTempUberFunctionName, functcommand =functionCallDeails$RuntimeParamValues, message =uarerrmsg, uamessage =appErrMsgDisp);			
		#uawritelog(type="Error", functionname= functionCallDeails$FuntionCallMade , uberfunct=uadatasets.sk$uaTempUberFunctionName, functcommand =functionCallDeails$RuntimeParamValues, message =uarerrmsg, uamessage =appErrMsgDisp);			
		# uadatasets.sk$currentVarNamesAndFactorValues 
		# uadatasets.sk$currentDatasetname
		# uawritelog(type="Error", datasetName=uadatasets.sk$currentDatasetname, splitLevel=uadatasets.sk$currentVarNamesAndFactorValues,  functionName= functionCallDeails$FuntionCallMade , functionCommand =functionCallDeails$RuntimeParamValues, uberFunct=uadatasets.sk$uaTempUberFunctionName, RMessage =uarerrmsg, BSkyMessage =appErrMsgDisp, callfn = callSignature);
		# uawritelog(type="Error", datasetName=uadatasets.sk$currentDatasetname, splitLevel=uadatasets.sk$currentVarNamesAndFactorValues,  functionName= functionCallDeails$FuntionCallMade , functionCommand =functionCallDeails$RuntimeParamValues, uberFunct=uadatasets.sk$uaTempUberFunctionName, RMessage =uarerrmsg, BSkyMessage =appErrMsgDisp, nearestTryCatch = functionCallDeails$NearestTryCatchFnName, callingFnOneLevelUp = functionCallDeails$CallingFnName);
		#uawritelog(type="Error", datasetName=uadatasets.sk$currentDatasetname, splitLevel=uadatasets.sk$currentVarNamesAndFactorValues,  functionName= functionCallDeails$FuntionCallMade , functionCommand =functcommand, uberFunct=uadatasets.sk$uaTempUberFunctionName, RMessage =uarerrmsg, BSkyMessage =appErrMsgDisp, nearestTryCatch = functionCallDeails$NearestTryCatchFnName)
		uawritelog(type="Error", datasetName=uadatasets.sk$currentDatasetname, splitLevel=uadatasets.sk$currentVarNamesAndFactorValues,  functionName= functionCallDeails$FuntionCallMade , functionCommand =functcommand, uberFunct=uadatasets.sk$uaTempUberFunctionName, RMessage =uarerrmsg, BSkyMessage =appMsgDisp, nearestTryCatch = functionCallDeails$NearestTryCatchFnName, callingFnNameOneLevelUp = functionCallDeails$CallingFnNameOneLevelUp, runTimeParamValuesOneLevelUp = functionCallDeails$RuntimeParamValuesOneLevelUp, runTimeParamValuesTryCatchFunction = functionCallDeails$RunTimeParamValuesTryCatchFunction)
}


uaGlobalErrorFound <- function()
{
  		#if any error encountered in the Uber function with try catch block
  		#if(uadatasets.sk$totalErrors > 0)
  		if(uadatasets.sk$totalUberErrors > 0)
  		{
  		  return (TRUE)
  		}
  		else
      {
        #if no error encountered in the above try catch block
        return (FALSE)
      }
}

BSkyGlobalErrorFound <- function()
{
  		#if any error encountered in the Uber function with try catch block
  		
  		#if(uadatasets.sk$totalUberErrors > 0)
		if(uadatasets.sk$totalErrors > 0)
  		{
			return (TRUE)
  		}
  		else
        {
			#if no error encountered in the above try catch block
			return (FALSE)
        }
}
    
uaGlobalWarningFound <- function()
{
	  #if any warning encountered in the Uber function with try catch block
	  #if(uadatasets.sk$totalWarnings > 0)
	  if(uadatasets.sk$totalUberWarnings > 0)
	  {
		return (TRUE)
	  }
	  else
      {
        #if no warning encountered in the above try catch block
        return (FALSE)
      }
}

BSkyGlobalWarningFound <- function()
{
      #if any warning encountered in the Uber function with try catch block
      
      #if(uadatasets.sk$totalUberWarnings > 0)
	  if(uadatasets.sk$totalWarnings > 0)
  	  {
  		  return (TRUE)
  	  }
  	  else
      {
        #if no warning encountered in the above try catch block
        return (FALSE)
      }
}

uaErrorFound <- function()
{
  		#if any error encountered in the  try catch block
  		if(uadatasets$error == -1)
  		{
			return (TRUE)
  		}
  		else
		{
        #if no error encountered in the  try catch block
			return (FALSE)
		}
}
    
BSkyLocalErrorFound <- function()
{
		subFnName = ""
		# Get the UA or Bsky sub functiona name 
		fnCall<-sys.call(-1)
		
		if(!is.null(fnCall))
		{
			subFnName<-as.character(fnCall)[1]
			if(subFnName == "withCallingHandlers")
			{
				#for (i in 1:11)
				#{
				#	fnCall<-sys.call(-i)
				#	cat("iteration num: ",i,"\n")
				#	if(!is.null(fnCall))
				#	{
				#		print(subFnName<-as.character(fnCall)[1])
				#	}
				#}
				fnCall<-sys.call(-9)
				subFnName<-as.character(fnCall)[1]
			}
		}
		
		errFound = 0
		
		if(uadatasets.sk$callStackIndex > 0)
		{
			for(k in  uadatasets.sk$callStackIndex:1)
			{
				if(uadatasets.sk$callStack[[k]]$FunctionName == subFnName)
				{
					
					#list(FunctionName = functionName, FunctionParam = callDetails$RuntimeParamValues, BSkyErrMsg="", BSkyWarnMsg="", ErrFound=0, WarnFound=0 )
					errFound = uadatasets.sk$callStack[[k]]$ErrFound
					break
				}
			}
		}
  		#if any error encountered in the  try catch block
  		#if(uadatasets$error == -1)
		if(errFound == 1)
  		{
			return (TRUE)
  		}
  		else
		{
        #if no error encountered in the  try catch block
			return (FALSE)
		}
}

uaWarningFound <- function()
{
		#if any warning encountered in the try catch block
		if(uadatasets$warning == -1)
  		{
			return (TRUE)
  		}
  		else
		{
			#if no warning encountered in the try catch block
			return (FALSE)
		}
}

BSkyLocalWarningFound <- function()
{
		#if any warning encountered in the try catch block
		
		subFnName = ""
		
		# Get the UA or Bsky sub functiona name 
		fnCall<-sys.call(-1)
		
		if(!is.null(fnCall))
		{
			subFnName<-as.character(fnCall)[1]
			if(subFnName == "withCallingHandlers")
			{
				#for (i in 1:11)
				#{
				#	fnCall<-sys.call(-i)
				#	cat("iteration num: ",i,"\n")
				#	if(!is.null(fnCall))
				#	{
				#		print(subFnName<-as.character(fnCall)[1])
				#	}
				#}
				fnCall<-sys.call(-9)
				subFnName<-as.character(fnCall)[1]
			}
		}
		
		warnFound = 0
		
		if(uadatasets.sk$callStackIndex > 0)
		{
			for(k in  uadatasets.sk$callStackIndex:1)
			{
				if(uadatasets.sk$callStack[[k]]$FunctionName == subFnName)
				{
					
					#list(FunctionName = functionName, FunctionParam = callDetails$RuntimeParamValues, BSkyErrMsg="", BSkyWarnMsg="", ErrFound=0, WarnFound=0 )
					warnFound = uadatasets.sk$callStack[[k]]$WarnFound
					break
				}
			}
		}
  		#if any error encountered in the  try catch block
  		#if(uadatasets$error == -1)
		#if(uadatasets$warning == -1)
		if(warnFound == 1)
  		{
			return (TRUE)
  		}
  		else
		{
			#if no warning encountered in the try catch block
			return (FALSE)
		}
}

uaErrorFlagsReset<- function()
{
          uadatasets$errmsg = NULL
          uadatasets$error = 0
          uadatasets$errorindex = 0
          uadatasets$errorfn = 0
          invisible(TRUE)
}

uaWarningFlagsReset<- function()
{
          uadatasets$warnindex = 0
          uadatasets$warnmsg = NULL
          uadatasets$warning = 0
          uadatasets$uawarnfn = 0
          uadatasets$uawarnvar = NULL
          uadatasets$uawarnmsgdis = NULL
          uadatasets$uarwarnmsg=NULL
          
          invisible(TRUE)
}

BSkyLocalErrorFlagsReset<- function()
{
		subFnName = ""
		
		# Get the UA or Bsky sub functiona name 
		fnCall<-sys.call(-1)
		
		if(!is.null(fnCall))
		{
			subFnName<-as.character(fnCall)[1]
			if(subFnName == "withCallingHandlers")
			{
				#for (i in 1:11)
				#{
				#	fnCall<-sys.call(-i)
				#	cat("iteration num: ",i,"\n")
				#	if(!is.null(fnCall))
				#	{
				#		print(subFnName<-as.character(fnCall)[1])
				#	}
				#}
				fnCall<-sys.call(-9)
				subFnName<-as.character(fnCall)[1]
			}
		}
		
		if(uadatasets.sk$callStackIndex > 0)
		{
			for(k in  uadatasets.sk$callStackIndex:1)
			{
				if(uadatasets.sk$callStack[[k]]$FunctionName == subFnName)
				{
					
					#list(FunctionName = functionName, FunctionParam = callDetails$RuntimeParamValues, BSkyErrMsg="", BSkyWarnMsg="", ErrFound=0, WarnFound=0 )
					uadatasets.sk$callStack[[k]]$ErrFound = 0
					break
				}
			}
		}
          uadatasets$errmsg = NULL
          uadatasets$error = 0
          uadatasets$errorindex = 0
          uadatasets$errorfn = 0
          invisible(TRUE)
}

BSkyLocalWarningFlagsReset<- function()
{
		subFnName = ""

		# Get the UA or Bsky sub functiona name 
		fnCall<-sys.call(-1)

		if(!is.null(fnCall))
		{

			subFnName<-as.character(fnCall)[1]

			if(subFnName == "withCallingHandlers")
			{
				#for (i in 1:11)
				#{
				#	fnCall<-sys.call(-i)
				#	cat("iteration num: ",i,"\n")
				#	if(!is.null(fnCall))
				#	{
				#		print(subFnName<-as.character(fnCall)[1])
				#	}
				#}

				fnCall<-sys.call(-9)
				subFnName<-as.character(fnCall)[1]
			}
		}

		if(uadatasets.sk$callStackIndex > 0)
		{

			for(k in  uadatasets.sk$callStackIndex:1)
			{
				if(uadatasets.sk$callStack[[k]]$FunctionName == subFnName)
				{
					#list(FunctionName = functionName, FunctionParam = callDetails$RuntimeParamValues, BSkyErrMsg="", BSkyWarnMsg="", ErrFound=0, WarnFound=0 )
					uadatasets.sk$callStack[[k]]$WarnFound = 0

					break
				}
			}
		}

          uadatasets$warnindex = 0
          uadatasets$warnmsg = NULL
          uadatasets$warning = 0
          uadatasets$uawarnfn = 0
          uadatasets$uawarnvar = NULL
          uadatasets$uawarnmsgdis = NULL
          uadatasets$uarwarnmsg=NULL

          invisible(TRUE)
}
    
uaStoreApplicationWarnErrMsg <- function(BSkyErrMsg,BSkyWarnMsg)
{
    uadatasets.sk$uaTempWarnmsgdis = BSkyWarnMsg
    uadatasets.sk$uaTempErrmsgdis =  BSkyErrMsg
    #uadatasets.sk$uaTempWarnmsgdis = paste("Application Warn Msg : ", BSkyWarnMsg)
    #uadatasets.sk$uaTempErrmsgdis =  paste("Appilication Err Msg : ", BSkyErrMsg)
}	

BSkyStoreApplicationWarnErrMsg <- function(BSkyWarnMsg, BSkyErrMsg)
{
    uadatasets.sk$uaTempWarnmsgdis = BSkyWarnMsg
    uadatasets.sk$uaTempErrmsgdis =  BSkyErrMsg
    #uadatasets.sk$uaTempWarnmsgdis = paste("Application Warn Msg : ", BSkyWarnMsg)
    #uadatasets.sk$uaTempErrmsgdis =  paste("Appilication Err Msg : ", BSkyErrMsg)
	# Get the UA or Bsky sub functiona name to clean up the call stack
	# Get the UA or Bsky sub functiona name 
	fnCall<-sys.call(-1)
	
	if(!is.null(fnCall))
	{
		subFnName<-as.character(fnCall)[1]
		if(subFnName == "withCallingHandlers")
		{
			#for (i in 1:11)
			#{
			#	fnCall<-sys.call(-i)
			#	cat("iteration num: ",i,"\n")
			#	if(!is.null(fnCall))
			#	{
			#		print(subFnName<-as.character(fnCall)[1])
			#	}
			#	else
			#	{
			#		break
			#	}
			#}
			fnCall<-sys.call(-9)
			subFnName<-as.character(fnCall)[1]
		}
	}
	
	#cat("Printing callStack in BSkyStoreApplicationWarnErrMsg()\n")
	#print(uadatasets.sk$callStack)
	#cat("subFnName in BSkyStoreApplicationWarnErrMsg()",subFnName,"\n")
	
	if(uadatasets.sk$callStackIndex > 0)
	{
		for(k in  uadatasets.sk$callStackIndex:1)
		{
			if(uadatasets.sk$callStack[[k]]$FunctionName == subFnName)
			{
				
				#list(FunctionName = functionName, FunctionParam = callDetails$RuntimeParamValues, BSkyErrMsg="", BSkyWarnMsg="", ErrFound=0, WarnFound=0 )
				uadatasets.sk$callStack[[k]]$BSkyErrMsg = BSkyErrMsg
				uadatasets.sk$callStack[[k]]$BSkyWarnMsg = BSkyWarnMsg
				break
			}
		}
	}
}	

uaGetStoredApplicationWarnErrMsg <- function()
{
    return(c(uadatasets.sk$uaTempWarnmsgdis, uadatasets.sk$uaTempErrmsgdis)) 
}	

BSkyGetStoredApplicationWarnErrMsg <- function()
{
	#uadatasets.sk$uaTempWarnmsgdis = BSkyWarnMsg
    #uadatasets.sk$uaTempErrmsgdis =  BSkyErrMsg
    #uadatasets.sk$uaTempWarnmsgdis = paste("Application Warn Msg : ", BSkyWarnMsg)
    #uadatasets.sk$uaTempErrmsgdis =  paste("Appilication Err Msg : ", BSkyErrMsg)
	# Get the UA or Bsky sub functiona name to clean up the call stack
	# Get the UA or Bsky sub functiona name 
	fnCall<-sys.call(-1)
	
	if(!is.null(fnCall))
	{
		subFnName<-as.character(fnCall)[1]
		if(subFnName == "withCallingHandlers")
		{
			#for (i in 1:11)
			#{
			#	fnCall<-sys.call(-i)
			#	cat("iteration num: ",i,"\n")
			#	if(!is.null(fnCall))
			#	{
			#		print(subFnName<-as.character(fnCall)[1])
			#	}
			#}
			fnCall<-sys.call(-9)
			subFnName<-as.character(fnCall)[1]
		}
	}

	BSkyWarnMsg = ""
	BSkyErrMsg = ""
	
	if(uadatasets.sk$callStackIndex > 0)
	{
		for(k in  uadatasets.sk$callStackIndex:1)
		{
			if(uadatasets.sk$callStack[[k]]$FunctionName == subFnName)
			{
				
				#list(FunctionName = functionName, FunctionParam = callDetails$RuntimeParamValues, BSkyErrMsg="", BSkyWarnMsg="", ErrFound=0, WarnFound=0 )
				BSkyErrMsg = uadatasets.sk$callStack[[k]]$BSkyErrMsg
				BSkyWarnMsg = uadatasets.sk$callStack[[k]]$BSkyWarnMsg 
				break
			}
		}
	}
    return(c(BSkyWarnMsg, BSkyErrMsg)) 
}	

#===============================================================================================
#===============================================================================================
# SK - Global variable for testing need to me merged with uadataset global structure
	# uadatasets.sk <- NULL #commented by Anil. moved to init.r
	# the following global initialization need to be moved within uainit()
  # uadatasets.sk$uaTempUberFunctionName =""
  # uadatasets.sk$uaTempWarnmsgdis = ""
  # uadatasets.sk$uaTempErrmsgdis = ""
  # uadatasets.sk$splitIteration = 0
  # uadatasets.sk$rproc = ""
  # uadatasets.sk$callStack = NULL
  # uadatasets.sk$callStackIndex = 0
  # uadatasets.sk$callStackIndex = 0
  # uadatasets.sk$totalWarnings = 0
  # uadatasets.sk$totalUberWarnings = 0
  # uadatasets.sk$uberUaRwarnmsg = NULL
  # uadatasets.sk$uberUaAppwarnmsgdis = NULL
  # uadatasets.sk$totalErrors = 0
  # uadatasets.sk$totalUberErrors = 0
  # uadatasets.sk$uberUaRerrmsg = NULL
  # uadatasets.sk$uberUaApperrmsgdis = NULL
  # uadatasets.sk$currentVarNamesAndFactorValues = ""
  # uadatasets.sk$currentDatasetname = ""
  # uadatasets$retstructure <-list(NULL)
#===============================================================================================
#===============================================================================================
 ## Commented by Anil duplicate in init.R
# .onLoad<-function(libname, pkgname)
# {
 # #cat("Initializing the package in .onLoad function\n")
 # #print(libname)
 # #print(pkgname)

 # uadatasets.sk <<- new.env(parent=environment(.onLoad))
# }


	
# To be called only once from the uber UA function
# This function should not be called form any sub function that uber UA function may call
uaGlobalInit <- function()
{
	callDetails = uaLogFunArgsDetails()
 	#print( callDetails )
    	
	#uadatasets.sk <<- new.env(parent=globalenv())#
	#uadatasets.sk <<- new.env(parent=environment(.onLoad)  )
	
	###Functions to call at start
	uainit()
	
	uadatasets$uawarnmsgdis = ""
	uadatasets$uaerrmsgdis = ""
	
	uadatasets$retstructure <-list()
	
	# the following global initialization need to be moved within uainit()
	  uadatasets.sk$uaTempUberFunctionName =""
	  uadatasets.sk$uaTempWarnmsgdis = ""
	  uadatasets.sk$uaTempErrmsgdis = ""
	  uadatasets$uasplitinfo = NULL 
	  uadatasets.sk$splitIteration = 0
	  uadatasets.sk$uaSplitDatasetIndex = 0
	  bskyCurrentDatasetSplitSliceObj <<- NULL
	  uadatasets$index = 1
	  uadatasets.sk$rproc = ""
	  uadatasets.sk$callStack = NULL
	  uadatasets.sk$callStackIndex = 0
	  
	  uadatasets.sk$totalWarnings = 0
	  uadatasets.sk$totalUberWarnings = 0
	  uadatasets.sk$uberUaRwarnmsg = NULL
	  uadatasets.sk$uberUaAppwarnmsgdis = NULL
	  
	  uadatasets.sk$totalErrors = 0
	  uadatasets.sk$totalUberErrors = 0
	  uadatasets.sk$uberUaRerrmsg = NULL
	  uadatasets.sk$uberUaApperrmsgdis = NULL
	  
	  uadatasets.sk$currentVarNamesAndFactorValues = ""
	  uadatasets.sk$currentDatasetname = ""
  

	# print(callDetails$RuntimeParamValues)
	#The function below captures the complete call to the function
	#It must be stored in a global so that the function that creates the notes item, uaprocdesc can access it
	uadatasets.sk$rproc =callDetails$RuntimeParamValues
	# print(uadatasets.sk$rproc)
	
	uadatasets.sk$uaTempUberFunctionName =  as.character(callDetails$FuntionCallMade)[1]
	
	uastartlog(uadatasets.sk$uaTempUberFunctionName, uadatasets.sk$uaTempUberFunctionName)
	#cat("logindex1 :", uadatasets$logindex, "\n");
	
	uadatasets.sk$callStackIndex = uadatasets.sk$callStackIndex + 1
	funcCallDetails =   list(FunctionName = uadatasets.sk$uaTempUberFunctionName, FunctionParam = callDetails$RuntimeParamValues)
	uadatasets.sk$callStack = c(uadatasets.sk$callStack, list(funcCallDetails))
}

# To be called only once and be to be called at the first executable statement withing any 
# UA or BSky function
BSkyFunctionInit <- function()
{
	callDetails = uaLogFunArgsDetails()
 	#print( callDetails )
    
	#for (i in 1:20)
	#{
	#	fnCall<-sys.call(-i)
	#	cat("iteration num: ",i,"\n")
	#	if(!is.null(fnCall))
	#	{
	#		print(topLevelFnName<-as.character(fnCall)[1])
	#	}
	#	else
	#	{
	#		break
	#	}
	#}
	#uadatasets.sk <<- new.env(parent=globalenv())#
	#uadatasets.sk <<- new.env(parent=environment(.onLoad)  )
	
	topLevelFnName = ""
	fnCall<-sys.call(-2)
	
	if(!is.null(fnCall))
	{
		topLevelFnName<-as.character(fnCall)[1]
		if(topLevelFnName == "withCallingHandlers")
		{
			#for (i in 1:11)
			#{
			#	fnCall<-sys.call(-i)
			#	cat("iteration num: ",i,"\n")
			#	if(!is.null(fnCall))
			#	{
			#		print(topLevelFnName<-as.character(fnCall)[1])
			#	}
			#}
			fnCall<-sys.call(-10)
			topLevelFnName<-as.character(fnCall)[1]
		}
	}
	
	#if(is.null(fnCall) || (uadatasets.sk$callStackIndex < 1) ||(is.na(pmatch("UA",topLevelFnName)) && is.na(pmatch("BSky",topLevelFnName))))
	if( !exists("callStackIndex", envir=uadatasets.sk) || (uadatasets.sk$callStackIndex < 1)) ##14Jul2013 if(is.null(fnCall) || (uadatasets.sk$callStackIndex < 1)) 
	{
		#currentFnCall<-sys.call()
		#currentTopLevelFnName<-as.character(fnCall)[1]
		currentTopLevelFnName<-as.character(callDetails$FuntionCallMade)[1]
		#calling function is the uber UA or Bsky function
		#cat("In top level function ", currentTopLevelFnName, " function init \n")
	
		###Functions to call at start
		uainit()
		
		uadatasets$uawarnmsgdis = ""
		uadatasets$uaerrmsgdis = ""
	
		uadatasets$retstructure <-list()
	
		# the following global initialization need to be moved within uainit()
		
		uadatasets.sk$BSkyBatchCommandModeON = FALSE
		
		uadatasets.sk$uaTempUberFunctionName =""
		uadatasets.sk$uaTempWarnmsgdis = ""
		uadatasets.sk$uaTempErrmsgdis = ""
		uadatasets$uasplitinfo = NULL 
		uadatasets.sk$splitIteration = 0
		uadatasets.sk$uaSplitDatasetIndex = 0
		bskyCurrentDatasetSplitSliceObj <<- NULL
		uadatasets$index = 1
		uadatasets.sk$rproc = ""
		uadatasets.sk$callStack = NULL
		uadatasets.sk$callStackIndex = 0
	  
		uadatasets.sk$totalWarnings = 0
		uadatasets.sk$totalUberWarnings = 0
		uadatasets.sk$uberUaRwarnmsg = NULL
		uadatasets.sk$uberUaAppwarnmsgdis = NULL
	  
		uadatasets.sk$totalErrors = 0
		uadatasets.sk$totalUberErrors = 0
		uadatasets.sk$uberUaRerrmsg = NULL
		uadatasets.sk$uberUaApperrmsgdis = NULL
	  
		uadatasets.sk$currentVarNamesAndFactorValues = ""
		uadatasets.sk$currentDatasetname = ""
		
		# Newly added (6/9/13) to store intermediate results tables generated by analytical function
		# in each data split iteration
		uadatasets.sk$uaStatResults = list()
	
		if(currentTopLevelFnName != "BSkyBatchCommand")
		{
			uadatasets.sk$uaTempUberFunctionName =  currentTopLevelFnName
			# print(callDetails$RuntimeParamValues)
			#The function below captures the complete call to the function
			#It must be stored in a global so that the function that creates the notes item, uaprocdesc can access it
			uadatasets.sk$rproc =callDetails$RuntimeParamValues
			# print(uadatasets.sk$rproc)
		
			uastartlog(uadatasets.sk$uaTempUberFunctionName, uadatasets.sk$uaTempUberFunctionName)
			#cat("logindex1 :", uadatasets$logindex, "\n");
		}
	
		uadatasets.sk$callStackIndex = uadatasets.sk$callStackIndex + 1
		#funcCallDetails =   list(FunctionName = uadatasets.sk$uaTempUberFunctionName, FunctionParam = callDetails$RuntimeParamValues)
		funcCallDetails =   list(FunctionName = currentTopLevelFnName, FunctionParam = callDetails$RuntimeParamValues, BSkyErrMsg="", BSkyWarnMsg="", ErrFound=0, WarnFound=0 )
		uadatasets.sk$callStack = c(uadatasets.sk$callStack, list(funcCallDetails))
	}
	else
	{
		#calling function a sub UA and Bsky function - not the ubber UA or Bsky function
		#print( callDetails )
		#currentSubLevelFnName<-as.character(callDetails$FuntionCallMade)[1]
		#parent function is an UA or Bsky function
		#cat("In sub function level function ", currentSubLevelFnName, " function init \n")
	
		functionName =  as.character(callDetails$FuntionCallMade)[1]
		
		if((uadatasets.sk$callStackIndex == 1) && BSkyIsInBatchCommandMode() == TRUE)
		{
			#uadatasets$uawarnmsgdis = ""
			#uadatasets$uaerrmsgdis = ""
		
			#uadatasets$retstructure <-list()
		
			# the following global initialization need to be moved within uainit()
			#uadatasets.sk$uaTempUberFunctionName =""
			#uadatasets.sk$uaTempWarnmsgdis = ""
			#uadatasets.sk$uaTempErrmsgdis = ""
			#uadatasets$uasplitinfo = NULL 
			#uadatasets.sk$splitIteration = 0
			#uadatasets.sk$uaSplitDatasetIndex = 0
			#uadatasets$index = 1
			#uadatasets.sk$rproc = ""
			#uadatasets.sk$callStack = NULL
			#uadatasets.sk$callStackIndex = 0
		  
			#uadatasets.sk$totalWarnings = 0
			#uadatasets.sk$totalUberWarnings = 0
			#uadatasets.sk$uberUaRwarnmsg = NULL
			#uadatasets.sk$uberUaAppwarnmsgdis = NULL
		  
			#uadatasets.sk$totalErrors = 0
			#uadatasets.sk$totalUberErrors = 0
			#uadatasets.sk$uberUaRerrmsg = NULL
			#uadatasets.sk$uberUaApperrmsgdis = NULL
		  
			#uadatasets.sk$currentVarNamesAndFactorValues = ""
			#uadatasets.sk$currentDatasetname = ""
			
			# Newly added (6/9/13) to store intermediate results tables generated by analytical function
			# in each data split iteration
			#uadatasets.sk$uaStatResults = list()
			
			BSkyBatchFunctionModeResetForUberFunction()
			
			uadatasets.sk$uaTempUberFunctionName = functionName
			
			#print(callDetails$RuntimeParamValues)
			#The function below captures the complete call to the function
			#It must be stored in a global so that the function that creates the notes item, uaprocdesc can access it
			uadatasets.sk$rproc =callDetails$RuntimeParamValues
			# print(uadatasets.sk$rproc)
		
			uastartlog(uadatasets.sk$uaTempUberFunctionName, uadatasets.sk$uaTempUberFunctionName)
			#cat("logindex1 :", uadatasets$logindex, "\n");
		}
		else
		{
			if (uaperformance ==2)
			{
				uastartlog(functionName, uadatasets.sk$uaTempUberFunctionName)
			}
			
			# reset warning and errors flags used for keeping track of sub function level error and warnings
			# These are not for the error and warnings tracking for the Uber/Top level function 
			#uaErrorFlagsReset()
			#uaWarningFlagsReset()
			#BSkyLocalWarningFlagsReset()
			#BSkyLocalErrorFlagsReset()
		}
	
		uadatasets.sk$callStackIndex = uadatasets.sk$callStackIndex + 1
		#funcCallDetails =   list(FunctionName = functionName, FunctionParam = callDetails$RuntimeParamValues)
		funcCallDetails =   list(FunctionName = functionName, FunctionParam = callDetails$RuntimeParamValues, BSkyErrMsg="", BSkyWarnMsg="", ErrFound=0, WarnFound=0 )
		uadatasets.sk$callStack = c(uadatasets.sk$callStack, list(funcCallDetails))
	}
}

BSkyBatchFunctionModeResetForUberFunction <- function()
{
	
		###Functions to call at start
	if(exists("temp", envir=uadatasets)) uadatasets$temp=NULL
	# rm(temp,envir=uadatasets)
	if(exists("temppairs",envir=uadatasets)) uadatasets$temppairs=NULL
	#rm(temppairs,envir =uadatasets)
	if(exists("ostarttime", envir=uadatasets))uadatasets$ostarttime =NULL
	if(exists("starttime", envir=uadatasets))uadatasets$starttime=NULL
	if(exists("funtname", envir=uadatasets))uadatasets$funtname=NULL
	if(exists("initialmem", envir=uadatasets))uadatasets$initialmem=NULL
	#if(exists("totaltime", envir=uadatasets))uadatasets$totaltime=NULL
	if(exists("totalcputime", envir=uadatasets))uadatasets$totalcputime=NULL
	if(exists("elapsedtime", envir=uadatasets))uadatasets$elapsedtime=NULL
	if(exists("warning", envir=uadatasets))uadatasets$warning=0
	if(exists("error", envir=uadatasets))uadatasets$error=0	
	if(exists("warnindex", envir=uadatasets))uadatasets$warnindex=0	
	if(exists("warnindex", envir=uadatasets))uadatasets$uawarnfn=0	
	if(exists("warnmsg", envir=uadatasets))uadatasets$warnmsg=NULL
	if(exists("errmsg", envir=uadatasets))uadatasets$errmsg=NULL
	if(exists("logindex", envir=uadatasets))uadatasets$logindex=1
	if(exists("df", envir=ualog))ualog$df=NULL
	if(exists("errorindex", envir=uadatasets))uadatasets$errorindex=0
# if(exists("split", envir=uadatasets))uadatasets$split=NULL
	#Globals for simplifying the try catch loop
	if(exists("functname"))uadatasets$functname=NULL
	if(exists("uberfunct"))uadatasets$uberfunct=NULL
	if(exists("uaerrmsgdis"))uadatasets$uaerrmsgdis=NULL
	if(exists("uawarnmsgdis"))uadatasets$uawarnmsgdis=NULL
	if(exists("uavar"))uadatasets$uavar=NULL
	
		uadatasets$uawarnmsgdis = ""
		uadatasets$uaerrmsgdis = ""
		
		uadatasets$retstructure <- NULL
		uadatasets$retstructure <-list()
	
		# the following global initialization need to be moved within uainit()
		#uadatasets.sk$uaTempUberFunctionName =""
		uadatasets.sk$uaTempWarnmsgdis = ""
		uadatasets.sk$uaTempErrmsgdis = ""
# uadatasets$uasplitinfo = NULL 
# uadatasets.sk$splitIteration = 0
# uadatasets.sk$uaSplitDatasetIndex = 0
		uadatasets$index = 1
		uadatasets.sk$rproc = ""
		# uadatasets.sk$callStack = NULL
		# uadatasets.sk$callStackIndex = 0
	  
		uadatasets.sk$totalWarnings = 0
		uadatasets.sk$totalUberWarnings = 0
		uadatasets.sk$uberUaRwarnmsg = NULL
		uadatasets.sk$uberUaAppwarnmsgdis = NULL
	  
		uadatasets.sk$totalErrors = 0
		uadatasets.sk$totalUberErrors = 0
		uadatasets.sk$uberUaRerrmsg = NULL
		uadatasets.sk$uberUaApperrmsgdis = NULL
	  
# uadatasets.sk$currentVarNamesAndFactorValues = ""
		# uadatasets.sk$currentDatasetname = ""
		
		# Newly added (6/9/13) to store intermediate results tables generated by analytical function
		# in each data split iteration
		uadatasets.sk$uaStatResults = NULL
		uadatasets.sk$uaStatResults = list()
		
		if(BSkyIsInBatchCommandMode() == FALSE)
		{
			uadatasets.sk$callStack <- NULL
			uadatasets.sk$callStackIndex = 0
		}
}

###Function to call at end
#uaGlobalFunctionWrapUp <- function(callDetails, datasetname, missing)
uaGlobalFunctionWrapUp <- function()
{
    ## sanjay for dedugging only
    #cat("logindex2 :", uadatasets$logindex, "\n");
    while(uadatasets$logindex > 2 )
    {
      ualogcommand()
    }
    #cat("logindex3 :", uadatasets$logindex, "\n");
    
	ualogcommand()
	
    datasetname = uaGetCurrentDatabaseName()
	#cat("DS name:",datasetname)
	
	if(datasetname=="") ## Added by anil
	{
		datasetname = uadatasets$name[length(uadatasets$name)]
		#cat("DS name 2 :",datasetname)
	}
	
    index=which(datasetname==uadatasets$name)
	
	#cat("Step1:")
	uadatasets$uasummary =uaprocdesc(index,missing)
	
	#cat("Step2:")
	ualog$df = uaretlog(uadatasets.sk$uaTempUberFunctionName)
	
	#cat("Step3:")
	if(uadatasets.sk$uaSplitDatasetIndex != 0)
	{
	  # Clean up the last split dataset slice left behind by last split iteration
	  uadatasets$lst[[uadatasets.sk$uaSplitDatasetIndex]] <- NULL
	  bskyCurrentDatasetSplitSliceObj <<- NULL
	}
	
	#cat("Step4:")
	#functionName =  as.character(callDetails$FuntionCallMade)[1]
	functionName =  uadatasets.sk$uaTempUberFunctionName
	#cat("Step4b:")
	for(k in  uadatasets.sk$callStackIndex:1)
	{     
        if(k > 1)
        {  
		      cat("callstack index :", k, "\n")
  		      print(uadatasets.sk$callStack[[k]]$FunctionName)
        }
	}
	uadatasets.sk$callStack <- NULL
	#cat("Step5:")
}

###Function to call at the end of any UA or BSky function
#uaGlobalFunctionWrapUp <- function(callDetails, datasetname, missing)
BSkyFunctionWrapUp <- function()
{
	# SK modified on 06/13/15
	
	topLevelFnName = ""
	fnCall<-sys.call(-1)
	
	if(!is.null(fnCall))
	{
		topLevelFnName<-as.character(fnCall)[1]
		if(topLevelFnName == "withCallingHandlers")
		{
			#for (i in 1:15)
			#{
			#	fnCall<-sys.call(-i)
			#	cat("iteration num: ",i,"\n")
			#	if(!is.null(fnCall))
			#	{
			#		print(topLevelFnName<-as.character(fnCall)[1])
			#	}
			#}
			fnCall<-sys.call(-10)
			topLevelFnName<-as.character(fnCall)[1]
		}
	}
	
	#if(is.null(fnCall) || ((is.na(pmatch("UA",topLevelFnName)) && is.na(pmatch("BSky",topLevelFnName))))
	if(is.null(fnCall) || topLevelFnName == uadatasets.sk$uaTempUberFunctionName || topLevelFnName == "BSkyBatchCommand")
	{
		if(is.null(fnCall) || topLevelFnName != "BSkyBatchCommand")
		{
			#calling function is the uber UA or Bsky function
			#cat("In top level function ", topLevelFnName, " function wrap up \n")
    
			## sanjay for dedugging only
			#cat("logindex2 :", uadatasets$logindex, "\n");
			while(uadatasets$logindex > 2 )
			{
				ualogcommand()
			}
			# cat("logindex3 :", uadatasets$logindex, "\n");
    
			if(uadatasets$logindex > 1)
			{
				ualogcommand()
			}
	
			datasetname = uaGetCurrentDatabaseName()
			#cat("DS name:",datasetname)
	
			if(datasetname=="") ## Added by anil
			{
				#datasetname = uadatasets$name[length(uadatasets$name)]
				datasetname = c("defualt") # SK added on 6/13/15
				#cat("DS name 2 :",datasetname)
			}
	
			if(!is.null(uadatasets$name) && length(uadatasets$name) > 0) # SK modified on 6/13/15
			{
				index=which(datasetname==uadatasets$name)
				#cat("0DS index :",index)
			}
			else
			{
				index = c()
				#cat("1DS index  :",index)
			}
	
			#cat("Step1:",index)
			if(length(index) > 0)
			{
				#cat("Step1b:")
				# Sanjay - what should be the value of missing - check later
				#uadatasets$uasummary =uaprocdesc(index,missing = 0)  ##ERROR here Error in uadatasets$lst[[index]] : subscript out of bounds
				uadatasets$uasummary =uaprocdesc(datasetname,missing = 0)
			}
			else
			{
				uadatasets$uasummary = list()
			}
	
			#cat("Step2:")
			if(!is.null(ualog$df) && nrow(ualog$df) > 0)
			{
				ualog$df = uaretlog(uadatasets.sk$uaTempUberFunctionName)
			}
			else
			{
				ualog$df = data.frame()
			}
			
			#cat("Step4:")
			#functionName =  as.character(callDetails$FuntionCallMade)[1]
			functionName =  uadatasets.sk$uaTempUberFunctionName
		}
		else if(is.null(fnCall) || topLevelFnName == "BSkyBatchCommand")
		{
			#cat("Step3:")
			if(uadatasets.sk$uaSplitDatasetIndex != 0)
			{
			#cat("Step3:a")
				# Clean up the last split dataset slice left behind by last split iteration
				#Sanjay - looks like no need to change
				uadatasets$lst[[uadatasets.sk$uaSplitDatasetIndex]] <- NULL
				bskyCurrentDatasetSplitSliceObj <<- NULL
			}
			
			#cat("Step4:")
			#functionName =  as.character(callDetails$FuntionCallMade)[1]
			functionName =  "BSkyBatchCommand"
		}
		
		#cat("Step4b:")
		if(uadatasets.sk$callStackIndex > 0)
		{
			for(k in  uadatasets.sk$callStackIndex:1)
			{
				if(uadatasets.sk$callStack[[k]]$FunctionName == functionName)
				{
					if( k < uadatasets.sk$callStackIndex )
					{
						#cat("Warning: 1. Leftover callstack due to one or more sub function not having try-catch block - current loc :, callstack index :", k, uadatasets.sk$callStackIndex, "\n")
						cat("Warning 1: Cleaning up the call stack.")
					}
					
					# clean up the stack for those sub function that did not call the uaFunctionWrapUp() 
					for(j in uadatasets.sk$callStackIndex:k)
					{
					  uadatasets.sk$callStack[[j]] <- NULL
					  uadatasets.sk$callStackIndex = uadatasets.sk$callStackIndex - 1
					}
					break
				}
			}
			
			if(uadatasets.sk$callStackIndex > 1)
			{
				#cat("Warning: 2. Leftover callstack due to one or more sub function not having try-catch block - callstack index :", uadatasets.sk$callStackIndex, "\n")
				cat("Warning 2: Cleaning up the call stack.")
				for(k in  uadatasets.sk$callStackIndex:1)
				{
				  print(uadatasets.sk$callStack[[k]]$FunctionName)
				}
			}
		}
		
		if(is.null(fnCall) || topLevelFnName == "BSkyBatchCommand" || BSkyIsInBatchCommandMode() == FALSE)
		{
			uadatasets.sk$callStack <- NULL
			uadatasets.sk$callStackIndex = 0
			# cat("Step5:")
		}
	}
	else
	{
		# Get the UA or Bsky sub functiona name to clean up the call stack
		fnCall<-sys.call(-1)
		subFnName<-as.character(fnCall)[1]
		
		#cat("In sub function ", subFnName, " function wrap up \n")
		#index=which(datasetname==uadatasets$name)
    
		if (uaperformance ==2)
		{
			ualogcommand()
		}
		# uadatasets$uasummary =uaprocdesc(index,missing)
		# ualog=uaretlog(uadatasets.sk$uaTempUberFunctionName)
		
		#functionName =  as.character(callDetails$FuntionCallMade)[1]
		
		
		if(uadatasets.sk$callStackIndex > 0)
		{
			for(k in  uadatasets.sk$callStackIndex:1)
			{
				if(uadatasets.sk$callStack[[k]]$FunctionName == subFnName)
				{
					# clean up the stack for those sub function that did not call the uaFunctionWrapUp() 
					for(j in uadatasets.sk$callStackIndex:k)
					{
					  uadatasets.sk$callStack[[j]] <- NULL
					  uadatasets.sk$callStackIndex = uadatasets.sk$callStackIndex - 1
					}
					break
				}
			}
		}
		
		# Newly added (6/9/13) to store intermediate results tables generated by analytical function
		# in each data split iteration
		##uadatasets.sk$uaStatResults <- NULL
	
		# reset warning and errors flags used for keeping track of sub function level error and warnings
		# These are not for the error and warnings tracking for the Uber/Top level function 
		#uaErrorFlagsReset()
		#uaWarningFlagsReset()
		BSkyLocalWarningFlagsReset()
		BSkyLocalErrorFlagsReset()
	}
}


BSkyBuildReturnTableStructure <- function(vars=c(), datasetname="", OutputDataTableListIfPassed=NA) ## vars and datasetname should be optional. Later we will see
{
		#used to track the number of tables returned
		#nooftablestodis =0
      
		#uadatasets$retstructure <-list()
     
		# To build up the list of stat result tables returned by ua sub function                  
    	#uaStatResults =list()
		
		#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
		# Fill out the uamat and datatable for each split iteration in the return result section
		#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
		# for testing only
		#uatemp = list(NULL)
		#uatemp[[1]]$type="table"
		#numoftablestructures =  length(uatemp)
		
		#uaSplitIterationCounter = uadatasets.sk$splitIteration
  
		if(is.na(OutputDataTableListIfPassed))
		{
			numoftablestructures =  length(uadatasets$retstructure)
		}
		else
		{
			numoftablestructures = length(OutputDataTableListIfPassed)
		}
		
		# cat("\n No. of Tables = ", numoftablestructures,"\t\t Vars =",vars)
		# print(uadatasets$retstructure)
		# cat("\n###########################\n")
		if(numoftablestructures > 0 )
		{
			for (k in 1:numoftablestructures)
			{
				if(is.na(OutputDataTableListIfPassed))
				{
					uadatasets$retstructure[[k]]$cartlevel = BSkyComputeCurrentVarNamesAndFactorValues(datasetname)
					uadatasets$retstructure[[k]]$varindex = BSkygetIndexesOfCols(vars, datasetname)
					uadatasets.sk$uaStatResults <- c(uadatasets.sk$uaStatResults, list(uadatasets$retstructure[[k]]))
						# cat("\nNo.",k,"**********************\n")
						# print(uadatasets.sk$uaStatResults)
						# cat("\nNo.",k,"*********Ends*************\n")
				}
				else
				{
				# 'table' was getting converted to list. So we must not modify OutputDataTableListIfPassed, in any way
				#Anil Commented this OutputDataTableListIfPassed[[k]]$cartlevel = BSkyComputeCurrentVarNamesAndFactorValues(datasetname)
				#20Aep2013 OutputDataTableListIfPassed[[k]]$varindex = UAgetIndexesOfCols(vars, datasetname)
				uadatasets.sk$uaStatResults <- c(uadatasets.sk$uaStatResults, list(OutputDataTableListIfPassed[[k]]))
				}
			
			#This variable calculates the number of tables as there could be empty datasets for splits 
			#where there is no data. In the case of an empty dataset, we are only returning a single 
			#table per split
			#nooftablestodis=nooftablestodis+1

			}
		}
		# cat("\nNo.",k,"******Outside*****Final***********\n")
		# print(uadatasets.sk$uaStatResults)
		# cat("\n*********Final ends*************\n")
		if(is.na(OutputDataTableListIfPassed))
		{
			#reinitializing the return structure to preapare for the next split iteration if there is a split
			uadatasets$retstructure <-list() 
		}
}


###22Sep20913 Modify this code to put ewtables in last. 
###25Apr2021 Error warning printing to the sink file
BSkyReturnStructure <-function(bskyAdditionalTableList = NA) ### passs as list(value)
{

	#print(" SK 2 - I am in BSkyReturnStructure")
	
### Common Errors & Warning ) #### Same as Aaron's ####
# -1 Error
# -2 No need to code (Critical Error)
# 1 Warning

	## The following is just as notes for now.....
	
  	#The 1st return value tells us whether there is a split or not (1=Split, 0=No split)
  
  	#The 2nd return value tells us whether there is an error or not (-2 if there is an error, -1 for warning, and 0 if there is no issue)
	#(11Aug2019 : we changed the code to 1 for warning)
  
  	#The 3rd return value gives us the number of errors
  
  	#The 4th return value tells us how many warnings there are
  
  	#The 5th return value gives us all the log values (This is a dataframe)
  
  	#The 6th return value gives us the summary of the function call with the actual parameter valuse passed
  
    #The 7th return value tells us now many tables we are going to display in the output
  
  	#8th position onward in case there is al least table output to be returned
  	
	nooftablestodis = 0
	
  	retlist = list()
    retlist = c( retlist, list(BSkySplit= (if(!is.null(uadatasets$uasplitinfo)) uadatasets$uasplitinfo[[1]] else 0 ))) #BSkySplit in place  of split
    retlist = c( retlist, list(executionstatus= (if(uadatasets.sk$totalErrors > 0) -2 else if(uadatasets.sk$totalWarnings > 0) 1 else 0 )))
    retlist = c( retlist, list(nooferrors=uadatasets.sk$totalErrors ))
    retlist = c( retlist, list(noofwarnings=uadatasets.sk$totalWarnings ))
    retlist = c( retlist, list(log=ualog$df ))
    retlist = c( retlist, list(uasummary = uadatasets$uasummary ))
	
	#print(ualog$df)
	totalRowsinLogTable = nrow(ualog$df)
	
	retstructure = list()
	uaStatTablesAndErrWarnTable = list()
	bskyNoofTables = 0
	bskyNoofStatTables = 0
	bskyNoofAdditionalTables = 0
	isErrorWarning = FALSE #Anil
	# cat("\nNoof rows:: ",totalRowsinLogTable,"\n")
	for(i in 1: totalRowsinLogTable)
	{
		currentLogRow <- ualog$df[i,]
		
		if(currentLogRow$type=="Error" ||  currentLogRow$type=="Warning")
		{
			isErrorWarning = TRUE
			# cat("\nError in Ret Stru\n")
			#ualog$df structure/column names example
			#type="Warning", or "Error" ,
			#datasetName=uadatasets.sk$currentDatasetname, 
			#splitLevel=uadatasets.sk$currentVarNamesAndFactorValues,  
			#functionName= functionCallDeails$FuntionCallMade , 
			#functionCommand =functcommand, 
			#uberFunct=uadatasets.sk$uaTempUberFunctionName, 
			#RMessage =uarwarnmsg, 
			#BSkyMessage =appMsgDisp, 
			#callingFnNameOneLevelUp = functionCallDeails$CallingFnNameOneLevelUp, 
			#runTimeParamValuesOneLevelUp = functionCallDeails$RuntimeParamValuesOneLevelUp, 
			#nearestTryCatch = functionCallDeails$NearestTryCatchFnName, 
			#runTimeParamValuesTryCatchFunction = functionCallDeails$RunTimeParamValuesTryCatchFunction)
			
		
			offendingFuncName <- as.character(currentLogRow$functionName)
			offendingFuncParam <- as.character(currentLogRow$functionCommand)
			RMsg <- as.character(currentLogRow$RMessage)
			BSkyAppMsg <- as.character(currentLogRow$BSkyMessage)
			uberFunctionName <- uaGetUberFunctionName()
			uberFunctionParam <- uaGetUberFunctionParam() 
    
			if(currentLogRow$type=="Error")
			{ 
				# severityType = -2
				severityType = -1 #Error
			}
			else
			{
				# severityType = -1
				severityType = 1 #Warnings
			}
			
	
			#bskyNoofTables=length(uadatasets$retstructure)
			
			if (bskyNoofTables==0)
			{
				#uadatasets$retstructure[[1]]<-list()
				#uadatasets$retstructure[[1]]$type="table"
				#uadatasets$retstructure[[1]]$metadata="yes"
				#uadatasets$retstructure[[1]]$nometadatatables=1
				#uadatasets$retstructure[[1]]$metadatatabletype="normal"
				#uadatasets$retstructure[[1]]$metadatatable=list()
				#uadatasets$retstructure[[1]]$metadatatable[[1]]=data.frame(varIndex=NA,severityType=-2,varName=NA,dataTableRow=NA,startCol=NA,endCol=NA,BSkyMsg=BSkyAppMsg,RMsg=RMsg)
			
				retstructure[[1]]<-list()
				retstructure[[1]]$type="ewtable"
				retstructure[[1]]$metadata="yes"
				retstructure[[1]]$nometadatatables=1
				retstructure[[1]]$metadatatabletype="normal"
				retstructure[[1]]$metadatatable=list()
				#cat("printing in reststructure 1 construction in BSkyReturnStructure() \n")
				retstructure[[1]]$metadatatable[[1]]=data.frame(varIndex=NA,severityType=severityType,varName=NA,dataTableRow=NA,startCol=NA,endCol=NA,BSkyMsg=BSkyAppMsg,RMsg=RMsg)
				
				# SK added on 04/24/21 - to avoid return strucutre to be processed by app just for the errors and warnings after returning
				# from the BSky function calls 
				#08Jun2021 the 3 templated function call BSkyReturnStructure instead of BSkyReturnStructure2, wirting into the sink file will be handled by BSkyReturnStructure2, hence it is not required here.
				#writeLines(as.character(BSkyAppMsg))
				#cat("\n")
				#writeLines(as.character(RMsg))
				#cat("\n")
				
				#cat("printing in reststructure 2 construction in BSkyReturnStructure() \n")
				bskyNoofTables = 1
			}
			else
			{
				#cat("printing in reststructure 3 construction in BSkyReturnStructure() \n")
				#cat("varIndex=NA,severityType=-2,varName=NA,dataTableRow=NA,startCol=NA,endCol=NA,BSkyMsg= ",BSkyAppMsg,"RMsg= ", RMsg,"\n")
				retstructure[[1]]$metadatatable[[1]]=rbind(retstructure[[1]]$metadatatable[[1]], data.frame(varIndex=NA,severityType=severityType,varName=NA,dataTableRow=NA,startCol=NA,endCol=NA,BSkyMsg=BSkyAppMsg,RMsg=RMsg))
				
				# SK added on 04/24/21 - to avoid return strucutre to be processed by app just for the errors and warnings after returning
				# from the BSky function calls 
				#08Jun2021 the 3 templated function call BSkyReturnStructure instead of BSkyReturnStructure2, wirting into the sink file will be handled by BSkyReturnStructure2, hence it is not required here.
				# writeLines(as.character(BSkyAppMsg))
				# cat("\n")
				# writeLines(as.character(RMsg))
				# cat("\n")
				
				#cat("printing in reststructure 4 construction in BSkyReturnStructure() \n")
				#uadatasets$retstructure[[bskyNoofTables]]$metadatatable[[1]]=rbind(uadatasets$retstructure[[bskyNoofTables]]$metadatatable[[1]],data.frame(varIndex=NA,type=severityType,varName=NA,dataTableRow=NA,startCol=NA,endCol=NA,BSkyMsg=BSkyAppMsg,RMsg=RMsg))
			}
		}
	}
	
	# SK added on 04/24/21 - to avoid return strucutre to be processed by app just for the errors and warnings after returning
	# from BSky function like BSky recode - rather print them to sync file now before returning back to the app environment 
	# Force Error and Warnings tables in the return strucutred to be nullified by setting isErrorWarning = FALSE. This is needed 
	# because printing of BSkyAppMsg and RMsg to sync file have already taken place above - hence no need to send back the 
	# same errors and warnings via the return structure to the app  
	# after returing to the app environment, the app will process the sync file and print out all the BSkyAppMsg and RMsg i.e. all
	# errors and warnings onto the app output window
	
	#08Jun2021 the 3 templated function call BSkyReturnStructure instead of BSkyReturnStructure2, wirting into the sink file will be handled by BSkyReturnStructure2, hence it is not required here.
	#isErrorWarning = FALSE
	
	##20Jun2013 By Anil create empty ewtable
	if(!isErrorWarning) 
	{
		# cat("\nNoError in Ret Stru\n")
			retstructure[[1]]<-list()
			retstructure[[1]]$type="ewtable"
			retstructure[[1]]$metadata="yes"
			retstructure[[1]]$nometadatatables=0
			retstructure[[1]]$metadatatabletype="normal"
			retstructure[[1]]$metadatatable=list()
							
			bskyNoofTables = 1
	}
	
	if((!is.null(uadatasets.sk$uaStatResults)) && length(uadatasets.sk$uaStatResults) > 0)
	{
		bskyNoofStatTables = length(uadatasets.sk$uaStatResults)
		uaStatTablesAndErrWarnTable = uadatasets.sk$uaStatResults 
	}
	
	##following commented to move ewtable to the last of the return structure
	# if(length(retstructure) > 0)
	# {
		# uaStatTablesAndErrWarnTable = c(uaStatTablesAndErrWarnTable, list(retstructure[[1]]))
	# }
	
	
	NoofAdditionalTables = length(bskyAdditionalTableList) ## Anil moved this out of 'if' and 'if' condition modified
	if( (NoofAdditionalTables > 1) || ((!is.na(bskyAdditionalTableList)) && NoofAdditionalTables==1) )
	{	
	#old if(!is.na(bskyAdditionalTableList))
	#old {	    
		bskyNoofAdditionalTables = length(bskyAdditionalTableList)
		#print(bskyAdditionalTableList)
		#cat("I am here 2 bskyNoofAdditionalTables = \n", bskyNoofAdditionalTables)
		
		if(bskyNoofAdditionalTables >0)
		{
			for(i in 1:bskyNoofAdditionalTables)
			{
				uaStatTablesAndErrWarnTable = c(uaStatTablesAndErrWarnTable, list(bskyAdditionalTableList[[i]]))
			}
		}
	}
	
	bskyNoofTables = bskyNoofTables + bskyNoofStatTables + bskyNoofAdditionalTables
	
	#T+he 7th return value tells us now many tables we are going to display in the output
	retlist = c(retlist, nooftables = list(bskyNoofTables))
	
	#### Following if conditioned moved from above to this place. So that "ewtable" will always be the last table in the list of tables
	#### For this C# XML DOM has to match properly.
	#### Earlier the sequence of tables wa [ BSkyStat/stacked tables THEN ewtable THEN addtionalTables ]
	#### Now return structure table sequence :  [ BSkyStat/stacked tables THEN addtionalTables THEN ewtable  ]
	 if(length(retstructure) > 0)
	{
		uaStatTablesAndErrWarnTable = c(uaStatTablesAndErrWarnTable, list(retstructure[[1]]))
	}
	
  	#8th position onward in case there is al least table output to be returned
	retlist = c( retlist, tables = list(uaStatTablesAndErrWarnTable)) #### length(tables) last table should have return value(s)
	
	BSkyBatchFunctionModeResetForUberFunction()
	# print (retlist)
	return(retlist)
}


# This initialization function should be called from within any ua subfunction (not from Uber function
uaFunctionInit <- function()
{
	callDetails = uaLogFunArgsDetails()
	#print( callDetails )
	
	functionName =  as.character(callDetails$FuntionCallMade)[1]
	
	if (uaperformance ==2)
	{
	   uastartlog(functionName, uadatasets.sk$uaTempUberFunctionName)
	}
	
	uadatasets.sk$callStackIndex = uadatasets.sk$callStackIndex + 1
	funcCallDetails =   list(FunctionName = functionName, FunctionParam = callDetails$RuntimeParamValues)
	uadatasets.sk$callStack = c(uadatasets.sk$callStack, list(funcCallDetails))
}
	
###Functions to call at end
uaFunctionWrapUp <- function(functionName)
{
		#index=which(datasetname==uadatasets$name)
    
		if (uaperformance ==2)
		{
			ualogcommand()
		}
		# uadatasets$uasummary =uaprocdesc(index,missing)
		# ualog=uaretlog(uadatasets.sk$uaTempUberFunctionName)
		
		#functionName =  as.character(callDetails$FuntionCallMade)[1]
		for(k in  uadatasets.sk$callStackIndex:1)
		{
			if(uadatasets.sk$callStack[[k]]$FunctionName == functionName)
			{
				# clean up the stack for those sub function that did not call the uaFunctionWrapUp() 
				for(j in uadatasets.sk$callStackIndex:k)
				{
				  uadatasets.sk$callStack[[j]] <- NULL
				  uadatasets.sk$callStackIndex = uadatasets.sk$callStackIndex - 1
				}
				break
			}
		}        
}

uaprocdesc <-function(datasetname, missing =0)
{
	# SK modified on 06/13/15
	#cat('\nuaprocdesc ')
	#cat(datasetname)
	i=1
	uasplitvars=NULL
	uasplitindex=NULL
	uafilter=NA
	uaweights=NA
	
	### Added by Anil ####S
	index = which(datasetname == uadatasets$name)
	#cat("\nIndex:uaprocdesc ")
	#cat(index)
	totrows = -1 #no dataset in lst index location
	totalloadedDS = length(uadatasets$lst)
	if(index > 0 && index <= totalloadedDS )
	{
		totrows = nrow(uadatasets$lst[[index]])
	}
	else
	{
		totrows = eval(parse(text=paste('nrow(',datasetname,')')))
	}
	### Added by Anil ####E
	
	
	# Total number of rows in dataset
	uatotalrows = totrows   #Anil commented  nrow(uadatasets$lst[[index]]) ##LST used here and gives error while loading dataset as lst is empty
	# Ckecking whether there any any splits involved with the dataset
	if (!is.null(uadatasets$split))
	{
		uasplitindex=uadatasets$split
		len =length(as.numeric(uasplitindex))
		for (i in 1:len)
		{
			uasplitvars =c(uasplitvars, names(uadatasets$lst[[index]][uasplitindex[i]]))
		}
	}
	else
	{
		uasplitvars=NULL
	}
	 #cat("\nuaproc:", uadatasets.sk$rproc,"\n")
	if(index > 0 && index <= totalloadedDS )
	{
		#cat('\nuaprocdesc: return from IF ')
		return(list(uadatasets$fullpath[index],uadatasets$name[index],uafilter,uaweights,uasplitvars,uatotalrows,uadatasets.sk$rproc,uadatasets$totalcputime,uadatasets$elapsedtime))
	}
	else
	{
		#cat('\nuaprocdesc: return from ELSE ')
		return(list(c(" "),c(" "),uafilter,uaweights,uasplitvars,uatotalrows,uadatasets.sk$rproc,uadatasets$totalcputime,uadatasets$elapsedtime))
	}
}	

uagetlevels <-function(index,uasplitindex)
{
	uaretlist <-list(NULL)
	len =length(uasplitindex)
	i=1
	for(i in 1:len)	
	{
		if( "tbl_df" %in% class(uadatasets$lst[[index]]))
		{
		#this if block was added to handle tibble dataset. Finding levels() in tibble is different. 
		#This did not help. We added some code in C# to convert tibble to data.frame and run the batch-commands
		#finally when the closing batch-command is executed we conver data.frame back to tibble.
		# so basicall we can delete this 'if block and take out the staements inside 'else' so that it always executes
		# just like it was before added if-else.
		
			uaretlist[[i]] <- levels(as.data.frame(uadatasets$lst[[index]])[,uasplitindex[i]])
		}
		else
		{
			uaretlist[[i]] <- levels(uadatasets$lst[[index]][,uasplitindex[i]])
		}
	}
	return(uaretlist)
}

uagetsplitcriteria <-function(noofvars)
{
	i=1
	uasplitcriteria ="uadatasets$lst[[index]][,uasplitvarindex[(i=i+1)]]==uacartlevels[j,i]"
	while (i < noofvars)
	{
		uasplitcriteria =paste(uasplitcriteria,"&","uadatasets$lst[[index]][,uasplitvarindex[(i=i+1)]]==uacartlevels[j,i]")
		i=i+1
	}
	return(uasplitcriteria)
}

uagetsplitinfo <- function(index,uavarindex)
{
	noofcols=ncol(uadatasets$lst[[index]])
	uasplitvarindex=NULL 
	uaplaceholder =NULL
	noofsplits= 1
	uasplitcriteria=NULL
	uacartlevels = NULL
	
	#i=1
	#for(i in 1:noofcols)
	#{
	#	if(TRUE %in% attributes(uadatasets$lst[[index]][,i])$split)	uasplitvarindex =c(uasplitvarindex, i)
	#}
	
	datasetname = uadatasets$name[[index]]
	splitInfo = UAgetDataframeSplit(datasetname)
	#cat("\nSplitInfo")
	#print(splitInfo)
	if( !is.null(splitInfo$DFsplit) && splitInfo$DFsplit == TRUE)#10Mar2017 is.null added by Anil
	{
	   uasplitvarindex = splitInfo$DFsplitcolindex  # $DFsplitcolnames
	}

	# print(uasplitvarindex)
	#Case where there are no splits defined. Should it be set to 0 as opposed to NULL?
	uadatasets$split=uasplitvarindex
	
	##if (length(uasplitvarindex) ==0) return(FALSE)
	##commented by anil if(length(uasplitvarindex) ==0)
	if(is.null(uasplitvarindex))
  {
	# print("if condition")
    return(list(0,noofsplits,uasplitcriteria,uacartlevels,uasplitvarindex))
  }
  else
  {
		# print("else condition")
	   uaretlist=uagetlevels(index,uasplitvarindex)
	   # print( uaretlist )
	   uacartlevels =expand.grid(uaretlist)
	   # print ( uacartlevels )
	   noofvars =ncol(uacartlevels)
	   noofsplits =nrow(uacartlevels)
	   uasplitcriteria=uagetsplitcriteria(noofvars)
	   # print( uasplitcriteria)
	   # The 1st value of the list indicates that there are splits and they work fine
	   #2nd argument returns the noofsplits
	   #3rd argument returns the split criteria
	   return(list(1,noofsplits,uasplitcriteria,uacartlevels,uasplitvarindex))
  }
}

BSkyComputeSplitdataset.old <- function(vars, datasetname)
{
  index=which(datasetname==uadatasets$name)
  # cat("\nVars:",vars,"\tindex:",index)
	uavarindex =uagetindex(vars, uaindex=index)
		# print("before call")
	uadatasets$uasplitinfo=uagetsplitinfo(index,uavarindex)
	# print("after call")
	#cat("printing  uadatasets$uasplitinfo \n")
	#cat(  uadatasets$uasplitinfo[[1]], "\n\n" )
	#cat(  uadatasets$uasplitinfo[[2]], "\n\n" )
	#cat(  uadatasets$uasplitinfo[[3]], "\n\n" )
	#cat(  uadatasets$uasplitinfo[[5]], "\n\n" )
	
	uadatasets.sk$currentDatasetname = datasetname
	
  return(uadatasets$uasplitinfo[[2]])
}

New.version.BSkyComputeSplitdataset <- function(datasetname)
{

if(exists("split", envir=uadatasets))uadatasets$split=NULL
uadatasets$uasplitinfo = NULL 
uadatasets.sk$splitIteration = 0
uadatasets.sk$uaSplitDatasetIndex = 0
bskyCurrentDatasetSplitSliceObj <<- NULL
uadatasets.sk$currentVarNamesAndFactorValues = ""

	# if(is.null(uadatasets$uasplitinfo))
	# {
		#uadatasets.sk$currentDatasetname = datasetname
		index=which(datasetname==uadatasets$name)
		# cat("\nVars:",vars,"\tindex:",index)
		#uavarindex =uagetindex(vars, uaindex=index) 
		uavarindex = NULL  #Sanjay Check again
			# print("before call")
		uadatasets$uasplitinfo=uagetsplitinfo(index,uavarindex)
		# print("after call")
		# cat("printing  uadatasets$uasplitinfo \n")
		# cat(  uadatasets$uasplitinfo[[1]], "\n\n" )
		# cat(  uadatasets$uasplitinfo[[2]], "\n\n" )
		# cat(  uadatasets$uasplitinfo[[3]], "\n\n" )
		# cat(  uadatasets$uasplitinfo[[5]], "\n\n" )
		
	  return(uadatasets$uasplitinfo[[2]])
	# }
	# else
	# {
		# return(noofsplits = 1)
	# }
}


BSkyComputeSplitdataset <- function(vars, datasetname)
{

if(exists("split", envir=uadatasets))uadatasets$split=NULL
uadatasets$uasplitinfo = NULL 
uadatasets.sk$splitIteration = 0
uadatasets.sk$uaSplitDatasetIndex = 0
uadatasets.sk$currentVarNamesAndFactorValues = ""

	# if(is.null(uadatasets$uasplitinfo))
	# {
		#uadatasets.sk$currentDatasetname = datasetname
		index=which(datasetname==uadatasets$name)
		#cat("\nVars:",vars,"\tindex:",index)
		uavarindex =uagetindex(vars, uaindex=index)  
		#print("before call")
		uadatasets$uasplitinfo=uagetsplitinfo(index,uavarindex)
		#print("after call")
		# cat("printing  uadatasets$uasplitinfo \n")
		# cat(  uadatasets$uasplitinfo[[1]], "\n\n" )
		# cat(  uadatasets$uasplitinfo[[2]], "\n\n" )
		# cat(  uadatasets$uasplitinfo[[3]], "\n\n" )
		# cat(  uadatasets$uasplitinfo[[5]], "\n\n" )
		
	  return(uadatasets$uasplitinfo[[2]])
	# }
	# else
	# {
		# return(noofsplits = 1)
	# }
}


##BSkySetCurrentDatasetName
BSkySetCurrentDatasetName.old<- function(datasetName, setDatasetIndex = 'N')
{
	index=which(datasetName==uadatasets$name)
	
	if(setDatasetIndex != 'N')
	{
		if(exists(datasetName))
		{
			if(index > 0)
			{
				if(!is.null(uadatasets$lst[index]))
				{
					uadatasets$lst[index] <- NULL
				}
				eval(parse(text=paste("uadatasets$lst[[index]]=",datasetName)))
				names(uadatasets$lst)[index] <- datasetName
			}
		}
	}
	uadatasets.sk$currentDatasetname = datasetName

	return(index)
}

BSkySetCurrentDatasetName<- function(datasetNameOrDatasetGlobalIndex, setDatasetIndex = 'N')
{
	datasetName = ""
	index = 0
	#17Jan2016 removing .GlobalEnv$ ,if it is prefixed with dataset name
		if(is.character(datasetNameOrDatasetGlobalIndex))
		{
			charcount <- nchar(datasetNameOrDatasetGlobalIndex)
			if(charcount > 11) # .GlobalEnv$
			{
				if(substr(datasetNameOrDatasetGlobalIndex, 1,11) == ".GlobalEnv$")
				{
					datasetNameOrDatasetGlobalIndex = substr(datasetNameOrDatasetGlobalIndex, 12,charcount)
				}
			}
		}
	#17Jan2016 end
	
	#cat('\nBSkySetCurrentDatasetName: ',datasetNameOrDatasetGlobalIndex	)
	#cat('\n2BSkySetCurrentDatasetName: ',uadatasets.sk$currentDatasetname,'\nAll Names:')
	#print(uadatasets$name)
	if(is.numeric(datasetNameOrDatasetGlobalIndex) && datasetNameOrDatasetGlobalIndex > length(uadatasets$name))
	{
		datasetName = uadatasets.sk$currentDatasetname
		index=which(datasetName==uadatasets$name)
	}
	else if(is.numeric(datasetNameOrDatasetGlobalIndex) && datasetNameOrDatasetGlobalIndex <= length(uadatasets$name))
	{
		index = datasetNameOrDatasetGlobalIndex
		datasetName = uadatasets$name[index]
	}
	else if(is.character(datasetNameOrDatasetGlobalIndex))
	{
		datasetName = datasetNameOrDatasetGlobalIndex
		index=which(datasetName==uadatasets$name)
	}
	#cat('\nBSkySetCurrentDatasetName Index: ',index)
	
	if(setDatasetIndex != 'N')
	{
		if(exists(datasetName, envir=.GlobalEnv))
		{
			#cat('\nDataset Exists: ',datasetName)
			if(index > 0)
			{
			
eval(parse(text=paste("uadatasets$lst[[index]]=.GlobalEnv$",datasetName,sep='')))
names(uadatasets$lst)[index] <- datasetName
			#commenting following IF/ELSE and using 2 lines above instead
			
				# if(BSkyIsInBatchCommandMode() == FALSE)
				# {
					# if(!is.null(uadatasets$lst[index]))
					# {
						# uadatasets$lst[index] <- NULL
					# }
					# #17jan2016 eval(parse(text=paste("uadatasets$lst[[index]]=",datasetName)))
# eval(parse(text=paste("uadatasets$lst[[index]]=.GlobalEnv$",datasetName,sep='')))
# names(uadatasets$lst)[index] <- datasetName

					
					# # cat("\nThis is set in non-Batch mode\n")
					# # print(uadatasets$lst[[index]])
				# }
				# else
				# {
					# ## condition changed by Anil
					# if(is.null(uadatasets$lst[index][[1]]) )  ## is(is.null(uadatasets$lst[index]) ) 
					# {
						# #17jan2016 eval(parse(text=paste("uadatasets$lst[[index]]=",datasetName)))
# eval(parse(text=paste("uadatasets$lst[[index]]=.GlobalEnv$",datasetName,sep='')))						
					# # cat("\nno of datasets in lst\t")
					# # print(length(uadatasets$lst))
# names(uadatasets$lst)[index] <- datasetName
					# # cat("\nno of names in lst\t")
					# # print(names(uadatasets$lst))
					# }
					# # cat("\nThis is set in Batch mode\n")
					# # print(uadatasets$lst[[index]])
				# }
			}
		}
	}
	uadatasets.sk$currentDatasetname = datasetName
	
	#cat('\nBSkySetCurrentDatasetName: ',datasetNameOrDatasetGlobalIndex	)
	#cat('\n2BSkySetCurrentDatasetName: ',uadatasets.sk$currentDatasetname,'\nAll Names:')
	#print(uadatasets$name)	
	
	return(index)
}


BSkyGetDatasetName <- function(datasetNameOrDatasetGlobalIndex)
{
	datasetname = ""
	if(is.numeric(datasetNameOrDatasetGlobalIndex))
	{
		if(datasetNameOrDatasetGlobalIndex > length(uadatasets$name) && datasetNameOrDatasetGlobalIndex <= length(uadatasets$lst))
		{
			#datasetname = paste(attr(uadatasets$lst[datasetNameOrDatasetGlobalIndex],"name"), "_Slice",uadatasets.sk$splitIteration, sep="")
			datasetname = names(uadatasets$lst[datasetNameOrDatasetGlobalIndex])
		}
		else if(datasetNameOrDatasetGlobalIndex <= length(uadatasets$name))
		{
			datasetname = uadatasets$name[datasetNameOrDatasetGlobalIndex]
		}
	}
	else if(is.character(datasetNameOrDatasetGlobalIndex))
	{
		datasetname = datasetNameOrDatasetGlobalIndex
	}
	
	return (datasetname)
}

#uavarIndex = BSkygetIndexesOfCols(vars, datasetname)

BSkyGetVarNames <- function(varNamesOrVarGlobalIndices, datasetNameOrDatasetGlobalIndex)
{
	varnames = c()
	if(is.numeric(varNamesOrVarGlobalIndices))
	{
		if(is.numeric(datasetNameOrDatasetGlobalIndex))
		{
			if(datasetNameOrDatasetGlobalIndex <= length(uadatasets$lst))
			{
				count = length(varNamesOrVarGlobalIndices)
				if(count > 0)
				{
					for(i in 1:count)
					{
						varnames = c(varnames, names(uadatasets$lst[[datasetNameOrDatasetGlobalIndex]][varNamesOrVarGlobalIndices[i]])) #first [] were single
					}
					# cat("\n1st condi\n")
				}
			}
		}
		else if(is.character(datasetNameOrDatasetGlobalIndex))
		{
			index = which(datasetNameOrDatasetGlobalIndex == uadatasets$name)
			
			count = length(varNamesOrVarGlobalIndices)
			if(count > 0 && index > 0)
			{
				for(i in 1:count)
				{
					varnames = c(varnames, names(uadatasets$lst[[index]][varNamesOrVarGlobalIndices[i]]))#first [] were single
				}
				# cat("\n2nd condi Lst index :",index," Var global indices:",varNamesOrVarGlobalIndices,"\n")
			}
		}
	}
	else if(is.character(varNamesOrVarGlobalIndices))
	{
		varnames = varNamesOrVarGlobalIndices
		# cat("\n3rd condi\n")
	}
	# print(varnames)	
	return (varnames)
}

BSkyGetDatasetGlobalIndex <- function(datasetNameOrDatasetGlobalIndex)
{
	datasetglobalindex = 0
	
	if(is.character(datasetNameOrDatasetGlobalIndex))
	{
		datasetglobalindex = which(datasetNameOrDatasetGlobalIndex == uadatasets$name)
	}
	else if(is.numeric(datasetNameOrDatasetGlobalIndex))
	{
		if(datasetNameOrDatasetGlobalIndex <= length(uadatasets$lst))
		{
			datasetglobalindex = datasetNameOrDatasetGlobalIndex
		}
	}
	
	return (datasetglobalindex)
}

BSkyGetVarGlobalIndices <- function(varNamesOrVarGlobalIndices, datasetNameOrDatasetGlobalIndex)
{
	varindices = c()
	
	if(is.character(varNamesOrVarGlobalIndices))
	{
		if(is.numeric(datasetNameOrDatasetGlobalIndex))
		{
			if(datasetNameOrDatasetGlobalIndex <= length(uadatasets$lst))
			{
				count = length(varNamesOrVarGlobalIndices)
				if(count > 0)
				{
					for(i in 1:count)
					{
						varindices = c(varindices, which(varNamesOrVarGlobalIndices[i] == names(uadatasets$lst[[datasetNameOrDatasetGlobalIndex]])))
					}
				}
			}
		}
		else if(is.character(datasetNameOrDatasetGlobalIndex)) #may have potential issue with this condition if the split is ON
		{
			index = which(datasetNameOrDatasetGlobalIndex == uadatasets$name)
			
			count = length(varNamesOrVarGlobalIndices)
			if(count > 0 && index > 0)
			{
				for(i in 1:count)
				{
					varindices = c(varindices, which(varNamesOrVarGlobalIndices[i] == names(uadatasets$lst[[index]])))
				}
			}
		}
	}
	else
	{
		varindices = varNamesOrVarGlobalIndices
	}
		
	return (varindices)
}

uaGetCurrentDatabaseName <- function()
{
  return(uadatasets.sk$currentDatasetname)
}

BSkyGetCurrentDatabaseName <- function()
{
  return(uadatasets.sk$currentDatasetname)
}

BSkyGetNextDatasetSliceIndex.old <- function(vars, datasetname)
{
    index=which(datasetname==uadatasets$name)
    # print(uadatasets$uasplitinfo)
    if(!is.null(uadatasets$uasplitinfo) && (uadatasets$uasplitinfo[[1]] == 1))
    {
        if(uadatasets.sk$splitIteration == 0)
        {
          # cat("yes split 1st iteration", "\n\n")
    		  uadatasets.sk$splitIteration = 1
    		  j =  uadatasets.sk$splitIteration
    		  
    		  #uasplitindex=uadatasets$index+1
          uadatasets.sk$uaSplitDatasetIndex=length(uadatasets$lst)+1
          uasplitindex =  uadatasets.sk$uaSplitDatasetIndex
		  # cat("\n UASplit index:",uasplitindex,"\n")
		  
		# cat("uasplitinfo:" , "\n")
        #print(uadatasets$uasplitinfo)
        }
        else
        {
          uadatasets.sk$splitIteration  = uadatasets.sk$splitIteration + 1
          # cat("yes split iteration num", uadatasets.sk$splitIteration, "\n\n")
          j =  uadatasets.sk$splitIteration
          
          uadatasets$lst[uadatasets.sk$uaSplitDatasetIndex] <- NULL
          uasplitindex =  uadatasets.sk$uaSplitDatasetIndex
        }
        
        i = 0
        uacartlevels=uadatasets$uasplitinfo[[4]]
        uasplitvarindex=uadatasets$uasplitinfo[[5]]
		# cat("uasplitinfo:" , "\n")
        # print(uadatasets$uasplitinfo)
		
        uavarIndex = BSkygetIndexesOfCols(vars, datasetname)
        # cat ("Var index :",  uavarIndex, "\n")
        # cat ("before printing\n")
        # cat(" j= ", j, "\n")
        # cat(" i =", i, "\n")
        # cat("uacartlevels" , "\n")
        # print(uacartlevels)
        # cat("uasplitvarindex" , "\n")
        # print(uasplitvarindex)
        # cat("uavarIndex" , "\n")
        # print(uavarIndex)
		# cat("uasplitinfo:" , "\n")
        # print(uadatasets$uasplitinfo)
        # cat ("\nbefore subset op\n\n")
		# print((parse(text =(uadatasets$uasplitinfo[[3]]))))
		# cat("\nAnother\n")
		#print(eval(parse(text =(uadatasets$uasplitinfo[[3]]))))
        uadatasets$lst[[uasplitindex]]=subset(uadatasets$lst[[index]],subset=eval(parse(text =(uadatasets$uasplitinfo[[3]]))), select=uavarIndex)
        # cat ("\nAfter subset op\n\n")
        uadatasets.sk$currentVarNamesAndFactorValues = BSkyComputeCurrentVarNamesAndFactorValues(j, datasetname)
        return (list(datasetSliceIndex =  uasplitindex, varIndex =1:length(uavarIndex)))
    }
    else
    {
        # cat("no split \n\n")
        #uavarindex =uagetindex(vars, uaindex=index)
        #cat("uavarIndex" , "\n")
        # print(uavarIndex)
        uavarIndex = BSkygetIndexesOfCols(vars, datasetname)
        uadatasets.sk$currentVarNamesAndFactorValues = BSkyComputeCurrentVarNamesAndFactorValues(1, datasetname)
        return (list(datasetSliceIndex =  index, varIndex = uavarIndex))
    }
}

BSkyGetNextDatasetSliceIndex <- function(vars, datasetname)
{
    index=which(datasetname==uadatasets$name)
    #cat('\nBSkyGetNextDatasetSliceIndex: datasetname: ',datasetname)
    if(!is.null(uadatasets$uasplitinfo) && (uadatasets$uasplitinfo[[1]] == 1)) 
    {
        if(uadatasets.sk$splitIteration == 0)
        {
          #cat("yes split 1st iteration", "\n\n")
    		  uadatasets.sk$splitIteration = 1
    		  j =  uadatasets.sk$splitIteration
    		  
    		  #uasplitindex=uadatasets$index+1
			
		  #Sanjay - use length(uadatasets$names) 
          #uadatasets.sk$uaSplitDatasetIndex=length(uadatasets$lst)+1
		  
		  #Sanjay - changed on 6/16/13
		  uadatasets.sk$uaSplitDatasetIndex=length(uadatasets$name)+1
		  
          uasplitindex =  uadatasets.sk$uaSplitDatasetIndex
		  # cat("\nif condition:")
		  # print(uadatasets.sk$uaSplitDatasetIndex)
        }
        else
        {
          uadatasets.sk$splitIteration  = uadatasets.sk$splitIteration + 1
          # cat("yes split iteration num", uadatasets.sk$splitIteration, "\n\n")
          j =  uadatasets.sk$splitIteration
          
		  #Sanjay - this should be OK - no change needed
          uadatasets$lst[uadatasets.sk$uaSplitDatasetIndex] <- NULL
          uasplitindex =  uadatasets.sk$uaSplitDatasetIndex
		  # cat("\nelse condition:")
		  # print(uadatasets.sk$uaSplitDatasetIndex)
        }
        
        i = 0
        uacartlevels=uadatasets$uasplitinfo[[4]]
        uasplitvarindex=uadatasets$uasplitinfo[[5]]
		#cat('\n1BSkyGetNextDatasetSliceIndex: ',uadatasets.sk$currentDatasetname)
        uavarIndex = BSkygetIndexesOfCols(vars, datasetname)
		#cat('\n2BSkyGetNextDatasetSliceIndex: ',uadatasets.sk$currentDatasetname)
        # cat ("Var index :",  uavarIndex, "\n")
        # cat ("before printing\n")
        # cat(" j= ", j, "\n")
        # cat(" i =", i, "\n")
        # cat("uacartlevels" , "\n")
        # print(uacartlevels)
        # cat("uasplitvarindex" , "\n")
        # print(uasplitvarindex)
        # cat("uavarIndex" , "\n")
        # print(uavarIndex)
        
        # cat (" before subset op\n\n")
		# print(uasplitindex)
		#Sanjay - need to make changes for subset(uadatasets$lst[[index]].... with uadatsets$names[index]
        uadatasets$lst[[uasplitindex]]=subset(uadatasets$lst[[index]],subset=eval(parse(text =uadatasets$uasplitinfo[[3]])), select=uavarIndex)
        #cat ("After subset op\n\n")
		names(uadatasets$lst)[uasplitindex] <- datasetname
        uadatasets.sk$currentVarNamesAndFactorValues = BSkyComputeCurrentVarNamesAndFactorValues(datasetname)
        return (list(datasetSliceIndex =  uasplitindex, varIndex =1:length(uavarIndex), globalSplitIterationCounter = uadatasets.sk$splitIteration))
    }
    else
    {
        #cat("no split \n\n")
        #uavarindex =uagetindex(vars, uaindex=index)
        #cat("uavarIndex" , "\n")
        #print(uavarIndex)
		#cat('\n1BSkyGetNextDatasetSliceIndex: ',uadatasets.sk$currentDatasetname)
        uavarIndex = BSkygetIndexesOfCols(vars, datasetname)
		#cat('\n2BSkyGetNextDatasetSliceIndex: ',uadatasets.sk$currentDatasetname)
        #uavarIndex = BSkygetIndexesOfCols(vars, datasetname)
        uadatasets.sk$currentVarNamesAndFactorValues = BSkyComputeCurrentVarNamesAndFactorValues(datasetname)
        return (list(datasetSliceIndex =  index, varIndex = uavarIndex, globalSplitIterationCounter = 1))
    }
}

New.version.BSkyGetNextDatasetSplitSlice <- function(datasetname)
{
    index=which(datasetname==uadatasets$name)
    
    if(!is.null(uadatasets$uasplitinfo) && (uadatasets$uasplitinfo[[1]] == 1)) 
    {
        if(uadatasets.sk$splitIteration == 0)
        {
          #cat("yes split 1st iteration", "\n\n")
    		  uadatasets.sk$splitIteration = 1
    		  j =  uadatasets.sk$splitIteration
    		  
    		  #uasplitindex=uadatasets$index+1
			
		  #Sanjay - use length(uadatasets$names) 
          #uadatasets.sk$uaSplitDatasetIndex=length(uadatasets$lst)+1
		  
		  #Sanjay - changed on 6/16/13
		  uadatasets.sk$uaSplitDatasetIndex=length(uadatasets$name)+1
		  
          uasplitindex =  uadatasets.sk$uaSplitDatasetIndex
		  # cat("\nif condition:")
		  # print(uadatasets.sk$uaSplitDatasetIndex)
        }
        else
        {
          uadatasets.sk$splitIteration  = uadatasets.sk$splitIteration + 1
          # cat("yes split iteration num", uadatasets.sk$splitIteration, "\n\n")
          j =  uadatasets.sk$splitIteration
          
		  #Sanjay - this should be OK - no change needed
          uadatasets$lst[uadatasets.sk$uaSplitDatasetIndex] <- NULL
          uasplitindex =  uadatasets.sk$uaSplitDatasetIndex
		  # cat("\nelse condition:")
		  # print(uadatasets.sk$uaSplitDatasetIndex)
        }
        
        i = 0
        uacartlevels=uadatasets$uasplitinfo[[4]]
        uasplitvarindex=uadatasets$uasplitinfo[[5]]
        #uavarIndex = BSkygetIndexesOfCols(vars, datasetname)
        # cat ("Var index :",  uavarIndex, "\n")
        # cat ("before printing\n")
        # cat(" j= ", j, "\n")
        # cat(" i =", i, "\n")
        # cat("uacartlevels" , "\n")
        # print(uacartlevels)
        # cat("uasplitvarindex" , "\n")
        # print(uasplitvarindex)
        # cat("uavarIndex" , "\n")
        # print(uavarIndex)
        
        # cat (" before subset op\n\n")
		# print(uasplitindex)
		#Sanjay - need to make changes for subset(uadatasets$lst[[index]].... with uadatsets$names[index]
        #uadatasets$lst[[uasplitindex]]=subset(uadatasets$lst[[index]],subset=eval(parse(text =uadatasets$uasplitinfo[[3]])), select=uavarIndex)
		uadatasets$lst[[uasplitindex]]=subset(uadatasets$lst[[index]],subset=eval(parse(text =uadatasets$uasplitinfo[[3]])))
		
		bskyCurrentDatasetSplitSliceObj <<- NULL
		bskyCurrentDatasetSplitSliceObj <<-subset(uadatasets$lst[[index]],subset=eval(parse(text =uadatasets$uasplitinfo[[3]])))
		
        #cat ("After subset op\n\n")
		names(uadatasets$lst)[uasplitindex] <- datasetname
        uadatasets.sk$currentVarNamesAndFactorValues = BSkyComputeCurrentVarNamesAndFactorValues(datasetname)
        #return (list(datasetSliceIndex =  uasplitindex, varIndex =1:length(uavarIndex), globalSplitIterationCounter = uadatasets.sk$splitIteration))
		return (list(datasetSliceIndex =  uasplitindex, varIndex = NULL, globalSplitIterationCounter = uadatasets.sk$splitIteration))
    }
    else
    {
		bskyCurrentDatasetSplitSliceObj <<- NULL
		
        #cat("no split \n\n")
        #uavarindex =uagetindex(vars, uaindex=index)
        #cat("uavarIndex" , "\n")
        #print(uavarIndex)
        #uavarIndex = BSkygetIndexesOfCols(vars, datasetname)
        uadatasets.sk$currentVarNamesAndFactorValues = BSkyComputeCurrentVarNamesAndFactorValues(datasetname)
        #return (list(datasetSliceIndex =  index, varIndex = uavarIndex, globalSplitIterationCounter = 1))
		return (list(datasetSliceIndex =  index, varIndex = NULL, globalSplitIterationCounter = 1))
    }
}


BSkyComputeCurrentVarNamesAndFactorValues <- function( datasetname)
{
    varNameFactorCombination = ""
    
	splitIteratonIndex = uadatasets.sk$splitIteration
	
    index=which(datasetname==uadatasets$name)
	
    if(!is.null(uadatasets$uasplitinfo) && (uadatasets$uasplitinfo[[1]] == 1))
    {
        uacartlevels=uadatasets$uasplitinfo[[4]]
        len = length( uadatasets$uasplitinfo[[5]])
        for (i in 1:len)
        {
          varName = names(uadatasets$lst[[index]][uadatasets$uasplitinfo[[5]][i]])
          # factorValue =  uacartlevels[splitIteratonIndex,i]
          factorValue =  uacartlevels[uadatasets.sk$splitIteration,i]
          
          if(i == 1)
          {
            varNameFactorCombination = paste(varNameFactorCombination, "Split = Y, Split Iteration Number =", splitIteratonIndex, ", ")
          }
          
          if(i > 1)
          {
            varNameFactorCombination = paste(varNameFactorCombination, ", ")
          }
          
          varNameFactorCombination = paste(varNameFactorCombination,  varName, "=", factorValue) 
        }
    }
    else
    {
        varNameFactorCombination = paste(varNameFactorCombination, "Split = N ")
    }
    
    return(varNameFactorCombination)
}

BSkyGetCurrentSplitFactorValues <- function()
{
	return(uadatasets.sk$currentVarNamesAndFactorValues)
}

uaIsDatasetSplit <- function(datasetname)
{
    index=which(datasetname==uadatasets$name)
    splitInfo = UAgetDataframeSplit(datasetname)
	
	if( !is.null(splitInfo$DFsplit) && splitInfo$DFsplit == TRUE)#10Mar2017 is.null added by Anil
    {
        return(TRUE)
    }
    else
    {
        return(FALSE)
    }
}
## Added by Aaron and Sanjay
## Added to support Artems request
BSkyIsDatasetSplit <- function(datasetname)
{
     return(invisible(uaIsDatasetSplit (datasetname)))
}

uaRetInitialize <- function()
{   
    ## The following is just as notes for now.....
	
  	#The 1st return value tells us whether there is a split or not (1=Split, 0=No split)
  
  	#The 2nd return value tells us whether there is an error or not (-1 if there is an error, 0 if there is no error)
  
  	#The 3rd return value gives us the number of errors
  
  	#The 4th return value tells us how many warnings there are
  
  	#The 5th return value gives us all the log values (This is a dataframe)
  
  	#The 6th return value gives us the summary of the function call with the actual parameter valuse passed
  
    #The 7th return value tells us now many tables we are going to display in the output
  
  	# 8th position onward in case there is al least table output to be returned
  	
  	retlist = list()
    retlist = c( retlist, list(split= (if(!is.null(uadatasets$uasplitinfo)) uadatasets$uasplitinfo[[1]] else 0 )))
    retlist = c( retlist, list(error= (if(uadatasets.sk$totalErrors > 0) -1 else 0)))
    retlist = c( retlist, list(nooferrors=uadatasets.sk$totalErrors ))
    retlist = c( retlist, list(noofwarnings=uadatasets.sk$totalWarnings ))
    retlist = c( retlist, list(log=ualog$df ))
    retlist = c( retlist, list(uasummary = uadatasets$uasummary ))
    return (retlist)
} 		


uaGetUberFunctionName<-function()
{
    return( uadatasets.sk$uaTempUberFunctionName )
}
	

uaGetUberFunctionParam<-function()
{
    return( uadatasets.sk$callStack[[1]]$FunctionParam )
}	

uaGetCurrentSplitLevel<-function()
{
  return(uadatasets.sk$currentVarNamesAndFactorValues)
}
	
WriteLatestUberErrorInReturnTables <- function()
{
    uadatasets.sk$totalUberErrors = 0
    #Variable below is created to indicate that we have created a new table structure to hold the error
	   BSkyTableInit=NULL
    #uaretstructure <-list(NULL)
	   #print( ualog$df[nrow(ualog$df),] )
    lastLogRow <- ualog$df[nrow(ualog$df),]
    #print( ualog$df[nrow(ualog$df),] )
    
    offendingFuncName <- as.character(lastLogRow$functionName)
    offendingFuncParam <- as.character(lastLogRow$functionCommand)
    RerrMsg <- as.character(lastLogRow$RMessage)
    UAerrMsg <- as.character(lastLogRow$BSkyMessage)
    uberFunctionName <- uaGetUberFunctionName()
    uberFunctionParam <- uaGetUberFunctionParam() 
    #allMsgStrings <- list(UAerrMsg,RerrMsg,offendingFuncName, offendingFuncParam, uberFunctionName, uberFunctionParam)
  	#"Critical error in function %s, the function that caused the error is %s, the R error message is %s, 
  	#uberFunctionName
  	#offendingFuncName 
  	#RerrMsg
	
  	bskyNoofTables=length(uadatasets$retstructure)
  	if (bskyNoofTables==0)
  	{
  		uadatasets$retstructure[[1]]<-list()
  		uadatasets$retstructure[[1]]$type="table"
  		uadatasets$retstructure[[1]]$metadata="yes"
  		uadatasets$retstructure[[1]]$nometadatatables=1
  		uadatasets$retstructure[[1]]$metadatatabletype="normal"
  		uadatasets$retstructure[[1]]$metadatatable=list()
  		uadatasets$retstructure[[1]]$metadatatable[[1]]=data.frame(varIndex=NA,type=-2,varName=NA,dataTableRow=NA,startCol=NA,endCol=NA,BSkyMsg=UAerrMsg,RMsg=RerrMsg)
  		BSkyTableInit=TRUE
  	}
  	else
  	{
  		uadatasets$retstructure[[bskyNoofTables]]$metadatatable[[1]]=rbind(uadatasets$retstructure[[bskyNoofTables]]$metadatatable[[1]],data.frame(varIndex=NA,type=-2,varName=NA,dataTableRow=NA,startCol=NA,endCol=NA,BSkyMsg=UAerrMsg,RMsg=RerrMsg))
  		BSkyTableInit=FALSE
  	}
  		
    return(BSkyTableInit)
    
    #return(list(type = "table", metadata="yes", nometadatatables = "yes", nometadatatables = 1, metadatatabletype = "normal", metadatatable = list(UAerrMsg,RerrMsg,offendingFuncName, offendingFuncParam, uberFunctionName, uberFunctionParam  )))
}
	

 #================================================================
 # The follwoing set of function are for testing try-catch handler
 #=================================================================

# This is just a test ua sub function

  uaTestSubfuncNoHandler <- function(uavarindex, mu,conf.level,index, missing)
  {
    # every ua sub function can have its own try catch or none
    
    # every ua sub function must call the following two 
    # initislization call at the begining of the function as follows 
    #callParamDetails = uaLogFunArgsDetails()
    #uaFunctionInit(callParamDetails)
    uaFunctionInit()
    
    # do staff whatever sub function supposed to do
    #cat("in test function \n")
    
    uaTestSubSubfuncNoHandler (uavarindex, mu,conf.level,index, missing)
    
    #warning("warning from uaTestSubfuncNoHandler")
    #a <- XXX + 1
    
    # every ua sub function must call the following function to write the log
    uaFunctionWrapUp("uaTestSubfuncNoHandler")
  }


  # This is just a test ua sub function

  uaTestSubfuncWithHandler <- function(uavarindex, mu,conf.level,index, missing)
  {
    # every ua sub function can have its own try catch or none
    
    # every ua sub function must call the following two 
    # initislization call at the begining of the function as follows 
    #callParamDetails = uaLogFunArgsDetails()
    #uaFunctionInit(callParamDetails)
    uaFunctionInit()
    
    tryCatch(
    		{
    			withCallingHandlers(
    			{
              # do staff whatever sub function supposed to do
              #cat("in test function \n")
              warning("warning from uaTestSubfuncWithHandler")
              #uaTestSubSubfuncNoHandler(uavarindex, mu,conf.level,index, missing)
              #a <- YYY + 1
              #tt= "test"
              #as.numeric(tt)
              plot(YYY+90)
          },
    			warning = UAwarnHandlerFn
    			) # end of withCallingHandlers for catching warnings and continuing execution
    		},
    		error = UAerrHandlerFn,
    		silent =TRUE
    	)
    	

    	if( uaErrorFound() == TRUE)
    	{
    		#if any error encountered in the above try catch block
    		
    		#After doing whatever it needs to do, reset the error flags
        uaErrorFlagsReset()
    	}
    
    	if(uaWarningFound() == TRUE)
    	{
    		#if any warning encountered in the above try catch block
    		
    		#After doing whatever it needs to do, reset the warning flags to continue processing
    		uaWarningFlagsReset()
    	}
   	
    # every ua sub function must call the following function to write the log
    uaFunctionWrapUp("uaTestSubfuncWithHandler")
  }
  
  
  
  # This is just a test ua sub sub function
  uaTestSubSubfuncNoHandler <- function(uavarindex, mu,conf.level,index, missing)
  {
    # every ua sub function can have its own try catch or none
    
    # every ua sub function must call the following two 
    # initislization call at the begining of the function as follows 
    
    #callParamDetails = uaLogFunArgsDetails()
    #uaFunctionInit(callParamDetails)
    uaFunctionInit()
    
    # do staff whatever sub function supposed to do
    #cat("in test function \n")
    #for(i in 1:1)
    #{
      warning("warning from uaTestSubSubfuncNoHandler")
      tt= "test"
      as.numeric(tt)
      #a <- plot(XXX + 1)
    #}
    
    # every ua sub function must call the following function to write the log
    uaFunctionWrapUp("uaTestSubSubfuncNoHandler")
  }
  
#========

# This version of uawritelog should replace the uawritelog function in Anil's uaDataPackage 
#uawritelog <-function(type=NA,datasetName=NA, splitLevel = NA, functionName=NA,functionCommand=NA,nearestTryCatch=NA,callingFnOneLevelUp=NA, uberFunct=NA,startTime=NA,endTime =NA,userTime=NA, systemTime=NA,totalCpuTime =NA,elapsed=NA,initNCells=NA,
#	initVCells=NA,initTotal=NA, finNCells=NA, finVCells=NA, finTotal=NA,maxNCells=NA,maxVCells=NA,maxTotal=NA,RMessage =NA,BSkyMessage =NA,callfn=NA)
uawritelog <-function(type=NA,datasetName=NA, splitLevel = NA, functionName=NA, functionCommand=NA,callingFnNameOneLevelUp =NA, runTimeParamValuesOneLevelUp=NA, nearestTryCatch=NA, runTimeParamValuesTryCatchFunction=NA,uberFunct=NA, RMessage =NA,BSkyMessage =NA, startTime=NA,endTime =NA,userTime=NA, systemTime=NA,totalCpuTime =NA,elapsed=NA,initNCells=NA,
	initVCells=NA,initTotal=NA, finNCells=NA, finVCells=NA, finTotal=NA,maxNCells=NA,maxVCells=NA,maxTotal=NA)
{	
   #cat("entering in writelog\n")
   #cat(type,datasetName, splitLevel, functionName,functionCommand,uberFunct,startTime,endTime,userTime, systemTime,totalCpuTime,elapsed,initNCells,initVCells,initTotal, finNCells, finVCells, finTotal,maxNCells,maxVCells,maxTotal,RMessage,BSkyMessage,callfn, "\n")
   #cat("exiting from writelog\n\n")
  
  if (is.na(datasetName)) 
  {
    datasetName = uadatasets.sk$currentDatasetname
  }
  
  if(!is.na(type) && (type == "Warning" || type == "Error") )
  {
    if (is.na(splitLevel)) splitLevel = uadatasets.sk$currentVarNamesAndFactorValues
  }
  #print("UADataset Sk Env")
  #print(ls(envir=uadatasets.sk))
  #print("Uber Funct:")
  #print(uberFunct)
  if (is.na(uberFunct)) 
  {
    uberFunct = uaGetUberFunctionName()
  }
  
	if (!is.data.frame(ualog$df))
	{
		#ualog$df =data.frame(type,datasetName, splitLevel, functionName,functionCommand, callingFnOneLevelUp, nearestTryCatch, uberFunct,startTime,endTime,userTime, systemTime,totalCpuTime,elapsed,initNCells,initVCells,initTotal, finNCells, finVCells, finTotal,maxNCells,maxVCells,maxTotal,RMessage,BSkyMessage,callfn) 
		ualog$df =data.frame(type,datasetName, splitLevel, functionName,functionCommand, callingFnNameOneLevelUp, runTimeParamValuesOneLevelUp, nearestTryCatch, runTimeParamValuesTryCatchFunction,uberFunct,RMessage,BSkyMessage, startTime,endTime,userTime, systemTime,totalCpuTime,elapsed,initNCells,initVCells,initTotal, finNCells, finVCells, finTotal,maxNCells,maxVCells,maxTotal) 
  }
	else
	{
		# ualog$df =rbind(ualog$df,data.frame(type,datasetName, splitLevel, functionName,functionCommand, callingFnOneLevelUp, nearestTryCatch,uberFunct,startTime,endTime,userTime, systemTime,totalCpuTime,elapsed,initNCells,initVCells,initTotal, finNCells, finVCells, finTotal,maxNCells,maxVCells,maxTotal,RMessage,BSkyMessage,callfn))
    ualog$df =rbind(ualog$df,data.frame(type,datasetName, splitLevel, functionName,functionCommand, callingFnNameOneLevelUp, runTimeParamValuesOneLevelUp, nearestTryCatch, runTimeParamValuesTryCatchFunction,uberFunct,RMessage,BSkyMessage,startTime,endTime,userTime, systemTime,totalCpuTime,elapsed,initNCells,initVCells,initTotal, finNCells, finVCells, finTotal,maxNCells,maxVCells,maxTotal))
  }
}
  
uastartlog <-function(name,uberfnname)
{
	#These globals are created so that the log command can access them
	uadatasets$ostarttime[uadatasets$logindex] =date()
	uadatasets$starttime[[uadatasets$logindex]]=proc.time()
	uadatasets$funtname[uadatasets$logindex]=name
	uadatasets$uberfunct[uadatasets$logindex]=uberfnname
	if (uaperformance ==2)
	{
		uawritelog(type="Function call",functionName =uadatasets$funtname[uadatasets$logindex],uberFunct=uberfnname,startTime=uadatasets$ostarttime[uadatasets$logindex])	
		uadatasets$initialmem[[uadatasets$logindex]]=gc()
	}	
	uadatasets$logindex =uadatasets$logindex+1
}

ualogcommand <-function()
{
	oendtime=date()
	endtime=proc.time()
#uadatasets$elapsedtime This is the time from the beginning to the end of a procedure in R. NOTE I AM SEEING ERRORS IN THIS,
#SUBTRACTING 2 PROC TIMES DOES NOT SEEM TO BE THE ANSWER, WE MAY NEED TO USE SYSTIME
#uadatasetslogindex always holds the index into the next function should be logged
	time=endtime-uadatasets$starttime[[uadatasets$logindex-1]]
	finalmem=gc()
	# Total CPU time and elapsed time is accessed in the Notes/description of the procedure command
	uadatasets$totalcputime=time[1]+time[2]
	uadatasets$elapsedtime=time[3]
	if(uaperformance==1)
	{
	   uawritelog(type="Function call",functionName =uadatasets$funtname[uadatasets$logindex-1],uberFunct =uadatasets$uberfunct[uadatasets$logindex-1],startTime=uadatasets$ostarttime[uadatasets$logindex-1], elapsed = uadatasets$elapsedtime, endTime=oendtime)	
	}
	if (uaperformance ==2)
 	{
		uawritelog(type ="Function call", functionName=uadatasets$funtname[uadatasets$logindex-1],uberFunct =uadatasets$uberfunct[uadatasets$logindex-1],startTime =uadatasets$ostarttime[uadatasets$logindex-1],endTime=oendtime,userTime=time[1],systemTime=time[2],elapsed=time[3],totalCpuTime=time[1]+time[2],
		initNCells=uadatasets$initialmem[[uadatasets$logindex-1]][1,2],initVCells=uadatasets$initialmem[[uadatasets$logindex-1]][2,2],initTotal=uadatasets$initialmem[[uadatasets$logindex-1]][1,2]+uadatasets$initialmem[[uadatasets$logindex-1]][2,2], finNCells=finalmem[1,2], finVCells=finalmem[2,2], finTotal=finalmem[1,2]+finalmem[2,2],maxNCells=finalmem[1,6],maxVCells=finalmem[2,6],maxTotal=finalmem[1,6]+finalmem[2,6])
	}
	uadatasets$logindex =uadatasets$logindex-1
}   

uaretlog <-function(name)
{
	#ualog=subset(ualog$df,subset=((ualog$df['type']=="error" & ualog$df['functionname']==name) | (ualog$df['type']=="warning" & ualog$df['functionname']==name)),select=c(type,uberfunct,functionname,callfn,message,uamessage))
	# With callfn left out
	#ualog=subset(ualog$df,subset=((ualog$df['type']=="error" & ualog$df['uberfunct']==name) | (ualog$df['type']=="warning" & ualog$df['functionname']==name)),select=c(type,uberfunct,functionname,message,uamessage))
	#ualog=subset(ualog$df,subset=(ualog$df['uberfunct']==name),select=c(type,uberfunct,functionname,message,uamessage))
	
	#printing ualog$df before trimming it using uber (top level) funactiona name
	#print (ualog$df)
	ualog=subset(ualog$df,subset=(ualog$df['uberFunct']==name))
	return(ualog)
}