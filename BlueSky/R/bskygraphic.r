######## list of methods in this file; in sequence (few may not be ready and unused ) ###########
# bsky.plot
# bksy.histogram
# bsky.linegraph
#################################################################################################

### PLOT ###
bsky.plot<-function(xcolNameOrIndex, ycolNameOrIndex, dataSetNameOrIndex, width=700, height=700)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("bsky.plot: Error in Plot : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(xcolNameOrIndex, ycolNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("bsky.plot: Warning in Plot : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(xcolNameOrIndex, ycolNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	templist <- list()
imgpath<-paste(Sys.getenv("TMP"), '\\rimage.png', sep='')
	DataSetIndex = 0
	tryCatch(
		{
	
		withCallingHandlers(
		{
if( file.exists(imgpath) )
file.remove(imgpath)
datasetname <- dataSetNameOrIndex #BSkyValidateDataset(dataSetNameOrIndex)
	#if dataset index is valid
	if(!is.null(datasetname))
	{		

		xcolIndex<-BSkyValidateColumn(datasetname, xcolNameOrIndex)
		
		ycolIndex<-BSkyValidateColumn(datasetname, ycolNameOrIndex)
		#cat("\nX = ",xcolIndex, "\tY = ", ycolIndex,"\n")
		if(xcolIndex > 0 && ycolIndex > 0)
		{
			imgpath<-paste(Sys.getenv("TMP"), '\\rimage.png', sep='')
			#"png(\"" + imgpath + "\", width="+width+",height="+height+")";
			png(imgpath, width = width, height=height);
			xarg <- eval(parse(text=paste(datasetname,'[[',xcolIndex,']]',sep='')))
			#print(xarg)
			yarg <- eval(parse(text=paste(datasetname,'[[',ycolIndex,']]',sep='')))
			#print(yarg)
			plot(x=xarg, y=yarg, main="Plot", xlab=xcolNameOrIndex,ylab=ycolNameOrIndex)		
			dev.off();
				###creating return list 
	# commnd <- paste("bsky.plot(xcolNameOrIndex=c('",xcolNameOrIndex,"')",", ycolNameOrIndex=c('",ycolNameOrIndex,"')",", dataSetNameOrIndex='",datasetname,"')")
	# uas<-list("a","b","c","d","e","f",commnd)
    # templist <- list(filen=imgpath, uasummary=uas,bb="")
	#-return (templist);
		}
				else
		{
				#Error:colname not found
			#cat("\ninValid Col\n")
			#Column not found
			warning("bsky.plot: Column not found..")
			BSkyErrMsg = paste("bsky.plot: Error in Plot Column not found : ", "DataSetName :", datasetname," ", "Variable Name List :", paste(xcolNameOrIndex, collapse = ","),sep="")
		}
	}
	else
	{
			#Error: dataSetName and colname not found
	#cat("\ninValid Dataset\n")
		#Dataset not found
		warning("bsky.plot: Dataset not found..")
		BSkyErrMsg = paste("bsky.plot: Error in Full Plot Dataset not found : ", "DataSetName :", datasetname," ", "Variable Name List :", paste(xcolNameOrIndex, collapse = ","),sep="")
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
			
			# cat("Error caught in bsky.plot \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level Plot function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in bsky.plot \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level bsky.plot function\n")
		return(invisible(BSkyReturnStructure(list(extra=c(imgpath))))) #return (templist);
}

### HISTOGRAM ###
bsky.histogram<-function(colNameOrIndex, dataSetNameOrIndex, width=700, height=700)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("bsky.histogram: Error in Histogram : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("bsky.histogram: Warning in Histogram : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	templist <- list()
	imgpath<-paste(Sys.getenv("TMP"), '\\rimage.png', sep='')
	DataSetIndex = 0
	tryCatch(
		{
	
		withCallingHandlers(
		{
		if( file.exists(imgpath) )
			file.remove(imgpath)
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
	#if dataset index is valid
	if(!is.null(datasetname))
	{
		#cat("\nValid Dataset\n")
colIndex<-BSkyValidateColumn(datasetname, colNameOrIndex)
		#Error: dataSetName and colname not found
		if(colIndex > 0)
		{
			#cat("\nValid Col\n")
			#imgpath<-paste(Sys.getenv("TMP"), '\\rimage.png', sep='')
			png(imgpath, width=width, height=height);
			xarg <- eval(parse(text=paste(datasetname,'[[',colIndex,']]',sep='')))
			
			hist(x=xarg, breaks = 20, col="lightblue", border="red", main="Histogram ", xlab=colNameOrIndex)		
			dev.off();
			
	###creating return list 
	commnd <- paste("bsky.histogram(colNameOrIndex=c('",colNameOrIndex,"')",", dataSetNameOrIndex='",datasetname,"')")
	uas<-list("a","b","c","d","e","f",commnd)
    #templist <- list(filen=imgpath, uasummary=uas, uas1=uas,uas2=uas,uas3=uas,uas4=uas,bb="")	
	templist <- list(filen=imgpath)
	#-return (templist);
		}
		else
		{
			#cat("\ninValid Col\n")
			#Column not found
			warning("bsky.histogram: Column not found..")
			BSkyErrMsg = paste("bsky.histogram: Error in Histogram Column not found : ", "Col name :", colNameOrIndex, sep="")
		}
	}	
	else
	{
	#cat("\ninValid Dataset\n")
		#Dataset not found
		warning("bsky.histogram: Dataset not found..")
		BSkyErrMsg = paste("bsky.histogram: Error in Histogram Dataset not found : ", "DataSetName :", datasetname,sep="")
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
			
			# cat("Error caught in bsky.histogram \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level histogram function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in bsky.histogram \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level histogram function\n")
		return(invisible(BSkyReturnStructure(list(extra=c(imgpath))))) #return (templist);
}


### FULLHISTOGRAM ###
bsky.fullhistogram<-function(colNameOrIndex, dataSetNameOrIndex, .breaks,.freq,.right,.density,.angle,.col,.border,.main,.xlab,.ylab,.axes,.plot,.labels)
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("bsky.fullhistogram: Error in Full Histogram : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyWarnMsg = paste("bsky.fullhistogram: Warning in Full Histogram : ", "DataSetName :", dataSetNameOrIndex," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	templist <- list()
	imgpath<-paste(Sys.getenv("TMP"), '\\rimage.png', sep='')
	DataSetIndex = 0
	tryCatch(
		{
	
		withCallingHandlers(
		{
if( file.exists(imgpath) )
file.remove(imgpath)		
datasetname <- dataSetNameOrIndex #BSkyValidateDataset(dataSetNameOrIndex)
	#if dataset index is valid
	if(!is.null(datasetname))
	{		
colIndex<-BSkyValidateColumn(datasetname, colNameOrIndex)
		#Error: dataSetName and colname not found
		if(colIndex > 0)
		{
		 #cat("Found col")
			#imgpath<-paste(Sys.getenv("TMP"), '\\rimage.png', sep='')
			png(imgpath);
			xarg <- eval(parse(text=paste(datasetname,'[[',colIndex,']]',sep='')))
			#print(xarg)
			hist(x=xarg,breaks=.breaks,freq=.freq,right=.right,density=.density,angle=.angle,col=.col,border=.border,main=.main,xlab=.xlab,ylab=.ylab,axes=.axes,plot=.plot,labels=.labels)
			##hist(x=uadatasets$lst[[DataSetIndex]][[colIndex]], breaks = 20, col="lightblue", border="red", main="Histogram ", xlab=colNameOrIndex)		
			dev.off();
			#cat("device turned off")
				###creating return list 
	commnd <- paste("bsky.fullhistogram(colNameOrIndex=c('",colNameOrIndex,"')",", dataSetNameOrIndex='",datasetname,"')")
	uas<-list("a","b","c","d","e","f",commnd)
    templist <- list(filen=imgpath, uasummary=uas,bb="")	
	#- return (templist);
		}
		else
		{
			cat("\ninValid Col\n")
			#Column not found
			warning("bsky.fullhistogram: Column not found..")
			BSkyErrMsg = paste("bsky.fullhistogram: Error in Full Histogram Column not found : ", "DataSetName :", datasetname," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
		}
	}
	else
	{
	cat("\ninValid Dataset\n")
		#Dataset not found
		warning("bsky.fullhistogram: Dataset not found..")
		BSkyErrMsg = paste("bsky.fullhistogram: Error in Full Histogram Dataset not found : ", "DataSetName :", datasetname," ", "Variable Name List :", paste(colNameOrIndex, collapse = ","),sep="")
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
			
			# cat("Error caught in bsky.fullhistogram \n")
			#BSkyLocalErrorFlagsReset() #if needed
    	}
		#cat("\nWARNING:: top level full histogram function\n")
		if(BSkyLocalWarningFound() == TRUE)
    	{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in bsky.fullhistogram \n")
			BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function
    	}
		BSkyFunctionWrapUp()
		#print(BSkyReturnStructure())
		#cat("Returning return structure from this top level full histogram function\n")
		return(invisible(BSkyReturnStructure(list(extra=c(imgpath)))))
}
