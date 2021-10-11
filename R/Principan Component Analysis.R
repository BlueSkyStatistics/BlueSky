
BSkyPrinCompAnalysis <-function (vars,cor=FALSE,componentsToRetain=0, generateScreeplot=FALSE,prefixForComponents="", dataset, excludeEnvPrefix=FALSE)

{
	BSkyFunctionInit()
	count =length(vars)
	result=c()
	
	if (componentsToRetain !=0)
	{
		if(prefixForComponents =="")
		{
			errorMessage="If you have selected to retain components, you need to enter a variable name prefix. We will save the components in a new variable name"
			#print("If you have selected factor scores, you need to enter a variable name prefix. We will save the scores in a new variable name")
			BSkyBuildReturnTableStructure2(errorMessage)
			BSkyFunctionWrapUp()
			obj = BSkyReturnStructure2()
			# number of decimal digit formatting from the config option for the numeric values within every cell of every table
			#obj = BSkyDecimalDigitFormating(obj, decimalDigits = 2)
			return(invisible(obj))
		}
	}
		
	for (i in 1:count)
	{
		result =c(result, paste(vars[i]))
		if (i !=count) result = c(result, '+')
	}
	
 #23Jan2016 Adding logic to add .Global if its not there
	if(!excludeEnvPrefix )
	{
		charcount <- nchar(dataset)
		if(charcount > 11) # .GlobalEnv$
		{
			if(substr(dataset, 1,11)!= '.GlobalEnv$')
			{
				dataset <- paste(".GlobalEnv$",dataset, sep='')
			}
		}
		else
		{
			dataset <- paste(".GlobalEnv$",dataset, sep='')
		}
	}	
	
	stringForPCACommand = c("princomp(~", result,",","cor=",cor,",",	"data=", dataset,")" )
 
	BSkyPCA <<- eval(parse(text=stringForPCACommand))
	
	#Getting the component loadings
	matrixComptLoad =unclass(loadings(BSkyPCA))
	listOfMatrixCompLoad=list(matrixComptLoad)
	names(listOfMatrixCompLoad)="Component Loadings"
	BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = listOfMatrixCompLoad)
	
	#Getting the component variances
	BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = BSkyPCA$sd^2, singleTableOutputHeader="Component Variances")
	
	#Printing the summary of the model
	i=10
	BSkyPrintSummaryPrincomp(i)	
	
	#Saving the component scores back to the dataset
	
	countOfComponents=1
	if ((componentsToRetain > 0) &&(prefixForComponents !="") )
	{
			rowNamesSyntax= c("row.names(", dataset,")")
			rowNames = eval(parse(text=rowNamesSyntax))
			componentsToBeAssignedWithNAs <- rep(NA, length(rowNames))
			while (countOfComponents <=componentsToRetain)
			{
				namesOfScores =c("names(","BSkyPCA$scores[,",countOfComponents,"]",")")
				namesWithValidComponents = eval(parse(text=namesOfScores))
				validData <- rowNames %in% namesWithValidComponents
				componentsToBeAssignedWithNAs[validData] <- BSkyPCA$scores[,countOfComponents]
				#vartemp =paste(dataset, "<<-within(", dataset, ",{",varNameForCluster, "<-scoresVector})") 
				varNameForComponents =paste(prefixForComponents,countOfComponents,sep="")
				
				var3 =c(dataset,"$",varNameForComponents,"<-componentsToBeAssignedWithNAs" )
				var4=paste(var3, collapse = '')
				eval(parse(text=var4))
				countOfComponents=countOfComponents+1
			}
	}

	#Plot Screeplot
	if (generateScreeplot)
	{
		screeplot(BSkyPCA, main='Screeplot')
	}
	
	#if (exists("BSkyPCA") )
	#{
		#rm(BSkyPCA,inherits=TRUE)
	#}
	
	BSkyFunctionWrapUp()
  
   # obj is the Bsky return structure with all tables stored within it
   obj = BSkyReturnStructure2()
   
   # number of decimal digit formatting from the config option for the numeric values within every cell of every table
 #  obj = BSkyDecimalDigitFormating(obj, decimalDigits = 2)
  
  return(invisible(obj))
}


##Access original source code of function at getAnywhere(print.summary.princomp) 
BSkyPrintSummaryPrincomp <-function ( i=0) 
{
    	BSkyFunctionInit()
	vars <- (BSkyPCA$sdev)^2
    vars <- vars/sum(vars)
    #cat("Importance of components:\n")
    #print(rbind(`Standard deviation` = BSkyPCA$sdev, `Proportion of Variance` = vars, 
     #   `Cumulative Proportion` = cumsum(vars)))
	
	matrixSummary =rbind(`Standard deviation` = BSkyPCA$sdev, `Proportion of Variance` = vars, `Cumulative Proportion` = cumsum(vars))
	matrixSummaryList =list(matrixSummary)
	names(matrixSummaryList)="Importance of components"
	BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = matrixSummaryList)
	
	 BSkyFunctionWrapUp()
	return (TRUE);
	
	#Aaron: I have commented this from original source code as its not used
	#loadings <-BSkyPCA$print.loadings
    #if (loadings!=NULL) {
        #cat("\nLoadings:\n")
        #cx <- format(round(BSkyPCA$loadings, digits = digits))
        #cx[abs(BSkyPCA$loadings) < cutoff] <- paste(rep(" ", nchar(cx[1, 
            #1], type = "w")), collapse = "")
        #print(cx, quote = FALSE, ...)
  }

