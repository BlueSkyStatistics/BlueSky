	
	BSkyHeatMap <- function(rowVariable, colVariables, main ="Heatmap",Colv=FALSE, Rowv=TRUE, dendrogram="row",  scale="none",noteForCells	=FALSE, notecol="cyan", density.info=c("none"),trace="none", na.rm=TRUE, dataset, excludeEnvPrefix=FALSE)
	{
		BSkyFunctionInit()
		
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
		
		
		varForMatrixGen=c( "data.matrix(", dataset, "[,", deparse(colVariables), "])")
		dataMatrix =eval(parse(text=varForMatrixGen))
		varForRowNames =paste(dataset, "[,c(", deparse(rowVariable), ")]")
		rownames(dataMatrix) = eval(parse(text=varForRowNames))
		#heatmap.2(dataMatrix, hclustfun = hclust,distfun=dist,Colv, Rowv, dendogram, cellnote=dataMatrix, trace, na.rm=TRUE, reorderfun = function(d, w) reorder(d, w),
        #   symm = FALSE,scale="row")
		if (noteForCells)
		{
		heatmap.2(dataMatrix,dendrogram=dendrogram,main=main,na.rm=na.rm,Colv=Colv,Rowv=Rowv,cellnote =dataMatrix, notecol=notecol, density.info =density.info,scale=scale,trace=trace)
		}
		else
		{
		heatmap.2(dataMatrix,dendrogram=dendrogram,main=main,na.rm=na.rm,Colv=Colv,Rowv=Rowv, density.info=density.info, scale=scale,trace=trace)
		}
		#rnames <- data[,1
		#mat_data <- data.matrix(data[,2:ncol(data)])
		BSkyFunctionWrapUp()
  
   # obj is the Bsky return structure with all tables stored within it
		#obj = BSkyReturnStructure2()
   
   # number of decimal digit formatting from the config option for the numeric values within every cell of every table
	   #obj = BSkyDecimalDigitFormating(obj, decimalDigits = 2)
	  
		#invisible(obj)	
		
		
	}