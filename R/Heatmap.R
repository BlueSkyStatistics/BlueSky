	
	### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title HeatMap
#'
#' @description A heat map is a graphical representation of data where the individual values contained in a matrix are represented as colors.  Facets can be optionally created by specifying a factor variable. You can also optionally specify themes, and specify a title and labels for the x and y axis
#'  The dataset must be in the format  column1, column2…columnN where
#' column1: Identifies the subjects e.g. names of people whose the cholesterol values are plotted
#' column2:  Identifies the cholesterol values at time 1
#' column3:  Identifies the cholesterol values at time 2
#' column4:  Identifies the cholesterol values at time 3
#' column5:  Identifies the cholesterol values at time 4…
#' Column1 is a label for each row in the heatmap.
#' For Column 2 onwards, values in each row of the column are represented as colors in the heatmap.
#'
#' @param rowVariable The column variable that contains information about the subjects that the graph is constructed for, see above
#' @param colVariables  a list of column variables that contain values that will be represented as colors in the heatmap
#' @param Colv determines if and how the column dendrogram should be reordered. Has the options as the Rowv argument above and additionally when x is a square matrix, Colv="Rowv" means that columns should be treated identically to the rows.
#' @param Rowv   determines if and how the row dendrogram should be reordered. By default, it is TRUE, which implies dendrogram is computed and reordered based on row means. If NULL or FALSE, then no dendrogram is computed and no reordering is done. If a dendrogram, then it is used "as-is", i.e. without any reordering. If a vector of integers, then dendrogram is computed and reordered based on the order of the vector.
#' @param na.rm logical determining whether NA’s should be removed
#' @param dendrogram character string indicating whether to draw 'none', 'row', 'column' or 'both' dendrograms. Defaults to 'both'. However, if Rowv (or Colv) is FALSE or NULL and dendrogram is 'both', then a warning is issued and Rowv (or Colv) arguments are honored.
#' @param scale character indicating if the values should be centered and scaled in either the row direction or the column direction, or none. The default is "none".
#' @param trace character indicating if the values should be centered and scaled in either the row direction or the column direction, or none. The default is "none".
#' @param density.info character string indicating whether to superimpose a 'histogram', a 'density' plot, or no plot ('none') on the color-key.
#' @param noteForCells logical indicating whether values should be placed in the cells of the heatmap.
#' @param notecol color of notes. If specified values are noted in the heatmap cells in color specified.
#'
#' @return
#'
#' @examples
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