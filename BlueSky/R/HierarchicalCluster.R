BSkyHierClus.old <-function(varsToCluster, method, noOfClusters, distance,plotDendogram, assignClusterToDataset, label="", plotBiplot,dataset, excludeEnvPrefix=FALSE) 
{
BSkyFunctionInit()
	noOfVarsToCluster =length(varsToCluster)
	formulaString=c()
  for (i in 1:noOfVarsToCluster)
  {
  formulaString =c(formulaString, paste(varsToCluster[i]))
  if (i !=noOfVarsToCluster) 
	{
	formulaString = c(formulaString, '+')
	}
  }
  
 #23Jan2016 Adding logic to add .Global if its not there
 dataset1 = dataset
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
  
  modMatrixString = c("model.matrix(~-1 +" ,formulaString,",",dataset,")")
  BSkyModMatrix <<- eval(parse(text=modMatrixString))

BSkyHClust <<- hclust(dist(BSkyModMatrix) , method)
retresults <-summary(as.factor(cutree(BSkyHClust, noOfClusters)))
 xlabVar=   paste ("Observation Number in DataSet " ,dataset1,collapse = ", ")
subVar=paste("Method=", method, "; Distance=", distance,collapse = ", ")

clusterNames =generateClusterNames(noOfClusters)
mdat <- matrix(c(retresults[1],retresults[2]), nrow = 1, ncol = noOfClusters, byrow = FALSE,dimnames = list(c(""), clusterNames))
res2=by(BSkyModMatrix, as.factor(cutree(BSkyHClust, k = noOfClusters)), colMeans)
				
 if (assignClusterToDataset ==TRUE && label =="")
  {
    message =c("Error: You must provide a valid variable name to store the cluster. The variable name is empty")
    df = data.frame(c)
  }
   
   if(assignClusterToDataset ==TRUE && label !="")
   {
	var3 =c(dataset,"$",label,"<<-assignCluster(BSkyModMatrix,",dataset,", cutree(BSkyHClust,k=",noOfClusters,"))"  )
	 var4=paste(var3, collapse = '')
    eval(parse(text=var4))
	}						
retlist=list(mdat,res2)
names(retlist) =c("Cluster sizes","Cluster centroids")
 BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = retlist)

  BSkyFunctionWrapUp()
  
   # obj is the Bsky return structure with all tables stored within it
   obj = BSkyReturnStructure2()
   
   # number of decimal digit formatting from the config option for the numeric values within every cell of every table
   #obj = BSkyDecimalDigitFormating(obj, decimalDigits = 2)
  
  #return(invisible(obj))
return(invisible(retlist))
}

#18Apr2019 some defect fixed by Aaron for BSS ver 6.0 release
BSkyHierClus<-function (varsToCluster, method, noOfClusters, distance, plotDendogram,
    assignClusterToDataset, label = "", plotBiplot, dataset,
    excludeEnvPrefix = FALSE)
{
    BSkyFunctionInit()
    noOfVarsToCluster = length(varsToCluster)
    formulaString = c()
    for (i in 1:noOfVarsToCluster) {
        formulaString = c(formulaString, paste(varsToCluster[i]))
        if (i != noOfVarsToCluster) {
            formulaString = c(formulaString, "+")
        }
    }
    dataset1 = dataset
    if (!excludeEnvPrefix) {
        charcount <- nchar(dataset)
        if (charcount > 11) {
            if (substr(dataset, 1, 11) != ".GlobalEnv$") {
                dataset <- paste(".GlobalEnv$", dataset, sep = "")
            }
        }
        else {
            dataset <- paste(".GlobalEnv$", dataset, sep = "")
        }
    }
    modMatrixString = c("model.matrix(~-1 +", formulaString,
        ",", dataset, ")")
    BSkyModMatrix <<- eval(parse(text = modMatrixString))
    BSkyHClust <<- hclust(dist(BSkyModMatrix), method)
    retresults <- summary(as.factor(cutree(BSkyHClust, noOfClusters)))
    xlabVar = paste("Observation Number in DataSet ", dataset1,
        collapse = ", ")
    subVar = paste("Method=", method, " ;Distance=", distance,
        collapse = ", ")
    class(retresults)
    clusterNames = generateClusterNames(noOfClusters)
    mdat <- matrix(c(retresults[1], retresults[2]), nrow = 1,
        ncol = noOfClusters, byrow = FALSE, dimnames = list(c(""),
            clusterNames))
    mdat <- matrix(retresults, nrow = 1, ncol = noOfClusters,
        byrow = FALSE, dimnames = list(c(""), clusterNames))
    res2 = by(BSkyModMatrix, as.factor(cutree(BSkyHClust, k = noOfClusters)),
        colMeans)
    if (assignClusterToDataset == TRUE && label == "") {
        message = c("Error: You must provide a valid variable name to store the cluster. The variable name is empty")
        df = data.frame(c)
    }
    if (assignClusterToDataset == TRUE && label != "") {
        var3 = c(dataset, "$", label, "<-assignCluster(BSkyModMatrix,",
            dataset, ", cutree(BSkyHClust,k=", noOfClusters,
            "))")
        var4 = paste(var3, collapse = "")
        eval(parse(text = var4))
    }
    retlist = list(mdat, res2)
    names(retlist) = c("Cluster sizes", "Cluster centroids")
    BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = retlist)
    BSkyFunctionWrapUp()
    obj = BSkyReturnStructure2()
    return(invisible(retlist))
}


assignCluster<-function (clusterData, origData, clusterVec) 
{
    rowsDX <- row.names(clusterData)
    rowsX <- row.names(origData)
    clustAssign <- rep(NA, length(rowsX))
    validData <- rowsX %in% rowsDX
    clustAssign[validData] <- clusterVec
    return(as.factor(clustAssign))
}

generateClusterNames <-function(noOfClusters)
{
i=1
clusterstrings=c()
for (i in 1:noOfClusters)
  {
  formulaString =paste("Cluster ", paste(i), collapse=",")
 clusterstrings=c(clusterstrings, formulaString)

  }
clusterstrings
}



plotDendogram<-function(method,distance,dataset)
{
xlabVar=   paste ("Observation Number in DataSet " ,dataset,collapse = ", ")
subVar=paste("Method=", method, "; Distance=", distance,collapse = ", ")

plot(BSkyHClust, main= "Cluster Dendrogram ", xlab= xlabVar, sub=subVar)
	
}


 BSkyHClustBiPlot <-function ( noOfClusters) 
 {
	biplot(princomp(BSkyModMatrix), xlabs = as.character(cutree(BSkyHClust, k = noOfClusters)), main ="Biplot")
	
	# rm(clusters, envir=ptrToGlobalEnvirn)
 }