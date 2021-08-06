BSkyKMeans <-function (vars, centers, iter.max = 10, num.seeds = 10,storeClusterInDataset =FALSE, varNameForCluster="", dataset, excludeEnvPrefix=FALSE) 
 {
  BSkyFunctionInit()
  count =length(vars)
  result=c()
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
  
  var2 = c("model.matrix(~-1 +" ,result,",",dataset,")")
uadatasets$BSkyModMatrix=NULL
  uadatasets$BSkyModMatrix <- eval(parse(text=var2))
 
  if (mode(uadatasets$BSkyModMatrix) == "numeric") uadatasets$BSkyModMatrix <- data.frame(new.BSkyModMatrix = uadatasets$BSkyModMatrix)
  uadatasets$BSkyKM <- kmeans(x = uadatasets$BSkyModMatrix, centers = centers, iter.max = iter.max)
  for (i in 2:num.seeds) 
    {
   newKM <- kmeans(x = uadatasets$BSkyModMatrix, centers = centers, iter.max = iter.max)
   if (sum(newKM$withinss) < sum(uadatasets$BSkyKM$withinss))
   {
    uadatasets$BSkyKM <- newKM
   }
  }
  uadatasets$BSkyKM$tot.withinss <- sum(uadatasets$BSkyKM$withinss)
  xmean <- apply(uadatasets$BSkyModMatrix, 2, mean)
  centers <- rbind(uadatasets$BSkyKM$centers, xmean)
  bss1 <- as.matrix(dist(centers)^2)
  uadatasets$BSkyKM$betweenss <- sum(as.vector(bss1[nrow(bss1), ]) * c(uadatasets$BSkyKM$size,  0))
  
  if (storeClusterInDataset ==TRUE && varNameForCluster =="")
  {
    message =c("Error: You must provide a valid variable name to store the cluster. The variable name is empty")
    df = data.frame(c)
  }
   
   if(storeClusterInDataset ==TRUE && varNameForCluster !="")
   {
     var3 =c(dataset,"$",varNameForCluster,"<-clustAssign" )
    #var2 = c("model.matrix(~-1 +" ,result,",",dataset,")")
    test= c("row.names(", dataset,")")
    rowsX = eval(parse(text=test))
    
    best =c("names(","uadatasets$BSkyKM$cluster",")")
    rowsDX = eval(parse(text=best))
    
    clustAssign <- rep(NA, length(rowsX))
    validData <- rowsX %in% rowsDX
    clustAssign[validData] <- uadatasets$BSkyKM$cluster
    clustAssign =as.factor(clustAssign)
   
    #Store the cluster centers in a global variable so that biplot can access it
    
    #ptrToGlobalEnvirn$clusters=KM
	
	#biplot(princomp(ptrToGlobalEnvirn$x), xlabs = as.character(ptrToGlobalEnvirn$clusters$cluster))
   
    var4=paste(var3, collapse = '')
    eval(parse(text=var4))
   }


  # Creating a matrix to hold the within cluster sum of squares
  withinss=as.matrix(uadatasets$BSkyKM$withinss,nrow=2,ncol=1)
  colnames(withinss)="Within cluster sum of sq"
  
 #Creating a matrix to store swithin and between cluster sum of squares
  sumsqmatrix=matrix(c(uadatasets$BSkyKM$tot.withinss,uadatasets$BSkyKM$betweenss),nrow=1,ncol=2)
  colnames(sumsqmatrix)  =c("Total Within Sum of Squares","Between Cluster Sum of Squares")
  rownames(sumsqmatrix)="";
#Added 06/14/2015
#Naming the names of the columns appropriately (we are removing the model name prefix
#colnames(BSkyKM$centers) <<-vars


for (j in 1:count)
{
colnames(uadatasets$BSkyKM$centers)[j] =vars[j]
}

 #Creating a list of tables to display in the output
 if (storeClusterInDataset ==TRUE && varNameForCluster =="")
  {
    listoftables = list(withinss,uadatasets$BSkyKM$centers,sumsqmatrix,df)
    names(listoftables) =c("Within Cluster Sum of Squares","Cluster centroids","Sum of Squares", "Error")
 } 
 else
 {
  listoftables = list(withinss,uadatasets$BSkyKM$centers,sumsqmatrix)

  names(listoftables) =c("Within Cluster Sum of Squares","Cluster centroids","Sum of Squares")
 }
#if (plot==TRUE)biplot(princomp(BSkyModMatrix), xlabs = as.character(BSkyKM$cluster))

 #Passing the list of tables to the BSky function to ensure that each table/matrix is displayed as a properly formatted table in the output
  BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = listoftables)
	# uadatasets$BSkyKM <-NULL;
	# uadatasets$BSkyModMatrix=NULL
	# rm(BSkyKM, envir=uadatasets)
	# rm(BSkyModMatrix, envir=uadatasets)
 
 BSkyFunctionWrapUp()
  
   # obj is the Bsky return structure with all tables stored within it
   obj = BSkyReturnStructure2()
   
   # number of decimal digit formatting from the config option for the numeric values within every cell of every table
   obj = BSkyDecimalDigitFormating(obj, decimalDigits = 2)
  
  return(invisible(obj))
  
   
 }


 BSkyBiPlot <-function (vars, dataset) 
 {
	biplot(princomp(uadatasets$BSkyModMatrix), xlabs = as.character(uadatasets$BSkyKM$cluster),main="Biplot")
	# rm(clusters, envir=ptrToGlobalEnvirn)
	uadatasets$BSkyKM <-NULL;
	uadatasets$BSkyModMatrix=NULL
	rm(BSkyKM, envir=uadatasets)
	rm(BSkyModMatrix, envir=uadatasets)
 }

