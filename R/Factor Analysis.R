
BSkyFactorAnalysis.old <-function (vars, autoextraction =TRUE,factors=1, rotation="varimax", saveScores=FALSE,screeplot=FALSE,scores="none" ,prefixForScores="", dataset, excludeEnvPrefix=FALSE) 
 {
  BSkyFunctionInit()
  count =length(vars)
  result=c()
  countOfFactors =1
 if (saveScores ==TRUE)
{
 if ((scores =="Bartlett" || scores =="Regression"))
  {
	if(prefixForScores =="")
	{ f
		errorMessage="ERROR: You need to enter a variable name prefix as you have selected factor scores. We will save the scores in a new variable name"
		#print("If you have selected factor scores, you need to enter a variable name prefix. We will save the scores in a new variable name")
		BSkyBuildReturnTableStructure2(errorMessage)
		
		BSkyFunctionWrapUp()
		obj = BSkyReturnStructure2()
		# number of decimal digit formatting from the config option for the numeric values within every cell of every table
		#obj = BSkyDecimalDigitFormating(obj, decimalDigits = 2)
		return(invisible(obj))
	}
  }
}

  for (i in 1:count)
  {
  result =c(result, paste(vars[i]))
  if (i !=count) 
{
result = c(result, '+')
}
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
  
if (autoextraction ==TRUE || screeplot ==TRUE)
{

#paste( "eigen(cor(", dataset, "[,", deparse(vars,width.cutoff = 500), "],", " use=""complete.obs""))" 
#ev <- eigen(cor(vars,use="complete.obs")) # get eigenvalues
ev =eval(parse(text =paste( "eigen(cor(", dataset, "[,", deparse(vars,width.cutoff = 500), "],", " use=\"complete.obs\"))" )))
}
if (autoextraction == TRUE )
{
factors = length(which(ev$values >=1))
}



  #var2 = c("model.matrix(~-1 +" ,result,",",dataset,")")

 # .FA <- factanal(~accel+engine+horse, factors=1, rotation="varimax", scores="Bartlett", data=cars1)

#list the unrotated and rotated loadings
if (rotation != "none")
{
     # list the un rotated loadings
     originalRotation =rotation;
     rotation ="none"
     var2 = c("factanal(~", result,",","factors=",factors,",","rotation=", deparse(rotation),",","scores=",deparse(scores), ",","data=", dataset,")" )
 
     BSkyFA <<- eval(parse(text=var2))
listOfMatrices <-BSkyPrint.factanal(BSkyFA, rotation )

     rotation =originalRotation

      #list the rotated loadings

     var2 = c("factanal(~", result,",","factors=",factors,",","rotation=", deparse(rotation),",","scores=",deparse(scores), ",","data=", dataset,")" )
      BSkyFA <<- eval(parse(text=var2))
listOfMatrices <-BSkyPrint.factanal(BSkyFA,rotation )
 }
else
{
var2 = c("factanal(~", result,",","factors=",factors,",","rotation=", deparse(rotation),",","scores=",deparse(scores), ",","data=", dataset,")" )
      BSkyFA <<- eval(parse(text=var2))
listOfMatrices <-BSkyPrint.factanal(BSkyFA,rotation )
}
#list the un rotated loadings
	if ((scores =="Bartlett" || scores =="Regression")&&(prefixForScores !="") )
	{
				
			rowNamesSyntax= c("row.names(", dataset,")")
			rowNames = eval(parse(text=rowNamesSyntax))
			scoresToBeAssignedWithNAs <- rep(NA, length(rowNames))
			while (countOfFactors <=factors)
			{
				namesOfScores =c("names(","BSkyFA$scores[,",countOfFactors,"]",")")
				namesWithValidScores = eval(parse(text=namesOfScores))
				validData <- rowNames %in% namesWithValidScores
				scoresToBeAssignedWithNAs[validData] <- BSkyFA$scores[,countOfFactors]
				#vartemp =paste(dataset, "<<-within(", dataset, ",{",varNameForCluster, "<-scoresVector})") 
				varNameForScores =paste(prefixForScores,countOfFactors,sep="")
				
				var3 =c(dataset,"$",varNameForScores,"<-scoresToBeAssignedWithNAs" )
				var4=paste(var3, collapse = '')
				eval(parse(text=var4))
				countOfFactors=countOfFactors+1
			}
	}


	
	if (screeplot)
	{
		#screeplot.factanal(BSkyFA)
                        
                  plot(ev$values, type="b", main="Eigen values", ylab="Eigen Values",xlab="Factor(s)")
                  abline(h=1,lty=3)
#text(1,1, "Cutoff for factors", col = "gray60", adj = c(0, -.1))
legend("topright", c("Cutoff for factors"),lty=3)
	}




 
	#Passing the list of tables to the BSky function to ensure that each table/matrix is displayed as a properly formatted table in the output
	# BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = listoftables)
	#print(BSkyFA)
	#if (exists("BSkyFA") )
	#{
		#rm(BSkyFA,inherits=TRUE)
		#}
	#if (exists("BSkyFA") ) print(BSkyFA)
  BSkyFunctionWrapUp()
  
   # obj is the Bsky return structure with all tables stored within it
   obj = BSkyReturnStructure2()
   
   # number of decimal digit formatting from the config option for the numeric values within every cell of every table
 #  obj = BSkyDecimalDigitFormating(obj, decimalDigits = 2)
  
  return(invisible(obj))
  
   
 }


 
#26Jun2018 Aaron modified for Daniel's issue (limit to 33 variables)
#27Aug2018 Aaron modified for Daniel's issue (cannot calulate factor scores)
### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Factor Analysis
#'
#' @description Perform maximum-likelihood factor analysis on a covariance matrix or data matrix and generates a screeplot. Calls the function factanal in the stats package.
#'
#' @param vars One or more numeric variables to extract factors from.
#' @param autoextraction Automatically determine the number factors or extract specific numbers of factors.
#' @param screeplot If TRUE generates a screeplot.
#' @param rotation determine the type of rotation and takes one of the values (none, quartimax, geominT, varimax, oblimin, simplimax, promax, geominQ and bentlerQ)
#' @param saveScores saves the factor scores in the dataset
#' @param dataset The dataset from which the 'vars' have been picked.
#'
#' @return
#'
#' @examples
BSkyFactorAnalysis <-function (vars, autoextraction = TRUE, factors = 1, rotation = "varimax", 
    saveScores = FALSE, screeplot = FALSE, scores = "none", prefixForScores = "", 
    dataset, excludeEnvPrefix = FALSE) 
{
    BSkyFunctionInit()
    count = length(vars)
    result = c()
    countOfFactors = 1
    if (saveScores == TRUE) {
        if ((scores == "Bartlett" || scores == "regression")) {
            if (prefixForScores == "") {
                f
                errorMessage = "ERROR: You need to enter a variable name prefix as you have selected factor scores. We will save the scores in a new variable name"
                BSkyBuildReturnTableStructure2(errorMessage)
                BSkyFunctionWrapUp()
                obj = BSkyReturnStructure2()
                return(invisible(obj))
            }
        }
    }
    for (i in 1:count) {
        result = c(result, paste(vars[i]))
        if (i != count) {
            result = c(result, "+")
        }
    }
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
    if (autoextraction == TRUE || screeplot == TRUE) {
        ev = eval(parse(text = paste("eigen(cor(", dataset, "[,", 
            paste(deparse(vars), sep = "", collapse = ""), "],", 
            " use=\"complete.obs\"))")))
    }
    if (autoextraction == TRUE) {
        factors = length(which(ev$values >= 1))
    }
    if (rotation != "none") {
        originalRotation = rotation
        rotation = "none"
        var2 = c("factanal(~", result, ",", "factors=", factors, 
            ",", "rotation=", deparse(rotation), ",", "scores=", 
            paste(deparse(scores), sep = "", colapse = ""), ",", 
            "data=", dataset, ")")
        BSkyFA <<- eval(parse(text = var2))
        listOfMatrices <- BSkyPrint.factanal(BSkyFA, rotation)
        rotation = originalRotation
        var2 = c("factanal(~", result, ",", "factors=", factors, 
            ",", "rotation=", deparse(rotation), ",", "scores=", 
            deparse(scores), ",", "data=", dataset, ")")
        BSkyFA <<- eval(parse(text = var2))
        listOfMatrices <- BSkyPrint.factanal(BSkyFA, rotation)
    }
    else {
        var2 = c("factanal(~", result, ",", "factors=", factors, 
            ",", "rotation=", deparse(rotation), ",", "scores=", 
            paste(deparse(scores), sep = "", collapse = ""), 
            ",", "data=", dataset, ")")
        BSkyFA <<- eval(parse(text = var2))
        listOfMatrices <- BSkyPrint.factanal(BSkyFA, rotation)
    }
    if ((scores == "Bartlett" || scores == "regression") && (prefixForScores != 
        "")) {
        rowNamesSyntax = c("row.names(", dataset, ")")
        rowNames = eval(parse(text = rowNamesSyntax))
        scoresToBeAssignedWithNAs <- rep(NA, length(rowNames))
        while (countOfFactors <= factors) {
            namesOfScores = c("names(", "BSkyFA$scores[,", countOfFactors, 
                "]", ")")
            namesWithValidScores = eval(parse(text = namesOfScores))
            validData <- rowNames %in% namesWithValidScores
            scoresToBeAssignedWithNAs[validData] <- BSkyFA$scores[, 
                countOfFactors]
            varNameForScores = paste(prefixForScores, countOfFactors, 
                sep = "")
            var3 = c(dataset, "$", varNameForScores, "<-scoresToBeAssignedWithNAs")
            var4 = paste(var3, collapse = "")
            eval(parse(text = var4))
            countOfFactors = countOfFactors + 1
        }
    }
    if (screeplot) {
        plot(ev$values, type = "b", main = "Eigen values", ylab = "Eigen Values", 
            xlab = "Factor(s)")
        abline(h = 1, lty = 3)
        legend("topright", c("Cutoff for factors"), lty = 3)
    }
    BSkyFunctionWrapUp()
    obj = BSkyReturnStructure2()
    return(invisible(obj))
}





BSkyPrint.factanal <-function (x, rotation, digits = 3, ...) 
{
	BSkyFunctionInit()
   # cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
   # cat("Uniquenesses:\n")
   # print(round(x$uniquenesses, digits), ...)
	
	
#	uniqueness =as.matrix(round(x$uniquenesses, digits))
	uniqueness =round(x$uniquenesses, digits)
	#colnames(uniqueness) =c("Values")
	#names(uniqueness) =c("Values")
	#uniquenessList =list(uniqueness)
	#names(uniquenessList) ="Uniqueness"
     if (rotation =="none")
               {
singleTableOutputHeader="Uniqueness (un-rotated)"
 #names(fxList) ="Loadings(Un-rotated )"
               }
               else
               {
               singleTableOutputHeader=paste("Uniqueness", "(", rotation, "rotation)")
               #names(fxList) =paste("Rotated Loadings" ,"(" ,rotation,")")
               }

	BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = uniqueness, singleTableOutputHeader)
    loadingsAndFactors =BSkyPrint.loadings(x$loadings, rotation, digits = digits, ...)
	
    if (!is.null(x$rotmat)) 
	{
        tmat <- solve(x$rotmat)
        R <- tmat %*% t(tmat)
        factors <- x$factors
        rownames(R) <- colnames(R) <- paste0("Factor", 1:factors)
        if (TRUE != all.equal(c(R), c(diag(factors)))) 
		{
            #cat("\nFactor Correlations:\n")
            #print(R, digits = digits, ...)
			listForR =list(R)
			names(listForR) ="Factor Correlations";
			BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = listForR)
			
        }
    }
    if (!is.null(x$STATISTIC)) 
	{
        factors <- x$factors
        stringToPrint =paste("Test of the hypothesis that", factors, if (factors == 1) "factor is" else "factors are", "sufficient.")
		BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = stringToPrint,singleTableOutputHeader="")
	    stringToPrint=paste("The chi square statistic is", round(x$STATISTIC, 2), "on", x$dof, if (x$dof == 1) "degree" else "degrees", "of freedom.\nThe p-value is", signif(x$PVAL, 3))
		BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = stringToPrint,singleTableOutputHeader="")
		rownames=c("Chi-sq statistic","Degress of freedom","P-value")
		colnames=c("Values")
		matrixForHypTest =matrix(nrow=3,ncol=1, dimnames =list(rownames,colnames))
		matrixForHypTest[1,1]=round(x$STATISTIC, 2)
		matrixForHypTest[2,1]=x$dof
		matrixForHypTest[3,1]=signif(x$PVAL, 3)
		tabList = list(matrixForHypTest)
		names(tabList) = c("Hypothesis test")

		BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = tabList)
	}
    else 
	{
        stringToPrint =paste("The degrees of freedom for the model is", x$dof, "and the fit was", round(x$criteria["objective"], 4))
		BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = stringToPrint,singleTableOutputHeader="")
			
		rownames=c("Degress of freedom","Fit")
		colnames=c("Values")
		matrixForHypTest =matrix(nrow=2,ncol=1, dimnames =list(rownames,colnames))
		matrixForHypTest[1,1]=x$dof
		matrixForHypTest[2,1]=round(x$criteria["objective"])
		tabList = list(matrixForHypTest)
		names(tabList) = c("Hypothesis test")
		BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = tabList)
		#	tabList = list(matrixForHypTest)
		#names(tabList) = c("blah1")
	
		#BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = tabList)
	
	#resultList =list(uniqueness,loadingsAndFactors[[1]],loadingsAndFactors[[2]])
	}
	BSkyFunctionWrapUp()
	#return(uniquenessLoadingsAndFactors)
	
	
    invisible(x)
}





BSkyPrint.loadings<-function (x, rotation, digits = 3L, cutoff = 0.1, sort = FALSE, ...) 
{
    BSkyFunctionInit()
	Lambda <- unclass(x)
    p <- nrow(Lambda)
    factors <- ncol(Lambda)
    if (sort) {
        mx <- max.col(abs(Lambda))
        ind <- cbind(1L:p, mx)
        mx[abs(Lambda[ind]) < 0.5] <- factors + 1
        Lambda <- Lambda[order(mx, 1L:p), ]
    }
   
	# cat("\nLoadings:\n")
    fx <- setNames(format(round(Lambda, digits)), NULL)
    nc <- nchar(fx[1L], type = "c")
    fx[abs(Lambda) < cutoff] <- paste(rep(" ", nc), collapse = "")
    #print(fx, quote = FALSE, ...)
    vx <- colSums(x^2)
    varex <- rbind(`SS loadings` = vx)
    if (is.null(attr(x, "covariance"))) {
        varex <- rbind(varex, `Proportion Var` = vx/p)
        if (factors > 1) 
		{
            varex <- rbind(varex, `Cumulative Var` = cumsum(vx/p))
			}
    }
   # cat("\n")
    #print(round(varex, digits))
	#loadingsAndFactors=list(fx,round(varex, digits))
	fxList =list(fx)
               if (rotation =="none")
               {
	names(fxList) ="Loadings(Un-rotated )"
               }
               else
               {
               names(fxList) =paste("Rotated Loadings" ,"(" ,rotation," rotation)")
               }
	BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = fxList)
	varexList =list(round(varex, digits))


	

               if (rotation =="none")
               {
                  names(varexList) ="Factors (Un-rotated)"
                  
               }
               else
               {
                    names(varexList) =paste("Factors" ,"(" ,rotation, " rotation)")
               }





	BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = varexList)
	BSkyFunctionWrapUp()
    #invisible(x)
	#return(loadingsAndFactors)
}


screeplot.factanal <- function(fa.fit,xlab="factor",ylab="eigenvalue",...) {
	# sum-of-squares function for repeated application
	sosq <- function(v) {sum(v^2)}
	# Get the matrix of loadings
	my.loadings <- as.matrix(fa.fit$loadings)
	# Eigenvalues can be recovered as sum of
	# squares of each column
	evalues <- apply(my.loadings,2,sosq)
	plot(evalues,xlab=xlab,ylab=ylab,...)
}





