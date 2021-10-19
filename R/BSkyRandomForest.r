### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Random Forest
#'
#' @description randomForest implements Breiman's random forest algorithm (based on Breiman and Cutler's original Fortran code) for classification and regression. It can also be used in unsupervised mode for assessing proximities among data points.
#'
#' @param x A data frame or a matrix of predictors, or a formula describing the model to be fitted (for the print method, an randomForest object).
#' @param y A response vector. If a factor, classification is assumed, otherwise regression is assumed. If omitted, randomForest will run in unsupervised mode.
#' @param ntree Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
#' @param importance Should importance of predictors be assessed? 
#' @param proximity Should proximity measure among the rows be calculated?
#' @param ... optional parameters to be passed to the low level function randomForest.default.
#'
#' @return An object of class randomForest, which is a list with the following components:
#' call: the original call to randomForest
#' type: one of regression, classification, or unsupervised.
#' predicted: the predicted values of the input data based on out-of-bag samples.
#' importance:	a matrix with nclass + 2 (for classification) or two (for regression) columns. For classification, the first nclass columns are the class-specific measures computed as mean descrease in accuracy. The nclass + 1st column is the mean descrease in accuracy over all classes. The last column is the mean decrease in Gini index. For Regression, the first column is the mean decrease in accuracy and the second the mean decrease in MSE. If importance=FALSE, the last measure is still returned as a vector.
#' importanceSD: The “standard errors” of the permutation-based importance measure. For classification, a p by nclass + 1 matrix corresponding to the first nclass + 1 columns of the importance matrix. For regression, a length p vector.
#' localImp: a p by n matrix containing the casewise importance measures, the [i,j] element of which is the importance of i-th variable on the j-th case. NULL if localImp=FALSE.
#' ntree: number of trees grown.
#' mtry: number of predictors sampled for spliting at each node.
#' forest: (a list that contains the entire forest; NULL if randomForest is run in unsupervised mode or if keep.forest=FALSE.
#' err.rate: (classification only) vector error rates of the prediction on the input data, the i-th element being the (OOB) error rate for all trees up to the i-th.
#' confusion: (classification only) the confusion matrix of the prediction (based on OOB data).
#' votes: (classification only) a matrix with one row for each input data point and one column for each class, giving the fraction or number of (OOB) ‘votes’ from the random forest.
#' oob.times: number of times cases are ‘out-of-bag’ (and thus used in computing OOB error estimate)
#' proximity: if proximity=TRUE when randomForest is called, a matrix of proximity measures among the input (based on the frequency that pairs of data points are in the same terminal nodes).
#' mse	: (regression only) vector of mean square errors: sum of squared residuals divided by n.
#' rsq	: (regression only) “pseudo R-squared”: 1 - mse / Var(y).
#' test: if test set is given (through the xtest or additionally ytest arguments), this component is a list which contains the corresponding predicted, err.rate, confusion, votes (for classification) or predicted, mse and rsq (for regression) for the test set. If proximity=TRUE, there is also a component, proximity, which contains the proximity among the test set as well as proximity between test and training data.
#'
#' @examples
BSkyPrintRandomForest <- function (x, ...) 
{
	BSkyFunctionInit()

    #cat("\nCall:\n", deparse(x$call), "\n")
	m1<-paste(deparse(x$call),collapse='',sep='')
	msg = paste0('Call:\n', m1, '\n\n',collapse='',sep='')
    #cat("               Type of random forest: ", x$type, "\n", sep = "")
    #cat("                     Number of trees: ", x$ntree, "\n", sep = "")
    #cat("No. of variables tried at each split: ", x$mtry, "\n\n", sep = "")
	bkytempdf <- data.frame('Type of random forest'=x$type,'Number of trees'=x$ntree, 'No. of variables tried at each split'=x$mtry)
    if (x$type == "classification") {
        if (!is.null(x$confusion)) {
            #cat("        OOB estimate of  error rate: ", round(x$err.rate[x$ntree, "OOB"] * 100, digits = 2), "%\n", sep = "")
			bkytempdf <- cbind(bkytempdf, 'OOB estimate of  error rate'=paste(round(x$err.rate[x$ntree, "OOB"] * 100, digits = 2), "%", sep = ""))
			
            #cat("Confusion matrix:\n")
            #print(x$confusion)
			
			### IMPORTANT : ASK AARON HOW TO HANDLE FOLLOWING
            if (!is.null(x$test$err.rate)) {
                cat("                Test set error rate: ", round(x$test$err.rate[x$ntree, "Test"] * 100, digits = 2), "%\n", sep = "")
                cat("Confusion matrix:\n")
                print(x$test$confusion)
            }
        }
    }
    if (x$type == "regression") {
        if (!is.null(x$mse)) {
            #cat("          Mean of squared residuals: ", x$mse[length(x$mse)], "\n", sep = "")
            bkytempdf <- cbind(bkytempdf, 'Mean of squared residuals'=paste(x$mse[length(x$mse)], "\n", sep = ""))
			
			#cat("                    % Var explained: ", round(100 * x$rsq[length(x$rsq)], digits = 2), "\n", sep = "")
			bkytempdf <- cbind(bkytempdf, '% Var explained'=paste(round(100 * x$rsq[length(x$rsq)], digits = 2), "\n", sep = ""))
			
			### IMPORTANT : ASK AARON HOW TO HANDLE FOLLOWING
            if (!is.null(x$test$mse)) {
                cat("                       Test set MSE: ", round(x$test$mse[length(x$test$mse)], digits = 2), "\n", sep = "")
                cat("                    % Var explained: ", round(100 * x$test$rsq[length(x$test$rsq)], digits = 2), "\n", sep = "")
            }
        }
		
		### IMPORTANT : ASK AARON HOW TO HANDLE FOLLOWING
        if (!is.null(x$coefs)) {
            cat("  Bias correction applied:\n")
            cat("  Intercept: ", x$coefs[1], "\n")
            cat("      Slope: ", x$coefs[2], "\n")
        }
    }
	BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = bkytempdf,singleTableOutputHeader=msg)
	BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = x$confusion, singleTableOutputHeader="Confusion matrix")
	BSkyFunctionWrapUp()
	# obj is the Bsky return structure with all tables stored within it
    obj = BSkyReturnStructure2()

    # number of decimal digit formatting from the config option for the numeric values within every cell of every table
	# obj = BSkyDecimalDigitFormating(obj, decimalDigits = 2)
	return(invisible(obj))
}

### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Optimal number of trees
#'
#' @description Starting with the default value of mtry, search for the optimal value (with respect to Out-of-Bag error estimate) of ntree (no of trees to grow) for randomForest. The function BSkyMultiRandomForest calls the function randomForest with the arguments defined below
#'
#' @param x a data frame or a matrix of predictors, or a formula describing the model to be fitted (for the print method, an randomForest object).
#' @param y A response vector. If a factor, classification is assumed, otherwise regression is assumed. If omitted, randomForest will run in unsupervised mode.
#' @param startval starting value for ntree argument in randomForest().
#' @param endval ending value for ntree argument in randomForest().
#' @param stepval count by which startval should be incremented in every iteration till it reaches endval value.
#' @param mtry Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)
#'
#' @return Prints the optimal number of trees. Also displays a table with ntree and the corresponding Out-of-Bag error
#'
#' @examples
BSkyMultiRandomForest <- function(x , y=NULL, startval, endval, stepval, mtry)#, proximity) #Aaron: proximity not needed in the Optimal tree dialog.
{
	bskydf = data.frame()
	firstTime=TRUE
	minoob=9999;
	minntree=0
	bskytemp2=character(0)
	for(ntree in seq(startval,endval, by = stepval))
	{
		BSkyRandomForestModelTemp <- randomForest(x = x, y=y, ntree=ntree, mtry=mtry)#, proximity =proximity) 
		
		bskytemp1 <- BSkyRandomForestModelTemp$ntree

		bskytemp2 <- paste(round(BSkyRandomForestModelTemp$err.rate[BSkyRandomForestModelTemp$ntree, "OOB"] * 100, digits = 2), sep = "")
		
		# print(nchar(bskytemp2))
		# cat('@@@')
		# cat('\n ntree =', bskytemp1, ' OOB =', bskytemp2)
		
		if(!exists('bskytemp2') || is.null(bskytemp2) || length(bskytemp2)==0)# this executes if bskytemp2 has no value (happens when dependent var is scale)
		{
			#cat('\nDependent var is scale\n')
			bskydf <- rbind(bskydf, c(as.numeric(bskytemp1), as.numeric('')))
			if(firstTime)
			{
				names(bskydf) <- c("Number of trees", "OOB estimate of  error rate %")
				firstTime=FALSE
			}	
			minoob=NA
		}
		else
		{
			#cat('\nDependent var is factor\n')
			bskydf <- rbind(bskydf, c(as.numeric(bskytemp1), as.numeric(bskytemp2)))
			if(firstTime)
			{
				names(bskydf) <- c("Number of trees", "OOB estimate of  error rate %")
				firstTime=FALSE
			}
			
			#find mini OOB
			if(as.numeric(bskytemp2) < minoob)
			{
				minntree = as.numeric(bskytemp1)
				minoob = as.numeric(bskytemp2)
			}		
		}
		

	}
	cat('\nResult: ntree =', minntree, 'Min OOB =', minoob)
	return(bskydf)
}

RF.proximity.predictor <- function(rfmodel, proximity, newdsname, predictor, newcolname, datasetname)
{
	# Loading newly created Proximity dataset
	if(proximity && !is.null(newdsname) && length(trimws(newdsname)) > 0)
	{
		eval(parse(text=paste(newdsname,' <<- as.data.frame( rfmodel$proximity )',sep=''))) 
	}
	#BSkyLoadRefreshDataframe(newds, proximity )

	#Adding Predictor col to existing dataset
	if(predictor)
	{
		eval(parse(text=paste(newcolname,' <- rfmodel$predicted',sep='' ))) 
		#datasetname <- cbind( datasetname}, newcolname)
		eval(parse(text=paste(datasetname,'<<- cbind(',datasetname,', ',newcolname,')', sep='' )))
	}
	return(NULL)
}



# Loading newly created Proximity dataset
# if(length(trimws('{{newds}}')) >0)
# {
# {{newds}} <- as.data.frame( BSkyRandomForestModel$proximity ) 
# }
# BSkyLoadRefreshDataframe({{newds}}, {{proximity}} )

# #Adding Predictor col to existing dataset
# if({{predictor}})
# {
# {{newcolname}} <- BSkyRandomForestModel$predicted 
# {{%DATASET%}} <- cbind( {{%DATASET%}}, {{newcolname}})

# }
# BSkyLoadRefreshDataframe({{%DATASET%}}, {{predictor}})