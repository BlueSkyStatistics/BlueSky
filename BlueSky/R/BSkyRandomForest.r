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