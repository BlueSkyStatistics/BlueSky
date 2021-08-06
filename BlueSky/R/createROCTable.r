createROCTable <- function( predictedprobs, dependentvariable, modelname, datasetname)
{
i=1
predictions =NULL
results=NULL
indepVars =getModelIndependentVariables (modelname)
modclass = character(0)
length = eval( parse (text =paste("nrow(", datasetname, ")", sep="", collapse="") ))

for ( i in 1:length)
{
    cutoff = predictedprobs[i]
    modclass = getModelClass(modelname)
    if (is.null(modclass)) 
    {
        msg = paste0("ERROR: Could not find model class. Further processing cantnot be done. Please email BlueSky Statistics support support@blueskystatistics.com with related information", 
            collapse = "", sep = "")
        print(msg)
        return(NULL)
    }
   
    dependentclass = character(0)
    depvar <- getModelDependentVariable(modelname)
    if (!is.null(depvar)) 
    {
        #dependentvariable <- eval(parse(text = paste(datasetname,      "$", depvar, sep = "")))
        dependentclass = eval(parse(text = paste("class(dependentvariable)", 
            sep = "")))
    }

    if (dependentclass =="numeric" || dependentclass=="integer" || dependentclass =="double" )
    {
        predictions = ifelse(predictedprobs >= cutoff, 1, 0)
    }

	# if ((dependentclass =="factor" ||dependentclass =="string") && modelclass ="xgb.Booster")
	# {
	# predictions = ifelse(predictedprobs >= cutoff, levels(dependentvariable)[1], 
					# levels(dependentvariable)[2])
	# #predictions =as.factor(predictions)
	# }
	# else
	if (dependentclass =="factor" || dependentclass =="ordered" )
	{
	predictions = ifelse(predictedprobs >= cutoff, levels(dependentvariable)[2], 
					levels(dependentvariable)[1])
	}
	if (dependentclass =="logical" )
    {
        predictions = ifelse(predictedprobs >= cutoff, TRUE, FALSE)
    }
  #  depvar <- getModelDependentVariable(modelname)
   # dependentvariable <- eval(parse(text = paste(datasetname, "$", depvar, sep = "")))
    if (is.null(dependentvariable))
            {
                msg = paste0("Confusion matrix cannot be created as the dependent variable is missing in the dataset", 
                  collapse = "", sep = "")
                cat("\n", msg)
                return
            }
	
	# ####################################################################################################
# #The code below is duplicated in the ROC table and Predict.r
# ####################################################################################################
	
	# predictionsclass = eval(parse(text = paste("class(predictions)", sep = "")))
					# if ((dependentclass =="logical" || dependentclass =="numeric"  || dependentclass =="integer"))
                    # {
                        # predictions =factor(predictions, levels =levels(dependentvariable))
                        # dependentvariable =factor(dependentvariable, levels)
                    # }
# #This handles the fact that predictions is a factor and dependent variable is a string
					# else if (dependentclass =="character" &&  predictionsclass =="factor" && (modclass=="rpart"|| modclass =="randomForest"|| modclass=="multinom"))
					# {
						# dependentvariable =as.factor(dependentvariable)
					# }
                    # else if (dependentclass =="factor" &&  predictionsclass =="character" && modclass=="glm" && fly =="binomial")
                    # {
                        # predictions =as.factor(predictions)
                        # dependentvariable =as.factor(dependentvariable)
                    # }
					 # else if (dependentclass =="character" &&  predictionsclass =="character" && (modclass=="nnet" || modclass =="nnet.formula"))
                    # {
                        # predictions =as.factor(predictions)
                        # dependentvariable =as.factor(dependentvariable)
                    # }
	
# ####################################################################################################

	#Added this code 06/01/2020
	#xgb.booster will only work with dependent variable is numeric
	# the code below factor(predictions, levels=levels(dependentvariable)) will NOT work for numerics as factor(dependentvariable) is NULL
	
	if (modclass == "xgb.Booster") {
            dependentvariable = as.factor(dependentvariable)
			 #This ensures that the levels in the predictions and dependent variable match
            predictions = factor(predictions, levels = levels(dependentvariable))
				positive=levels(dependentvariable)[2]
            bskyconfmatrix <- caret::confusionMatrix(predictions,
                dependentvariable, positive=positive)
        }
        else {
			positive=levels(dependentvariable)[2]
            bskyconfmatrix <- caret::confusionMatrix(factor(predictions,
                levels = levels(dependentvariable)), factor(dependentvariable,
                levels = levels(dependentvariable)),positive=positive)
        }
	
	# if (modclass =="xgb.Booster")
	# {
	   # dependentvariable =as.factor(dependentvariable)
	   # #This ensures that the levels in the predictions and dependent variable match
		# predictions =factor(predictions, levels =levels(dependentvariable))
		# #bskyconfmatrix <- caret::confusionMatrix(predictions, dependentvariable,positive=tail (levels(dependentvariable),1)) 
	# }	
	
	#predictions =factor (predictions, levels =rev(levels(dependentvariable)))
	#dependentvariable=factor (dependentvariable, levels =rev(levels(dependentvariable)))
	#bskyconfmatrix <- caret::confusionMatrix(predictions, dependentvariable, positive=positive)
	
	####################################################################################################	
    probability <-cutoff
    Sensitivity <-bskyconfmatrix$byClass[1]
	Specificity<-bskyconfmatrix$byClass[2]
    OneMinusSpecificity <- 1-bskyconfmatrix$byClass[2]
	YoudenIndex= Sensitivity - (1-bskyconfmatrix$byClass[2])
    #truepos <-bskyconfmatrix$table[1,1]
    #falsepos  <-bskyconfmatrix$table[1,2]
    #falseneg <-bskyconfmatrix$table[2,1]
    #trueneg <-bskyconfmatrix$table[2,2]
	
	truepos <-bskyconfmatrix$table[2,2]
    falsepos  <-bskyconfmatrix$table[2,1]
    falseneg <-bskyconfmatrix$table[1,2]
    trueneg <-bskyconfmatrix$table[1,1]
	
	if (length(indepVars) ==1)
	{
	PredictorValue=eval(parse(text=paste (datasetname, "$", indepVars[1], "[",i, "]",sep="",collapse="")))
    results <- eval(parse(text=paste ("rbind( results, data.frame( RowID =i,", indepVars[1], "=", PredictorValue ,", probability = probability,OneMinusSpecificity =OneMinusSpecificity, Sensitivity =Sensitivity,YoudenIndex=YoudenIndex, Specificity =Specificity, truepos =truepos, trueneg=trueneg, falsepos =falsepos, falseneg=falseneg),make.row.names = FALSE)", sep="",collapse="")))
	}
	else
	{
	 results <- rbind( results, data.frame( RowID =i,probability = probability,OneMinusSpecificity =OneMinusSpecificity, Sensitivity =Sensitivity,YoudenIndex=YoudenIndex, Specificity =Specificity, truepos =truepos, trueneg=trueneg, falsepos =falsepos, falseneg=falseneg),make.row.names = FALSE)
	}
	
	i=i+1
  #cat(i, sep="\n")    
}
maxIndex =which.max(results$YoudenIndex)
results$YoudenIndex =as.character(results$YoudenIndex)
results$YoudenIndex[maxIndex] = paste(base::round(as.numeric(results$YoudenIndex[maxIndex]),digits=BSkyGetDecimalDigitSetting()) , "**",sep="",collapse="")

results = results %>%
	 arrange(desc(probability))
	 
return(results)
}


