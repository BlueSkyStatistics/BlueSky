
# here objclasslist is a vector/list of classes
# User wants to search and get a list of all the objects currently available that match to one of the class
# based on defaults set : user will see a list of objects whose class is "lm", "glm", "randomForest"
###########################################################
#Given a model class returns the objects /models of that class, also returns objects 
#returnClassTrain =TRUE suppresses all models of class train (Built with Model Tuning) that have a finalModel class in objclasslist=c("lm", "glm", "randomForest")
###########################################################
### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Fetches currently available models
#'
#' @description Fetches currently available models of a specified classes.
#'
#' @param objclasslist class of model to fetch
#' @param returnClassTrain 
#'
#' @return
#'
#' @examples
BSkyGetAvailableModels <- function(objclasslist=c("lm", "glm", "randomForest"), returnClassTrain =TRUE)
{
	
	if ("All_Models" %in% objclasslist )
	{
	#09/04/2021
	#Removed function from the list below. This is probably one model function that returns a model of class function, I don't know what that model is
	# when function is in the list, all functions created in the global space are getting returned, which is not needed
		objclasslist = c("NaiveBayes","randomForest","lm", "glm", "rpart", "multinom", "nnet", "polr","ksvm","blasso","knn3","real_adaboost","adaboost", "lmerModLmerTest","xgb.Booster","C5.0","BinaryTree","lognet","glmnet","earth","mlp","rsnns","RandomForest","rlm","rq","ranger","gbm","train","nn")
	}
	ClassFilterAdvanced <- function(x) 
	{ 
		eval(parse(text=paste('inherits(get(x), "',objclasslist,'" )', collapse='||', sep='')))
	}
	
	
	#print(ls(envir=.GlobalEnv))
    #All the models of the classes specified
	Objs <- Filter( ClassFilterAdvanced, ls(envir=.GlobalEnv))
	
	if (returnClassTrain)
	{
    originalObjClassList = objclasslist
    objclasslist = c("train")
    # All the objects of class train
    trainObjects <- Filter( ClassFilterAdvanced, ls(envir=.GlobalEnv))
    validTrainObjects =NULL
    #All train models that have a final model of classes specified
    for (trainobj in trainObjects)
    {
        finalModelClass = eval(parse(text=paste('class('  ,trainobj, '$finalModel',  ')', collapse='', sep='')))
        if (finalModelClass %in% originalObjClassList)
        {
            Objs = c( trainobj, Objs)
        }
    }
    }
	#print(Objs)
	return(Objs)
}


### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Fetches currently available models
#'
#' @description Fetches currently available models of a specified classes.
#'
#' @param objclasslist class of model to fetch
#' @param returnClassTrain
#'
#' @return
#'
#' @examples
BSkyGetAvailableModelsCP <- function(objclasslist=c("lm", "glm", "randomForest"), returnClassTrain =TRUE)
{
	
	if ("All_Models" %in% objclasslist)
	{
	#09/04/2021
	#Removed function from the list below. This is probably one model function that returns a model of class function, I don't know what that model is
	# when function is in the list, all functions created in the global space are getting returned, which is not needed
		objclasslist = c("NaiveBayes","randomForest","lm", "glm", "rpart", "multinom", "nnet", "polr","ksvm","blasso","knn3","real_adaboost","adaboost", "lmerModLmerTest","xgb.Booster","C5.0","BinaryTree","lognet","glmnet","earth","mlp","rsnns","RandomForest","rlm","rq","ranger","gbm","train","nn")
	}
	ClassFilterAdvanced <- function(x) 
	{ 
		eval(parse(text=paste('inherits(get(x), "',objclasslist,'" )', collapse='||', sep='')))
	}
	
	
	#print(ls(envir=.GlobalEnv))
    #All the models of the classes specified
	Objs <- Filter( ClassFilterAdvanced, ls(envir=.GlobalEnv))
	
	if (returnClassTrain)
	{
    originalObjClassList = objclasslist
    objclasslist = c("train")
    # All the objects of class train
    trainObjects <- Filter( ClassFilterAdvanced, ls(envir=.GlobalEnv))
    validTrainObjects =NULL
    #All train models that have a final model of classes specified
    for (trainobj in trainObjects)
    {
        finalModelClass = eval(parse(text=paste('class('  ,trainobj, '$finalModel',  ')', collapse='', sep='')))
        if (finalModelClass %in% originalObjClassList)
        {
            Objs = c( trainobj, Objs)
        }
    }
    }
	#print(Objs)
	# Added by Aaron 09/07/2021
	#When ever there are no models that match the selected model class, Filter function above returns character 0
	if (identical(Objs, character(0)))
	{
		return (c(''))
	}
	
	return(Objs)
}


ClassFilter <- function(x) 
{ 
	inherits(get(x), 'lm' )|| inherits(get(x), 'glm' ) || inherits(get(x), 'randomForest' )
}

