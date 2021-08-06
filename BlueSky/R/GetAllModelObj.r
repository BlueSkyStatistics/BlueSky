
# here objclasslist is a vector/list of classes
# User wants to search and get a list of all the objects currently available that match to one of the class
# based on defaults set : user will see a list of objects whose class is "lm", "glm", "randomForest"
###########################################################
#Given a model class returns the objects /models of that class, also returns objects 
#returnClassTrain =TRUE suppresses all models of class train (Built with Model Tuning) that have a finalModel class in objclasslist=c("lm", "glm", "randomForest")
###########################################################
BSkyGetAvailableModels <- function(objclasslist=c("lm", "glm", "randomForest"), returnClassTrain =TRUE)
{
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
ClassFilter <- function(x) 
{ 
	inherits(get(x), 'lm' )|| inherits(get(x), 'glm' ) || inherits(get(x), 'randomForest' )
}

