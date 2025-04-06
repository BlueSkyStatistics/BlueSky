bivariateLevels <- function (datasetName, dependentVariable)
  {
 
  noOflevels = eval(parse( text=paste ("nlevels(.GlobalEnv$",datasetName, "$",dependentVariable, ")",sep="" )))
  if (noOflevels !=2)
  {
    levelsArray=c("")
  }else
    {
    levelsArray = eval(parse( text=paste ("levels(.GlobalEnv$",datasetName, "$",dependentVariable, ")",sep="" )))
   
  }
  return (levelsArray)
}


predictPrerequisite <-function (modelname, curdatasetname) 
{
    msg = character(0)
    modclass = character(0)
    modclass = getModelClass(modelname)
    if (is.null(modclass)) {
        msg = paste0("ERROR: Could not find model class. Further processing cannot be done. Aborting..", 
            collapse = "", sep = "")
        return(msg)
    }
    successMsg1 = "SUCCESS: The predictor variables that the model requires for scoring are available in the dataset."
    successMsg2 = ""
    #failureMsg1 = "ERROR: The predictor variables that the model requires for scoring are not available in the dataset. [ "
    #failureMsg2 = " variables are not found ]."
	
	failureMsg1 = "ERROR: The predictor variables that the model requires for scoring are not available in the current dataset. \nOne or more of the variables [ "
    failureMsg2 = "  ] are not found."
	
    modelselmsg = paste0("\n -Currently selected model:  ", modelname, 
        sep = "")
    modelclasmsg = paste0("\n -Model Class:  ", modclass, sep = "")
    datasetmsg = paste0("\n -Currently selected dataset:  ", 
        curdatasetname, sep = "")
    finalmsg = paste ("\nNOTE: The variable names in the dataset you are trying to score must match the variable names of the dataset you used to build the model.\n")
    dependentvar = character(0)
    dependentvar <- getModelDependentVariable(modelname)
    modelvars <- list()
    modelvars <- getModelIndependentVariables(modelname)
    vardiff <- list()
    j = 1
    vrlst <- paste(modelvars, collapse = ", ", sep = ",")
    dependentvarmsg = paste("\n -Dependent variable of the selected model:  ", 
        dependentvar, sep = "")
    independentvarmsg = paste("\n -Independent variables of the selected model:  ", 
        vrlst, sep = "")
    commonMsg = paste(modelselmsg, modelclasmsg, datasetmsg, 
        dependentvarmsg, independentvarmsg, sep = "")
    modvarcount <- length(modelvars)
    datasetvars <- eval(parse(text = paste("names(", curdatasetname, 
        ")", sep = "")))
    dsvarcount <- length(datasetvars)
    if (modvarcount > 0) {
        for (i in 1:modvarcount) {
            if (!(modelvars[i] %in% datasetvars)) {
                vardiff <- append(vardiff, modelvars[i])
            }
        }
        if (length(vardiff) > 0) {
            help <- "\nNOTE: USE THE RED X ON THE TOP RIGHT HAND CORNER OF THE DIALOG TO CLOSE"
            vrlst <- paste(vardiff, collapse = ", ", sep = ",")
            msg = paste0(failureMsg1, vrlst, failureMsg2, commonMsg, 
                finalmsg,help, collapse = "", sep = "")
        }
        else {
            msg = paste0(successMsg1, commonMsg, collapse = "", 
                sep = "")
        }
    }
    else {
        msg = paste0("ERROR: Could not find independent variables or select the right model and dataset.", 
            collapse = "", sep = "")
    }
    print(msg)
    return(msg)
}

#Diagnostic Messages displayed in the cross platform 
#when you are scoring a dataset are different from c#
#predictPrerequisiteCP <-function (modelname, curdatasetname) 
#This function is called by cross platform not C#
predictPrerequisiteCP <-function (modelname, curdatasetname,BSkySurvival = FALSE, BSkySurvivalType ="ML") 
{
    if (modelname =="")
	{
	return("Test Results: As soon as a model is selected, we will run tests to see whether dependent variables specified in the model are available in \nthe dataset to be scored. The results will be displayed here")
	}
	
	msg = character(0)
    modclass = character(0)
    modclass = getModelClass(modelname)
    if (is.null(modclass)) {
        msg = paste0("ERROR: Could not find model class. Further processing cannot be done. Aborting..", 
            collapse = "", sep = "")
        return(msg)
    }
    successMsg1 = "SUCCESS: The predictor variables that the model requires for scoring are available in the dataset."
    successMsg2 = ""
	
	failureMsg1 = "ERROR: The predictor variables that the model requires for scoring are not available in the current dataset. \nOne or more of the variables [ "
    failureMsg2 = "  ] are not found."
	
    modelselmsg = paste0("\n -Currently selected model:  ", modelname, 
        sep = "")
    modelclasmsg = paste0("\n -Model Class:  ", modclass, sep = "")
    datasetmsg = paste0("\n -Currently selected dataset:  ", 
        curdatasetname, sep = "")
    finalmsg = paste ("\nNOTE: The variable names in the dataset you are trying to score must match the variable names of the dataset you used to build the model.\n")
    dependentvar = character(0)
    dependentvar <- getModelDependentVariable(modelname)
    modelvars <- list()
	
	if (BSkySurvival == TRUE && BSkySurvivalType =="CL")
	{
		modelvars <-  getModelIndependentVariables(modelname, BSkySurvival = TRUE, BSkySurvivalType ="CL")
	} else
	{
	    modelvars <- getModelIndependentVariables(modelname)
	}
	
    vardiff <- list()
    j = 1
    vrlst <- paste(modelvars, collapse = ", ", sep = ",")
    dependentvarmsg = paste("\n -Dependent variable of the selected model:  ", 
        dependentvar, sep = "")
    independentvarmsg = paste("\n -Independent variables of the selected model:  ", 
        vrlst, sep = "")
    commonMsg = paste(modelselmsg, modelclasmsg, datasetmsg, 
        dependentvarmsg, independentvarmsg, sep = "")
    modvarcount <- length(modelvars)
    datasetvars <- eval(parse(text = paste("names(", curdatasetname, 
        ")", sep = "")))
    dsvarcount <- length(datasetvars)
    if (modvarcount > 0) {
        for (i in 1:modvarcount) {
            if (!(modelvars[i] %in% datasetvars)) {
                vardiff <- append(vardiff, modelvars[i])
            }
        }
        if (length(vardiff) > 0) {
            #help <- "\nNOTE: USE THE RED X ON THE TOP RIGHT HAND CORNER OF THE DIALOG TO CLOSE"
            vrlst <- paste(vardiff, collapse = ", ", sep = ",")
           # msg = paste0(failureMsg1, vrlst, failureMsg2, commonMsg, 
             #   finalmsg,help, collapse = "", sep = "")
			msg = paste0(failureMsg1, vrlst, failureMsg2, commonMsg, 
                finalmsg, collapse = "", sep = "")
        }
        else {
            msg = paste0(successMsg1, commonMsg, collapse = "", 
                sep = "")
        }
    }
    else {
        msg = paste0("ERROR: Could not find independent variables or select the right model and dataset.", 
            collapse = "", sep = "")
    }
    print(msg)
    return(msg)
}






#return model class. 
#Returns first if model has multiple model classes(after discussion with Aaron. Right now we think this should suffice.). 
#Returns null if class could not be evaluated
getModelClass <- function(modelname)
{
	msg=character(0)
	modclass=character(0)
	#get model object's class from modelname
	modclasses = eval(parse(text=paste('class(',modelname,')', collapse='', sep='')))
	if(length(modclasses)>1)
	{
		modclass=modclasses[1] #pick the first class (eg from "glm" "lm" pick "glm")
	}
	else if(length(modclasses)==1)
	{
		modclass=modclasses
	}
	else
	{
		modclass=NULL
	}
	return(modclass)
}

##Get dependent variables for a model class passed as an argument
getModelDependentVariable <- function(modelname)
{
	msg=character(0)
	modclass=character(0)
	depvar <- character(0)
	#get model object's class from modelname
	#modclasses = eval(parse(text=paste('class(',modelname,')', collapse='', sep='')))
	modclass = getModelClass(modelname)
	if(is.null(modclass))
	{
		msg =paste0('ERROR: Could not find model class. Further processing cannot be done. Aborting..', collapse='', sep='')
		print(msg)
		return(depvar)
	}
	## Find dependent var(say:dependentvariable) and its class (say:dependentclass). 
	## If class!="factor" the don't create confusion matrix.

	dependentvariable <- NULL
	dependentclass=character(0)
	
	##if model is created using Model-Tuning then follwing attribute will not be null
	depvar <- eval(parse(text=paste('attr(',modelname,',"depvar")', sep='')))
	if(!is.null(depvar) )
	{
		##no code here because depvar is already extracted above.
	}
	else if(modclass == 'randomForest')
	{
		#a1 <- eval(parse(text=paste('as.character((',modelname,'$call$y)[[2]])', sep='') ) ) 
		objlen = eval(parse(text=paste('length(',modelname,'$call$y)', sep='') ) ) #26Jan2017 find length
		#length is 4 when y is passed as Dataset[,c('colname')]
		#length is 3 when y is passed as Dataset$colname. We made this fix in RandomForest dialog on 25Jan2017 for Q's 
		#issue Dataset[,c('colname')] was turning into a tibble and not behaving as column.
		if(objlen==4)
		{
			a2 <- eval(parse(text=paste('as.character((',modelname,'$call$y)[[4]])', sep='') ) ) 
			depvar <- a2[2]
		}
		else if(objlen==3)
		{
			depvar = eval(parse(text=paste('as.character((',modelname,'$call$y)[[3]])', sep='') ) ) 
		}
		#dependentvariable <- eval(parse(text=paste(a1,'[,c("',depvar,'")]', collapse='', sep='') ) )
		#dependentclass=eval( parse( text=paste('class(',a1,'[,c("',depvar,'")])',sep='') ) )
	}
	else if(modclass == 'NaiveBayes' && !eval(parse(text=paste('is.null(',modelname,'$dependentvar)', sep=''))))
	{
		depvar <- eval(parse(text=paste('as.character((',modelname,'$dependentvar))', sep='') ) )
		#dependentvariable <- eval(parse(text=paste('{{%DATASET%}}','$',depvar, sep='') ) )
		#dependentclass=eval( parse( text=paste('class(dependentvariable)',sep='') ) )
	}
	else if(modclass == 'rpart') 
	{
		depvar <- eval(parse(text=paste('as.character(',modelname,'$call$formula[[2]])', sep='') ) )
		#dependentvariable <- eval(parse(text=paste('{{%DATASET%}}','$',depvar, sep='') ) )
		#dependentclass=eval( parse( text=paste('class(dependentvariable)',sep='') ) )
	}
	else if('polr' %in% modclass || 'multinom' %in% modclass || 'glm' %in% modclass || 'lm' %in% modclass || 'negbin' %in% modclass)
	{
		depvar <- eval(parse(text=paste('as.character(',modelname,'$call$formula[[2]])', sep='') ) )
		#dependentvariable <- eval(parse(text=paste('{{%DATASET%}}','$',depvar, collapse='', sep='') ) )
		#dependentclass=eval( parse( text=paste('class(dependentvariable)',sep='') ) )
	}
	else if(modclass =='npregression') #12Dec2018 Anil
	{
		# modelname$ynames returns "as.numeric(lwages)" but we only want "lwages"
		# so we need to strip it out using substr()
		startstr=12
		endstr=16
		depvar <- eval(parse(text=paste('substr(',modelname,'$ynames, start=',startstr,',stop=',endstr,')', sep='') ) )
	}
	# else if(modclass == 'ksvm') ## can't find depvar from modelname. Its not stored there. So I added as attr
	# {
		# depvar <- eval(parse(text=paste('attr(',modelname,',"depvar")', sep='')))
	# }
	return(depvar)
}


BSkyisValidName <- function(string) {
    grepl("^([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*$", string)
}



##Get independent variables for a model class passed as an argument

#5/17/2020 commented the line below as I changed formula operators
#getModelIndependentVariables <- function(modelname, formulaoperators="[-+*/:^)(]|%in%")

getModelIndependentVariables <- function(modelname, formulaoperators="[-+*/:^,)(]|%in%", BSkySurvival = FALSE, BSkySurvivalType ="ML" )
{
	
	msg=character(0)
	modclass=character(0)
	modelvars <- list()
	#get model object's class from modelname
	#modclasses = eval(parse(text=paste('class(',modelname,')', collapse='', sep='')))
	modclass = getModelClass(modelname)
	if(is.null(modclass))
	{
		msg =paste0('ERROR: Could not find model class. Further processing cannot be done. Aborting..', collapse='', sep='')
		print(msg)
		return(modelvars)
	}
	
	####### Based on what model class is selected, find model independent variables. ####### 
	
	##if model is created using Model-Tuning then follwing attribute will not be null
	if (BSkySurvival == TRUE && BSkySurvivalType =="CL")
	{
		modelvars <- eval(parse(text=paste('attr(',modelname,',"indepvarCL")', sep='')))
	}
	else
	{
		modelvars <- eval(parse(text=paste('attr(',modelname,',"indepvar")', sep='')))
	}
	if(!is.null(modelvars) )
	{
		#modelvars <- eval(parse(text=modelvars))
		##This extracts dependent variables when they are in the formula format see neuralnet
		##The above format is used for typical models where dependent vars are in form 'var1','var2','var3'
		## see how formula is constructed for MLP
		if (str_detect(modelvars, "c\\("))
		{
			modelvars <- eval(parse(text = modelvars))
		}else
		##This extracts dependent variables when they are in the format "'var1','var2','var3'"
		##The above format is used for typical models where dependent vars are in form 'var1','var2','var3'
		## see how formula is constructed for MLP
		{
			modelvars <-strsplit(gsub("'","",modelvars),",")[[1]]
		}
	}
	else if('NaiveBayes' %in% modclass)
	{
		modelvars <- eval( parse(text=paste(modelname,'$varname',sep='')))
	}
	else if('randomForest' %in% modclass)
	{
		modelvars <- eval( parse(text=paste('as.character(',modelname,'$call$x)[[4]]', sep='' ) ) )
		modelvars <- eval(parse(text=modelvars))
	}
	else if('polr' %in% modclass || 'multinom' %in% modclass || 'glm' %in% modclass || 'lm' %in% modclass || 'negbin' %in% modclass)
	{
		#modelvars <- eval( parse(text=paste('base::unlist(base::strsplit( as.character(',modelname,'$call[[2]])[3], formulaoperators ))', sep='' ) ) )
		#modelvars <- gsub("^\\s+|\\s+$", "", modelvars) 
		#modelvars<-modelvars[sapply(modelvars, BSkyisValidName)]
		
		#WE DON'T HANDLE VARIABLE NAMES THAT ARE ALSO FUNCTION NAMES
		#We have added, and placed the , before the ) so that we can handle poly(X,2,raw=T) in lm(Y ~ -1 + poly(X,2,raw=T))
		#We want the X captured
		#formulaoperators="[-+*/:^,)(]|%in%"
		#This gives us the portion of the model formula after ~
		indepVars =eval( parse(text=paste('as.character(',modelname,'$call[[2]])[3]',  sep='' ) ) )
		#Remove package names for example stats::poly(x, degree)
		indepVars <- gsub( "[\\.,_,A-Z,a-z,0-9]+::", "",  indepVars)
		#Remove function names
		indepVars <- gsub( "[\\.,_,A-Z,a-z,0-9]+\\s*\\(", "",  indepVars)
		#We extract all functions i.e. poly(, exp(, .bgv(
		#indepVars <-str_extract_all(indepVars, "[\\.,_,A-Z,a-z,0-9]+\\s*\\(")
		#We make it a string
		#functstring <- unlist(functList)
		#Remove spaces \\s+ removes spaces from the start of the string, \\s+$ removes spaces from the end of the string
		#functstring <- gsub("^\\s+|\\s+$", "", functstring)
		#We generate a vector of all the function names
		#functstring <-str_replace_all(functstring, "\\(", "")
		#wE TOKENIZE THE portion of the formula after ~
		#modelvars <- eval( parse(text=paste('base::unlist(base::strsplit( as.character(',modelname,'$call[[2]])[3], formulaoperators ))', sep='' ) ) )
		modelvars = base::unlist(base::strsplit( indepVars, formulaoperators ))
		#Remove spaces \\s+ removes spaces from the start of the string, \\s+$ removes spaces from the end of the string
		modelvars <- gsub("^\\s+|\\s+$", "", modelvars) 
		#Finding the position of the formula as they are not valid variable names in the dataset
		#BSkyRemove <-which (modelvars %in% functstring)
		#if (length(BSkyRemove) !=0)
		#{
		#modelvars <-modelvars[-BSkyRemove]
		#}
		#REmove numbers as they are not valid variable names
		modelvars<-modelvars[sapply(modelvars, BSkyisValidName)]
		
	}
	else if(modclass =='rpart')
	{
		modelvars <- eval( parse(text=paste('base::unlist(base::strsplit( as.character(',modelname,'$call[[2]])[3], "[+]" ))',sep='' ) ) )
		modelvars <- gsub("^\\s+|\\s+$", "", modelvars) 
	}
	else if(modclass =='npregression') #12Dec2018 Anil
	{
		modelvars <- eval(parse(text=paste('as.character(',modelname,'$xnames)', sep='') ) )
	}	
	# else if(modclass == 'ksvm')## 21Jun2017
	# {
		# modelvars <-  eval(parse(text=paste('names(attr(',modelname,', "scaling")$x.scale$`scaled:center`)', sep=''))) 
		# #modelvars <- eval(parse(text=paste('attr(',modelname,',"indepvar")', sep='')))
	# }
	return(modelvars)
}

## Execute 'predict' and generates cols and add those cols to current dataset.
## This function uses a generic approach
## Works same as (as of 04Oct2016) the BSkyPredict function below this(BSkyPredict.Generic) function.
## Since this function works on generic approach, there may a model in future which 
## may not work correctly in this generic approach. In that case we must switch to other predict function below.
BSkyPredict.Generic <- function(modelname, prefix, datasetname)
{
	predictions=NULL
	
	msg=character(0)
	modclass=character(0)
	#get model object's class from modelname
	#modclasses = eval(parse(text=paste('class(',modelname,')', collapse='', sep='')))
	modclass = getModelClass(modelname)
	if(is.null(modclass))
	{
		msg =paste0('ERROR: Could not find model class. Further processing cannot be done. Aborting..', collapse='', sep='')
		print(msg)
		return(predictions)
	}
	
	
	depvar <- getModelDependentVariable(modelname)
	dependentvariable <- eval(parse(text=paste(datasetname,'$',depvar, sep='') ) )	
	dependentclass=eval( parse( text=paste('class(dependentvariable)',sep='') ) )
		
	# running 'predict' on selected model and passing a data.frame of 
	# independent vars in this format ( data.frame(mpg=Dataset2$mpg, weight=Dataset2$weight)
	modelvars <- getModelIndependentVariables(modelname)
	tmpstr1 = paste0(modelvars ,'=',datasetname,'$',modelvars , collapse=',',sep='')
	#cat('\n',tmpstr1)
	tmpstr2 = paste0('data.frame(',tmpstr1,')', collapse=',', sep='')
	#cat('\n',tmpstr2)
	
	#if modclass is 'rpart' and deendent variable is a factor then we pass type="class" in the predict()
	if(modclass=="rpart" && dependentclass=="factor")
	{
		predictions <- eval(parse(text=paste('predict(',modelname,',',tmpstr2,', type="class")',collapse='',sep='') ))
	}
	else
	{
		predictions <- eval(parse(text=paste('predict(',modelname,',',tmpstr2,')',collapse='',sep='') ))
	}

	#save predictions(newcol) to the dataset. #multiple cols in case of NaiveBayes and tree so we prefix those
	# if(modclass == "NaiveBayes")
	# {
		# predictedCols <- predictions
		# #aa <- paste(prefix,'_',names(predictedCols), sep='' )
		# #names(predictedCols) <- aa
		# names(predictedCols) <- paste(prefix,'_',names(predictedCols), sep='' )
		
		# ##this one is causing some mess if col in prefix already exists.
		# eval(parse(text=paste(datasetname,' <<- cbind(',datasetname,', predictedCols)', sep='') ) )
	# }
	# else
	# {
		# #notworking eval(parse(text=paste(prefix,' <- ',predictions, sep='') ) )
		# eval(parse(text=paste(prefix,' <- predictions', sep='') ) )
		
		# #this one is causing some mess if col in prefix already exists.
		# eval(parse(text=paste(datasetname,' <<- cbind(',datasetname,',', prefix,')', sep=''))) 
		# #notworking eval(parse(text=paste(datasetname,' <<- cbind(',datasetname,', prefix)', sep='') ) )
	# }
	
##if else above is commented and following line took its place
## as.data.frame is used below because NavieBayes generates more than 1 col so we needed this. This works for all 1 col or multicol predictions.
df <- as.data.frame(predictions )
# datasetname='Dataset3'
# prefix='pop'
# names(Dataset3)
nm <- names(df)
ll <- length(nm)
for(i in 1:ll)
{
#in the line below we used double square brackets instead on single because with single bracket the col(in dataset) class was becoming data.frame
# and if the class is data.frame for that new col, it is filtered out from the dialog's source list box. Source list box shows basic class vars
# like character, numeric, double etc.
aa <- paste(datasetname,'$',prefix,'_',nm[i],'<<-df[[',i,']]',sep='' )
#print( aa)
eval(parse(text=aa))
}

	## Not sure we can do following from here, so commenting it and will instead call it
	## from the dialog. Because C# is supposed to trap this call in C# code
	#BSkyLoadRefreshDataframe(datasetname)
	return(predictions)
}


## not in use
AddPredictColToDataset <- function(predictor, newcolname, datasetname)
{
	if(predictor)
	{
		eval(parse(text=paste(newcolname,' <- rfmodel$predicted',sep='' ))) 
		#datasetname <- cbind( datasetname}, newcolname)
		eval(parse(text=paste(datasetname,'<<- cbind(',datasetname,', ',newcolname,')', sep='' )))
	}
	return(NULL)
}


#####end  ### New Functions 11Sep2017


###### Start #### Aaron's new functions 28Oct2017  ########


#BSkyStepAIC
#BSkyStepAIC.mass
#multinorm will work with string

#THINGS TO NOTE, HOW DO I CREATE A LOGICAL TYPE OR CONVERT FROM FACTOR TO LOGICAL
#FREQUENCIES ON LOGICAL DONT WORK

# OK random forest works with multi level factor,and logical, and 2 level factor, random forest does not allow string
## OK glm will work with factor and numeric and logical. String will not work. glm with factor variables with more than 2 levels is not supported
# OK rpart works with multi level factor, string logical and numeric dependent variables. See dataset crx.data.rdata     and carssales.rdata
#multinorm works with string, factor, logical. We should prevent numeric
#Naive Bayes, dependent variable must be a factor.  Logical, string, numeric not suported

##From Aaron: 20Sep2018
#Predicted probabilities are supported for rpart, random forest, Naive Bayes
#Not supported for KNN, GLM

#For rpart savePredictedProbs have the probabilities
#For Naive Bayes, savePredictedProbs have the probs
#ROC determines whether we show the ROC
#We show the ROC plot only for logistic, decision trees, randon forest when the predictor is logical or a factor with 2 levels
#The ROC plot is generated based on the first level of the predictor i.e. if we are predicting automobile which has 2 levels car and truck, ROC curve is generated for probabilities associated with automobile =car


#27Jul2019
#BSkyPredict <-function(modelname='Logistic1',prefix='ss', datasetname='Dataset4')
#3Aug2019

#Called when the dependent variable is part of the dataset or is not present 
#If dependent variable is not in dataset which often happens when you are scoring brand new data, then dependent variable =NULL and a confusionmatrix and ROC is not displayed
### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Score a dataset using a model
#'
#' @description     Model scoring does the following
#' 1. Scores the current dataset using the selected prebuilt model. Stores predictions with the specified confidence interval in the current dataset using the specified prefix.
#' 2. Optionally creates a confusion matrix and a ROC curve
#' predict is a generic function for making predictions using the selected model.
#' 
#' @param modelname a model object for which prediction is desired.
#' @param prefix prefix string that will be used to create new variables containing the predictions.
#' @param datasetname is the current dataset to score and save predictions to.
#'
#' @return
#'
#' @examples
BSkyPredict <-function(modelname='multinom',prefix='multinom', confinterval=FALSE, level =.95, datasetname='Dataset4',BSkySurvival = FALSE, BSkySurvivalType ="ML", BSkyTime ="", predinterval = FALSE) 
{
    
	#Logistic regression family
	fly=""
	#Used by mlp neuralnet to indicate that the predictions are saved
	#This ensures that we don't invoke the default code to save predictions
	predictionsSaved=FALSE
	MultipleDependentVars=FALSE
    noofDigitsToRound=BSkyGetDecimalDigitSetting()
    ROC = FALSE
    predictions = NULL
    predictedProbs = NULL
	savePredictedProbs =NULL
    msg = character(0)
	predictionsclass =""
    modclass = character(0)
    modclass = getModelClass(modelname)
	#######
	#This is the class of the model within the train model i.e. train$finalModel
	classOfFinalModel=""
	if (modclass =="train")
	{
    classOfFinalModel = getModelClass(paste (modelname ,"$finalModel",sep="",collapse=""))
	}
#######
    if (is.null(modclass)) {
        msg = paste0("ERROR: Could not find model class. Further processing cannot be done. Please email BlueSky Statistics support support@blueskystatistics.com with related information", 
            collapse = "", sep = "")
        print(msg)
        return(predictions)
    }
    dependentvariable <- NULL
    dependentclasses = character(0)
	
	#Aaron 02/16/2022 
	#we point to the sample of the dependent variable used to create the model
	#This is necessary as the dataset been scored does not always contain the dependent variable
	#We need the dependent variable to name the variable to contain the scores
	#we need levels to name the predicted values with the correct levels
	
	dependentvariable   <- eval(parse(text = paste("attr(", modelname, ",\"depVarSample\")",
        sep = "")))
		
    depvar <- getModelDependentVariable(modelname)
	
	#Added by Aaron 5/16/2020
	 #This is the case where there are 2 dependent variables, this happens for neural nets, you dummy code a factor variable 
	 #Think species in the iris dataset, you dummy code species, you get 2 scale variables
	 #in this case the depvar contains 2 variables separated by , and dependentvariable is NULL
	 #We don't make predictions in this case
	
	if (!is.null(depvar)) {
        if (str_detect(depvar, ",")) {
            dependentclass = "factor"
            MultipleDependentVars =TRUE
        }
        else if (str_detect(depvar, "c\\(")) 
        {
            depvar <-str_extract_all(depvar, "'[A-Z,a-z,0-9,_,.]*'")

            #dependentvariable <- eval(parse(text = paste(datasetname, 
              #  "$", eval(parse(text = depvar)), sep = "")))
            #dependentclasses = eval(parse(text = paste("class(dependentvariable)", 
             #   sep = "")))
            #if (length(dependentclasses) > 1) {
             #   dependentclass = dependentclasses[1]
            #}
            #else if (length(dependentclasses) == 1) {
             #   dependentclass = dependentclasses
            #}
        }
       if (!MultipleDependentVars) 
            {
			#Commented by Aaron 02/15/2022
           # dependentvariable <- eval(parse(text = paste(datasetname, 
			#"$", depvar, sep = "")))
           # dependentclasses = eval(parse(text = paste("class(dependentvariable)", 
            #    sep = "")))
			dependentclasses  <- eval(parse(text = paste("attr(", modelname, ",\"classDepVar\")",
        sep = "")))
            if (length(dependentclasses) > 1) {
                dependentclass = dependentclasses[1]
            }
            else if (length(dependentclasses) == 1) {
                dependentclass = dependentclasses
            }
        }
    }
	
	#This code cannot be run as BSKYPredict as the dependent variable MAY not be in a dataset that you are scoring
    # if (!is.null(depvar)) {
        # dependentvariable <- eval(parse(text = paste(datasetname, 
            # "$", depvar, sep = "")))
		
		# if (is.null(dependentvariable)) {
        # msg = paste0("ERROR: Could not find model class. Further processing cannot be done. Please email BlueSky Statistics support support@blueskystatistics.com with related information", 
            # collapse = "", sep = "")
        # print(msg)
        # return(predictions)
    # }
		
        
        
    
    modelvars <- getModelIndependentVariables(modelname, BSkySurvival =BSkySurvival, BSkySurvivalType =BSkySurvivalType)
    tmpstr1 = paste0(modelvars, "=", datasetname, "$", modelvars, 
        collapse = ",", sep = "")
    tmpstr2 = paste0("data.frame(", tmpstr1, ")", collapse = ",", 
        sep = "")
		
	################
	
	
	
	#Handles model class gbm for boosted trees, glmnet, , blasso, c5.0Tree, dnn, rpart, adaboost
	if (modclass == "train" ) 
	{
		# Prediction with confidence interval
		#conf_interval <- predict(model, newdata = new_data, interval = "confidence")
		# Prediction with prediction interval
		#pred_interval <- predict(model, newdata = new_data, interval = "prediction")

		#Added by Aaron 09/04/2020
		##This saves the confidence intervals of the predictions for models created with Model Tuning
		if ("lm" %in% classOfFinalModel  && confinterval == TRUE)
		{
		predictions <- eval(parse(text = paste("predict(", modelname, 
            "$finalModel,", tmpstr2, ", interval =\"prediction\", level=" ,level, " )", collapse = "", sep = "")))
		
		#predictions_conf_int <- eval(parse(text = paste("predict(", modelname, 
        #    "$finalModel,", tmpstr2, ", interval =\"confidence\", level=" ,level, " )", collapse = "", sep = "")))
		
		#cat("\nfrom here 1\n")
		#cat(paste("predict(", modelname, 
        #    ",", tmpstr2, ", interval =\"prediction\", level=" ,level, " )", collapse = "", sep = ""))
		#cat("\n")
		#print(predictions_conf_int[2:3])
		#predictions = cbind(predictions, predictions_conf_int[2:3])
		#Adden by Aaron 02/02/2025
		# when confidence interval was TRUE the error Error: 'length = 2' in coercion to 'logical(1)'
		# was returned as the code predictionsclass =class(predictions) returned a vector with 2 values matrix and array
		#
		predictionsclass =class(predictions[,1])
		}
		else
		{
		predictions <- eval(parse(text = paste("predict(", modelname, 
		",", tmpstr2, ")", collapse = "", 
		sep = "")))
		#Adden by Aaron 02/02/2025
		# when confidence interval was TRUE the error Error: 'length = 2' in coercion to 'logical(1)'
		# was returned as the code predictionsclass =class(predictions) returned a vector with 2 values matrix and array
		# Just brought line below into the else part
		predictionsclass =class(predictions)
		
		}
		
			
		
				
	#######
        if ((predictionsclass == "factor" || predictionsclass ==  "ordered" || predictionsclass == "logical" )&& ( classOfFinalModel !="ranger"))
#######
		{
			savePredictedProbs <- eval(parse(text = paste("predict(", 
				modelname, ",", tmpstr2, ",type =\"prob\"" ,")", collapse = "", 
				sep = "")))
			savePredictedProbs =as.data.frame( savePredictedProbs)
			names(savePredictedProbs) =levels(predictions)
			
			if (!is.null(depvar)) 
			{
				if (dependentclass =="character")
				{
					dependentvariable = as.factor(dependentvariable)
				}
			}
		}	

	#This is because when the dependent class is a string , we need to make it a factor else the Confusion matrix fails
		 if ((predictionsclass == "factor"  )&& ( classOfFinalModel =="ranger"))
		 {
		  if (dependentclass =="character")
			{
            dependentvariable = as.factor(dependentvariable)
			}
		 }
			
        
    }
	
	else	if (modclass == "xgb.Booster" && (dependentclass == "numeric" || dependentclass == "double" || dependentclass == "integer"))
    {
       
       predictions <- eval(parse(text = paste("predict(", 
            modelname, ",data.matrix(", tmpstr2, "), reshape = TRUE)", collapse = "", sep = "")))
       
        #predictions <- eval(parse(text = paste("predict(", modelname, 
         #   ",data.matrix(", tmpstr2, "), type=\"class\")", collapse = "", 
         #   sep = "")))
        #predictions <-colnames(savePredictedProbs)[apply(savePredictedProbs,1,which.max)]
    }
	
	
	else if (modclass == "rpart" && (dependentclass == "factor" || dependentclass == "character"||dependentclass == "ordered")) {
	
	 if (dependentclass =="character")
        {
            dependentvariable = as.factor(dependentvariable)
        }
        savePredictedProbs <- eval(parse(text = paste("predict(", 
            modelname, ",", tmpstr2, ")", collapse = "", sep = "")))
        predictions <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ", type=\"class\")", collapse = "", 
            sep = "")))
			
			
			if (dependentclass =="factor")
			{
				predictions =factor(predictions, levels =levels(dependentvariable))
			}
         if (dependentclass =="ordered")
			{
				predictions =factor(predictions, levels =levels(dependentvariable), ordered=TRUE)
			}
    }
	
	
	#
	# #Added by Aaron 5/18/2020
	# # Dependent class is factor for MLP with more than one dependent variable
			# else if (modclass == "mlp" && (dependentclass == "factor" || dependentclass == "character"||dependentclass == "ordered")) {
        # if (dependentclass =="character")
        # {
            # dependentvariable = as.factor(dependentvariable)
        # }
        # savePredictedProbs <- eval(parse(text = paste("predict(", 
            # modelname, ",", tmpstr2, ")", collapse = "", 
            # sep = "")))
			# savePredictedProbs =as.data.frame( savePredictedProbs)
			
		# #Added by Aaron 5/16/2020
	 # #This is the case where there are 2 dependent variables, this happens for neural nets, you dummy code a factor variable 
	 # #Think species in the iris dataset, you dummy code species, you get 2 scale variables
	 # #in this case the dependent variable is NULL
	 # #We don't make predictions in this case
			  # if (!is.null(dependentvariable))
        # {
            # names(savePredictedProbs) = levels(dependentvariable)
            # predictions <- colnames(savePredictedProbs)[apply(savePredictedProbs, 
            # 1, which.max)]
        
            # if (dependentclass == "factor" || dependentclass == "character") {
                # predictions = factor(predictions, levels = levels(dependentvariable))
            # }
            # if (dependentclass == "ordered") {
                # predictions = factor(predictions, levels = levels(dependentvariable), 
                    # ordered = TRUE)
            # }
        # }
        
    # }
	
	
	#Added by Aaron 5/18/2020
	# Dependent class is factor for MLP with more than one dependent variable
	#if there is a single dependent variable, it must be scale
			else if (modclass == "mlp" && (dependentclass == "factor" )) {
       
			
        predictions <- eval(parse(text = paste("predict(", 
            modelname, ",", tmpstr2, ")", collapse = "", 
            sep = "")))
			
			
			if (MultipleDependentVars==FALSE)
			{
			# predictions = RSNNS::encodeClassLabels(predictions)
			# #test = c(1,2,3,0,1,2,3)
			# predictions[which(predictions == 0)]=NA
			
			# levelsDepVar <-deparse(levels(dependentvariable))
			# predictions <-eval(parse(text=paste ("recode( predictions, ", "\"",substr(levelsDepVar , 4, nchar(levels)-1),  ")", sep="")) )
			# #predictions =dplyr::recode (predictions, na.omit(levels(dependentvariable)), default=NA)
			 savePredictedProbs  =predictions
			predictions = RSNNS::encodeClassLabels(predictions)
            predictions[which(predictions == 0)] = NA
            levelsDepVar <- deparse(levels(dependentvariable))
            predictions <- eval(parse(text = paste("dplyr::recode( predictions, ", 
                "\"", substr(levelsDepVar, 4, nchar(levelsDepVar) -  1), ")", sep = "")))
            predictions =factor(predictions)
			
			}
			
			predictions =as.data.frame( predictions)
			
			
		#Added by Aaron 5/16/2020
	 #This is the case where there are 2 dependent variables, this happens for neural nets, you dummy code a factor variable 
	 #Think species in the iris dataset, you dummy code species, you get 2 scale variables
	 #in this case the dependent variable is NULL
	 #We don't make predictions in this case
	 
			  # if (!is.null(dependentvariable))
        # {
            # names(savePredictedProbs) = levels(dependentvariable)
            # predictions <- colnames(savePredictedProbs)[apply(savePredictedProbs, 
            # 1, which.max)]
        
            # if (dependentclass == "factor" || dependentclass == "character") {
                # predictions = factor(predictions, levels = levels(dependentvariable))
            # }
            # if (dependentclass == "ordered") {
                # predictions = factor(predictions, levels = levels(dependentvariable), 
                    # ordered = TRUE)
            # }
        # }
        
    }
	
	
	
	#Added by Aaron 5/18/2020
	# Dependent class is factor for MLP with more than one dependent variable
	#if there is a single dependent variable, it must be scale
			else if (modclass == "nn" && (dependentclass == "factor" )) {
       
			
        predictions <- eval(parse(text = paste("predict(", 
            modelname, ",", tmpstr2, ")", collapse = "", 
            sep = "")))
			
			if (!MultipleDependentVars)
			{
			# predictions = RSNNS::encodeClassLabels(predictions)
			# #test = c(1,2,3,0,1,2,3)
			# predictions[which(predictions == 0)]=NA
			
			# levelsDepVar <-deparse(levels(dependentvariable))
			# predictions <-eval(parse(text=paste ("recode( predictions, ", "\"",substr(levelsDepVar , 4, nchar(levels)-1),  ")", sep="")) )
			# #predictions =dplyr::recode (predictions, na.omit(levels(dependentvariable)), default=NA)
			savePredictedProbs  =predictions
			predictions = RSNNS::encodeClassLabels(predictions)
            predictions[which(predictions == 0)] = NA
            levelsDepVar <- deparse(levels(dependentvariable))
            predictions <- eval(parse(text = paste("dplyr::recode( predictions, ", 
                "\"", substr(levelsDepVar, 4, nchar(levelsDepVar) -  1), ")", sep = "")))
            predictions =factor(predictions)
			
			}
			
			predictions =as.data.frame( predictions)
			
			
		#Added by Aaron 5/16/2020
	 #This is the case where there are 2 dependent variables, this happens for neural nets, you dummy code a factor variable 
	 #Think species in the iris dataset, you dummy code species, you get 2 scale variables
	 #in this case the dependent variable is NULL
	 #We don't make predictions in this case
	 
			  # if (!is.null(dependentvariable))
        # {
            # names(savePredictedProbs) = levels(dependentvariable)
            # predictions <- colnames(savePredictedProbs)[apply(savePredictedProbs, 
            # 1, which.max)]
        
            # if (dependentclass == "factor" || dependentclass == "character") {
                # predictions = factor(predictions, levels = levels(dependentvariable))
            # }
            # if (dependentclass == "ordered") {
                # predictions = factor(predictions, levels = levels(dependentvariable), 
                    # ordered = TRUE)
            # }
        # }
        
    }
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	#Added by Aaron 5/18/2020
	# Dependent class is factor for MLP with more than one dependent variable
	#if there is a single dependent variable, it must be scale
			# else if (modclass == "nn" && ((dependentclass == "factor" )||(dependentclass == "ordinal" ) )) {
       
        # predictions <- eval(parse(text = paste("predict(", 
            # modelname, ",", tmpstr2, ")", collapse = "", 
            # sep = "")))
			# predictions =as.data.frame( predictions)
			
		# #Added by Aaron 5/16/2020
	 # #This is the case where there are 2 dependent variables, this happens for neural nets, you dummy code a factor variable 
	 # #Think species in the iris dataset, you dummy code species, you get 2 scale variables
	 # #in this case the dependent variable is NULL
	 # #We don't make predictions in this case
	 
			  # # if (!is.null(dependentvariable))
        # # {
            # # names(savePredictedProbs) = levels(dependentvariable)
            # # predictions <- colnames(savePredictedProbs)[apply(savePredictedProbs, 
            # # 1, which.max)]
        
            # # if (dependentclass == "factor" || dependentclass == "character") {
                # # predictions = factor(predictions, levels = levels(dependentvariable))
            # # }
            # # if (dependentclass == "ordered") {
                # # predictions = factor(predictions, levels = levels(dependentvariable), 
                    # # ordered = TRUE)
            # # }
        # # }
        
    # }
	
	
# else if (modclass == "nn" && (dependentclass == "factor" || dependentclass == "character"||dependentclass == "ordered")) {
        # if (dependentclass =="character")
        # {
            # dependentvariable = as.factor(dependentvariable)
        # }
        # savePredictedProbs <- eval(parse(text = paste("predict(", 
            # modelname, ",", tmpstr2, ")", collapse = "", 
            # sep = "")))
			# savePredictedProbs =as.data.frame( savePredictedProbs)
			
		# #Added by Aaron 5/16/2020
	 # #This is the case where there are 2 dependent variables, this happens for neural nets, you dummy code a factor variable 
	 # #Think species in the iris dataset, you dummy code species, you get 2 scale variables
	 # #in this case the dependent variable is NULL
	 # #We don't make predictions in this case
			  # if (!is.null(dependentvariable))
        # {
            # names(savePredictedProbs) = levels(dependentvariable)
            # predictions <- colnames(savePredictedProbs)[apply(savePredictedProbs, 
            # 1, which.max)]
        
            # if (dependentclass == "factor" || dependentclass == "character") {
                # predictions = factor(predictions, levels = levels(dependentvariable))
            # }
            # if (dependentclass == "ordered") {
                # predictions = factor(predictions, levels = levels(dependentvariable), 
                    # ordered = TRUE)
            # }
        # }
        
    # }
	
	
else  if (modclass == "C5.0" && (dependentclass == "factor" || dependentclass == "character" ||dependentclass == "ordered")) 
    {
         if (dependentclass =="character")
        {
            dependentvariable = as.factor(dependentvariable)
        }
		savePredictedProbs <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ", type=\"prob\")", collapse = "", 
            sep = "")))
        predictions <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ", type=\"class\")", collapse = "", 
            sep = "")))
        if (dependentclass =="factor")
			{
				predictions =factor(predictions, levels =levels(dependentvariable))
			}
         if (dependentclass =="ordered")
			{
				predictions =factor(predictions, levels =levels(dependentvariable), ordered=TRUE)
			}
    }


 else if (modclass == "rsnns" && (dependentclass == "factor" || dependentclass == "character" ||dependentclass == "ordered"))  {
        if (dependentclass =="character")
        {
            dependentvariable = as.factor(dependentvariable)
        }
        savePredictedProbs <- eval(parse(text = paste("predict(", 
            modelname, ",", tmpstr2, ")", collapse = "", 
            sep = "")))
			savePredictedProbs =as.data.frame( savePredictedProbs)
			names(savePredictedProbs) =levels(dependentvariable)
			predictions <-colnames(savePredictedProbs)[apply(savePredictedProbs,1,which.max)]
			if (dependentclass =="factor" )
			{
				predictions =factor(predictions, levels =levels(dependentvariable))
			}
			if (dependentclass =="ordered")
			{
				predictions =factor(predictions, levels =levels(dependentvariable), ordered=TRUE)
			}
        
    }


    else if (modclass == "rpart" && (dependentclass == "logical" )) {
        #savePredictedProbs <- eval(parse(text = paste("predict(", 
         #   modelname, ",", tmpstr2, ")", collapse = "", sep = "")))
        predictions <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ", type=\"vector\")", collapse = "", 
            sep = "")))
    }

    else if (modclass == "randomForest" && (dependentclass == "factor" || dependentclass == "character"||dependentclass == "ordered")) {
		  if (dependentclass =="character")
        {
            dependentvariable = as.factor(dependentvariable)
        }
	
        savePredictedProbs <- eval(parse(text = paste("predict(", 
            modelname, ",", tmpstr2, ",type =\"prob\")", collapse = "", 
            sep = "")))
        predictions <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ", type=\"class\")", collapse = "", 
            sep = "")))
			
			if (dependentclass =="factor")
			{
				predictions =factor(predictions, levels =levels(dependentvariable))
			}
         if (dependentclass =="ordered")
			{
				predictions =factor(predictions, levels =levels(dependentvariable), ordered=TRUE)
			}
    }
    else if (modclass == "randomForest" && dependentclass == "logical") {
        savePredictedProbs <- eval(parse(text = paste("predict(", 
            modelname, ",", tmpstr2, ")", collapse = "", sep = "")))
        predictions = base::ifelse(savePredictedProbs >= 0.5, TRUE, 
            FALSE)
    }
    else if (modclass == "NaiveBayes" && (dependentclass == "factor" || dependentclass == "character" ||dependentclass == "ordered"))  {
	if (dependentclass =="character")
        {
            dependentvariable = as.factor(dependentvariable)
        }
	
        temp <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ")", collapse = "", sep = "")))
        savePredictedProbs = temp$posterior
        predictions = temp$class
		if (dependentclass =="factor")
			{
				predictions =factor(predictions, levels =levels(dependentvariable))
			}
         if (dependentclass =="ordered")
			{
				predictions =factor(predictions, levels =levels(dependentvariable), ordered=TRUE)
			}
    }
    else if (modclass == "knn3" && (dependentclass == "factor" || dependentclass == "character" ||dependentclass == "ordered")) {
        if (dependentclass =="character")
        {
            dependentvariable = as.factor(dependentvariable)
        }
		
		predictions <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ", type=\"class\")", collapse = "", 
            sep = "")))
			if (dependentclass =="factor")
			{
				predictions =factor(predictions, levels =levels(dependentvariable))
			}
         if (dependentclass =="ordered")
			{
				predictions =factor(predictions, levels =levels(dependentvariable), ordered=TRUE)
			}
    }
    else if (modclass == "adaboost" && (dependentclass == "factor" || dependentclass == "character" || dependentclass == "ordered") || modclass =="fast_adaboost" && (dependentclass == "factor" || dependentclass == "character" || dependentclass == "ordered") || modclass == "real_adaboost" && (dependentclass == "factor" || dependentclass == "character" || dependentclass == "ordered")) {
	   if (dependentclass =="character")
        {
            dependentvariable = as.factor(dependentvariable)
        }
	
        tmpPredictions <- eval(parse(text = paste("predict(", 
            modelname, ",", tmpstr2, ")", collapse = "", sep = "")))
        predictions = tmpPredictions$class
		if (dependentclass =="factor")
			{
				predictions =factor(predictions, levels =levels(dependentvariable))
			}
         if (dependentclass =="ordered")
			{
				predictions =factor(predictions, levels =levels(dependentvariable), ordered=TRUE)
			}
		
		
    }
    else if ((modclass == "glm" && (dependentclass == "factor" || dependentclass == "logical"|| dependentclass == "ordered")) || (modclass == "glm" && (dependentclass == "numeric"|| dependentclass =="integer"))) {
	
		fly =eval( parse(text=paste ("family(" ,modelname, ")$family") ))
		if (fly =="binomial")
		{
        predictions <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ", type=\"response\")", collapse = "", 
            sep = "")))
		}
		else		
		{
		predictions <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ")", collapse = "", sep = "")))
		}
    }
    else if (modclass == "multinom" && (dependentclass == "factor" ||dependentclass == "ordered" ) ||  modclass == "multinom" && (dependentclass == "character") || modclass == "multinom" && (dependentclass == "numeric")) {
         savePredictedProbs <- eval(parse(text = paste("predict(", 
            modelname, ",", tmpstr2, ",type =\"prob\")", collapse = "", 
            sep = "")))
        predictions <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ", type=\"class\")", collapse = "", 
            sep = "")))
    }

else if (modclass == "polr" && (dependentclass == "ordered") ) {
         savePredictedProbs <- eval(parse(text = paste("predict(", 
            modelname, ",", tmpstr2, ",type =\"prob\")", collapse = "", 
            sep = "")))
        predictions <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ", type=\"class\")", collapse = "", 
            sep = "")))
    }
	
	#To the best of my knowledege the code below is never executed as xgbooster will not support dependent class of factor
	
else	if (modclass == "xgb.Booster" && (dependentclass == "factor" || dependentclass == "character"))
    {
        if (dependentclass =="character")
        {
            dependentvariable = as.factor(dependentvariable)
        }
        savePredictedProbs <- eval(parse(text = paste("predict(", 
            modelname, ",data.matrix(", tmpstr2, "), reshape = TRUE)", collapse = "", sep = "")))
        if (class(savePredictedProbs) =="matrix")
        {
            savePredictedProbs =data.frame(savePredictedProbs)
            names(savePredictedProbs) =levels(dependentvariable)
            predictions <-colnames(savePredictedProbs)[apply(savePredictedProbs,1,which.max)]
			
			if (dependentclass =="factor")
			{
				predictions =factor(predictions, levels =levels(dependentvariable))
			}
         if (dependentclass =="ordered")
			{
				predictions =factor(predictions, levels =levels(dependentvariable), ordered=TRUE)
			}
			 
        }
        else
        {
          predictions = base::ifelse(savePredictedProbs >= 0.5, levels(dependentvariable)[2], levels(dependentvariable)[1])
          predictions =factor(predictions, levels =levels(dependentvariable)) 
        }
        #predictions <- eval(parse(text = paste("predict(", modelname, 
         #   ",data.matrix(", tmpstr2, "), type=\"class\")", collapse = "", 
         #   sep = "")))
        #predictions <-colnames(savePredictedProbs)[apply(savePredictedProbs,1,which.max)]
    }
	

 else if (modclass == "multinom" && (dependentclass == "factor") ||  modclass == "multinom" && (dependentclass == "character") || modclass == "multinom" && (dependentclass == "numeric")) {
         savePredictedProbs <- eval(parse(text = paste("predict(", 
            modelname, ",", tmpstr2, ",type =\"prob\")", collapse = "", 
            sep = "")))
        predictions <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ", type=\"class\")", collapse = "", 
            sep = "")))
    }   
	
	 else if (modclass == "nnet" && (dependentclass == "factor" || dependentclass == "character" ||dependentclass == "ordered"))  {
           if (dependentclass =="character")
        {
            dependentvariable = as.factor(dependentvariable)
        }
		 savePredictedProbs <- eval(parse(text = paste("predict(", 
            modelname, ",", tmpstr2, ",type =\"raw\")", collapse = "", 
            sep = "")))
        predictions <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ",type =\"class\")", collapse = "", sep = "")))

			 if (dependentclass =="factor")
			{
				predictions =factor(predictions, levels =levels(dependentvariable))
			}
         if (dependentclass =="ordered")
			{
				predictions =factor(predictions, levels =levels(dependentvariable), ordered=TRUE)
			}
    }


 else if (modclass == "nnet.formula" && (dependentclass == "factor" || dependentclass == "character" ||dependentclass == "ordered")) {
			  if (dependentclass =="character")
        {
            dependentvariable = as.factor(dependentvariable)
        }
		savePredictedProbs <- eval(parse(text = paste("predict(", 
            modelname, ",", tmpstr2, ",type =\"raw\")", collapse = "", 
            sep = "")))
        predictions <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ",type =\"class\")", collapse = "", sep = "")))

        if (dependentclass =="factor")
			{
				predictions =factor(predictions, levels =levels(dependentvariable))
			}
         if (dependentclass =="ordered")
			{
				predictions =factor(predictions, levels =levels(dependentvariable), ordered=TRUE)
			}
    }





 else if (modclass == "nnet" && (dependentclass == "numeric")) {
         
        predictions <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ",type =\"raw\")", collapse = "", sep = "")))
			  predictions =as.numeric(predictions)
		## Added Aaron 01/25/2025
		##neuralnets does not return a model of class nnet hence below is not valid
		##Model fitting neuralnets in R 4.4 is returning a matrix
		# if ("matrix" %in% class(predictions))
        # {
         # predictions = predictions[,1] 
        # }
        
    }

 else if (modclass == "nnet.formula" && (dependentclass == "numeric")) {
        
        predictions <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ",type =\"raw\")", collapse = "", sep = "")))
			  predictions =as.numeric(predictions)
			 

        
    }
	

    else if (modclass == "multinom" && (dependentclass == "logical")) {
         savePredictedProbs <- eval(parse(text = paste("predict(", 
            modelname, ",", tmpstr2, ",type =\"prob\")", collapse = "", 
            sep = "")))
        predictions <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ")", collapse = "", sep = "")))
    }
    else if (modclass == "npregression") {
        predictions <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ",se.fit=TRUE)", collapse = "", sep = "")))
    }
    
	#Added 09/04/2020
	#Handle confidence interval of the predictions for 
	else if (modclass == "lm") {
		if (confinterval == TRUE)
		{
		#predict(model, newdata = new.speeds, interval = "prediction")
		predictions <- eval(parse(text = paste("stats::predict(", modelname, 
            ",", tmpstr2, ", interval =\"prediction\", level=" ,level, " )", collapse = "", sep = "")))
		
		predictions_conf_int <- eval(parse(text = paste("stats::predict(", modelname, 
            ",", tmpstr2, ", interval =\"confidence\", level=" ,level, " )", collapse = "", sep = "")))
		
		#print(predictions_conf_int[,c(2:3)])
		predictions = cbind(predictions_conf_int, PI_lwr = predictions[,2], PI_upr = predictions[,3])
		
		dimnames(predictions)[[2]] = c(paste0("Fit_",level*100), "CI_lwr", "CI_upr", "PI_lwr", "PI_upr")
		}
		else
		{
        predictions <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ")", collapse = "", sep = "")))
			
		}
    }
	
	
	
	
	#04/19/2020 The else below is NOT invoked for model class glm and dependent variable numeric
	#Its invoked for negbin and other glm models with different families
	
	else if(modclass =='coxph' ) 
	{
	
		if (BSkySurvival == TRUE && BSkySurvivalType =="CL")
		{
			# modelvars <- eval(parse(text=paste('as.character(',modelname,'$xnames)', sep='') ) )
			# tmpPredictions <- eval(parse(text = paste("predict(", modelname,
						# ",", ", type=\"survival\")", collapse = "",
						# sep = "")))
			# BSKyMarkerForMissing <- which(is.na(tmpPredictions))
			# #BSkyRawPredictions <- summary(survfit(CoxRegModel1, newdata=mockstudy), time=c(500, 1000))$surv
			
			#BSKyMarkerForMissing <- eval(parse (text = paste ("which(is.na(", tmpstr2, "))")))
				predictionProbs <- eval(parse(text =paste("summary(survfit(", modelname, ", newdata =", 
                                             tmpstr2, "),time =",
													deparse(substitute(BSkyTime)),")$surv"))) 
			BSKyMarkerForMissing =eval(parse(text =paste("stats::na.action(stats::na.exclude(", tmpstr2, "))") ))
				predictionProbs = base::t(predictionProbs)
			# Here is the trick: give the "bad" vector a class

			#class(BSKyMarkerForMissing) <- "exclude"
			predictionProbs <- stats::napredict(BSKyMarkerForMissing, predictionProbs)
		}
		else
		{
			 predictionProbs <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ", type=\"survival\")", collapse = "", sep = "")))
		}
	}
    
	else if(modclass =='flexsurvreg') 
	{
	
		if (BSkySurvival == TRUE && BSkySurvivalType =="CL")
		{
			
			# print((text =paste("summary(", modelname, ", newdata =", 
                                             # tmpstr2, ",t =",
													# deparse(substitute(BSkyTime)),")")))
													
			surv_predictionProbs <- eval(parse(text =paste("summary(", modelname, ", newdata =", 
                                             tmpstr2, ",t =",
													deparse(substitute(BSkyTime)),")"))) 
			#BSKyMarkerForMissing =eval(parse(text =paste("stats::na.action(stats::na.exclude(", tmpstr2, "))") ))
			#	predictionProbs = base::t(predictionProbs)
			
			# Here is the trick: give the "bad" vector a class

			#class(BSKyMarkerForMissing) <- "exclude"
			#predictionProbs <- stats::napredict(BSKyMarkerForMissing, predictionProbs)
			
			# Function to reorganize a single data frame
			reorganize_df <- function(df) {
			  df %>%
				pivot_longer(cols = -time, names_to = "metric", values_to = "value") %>%
				unite(temp, metric, time, sep = "") %>%
				pivot_wider(names_from = temp, values_from = value)
			}

			# Apply transformation to each element in the list
			surv_result = as.data.frame(bind_rows(lapply(surv_predictionProbs, reorganize_df), .id = "group"))[,c(-1)]
			fail_result = 1 - surv_result
			names(surv_result) = paste0("Surv_",names(surv_result))
			names(fail_result) = paste0("Fail_",names(surv_result))
			predictionProbs = cbind(surv_result, fail_result)
		}
		else
		{
			 predictionProbs <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ", type=\"survival\")", collapse = "", sep = "")))
		}
	}
    
	else {
        predictions <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ")", collapse = "", sep = "")))
			
		## Added Aaron 01/25/2025
		##Model fitting neuralnets in R 4.4 is returning a matrix
		if ("matrix" %in% class(predictions) )
        {
         predictions = predictions[,1] 
        }
    }
	


##################################################################################################################
#Here is where we saved the predicted values to the dataset
##################################################################################################################


   #######
   #Note we don't need string below because predictionsclass is never a string
    if (modclass == "train" && (predictionsclass == "factor" || predictionsclass == "ordered" || predictionsclass == "logical")  && ( classOfFinalModel !="ranger") ) 
####### 
	{
        df <- as.data.frame(savePredictedProbs)
        nm <- names(savePredictedProbs)
        ll <- length(nm)
        for (i in 1:ll) {
              savePredictedProbs[[i]] =round (savePredictedProbs[[i]], digits=noofDigitsToRound)
            aa <- paste(datasetname, "$", prefix, "_", nm[i],"_PredictedProbs", 
                "<<-savePredictedProbs[[", i, "]]", sep = "")
            eval(parse(text = aa))
        }
         if ((nlevels(predictions) == 2) && (predictionsclass == 
            "factor" || predictionsclass =="ordered"|| predictionsclass == "logical")) {
            predictedProbs = df[, 2]
            ROC = TRUE
        }
        else
        {
            predictedProbs =savePredictedProbs	
        }
    }
	
	#Code here is executed as dependent class must be numeric
	else if (modclass == "xgb.Booster" && (dependentclass == "numeric"|| dependentclass == "double" || dependentclass == "integer" ))  {
            predictions =data.frame(predictions)
            ll <- ncol(predictions)
           xgbobjective = eval( parse ( text = paste (modelname, "$params$objective")))
            if ( xgbobjective =="reg:squarederror")
            {
                 for (i in 1:ll) 
                 {
                    #There is only a single column in the data frame
					predictions[[i]] = round(predictions[[i]], digits = noofDigitsToRound)
					aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar))[i], "Pred_XG_Reg_SqErr", "<<-predictions[[", i, "]]", sep = "")
					eval(parse(text = aa))
					
					
				 }
                 #predictions =predictions[,1]
				 #Predictions are being set to null because we are treating the dependent variable as a numeric, and hence will not create a confusion matrix and a ROC curve
				 #
				  predictions = NULL
            }

          
            if ( xgbobjective =="reg:logistic")
            {
                 for (i in 1:ll) 
                 {
                    #There is only a single column in the data frame
					#predictions[[i]] = round(predictions[[i]], digits = noofDigitsToRound)
					predictions[[i]] = round(predictions[[i]], digits = noofDigitsToRound)
					aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar))[i], "Pred_XG_logistic", "<<-predictions[[", i, "]]", sep = "")
					eval(parse(text = aa))
					#This is to return the factor as thats what the confusionmatrix expects
					
				 }
				 #Predictions are being set to null because we are treating the dependent variable as a numeric, additionally the call to predictions does not return predictions but scores
                 predictions =NULL

            }

            if ( xgbobjective =="binary:logistic")
            {
              #  for (i in 1:ll) 
               #  {
                    #There is only a single column in the data frame
					#predictions[[i]] = round(predictions[[i]], digits = noofDigitsToRound)
					predictions[[1]] = round(predictions[[1]], digits = noofDigitsToRound)
					aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar))[1], "_Pred_XG_binary_logistic", "<<-round(predictions[[", 1, "]])", sep = "")
					eval(parse(text = aa))
					#This is to return the factor as thats what the confusionmatrix expects
					predictedProbs =predictions[,1]
				# }
                # predictions = ifelse( predictions >= 0.5, 0, 1)
                predictions =base::ifelse( predictions[,1] >= 0.5, 0, 1)
				aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar))[1], "_Pred_XG_binary_logistic", "<<-predictions[[", 1, "]]", sep = "")
					eval(parse(text = aa))
				ROC=TRUE
            }


            if ( xgbobjective =="binary:logitraw")
            {
                for (i in 1:ll) 
                 {
                    #There is only a single column in the data frame
					predictions[[i]] = round(predictions[[i]], digits = noofDigitsToRound)
					aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar))[1], "_Pred_XG_binary_logitraw", "<<-predictions[[", i, "]]", sep = "")
					eval(parse(text = aa))
					#This is to return the factor as thats what the confusionmatrix expects
					
				 }
				  #Predictions are being set to null because we are treating the dependent variable as a numeric, additionally the call to predictions does not return predictions but scores
                 predictions = NULL

            }

            if ( xgbobjective =="multi:softmax")
            {
                 for (i in 1:ll) 
                 {
                    #There is only a single column in the data frame
					predictions[[i]] = round(predictions[[i]], digits = noofDigitsToRound)
					aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar))[1], "_Pred_XG_softmax", "<<-predictions[[", i, "]]", sep = "")
					eval(parse(text = aa))
					#This is to return the factor as thats what the confusionmatrix expects
					
				 }
                 predictions =predictions[,1]
            }

            if ( xgbobjective =="multi:softprob")
            {
                for (i in 1:ll) 
                {
                    #There is only a single column in the data frame
					predictions[[i]] = round(predictions[[i]], digits = noofDigitsToRound)
					aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar))[1], "_Pred_XG_softprob_", as.character( i), "<<-predictions[[", i, "]]", sep = "")
					eval(parse(text = aa))
					#This is to return the factor as thats what the confusionmatrix expects
					
				}
                predictedProbs=predictions
				if (nlevels(factor(dependentvariable)) == 2) 
				{
					predictedProbs = predictions[, 2]
					ROC = TRUE
				}
				else
				{
					predictedProbs =predictions	
				}
				   #Predictions are being set to null because predictions does not return predictions but the probabilities
                 predictions =NULL
				 
            }

            if ( xgbobjective =="rank:pairwise")
            {
                 for (i in 1:ll) 
                 {
                    #There is only a single column in the data frame
					predictions[[i]] = round(predictions[[i]], digits = noofDigitsToRound)
					aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar))[1], "_Pred_XG_pairwise_", as.character( i), "<<-predictions[[", i, "]]", sep = "")
					eval(parse(text = aa))
					#This is to return the factor as thats what the confusionmatrix expects
					
				 }
				  #Predictions are being set to null because we are treating the dependent variable as a numeric, additionally the call to predictions does not return predictions but scores
                 predictions =NULL
            }
			predictionsSaved=TRUE
			
		
        
             # if ((ll == 2) && (dependentclass == 
            # "factor" || dependentclass == "character" || dependentclass == 
            # "ordered")) {
            # predictedProbs = df[, 2]
           


    }
	
	else if (modclass == "NaiveBayes" && (dependentclass == "factor" || dependentclass == "character" ||dependentclass == "ordered"))  {
        df <- as.data.frame(savePredictedProbs)
        nm <- base::make.names(names(df))
        ll <- length(nm)
        for (i in 1:ll) {
             df[[i]] =round (df[[i]], digits=noofDigitsToRound)
            aa <- paste(datasetname, "$", prefix, "_", nm[i], "_PredictedProbs",
                "<<-df[[", i, "]]", sep = "")
            eval(parse(text = aa))
        }
		if ((nlevels(predictions) == 2) && (dependentclass == 
            "factor" || dependentclass =="ordered"|| dependentclass == "logical")) {
            predictedProbs = df[, 2]
            ROC = TRUE
        }	
    }
	
	 else if (modclass == "nnet" && (dependentclass == "factor" || dependentclass == "character" ||dependentclass == "ordered"))  {
        df <- as.data.frame(savePredictedProbs)
        ##Set the names of the levels
        nm <- base::make.names(names(df))
        ll <- length(nm)
        for (i in 1:ll) {
              df[[i]] =round (df[[i]], digits=noofDigitsToRound)
            aa <- paste(datasetname, "$", prefix, "_", nm[i],"_PredictedProbs", 
                "<<-df[[", i, "]]", sep = "")
            eval(parse(text = aa))
        }
        if ((nlevels(dependentvariable) == 2) && (dependentclass == "factor"|| dependentclass == "character" || dependentclass == "ordered" )) {
            predictedProbs = df[, 1]
            ROC = TRUE
        }
        else
        {
            predictedProbs =savePredictedProbs	
        }
    }
else if (modclass == "xgb.Booster" && (dependentclass == "factor"|| dependentclass == "character" || dependentclass == "ordered" ))  {
        df <- as.data.frame(savePredictedProbs)
        nm <- base::make.names(names(df))
        ll <- length(nm)
        for (i in 1:ll) {
              df[[i]] =round (df[[i]], digits=noofDigitsToRound)
           # aa <- paste(datasetname, "$", prefix, "_", nm[i],"_PredictedProbs", 
            #    "<<-df[[", i, "]]", sep = "")
            aa <- paste(datasetname, "$", prefix, "_", nm[i],"_PredictedProbs", 
               "<-df[[", i, "]]", sep = "")
            eval(parse(text = aa))
        }
        if ((nlevels(dependentvariable) == 2) && (dependentclass == "factor" || dependentclass == "character"|| dependentclass == "ordered" )) {
            predictedProbs = df[, 1 ]
            ROC = TRUE
        }
        else
        {
            predictedProbs =savePredictedProbs	
        }
    }
	
	
 # else if (modclass == "mlp" && (dependentclass == "factor" || 
        # dependentclass == "character" || dependentclass == "ordered")) {
        # df <- as.data.frame(savePredictedProbs)
        # nm <- names(df)
     # #Added by Aaron 5/16/2020
	 # #This is the case where there are 2 dependent variables, this happens for neural nets, you dummy code a factor variable 
	 # #Think species in the iris dataset, you dummy code species, you get 2 scale variables
	 # ##in this case the depvar contains 2 variables separated by , and dependentvariable is NULL
	 
       # if (is.null(dependentvariable))
        # {
            # ll <- ncol(df)
            # for (i in 1:ll) {
                # df[[i]] = round(df[[i]], digits = noofDigitsToRound)
                # aa <- paste(datasetname, "$", prefix, "_", nm[i], 
                    # "_PredictedProbs", "<<-df[[", i, "]]", sep = "")
                # eval(parse(text = aa))
            # }
        
             # if ((ll == 2) && (dependentclass == 
            # "factor" || dependentclass == "character" || dependentclass == 
            # "ordered")) {
            # predictedProbs = df[, 2]
            # ROC = TRUE
            # }
            # else
            # {
            # predictedProbs = savePredictedProbs
            # }

        # }
        # else
        # {

            # ll <- length(nm)
            # for (i in 1:ll) {
                # df[[i]] = round(df[[i]], digits = noofDigitsToRound)
                # aa <- paste(datasetname, "$", prefix, "_", nm[i], 
                    # "_PredictedProbs", "<<-df[[", i, "]]", sep = "")
                # eval(parse(text = aa))
            # }
       
        # if ((nlevels(dependentvariable) == 2) && (dependentclass == "factor" || dependentclass == "character" || dependentclass ==  "ordered")) {
            # predictedProbs = df[, 2]
            # ROC = TRUE
        # }
        # else {
            # predictedProbs = savePredictedProbs
        # }
    # }

    # }
	
	
		
 else if (modclass == "mlp" && (dependentclass == "factor" )) {
     #   df <- as.data.frame(savePredictedProbs)
      #  nm <- names(df)
     #Added by Aaron 5/16/2020
	 #This is the case where there are 2 dependent variables, this happens for neural nets, you dummy code a factor variable 
	 #Think species in the iris dataset, you dummy code species, you get 2 scale variables
	 ##in this case the depvar contains 2 variables separated by , and dependentvariable is NULL
	 
     #if (is.null(dependentvariable))
     # {
	   
	
	   
            ll <- ncol(predictions)
            for (i in 1:ll) {
			#The predictions are numeric, so I round
			if (MultipleDependentVars)
				{
					predictions[[i]] = round(predictions[[i]], digits = noofDigitsToRound)
					aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar))[i], 
						"_Predictions", "<<-predictions[[", i, "]]", sep = "")
					eval(parse(text = aa))
				}
				else
				#The predictions are not numeric, so I dont round
				{
	
					#There is only a single column in the data frame
					#predictions[[i]] = round(predictions[[i]], digits = noofDigitsToRound)
					aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar))[i], "_Predictions", "<<-predictions[[", i, "]]", sep = "")
					eval(parse(text = aa))
					#This is to return the factor as thats what the confusionmatrix expects
					predictions =predictions[,1]
					
				}
            } #Close of for loop
			predictionsSaved=TRUE
			   ROC = FALSE
			 if (!MultipleDependentVars)
			{
	 			df <- as.data.frame(savePredictedProbs)
				
				nm <- levels(dependentvariable)
				noofvars <- length(nm)
				for (i in 1:noofvars) 
				{
				  df[[i]] =round (df[[i]], digits=noofDigitsToRound)
				  aa <- paste(datasetname, "$", prefix, "_", nm[i],"_PredictedProbs", "<<-df[[", i, "]]", sep = "")
				  eval(parse(text = aa))
				}
				if (noofvars==2) 
				{
						predictedProbs = df[, 2]
						ROC = TRUE
				}
				else
				{
						predictedProbs =savePredictedProbs	
				}
			}
		
        
             # if ((ll == 2) && (dependentclass == 
            # "factor" || dependentclass == "character" || dependentclass == 
            # "ordered")) {
            # predictedProbs = df[, 2]
         
            # }
            # else
            # {
            # predictedProbs = savePredictedProbs
            # }

        #}
		
        
    }
	
	
	 else if (modclass == "nn" && (dependentclass == "factor" )) {
     #   df <- as.data.frame(savePredictedProbs)
      #  nm <- names(df)
     #Added by Aaron 5/16/2020
	 #This is the case where there are 2 dependent variables, this happens for neural nets, you dummy code a factor variable 
	 #Think species in the iris dataset, you dummy code species, you get 2 scale variables
	 ##in this case the depvar contains 2 variables separated by , and dependentvariable is NULL
	 
       #if (is.null(dependentvariable))
       # {
            ll <- ncol(predictions)
            for (i in 1:ll) {
			#The predictions are numeric, so I round
			if (MultipleDependentVars)
				{
					predictions[[i]] = round(predictions[[i]], digits = noofDigitsToRound)
					aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar))[i], 
						"_Predictions", "<<-predictions[[", i, "]]", sep = "")
					eval(parse(text = aa))
				}
				else
				#The predictions are not numeric, so I dont round
				{
					#There is only a single column in the data frame
					#predictions[[i]] = round(predictions[[i]], digits = noofDigitsToRound)
					aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar))[i], "_Predictions", "<<-predictions[[", i, "]]", sep = "")
					eval(parse(text = aa))
					#This is to return the factor as thats what the confusionmatrix expects
					predictions =predictions[,1]
					
				}
            }
			predictionsSaved=TRUE
			 ROC = FALSE
			 if (!MultipleDependentVars)
			{
	 			df <- as.data.frame(savePredictedProbs)
				
				nm <- levels(dependentvariable)
				noofvars <- length(nm)
				for (i in 1:noofvars) 
				{
				  df[[i]] =round (df[[i]], digits=noofDigitsToRound)
				  aa <- paste(datasetname, "$", prefix, "_", nm[i],"_PredictedProbs", "<<-df[[", i, "]]", sep = "")
				  eval(parse(text = aa))
				}
				if (noofvars==2) 
				{
						predictedProbs = df[, 2]
						ROC = TRUE
				}
				else
				{
						predictedProbs =savePredictedProbs	
				}
			}
		
        
             # if ((ll == 2) && (dependentclass == 
            # "factor" || dependentclass == "character" || dependentclass == 
            # "ordered")) {
            # predictedProbs = df[, 2]
           
            # }
            # else
            # {
            # predictedProbs = savePredictedProbs
            # }

        #}
		
        
    }
	
	 # else if (modclass == "nn" && (dependentclass == "factor" )) {
     # #   df <- as.data.frame(savePredictedProbs)
      # #  nm <- names(df)
     # #Added by Aaron 5/16/2020
	 # #This is the case where there are 2 dependent variables, this happens for neural nets, you dummy code a factor variable 
	 # #Think species in the iris dataset, you dummy code species, you get 2 scale variables
	 # ##in this case the depvar contains 2 variables separated by , and dependentvariable is NULL
	 
       # if (is.null(dependentvariable))
        # {
            # ll <- ncol(predictions)
            # for (i in 1:ll) {
                # predictions[[i]] = round(predictions[[i]], digits = noofDigitsToRound)
                # aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar))[i], 
                    # "_Predictions", "<<-predictions[[", i, "]]", sep = "")
                # eval(parse(text = aa))
            # }
			
		
        
             # # if ((ll == 2) && (dependentclass == 
            # # "factor" || dependentclass == "character" || dependentclass == 
            # # "ordered")) {
            # # predictedProbs = df[, 2]
            # ROC = FALSE
            # # }
            # # else
            # # {
            # # predictedProbs = savePredictedProbs
            # # }

        # }
        
    # }


else if (modclass == "rsnns" && (dependentclass == "factor"|| dependentclass == "character" || dependentclass == "ordered" )) {
        df <- as.data.frame(savePredictedProbs)
        ##Set the names of the levels
        nm <- base::make.names(names(df))
        ll <- length(nm)
        for (i in 1:ll) {
              df[[i]] =round (df[[i]], digits=noofDigitsToRound)
            aa <- paste(datasetname, "$", prefix, "_", nm[i],"_PredictedProbs", 
                "<<-df[[", i, "]]", sep = "")
            eval(parse(text = aa))
        }
        if ((nlevels(dependentvariable) == 2) && (dependentclass == "factor"|| dependentclass == "character" || dependentclass == "ordered" )) {
            predictedProbs = df[, 2]
            ROC = TRUE
        }
        else
        {
            predictedProbs =savePredictedProbs	
        }
    }
	else if (modclass == "mlp" && (dependentclass == "numeric")) {
         
        predictions <- eval(parse(text = paste("predict(", modelname, ",", tmpstr2, ",type =\"raw\")", collapse = "", sep = "")))
		predictions =round (predictions, digits=noofDigitsToRound)
			aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar)), "_Predictions", "<<-predictions", sep = "")
					eval(parse(text = aa))
					predictionsSaved=TRUE
			
       
    }

 else if (modclass == "rsnns" && (dependentclass == "numeric")) {
         
        predictions <- eval(parse(text = paste("predict(", modelname, 
            ",", tmpstr2, ",type =\"raw\")", collapse = "", sep = "")))
			}


 else if (modclass == "nnet.formula" && (dependentclass == "factor"|| dependentclass == "character" || dependentclass == "ordered" )) {
        df <- as.data.frame(savePredictedProbs)
        ##Set the names of the levels
        nm <- base::make.names(names(df))
        ll <- length(nm)
        for (i in 1:ll) 
        {
              df[[i]] =round (df[[i]], digits=noofDigitsToRound)
            aa <- paste(datasetname, "$", prefix, "_", nm[i],"_PredictedProbs", 
                "<<-df[[", i, "]]", sep = "")
            eval(parse(text = aa))
        }
         if ((nlevels(dependentvariable) == 2) && (dependentclass == "factor"|| dependentclass == "character" || dependentclass == "ordered" )) {
            predictedProbs = df[, 1]
            ROC = TRUE
        }
        else
        {
            predictedProbs =savePredictedProbs	
        }
    }

	
    else if (modclass == "rpart" && (dependentclass == "factor"|| dependentclass == "character" || dependentclass == "ordered" )) {
        df <- as.data.frame(savePredictedProbs)
        nm <- base::make.names(names(df))
        ll <- length(nm)
        for (i in 1:ll) {
              df[[i]] =round (df[[i]], digits=noofDigitsToRound)
            aa <- paste(datasetname, "$", prefix, "_", nm[i],"_PredictedProbs", 
                "<<-df[[", i, "]]", sep = "")
            eval(parse(text = aa))
        }
        if ((nlevels(dependentvariable) == 2) && (dependentclass == "factor"|| dependentclass == "character" || dependentclass == "ordered" )) {
            predictedProbs = df[, 2]
            ROC = TRUE
        }
        else
        {
            predictedProbs =savePredictedProbs	
        }
    }
    else if (modclass == "rpart" && dependentclass == "logical") {
         df <- as.data.frame(predictions)
           names(df)=depvar
		   nm <- base::make.names(names(df))
        ll <- length(nm)
        for (i in 1:ll) {
              df[[i]] =round (df[[i]], digits=noofDigitsToRound)
            aa <- paste(datasetname, "$", prefix, "_" , nm[i],"_PredictedProbs",  
                "<<-df[[", i, "]]", sep = "")
            eval(parse(text = aa))
        }
        predictedProbs = predictions
        predictions = base::ifelse(predictions >= 0.5, TRUE, FALSE)
        ROC = TRUE
    }
    else if (modclass == "randomForest" && (dependentclass == "logical" || dependentclass == "factor" || dependentclass == "character")) 
{
		if (dependentclass =="logical")
		{
            df <- as.data.frame(savePredictedProbs)
            names(df)[1]=depvar
         }
		else
        {
        df <- as.data.frame(savePredictedProbs)
        }           
      #  df = eval(parse (text=paste ("lapply(savePredictedProbs, round, digits =", noofDigitsToRound, ")")))
      #  df <- as.data.frame(savePredictedProbs)
        nm <- base::make.names(names(df))
        ll <- length(nm)
         for (i in 1:ll) {
            df[[i]] =round (df[[i]], digits=noofDigitsToRound)
            #aa <- paste(datasetname, "$", prefix, "_prob_", nm[i], 
             #   "<<-round(df[[", i, "]],digits=" , noofDigitsToRound, ")", sep = "")
            aa <- paste(datasetname, "$", prefix, "_" ,  nm[i], "_PredictedProbs", 
                "<<-df[[", i, "]]", sep = "")
            eval(parse(text = aa))
        }
        if (dependentclass == "logical") {
            predictedProbs = savePredictedProbs
            ROC = TRUE
        }
        
        else if ((nlevels(dependentvariable) == 2) && (dependentclass == "factor"|| dependentclass == "character" || dependentclass == "ordered" ))        {
            predictedProbs = df[, 2]
            ROC = TRUE
        }
        else 
        {
             predictedProbs = savePredictedProbs
        }       
    }

     else if (modclass == "multinom" && (dependentclass == "logical" || dependentclass == "factor" || dependentclass == "character" ||dependentclass == "ordered")) 
{
		if (dependentclass =="logical")
		{
            df <- as.data.frame(savePredictedProbs)
            names(df)[1]=depvar
         }
		else
        {
        df <- as.data.frame(savePredictedProbs)
        }           
      #  df = eval(parse (text=paste ("lapply(savePredictedProbs, round, digits =", noofDigitsToRound, ")")))
      #  df <- as.data.frame(savePredictedProbs)
        nm <- base::make.names(names(df))
        ll <- length(nm)
         for (i in 1:ll) {
            df[[i]] =round (df[[i]], digits=noofDigitsToRound)
            #aa <- paste(datasetname, "$", prefix, "_prob_", nm[i], 
             #   "<<-round(df[[", i, "]],digits=" , noofDigitsToRound, ")", sep = "")
            aa <- paste(datasetname, "$", prefix, "_" ,  nm[i], "_PredictedProbs", 
                "<<-df[[", i, "]]", sep = "")
            eval(parse(text = aa))
        }
        if (dependentclass == "logical") {
            predictedProbs = savePredictedProbs
            ROC = TRUE
        }
        
        else if ((nlevels(dependentvariable) == 2) && (dependentclass == "factor"|| dependentclass == "character" || dependentclass == "ordered" ))       {
            predictedProbs = df[, 1]
            ROC = TRUE
        }
        else 
        {
             predictedProbs = savePredictedProbs
        }       
    }


 else if (modclass == "polr" && (dependentclass == "ordered" )) 
{
		if (dependentclass =="logical")
		{
            df <- as.data.frame(savePredictedProbs)
            names(df)[1]=depvar
         }
		else
        {
        df <- as.data.frame(savePredictedProbs)
        }           
      #  df = eval(parse (text=paste ("lapply(savePredictedProbs, round, digits =", noofDigitsToRound, ")")))
      #  df <- as.data.frame(savePredictedProbs)
        nm <- base::make.names(names(df))
        ll <- length(nm)
         for (i in 1:ll) {
            df[[i]] =round (df[[i]], digits=noofDigitsToRound)
            #aa <- paste(datasetname, "$", prefix, "_prob_", nm[i], 
             #   "<<-round(df[[", i, "]],digits=" , noofDigitsToRound, ")", sep = "")
            aa <- paste(datasetname, "$", prefix, "_" ,  nm[i], "_PredictedProbs", 
                "<<-df[[", i, "]]", sep = "")
            eval(parse(text = aa))
        }
        if (dependentclass == "logical") {
            predictedProbs = savePredictedProbs
            ROC = TRUE
        }
        
        else if ((nlevels(dependentvariable) == 2) && (dependentclass ==  "factor"))        {
            predictedProbs = df[, 2]
            ROC = TRUE
        }
        else 
        {
             predictedProbs = savePredictedProbs
        }       
    }

    else if (modclass == "glm" && (dependentclass == "factor" || dependentclass == "ordered")) {
       
        
		
		# We are assuming that this is a logistic model
          
       # if (nlevels(dependentvariable) == 2) 
        if (fly =="binomial")
        {
             predictedProbs = predictions
			predictions = base::ifelse(predictions >= 0.5, levels(dependentvariable)[2], 
                levels(dependentvariable)[1])
			#Added 04/19/2020
			#The code abobe makes predictions a string, we need to convert to factor
			if (dependentclass =="factor")
			{
				predictions =factor(predictions, levels =levels(dependentvariable))
			}
			if (dependentclass =="ordered")
			{
				predictions =factor(predictions, levels =levels(dependentvariable), ordered=TRUE)
			}
			
	
			#Added 04/19/2020 Predictions is a string for 
			
            ROC = 	TRUE
            predictedProbs = round (predictedProbs,noofDigitsToRound) 
            bb <- paste(datasetname, "$", prefix, "_" , depvar,  "_PredictedProbs<<-predictedProbs ", 
                sep = "")
            eval(parse(text = bb))
        }
    }
####new 
    else if (modclass == "glm" && ((dependentclass == "numeric") || dependentclass =="integer")) {
	
		if (fly =="binomial")
        {
			predictedProbs = predictions
            predictedProbs = round (predictedProbs,digits=noofDigitsToRound) 
			#04/19/2020
			#not necessary as the algorithm only takes 0-1 
			#highlevel =max(dependentvariable)
			#lowlevel=min(dependentvariable)
            predictions = base::ifelse(predictions >= 0.5, 1, 0)
			predictions =as.numeric(predictions)
            ROC = TRUE
            bb <- paste(datasetname, "$", prefix,"_" , depvar,  "_PredictedProbs<<-predictedProbs ", 
                sep = "")
            eval(parse(text = bb))
		}
   
    }

    else if (modclass == "glm" && (dependentclass == "logical")) {
    #Predictions contain TRUE FALSE
        predictedProbs = predictions
        predictedProbs = round (predictedProbs,noofDigitsToRound) 
        predictions = base::ifelse(predictions >= 0.5, TRUE, FALSE)
        ROC = TRUE
        bb <- paste(datasetname, "$", prefix, "_" , depvar, "_PredictedProbs<<-predictedProbs ", 
            sep = "")
        eval(parse(text = bb))
    }
	
	else if (modclass == "C5.0" && (dependentclass == "factor" ||   dependentclass == "character" || dependentclass == "ordered")) {
        df <- as.data.frame(savePredictedProbs)
        nm <- base::make.names(names(df))
        ll <- length(nm)
        for (i in 1:ll) {
              df[[i]] =round (df[[i]], digits=noofDigitsToRound)
          #  aa <- paste(datasetname, "$", prefix, "_", nm[i],"_PredictedProbs", 
            #    "<<-df[[", i, "]]", sep = "")
         aa <- paste(datasetname, "$", prefix, "_", nm[i],"_PredictedProbs", 
                "<<-df[[", i, "]]", sep = "")
            eval(parse(text = aa))
        }
        if ((nlevels(dependentvariable) == 2) && (dependentclass == "factor" ||   dependentclass == "character" || dependentclass == "ordered"))  {
            predictedProbs = df[, 2]
            ROC = TRUE
        }
        else
        {
            predictedProbs =savePredictedProbs	
        }
    }
	
    else if (modclass == "npregression") {
        df <- as.data.frame(predictions)
        nm <- base::make.names(names(df))
        ll <- length(nm)
        for (i in 1:ll) {
            aa <- paste(datasetname, "$", prefix, "_", nm[i], 
                "<<-df[[", i, "]]", sep = "")
            eval(parse(text = aa))
        }
    }
	
	 else if (modclass == "lm" || "lm" %in% classOfFinalModel) 
	{
		if ( substr(x=depvar,start=1,stop=1) =="\'")
		{
			depvar = substr(x=depvar,start=2, stop=nchar(depvar)-1)
		}	
		if (confinterval ==TRUE)
		{
			df <- as.data.frame(predictions)
			df <-round(df[,], noofDigitsToRound) 
			nm <- base::make.names(names(df))
			ll <- length(nm)
			for (i in 1:ll) 
			{
				aa <- paste(datasetname, "$", prefix, "_", depvar, "_", nm[i], 
					"<<-df[[", i, "]]", sep = "")
				eval(parse(text = aa))
			}
			predictionsSaved = TRUE
			#predictions = df$fit
			predictions = df[,1]
		}
		else
		{
		predictions = round (predictions,noofDigitsToRound) 
		aa <- paste(datasetname, "$", prefix, "_" , depvar, "_Fit<<-predictions", 
			sep = "")
		eval(parse(text = aa))
		predictionsSaved = TRUE
		
		}
		
       
    }
	
else if (modclass == "coxph" && (dependentclass == "numeric"|| dependentclass == "double" || dependentclass == "integer" ))
{
	predictionProbs =data.frame(predictionProbs)
    ll <- ncol(predictionProbs )
    for (i in 1:ll) 
    {
		# #There is only a single column in the data frame
		# predictions[[i]] = round(predictions[[i]], digits = noofDigitsToRound)
		# aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar))[1],"_", BSkyTime[i], "<<-predictions[[", i, "]]", sep = "")
		# eval(parse(text = aa))
		predictions = base::ifelse(predictionProbs[[i]] >= 0.5, 1, 0)
		#There is only a single column in the data frame
		predictionProbs[[i]] = round(predictionProbs[[i]], digits = noofDigitsToRound)
		aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar))[1],"_", BSkyTime[i], "<<-predictions", sep = "")
		eval(parse(text = aa))
		aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar))[1],"_Prob_", BSkyTime[i], "<<-predictionProbs[[", i, "]]", sep = "")
		eval(parse(text = aa))
	}
    predictions = base::ifelse(predictionProbs[[1]] >= 0.5, 1, 0)
	predictedProbs = predictionProbs[[1]]
	ROC = TRUE
	predictionsSaved = TRUE
}

else if (modclass == "flexsurvreg")
{
	#BSkyFormat(predictionProbs)
	#BSkyFormat(eval(parse(text=(paste(datasetname)))))
	#print(prefix)
	
	if(trimws(prefix) == '')
	{
		#print(paste(datasetname, "<<-", "cbind(", datasetname, ", predictionProbs)"))
		eval(parse(text=(paste(datasetname, "<<-", "cbind(", datasetname, ", predictionProbs)"))))
		#BSkyFormat(eval(parse(text=(paste(datasetname)))))
	}
	else
	{
		names(predictionProbs) = paste0(paste0(prefix,"_"),names(predictionProbs))
		#print(paste(datasetname, "<<-", "cbind(", datasetname, ", predictionProbs)"))
		eval(parse(text=(paste(datasetname, "<<-", "cbind(", datasetname, ", predictionProbs)"))))
		#BSkyFormat(eval(parse(text=(paste(datasetname)))))
	}
	
    # ll <- ncol(predictionProbs )
    # for (i in 1:ll) 
    # {
		# # #There is only a single column in the data frame
		# # predictions[[i]] = round(predictions[[i]], digits = noofDigitsToRound)
		# # aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar))[1],"_", BSkyTime[i], "<<-predictions[[", i, "]]", sep = "")
		# # eval(parse(text = aa))
		# predictions = base::ifelse(predictionProbs[[i]] >= 0.5, 1, 0)
		# #There is only a single column in the data frame
		# predictionProbs[[i]] = round(predictionProbs[[i]], digits = noofDigitsToRound)
		# aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar))[1],"_", BSkyTime[i], "<<-predictions", sep = "")
		# eval(parse(text = aa))
		# aa <- paste(datasetname, "$", prefix, "_",eval(parse(text=depvar))[1],"_Prob_", BSkyTime[i], "<<-predictionProbs[[", i, "]]", sep = "")
		# eval(parse(text = aa))
	# }
    predictions = predictionProbs
	ROC = FALSE
	predictionsSaved = TRUE
}


	
	#Added by Aaron 5/16/2020
	 #When i come to the code below in most cases predictionsSaved = FALSE, note predictions are generated in code above
	 #This is the case where there are 2+ dependent variables, this happens for neural nets, you dummy code a factor variable 
	 #Think species in the iris dataset, you dummy code species, you get 2 scale variables
	 #in this case depvar contains multiple variables separated by , and dependentvariable is null
	 #Also MultipleDependentVars is TRUE and predictions are saved to the dataset above
	 # When MultipleDependentVars is FALSE there we run predictions below and the dependent variable is numeric
	 #PredictionsSaved is used by mlp to indicate that the predictions are saved		
	 
	if (! predictionsSaved)
    {
		if (class(predictions) =="numeric" ||class(predictions) =="double" || class(predictions) =="integer")
		{
		predictions = round (predictions,noofDigitsToRound) 
		}

	# added by Aaron and Anil to handle the extra quotes added by the model tuning functions 
	# attr(MixedModel1, "depvar") <-"'frequency'"
	# attr (MixedModel1, "indepvar") ="c('gender', 'scenario', 'subject', 'attitude')"
	
	#Code I added 07/26/2023, I just added the top if to check for null
	# This code supports a dependent variable = null for scoring cox regression models
	# i.e. codels where the dependent variable does not exist and the class of the dependent variable does not exist
	if (!base::is.null(depvar))
    {
      if (substr(x = depvar, start = 1, stop = 1) == "'") 
	  {
            depvar = substr(x = depvar, start = 2, stop = nchar(depvar) -
                1)
      }
    }
	
		#Original code that I commented on 07/26/2023
		# if ( substr(x=depvar,start=1,stop=1) =="\'")
		# {
		# depvar = substr(x=depvar,start=2, stop=nchar(depvar)-1)
		# }
		aa <- paste(datasetname, "$", prefix, "_" , depvar, "_Predictions<<-predictions", 
			sep = "")
		eval(parse(text = aa))
	}
	
	# 06/01/2020
	#Added this code as ROCR::prediction requires a numeric, it will not work with an integer
	#Code I added 07/26/2023
	# This code supports a dependent variable = null for scoring cox regression models
	# i.e. codels where the dependent variable does not exist and the class of the dependent variable does not exist
	
	if (!base::is.null(dependentclass))
    {
		if (dependentclass == "integer") {
			dependentvariable = as.numeric(dependentvariable)
		}
    }
	#Aaron Added 02/16/2022
	#Restoring dependentvariable to original value so that the confusion matrix 
	#does not display if the dependentvariable does not exist, this will happen when the 
	#database scored does not contain the dependent variable
	#Code I added 07/26/2023
	# This code supports a dependent variable = null for scoring cox regression models
	# i.e. codels where the dependent variable does not exist and the class of the dependent variable does not exist
	if (!base::is.null(depvar))
	{
	dependentvariable <- eval(parse(text = paste(datasetname, 
                "$", depvar, sep = "")))
	}else {
	dependentvariable =NULL
	}
	
    return(list(predictions, predictedProbs, dependentvariable, 
        ROC))
}



#Generates the confusion matrix for train classes created by model tuning
### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Confusion matrix train
#'
#' @description 
#'
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param
#' @param
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param
#' @param
#'
#' @return
#'
#' @examples
BSkyConfusionMatrixTrain <- function (predictions, reference, levelOfInterest ="")
{

if (class(predictions) =="factor" ||  class(predictions) =="ordered" || class(predictions) =="logical"  )
{
    
	if (nlevels(reference) ==2)
					{
					
						#The code below works
						##########################################################################
						#positive=levels(dependentvariable)[2]
						#bskyconfmatrix <- caret::confusionMatrix(predictions, dependentvariable,positive=tail (levels(dependentvariable),1))
						#bskyconfmatrix <- caret::confusionMatrix(predictions, dependentvariable,positive=positive)
						#BSkyFormat(bskyconfmatrix$table, singleTableOutputHeader = paste("Confusion Matrix, positive level: ", positive, sep="",collapse="") )
						#################################################################################################
						# Code tat I tried to display the confusion matrix where the level of interest is in the 1st column
						#The code below has the advantage that the confusion matrix displays the variable of interest first
						
						
						if (which(levels(reference) ==levelOfInterest) ==1)
						{
							levelOfInterest ="1st"
						} else{
						levelOfInterest ="2nd"
						}
							
						
						if (levelOfInterest =="2nd")
						#Code here is introduced for Ross at mayo so that the confusion matrix is displayed differently based on the level of interest
						#This controls whether the top left of the confusion matrix is 0 or 1
						#Accuracy and kappa statistics are calculated based on the reference
						{
							predictions =factor (predictions, levels =rev(levels(reference)))
							reference=factor (reference, levels =rev(levels(reference)))
							#Since we have reversed the levels, R by default sets the 1st reference as the default which is the 2nd due to the reverse, there is no need to set the 2nd as default
							positive=levels(reference)[1]
							bskyconfmatrix <- caret::confusionMatrix(predictions, reference,positive=positive)
							BSkyFormat(bskyconfmatrix$table, singleTableOutputHeader = paste("Confusion Matrix, positive level: ", positive, sep="",collapse="") )
						} 
						if (levelOfInterest =="1st")
						{
							positive=levels(reference)[1]
							bskyconfmatrix <- caret::confusionMatrix(predictions, reference,positive=positive)
							BSkyFormat(bskyconfmatrix$table, singleTableOutputHeader = paste("Confusion Matrix, positive level: ", positive, sep="",collapse="") )
						
						
						}
					}
	else
	{
	
	bskyconfmatrix <- caret::confusionMatrix(predictions,reference)
	
    BSkyFormat(bskyconfmatrix$table,singleTableOutputHeader = "Confusion Matrix (Predictions made using optimized model on training dataset)")
	}
    accuracyKappaStats <- as.matrix(bskyconfmatrix$overall)
	rownames(accuracyKappaStats) <- c("Accuracy","Kappa","Accuracy: 95%CILower","Accuracy: 95%CIUpper","Accuracy: No Info Rate","Accuracy: PValue", "Mcnemar's Test: PValue")
    colnames(accuracyKappaStats) <- c("Values")
    BSkyFormat(as.matrix(accuracyKappaStats),singleTableOutputHeader = "Accuracy and Kappa Statistics (Statistics computed using predictions made using optimized model on training dataset)")
    additionalStats <- as.matrix(bskyconfmatrix$byClass)
    if (((class(predictions) == "factor" ||class(predictions) == "ordered") &&   nlevels(predictions) <= 2) || class(predictions) ==  "logical") 
    {
        colnames(additionalStats) <- c("Values")
    }
    BSkyFormat(as.matrix(additionalStats),singleTableOutputHeader = "Additional Statistics (Statistics computed using predictions made using optimized model on training dataset)")
}
else 
{
msg = paste0("ERROR: Confusion matrix and ROC curve cannot be created as the class of the dependent variable is numeric", 
                  collapse = "", sep = "")
                cat("\n", msg)
                return
}
	
}

#Creates the confusion matrix for models created by the specific modeling algorithm
### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Confusion matrix
#'
#' @description Creates a confusion matrix by cross-tabulating the observed and predicted classes with associated statistics. 
#'
#' @param modelname a model object for which confusion matrix is desired.
#' @param showCofusionMatrix  logical, if TRUE the confusion matrix is generated (if it applies), if FALSE, confusion matrix is not generated.
#' @param predictions an object that is returned as a result of predict() call.
#' @param datasetname is the current datasetname using which we want to make predictions.
#'
#' @return Displays the confusion matrix using the function confusionMatrix in the package caret
#'
#' @examples
BSkyConfusionMatrix<-function (modelname, showConfusionMatrix = FALSE, predictions, 
    datasetname,levelOfInterest = "") 
{
    fly=""
	modclass = character(0)
    modclass = getModelClass(modelname)
	
	if (is.null(modclass)) {
                msg = paste0("ERROR: Could not find model class. Further processing cantnot be done. Aborting..", 
                  collapse = "", sep = "")
                cat("\n", msg)
              
            }
    else if (showConfusionMatrix) {
        if (!is.null(predictions)) {
            msg = character(0)
            
            depvar <- getModelDependentVariable(modelname)
			#Added by Aaron 05/20
			#Added this as neuralnet and mlp dialogs put a c() around the dependent variable
			if (str_detect(depvar, "c\\(")) 
			{
				#We strip out the dependent variable in the format "'test'"
			   depvar <-str_extract_all(depvar, "'[A-Z,a-z,0-9,_,.]*'")

				#dependentvariable <- eval(parse(text = paste(datasetname, 
				  #  "$", eval(parse(text = depvar)), sep = "")))
				#dependentclasses = eval(parse(text = paste("class(dependentvariable)", 
				 #   sep = "")))
				#if (length(dependentclasses) > 1) {
				 #   dependentclass = dependentclasses[1]
				#}
				#else if (length(dependentclasses) == 1) {
				 #   dependentclass = dependentclasses
				#}
			}
			
            dependentvariable <- eval(parse(text = paste(datasetname, 
                "$", depvar, sep = "")))
            if (is.null(dependentvariable)) 
			{
                msg = paste0("Confusion matrix and ROC curve cannot be created as the dependent variable is missing in the dataset", 
                  collapse = "", sep = "")
                cat("\n", msg)
                
            }
			
			else
			{
				#  dependentclass = eval(parse(text = paste("class(dependentvariable)", 
				#   sep = "")))
				# dependentclass = getDependentVariableClass(dependentvariable)

			#START
			dependentclasses = eval(parse(text = paste("class(dependentvariable)", 
            sep = "")))
        
			if(length(dependentclasses)>1)
			{
				dependentclass=dependentclasses[1] #pick the first class (eg from "glm" "lm" pick "glm")
			}
			else if(length(dependentclasses)==1)
			{
				dependentclass=dependentclasses
			}
			

            if (modclass=="glm")
            {
             fly =eval( parse(text=paste ("family(" ,modelname, ")$family") ))
            }
            generateConfusionmatrix = FALSE
            if ((modclass == "randomForest" && (dependentclass =="factor" || dependentclass == "character" || dependentclass == "logical")) || (modclass == "mlp" && (dependentclass =="factor" || dependentclass == "character" || dependentclass == "ordered" )) ||(modclass == "nn" && (dependentclass =="factor" || dependentclass == "character" || dependentclass == "ordered" )) || (modclass == "xgb.Booster" && (dependentclass == "factor" ||   dependentclass == "character" || dependentclass == "ordinal")) ||(modclass == "rsnns" && (dependentclass =="factor" || dependentclass == "character" || dependentclass == "ordered"  ))|| (modclass == "nnet" && (dependentclass =="factor" || dependentclass == "character" || dependentclass == "ordered" )) || ( modclass == "nnet.formula" && (dependentclass =="factor" || dependentclass == "character" || dependentclass == "ordered")) ||(modclass == "polr" && (dependentclass == "ordered" ))  || modclass == "table" || modclass == "NaiveBayes" || (modclass == "rpart" && (dependentclass == "factor" || dependentclass == "character" || dependentclass == "logical")) ||  modclass == "multinom" || (modclass == "ksvm" &&                 (dependentclass == "factor" || dependentclass ==  "character")) || (modclass == "adaboost" &&                 (dependentclass == "factor" || dependentclass ==                   "character")) || (modclass == "real_adaboost" &&                 (dependentclass == "factor" || dependentclass ==                   "character")) || (modclass == "fast_adaboost" &&                 (dependentclass == "factor" || dependentclass ==                   "character")) || (modclass == "knn3" && (dependentclass ==                 "factor" || dependentclass == "character")) ||                 (modclass == "glm" && (dependentclass == "factor" ||                   dependentclass == "logical"  || fly == "binomial"))  ||(modclass == "C5.0" && (dependentclass == "factor" || dependentclass == "ordered" || dependentclass == "character" )) || (modclass == "coxph")) {
                generateConfusionmatrix = TRUE
            }
			
			#Added the code 06/01/2020 as this is the condition where we create a confusionmatrix
			else if (modclass =="xgb.Booster")
			{
				xgbobjective = eval( parse ( text = paste (modelname, "$params$objective")))
				if ( xgbobjective =="multi:softmax"  || xgbobjective ==  "binary:logistic")
				{
				generateConfusionmatrix = TRUE
				}
				
				
			}
            else 
			{
                if (!(dependentclass == "factor" || dependentclass == "character") ) {
                  msg = paste("Error: The creation of a confusion matrix for a dependent variable of class ", 
                    dependentclass, " is not supported.")
                  cat("\n", msg)
                }
                if (!(modclass == "randomForest" || modclass ==  "NaiveBayes" || modclass == "rpart" || modclass =="ksvm" || modclass == "adaboost" || modclass ==  "real_adaboost" || modclass == "fast_adaboost" ||modclass == "knn3" || modclass == "glm" ||  modclass == "lm" || modclass == "npregression"))
				{
                  msg = paste("Error: The creation of a confusion matrix for a model of class ", 
                    modclass, " and dependent variable of class ", dependentclass, " is not supported. Please contact BlueSky Statistics at support@blueskystatistics.com")
                  cat("\n", msg)
                }
				#NOTE: RETURN IN R DOES NOT EXIT, THE REYRN BELOW IS USELESS, WE NEED TO USE STOP, HOWEVER SINCE ALL THE IF CONDITIONS
				#BELOW ARE NOT MET, THE CODE FALLS OUT
                return
            }
            bskyconfmatrix <- NULL
            if (generateConfusionmatrix && showConfusionMatrix) {
                if (modclass == "NaiveBayes") 
				{
					if (nlevels(dependentvariable) ==2)
					{
						#Added 09/03/2022
						# This is in the rare case when I am building a binomial logistic model and the dependent variable is integer
						#open the titanic dataset in the neuralnet folder in the install directory
						#in this case, as the dependent is numeric, we cannot allow the user to select the reference level
						indexofMatch =base::which(levels(dependentvariable) ==levelOfInterest)
						
						if (length(indexofMatch) ==0)
						{
							levelOfInterest ="1st"
						}
						else
						{
							if (base::which(levels(dependentvariable) ==levelOfInterest) ==1)
							{
								levelOfInterest ="1st"
							} else{
							levelOfInterest ="2nd"
							}
						}
						if ( levelOfInterest == "2nd")
						{
						#Code here is introduced for Ross at mayo so that the confusion matrix is displayed differently based on the level of interest
						#This controls whether the top left of the confusion matrix is 0 or 1
						#Accuracy and kappa statistics are calculated based on the reference
						positive=levels(dependentvariable)[2]
						predictions =factor (predictions, levels =rev(levels(dependentvariable)))
						dependentvariable=factor (dependentvariable, levels =rev(levels(dependentvariable)))
						bskyconfmatrix <- caret::confusionMatrix(predictions, dependentvariable,positive=positive)
						BSkyFormat(bskyconfmatrix$table, singleTableOutputHeader = paste("Confusion Matrix, positive level: ", positive, sep="",collapse="") )
						}
						
						if ( levelOfInterest == "1st")
						{
						
						positive=levels(dependentvariable)[1]
						#predictions =factor (predictions, levels =rev(levels(dependentvariable)))
						#dependentvariable=factor (dependentvariable, levels =rev(levels(dependentvariable)))
						bskyconfmatrix <- caret::confusionMatrix(predictions, dependentvariable,positive=positive)
						BSkyFormat(bskyconfmatrix$table, singleTableOutputHeader = paste("Confusion Matrix, positive level: ", positive, sep="",collapse="") )
						}
						
					}
					else
					{
					bskyconfmatrix <- caret::confusionMatrix(predictions, 
                    dependentvariable)
					 BSkyFormat(bskyconfmatrix$table, singleTableOutputHeader = "Confusion Matrix")
					}
                }
                else {
#Confusion matrix requires both predictions and dependent variable to be factors
#The check for whether we can generate a confusion matrix is already done above, so 
#if its a regression we will not generate a confusion matrix

####################################################################################################
#The code below is duplicated in the ROC table and Predict.r
####################################################################################################

					predictionsclass = eval(parse(text = paste("class(predictions)", sep = "")))
					if ((dependentclass =="logical" || dependentclass =="numeric"  || dependentclass =="integer"))
                    {
                        
						#When generating the confusion matrix the dependent variable and predictions must contain the same levels
					   dependentvariable =as.factor(dependentvariable)
						predictions =factor(predictions, levels =levels(dependentvariable))
                    }
#This handles the fact that predictions is a factor and dependent variable is a string
					else if (dependentclass =="character" &&  predictionsclass =="factor" && (modclass=="rpart"|| modclass =="randomForest"|| modclass=="multinom"|| modclass =="C5.0"))
					{
						dependentvariable =as.factor(dependentvariable)
					}
					# Added by Aaron 04/19/2020, commented code below and moved the code to the BSKYPredict in the save predicted probs section
                    # else if ((dependentclass =="factor" || dependentclass =="ordered") &&  predictionsclass =="character" && modclass=="glm" && fly =="binomial")
                    # {
                        # predictions =as.factor(predictions)
                        # dependentvariable =as.factor(dependentvariable)
                    # }
					 else if (dependentclass =="character" &&  predictionsclass =="character" && (modclass=="nnet" || modclass =="nnet.formula"))
                    {
                        
                        dependentvariable =as.factor(dependentvariable)
						predictions =factor(predictions, levels =levels(dependentvariable)) #as.factor(predictions, levels =levels(dependentvariable))
                    }
					else if (dependentclass =="character" &&  predictionsclass =="factor" && (modclass=="mlp" || modclass =="rsnns" || modclass=="xgb.Booster"))
                    {
                      #  predictions =as.factor(predictions)
                        dependentvariable =as.factor(dependentvariable)
                    }
                    
					if (nlevels(dependentvariable) ==2)
					{
					
					#The code below works
					##########################################################################
					#positive=levels(dependentvariable)[2]
					#bskyconfmatrix <- caret::confusionMatrix(predictions, dependentvariable,positive=tail (levels(dependentvariable),1))
					#bskyconfmatrix <- caret::confusionMatrix(predictions, dependentvariable,positive=positive)
					#BSkyFormat(bskyconfmatrix$table, singleTableOutputHeader = paste("Confusion Matrix, positive level: ", positive, sep="",collapse="") )
					#################################################################################################
					# Code tat I tried to display the confusion matrix where the level of interest is in the 1st column
					#The code below has the advantage that the confusion matrix displays the variable of interest first
					# positive=levels(dependentvariable)[2]
					# predictions =factor (predictions, levels =rev(levels(dependentvariable)))
					# dependentvariable=factor (dependentvariable, levels =rev(levels(dependentvariable)))
					# bskyconfmatrix <- caret::confusionMatrix(predictions, dependentvariable,positive=positive)
					# BSkyFormat(bskyconfmatrix$table, singleTableOutputHeader = paste("Confusion Matrix, positive level: ", positive, sep="",collapse="") )
					

					#Added 09/03/2022
						# This is in the rare case when I am building a binomial logistic model and the dependent variable is integer
						#open the titanic dataset in the neuralnet folder in the install directory
						#in this case, as the dependent is numeric, we cannot allow the user to select the reference level
						indexofMatch =base::which(levels(dependentvariable) ==levelOfInterest)
						
						if (length(indexofMatch) ==0)
						{
							levelOfInterest ="1st"
						}
						else
						{
							if (base::which(levels(dependentvariable) ==levelOfInterest) ==1)
							{
								levelOfInterest ="1st"
							} else{
							levelOfInterest ="2nd"
							}
						}
					
					if ( levelOfInterest == "2nd")
						{
						#Code here is introduced for Ross at mayo so that the confusion matrix is displayed differently based on the level of interest
						#This controls whether the top left of the confusion matrix is 0 or 1
						#Accuracy and kappa statistics are calculated based on the reference
						positive=levels(dependentvariable)[2]
						predictions =factor (predictions, levels =rev(levels(dependentvariable)))
						dependentvariable=factor (dependentvariable, levels =rev(levels(dependentvariable)))
						bskyconfmatrix <- caret::confusionMatrix(predictions, dependentvariable,positive=positive)
						BSkyFormat(bskyconfmatrix$table, singleTableOutputHeader = paste("Confusion Matrix, positive level: ", positive, sep="",collapse="") )
						}
						
						if ( levelOfInterest == "1st")
						{
						
						positive=levels(dependentvariable)[1]
						#predictions =factor (predictions, levels =rev(levels(dependentvariable)))
						#dependentvariable=factor (dependentvariable, levels =rev(levels(dependentvariable)))
						bskyconfmatrix <- caret::confusionMatrix(predictions, dependentvariable,positive=positive)
						BSkyFormat(bskyconfmatrix$table, singleTableOutputHeader = paste("Confusion Matrix, positive level: ", positive, sep="",collapse="") )
						}
					
					
					
					
					}
					else
					{
					bskyconfmatrix <- caret::confusionMatrix(predictions, 
                    dependentvariable)
					 BSkyFormat(bskyconfmatrix$table, singleTableOutputHeader = "Confusion Matrix")
					}
					#bskyconfmatrix <- caret::confusionMatrix(predictions, dependentvariable,positive=tail (levels(dependentvariable),1))
####################################################################################################					

                }
               
                accuracyKappaStats <- as.matrix(bskyconfmatrix$overall)
				rownames(accuracyKappaStats) <- c("Accuracy","Kappa","Accuracy: 95%CILower","Accuracy: 95%CIUpper","Accuracy: No Info Rate","Accuracy: PValue", "Mcnemar's Test: PValue")
                colnames(accuracyKappaStats) <- c("Values")
                BSkyFormat(as.matrix(accuracyKappaStats), singleTableOutputHeader = "Accuracy and Kappa Statistics")
                additionalStats <- as.matrix(bskyconfmatrix$byClass)
                ## Added Aaron 01/25/2025
				## Error: 'length = 2' in coercion to 'logical(1)' was returned by colnames(additionalStats) <- c("Values")
				## The above was an issue when scoring ordinal regression models
                if (( "factor" %in% class(dependentvariable) && nlevels(dependentvariable) <= 2) ||  "logical" %in% class(dependentvariable)) {
                  colnames(additionalStats) <- c("Values")
                  
                }
                BSkyFormat(as.matrix(additionalStats), singleTableOutputHeader = "Additional Statistics")
            }
        }
		}
        else {
		
			if (modclass =="xgb.Booster")
			{
				 xgbobjective = eval( parse ( text = paste (modelname, "$params$objective")))
				 if (xgbobjective != "multi:softprob")
				 {
					cat("A confusion matrix and ROC curve is not generated when the objective selected is one of \nreg:squarederror, reg:logistic, binary:logitraw and rank:pairwise as the predict function \ndoes not return the class of the dependent variable. The numeric predictions \n(in the case of reg:squarederror), scores, ranks etc that the predict function returns \nare stored in the dataset. See help(predict.xgb.Booster) for more details \n")
				 }
				 if (xgbobjective == "multi:softprob")
				 {
					cat("A confusion matrix is not generated when the objective selected is multi:softprob as the predict\nfunction returns the predicted probabilities and not the class of the dependent variable. \nThe predicted probabilities are saved in the dataset and the ROC curve is generated.\nTo see the confusion matrix, select multi:softmax as the objective ")
				 }
			}
			else
			{
            cat("Error: A confusion matrix cannot be created as the predicted values are NULL \n")
			}
        }
    }
}






























