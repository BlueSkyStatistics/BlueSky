#### by default all the checks will be run as they are set to TRUE. If you dont want to run any check for any reason, make it FALSE
#### while calling this function
#### validateDatasetCheck : if TRUE it will verify that the dataset is valid and also prefix curdatasetname with .GlobalEnv
#### datasetSameCheck : if TRUE it will check if the dataset used for creating model is same as the current dataset.
#### NAinVarsCheck : if TRUE if will look for NAs in dependent and independent vars used for creating model.
####
####
StepwisePrerequisite <- function(modelname, curdatasetname, validateDatasetCheck=TRUE, datasetSameCheck=TRUE, NAinVarsCheck=TRUE )
{
	#get model object's class from modelname
	modclass = eval(parse(text=paste('class(',modelname,')', collapse='', sep='')))
	
	##08Mar2017 if modclass has multiple classes
	if(length(modclass)>1)
	{
		modclass = modclass[1] # just using first item for now so that I can test.
	}
	
	#various messages. Will be chosen to combined in some order for SUCCESS and FAILURE
	successMsg1='SUCCESS: The selected model type is supported.'
	successMsg2=''
	
	failureMsg1='ERROR: The predictor variables that the model requires for scoring are not available in the dataset. [ '
	failureMsg2=' variables are not found ].'
	
	modelselmsg =  paste0('\n -Currently selected model:  ', modelname, sep='')
	modelclasmsg =  paste0('\n -Model Class:  ', modclass, sep='')
	datasetmsg =  paste0('\n -Currently selected dataset:  ', curdatasetname, sep='')

	finalmsg='\n\nNOTE: The variable names in the dataset you are trying to score must match the variable names of the dataset you used to build the model.'
	
	####### VALIDATING A CURRENT DATASET  ######
	if(validateDatasetCheck)
	{
		curdatasetname <- BSkyValidateDataset(curdatasetname)## if its a invalid dataset, return value will be null
		if(is.null(curdatasetname))
		{
			invdsmsg =paste0('ERROR: Invalid dataset!', collapse='', sep='')
			return(invdsmsg)
		}
	}
	dependentvar = character(0)		
	msg=character(0)
	

	vardiff <- list()
	j=1
	modelvars <- list()
	
	##what model to support can be controled by following IF-ELSE ladder. 
	## OR. It can also be controled from stepwise dialog.
	
	#Get independent(say modelvars) and dependentvar using the provided model.
	#Getting dependent and independent var from a model is different for each model class type.
	if(modclass =='lm' || modclass =='polr' || modclass =='glm')
	{
		modelvars <- eval( parse( text=paste( 'unlist(strsplit( as.character(',modelname,'$call[[2]])[3], "[+]" ))', collapse='', sep='') ) )
		modelvars <- gsub("^\\s+|\\s+$", "", modelvars) #remove extra space from both sides of each var.
		#modelvars <- eval(parse(text=modelvars))
		
		#also find dependentvar
		dependentvar <-  eval( parse(text=paste('as.character(',modelname,'$call$formula[[2]])',sep='')))
	}
	else if(modclass =='NaiveBayes')
	{
		#09Mar2017 I found that this model doesn't stores datasetname using which model was generated.
		#So stepwise fails to  process this model. We may find a workaround like setting datasetname
		#to current dataset name but I am not sure if that is meaningful/logical or not.
		modelvars <- eval( parse(text=paste(modelname,'$varname',sep='')))
		
		#also find dependentvar
		if(! eval(parse(text=paste('is.null(',modelname,'$dependentvar)', sep=''))))
		{
		dependentvar <- eval( parse(text=paste(modelname,'$dependentvar',sep='')))
		}	
	}
	else if(modclass =='randomForest')
	{
		#09Mar2017 I found that this model doesn't stores datasetname using which model was generated.
		#So stepwise fails to  process this model. We may find a workaround like setting datasetname
		#to current dataset name but I am not sure if that is meaningful/logical or not.
		
		#BSkyRandomForestModel1$call$x
		#Dataset2[, c("mpg", "engine", "horse", "weight", "accel", "year", "cylinder", "filter_.")]
		modelvars <- eval( parse( text=paste( 'as.character(',modelname,'$call$x)[[4]]', collapse='', sep='') ) )
		modelvars <- eval(parse(text=modelvars))
		
		#also find dependentvar
		objlen = eval(parse(text=paste('length(',modelname,'$call$y)', sep='') ) ) #26Jan2017 find length
		#length is 4 when y is passed as Dataset[,c('colname')]
		#length is 3 when y is passed as Dataset$colname. We made this fix in RandomForest dialog on 25Jan2017 for Q's 
		#issue Dataset[,c('colname')] was turning into a tibble and not behaving as column.
		if(objlen==4)
		{
			a2 <- eval(parse(text=paste('as.character((',modelname,'$call$y)[[4]])', sep='') ) ) 
			dependentvar <- a2[2]
		}
		else if(objlen==3)
		{
			dependentvar = eval(parse(text=paste('as.character((',modelname,'$call$y)[[3]])', sep='') ) ) 
		}
		#dependentvariable <- eval(parse(text=paste(a1,'[,c("',depvar,'")]', collapse='', sep='') ) )
		#dependentclass=eval( parse( text=paste('class(',a1,'[,c("',depvar,'")])',sep='') ) )
	}
	else if(modclass =='rpart')
	{
		#09Mar2017 This model also crashed the application when stepwise was run on it.
		#StackOverflow in C#
		modelvars <- eval( parse( text=paste( 'unlist(strsplit( as.character(',modelname,'$call[[2]])[3], "[+]" ))', collapse='', sep='') ) )
		modelvars <- gsub("^\\s+|\\s+$", "", modelvars) #remove extra space from both sides of each var.
		#modelvars <- eval(parse(text=modelvars))
		
		#also find dependentvar
		dependentvar <- eval( parse(text=paste('as.character(',modelname,'$call$formula[[2]])',sep='')))
	}
	else if(modclass =='ANILmultinom')#08Mar2017 We are not supporting this right now. This is crashing the C# app.
	{ 
		## StackOverflow error in C#
		modelvars <- eval( parse( text=paste( 'unlist(strsplit( as.character(',modelname,'$call[[2]])[3], "[+]" ))', collapse='', sep='') ) )
		modelvars <- gsub("^\\s+|\\s+$", "", modelvars) #remove extra space from both sides of each var.
		#modelvars <- eval(parse(text=modelvars))
		
		#also find dependentvar
		dependentvar <-  eval( parse(text=paste('as.character(',modelname,'$call$formula[[2]])',sep='')))
	}	
	else 
	{
		notsupportedmsg =paste0('ERROR: Model not supported.', collapse='', sep='')
		return(notsupportedmsg)
	}

	####### CHECK IF CURRENT DATASET AND THE DATASET USED FOR CREATING THE MODEL ARE SAME OR NOT. THEY MUST BE SAME. ########
	if(datasetSameCheck)
	{
		modsname = eval( parse(text=paste(modelname,'$call$data',collapse='',sep=''))) 
		deparseddsname = deparse(modsname)
		##modeldataset = ExtractDatasetNameFromGlobal(deparseddsname)
		dsmsg=character(0)
		
			#09Mar2017 these two lines are moved out ot the 'if' block below. 
			#Because the 'if' condition was not working properly when the model was MLM. So 'if'
			#condition is also replace with new 'if' condition. Looks like old 'if' may be having 
			#some trouble because of .GlobalEnv$ prefixed to the curdatasetname or deparseddsname
			curds <- ExtractDatasetNameFromGlobal(curdatasetname)
			modds <- ExtractDatasetNameFromGlobal(deparseddsname)#.GlobalEnv$ will be trimmed off		
		
		###if(bsky.trim(deparseddsname) != bsky.trim(curdatasetname))
		if(bsky.trim(modds) != bsky.trim(curds))
		{
			#09Mar2017 following two lines moved out of this block
			#curds <- ExtractDatasetNameFromGlobal(curdatasetname)
			#modds <- ExtractDatasetNameFromGlobal(deparseddsname)
			dsmsg <- paste0('ERROR: Dataset mismatch.\nCurrent dataset name: ',curds,'\nDataset used for creating the model: ',modds, collapse='', sep='')
			return(dsmsg)
		}
	}
	########## CHECK IF INDEPENDENT OR DEPENDENT VARS HAVE NAs. DONT PROCESS IF NAs ARE PRESENT ##########
	if(NAinVarsCheck)
	{
		namsg=paste0('ERROR: Stepwise cannot run if there are NAs. You must first remove NAs from the dataset (go to Data -> Remove NAs). Then recreate the model and finally run stepwise on that model.\nVariables having NAs are:\n ', collapse='',sep='')
		navarlst=character(0)
		hasNA=FALSE
		if( eval(parse(text=paste('anyNA(',curdatasetname,'$',dependentvar,')', sep='')))) #if dependent var has NAs
		{
			navarlst = paste0(dependentvar, sep='') ##has NAs
			hasNA=TRUE
		}
		indvarlst <- modelvars
		indlen = length(indvarlst)
		for( i  in 1 : indlen)
		{
			tmpvar = indvarlst[i]
			
			if( eval(parse(text=paste('anyNA(',curdatasetname,'$',tmpvar,')', collapse='',sep='')))) 
			{
				navarlst = paste0(navarlst, ', ',tmpvar, sep='') ##has NAs
				hasNA=TRUE
			}
			else
			{
				#navarlst = paste0(navarlst, '\n',tmpvar,' has no NAs', sep='')
			}
		}
		
		if(hasNA)##if there are NAs
		{
			namsg = paste0(namsg, navarlst)
			return(namsg)
		}
	}
	
	##### IF ABOVE CHECKS PASS, NOW CREATE MORE MESSAGES TO ADD THEM TO FINAL SUCCESSS MESSAGE  #####
	
	vrlst <- paste(modelvars,collapse=', ', sep=',')
	dependentvarmsg = paste('\n -Dependent variable of the selected model:  ', dependentvar, sep='')
	independentvarmsg = paste('\n -Independent variables of the selected model:  ', vrlst,sep='')
	
	commonMsg = paste(modelselmsg, modelclasmsg, datasetmsg, dependentvarmsg, independentvarmsg,sep='')
	
	#get independent vars( here modelvars)
	modvarcount <- length(modelvars)
	print(modelvars)
	
	#get datsetvars
	datasetvars <- eval( parse(text=paste('names(',curdatasetname,')',sep='')))
	print(datasetvars)
	dsvarcount <- length(datasetvars)
	
	#if there are independent vars then go inside of 'if'. Otherwise, jump to 'else' and return the msg created inside of the 'else'
	if(modvarcount > 0)
	{
		#now we should have all the independent vars(say modelvars) and match those with current dataset vars
		for(i in 1:modvarcount)
		{
			if(!(modelvars[i] %in% datasetvars))
			{
				#vardiff[j] <- modelvars[i]
				vardiff <- append(vardiff, modelvars[i])
			}
		}

		#based on all matched or not we create and approriate message that will be returned from this function
		if(length(vardiff)>0)
		{
			vrlst <- paste(vardiff,collapse=', ', sep=',')
			msg = paste0(failureMsg1,vrlst,failureMsg2, commonMsg,finalmsg, collapse='', sep='')
		}
		else
		{
			#vrlst <- paste(modelvars,collapse=', ', sep=',')
			msg =paste0(successMsg1, commonMsg, finalmsg, collapse='', sep='')
		}
	}
	else
	{
		msg =paste0('ERROR:Could not find independent variables or selected the right model and dataset.', collapse='', sep='')
	}
	print(msg)
	return(msg)
}

## Cloned the function above (StepwisePrerequisite ) to use it so that it does not print too much
##Its a clone of function StepwisePrerequisite 
ModelMatchesDataset <- function(modelname, curdatasetname, validateDatasetCheck=TRUE, datasetSameCheck=TRUE, NAinVarsCheck=TRUE )
{
	#get model object's class from modelname
	modclass = eval(parse(text=paste('class(',modelname,')', collapse='', sep='')))
	
	##08Mar2017 if modclass has multiple classes
	if(length(modclass)>1)
	{
		modclass = modclass[1] # just using first item for now so that I can test.
	}
	
	#various messages. Will be chosen to combined in some order for SUCCESS and FAILURE
	successMsg1='SUCCESS: The variables used to create the model are present in the active dataset.'
	successMsg2=''
	
	failureMsg1='ERROR: The independent variables used to build the model are not available in the ACTIVE dataset. [ '
	failureMsg2=' variables are not found ].'
	
	modelselmsg =  paste0('\n -Currently selected model:  ', modelname, sep='')
	modelclasmsg =  paste0('\n -Model Class:  ', modclass, sep='')
	datasetmsg =  paste0('\n -Currently selected dataset:  ', curdatasetname, sep='')
	informationalMessage ="\nInformational Message: If you see errors below, validate that the model was built with na.action =na.exclude or simply remove NAs and rebuild the model and re-run the analysis. See Data>Missing Values>Remove NAs to remove missing values\n\n"

	finalmsg='\n\nNOTE: The variables used to build the model must be in the active dataset. \nMake sure that the correct dataset is the active dataset.\nYou can do this by simply selecting the dataset in the main application window.'
	
	####### VALIDATING A CURRENT DATASET  ######
	if(validateDatasetCheck)
	{
		curdatasetname <- BSkyValidateDataset(curdatasetname)## if its a invalid dataset, return value will be null
		if(is.null(curdatasetname))
		{
			invdsmsg =paste0('ERROR: Invalid dataset!', collapse='', sep='')
			return(invdsmsg)
		}
	}
	dependentvar = character(0)		
	msg=character(0)
	

	vardiff <- list()
	j=1
	modelvars <- list()
	
	##what model to support can be controled by following IF-ELSE ladder. 
	## OR. It can also be controled from stepwise dialog.
	
	#Get independent(say modelvars) and dependentvar using the provided model.
	#Getting dependent and independent var from a model is different for each model class type.
	if(modclass =='lm' || modclass =='polr' || modclass =='glm')
	{
		modelvars <- eval( parse( text=paste( 'unlist(strsplit( as.character(',modelname,'$call[[2]])[3], "[+]" ))', collapse='', sep='') ) )
		modelvars <- gsub("^\\s+|\\s+$", "", modelvars) #remove extra space from both sides of each var.
		#modelvars <- eval(parse(text=modelvars))
		
		#also find dependentvar
		dependentvar <-  eval( parse(text=paste('as.character(',modelname,'$call$formula[[2]])',sep='')))
	}
	else if (modclass =="train")
	{
	 dependentvar <- getModelDependentVariable(modelname)
    modelvars <- list()
    modelvars <- getModelIndependentVariables(modelname)
	}
	
	else if(modclass =='NaiveBayes')
	{
		#09Mar2017 I found that this model doesn't stores datasetname using which model was generated.
		#So stepwise fails to  process this model. We may find a workaround like setting datasetname
		#to current dataset name but I am not sure if that is meaningful/logical or not.
		modelvars <- eval( parse(text=paste(modelname,'$varname',sep='')))
		
		#also find dependentvar
		if(! eval(parse(text=paste('is.null(',modelname,'$dependentvar)', sep=''))))
		{
		dependentvar <- eval( parse(text=paste(modelname,'$dependentvar',sep='')))
		}	
	}
	else if(modclass =='randomForest')
	{
		#09Mar2017 I found that this model doesn't stores datasetname using which model was generated.
		#So stepwise fails to  process this model. We may find a workaround like setting datasetname
		#to current dataset name but I am not sure if that is meaningful/logical or not.
		
		#BSkyRandomForestModel1$call$x
		#Dataset2[, c("mpg", "engine", "horse", "weight", "accel", "year", "cylinder", "filter_.")]
		modelvars <- eval( parse( text=paste( 'as.character(',modelname,'$call$x)[[4]]', collapse='', sep='') ) )
		modelvars <- eval(parse(text=modelvars))
		
		#also find dependentvar
		objlen = eval(parse(text=paste('length(',modelname,'$call$y)', sep='') ) ) #26Jan2017 find length
		#length is 4 when y is passed as Dataset[,c('colname')]
		#length is 3 when y is passed as Dataset$colname. We made this fix in RandomForest dialog on 25Jan2017 for Q's 
		#issue Dataset[,c('colname')] was turning into a tibble and not behaving as column.
		if(objlen==4)
		{
			a2 <- eval(parse(text=paste('as.character((',modelname,'$call$y)[[4]])', sep='') ) ) 
			dependentvar <- a2[2]
		}
		else if(objlen==3)
		{
			dependentvar = eval(parse(text=paste('as.character((',modelname,'$call$y)[[3]])', sep='') ) ) 
		}
		#dependentvariable <- eval(parse(text=paste(a1,'[,c("',depvar,'")]', collapse='', sep='') ) )
		#dependentclass=eval( parse( text=paste('class(',a1,'[,c("',depvar,'")])',sep='') ) )
	}
	else if(modclass =='rpart')
	{
		#09Mar2017 This model also crashed the application when stepwise was run on it.
		#StackOverflow in C#
		modelvars <- eval( parse( text=paste( 'unlist(strsplit( as.character(',modelname,'$call[[2]])[3], "[+]" ))', collapse='', sep='') ) )
		modelvars <- gsub("^\\s+|\\s+$", "", modelvars) #remove extra space from both sides of each var.
		#modelvars <- eval(parse(text=modelvars))
		
		#also find dependentvar
		dependentvar <- eval( parse(text=paste('as.character(',modelname,'$call$formula[[2]])',sep='')))
	}
	else if(modclass =='ANILmultinom')#08Mar2017 We are not supporting this right now. This is crashing the C# app.
	{ 
		## StackOverflow error in C#
		modelvars <- eval( parse( text=paste( 'unlist(strsplit( as.character(',modelname,'$call[[2]])[3], "[+]" ))', collapse='', sep='') ) )
		modelvars <- gsub("^\\s+|\\s+$", "", modelvars) #remove extra space from both sides of each var.
		#modelvars <- eval(parse(text=modelvars))
		
		#also find dependentvar
		dependentvar <-  eval( parse(text=paste('as.character(',modelname,'$call$formula[[2]])',sep='')))
	}	
	else 
	{
		#09/07/2020
		#I cannot verify whether the valiables used to build the model are present in the active dataset for all models, the code below
		#ensures that processing continues
		notsupportedmsg =paste0('Informational Message: We have not been able to verify whether the independent variables used to create the model you selected are \navailable in the active/selected dataset in the main application window. \nWe will proceed to run the statistics requested. If you see errors, please validate that the active/selected dataset matches the dataset used to build the model.\nAlso validate that the model was built with the na.action =na.exclude or remove NAs and rebuild the model and re-run the analysis. \nSee Data>Missing Values>Remove NAs to remove missing values\n', collapse='', sep='')
		success=TRUE
		cat(notsupportedmsg)
		return(list(msg =notsupportedmsg, success=success))
	}

	####### CHECK IF CURRENT DATASET AND THE DATASET USED FOR CREATING THE MODEL ARE SAME OR NOT. THEY MUST BE SAME. ########
	if(datasetSameCheck && modclass != "train")
	{
		modsname = eval( parse(text=paste(modelname,'$call$data',collapse='',sep=''))) 
		deparseddsname = deparse(modsname)
		##modeldataset = ExtractDatasetNameFromGlobal(deparseddsname)
		dsmsg=character(0)
		
			#09Mar2017 these two lines are moved out ot the 'if' block below. 
			#Because the 'if' condition was not working properly when the model was MLM. So 'if'
			#condition is also replace with new 'if' condition. Looks like old 'if' may be having 
			#some trouble because of .GlobalEnv$ prefixed to the curdatasetname or deparseddsname
			curds <- ExtractDatasetNameFromGlobal(curdatasetname)
			modds <- ExtractDatasetNameFromGlobal(deparseddsname)#.GlobalEnv$ will be trimmed off		
		
		###if(bsky.trim(deparseddsname) != bsky.trim(curdatasetname))
		if(bsky.trim(modds) != bsky.trim(curds))
		{
			#09Mar2017 following two lines moved out of this block
			#curds <- ExtractDatasetNameFromGlobal(curdatasetname)
			#modds <- ExtractDatasetNameFromGlobal(deparseddsname)
			dsmsg <- paste0('ERROR: The dataset selected/active dataset in the main application window must be the dataset used to build the model. \n Please select the dataset you used to build the model in the main application window. \nName of the current dataset/selected dataset in the main application window: ',curds,'\nDataset used for creating the model: ',modds, collapse='', sep='')
			cat (dsmsg)
			success=FALSE
			return(list( msg=dsmsg, success=success ))
		}
	}
	########## CHECK IF INDEPENDENT OR DEPENDENT VARS HAVE NAs. DONT PROCESS IF NAs ARE PRESENT ##########
	if(NAinVarsCheck)
	{
		namsg=paste0('ERROR: Stepwise cannot run if there are NAs in the dataset used to build the model. \nYou must first remove NAs from the dataset (go to Data > Missing Values > Remove NAs). \nRecreate the model on the dataset with NAs removed and then run stepwise.\nVariables having NAs are:\n ', collapse='',sep='')
		navarlst=character(0)
		hasNA=FALSE
		if( eval(parse(text=paste('anyNA(',curdatasetname,'$',dependentvar,')', sep='')))) #if dependent var has NAs
		{
			navarlst = paste0(dependentvar, sep='') ##has NAs
			hasNA=TRUE
		}
		indvarlst <- modelvars
		indlen = length(indvarlst)
		for( i  in 1 : indlen)
		{
			tmpvar = indvarlst[i]
			
			if( eval(parse(text=paste('anyNA(',curdatasetname,'$',tmpvar,')', collapse='',sep='')))) 
			{
				navarlst = paste0(navarlst, ', ',tmpvar, sep='') ##has NAs
				hasNA=TRUE
			}
			else
			{
				#navarlst = paste0(navarlst, '\n',tmpvar,' has no NAs', sep='')
			}
		}
		
		if(hasNA)##if there are NAs
		{
			namsg = paste0(namsg, navarlst)
			cat(namsg)
			success =FALSE
			return(list(msg=namsg, success=success))
			#return(namsg)
		}
	}
	
	##### IF ABOVE CHECKS PASS, NOW CREATE MORE MESSAGES TO ADD THEM TO FINAL SUCCESSS MESSAGE  #####
	
	vrlst <- paste(modelvars,collapse=', ', sep=',')
	dependentvarmsg = paste('\n -Dependent variable of the selected model:  ', dependentvar, sep='')
	independentvarmsg = paste('\n -Independent variables of the selected model:  ', vrlst,sep='')
	
	commonMsg = paste(modelselmsg, modelclasmsg, datasetmsg, dependentvarmsg, independentvarmsg, "\n",sep='')
	
	#get independent vars( here modelvars)
	modvarcount <- length(modelvars)
	#print(modelvars)
	
	#get datsetvars
	datasetvars <- eval( parse(text=paste('names(',curdatasetname,')',sep='')))
	#print(datasetvars)
	dsvarcount <- length(datasetvars)
	
	#if there are independent vars then go inside of 'if'. Otherwise, jump to 'else' and return the msg created inside of the 'else'
	if(modvarcount > 0)
	{
		#now we should have all the independent vars(say modelvars) and match those with current dataset vars
		for(i in 1:modvarcount)
		{
			if(!(modelvars[i] %in% datasetvars))
			{
				#vardiff[j] <- modelvars[i]
				vardiff <- append(vardiff, modelvars[i])
			}
		}

		#based on all matched or not we create and approriate message that will be returned from this function
		if(length(vardiff)>0)
		{
			vrlst <- paste(vardiff,collapse=', ', sep=',')
			msg = paste0(failureMsg1,vrlst,failureMsg2, commonMsg,finalmsg, collapse='', sep='')
			success=FALSE
		}
		else
		{
			#vrlst <- paste(modelvars,collapse=', ', sep=',')
			msg =paste0(successMsg1, commonMsg, informationalMessage, collapse='', sep='')
			success =TRUE
		}
	}
	else
	{
		msg =paste0('ERROR:Could not find independent variables or select the right model and dataset.', collapse='', sep='')
		success=FALSE
	}
	#print(msg)
	cat(msg)
	return(list(msg =msg, success=success))
}













StepwisePrerequisite.old <- function (modelname, curdatasetname)
{

## in future we can use modelname object and change the hardcoded dataset name with curdatasetname so that model can be executed on
## current dataset. Otherwise it only work witl the hardcoded dataset(eg.. Dataset2). Dataset2 must be the that has all independent and 
## dependent vars. Rather it should look at the current dataset but that is not the case here.

 #instead of checking all dependent/independent vars, checking whole dataset for simplicity
if( eval(parse(text=paste('anyNA(',curdatasetname,')',sep=''))) )
{
	msg = paste0('ERROR: Stepwise cannot run if there are NAs. You must first remove NAs from the dataset (go to Data -> Remove NAs). Then recreate the model and finally run stepwise on that model', collapse='',sep='')
	print(msg)
	return(msg)	
}
else
{
	msg = paste0('SUCCESS!', collapse='',sep='')
	print(msg)
	return(msg)	
}
}

BSkyStepAIC <- function (mod, direction = c("backward/forward", "forward/backward", 
    "backward", "forward"), criterion = c("BIC", "AIC"), ...) 
{
    criterion <- match.arg(criterion)
    direction <- match.arg(direction)
    #cat("\nDirection: ", direction)
    #cat("\nCriterion: ", criterion, "\n\n")
    k <- if (criterion == "BIC") 
        log(nrow(model.matrix(mod)))
    else 2
    rhs <- paste(c("~", deparse(formula(mod)[[3]])), collapse = "")
    rhs <- gsub(" ", "", rhs)
    if (direction == "forward" || direction == "forward/backward") 
        mod <- update(mod, . ~ 1)
    if (direction == "backward/forward" || direction == "forward/backward") 
        direction <- "both"
    lower <- ~1
    upper <- eval(parse(text = rhs))
    BSkyStepAIC.mass(mod, scope = list(lower = lower, upper = upper), 
        direction = direction, k = k, ...)
}





BSkyStepAIC.mass <- function (object, scope, scale = 0, direction = c("both", "backward", 
    "forward"), trace = 1, keep = NULL, steps = 1000, use.start = FALSE, 
    k = 2, ...) 
{
	BSkyFunctionInit()
	#print("Hello")

    mydeviance <- function(x, ...) 
	{
        dev <- deviance(x)
        if (!is.null(dev)) 
		{
            dev
		}
        else 
		{
			extractAIC(x, k = 0)[2L]
		}
    }
    cut.string <- function(string) 
	{
        if (length(string) > 1L) 
		{
            string[-1L] <- paste("\n", string[-1L], sep = "")
		}
        string
    }
    re.arrange <- function(keep) 
	{
        namr <- names(k1 <- keep[[1L]])
        namc <- names(keep)
        nc <- length(keep)
        nr <- length(k1)
        array(unlist(keep, recursive = FALSE), c(nr, nc), list(namr, 
            namc))
    }
    step.results <- function(models, fit, object, usingCp = FALSE) 
	{
        change <- sapply(models, "[[", "change")
        rd <- sapply(models, "[[", "deviance")
        dd <- c(NA, abs(diff(rd)))
        rdf <- sapply(models, "[[", "df.resid")
        ddf <- c(NA, abs(diff(rdf)))
        AIC <- sapply(models, "[[", "AIC")
        heading <- c("Stepwise Model Path \nAnalysis of Deviance Table", 
            "\nInitial Model:", deparse(formula(object)), "\nFinal Model:", 
            deparse(formula(fit)), "\n")
	
        aod <- if (usingCp)
		{
            data.frame(Step = change, Df = ddf, Deviance = dd, 
                `Resid. Df` = rdf, `Resid. Dev` = rd, Cp = AIC, 
                check.names = FALSE)
		}
        else 
		{
			data.frame(Step = change, Df = ddf, Deviance = dd, 
            `Resid. Df` = rdf, `Resid. Dev` = rd, AIC = AIC, 
            check.names = FALSE)
		}
        attr(aod, "heading") <- heading
        class(aod) <- c("Anova", "data.frame")
        fit$anova <- aod
		
		#Original
        #fit
		
		#Method 1
		#BSkyTemp[[bskycounter]] <- fit
		#BSkyTemp
		
		#Method2
		tmpobj1 <- BSkyConvertObj(fit)
		BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = tmpobj1)
		#BSkyReturnStructure2(bskyAdditionalTableList=as.list(fit))
		
    }
	BSkyTemp <- list()
	msg=character(0)
	bskycounter=1
    Terms <- terms(object)
    object$formula <- Terms
    if (inherits(object, "lme")) 
	{
        object$call$fixed <- Terms
	}
    else if (inherits(object, "gls")) 
	{
        object$call$model <- Terms
	}
    else 
	{
		object$call$formula <- Terms
	}
    if (use.start) 
	{
        warning("'use.start' cannot be used with R's version of 'glm'")
	}
    md <- missing(direction)
    direction <- match.arg(direction)
    backward <- direction == "both" | direction == "backward"
    forward <- direction == "both" | direction == "forward"
    if (missing(scope)) 
	{
        fdrop <- numeric()
        fadd <- attr(Terms, "factors")
        if (md) 
		{
            forward <- FALSE
		}
    }
    else 
	{
        if (is.list(scope)) 
		{
            fdrop <- if (!is.null(fdrop <- scope$lower)) 
			{
                attr(terms(update.formula(object, fdrop)), "factors")
			}
            else 
			{
				numeric()
			}
            fadd <- if (!is.null(fadd <- scope$upper))
			{
                attr(terms(update.formula(object, fadd)), "factors")
			}
        }
        else 
		{
            fadd <- if (!is.null(fadd <- scope)) 
			{
                attr(terms(update.formula(object, scope)), "factors")
			}
            fdrop <- numeric()
        }
    }
	
    models <- vector("list", steps)
    if (!is.null(keep))
	{
        keep.list <- vector("list", steps)
	}
    n <- nobs(object, use.fallback = TRUE)
    fit <- object
    bAIC <- extractAIC(fit, scale, k = k, ...)
    edf <- bAIC[1L]
    bAIC <- bAIC[2L]
    if (is.na(bAIC)) 
	{
        stop("AIC is not defined for this model, so 'stepAIC' cannot proceed")
	}
    if (bAIC == -Inf) 
	{
        stop("AIC is -infinity for this model, so 'stepAIC' cannot proceed")
	}
    nm <- 1
    Terms <- terms(fit)
    if (trace) 
	{
		#Original
        #cat("Start:  AIC=", format(round(bAIC, 2)), "\n", cut.string(deparse(formula(fit))), "n\n", sep = "")
		#msg <- paste0("Anova Tables\n\nStart:  AIC=", format(round(bAIC, 2)), "\n", cut.string(deparse(formula(fit))), collapse='', sep = '')
		fitstrcut1 <- paste0(cut.string(deparse(formula(fit))), collapse='')
		msg <- paste0("Anova Tables\n\nStart:  AIC=", format(round(bAIC, 2)), "\n", fitstrcut1, sep = '')
		
		# Method 1
		#BSkyTemp[[bskycounter]] <- msg
		#bskycounter = bskycounter + 1
		
        utils::flush.console()
    }
    models[[nm]] <- list(deviance = mydeviance(fit), df.resid = n - 
        edf, change = "", AIC = bAIC)
    if (!is.null(keep)) 
	{
        keep.list[[nm]] <- keep(fit, bAIC)
	}
    usingCp <- FALSE
    while (steps > 0) 
	{
        steps <- steps - 1
        AIC <- bAIC
        ffac <- attr(Terms, "factors")
        if (!is.null(sp <- attr(Terms, "specials")) && !is.null(st <- sp$strata))
		{
            ffac <- ffac[-st, ]
		}
        scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
        aod <- NULL
        change <- NULL
        if (backward && length(scope$drop)) 
		{
            aod <- dropterm(fit, scope$drop, scale = scale, trace = max(0, 
                trace - 1), k = k, ...)
            rn <- row.names(aod)
            row.names(aod) <- c(rn[1L], paste("-", rn[-1L], sep = " "))
            if (any(aod$Df == 0, na.rm = TRUE)) 
			{
                zdf <- aod$Df == 0 & !is.na(aod$Df)
                nc <- match(c("Cp", "AIC"), names(aod))
                nc <- nc[!is.na(nc)][1L]
                ch <- abs(aod[zdf, nc] - aod[1, nc]) > 0.01
                if (any(is.finite(ch) & ch)) 
				{
                  warning("0 df terms are changing AIC")
                  zdf <- zdf[!ch]
                }
                if (length(zdf) > 0L)
				{
                  change <- rev(rownames(aod)[zdf])[1L]
			    }
            }
        }
        if (is.null(change)) 
		{
            if (forward && length(scope$add)) 
			{
                aodf <- addterm(fit, scope$add, scale = scale, 
                  trace = max(0, trace - 1), k = k, ...)
                rn <- row.names(aodf)
                row.names(aodf) <- c(rn[1L], paste("+", rn[-1L], 
                  sep = " "))
                aod <- if (is.null(aod)) 
				{
                  aodf
				}
                else 
				{
				  rbind(aod, aodf[-1, , drop = FALSE])
				}
            }
            attr(aod, "heading") <- NULL
            if (is.null(aod) || ncol(aod) == 0) 
			{
                break
			}
            nzdf <- if (!is.null(aod$Df)) 
            {
				aod$Df != 0 | is.na(aod$Df)
			}
            aod <- aod[nzdf, ]
            if (is.null(aod) || ncol(aod) == 0) 
            {
				break
			}
            nc <- match(c("Cp", "AIC"), names(aod))
            nc <- nc[!is.na(nc)][1L]
            o <- order(aod[, nc])
            if (trace) 
			{
				#Original code
                #print(aod[o, ])
				
				#Method 1
				#BSkyTemp[[bskycounter]] <- aod[o, ]
				#bskycounter = bskycounter + 1
				
				#Method 2
				#cat('\nNo of tables in one object :',length(aod[o, ]),'\nClass:\n',class(aod[o, ]))
				#cat('\nNames:', names(aod[o, ]),'\nMsg:',msg ,'\n...')
				tempdf <- ( as.data.frame(aod[o, ]) )
				#tempdf <- (aod[o, ]) #Doesn't work properly
				#cat('\nObj Len: ', length(tempdf),'\nMSG Len: ',length(msg) ,'\n')
				
				#cat('\nNo of tables in one object after as.data.frame :',length(tempdf),' Class ',class(tempdf))
				#cat('\n', names(tempdf),'\n',msg ,'\n')
				#names(tempdf) = "This is  header"
				

				if(length(msg)>1)#There are multiple headers.First header is passed
				{
					BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = tempdf, singleTableOutputHeader=msg[1])
				}
				else if(length(msg)==1)#if only one header is present
				{
					BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = tempdf, singleTableOutputHeader=msg)
				}
				else if(length(msg)!=length(tempdf))#no headers will be printed but tables will be shown, also avoiding crash.
				{
					BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = tempdf)
				}
				
				#BSkyFormat(msg) not good for msg printing. msg is table header
				#BSkyBuildReturnTableStructure2(msg) not good for msg printing. msg is table header
				#BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = tempdf)
				
				#BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = aod[o, ]) #This does not work.see error in next line.
				#Object type passed (not a numeric, charater, or logical array or single Matrix or Data Frame, or list of Matrix or Data Frames)
				
				msg=character(0)
				
                utils::flush.console()
            }
            if (o[1L] == 1) 
            {
				break
			}
            change <- rownames(aod)[o[1L]]
        }
        usingCp <- match("Cp", names(aod), 0) > 0
        fit <- update(fit, paste("~ .", change), evaluate = FALSE)
        fit <- eval.parent(fit)
        nnew <- nobs(fit, use.fallback = TRUE)
        if (all(is.finite(c(n, nnew))) && nnew != n) 
        {
			stop("number of rows in use has changed: remove missing values?")
		}
        Terms <- terms(fit)
        bAIC <- extractAIC(fit, scale, k = k, ...)
        edf <- bAIC[1L]
        bAIC <- bAIC[2L]
        if (trace) 
		{
			#Original
            #cat("\nStep:  AIC=", format(round(bAIC, 2)), "\n", cut.string(deparse(formula(fit))), "\n\n", sep = "")
			fitstrcut1 <- paste0(cut.string(deparse(formula(fit))), collapse='')
		    msg <- paste0("Step:  AIC=", format(round(bAIC, 2)), "\n", fitstrcut1, sep = '')
		   
		    #Method 1
		    BSkyTemp[[bskycounter]] <- msg
		    bskycounter = bskycounter + 1
		
            utils::flush.console()
        }
        if (bAIC >= AIC + 1e-07) 
        {
			break
		}
        nm <- nm + 1
        models[[nm]] <- list(deviance = mydeviance(fit), df.resid = n - 
            edf, change = change, AIC = bAIC)
        if (!is.null(keep)) 
		{
            keep.list[[nm]] <- keep(fit, bAIC)
		}
    }
    if (!is.null(keep)) 
	{
        fit$keep <- re.arrange(keep.list[seq(nm)])
	}
    step.results(models = models[seq(nm)], fit, object, usingCp)
	
	
	BSkyFunctionWrapUp()
	# obj is the Bsky return structure with all tables stored within it
    obj = BSkyReturnStructure2()
	#obj$lmtbl <- fit #Adding lm to return structure so that it can be separately printed using BSkyFormat
    # number of decimal digit formatting from the config option for the numeric values within every cell of every table
	# obj = BSkyDecimalDigitFormating(obj, decimalDigits = 2)
	return(invisible(obj))
}


BSkyStepAIC.test <- function (object, scope, scale = 0, direction = c("both", "backward", 
    "forward"), trace = 1, keep = NULL, steps = 1000, use.start = FALSE, 
    k = 2, ...) 
{
	BSkyFunctionInit()
	print("Hello")
	BSkyFunctionWrapUp()
}

## following not in use. It is a possibility that original stepAIC function is calling some function that calls back stepAIC
## because we changed the name, maybe that is why we are getting stack overflow exception. Not sure though.
##stepwiseAIC original code only the function names are changed and still when we try to run stepwise on multinom model
##it crashes with StackOverflowException in C#.
Original.BSkyStepAIC <- function (mod, direction = c("backward/forward", "forward/backward", 
    "backward", "forward"), criterion = c("BIC", "AIC"), ...) 
{
    criterion <- match.arg(criterion)
    direction <- match.arg(direction)
    cat("\nDirection: ", direction)
    cat("\nCriterion: ", criterion, "\n\n")
    k <- if (criterion == "BIC") 
        log(nrow(model.matrix(mod)))
    else 2
    rhs <- paste(c("~", deparse(formula(mod)[[3]])), collapse = "")
    rhs <- gsub(" ", "", rhs)
    if (direction == "forward" || direction == "forward/backward") 
        mod <- update(mod, . ~ 1)
    if (direction == "backward/forward" || direction == "forward/backward") 
        direction <- "both"
    lower <- ~1
    upper <- eval(parse(text = rhs))
    stepAIC1(mod, scope = list(lower = lower, upper = upper), 
        direction = direction, k = k, ...)
}

Original.stepAIC1 <- function (object, scope, scale = 0, direction = c("both", "backward", 
    "forward"), trace = 1, keep = NULL, steps = 1000, use.start = FALSE, 
    k = 2, ...) 
{
    mydeviance <- function(x, ...) {
        dev <- deviance(x)
        if (!is.null(dev)) 
            dev
        else extractAIC(x, k = 0)[2L]
    }
    cut.string <- function(string) {
        if (length(string) > 1L) 
            string[-1L] <- paste("\n", string[-1L], sep = "")
        string
    }
    re.arrange <- function(keep) {
        namr <- names(k1 <- keep[[1L]])
        namc <- names(keep)
        nc <- length(keep)
        nr <- length(k1)
        array(unlist(keep, recursive = FALSE), c(nr, nc), list(namr, 
            namc))
    }
    step.results <- function(models, fit, object, usingCp = FALSE) {
        change <- sapply(models, "[[", "change")
        rd <- sapply(models, "[[", "deviance")
        dd <- c(NA, abs(diff(rd)))
        rdf <- sapply(models, "[[", "df.resid")
        ddf <- c(NA, abs(diff(rdf)))
        AIC <- sapply(models, "[[", "AIC")
        heading <- c("Stepwise Model Path \nAnalysis of Deviance Table", 
            "\nInitial Model:", deparse(formula(object)), "\nFinal Model:", 
            deparse(formula(fit)), "\n")
        aod <- if (usingCp) 
            data.frame(Step = change, Df = ddf, Deviance = dd, 
                `Resid. Df` = rdf, `Resid. Dev` = rd, Cp = AIC, 
                check.names = FALSE)
        else data.frame(Step = change, Df = ddf, Deviance = dd, 
            `Resid. Df` = rdf, `Resid. Dev` = rd, AIC = AIC, 
            check.names = FALSE)
        attr(aod, "heading") <- heading
        class(aod) <- c("Anova", "data.frame")
        fit$anova <- aod
        fit
    }
    Terms <- terms(object)
    object$formula <- Terms
    if (inherits(object, "lme")) 
        object$call$fixed <- Terms
    else if (inherits(object, "gls")) 
        object$call$model <- Terms
    else object$call$formula <- Terms
    if (use.start) 
        warning("'use.start' cannot be used with R's version of 'glm'")
    md <- missing(direction)
    direction <- match.arg(direction)
    backward <- direction == "both" | direction == "backward"
    forward <- direction == "both" | direction == "forward"
    if (missing(scope)) {
        fdrop <- numeric()
        fadd <- attr(Terms, "factors")
        if (md) 
            forward <- FALSE
    }
    else {
        if (is.list(scope)) {
            fdrop <- if (!is.null(fdrop <- scope$lower)) 
                attr(terms(update.formula(object, fdrop)), "factors")
            else numeric()
            fadd <- if (!is.null(fadd <- scope$upper)) 
                attr(terms(update.formula(object, fadd)), "factors")
        }
        else {
            fadd <- if (!is.null(fadd <- scope)) 
                attr(terms(update.formula(object, scope)), "factors")
            fdrop <- numeric()
        }
    }
    models <- vector("list", steps)
    if (!is.null(keep)) 
        keep.list <- vector("list", steps)
    n <- nobs(object, use.fallback = TRUE)
    fit <- object
    bAIC <- extractAIC(fit, scale, k = k, ...)
    edf <- bAIC[1L]
    bAIC <- bAIC[2L]
    if (is.na(bAIC)) 
        stop("AIC is not defined for this model, so 'stepAIC' cannot proceed")
    if (bAIC == -Inf) 
        stop("AIC is -infinity for this model, so 'stepAIC' cannot proceed")
    nm <- 1
    Terms <- terms(fit)
    if (trace) {
        cat("Start:  AIC=", format(round(bAIC, 2)), "\n", cut.string(deparse(formula(fit))), 
            "\n\n", sep = "")
        utils::flush.console()
    }
    models[[nm]] <- list(deviance = mydeviance(fit), df.resid = n - 
        edf, change = "", AIC = bAIC)
    if (!is.null(keep)) 
        keep.list[[nm]] <- keep(fit, bAIC)
    usingCp <- FALSE
    while (steps > 0) {
        steps <- steps - 1
        AIC <- bAIC
        ffac <- attr(Terms, "factors")
        if (!is.null(sp <- attr(Terms, "specials")) && !is.null(st <- sp$strata)) 
            ffac <- ffac[-st, ]
        scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
        aod <- NULL
        change <- NULL
        if (backward && length(scope$drop)) {
            aod <- dropterm(fit, scope$drop, scale = scale, trace = max(0, 
                trace - 1), k = k, ...)
            rn <- row.names(aod)
            row.names(aod) <- c(rn[1L], paste("-", rn[-1L], sep = " "))
            if (any(aod$Df == 0, na.rm = TRUE)) {
                zdf <- aod$Df == 0 & !is.na(aod$Df)
                nc <- match(c("Cp", "AIC"), names(aod))
                nc <- nc[!is.na(nc)][1L]
                ch <- abs(aod[zdf, nc] - aod[1, nc]) > 0.01
                if (any(is.finite(ch) & ch)) {
                  warning("0 df terms are changing AIC")
                  zdf <- zdf[!ch]
                }
                if (length(zdf) > 0L) 
                  change <- rev(rownames(aod)[zdf])[1L]
            }
        }
        if (is.null(change)) {
            if (forward && length(scope$add)) {
                aodf <- addterm(fit, scope$add, scale = scale, 
                  trace = max(0, trace - 1), k = k, ...)
                rn <- row.names(aodf)
                row.names(aodf) <- c(rn[1L], paste("+", rn[-1L], 
                  sep = " "))
                aod <- if (is.null(aod)) 
                  aodf
                else rbind(aod, aodf[-1, , drop = FALSE])
            }
            attr(aod, "heading") <- NULL
            if (is.null(aod) || ncol(aod) == 0) 
                break
            nzdf <- if (!is.null(aod$Df)) 
                aod$Df != 0 | is.na(aod$Df)
            aod <- aod[nzdf, ]
            if (is.null(aod) || ncol(aod) == 0) 
                break
            nc <- match(c("Cp", "AIC"), names(aod))
            nc <- nc[!is.na(nc)][1L]
            o <- order(aod[, nc])
            if (trace) {
                print(aod[o, ])
                utils::flush.console()
            }
            if (o[1L] == 1) 
                break
            change <- rownames(aod)[o[1L]]
        }
        usingCp <- match("Cp", names(aod), 0) > 0
        fit <- update(fit, paste("~ .", change), evaluate = FALSE)
        fit <- eval.parent(fit)
        nnew <- nobs(fit, use.fallback = TRUE)
        if (all(is.finite(c(n, nnew))) && nnew != n) 
            stop("number of rows in use has changed: remove missing values?")
        Terms <- terms(fit)
        bAIC <- extractAIC(fit, scale, k = k, ...)
        edf <- bAIC[1L]
        bAIC <- bAIC[2L]
        if (trace) {
            cat("\nStep:  AIC=", format(round(bAIC, 2)), "\n", 
                cut.string(deparse(formula(fit))), "\n\n", sep = "")
            utils::flush.console()
        }
        if (bAIC >= AIC + 1e-07) 
            break
        nm <- nm + 1
        models[[nm]] <- list(deviance = mydeviance(fit), df.resid = n - 
            edf, change = change, AIC = bAIC)
        if (!is.null(keep)) 
            keep.list[[nm]] <- keep(fit, bAIC)
    }
    if (!is.null(keep)) 
        fit$keep <- re.arrange(keep.list[seq(nm)])
    step.results(models = models[seq(nm)], fit, object, usingCp)
}

