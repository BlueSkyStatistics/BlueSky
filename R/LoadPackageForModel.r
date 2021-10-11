## http://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/
## This function is called from Model Tuning dialogs to load (and not install) the
## required R package based on what model was selected in the dialog's combobox
## Complete List of models : http://topepo.github.io/caret/available-models.html


## only pass model initials e.g. 'rf', not full name randomForest
loadReqModelPackage.old <- function(modelselected=NULL)
{
	success = FALSE
	if(!is.null(modelselected))
	{
	
		if(modelselected == "adaboost")
		{
			#fastAdaboost;
			m1=LoadPackage("fastAdaboost")
			if( !grepl("ERROR", m1) )
			{
				success = TRUE
			}
		}
		else if(modelselected == "logicBag")
		{
			#logicFS;
			m1=LoadPackage("logicFS")
			if( !grepl("ERROR", m1) )
			{
				success = TRUE
			}
		}
		else if(modelselected == "bridge")
		{
			#monomvn;
			m1=LoadPackage("monomvn")
			if( !grepl("ERROR", m1) )
			{
				success = TRUE
			}
		}		
		else if(modelselected == "rpart")
		{
			#rpart;
			m1=LoadPackage("rpart")
			#m2=LoadPackage("rpart.plot")
			#m3=LoadPackage("rattle")
			if( !grepl("ERROR", m1) )#&&  !grepl("ERROR", m2) &&  !grepl("ERROR", m3))
			{
				success = TRUE
			}
		}
		 else if(modelselected == "nb")
		{
			#klaR
			#m1 = LoadPackage("caret")
			m2 = LoadPackage("klaR")
			if( !grepl("ERROR", m2))# !grepl("ERROR", m1) && 
			{
				success = TRUE
			}
		}
		else if(modelselected == "rf")
		{
			#randomForest
			m1=LoadPackage("randomForest")
			if( !grepl("ERROR", m1))
			{
				success = TRUE
			}
		}
		else if(modelselected == "svmLinear")
		{
			#kernlab
			m1=LoadPackage("kernlab")
			if( !grepl("ERROR", m1))
			{
				success = TRUE
			}
		}
		else if(modelselected == "svmPoly")
		{
			#kernlab
			m1=LoadPackage("kernlab")
			if( !grepl("ERROR", m1))
			{
				success = TRUE
			}
		}
		else if(modelselected == "svmRadial")
		{
			#kernlab
			m1=LoadPackage("kernlab")
			if( !grepl("ERROR", m1))
			{
				success = TRUE
			}
		}		
		else
		{
			cat(modelselected," model not supported.");
		}
	}
	else
	{
		print("No model selected");
	}
	invisible(success)
}

LoadPackage <- function(packagename)
{
	msg=""
	if (isPackageInstalled(packagename)) ## is package already installed
	{
		if (!isPackageloaded(packagename)) ## if package is not already loaded in memory
		{
			 eval(parse(text = paste("library(", packagename, ")")))
		}
		if (isPackageloaded(packagename)) 
		{
			msg=paste("Package", packagename, "loaded successfully")
			print(msg)
		}
		else 
		{
		  msg=paste("ERROR:Package", packagename, "loading failed")
		  print(msg)
		}
	}
	else
	{
		msg=paste("ERROR:Package", packagename, "not installed on your system.")
		msg2=paste("To install the package(s) go to Tools -> Package -> Install/Update package(s) from CRAN")
		cat(msg)
		cat("\n",msg2)
	}
	invisible( msg)
}

#get short name for  selected model 
#modelshortname = getModelInitials("{{modelselected}}")
#pass fullname e.g. 'randomForest' and it will return the initials like 'rf'
getModelInitials.old <- function(modelselected=NULL)
{

	initials=character(0)
	if(modelselected=="AdaBoost Classification Trees")#using fastAdaboost package
	{
		initials="adaboost"
	}
	else if(modelselected=="Bagged Logic Regression")#using logicFS package
	{
		initials="logicBag"
	}
	else if(modelselected=="Bayesian Ridge Regression")#using monomvn package
	{
		initials="bridge"
	}
	else if(modelselected=="CART") #using rpart package
	{
		initials="rpart"
	}	
	else if(modelselected=="Naive Bayes")#using klaR package
	{
		initials="nb"
	}
	else if(modelselected=="Random Forest") #using randomForest package and not Rborist
	{
		initials="rf"
	}
	else if(modelselected=="SVM (linear Kernal)") #Support Vector Machines with Linear Kernel: Required packages: kernlab
	{
		initials="svmLinear"
	}
	else if(modelselected=="SVM (Polynomial Kernal)") #Support Vector Machines with Polynomial Kernel: Required packages: kernlab
	{
		initials="svmPoly"
	}
	else if(modelselected=="SVM (Radial Basis)") #Support Vector Machines with Radial Basis Function Kernel: Required packages: kernlab
	{
		initials="svmRadial"
	}	
	else
	{
		initials="model not supported!"
	}
}

###### Aaron's New Functions 11Sep2017 ########
getModelInitials<-function (modelselected = NULL) 
{
    initials = character(0)
    if (modelselected == "AdaBoost Classification Trees") {
        initials = "adaboost"
    }
    else if (modelselected == "Bagged Logic Regression") {
        initials = "logicBag"
    }
    else if (modelselected == "Bayesian Ridge Regression") {
        initials = "bridge"
    }
    else if (modelselected == "CART") {
        initials = "rpart"
    }
    else if (modelselected == "Naive Bayes") {
        initials = "nb"
    }
    else if (modelselected == "Random Forest") {
        initials = "rf"
    }
    else if (modelselected == "SVM (linear Kernal)") {
        initials = "svmLinear"
    }
    else if (modelselected == "SVM (Polynomial Kernal)") {
        initials = "svmPoly"
    }
    else if (modelselected == "SVM (Radial Basis)") {
        initials = "svmRadial"
    }
    else if (modelselected == "KNN") {
        initials = "knn"
    }
    else {
        initials = "model not supported!"
    }
}


#renamed to Old on 19 Jan 2020
loadReqModelPackage.Old<-function (modelselected = NULL) 
{
    success = FALSE
    if (!is.null(modelselected)) {
        if (modelselected == "adaboost") {
            m1 = LoadPackage("fastAdaboost")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        else if (modelselected == "logicBag") {
            m1 = LoadPackage("logicFS")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        else if (modelselected == "bridge") {
            m1 = LoadPackage("monomvn")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        else if (modelselected == "rpart") {
            m1 = LoadPackage("rpart")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        else if (modelselected == "nb") {
            m2 = LoadPackage("klaR")
            if (!grepl("ERROR", m2)) {
                success = TRUE
            }
        }
        else if (modelselected == "rf") {
            m1 = LoadPackage("randomForest")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        else if (modelselected == "svmLinear") {
            m1 = LoadPackage("kernlab")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        else if (modelselected == "svmPoly") {
            m1 = LoadPackage("kernlab")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        else if (modelselected == "svmRadial") {
            m1 = LoadPackage("kernlab")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
         else if (modelselected == "knn") {
            m1 = LoadPackage("class")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }


        else {
            cat(modelselected, " model not supported.")
        }
    }
    else {
        print("No model selected")
    }
    invisible(success)
}

#new version on 19 Jan 2020
loadReqModelPackage.Old <-function (modelselected = NULL) 
{
    success = FALSE
	if (!is.null(modelselected)) 
	{
        if (modelselected == "gbm") 
		{
            m1 = LoadPackage("gbm")
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
			m1 = LoadPackage("plyr")
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
			 
        }
	
	    else if (modelselected == "xgbTree") 
		{
            m1 = LoadPackage("xgboost")#X was in capital letter I changed it to small because that was the package I found on CRAN: Anil
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
			m1 = LoadPackage("plyr")
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }						
        }
		
		else if (modelselected == "C5.0") 
		{
			m1 = LoadPackage("C50")
			if (!grepl("ERROR", m1)) 
			{
                success = TRUE
			}
			m1 = LoadPackage("plyr")
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
		}
		else if (modelselected == "C5.0Tree") 
		{
			m1 = LoadPackage("C50")
			if (!grepl("ERROR", m1)) 
			{
                success = TRUE
			}
			m1 = LoadPackage("plyr")
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
		}
		
        else if (modelselected == "ctree") 
		{
            m1 = LoadPackage("party")
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
        }
		
		else if (modelselected == "rlm") 
		{
            m1 = LoadPackage("MASS")
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
        }
		
		else if (modelselected == "glmnet") 
		{
            m1 = LoadPackage("glmnet")#G was in capital letter I changed it to small because that was the package I found on CRAN : Anil
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
			m1 = LoadPackage("Matrix")
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
        }
		
		else if (modelselected == "earth") 
		{
            m1 = LoadPackage("earth")
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
        }
		else if (modelselected == "nb") 
		{
            m1 = LoadPackage("klaR")
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
        }
		
		else if (modelselected == "nnet") 
		{
            m1 = LoadPackage("nnet")
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
        }
		else if (modelselected == "neuralnet") 
		{
            m1 = LoadPackage("neuralnet")
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
        }
		
		else if (modelselected == "dnn") 
		{
            m1 = LoadPackage("deepnet")
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
        }
		
		else if (modelselected == "rbf") 
		{
            m1 = LoadPackage("RSNNS")
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
        }
		
		else if (modelselected == "mlp") 
		{
            m1 = LoadPackage("RSNNS")
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
        }
		
		
		else if (modelselected == "cforest")
		{
            m1 = LoadPackage("party")
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
		}
		
		else if (modelselected == "ranger")
		{
            m1 = LoadPackage("ranger")
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
			m1 = LoadPackage("e1071")
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
			m1 = LoadPackage("dplyr")
            if (!grepl("ERROR", m1)) 
			{
                success = TRUE
            }
		}

		else if (modelselected == "svmPoly") 
		{
            m1 = LoadPackage("kernlab")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
		
		else if (modelselected == "svmRadial") 
		{
            m1 = LoadPackage("kernlab")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
		
		
		else if (modelselected == "adaboost") 
		{
            m1 = LoadPackage("fastAdaboost")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        else if (modelselected == "logicBag") {
            m1 = LoadPackage("logicFS")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        else if (modelselected == "bridge") {
            m1 = LoadPackage("monomvn")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        else if (modelselected == "rpart") {
            m1 = LoadPackage("rpart")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        else if (modelselected == "nb") {
            m2 = LoadPackage("klaR")
            if (!grepl("ERROR", m2)) {
                success = TRUE
            }
        }
        else if (modelselected == "rf") {
            m1 = LoadPackage("randomForest")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
		
		
        else if (modelselected == "svmLinear") {
            m1 = LoadPackage("kernlab")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        else if (modelselected == "svmPoly") {
            m1 = LoadPackage("kernlab")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        else if (modelselected == "svmRadial") {
            m1 = LoadPackage("kernlab")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        else if (modelselected == "knn") {
            m1 = LoadPackage("class")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        else {
            cat(modelselected, " model not supported.")
        }
    }
    else {
        print("No model selected")
    }
    invisible(success)
}

printModelSummary <- function(modelfamily, package,parameter)
{

mdat <- matrix(c(modelfamily,package,parameter), nrow = 3, ncol = 1, byrow = TRUE,
               dimnames = list(c("Model Family", "R Package","String specifying model type")
                               ))

BSkyFormat(mdat, singleTableOutputHeader="Model Tuning Summary")
}

#new 21 Jan 2020
loadReqModelPackage <-function (modelselected = NULL) 
{
    success = FALSE
	modelfamily=""
    if (!is.null(modelselected)) {
        if (modelselected == "gbm") {
            m1 = LoadPackage("gbm")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Boosted Trees"
				package ="gbm"
				parameter="gbm"
				printModelSummary(modelfamily, package, parameter)
				
            }
            m1 = LoadPackage("plyr")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        
		 else if (modelselected == "lmStepAIC") {
            m1 = LoadPackage("MASS")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Linear Regression"
				package ="MASS"
				parameter="lmStepAIC"
				printModelSummary(modelfamily, package, parameter)
				
				
            }
           
        }
		
		else if (modelselected == "xgbTree") {
            m1 = LoadPackage("xgboost")		
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Boosted Trees (Extreme gradient boosting)"
				package ="xgboost"
				parameter="xgbTree"
				printModelSummary(modelfamily, package, parameter)
            }
            m1 = LoadPackage("plyr")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        else if (modelselected == "C5.0") {
            m1 = LoadPackage("C50")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Boosted Trees"
				package ="C50"
				parameter="C5.0"
				printModelSummary(modelfamily, package, parameter)
            }
            m1 = LoadPackage("plyr")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        else if (modelselected == "C5.0Tree") {
            m1 = LoadPackage("C50")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Decision Trees (C50)"
				package ="C50"
				parameter="C5.0Tree"
				printModelSummary(modelfamily, package, parameter)
				
            }
            m1 = LoadPackage("plyr")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        else if (modelselected == "ctree") {
				
            m1 = LoadPackage("party")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Decision Trees (Conditional inference trees)"
				package="party"
				parameter="ctree"
				printModelSummary(modelfamily, package, parameter)
				
				
				
            }
        }
        else if (modelselected == "rlm") {
            m1 = LoadPackage("MASS")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Robust Linear Regression"
				package ="MASS"
				parameter="rlm"
				printModelSummary(modelfamily, package, parameter)
            }
        }
        else if (modelselected == "glmnet") {
            m1 = LoadPackage("glmnet")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Logistic Regression (Lasso and Elastic-Net Regularized Generalized Linear Models)"
				package ="glmnet"
				parameter="glmnet"
				printModelSummary(modelfamily, package, parameter)
				
				
            }
            m1 = LoadPackage("Matrix")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
   else if (modelselected == "glm") 
        {
          success = TRUE
		  modelfamily="Logistic Regression (Generalized Linear Model)"
			package ="stats"
				parameter="glm"
				printModelSummary(modelfamily, package, parameter)
        }
    else if (modelselected == "lm") 
        {
          success = TRUE
		   modelfamily="Linear Regression (linear models)"
		   package ="stats"
				parameter="lm"
				printModelSummary(modelfamily, package, parameter)
        }
        else if (modelselected == "earth") {
            m1 = LoadPackage("earth")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Multi-variate Adaptive Regression Spline"
				
				package ="earth"
				parameter="earth"
				printModelSummary(modelfamily, package, parameter)
            }
        }
        else if (modelselected == "nb") {
            m1 = LoadPackage("klaR")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Naive Bayes"
				package ="kLaR"
				parameter="nb"
				printModelSummary(modelfamily, package, parameter)
				
            }
        }
        else if (modelselected == "nnet") {
            m1 = LoadPackage("nnet")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Neural Net (Single hidden layer)"
				package ="nnet"
				parameter="nnet"
				printModelSummary(modelfamily, package, parameter)
				
            }
        }
        else if (modelselected == "neuralnet") {
            m1 = LoadPackage("neuralnet")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Neural Net (Train neural nets using backpropagation, RPROP, GRPROP)"
				package ="neuralnet"
				parameter="neuralnet"
				printModelSummary(modelfamily, package, parameter)
            }
        }
        else if (modelselected == "dnn") {
            m1 = LoadPackage("deepnet")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Neural Net (including BP,RBM,DBN,Deep autoencoder and so on.)"
				package ="deepnet"
				parameter="dnn"
				printModelSummary(modelfamily, package, parameter)
            }
        }
        else if (modelselected == "rbf") {
            m1 = LoadPackage("RSNNS")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Neural Net (RBF neural network)"
				package ="RSNNS"
				parameter="rbf"
				printModelSummary(modelfamily, package, parameter)
            }
        }
        else if (modelselected == "mlp") {
            m1 = LoadPackage("RSNNS")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Neural Net (multi-later perceptron)"
				package ="RSNNS"
				parameter="mlp"
				printModelSummary(modelfamily, package, parameter)
            }
        }
        else if (modelselected == "cforest") {
            m1 = LoadPackage("party")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Random Forest"
				package ="party"
				parameter="cforest"
				printModelSummary(modelfamily, package, parameter)
				
				
            }
        }
        else if (modelselected == "ranger") {
            m1 = LoadPackage("ranger")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Random Forest"
				package ="ranger"
				parameter="ranger"
				printModelSummary(modelfamily, package, parameter)
				
            }
            m1 = LoadPackage("e1071")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
            m1 = LoadPackage("dplyr")
            if (!grepl("ERROR", m1)) {
                success = TRUE
            }
        }
        else if (modelselected == "svmPoly") {
            m1 = LoadPackage("kernlab")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Support Vector Machines"
				package ="kernlab"
				parameter="svmPoly"
				printModelSummary(modelfamily, package, parameter)
				
            }
        }
        else if (modelselected == "svmRadial") {
            m1 = LoadPackage("kernlab")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Support Vector Machines"
				package ="kernlab"
				parameter="svmRadial"
				printModelSummary(modelfamily, package, parameter)
            }
        }
        else if (modelselected == "adaboost") {
            m1 = LoadPackage("fastAdaboost")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Boosted Trees/Adaboost Classification Trees"
				package ="fastAdaboost"
				parameter="adaboost"
				printModelSummary(modelfamily, package, parameter)
            }
        }
        else if (modelselected == "logicBag") {
            m1 = LoadPackage("logicFS")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Bagged Logic Regression"
				package ="logicFS"
				parameter="logicBag"
				printModelSummary(modelfamily, package, parameter)
				
            }
        }
        else if (modelselected == "bridge") {
            m1 = LoadPackage("monomvn")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Bayesian Ridge Regression"
				package ="monomvn"
				parameter="bridge"
				printModelSummary(modelfamily, package, parameter)
            }
        }
        else if (modelselected == "rpart") {
            m1 = LoadPackage("rpart")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Decision Trees (Recursive Partitioning and Regression Trees)"
				package ="rpart"
				parameter="rpart"
				printModelSummary(modelfamily, package, parameter)
            }
        }
        # else if (modelselected == "nb") {
            # m2 = LoadPackage("klaR")
            # if (!grepl("ERROR", m2)) {
                # success = TRUE
            # }
        # }
        else if (modelselected == "rf") {
            m1 = LoadPackage("randomForest")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Random Forest"
				package ="randomForest"
				parameter="rf"
				printModelSummary(modelfamily, package, parameter)
            }
        }
        else if (modelselected == "svmLinear") {
            m1 = LoadPackage("kernlab")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="Support Vector Machines"
				package ="kernlab"
				parameter="svmLinear"
				printModelSummary(modelfamily, package, parameter)
            }
        }
       
      
        else if (modelselected == "knn") {
            m1 = LoadPackage("class")
            if (!grepl("ERROR", m1)) {
                success = TRUE
				modelfamily="K Nearest Neighbors"
				package ="class"
				parameter="knn"
				printModelSummary(modelfamily, package, parameter)
				
            }
        }
        else {
            cat(modelselected, "model not supported.")
        }
    }
    else {
        print("No model selected")
    }
    invisible(success)
}

#####end  ### New Functions 11Sep2017