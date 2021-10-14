
# test123 <-function( i =1) 
# {
  # BSkyFunctionInit()
  # obj = c(1,2,3,4)
  # BSkyFunctionWrapUp()
  # obj=BSkyReturnStructure2()
  # return(invisible(obj))
# }


# From http://stackoverflow.com/questions/23617662/extract-arima-specificaiton
BSkyArimaString <-function (object) 
{
  order <- object$arma[c(1, 6, 2, 3, 7, 4, 5)]
  result <- paste("ARIMA(", order[1], ",", order[2], ",", order[3],  ")", sep = "")
  if (order[7] > 1 & sum(order[4:6]) > 0) 
  {
    result <- paste(result, "(", order[4], ",", order[5], ",", order[6], ")[", order[7], "]", sep = "")
  }
  if (is.element("constant", names(object$coef)) | is.element("intercept", names(object$coef))) 
  {
    result <- paste(result, "with non-zero mean")
  }
  else if (is.element("drift", names(object$coef))) 
  {
    result <- paste(result, "with drift        ")
  }
  else if (order[2] == 0 & order[5] == 0) 
  {
    result <- paste(result, "with zero mean    ")
  }
  else
    {
      result <- paste(result, "                  ")
    }
  return(result)
  
}



BSkyPrintARIMA<-function (x, digits = max(3, getOption("digits") - 3), se = TRUE, ...) 
{
  BSkyFunctionInit()
 # BSkyBuildReturnTableStructure2(paste("Series:", x$series))
  BSkyBuildReturnTableStructure2(BSkyArimaString(x), singleTableOutputHeader = "Series:")
  if (!is.null(x$lambda)) 
  {
    BSkyBuildReturnTableStructure2(paste("Box Cox transformation: lambda=", x$lambda))
  }
  if (length(x$coef) > 0) 
    {
    #cat("\nCoefficients:\n")
    coef <- round(x$coef, digits = digits)
    if (se && NROW(x$var.coef)) 
      {
      ses <- rep.int(0, length(coef))
      ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
      coef <- matrix(coef, 1L, dimnames = list(NULL, names(coef)))
      coef <- rbind(coef, s.e. = ses)
      }
    #print.default(coef, print.gap = 2)
    listOfCoeff =list(coef)
    names (listOfCoeff) =c("Coefficients")
    BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = listOfCoeff)
    }
  cm <- x$call$method
  if (is.null(cm) || cm != "CSS") 
    {
    #cat("\nsigma^2 estimated as ", format(x$sigma2, digits = digits),  ":  log likelihood=", format(round(x$loglik, 2L)),   "\n", sep = "")
    npar <- length(x$coef[x$mask]) + 1
    nstar <- length(x$residuals) - x$arma[6] - x$arma[7] * x$arma[5]
    bic <- x$aic + npar * (log(nstar) - 2)
    aicc <- x$aic + 2 * npar * (nstar/(nstar - npar - 1) -  1)
    #cat("AIC=", format(round(x$aic, 2L)), sep = "")
    #cat("   AICc=", format(round(aicc, 2L)), sep = "")
    #cat("   BIC=", format(round(bic, 2L)), "\n", sep = "")
    
    #help(matrix)
    mdat =matrix(c(format(x$sigma2, digits = digits),format(round(x$loglik, 2L)),format(round(x$aic, 2L)),format(round(aicc, 2L)),format(round(bic, 2L))),nrow=5,ncol=1,dimnames = list(c("nsigma^2 estimated as", "log likelihood","AIC","AICc","BIC"),c("Values")))
    listformdat =list(mdat)
	names(listformdat)=c("Model Statistics")
    BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = listformdat, singleTableOutputHeader ="Model statistics" )
  
    
    }
  else 
    {
     # cat("\nsigma^2 estimated as ", format(x$sigma2, digits = digits), ":  part log likelihood=", format(round(x$loglik, 2)), "\n", sep = "")
      mdat =matrix(c(format(x$sigma2, digits = digits),format(round(x$loglik, 2))),nrow=2,ncol=1,dimnames = list(c("nsigma^2 estimated as", "part log likelihood")  ,c("Values")))
      listformdat =list(mdat)
      names(listformdat)=c("Model Statistics")
      BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = listformdat )

    }
  BSkyFunctionWrapUp()
  
}










#the maintitle and ylab apply to the main time series only and not the correlation plots
  
  BSkyPlotSeriesWithCorrelations <-function (vars, start, frequency, autocorrelation=FALSE,autocovariance=FALSE, partialautocorrelations=FALSE, main ="",ylab="",dataset, excludeEnvPrefix=FALSE)
  {
    
    BSkyFunctionInit()
	
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
	
    varcount =length(vars)
    for (i in 1:varcount)
    {
      stringForPlot =c("ts(",dataset, "[,", deparse(vars[i]),"]",",","start=",deparse(start),",","frequency =",frequency,")")
      
      BSkyTS= eval(parse(text=stringForPlot))
      #Plot series
      if(main !="")
      {
        main =paste("Time series plot for variable ", vars[i])
      }
      if (ylab =="")
      {
        plot(BSkyTS, main =main)
      }
      else
      {
        plot(BSkyTS, main =main,ylab=ylab)
        
      }
      if (autocorrelation)
      {
        mainTitle =paste("Autocorrelation plot for variable ", vars[i])
        forecast::Acf(x=BSkyTS, main =mainTitle)
      }
      
      if(autocovariance)
      {
        mainTitle =paste("Autocovariance plot for variable ", vars[i])
        forecast::Acf(x=BSkyTS, type="covariance", main =mainTitle)
      }
      
      if(partialautocorrelations)
      {
        mainTitle =paste("Partial autocorrelation plot for variable ", vars[i])
        forecast::Pacf(x=BSkyTS, main =mainTitle)
      }
    }
    
    BSkyFunctionWrapUp()
    #obj=BSkyReturnStructure2()
    #return(invisible(obj))
    
  }
  
  
  
  
  ### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Exponential Smoothing
#'
#' @description BSkyHoltWintersSeasonal is a wrapper function for HoltWinters from package stats that computes Holt-Winters Filtering of a given time series. Unknown parameters are determined by minimizing the squared prediction error. Internally calls HoltWinters with  gamma parameter used for the seasonal component set to FALSE, beta parameter of Holt-Winters Filter is set to FALSE to do exponential smoothing and seasonal = "None".
#' 
#' @param vars select a variable to build a model for
#' @param start Time of first observation should be entered in the format year,month or year,quarter e.g.( if your data is organized in months the 1992,1 for Jan 1992 or if your data is organized in quarters then 1992,1 refers to the first quarter of 1992.
#' @param frequency Number of observations in unit time. Example: for monthly there are 12 observation in a year. For quarterly there are 4 observation in a year.
#' @param exponential Determines whether exponential smoothing will be done, value set to TRUE
#' @param seasonal a character string  "None" for exponential smoothing.
#' @param plotSeries if TRUE a  time series plot will also be generated.
#' @param saveFitted if TRUE fit values are saved.
#' @param plotOriginalandForecast Plot original and forecasted series
#' @param predict if TRUE predicted values  will also be generated.
#' @param savePredictedVals predicted values will be saved.
#' @param plotPredictedValues predicted values will also be plotted.
#' @param correlogram if TRUE a correlogram will be generated.
#' @param main main title of the plot
#' @param ylab title for the y axis
#' @param dataset the name of the dataset from which the variables have been selected
#'
#' @return An object of class "HoltWinters", a list with components:
#' fitted: A multiple time series with one column for the filtered series as well as for the level, trend and seasonal components, estimated contemporaneously (that is at time t and not at the end of the series).
#' x: The original series
#' alpha: alpha used for filtering
#' beta: beta used for filtering
#' gamma: gamma used for filtering
#' coefficients: A vector with named components a, b, s1, ..., sp containing the estimated values for the level, trend and seasonal components
#' seasonal: The specified seasonal parameter
#' SSE: The final sum of squared errors achieved in optimizing
#' call: The call used
#'
#' @examples
  BSkyHoltWintersSeasonal <-function (vars, start, frequency, seasonal="add", exponential =FALSE,plotSeries=TRUE,saveFitted=FALSE, fittedValsDatasetName="", plotOriginalandForecast=FALSE,predict=FALSE, periodToPredict=0, savePredictedVals=FALSE, predictedValsDatasetName="",plotPredictedValues=FALSE, correlogram=FALSE,lag.max=0,Ljung_Boxtest=FALSE, maintitle ="",ylab="",dataset, excludeEnvPrefix=FALSE)
  {
      BSkyFunctionInit()
	  
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
	  
    varcount =length(vars)
    for (i in 1:varcount)
    {
      stringForPlot =c("ts(",dataset, "[,", deparse(vars[i]),"]",",","start=",deparse(start),",","frequency =",frequency,")")
      
      BSkyTS= eval(parse(text=stringForPlot))
      #Plot series
      if (plotSeries ==TRUE)
      {
        mainTitle =paste("Time series plot for variable ", vars[i])
        if (ylab=="") 
          {
          ylab =vars[i]
        }
        plot(BSkyTS, main =mainTitle,ylab=ylab)
      }
      
      if (seasonal=="Non Seasonal")
      {
        BSkyRes <-HoltWinters(BSkyTS, gamma=FALSE)
      }
      else if (exponential ==TRUE)
      {
        BSkyRes <-HoltWinters(BSkyTS, gamma=0, beta =0)
        
      }
      else if (seasonal =="add" ||seasonal =="mult")
      {
        BSkyRes <-HoltWinters(BSkyTS, seasonal =seasonal)
      }
      
      #Code for smoothing parameters
      matForAlphaBetaGamma =matrix(c(BSkyRes$alpha,BSkyRes$beta,BSkyRes$gamma),nrow=3, ncol=1, dimnames=list(c("alpha","beta","gamma"), c("values")) )
      listForAlphaBetaGamma =list(matForAlphaBetaGamma)
      names(listForAlphaBetaGamma)="Smoothing parameters"
      #BSkyFormat(listForAlphaBetaGamma)
      BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = listForAlphaBetaGamma)
      
      
      #Code for sum of square errors
      BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = BSkyRes$SSE,singleTableOutputHeader="Sum of square Errors")
      
      
      #Code for Coefficients
      matForCoeff =matrix(BSkyRes$coeff,ncol=1, dimnames=list(names(BSkyRes$coeff), c("values")) )
      listForCoeff =list(matForCoeff)
      names(listForCoeff) ="Coefficients"
      BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = listForCoeff)
      #BSkyFormat(listForCoeff)
      
      
      #Save fitted values to the dataset
      
      if (saveFitted ==TRUE)
      {
        stringToEvaluate =c(fittedValsDatasetName,"<<-as.data.frame(BSkyRes$fitted)")
        stringToEvaluate=paste(stringToEvaluate, collapse='')
        eval(parse(text=stringToEvaluate))
      }
      
      #Plot original against the fitted
      
      if (plotOriginalandForecast==TRUE)
      {
        mainTitle =paste("Observed vs fitted for variable", vars[i])
        plot(BSkyRes, main=mainTitle,ylab=ylab)
        legend("topright", c("observed","fitted"), lty=c(1,1), lwd=c(2.5,2.5), col=c(1,2))
      }
      
      
      #Creating a forecast
      if (predict ==TRUE)
      {
        BSkyForecastRes <- forecast::forecast(BSkyRes, h=periodToPredict)
        
        #Plot the forecast 
        if (plotPredictedValues ==TRUE)
        {
          mainTitle =paste("Predicted forecast for ", vars[i])        
          plot(BSkyForecastRes,main=mainTitle, ylab=ylab,fcol=4, col=1)
          legend("topright", c("observed","predicted interval"), lty=c(1,1), lwd=c(2.5,2.5), col=c(1,4))
        }
        
        if (savePredictedVals ==TRUE)
        {
          stringToEvaluate =c(predictedValsDatasetName,"<<-as.data.frame(BSkyForecastRes$fitted)")
          stringToEvaluate=paste(stringToEvaluate, collapse='')
          eval(parse(text=stringToEvaluate))
        }
        
        #calculate a correlogram
        if (correlogram ==TRUE)
        {
          forecast::Acf(residuals(BSkyForecastRes$model), lag.max=lag.max, main =paste("Correlogram of in sample errors in predictions of ",vars[i], "with Max lag",lag.max))
          
        }
        
        
        
        #             Ljung_Boxtest=TRUE
        if (Ljung_Boxtest==TRUE)
        {
          boxTestRes<- Box.test(BSkyForecastRes$residuals, lag=lag.max, type="Ljung-Box")
          htestres <-BSkyFormat2(boxTestRes)
          #              htestres <-BSkyFormat2 (boxTestRes, bSkyFormatAppRequest = TRUE)
          #              listForReturnStructure =list(htestres$tables[[1]])
          #              BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = listForReturnStructure)
        }
      }
    }
    
    BSkyFunctionWrapUp()
    obj=BSkyReturnStructure2()
    return(invisible(obj))
  }
  
  ### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Plot Time Series
#'
#' @description Creates time series plot in combined or separately. When combined multiple variables are plotted together, when separate each variable is plotted independently. BSkyPlotSeriesWithCorrelations is a wrapper function that creates a time series object and internally calls plot.ts. 
#'
#' @param vars selected variables to plot
#' @param start Time of first observation should be entered in the format year,month or year,quarter e.g.( if your data is organized in months the 1992,1 for Jan 1992 or if your data is organized in quarters then 1992,1 refers to the first quarter of 1992.
#' @param frequency Number of observations in unit time. Example: for monthly there are 12 observation in a year. For quarterly there are 4 observation in a year.
#' @param plot.type "multiple" for separate and "single" for combined plot.
#' @param naturalLogYaxis if TRUE an Y axis is shown as natural log value.
#' @param main main title of the plot
#' @param ylab title for the y axis
#' @param dataset the name of the dataset from which the vars has been picked.
#'
#' @return
#'
#' @examples
  BSkyPlotTimeSeries <-function (vars ,start, frequency, plot.type="multiple", naturalLogYaxis=FALSE, main ="", ylab="",dataset, excludeEnvPrefix=FALSE) 
    
  {
    
    BSkyFunctionInit()
	
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
	
    stringForPlot =c("ts(",dataset, "[,", deparse(vars),"]",",","start=",deparse(start),",","frequency =",frequency,")")
    
    BSkyTS= eval(parse(text=stringForPlot))
    
    if (plot.type =="single")
    {
      lty =1:length(vars)
      col=2:(length(vars)+1)
      
      if (naturalLogYaxis ==TRUE)
      {
        plot(x=BSkyTS, plot.type = plot.type, lty=lty,log="y", col=col, ylab=ylab, main=main ) 
        legend("topright", vars, lty=rep(1,length(vars)), lwd=rep(2.5,length(vars)),col=col)
        
      }
      else
      {
        plot(x=BSkyTS, plot.type = plot.type, lty=lty, col=col, ylab=ylab, main=main ) 
        legend("topright", vars, lty=rep(1,length(vars)), lwd=rep(2.5,length(vars)),col=col)
      }
    }
    if (plot.type =="multiple")
    {
      if (naturalLogYaxis ==TRUE)
      {
        plot(x=BSkyTS, plot.type = plot.type,log="y", ylab=ylab, main=main )
      }
      else
      {
        plot(x=BSkyTS, plot.type = plot.type, ylab=ylab, main=main ) 
      }
    }
    BSkyFunctionWrapUp()
    
  }
  

  BSkyAutoArima <-function (vars, start, frequency, ic="bic", plotSeries=TRUE,saveFitted=FALSE,fittedValsDatasetName="",plotResiduals =FALSE, plotOriginalandForecast=FALSE,predict=FALSE, periodToPredict=0,confInterval=c(95), savePredictedVals=FALSE, predictedValsDatasetName="",plotPredictedValues=FALSE, correlogram=FALSE,lag.max=0,Ljung_Boxtest=FALSE, maintitle ="",ylab="",dataset, excludeEnvPrefix=FALSE)
  {
    
    BSkyFunctionInit()
	
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
	
    varcount =length(vars)
    for (i in 1:varcount)
    {
      stringForPlot =c("ts(",dataset, "[,", deparse(vars[i]),"]",",","start=",deparse(start),",","frequency =",frequency,")")
      
      BSkyTS= eval(parse(text=stringForPlot))
      if (ylab=="") 
      {
        ylab =vars[i]
      }
      #Plot series
      if (plotSeries ==TRUE)
      {
        mainTitle =paste("Time series plot for variable ", vars[i])
        
        plot(BSkyTS, main =mainTitle,ylab=ylab)
      }
      
      if (ic=="bic")
      {
        BSkyRes <-auto.arima(BSkyTS,ic="bic")
      }
      else if (ic =="aicc")
      {
        BSkyRes <-auto.arima(BSkyTS, ic="aicc")
        
      }
      else if (ic=="aic")
      {
        BSkyRes <-auto.arima(BSkyTS, ic="aic")
      }
      
     BSkyPrintARIMA(BSkyRes)
      #BSkyFormat(listForCoeff)
      
      
     
     if (plotResiduals ==TRUE)
     {
       mainTitle =paste("Residuals of fitted model for ", vars[i])
       ylab =vars[i]
       plot(BSkyRes$residuals,ylab=ylab,main =mainTitle) 
       
     }
     
      #Save fitted values to the dataset
     
      
      if (saveFitted ==TRUE)
      {
        stringToEvaluate =c(fittedValsDatasetName,"<<-as.data.frame(BSkyRes$fitted)")
        stringToEvaluate=paste(stringToEvaluate, collapse='')
        eval(parse(text=stringToEvaluate))
      }
      
      #Plot original against the fitted
      
      if (plotOriginalandForecast==TRUE)
      {
        mainTitle =paste("Observed vs fitted for variable", vars[i])
        plot(BSkyRes, main=mainTitle)
        legend("topright", c("observed","fitted"), lty=c(1,1), lwd=c(2.5,2.5), col=c(1,2))
      }
      
      
      #Creating a forecast
      if (predict ==TRUE)
      {
        BSkyForecastRes <- forecast::forecast(BSkyRes, h=periodToPredict, level =confInterval)
        
        #Plot the forecast 
        if (plotPredictedValues ==TRUE)
        {
          mainTitle =paste("Predicted forecast for ", vars[i])        
          plot(BSkyForecastRes,main=mainTitle,ylab=ylab, fcol=4, col=1)
          legend("topright", c("observed","predicted interval"), lty=c(1,1), lwd=c(2.5,2.5), col=c(1,4))
        }
        
        if (savePredictedVals ==TRUE)
        {
          stringToEvaluate =c(predictedValsDatasetName,"<<-as.data.frame(BSkyForecastRes$fitted)")
          stringToEvaluate=paste(stringToEvaluate, collapse='')
          eval(parse(text=stringToEvaluate))
        }
        
        #calculate a correlogram
        if (correlogram ==TRUE)
        {
          forecast::Acf(residuals(BSkyForecastRes$model), lag.max=lag.max, main =paste("Correlogram of in sample errors in predictions of ",vars[i], "with Max lag",lag.max))
          
        }
        
        
        
        #             Ljung_Boxtest=TRUE
        if (Ljung_Boxtest==TRUE)
        {
          boxTestRes<- Box.test(BSkyForecastRes$residuals, lag=lag.max, type="Ljung-Box")
          htestres <-BSkyFormat2(boxTestRes)
          #              htestres <-BSkyFormat2 (boxTestRes, bSkyFormatAppRequest = TRUE)
          #              listForReturnStructure =list(htestres$tables[[1]])
          #              BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = listForReturnStructure)
        }
      }
    }
    
    BSkyFunctionWrapUp()
    obj=BSkyReturnStructure2()
    return(invisible(obj))
  }
  
  ### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Plot Time Series
#'
#' @description Creates time series plot in combined or separately. When combined multiple variables are plotted together, when separate each variable is plotted independently. BSkyPlotSeriesWithCorrelations is a wrapper function that creates a time series object and internally calls plot.ts. 
#'
#' @param vars selected variables to plot
#' @param start Time of first observation should be entered in the format year,month or year,quarter e.g.( if your data is organized in months the 1992,1 for Jan 1992 or if your data is organized in quarters then 1992,1 refers to the first quarter of 1992.
#' @param frequency Number of observations in unit time. Example: for monthly there are 12 observation in a year. For quarterly there are 4 observation in a year.
#' @param plot.type "multiple" for separate and "single" for combined plot.
#' @param naturalLogYaxis if TRUE an Y axis is shown as natural log value.
#' @param main main title of the plot
#' @param ylab title for the y axis
#' @param dataset the name of the dataset from which the vars has been picked.
#'
#' @return
#'
#' @examples
  BSkyPlotTimeSeries <-function (vars ,start, frequency, plot.type="multiple", naturalLogYaxis=FALSE, main ="", ylab="",dataset, excludeEnvPrefix=FALSE) 
    
  {
    
    BSkyFunctionInit()
	
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
	
    stringForPlot =c("ts(",dataset, "[,", deparse(vars),"]",",","start=",deparse(start),",","frequency =",frequency,")")
    
    BSkyTS= eval(parse(text=stringForPlot))
    
    if (plot.type =="single")
    {
      lty =1:length(vars)
      col=2:(length(vars)+1)
      
      if (naturalLogYaxis ==TRUE)
      {
        plot(x=BSkyTS, plot.type = plot.type, lty=lty,log="y", col=col, ylab=ylab, main=main ) 
        legend("topright", vars, lty=rep(1,length(vars)), lwd=rep(2.5,length(vars)),col=col)
        
      }
      else
      {
        plot(x=BSkyTS, plot.type = plot.type, lty=lty, col=col, ylab=ylab, main=main ) 
        legend("topright", vars, lty=rep(1,length(vars)), lwd=rep(2.5,length(vars)),col=col)
      }
    }
    if (plot.type =="multiple")
    {
      if (naturalLogYaxis ==TRUE)
      {
        plot(x=BSkyTS, plot.type = plot.type,log="y", ylab=ylab, main=main )
      }
      else
      {
        plot(x=BSkyTS, plot.type = plot.type, ylab=ylab, main=main )
      }
     }
    BSkyFunctionWrapUp()
    
  }

