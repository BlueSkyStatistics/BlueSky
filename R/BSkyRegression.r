BSkyRegression.no.weights <-function (depVars,indepVars,dataset, excludeEnvPrefix=FALSE)
{
BSkyFunctionInit()
count =length(indepVars)
result=c()
for (i in 1:count)
 {
  result =c(result, paste(indepVars[i]))
  if (i !=count) result = c(result, '+')
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
		
 
 stringForlm= c("lm(",depVars, "~",result, ",na.action=na.exclude, data=", dataset,")" )
#print(stringForlm)
mod =eval(parse(text= stringForlm))
modsum <-summary (mod)
 BSkyFormat(modsum)
#BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = sumTable)

modAnova =anova(mod)
df =as.data.frame(modAnova)
#df =tidy(modAnova)
#colnames(df)[5] <- "F Value"

#BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed =df, singleTableOutputHeader="Anova Table")
BSkyFormat(df, singleTableOutputHeader="Anova Table")
totalrows =nrow(df)

regSumOfSquares =sum(df[1:totalrows-1,3])
residualSumOfSquares =df[totalrows,3]
totalSumOfSquares =regSumOfSquares+residualSumOfSquares

matSumOfSquares =matrix(c(regSumOfSquares,residualSumOfSquares,totalSumOfSquares),nrow=3,ncol=1,dimnames =list(c("Sum of squares of Regression", "Sum of squares of residuals","Total sum of squares" ), c("Values")  ))
#print(matSumOfSquares)
#BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed =matSumOfSquares, singleTableOutputHeader="Sum of squares Table")
BSkyFormat(matSumOfSquares,singleTableOutputHeader="Sum of squares Table")

 BSkyFunctionWrapUp()
return(invisible(mod))

}


BSkyRegression <-function (depVars,indepVars,weights=NA, nointercept=FALSE,dataset, excludeEnvPrefix=FALSE)
{
BSkyFunctionInit()
count =length(indepVars)
result=c()
for (i in 1:count) {
        if (nointercept && i ==1)
        {
            result = "0 + "
        }
        result = c(result, paste(indepVars[i]))
        if (i != count) 
        {
            result = c(result, "+")
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
		
 if(is.na(weights))
 {
 stringForlm= c("lm(",depVars, "~",result, ",na.action=na.exclude ,data=", dataset,")" )

 }
 else
 {
  stringForlm= c("lm(",depVars, "~",result,", weights=",weights, ",na.action=na.exclude, data=", dataset,")" )
 }
#print(stringForlm)
mod =eval(parse(text= stringForlm))
modsum <-summary (mod)
 BSkyFormat(modsum)
#BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = sumTable)

modAnova =anova(mod)
df =as.data.frame(modAnova)
#df =tidy(modAnova)
#colnames(df)[5] <- "F Value"

#BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed =df, singleTableOutputHeader="Anova Table")
BSkyFormat(df, singleTableOutputHeader="Anova Table")
totalrows =nrow(df)

regSumOfSquares =sum(df[1:totalrows-1,2])
residualSumOfSquares =df[totalrows,2]
totalSumOfSquares =regSumOfSquares+residualSumOfSquares

matSumOfSquares =matrix(c(regSumOfSquares,residualSumOfSquares,totalSumOfSquares),nrow=3,ncol=1,dimnames =list(c("Sum of squares of Regression", "Sum of squares of residuals","Total sum of squares" ), c("Values")  ))
#print(matSumOfSquares)
#BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed =matSumOfSquares, singleTableOutputHeader="Sum of squares Table")
BSkyFormat(matSumOfSquares,singleTableOutputHeader="Sum of squares Table")

 BSkyFunctionWrapUp()
return(invisible(mod))

}

