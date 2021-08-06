

BSky_Shapiro_Wilk_normality_test<-function(vars, dataset)
{
#BSkyFunctionInit()
count =length(vars)
 i=1
mdat <- matrix( nrow = 2, ncol = count, byrow = TRUE,
               dimnames = list(c("W", "p-value"),
                               vars))
for(i in 1:count)
{
    
   commandGen =paste("shapiro.test(.GlobalEnv$", dataset, "[,vars[i]])", sep="") #23Jan2016
#commandGen
    #temp <-shapiro.test(dataset2$var[i])
   temp <-eval(parse(text=commandGen))
    if (!is.null(temp))
    {
        mdat[1,i]=temp$statistic
        mdat[2,i]=temp$p.value
    }
    else
    {
         print("Error with variable",var[i])
    }

}
BSkyFormat(mdat,singleTableOutputHeader="Shapiro Wilks Normality Test")

 #BSkyFunctionWrapUp()
#return(invisible(mdat))
}
