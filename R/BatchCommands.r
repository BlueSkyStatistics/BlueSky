CommandBatch <- function(datasetname, vars)
{
	BSkyFunctionInit()
		# datasetname = "accidents"; vars = c('accid', 'pop')
		#This is just to set the context to the current dataset name
		BSkySetCurrentDatasetName(datasetname, setDatasetIndex ="y")
      	uaSplitIterationCount = BSkyComputeSplitdataset(vars, datasetname) ## vars:: all variables in target list box
			# cat("\nNo. of DS in lst before loop:\n")
			# print(length(uadatasets$lst))
      	for (uaSplitIterationCounter in 1:uaSplitIterationCount)
		{
			# cat("\nNo. of DS in lst in loop:\n",length(uadatasets$lst))
			uaDatasetSliceIndex = BSkyGetNextDatasetSliceIndex(vars, datasetname)## "Dataset1"

        	uaGlobalDataSliceIndexToWorkOn = uaDatasetSliceIndex$datasetSliceIndex
			#cat("\nSlice index:"); print(uaDatasetSliceIndex); cat("\tSliced Dataset\n")
        	#print(uadatasets$lst[[uaGlobalDataSliceIndexToWorkOn]])
			# print(uaVariableColumnIndexOnDataSlice)
AA <- length(vars)
BB <- nrow(uadatasets$lst[[uaGlobalDataSliceIndexToWorkOn]] )
CC <- AA * BB
#cat("\nAA=",AA,"\tBB=",BB,"\tCC=",CC)
group <- gl(AA,BB,CC, labels=vars)
weight <- c(uadatasets$lst[[uaGlobalDataSliceIndexToWorkOn]]$accid, uadatasets$lst[[uaGlobalDataSliceIndexToWorkOn]]$pop)
lm.D9 <- lm(weight ~ group)
lm.D90 <- lm(weight ~ group - 1) # omitting intercept
#BSkyFormat(lm.d90$df)
anova(lm.D9)
summary(lm.D90)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.D9, las = 1)      # Residuals, Fitted, ...
par(opar)
Sys.sleep(5)
		}
	}
		
# > CommandBatchTest <- function(flag)
# + {
# + if(flag==1)
# + BSkyFunctionInit()
# + else
# + BSkyFunctionWrapUp()
# + }