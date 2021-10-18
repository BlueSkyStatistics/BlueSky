######## list of methods in this file; in sequence (few may not be ready and unused ) ###########
# bsky.one.sm.t.test.oldest
# bsky.one.sm.t.test.old
# BSkyOneSmTTest
#################################################################################################

#30Jul2018	
# Changes to BSkyOneSmTTest,  uaonesample, uaonesamttest to support alternative

#14Oct2021
# Means> T-test, Independent Sample 
# Last modified 10/7/2021
### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title t-test, One Sample
#'
#' @description Performs one sample t-tests on selected variables. Optionally computes effect size indices for standardized differences: Cohen's d and Hedges' g (This function returns the population estimate.)
#'
#' @param varNamesOrVarGlobalIndices selected scale variables (say var1, var2)
#' @param mu a number indicating the true value of the mean. (say 10)
#' @param conf.level a numeric value  (say 0.95).
#' @param missing missing values are handled on a per variable basis (missing =0) or list wise across all variables (missing=1).
#' @param datasetNameOrDatasetGlobalIndex Name of the dataset (say Dataset)  from which var1, var2 and var3 are selected.
#' @param alternative  a character string specifying the alternative hypothesis, must be one of "two.sided"  (default), "greater" or "less". You can specify just the initial letter.
#'
#' @return A list with the results of the test
#'
#' @examples Dataset <- data.frame(Expenses=c(20,23,19,25,26), Sales=c(48,50,55,51,49), Gender=c('m','f','f','m','m'), Deptt=c('IT', 'Sales', 'IT','Sales','IT'))
#' BSky_One_Simple_T_Test = BSkyOneSmTTest(varNamesOrVarGlobalIndices =c('Sales','Expenses'),mu=.9,conf.level=0.95,alternative ="two.sided", datasetNameOrDatasetGlobalIndex ='Dataset',missing=1)
BSkyOneSmTTest <-function (data = NULL, varNamesOrVarGlobalIndices = NULL, mu = 0, conf.level = 0.95, alternative = 'two.sided',
    datasetNameOrDatasetGlobalIndex = NULL, missing = 0, bSkyHandleSplit = TRUE, 
	cohens_d=FALSE, cohensd_correction=FALSE, hedges_g =FALSE, hedgesg_correction=FALSE, glass_d=FALSE, glassd_correction=FALSE) 
{
    BSkyFunctionInit()
	#print(match.call())
	
	datasetname_passed = c("")
	
	stripped_data = data
	
	if(!is.null(data))
	{
		if(class(data)[1] != "character")
		{
			dataset_name_str = deparse(substitute(data))
			
			if(dataset_name_str == ".")
			{
				datasetname_passed = dataset_name_str
				dataset_name_str = "data" 
			}
			
			#print(head(data))
		}
		else
		{
			dataset_name_str = data
			data = eval(parse(text=data), envir = globalenv())
		}
		
		datasetNameOrDatasetGlobalIndex = dataset_name_str
	}
	else if(length(datasetNameOrDatasetGlobalIndex) == 0)
	{
		return(invisible(NULL))
	}
	else 
	{
		# For Rstudio to work correctly when data parameter is NULL (i.e. not used with %>%)
		# data  is null but datasetNameOrDatasetGlobalIndex has the dataset name
		# BSkyLoadRefresh is needed to load the dataset in ua dataset list global obj
		# for BSKy functions e.g. crosstab, ind sample and one sample to work in RStudio 
		BSkyLoadRefresh(datasetNameOrDatasetGlobalIndex)
	}
	
	
	if(!is.null(data)) # No group by for one sample t-test if(length(group) == 0 && !is.null(data)) - but filer out grouping variable if passed 
	{
		group_by_col_names = names(as.data.frame(attr(data, "groups")))
		
		if(length(group_by_col_names) > 0) 
		{
			group_by_col_names = group_by_col_names[1:(length(group_by_col_names) - 1)] # dropping the ".rows" col names from dplyr tibble table 
			
			#selected_group_by_col_names_list = paste(paste(group_by_col_names, "=", dataset_name_str, 
			#											"$", group_by_col_names, sep = ""), sep = "", collapse = ",")
			
			#group = eval(parse(text = paste("list(", selected_group_by_col_names_list, ")", sep = "")))
			group = group_by_col_names
			
			#print(selected_group_by_col_names_list)								
			#print(group)
			
			#stripped_data = data[c(length(group_by_col_names): ncol(data))] # group_by_col_names includes columns and additional .rows column
			stripped_data = data[, !(names(data) %in% c(group_by_col_names))] # group_by_col_names includes columns and additional .rows column
		}
	}
	
	
	if(length(varNamesOrVarGlobalIndices) == 0 && !is.null(data))
	{
		varNamesOrVarGlobalIndices = dimnames(stripped_data)[[2]]
		
		#selected_col_names_list = paste(paste(col_names, "=", dataset_name_str, 
		#												"$", col_names, sep = ""), sep = "", collapse = ",")
			
		#datasetColumnObjects = eval(parse(text = paste("list(", 
		#												selected_col_names_list, ")", sep = "")))
	}
	
	
	if(datasetname_passed == ".")
	{
		#temp_pipe_dataset_name = tempfile(pattern = "pipe_data_", tmpdir = "")
		#temp_pipe_dataset_name = substr(temp_pipe_dataset_name, 2, nchar(temp_pipe_dataset_name))
		temp_pipe_dataset_name = "bsky_piped_temp_dataset"
		eval(parse(text= paste(temp_pipe_dataset_name, "<<- data"))) #, envir = globalenv())
		BSkyLoadRefresh(temp_pipe_dataset_name)
		 
		datasetname = temp_pipe_dataset_name

		BSkySetCurrentDatasetName(datasetname, setDatasetIndex ="y")
		bSkyDatasetname = BSkyGetDatasetName(datasetname)
		datasetNameOrDatasetGlobalIndex = datasetname
		#bSkyHandleSplit = FALSE
	
			replace_uasummary_7 =	paste("BSkyOneSmTTest(",
									"alternative=c(", alternative, "),", 
									"bSkyHandleSplit=c(", bSkyHandleSplit, "),", 
									"conf.level=c(", conf.level, "),", 
									"mu=c(", mu, "),", 
									"datasetNameOrDatasetGlobalIndex=c('", datasetNameOrDatasetGlobalIndex, "'),",
									"cohens_d=c(",cohens_d, "),", 
									"cohensd_correction=c(",cohensd_correction, "),", 									
									"glass_d=c(", glass_d, "),", 
									"glassd_correction=c(", glassd_correction, "),", 
									"hedges_g=c(",hedges_g, "),", 
									"hedgesg_correction=c(", hedgesg_correction, "),", 
									"missing=c(",missing, "),", 
									"varNamesOrVarGlobalIndices=c(",paste(varNamesOrVarGlobalIndices,collapse=","),"))", sep="")
							
		#print(replace_uasummary_7)
		#cat("\n",replace_uasummary_7, "\n")
		#return 
	}
	else
	{
		#This is just to set the context to the current dataset name
		#This also copies the dataset to uadatasets$lst for backward compatibility
		BSkySetCurrentDatasetName(datasetNameOrDatasetGlobalIndex, setDatasetIndex ="y")
		bSkyDatasetname = BSkyGetDatasetName(datasetNameOrDatasetGlobalIndex)
	}
	
	
    pos = regexpr("varNamesOrVarGlobalIndices", uadatasets.sk$rproc)
    newstr = substr(uadatasets.sk$rproc, 1, pos[1] + 26)
    y = paste("c('", paste(varNamesOrVarGlobalIndices, sep = ",", 
        collapse = "','"), "')", sep = "")
    finalstr = paste(newstr, y, ")", sep = "")
    uadatasets.sk$rproc = finalstr
    
	#BSkySetCurrentDatasetName(datasetNameOrDatasetGlobalIndex, setDatasetIndex = "y")
    #bSkyDatasetname = BSkyGetDatasetName(datasetNameOrDatasetGlobalIndex)
    
	bSkyVarnames = BSkyGetVarNames(varNamesOrVarGlobalIndices, 
        datasetNameOrDatasetGlobalIndex)
    BSkyErrMsg = paste("Error in One Sample T Test", " Dataset Name:", 
        bSkyDatasetname, "Variables:", paste(bSkyVarnames, collapse = ","))
    BSkyWarnMsg = paste("Warning in One Sample T Test", " Dataset Name:", 
        bSkyDatasetname, "Variables:", paste(bSkyVarnames, collapse = ","))
    BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
    tryCatch({
        withCallingHandlers({
            if (bSkyHandleSplit == TRUE) {
                bSkySplitIterationCount = BSkyComputeSplitdataset(bSkyVarnames, 
                  bSkyDatasetname)
                for (bSkySplitIterationCounter in 1:bSkySplitIterationCount) {
                  bSkyDatasetSliceIndex = BSkyGetNextDatasetSliceIndex(bSkyVarnames, 
                    bSkyDatasetname)
                  bSkyGlobalDataSliceIndexToWorkOn = bSkyDatasetSliceIndex$datasetSliceIndex
                  bSkyVariableColumnIndicesOnDataSlice = bSkyDatasetSliceIndex$varIndex
                  if (bSkySplitIterationCount > 1) {
                    BSkyErrMsgwithSplit = paste(BSkyErrMsg, "Current Factors, if there is Split :", 
                      paste(BSkyComputeCurrentVarNamesAndFactorValues(bSkyDatasetname), 
                        collapse = ","), sep = " * ")
                    BSkyWarnMsgWithSplit = paste(BSkyWarnMsg, 
                      "Current Factors, if there is Split :", 
                      paste(BSkyComputeCurrentVarNamesAndFactorValues(bSkyDatasetname), 
                        collapse = ","), sep = " * ")
                    BSkyStoreApplicationWarnErrMsg(BSkyWarnMsgWithSplit, 
                      BSkyErrMsgwithSplit)
                  }
                  uaonesample(bSkyVariableColumnIndicesOnDataSlice, 
                    mu, conf.level, alternative ,cohens_d=cohens_d, cohensd_correction=cohensd_correction,hedges_g =hedges_g,hedgesg_correction=hedgesg_correction,glass_d=glass_d, glassd_correction=glassd_correction,bSkyGlobalDataSliceIndexToWorkOn, 
                    missing)
                  BSkyBuildReturnTableStructure(bSkyVarnames, 
                    bSkyDatasetname, OutputDataTableListIfPassed = NA)
                }
            }
            else {
                bSkyGlobalDatasetIndexToWorkOn = BSkyGetDatasetGlobalIndex(datasetNameOrDatasetGlobalIndex)
                bSkyVariableColumnIndicesOnDataset = BSkyGetVarGlobalIndices(varNamesOrVarGlobalIndices, 
                  datasetNameOrDatasetGlobalIndex)
                uaonesample(bSkyVariableColumnIndicesOnDataset, 
                  mu, conf.level, alternative,cohens_d=cohens_d, cohensd_correction=cohensd_correction,hedges_g =hedges_g,hedgesg_correction=hedgesg_correction,glass_d=glass_d, glassd_correction=glassd_correction,bSkyGlobalDatasetIndexToWorkOn, 
                  missing)
                BSkyBuildReturnTableStructure(bSkyVarnames, bSkyDatasetname, 
                  OutputDataTableListIfPassed = NA)
            }
        }, warning = UAwarnHandlerFn)
    }, error = UAerrHandlerFn, silent = TRUE)
    if (BSkyGlobalErrorFound() == TRUE) {
        BSkyBuildReturnTableStructure(bSkyVarnames, bSkyDatasetname, 
            OutputDataTableListIfPassed = NA)
    }
    if (BSkyGlobalWarningFound() == TRUE) {
    }
    BSkyFunctionWrapUp()
	
	bsky_return_structure = BSkyReturnStructure2()
	
	if(datasetname_passed == ".")
	{
		bsky_return_structure$uasummary[[7]] = replace_uasummary_7
	}
	
	#print("Returning from one sample")
    #invisible(bsky_return_structure)
	table_list = BSkyFormatBSkyOneSampleTtest(bsky_return_structure)
	table_list = table_list$tables[1:(table_list$nooftables -1)]
	
	
	if(BSkyIsRmarkdownOutputOn() == TRUE)
	{
		return(noquote(table_list))
	}
	else
	{
		return(invisible(table_list))
	}
}
