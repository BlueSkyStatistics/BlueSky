#r CMD INSTALL --build uadatapackage
#14Oct2021
### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title t-test, independent samples
#'
#' @description Performs a one sample t-tests against the two groups formed by a factor variable (with two levels). Displays results for equal variances TRUE and FALSE. For equal variances the pooled variance is used otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used. Internally calls t.test in the stats package for every selected variable 
#'
#' @param varNamesOrVarGlobalIndices selected scale variables (say var1, var2)
#' @param group a factor variable with two levels (say var3)
#' @param conf.level a numeric value  (say 0.95).
#' @param missing missing values are handled on a per variable basis (missing =0) or list wise across all variables (missing=1).
#' @param datasetNameOrDatasetGlobalIndex Name of the dataset (say Dataset)  from which var1, var2 and var3 are selected.
#' @param alternative  a character string specifying the alternative hypothesis, must be one of "two.sided"  (default), "greater" or "less". You can specify just the initial letter.
#'
#' @return A list with resulting tables is returned.
#'
#' @examples Dataset <- data.frame(Expenses=c(20,23,19,25,26), Sales=c(48,50,55,51,49), Gender=c('m','f','f','m','m'), Deptt=c('IT', 'Sales', 'IT','Sales','IT'))
#' BSky_One_Simple_T_Test = BSkyIndSmTTest(varNamesOrVarGlobalIndices =c('Sales','Expenses'),group=c('Deptt'),conf.level=0.95, alternative="less", datasetNameOrDatasetGlobalIndex ='Dataset')
BSkyIndSmTTest <-function (data = NULL, varNamesOrVarGlobalIndices = NULL, group = NULL, conf.level = 0.95, alternative="two.sided",
    datasetNameOrDatasetGlobalIndex = NULL, missing = 0, bSkyHandleSplit = TRUE, excludeEnvPrefix = FALSE,
    cohens_d=FALSE, cohensd_correction=FALSE, hedges_g =FALSE, hedgesg_correction=FALSE, glass_d=FALSE, glassd_correction=FALSE, debug=FALSE) 
{
    BSkyFunctionInit()
	
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
	else if(BSkyIsRmarkdownOutputOn() == TRUE)
	{
		# For Rstudio to work correctly when data parameter is NULL (i.e. not used with %>%)
		# data  is null but datasetNameOrDatasetGlobalIndex has the dataset name
		# BSkyLoadRefresh is needed to load the dataset in ua dataset list global obj
		# for BSKy functions e.g. crosstab, ind sample and one sample to work in RStudio 
		if(!exists("name", envir = uadatasets) || !(datasetNameOrDatasetGlobalIndex %in% uadatasets$name))
		{
			BSkyLoadRefresh(datasetNameOrDatasetGlobalIndex)
		}
	}
	
	
	if(length(group) == 0 && !is.null(data))
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
		
			replace_uasummary_7 =	paste("BSkyIndSmTTest(",
									"alternative=c(", alternative, "),", 
									"bSkyHandleSplit=c(", bSkyHandleSplit, "),", 
									"cohens_d=c(",cohens_d, "),", 
									"cohensd_correction=c(",cohensd_correction, "),", 
									"conf.level=c(", conf.level, "),", 
									"datasetNameOrDatasetGlobalIndex=c('", datasetNameOrDatasetGlobalIndex, "'),", 
									"excludeEnvPrefix=c(",excludeEnvPrefix, "),", 
									"glass_d=c(", glass_d, "),", 
									"glassd_correction=c(", glassd_correction, "),", 
									"group=c('", group, "'),", 
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
    bSkyVarnamesSplit = c(bSkyVarnames, group)
    BSkyErrMsg = paste("Error in Independent Sample T.Test", 
        " Dataset Name:", bSkyDatasetname, "Variables:", paste(bSkyVarnames, 
            collapse = ","))
    BSkyWarnMsg = paste("Warning in Independent Sample T.Test", 
        " Dataset Name:", bSkyDatasetname, "Variables:", paste(bSkyVarnames, 
            collapse = ","))
    BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
    tryCatch({
       withCallingHandlers({
            if (bSkyHandleSplit == TRUE) {
                bSkySplitIterationCount = BSkyComputeSplitdataset(bSkyVarnamesSplit, 
                  bSkyDatasetname)
                for (bSkySplitIterationCounter in 1:bSkySplitIterationCount) {
                  bSkyDatasetSliceIndex = BSkyGetNextDatasetSliceIndex(bSkyVarnamesSplit, 
                    bSkyDatasetname)
                  bSkyGlobalDataSliceIndexToWorkOn = bSkyDatasetSliceIndex$datasetSliceIndex
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
                  bSkyVarnamesIndex = BSkygetIndexesOfCols(bSkyVarnames, 
                    bSkyGlobalDataSliceIndexToWorkOn)
                  groupindex = BSkygetIndexesOfCols(group, bSkyGlobalDataSliceIndexToWorkOn)
                  uatemp = uaindsm(bSkyVarnamesIndex, groupindex, 
                    conf.level,alternative, index = bSkyGlobalDataSliceIndexToWorkOn, 
                    missing,cohens_d=cohens_d, cohensd_correction=cohensd_correction,hedges_g =hedges_g, hedgesg_correction=hedgesg_correction,glass_d=glass_d, glassd_correction=glassd_correction)
                  BSkyBuildReturnTableStructure(bSkyVarnames, 
                    bSkyDatasetname, OutputDataTableListIfPassed = NA)
                }
            }
            else {
                bSkyGlobalDatasetIndexToWorkOn = BSkyGetDatasetGlobalIndex(datasetNameOrDatasetGlobalIndex)
                bSkyVariableColumnIndicesOnDataSlice = BSkyGetVarGlobalIndices(bSkyVarnames, 
                  datasetNameOrDatasetGlobalIndex)
                groupindex = BSkygetIndexesOfCols(group, bSkyGlobalDataSliceIndexToWorkOn)
                uatemp = uaindsm(bSkyVariableColumnIndicesOnDataSlice, 
                  groupindex, conf.level,alternative, index = bSkyGlobalDataSliceIndexToWorkOn, 
                  missing,cohens_d=cohens_d, cohensd_correction=cohensd_correction,hedges_g =hedges_g, hedgesg_correction=hedgesg_correction,glass_d=glass_d, glassd_correction=glassd_correction)
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
	
	if(debug == TRUE)
	{
		return(invisible(bsky_return_structure))
	}
		
    #invisible(bsky_return_structure)
	#return(bsky_return_structure)
	table_list = BSkyFormatBSkyIndSampleTtest(bsky_return_structure)
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




bskystderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
	
	
uaindsm <- function (uavarindex, groupindex, conf.level,alternative, index, missing,cohens_d=FALSE, cohensd_correction=FALSE,hedges_g =FALSE, hedgesg_correction=FALSE,glass_d=FALSE, glassd_correction=FALSE) 
{
    BSkyFunctionInit()
    BSkyErrMsg = "Error in Independent Sample T.test"
    BSkyWarnMsg = "Warning in Independent Sample T.test"
    BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
    bskyNoofTables = length(uadatasets$retstructure)
    if (nrow(uadatasets$lst[[index]]) == 0) 	{
        uawritelog(type = "Error", BSkyMessage = "Independent Sample T test cannot be run as the dataset is empty")
        if (bskyNoofTables == 0) {
            uadatasets$retstructure[[1]] <- list()
            uadatasets$retstructure[[1]]$metadatatable[[1]] = data.frame()
            uadatasets$retstructure[[1]]$type = "table"
            uadatasets$retstructure[[1]]$metadata = "yes"
            uadatasets$retstructure[[1]]$nometadatatables = 1
            uadatasets$retstructure[[1]]$metadatatabletype = "normal"
            uadatasets$retstructure[[1]]$metadatatable = list()
            uadatasets$retstructure[[1]]$metadatatable[[1]] = data.frame(varIndex = NA, 
                type = -2, varName = NA, dataTableRow = NA, startCol = NA, 
                endCol = NA, BSkyMsg = "Independent Sample T Test cannot be run as the dataset is empty", 
                Rmsg = NA)
            uadatasets$retstructure[[1]]$datatable = NULL
            uadatasets$error = -1
            uadatasets$errorindex = uadatasets$errorindex + 1
        }
        else {
            uadatasets$retstructure[[bskyNoofTables]]$metadatatable[[1]] = rbind(uadatasets$retstructure[[bskyNoofTables]]$metadatatable[[1]], 
                data.frame(varIndex = NA, type = -2, varName = NA, 
                  dataTableRow = NA, startCol = NA, endCol = NA, 
                  BSkyMsg = "Independent Sample T Test cannot be run as the dataset is empty", 
                  Rmsg = NA))
        }
        BSkyFunctionWrapUp()
        return(TRUE)
    }
    m = 1
    n = 1
    noofvars = length(uavarindex)
    noofrows = noofvars * 2
    cindex = uahandlemissvalskind(index, uavarindex, groupindex, 
        noofvars, missing)
    uadesc = uaindsmdesc(cindex, uavarindex, groupindex, noofvars, 
        index)
    uamat = uaindsmttest(cindex, uavarindex, groupindex, noofvars, 
        alternative, conf.level, FALSE, index, uadesc[[2]])
    ualevene = ualevene.test(cindex, uavarindex, groupindex, 
        noofvars, index)
    while (m <= noofrows) {
        uamat[[1]][m, 1:2] = ualevene[[1]][n, 1:2]
        m = m + 2
        n = n + 1
    }
    uamat[[2]] = uacombinemat(uamat[[2]], ualevene[[2]])
	
	
	indexInReturnStructure=3		  
	if (cohens_d || cohensd_correction )
	{
	
	 uadatasets$retstructure[[indexInReturnStructure]] <- list()
    uadatasets$retstructure[[indexInReturnStructure]]$type = "table"
    uadatasets$retstructure[[indexInReturnStructure]]$metadata = "yes"
    uadatasets$retstructure[[indexInReturnStructure]]$nometadatatables = 1
    uadatasets$retstructure[[indexInReturnStructure]]$metadatatabletype = c("normal")
    uadatasets$retstructure[[indexInReturnStructure]]$metadatatable = list()
    uadatasets$retstructure[[indexInReturnStructure]]$metadatatable[[1]] = data.frame()
	
	cohensdIndSmTTest (cindex,uavarindex, groupindex, noofvars,correction=cohensd_correction, uacipass=conf.level, index, indexInReturnStructure)
	

	
	indexInReturnStructure =indexInReturnStructure+1
	
	}
	if (hedges_g || hedgesg_correction)
	{
	 uadatasets$retstructure[[indexInReturnStructure]] <- list()
    uadatasets$retstructure[[indexInReturnStructure]]$type = "table"
    uadatasets$retstructure[[indexInReturnStructure]]$metadata = "yes"
    uadatasets$retstructure[[indexInReturnStructure]]$nometadatatables = 1
    uadatasets$retstructure[[indexInReturnStructure]]$metadatatabletype = c("normal")
    uadatasets$retstructure[[indexInReturnStructure]]$metadatatable = list()
    uadatasets$retstructure[[indexInReturnStructure]]$metadatatable[[1]] = data.frame()
	hedgesgIndSmTTest (cindex,uavarindex,groupindex, noofvars,correction=hedgesg_correction, uacipass=conf.level, index,indexInReturnStructure)
	indexInReturnStructure =indexInReturnStructure+1
	}
	if (glass_d || glassd_correction )
	{
	 uadatasets$retstructure[[indexInReturnStructure]] <- list()
    uadatasets$retstructure[[indexInReturnStructure]]$type = "table"
    uadatasets$retstructure[[indexInReturnStructure]]$metadata = "yes"
    uadatasets$retstructure[[indexInReturnStructure]]$nometadatatables = 1
    uadatasets$retstructure[[indexInReturnStructure]]$metadatatabletype = c("normal")
    uadatasets$retstructure[[indexInReturnStructure]]$metadatatable = list()
    uadatasets$retstructure[[indexInReturnStructure]]$metadatatable[[1]] = data.frame()
	glassdIndSmTTest (cindex,uavarindex, groupindex,noofvars,correction=glassd_correction, uacipass=conf.level, index,indexInReturnStructure)
	}
	
	
    BSkyFunctionWrapUp()
    return(list(uadesc[[1]], uamat[[1]], uamat[[2]]))
}


		
	
uaindsmdesc <-function(cindex,uavarindex,groupindex,noofvars,index)
{
	BSkyFunctionInit()
	stdErrDiff=rep(NA,noofvars)
	bskyNoofTables=length(uadatasets$retstructure)
	
	
	if (bskyNoofTables ==0)
	{	
		#uadatasets$retstructure <-list(NULL)
		uadatasets$retstructure[[1]]<-list()
		uadatasets$retstructure[[1]]$type="table"
		uadatasets$retstructure[[1]]$metadata="yes"
		uadatasets$retstructure[[1]]$nometadatatables=1
		uadatasets$retstructure[[1]]$metadatatabletype=c("normal")
		#The line of code below is very important, this declares that metadatatable is a list.
		#If we don't do this and the object is undefined and we browse the object 
		#uadatasets$retstructure[[1]]$metadatatable, we get the value of uadatasets$retstructure[[1]]$metadatatabletype
		#This is because if x$test =10 and if x$te is undefined, when you browse x$te you get x$test
		uadatasets$retstructure[[1]]$metadatatable=list()
		
		#The line of code below is very important. This allocates an object for the 1 element of the list
		
		uadatasets$retstructure[[1]]$metadatatable[[1]]=data.frame()
	}
	#cat("\nwill call tapply soon\n")
	k=1
	j=1
	i=1
	len=noofvars*2
	uadatasets$retstructure[[1]]$datatable=matrix(nrow=len,ncol=4)
	#uamat =matrix(nrow=len,ncol=4)
	while (k < len)
		{
			#We are getting the counts only with the variable that we are doing an independent samples t.test on
			#We are not doing counts on the grouping variable
			#y <- y[!is.na(y)]
			temp1 =!is.na(eval(uadatasets$temppairs[k]))
			temp2=eval(uadatasets$temppairs[k+1])
			uatempcounts <-table(temp2[temp1])
			#uatemp <-table(eval(uadatasets$temppairs[k+1])[!is.na(eval(uadatasets$temppairs[k]))])
			#uatemp=table(eval(uadatasets$temppairs[k]))
			#This is the number of cases
			uadatasets$retstructure[[1]]$datatable[j,1] =uatempcounts[1]
			uadatasets$retstructure[[1]]$datatable[j+1,1]=uatempcounts[2]
			#cat("\ncalling tapply \n")
			#print(uadatasets$temppairs[k])
			#print(uadatasets$temppairs[k+1])

uatemp=tapply(eval(uadatasets$temppairs[k]), eval(uadatasets$temppairs[k+1]), base::mean, na.rm=TRUE)
			#cat("\nreturned tapply \n")
uadatasets$retstructure[[1]]$datatable[j,2] =uatemp[1]
uadatasets$retstructure[[1]]$datatable[j+1,2]=uatemp[2]

# uadatasets$retstructure[[1]]$datatable[j,2] =999
# uadatasets$retstructure[[1]]$datatable[j+1,2]=888

			#This is the std deviation
			
			if (uatempcounts[1] <= 1)
			{
				BSkywarnmsgdis =sprintf("Warning: Standard deviation cannot be calculated for variable \'%s\' for factor value \'%s\' as the number of valid cases is 1 or less",names(uadatasets$lst[[index]][uavarindex[i]]),names(uatempcounts[1]))
			
				uadatasets$warnindex =uadatasets$warnindex +1
				uadatasets$retstructure[[1]]$metadatatable[[1]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[1]], data.frame(varIndex=i,type=-1,varName=names(uadatasets$lst[[index]][uavarindex[i]]),dataTableRow=k,startCol=2,endCol=2,BSkyMsg=BSkywarnmsgdis,RMsg=""))		
				uawritelog(type="Warning", functionName="uaindsmdesc", BSkyMessage =BSkywarnmsgdis )
				
				
				BSkywarnmsgdis =sprintf("Warning: Standard error cannot be calculated for variable \'%s\' for factor value \'%s\' as the number of valid cases is 1 or less",names(uadatasets$lst[[index]][uavarindex[i]]),names(uatempcounts[1]))
				uadatasets$warnindex =uadatasets$warnindex +1
				uadatasets$retstructure[[1]]$metadatatable[[1]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[1]], data.frame(varIndex=i,type=-1,varName=names(uadatasets$lst[[index]][uavarindex[i]]),dataTableRow=k,startCol=2,endCol=3,BSkyMsg=BSkywarnmsgdis,RMsg=""))		
				uawritelog(type="Warning", functionName="uaindsmdesc", BSkyMessage =BSkywarnmsgdis )
			}
		
			if (uatempcounts[2] <=1)
			{
				BSkywarnmsgdis =sprintf("Warning: Standard deviation cannot be calculated for variable \'%s\' for factor value \'%s\' as the number of valid cases is 1 or less",names(uadatasets$lst[[index]][uavarindex[i]]),names(uatempcounts[2]))
			
				uadatasets$warnindex =uadatasets$warnindex +1
				uadatasets$retstructure[[1]]$metadatatable[[1]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[1]], data.frame(varIndex=i,type=-1,varName=names(uadatasets$lst[[index]][uavarindex[i]]),dataTableRow=k,startCol=2,endCol=2,BSkyMsg=BSkywarnmsgdis,RMsg=""))		
				uawritelog(type="Warning", functionName="uaindsmdesc", BSkyMessage =BSkywarnmsgdis )
				
				BSkywarnmsgdis =sprintf("Warning: Standard error cannot be calculated for variable \'%s\' for factor value \'%s\' as the number of valid cases is 1 or less",names(uadatasets$lst[[index]][uavarindex[i]]),names(uatempcounts[2]))
			
				uadatasets$warnindex =uadatasets$warnindex +1
				uadatasets$retstructure[[1]]$metadatatable[[1]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[1]], data.frame(varIndex=i,type=-1,varName=names(uadatasets$lst[[index]][uavarindex[i]]),dataTableRow=k,startCol=3,endCol=3,BSkyMsg=BSkywarnmsgdis,RMsg=""))		
				uawritelog(type="Warning", functionName="uaindsmdesc", BSkyMessage =BSkywarnmsgdis )
			}
			
			uatemp=tapply(eval(uadatasets$temppairs[k]), eval(uadatasets$temppairs[k+1]), stats::sd, na.rm=TRUE)
			uadatasets$retstructure[[1]]$datatable[j,3] =uatemp[1]
			uadatasets$retstructure[[1]]$datatable[j+1,3]=uatemp[2]
			uatemp=tapply(eval(uadatasets$temppairs[k]), eval(uadatasets$temppairs[k+1]), bskystderr)
			uadatasets$retstructure[[1]]$datatable[j,4] =uatemp[1]
			uadatasets$retstructure[[1]]$datatable[j+1,4]=uatemp[2]
			#2015
			#
			stdErrDiff[i] =uatemp[1]-uatemp[2]
			k=k+2
			j=j+2
			i=i+1
		}
	BSkyFunctionWrapUp()
	return (list(uadatasets$retstructure[[1]]$datatable,stdErrDiff))
}
	
	
uaindsmttest <-function (cindex, uavarindex, groupindex, noofvars, uaopt1pass = "two.sided", 
    uacipass, uatyoftestpass = FALSE, index, stdErrDiff) 
{
    BSkyFunctionInit()
    i = 1
    j = 1
    p = 1
    q = 1
	uatemp=NULL
    uadatasets$retstructure[[2]] <- list()
    uadatasets$retstructure[[2]]$type = "table"
    uadatasets$retstructure[[2]]$metadata = "yes"
    uadatasets$retstructure[[2]]$nometadatatables = 1
    uadatasets$retstructure[[2]]$metadatatabletype = c("normal")
    uadatasets$retstructure[[2]]$metadatatable = list()
    uadatasets$retstructure[[2]]$datatable = matrix(nrow = (noofvars * 
        2), ncol = 9)
    uadatasets$retstructure[[2]]$metadatatable[[1]] = data.frame()
    while (i <= noofvars) 
	{
        uavar = names(uadatasets$lst[[index]][uavarindex[i]])
        uadatasets$uawarnmsgdis = sprintf("Independent Sample T test on variable '%s' generated a warning", 
            uavar)
        uadatasets$uaerrmsgdis = sprintf("Independent Sample T test on variable '%s' generated an error", 
            uavar)
        if (!is.na(uavarindex[i])) 
		{
            while (q <= 2) 
			{
                if (q == 1) {
                  uavarequal = TRUE
                  k = i * 2 - 1
                }
                else {
                  uavarequal = FALSE
                  k = i * 2
                }
                if (uaperformance == 2) {
                  uastartlog("t.test", "uaindsmt.test")
                }
                tryCatch({
                  withCallingHandlers({
                    if (uavarequal == FALSE) {
                      temp1 = !is.na(eval(uadatasets$temppairs[p]))
                      temp2 = eval(uadatasets$temppairs[p + 
                        1])
                      uatempcounts <- table(temp2[temp1])
                        if (uatempcounts[1] <= 1 || uatempcounts[2] <= 
                        1) {
                        BSkywarnmsgdis = sprintf("Warning: T Test cannot be calculated for variable '%s' as there are too few cases", 
                          names(uadatasets$lst[[index]][uavarindex[i]]))
                        uadatasets$warnindex = uadatasets$warnindex + 
                          1
						  #Changed by Aaron, commented below
                        #uadatasets$retstructure[[1]]$metadatatable[[1]] = rbind(uadatasets$retstructure[[1]]$metadatatable[[1]], 
                        #  data.frame(varIndex = i, type = -1, 
                         #   varName = names(uadatasets$lst[[index]][uavarindex[i]]), 
                          #  dataTableRow = k, startCol = 2, endCol = 2, 
                          #  BSkyMsg = BSkywarnmsgdis, RMsg = ""))
							uadatasets$retstructure[[2]]$metadatatable[[1]] = rbind(uadatasets$retstructure[[2]]$metadatatable[[1]], 
                          data.frame(varIndex = i, type = -1, 
                            varName = names(uadatasets$lst[[index]][uavarindex[i]]), 
                            dataTableRow = k, startCol = 2, endCol = 2, 
                            BSkyMsg = BSkywarnmsgdis, RMsg = ""))
                        uawritelog(type = "Warning", functionName = "uaindsmttest", 
                          BSkyMessage = BSkywarnmsgdis)
                        uatemp$statistic = NA
                        uatemp$parameter = NA
                        uatemp$p.value = NA
                        uatemp$estimate[1] = NA
                        uatemp$estimate[2] = NA
                        uatemp$conf.int[1] = NA
                        uatemp$conf.int[2] = NA
                      }
                      else {
                        uatemp <- t.test(eval(uadatasets$temppairs[p]) ~ 
                          eval(uadatasets$temppairs[p + 1]), 
                          alternative = uaopt1pass, conf.level = uacipass, 
                          paired = uatyoftestpass, var.equal = uavarequal)
                      }
                    }
                    else {
                      uatemp <- t.test(eval(uadatasets$temppairs[p]) ~ 
                        eval(uadatasets$temppairs[p + 1]), alternative = uaopt1pass, 
                        conf.level = uacipass, paired = uatyoftestpass, 
                        var.equal = uavarequal)
                    }
                  }, warning = UAwarnHandlerFn)
                }, error = UAerrHandlerFn, silent = TRUE)
                if (uaperformance == 2) {
                  ualogcommand()
                }
                if (uadatasets$errorfn == -1) {
                  uadatasets$retstructure[[2]]$metadatatable[[1]] = rbind(uadatasets$retstructure[[2]]$metadatatable[[1]], 
                       data.frame(varIndex = i, type = -1, varName = uavar, 
                        dataTableRow = (i + q - 1), startCol = 3, 
                        endCol = 9, BSkyMsg = uadatasets$uaerrmsgdis, 
                        RMsg = uadatasets$uarerrmsg))
                  j = j + 1
                }
				
				
                if (uadatasets$uawarnfn == -1) {
                  len1 = length(uadatasets$uarwarnmsg)
                  k = 1
                  for (k in 1:len1) {
                    uadatasets$retstructure[[2]]$metadatatable[[1]] = rbind(uadatasets$retstructure[[2]]$metadatatable[[1]], 
                      data.frame(varIndex = i, type = 1, varName = uavar, 
                        dataTableRow = (i + q - 1), startCol = 3, 
                        endCol = 9, BSkyMsg = uadatasets$uawarnmsgdis, 
                        RMsg = uadatasets$uarwarnmsg[k]))
                  }
                  uadatasets$uawarnvar = NULL
                  uadatasets$uawarnmsgdis = NULL
                  uadatasets$uarwarnmsg = NULL
                }
				
				
				
                if (uadatasets$errorfn != -1) {
                  if (q == 1) {
                    uadatasets$errorfn = 0
                    uadatasets$warning = 0
                    uadatasets$retstructure[[2]]$datatable[j, 
                      3] <- uatemp$statistic
                    uadatasets$retstructure[[2]]$datatable[j, 
                      4] <- uatemp$parameter
                    uadatasets$retstructure[[2]]$datatable[j, 
                      5] <- uatemp$p.value
                    uadatasets$retstructure[[2]]$datatable[j, 
                      6] <- uatemp$estimate[1] - uatemp$estimate[2]
                    uadatasets$retstructure[[2]]$datatable[j, 
                      7] = stdErrDiff[i]
                    uadatasets$retstructure[[2]]$datatable[j, 
                      8] <- uatemp$conf.int[1]
                    uadatasets$retstructure[[2]]$datatable[j, 
                      9] <- uatemp$conf.int[2]
                    j <- j + 1
                  }
                  if (q == 2) {
                    uadatasets$errorfn = 0
                    uadatasets$warning = 0
                    uadatasets$retstructure[[2]]$datatable[j, 
                      3] <- uatemp$statistic
                    uadatasets$retstructure[[2]]$datatable[j, 
                      4] <- uatemp$parameter
                    uadatasets$retstructure[[2]]$datatable[j, 
                      5] <- uatemp$p.value
                    uadatasets$retstructure[[2]]$datatable[j, 
                      6] <- uatemp$estimate[1] - uatemp$estimate[2]
                    uadatasets$retstructure[[2]]$datatable[j, 
                      8] <- uatemp$conf.int[1]
                    uadatasets$retstructure[[2]]$datatable[j, 
                      9] <- uatemp$conf.int[2]
                    j <- j + 1
                  }
                }
                uadatasets$errorfn = 0
                uadatasets$uawarnfn = 0
                q = q + 1
            }
        }
        i = i + 1
        p = p + 2
        q = 1
    }
    return(list(uadatasets$retstructure[[2]]$datatable, uadatasets$retstructure[[2]]$metadatatable[[1]]))
    BSkyFunctionWrapUp()
}



# Do I need a call ualog=uaretlog("uaonesmt.test")



#Levenes test gets run for equal variances assumed and not
#At some point we may want to tweak the code for 
#uavarequal =FALSE as the test does not need to be run
# Although the test is run for uavarequal =FALSE it appropriately handles the situation and returns NAs



ualevene.test <-function(cindex,uavarindex,groupindex,noofvars, index)
{
	i=1
	p=1
	j=1
	uamat =matrix(nrow=noofvars,ncol=2)
	uamatdis=matrix(nrow=0,ncol=5)

	while (i <=noofvars)
	{
		if (!is.na(uavarindex[i]))
		{ 
			if(uaperformance==2)
			{
			uastartlog("levene.test","uaindsmt.test")	
			}
		
			tryCatch(
			{
			#NOTE: THE CODE HANDLES THE FACT THAT THERE CAN BE AN ERROR AND ONE OR MORE WARNINGS
			#ON RUNNING A ONE SAMPLE T.TEST ON A SINGLE VARIABLE. WE ALWAYS RETURN THE ERROR FIRST EVEN THOUGH THE WARNING OCCURED FIRST
			withCallingHandlers(
				{
				uatemp <-levenemod.test(eval(uadatasets$temppairs[p]),eval(uadatasets$temppairs[p+1]))
				},
			warning = function(ex) 
				{
				if(is.null(ex)) {ex <- warnings();}
				uadatasets$warning =-1;
				uadatasets$uawarnfn=-1
				uadatasets$warnindex=uadatasets$warnindex+1;
				uawarnvar =names(uadatasets$lst[[index]][uavarindex[i]])
				uarwarnmsg =conditionMessage(ex)
				uawarnmsgdis =sprintf("levenes test on variable %s generated a warning",uawarnvar)
				uadatasets$uawarnvar =c(uadatasets$uawarnvar,uawarnvar)
				uadatasets$uawarnmsgdis =c(uadatasets$uawarnmsgdis,uawarnmsgdis)
				uadatasets$uarwarnmsg =c(uadatasets$uarwarnmsg,uarwarnmsg );
				uawritelog(type="Warning", functionName="t.test", uberFunct="uaindsmt.test",RMessage =uarwarnmsg,BSkyMessage =uadatasets$warnmsgdis);
				invokeRestart("muffleWarning");
				}
			         ) # end of withCallingHandlers for catching warnings and continuing execution
			},

			error = function(ex) 
		        {
					uadatasets$error =-1;
					uadatasets$errorindex =uadatasets$errorindex +1
					uadatasets$errorfn=-1
					uaerrvar =names(uadatasets$lst[[index]][uavarindex[i]])
					uaerrmsgdis =sprintf("Levenes test on variable %s generated an error",uaerrvar) 		
					uarerrmsg=conditionMessage(ex)
					uawritelog(type="Error", functionName="levene.test",uberFunct="uaindsmt.test",RMessage =uarerrmsg,BSkyMessage=uaerrmsgdis);
					uadatasets$results=c(as.character(i),"-1",uaerrvar,uaerrmsgdis, uarerrmsg)		        
		        },
		silent =TRUE) #end of try catch
		
			if(uaperformance==2)
			{
			ualogcommand()	
			}
			if( uadatasets$errorfn ==-1)
			{
				uadatasets$retstructure[[2]]$metadatatable[[1]] =rbind(uadatasets$retstructure[[2]]$metadatatable[[1]],uadatasets$results)
				# AS the levene test for the variable failed, the entries in the uamat for that variable should be NA
				# we need to advance the uamat to the next variable
				j=j+1
			}	
			if(uadatasets$uawarnfn ==-1)
			{
				len1 =length(uadatasets$uarwarnmsg)
				# k is used for indexing the warning variables
				k=1
				for (k in 1:len1)
				{
					#1st position is the index of the variable in uavarindex
					#2nd position is whether everything is OK (1), its an error(-1) or warning(-2)
					#3rd position is the variable name
					#4th position is the warning message that should be displayed
					#5th position is the R warning message
					uadatasets$retstructure[[2]]$metadatatable[[1]]= rbind(uadatasets$retstructure[[2]]$metadatatable[[1]], c(as.character(i),"1",uadatasets$uawarnvar[k],uadatasets$uawarnmsgdis[k],uadatasets$uarwarnmsg[k]))
				}	
				uadatasets$uawarnvar=NULL
				uadatasets$uawarnmsgdis=NULL
				uadatasets$uarwarnmsg=NULL
			}
			if (uadatasets$errorfn != -1)
			{			
				uadatasets$errorfn =0
				uadatasets$warning=0
				#uamatdis =rbind(uamatdis,c(as.character(i), "0",NA,NA,NA))
				#uadatasets$retstructure[[2]]$datatable[j,1]
				#uamat[j,1] <-uatemp[[1]]
				#uamat[j,2] <-uatemp[[2]]
				uadatasets$retstructure[[2]]$datatable[j,1]<-uatemp[[2]]
				uadatasets$retstructure[[2]]$datatable[j,2]<-uatemp[[1]]
				j<-j+2				
			}
			uadatasets$errorfn=0
			uadatasets$uawarnfn=0
		}#End of if
	i=i+1
	p=p+2
	}#End of while
return(list(uamat,uamatdis))
}	

uacombinemat <-function(mat1,mat2)
{
	len1 =nrow(mat1)
	len2=nrow(mat2)
	newmat=matrix(nrow=0,ncol=5)
	i=1
	j=1
	while (j<=len2)
	{
		newmat=rbind(newmat,mat2[j,])
		i=1
		while (i<=len1)		
		{
			if(mat2[j,1] ==mat1[i,1])	newmat=rbind(newmat,mat1[i,])
			i=i+1	
		}
		j=j+1	
	}
	newmat
}


	
	
	
	
