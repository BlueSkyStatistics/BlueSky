
#08Oct2021
BSky_Shapiro_Wilk_normality_test <- function (data = NULL, vars = NULL, dataset = NULL) 
{
	table_list = list()
	table_list_names = c()
	
	datasetname_passed = c("")
	
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
		
		if(class(data)[1] != "data.frame" && class(data)[1] != "matrix")
		{
			return(invisible(NULL))
		}
		
		if(is.null(vars) || length(trimws(vars)) == 0)
		{
			vars = dimnames(data)[[2]]
		}
		
		dataset = dataset_name_str
	}
	
	
    if(datasetname_passed == ".")
	{
		datasetObj = eval(parse(text = paste(dataset)))
		numeric_cols_names = unlist(lapply(datasetObj, is.numeric))
		vars = names(datasetObj)[numeric_cols_names]
	}
	else
	{
		datasetObj = eval(parse(text = paste(dataset)), envir = globalenv())
		numeric_cols_names = unlist(lapply(datasetObj, is.numeric))
		
		if(length(vars) > 0)
		{
			vars = vars[vars %in% names(numeric_cols_names[(numeric_cols_names == TRUE)])]
		}
		else
		{
			vars = names(datasetObj)[numeric_cols_names]
		}
	}
	
	count = length(vars)
	
    i = 1
    
	mdat <- matrix(nrow = 2, ncol = count, byrow = TRUE)
	dimnames(mdat)[[1]] = c("W", "p-value")
	dimnames(mdat)[[2]] = vars
    
	for (i in 1:count) {
		
		if(datasetname_passed == ".")
		{
			commandGen = paste("shapiro.test(", dataset, "[,vars[i]])", sep = "")
		}
		else
		{
			commandGen = paste("shapiro.test( .GlobalEnv$", dataset, "[,vars[i]])", sep = "")
		}
		
		#print(commandGen)
		
        temp <- eval(parse(text = commandGen))
		
        if (!is.null(temp)) {
            mdat[1, i] = temp$statistic
            mdat[2, i] = temp$p.value
        }
        else {
            print("Error with variable", var[i])
        }
    }
    
	#BSkyFormat(mdat, singleTableOutputHeader = "Shapiro Wilks Normality Test")
	table_list = list(mdat)
	table_list_names = c("Shapiro Wilks Normality Test")
	names(table_list) = table_list_names
	
	return(invisible(table_list))
}

