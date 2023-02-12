
#13Oct2021
### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Shapiro-Wilk Normality Test
#'
#' @description Performs the Shapiro-Wilk test of normality.
#'
#' @param x: a numeric vector of data values. Missing values are allowed, but the number of non-missing values must be between 3 and 5000.
#'
#' @return A list with class "htest" containing the following components:
#' statistic: the value of the Shapiro-Wilk statistic.
#' p.value: an approximate p-value for the test. This is said in Royston (1995) to be adequate for p.value < 0.1.
#' method: the character string "Shapiro-Wilk normality test".
#' data.name: a character string giving the name(s) of the data.
#'
#' @examples
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
    
	#BSkyFormat(mdat, singleTableOutputHeader = "Shapiro Wilk Normality Test")
	table_list = list(mdat)
	table_list_names = c("Shapiro Wilk Normality Test")
	names(table_list) = table_list_names
	
	if(BSkyIsRmarkdownOutputOn() == TRUE)
	{
		return((table_list))
	}
	else
	{
		return(invisible(table_list))
	}
}



