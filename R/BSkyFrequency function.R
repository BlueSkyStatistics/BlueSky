
#Dialog editor calling syntax
# BSkyNullMucher = BSkyFrequency( vars = c("var1 name", "var2 name",...) , data = {{%DATASET%}})
# data is the dataset object itself not the string name

#This function goes into BSky R package 
#13Oct2021
### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Frequency table
#'
#' @description Generates the frequencies for every unique value in one or more variables or column names selected.
#'
#' @param data The dataset name
#' @param vars selected variables
#'
#' @return frequency table
#'
#' @examples
BSkyFrequency.old <- function (data = NULL, vars = NULL, decreasing = TRUE)
{
    table_list = list()
    table_list_names = c()
    if (class(data)[1] != "character") {
        dataset_name_str = deparse(substitute(data))
        if (dataset_name_str == ".") {
            dataset_name_str = "data"
        }
    }
    else {
        dataset_name_str = data
        data = eval(parse(text = data), envir = globalenv())
    }
    if (is.null(vars) || length(trimws(vars)) == 0) {
        vars = dimnames(data)[[2]]
    }
    BSky_Dataset_Overview = data.frame(Dataset = c(dataset_name_str),
        Variables = length(names(data)), Nominals = length(x = which(lapply(data,
            is.factor) == TRUE)), Observations = nrow(data))
    selected_var_list = paste(paste(vars, "=", dataset_name_str,
        "$", vars, sep = ""), sep = "", collapse = ",")
    BSky_Variable_List = eval(parse(text = paste("list(", selected_var_list,
        ")", sep = "")))
    BSky_Summary_By_Variable = ftable(summary(data[names(BSky_Variable_List)]))
    BSky_Summary_By_Variable = as.data.frame(as.matrix(BSky_Summary_By_Variable))
    rownames(BSky_Summary_By_Variable) = c()
    table_list = list(BSky_Dataset_Overview)
    table_list_names = c("Dataset Overview")
    names(table_list) = table_list_names
    table_list = c(table_list, list(BSky_Summary_By_Variable))
    table_list_names = c(table_list_names, "Summary By Variable")
    names(table_list) = table_list_names
    for (i in 1:length(BSky_Variable_List)) {
        freq = as.data.frame(table(BSky_Variable_List[[i]], useNA = "always"))
        percent = as.data.frame(prop.table(table(BSky_Variable_List[[i]],
            useNA = "always")))
        percent$Freq = percent$Freq * 100
        y = 0
        cumPercent = t(data.frame((lapply(percent$Freq, function(x) y <<- y +
            x))))
        validFreq = as.data.frame(table(BSky_Variable_List[[i]]))
        validPercent = as.data.frame(prop.table(table(BSky_Variable_List[[i]])))
        validPercent$Freq = validPercent$Freq * 100
        y = 0
        validCumPercent = t(data.frame((lapply(validPercent$Freq,
            function(x) y <<- y + x))))
        if (length(cumPercent) > length(validCumPercent)) {
            validPercent[] = lapply(validPercent, as.character)
            validPercent = rbind(validPercent, c("", ""))
            validCumPercent = rbind(validCumPercent, c(""))
        }
        
        BSky_freqTable = cbind(freq, percent$Freq, cumPercent,
            validPercent$Freq, validCumPercent)
        BSky_freqTable = BSky_freqTable[order(BSky_freqTable$Freq, decreasing = decreasing),]
        names(BSky_freqTable) = c(names(BSky_Variable_List)[i],
            "Frequency", "Percent", "CumPercent", "Valid Percent",
            "Valid CumPercent")
        row.names(BSky_freqTable) = NULL
        tableHeader = paste("Frequency Table for", names(BSky_Variable_List)[i])
        table_list = c(table_list, list(BSky_freqTable))
        table_list_names = c(table_list_names, tableHeader)
        names(table_list) = table_list_names
    }
    if (BSkyIsRmarkdownOutputOn() == TRUE) {
        return((table_list))
    }
    else {
        return(invisible(table_list))
    }
}


#08May24
BSkyFrequency <- function (data = NULL, vars = NULL, order_by = c("freq","var","none"), decreasing = TRUE)
{
    table_list = list()
    table_list_names = c()
    if (class(data)[1] != "character") {
        dataset_name_str = deparse(substitute(data))
        if (dataset_name_str == ".") {
            dataset_name_str = "data"
        }
    }
    else {
        dataset_name_str = data
        data = eval(parse(text = data), envir = globalenv())
    }
    if (is.null(vars) || length(trimws(vars)) == 0) {
        vars = dimnames(data)[[2]]
    }
    BSky_Dataset_Overview = data.frame(Dataset = c(dataset_name_str),
        Variables = length(names(data)), Nominals = length(x = which(lapply(data,
            is.factor) == TRUE)), Observations = nrow(data))
    selected_var_list = paste(paste(vars, "=", dataset_name_str,
        "$", vars, sep = ""), sep = "", collapse = ",")
    BSky_Variable_List = eval(parse(text = paste("list(", selected_var_list,
        ")", sep = "")))
    BSky_Summary_By_Variable = ftable(summary(data[names(BSky_Variable_List)]))
    BSky_Summary_By_Variable = as.data.frame(as.matrix(BSky_Summary_By_Variable))
    rownames(BSky_Summary_By_Variable) = c()
    table_list = list(BSky_Dataset_Overview)
    table_list_names = c("Dataset Overview")
    names(table_list) = table_list_names
    table_list = c(table_list, list(BSky_Summary_By_Variable))
    table_list_names = c(table_list_names, "Summary By Variable")
    names(table_list) = table_list_names
	
  
    for (i in 1:length(BSky_Variable_List)) {
        
		if(order_by[1] == "var")
		{
			if(is.factor(BSky_Variable_List[[i]])) 
				BSky_Variable_List[[i]] = as.character(BSky_Variable_List[[i]])
			BSky_Variable_List[[i]] = BSky_Variable_List[[i]][order(BSky_Variable_List[[i]], decreasing =decreasing)]
			BSky_Variable_List[[i]] = factor(BSky_Variable_List[[i]], levels = unique(BSky_Variable_List[[i]]))
		}

		freq = as.data.frame(table(BSky_Variable_List[[i]], useNA = "always"))
      	percent = as.data.frame(prop.table(table(BSky_Variable_List[[i]],useNA = "always")))
        percent$Freq = percent$Freq * 100
		
		if(order_by[1] == "freq")
		{
			freq = freq[order(freq$Freq, decreasing =decreasing),]
			percent = percent[order(percent$Freq, decreasing =decreasing),]
		}
      
        y = 0
        cumPercent = t(data.frame((lapply(percent$Freq, function(x) y <<- y + x))))
        
		validFreq = as.data.frame(table(BSky_Variable_List[[i]]))
        validPercent = as.data.frame(prop.table(table(BSky_Variable_List[[i]])))
        validPercent$Freq = validPercent$Freq * 100
		
		if(order_by[1] == "freq")
		{
			validFreq = validFreq[order(validFreq$Freq, decreasing =decreasing),]
			validPercent = validPercent[order(validPercent$Freq, decreasing =decreasing),]
		}
      
        y = 0
        validCumPercent = t(data.frame((lapply(validPercent$Freq,function(x) y <<- y + x))))
        
		if (length(cumPercent) > length(validCumPercent)) {
            validPercent[] = lapply(validPercent, as.character)
            validPercent = rbind(validPercent, c("", ""))
            validCumPercent = rbind(validCumPercent, c(""))
        }
        
		BSky_freqTable = cbind(freq, percent$Freq, cumPercent,
            validPercent$Freq, validCumPercent)
        
		# BSky_freqTable = BSky_freqTable[order(BSky_freqTable$Freq,
            # decreasing = decreasing), ]
        
        names(BSky_freqTable) = c(names(BSky_Variable_List)[i],
            "Frequency", "Percent", "CumPercent", "Valid Percent","Valid CumPercent")
                                               
        row.names(BSky_freqTable) = NULL
        tableHeader = paste("Frequency Table for", names(BSky_Variable_List)[i])
        table_list = c(table_list, list(BSky_freqTable))
        table_list_names = c(table_list_names, tableHeader)
        names(table_list) = table_list_names
    }
    if (BSkyIsRmarkdownOutputOn() == TRUE) {
        return((table_list))
    }
    else {
        return(invisible(table_list))
    }
}
