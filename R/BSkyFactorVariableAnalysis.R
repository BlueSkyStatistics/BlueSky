
#### "Analysis" menu >> "Summary Analysis" sub-menu >> "Factor Variable Analysis" dialog syntax

#Dialog editor calling syntax
#BSkyNullMucher = BSkyFactorVariableAnalysis( vars = c("var1 name", "var2 name",...) , data = {{%DATASET%}}, show.only.top.factors ={{ChkboxShowOnlyTopFewFactors}}, max.number.top.factors={{txtNumTopFactorsToShow}})

#This function goes into BSky R package
#13Oct2021
### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Factor Variable Analysis, top N
#'
#' @description Displays the counts of every level of each factor variable.
#'
#' @param data The dataset name
#' @param vars The factor variables to display level counts
#' @param show.only.top.factors A boolean determining whether counts of only top levels or all levels should be displayed.
#' @param max.number.top.factors if show.only.top.factors = TRUE , specifies the number of levels to display counts for
#'
#' @return Factor variable analysis
#'
#' @examples
BSkyFactorVariableAnalysis <- function(data = NULL, vars = NULL, show.only.top.factors=TRUE, max.number.top.factors=30)
{
		ChkboxShowOnlyTopFewFactors = show.only.top.factors
		txtNumTopFactorsToShow = max.number.top.factors
		
		m1=character(0)
		m2=character(0)
		message=character(0)
		isAnyNonFactor=FALSE
		
		#BSky_Factor_Variable_List = list(model = Dataset2$model,type = Dataset2$type)
		#BSky_Factor_Variable_List = list({{subsetvars}})
		
		#dataset_name_str = deparse(substitute(data))
		
		table_list = list()
		table_list_names = c()
		
		if(class(data)[1] != "character")
		{
			dataset_name_str = deparse(substitute(data))
			
			if(dataset_name_str == ".")
			{
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
		
		#BSky_Dataset_Overview = data.frame(Dataset = c("{{%DATASET%}}"),Variables = length(names({{%DATASET%}})),Nominals = length(x = which(lapply({{%DATASET%}}, is.factor) == TRUE)), Observations = nrow({{%DATASET%}}))
		BSky_Dataset_Overview = data.frame(Dataset = c(dataset_name_str),Variables = length(names(data)),Nominals = length(x = which(lapply(data, is.factor) == TRUE)), Observations = nrow(data))
		
		#BSkyFormat(BSky_Dataset_Overview, singleTableOutputHeader=c("Dataset Overview"))
		table_list = list(BSky_Dataset_Overview)
		table_list_names = c("Dataset Overview")
		names(table_list) = table_list_names
		
		selected_var_list = paste(paste(vars,"=",dataset_name_str,"$",vars, sep=""), sep="", collapse=",")
		#print(selected_var_list)
		
		#BSky_Dataset_Overview = data.frame(Dataset = c(dataset_name_str),Variables = length(names(data)),Nominals = length(x = which(lapply(data, is.factor) == TRUE)), Observations = nrow(data))
		BSky_Factor_Variable_List = eval(parse(text= paste("list(",selected_var_list,")", sep="")))
		#print(BSky_Factor_Variable_List)
		
		for (i in 1:length(BSky_Factor_Variable_List))
		{
			 if(!is.factor(BSky_Factor_Variable_List[[i]]))
			 {
				m1 = paste(", '",names(BSky_Factor_Variable_List)[[i]],"'",sep='')
				m2=paste(m2, m1,sep='')
				isAnyNonFactor=TRUE
			 }
		}
		
		if(isAnyNonFactor)
		{
			message=paste('\nCannot run factor variable analysis on',m2, ' ( non-factor variable(s) ).', sep='')
		}
		
		if(!isAnyNonFactor)
		{
			#BSky_Factor_Variable_List = list({{subsetvars}})

			BSky_Summary_By_Variable_df = data.frame(t(rep(c(""), 4)), stringsAsFactors = FALSE)
			names(BSky_Summary_By_Variable_df) = c("Observations", "Non NAs", "NAs", "Factor Levels")
			rowNamesOfBSky_Summary_By_Variable_df = c("dummy")

			#maxrowsOfBSky_Factor_By_Variable_df = max(as.numeric(lapply({{%DATASET%}}, function(x) if(is.factor(x)) length(levels(x)) else 0)))
			maxrowsOfBSky_Factor_By_Variable_df = max(as.numeric(lapply(data, function(x) if(is.factor(x)) length(levels(x)) else 0)))

			if(ChkboxShowOnlyTopFewFactors == TRUE && as.numeric(txtNumTopFactorsToShow) < maxrowsOfBSky_Factor_By_Variable_df)
			{
				maxrowsOfBSky_Factor_By_Variable_df = as.numeric(txtNumTopFactorsToShow)
			}

			BSky_Factor_By_Variable_df =data.frame(dummy = rep(c(""), maxrowsOfBSky_Factor_By_Variable_df), stringsAsFactors = FALSE)

			colNamesOfBSky_Factor_By_Variable_df = c("dummy")

			for (i in 1:length(BSky_Factor_Variable_List))
			{
				 if(is.factor(BSky_Factor_Variable_List[[i]]))
				 {
					  #BSky_Summary_By_Variable_df = rbind(BSky_Summary_By_Variable_df,c(Observations=nrow({{%DATASET%}}), NonNAs = sum(!is.na(BSky_Factor_Variable_List[[i]])), NAs = sum(is.na(BSky_Factor_Variable_List[[i]])), FactorLevels = length(levels(BSky_Factor_Variable_List[[i]]))))
					  BSky_Summary_By_Variable_df = rbind(BSky_Summary_By_Variable_df,c(Observations=nrow(data), NonNAs = sum(!is.na(BSky_Factor_Variable_List[[i]])), NAs = sum(is.na(BSky_Factor_Variable_List[[i]])), FactorLevels = length(levels(BSky_Factor_Variable_List[[i]]))))
					 
					  rowNamesOfBSky_Summary_By_Variable_df = c(rowNamesOfBSky_Summary_By_Variable_df, names(BSky_Factor_Variable_List)[i])
					  
					  temp_BSky_Factor_By_Variable_df = data.frame(table(t(BSky_Factor_Variable_List[[i]])), stringsAsFactors = FALSE)

					  if(ChkboxShowOnlyTopFewFactors == TRUE) 
					  {
						   temp_BSky_Factor_By_Variable_df = temp_BSky_Factor_By_Variable_df[order(-temp_BSky_Factor_By_Variable_df[,2]),] 
						   temp_BSky_Factor_By_Variable_df = head(temp_BSky_Factor_By_Variable_df, txtNumTopFactorsToShow) 
						   row.names(temp_BSky_Factor_By_Variable_df) = NULL
					  }

					  temp_BSky_Factor_By_Variable_df[] = lapply(temp_BSky_Factor_By_Variable_df, as.character)
		  
					  if(nrow(temp_BSky_Factor_By_Variable_df) < maxrowsOfBSky_Factor_By_Variable_df)
					  {
						   for(j in 1:(maxrowsOfBSky_Factor_By_Variable_df - nrow(temp_BSky_Factor_By_Variable_df)))
						   {
							temp_BSky_Factor_By_Variable_df = rbind(temp_BSky_Factor_By_Variable_df, c("",""))
						   }
					  }
		  
					  BSky_Factor_By_Variable_df = cbind(BSky_Factor_By_Variable_df, temp_BSky_Factor_By_Variable_df)
		  
					  colNamesOfBSky_Factor_By_Variable_df = c(colNamesOfBSky_Factor_By_Variable_df, names(BSky_Factor_Variable_List)[i], "Count")
				}
			}

			row.names(BSky_Summary_By_Variable_df) = rowNamesOfBSky_Summary_By_Variable_df
			BSky_Summary_By_Variable_df = BSky_Summary_By_Variable_df[-1,]
			BSky_Factor_By_Variable_df = BSky_Factor_By_Variable_df[,-1]
			names(BSky_Factor_By_Variable_df) = colNamesOfBSky_Factor_By_Variable_df[-1]
	
			#BSkyFormat(BSky_Summary_By_Variable_df, singleTableOutputHeader=c("Nominal Variable Summary"))
			table_list = c(table_list, list(BSky_Summary_By_Variable_df))
			table_list_names = c(table_list_names, "Nominal Variable Summary")
			names(table_list) = table_list_names

			if(ChkboxShowOnlyTopFewFactors == TRUE)
			{
			   msg1 = paste("Maximum of", txtNumTopFactorsToShow, "Factors")
			   #BSkyFormat(BSky_Factor_By_Variable_df, singleTableOutputHeader=c("Maximum of {{txtNumTopFactorsToShow}} Factors"))
			   #BSkyFormat(BSky_Factor_By_Variable_df, singleTableOutputHeader=c(msg1))
			   table_list = c(table_list, list(BSky_Factor_By_Variable_df))
			   table_list_names = c(table_list_names, msg1)
			   names(table_list) = table_list_names
			}
			else
			{
			   #BSkyFormat(BSky_Factor_By_Variable_df, singleTableOutputHeader=c("All Factors"))
			   table_list = c(table_list, list(BSky_Factor_By_Variable_df))
			   table_list_names = c(table_list_names, "All Factors")
			   names(table_list) = table_list_names
			}

			#remove(BSky_Factor_Variable_List)
			#remove(BSky_Dataset_Overview)
			#remove(BSky_Summary_By_Variable_df)
			#remove(temp_BSky_Factor_By_Variable_df)
			#remove(BSky_Factor_By_Variable_df)
		}
		cat(message)
		
		if(BSkyIsRmarkdownOutputOn() == TRUE)
		{
			return((table_list))
		}
		else
		{
			return(invisible(table_list))
		}
}



