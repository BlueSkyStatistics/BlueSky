BSkyDOESetupFactorDataGrid <- function(num_factors = 5, num_factor_levels = 2, factor_levels = c(1,-1), autofil_levels = TRUE)
{
	factor_names = paste("F", c(1:num_factors), sep="")
	
	if(num_factor_levels <= 2)
	{
		num_factor_levels = 2
	}
	
	factor_data_frame = data.frame(Factors = factor_names)
	
	
	for(i in 1:num_factor_levels)
	{
		if(i <= length(factor_levels) && autofil_levels == TRUE)
		{
			factor_data_frame = cbind(factor_data_frame, rep(factor_levels[i], num_factors))
		}
		else
		{
			factor_data_frame = cbind(factor_data_frame, rep(c(" "), num_factors))
		}
	}
	
	factor_data_frame = as.data.frame(t(factor_data_frame[,c(2:ncol(factor_data_frame))]))
	names(factor_data_frame) = factor_names
	row.names(factor_data_frame) = c()
	
	#others_col_names = paste("Factor_Level_", c(1: num_factor_levels), sep="")
	#names(factor_data_frame) = c("Factors", others_col_names)
	
	attr(factor_data_frame, "BSkyDOEFactorGrid") = TRUE
	
	return(factor_data_frame)
}  

BSkyDOECreateFactorListParam <- function(factor_dataframe= NULL, factor_names = NULL, num_factors = 1, max_num_factor_levels = 2, variable_num_of_levels = TRUE)
{
	return_val = list()
	
	if(!is.null(factor_dataframe) && !is.na(factor_dataframe) && (class(factor_dataframe) %in% c("data.frame", "matrix")))
	{
		if(!is.null(factor_names) && length(factor_names) > 0)
		{
			selected_col_names = factor_names[factor_names %in% names(factor_dataframe)]
			factor_dataframe = as.data.frame(factor_dataframe[,selected_col_names])
			# print(selected_col_names)
			# BSkyFormat(factor_dataframe)
			names(factor_dataframe) = selected_col_names
			max_num_factors = dim(factor_dataframe)[2]
		}
		
		factor_dataframe = as.data.frame(t(factor_dataframe))
		factor_dataframe = cbind(dimnames(factor_dataframe)[[1]], factor_dataframe)
	
		if(is.null(factor_names) || length(factor_names) ==0)
		{
			if(num_factors == 0)
			{
				num_factors = dim(factor_dataframe)[1]
			}
			
			max_num_factors = num_factors
			if(dim(factor_dataframe)[1] < num_factors)
			{
				max_num_factors = dim(factor_dataframe)[1]
			}
		}
		
		max_factor_levels = max_num_factor_levels
		
		if(max_num_factor_levels > (dim(factor_dataframe)[2] -1))
		{
			max_factor_levels = dim(factor_dataframe)[2] - 1
		}
		
		max_factor_levels = max_factor_levels + 1
	}
	
	# BSkyFormat(factor_dataframe)
	# print(dim(factor_dataframe))
	# print(max_num_factors)
	# print(max_factor_levels)
	
	#factor.names=list( A=c(1,2,3),B=c(1,"", 2),C=c(1,2,3,4),D=c("",1) )
	
	factor_dataframe[is.na(factor_dataframe)]= c("")
	
	factorlist_and_levels_str = c("")
	level_count_vector = c()
	
	for(i in 1:max_num_factors)
	{
		if(i > 1)
		{
			factorlist_and_levels_str = paste(factorlist_and_levels_str, ", ", sep="")
		}
		
		factorlist_and_levels_str = paste(factorlist_and_levels_str, paste(factor_dataframe[i,1], "="))
		
		factorlist_and_levels_str = paste(factorlist_and_levels_str, "c(", sep="")
		
		#cat("\n", factorlist_and_levels_str, "\n")
		
		levels_str =c("") 
		level_count = 0
		
		for(j in 2:max_factor_levels)
		{	
			if(j == 2 & trimws(as.character(factor_dataframe[i,j])) == "")
			{
				if(max_factor_levels == (j+1) || (max_factor_levels > j+1 && all((trimws(as.character(factor_dataframe[i,(j+2):max_factor_levels]))) == "") == TRUE))
				{
					levels_str = paste(levels_str, "\"","1","\"", sep="")
				}
				else if(all((trimws(as.character(factor_dataframe[i,j:max_factor_levels]))) == "") == TRUE)
				{
					levels_str = paste(levels_str, "\"","1","\"", sep="") 
				}
				else
				{
					levels_str = paste(levels_str, "\"", factor_dataframe[i,j], "\"", sep="")
				}
			}
			else if(j == 3 & trimws(as.character(factor_dataframe[i,j])) == "")
			{
				if(all((trimws(as.character(factor_dataframe[i,j:max_factor_levels]))) == "") == TRUE)
				{
					levels_str = paste(levels_str, "\"","-1","\"", sep="") 
				}
				else
				{
					levels_str = paste(levels_str, "\"", factor_dataframe[i,j], "\"", sep="")
				}
			}
			else if(trimws(as.character(factor_dataframe[i,j])) == "")
			{
				if(all((trimws(as.character(factor_dataframe[i,j:max_factor_levels]))) == "") == TRUE)
				{
					# to remove the left over extra comma in the string 
					levels_str = substr(levels_str, 1, nchar(levels_str)-2)
					break
				}
				else
				{
					levels_str = paste(levels_str, "\"", factor_dataframe[i,j], "\"", sep="")
				}
			}
			else
			{
				levels_str = paste(levels_str, "\"",factor_dataframe[i,j],"\"", sep="")
			}
			
			if(j < max_factor_levels)
			{
				levels_str = paste(levels_str, ", ", sep="")
			}
			
			level_count = level_count + 1
		}

		level_count_vector = c(level_count_vector, level_count)
		
		factorlist_and_levels_str = paste(factorlist_and_levels_str, levels_str, ")", sep="")
		
		#cat("\n factorlist_and_levels_str\n")
	}
	
	factorlist_and_levels_str = paste("list( ", factorlist_and_levels_str, " )", sep="")
	
	
	return_val = list(factor.names = eval(parse(text=factorlist_and_levels_str)))
	return_val = c(return_val, nlevels = list(level_count_vector))
	return_val = c(return_val, nfactors = list(length(level_count_vector)))
	#cat("\n", level_count_vector, "\n")
	#cat("\n", factorlist_and_levels_str, "\n")
	#print(return_val)
	
	return(invisible(return_val))
}