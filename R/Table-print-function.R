# http://www.inside-r.org/node/219017 (htest structure definition
# xtab table type test case
#x <- data.frame(sample = paste("Sample", c(1,1,2,2,3,4), sep="."),species = c(paste("Species", c(1,1,1,2,3), sep="."),  "zero.pseudo"),count = c(1,2,10,3,4,0))
#x
#x0 <- xtabs(~ sample + species, x)
#class(x0)
#[1] "xtabs" "table" 
# x = cbind(x, Gender = c("Male","Female", "Female", "Female", "Male","Female"))
# 
# xx = cbind(x, Store = c("store2","store1", "store1", "store2", "store2","store1"))
# x0 <- xtabs(~ sample + species + Gender, x)
# x0 <- xtabs(~ sample + Store+ species + Gender, x)
# BSkyTableObjectFormat(x0)

#product = rep(c("A1","B2", "C3"), 10)
#location = rep(c("x","y", "z","w","u"), 6)
#gender = rep(c("male","female","female"),10)
#contact = rep(c("contact-N", "contact-Y","contact-N", "contact-Maybe","contact-N", "contact-Y"),5)
#store = rep(c("store1","store2","store1"),10)
#table(store,contact,gender)
#table(store,contact,gender,product)
# BSkyTableObjectFormat(table(store,contact,gender))

#ftable(store,contact,gender,product)
#BSkyTableObjectFormat(ftable(store,contact,gender,product))
#a1 = as.data.frame(ftable(store,contact,gender,product))

# Dataset is the Satisf.sav
# Dataset <- read.spss("C:/Users/bskyuser1/Documents/Workfolder/Example data files/satisf.sav", use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)
#summary(Dataset)
#BSkyTableObjectFormat(summary(Dataset))
#table(Dataset$store,Dataset$contact, Dataset$gender)
#BSkyTableObjectFormat(table(Dataset$store,Dataset$contact, Dataset$gender))
#ftable(Dataset$store,Dataset$contact, Dataset$gender)
#BSkyTableObjectFormat(ftable(Dataset$store,Dataset$contact, Dataset$gender))
#xtabs(~store+contact+gender+overall, Dataset)
#BSkyTableObjectFormat(xtabs(~store+contact+gender+overall, Dataset))
#table(Dataset$store,Dataset$contact,Dataset$gender,Dataset$overall)
#BSkyTableObjectFormat(table(Dataset$store,Dataset$contact,Dataset$gender,Dataset$overall))
#ftable(Dataset$overall, Dataset$gender, Dataset$store, Dataset$contact)
#BSkyTableObjectFormat(ftable(Dataset$overall, Dataset$gender, Dataset$store, Dataset$contact))

#a=BSkyTableObjectFormat(ftable(Dataset$overall, Dataset$gender, Dataset$store, Dataset$contact),1,0) 
#a=BSkyTableObjectFormat(table(Dataset$store,Dataset$contact,Dataset$gender,Dataset$overall),1,0) 
#a=BSkyTableObjectFormat(xtabs(~store+contact+gender+overall, Dataset),1,1) 

#xtabs(formula = ~., data = parent.frame(), subset, sparse = FALSE, na.action, exclude = c(NA, NaN), drop.unused.levels = FALSE)

#a=numSummary(Dataset2[,c("accel", "engine", "horse", "mpg", "weight")],statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1))
#a$table #Matrix
#dim(a$table)
#dimnames(a$table)

#BSkyFormat(BSkyTableObjectFormat(a))
#BSkyFormat(BSkyTableObjectFormat(xtabs(~store+contact+gender+overall, Dataset1,drop.unused.levels = TRUE),0,0))
#BSkyFormat(BSkyListObjectFormat(summary(xtabs(~store+contact+gender+overall, Dataset1,drop.unused.levels = TRUE))))

#BSkyFormat(BSkyTableObjectFormat(xtabs(~store+contact+gender+overall, Dataset1,drop.unused.levels = TRUE),0,0))
#BSkyFormat(BSkyListObjectFormat(summary(xtabs(~store+contact+gender+overall, Dataset1,drop.unused.levels = TRUE))))
#a=summary(xtabs(~store+contact+gender+overall, Dataset1,drop.unused.levels = TRUE))
#print(a)
#BSkyFormat(BSkyListObjectFormat(a))
#y=BSkyListObjectFormat(a)
#BSkyFormat(y)
#BSkyFormat(BSkyTableObjectFormat(summary(Dataset2)))

# changed on 08/Mar/2021
##########################################################################################################
# Sanjay edit on 01/18/21.
# R 4.0.x changed the class definition of Matrix object from returning just a "matrix" 
# to ("matrix", "array") to reflect that matrix is a two dimensional array to inherit from the array class
# Because of the above change in base R, throughout this file all if(length(class(obj) == 1) needs to be changed 
# to if(length(class(obj) > 0). The other change needed is convert all if(class(obj) == "class") to if(class(obj)[1] == "class") 
# to handle class(obj) calls where it returns more than one value like class(matrix_obj) will return ("matrix", "array")
###########################################################################################################

BSkyListObjectFormat <- function(myList, ftable_change_variable_order = TRUE, sublist_length = 3, remove_rows_with_zero_count = FALSE, no_row_column_headers = FALSE)
{

	# SK 03/08/21 Add condition to handle the class/typeof "logical" which was missing in BSkyListObjectFormat()

	master_data_frame = NULL
	master_list = list()
	longest_row_length = 0
	abort_manual_list_traversing = FALSE 
	table_list = list()
	table_names = c()
	final_table_list = list()
	
	if(typeof(myList)!="list" || length(myList) == 0)
	{
		print("Not a List type object\n")
		return(NULL)
	}
	
	# the unlist() can be used to recursively flatten a list of list to a character vector
	# However, language/formaula elements will not convert to character vector
	# a = unlist(myList) 
	
	#toplevel_list_names = names(myList)
	element_count = 1
	
	top_list_names = list()
	
	for(i in 1:length(myList))
	{
		if(is.null(names(myList[i])) || (names(myList[i]) == "") )
		{
			top_list_names = c(top_list_names, paste("unnamed_element", element_count, sep = ""))
			element_count = element_count+1
		}
		else
		{
			top_list_names = c(top_list_names, names(myList[i]))
		}
	}
	names(myList) = top_list_names
	toplevel_list_names = names(myList)
	
	
	#if(length(toplevel_list_names) == 0)
	#{
	#	top_list_names = list()
	#	
	#	for(element_count in 1:length(myList))
	#	{
	#		top_list_names = c(top_list_names, paste("result_element", element_count, sep = ""))
	#	}
	#	names(myList) = top_list_names
	#	toplevel_list_names = names(myList)
	#}
	
	# SK 01/20/21
	#cat("\n 1. printing the object\n")
	#print(myList)
	
	for (name in 1:length(names(myList))) 
	{
		# SK 01/20/21
		#print("loop counter ===============")
		#print(name)
		#print(toplevel_list_names[name])
		#print(length(myList[[name]]))
		#print(class(myList[[name]]))
		#print(typeof(myList[[name]]))
		#print("+++++++++++++++++++")
		
		if(length(myList[[name]])> sublist_length && (class(myList[[name]])[1] == "array" || class(myList[[name]])[1] == "numeric" || class(myList[[name]])[1] == "integer" || class(myList[[name]])[1] == "character" || class(myList[[name]])[1] == "logical"))
		{
			if(class(myList[[name]])[1] == "array" )
			{
				table_list = c(table_list, list(as.data.frame(myList[[name]])))
			}
			else
			{
				table_list = c(table_list, list(t(as.data.frame(myList[[name]]))))
			}
			
			row.names (table_list[[length(table_list)]]) = NULL

			table_names = c(table_names, paste(toplevel_list_names[name], sep=""))
		}
		#=============
		else if((length(class(myList[[name]]))> 0) &&  class(myList[[name]])[1]== "matrix" )
		{
			table_list = c(table_list, list(BSkyMatrixObjectFormat(myList[[name]], ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)))
			table_names = c(table_names, paste(toplevel_list_names[name], sep=""))
		}
		else if((length(class(myList[[name]]))> 0) &&  class(myList[[name]])[1]== "data.frame" )
		{
			#table_list = c(table_list, list(myList[[name]]))
			#table_names = c(table_names, paste(toplevel_list_names[name], sep=""))
			table_list = c(table_list, list(BSkyDataFrameObjectFormat(myList[[name]], ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)))
			table_names = c(table_names, paste(toplevel_list_names[name], sep=""))
			
			# SK 01/20/21
			#cat("\n 2. printing the object\n")
			#print(table_names)
			#print(table_list)
		}
		else if((length(class(myList[[name]]))> 0) &&  class(myList[[name]])[1] == "by" )
		{
			table_list = c(table_list, list(BSkyByObjectFormat(myList[[name]], ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)))
			table_names = c(table_names, paste(toplevel_list_names[name], sep=""))
		}
		else if((length(class(myList[[name]]))> 0) && (class(myList[[name]])[1] == "numSummary" || class(myList[[name]])[1]=="table" || class(myList[[name]])[1]=="ftable"))
		{
			#print ("I am here - 1")
			#print(table_list)
			
			table_list = c(table_list, list(BSkyTableObjectFormat(myList[[name]], ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)))
			table_names = c(table_names, paste(toplevel_list_names[name], sep=""))
			
			#print(table_list)
		}
		else if((length(class(myList[[name]]))> 1) && (class(myList[[name]]))[1]=="xtabs")
		{
			table_list = c(table_list, list(BSkyTableObjectFormat(myList[[name]], ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)))
			table_names = c(table_names, paste(toplevel_list_names[name], sep=""))
		}
		
		#==============
		#else if(class(myList[[name]]) == "matrix" || class(myList[[name]]) == "data.frame")
		#{
		#	if(class(myList[[name]]) == "matrix")
		#	{
		#		table_list = c(table_list, list(BSkyMatrixObjectFormat(myList[[name]]))) # SK Change
		#	}
		#	else
		#	{
		#		table_list = c(table_list, list(myList[[name]]))
		#	}
		#	table_names = c(table_names, paste(toplevel_list_names[name], sep="")) #SK change
		#}
		#else if(typeof(myList[[name]]) != "list" && class(myList[[name]]) != "matrix" && class(myList[[name]]) != "data.frame")
		else if(typeof(myList[[name]]) != "list")
		{
			row = c()
			row = c(row, toplevel_list_names[name])
			
			subnames = names(myList[[name]])
			if(length(subnames)>0)
			{
				for(j in 1:length(subnames))
				{
					row = c(row, paste(subnames[j], sep=""))  #SK change
					row = c(row, paste(myList[[name]][j], sep="")) #SK change
				}
			}
			else
			{
				row = c(row, paste(myList[[name]], sep="")) #SK change
			}
			#cat(row)
			#cat("\n")
			#print(row)
			master_list = c(master_list, list(row))
			
			if(length(row)> longest_row_length)
			{
				longest_row_length = length(row)
			}
		}
		else if(typeof(myList[[name]]) == "list" && length(myList[[name]]) > 0)
		{
			row = c()
			
			subnames = names(myList[[name]])
			
			second_level_list_names = list()
			
			for(j in 1:length(myList[[name]]))
			{
				if(is.null(subnames) || (subnames[j] == "") )
				{
					second_level_list_names = c(second_level_list_names, paste("unnamed_element", element_count , sep = ""))
					element_count = element_count+1
				}
				else
				{
					second_level_list_names = c(second_level_list_names, subnames[j])
				}
			}
			names(myList[[name]]) = second_level_list_names
			subnames = names(myList[[name]])
			
			#if(length(subnames) == 0)
			#{
			#	second_level_list_names = list()
			#	
			#	for(j in 1:length(myList[[name]]))
			#	{
			#		second_level_list_names = c(second_level_list_names, paste("result_element", element_count+j , sep = ""))
			#	}
			#	element_count = element_count+j
			#	
			#	names(myList[[name]]) = second_level_list_names
			#	subnames = names(myList[[name]])
			#}
			
			
			if(length(subnames)>0)
			{
				for(j in 1:length(subnames))
				{
					if(length(myList[[name]][[subnames[j]]])> sublist_length && (class((myList[[name]][[subnames[j]]]))[1] == "array" || class((myList[[name]][[subnames[j]]]))[1] == "numeric" || class((myList[[name]][[subnames[j]]]))[1] == "integer" || class((myList[[name]][[subnames[j]]]))[1] == "character" || class((myList[[name]][[subnames[j]]]))[1] == "logical"))
					{
						if(class(myList[[name]][[subnames[j]]])[1] == "array" )
						{
							table_list = c(table_list, list(as.data.frame(myList[[name]][[subnames[j]]])))
						}
						else
						{
							table_list = c(table_list, list(t(as.data.frame(myList[[name]][[subnames[j]]]))))
						}
						
						row.names (table_list[[length(table_list)]]) = NULL

						#table_names = c(table_names, paste(toplevel_list_names[name], sep=""))
						table_names = c(table_names, paste(toplevel_list_names[name], "_", subnames[j], sep=""))
					}
					#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
					else if((length(class(myList[[name]][[subnames[j]]]))> 0) &&  class(myList[[name]][[subnames[j]]])[1] == "matrix" )
					{
						table_list = c(table_list, list(BSkyMatrixObjectFormat(myList[[name]][[subnames[j]]], ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)))
						table_names = c(table_names, paste(toplevel_list_names[name], "_", subnames[j], sep=""))
					}
					else if((length(class(myList[[name]][[subnames[j]]]))> 0) &&  class(myList[[name]][[subnames[j]]])[1] == "data.frame" )
					{
						#table_list = c(table_list, list(myList[[name]][[subnames[j]]]))
						#table_names = c(table_names, paste(toplevel_list_names[name], "_", subnames[j], sep=""))
						table_list = c(table_list, list(BSkyDataFrameObjectFormat(myList[[name]][[subnames[j]]], ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)))
						table_names = c(table_names, paste(toplevel_list_names[name], "_", subnames[j], sep=""))
					}
					else if((length(class(myList[[name]]))> 0) &&  class(myList[[name]])[1] == "by" )
					{
						table_list = c(table_list, list(BSkyByObjectFormat(myList[[name]], ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)))
						table_names = c(table_names, paste(toplevel_list_names[name], sep=""))
					}
					else if((length(class(myList[[name]][[subnames[j]]]))> 0) && (class(myList[[name]][[subnames[j]]])[1] == "numSummary" || class(myList[[name]][[subnames[j]]])[1]=="table" || class(myList[[name]][[subnames[j]]])[1]=="ftable"))
					{
						#print ("I am here - 2")
						#print(table_list)
						
						table_list = c(table_list, list(BSkyTableObjectFormat(myList[[name]][[subnames[j]]], ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)))
						table_names = c(table_names, paste(toplevel_list_names[name], "_", subnames[j], sep=""))
						
						#print(table_list)
					}
					else if((length(class(myList[[name]][[subnames[j]]]))> 1) && (class(myList[[name]][[subnames[j]]]))[1]=="xtabs")
					{
						table_list = c(table_list, list(BSkyTableObjectFormat(myList[[name]][[subnames[j]]], ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)))
						table_names = c(table_names, paste(toplevel_list_names[name], "_", subnames[j], sep=""))
					}
					#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
					#else if(class((myList[[name]][[subnames[j]]])) == "matrix" || class((myList[[name]][[subnames[j]]])) == "data.frame")
					#{
					#	if(class((myList[[name]][[subnames[j]]])) == "matrix")
					#	{
					#		table_list = c(table_list, list(BSkyMatrixObjectFormat(myList[[name]][[subnames[j]]])))
					#	}
					#	else
					#	{
					#		table_list = c(table_list, list(myList[[name]][[subnames[j]]]))
					#	}
					#	table_names = c(table_names, paste(toplevel_list_names[name], "_", subnames[j], sep=""))
					#}
					else if(typeof((myList[[name]][[subnames[j]]])) == "list")
					{
						abort_manual_list_traversing = TRUE
					}
					else
					{
						if(length(row)== 0)
						{
							row = c(row, toplevel_list_names[name])
						}
						row = c(row, paste(subnames[j], sep="")) #SK change
						row = c(row, paste(myList[[name]][j], sep="")) #SK change
					}
				}
			}
			else
			{
				if(length(row)== 0)
				{
					row = c(row, toplevel_list_names[name])
				}
				row = c(row, paste(myList[[name]], sep=""))
			}
			#cat(row)
			#cat("\n")
			#print(row)
			if(length(row) > 0)
			{
				master_list = c(master_list, list(row))
			
				if(length(row)> longest_row_length)
				{
					longest_row_length = length(row)
				}
			}
		}
	}
	#cat("Logest row length: ",longest_row_length)
	#cat("\n")
	#print(master_list)
	#cat("\n")
	
	#return(master_list)
	
	# SK 01/20/21
	#cat("\n 3. printing the object\n")
	#cat("\nlength(master_list) ",length(master_list), "\n")
	#print(table_names)
	#print(table_list)
	
	if(abort_manual_list_traversing == FALSE)
	{
		if(length(master_list)> 0)
		{
			for(i in 1: length(master_list))
			{
				if(length(master_list[[i]]) < longest_row_length)
				{
					for(j in 1:(longest_row_length - length(master_list[[i]])))
					{
						master_list[[i]] = c(master_list[[i]], "")
					}
				}
				
				#print(master_list[[i]])
				#print(length(master_list[[i]]))
				#cat("\n")
				
				if(is.null(master_data_frame))
				{
					master_data_frame = data.frame(master_list[[i]],stringsAsFactors=FALSE)
				}
				else
				{
					master_data_frame = cbind(master_data_frame, master_list[[i]])
				}
			}
			
			master_data_frame = t(master_data_frame)
			
			row.names(master_data_frame) = NULL
			names(master_data_frame) = NULL
		}
		
		if(length(table_list) > 0)
		{
			if(length(master_data_frame)>0)
			{
				final_table_list = c(final_table_list, list(master_data_frame))
				for(i in 1:length(table_list))
				{
					final_table_list = c(final_table_list, list(table_list[[i]]))
				}
				table_names = c("stat_result",table_names) #11Aug2015 changed back to stat_result   ## 'stat_result' changed to 'Overview'
			}
			else
			{
				final_table_list = table_list
			}
			names(final_table_list) = table_names
		}
		else
		{
			final_table_list = as.data.frame(master_data_frame)
		}
	}
	else
	{
		#print("at the right place list of list of list")
		element_num = 1
		list_element_names = list()
		x1 = rapply(myList, 
						function(b) 
						{ 
							#print(class(b))
							if(!is.data.frame(b) && !is.matrix(b))
							{
								#b = as.data.frame(b)
								b = as.matrix(b)
								#names(b) = ""
								#rownames(b) = NULL 
							}
							list_element_names <<- c(list_element_names, paste("result_element", element_num, sep=""))
							element_num <<- element_num + 1
							final_table_list <<- c(final_table_list,list(b))
						} 
					)
		names(final_table_list) = list_element_names
	}
	
	return(invisible(final_table_list))
}



BSkyMatrixObjectFormat <- function(x, ftable_change_variable_order = TRUE, remove_rows_with_zero_count = FALSE, no_row_column_headers = FALSE)
{
	if(length(class(x))> 0 && class(x)[1] != "matrix")
	{
		print("Not a matrix type object\n")
		return(NULL)
	}
	
	names_of_row_column_attr = names(dimnames(x))
	
	if(is.null(names_of_row_column_attr))
	{
		return(invisible(x))
	}
	
	row_col_names = dimnames(x) 
	
	#if(is.null(row_col_names) || (is.null(row_col_names[[1]]) && is.null(row_col_names[[1]])))
	#{
	#	return(invisible(x)) 
	#}
	
	if(!is.null(row_col_names))
	{
		if(!is.null(row_col_names[[2]]))
		{
			column_name_row_for_data_cell = row_col_names[[2]] 
		}
		else
		{
			column_name_row_for_data_cell = c(rep(c(" "),(dim(x))[2]))
		}
		
		column_name_row_for_data_cell = c(c(" "),column_name_row_for_data_cell)
		
		#cat("\nSK print 1 \n")
		#print(column_name_row_for_data_cell) 
		
		if(!is.null(row_col_names[[1]]))
		{
			row_name_for_data_cell = row_col_names[[1]]
		}
		else
		{
			row_name_for_data_cell = c(rep(c(" "),(dim(x))[1]))
		}
		
		#cat("\nSK print 2 \n")
		#print(row_name_for_data_cell)
		
		fullyFormatedDataFrame = data.frame(x, stringsAsFactors=FALSE)
		#cat("\nSK print 3 \n")
		#print(fullyFormatedDataFrame)
		
		fullyFormatedDataFrame = cbind(row_name_for_data_cell, fullyFormatedDataFrame, stringsAsFactors=FALSE)
		#cat("\nSK print 4 \n")
		#print(fullyFormatedDataFrame)
		
		fullyFormatedDataFrame = rbind(column_name_row_for_data_cell, fullyFormatedDataFrame)
		#cat("\nSK print 5 \n")
		#print(fullyFormatedDataFrame)
	}
	
	data_frame_name =c()
		
	if(!is.null(names_of_row_column_attr[2]))
	{
		data_frame_name = c(rep(c(names_of_row_column_attr[2]),(dim(x))[2]))
	}
	else
	{
		data_frame_name = c(rep(c(" "),(dim(x))[2]))
	}
	
	#cat("\nSK print 6 \n")
	#print(data_frame_name)
	
	if(!is.null(names_of_row_column_attr[1]))
	{
		data_frame_name = c(names_of_row_column_attr[1],data_frame_name)
	}
	else
	{
		data_frame_name = c(c(" "), data_frame_name)
	}
	
	#cat("\nSK print 7 \n")
	#print(data_frame_name)
	
	names(fullyFormatedDataFrame) = data_frame_name 
	row.names(fullyFormatedDataFrame) = NULL 
	
	#cat("\nSK print 8 \n")
	#print(fullyFormatedDataFrame)
	
	return(invisible(fullyFormatedDataFrame))
}

BSkyDataFrameObjectFormat <- function(df, ftable_change_variable_order = TRUE, remove_rows_with_zero_count = FALSE, no_row_column_headers = FALSE)
{
	col_names = names(df)
	
	class_names = sapply(df, class)
	
	if(is.na(!any(NA, match(c("data.frame", "list", "matrix"), as.character(class_names)))))
	{
		return(invisible(df))
	}
	
	final_df = data.frame()
	final_df_col_names = c()
	
	
	for(i in 1:length(col_names))
	{
		 #cat ("element : ", i, "col name and class: ", col_names[i] , class(df[[col_names[i]]]), "\n")
		 # SK 01/22/21 changed class check from [[ ]] to []
		 #if(class(df[[col_names[i]]])[1] == "matrix" || class(df[[col_names[i]]])[1] == "data.frame")
		 if(class(df[col_names[i]])[1] == "matrix" || class(df[col_names[i]])[1] == "data.frame")
		 { 
			  #final_df = do.call (cbind, df[[col_names[i]]] )
			  if(length( final_df) == 0)
			  {
				  #SK 01/22/21 changed from [[ ]] to []
				  #final_df = data.frame(df[[col_names[i]]])
				  final_df = data.frame(df[col_names[i]]) 
			  }
			  else
			  {
				 #SK 01/22/21 changed from [[ ]] to []
				 #final_df = cbind(final_df, df[[col_names[i]]])
				 final_df = cbind(final_df, df[col_names[i]])
			  }
			  
			  #SK 01/22/21 changed from [[ ]] to []
			  #if(!is.null(dimnames(df[[col_names[i]]])[2]))
			  if(!is.null(dimnames(df[col_names[i]])[2]))
			  {
				#final_df_col_names = c(final_df_col_names, as.character(sapply(dimnames(df[[col_names[i]]])[2], function(x) paste(col_names[i],".",x,sep="") )))
				#final_df_col_names = c(final_df_col_names, as.character(sapply(dimnames(df[col_names[i]])[2], function(x) paste(col_names[i],".",x,sep="") )))
				#SK 01/22/21 changed from [[ ]] to [] and following 
				final_df_col_names = c(final_df_col_names, dimnames(df[col_names[i]])[2])
			  }
			  else
			  {
				 for(j in 1:dim(df[[col_names[i]]])[2])
				 {
					final_df_col_names = c(final_df_col_names, paste(col_names[i],".",j,sep=""))
				 }
			  }

			  #cat(" final_df_col_names: ", final_df_col_names, "\n")
		 }
		 # SK 01/22/21 changed class check from [[ ]] to []
		 #else if(class(df[[col_names[i]]])[1] == "list")
		 else if(class(df[col_names[i]])[1] == "list")
		 {
			  # SK 01/22/21 - changed the calls from rbind to cbind - since cbind flattens a list without issue comapred to rbind call 
			  #temp_df = as.data.frame(do.call (rbind, df[[col_names[i]]] ))
			  temp_df = as.data.frame(do.call (cbind, df[[col_names[i]]] ))
			  
			  if(length( final_df) == 0)
			  {
				  # SK 01/22/21 - to handle a data frame with list in columns but not have NAs in column names 
				  if(length(which(names(temp_df) == "NA's"))== 0)
				  {
						final_df = temp_df
				  }
				  else
				  {
						final_df = temp_df[,-(which(names(temp_df) == "NA's"))]
				  }
			  }
			  else
			  {
					  # SK 01/22/21 - to handle a data frame with list in columns but not have NAs in column names 
					  if(length(which(names(temp_df) == "NA's"))== 0)
					  {
						final_df = cbind(final_df, temp_df)
					  }
					  else
					  {
						final_df = cbind(final_df, temp_df[,-(which(names(temp_df) == "NA's"))])
					  }
			  }
			
			  # SK 01/22/21 - to handle a data frame with list in columns but not have NAs in column names 
			  if(length(which(names(temp_df) == "NA's"))== 0)
			  {
					final_df_col_names = c(final_df_col_names, as.character(sapply(names(temp_df), function(x) paste(col_names[i],".",x,sep="") )))
			  }
			  else
			  {
					final_df_col_names = c(final_df_col_names, as.character(sapply(names(temp_df)[-(which(names(temp_df) == "NA's"))], function(x) paste(col_names[i],".",x,sep="") )))
			  }
			 #cat(" final_df_col_names: ", final_df_col_names, "\n")
		 }
		 else
		 {
			 if(length( final_df) == 0)
			 {
				 final_df = data.frame(df[col_names[i]])
			 }
			 else
			 {
				 final_df = cbind(final_df, df[col_names[i]])
			 }
	   
			 # SK 01/22/21 chnaged to fix the final column names 
			 final_df_col_names = c(final_df_col_names, col_names[i])
			 #final_df_col_names = c(final_df_col_names, names(final_df))

			 #cat(" final_df_col_names: ", final_df_col_names, "\n")
		 }
	}

	names(final_df) = final_df_col_names
	#row.names(final_df) = NULL 
	
	return(invisible(final_df))
}

BSkyByObjectFormat <- function(x, ftable_change_variable_order = TRUE, remove_rows_with_zero_count = FALSE, no_row_column_headers = FALSE)
{
	#$call - by.data.frame(data = Dataset2[, 1:length(names(Dataset2))], INDICES = list(Dataset2$origin,Dataset2$year, Dataset2$filter_.), FUN = summary)
	no_row_names = FALSE
	
	if(is.null(dim(x)))
	{
		if( (length(class(x[[1]])) > 0 && (class(x[[1]])[1] == "numeric" || class(x[[1]])[1] == "integer" || class(x[[1]])[1] == "character")) || (length(class(x[[1]])) >1 && class(x[[1]])[1] != "table"))
		{
			if(length(class(x[[1]]))>1)
			{
				retDFObj = as.data.frame(as.matrix(x[[1]]))
			}
			else
			{
				retDFObj = as.data.frame(t(x[[1]]))
			}
		}
		else
		{
			retDFObj = BSkyTableObjectFormat(x[[1]], ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)
		}
		
		#if(length(x[[1]]) == 1)
		#{
			if(length((attr(x, "call"))$data) > 1)
			{
				if(length(grep("\"", ((attr(x, "call"))$data)[3])) == 0)
				{
					dataset_var_name = paste("names(.GlobalEnv$",((attr(x, "call"))$data)[2],"[", "'", ((attr(x, "call"))$data)[3], "'", "]",")",sep="")
				}
				else
				{
					dataset_var_name = paste("names(.GlobalEnv$",((attr(x, "call"))$data)[2],"[", ((attr(x, "call"))$data)[3], "]",")",sep="")
				}
				#cat('\n1.dataset_var_name:')
				#print(dataset_var_name)

				dataset_var_name = eval(parse(text=dataset_var_name))
			}
			else
			{
				dataset_var_name =as.character((attr(x, "call"))$data)
			}
			
			function_name = deparse((attr(x, "call"))$FUN) #as.charater changed to deparse 31Jan2016
	#cat('\nFun Name')
	#print(function_name)
			#names(retDFObj) = paste(function_name,"(",dataset_var_name,")",sep="")
			#column_header = paste(function_name,"(",dataset_var_name,")",sep="")
			#column_header = rep(c(column_header),dim(retDFObj)[2])
			
			column_header = rep(c(function_name),dim(retDFObj)[2])
			names(retDFObj) = column_header
		#}
		return(invisible(retDFObj))
	}
	
	array_row_dims = sapply(x, function(a) if(!is.null(dim(a)[1]))dim(a)[1] else 0)
	max_array_row_dim = max(array_row_dims)
	max_array_row_dim_index = which.max(array_row_dims)
	max_array_col_dim = max(sapply(x, function(a) if(!is.null(dim(a)[2]))dim(a)[2] else 0))
	
	#if(is.null(dim(x[[1]])))
	if(max_array_row_dim == 0 || max_array_row_dim == 1)
	{
		array_col_dims = sapply(x, function(a) length(a))
		max_array_col_dim = max(array_col_dims)
		max_array_col_dim_index = which.max(array_col_dims)
		
		max_array_row_dim = 1
		max_array_row_dim_index = max_array_col_dim_index
		
		arr1 = array(c(""), c(1,max_array_col_dim,dim(x)))
		
		if(is.null(row.names(x[max_array_col_dim_index])) || is.null(row.names(x[[max_array_col_dim_index]])))
		{
			no_row_names = TRUE
			#row_names = paste("r",i=1:(dim(x[[1]])[1]),sep="")
			row_names = paste("\"","r",i=1,"\",",sep="", collapse="")
			
			if(is.null(names(x[[max_array_col_dim_index]])))
			{
				if(length((attr(x, "call"))$data)>1)
				{
					#dataset_var_name = paste(((attr(x, "call"))$data)[2],"[", ((attr(x, "call"))$data)[3], "]",sep="")
					#dataset_var_name = paste("names(",((attr(x, "call"))$data)[2],"[", "'",((attr(x, "call"))$data)[3], "'", "]",")",sep="")
					#dataset_var_name = paste("names(",((attr(x, "call"))$data)[2],"[",((attr(x, "call"))$data)[3], "]",")",sep="")
					#dataset_var_name = eval(parse(text=dataset_var_name))
					if(length((attr(x, "call"))$data) > 1)
					{
						if(length(grep("\"", ((attr(x, "call"))$data)[3])) == 0)
						{
							dataset_var_name = paste("names(.GlobalEnv$",((attr(x, "call"))$data)[2],"[", "'", ((attr(x, "call"))$data)[3], "'", "]",")",sep="")
						}
						else
						{
							dataset_var_name = paste("names(.GlobalEnv$",((attr(x, "call"))$data)[2],"[", ((attr(x, "call"))$data)[3], "]",")",sep="")
						}
				#cat('\n2.dataset_var_name:')
				#print(dataset_var_name)						
						dataset_var_name = eval(parse(text=dataset_var_name))
					}
				}
				else
				{
					dataset_var_name =as.character((attr(x, "call"))$data)
				}
				#cat('\n2a\n')
				dim_names_str = paste("dimnames(arr1) = list(r=", paste("c(", substr(row_names, 1, (nchar(row_names)-1)), ")", collapse=""),",","c= dataset_var_name",collapse="", sep="")
				#cat('\n2b\n')
			}
			else
			{
				dim_names_str = paste("dimnames(arr1) = list(r=", paste("c(", substr(row_names, 1, (nchar(row_names)-1)), ")", collapse=""),",","c= names(x[[max_array_col_dim_index]])",collapse="", sep="")
			}

		}
		else
		{
			if(is.null(names(x[[max_array_col_dim_index]])))
			{
				if(length((attr(x, "call"))$data) > 1)
				{
					#dataset_var_name = paste(((attr(x, "call"))$data)[2],"[", ((attr(x, "call"))$data)[3], "]",sep="")
					#dataset_var_name = paste("names(",((attr(x, "call"))$data)[2],"[", "'",((attr(x, "call"))$data)[3], "'", "]",")",sep="")
					#dataset_var_name = paste("names(",((attr(x, "call"))$data)[2],"[",((attr(x, "call"))$data)[3], "]",")",sep="")
					#dataset_var_name = eval(parse(text=dataset_var_name))
					if(length((attr(x, "call"))$data) > 1)
					{
						if(length(grep("\"", ((attr(x, "call"))$data)[3])) == 0)
						{
							dataset_var_name = paste("names(.GlobalEnv$",((attr(x, "call"))$data)[2],"[", "'", ((attr(x, "call"))$data)[3], "'", "]",")",sep="")
						}
						else
						{
							dataset_var_name = paste("names(.GlobalEnv$",((attr(x, "call"))$data)[2],"[", ((attr(x, "call"))$data)[3], "]",")",sep="")
						}
				#cat('\n3.dataset_var_name:')
				#print(dataset_var_name)						
						dataset_var_name = eval(parse(text=dataset_var_name))
					}
				}
				else
				{
					dataset_var_name = as.character((attr(x, "call"))$data)
				}
				
				dim_names_str = c("dimnames(arr1) = list(r=row.names(x[[max_array_col_dim_index]]), c= dataset_var_name", collapse="", sep="")
			}
			else
			{
				dim_names_str = c("dimnames(arr1) = list(r=row.names(x[[max_array_col_dim_index]]), c= names(x[[max_array_col_dim_index]])", collapse="", sep="")
			}
		}
		
		#print(dim_names_str)
	}
	else
	{
		#print('BSkyFormat:0')
		arr1 = array(c(""), c(max_array_row_dim, max_array_col_dim, dim(x)))
	
		#dimnames(arr1) = list(r=c("r1","r2","r3","r4","r5","r6","r7"), c= attr(x[[1]], "dimnames")[[2]],filter = attr(x, "dimnames")[[3]],year = attr(x, "dimnames")[[2]], origin = attr(x, "dimnames")[[1]] )
		#dimnames(arr1) = list(r=attr(x[[1]], "dimnames")[[1]], c= attr(x[[1]], "dimnames")[[2]],filter = attr(x, "dimnames")[[3]],year = attr(x, "dimnames")[[2]], origin = attr(x, "dimnames")[[1]] )
#Error in dimnames(arr1) = list(r = c("r1"), c = dataset_var_name, origin = attr(x,  : 
#length of 'dimnames' [2] not equal to array extent
		if(all((attr(x[[max_array_row_dim_index]], "dimnames")[[1]])== c("")))
		{
			no_row_names = TRUE
			#print('BSkyFormat:1')
			#row_names = paste("r",i=1:(dim(x[[1]])[1]),sep="")
			row_names = paste("\"","r",i=1:max_array_row_dim,"\",",sep="", collapse="")
			#print('BSkyFormat:2')
			#dim_names_str = paste("dimnames(arr1) = list(r= c(\"", paste(row_names,collapse=",\""), "\"),","c= attr(x[[1]], \"dimnames\")[[2]]",collapse="", sep="")
			dim_names_str = paste("dimnames(arr1) = list(r=", paste("c(", substr(row_names, 1, (nchar(row_names)-1)), ")", collapse=""),",","c= attr(x[[max_array_row_dim_index]], \"dimnames\")[[2]]",collapse="", sep="")
			#print('BSkyFormat:3')
		}
		else
		{
			#print('BSkyFormat:4')
			dim_names_str = c("dimnames(arr1) = list(r=attr(x[[max_array_col_dim_index]], \"dimnames\")[[1]], c= attr(x[[max_array_row_dim_index]], \"dimnames\")[[2]]")
			#print('BSkyFormat:5')
		}
	}
	#print('BSkyFormat:6')
	#layer_names = c("origin","year","filter_.")
	lnames = as.character((attr(x, "call"))$INDICES)
	
	layer_names = sapply(lnames[-1], function(x){ a = strsplit(x, '\\$'); return(if(length(a[[1]]) == 1)a[[1]][1] else a[[1]][2]) })
	#print('BSkyFormat:7')
	if(length(layer_names) == 0)
	{
		layer_names = names(dimnames(x))
	}
	
	#layer_names = layer_names
		#print('BSkyFormat:8')
	layer_dimension = dim(x)
	
	for(i in 1:length(layer_dimension))
	{
		dim_names_str = paste(dim_names_str, paste(",",layer_names[i],"=","attr(x, \"dimnames\")[[",i,"]]", sep=""))
	}
	dim_names_str = paste(dim_names_str,")")
	#print('BSkyFormat:9')
	#print(attributes(arr1))
	#print(dim_names_str)
	
	eval(parse(text=dim_names_str))
#print('BSkyFormat:10')
	array_dim_names = names(dimnames(arr1))

	function_name = deparse((attr(x, "call"))$FUN) #as.charater changed to deparse 31Jan2016
	#cat('\n2.Fun Name')
	#print(function_name)
	if(!is.null(function_name))
	{

		array_dim_names[2] = function_name
	}

	names(dimnames(arr1)) = array_dim_names
	
	#print(attributes(arr1))

	
	j = 1  # j should be iterated thru table_size
	#for(i in 1:(length(arr1)/(7*9))){ cat("I am here 0: i=",i,"\n"); if(length(x[[i]]) > 0){ cat("I am here 1\n"); for(k in 1:length(x[[i]])){ cat("I am here 2\n"); arr1[[j]] = x[[i]][k]; cat("I am here 3:K =",k,"\n"); j = j+1}}else{j = j+(7*9)}}
	
	if(is.null(dim(x[[max_array_row_dim_index]])))
	{
		table_size = length(x[[max_array_row_dim_index]])
	}
	else
	{
		table_size = max_array_row_dim * max_array_col_dim
	}

	for(i in 1:(length(arr1)/table_size))
	{ 
		if(length(x[[i]]) > 0)
		{
			if(is.null(dim(x[[i]])) || (length(x[[i]]) == table_size) )
			{
				for(k in 1:length(x[[i]]))
				{
					arr1[[j]] = x[[i]][k]
					j = j+1
				}
				
				if(k < table_size)
				{
					for(m in (k+1):table_size)
					{
						arr1[[j]] = c("")
						j = j+1
					}
				}
			}
			else
			{
				m = as.matrix(x[[i]])
				m_nrow = nrow(m)
				if(m_nrow < max_array_row_dim)
				{
					m = rep(rbind(m, rep(c(""), length(m[1,]))), (max_array_row_dim - m_nrow))
				}
				
				for(k in 1:length(m))
				{
					arr1[[j]] = m[[k]]
					j = j+1
				}
			}
		}
		else
		{
			for(k in 1:(table_size))
			{
				arr1[[j]] = ""
				j = j+1
			}
		}
	}

	#print(arr1)
	#return(invisible(arr1))
	
	#retDFObj = BSkyTableObjectFormat(ftable(arr1), ftable_change_variable_order = FALSE, remove_rows_with_zero_count, no_row_column_headers)
	retDFObj = BSkyTableObjectFormat(ftable(arr1), ftable_change_variable_order = FALSE, remove_rows_with_zero_count = FALSE, no_row_column_headers)
	#print('BSkyFormat:13')
	if(no_row_names && !remove_rows_with_zero_count)
	{
		col_names = names(retDFObj)[-c(length(layer_dimension)+1)]
		retDFObj = retDFObj[-c(length(layer_dimension)+1)]
		names(retDFObj) = col_names
	}
	#print('BSkyFormat:14')
	return(invisible(retDFObj))
}


BSkyTableObjectFormat <- function(x, ftable_change_variable_order = TRUE, remove_rows_with_zero_count = FALSE, no_row_column_headers = FALSE)
{
	fullyFormatedDataFrame = data.frame()
	
	if(length(class(x))>1)
	{
		if((class(x))[1]!="xtabs")
		{
			print("Not a xtab type object\n")
			return(NULL)
		}
	}
	else
	{
		#if(class(x) == "numSummary")
		#{
		#	x1 = as.data.frame(x$table)
		#	x1 = cbind(x1, n=t(x$n), stringsAsFactors=FALSE)
		#	x1 = cbind(x1, NAs=t(x$NAs), stringsAsFactors=FALSE)
		#	#print(x1)
		#	row.names(x1) = NULL
		#	return(invisible(x1))
		#}
		
		if((class(x)[1] != "numSummary")&&(class(x)[1]!="table") && (class(x)[1]!="ftable"))
		{
			print("Not a table or ftable type object\n")
			return(NULL)
		}
	}
	
	if((length(class(x)) > 0) && class(x)[1] == "numSummary")
	{
		if(class(x$table)[1] == "matrix")
		{
			x1 = as.data.frame(ftable(x$table))
		}
		else if(class(x$table)[1] == "array")
		{
			if(length(dimnames(x$table)) == 1)
			{
				x1 = as.data.frame(x$table)
				names(x1) = x$statistics
				fullyFormatedDataFrame = cbind(row.names(x1), x1, stringsAsFactors=FALSE)
				names(fullyFormatedDataFrame) = c(" ", names(x1))
				row.names(fullyFormatedDataFrame) = NULL
			}
			else
			{
				x1 = as.data.frame(ftable(x$table))
			}
		}
		else 
		{
			x1 = as.data.frame(t(as.data.frame(x$table)))
			fullyFormatedDataFrame = x1
			row.names(x1) = NULL 
			row.names(fullyFormatedDataFrame) = NULL
		}
	}
	else
	{
		x1 = as.data.frame(x)
	}
	
	x1_columns = names(x1)
	x1_len = length(x1_columns)
	
	x1_row_levels =c()
	x1_col1_levels = c()
	row_names = c()
	inner_most_row_names = c()
	layer_row_names = c()
	rows_to_skip = 1
	num_of_layers = 1
	repeat_layer_row_names = c()
	
	
	if(x1_len == 1)
	{
		#print("do nthing - specially for NumSummary")
	}
	
	if(x1_len == 2)
	{
		x1_col1_levels = levels(x1[[1]])
		num_of_columns = length(x1_col1_levels)
		num_of_rows = nrow(x1)/num_of_columns
		
		data_cells = matrix(x1[[2]],ncol=num_of_columns)
		#dimnames(data_cells) = list(NULL, x1_col1_levels)
		#print(data_cells)
		fullyFormatedDataFrame = data.frame(data_cells, stringsAsFactors=FALSE)
		#print(fullyFormatedDataFrame)
		#fullyFormatedDataFrame = rbind(x1_col1_levels, fullyFormatedDataFrame)
		if(length(names(dimnames(x))) > 0)
		{
			table_name = rep(names(dimnames(x)),num_of_columns)
			fullyFormatedDataFrame = rbind(x1_col1_levels, fullyFormatedDataFrame)
			names(fullyFormatedDataFrame) = table_name
		}
		else
		{
			names(fullyFormatedDataFrame) = x1_col1_levels
		}
		
		row.names(fullyFormatedDataFrame) = NULL
	}
	else if(x1_len == 3)
	{
		if(any((x1[1] == "")== FALSE))
		{
			x1_row_levels = levels(x1[[1]])
		}
		else
		{
			x1_row_levels =c()
		}
		
		x1_col1_levels = levels(x1[[2]])
		num_of_columns = length(x1_col1_levels)
		num_of_rows = nrow(x1)/num_of_columns
		
		data_cells = matrix(x1[[3]],ncol=num_of_columns)
		#dimnames(data_cells) = list(x1_row_levels, x1_col1_levels)
		
		fullyFormatedDataFrame = data.frame(data_cells, stringsAsFactors=FALSE)
		
		if((length(class(x)) > 0) && class(x)[1] == "numSummary")
		{
			if(length(x1_row_levels) > 0)
			{
				column_name_row_for_data_cell = c(c("Variable"), x1_col1_levels)
				fullyFormatedDataFrame = cbind(x1_row_levels, fullyFormatedDataFrame, stringsAsFactors=FALSE)
				#fullyFormatedDataFrame = rbind(column_name_row_for_data_cell, fullyFormatedDataFrame)
				names(fullyFormatedDataFrame) = column_name_row_for_data_cell
			}
			else
			{
				names(fullyFormatedDataFrame) = x1_col1_levels
			}
			row.names(fullyFormatedDataFrame) = NULL
		}
		else
		{
			if(length(x1_row_levels) > 0)
			{
				column_name_row_for_data_cell = c(c(" "), x1_col1_levels)
				fullyFormatedDataFrame = cbind(x1_row_levels, fullyFormatedDataFrame, stringsAsFactors=FALSE)
				fullyFormatedDataFrame = rbind(column_name_row_for_data_cell, fullyFormatedDataFrame)
				names(fullyFormatedDataFrame) = c(names(x1[1]),c(rep(names(x1[2]),length(x1_col1_levels))))
			}
			else
			{
				fullyFormatedDataFrame = rbind(x1_col1_levels, fullyFormatedDataFrame)
				# to avoid printing "Var2 as column header"
				# example: BSky_Dataset_Summary = ftable(summary(Dataset1))
				#names(fullyFormatedDataFrame) = c(rep(names(x1[2]),length(x1_col1_levels)))
				names(fullyFormatedDataFrame) = c(rep(c(" "),length(x1_col1_levels)))
			}
			row.names(fullyFormatedDataFrame) = NULL
		}
		
		#print(fullyFormatedDataFrame)
		
		#print(nrow(fullyFormatedDataFrame))
		#fullyFormatedDataFrame = rbind(x1_col1_levels, fullyFormatedDataFrame)
		#data_frame_col_names = c(x1_columns[1],rep(x1_columns[2],num_of_columns))
		#names(fullyFormatedDataFrame) = data_frame_col_names
		#row.names(fullyFormatedDataFrame) = NULL 
		#names(fullyFormatedDataFrame) = x1_col1_levels
		#print(fullyFormatedDataFrame)
		#print(nrow(fullyFormatedDataFrame))
		
		#if(length(x1_row_levels) > 0)
		#{
			#x1_row_levels = c("Row-Var", x1_row_levels)
			#print(x1_row_levels)
			#print(length(x1_row_levels))
			#print(nrow(fullyFormatedDataFrame))
			#fullyFormatedDataFrame = cbind(x1_row_levels, fullyFormatedDataFrame, stringsAsFactors=FALSE)
			#row.names(fullyFormatedDataFrame) = x1_row_levels
			#print(fullyFormatedDataFrame)
		#}
	}
	else if(x1_len > 3)
	{
		#print("I am here 1\n")
		
		if((length(class(x))> 0) &&(class(x)[1]=="ftable") && ftable_change_variable_order == TRUE)
		{
			x2 = data.frame(x1[ncol(x1)-2])
			x2 = cbind(x2,x1[ncol(x1)-1])
			for(k in (ncol(x1)-3):1)
			{
				x2 = cbind(x2, x1[k])
			}
			x2 = cbind(x2, x1[ncol(x1)])
			#print(x1)
			#print(x2)
			x1 = x2
			#print(x1)
			x1_columns = names(x1)
			x1_len = length(x1_columns)
			#print(x1_columns)
			#print("I should not be here\n")
		}
		
		#print("I am here 2\n")
		
		
		x1_row_levels = levels(x1[[1]])
		x1_col1_levels = levels(x1[[2]])
		num_of_columns = length(x1_col1_levels)
		
		#data_cells = matrix(x1[[x1_len]],ncol=num_of_columns, byrow = TRUE)
		
		num_of_rows = nrow(x1)/num_of_columns
		num_of_inner_most_row_names_repeat_count = num_of_rows/length(x1_row_levels)
		
		observations_to_reach_into_in_frq_column = length(x1_row_levels) * num_of_columns
		start_index = 1
		data_cells = c()
		
		for(i in 1:num_of_inner_most_row_names_repeat_count)
		{
			observations_to_be_picked_up_in_this_iteration = x1[[x1_len]][start_index:(start_index+ observations_to_reach_into_in_frq_column - 1)]
			start_index = start_index + observations_to_reach_into_in_frq_column
			
			if(length(data_cells) == 0)
			{
				data_cells = matrix(observations_to_be_picked_up_in_this_iteration, ncol=num_of_columns) #byrow = TRUE)
			}
			else
			{
				data_cells = rbind(data_cells, matrix(observations_to_be_picked_up_in_this_iteration, ncol=num_of_columns)) # byrow = TRUE))
			}
			
			inner_most_row_names = c(inner_most_row_names, x1_row_levels)
		}
		
		if((length(class(x))> 0) &&(class(x)[1]=="ftable") && ftable_change_variable_order == TRUE)
		{
			temp = data.frame(expand.grid(rev(attr(x, "row.vars"))), unclass(x))
			data_cells = temp[,(ncol(temp)- num_of_columns + 1):ncol(temp)]
		}

		
		#print(inner_most_row_names)
		x1[2] <- NULL # removing column hearder info from the data frame
		# original x1_len is lowered by 1 due to column drop from x1 i.e. x1[2] <- NULL
		
		num_of_layers = 1
		for(i in 2:((x1_len-1)-1))
		{
			level_names = levels(x1[[i]])
			#for(j in 1:length(level_names))
			#{
				#row_names = c(row_names,level_names[[j]])
				
				rows_to_skip = 1
				
				for(k in (i-1):1)
				{
					rows_to_skip = rows_to_skip*length(levels(x1[[k]]))
				}
				
				rows_to_skip = rows_to_skip - num_of_layers
				#num_of_layers = num_of_layers + 1
				
				#cat ("rows to skip: ",rows_to_skip)
				#cat("\n")
				
				layer_row_names = c()
				for(l in 1:length(level_names))
				{
					layer_row_names = c(layer_row_names, level_names[[l]])
					
					if(rows_to_skip >0)
					{
						for(m in 1:rows_to_skip)
						{
							layer_row_names = c(layer_row_names,"")
						}
					}
				}
				
				repeat_layer_row_names = c()
				num_of_layer_names_repeat_count = length(inner_most_row_names)/length(layer_row_names)
				for(n in 1:num_of_layer_names_repeat_count)
				{
					repeat_layer_row_names = c(repeat_layer_row_names, layer_row_names)
				}
			#}
			#print(repeat_layer_row_names)
			if(nrow(fullyFormatedDataFrame) == 0)
			{
				fullyFormatedDataFrame = data.frame(repeat_layer_row_names, stringsAsFactors=FALSE)
			}
			else
			{
				fullyFormatedDataFrame = cbind(repeat_layer_row_names, fullyFormatedDataFrame, stringsAsFactors=FALSE)
			}
		}
		
		fullyFormatedDataFrame = cbind(fullyFormatedDataFrame,inner_most_row_names, stringsAsFactors=FALSE)
		fullyFormatedDataFrame = cbind(fullyFormatedDataFrame,data_cells, stringsAsFactors=FALSE)
		
		#print(fullyFormatedDataFrame)
		
		column_name_row_for_data_cell =c()
		#print(x1_columns)
		for(j in 1:(x1_len - 3))
		{
			#column_name_row_for_data_cell = c(column_name_row_for_data_cell, paste("Layer-",j, sep=""))
			column_name_row_for_data_cell = c(column_name_row_for_data_cell, x1_columns[length(x1_columns)-j])
		}
		
		#column_name_row_for_data_cell = c(column_name_row_for_data_cell, "Row-Var")
		column_name_row_for_data_cell = c(column_name_row_for_data_cell, x1_columns[1])
		
		column_name_begin_index = length(column_name_row_for_data_cell)
		
		for(k in 1:num_of_columns)
		{
			#if(k == as.integer(num_of_columns/2))
			#{
				column_name_row_for_data_cell = c(column_name_row_for_data_cell, x1_columns[2])
			#}
			#else
			#{
			#	column_name_row_for_data_cell = c(column_name_row_for_data_cell, "")
			#}	
		}
		
		names(fullyFormatedDataFrame) = column_name_row_for_data_cell
		
		
		#for(k in 1:num_of_columns)
		#{
		#	column_name_row_for_data_cell = c(column_name_row_for_data_cell, x1_col1_levels[k])
		#}
		
		#print(column_name_row_for_data_cell)
		#print(fullyFormatedDataFrame)
		
		#fullyFormatedDataFrame = rbind(column_name_row_for_data_cell, fullyFormatedDataFrame)
		
		col_var_name = c()
		for(n in 1:column_name_begin_index)
		{
			col_var_name = c(col_var_name, "")
		}
		
		col_var_name = c(col_var_name, x1_col1_levels)
			
		fullyFormatedDataFrame = rbind(col_var_name, fullyFormatedDataFrame)
		
		#print(fullyFormatedDataFrame)
	}
	
	if (remove_rows_with_zero_count == TRUE)
	{
		data_cells = data_cells[!(apply(data_cells, 1, function(y) all(y == 0))),]
		fullyFormatedDataFrame = fullyFormatedDataFrame[!(apply(fullyFormatedDataFrame[,(dim(fullyFormatedDataFrame)[2]-dim(data_cells)[2]+1):(dim(fullyFormatedDataFrame)[2])], 1, function(y) {all(y == 0)})),]
	}
	
	if(no_row_column_headers == TRUE)
	{
		#print(x1_columns) #original column names in the data frame after converting from Table, ftable, or xtabs
		#print(data_cells)
		return(invisible(data_cells))
	}
	else
	{
		#if((length(class(x)) == 1) && class(x) == "numSummary" && x1_len <= 3)
		if((length(class(x)) > 0) && class(x)[1] == "numSummary")
		{
			if(class(x$n)[1] != "matrix")
			{
				#print("Here 1")
				#print(fullyFormatedDataFrame)
				#fullyFormatedDataFrame = cbind(fullyFormatedDataFrame, n=x$n, stringsAsFactors=FALSE)
				if(x1_len > 3)
				{
					fullyFormatedDataFrame = cbind(fullyFormatedDataFrame, n=c(c("n"),as.character(x$n)), stringsAsFactors=FALSE)
				}
				else
				{
					tempColNames = names(fullyFormatedDataFrame)
					fullyFormatedDataFrame = cbind(fullyFormatedDataFrame, n=as.character(x$n), stringsAsFactors=FALSE)
					tempColNames = c(tempColNames, "n")
					names(fullyFormatedDataFrame) = tempColNames
				}
				
				if(c("NAs") %in% names(x))
				{
					#fullyFormatedDataFrame = cbind(fullyFormatedDataFrame, NAs=x$NAs, stringsAsFactors=FALSE)
					if(x1_len > 3)
					{
						fullyFormatedDataFrame = cbind(fullyFormatedDataFrame, NAs=c(c("NAs"),as.character(x$NAs)), stringsAsFactors=FALSE)
					}
					else
					{
						tempColNames = names(fullyFormatedDataFrame)
						fullyFormatedDataFrame = cbind(fullyFormatedDataFrame, NAs=as.character(x$NAs), stringsAsFactors=FALSE)
						tempColNames = c(tempColNames, "NAs")
						names(fullyFormatedDataFrame) = tempColNames
					}
				}
			}
			else
			{
				#print("Here 2")
				#print(fullyFormatedDataFrame)
				if(x1_len > 3)
				{
					fullyFormatedDataFrame = cbind(fullyFormatedDataFrame, n=c(c("n"),as.character(t(x$n))), stringsAsFactors=FALSE)
				}
				else
				{
					tempMatrix = t(x$n)
					if(length(dimnames(tempMatrix)[[2]]) ==1)
					{
						dimnames(tempMatrix)[[2]] = "n"
					}
					row.names(tempMatrix) = NULL
					fullyFormatedDataFrame = cbind(fullyFormatedDataFrame, n=tempMatrix, stringsAsFactors=FALSE)
					tempColNames = names(fullyFormatedDataFrame)
					tempColNames[[1]] = c(" ")
					names(fullyFormatedDataFrame) = tempColNames
				}
				#fullyFormatedDataFrame = cbind(fullyFormatedDataFrame, n=as.character(t(x$n)), stringsAsFactors=FALSE)
				if(c("NAs") %in% names(x))
				{
					if(x1_len > 3)
					{
						fullyFormatedDataFrame = cbind(fullyFormatedDataFrame, NAs=c(c("NAs"),as.character(t(x$NAs))), stringsAsFactors=FALSE)
					}
					else
					{
						tempMatrix = t(x$NAs)
						if(length(dimnames(tempMatrix)[[2]]) ==1)
						{
							dimnames(tempMatrix)[[2]] = "NAs"
						}
						row.names(tempMatrix) = NULL
						fullyFormatedDataFrame = cbind(fullyFormatedDataFrame, NAs=tempMatrix, stringsAsFactors=FALSE)
						tempColNames = names(fullyFormatedDataFrame)
						tempColNames[[1]] = c(" ")
						names(fullyFormatedDataFrame) = tempColNames
					}
					#fullyFormatedDataFrame = cbind(fullyFormatedDataFrame, NAs=as.character(t(x$NAs)), stringsAsFactors=FALSE)
				}
			}
			#print("Here 3")
			#print(fullyFormatedDataFrame)
			#print(row.names(fullyFormatedDataFrame))
			#print(row.names(x$table))
			
			#row.names(fullyFormatedDataFrame) = c(c(" "),row.names(x$table))
		}
		else
		{
			row.names(fullyFormatedDataFrame) = NULL
		}
		#print(x1_columns)  #original column names in the data frame after converting from Table, ftable, or xtabs 
		#print(fullyFormatedDataFrame)
		return(invisible(fullyFormatedDataFrame))
	}
}

#======================================================================
#uadatasets.sk$holdBSkyFormatObject = NULL

#BSkyFormat2 <- function(obj, bSkyFormatAppRequest = FALSE, ftable_change_variable_order = TRUE, sublist_length = 3, remove_rows_with_zero_count = FALSE, no_row_column_headers = FALSE, decimalDigitsRounding = BSkyGetDecimalDigitSetting(), singleTableOutputHeader=c())
BSkyConvertObj <- function(obj, silentFormatting = FALSE, decimalDigitsRounding = BSkyGetDecimalDigitSetting(), engNotationSetting = BSkyGetEngNotationSetting(),singleTableOutputHeader=c(), bSkyFormatAppRequest = TRUE, bSkyReturnObj = FALSE, ftable_change_variable_order = TRUE, sublist_length = 3, remove_rows_with_zero_count = FALSE, no_row_column_headers = FALSE)
{
	# changed on 11/04/18 to introduce silentFormatting = FALSE
	return(invisible(BSkyFormat2(obj=obj, silentFormatting = silentFormatting, decimalDigitsRounding=decimalDigitsRounding, engNotationSetting=engNotationSetting, singleTableOutputHeader=singleTableOutputHeader, bSkyFormatAppRequest=bSkyFormatAppRequest,bSkyReturnObj=bSkyReturnObj, ftable_change_variable_order=ftable_change_variable_order, sublist_length = sublist_length, remove_rows_with_zero_count=remove_rows_with_zero_count, no_row_column_headers=no_row_column_headers)))
}

# changed on 09/Mar/2021 to introduce silentFormatting = FALSE
BSkyFormat2 <- function(obj, silentFormatting = FALSE, decimalDigitsRounding = BSkyGetDecimalDigitSetting(), engNotationSetting = BSkyGetEngNotationSetting(), singleTableOutputHeader=c(), bSkyFormatAppRequest = FALSE, bSkyReturnObj = TRUE, ftable_change_variable_order = TRUE, sublist_length = 3, remove_rows_with_zero_count = FALSE, no_row_column_headers = FALSE, isRound = BSkyGetRound(),coefConfInt = 0.95, isRmarkdownOutputOn = BSkyIsRmarkdownOutputOn())
{
	#SK 01/22/21 - Add the check for empty object - otherwise empty object like obj = data.frame() or list() are failing 
	if(length(obj)== 0)
	{ 
		if(silentFormatting == FALSE) #anil added one more equal sign
		{
			# print("type of Obj cannot be formatted")
			BSkyErrMsg =paste("BSkyFormat2: BSky cannot format this empty object. Length of the object is zero")
			warning("BSkyFormat2: BSky cannot format this empty object. Length of the object is zero")
		}
		return(invisible(NULL))
	}
			
	BSkyFunctionInit()
	
	#BSkySetCurrentDatasetName(dataSetNameOrIndex)
	#BSkySetCurrentDatasetName("Dataset2")
	
	list_of_tables = FALSE 
	return_table_list = list()
	
	# SK 05/20/21 
	# This is only for Data Frame, Matrix, list of Data Frames and list of Matrix obj class type as well as BSkyReturn sructure list to 
	# handle the attributes may be attached to the imput table - that should be put back into the table 
	# before returning from BSkyFormat2() 
	orig_list_of_tables_passed = NULL 
	
	#BSkyErrMsg = paste("BSkyFormat2: Error in formatting BSky object : ", "Object :", paste(obj, collapse = ","),sep="")
	#BSkyWarnMsg = paste("BSkyFormat2: Warning in formatting BSky object : ", "Object :", paste(obj, collapse = ","),sep="")
	
	BSkyErrMsg = paste("BSkyFormat2: Error in formatting BSky object : ", "Object :", paste(deparse(substitute(obj))), sep="")
	BSkyWarnMsg = paste("BSkyFormat2: Warning in formatting BSky object : ", "Object :", paste(deparse(substitute(obj))), sep="")
	
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	
	# SK 01/20/21
	#cat("\n-3. printing the object\n")
	#print(obj)

	
	#tryCatch(
	#	{
	
	#	withCallingHandlers(
	#	{
#ptm1 <- proc.time()
			
			if((length(class(obj))> 0) && class(obj)[1]=="anova" )
			{
				obj = as.data.frame(obj)
			}
			
			if((length(class(obj))> 0) && class(obj)[1]=="BSkyMessage" )## 22 Nov 2020 added for electron app.
			{
			
			}
			else if((length(class(obj))> 0) && class(obj)[1]=="array" )
			{																			
				## modify obj to look like list of list. 
				## Location [[2]] will be output template name which is same as function name.
				## newobj = modified obj;
				#-return(obj)
				
				# changed on 11/04/18 to introduce silentFormatting = FALSE
				if(silentFormatting != FALSE)
				{
					obj = NULL 
				}
			}
			else if((length(class(obj))> 0) &&  (class(obj)[1]== "numeric" || class(obj)[1]== "integer" || class(obj)[1]== "character" || class(obj)[1]== "logical"))
			{
				#obj = BSkyDataFrameObjectFormat(as.data.frame(t(obj)), ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)
				
				#obj = BSkyMatrixObjectFormat(as.matrix(t(obj)), ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)
				
				# 05Apr2021 since matrix and as.matrix does not seem to produce exact same results,
				# so in place of the line above following code has been added.
				obj = as.matrix(t(obj))

				if(class(obj)[1] != "matrix")
				{
					names_obj = names(obj)
					obj = matrix(obj, ncol = length(obj))

					if(length(names_obj) > 0)
					{
						dimnames(obj)[[2]] = names_obj
					}
				}

				obj = BSkyMatrixObjectFormat(obj, ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)
			}
			else if((length(class(obj))> 0) &&  class(obj)[1]== "data.frame" )
			{
				# SK 05/20/21 
				# This is only for Data Frame, Matrix, list of Data Frames and list of Matrix obj class type as well as BSkyReturn sructure list to 
				# handle the attributes may be attached to the imput table - that should be put back into the table 
				# before returning from BSkyFormat2() 
				orig_list_of_tables_passed = obj
				
				obj = BSkyDataFrameObjectFormat(obj, ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)
			}
			else if((length(class(obj)) > 0) &&  class(obj)[1] == "matrix" )
			{
				# SK 05/20/21 
				# This is only for Data Frame, Matrix, list of Data Frames and list of Matrix obj class type as well as BSkyReturn sructure list to 
				# handle the attributes may be attached to the imput table - that should be put back into the table 
				# before returning from BSkyFormat2() 
				orig_list_of_tables_passed = obj
				
				obj = BSkyMatrixObjectFormat(obj, ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)
			}
			else if((length(class(obj))>0) && (class(obj)[1] == "numSummary" || class(obj)[1]=="table" || class(obj)[1]=="ftable"))
			{
				obj = BSkyTableObjectFormat(obj, ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)
			}
			else if((length(class(obj))> 1) && (class(obj))[1]=="xtabs")
			{
				obj = BSkyTableObjectFormat(obj, ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)
			}
			else if((length(class(obj))> 0) && (class(obj))[1]=="by")
			{
				obj = BSkyByObjectFormat(obj, ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)
			}
			else if( (typeof(obj) == "list") || (class(obj)[1]== "list"))
			{
				# SK 01/22/21 - To supress the warning msg from obj[1] for certain object type  
				#if(obj[1] == "D:/gimage.png") ##for bsky graphic commands
				if(!is.na(suppressWarnings(as.character(obj[1]))) && (suppressWarnings(as.character(obj[1]))) == "D:/gimage.png")
				{
					#-return(obj)
				}
				#else if(length(obj) > 7 && obj[[8]]$type=="table") ## [[2]] will contain the name of BSkyfunction. which is the same as the name of the output template.
				else if(length(obj) >= 7 && (c("BSkySplit") %in% names(obj)) ) ## [[2]] will contain the name of BSkyfunction. which is the same as the name of the output template.
				{
					# SK 05/20/21 
					# This is only for Data Frame, Matrix, list of Data Frames and list of Matrix obj class type as well as BSkyReturn sructure list to 
					# handle the attributes may be attached to the imput table - that should be put back into the table 
					# before returning from BSkyFormat2() 
					orig_list_of_tables_passed = obj$tables
					
					#-return(obj)
				}
				else if(((length(class(obj))> 0) &&  class(obj)[1]== "htest" ) || (any(as.character(lapply(obj, class))== "htest")) ) #else if((length(class(obj))==1) &&  class(obj)== "htest" )
				{
					#b = c()
					#c = c()
					#d = c()
					#colCount = 0
					htest_items_in_obj_list = c()
					
					if(any(as.character(lapply(obj, class))== "htest"))
					{
						y1 = match(as.character(lapply(obj,class)), "htest")
						htest_items_in_obj_list = which(y1==1)
						
						flattened_list_without_htest_items = BSkyListObjectFormat(obj[-c(htest_items_in_obj_list)],ftable_change_variable_order, sublist_length, remove_rows_with_zero_count, no_row_column_headers)
						
						if(length(flattened_list_without_htest_items) > 0)
						{
							if(bSkyReturnObj == TRUE)
							{
								BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = flattened_list_without_htest_items)
							}
							else
							{
								return_table_list = c(return_table_list, flattened_list_without_htest_items)
							}
						}
					}
						
					if(length(htest_items_in_obj_list) > 0) 
					{
						loop_count = length(htest_items_in_obj_list) 
					}
					else 
					{
						loop_count = 1
					}
					
					orig_obj = obj
					
					for(i in 1:loop_count)
					{
						b = c()
						c = c()
						d = c()
						colCount = 0
						
						if(length(htest_items_in_obj_list) == 0)
						{
							flattened_htest_list = BSkyListObjectFormat(orig_obj,ftable_change_variable_order, sublist_length, remove_rows_with_zero_count, no_row_column_headers)
						}
						else
						{
							flattened_htest_list = BSkyListObjectFormat(orig_obj[htest_items_in_obj_list[i]][[1]],ftable_change_variable_order, sublist_length, remove_rows_with_zero_count, no_row_column_headers)
							obj = orig_obj[htest_items_in_obj_list[i]][[1]]
						}
						
						if(typeof(flattened_htest_list) == "list" && class(flattened_htest_list)[1] != "data.frame" && length(flattened_htest_list) > 1 )
						{
							a1 = as.data.frame(flattened_htest_list[[1]])
						}
						else
						{
							a1 = flattened_htest_list
						}
					
						if(length(a1[a1[[1]] == "statistic", 1]) > 0)
						{
							b = c(b, "")
							c = c(c, as.character(a1[a1[[1]] == "statistic", 2]))
							d = c(d, as.character(a1[a1[[1]] == "statistic", 3]))
							colCount = colCount + 1
						}
						
						if(length(a1[a1[[1]] == "parameter", 1]) > 0 && (a1[a1[[1]] == "parameter", 2]) != "")
						{
							b = c(b, "")
							c = c(c, as.character(a1[a1[[1]] == "parameter", 2]))
							d = c(d, as.character(a1[a1[[1]] == "parameter", 3]))
							colCount = colCount + 1
							
							if( length(a1[a1[[1]] == "parameter", 4])> 0 && (a1[a1[[1]] == "parameter", 4]) != "")
							{
								b = c(b, "")
								c = c(c, as.character(a1[a1[[1]] == "parameter", 4]))
								d = c(d, as.character(a1[a1[[1]] == "parameter", 5]))
								colCount = colCount + 1
							}
						}
						
						#commented on 28Oct2018
						# if(length(a1[a1[[1]] == "p.value", 1]) > 0)
						# {
							# b = c(b, "")
							# c = c(c, "p-value")
							# d = c(d, as.character(a1[a1[[1]] == "p.value", 2]))
							# colCount = colCount + 1
						# }
						
						#28Oct2018
						if(length(a1[a1[[1]] == "p.value", 1]) > 0)
						{
							b = c(b, "")
							c = c(c, "p-value")
							
							is_p_value_numeric = suppressWarnings(as.numeric(as.character((a1[a1[[1]] == "p.value", 2]))))
							
							if(!is.na(is_p_value_numeric))
							{
								d = c(d, as.character(a1[a1[[1]] == "p.value", 2]))
							}
							else
							{
								# SK 02/21/21 - added the following check to handle p-value for h-test to return NaN
								is_NaN_p_value = ((as.character((a1[a1[[1]] == "p.value", 2]))) == "NaN") 
								
								if(!is_NaN_p_value)
								{
									d = c(d, as.character(a1[a1[[1]] == "p.value", 3]))
								}
								else
								{
									d = c(d, "NaN")
								}
							}
							colCount = colCount + 1
						}						
						
						if(length(a1[a1[[1]] == "estimate", 1]) > 0)
						{
							b = c(b, "sample estimate")
							c = c(c, as.character(a1[a1[[1]] == "estimate",2]))
							d = c(d, as.character(a1[a1[[1]] == "estimate", 3]))
							colCount = colCount + 1
							
							# print("printing a1:")
							# print(a1)
							# print("printing done a1:")
							if(length(a1[a1[[1]] == "estimate", 4])> 0 && (a1[a1[[1]] == "estimate", 4]) != "")
							{
								b = c(b, "sample estimate")
								c = c(c, as.character(a1[a1[[1]] == "estimate",4]))
								d = c(d, as.character(a1[a1[[1]] == "estimate", 5]))
								colCount = colCount + 1
							}
						}
						
						if(length(a1[a1[[1]] == "conf.int", 1]) > 0)
						{
							confInterval = attr(obj$conf.int, "conf.level")
							b = c(b, paste("confidence: ", confInterval), paste("confidence: ", confInterval))
							c = c(c, "lower", "upper")
							d = c(d, as.character(a1[a1[[1]] == "conf.int",2]), as.character(a1[a1[[1]] == "conf.int",3]))
							colCount = colCount + 1 + 1
						}
						
						if(length(as.character(a1[a1[[1]] == "null.value",1])) > 0)
						{
							if(!is.null(obj$null.value))
							{
								x = c(rep(paste("Null Value Considered: ",a1[a1[[1]] == "null.value", 3]), colCount))
							}
							else
							{
								x = c(rep(paste("Test Result"), colCount))
							}
						}
						else
						{
							#x = c(rep(paste("Test Method: ",a1[a1[[1]] == "method", 2]), colCount))
							x = c(rep(paste("Test Result"), colCount))
						}
						
						htestStatResult = as.data.frame(t(data.frame(b,c,d)))
						row.names(htestStatResult) = NULL
						names(htestStatResult) = x
# print("printing htestStatResult:")
# print(htestStatResult)
# print("printing done htestStatResult:")
#obj = htestStatResult
# print("printing addlComments:")

# addlComments = data.frame( c1= c(as.character(a1[a1[[1]] == "method",2])),stringsAsFactors=FALSE)
# print(addlComments)
# rowNames = c("Test Method Performed")
# print(addlComments)
# print("printing done addlComments:")							
						if(length(as.character(a1[a1[[1]] == "method",1])) > 0)
						{
							addlComments = data.frame( c1= c(as.character(a1[a1[[1]] == "method",2])),stringsAsFactors=FALSE)
							rowNames = c("Test Method Performed")
							
							if(length(as.character(a1[a1[[1]] == "alternative",1])) > 0)
							{
								addlComments = rbind(addlComments, as.character(a1[a1[[1]] == "alternative",2]))
								rowNames = c(rowNames, "Alternative")
							}
							
							if(length(as.character(a1[a1[[1]] == "alternative",1])) > 0 && length(as.character(a1[a1[[1]] == "null.value",1])) > 0)
							{
								addlComments = rbind(addlComments, as.character(a1[a1[[1]] == "null.value", 3]))
								rowNames = c(rowNames, "Null Value")
							}
							
							if(!is.null(obj$null.value))
							{
								if(length(as.character(a1[a1[[1]] == "alternative",1])) > 0)
								{
									#alternative hypothesis: true tau is greater than 0 
									if( (as.character(a1[a1[[1]] == "alternative",2])) == "two.sided")
									{
										alt_hypo_str = c("is not equal to")
									}
									else 
									{
										alt_hypo_str = paste("is",(as.character(a1[a1[[1]] == "alternative",2])), "than", sep=" ")
									}
									
									alt_hypo_str = paste("True", as.character(a1[a1[[1]] == "null.value", 2]), alt_hypo_str, as.character(a1[a1[[1]] == "null.value", 3]), sep= " ")
									addlComments = rbind(addlComments, alt_hypo_str)
									rowNames = c(rowNames, "Alternative Hypothesis")
								}
							}
							
							#commented on 28Oct2018 
							# if(length(a1[a1[[1]] == "p.value", 1]) > 0)
							# {
								# #signif(pval,2) <= 2.2e-16
								# if(signif(as.numeric(as.character((a1[a1[[1]] == "p.value", 2]))),2) <= 2.2e-16)
								# {
									# if(engNotationSetting == TRUE)
									# {
										# addlComments = rbind(addlComments, "p-value < 2.2e-16")
									# }
									# else
									# {
										# addlComments = rbind(addlComments, "0")
									# }
									# rowNames = c(rowNames, "p-value")
								# }
							# }
							
							#28Oct2018
							if(length(a1[a1[[1]] == "p.value", 1]) > 0)
							{
								is_p_value_numeric = suppressWarnings(as.numeric(as.character((a1[a1[[1]] == "p.value", 2]))))
								
								if(!is.na(is_p_value_numeric))
								{
									#signif(pval,2) <= 2.2e-16
									if(signif(as.numeric(as.character((a1[a1[[1]] == "p.value", 2]))),2) <= 2.2e-16)
									{
										if(engNotationSetting == TRUE)
										{
											addlComments = rbind(addlComments, "p-value < 2.2e-16")
										}
										else
										{
											addlComments = rbind(addlComments, "0")
										}
										
										rowNames = c(rowNames, "p-value")
									}
								}
								else
								{
									# SK 02/21/21 - added the following check to handle p-value for h-test to return NaN
									is_NaN_p_value = (((as.character((a1[a1[[1]] == "p.value", 2]))) == "NaN") || ((as.character((a1[a1[[1]] == "p.value", 3]))) == "NaN"))
									
									if(!is_NaN_p_value)
									{
										#signif(pval,2) <= 2.2e-16
										if(signif(as.numeric(as.character((a1[a1[[1]] == "p.value", 3]))),2) <= 2.2e-16)
										{
											if(engNotationSetting == TRUE)
											{
												addlComments = rbind(addlComments, "p-value < 2.2e-16")
											}
											else
											{
												addlComments = rbind(addlComments, "0")
											}
											
											rowNames = c(rowNames, "p-value")
										}
									}	
								}
							}							
							
							# SK - 05/22/21 - commenting out as RStudio HTML output does not format wll
							# Also for split iteration it prints split dataset name - does not look good 
							# if(length(as.character(a1[a1[[1]] == "data.name",1])) > 0)
							# {
								# addlComments = rbind(addlComments, as.character(a1[a1[[1]] == "data.name",2]))
								# rowNames = c(rowNames, "Data Variables Used")
							# }
							
							if(length(as.character(a1[a1[[1]] == "sample.size",1])) > 0)
							{
								addlComments = rbind(addlComments, as.character(a1[a1[[1]] == "sample.size",2]))
								rowNames = c(rowNames, "Non-missing Observations")
							}
							
							if(length(as.character(a1[a1[[1]] == "bad.obs",1])) > 0)
							{
								addlComments = rbind(addlComments, as.character(a1[a1[[1]] == "bad.obs",2]))
								rowNames = c(rowNames, "Bad Observations Removed")
							}
							# print("My msg:")
							# print(addlComments)
							# print("Row names")
							# print(rowNames)
							names(addlComments) = c("Additional Comments")
							# print("Col name assigned")
							row.names(addlComments) = rowNames
							# print("Row name assigned")
						}
						
						#obj = htestStatResult
						tablelist = list(htestStatResult)
						tablelist = c(tablelist, list(addlComments))
						names(tablelist) = c(paste(as.character(a1[a1[[1]] == "method",2])), "Additional Details")
						
						if(length(tablelist) > 0)
						{
							if(bSkyReturnObj == TRUE)
							{
								BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = tablelist)
							}
							else
							{
								return_table_list = c(return_table_list, tablelist)
							}
						}
						
						if(typeof(flattened_htest_list) == "list" && class(flattened_htest_list)[1] != "data.frame" && length(flattened_htest_list) > 1 )
						{
							if(bSkyReturnObj == TRUE)
							{
								BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = flattened_htest_list[-1])
							}
							else
							{
								return_table_list = c(return_table_list, flattened_htest_list[-1])
							}
						}
						#BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = list(htestStatResult))
						#BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = list(addlComments))
						list_of_tables = TRUE 
					}
					
					obj = orig_obj 
				}
				else if((length(class(obj))> 0) &&  (class(obj)[1]== "lm" || class(obj)[1]== "summary.lm"))
				{
					if(class(obj)[1]== "lm")
					{
						obj = summary(obj)
					}
					
					b = c()
					c = c()
					d = c()
					colCount = 0
					
					a1 = BSkyListObjectFormat(obj,ftable_change_variable_order, sublist_length, remove_rows_with_zero_count, no_row_column_headers)
					
					a1$stat_result = as.data.frame(a1$stat_result)
					
					if(length(a1$stat_result[a1$stat_result[[1]] == "sigma", 1]) > 0)
					{
						b = c(b, "") 
						c = c(c, "Residual Std. Error")
						#c = c(c, as.character(a1[a1[[1]] == "statistic", 2]))
						d = c(d, as.character(a1$stat_result[a1$stat_result[[1]] == "sigma", 2]))
						colCount = colCount + 1
					}
					
					if(length(a1$stat_result[a1$stat_result[[1]] == "df", 1]) > 0)
					{
						b = c(b, "") 
						c = c(c, "df")
						d = c(d, as.character(a1$stat_result[a1$stat_result[[1]] == "df", 3]))
						colCount = colCount + 1
					}
					
					if(length(a1$stat_result[a1$stat_result[[1]] == "r.squared", 1]) > 0)
					{
						b = c(b, "") 
						c = c(c, "R-squared")
						d = c(d, as.character(a1$stat_result[a1$stat_result[[1]] == "r.squared", 2]))
						colCount = colCount + 1
					}
					
					if(length(a1$stat_result[a1$stat_result[[1]] == "adj.r.squared", 1]) > 0)
					{
						b = c(b, "") 
						c = c(c, "Adjusted R-squared")
						d = c(d, as.character(a1$stat_result[a1$stat_result[[1]] == "adj.r.squared", 2]))
						colCount = colCount + 1
					}
					
					if(length(a1$stat_result[a1$stat_result[[1]] == "fstatistic", 1]) > 0)
					{
						#b = c(b, "F") 
						b = c(b, "")
						c = c(c, "F-statistic")
						d = c(d, as.character(a1$stat_result[a1$stat_result[[1]] == "fstatistic", 3]))
						colCount = colCount + 1
						
						if(length(a1$stat_result[a1$stat_result[[4]] == "numdf", 4]) > 0)
						{
							b = c(b, "") 
							c = c(c, "numdf")
							d = c(d, as.character(a1$stat_result[a1$stat_result[[4]] == "numdf", 5]))
							colCount = colCount + 1
						}
						
						if(length(a1$stat_result[a1$stat_result[[6]] == "dendf", 6]) > 0)
						{
							b = c(b, "") 
							c = c(c, "dendf")
							d = c(d, as.character(a1$stat_result[a1$stat_result[[6]] == "dendf", 7]))
							colCount = colCount + 1
							
							# newly added to print p-value on 06/27/15
							#f <- summary(lm.D90)$fstatistic
							#p <- pf(f[1],f[2],f[3],lower.tail=F)
							
							#################################################################################################
							# SK 06/11/21 The following suppressWarnings() added to supress the warning due to bad data from split dataset slice 
							#################################################################################################
							suppressWarnings((pval = pf(obj$fstatistic[1], obj$fstatistic[2], obj$fstatistic[3], lower.tail=F)))
							
							b = c(b, "") 
							c = c(c, "p-value")
							
							# SK - 05/22/21 - "not a NaN" check is put is place as "signif(pval,2) <= 2.2e-16" was failing for a split dataset testing 
							if(!is.nan(pval) && signif(pval,2) <= 2.2e-16)
							{
								if(engNotationSetting == TRUE)
								{
									d = c(d, c("p-value < 2.2e-16"))
								}
								else
								{
									d = c(d, c("0"))
								}
							}
							else
							{
								d = c(d, as.character(pval)) 
							}
							
							colCount = colCount + 1
						}
					}
					
					# following if then else introduced on 06/27/15
					# if(length(obj$call) == 2)
					# {
						# model_formula = paste("Model: ", obj$call[1], "(", (names(obj$call))[2], "=", paste(obj$call[2],collapse=""), ")", collapse = "")
					# }
					# else if( length(obj$call) > 2)
					# {
						# model_formula = paste("Model: ", obj$call[1], "(", (names(obj$call))[2], "=", paste(obj$call[2],collapse=""), ",", (names(obj$call))[3], "=", obj$call[[3]], ")", collapse = "")
					# }
					# else
					# {
						# model_formula =c()
					# }
					
					## 26Sep2018 Commented above if-else and using following line instead.
					model_formula = paste("Model:", paste(deparse(obj$call), collapse=""))
					
					###########################################################################
					# SK 06/11/21 The following added to replace ugly looking split dataset name 
					# to the current dataset if there is a split iteration is going on 
					############################################################################
					model_formula = BSkyReplaceSplitDatasetName(model_formula)
					
					x = c(rep("LM Summary", colCount))
					#htestStatResult = as.data.frame(t(data.frame(b,c,d)))
					htestStatResult = as.data.frame(t(data.frame(c,d)))
					row.names(htestStatResult) = NULL
					names(htestStatResult) = x
					
					tablelist = list(htestStatResult)
					names(tablelist) = c(model_formula)
					
					#################################################################################################
					# SK 06/11/21 The following added to supress the warning due to bad data from split dataset slice 
					#################################################################################################
					if(length(a1$residuals) > 0 && any(!is.na(a1$residuals)) == TRUE)
					{
						#this changed to following quant = quantile(a1$residuals, probs = seq(0,1,0.25))
						quant = quantile(a1$residuals, probs = seq(0,1,0.25), na.rm=TRUE)
						
						residual_df = data.frame(min(a1$residuals, na.rm = TRUE), quant[[2]], median(a1$residuals, na.rm = TRUE), quant[[4]], max(a1$residuals, na.rm = TRUE))
						names(residual_df) = c("Min", "1Q", "Median", "3Q", "Max")
					}
					else
					{
						residual_df = data.frame("Residual table could not be produced due to no non-NA residuals")
						names(residual_df) = c() 
					}
					
					tab_list_names = names(tablelist)
					tablelist = c(tablelist, list(residual_df))
					names(tablelist) = c(tab_list_names, "Residuals")					
					
					
					############################################################################################
					#SK 03/07/21 Additional code to print the NA rows for Factor variable for coefficients table
					############################################################################################
					
					if(any(obj$aliased == TRUE))
					{
						#processing coefficients
						a1$coefficients = ((tempTable = merge(cbind(row_id=seq(1:length(obj$aliased)),as.data.frame(obj$aliased)),a1$coefficients,by='row.names',all=TRUE,sort = FALSE))[order(tempTable$row_id),])[,-c(1,2,3)]
						row.names(a1$coefficients) = names(obj$aliased)
					}
					
					# Computing confidence interval of the coefficients on the processed coefficients with parameter coefConfInt (default 95%) 
					# Also eval(a1$call) to get back the Linera model from the LM summary object 
					#a1$coefficients <-cbind(a1$coefficients,stats::confint(eval(obj$call),level=coefConfInt,type="LR"))
					
					#################################################################################################
					# SK 06/11/21 The following suppressWarnings() added to supress the warning due to bad data from split dataset slice 
					#################################################################################################
					
					##11Oct2021 suppressWarnings is now enclosed in a 'if'
					if(as.list(obj$call)$data != ".")
					{
						suppressWarnings((a1$coefficients <-cbind(a1$coefficients,stats::confint(eval(obj$call, envir=globalenv()),level=coefConfInt,type="LR")))) 
					}
					
					tab_list_names = names(tablelist)
					tablelist = c(tablelist, list(a1$coefficients))
					
					#names(tablelist) = c(model_formula, "Residuals", "Coefficients")
					names(tablelist) = c(tab_list_names, "Coefficients")
					
					###########################################################################################
					#SK 03/07/21 Additional code to add comments to coefficients for the NA rows for Factor variable for coefficients table
					############################################################################################
					
					coeff_table_name_footer = c("")
					
					if(length(obj$na.action) > 0)
					{
						coeff_table_name_footer = "Coefficients:"
						coeff_table_name_footer = paste(coeff_table_name_footer, "\n", "(",length(obj$na.action)," observations deleted due to missing values)",sep = "")
					}
					
					if(any(obj$aliased == TRUE))
					{
						if(coeff_table_name_footer == c(""))
						{
							coeff_table_name_footer = "Coefficients:"
						}
						coeff_table_name_footer = paste(coeff_table_name_footer, "\n", "(",length(which(obj$aliased,TRUE))," not defined because of singularities)", sep = "")
					}
					
					if(coeff_table_name_footer != c(""))
					{
						coeff_footer_table = data.frame(coeff_table_name_footer)
						names(coeff_footer_table) = c() 
						tablelist = c(tablelist, list(coeff_footer_table))
					}
						
					
					if(bSkyReturnObj == TRUE)
					{
						BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = tablelist)
					}
					else
					{
						return_table_list = c(return_table_list, tablelist)
					}
					
					list_of_tables = TRUE 
				}
				#else if(((length(class(obj))==2) &&  class(obj)[1] == "glm")|| ((length(class(obj))==1) &&  class(obj) == "summary.glm"))
				#{
				#	if((length(class(obj))==2) &&  class(obj)[1] == "glm")
				#	{
				else if(((length(class(obj))>1) &&  class(obj)[1] == "glm")|| ((length(class(obj))> 0) &&  class(obj)[1] == "summary.glm"))
				{
					if((length(class(obj))> 1) &&  class(obj)[1] == "glm")
					{
						obj = summary(obj)
					}
					
					b = c()
					c = c()
					d = c()
					colCount = 0
					
					a1 = BSkyListObjectFormat(obj,ftable_change_variable_order, sublist_length, remove_rows_with_zero_count, no_row_column_headers)
					
					a1$stat_result = as.data.frame(a1$stat_result)
					
					if(length(a1$stat_result[a1$stat_result[[1]] == "null.deviance", 1]) > 0)
					{
						b = c(b, "") 
						c = c(c, "Null Deviance")
						#c = c(c, as.character(a1[a1[[1]] == "statistic", 2]))
						d = c(d, as.character(a1$stat_result[a1$stat_result[[1]] == "null.deviance", 2]))
						colCount = colCount + 1
					}
					
					if(length(a1$stat_result[a1$stat_result[[1]] == "df.null", 1]) > 0)
					{
						b = c(b, "") 
						c = c(c, "df")
						d = c(d, as.character(a1$stat_result[a1$stat_result[[1]] == "df.null", 2]))
						colCount = colCount + 1
					}
					
					if(length(a1$stat_result[a1$stat_result[[1]] == "deviance", 1]) > 0)
					{
						b = c(b, "") 
						c = c(c, "Residual Deviance")
						d = c(d, as.character(a1$stat_result[a1$stat_result[[1]] == "deviance", 2]))
						colCount = colCount + 1
					}
					
					if(length(a1$stat_result[a1$stat_result[[1]] == "df.residual", 1]) > 0)
					{
						b = c(b, "") 
						c = c(c, "df Residual")
						d = c(d, as.character(a1$stat_result[a1$stat_result[[1]] == "df.residual", 2]))
						colCount = colCount + 1
					}
					
					if(length(a1$stat_result[a1$stat_result[[1]] == "aic", 1]) > 0)
					{
						b = c(b, "") 
						c = c(c, "AIC")
						d = c(d, as.character(a1$stat_result[a1$stat_result[[1]] == "aic", 2]))
						colCount = colCount + 1
					}
					
					if(length(a1$stat_result[a1$stat_result[[1]] == "dispersion", 1]) > 0)
					{
						b = c(b, "") 
						c = c(c, "Dispersion")
						d = c(d, as.character(a1$stat_result[a1$stat_result[[1]] == "dispersion", 2]))
						colCount = colCount + 1
					}
					
					if(length(a1$stat_result[a1$stat_result[[1]] == "iter", 1]) > 0)
					{
						b = c(b, "") 
						c = c(c, "Iterations")
						d = c(d, as.character(a1$stat_result[a1$stat_result[[1]] == "iter", 2]))
						colCount = colCount + 1
					}
					
					#call_items = lapply(obj$call, function(x) x)
					# if(length(call_items) == 3)
					# {
						# model_formula = paste("Model: ", obj$call[1], "(", (names(obj$call))[2], "=", paste(obj$call[2],collapse=""), ",", (names(obj$call))[3], "=", paste(obj$call[[3]],collapse = "",sep=""), ")", collapse = "")
					# }
					# else if(length(call_items) == 4)
					# {
						# if(length(obj$call[[3]]) > 1)
						# {
						# model_formula = paste("Model: ", obj$call[1], "(", (names(obj$call))[2], "=", paste(obj$call[2],collapse=""), ",", (names(obj$call))[3], "=", paste(obj$call[[3]][1],collapse = "",sep=""), "(",paste(obj$call[[3]][2],collapse = "",sep=""),")", ",", (names(obj$call))[4], "=", paste(obj$call[[4]],collapse = "",sep=""), ")", collapse = "")
						# }
						# else
						# {
						# model_formula = paste("Model: ", obj$call[1], "(", (names(obj$call))[2], "=", paste(obj$call[2],collapse=""), ",", (names(obj$call))[3], "=", paste(obj$call[[3]],collapse = "",sep=""), ",", (names(obj$call))[4], "=", paste(obj$call[[4]],collapse = "",sep=""), ")", collapse = "")						
						# }
					# }
					
					## 26Sep2018 Commented if-else above and using line below instead.
					model_formula = paste("Model:", paste(deparse(obj$call), collapse=""))
					
					###########################################################################
					# SK 06/11/21 The following added to replace ugly looking split dataset name 
					# to the current dataset if there is a split iteration is going on 
					############################################################################
					model_formula = BSkyReplaceSplitDatasetName(model_formula)
					
					x = c(rep("GLM Summary", colCount))
					htestStatResult = as.data.frame(t(data.frame(b,c,d)))
					row.names(htestStatResult) = NULL
					names(htestStatResult) = x
					
					#################################################################################################
					# SK 06/11/21 The following suppressWarnings() added to supress the warning due to bad data from split dataset slice 
					#################################################################################################
					
					#this changed to following quant = quantile(a1$deviance.resid, probs = seq(0,1,0.25))
					suppressWarnings((quant = quantile(a1$deviance.resid, probs = seq(0,1,0.25), na.rm=TRUE)))
					
					#################################################################################################
					# SK 06/11/21 The following added to supress the warning due to bad data from split dataset slice 
					#################################################################################################
					if(length(a1$deviance.resid) > 0 && any(!is.na(a1$deviance.resid)) == TRUE)
					{
						#residual_df = data.frame(min(a1$deviance.resid), quant[[2]], median(a1$deviance.resid), mean(a1$deviance.resid), quant[[4]], max(a1$deviance.resid))
						#names(residual_df) = c("Min", "1Q", "Median", "Mean", "3Q", "Max")
						residual_df = data.frame(min(a1$deviance.resid, na.rm=TRUE), quant[[2]], median(a1$deviance.resid, na.rm=TRUE), quant[[4]], max(a1$deviance.resid, na.rm=TRUE))
						names(residual_df) = c("Min", "1Q", "Median", "3Q", "Max")
					}
					else
					{
						residual_df = data.frame("Deviance Residual Summary table could not be produced due to no non-NA residuals")
						names(residual_df) = c() 
					}
					
					tablelist = list(htestStatResult)
					tablelist = c(tablelist, list(residual_df))
					#tablelist = c(tablelist, list(a1$deviance.resid))
					#tablelist = c(tablelist, list(a1$coefficients))
					
					############################################################################################
					#SK 03/07/21 Additional code to print the NA rows for Factor variable for coefficients table
					############################################################################################
					
					if(any(obj$aliased == TRUE))
					{
						#processing coefficients
						a1$coefficients = ((tempTable = merge(cbind(row_id=seq(1:length(obj$aliased)),as.data.frame(obj$aliased)),a1$coefficients,by='row.names',all=TRUE,sort = FALSE))[order(tempTable$row_id),])[,-c(1,2,3)]
						row.names(a1$coefficients) = names(obj$aliased)
					}
					
					# No need for 95% conf calculation for GLM
					# Computing confidence interval of the coefficients on the processed coefficients with parameter coefConfInt (default 95%) 
					# Also eval(a1$call) to get back the Linera model from the LM summary object 
					
					#a1$coefficients <-cbind(a1$coefficients,stats::confint(eval(obj$call),level=coefConfInt,type="LR"))
					
					tablelist = c(tablelist, list(a1$coefficients))
					
					#names(tablelist) = c(model_formula, "Deviance Residual Summary", "Deviance Residuals", "Coefficients")
					names(tablelist) = c(model_formula, "Deviance Residual Summary", "Coefficients")
					
					###########################################################################################
					#SK 03/07/21 Additional code to add comments to coefficients for the NA rows for Factor variable for coefficients table
					############################################################################################
					
					coeff_table_name_footer = c("")
					
					if(length(obj$na.action) > 0)
					{
						coeff_table_name_footer = "Coefficients:"
						coeff_table_name_footer = paste(coeff_table_name_footer, "\n", "(",length(obj$na.action)," observations deleted due to missing values)",sep = "")
					}
					
					if(any(obj$aliased == TRUE))
					{
						if(coeff_table_name_footer == c(""))
						{
							coeff_table_name_footer = "Coefficients:"
						}
						coeff_table_name_footer = paste(coeff_table_name_footer, "\n", "(",length(which(obj$aliased,TRUE))," not defined because of singularities)", sep = "")
					}
					
					if(coeff_table_name_footer != c(""))
					{
						coeff_footer_table = data.frame(coeff_table_name_footer)
						names(coeff_footer_table) = c() 
						tablelist = c(tablelist, list(coeff_footer_table))
					}
					
					
					if(bSkyReturnObj == TRUE)
					{
						BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = tablelist)
					}
					else
					{
						return_table_list = c(return_table_list, tablelist)
					}
					
					list_of_tables = TRUE 
				}
				#else if(((length(class(obj))==2) &&  class(obj)[1] == "multinom")|| ((length(class(obj))==1) &&  (class(obj) == "polr" || class(obj) == "summary.polr" || class(obj) == "summary.multinom")))
				#{
				#	if(((length(class(obj))==2) &&  class(obj)[1] == "multinom") || (class(obj)[1] == "polr"))
				#	{
				#		#obj = summary(obj)
				#	}
				else if(((length(class(obj))> 1) &&  class(obj)[1] == "multinom")|| ((length(class(obj))> 0) &&  (class(obj)[1] == "polr" || class(obj)[1] == "summary.polr" || class(obj)[1] == "summary.multinom")))
				{
					if(((length(class(obj))> 1) &&  class(obj)[1] == "multinom") || (class(obj)[1] == "polr"))
					{
						#obj = summary(obj)
					}
					
					b = c()
					c = c()
					d = c()
					colCount = 0
					
					a1 = BSkyListObjectFormat(obj,ftable_change_variable_order, sublist_length, remove_rows_with_zero_count, no_row_column_headers)
					
					a1$stat_result = as.data.frame(a1$stat_result)
					
					
					if(length(a1$stat_result[a1$stat_result[[1]] == "deviance", 1]) > 0)
					{
						b = c(b, "") 
						c = c(c, "Residual Deviance")
						d = c(d, as.character(a1$stat_result[a1$stat_result[[1]] == "deviance", 2]))
						colCount = colCount + 1
					}
					
					if(length(a1$stat_result[a1$stat_result[[1]] == "edf", 1]) > 0)
					{
						b = c(b, "") 
						c = c(c, "Effective df")
						d = c(d, as.character(a1$stat_result[a1$stat_result[[1]] == "edf", 2]))
						colCount = colCount + 1
					}
					
					if(length(a1$stat_result[a1$stat_result[[1]] == "edf", 1]) > 0 && length(a1$stat_result[a1$stat_result[[1]] == "deviance", 1]) > 0)
					{
						b = c(b, "") 
						c = c(c, "AIC")
						d = c(d, (as.numeric(as.character(a1$stat_result[a1$stat_result[[1]] == "deviance", 2]))) + (as.numeric(as.character(a1$stat_result[a1$stat_result[[1]] == "edf", 2]))*2))
						colCount = colCount + 1
					}
					
					call_items = lapply(obj$call, function(x) x)
					
					model_formula = paste( call_items[1], "(", sep="")
					for(i in 2:length(call_items))
					{
					   items = paste(names(call_items[i]), "=", sep="")

					   if(class(call_items[[i]])[1] == "character")
					   {
						  items = paste(items, "'", paste(call_items[i], collapse=""), "'", sep="")
					   }
					   else
					   {
						  items = paste(items, paste(call_items[i], collapse=""), sep="")
					   }
					 
					   if(i < length(call_items))
					   {
						items = paste(items, ", ", sep="")
					   }
					   
					   model_formula = paste(model_formula, items, sep="")
					}
					model_formula = paste(model_formula, ")", sep="")
					
					###########################################################################
					# SK 06/11/21 The following added to replace ugly looking split dataset name 
					# to the current dataset if there is a split iteration is going on 
					############################################################################
					model_formula = BSkyReplaceSplitDatasetName(model_formula)
					
					summary_name = paste(toupper(class(obj)[1]))
					x = c(rep(summary_name, colCount))
					htestStatResult = as.data.frame(t(data.frame(b,c,d)))
					row.names(htestStatResult) = NULL
					names(htestStatResult) = x
					
					tablelist = list(htestStatResult)
					
					
					if(!is.null(a1$coefficients))
					{
						if((dim(a1$coefficients))[1] == 1)
						{
							a1$coefficients = as.data.frame(t(a1$coefficients))
							names(a1$coefficients) = "Value"
							tablelist = c(tablelist, list(a1$coefficients))
						}
						else
						{
							tablelist = c(tablelist, list(a1$coefficients))
						}
					}
					else if(!is.null(obj$coefficients))
					{
						coeff_table = as.data.frame(obj$coefficients)
						names(coeff_table) = "Value"
						tablelist = c(tablelist, list(coeff_table))
					}
					else
					{
						coeff_table = data.frame(t(rep("", length(obj$coefnames))))
						names(coeff_table)= obj$coefnames
						row.names(coeff_table)= NULL
						tablelist = c(tablelist, list(coeff_table))
					}
					
					if(length(obj$na.action) > 0)
					{
						coeff_table_name = paste("Coefficients","(",length(obj$na.action)," observations deleted due to missing values)",sep = "")
					}
					else
					{
						coeff_table_name = "Coefficients"
					}
					
					table_list_names = c(model_formula, coeff_table_name)
					
					if(!is.null(obj$standard.errors)) ##29Sep2018 for printing Std Err in Multinom
					{
						tablelist = c(tablelist, list(obj$standard.errors))
						table_list_names = c(table_list_names, "Std.Err")
					}

					if(!is.null(obj$Wald.ratios)) ##29Sep2018 for printing Value/SE in Multinom
					{
						tablelist = c(tablelist, list(obj$Wald.ratios))
						table_list_names = c(table_list_names, "Value/SE")
					}
					
					##20Sep2018 zeta is at a1 level not at the stat_result level, if needed fix it later.
					if(length(a1$stat_result[a1$stat_result[[1]] == "zeta", 1]) > 0)
					{
						intercepts = as.data.frame(obj$zeta)
						names(intercepts) = "Value"
						
						tablelist = c(tablelist, list(intercepts))
						table_list_names = c(table_list_names, "Intercepts")
					}
					
					# 29Sep2018. Ordinal regression was printing this but not plain R.
					# if(!is.null(a1$Hessian))
					# {
						# tablelist = c(tablelist, list(a1$Hessian))
						# table_list_names = c(table_list_names, "Hessian")
					# }
					
					names(tablelist) = table_list_names
					
					if(bSkyReturnObj == TRUE)
					{
						BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = tablelist)
					}
					else
					{
						return_table_list = c(return_table_list, tablelist)
					}
							
					list_of_tables = TRUE 
				}
				else
				{
					# changed on 11/04/18 to introduce silentFormatting = FALSE
					if(silentFormatting == FALSE) #anil added one more equal sign
					{
						# SK 05/20/21 
						# This is only for Data Frame, Matrix, list of Data Frames and list of Matrix obj class type as well as BSkyReturn sructure list to 
						# handle the attributes may be attached to the imput table - that should be put back into the table 
						# before returning from BSkyFormat2() 
						orig_list_of_tables_passed = obj
						
						obj = BSkyListObjectFormat(obj,ftable_change_variable_order, sublist_length, remove_rows_with_zero_count, no_row_column_headers)
						
						list_of_tables = FALSE
						
						#cat("I am here 15-1\n")
						#print(class(obj1))
						#cat("I am here 16\n")
						
						if(typeof(obj) == "list" && class(obj)[1] != "data.frame")
						{
							#cat("I am here 17\n")
							#for(i in 1:(length(obj)))
							#{	
							#	BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed=list(obj[[i]]))
							#10Nov2016 Anil want Commented because first table header was overwritten by 'Overview' sometimes
							#Ask Sanjay why did we do it in the first place. Write some code to decide when to run following line
							
							#names(obj) = c("Overview", names(obj)[-1]  ) #11Aug2015 added this line
							#SK 01/22/21 - commenting out the above line to stop chnaging the first name to overview
							# and added the following to change stat_result to overview as the first table name 
							
							if(names(obj)[1] == "stat_result")
							{
								names(obj) = c("Overview", names(obj)[-1]  ) 
							}
							
							if(bSkyReturnObj == TRUE)
							{
								BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = obj)
							}
							else
							{
								return_table_list = c(return_table_list, obj)
							}
							#return(list(obj, names(obj), uadatasets.sk$uaStatResults, names(uadatasets.sk$uaStatResults)))
							list_of_tables = TRUE
							#}
						}
					}
					else
					{
						obj = NULL 
					}
				}
				#else
				#{
				#	#print("type of Obj cannot be formatted")
				#	BSkyErrMsg =paste("BSkyFormat: BSky cannot format this object.")
				#	warning("BSkyFormat: BSky cannot format this object.")
				#	obj=""
				#	#-return(NULL)
				#}
			}
			else
			{
				# changed on 11/04/18 to introduce silentFormatting = FALSE
				if(silentFormatting == FALSE) #anil added one more equal sign
				{
					# print("type of Obj cannot be formatted")
					BSkyErrMsg =paste("BSkyFormat2: BSky cannot format this object.")
					warning("BSkyFormat2: BSky cannot format this object.")
				}
				#obj=""
				obj=NULL
				#-return(NULL)
			}
		
				
			# SK 01/20/21
			# cat("\n-2. printing the object\n")
			# print(obj)
			
			if(is.null(obj))
			{
				# changed on 11/04/18 to introduce silentFormatting = FALSE
				if(silentFormatting == FALSE) #anil added one more equal sign
				{
					# print("type of Obj cannot be formatted")
					BSkyErrMsg =paste("BSkyFormat2: BSky cannot format this object.")
					warning("BSkyFormat2: BSky cannot format this object.")
				}
				#obj=""
				obj=NULL
				#-return(NULL)
			}
		#},
		
		#warning = UAwarnHandlerFn

		#) # end of withCallingHandlers for catching warnings and continuing execution	
	
		#},
		#error = UAerrHandlerFn,
		
		#silent =TRUE		
	#)
	
		#FormatingErrorFound = FALSE
		
	    #if(BSkyLocalErrorFound() == TRUE)
    	#{
    		# if anything needs to be checked or print etc
			# Error is already handled and logged
			# Whereever the error occured execution cannot continue
			# control will be passed to the error handler function
			# and then the control will come out ogf the Try-Catch block 
			
			# cat("Error caught in BSkyFormat \n")
			#BSkyLocalErrorFlagsReset() #if needed
			#FormatingErrorFound = TRUE
    	#}
		#cat("\nWARNING:: top level BSky Foramt function\n")
		#if(BSkyLocalWarningFound() == TRUE)
    	#{
    		# if anything needs to be checked or print etc
			# All Bsky functions continue from the point where the warning occured
			# so by the time controls comes here - all warnings are already handled 
			# and execution of the code had continued
			
			# cat("Warning caught in BSkyFormat \n")
			#BSkyLocalWarningFlagsReset() #if needed to continue without returning back to the top level function 
    	#}
#cat("timing from the begining - before decimal formatting - timing 2:\n")
#print( proc.time() - ptm1)

		BSkyFunctionWrapUp()
		
		# SK 01/20/21
		# cat("\n-1. printing the object\n")
		# print(obj)
		
		if(!is.null(obj))
		{
			#if(list_of_tables == TRUE || FormatingErrorFound == TRUE)
			if(list_of_tables == TRUE)
			{
				#print(BSkyReturnStructure())
				#cat("Returning return structure from this top level BSky Foramt function\n")
				#return(BSkyReturnStructure(list(extra=list(obj))))
				#obj = BSkyReturnStructure2()
				if(bSkyReturnObj == TRUE)
				{
					obj = BSkyReturnStructure2()
				}
				else
				{
					obj = return_table_list 
				}
			}
			else if(length(obj) >= 7 && (c("BSkySplit") %in% names(obj)) ) ## [[2]] will contain the name of BSkyfunction. which is the same as the name of the output template.
			{
					# do nothing - Obj is already a BSky return structure
			}
			else
			{
				# SK 01/20/21
				# cat("\n0. printing the object\n")
				# print(obj)
		
				tabList = list(obj)
				names(tabList) = singleTableOutputHeader #c("My hardcoded table name") 
				
				if(bSkyReturnObj == TRUE)
				{
					BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed = tabList)
					#print("Before")
					obj = BSkyReturnStructure2()
					#print("After")
					#print(obj)
				}
				else
				{
					obj = tabList 
				}
				
			}
		}
		
		# SK 01/20/21
		# cat("\n1. printing the object\n")
		# print(obj)
		
		#For debugging only to get the return object before decimal conversion 
		#return(obj)
		
#pmt3 = proc.time()
		if(!is.null(obj))
		{
			#print("calling BSkyDecimalDigitFormating on obj")
			obj = BSkyDecimalDigitFormating(obj, decimalDigits = decimalDigitsRounding, engNotationSetting = engNotationSetting, isRound=isRound)
			#print("BSkyDecimalDigitFormating finished")
		}
		
		# SK 01/20/21
		# cat("\n2. printing the object\n")
		# print(obj)
		
#cat("Decimal formatting - timing 3:\n")
#print( proc.time() - pmt3)
#print("last if condition started")

			###############################################################################################
			# 11/24/20 the following code needs to be put towards the very end of the BSkyFormat2 function BSkyFootnote_BSkySplit
			###############################################################################################
		if(bSkyFormatAppRequest == FALSE && !is.null(obj))
		{
			# SK 05/20/21 
			# This is only for Data Frame, Matrix, list of Data Frames and list of Matrix obj class type as well as BSkyReturn sructure list to 
			# handle the attributes may be attached to the imput table - that should be put back into the table 
			# before returning from BSkyFormat2() 
			if(!is.null(orig_list_of_tables_passed) && ((class(orig_list_of_tables_passed)[1] == "list" && length(orig_list_of_tables_passed) == 1 && (class(orig_list_of_tables_passed[[1]])[1] %in% c("data.frame", "matrix"))) || (class(orig_list_of_tables_passed)[1] %in% c("data.frame", "matrix"))))
			{
				orig_custom_attributes = attributes(orig_list_of_tables_passed)
				#orig_custom_attributes = orig_custom_attributes[!names(orig_custom_attributes) %in% c("names", "class", "row.names","dimnames", "dim")]
				orig_custom_attributes = orig_custom_attributes[grepl("(footnote|Footnote)", names(orig_custom_attributes))] 
				
				if(!is.null(orig_custom_attributes) && length(orig_custom_attributes) > 0)
				{
					attributes(obj$tables[[1]]) = c(attributes(obj$tables[[1]]), orig_custom_attributes)
				}
			}
			else if(!is.null(orig_list_of_tables_passed) && class(orig_list_of_tables_passed)[1] == "list" && length(orig_list_of_tables_passed) > 1)
			{
				#class_list = unlist(lapply(orig_list_of_tables_passed, function(x){class(x)[1]}))
				
				#names(class_list) = c()
				
				for(n in 1: length(orig_list_of_tables_passed))
				{
					if(class(orig_list_of_tables_passed[[n]])[1] %in% c("data.frame", "matrix"))
					{
						orig_custom_attributes = attributes(orig_list_of_tables_passed[[n]])
						#orig_custom_attributes = orig_custom_attributes[!names(orig_custom_attributes) %in% c("names", "class", "row.names","dimnames", "dim")]
						orig_custom_attributes = orig_custom_attributes[grepl("(footnote|Footnote)", names(orig_custom_attributes))] 
				
						if(!is.null(orig_custom_attributes) && length(orig_custom_attributes) > 0)
						{
							attributes(obj$tables[[n]]) = c(attributes(obj$tables[[n]]), orig_custom_attributes)
						}
					}
				}
				
			}
			
			if(!exists("holdBSkyFormatObject", env=uadatasets.sk) || is.null(uadatasets.sk$holdBSkyFormatObject))
			{
				uadatasets.sk$holdBSkyFormatObject = list(obj)
			}
			else
			{
				uadatasets.sk$holdBSkyFormatObject = c(uadatasets.sk$holdBSkyFormatObject, list(obj)) 
			}
			
			#Put a marker in the print sync file 
			if(isRmarkdownOutputOn == FALSE)
			{
				####16Mar2021 commenting temporarily for kable html output fix
				cat("\n") # forcing a new line in case someone created a cat() without a trailing new line
				print("BSkyFormatInternalSyncFileMarker")
			}
			
			#cat("\n") #This will make sure next thing goes to the next line. Otherwise, it appended to the marker text.
			#print(PrintBSkyNewLinE)
			
			###################################################################################
			#11/24/20 - The following code is for creating an in memory queue instead of a sync file to signal the new (Python, etc) application tier
			####################################################################################
			
			if(!exists("holdBSkyFormatObjectNew", env=uadatasets.sk) || is.null(uadatasets.sk$holdBSkyFormatObjectNew))
			{
				uadatasets.sk$holdBSkyFormatObjectNew = list(list(type=c("BSkyFormat"), object=obj))
			}
			else
			{
				uadatasets.sk$holdBSkyFormatObjectNew = c(uadatasets.sk$holdBSkyFormatObjectNew, list(list(type=c("BSkyFormat"), object=obj)))
			}			

		}
		#print("last if condition finished")
#cat("Complete timing - timing 4:\n")
#print( proc.time() - ptm1)

		##############################################################################################################
		# SK 6/8/21 - if the obj is of BSkyFormat return structure - do one last thing to process the error/warnings 
		# Since obj of type BSkyFormat return structure does not go through the BSkyReturnStructure2() - so we need to
		# write the error/warning to the sync file here 
		##############################################################################################################
		if(length(obj) >= 7 && (c("BSkySplit") %in% names(obj)) ) 
		{
			obj = BSkyWriteErrorWarningsToSyncFile(obj)
		}
		return(invisible(obj))
}

#14Jun2021 this is used to replace the ugly data split name to the current dataset name 
BSkyReplaceSplitDatasetName <- function(stringWithDatasetName = c())
{
	if(length(stringWithDatasetName) > 0)
	{
		stringWithDatasetName = gsub("data = uadatasets\\$lst\\[\\[\\D+]]", paste("data =", BSkyGetCurrentDatabaseName()), stringWithDatasetName)
	}
	
	return(invisible(stringWithDatasetName))
}

BSkyWriteErrorWarningsToSyncFile <-function(obj) 
{
	# process only if this is a BSkyReturn structure type obj 
	if(length(obj) >= 7 && (c("BSkySplit") %in% names(obj)) ) 
	{
		if(is.null(obj$log))
		{
			totalRowsinLogTable=0
		}
		else
		{	
			totalRowsinLogTable = dim(obj$log)[1]
		}
		
		if(totalRowsinLogTable > 0)
		{
			for(i in 1: totalRowsinLogTable)
			{
				currentLogRow <- obj$log[i,]
				
				if(currentLogRow$type=="Error" ||  currentLogRow$type=="Warning")
				{
					RMsg <- as.character(currentLogRow$RMessage)
					BSkyAppMsg <- as.character(currentLogRow$BSkyMessage)
				
					# SK added on 06/07/21 - Add the following to print the split level iteration info if split is ON 
					if(!is.na(currentLogRow$splitLevel) && obj$BSkySplit == 1) #13Jun2021 handle error in no split.
					{
						split_iter_info = as.character(currentLogRow$splitLevel)
						
						split_iter_info = substr(split_iter_info, 12, nchar(split_iter_info))
						
						writeLines(as.character(split_iter_info))
						cat("\n")
					}
					
					# SK added on 04/24/21 - to avoid return strucutre to be processed by app just for the errors and warnings after returning
					# from the BSky function calls 
					
					# SK added on 06/07/21 - na check is put in place to avoid printing NA on the output 
					if(!is.na(currentLogRow$BSkyMessage) && trimws(currentLogRow$BSkyMessage) != c("") && trimws(currentLogRow$BSkyMessage) != c("\"\""))
					{
						writeLines(as.character(BSkyAppMsg))
						cat("\n")
					}
					
					# SK added on 06/07/21 - na check is put in place to avoid printing NA on the output 
					if(!is.na(currentLogRow$RMessage) && trimws(currentLogRow$RMessage) != c("") && trimws(currentLogRow$RMessage) != c("\"\""))
					{
						writeLines(as.character(RMsg))
						cat("\n")
					}	
				}
			}
		}
	
		# reset/empty out the ewtable since the error/warnings are already written to the sync file above 
		last_table = length(obj$tables)
		obj$tables[[last_table]]$nometadatatables = 0
		obj$tables[[last_table]]$metadatatable=list()
	}
	
	return(invisible(obj))
}



# BSkyGraphicsFormat <- function (bSkyFormatAppRequest = FALSE, noOfGraphics=1)
# {
	# if(bSkyFormatAppRequest == FALSE)
	# {
		# cat("\n") # forcing a new line in case someone created a cat() without a trailing new line
		
		# for(i in 1 : noOfGraphics)
		# {
			# print("BSkyGraphicsFormatInternalSyncFileMarker")
			
			# ###################################################################################
			# #11/24/20 - The following code is for creating an in memory queue instead of a sync file to signal the new (Python, etc) application tier
			# ####################################################################################
			
			# if(!exists("holdBSkyFormatObjectNew", env=uadatasets.sk) || is.null(uadatasets.sk$holdBSkyFormatObjectNew))
			# {
				# uadatasets.sk$holdBSkyFormatObjectNew = list(list(type=c("BSkyGraphicsFormat"), object=c("BSkyGraphics")))
			# }
			# else
			# {
				# uadatasets.sk$holdBSkyFormatObjectNew = c(uadatasets.sk$holdBSkyFormatObjectNew, list(list(type=c("BSkyGraphicsFormat"), object=c("BSkyGraphics"))))
			# }
		# }
	# }
# }

BSkyGraphicsFormat <- function (bSkyFormatAppRequest = FALSE, noOfGraphics= 1, isRmarkdownOutputOn = BSkyIsRmarkdownOutputOn())
{
	if(bSkyFormatAppRequest == FALSE)
	{
		# if((exists("uadatasets.sk") && exists("BSkyKableFormatting", env=uadatasets.sk) && uadatasets.sk$BSkyKableFormatting == FALSE))
		# {
			# doKableFormatting = FALSE
		# }
		# else
		# {
			# doKableFormatting = TRUE
			
			# if((exists("uadatasets.sk") && exists("BSkyRmarkdownFormatting", env=uadatasets.sk) && uadatasets.sk$BSkyRmarkdownFormatting == FALSE))
			# {
				# doRmarkdownFormatting = FALSE
			# }
			# else
			# {
				# doRmarkdownFormatting = TRUE
			# }
			
			# isRmarkdownOutputOn = doRmarkdownFormatting
		# }
		
		cat("\n") # forcing a new line in case someone created a cat() without a trailing new line
		
		for(i in 1 : noOfGraphics)
		{
			if(isRmarkdownOutputOn == FALSE)
			{
				print("BSkyGraphicsFormatInternalSyncFileMarker")
			}
			
			###################################################################################
			#11/24/20 - The following code is for creating an in memory queue instead of a sync file to signal the new (Python, etc) application tier
			####################################################################################
			
			if(!exists("holdBSkyFormatObjectNew", env=uadatasets.sk) || is.null(uadatasets.sk$holdBSkyFormatObjectNew))
			{
				uadatasets.sk$holdBSkyFormatObjectNew = list(list(type=c("BSkyGraphicsFormat"), object=c("BSkyGraphics")))
			}
			else
			{
				uadatasets.sk$holdBSkyFormatObjectNew = c(uadatasets.sk$holdBSkyFormatObjectNew, list(list(type=c("BSkyGraphicsFormat"), object=c("BSkyGraphics"))))
			}
		}
	}
}



# is round to apply round() if true else apply floor()
BSkySetRound <- function(isround = TRUE)
{
	uadatasets.sk$BSkyIsRound = isround
	return(invisible(uadatasets.sk$BSkyIsRound))
}

BSkyGetRound <- function()
{
	if(exists("BSkyIsRound", env=uadatasets.sk))
	{
		return(invisible(uadatasets.sk$BSkyIsRound))
	}
	else
	{
		return(invisible((uadatasets.sk$BSkyIsRound = TRUE)))
	}
}

BSkySetEngNotationSetting <- function(engNotationSetting = TRUE)
{
	uadatasets.sk$BSkyEngNotationSetting = engNotationSetting
	return(invisible(uadatasets.sk$BSkyEngNotationSetting))
}

BSkyGetEngNotationSetting <- function()
{
	if(exists("BSkyEngNotationSetting", env=uadatasets.sk))
	{
		return(invisible(uadatasets.sk$BSkyEngNotationSetting))
	}
	else
	{
		return(invisible((uadatasets.sk$BSkyEngNotationSetting = TRUE)))
	}
}


#17Apr2021 This function code has been changed to implement the decimal formatting with padded 0s and leave alone the whole numbers. 
BSkyDecimalDigitFormating <- function(anyDataFrameOrBSkyReturnObj, decimalDigits = BSkyGetDecimalDigitSetting(), engNotationSetting = BSkyGetEngNotationSetting(), isRound=BSkyGetRound())
{

#ptm1 = proc.time()
	#print(" SK 0 BSkyDecimalDigitFormating: begins")
	if(decimalDigits >= 0)
	{
		#print("BSkyDecimalDigitFormating: decimalDigits >=0")
		if((length(class(anyDataFrameOrBSkyReturnObj))> 0) &&  (class(anyDataFrameOrBSkyReturnObj)[1]== "data.frame" || class(anyDataFrameOrBSkyReturnObj)[1]== "matrix"))
		{
			#print("BSkyDecimalDigitFormating: if matrix or data.frame")
			if(class(anyDataFrameOrBSkyReturnObj)[1] == "data.frame")
			{
				rnames = row.names(anyDataFrameOrBSkyReturnObj)
				cnames = names(anyDataFrameOrBSkyReturnObj)
			}
			else
			{
				#print("BSkyDecimalDigitFormating: if anything but data.frame")
				allNames = dimnames(anyDataFrameOrBSkyReturnObj)
				#print("BSkyDecimalDigitFormating: dimnames=")
				#print(allNames)
				if(!is.null(allNames))
				{
					#print("BSkyDecimalDigitFormating: assigning allnames to rnames and cnames")
					rnames = allNames[[1]]
					cnames = allNames[[2]]
					#print("BSkyDecimalDigitFormating: assigned allnames to rnames and cnames")
				}
				else
				{
					#print("BSkyDecimalDigitFormating: assigning NULL to rnames and cnames")
					rnames = NULL
					cnames = NULL
					#print("BSkyDecimalDigitFormating: assigned NULL to rnames and cnames")
				}
			}
							
			#rnames = row.names(as.data.frame(anyDataFrameOrBSkyReturnObj))
			#cnames = names(as.data.frame(anyDataFrameOrBSkyReturnObj))
#ptm2 = proc.time()
			anyDataFrameOrBSkyReturnObj <- data.frame(lapply(as.data.frame(anyDataFrameOrBSkyReturnObj), as.character), stringsAsFactors=FALSE)
#cat("timing for dataframe conversion:\n")
#print(proc.time() - ptm2)
			
			#print("BSkyDecimalDigitFormating: names(anyDataFrameOrBSkyReturnObj) = cnames")
			names(anyDataFrameOrBSkyReturnObj) = cnames
			#print("BSkyDecimalDigitFormating: row.names(anyDataFrameOrBSkyReturnObj) = rnames")
			row.names(anyDataFrameOrBSkyReturnObj) = rnames
			#print("BSkyDecimalDigitFormating: assigned rnames and cnames")
			
			#print("BSkyDecimalDigitFormating: dfdim = dim(anyDataFrameOrBSkyReturnObj)")
			dfdim = dim(anyDataFrameOrBSkyReturnObj)
			#print("BSkyDecimalDigitFormating: assigned dim")

#ptm3 = proc.time()
			for(j in 1:dfdim[2])
			{
			   if(length(grep("\\.", as.character(anyDataFrameOrBSkyReturnObj[,j]))) > 0)
			   {
					thisColumnHasDecimal = TRUE
			   }
			   else
			   {
					thisColumnHasDecimal = FALSE 
			   }
				
			   for(i in 1:dfdim[1])
			   {
					#cat("\n\n<br> SK-1 <br>\n\n")
					#print(anyDataFrameOrBSkyReturnObj[i,j])
					#cat("\n\n ============================ <br>\n\n")
						
				  if(!is.na(suppressWarnings(as.numeric(anyDataFrameOrBSkyReturnObj[i,j]))))
				  {
					if(grepl('e', as.character(anyDataFrameOrBSkyReturnObj[i,j]), fixed=TRUE) == TRUE)
					{
						if(engNotationSetting == TRUE || abs(as.numeric(anyDataFrameOrBSkyReturnObj[i,j])) >= 1)
						{
							anyDataFrameOrBSkyReturnObj[i,j] = signif(as.numeric(anyDataFrameOrBSkyReturnObj[i,j]), digits = (decimalDigits+1))
							#cat('\n running signif')
							
							if(engNotationSetting == TRUE)
							{
								# SK change 04/09/21 - padding training 0s to comply with APA format to have equal digits after the decimal point for all cells
								anyDataFrameOrBSkyReturnObj[i,j] = formatC(as.numeric(anyDataFrameOrBSkyReturnObj[i,j]), format="e", digits = decimalDigits)
							}
							else
							{
								# SK change 04/09/21 - padding training 0s to comply with APA format to have equal digits after the decimal point for all cells
								anyDataFrameOrBSkyReturnObj[i,j] = format(as.numeric(anyDataFrameOrBSkyReturnObj[i,j]), nsmall= decimalDigits)
							}
						}
						else
						{
							if(isRound)
							{
								##04/Jan/20 anyDataFrameOrBSkyReturnObj[i,j] = round(as.numeric(anyDataFrameOrBSkyReturnObj[i,j]), digits = decimalDigits)
								
								anyDataFrameOrBSkyReturnObj[i,j] = eval(parse(text=paste( 'sprintf("%.',decimalDigits,'f",', as.numeric(anyDataFrameOrBSkyReturnObj[i,j]),')', sep='')))
								#cat('\n running round')
							}
							else
							{
								anyDataFrameOrBSkyReturnObj[i,j] = floor(as.numeric(anyDataFrameOrBSkyReturnObj[i,j]))
								#cat('\n running floor')
							}
							
							if(thisColumnHasDecimal == TRUE) #grepl('\\.', as.character(anyDataFrameOrBSkyReturnObj[i,j])) == TRUE)
							{
								# SK change 04/09/21 - padding training 0s to comply with APA format to have equal digits after the decimal point for all cells
								anyDataFrameOrBSkyReturnObj[i,j] = format(as.numeric(anyDataFrameOrBSkyReturnObj[i,j]), nsmall= decimalDigits, scientific=FALSE)
							}
						}
					}
					else
					{
						#14Aug2016 replace by following for sci fix. anyDataFrameOrBSkyReturnObj[i,j] = round(as.numeric(anyDataFrameOrBSkyReturnObj[i,j]), digits = decimalDigits) 
						anyDataFrameOrBSkyReturnObj[i,j] = format(round(as.numeric(anyDataFrameOrBSkyReturnObj[i,j]), digits = decimalDigits), scientific=FALSE) 
						#cat('\n running round')
						
						## NOTE : signif first and the format. DO NOT RUN round().
						## put engNotationSetting here to change to e notation. 2 more place below in this code
						# if(engNotationSetting==TRUE)
						# {
							# anyDataFrameOrBSkyReturnObj[i,j] = format( signif(as.numeric(anyDataFrameOrBSkyReturnObj[i,j]), digits = (decimalDigits+1)), scientific=TRUE)
						# }
						
						if(thisColumnHasDecimal == TRUE) #grepl('\\.', as.character(anyDataFrameOrBSkyReturnObj[i,j])) == TRUE)
						{
							# SK change 04/09/21 - padding training 0s to comply with APA format to have equal digits after the decimal point for all cells
							anyDataFrameOrBSkyReturnObj[i,j] = format(as.numeric(anyDataFrameOrBSkyReturnObj[i,j]), nsmall= decimalDigits, scientific=FALSE)
						}
						# cat("\n\n<br> SK-2 <br>\n\n")
						# print(anyDataFrameOrBSkyReturnObj[i,j])
						# cat("\n\n ============================ <br>\n\n")
					}
				  }
			   }
			}
#cat("timing for scanning the entire data frame with decimal conversion:\n")
#print(proc.time() - ptm3)
			#return(invisible(anyDataFrameOrBSkyReturnObj))
		}
		else if((length(class(anyDataFrameOrBSkyReturnObj)) > 0) &&(class(anyDataFrameOrBSkyReturnObj)[1]== "list"))
		{	
			#print("BSkyDecimalDigitFormating: obj is list")
			if(anyDataFrameOrBSkyReturnObj[1] == "D:/gimage.png") ##for bsky graphic commands
			{
				#return(invisible(anyDataFrameOrBSkyReturnObj))
			}
			#else if(length(obj) > 7 && obj[[8]]$type=="table") ## [[2]] will contain the name of BSkyfunction. which is the same as the name of the output template.
			else if(length(anyDataFrameOrBSkyReturnObj) >= 7 && (c("BSkySplit") %in% names(anyDataFrameOrBSkyReturnObj)) ) ## [[2]] will contain the name of BSkyfunction. which is the same as the name of the output template.
			{
				num_tables = length(anyDataFrameOrBSkyReturnObj$tables) - 1
objClassMatrix=FALSE #by Anil
				if( num_tables > 0)
				{
					for(k in 1:num_tables)
					{
						# SK 01/20/21
						# cat("\n",k,".1 printing table\n")
						# print(anyDataFrameOrBSkyReturnObj$tables[[k]])
						# print(class(anyDataFrameOrBSkyReturnObj$tables[[k]]))
						
						# 01/21/21 - changed the following if clause to handle a matrix object returned is.list to be TRUE 
						#if(!is.list(anyDataFrameOrBSkyReturnObj$tables[[k]]) || (is.null(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable) && anyDataFrameOrBSkyReturnObj$tables[[k]]$type != "ewtable"))
						if(!is.list(anyDataFrameOrBSkyReturnObj$tables[[k]]) || (is.list(anyDataFrameOrBSkyReturnObj$tables[[k]]) && !("datatable" %in% names(anyDataFrameOrBSkyReturnObj$tables[[k]])) && (!("type" %in% names(anyDataFrameOrBSkyReturnObj$tables[[k]])) || (anyDataFrameOrBSkyReturnObj$tables[[k]]$type != "ewtable"))))
						{
							# SK 01/20/21
							# cat("\n",k,".2 printing table\n")
						
							# Commented by Aaron 1/12/2021 as warnings were displayed in R 4.0
							#if(class(anyDataFrameOrBSkyReturnObj$tables[[k]]) == "data.frame")
							if("data.frame" %in% class(anyDataFrameOrBSkyReturnObj$tables[[k]]) )
							{
								rnames = row.names(anyDataFrameOrBSkyReturnObj$tables[[k]])
								cnames = names(anyDataFrameOrBSkyReturnObj$tables[[k]])
							}
							else # if it's anything other than data.frame
							{
								# SK 01/20/21
								# cat("\n",k,".3 printing table\n")
							
								# Commented by Aaron 1/12/2021 as warnings were displayed in R 4.0
								#if(class(anyDataFrameOrBSkyReturnObj$tables[[k]]) == "matrix")
								if("matrix" %in% class(anyDataFrameOrBSkyReturnObj$tables[[k]]))#by Anil
									objClassMatrix=TRUE
									
								allNames = dimnames(anyDataFrameOrBSkyReturnObj$tables[[k]])
								#print(allNames)
								if(!is.null(allNames))
								{
									rnames = allNames[[1]]
									cnames = allNames[[2]]
								}
								else
								{
									rnames = NULL
									cnames = NULL
								}
							}
							
							#rnames = row.names(as.data.frame(anyDataFrameOrBSkyReturnObj$tables[[k]]))
							#cnames = names(as.data.frame(anyDataFrameOrBSkyReturnObj$tables[[k]]))
						
							anyDataFrameOrBSkyReturnObj$tables[[k]] <- data.frame(lapply(as.data.frame(anyDataFrameOrBSkyReturnObj$tables[[k]]), as.character), stringsAsFactors=FALSE)
						
							# SK 01/20/21
							# cat("\n",k,".4 printing table\n")
							# print(anyDataFrameOrBSkyReturnObj$tables[[k]])
						
							#print("BSkyDecimalDigitFormating: names()=cnames")
							names(anyDataFrameOrBSkyReturnObj$tables[[k]]) = cnames
							
							if(!objClassMatrix)#following enclosed in 'if' by Anil. earlier row.names was without 'if'
							{
								#print("BSkyDecimalDigitFormating: row.names = rnames. following line has a problem")
								row.names(anyDataFrameOrBSkyReturnObj$tables[[k]]) = rnames
							}
							anyDataFrameOrBSkyReturnObj$tables[[k]] <- as.matrix(anyDataFrameOrBSkyReturnObj$tables[[k]])

							if(objClassMatrix)#Fix for duplicate row names in matrix. This 'if' with dimnames was added by Anil.
							{
								dimnames(anyDataFrameOrBSkyReturnObj$tables[[k]])=allNames 
							}
							
							dfdim = dim(anyDataFrameOrBSkyReturnObj$tables[[k]])

							for(j in 1:dfdim[2])
							{
							   if(length(grep("\\.", as.character(anyDataFrameOrBSkyReturnObj$tables[[k]][,j]))) > 0)
							   {
									thisColumnHasDecimal = TRUE
							   }
							   else
							   {
									thisColumnHasDecimal = FALSE 
							   }
							   
							   for(i in 1:dfdim[1])
							   {
									#cat("\n\n<br> SK-3 <br>\n\n")
									#print(anyDataFrameOrBSkyReturnObj[i,j])
									#cat("\n\n ============================ <br>\n\n")
					
								  if(!is.na(suppressWarnings(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]][i,j]))))
								  {
									if(grepl('e', as.character(anyDataFrameOrBSkyReturnObj$tables[[k]][i,j]), fixed=TRUE) == TRUE)
									{
										if(engNotationSetting == TRUE || abs(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]][i,j])) >= 1)
										{
											anyDataFrameOrBSkyReturnObj$tables[[k]][i,j] = signif(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]][i,j]), digits = (decimalDigits+1))
											
											if(engNotationSetting == TRUE)
											{
												# SK change 04/09/21 - padding training 0s to comply with APA format to have equal digits after the decimal point for all cells
												anyDataFrameOrBSkyReturnObj$tables[[k]][i,j] = formatC(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]][i,j]), format="e", digits= decimalDigits)
											}
											else
											{
												# SK change 04/09/21 - padding training 0s to comply with APA format to have equal digits after the decimal point for all cells
												anyDataFrameOrBSkyReturnObj$tables[[k]][i,j] = format(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]][i,j]), nsmall= decimalDigits, scientific=FALSE)
											}
										}
										else
										{
											if(isRound)
											{
												##04/Jan/20 anyDataFrameOrBSkyReturnObj$tables[[k]][i,j] = round(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]][i,j]), digits = decimalDigits)
												anyDataFrameOrBSkyReturnObj$tables[[k]][i,j] = eval(parse(text=paste( 'sprintf("%.',decimalDigits,'f",', as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]][i,j]),')', sep='')))
											}
											else
											{
												anyDataFrameOrBSkyReturnObj$tables[[k]][i,j] = floor(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]][i,j]))
											}
											
											if(thisColumnHasDecimal == TRUE) #grepl('\\.', as.character(anyDataFrameOrBSkyReturnObj$tables[[k]][i,j])) == TRUE)
											{
												# SK change 04/09/21 - padding training 0s to comply with APA format to have equal digits after the decimal point for all cells
												anyDataFrameOrBSkyReturnObj$tables[[k]][i,j] = format(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]][i,j]), nsmall= decimalDigits, scientific=FALSE)
											}
										}
									}
									else
									{
										#14Aug2016 replace by following for sci fix.# anyDataFrameOrBSkyReturnObj$tables[[k]][i,j] = round(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]][i,j]), digits = decimalDigits)
										anyDataFrameOrBSkyReturnObj$tables[[k]][i,j] = format(round(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]][i,j]), digits = decimalDigits), scientific=FALSE)
										
										if(thisColumnHasDecimal == TRUE) #grepl('\\.', as.character(anyDataFrameOrBSkyReturnObj$tables[[k]][i,j])) == TRUE)
										{
											# SK change 04/09/21 - padding training 0s to comply with APA format to have equal digits after the decimal point for all cells
											anyDataFrameOrBSkyReturnObj$tables[[k]][i,j] = format(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]][i,j]), nsmall= decimalDigits, scientific=FALSE)
										}
									}
								  }
								  
								}
							}
						}
						# 01/21/21 - changed the following if clause to handle potential error for non exsitant check of $datatable and $type object
						#else if(is.list(anyDataFrameOrBSkyReturnObj$tables[[k]]) && !is.null(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable) && anyDataFrameOrBSkyReturnObj$tables[[k]]$type != "ewtable")
						else if(is.list(anyDataFrameOrBSkyReturnObj$tables[[k]]) && ("datatable" %in% names(anyDataFrameOrBSkyReturnObj$tables[[k]])) && (!("type" %in% names(anyDataFrameOrBSkyReturnObj$tables[[k]])) || (anyDataFrameOrBSkyReturnObj$tables[[k]]$type != "ewtable")))
						{
							if(class(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable)[1] == "data.frame")
							{
								#print("BSkyDecimalDigitFormating: obj is list with data.frame")
								rnames = row.names(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable)
								cnames = names(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable)
							}
							else
							{
								#print("BSkyDecimalDigitFormating: obj is list with anything but data.frame")
								allNames = dimnames(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable)
								if(!is.null(allNames))
								{
									rnames = allNames[[1]]
									cnames = allNames[[2]]
								}
								else
								{
									rnames = NULL
									cnames = NULL
								}
								#print("BSkyDecimalDigitFormating: obj is list with anything but data.frame. rnames and cnames ready")
							}
							
							#rnames = row.names(as.data.frame(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable))
							#cnames = names(as.data.frame(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable))
							
							anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable <- data.frame(lapply(as.data.frame(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable), as.character), stringsAsFactors=FALSE)
						
							#print("BSkyDecimalDigitFormating: assigning cnames")
							names(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable) = cnames
							
							#print("BSkyDecimalDigitFormating: row.names = rnames")
							row.names(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable) = rnames
							
							#print("BSkyDecimalDigitFormating: as.matrix")
							anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable <- as.matrix(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable)
							
							#print("BSkyDecimalDigitFormating: dfdim = dim(of table in retStruct)")
							dfdim = dim(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable)

							for(j in 1:dfdim[2])
							{
							   if(length(grep("\\.", as.character(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[,j]))) > 0)
							   {
									thisColumnHasDecimal = TRUE
							   }
							   else
							   {
									thisColumnHasDecimal = FALSE 
							   }
							   
							   for(i in 1:dfdim[1])
							   {
							   
									#cat("\n\n<br> SK-4 <br>\n\n")
									#print(anyDataFrameOrBSkyReturnObj[i,j])
									#cat("\n\n ============================ <br>\n\n")
					
								  if(!is.na(suppressWarnings(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j]))))
								  {
									if(grepl('e', as.character(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j]), fixed=TRUE) == TRUE)
									{
										if(engNotationSetting == TRUE || abs(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j])) >= 1 )
										{
											anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j] = signif(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j]), digits = (decimalDigits+1))
											
											if(engNotationSetting == TRUE)
											{
												# SK change 04/09/21 - padding training 0s to comply with APA format to have equal digits after the decimal point for all cells
												# formatC(as.numeric(x), format = "e", digits = 2)
												anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j] = formatC(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j]), format = "e", digits = decimalDigits)
											}
											else
											{
												# SK change 04/09/21 - padding training 0s to comply with APA format to have equal digits after the decimal point for all cells
												anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j] = format(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j]), nsmall = decimalDigits, scientific=FALSE)
											}
										}
										else
										{
											if(isRound)
											{
											#04/Jan/20 anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j] = round(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j]), digits = decimalDigits)
											anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j] = eval(parse(text=paste( 'sprintf("%.',decimalDigits,'f",', as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j]),')', sep='')))
											}
											else
											{
											anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j] = floor(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j]))
											}
											
											if(thisColumnHasDecimal == TRUE)
											{
												# SK change 04/09/21 - padding training 0s to comply with APA format to have equal digits after the decimal point for all cells
												anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j] = format(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j]), nsmall = decimalDigits, scientific=FALSE)	
											}
										}
									}
									else
									{
										#14Aug2016 replace by following for sci fix.#anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j] = round(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j]), digits = decimalDigits)
										anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j] = format(round(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j]), digits = decimalDigits), scientific=FALSE)

										if(thisColumnHasDecimal == TRUE) #grepl('\\.', as.character(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j])) == TRUE)
										{
											# SK change 04/09/21 - padding training 0s to comply with APA format to have equal digits after the decimal point for all cells
											anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j] = format(as.numeric(anyDataFrameOrBSkyReturnObj$tables[[k]]$datatable[i,j]), nsmall = decimalDigits, scientific=FALSE)
										}
									}
								  }

							   }
							}
						}
					}
				}
				#return(invisible(anyDataFrameOrBSkyReturnObj))
			}
		}
	}
#print("BSkyDecimalDigitFormating: just before return")
#print("BSkyDecimalFormatting timing:")
#print(proc.time() - ptm1)

#### Logic from Sanjay : to convert non-e notaion numbers to e-notaion (when e-notaion was ON) 
### we can check specific col (e.g. p.value) and convert them instead of going row by row
### Or
### We can check if there is any number with 'e' notaion in a column and then decide to convert all values of that column to e-notaion.

	return(invisible(anyDataFrameOrBSkyReturnObj))
}


BSkySetDecimalDigitSetting <- function(decimalDigitSetting = 2)
{
	#26May2020 For handlin e notation to decimals in tibble. Bob suggest this from following url
	#https://stackoverflow.com/questions/60004515/how-to-convert-scientific-notation-to-decimal-in-tibbles/60004882#60004882
	options(pillar.sigfig = decimalDigitSetting)
	
	uadatasets.sk$BSkyDecimalDigitSetting = decimalDigitSetting
	return(invisible(uadatasets.sk$BSkyDecimalDigitSetting))
}

BSkyGetDecimalDigitSetting <- function()
{
	if(exists("BSkyDecimalDigitSetting", env=uadatasets.sk))
	{
		return(invisible(uadatasets.sk$BSkyDecimalDigitSetting))
	}
	else
	{
		return(invisible((uadatasets.sk$BSkyDecimalDigitSetting = 2)))
	}
}

###################################################################
# 11/24/20 - Some changes to the following exsiting function    
# Once the following function is called from the application tier to get the processing queue structure, the queue is reset/NULLed
# Anil's C# app tier should call the function with bSkyCompatibility = 1 to get the old global queue structure
# Python app tier just needs to call BSkyGetHoldFormatObjList() without any parameter to get the new global queue structure
# bSkyDebug parameter is just for testing the global queue content to avoid NULLing it if Debug=1 is passed as parameter 
###################################################################

BSkyGetHoldFormatObjList <- function(bSkyCompatibility=0, bSkyDebug = 0)
{
	# SK modified on 06/13/15
	
	objList = NULL
	
	########################################################################################################
	# 11/24/20 - Determine which global queue to return - old for C# app tier or new for Python app tier  ##
	########################################################################################################
		
	if(bSkyCompatibility ==1)
	{
		if(exists("holdBSkyFormatObject", env=uadatasets.sk))
		{
			objList = uadatasets.sk$holdBSkyFormatObject
		
			#if(bSkyDebug == 0)
			#{
			#	uadatasets.sk$holdBSkyFormatObject = NULL
			#}
		}
	}
	else ## new app tier code from python, etc 
	{
		if(exists("holdBSkyFormatObjectNew", env=uadatasets.sk))
		{
			objList = uadatasets.sk$holdBSkyFormatObjectNew
		
			#if(bSkyDebug == 0)
			#{
			#	uadatasets.sk$holdBSkyFormatObjectNew = NULL
			#}
		}
	}
	
	###############################################
	# 11/24/20 - reset the global queue          ##
	###############################################
	#Reset the global queue - both the older queue and the newer queue 
	
	if(bSkyDebug == 0)
	{
		if(exists("holdBSkyFormatObject", env=uadatasets.sk))
		{
			uadatasets.sk$holdBSkyFormatObject = NULL
		}
		
		if(exists("holdBSkyFormatObjectNew", env=uadatasets.sk))
		{
			uadatasets.sk$holdBSkyFormatObjectNew = NULL
		}
	}
	
	return (invisible(objList))
}


#BSkyBuildReturnTableStructure2 <- function(vars=c("mpg"), datasetname = "Dataset2", OutputDataTableListIfPassed=NA, singleTableOutputHeader=c())
BSkyBuildReturnTableStructure2 <- function(OutputDataTableListIfPassed=list(), singleTableOutputHeader=c(" "))
{
		#used to track the number of tables returned
		#nooftablestodis =0
      
		#uadatasets$retstructure <-list()
     
		# To build up the list of stat result tables returned by ua sub function                  
    	#uaStatResults =list()
		
		#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
		# Fill out the uamat and datatable for each split iteration in the return result section
		#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
		# for testing only
		#uatemp = list(NULL)
		#uatemp[[1]]$type="table"
		#numoftablestructures =  length(uatemp)
		
		#uaSplitIterationCounter = uadatasets.sk$splitIteration
  
		#if(is.na(OutputDataTableListIfPassed))
		if(length(OutputDataTableListIfPassed) == 0)
		{
			numoftablestructures =  length(uadatasets$retstructure)
		}
		else
		{
			if((length(class(OutputDataTableListIfPassed))> 0) &&  (class(OutputDataTableListIfPassed)[1]== "numeric" || class(OutputDataTableListIfPassed)[1]== "integer" || class(OutputDataTableListIfPassed)[1]== "character" || class(OutputDataTableListIfPassed)[1]== "logical"))
			{
				#cat('\nFirst\n')
				#obj = BSkyDataFrameObjectFormat(t(as.data.frame(OutputDataTableListIfPassed))) 
				obj = BSkyMatrixObjectFormat(t(as.matrix(OutputDataTableListIfPassed)))
				OutputDataTableListIfPassed = list(obj)
				names(OutputDataTableListIfPassed) = singleTableOutputHeader #singleTableOutputHeader #c("My hardcoded table name")
				numoftablestructures = 1
			}
			else if((length(class(OutputDataTableListIfPassed))> 0) &&  (class(OutputDataTableListIfPassed)[1]== "matrix" || class(OutputDataTableListIfPassed)[1]== "data.frame"))
			{
				#cat('\nSecond\n')
				## 5/31/15 - new code to handle single Matrix or single DataFrame table without wrapping them within a list object
				obj = OutputDataTableListIfPassed
				OutputDataTableListIfPassed = list(obj)
				names(OutputDataTableListIfPassed) = singleTableOutputHeader #singleTableOutputHeader #c("My hardcoded table name")
				numoftablestructures = 1
			}
			else if((length(class(OutputDataTableListIfPassed))> 0) &&  (class(OutputDataTableListIfPassed)[1]== "list"))
			{
				#cat('\nThird\n')
				## 5/31/15 - added new code to ensure catch all condition is the list object
				numoftablestructures = length(OutputDataTableListIfPassed)
			}
			else if(length(OutputDataTableListIfPassed) > 0)
			{
				#cat('\nFourth\n')
				## 5/31/15 - added new code to ensure if catch all condition is not the list object, then do not do any processing
				numoftablestructures = 0
				cat("\n Object type passed (not a numeric, character, or logical array or single Matrix or Data Frame, or list of Matrix or Data Frames)\n")
				cat("in BSkyBuildReturnTableStructure2(OutputDataTableListIfPassed, singleTableOutputHeader)\n")
			}
		}
		
		# cat("\n No. of Tables = ", numoftablestructures,"\n")
		# print(uadatasets$retstructure)
		# cat("\n###########################\n")
		
		if(numoftablestructures > 0)
		{
			## 5/31/15 - added new code to make sure numoftablestructures is not 0
			
			for (k in 1:numoftablestructures)
			{
				#if(is.na(OutputDataTableListIfPassed))
				if(length(OutputDataTableListIfPassed) == 0)
				{
					#uadatasets$retstructure[[k]]$cartlevel = uaComputeCurrentVarNamesAndFactorValues(datasetname)
					#uadatasets$retstructure[[k]]$varindex = UAgetIndexesOfCols(vars, datasetname)
					uadatasets.sk$uaStatResults <- c(uadatasets.sk$uaStatResults, list(uadatasets$retstructure[[k]]))
							# cat("\nNo.",k,"**********************\n")
							# print(uadatasets.sk$uaStatResults)
							# cat("\nNo.",k,"*********Ends*************\n")
				}
				else
				{
					x = names(uadatasets.sk$uaStatResults)
					x = c(x, names(OutputDataTableListIfPassed[k]))
					
					#OutputDataTableListIfPassed[[k]]$cartlevel = uaComputeCurrentVarNamesAndFactorValues(datasetname)
					#OutputDataTableListIfPassed[[k]]$varindex = UAgetIndexesOfCols(vars, datasetname)
					uadatasets.sk$uaStatResults <- c(uadatasets.sk$uaStatResults, list(as.matrix(OutputDataTableListIfPassed[[k]])))
					
					names(uadatasets.sk$uaStatResults) = x
				}
				
				#This variable calculates the number of tables as there could be empty datasets for splits 
				#where there is no data. In the case of an empty dataset, we are only returning a single 
				#table per split
				#nooftablestodis=nooftablestodis+1

			}
		}
		# cat("\nNo.",k,"******Outside*****Final***********\n")
		# print(uadatasets.sk$uaStatResults)
		# cat("\n*********Final ends*************\n")
		
		#if(is.na(OutputDataTableListIfPassed))
		if(length(OutputDataTableListIfPassed) == 0)
		{
			#reinitializing the return structure to preapare for the next split iteration if there is a split
			uadatasets$retstructure <-list() 
		}
}

#25Apr2021 Error/warning printing to sink file
#08Jun2021
BSkyReturnStructure2 <-function(bskyAdditionalTableList = list()) ### passs as list(value)
{
	# print(" SK 1 - I am in BSkyReturnStructure2")
	
	# SK modified on 06/13/15
	
	### Common Errors & Warning ) #### Same as Aaron's ####
	# -1 Error
	# -2 No need to code (Critical Error)
	# 1 Warning

	## The following is just as notes for now.....
	
  	#The 1st return value tells us whether there is a split or not (1=Split, 0=No split)
  
  	#The 2nd return value tells us whether there is an error or not (-2 if there is an error, -1 for warning, and 0 if there is no issue)
  
  	#The 3rd return value gives us the number of errors
  
  	#The 4th return value tells us how many warnings there are
  
  	#The 5th return value gives us all the log values (This is a dataframe)
  
  	#The 6th return value gives us the summary of the function call with the actual parameter valuse passed
  
    #The 7th return value tells us now many tables we are going to display in the output
  
  	#8th position onward in case there is al least table output to be returned
  	
	nooftablestodis = 0
	
  	retlist = list()
    retlist = c( retlist, list(BSkySplit= (if(!is.null(uadatasets$uasplitinfo)) uadatasets$uasplitinfo[[1]] else 0 ))) #BSkySplit in place  of split
    retlist = c( retlist, list(executionstatus= (if(uadatasets.sk$totalErrors > 0) -2 else if(uadatasets.sk$totalWarnings > 0) -1 else 0 )))
    retlist = c( retlist, list(nooferrors=uadatasets.sk$totalErrors ))
    retlist = c( retlist, list(noofwarnings=uadatasets.sk$totalWarnings ))
    retlist = c( retlist, list(log=ualog$df ))
	if(!is.null(uadatasets$uasummary) && length(uadatasets$uasummary) >0)
	{
		retlist = c( retlist, list(uasummary = uadatasets$uasummary ))
	}
	else
	{
		retlist = c( retlist, list(uasummary = list("","","","","","",uadatasets.sk$rproc,"","")))
	}
	
	#print(ualog$df)
	if(is.null(ualog$df))
	{
		totalRowsinLogTable=0
	}
	else
	{	
		totalRowsinLogTable = nrow(ualog$df)
	}
	
	retstructure = list()
	uaStatTablesAndErrWarnTable = list()
	bskyNoofTables = 0
	bskyNoofStatTables = 0
	bskyNoofAdditionalTables = 0
	NoofAdditionalTables = 0
	isErrorWarning = FALSE #Anil
	# cat("\nNoof rows:: ",totalRowsinLogTable,"\n")
	if(totalRowsinLogTable > 0)
	{
		for(i in 1: totalRowsinLogTable)
		{
			currentLogRow <- ualog$df[i,]
			
			if(currentLogRow$type=="Error" ||  currentLogRow$type=="Warning")
			{
				isErrorWarning = TRUE
				# cat("\nError in Ret Stru\n")
				#ualog$df structure/column names example
				#type="Warning", or "Error" ,
				#datasetName=uadatasets.sk$currentDatasetname, 
				#splitLevel=uadatasets.sk$currentVarNamesAndFactorValues,  
				#functionName= functionCallDeails$FuntionCallMade , 
				#functionCommand =functcommand, 
				#uberFunct=uadatasets.sk$uaTempUberFunctionName, 
				#RMessage =uarwarnmsg, 
				#BSkyMessage =appMsgDisp, 
				#callingFnNameOneLevelUp = functionCallDeails$CallingFnNameOneLevelUp, 
				#runTimeParamValuesOneLevelUp = functionCallDeails$RuntimeParamValuesOneLevelUp, 
				#nearestTryCatch = functionCallDeails$NearestTryCatchFnName, 
				#runTimeParamValuesTryCatchFunction = functionCallDeails$RunTimeParamValuesTryCatchFunction)
				
			
				offendingFuncName <- as.character(currentLogRow$functionName)
				offendingFuncParam <- as.character(currentLogRow$functionCommand)
				
				RMsg <- as.character(currentLogRow$RMessage)
				BSkyAppMsg <- as.character(currentLogRow$BSkyMessage)
				
				uberFunctionName <- uaGetUberFunctionName()
				uberFunctionParam <- uaGetUberFunctionParam() 
		
				if(currentLogRow$type=="Error")
				{ 
					# severityType = -2
					severityType = -1 #Error
				}
				else
				{
					# severityType = -1
					severityType = 1 #Warnings
				}
				
		
				#bskyNoofTables=length(uadatasets$retstructure)
				
				if (bskyNoofTables==0)
				{
					#uadatasets$retstructure[[1]]<-list()
					#uadatasets$retstructure[[1]]$type="table"
					#uadatasets$retstructure[[1]]$metadata="yes"
					#uadatasets$retstructure[[1]]$nometadatatables=1
					#uadatasets$retstructure[[1]]$metadatatabletype="normal"
					#uadatasets$retstructure[[1]]$metadatatable=list()
					#uadatasets$retstructure[[1]]$metadatatable[[1]]=data.frame(varIndex=NA,severityType=-2,varName=NA,dataTableRow=NA,startCol=NA,endCol=NA,BSkyMsg=BSkyAppMsg,RMsg=RMsg)
				
					retstructure[[1]]<-list()
					retstructure[[1]]$type="ewtable"
					retstructure[[1]]$metadata="yes"
					retstructure[[1]]$nometadatatables=1
					retstructure[[1]]$metadatatabletype="normal"
					retstructure[[1]]$metadatatable=list()
					#cat("printing in reststructure 1 construction in BSkyReturnStructure() \n")
					retstructure[[1]]$metadatatable[[1]]=data.frame(varIndex=NA,severityType=severityType,varName=NA,dataTableRow=NA,startCol=NA,endCol=NA,BSkyMsg=BSkyAppMsg,RMsg=RMsg)
					
					# SK added on 06/07/21 - Add the following to print the split level iteration info if split is ON 
					if(!is.na(currentLogRow$splitLevel))
					{
						split_iter_info = as.character(currentLogRow$splitLevel)
						
						split_iter_info = substr(split_iter_info, 12, nchar(split_iter_info))
						
						writeLines(as.character(split_iter_info))
						cat("\n")
					}
					
					# SK added on 04/24/21 - to avoid return strucutre to be processed by app just for the errors and warnings after returning
					# from the BSky function calls 
					
					# SK added on 06/07/21 - na check is put in place to avoid printing NA on the output 
					if(!is.na(currentLogRow$BSkyMessage) && trimws(currentLogRow$BSkyMessage) != c("") && trimws(currentLogRow$BSkyMessage) != c("\"\""))
					{
						writeLines(as.character(BSkyAppMsg))
						cat("\n")
					}
					
					# SK added on 06/07/21 - na check is put in place to avoid printing NA on the output 
					if(!is.na(currentLogRow$RMessage) && trimws(currentLogRow$RMessage) != c("") && trimws(currentLogRow$RMessage) != c("\"\""))
					{
						writeLines(as.character(RMsg))
						cat("\n")
					}	
					
					#cat("printing in reststructure 2 construction in BSkyReturnStructure() \n")
					bskyNoofTables = 1
				}
				else
				{
					#cat("printing in reststructure 3 construction in BSkyReturnStructure() \n")
					#cat("varIndex=NA,severityType=-2,varName=NA,dataTableRow=NA,startCol=NA,endCol=NA,BSkyMsg= ",BSkyAppMsg,"RMsg= ", RMsg,"\n")
					retstructure[[1]]$metadatatable[[1]]=rbind(retstructure[[1]]$metadatatable[[1]], data.frame(varIndex=NA,severityType=severityType,varName=NA,dataTableRow=NA,startCol=NA,endCol=NA,BSkyMsg=BSkyAppMsg,RMsg=RMsg))
					
					# SK added on 06/07/21 - Add the following to print the split level iteration info if split is ON 
					if(!is.na(currentLogRow$splitLevel))
					{
						#writeLines(as.character(currentLogRow$splitLevel))
						split_iter_info = as.character(currentLogRow$splitLevel)
						
						split_iter_info = substr(split_iter_info, 12, nchar(split_iter_info))
						
						writeLines(split_iter_info)
						cat("\n")
					}
					
					# SK added on 04/24/21 - to avoid return strucutre to be processed by app just for the errors and warnings after returning
					# from the BSky function calls 
					
					# SK added on 06/07/21 - na check is put in place to avoid printing NA on the output 
					if(!is.na(currentLogRow$BSkyMessage) && trimws(currentLogRow$BSkyMessage) != c("") && trimws(currentLogRow$BSkyMessage) != c("\"\""))
					{
						writeLines(as.character(BSkyAppMsg))
						cat("\n")
					}
					
					# SK added on 06/07/21 - na check is put in place to avoid printing NA on the output 
					if(!is.na(currentLogRow$RMessage) && trimws(currentLogRow$RMessage) != c("") && trimws(currentLogRow$RMessage) != c("\"\""))
					{
						writeLines(as.character(RMsg))
						cat("\n")
					}	
					
					#cat("printing in reststructure 4 construction in BSkyReturnStructure() \n")
					#uadatasets$retstructure[[bskyNoofTables]]$metadatatable[[1]]=rbind(uadatasets$retstructure[[bskyNoofTables]]$metadatatable[[1]],data.frame(varIndex=NA,type=severityType,varName=NA,dataTableRow=NA,startCol=NA,endCol=NA,BSkyMsg=BSkyAppMsg,RMsg=RMsg))
				}
			}
		}
		
	}
	
	# SK added on 04/24/21 - to avoid return strucutre to be processed by app just for the errors and warnings after returning
	# from BSky function like BSky recode - rather print them to sync file now before returning back to the app environment 
	# Force Error and Warnings tables in the return strucutred to be nullified by setting isErrorWarning = FALSE. This is needed 
	# because printing of BSkyAppMsg and RMsg to sync file have already taken place above - hence no need to send back the 
	# same errors and warnings via the return structure to the app  
	# after returing to the app environment, the app will process the sync file and print out all the BSkyAppMsg and RMsg i.e. all
	# errors and warnings onto the app output window
	isErrorWarning = FALSE
	
	##20Jun2013 By Anil create empty ewtable
	if(!isErrorWarning) 
	{
		# cat("\nNoError in Ret Stru\n")
			retstructure[[1]]<-list()
			retstructure[[1]]$type="ewtable"
			retstructure[[1]]$metadata="yes"
			retstructure[[1]]$nometadatatables=0
			retstructure[[1]]$metadatatabletype="normal"
			retstructure[[1]]$metadatatable=list()
							
			bskyNoofTables = 1
	}
	
	if((!is.null(uadatasets.sk$uaStatResults)) && length(uadatasets.sk$uaStatResults) > 0)
	{
		bskyNoofStatTables = length(uadatasets.sk$uaStatResults)
		uaStatTablesAndErrWarnTable = uadatasets.sk$uaStatResults 
	}
	
	##following commented to move ewtable to the last of the return structure
	# if(length(retstructure) > 0)
	# {
		# uaStatTablesAndErrWarnTable = c(uaStatTablesAndErrWarnTable, list(retstructure[[1]]))
	# }
	
	
	NoofAdditionalTables = length(bskyAdditionalTableList) ## Anil moved this out of 'if' and 'if' condition modified
	if(NoofAdditionalTables > 0) #|| ((!is.na(bskyAdditionalTableList)) && NoofAdditionalTables==1) )
	{	
	#old if(!is.na(bskyAdditionalTableList))
	#old {	    
		#bskyNoofAdditionalTables = length(bskyAdditionalTableList)
		#print(bskyAdditionalTableList)
		#cat("I am here 2 bskyNoofAdditionalTables = \n", bskyNoofAdditionalTables)
		
		#if(bskyNoofAdditionalTables >0)
		#{
			#for(i in 1:bskyNoofAdditionalTables)
			for(i in 1:NoofAdditionalTables)
			{
				uaStatTablesAndErrWarnTable = c(uaStatTablesAndErrWarnTable, list(bskyAdditionalTableList[[i]]))
			}
		#}
	}
	
	#bskyNoofTables = bskyNoofTables + bskyNoofStatTables + bskyNoofAdditionalTables
	bskyNoofTables = bskyNoofTables + bskyNoofStatTables + NoofAdditionalTables
	
	#T+he 7th return value tells us now many tables we are going to display in the output
	retlist = c(retlist, nooftables = list(bskyNoofTables))
	
	#### Following if conditioned moved from above to this place. So that "ewtable" will always be the last table in the list of tables
	#### For this C# XML DOM has to match properly.
	#### Earlier the sequence of tables wa [ BSkyStat/stacked tables THEN ewtable THEN addtionalTables ]
	#### Now return structure table sequence :  [ BSkyStat/stacked tables THEN addtionalTables THEN ewtable  ]
	 if(length(retstructure) > 0)
	{
		uaStatTablesAndErrWarnTable = c(uaStatTablesAndErrWarnTable, list(retstructure[[1]]))
	}
	
  	#8th position onward in case there is al least table output to be returned
	retlist = c( retlist, tables = list(uaStatTablesAndErrWarnTable)) #### length(tables) last table should have return value(s)
	
	#BSkyBatchFunctionModeResetForUberFunction()
	uadatasets.sk$uaStatResults = list()
	# print (retlist)
	return(invisible(retlist))
}




#BSkyFormat2 <- function(obj, bSkyFormatAppRequest = FALSE, ftable_change_variable_order = TRUE, sublist_length = 3, remove_rows_with_zero_count = FALSE, no_row_column_headers = FALSE)




################################################################################################################################
## SK 02/18/21 Made quite a bit of changes to allow random quantile/probability values between 0 and 1 
## i.e. other than the standard values allowed earlier like 0 (min), 0.25 (1st Qu), 0.50 (median), 0.75 (3rd Qu), and 1.00 (max) 
################################################################################################################################
#08Oct2021
BSkySummaryStats <-function(data = NULL, datasetColumnObjects=list(), groupByColumnObjects=list(), datasetName=c(), statFunctionList=c(), quantilesProbs=c(0, 0.25, 0.5, 0.75, 1), additionalStats = c(), bSkyFormatAppRequest = FALSE, ftable_change_variable_order = TRUE, sublist_length = 3, remove_rows_with_zero_count = FALSE, no_row_column_headers = FALSE)
{
	#print(match.call())
	#cat("\n")
	
	stripped_data = data
	
	if(!is.null(data))
	{
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
		
		datasetName = dataset_name_str
	}
	else if(length(datasetName) == 0)
	{
		return(invisible(NULL))
	}
	
	
	if(length(groupByColumnObjects) == 0 && !is.null(data))
	{
		group_by_col_names = names(as.data.frame(attr(data, "groups")))
		
		if(length(group_by_col_names) > 0) 
		{
			group_by_col_names = group_by_col_names[1:(length(group_by_col_names) - 1)] # dropping the ".rows" col names from dplyr tibble table 
			
			selected_group_by_col_names_list = paste(paste(group_by_col_names, "=", dataset_name_str, 
														"$", group_by_col_names, sep = ""), sep = "", collapse = ",")
			
			groupByColumnObjects = eval(parse(text = paste("list(", 
														selected_group_by_col_names_list, ")", sep = "")))
										
			# print(selected_group_by_col_names_list)								
			# print(groupByColumnObjects)
			
			#stripped_data = data[c(length(group_by_col_names): ncol(data))] # group_by_col_names includes columns and additional .rows column
			stripped_data = data[, !(names(data) %in% c(group_by_col_names))] # group_by_col_names includes columns and additional .rows column
		}
	}
	
	
	if(length(datasetColumnObjects) == 0 && !is.null(data))
	{
		col_names = dimnames(stripped_data)[[2]]
		
		selected_col_names_list = paste(paste(col_names, "=", dataset_name_str, 
														"$", col_names, sep = ""), sep = "", collapse = ",")
			
		datasetColumnObjects = eval(parse(text = paste("list(", 
														selected_col_names_list, ")", sep = "")))
	}
	
	
	if(length(datasetColumnObjects) == 0) #&& length(groupByColumnObjects) == 0)
	{
		if(length(datasetName) != 0)
		{
			Dataset = (eval(parse(text = datasetName)))
			if(length(groupByColumnObjects) == 0)
			{
				#return(by(Dataset,list(), summary))  # by() has stopped supporting empty grouping list ??
				return(ftable(summary(Dataset)))
				
			}
			else
			{
				return(by(Dataset,groupByColumnObjects, summary))
			}
		}
		else
		{
			return(NULL)
		}
	}
	
#statFunctionList = c(min=FALSE,max=TRUE,mean=TRUE,median=FALSE,sd=TRUE,stderror=FALSE,iqr=FALSE,quantiles=TRUE), probs=c(0,0.25,0.5,0.75,1)),
#additionalStats = c("min,"var")

	temp_statFunctionList = statFunctionList 
	stat_methods = c()
	
	if(length(statFunctionList) > 0)
	{	
		master_stat_func_list = c("min", "max", "mean", "median", "sum", "sd", "stderror", "iqr", "quantiles")
		statFunctionListNames = names(statFunctionList)
		missing_stat_func = master_stat_func_list[!(master_stat_func_list %in% statFunctionListNames)]
		
		# print(master_stat_func_list)
		# print(statFunctionList)
		# print(statFunctionListNames)
		# print(missing_stat_func)
		
		if(length(missing_stat_func) > 0)
		{
			statFunctionList = c(statFunctionList, rep(FALSE, length(missing_stat_func)))
			names(statFunctionList) = c(statFunctionListNames, missing_stat_func)
		}
		
		# print(statFunctionList)
		
		lenquantilesProbs = length(quantilesProbs)
		
		if(lenquantilesProbs > 0)
		{
			quantilesProbs = sort(quantilesProbs)
		}
		
		if((statFunctionList["quantiles"]==TRUE) && length(quantilesProbs) > 0)
		{
			if(statFunctionList["min"]==TRUE) stat_methods = c(stat_methods, "min")
			
			for(i in 1:lenquantilesProbs)
			{
				if(quantilesProbs[i] == 0) stat_methods = c(stat_methods, "min")
				else if(quantilesProbs[i] > 0 && quantilesProbs[i] < 0.25)
				{
					stat_methods = c(stat_methods, paste("Q-",quantilesProbs[i],sep=""))
				}
				else if(quantilesProbs[i] == 0.25) stat_methods = c(stat_methods, "1st Qu")
				else if(quantilesProbs[i] > 0.25 && quantilesProbs[i] < 0.50)
				{
					stat_methods = c(stat_methods, paste("Q-",quantilesProbs[i],sep=""))
				}
				else if(quantilesProbs[i] == 0.50) 
				{
					if(statFunctionList["mean"]==TRUE) stat_methods = c(stat_methods, "mean")
					stat_methods = c(stat_methods, "median")
				}
				else if(quantilesProbs[i] > 0.50 )
				{
					if(statFunctionList["mean"]==TRUE) stat_methods = c(stat_methods, "mean")
					if(statFunctionList["median"]==TRUE) stat_methods = c(stat_methods, "median")
					
					if(quantilesProbs[i] < 0.75)
					{
						stat_methods = c(stat_methods, paste("Q-",quantilesProbs[i],sep=""))
					}
					else if(quantilesProbs[i] == 0.75) stat_methods = c(stat_methods, "3rd Qu")
					else if(quantilesProbs[i] > 0.75 && quantilesProbs[i] < 1.00)
					{
						stat_methods = c(stat_methods, paste("Q-",quantilesProbs[i],sep=""))
					}
				}
			}
			
			if(statFunctionList["mean"]==TRUE) stat_methods = c(stat_methods, "mean")
			if(statFunctionList["median"]==TRUE) stat_methods = c(stat_methods, "median")
		}
		else
		{
			if(statFunctionList["min"]==TRUE) stat_methods = c(stat_methods, "min")
			if(statFunctionList["mean"]==TRUE) stat_methods = c(stat_methods, "mean")
			if(statFunctionList["median"]==TRUE) stat_methods = c(stat_methods, "median")
		}
		
		if((statFunctionList["max"]==TRUE) || ((statFunctionList["quantiles"]==TRUE) && !is.na(match(1, quantilesProbs)))) stat_methods = c(stat_methods, "max")
		if(statFunctionList["sd"]==TRUE) stat_methods = c(stat_methods, "sd")
		if(statFunctionList["stderror"]==TRUE) stat_methods = c(stat_methods, "std. error")
		if(statFunctionList["iqr"]==TRUE) stat_methods = c(stat_methods, "IQR")
		# newly added on 6/27/15
		if(!is.na(statFunctionList["sum"]) && statFunctionList["sum"]==TRUE) stat_methods = c(stat_methods, "sum") 
	}
	
	if(length(additionalStats) > 0 && !is.na(additionalStats)) 
	{
		stat_methods = c(stat_methods, additionalStats) 
	}
		
	stat_methods = unique(stat_methods) #stat_methods[!duplicated(stat_methods)]
	
	
	#print(stat_methods)
	
	statFunctionList = stat_methods
	
	groupByColumnObjects = rev(groupByColumnObjects)
	
	len_groupByColumnObjects = length(groupByColumnObjects)
	len_datasetColumnObjects = length(datasetColumnObjects)
	len_statFunctionList = length(statFunctionList)
	
	#cat(len_groupByColumnObjects, len_datasetColumnObjects, len_statFunctionList, " >>here with length", "\n")
	
	if(len_groupByColumnObjects > 0)
	{
		if(len_groupByColumnObjects > 1)
		{
			modified_groupByColumnObjects = c(groupByColumnObjects, list(groupByColumnObjects[[len_groupByColumnObjects]]))
		}
		else
		{
			modified_groupByColumnObjects = groupByColumnObjects
		}
		
		array_len = 1
		x1 = lapply(groupByColumnObjects, function(x){array_len <<- array_len * length(levels(x))})
		stat_result = array(c(""), c(array_len, 1, (len_statFunctionList+2), len_datasetColumnObjects))
		
		for(i in 1:len_datasetColumnObjects)
		{
			if(len_statFunctionList > 0)
			{
				for(j in 1:len_statFunctionList)
				{
				    if(is.numeric(datasetColumnObjects[[i]]))
					{
						#cat("i:", i, " j:", j,"\n")
						#print(datasetColumnObjects[i])
						#print(modified_groupByColumnObjects)
						#print(statFunctionList[j])
						
						#bsky_a = tapply(datasetColumnObjects[i], modified_groupByColumnObjects, eval(parse(text = statFunctionList[j])) ,na.rm=TRUE)
						if(statFunctionList[j] == "1st Qu")
						{
							bsky_a = tapply(datasetColumnObjects[[i]], modified_groupByColumnObjects, quantile , probs = c(0.25), na.rm=TRUE)
						}
						else if(statFunctionList[j] == "3rd Qu")
						{
							bsky_a = tapply(datasetColumnObjects[[i]], modified_groupByColumnObjects, quantile , probs = c(0.75), na.rm=TRUE)
						}
						else if(substr(statFunctionList[j],1,2) == "Q-")
						{
							bsky_a = tapply(datasetColumnObjects[[i]], modified_groupByColumnObjects, quantile , probs = c(as.numeric(substr(statFunctionList[j],3,nchar(statFunctionList[j])))), na.rm=TRUE)
						}
						else if(statFunctionList[j] == "std. error")
						{
							bsky_a = tapply(datasetColumnObjects[[i]], modified_groupByColumnObjects, function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x))))
						}
						else
						{
							bsky_a = tapply(datasetColumnObjects[[i]], modified_groupByColumnObjects, statFunctionList[j] ,na.rm=TRUE)
						}
						
						if(len_groupByColumnObjects > 1)
						{
							bsky_d = BSkyTableObjectFormat(ftable(bsky_a), ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)
							
							bsky_stat_result = apply(bsky_d[-1,(len_groupByColumnObjects+1):(dim(bsky_d)[2])], 1, function(x) { sum(as.numeric(x), na.rm = TRUE )})
							#print(bsky_stat_result)
							
							bsky_stat_result_NAs = apply(bsky_d[-1,(len_groupByColumnObjects+1):(dim(bsky_d)[2])], 1, function(x) { if(all(is.na(x))) c(NA) else 0})
							#print(bsky_stat_result_NAs)
							
							#bsky_stat_result_with_NAs = sum(bsky_stat_result, bsky_stat_result_NAs)
							bsky_stat_result_with_NAs = bsky_stat_result + bsky_stat_result_NAs
							#print(bsky_stat_result_with_NAs)
							
							#cat("dimension of bsky_stat_result: ", dim(bsky_stat_result))
							stat_result[,,j,i] = bsky_stat_result_with_NAs
						}
						else
						{
							#bsky_d = BSkyTableObjectFormat(bsky_a, ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)
							#bsky_d = data.frame(bsky_a)
							#bsky_stat_result = bsky_d[dim(bsky_d)[2]]
							
							bsky_stat_result = bsky_a  # already a numeric vector
							
							#cat("dimension of bsky_stat_result: ", dim(bsky_stat_result))
							stat_result[,,j,i] = bsky_stat_result
						}
					}
					else
					{
						stat_result[,,j,i] = rep(c("NA"), array_len)
					}
				}
			}
			else
			{
				j = 0
			}
			
			if(len_groupByColumnObjects > 1)
			{
				bsky_a = tapply(datasetColumnObjects[[i]], modified_groupByColumnObjects, function(x){sum(is.na(x))})
				bsky_d = BSkyTableObjectFormat(ftable(bsky_a), ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)
				bsky_stat_result_NAs = apply(bsky_d[-1,(len_groupByColumnObjects+1):(dim(bsky_d)[2])], 1, function(x) { sum(as.numeric(x), na.rm = TRUE )})
				stat_result[,,j+2,i] = bsky_stat_result_NAs
				
				bsky_a = tapply(datasetColumnObjects[[i]], modified_groupByColumnObjects, function(x){sum(!is.na(x))})
				bsky_d = BSkyTableObjectFormat(ftable(bsky_a), ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)
				bsky_stat_result_not_NAs = apply(bsky_d[-1,(len_groupByColumnObjects+1):(dim(bsky_d)[2])], 1, function(x) { sum(as.numeric(x), na.rm = TRUE )})
				#stat_result[,,j+1,i] = (bsky_stat_result_not_NAs + bsky_stat_result_NAs)
				stat_result[,,j+1,i] = bsky_stat_result_not_NAs 
			}
			else
			{
				bsky_stat_result_NAs = tapply(datasetColumnObjects[[i]], modified_groupByColumnObjects, function(x){sum(is.na(x))})
				stat_result[,,j+2,i] = bsky_stat_result_NAs
				
				bsky_stat_result_not_NAs = tapply(datasetColumnObjects[[i]], modified_groupByColumnObjects, function(x){sum(!is.na(x))})
				#stat_result[,,j+1,i] = (bsky_stat_result_not_NAs + bsky_stat_result_NAs)
				stat_result[,,j+1,i] = bsky_stat_result_not_NAs
			}
		}
	}
	else
	{
		stat_result = array(c(""), c(1, 1, (len_statFunctionList+2), len_datasetColumnObjects))
		for(i in 1:len_datasetColumnObjects)
		{
			if(len_statFunctionList > 0)
			{
				for(j in 1:len_statFunctionList)
				{
					if(is.numeric(datasetColumnObjects[[i]]))
					{
						#cat("i:", i, " j:", j,"\n")
						#print(datasetColumnObjects[i])
						#print(modified_groupByColumnObjects)
						#print(statFunctionList[j])
						
						#bsky_a = tapply(datasetColumnObjects[i], modified_groupByColumnObjects, eval(parse(text = statFunctionList[j])) ,na.rm=TRUE)
						#text_prep = paste(statFunctionList[j],"(","datasetColumnObjects[[i]]", ", na.rm=TRUE", ")")
						#print(text_prep)
						
						if(statFunctionList[j] == "1st Qu")
						{
							text_prep = paste("quantile","(","datasetColumnObjects[[i]]", ", na.rm=TRUE", ", probs = c(0.25)", ")")
						}
						else if(statFunctionList[j] == "3rd Qu")
						{
							text_prep = paste("quantile","(","datasetColumnObjects[[i]]", ", na.rm=TRUE", ", probs = c(0.75)", ")")
						}
						else if(substr(statFunctionList[j],1,2) == "Q-")
						{
							prob_value = c(as.numeric(substr(statFunctionList[j],3,nchar(statFunctionList[j]))))
							text_prep = paste("quantile","(","datasetColumnObjects[[i]]", ", na.rm=TRUE", ", probs =", prob_value , ")")
						}
						else if(statFunctionList[j] == "std. error")
						{
							text_prep = paste("sqrt(var","(","datasetColumnObjects[[i]]", ", na.rm=TRUE", ")", "/length(na.omit(", "datasetColumnObjects[[i]]",")))")
							#function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x))))
						}
						else
						{
							text_prep = paste(statFunctionList[j],"(","datasetColumnObjects[[i]]", ", na.rm=TRUE", ")")
						}
						
						bsky_stat_result = eval(parse(text = text_prep))
						stat_result[,,j,i] = bsky_stat_result
					}
					else
					{
						stat_result[,,j,i] = rep(c("NA"), 1)
					}
				}
			}
			else
			{
				j = 0
			}
	
			bsky_stat_result_NAs = sum(is.na(datasetColumnObjects[[i]]))
			stat_result[,,j+2,i] = bsky_stat_result_NAs
			
			bsky_stat_result_not_NAs = sum(!is.na(datasetColumnObjects[[i]]))
			#stat_result[,,j+1,i] = (bsky_stat_result_not_NAs + bsky_stat_result_NAs)
			stat_result[,,j+1,i] = bsky_stat_result_not_NAs
		}
	}
	
	# construction of the final data.frame table formatting
	
	if(len_groupByColumnObjects > 0)
	{
		if(len_groupByColumnObjects > 1)
		{
			bsky_a = tapply(datasetColumnObjects[[i]], modified_groupByColumnObjects, function(x){sum(!is.na(x))})
			bsky_d = BSkyTableObjectFormat(ftable(bsky_a), ftable_change_variable_order, remove_rows_with_zero_count, no_row_column_headers)
			bsky_stat_result_layer_columns = bsky_d[-1, (1:len_groupByColumnObjects)]
		}
		else
		{
			bsky_stat_result_layer_columns = data.frame(levels(modified_groupByColumnObjects[[1]]))
			names(bsky_stat_result_layer_columns) = names(modified_groupByColumnObjects[1])
		}
			
		final_data_frmae_column_names = names(bsky_stat_result_layer_columns)
		
		stat_function = c(statFunctionList, "n", "NAs")
		
		bsky_stat_result_layer_stat_column = matrix(c(""), nrow = 1, ncol = (len_groupByColumnObjects+1))
		row_padding = rep(c(""), len_groupByColumnObjects)
		
		for(i in 1:nrow(bsky_stat_result_layer_columns))
		{
			for(j in 1:length(stat_function))
			{
				if(j == 1)
				{
					row_layers_stat_method = c(as.character(bsky_stat_result_layer_columns[i,]), stat_function[j])
					bsky_stat_result_layer_stat_column = rbind(bsky_stat_result_layer_stat_column, row_layers_stat_method)
				}
				else
				{
					row_layers_stat_method = c(row_padding, stat_function[j])
					bsky_stat_result_layer_stat_column = rbind(bsky_stat_result_layer_stat_column, row_layers_stat_method)
				}
			}
		}
	
		bsky_stat_result_layer_stat_column = as.data.frame(bsky_stat_result_layer_stat_column[-1,])
		row.names(bsky_stat_result_layer_stat_column) = NULL
		
		final_data_frmae_column_names = c(final_data_frmae_column_names, "stat function")
		
		#print(bsky_stat_result_layer_stat_column)
		
		#bsky_stat_result_layer_stat_column = do.call (rbind, apply(bsky_stat_result_layer_columns, 1, function(x){merge(as.data.frame(t(x)),stat_function)}) )
		
		for(i in 1:len_datasetColumnObjects)
		{
			stat_result_for_each_dataset_var = as.character(t(stat_result[,,,i]))
			
			#print(stat_result_for_each_dataset_var)
			#print(bsky_stat_result_layer_stat_column)
			
			bsky_stat_result_layer_stat_column = cbind(bsky_stat_result_layer_stat_column, stat_result_for_each_dataset_var)
			
			#print(bsky_stat_result_layer_stat_column)
			
			if(length(names(datasetColumnObjects[i]) > 0))
			{
				final_data_frmae_column_names = c(final_data_frmae_column_names, names(datasetColumnObjects[i]))
			}
			else
			{
				final_data_frmae_column_names = c(final_data_frmae_column_names, paste("Var_",i,sep=""))
			}
		}
		
		names(bsky_stat_result_layer_stat_column) = final_data_frmae_column_names
		row.names(bsky_stat_result_layer_stat_column) = NULL
	}
	else
	{
		stat_function = c(statFunctionList, "n", "NAs")
		
		bsky_stat_result_layer_stat_column = data.frame(stat_function)
		names(bsky_stat_result_layer_stat_column) = "stat function"
		row.names(bsky_stat_result_layer_stat_column) = NULL
		
		final_data_frmae_column_names = names(bsky_stat_result_layer_stat_column)
		
		#print(bsky_stat_result_layer_stat_column)
		
		for(i in 1:len_datasetColumnObjects)
		{
			stat_result_for_each_dataset_var = as.character(t(stat_result[,,,i]))
			
			#print(stat_result_for_each_dataset_var)
			#print(bsky_stat_result_layer_stat_column)
			
			bsky_stat_result_layer_stat_column = cbind(bsky_stat_result_layer_stat_column, stat_result_for_each_dataset_var)
			
			#print(bsky_stat_result_layer_stat_column)
			
			if(length(names(datasetColumnObjects[i]) > 0))
			{
				final_data_frmae_column_names = c(final_data_frmae_column_names, names(datasetColumnObjects[i]))
			}
			else
			{
				final_data_frmae_column_names = c(final_data_frmae_column_names, paste("Var_",i,sep=""))
			}
		}
		
		names(bsky_stat_result_layer_stat_column) = final_data_frmae_column_names
		row.names(bsky_stat_result_layer_stat_column) = NULL
	}
	
	#print(stat_result)
	#print(bsky_stat_result_layer_stat_column)
	
	
	if(!is.null(data))
	{
		BSky_Dataset_Overview =  data.frame(Dataset = dataset_name_str, Variables = dim(stripped_data)[2], Observations = nrow(stripped_data))
		
		table_list = list(BSky_Dataset_Overview)
		table_list_names = c("Dataset Overview")
		names(table_list) = table_list_names
		
		table_list = c(table_list, list(bsky_stat_result_layer_stat_column))
		table_list_names = c(table_list_names, "Numerical Statistical Analysis by Variable")
		names(table_list) = table_list_names
		
		return(invisible(table_list))
	}
	else
	{
		return(invisible(bsky_stat_result_layer_stat_column))
	}
}


#08Oct2021
BSkyVariableSummaryStats <- function (data = NULL, vars = NULL, group_by_vars = NULL, maxsum = 0)
{
	table_list = list()
	table_list_names = c()
	
	stripped_data = data
	
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
	
	if(is.null(group_by_vars))
	{
		group_by_col_names = names(as.data.frame(attr(data, "groups")))
		
		if(length(group_by_col_names) > 0) 
		{
			group_by_col_names = group_by_col_names[1:(length(group_by_col_names) - 1)] # dropping the ".rows" col names from dplyr tibble table 
			
			#stripped_data = data[c(length(group_by_col_names): ncol(data))] # group_by_col_names includes columns and additional .rows column
			stripped_data = data[, !(names(data) %in% c(group_by_col_names))] # group_by_col_names includes columns and additional .rows column
		}
	}
	else
	{	
		group_by_col_names = group_by_vars
	}
	
	if(is.null(vars))
	{
		col_names = dimnames(stripped_data)[[2]]
	}
	else
	{	
		col_names = vars
	}
	
	stripped_data = stripped_data[c(col_names)]
		
	BSky_Dataset_Overview =  data.frame(Dataset = dataset_name_str, Variables = length(col_names), Observations = nrow(stripped_data))
	
	#BSkyFormat(BSky_Dataset_Overview, singleTableOutputHeader = c("Dataset Overview"))
	table_list = list(BSky_Dataset_Overview)
	table_list_names = c("Dataset Overview")
	names(table_list) = table_list_names

	
	if(length(group_by_col_names) == 0)
	{
		BSky_Summary_By_Variable <- ftable(summary(stripped_data, maxsum = 1+max(sapply(stripped_data 
							 [,sapply(stripped_data, is.factor)], nlevels  )) ))

		BSky_Summary_By_Variable[is.na(BSky_Summary_By_Variable[])]=c("")
		
		#BSkyFormat(BSky_Summary_By_Variable, singleTableOutputHeader=c("Summary By Variable"))
		table_list = c(table_list, list(BSky_Summary_By_Variable))
		table_list_names = c(table_list_names, "Summary By Variable")
		names(table_list) = table_list_names
	}
	else
	{
		selected_group_by_col_list = paste(paste(group_by_col_names, "=", dataset_name_str, 
					"$", group_by_col_names, sep = ""), sep = "", collapse = ",")
			
		selected_group_by_col_list_obj = eval(parse(text = paste("list(", 
					selected_group_by_col_list, ")", sep = "")))
		
		# print(max(sapply(stripped_data[,sapply(stripped_data, is.factor)], nlevels)))
		# if(maxsum == 0)
		# {
			# maxsum = 1 + max(sapply(stripped_data[,sapply(stripped_data, is.factor)], nlevels))
		# }
		
		if(maxsum == 0)
		{
			BSky_Summary_Statistics = by(stripped_data, 
			  #ifelse(length(group_by_col_names) ==1, list(data[,group_by_col_names]), as.list(data[,group_by_col_names])),
			  selected_group_by_col_list_obj,
			  base::summary,
			  maxsum =  1 + max(sapply(stripped_data[,sapply(stripped_data, is.factor)], nlevels)))
		}
		else
		{
			BSky_Summary_Statistics = by(stripped_data, 
			  #ifelse(length(group_by_col_names) ==1, list(data[,group_by_col_names]), as.list(data[,group_by_col_names])),
			  selected_group_by_col_list_obj,
			  base::summary,
			  maxsum =  maxsum)
		}		  

		#BSkyFormat(BSky_Summary_Statistics, singleTableOutputHeader=c("Summary Statistics by Group"))
		table_list = c(table_list, list(BSky_Summary_Statistics))
		table_list_names = c(table_list_names, "Summary Statistics by Group")
		names(table_list) = table_list_names
	}
	
	return(invisible(table_list))
}


