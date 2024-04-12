
 BSkyInAppScriptExtractDatasetNames <- function(code_chunk, debug = FALSE) 
 {
		# df1$col1
		# df2[, 'y']
		# df3[1,]
		# df4[2, 1:2]
		# with(df5, mean(a))
		# data = df6
		# df7 %>% filter(var1 == 'Yes')
  
   # Pattern to match dataset names in various formats (see above)
   pattern = "\\b\\w+\\s*\\$\\w+|\\b\\w+\\s*\\[.*?\\]|\\bwith\\(.*?\\)|\\bdata\\s*=\\s*\\w+|\\b\\w+\\s*%>%"
   
   # Extract dataset names using regex
   matches = regmatches(code_chunk, gregexpr(pattern, code_chunk, perl = TRUE))[[1]]
   
   if(debug)
   {
		cat("\nDataset name patterns matched\n")
		print(matches)
   }
   
   dataset_names = c()
   
   if(length(matches) > 0)
   {
		# Process matches to extract dataset names
		for (match in matches) {
		 dataset_names = c(dataset_names, gsub("\\s*\\$.*|\\s*\\[.*|\\bwith\\(|\\bdata\\s*=\\s*|\\s*%>%", "", match))
		}

		if(debug)
		{
			cat("\nDataset names before clean up\n")
			print(dataset_names)
		}

		matches = grep(",",dataset_names)

		if(length(matches) > 0)
		{
			for(i in 1: length(matches))
			{
				dataset_names[matches[i]] = strsplit(dataset_names[matches[i]], ",")[[1]][1]
			}
		}

		dataset_names = trimws(unique(dataset_names))
		false_dataset_names = c("selectedData", "chartTypes", "objects", "c", "data", "LinearRegModel1", "df", "GlobalEnv","as")
		dataset_names <- dataset_names[!dataset_names %in% false_dataset_names]
	}
	
	if(debug)
	{
		cat("\nDataset names after clean up and filtering\n")
		#cat("\n==\n",dataset_names,"\n==\n")
		print(dataset_names)
	}
		
	return(invisible(dataset_names))
}


BSkyInAppScriptExtractOldDatasetList <- function(bsky_script_full_file_path, expand_extraction = TRUE, debug = FALSE)
{
	if (!file.exists(bsky_script_full_file_path)) 
	{ 
		cat("\n", bsky_script_full_file_path, "- BlueSky script file not found", "..exiting..\n")
		return(invisible)
	}
	
	# Get the extension string
	file_type <- toupper(tools::file_ext(bsky_script_full_file_path))
	
	if(file_type != "BMD" && file_type != "RMD") 
	{ 
		cat("\n", bsky_script_full_file_path, "- file type must have the file extension of Bmd or Rmd", "..exiting..\n")
		return(invisible)
	}
	
	if(file_type == "BMD")
	{
		# Load the 'readr' package for reading lines from files
		suppressMessages(require(readr))
		
		# Define the path to the BSky Bmd zip file and the name of the text file inside the zip
		zip_file_path <- bsky_script_full_file_path
		script_file_name_without_ext <- tools::file_path_sans_ext(basename(bsky_script_full_file_path))

		
		# Open a connection to the zip file
		zip_connection <- unz(zip_file_path, script_file_name_without_ext)
		
		
		# Read the lines from the text file inside the zip
		rmd_content <- read_lines(zip_connection)
		

		# Close the zip connection
		#close(zip_connection)
	}
	else
	{
		rmd_content = readLines(bsky_script_full_file_path, warn = FALSE)
	}

	# Combine lines into a single string
	rmd_text <- paste(rmd_content, collapse = "\n")

	# Extract all text between "```{r}" and "```" for all occurrences (excluding the enclosures)
	code_chunks <- regmatches(rmd_text, gregexpr("```\\{r\\}(.*?)```", rmd_text))[[1]]

	# Remove the enclosures from each code chunk
	code_chunks <- gsub("```\\{r\\}|```", "", code_chunks)
	
	# if(file_type == "BMD")
	# {
		# # Extract lines that start with '#'
		# #code_chunks_comments <- grep("^#", rmd_content, value = TRUE)

		# # Define the regular expression pattern to match everything after "output_title:"
		# pattern <- '((Open Dataset:)\\s*([^"]+)\n)|((output_title:)\\s*"([^"]+)")' 
		# #pattern <- '(^Open Dataset: (.*)$)|((output_title:)\\s*"([^"]+)")'
		
		# # Extract the desired string using regmatches and regexpr
		# code_chunks_comments <- regmatches(rmd_text, gregexpr(pattern, rmd_text))[[1]]
		
		# # Remove the "output_title: " prefix and extra enclosure " around 
		# #code_chunks_comments <- gsub('output_title: ', '', code_chunks_comments) 
		# #code_chunks_comments <- gsub('"', '', code_chunks_comments)
		# code_chunks_comments = gsub('(")|(\n)|(`)|(\\{r\\})|(output_title: )', '', code_chunks_comments)
	# }
	# else
	# {
		# # Extract all text between "```" and "```{r}" title comment for the code chunk all chunk occurrences
		# code_chunks_comments <- regmatches(rmd_text, gregexpr("```(.*?)```\\{r\\}", rmd_text))[[1]]
		# extract_first_comment = regmatches(code_chunks_comments[1], gregexpr("```\\{r(.*?)```", code_chunks_comments[1]))[[1]]
		# code_chunks_comments[1] = gsub('(```|\\{r\\})',"",substr(code_chunks_comments[1], sum(nchar(extract_first_comment)), nchar(code_chunks_comments[1])))
		
		# # Remove the enclosures from each chunk comment
		# code_chunks_comments <- gsub("```\\{r\\}|```", "", code_chunks_comments)
		# #code_chunks_comments = gsub('(\n)|(#\\s+)', '', code_chunks_comments)
		# code_chunks_comments = gsub('(\\s\n)|(\n)|(#\\s)', '', code_chunks_comments)
	# }
	
	#print(code_chunks_comments)
	
	old_dataset_name = c()
	
	for (chunk in code_chunks) 
	{	
		if(regexpr(paste0("\nBSkyloadDataset", ".*"), trimws(chunk,which = "left", whitespace = "[ \t\r]"))[1] > 0)
		{
			
			# Define the regular expression pattern to match datasetName
			pattern <- "datasetName='([^']*)'"

			# Extract the value of datasetName using regmatches and regexpr
			match_result <- regmatches(chunk, regexpr(pattern, chunk))

			# Extract the dataset name from the match
			if (length(match_result) > 0) {
			  orig_dataset_name_extracted = gsub("datasetName='|'", "", match_result[[1]])
			} else {
			  orig_dataset_name_extracted <- NA 
			}
			
			old_dataset_name = c(old_dataset_name, orig_dataset_name_extracted)
			
			
			#cat("\n\nOriginal Dataset (loaded from a data file) used in the script:",old_dataset_name)
			#,"\n==========================\n")
		}
		else if(regexpr(paste0("\nBSkyLoadRpkgDataset", ".*"), trimws(chunk,which = "left", whitespace = "[ \t\r]"))[1] > 0)
		{
			#cat("\nSkipping BSkyLoadRpkgDataset Command\n",chunk,"\n==========================\n")
			
			
			# Define the regular expression pattern to match any sequence of characters up to the first '-' (excluding '-')
			pattern <- '(?<=\\")[^-]+'

			# Extract the desired string using regmatches and regexpr
			match_result <- regmatches(chunk, regexpr(pattern, chunk, perl = TRUE))
			
			# Extract the dataset name from the match
			if (length(match_result) > 0) {
			  orig_dataset_name_extracted = gsub("datasetName='|'", "", match_result[[1]])
			} else {
			  orig_dataset_name_extracted <- NA
			}
			
			old_dataset_name = c(old_dataset_name, orig_dataset_name_extracted)
			

			#cat("\n\nOriginal Dataset (loaded from a R pkg) used in the script:",old_dataset_name)
			#,"\n==========================\n")
		}
		else if(expand_extraction == TRUE)
		{
			script_dataset_names_used_in_code = BSkyInAppScriptExtractDatasetNames(chunk, debug)
			old_dataset_name = c(old_dataset_name, script_dataset_names_used_in_code)
		}
	}
	
	old_dataset_name = sort(unique(old_dataset_name))
	
	#cat("\n", old_dataset_name, "\n")
	return(invisible(old_dataset_name))
}


BSkyInAppScriptExecutionEngine <- function(bsky_script_full_file_path, json_output_file_path =c(), currentDatasetName = BSkyGetCurrentDatabaseName(), replaceOldDatasetName = c(), currentColumnNames = c(), replaceOldColumnNames = c(), expand_extraction = TRUE, debug = FALSE)
{ 
	if (!file.exists(bsky_script_full_file_path)) 
	{ 
		cat("\n", bsky_script_full_file_path, "- BlueSky script file not found", "..exiting..\n")
		return(invisible)
	}
	
	# Get the extension string
	file_type <- toupper(tools::file_ext(bsky_script_full_file_path))
	
	if(file_type != "BMD" && file_type != "RMD") 
	{ 
		cat("\n", bsky_script_full_file_path, "- file type must have the file extension of Bmd or Rmd", "..exiting..\n")
		return(invisible)
	}
	
	# if(!is.null(currentDatasetName) && length(currentDatasetName) > 0 && trimws(currentDatasetName) != "")
	# {
		# cat("\nThe script will be run using the Dataset:",currentDatasetName, "\n")
	
	
		# if(!is.null(replaceOldDatasetName) && length(replaceOldDatasetName) > 0 && trimws(replaceOldDatasetName) != "")
		# {
			# cat("\nThe script will be run after replacing the Dataset:", replaceOldDatasetName, "with the Dataset:",currentDatasetName,"\n")
		# }
	# }
	
	if(file_type == "BMD")
	{
		# Load the 'readr' package for reading lines from files
		suppressMessages(require(readr))

		# Define the path to the BSky Bmd zip file and the name of the text file inside the zip
		zip_file_path <- bsky_script_full_file_path
		script_file_name_without_ext <- tools::file_path_sans_ext(basename(bsky_script_full_file_path))

		# Open a connection to the zip file
		zip_connection <- unz(zip_file_path, script_file_name_without_ext)

		# Read the lines from the text file inside the zip
		rmd_content <- read_lines(zip_connection)

		# Close the zip connection
		#close(zip_connection)
	}
	else
	{
		rmd_content = readLines(bsky_script_full_file_path, warn = FALSE)
	}

	# Combine lines into a single string
	rmd_text <- paste(rmd_content, collapse = "\n")

	# Extract all text between "```{r}" and "```" for all occurrences (excluding the enclosures)
	code_chunks <- regmatches(rmd_text, gregexpr("```\\{r\\}(.*?)```", rmd_text))[[1]]

	# Remove the enclosures from each code chunk
	code_chunks <- gsub("```\\{r\\}|```", "", code_chunks)
	
	if(file_type == "BMD")
	{
		# Extract lines that start with '#'
		#code_chunks_comments <- grep("^#", rmd_content, value = TRUE)

		# Define the regular expression pattern to match everything after "output_title:"
		#pattern <- '(^Open Dataset: (.*)$)|((output_title:)\\s*"([^"]+)")'
		
		#pattern <- '((Open Dataset:)\\s*([^"]+)\n)|((output_title:)\\s*"([^"]+)")' 
		pattern <- '((Open Dataset:)\\s*([^"]+)\n)|((output_title:)\\s*"([^"]+)")|(```\\{console\\}(.*?)```)' 
		
		# Extract the desired string using regmatches and regexpr
		code_chunks_comments <- regmatches(rmd_text, gregexpr(pattern, rmd_text))[[1]]
		
		# Remove the "output_title: " prefix and extra enclosure " around 
		#code_chunks_comments <- gsub('output_title: ', '', code_chunks_comments) 
		#code_chunks_comments <- gsub('"', '', code_chunks_comments)
		code_chunks_comments = gsub('(")|(\n)|(`)|(\\{r\\})|(output_title: )|(\\{console\\})', '', code_chunks_comments)
	}
	else
	{
		# Extract all text between "```" and "```{r}" title comment for the code chunk all chunk occurrences
		code_chunks_comments <- regmatches(rmd_text, gregexpr("```(.*?)```\\{r\\}", rmd_text))[[1]]
		extract_first_comment = regmatches(code_chunks_comments[1], gregexpr("```\\{r(.*?)```", code_chunks_comments[1]))[[1]]
		code_chunks_comments[1] = gsub('(```|\\{r\\})',"",substr(code_chunks_comments[1], sum(nchar(extract_first_comment)), nchar(code_chunks_comments[1])))
		
		# Remove the enclosures from each chunk comment
		code_chunks_comments <- gsub("```\\{r\\}|```", "", code_chunks_comments)
		#code_chunks_comments = gsub('(\n)|(#\\s+)', '', code_chunks_comments)
		code_chunks_comments = gsub('(\\s\n)|(\n)|(#\\s)', '', code_chunks_comments)
	}
	
	# For debugging
	#print(code_chunks_comments)
	
	if(is.null(replaceOldDatasetName) || length(replaceOldDatasetName) == 0 || (length(replaceOldDatasetName) == 1 && trimws(replaceOldDatasetName) == ""))
	{
		old_dataset_name = BSkyInAppScriptExtractOldDatasetList(bsky_script_full_file_path, expand_extraction, debug)
	}
	else
	{
		old_dataset_name = replaceOldDatasetName
	}
	
	if(debug)
	{
		cat("\nFinal list of old dataset names extracted from the script code\n")
		print(old_dataset_name)
		cat("\n")
	}
	
	print_dataset_info_flag = TRUE
	code_chunk_list = list()
				
	codeChunkNum = 0
	codeChunkCommentNum = 0
						
	# Print the extracted R code chunks
	
	for (chunk in code_chunks) 
	{	
		codeChunkCommentNum = codeChunkCommentNum + 1
		
		if(regexpr(paste0("BSkyInAppScriptExecutionEngine", ".*"), trimws(chunk,which = "left", whitespace = "[ \t\r]"))[1] > 0)
		{
			#print("Entered rerurn if")
		}
		else if(regexpr(paste0("\nBSkyloadDataset", ".*"), trimws(chunk,which = "left", whitespace = "[ \t\r]"))[1] > 0)
		{
			#cat("\nSkipping BSkyloadDataset Command\n",chunk,"\n==========================\n")
			
			# if(is.null(replaceOldDatasetName) || length(replaceOldDatasetName) == 0 || (length(replaceOldDatasetName) == 1 && trimws(replaceOldDatasetName) == ""))
			# {
				# # Define the regular expression pattern to match datasetName
				# pattern <- "datasetName='([^']*)'"

				# # Extract the value of datasetName using regmatches and regexpr
				# match_result <- regmatches(chunk, regexpr(pattern, chunk))

				# # Extract the dataset name from the match
				# if (length(match_result) > 0) {
				  # orig_dataset_name_extracted = gsub("datasetName='|'", "", match_result[[1]])
				# } else {
				  # orig_dataset_name_extracted <- NA 
				# }
				
				# if(length(old_dataset_name) == 0 || old_dataset_name != orig_dataset_name_extracted)
				# {
					# print_dataset_info_flag = TRUE
				# }
				# else
				# {
					# print_dataset_info_flag = FALSE
				# }
				
				# old_dataset_name = orig_dataset_name_extracted
			# }
			
			# cat("\n\nOriginal Dataset (loaded from a data file) used in the script:",old_dataset_name)
			#,"\n==========================\n")
		}
		else if(regexpr(paste0("\nBSkyLoadRpkgDataset", ".*"), trimws(chunk,which = "left", whitespace = "[ \t\r]"))[1] > 0)
		{
			#cat("\nSkipping BSkyLoadRpkgDataset Command\n",chunk,"\n==========================\n")
			
			# if(is.null(replaceOldDatasetName) || length(replaceOldDatasetName) == 0 || (length(replaceOldDatasetName) == 1 && trimws(replaceOldDatasetName) == ""))
			# {
				# # Define the regular expression pattern to match any sequence of characters up to the first '-' (excluding '-')
				# pattern <- '(?<=\\")[^-]+'

				# # Extract the desired string using regmatches and regexpr
				# match_result <- regmatches(chunk, regexpr(pattern, chunk, perl = TRUE))
				
				# # Extract the dataset name from the match
				# if (length(match_result) > 0) {
				  # orig_dataset_name_extracted = gsub("datasetName='|'", "", match_result[[1]])
				# } else {
				  # orig_dataset_name_extracted <- NA
				# }
		
				# if(length(old_dataset_name) == 0 || old_dataset_name != orig_dataset_name_extracted)
				# {
					# print_dataset_info_flag = TRUE
				# }
				# else
				# {
					# print_dataset_info_flag = FALSE
				# }
				
				# old_dataset_name = orig_dataset_name_extracted
			# }

			# cat("\n\nOriginal Dataset (loaded from a R pkg) used in the script:",old_dataset_name)
			#,"\n==========================\n")
		}
		else
		{
			codeChunkNum = codeChunkNum + 1
			#cat("\nCommand to be executed\nBEGIN ORIGINAL COMMAND #",codeChunkNum,"\n\n",chunk,"\n\nEND ORIGINAL COMMAND #",codeChunkNum,"\n==========================\n")
		
			chunk_modified = chunk
			
			if(!is.null(currentDatasetName) && !is.na(currentDatasetName) && length(currentDatasetName) > 0 && trimws(currentDatasetName[1]) != "")
			{
				if(!is.null(old_dataset_name) && !is.na(old_dataset_name) && length(old_dataset_name) > 0 && trimws(old_dataset_name[1]) != "")
				{	
					if(print_dataset_info_flag == TRUE || codeChunkNum == 1)
					{
						print_dataset_info_flag = FALSE
						if(identical(trimws(old_dataset_name), trimws(currentDatasetName)))
						{
							cat("\nThe Active Dataset", currentDatasetName, ", the same name as the dataset used",old_dataset_name, "in the script\n")
							cat("The script will be run with the original Dataset",old_dataset_name,"(i.e., no dataset replacement will be performed in the script)\n\n")
						}
						else
						{
							#cat("\nOld dataset name:", old_dataset_name)
							cat("\nThe script will be run after replacing the Dataset:", old_dataset_name, "with the Dataset:",currentDatasetName,"\n")
						}
					}
					
					#cat("\n\nCurrent Dataset Name =", currentDatasetName, "Old dataset Name =", old_dataset_name, "\n\n") 
					#cat("\nCommand to be modified\nBEGIN Original COMMAND #",codeChunkNum,"\n\n",chunk,"\n\nEND Original COMMAND #",codeChunkNum,"\n==========================\n")
				
					if(!identical(trimws(old_dataset_name), trimws(currentDatasetName)))
					{
						num_subs = min(length(old_dataset_name), length(currentDatasetName))
						
						for(sub in 1:num_subs)
						{
							if(!identical(trimws(old_dataset_name[sub]), trimws(currentDatasetName[sub])))
							{
								chunk_modified = BSkyDatasetNameSubstitute(datasetName = old_dataset_name[sub], toDatasetName = currentDatasetName[sub], replaceOldColumnNames = replaceOldColumnNames, currentColumnNames = currentColumnNames, RcommandString = chunk_modified)
							}
						}
					}
					#cat("\nCommand to be executed\nBEGIN MODIFIED COMMAND #",codeChunkNum,"\n\n",chunk_modified,"\n\nEND MODIFIED COMMAND #",codeChunkNum,"\n==========================\n")
				}
				else if(length(old_dataset_name) == 0 || is.na(old_dataset_name))
				{
					if(print_dataset_info_flag == TRUE || codeChunkNum == 1)
					{
						print_dataset_info_flag = FALSE
						cat("\nUnable to determine the original Dataset name from the script. So, the script will be run with the original Dataset (i.e., no dataset replacement will be performed with",currentDatasetName,"in the script)","\n")
					}
				}
			}
			else
			{
				if(print_dataset_info_flag == TRUE || codeChunkNum == 1)
				{
					print_dataset_info_flag = FALSE
					
					if(length(old_dataset_name) == 0)
					{
						cat("\nUnable to determine the original Dataset name from the script. Anyway, the script will be run with the original Dataset","\n")
					}
					else
					{
						cat("\nThe script will be run with the original Dataset (i.e., no dataset replacement will be performed):", old_dataset_name,"\n")
					}
				}
			}
			
			#code_chunks_comment = paste0("BSkyFormat(",'"',code_chunks_comments[codeChunkCommentNum],'"',")")
			#code_chunks_comment = paste0("cat(",'"',code_chunks_comments[codeChunkCommentNum],'"',")")
			code_chunks_comment = paste0("Title:",code_chunks_comments[codeChunkCommentNum]) #Anil 10Apr24. used this instead of line abv for title
			#cat("\n",code_chunks_comment,"\n")
			
			code_chunk_list = c(code_chunk_list, chunk_header = list(code_chunks_comment))
			#code_chunk_list = c(code_chunk_list, chunk_new_line = list("BSkyFormat('\n')")) #Anil 10Apr24. Now we have title, this is not needed
			code_chunk_list = c(code_chunk_list, chunk_code = list(chunk_modified))
			
			###################################################################################################
			# Execute the script copied from BlueSky Statistics App as is without any dataset name substitution
			###################################################################################################
			#cmd_execution_status = BSkyEvalRcommand(RcommandString = chunk)
			
			#cat("\nCommand Execution Status : ", cmd_execution_status$executionStatus, "\n") if -1 that indicates failed execution 
		}
	}
	
	if(length(json_output_file_path) == 0)
	{
		return(invisible(code_chunk_list))
	}
	else
	{
		# Suppress warnings and info messages for require()
		suppressMessages(require(jsonlite))
		#suppressWarnings(require(jsonlite))
		
		#for(i in 1:length(code_chunk_list)){
		# dump the values into json file 
		#}
		
		if (!file.exists(json_output_file_path)) 
		{
			#print(json_output_file_path)
			# json_file_conn = file.create(json_output_file_path)
			# close(json_file_conn)
			#09Apr24 Anil commented above code because of error below
			#Error: no applicable method for 'close' applied to an object of class "logical"
			#instead used following code and it worked
			file.create(json_output_file_path)
		}
	
		# Convert the combined list to JSON format
		json_data <- toJSON(code_chunk_list, pretty = TRUE, auto_unbox = TRUE)

		#print(json_data)
		
		# Write the JSON data to a file
		write(json_data, json_output_file_path)

		#return(invisible())
		return(invisible(code_chunk_list))
	}
}


