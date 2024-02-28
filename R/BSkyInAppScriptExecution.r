
BSkyInAppScriptExecutionEngine <- function(bsky_script_full_file_path, json_output_file_path =c(), currentDatasetName = BSkyGetCurrentDatabaseName(), replaceOldDatasetName = c(), currentColumnNames = c(), replaceOldColumnNames = c())
{ 
	if (!file.exists(bsky_script_full_file_path)) 
	{ 
		cat("\n", bsky_script_full_file_path, "- BlueSky script file not found", "..exiting..\n")
		return(invisible)
	}
	
	# Get the extension string
	file_type <- tools::file_ext(bsky_script_full_file_path)
	
	if(file_type != "Bmd" && file_type != "Rmd") 
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
	
	if(file_type == "Bmd")
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
	
	if(file_type == "Bmd")
	{
		# Extract lines that start with '#'
		#code_chunks_comments <- grep("^#", rmd_content, value = TRUE)

		# Define the regular expression pattern to match everything after "output_title:"
		pattern <- '((Open Dataset:)\\s*([^"]+)\n)|((output_title:)\\s*"([^"]+)")' 
		#pattern <- '(^Open Dataset: (.*)$)|((output_title:)\\s*"([^"]+)")'
		
		# Extract the desired string using regmatches and regexpr
		code_chunks_comments <- regmatches(rmd_text, gregexpr(pattern, rmd_text))[[1]]
		
		# Remove the "output_title: " prefix and extra enclosure " around 
		#code_chunks_comments <- gsub('output_title: ', '', code_chunks_comments) 
		#code_chunks_comments <- gsub('"', '', code_chunks_comments)
		code_chunks_comments = gsub('(")|(\n)|(`)|(\\{r\\})|(output_title: )', '', code_chunks_comments)
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
	
	old_dataset_name = replaceOldDatasetName
	print_dataset_info_flag = FALSE
	code_chunk_list = list()
				
	codeChunkNum = 0
	codeChunkCommentNum = 0
						
	# Print the extracted R code chunks
	
	for (chunk in code_chunks) 
	{	
		codeChunkCommentNum = codeChunkCommentNum + 1
		
		if(regexpr(paste0("\nBSkyloadDataset", ".*"), trimws(chunk,which = "left", whitespace = "[ \t\r]"))[1] > 0)
		{
			#cat("\nSkipping BSkyloadDataset Command\n",chunk,"\n==========================\n")
			
			if(is.null(replaceOldDatasetName) || length(replaceOldDatasetName) == 0 || trimws(replaceOldDatasetName) == "")
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
				
				if(length(old_dataset_name) == 0 || old_dataset_name != orig_dataset_name_extracted)
				{
					print_dataset_info_flag = TRUE
				}
				else
				{
					print_dataset_info_flag = FALSE
				}
				
				old_dataset_name = orig_dataset_name_extracted
			}
			
			cat("\n\nOriginal Dataset (loaded from a data file) used in the script:",old_dataset_name)
			#,"\n==========================\n")
		}
		else if(regexpr(paste0("\nBSkyLoadRpkgDataset", ".*"), trimws(chunk,which = "left", whitespace = "[ \t\r]"))[1] > 0)
		{
			#cat("\nSkipping BSkyLoadRpkgDataset Command\n",chunk,"\n==========================\n")
			
			if(is.null(replaceOldDatasetName) || length(replaceOldDatasetName) == 0 || trimws(replaceOldDatasetName) == "")
			{
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
		
				if(length(old_dataset_name) == 0 || old_dataset_name != orig_dataset_name_extracted)
				{
					print_dataset_info_flag = TRUE
				}
				else
				{
					print_dataset_info_flag = FALSE
				}
				
				old_dataset_name = orig_dataset_name_extracted
			}

			cat("\n\nOriginal Dataset (loaded from a R pkg) used in the script:",old_dataset_name)
			#,"\n==========================\n")
		}
		else
		{
			codeChunkNum = codeChunkNum + 1
			#cat("\nCommand to be executed\nBEGIN ORIGINAL COMMAND #",codeChunkNum,"\n\n",chunk,"\n\nEND ORIGINAL COMMAND #",codeChunkNum,"\n==========================\n")
		
			chunk_modified = chunk
			
			if(!is.null(currentDatasetName) && !is.na(currentDatasetName) && length(currentDatasetName) > 0 && trimws(currentDatasetName) != "")
			{
				if(!is.null(old_dataset_name) && !is.na(old_dataset_name) && length(old_dataset_name) > 0 && trimws(old_dataset_name) != "")
				{	
					if(print_dataset_info_flag == TRUE || codeChunkNum == 1)
					{
						print_dataset_info_flag = FALSE
						if(trimws(old_dataset_name) == trimws(currentDatasetName))
						{
							cat("\nThe Active Dataset is ", currentDatasetName, ", the same name as the dataset used",old_dataset_name, "in the script\n")
							cat("The script will be run with the original Dataset",old_dataset_name,"(i.e., no dataset replacement will be performed in the script)\n\n")
						}
						else
						{
							cat("\nThe script will be run after replacing the Dataset:", old_dataset_name, "with the Dataset:",currentDatasetName,"\n")
						}
					}
					
					#cat("\n\nCurrent Dataset Name =", currentDatasetName, "Old dataset Name =", old_dataset_name, "\n\n") 
					#cat("\nCommand to be modified\nBEGIN Original COMMAND #",codeChunkNum,"\n\n",chunk,"\n\nEND Original COMMAND #",codeChunkNum,"\n==========================\n")
				
					if(trimws(old_dataset_name) != trimws(currentDatasetName))
					{
						chunk_modified = BSkyDatasetNameSubstitute(datasetName = old_dataset_name, toDatasetName = currentDatasetName, replaceOldColumnNames = replaceOldColumnNames, currentColumnNames = currentColumnNames, RcommandString = chunk)
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
			code_chunks_comment = paste0("cat(",'"',code_chunks_comments[codeChunkCommentNum],'"',")")
			#cat("\n",code_chunks_comment,"\n")
			
			code_chunk_list = c(code_chunk_list, chunk_header = list(code_chunks_comment))
			code_chunk_list = c(code_chunk_list, chunk_new_line = list("BSkyFormat('\n')"))
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
			json_file_conn = file.create(json_output_file_path)
			close(json_file_conn)
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


