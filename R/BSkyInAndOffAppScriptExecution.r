## 12Dec2023 
## New function added for BlueSky script(exported from BSky app) automation. Although it is a generic function
BSkySplitCollapseDatasetWithRepeatingColumns <- function(datasetNameStr = c(), removeColsWithConstant = FALSE, splitDatsetSuffix = '', columnGpLength = 0, outputColumnNames = '', collapseDataset = FALSE, collapseGpIDColFirst = TRUE, collapseGpIDPrefix ='', collapseGpIDColName="OrigDatasetID", makeCollapseGpIDColFactor = TRUE, complete.cases = TRUE)
{
	if(is.null(columnGpLength))
	{
		columnGpLength =""
	}
	
	if(is.null(datasetNameStr) || datasetNameStr == '' || !exists(datasetNameStr))
	{
		cat("\nDataset:", datasetNameStr, "cannot be split or collapsed as it does not exist\n")
		return(invisible())
	}
	
	datasetObj = eval(parse(text = paste(datasetNameStr)))
	
	if(!is.data.frame(datasetObj))
	{
		cat("\nDataset:", datasetNameStr, "cannot be split or collapsed as it is not a data frame\n")
		return(invisible())
	}
	
	datasetObj_non_empty_cols = datasetObj %>% select_if(~any(!is.na(.)))
	
	if(removeColsWithConstant == TRUE)
	{
		# Identify columns with constant values# Identify columns with constant values
		constant_columns <- sapply(datasetObj_non_empty_cols, function(col) length(unique(tolower(col))) == 1)
		
		# Remove columns with constant values
		datasetObj_non_empty_cols <- datasetObj_non_empty_cols[, !constant_columns]
	}
	
	#print(head(datasetObj_non_empty_cols))
	#BSkyLoadRefresh('datasetObj_non_empty_cols')
	
	column_names = names(datasetObj_non_empty_cols)
	
	cleaned_string <- sub("\\.{3}\\d+$", "", column_names)
	
	if(columnGpLength == 0)
	{
		spilt_col_indices = grep(cleaned_string[1],cleaned_string)
	}
	else
	{
		if(trimws(columnGpLength) == '' || columnGpLength > length(column_names))
		{
			columnGpLength = length(column_names)
		}
		
		num_of_repeating_col_gp = length(column_names)/columnGpLength
	
		if((num_of_repeating_col_gp - floor(num_of_repeating_col_gp)) > 0)
		{
			#print(num_of_repeating_col_gp)
			#print(floor(num_of_repeating_col_gp))
			cat("\nDataset:", datasetNameStr, "cannot be split or collapsed as the repeating column groups do not contain the same number of columns\n")
			return(invisible())
		}
		
		outputColumnNames = trimws(outputColumnNames)
		if(outputColumnNames[1] != '')
		{
			cleaned_string = make.names(outputColumnNames)
			
			if(any((outputColumnNames == cleaned_string) == FALSE))
			{
				cat("\nColumn names provided have been modified to comply with the dataset column name requirements\n")
				cat("Names provided:", paste(outputColumnNames, collapse=","),"\n")
				cat("Modified names:", paste(cleaned_string, collapse=","), "\n")
			}
		}
		
		cleaned_string = unique(cleaned_string)
		
		if(length(cleaned_string) < columnGpLength)
		{
			cleaned_string = c(cleaned_string, paste0("col_", seq(from = (length(cleaned_string)+1),((length(cleaned_string)+1)+(columnGpLength - length(cleaned_string))-1))))
		}
		
		spilt_col_indices = seq(from = 1, to = length(column_names), by =columnGpLength)
	}
	
	# cat("\n=====================\n")
	# print(spilt_col_indices)
	# print(cleaned_string)
	
	if(length(spilt_col_indices) > 1 || columnGpLength > 0)
	{
		if(columnGpLength > 0)
		{
			no_of_unque_cols = columnGpLength
		}
		else
		{
			no_of_unque_cols = spilt_col_indices[2] - spilt_col_indices[1]
		}
		
		num_of_repeating_col_gp = length(column_names)/no_of_unque_cols
	
		if((num_of_repeating_col_gp - floor(num_of_repeating_col_gp)) > 0)
		{
			cat("\nDataset:", datasetNameStr, "cannot be split or collapsed as the repeating column groups do not contain the same number of columns\n")
			return(invisible())
		}
		
		# Use indexing to create subdatasets
		subdatasets <- lapply(spilt_col_indices, function(indices) {
		  datasetObj_non_empty_cols[, (indices:(indices+no_of_unque_cols-1)), drop = FALSE]
		})
		
		splitDatsetSuffix = trimws(splitDatsetSuffix)
		
		if(collapseDataset == FALSE)
		{
			if(splitDatsetSuffix[1] == '')
			{
				names(subdatasets) = paste0(datasetNameStr, "_", seq_along(subdatasets))
			}
			else
			{
				if(length(subdatasets) <= length(splitDatsetSuffix))
				{
					names(subdatasets) = paste0(datasetNameStr, "_", seq_along(subdatasets), "_", splitDatsetSuffix[1:length(subdatasets)])
				}
				else
				{
					cat("Number of output dataset name suffix strings provided not enough for", length(subdatasets), "output datasets - auto generating using the first string\n")
					names(subdatasets) = paste0(datasetNameStr, "_", splitDatsetSuffix[1], "_", seq_along(subdatasets))
				}
			}
		}
		
		collapse_gp_name = ''
		if(trimws(collapseGpIDPrefix) != '')
		{
			collapse_gp_name = paste0(collapseGpIDPrefix, "_")
		}
		
		collapsed_df = data.frame()
		
		for (i in seq_along(subdatasets)) 
		{
			if(complete.cases == TRUE)
			{
				subdatasets[[i]] = subdatasets[[i]][complete.cases(subdatasets[[i]]), ]
			}
			
			names(subdatasets[[i]]) = cleaned_string[1:no_of_unque_cols]
			
			# Make column names unique
			names(subdatasets[[i]]) <- make.unique(names(subdatasets[[i]]))
			
			if(collapseDataset == FALSE)
			{
				eval(parse(text=paste(".GlobalEnv$",names(subdatasets)[i], "=", "subdatasets[[",i,"]]" )))
				BSkyLoadRefresh(names(subdatasets)[i])
			}
			else
			{
				subdatasets[[i]] = cbind(subdatasets[[i]], paste0(collapse_gp_name,i))
				collapsed_df = rbind(collapsed_df, subdatasets[[i]])
			}
		}
		
		if(collapseDataset == TRUE && dim(collapsed_df)[1] > 0)
		{
			if(splitDatsetSuffix[1] == '')
			{
				collapsed_dataset_name = paste0(datasetNameStr, "_1")
			}
			else
			{
				collapsed_dataset_name = paste0(datasetNameStr, "_", splitDatsetSuffix[1])
			}
			
			if(trimws(collapseGpIDColName) == '')
			{
				collapseGpIDColName="OrigDatasetID"
			}
			
			if(collapseGpIDColFirst == TRUE)
			{
				# Move the last column to be the first column
				collapsed_df = collapsed_df[, c(ncol(collapsed_df), 1:(ncol(collapsed_df)-1))]
				names(collapsed_df) = c(collapseGpIDColName, cleaned_string[1:no_of_unque_cols])
			}
			else
			{
				names(collapsed_df) = c(cleaned_string[1:no_of_unque_cols], collapseGpIDColName)
			}
			
			# Make column names unique
			names(collapsed_df) <- make.unique(names(collapsed_df))
			
			if(makeCollapseGpIDColFactor == TRUE)
			{
				#collapsed_df[collapseGpIDColName] = factor(collapsed_df[, length(names(collapsed_df))])
				collapsed_df[collapseGpIDColName] = factor(collapsed_df[[collapseGpIDColName]])
			}
			
			eval(parse(text=paste(".GlobalEnv$", collapsed_dataset_name, "=", "collapsed_df" )))
			BSkyLoadRefresh(collapsed_dataset_name)
		}
		
		if(collapseDataset == FALSE)
		{
			cat("\nDataset:", datasetNameStr,"has been split into", length(names(subdatasets)),"datasets\n")
			cat(paste(names(subdatasets),collapse=',\n'))
			cat("\n")
		}
		else
		{
			cat("\nDataset:", datasetNameStr,"has been collapsed with", length(subdatasets),"underlying datasets to\n")
			cat(collapsed_dataset_name)
			cat("\n")
		}
	}
	else
	{
		cat("\nDataset:", datasetNameStr, "cannot be split or collapsed as no repeating column group is found by analyzing the column names\n")
	}
	
	return(invisible())
}


## 16Dec2023 
## New function added for BlueSky script(exported from BSky app) automation 
BSkyScriptAutomationInternalEngine <- function(bsky_script_input_root_dir, bsky_script_output_root_dir, bsky_script_system_dir, bsky_delete_data_file = FALSE)
{ 
	if (!dir.exists(bsky_script_system_dir)) 
	{ 
		cat("\n", bsky_script_system_dir, "- BlueSKy System Automation script direcotry not found", "..exiting..\n")
		return(invisible)
	}
	
	#####################################################################################################
	# Setting up the output subdirectory to write HTML file names for every BlueSky Statistics Script run
	#####################################################################################################
	cur_timestamp = format(Sys.time(), "%Y%m%d_%H%M%S")
	
	BSkyScriptSystemSinkFileMgmt(bsky_script_system_dir, cur_timestamp = cur_timestamp, init = TRUE)
	
	BSkySetGraphicsDirPath(bskyGraphicsDirPath = bsky_script_system_dir)

	Incoming_file_open_msg_log_dir = paste0(bsky_script_system_dir,"\\logs")
	if (!dir.exists(Incoming_file_open_msg_log_dir)) 
	{
		# Create the directory if it does not exist
		dir.create(Incoming_file_open_msg_log_dir)
	}

	if (!dir.exists(bsky_script_output_root_dir)) 
	{
		# Create the directory if it does not exist
		dir.create(bsky_script_output_root_dir)
	}
	
	dst_html_css_lib_dir = paste0(bsky_script_output_root_dir, "\\", 'htmlCSSlib')
	src_html_css_lib_dir = paste0(bsky_script_system_dir, "\\", 'htmlCSSlib')
	
	if (!dir.exists(dst_html_css_lib_dir)) 
	{
		file.copy(src_html_css_lib_dir, bsky_script_output_root_dir, recursive=TRUE)
	}
	
	subdirectories = c()
	
	if (dir.exists(bsky_script_input_root_dir)) 
	{ 
		subdirectories <- list.dirs(bsky_script_input_root_dir, recursive = FALSE, full.names = TRUE)
	}
	
	if(length(subdirectories) > 0)
	{
		list_across_all_script_dataset_full_file_paths = c()
		sink_file_conn = NULL
					
		for(input_dir in subdirectories)
		{
			#cat("\nprocessing input script directory:", input_dir, "\n")
			
			bsky_script_output_dir = paste(bsky_script_output_root_dir, "\\", basename(input_dir), sep='')
			
			if (!dir.exists(bsky_script_output_dir)) 
			{
				# Create the directory if it does not exist in the root output directory folder
				dir.create(bsky_script_output_dir)
			}
			
			# Determine whether the script in the current directory needs to be processed (i.e. active state or disabled)
			process_script = TRUE
			
			# Deternine if any addtional script configuration file present to indicate whether to process incoming data files
			process_incoming_datafile = FALSE
			
			# If the datafile will be picked up from an incoming datafile directory, use the script dir as default 
			incoming_datafile_dir = input_dir
			
			# dataset used to generate the script. This is needed to replace the dataset name with incoming datafile name
			script_dataset_name = ""						
			
			#pattern="script.config.*.txt"
			script_cfg_file = list.files(path = input_dir, pattern=paste0(".*", "config", ".*\\.txt$"), ignore.case = TRUE, full.names = TRUE)
			
			if(length(script_cfg_file) > 0)
			{
				script_cfg_entries = readLines(script_cfg_file[1], warn = FALSE)
				
				if(length(script_cfg_entries) > 0)
				{
					script_cfg_entry_index = grep("Script_Status", script_cfg_entries, ignore.case = TRUE)
					if(length(script_cfg_entry_index) > 0)
					{
						if(length(grep("=", script_cfg_entries[script_cfg_entry_index[1]])) > 0)
						{
							if(tolower(trimws(strsplit(script_cfg_entries[script_cfg_entry_index[1]], "=")[[1]][2])) != 'active')
							{
								process_script = FALSE		
							}
						}	
					}
				}
				
				#script_cfg_entries = readLines(script_cfg_file[1], warn = FALSE)
				if(length(script_cfg_entries) > 0)
				{
					script_cfg_entry_index = grep("Use_Incoming_Datafiles", script_cfg_entries, ignore.case = TRUE)
					if(length(script_cfg_entry_index) > 0)
					{
						if(length(grep("=", script_cfg_entries[script_cfg_entry_index[1]])) > 0)
						{
							if(tolower(trimws(strsplit(script_cfg_entries[script_cfg_entry_index[1]], "=")[[1]][2])) == 'yes')
							{
								process_incoming_datafile = TRUE
								
								script_cfg_entry_index = grep("Incoming_Datafiles_Dir", script_cfg_entries, ignore.case = TRUE)
								
								if(length(script_cfg_entry_index) > 0)
								{
									if(length(grep("=", script_cfg_entries[script_cfg_entry_index[1]])) > 0)
									{
										incoming_datafile_dir = trimws(strsplit(script_cfg_entries[script_cfg_entry_index[1]], "=")[[1]][2])
											
										if(incoming_datafile_dir == "")
										{
											incoming_datafile_dir = input_dir
										}
										else
										{
											script_cfg_entry_index = grep("DatasetName_Used_In_The_Script", script_cfg_entries, ignore.case = TRUE)
											if(length(script_cfg_entry_index) > 0)
											{
												if(length(grep("=", script_cfg_entries[script_cfg_entry_index[1]])) > 0)
												{
													script_dataset_name = trimws(strsplit(script_cfg_entries[script_cfg_entry_index[1]], "=")[[1]][2])
												}
											}
										}
									}
								}
							}
						}	
					}
				}
			}
			
			# cat("\n======================\n")
			# cat("\nprocess_script: ", process_script,"\n")
			# cat("\nprocess_incoming_datafile: ", process_incoming_datafile,"\n")
			# cat("\nscript_dataset_name: ", script_dataset_name,"\n")
			# cat("\nincoming_datafile_dir: ", incoming_datafile_dir,"\n")
			
			#################################################################################################
			# Do not process (i.e. skip) this script subdirectory if process_script is determined to be FALSE
			#################################################################################################
			if(process_script == FALSE)
			{
				cat("\nSkipping processing of the script (marked inactive) in:", input_dir, "\n")
				next #skip this iteration of subdirectory and move to the next subdirectory in the for loop 
			}
			
			script_full_names = list.files(path = input_dir, pattern=paste0(".*", "script", ".*\\.Rmd$"), ignore.case = TRUE, full.names = TRUE)
			
			if(length(script_full_names) > 0)
			{
				#################################
				# Get file name without extension
				#################################
				script_file_name_without_ext <- tools::file_path_sans_ext(basename(script_full_names[1]))
				script_file_name_without_ext = make.names(script_file_name_without_ext)
				script_file_name_without_ext = gsub("\\.","_", script_file_name_without_ext)
			
				rmd_content = readLines(script_full_names[1], warn = FALSE)

				# Combine lines into a single string
				rmd_text <- paste(rmd_content, collapse = "\n")

				# Extract all text between "```{r}" and "```" for all occurrences (excluding the enclosures)
				code_chunks <- regmatches(rmd_text, gregexpr("```\\{r\\}(.*?)```", rmd_text))[[1]]

				# Remove the enclosures from each code chunk
				code_chunks <- gsub("```\\{r\\}|```", "", code_chunks)
				
				# Extract all text between "```" and "```{r}" title comment for the code chunk all chunk occurrences
				code_chunks_comments <- regmatches(rmd_text, gregexpr("```(.*?)```\\{r\\}", rmd_text))[[1]]
				extract_first_comment = regmatches(code_chunks_comments[1], gregexpr("```\\{r(.*?)```", code_chunks_comments[1]))[[1]]
				code_chunks_comments[1] = gsub('(```|\\{r\\})',"",substr(code_chunks_comments[1], sum(nchar(extract_first_comment)), nchar(code_chunks_comments[1])))
				
				# Remove the enclosures from each chunk comment
				code_chunks_comments <- gsub("```\\{r\\}|```", "", code_chunks_comments)
				
				# For debugging
				#print(code_chunks_comments)
				
				
				if(process_script == TRUE && process_incoming_datafile == TRUE)
				{
					bsky_script_output_subdir = paste(bsky_script_output_dir,"\\OutputFiles_", script_file_name_without_ext, "_", cur_timestamp, sep='')
					bsky_script_output_file = paste(bsky_script_output_subdir,"\\","BSkyReport", "_", script_file_name_without_ext, "_", cur_timestamp, ".htm", sep='')
				
					if (!dir.exists(bsky_script_output_subdir)) 
					{
						# Create the directory if it does not exist
						dir.create(bsky_script_output_subdir)
					}
					
					# Copy the pre section from the HTML template file into the output HTML file
					file.copy(paste(bsky_script_system_dir, "\\template_pre.html", sep=''), bsky_script_output_file)
					
					codeChunkNum = 0
					list_script_dataset_full_file_paths = c()
					
					# Print the extracted R code chunks
					for (chunk in code_chunks) 
					{		
						delete_left_over_sink_svgs = list.files(path = bsky_script_system_dir, pattern=".*.svg|.txt", ignore.case = TRUE, full.names = TRUE)
						
						if(length(delete_left_over_sink_svgs) > 0)
						{
							unlink(delete_left_over_sink_svgs)
						}
							
						############################################################
						#  Open a sink file to redirect the stdout and stderr output
						############################################################
						#sink(file = paste(bsky_script_system_dir,"\\","BSkysink.txt", sep=''), append = FALSE, type = c("output", "message"), split = FALSE)
						
						sink_file_conn = file(paste(bsky_script_system_dir,"\\","BSkysink.txt", sep=''))
						
						# close the system level script files in bsky_script_system_dir\logs directory
						#suppressWarnings(sink())
						sink()
						
						sink(file = sink_file_conn, append = TRUE, type = c("output"), split = FALSE)
						sink(file = sink_file_conn, append = TRUE, type = c("message"), split = FALSE)
								
						#######################################################################################
						#  Open a SVG graphics device to collect the graphics files for each dataset processed
						#######################################################################################
						SvgFileName = "BSkyRplot%03d.svg"
						svg(paste(bsky_script_system_dir,"\\",SvgFileName, sep=''))
						
						if(regexpr(paste0("\nBSkyloadDataset", ".*"), trimws(chunk,which = "left", whitespace = "[ \t\r]"))[1] > 0)
						{
							#cat("\nI am here ==============\n")
							#gsub("BSkyloadDataset\\(fullpathfilename='[^=]+([\\\\/][^.,])+", "BSkyloadDataset(fullpathfilename='c:\\\\path to\\\\abc\\\\hell 1\\1", xyz)
							#script_dataset_name = sub('.*[\\\\/]+([^/\']+)\'.*', '\\1', chunk)
							
							BSkyloadDataset_string_pattern = "BSkyloadDataset\\(fullpathfilename=\\'.*[\\\\/]+([^/\\']+)'.*?"
							matches_positions <- gregexpr(BSkyloadDataset_string_pattern, chunk)
							all_matches <- regmatches(chunk, matches_positions)
							script_dataset_names = sub('.*[\\\\/]+([^/\']+)\'.*', '\\1', all_matches[[1]])
							
							#print(script_dataset_names)
							#cat("\n++++++++++++++++++++++++++++++++++++++++\n")
							
							if(length(script_dataset_names) > 0)
							{
								for(dataset_index in 1:length(script_dataset_names))
								{
									full_script_dataset_file_path = paste0(incoming_datafile_dir, "\\", script_dataset_names[dataset_index])
									
									if(file.exists(full_script_dataset_file_path))
									{
										BSkyloadDataset_to_replace <- gsub('\\\\', '\\\\\\\\', all_matches[[1]][dataset_index])
										BSkyloadDataset_to_replace <- gsub('\\(', '\\\\(', BSkyloadDataset_to_replace)
										
										full_script_dataset_file_path_prep =paste0("BSkyloadDataset(fullpathfilename='",full_script_dataset_file_path,"'")
										#full_script_dataset_file_path_prep <- gsub('\\\\', '\\\\\\\\', full_script_dataset_file_path_prep)
										full_script_dataset_file_path_prep <- gsub('\\\\', '/', full_script_dataset_file_path_prep)
										full_script_dataset_file_path_prep <- gsub('\\(', '\\\\(', full_script_dataset_file_path_prep)
										
										#print(chunk)
										
										chunk = gsub(BSkyloadDataset_to_replace, full_script_dataset_file_path_prep, chunk)
										
										list_script_dataset_full_file_paths = c(list_script_dataset_full_file_paths, full_script_dataset_file_path)
										#print(chunk)
									}
									else
									{
										cat("\nInfo:", script_dataset_names[dataset_index], "data file not found in the incoming file folder", incoming_datafile_dir, "- no file path replacement is performed in the script. It will attempt to open the data file at the original location when the script was generated from the BlueSky Statistics app.\n")
									}
								}
							}
						}
						
						###################################################################################################
						# Execute the script copied from BlueSky Statistics App as is without any dataset name substitution
						###################################################################################################
						cmd_execution_status = BSkyEvalRcommand(RcommandString = chunk)
						
						#cat("\nCommand Execution Status : ", cmd_execution_status$executionStatus, "\n") if -1 that indicates failed execution 
						
						
						################################
						# Close the SVG graphics device
						################################
						dev.off()
						
						##################################
						#  Close the chunk level sink file 
						##################################
						sink()
						
						#######################################################
						# Open back the bsky_script_system_dir\logs sink files 
						#######################################################
						BSkyScriptSystemSinkFileMgmt(bsky_script_system_dir, cur_timestamp)
						
						codeChunkNum = codeChunkNum + 1
						
						# Generate the HTML output file from the script
						BSkyWriteKableHtmlOutput(datasetName = script_file_name_without_ext, dirName = bsky_script_output_subdir, fileName = bsky_script_output_file, timeStamp = cur_timestamp, codeChunkNum = codeChunkNum, codeChunkCmt = code_chunks_comments[codeChunkNum])
					
						# No incoming datafile is used - so no datafile to move
						#file.copy(full_datafile_names[file_counter], paste(bsky_script_output_subdir,"\\", datafile_names[file_counter], sep=''))
						#file.remove(full_datafile_names[file_counter])
						
						delete_left_over_svgs = list.files(path = bsky_script_system_dir, pattern=".*.svg", ignore.case = TRUE, full.names = TRUE)
						if(length(delete_left_over_svgs) > 0)
						{
							unlink(delete_left_over_svgs)
						}
					}

					# Copy the post section from the HTML template file into the output HTML file
					html_closing_block <- readLines(paste(bsky_script_system_dir, "\\template_post.html", sep=''))
					outputHTMLfileConn = file(bsky_script_output_file, "a")

					# Write the combined lines to a new file
					writeLines(html_closing_block, outputHTMLfileConn)
					close(outputHTMLfileConn)
					
					# Move the datafile to the output datafile subdirectory from the input directory
					bsky_script_output_datafiles_subdir = paste(bsky_script_output_subdir,"\\datafile", sep='')
				
					if (!dir.exists(bsky_script_output_datafiles_subdir)) 
					{
						#Create the directory if it does not exist
						dir.create(bsky_script_output_datafiles_subdir)
					}
					
					#full_datafile_names = list.files(path = incoming_datafile_dir, pattern=".*.csv|.xls|.xlsx", ignore.case = TRUE, full.names = TRUE)
					#file.copy(full_datafile_names, file.path(bsky_script_output_datafiles_subdir, basename(full_datafile_names)))
					
					
					list_script_dataset_full_file_paths = unique(list_script_dataset_full_file_paths)
					#cat("\n============Copying files=============\n")
					#print(list_script_dataset_full_file_paths)
					file.copy(list_script_dataset_full_file_paths, file.path(bsky_script_output_datafiles_subdir, basename(list_script_dataset_full_file_paths)))
					
					list_across_all_script_dataset_full_file_paths = c(list_across_all_script_dataset_full_file_paths, list_script_dataset_full_file_paths)
					# for (file_name in full_datafile_names) 
					# {
						# print(file_name)
						# print(file.path(bsky_script_output_datafiles_subdir, file_name))
						# file.copy(file_name, file.path(bsky_script_output_datafiles_subdir, basename(file_name)))
					# }
					
					#file.remove(full_datafile_names) 
				}
				else if(process_script == TRUE)
				{
					bsky_script_output_subdir = paste(bsky_script_output_dir,"\\OutputFiles_", script_file_name_without_ext, "_", cur_timestamp, sep='')
					bsky_script_output_file = paste(bsky_script_output_subdir,"\\","BSkyReport", "_", script_file_name_without_ext, "_", cur_timestamp, ".htm", sep='')
				
					if (!dir.exists(bsky_script_output_subdir)) 
					{
						# Create the directory if it does not exist
						dir.create(bsky_script_output_subdir)
					}
					
					# Copy the pre section from the HTML template file into the output HTML file
					file.copy(paste(bsky_script_system_dir, "\\template_pre.html", sep=''), bsky_script_output_file)
					
					codeChunkNum = 0
					
					# Print the extracted R code chunks
					for (chunk in code_chunks) 
					{		
						delete_left_over_sink_svgs = list.files(path = bsky_script_system_dir, pattern=".*.svg|.txt", ignore.case = TRUE, full.names = TRUE)
						if(length(delete_left_over_sink_svgs) > 0)
						{
							unlink(delete_left_over_sink_svgs)
						}
							
						############################################################
						#  Open a sink file to redirect the stdout and stderr output
						############################################################
						#sink(file = paste(bsky_script_system_dir,"\\","BSkysink.txt", sep=''), append = FALSE, type = c("output", "message"), split = FALSE)
						
						sink_file_conn = file(paste(bsky_script_system_dir,"\\","BSkysink.txt", sep=''))
						
						####################################################
						# close the bsky_script_system_dir\logs sink files()
						####################################################
						#suppressWarnings(sink())
						sink()
						
						sink(file = sink_file_conn, append = TRUE, type = c("output"), split = FALSE)
						sink(file = sink_file_conn, append = TRUE, type = c("message"), split = FALSE)
								
						#######################################################################################
						#  Open a SVG graphics device to collect the graphics files for each dataset processed
						#######################################################################################
						SvgFileName = "BSkyRplot%03d.svg"
						svg(paste(bsky_script_system_dir,"\\",SvgFileName, sep=''))
						
						###################################################################################################
						# Execute the script copied from BlueSky Statistics App as is without any dataset name substitution
						###################################################################################################
						cmd_execution_status = BSkyEvalRcommand(RcommandString = chunk)
						
						#cat("\nCommand Execution Status : ", cmd_execution_status$executionStatus, "\n") if -1 that indicates failed execution 
						
						
						################################
						# Close the SVG graphics device
						################################
						dev.off()
						
						##################################
						#  Close the chunk level sink file 
						##################################
						sink()
						
						#######################################################
						# Open back the bsky_script_system_dir\logs sink files 
						#######################################################
						BSkyScriptSystemSinkFileMgmt(bsky_script_system_dir, cur_timestamp)
						
						codeChunkNum = codeChunkNum + 1
						
						# Generate the HTML output file from the script
						BSkyWriteKableHtmlOutput(datasetName = script_file_name_without_ext, dirName = bsky_script_output_subdir, fileName = bsky_script_output_file, timeStamp = cur_timestamp, codeChunkNum = codeChunkNum, codeChunkCmt = code_chunks_comments[codeChunkNum])
					
						# No incoming datafile is used - so no datafile to move
						#file.copy(full_datafile_names[file_counter], paste(bsky_script_output_subdir,"\\", datafile_names[file_counter], sep=''))
						#file.remove(full_datafile_names[file_counter])
						
						delete_left_over_svgs = list.files(path = bsky_script_system_dir, pattern=".*.svg", ignore.case = TRUE, full.names = TRUE)
						if(length(delete_left_over_svgs) > 0)
						{
							unlink(delete_left_over_svgs)
						}
					}

					# Copy the post section from the HTML template file into the output HTML file
					html_closing_block <- readLines(paste(bsky_script_system_dir, "\\template_post.html", sep=''))
					outputHTMLfileConn = file(bsky_script_output_file, "a")

					# Write the combined lines to a new file
					writeLines(html_closing_block, outputHTMLfileConn)
					close(outputHTMLfileConn)
				}
			}
		}
		
		#print(list_across_all_script_dataset_full_file_paths)
		# All script directories have been processed and if there is any data files in different incoming file, remove them
		if(bsky_delete_data_file && length(list_across_all_script_dataset_full_file_paths) > 0)
		{
			file.remove(list_across_all_script_dataset_full_file_paths)
		}
	}
}


## 12Dec2023 
## New function added for BlueSky script(exported from BSky app) automation 
BSkyScriptAutomationEngine <- function(bsky_script_input_root_dir, bsky_script_output_root_dir, bsky_script_system_dir, bsky_delete_data_file)
{
	BSkyScriptAutomationInternalEngine(bsky_script_input_root_dir, bsky_script_output_root_dir, bsky_script_system_dir, bsky_delete_data_file)
}


BSkyInAppScriptExtractDialogDataset <- function(rmd_text)
{
	# Define a function to extract "id" and "dataset" values from a string
	extract_values_fn <- function(str) {
	  id_match = regmatches(str, regexpr('id: "\\w+"', str))  # Extract "id" match
	  dataset_match = regmatches(str, regexpr('dataset: "\\w+"', str))  # Extract "dataset" match
	  
	  if (length(id_match) > 0 && length(dataset_match) > 0) {
		id = gsub('id: "|"', '', id_match)  # Extract "id" value
		dataset = gsub('dataset: "|"', '', dataset_match)  # Extract "dataset" value
		return(c(id = id, dataset = dataset))
	  } else {
		return(c(id = NA, dataset = NA))
	  }
	}
	
	dialog_pattern = 'dialog:.*\\n.*\\n\\s*dataset:\\s*"([^"]+)"'
	dialog_db = regmatches(rmd_text, gregexpr(dialog_pattern, rmd_text,perl = TRUE))[[1]]
	#print(dialog_db)

	# Apply the extract_values_fn function to each element of the string array
	result = sapply(dialog_db, extract_values_fn)

	# Create a data frame from the extracted values
	df = data.frame(t(result))
	
	#BSkyFormat(df)
	
	dataset_name_to_discard_df_freq = NULL
	
	dataset_name_to_discard = c("reRunOutput", "loadDatasetFromPackage", "saveAModel", "loadAModel")
	
	if(any(df$id %in% dataset_name_to_discard))
	{
		dataset_name_to_discard_df = df[(df$id %in% dataset_name_to_discard), ]
		row.names(dataset_name_to_discard_df) = c()
		
		dataset_name_to_discard_df_freq = data.frame(table(dataset_name_to_discard_df$dataset))
		#BSkyFormat(dataset_name_to_discard_df_freq)
	}
	
	return(invisible(dataset_name_to_discard_df_freq))
}


BSkyInAppScriptExtractOldDatasetList <- function(bsky_script_full_file_path = c(), json_output_file_path =c(), rmd_text=c(), file_type = "BMD", debug = FALSE)
{
	if(length(rmd_text) == 0)
	{
		if (!file.exists(bsky_script_full_file_path)) 
		{ 
			cat("\n", bsky_script_full_file_path, "- BlueSky script file not found", "..exiting..\n")
			return(invisible)
		}
		
		# Get the extension string
		file_type = toupper(tools::file_ext(bsky_script_full_file_path))
		
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
			zip_file_path = bsky_script_full_file_path
			script_file_name_without_ext = tools::file_path_sans_ext(basename(bsky_script_full_file_path))
			
			# Open a connection to the zip file
			zip_connection = unz(zip_file_path, script_file_name_without_ext)
			
			# Read the lines from the text file inside the zip
			rmd_content = read_lines(zip_connection)
			
			# Close the zip connection
			#close(zip_connection)
		}
		else
		{
			rmd_content = readLines(bsky_script_full_file_path, warn = FALSE)
		}

		# Combine lines into a single string
		rmd_text = paste(rmd_content, collapse = "\n")
		
		######################
		# SK for testing only 
		######################
		
		#Extract all text between "```{r}", "```{dialog}", "```{console}"and "```" for all occurrences
		#desired_blocks <- regmatches(rmd_text, gregexpr("```\\{r\\}|```\\{dialog\\}|```\\{console\\}", rmd_text))[[1]]
		desired_blocks = regmatches(rmd_text, gregexpr("```\\{r\\}(.*?)```|```\\{dialog\\}(.*?)```|```\\{console\\}(.*?)```", rmd_text))[[1]]
		#print(desired_blocks)
		
		# Combine lines into a single string
		desired_blocks = paste(desired_blocks, collapse = "\n")
		
		rmd_text = desired_blocks
		##########################
		##########################
	}

	if(file_type == "BMD")
	{
		#cat(rmd_text)
		
		#dataset_names_pattern = '(dataset:\\s*"([^"]+)")|(dataset:\\s*([^"]+)\\))'
		#dataset_names_pattern = '(\\bdataset:\\s*"([^"]+)")|(\\bdataset:[^)]+)'
		#dataset_names_pattern = 'dataset:\\s*"([^"]+)"|\\bdataset:[^)]+'
		 #dataset_names_pattern = 'dataset:\\s*(?:"([^"]+)"|([^"\\n()]+))'
		#dataset_names_pattern =  '\\(dataset:([^)"\\n]+)\\)'
		
		dataset_names_pattern = 'dataset:\\s*(?:"([^"]+)"|([^"()]+))'
		dataset_names = regmatches(rmd_text, gregexpr(dataset_names_pattern, rmd_text))[[1]]
		dataset_names = trimws(gsub('dataset:|["()]','',dataset_names))
		# dataset_names = trimws(gsub('name:|["()]','',dataset_names))
		# dataset_names = dataset_names[1]
		
		# cat("\nBefore removing unwanted dataset names\n")
		# print(dataset_names)
		
		discard_df_freq = BSkyInAppScriptExtractDialogDataset(rmd_text)
		
		if(!is.null(discard_df_freq))
		{
			for(i in 1:dim(discard_df_freq)[1])
			{
				# Find the indices of the occurrences of the string to remove
				indices_to_remove = which(dataset_names == discard_df_freq[i,1])

				# Check if the number of occurrences to remove is less than or equal to the actual occurrences
				if (discard_df_freq[i,2] <= length(indices_to_remove)) {
					# Remove the specified number of occurrences
					dataset_names = dataset_names[-indices_to_remove[1:discard_df_freq[i,2]]]
				} else {
					# If the number of occurrences to remove is greater than the actual occurrences, print a warning
					print("Number of occurrences of dataset names to remove exceeds the actual occurrences")
				}
			}
		}
		
		#cat("\nAfter removing unwanted dataset names\n")
		#print(dataset_names)
		
		dataset_names = sort(unique(dataset_names))
	}
	
	if(length(json_output_file_path) == 0)
	{
		return(invisible(dataset_names))
	}
	else
	{
		# Suppress warnings and info messages for require()
		suppressMessages(require(jsonlite))
		
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
		
		datasetname = list(datasetname = dataset_names)
		# Convert the combined list to JSON format
		json_data <- toJSON(datasetname, pretty = TRUE, auto_unbox = TRUE)

		# print(json_data)
		
		# Write the JSON data to a file
		write(json_data, json_output_file_path)

		#return(invisible())
		#return(invisible(dataset_names))
	}
}


BSkyInAppScriptExecutionEngine.Retired <- function(bsky_script_full_file_path, json_output_file_path =c(), currentDatasetName = BSkyGetCurrentDatabaseName(), replaceOldDatasetName = c(), currentColumnNames = c(), replaceOldColumnNames = c(), expand_extraction = TRUE, debug = FALSE)
{ 
	if (!file.exists(bsky_script_full_file_path)) 
	{ 
		cat("\n", bsky_script_full_file_path, "- BlueSky script file not found", "..exiting..\n")
		return(invisible)
	}
	
	# Get the extension string
	file_type = toupper(tools::file_ext(bsky_script_full_file_path))
	
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
		zip_file_path = bsky_script_full_file_path
		script_file_name_without_ext = tools::file_path_sans_ext(basename(bsky_script_full_file_path))

		# Open a connection to the zip file
		zip_connection = unz(zip_file_path, script_file_name_without_ext)

		# Read the lines from the text file inside the zip
		rmd_content = read_lines(zip_connection)

		# Close the zip connection
		#close(zip_connection)
	}
	else
	{
		rmd_content = readLines(bsky_script_full_file_path, warn = FALSE)
	}

	# Combine lines into a single string
	rmd_text = paste(rmd_content, collapse = "\n")
	
	######################
	# SK for testing only 
	######################
	
	#Extract all text between "```{r}", "```{dialog}", "```{console}"and "```" for all occurrences
	#desired_blocks = regmatches(rmd_text, gregexpr("```\\{r\\}|```\\{dialog\\}|```\\{console\\}", rmd_text))[[1]]
	desired_blocks = regmatches(rmd_text, gregexpr("```\\{r\\}(.*?)```|```\\{dialog\\}(.*?)```|```\\{console\\}(.*?)```", rmd_text))[[1]]
	#print(desired_blocks)
	
	# Combine lines into a single string
	desired_blocks <- paste(desired_blocks, collapse = "\n")
	
	rmd_text = desired_blocks

	##########################
	##########################

	# Extract all text between "```{r}" and "```" for all occurrences (excluding the enclosures)
	code_chunks = regmatches(rmd_text, gregexpr("```\\{r\\}(.*?)```", rmd_text))[[1]]

	# Remove the enclosures from each code chunk
	code_chunks = gsub("```\\{r\\}|```", "", code_chunks)
	
	if(file_type == "BMD")
	{
		# Extract lines that start with '#'
		#code_chunks_comments = grep("^#", rmd_content, value = TRUE)

		# Define the regular expression pattern to match everything after "output_title: and within ```console block"
		#title_pattern = '(^Open Dataset: (.*)$)|((output_title:)\\s*"([^"]+)")'
		
		#title_pattern = '((Open Dataset:)\\s*([^"]+)\n)|((output_title:)\\s*"([^"]+)")' 
		title_pattern = '((Open Dataset:)\\s*([^"]+)\n)|((output_title:)\\s*"([^"]+)")|(```\\{console\\}(.*?)```)' 
		
		# Extract the desired string using regmatches and regexpr
		code_chunks_comments = regmatches(rmd_text, gregexpr(title_pattern, rmd_text))[[1]]
		
		# Remove the "output_title: " prefix and extra enclosure " around 
		#code_chunks_comments = gsub('output_title: ', '', code_chunks_comments) 
		#code_chunks_comments = gsub('"', '', code_chunks_comments)
		code_chunks_comments = gsub('(")|(\n)|(`)|(\\{r\\})|(output_title: )|(\\{console\\})', '', code_chunks_comments)
		
		# Define the regular expression pattern with exact case matching for "dataset"
		#dataset_name_and_console_comment_pattern <- '(dataset:\\s*"([^"]+)")|(```\\{console\\}(.*?)```)'
		dataset_name_and_console_comment_pattern = 'dataset:\\s*(?:"([^"]+)"|([^"\\n()]+))|(```\\{console\\}(.*?)```)'
		datasets_names_and_console_comments = regmatches(rmd_text, gregexpr(dataset_name_and_console_comment_pattern, rmd_text))[[1]]
		
		# #dataset_names_pattern = '(dataset:\\s*"([^"]+)")|(dataset:\\s*([^"]+)\\))'
		# #dataset_names_pattern = 'dataset:\\s*(?:"([^"]+)"|([^"\\n()]+))'
		# dataset_names_pattern =  'dataset:\\s*(?:"([^"]+)"|([^"()]+))'
		# dataset_names = regmatches(rmd_text, gregexpr(dataset_names_pattern, rmd_text))[[1]]
		# dataset_names = trimws(gsub('dataset:|["()]','',dataset_names))
		
		dataset_names = BSkyInAppScriptExtractOldDatasetList(rmd_text = rmd_text, file_type = file_type)
		
		if(length(code_chunks) > length(code_chunks_comments))
		{
			#Extract "```{r}", "```{dialog}", "```{console}"and "```" for all occurrences
			dialog_console_r_blocks = regmatches(rmd_text, gregexpr("```\\{r\\}|```\\{dialog\\}|```\\{console\\}", rmd_text))[[1]]
			
			if(debug)
			{
				cat("\nnumber of ```r code blocks\n")
				print(length(code_chunks))
				cat("\nnumber of code titles\n")
				print(length(code_chunks_comments))
				cat("\nnumber of blocks - dialog,r,console blocks\n")
				print(length(dialog_console_r_blocks))
			
				BSkyFormat(cbind(code_chunks_comments,datasets_names_and_console_comments))
				BSkyFormat(dialog_console_r_blocks)
			}
			
			code_chunks_comments_modified = c()
			datasets_names_and_console_comments_modified = c()
			
			multiple_r_blocks_found = FALSE
			k = 1
			
			for(j in 1:length(dialog_console_r_blocks))
			{	
				if(trimws(dialog_console_r_blocks[j]) == "```{r}")
				{
					if(multiple_r_blocks_found == TRUE)
					{
						#Seccond or more continuous occurance of a ```r block for a given dialog block
						code_chunks_comments_modified = c(code_chunks_comments_modified, "BSky_NO_TITLE")
						datasets_names_and_console_comments_modified = c(datasets_names_and_console_comments_modified, "BSky_NO_TITLE")	
					}
					else
					{
						#First occurance of a ```r block - do nothing 
						multiple_r_blocks_found = TRUE
					}
				}
				else if(trimws(dialog_console_r_blocks[j]) == "```{dialog}" || trimws(dialog_console_r_blocks[j]) == "```{console}")
				{
					code_chunks_comments_modified = c(code_chunks_comments_modified, code_chunks_comments[k])
					datasets_names_and_console_comments_modified = c(datasets_names_and_console_comments_modified, datasets_names_and_console_comments[k])
					
					k = k+1
					multiple_r_blocks_found = FALSE
				}
				else
				{
					cat("\n","UNNOWN BLOCK==",trimws(dialog_console_r_blocks[j]),"==","\n")
				}
			}
			
			if(debug)
			{
				BSkyFormat(cbind(code_chunks_comments_modified,datasets_names_and_console_comments_modified))
			}
			code_chunks_comments = code_chunks_comments_modified
			datasets_names_and_console_comments = datasets_names_and_console_comments_modified
		}	
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
	# cat("\nCODE CHUNK COMMENTS\n")
	# print(code_chunks_comments)
	# cat("\ndatasets_names_and_console_comments\n")
	# print(datasets_names_and_console_comments)
	if(debug)
	{
		BSkyFormat(cbind(code_chunks_comments,datasets_names_and_console_comments))
		cat("\nDATASET NAMES\n")
		print(dataset_names)
		#return()
	}
	
	if(is.null(replaceOldDatasetName) || length(replaceOldDatasetName) == 0 || (length(replaceOldDatasetName) == 1 && trimws(replaceOldDatasetName) == ""))
	{
		old_dataset_name = dataset_names
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
	
	num_subs = min(length(old_dataset_name), length(currentDatasetName))
	final_dataset_names = currentDatasetName[1:	num_subs]	
	if(length(old_dataset_name) > num_subs)
		final_dataset_names = c(final_dataset_names, old_dataset_name[(num_subs+1):length(old_dataset_name)])
	
	if(debug)
	{
		cat("\nFinal list of dataset names to be used in script code\n")
		print(final_dataset_names)
		cat("\n")
	}
					
	codeChunkNum = 0
	codeChunkCommentNum = 0
	
	for (chunk in code_chunks) 
	{	
		codeChunkCommentNum = codeChunkCommentNum + 1 
		
		if(regexpr(paste0("BSkyInAppScriptExecutionEngine", ".*"), trimws(chunk,which = "left", whitespace = "[ \t\r]"))[1] > 0)
		{
		}
		else if(regexpr(paste0("BSkyInAppScriptExtractOldDatasetList", ".*"), trimws(chunk,which = "left", whitespace = "[ \t\r]"))[1] > 0)
		{
		}
		else if(regexpr(paste0("\nBSkyloadDataset", ".*"), trimws(chunk,which = "left", whitespace = "[ \t\r]"))[1] > 0)
		{
		}
		else if(regexpr(paste0("\nBSkyLoadRpkgDataset", ".*"), trimws(chunk,which = "left", whitespace = "[ \t\r]"))[1] > 0)
		{
		}
		else
		{
			codeChunkNum = codeChunkNum + 1
			
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
							cat("\nThe script will be run after replacing the Dataset:", old_dataset_name, "with the Dataset:",currentDatasetName,"\n")
						}
					}
					
					if(!identical(trimws(old_dataset_name), trimws(currentDatasetName)))
					{
						num_subs = min(length(old_dataset_name), length(currentDatasetName))
						
						for(sub in 1:num_subs)
						{
							if(!identical(trimws(old_dataset_name[sub]), trimws(currentDatasetName[sub])))
							{
								chunk_modified = BSkyDatasetNameSubstitute(datasetName = old_dataset_name[sub], toDatasetName = currentDatasetName[sub], replaceOldColumnNames = replaceOldColumnNames, currentColumnNames = currentColumnNames, RcommandString = chunk_modified)
								datasets_names_and_console_comments[codeChunkCommentNum] = BSkyDatasetNameSubstitute(datasetName = old_dataset_name[sub], toDatasetName = currentDatasetName[sub], replaceOldColumnNames = replaceOldColumnNames, currentColumnNames = currentColumnNames, RcommandString = datasets_names_and_console_comments[codeChunkCommentNum])
							
							# cat("\nOLD DB: ", old_dataset_name[sub], "\n") 
							# cat("\nNEW DB: ", currentDatasetName[sub], "\n")
							# cat("\nBefore Sub: ",code_chunks_comments[codeChunkCommentNum],"\n")
								code_chunks_comments[codeChunkCommentNum] = BSkyDatasetNameSubstitute(datasetName = old_dataset_name[sub], toDatasetName = currentDatasetName[sub], replaceOldColumnNames = replaceOldColumnNames, currentColumnNames = currentColumnNames, RcommandString = code_chunks_comments[codeChunkCommentNum])
							#cat("\nAfter Sub: ",code_chunks_comments[codeChunkCommentNum],"\n")		
							}
						}
					}
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
			
			if(debug)
			{
				cat("\nBefore: ",code_chunks_comments[codeChunkCommentNum],"\n")
			}
			
			if(!(code_chunks_comments[codeChunkCommentNum] %in% c("Save Models to a file", "Load Models from a file")))
			{
				if((length(grep("dataset:", code_chunks_comments[codeChunkCommentNum])) == 0) && length(grep("dataset:", datasets_names_and_console_comments[codeChunkCommentNum])) > 0)
				{
					datasets_names_and_console_comments[codeChunkCommentNum] = gsub('"', '', datasets_names_and_console_comments[codeChunkCommentNum])	
					code_chunks_comments[codeChunkCommentNum] = paste0(code_chunks_comments[codeChunkCommentNum], " (", datasets_names_and_console_comments[codeChunkCommentNum], ")")
				}
			}
			
			code_chunks_comment = paste0("Title:",code_chunks_comments[codeChunkCommentNum]) #Anil 10Apr24. used this instead of line abv for title
			
			if(debug)
			{
				cat("\nAfter: ",code_chunks_comment,"\n")
			}
			
			code_chunk_list = c(code_chunk_list, chunk_header = list(code_chunks_comment))
			#code_chunk_list = c(code_chunk_list, chunk_new_line = list("BSkyFormat('\n')")) #Anil 10Apr24. Now we have title, this is not needed
			code_chunk_list = c(code_chunk_list, chunk_code = list(chunk_modified))
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


BSkyInAppBMDScriptExecutionEngine <- function(bsky_script_full_file_path, bmd_output_file_path =c(), currentDatasetName = BSkyGetCurrentDatabaseName(), replaceOldDatasetName = c(), currentColumnNames = c(), replaceOldColumnNames = c(), scriptSinkfile = FALSE, expand_extraction = TRUE, debug = FALSE)
{ 
	if (!file.exists(bsky_script_full_file_path)) 
	{ 
		cat("\n", bsky_script_full_file_path, "- BlueSky script file not found", "..exiting..\n")
		return(invisible)
	}
	
	# Get the extension string
	file_type = toupper(tools::file_ext(bsky_script_full_file_path))
	
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
		zip_file_path = bsky_script_full_file_path
		script_file_name_without_ext = tools::file_path_sans_ext(basename(bsky_script_full_file_path))

		# Open a connection to the zip file
		zip_connection = unz(zip_file_path, script_file_name_without_ext)

		# Read the lines from the text file inside the zip
		rmd_content = read_lines(zip_connection)

		# Close the zip connection
		#close(zip_connection)
	}
	else
	{
		rmd_content = readLines(bsky_script_full_file_path, warn = FALSE)
	}

	# Combine lines into a single string
	rmd_text = paste(rmd_content, collapse = "\n")
	
	######################
	# SK for testing only 
	######################
	
	#Extract all text between "```{r}", "```{dialog}", "```{console}"and "```" for all occurrences
	#desired_blocks = regmatches(rmd_text, gregexpr("```\\{r\\}|```\\{dialog\\}|```\\{console\\}", rmd_text))[[1]]
	desired_blocks = regmatches(rmd_text, gregexpr("```\\{r\\}(.*?)```|```\\{dialog\\}(.*?)```|```\\{console\\}(.*?)```", rmd_text))[[1]]
	#print(desired_blocks)
	
	# Combine lines into a single string
	rmd_text <- paste(desired_blocks, collapse = "\n")

	##########################
	##########################

	# Extract all text between "```{dialog}", "```{console}" and "```" for all occurrences (including the enclosures)
	dialog_console_blocks = regmatches(rmd_text, gregexpr("```\\{dialog\\}(.*?)```|```\\{console\\}(.*?)```", rmd_text))[[1]]
	
	# Extract all text between "```{dialog}" and "```" for all occurrences (including the enclosures)
	dialog_blocks = regmatches(rmd_text, gregexpr("```\\{dialog\\}(.*?)```", rmd_text))[[1]]
	
	# Extract all text between "```{console}" and "```" for all occurrences (including the enclosures)
	console_blocks = regmatches(rmd_text, gregexpr("```\\{console\\}(.*?)```", rmd_text))[[1]]
	
	# Extract all text between "```{r}" and "```" for all occurrences (excluding the enclosures)
	code_chunks = regmatches(rmd_text, gregexpr("```\\{r\\}(.*?)```", rmd_text))[[1]]

	# Remove the enclosures from each code chunk
	code_chunks = gsub("```\\{r\\}|```", "", code_chunks)
	
	if(file_type == "BMD")
	{
		# Extract lines that start with '#'
		#code_chunks_comments = grep("^#", rmd_content, value = TRUE)

		# Define the regular expression pattern to match everything after "output_title: and within ```console block"
		#title_pattern = '(^Open Dataset: (.*)$)|((output_title:)\\s*"([^"]+)")'
		
		#title_pattern = '((Open Dataset:)\\s*([^"]+)\n)|((output_title:)\\s*"([^"]+)")' 
		title_pattern = '((Open Dataset:)\\s*([^"]+)\n)|((output_title:)\\s*"([^"]+)")|(```\\{console\\}(.*?)```)' 
		
		# Extract the desired string using regmatches and regexpr
		code_chunks_comments = regmatches(rmd_text, gregexpr(title_pattern, rmd_text))[[1]]
		
		# Remove the "output_title: " prefix and extra enclosure " around 
		#code_chunks_comments = gsub('output_title: ', '', code_chunks_comments) 
		#code_chunks_comments = gsub('"', '', code_chunks_comments)
		code_chunks_comments = gsub('(")|(\n)|(`)|(\\{r\\})|(output_title: )|(\\{console\\})', '', code_chunks_comments)
		
		# Define the regular expression pattern with exact case matching for "dataset"
		#dataset_name_and_console_comment_pattern <- '(dataset:\\s*"([^"]+)")|(```\\{console\\}(.*?)```)'
		dataset_name_and_console_comment_pattern = 'dataset:\\s*(?:"([^"]+)"|([^"\\n()]+))|(```\\{console\\}(.*?)```)'
		datasets_names_and_console_comments = regmatches(rmd_text, gregexpr(dataset_name_and_console_comment_pattern, rmd_text))[[1]]
		
		# #dataset_names_pattern = '(dataset:\\s*"([^"]+)")|(dataset:\\s*([^"]+)\\))'
		# #dataset_names_pattern = 'dataset:\\s*(?:"([^"]+)"|([^"\\n()]+))'
		# dataset_names_pattern =  'dataset:\\s*(?:"([^"]+)"|([^"()]+))'
		# dataset_names = regmatches(rmd_text, gregexpr(dataset_names_pattern, rmd_text))[[1]]
		# dataset_names = trimws(gsub('dataset:|["()]','',dataset_names))
		
		dataset_names = BSkyInAppScriptExtractOldDatasetList(rmd_text = rmd_text, file_type = file_type)
		
		if(length(code_chunks) > length(code_chunks_comments))
		{
			#Extract "```{r}", "```{dialog}", "```{console}"and "```" for all occurrences
			dialog_console_r_blocks = regmatches(rmd_text, gregexpr("```\\{r\\}|```\\{dialog\\}|```\\{console\\}", rmd_text))[[1]]
			
			if(debug)
			{
				cat("\nnumber of ```r code blocks\n")
				print(length(code_chunks))
				cat("\nnumber of code titles\n")
				print(length(code_chunks_comments))
				cat("\nnumber of blocks - dialog,r,console blocks\n")
				print(length(dialog_console_r_blocks))
			
				BSkyFormat(cbind(code_chunks_comments,datasets_names_and_console_comments))
				BSkyFormat(dialog_console_r_blocks)
			}
			
			code_chunks_comments_modified = c()
			datasets_names_and_console_comments_modified = c()
			dialog_console_blocks_modified = c()
			
			multiple_r_blocks_found = FALSE
			k = 1
			
			for(j in 1:length(dialog_console_r_blocks))
			{	
				if(trimws(dialog_console_r_blocks[j]) == "```{r}")
				{
					if(multiple_r_blocks_found == TRUE)
					{
						#Seccond or more continuous occurance of a ```r block for a given dialog block
						code_chunks_comments_modified = c(code_chunks_comments_modified, "BSky_NO_TITLE")
						datasets_names_and_console_comments_modified = c(datasets_names_and_console_comments_modified, "BSky_NO_TITLE")	
						dialog_console_blocks_modified = c(dialog_console_blocks_modified, "BSKY_NO_DIALOG_CONSOLE_BLOCK")
					}
					else
					{
						#First occurance of a ```r block - do nothing 
						multiple_r_blocks_found = TRUE
					}
				}
				else if(trimws(dialog_console_r_blocks[j]) == "```{dialog}" || trimws(dialog_console_r_blocks[j]) == "```{console}")
				{
					code_chunks_comments_modified = c(code_chunks_comments_modified, code_chunks_comments[k])
					datasets_names_and_console_comments_modified = c(datasets_names_and_console_comments_modified, datasets_names_and_console_comments[k])
					dialog_console_blocks_modified = c(dialog_console_blocks_modified, dialog_console_blocks[k])
					
					k = k+1
					multiple_r_blocks_found = FALSE
				}
				else
				{
					cat("\n","UNNOWN BLOCK==",trimws(dialog_console_r_blocks[j]),"==","\n")
				}
			}
			
			if(debug)
			{
				BSkyFormat(cbind(code_chunks_comments_modified,datasets_names_and_console_comments_modified))
			}
			code_chunks_comments = code_chunks_comments_modified
			datasets_names_and_console_comments = datasets_names_and_console_comments_modified
			dialog_console_blocks = dialog_console_blocks_modified
		}	
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
	# cat("\nCODE CHUNK COMMENTS\n")
	# print(code_chunks_comments)
	# cat("\ndatasets_names_and_console_comments\n")
	# print(datasets_names_and_console_comments)
	if(debug)
	{
		BSkyFormat(cbind(code_chunks_comments,datasets_names_and_console_comments))
		cat("\nDATASET NAMES\n")
		print(dataset_names)
		#return()
	}
	
	if(is.null(replaceOldDatasetName) || length(replaceOldDatasetName) == 0 || (length(replaceOldDatasetName) == 1 && trimws(replaceOldDatasetName) == ""))
	{
		old_dataset_name = dataset_names
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
	
	num_subs = min(length(old_dataset_name), length(currentDatasetName))
	final_dataset_names = currentDatasetName[1:	num_subs]	
	if(length(old_dataset_name) > num_subs)
		final_dataset_names = c(final_dataset_names, old_dataset_name[(num_subs+1):length(old_dataset_name)])
	
	if(debug)
	{
		cat("\nFinal list of dataset names to be used in script code\n")
		print(final_dataset_names)
		cat("\n")
	}
					
	codeChunkNum = 0
	codeChunkCommentNum = 0
	
	
	for (chunk in code_chunks) 
	{	
		codeChunkCommentNum = codeChunkCommentNum + 1 
		
		if(regexpr(paste0("BSkyInAppScriptExecutionEngine", ".*"), trimws(chunk,which = "left", whitespace = "[ \t\r]"))[1] > 0)
		{
		}
		else if(regexpr(paste0("BSkyInAppBMDScriptExecutionEngine", ".*"), trimws(chunk,which = "left", whitespace = "[ \t\r]"))[1] > 0)
		{
		}
		else if(regexpr(paste0("BSkyInAppScriptExtractOldDatasetList", ".*"), trimws(chunk,which = "left", whitespace = "[ \t\r]"))[1] > 0)
		{
		}
		else if(regexpr(paste0("\nBSkyloadDataset", ".*"), trimws(chunk,which = "left", whitespace = "[ \t\r]"))[1] > 0)
		{
		}
		else if(regexpr(paste0("\nBSkyLoadRpkgDataset", ".*"), trimws(chunk,which = "left", whitespace = "[ \t\r]"))[1] > 0)
		{
		}
		else
		{
			codeChunkNum = codeChunkNum + 1
			
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
							cat("\nThe script will be run after replacing the Dataset:", old_dataset_name, "with the Dataset:",currentDatasetName,"\n")
						}
					}
					
					if(!identical(trimws(old_dataset_name), trimws(currentDatasetName)))
					{
						num_subs = min(length(old_dataset_name), length(currentDatasetName))
						
						for(sub in 1:num_subs)
						{
							if(!identical(trimws(old_dataset_name[sub]), trimws(currentDatasetName[sub])))
							{
								chunk_modified = BSkyDatasetNameSubstitute(datasetName = old_dataset_name[sub], toDatasetName = currentDatasetName[sub], replaceOldColumnNames = replaceOldColumnNames, currentColumnNames = currentColumnNames, RcommandString = chunk_modified)
								
								dataset_name_pattern =  'dataset:\\s*(?:"([^"]+)"|([^"()]+))'
								dataset_name = regmatches(dialog_console_blocks[codeChunkCommentNum], gregexpr(dataset_name_pattern, dialog_console_blocks[codeChunkCommentNum]))[[1]]
								dataset_name = trimws(gsub('dataset:|["()]','',dataset_name))
								
								#cat("\nBlock dataset name:",dataset_name, "\n")
								
								if(length(dataset_name) > 0 && dataset_name == old_dataset_name[sub])
								{
									datasets_names_and_console_comments[codeChunkCommentNum] = BSkyDatasetNameSubstitute(datasetName = old_dataset_name[sub], toDatasetName = currentDatasetName[sub], replaceOldColumnNames = replaceOldColumnNames, currentColumnNames = currentColumnNames, RcommandString = datasets_names_and_console_comments[codeChunkCommentNum])
									dialog_console_blocks[codeChunkCommentNum] = gsub(old_dataset_name[sub], currentDatasetName[sub], dialog_console_blocks[codeChunkCommentNum])
								
									# cat("\nOLD DB: ", old_dataset_name[sub], "\n") 
									# cat("\nNEW DB: ", currentDatasetName[sub], "\n")
									# cat("\nBefore Sub: ",code_chunks_comments[codeChunkCommentNum],"\n")
									code_chunks_comments[codeChunkCommentNum] = BSkyDatasetNameSubstitute(datasetName = old_dataset_name[sub], toDatasetName = currentDatasetName[sub], replaceOldColumnNames = replaceOldColumnNames, currentColumnNames = currentColumnNames, RcommandString = code_chunks_comments[codeChunkCommentNum])
									#cat("\nAfter Sub: ",code_chunks_comments[codeChunkCommentNum],"\n")
								}
							}
						}
					}
					
					# Update the Dialog title to append with (datasetname) if it does not exist already
					output_title_pattern = 'output_title: "[^"]+"'
					output_title = regmatches(dialog_console_blocks[codeChunkCommentNum], gregexpr(output_title_pattern, dialog_console_blocks[codeChunkCommentNum]))[[1]]
					
					if(length(output_title) > 0)
					{
						# Extract dataset name (by now already updated with the new dataset) from the dialog definition 
						#dialog_dataset_name_pattern =  "^\\s+dataset: \".*\"(?!\\s+output_title:)"				   
						#dialog_dataset_name_pattern = "^\\s+dataset: \".*\"(?!(?:\\s+output_title:|\\s*\\(dataset:))"
						dialog_dataset_name_pattern = "dataset: \"[^\\\"]+\""

						dialog_dataset_name = regmatches(dialog_console_blocks[codeChunkCommentNum], gregexpr(dialog_dataset_name_pattern, dialog_console_blocks[codeChunkCommentNum]))[[1]]
						dialog_dataset_name = trimws(gsub('dataset:|["()]','',dialog_dataset_name))
					
						# Extract the last occurance (datasetname) substring from the dialog title string if it exists 
						extract_dataset_name_pattern <- "\\((.*?)\\)"
						matches <- regmatches(output_title, gregexpr(extract_dataset_name_pattern, output_title))
						
						if(length(matches[[1]]) > 0)
						{
							last_occurrence_data_name <- tail(matches[[1]], 1)

							# if last occurance (datasetname) substring does not exist in the dialog title, append (datasetname) to the dialog title
							if(length(grep(dialog_dataset_name, last_occurrence_data_name)) == 0)
							{
								replacement_string = paste0("\\1\\2", " (", dialog_dataset_name, ")", "\\3")
								dialog_console_blocks[codeChunkCommentNum] <- gsub("(output_title: \")([^\"]*)(\")", replacement_string , dialog_console_blocks[codeChunkCommentNum], perl = TRUE)
							}
						}
						else
						{
							# (datasetname) substring does not exist in the dialog title, append (datasetname) to the dialog title
							replacement_string = paste0("\\1\\2", " (", dialog_dataset_name, ")", "\\3")
							dialog_console_blocks[codeChunkCommentNum] <- gsub("(output_title: \")([^\"]*)(\")", replacement_string , dialog_console_blocks[codeChunkCommentNum], perl = TRUE)
						}
					}
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
			
			if(debug)
			{
				cat("\nBefore: ",code_chunks_comments[codeChunkCommentNum],"\n")
			}
			
			if(!(code_chunks_comments[codeChunkCommentNum] %in% c("Save Models to a file", "Load Models from a file")))
			{
				if((length(grep("dataset:", code_chunks_comments[codeChunkCommentNum])) == 0) && length(grep("dataset:", datasets_names_and_console_comments[codeChunkCommentNum])) > 0)
				{
					datasets_names_and_console_comments[codeChunkCommentNum] = gsub('"', '', datasets_names_and_console_comments[codeChunkCommentNum])	
					code_chunks_comments[codeChunkCommentNum] = paste0(code_chunks_comments[codeChunkCommentNum], " (", datasets_names_and_console_comments[codeChunkCommentNum], ")")
				}
			}
			
			code_chunks_comment = paste0("Title:",code_chunks_comments[codeChunkCommentNum]) #Anil 10Apr24. used this instead of line abv for title
			
			if(debug)
			{
				cat("\nAfter: ",code_chunks_comment,"\n")
			}
			
			#code_chunk_list = c(code_chunk_list, chunk_header = list(code_chunks_comment))
			code_chunk_list = c(code_chunk_list, dialog_console = list(dialog_console_blocks[codeChunkCommentNum]))
			#code_chunk_list = c(code_chunk_list, chunk_new_line = list("BSkyFormat('\n')")) #Anil 10Apr24. Now we have title, this is not needed
			code_chunk_list = c(code_chunk_list, chunk_code = list(chunk_modified))
		}
	}
	
	oldGraphicsDirPath = BSkyGetGraphicsDirPath()
	
	tryCatch({
        withCallingHandlers({
		
			if(length(code_chunk_list) > 1)
			{
				BSkyInAppBMDScriptExecuteRBlocks(bsky_script_system_dir = bmd_output_file_path, dialog_console_code_chunk_list = code_chunk_list, scriptSinkfile = scriptSinkfile, inAppScriptEnv = TRUE)
			}
            
        }, warning = BSkyInAppBMDScriptExecuteErrWarnHandler, silent = TRUE)
    }, error = BSkyInAppBMDScriptExecuteErrWarnHandler, silent = TRUE)
	
	
	# Restore the graphics path for the App to execute normally when it returns from the BMD execution
	BSkySetGraphicsDirPath(bskyGraphicsDirPath = oldGraphicsDirPath)
	
	# Close out sin() file
	#suppressWarnings(sink())
	suppressWarnings(sink(type = "output"))
	suppressWarnings(sink(type = "message"))
	
	if(length(bmd_output_file_path) == 0)
	{
		return(invisible(code_chunk_list))
	}
	else
	{
		return(invisible())
	}
}


BSkyInAppBMDScriptExecuteRBlocks <- function(bsky_script_system_dir, dialog_console_code_chunk_list, scriptSinkfile = FALSE, inAppScriptEnv = TRUE)
{
		if (!dir.exists(bsky_script_system_dir)) 
		{ 
			cat("\n", bsky_script_system_dir, "- BlueSKy temp script directory not found", "..exiting..\n")
			return(invisible)
		}
		
		if(inAppScriptEnv == TRUE)
		{
			bmd_script_working_dir = paste0(bsky_script_system_dir,"\\BlueSkyScript")
			if (!dir.exists(bmd_script_working_dir)) 
			{
				# Create the directory if it does not exist
				dir.create(bmd_script_working_dir)
			}
			
			if (dir.exists(bmd_script_working_dir)) 
			{
				bsky_script_system_dir = bmd_script_working_dir
			}
		}
		
		if(inAppScriptEnv == TRUE && scriptSinkfile == TRUE)
		{
			system_log_dir = paste0(bsky_script_system_dir,"\\logs")
			if (!dir.exists(system_log_dir)) 
			{
				# Create the directory if it does not exist
				dir.create(system_log_dir)
			}
		}
		
		# system_out_dir = paste0(bsky_script_system_dir,"\\out")
		# if (!dir.exists(system_out_dir)) 
		# {
			# # Create the directory if it does not exist
			# dir.create(system_out_dir)
		# }
	
		#####################################################################################################
		# Setting up the output directory to write HTML file names for every BlueSky Statistics Script run
		#####################################################################################################
		cur_timestamp = format(Sys.time(), "%Y%m%d_%H%M%S")
		
		if(inAppScriptEnv == TRUE && scriptSinkfile == TRUE)
		{
			BSkyScriptSystemSinkFileMgmt(bsky_script_system_dir, cur_timestamp = cur_timestamp, init = TRUE, inAppScriptEnv = inAppScriptEnv)
		}
		
		oldGraphicsDirPath = BSkyGetGraphicsDirPath()
		BSkySetGraphicsDirPath(bskyGraphicsDirPath = bsky_script_system_dir)
		
		
		if(inAppScriptEnv == TRUE)
		{
			bsky_script_output_file = paste(bsky_script_system_dir,"\\","BSkyBMDOutput", sep='')
		}
		else
		{
			bsky_script_output_file = paste(bsky_script_system_dir,"\\","BSkyBMDOutput", "_", cur_timestamp, ".htm", sep='')
		}
		
		writeLines("\n", bsky_script_output_file, sep = "\n") 
		
		codeChunkNum = 0

		for (chunk in 1:length(dialog_console_code_chunk_list)) 
		{	
			outputBMDfileConn = file(bsky_script_output_file, "ab")
					
			if(names(dialog_console_code_chunk_list[chunk]) == "dialog_console")
			{
				if(dialog_console_code_chunk_list[[chunk]] != "BSKY_NO_DIALOG_CONSOLE_BLOCK")
				{
					# Write the combined lines to a new file
					writeLines(dialog_console_code_chunk_list[[chunk]], outputBMDfileConn, sep = "\n")
					#writeLines("\n", outputBMDfileConn, sep = "\n")
				}
				
				close(outputBMDfileConn)
			}
			else if(names(dialog_console_code_chunk_list[chunk]) == "chunk_code")
			{
				# Write the combined lines to a new file
				
				#writeLines(paste("```{r}",dialog_console_code_chunk_list[chunk],"```"), outputBMDfileConn, sep = "\n")
				writeLines("```{r}", outputBMDfileConn, sep = "\n")
				writeLines(paste(dialog_console_code_chunk_list[chunk]), outputBMDfileConn, sep = "\n")
				writeLines("```", outputBMDfileConn, sep = "\n")
				
				#writeLines("\n", outputBMDfileConn)
				close(outputBMDfileConn)
				
				delete_left_over_sink_svgs = list.files(path = bsky_script_system_dir, pattern=".*.svg|.txt", ignore.case = TRUE, full.names = TRUE)
				if(length(delete_left_over_sink_svgs) > 0)
				{
					unlink(delete_left_over_sink_svgs)
				}
					
				############################################################
				#  Open a sink file to redirect the stdout and stderr output
				############################################################
				#sink(file = paste(bsky_script_system_dir,"\\","BSkysink.txt", sep=''), append = FALSE, type = c("output", "message"), split = FALSE)
				
				#sink_file_conn = file(paste(bsky_script_system_dir,"\\","BSkyBmdOutsink.txt", sep=''), open = "wt", encoding = "UTF-8")
				sink_file_conn = file(paste(bsky_script_system_dir,"\\","BSkyBmdOutsink.txt", sep=''), open = "wt", encoding = "UTF-8")
				
				####################################################
				# close the bsky_script_system_dir\logs sink files()
				####################################################
				#suppressWarnings(sink())
				sink()
				
				sink(file = sink_file_conn, append = TRUE, type = c("output"), split = FALSE)
				sink(file = sink_file_conn, append = TRUE, type = c("message"), split = FALSE)
						
				#######################################################################################
				#  Open a SVG graphics device to collect the graphics files for each dataset processed
				#######################################################################################
				SvgFileName = "BSkyRplot%03d.svg"
				#svg(paste(bsky_script_system_dir,"\\",SvgFileName, sep=''), width=10, height=6)
				#antialias = c("default", "none", "gray", "subpixel") pointsize = 12
				svg(paste(bsky_script_system_dir,"\\",SvgFileName, sep=''), width=par("din")[1], height=par("din")[2])
				
				
				#cat("\n============\n", dialog_console_code_chunk_list[[chunk]], "\n====\n")
				###################################################################################################
				# Execute the script copied from BlueSky Statistics App as is without any dataset name substitution
				###################################################################################################
				cmd_execution_status = BSkyEvalRcommandForAppScript(RcommandString = dialog_console_code_chunk_list[[chunk]])
				
				#cat("\nCommand Execution Status : ", cmd_execution_status$executionStatus, "\n") if -1 that indicates failed execution 
				
				################################
				# Close the SVG graphics device
				################################
				dev.off()
				
				##################################
				#  Close the chunk level sink file 
				##################################
				#sink()
				sink(type = "output")
				sink(type = "message")
		
				close(sink_file_conn)
				
				#######################################################
				# Open back the bsky_script_system_dir\logs sink files 
				#######################################################
				if(inAppScriptEnv == TRUE && scriptSinkfile == TRUE)
				{
					BSkyScriptSystemSinkFileMgmt(bsky_script_system_dir, cur_timestamp, inAppScriptEnv = TRUE)
				}
				
				codeChunkNum = codeChunkNum + 1
				
				# Generate the HTML output file from the script
				BSkyWriteKableHtmlOutput(datasetName = c(""), dirName = bsky_script_system_dir, fileName = bsky_script_output_file, timeStamp = cur_timestamp, codeChunkNum = codeChunkNum, codeChunkCmt = c(""), inAppScriptEnv = TRUE)
			
				# No incoming datafile is used - so no datafile to move
				#file.copy(full_datafile_names[file_counter], paste(bsky_script_output_subdir,"\\", datafile_names[file_counter], sep=''))
				#file.remove(full_datafile_names[file_counter])
				
				delete_left_over_svgs = list.files(path = bsky_script_system_dir, pattern=".*.svg", ignore.case = TRUE, full.names = TRUE)
				if(length(delete_left_over_svgs) > 0)
				{
					unlink(delete_left_over_svgs)
				}
			}
		}
		
		# Restore the graphics path for the App to execute normally when it returns from the BMD execution
		BSkySetGraphicsDirPath(bskyGraphicsDirPath = oldGraphicsDirPath)
		sink()
		
		require(zip)

		# List of files to include in the zip archive
		files_to_zip = bsky_script_output_file

		# Name of the output zip file
		zip_filename = paste0(files_to_zip, ".Bmd")
		
		#print(files_to_zip)
		#print(zip_filename)

		# Create the zip file
		zip::zip(zip_filename, files = files_to_zip, mode = "cherry-pick")
}


##30Apr2024 - a lot changes to handle in App BMD Script run in addition to the off App headless RMD script run
##30Apr2024 - introduced inAppScriptEnv TRUE/FALSE flag to seperate the logic for In App BMD script from Off App RMD script execution
##27Dec2023
# Add two link breaks e.g. <br><br> between two consecutive graphics to create some separation 
##23Nov2023
##31Mar2021
##11Nov2023 - made changes to handle the output formatting from automated (headless) BSky script run
## fileName = "C:/Users/User/Documents/workfolder/BSky/Rmarkdown/kableoutput1.html"
BSkyWriteKableHtmlOutput <- function(datasetName = c(), dirName = NULL, fileName = NULL, timeStamp = Sys.time(), fileOpenMode = "w", codeChunkNum = 1, codeChunkCmt = c(), inAppScriptEnv = FALSE, bSkyDebug = 0)
{
	#################################################################################
	# Note: Remeber to consider the following - 
	# For RMD script processing in the headless windows schedular execution mode
	# Consider making embedding SVG into the output HTML and also skip MathJS model equation handling
	# Currently those two conditions are handled only in the InApp BMD script execution mode
	#################################################################################
	
	retObjList = BSkyGetHoldFormatObjList(bSkyCompatibility=0, bSkyDebug)
	
	if(inAppScriptEnv == TRUE)
	{
		fileOpenWriteMode = "wb"
		fileOpenAppendMode = "ab"
	}
	else
	{
		fileOpenWriteMode = "w"
		fileOpenAppendMode = "a"
	}
	
	svg_file_names = c()
	full_svg_file_names = c()
	graphicsDir = BSkyGetGraphicsDirPath()
	
	sink_file_path = paste(graphicsDir, "\\", "BSkyBmdOutsink.txt", sep='')
	sink_file = file.info(sink_file_path) 
	
	if(sink_file$size[1] < 1)
	{
		return(invisible(FALSE))
	}
	
	
	if(!is.null(graphicsDir) && length(graphicsDir) > 0 && trimws(graphicsDir) != "" && dir.exists(graphicsDir))
	{
		full_svg_file_names = list.files(path = graphicsDir, pattern="png|svg", full.names = TRUE)
		
		if(length(full_svg_file_names) > 0)
		{
			# Get file information including creation time
			file_info <- file.info(full_svg_file_names)
			
			svg_file_names = list.files(path = graphicsDir, pattern="png|svg", full.names = FALSE)

			# Order files based on creation time
			svg_file_names <- svg_file_names[order(file_info$ctime)]
			full_svg_file_names <- full_svg_file_names[order(file_info$ctime)]
		}
	}
	
	if(sink_file$size[1] > 0)
	{
		ret_obj_list_len = length(retObjList)
		
		tempHTMLfile = paste(dirName,"\\BSkytempOutfile.html", sep='')
		tempfileConn = NULL
		
		
		plot_subdir = paste(dirName,"\\plots", sep='')
		log_subdir = paste(dirName,"\\logs", sep='')
		
		if(!is.null(dirName) && dirName != "")
		{
			if (!dir.exists(dirName)) 
			{
				# Create the directory with read and write permissions if it does not exist
				dir.create(dirName)
			}

			# Check if the directory was created successfully
			if (file.exists(dirName) && !is.null(fileName) && fileName != "")
			{
				#fileConn<-file(fileName, fileOpenWriteMode)
				tempfileConn = file(tempHTMLfile, fileOpenWriteMode)
			}
			else
			{
				return(invisible(FALSE))
			}
			
			if(inAppScriptEnv == FALSE)
			{
				if (!dir.exists(plot_subdir)) 
				{
					# Create the directory with read and write permissions if it does not exist
					dir.create(plot_subdir)
				}
				
				if (!dir.exists(log_subdir)) 
				{
					# Create the directory with read and write permissions if it does not exist
					dir.create(log_subdir)
				}
			}
		}
		else
		{
			return(invisible(FALSE))
		}
		
		
		plot_file_counter = 1
		ret_structure_counter = 1
		regular_sink_text_write_flag_on = FALSE
		
		# Open the file connection
		sink_file_conn <- file(sink_file_path, open = "rt", encoding = "UTF-8")

		if(inAppScriptEnv == FALSE)
		{
			if(trimws(codeChunkCmt) != "")
			{
				writeLines(paste("<h4><strong>", codeChunkCmt, "</strong></h4>"), tempfileConn, sep="\n")
			}
		}
		
		consecutive_graphics_count = 0
		
		# Read the file one line at a time
		while (length(sink_line <- readLines(sink_file_conn, n = 1)) > 0) 
		{
			#print(nchar(sink_line))
			#print(sink_line)
			
			if(nchar(sink_line) > 0 && length(grep("BSkyFormatInternalSyncFileMarker",sink_line))>0 && ret_structure_counter <= ret_obj_list_len)
			{
				#cat("\n======Inside BSkyFormat=======\n")
				#print(sink_line)
				
				if(regular_sink_text_write_flag_on == TRUE)
				{
					if(inAppScriptEnv == TRUE)
					{
						writeLines("```",tempfileConn, sep="\n")
					}
					else
					{
						writeLines("</code></pre>",tempfileConn)
					}
					regular_sink_text_write_flag_on = FALSE
				}
				
				consecutive_graphics_count = 0
				
				while(ret_structure_counter <= (ret_obj_list_len -1) && ((!is.null(retObjList[[ret_structure_counter]]$type) && retObjList[[ret_structure_counter]]$type != "BSkyFormat") || (is.null(retObjList[[ret_structure_counter]]$type) && retObjList[[ret_structure_counter]][[1]]$type != "BSkyFormat")))
				{
					# cat("\nret_structure_counter : ",ret_structure_counter,"\n")
					# if(!is.null(retObjList[[ret_structure_counter]]$type)) print(retObjList[[ret_structure_counter]]$type)
					# else print(retObjList[[ret_structure_counter]][[1]]$type)
					
					ret_structure_counter = ret_structure_counter + 1
				}
				
				# cat("\n2. ret_structure_counter : ",ret_structure_counter,"\n")
				# if(!is.null(retObjList[[ret_structure_counter]]$type))print(retObjList[[ret_structure_counter]]$type)
				# else print(retObjList[[ret_structure_counter]][[1]]$type)
				
				if(is.null(retObjList[[ret_structure_counter]]$type) && retObjList[[ret_structure_counter]][[1]]$type == "BSkyFormat")
				{
					#[[3]][[1]]$object$tables
					num_tables = length(retObjList[[ret_structure_counter]][[1]]$object$tables) - 1
					
					if(num_tables > 0)
					{
						for(j in 1:num_tables)
						{
							if(!is.null(tempfileConn))
							{	
								if(inAppScriptEnv == TRUE)
								{
									#writeLines("\n", tempfileConn, sep="\n")
									writeLines("```{html}",tempfileConn, sep="\n")
								}
								
								#writeLines("\n", tempfileConn, sep="\n")
								
								if(inAppScriptEnv == TRUE)
								{
									formulaEqFound = length(grep("begin\\{aligned\\}",retObjList[[ret_structure_counter]][[1]]$object$tables[[j]]))
									coefValueEqFound = length(grep("\\\\widehat",retObjList[[ret_structure_counter]][[1]]$object$tables[[j]]))
									
									if(formulaEqFound > 0 && coefValueEqFound > 0)
									{
										# Do notthing for Equation output in BMD script execution - Look into this later. Not a easy solution
										if(FALSE)
										{
											#writeLines("<span class=\"mjx-chtml MJXc-display\" style=\"text-align: center;\"><span id=\"MathJax-Element-4-Frame\" class=\"mjx-chtml MathJax_CHTML\" tabindex=\"0\" style=\"font-size: 112%; text-align: center;\"><span id=\"MJXc-Node-133\" class=\"mjx-math\"><span id=\"MJXc-Node-134\" class=\"mjx-mrow\"><span id=\"MJXc-Node-135\" class=\"mjx-mtable\" style=\"vertical-align: -0.324em; padding: 0px 0.167em;\"><span class=\"mjx-table\"><span id=\"MJXc-Node-136\" class=\"mjx-mtr\" style=\"height: 1.147em;\"><span id=\"MJXc-Node-137\" class=\"mjx-mtd\" style=\"padding: 0px; text-align: right; width: 1.889em;\"><span id=\"MJXc-Node-138\" class=\"mjx-mrow\" style=\"margin-top: -0.128em;\"><span id=\"MJXc-Node-139\" class=\"mjx-texatom\"><span id=\"MJXc-Node-140\" class=\"mjx-mrow\"><span id=\"MJXc-Node-141\" class=\"mjx-texatom\"><span id=\"MJXc-Node-142\" class=\"mjx-mrow\"><span id=\"MJXc-Node-143\" class=\"mjx-munderover\"><span class=\"mjx-stack\"><span class=\"mjx-over\" style=\"height: 0.334em; padding-bottom: 0.06em;\"><span id=\"MJXc-Node-148\" class=\"mjx-mo\" style=\"vertical-align: top;\"><span class=\"mjx-char MJXc-TeX-size4-R\" style=\"padding-top: 0.623em; padding-bottom: 0.251em;\">ˆ</span></span></span><span class=\"mjx-op\"><span id=\"MJXc-Node-144\" class=\"mjx-mrow\"><span id=\"MJXc-Node-145\" class=\"mjx-mi\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.127em; padding-bottom: 0.313em;\">m</span></span><span id=\"MJXc-Node-146\" class=\"mjx-mi\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.127em; padding-bottom: 0.561em;\">p</span></span><span id=\"MJXc-Node-147\" class=\"mjx-mi\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.189em; padding-bottom: 0.561em;\">g</span></span></span></span></span></span></span></span></span></span><span class=\"mjx-strut\"></span></span></span><span id=\"MJXc-Node-149\" class=\"mjx-mtd\" style=\"padding: 0px; text-align: left; width: 23.093em;\"><span id=\"MJXc-Node-150\" class=\"mjx-mrow\" style=\"margin-top: -0.128em;\"><span id=\"MJXc-Node-151\" class=\"mjx-mi\"></span><span id=\"MJXc-Node-152\" class=\"mjx-mo MJXc-space3\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.065em; padding-bottom: 0.313em;\">=</span></span><span id=\"MJXc-Node-153\" class=\"mjx-mn MJXc-space3\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.375em; padding-bottom: 0.375em;\">34.1849</span></span><span id=\"MJXc-Node-154\" class=\"mjx-mo MJXc-space2\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.313em; padding-bottom: 0.437em;\">−</span></span><span id=\"MJXc-Node-155\" class=\"mjx-mn MJXc-space2\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.375em; padding-bottom: 0.375em;\">1.2274</span></span><span id=\"MJXc-Node-156\" class=\"mjx-mo\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.437em; padding-bottom: 0.561em;\">(</span></span><span id=\"MJXc-Node-157\" class=\"mjx-mi\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.437em; padding-bottom: 0.561em;\">cyl</span></span><span id=\"MJXc-Node-158\" class=\"mjx-mo\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.437em; padding-bottom: 0.561em;\">)</span></span><span id=\"MJXc-Node-159\" class=\"mjx-mo MJXc-space2\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.313em; padding-bottom: 0.437em;\">−</span></span><span id=\"MJXc-Node-160\" class=\"mjx-mn MJXc-space2\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.375em; padding-bottom: 0.375em;\">0.0188</span></span><span id=\"MJXc-Node-161\" class=\"mjx-mo\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.437em; padding-bottom: 0.561em;\">(</span></span><span id=\"MJXc-Node-162\" class=\"mjx-mi\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.437em; padding-bottom: 0.561em;\">disp</span></span><span id=\"MJXc-Node-163\" class=\"mjx-mo\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.437em; padding-bottom: 0.561em;\">)</span></span><span id=\"MJXc-Node-164\" class=\"mjx-mo MJXc-space2\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.313em; padding-bottom: 0.437em;\">−</span></span><span id=\"MJXc-Node-165\" class=\"mjx-mn MJXc-space2\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.375em; padding-bottom: 0.375em;\">0.0147</span></span><span id=\"MJXc-Node-166\" class=\"mjx-mo\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.437em; padding-bottom: 0.561em;\">(</span></span><span id=\"MJXc-Node-167\" class=\"mjx-mi\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.437em; padding-bottom: 0.561em;\">hp</span></span><span id=\"MJXc-Node-168\" class=\"mjx-mo\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.437em; padding-bottom: 0.561em;\">)</span></span><span class=\"mjx-strut\"></span></span></span></span></span></span></span></span></span></span>", tempfileConn, sep="\n")
											writeLines("```", tempfileConn, sep="\n")
											
											writeLines("```{html}", tempfileConn, sep="\n")
											writeLines(paste("<script type=\"math/tex; mode=display\" id=\"MathJax-Element-4\">",retObjList[[ret_structure_counter]][[1]]$object$tables[[j]],"</script>"), tempfileConn, sep="\n")
										}
									}
									else if(formulaEqFound > 0)
									{
										# Do notthing for Equation output in BMD script execution - Look into this later. Not a easy solution
										if(FALSE)
										{
											#writeLines("```{html}", tempfileConn, sep="\n")
											
											#writeLines("<span class=\"mjx-chtml MJXc-display\" style=\"text-align: center;\"><span id=\"MathJax-Element-3-Frame\" class=\"mjx-chtml MathJax_CHTML\" tabindex=\"0\" style=\"font-size: 113%; text-align: center;\"><span id=\"MJXc-Node-85\" class=\"mjx-math\"><span id=\"MJXc-Node-86\" class=\"mjx-mrow\"><span id=\"MJXc-Node-87\" class=\"mjx-mtable\" style=\"vertical-align: -0.938em; padding: 0px 0.167em;\"><span class=\"mjx-table\"><span id=\"MJXc-Node-88\" class=\"mjx-mtr\" style=\"height: 1.225em;\"><span id=\"MJXc-Node-89\" class=\"mjx-mtd\" style=\"padding: 0px; text-align: right; width: 1.889em;\"><span id=\"MJXc-Node-90\" class=\"mjx-mrow\" style=\"margin-top: -0.2em;\"><span id=\"MJXc-Node-91\" class=\"mjx-mi\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.187em; padding-bottom: 0.556em;\">mpg</span></span><span class=\"mjx-strut\"></span></span></span><span id=\"MJXc-Node-92\" class=\"mjx-mtd\" style=\"padding: 0px; text-align: left; width: 16.014em;\"><span id=\"MJXc-Node-93\" class=\"mjx-mrow\" style=\"margin-top: -0.2em;\"><span id=\"MJXc-Node-94\" class=\"mjx-mi\"></span><span id=\"MJXc-Node-95\" class=\"mjx-mo MJXc-space3\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.065em; padding-bottom: 0.31em;\">=</span></span><span id=\"MJXc-Node-96\" class=\"mjx-mi MJXc-space3\"><span class=\"mjx-char MJXc-TeX-math-I\" style=\"padding-top: 0.249em; padding-bottom: 0.31em;\">α</span></span><span id=\"MJXc-Node-97\" class=\"mjx-mo MJXc-space2\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.31em; padding-bottom: 0.433em;\">+</span></span><span id=\"MJXc-Node-98\" class=\"mjx-msubsup MJXc-space2\"><span class=\"mjx-base\" style=\"margin-right: -0.007em;\"><span id=\"MJXc-Node-99\" class=\"mjx-mi\"><span class=\"mjx-char MJXc-TeX-math-I\" style=\"padding-top: 0.495em; padding-bottom: 0.495em; padding-right: 0.007em;\">β</span></span></span><span class=\"mjx-sub\" style=\"font-size: 70.7%; vertical-align: -0.212em; padding-right: 0.071em;\"><span id=\"MJXc-Node-100\" class=\"mjx-texatom\" style=\"\"><span id=\"MJXc-Node-101\" class=\"mjx-mrow\"><span id=\"MJXc-Node-102\" class=\"mjx-mn\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.372em; padding-bottom: 0.372em;\">1</span></span></span></span></span></span><span id=\"MJXc-Node-103\" class=\"mjx-mo\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.495em; padding-bottom: 0.618em;\">(</span></span><span id=\"MJXc-Node-104\" class=\"mjx-mi\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.433em; padding-bottom: 0.556em;\">cyl</span></span><span id=\"MJXc-Node-105\" class=\"mjx-mo\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.495em; padding-bottom: 0.618em;\">)</span></span><span id=\"MJXc-Node-106\" class=\"mjx-mo MJXc-space2\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.31em; padding-bottom: 0.433em;\">+</span></span><span id=\"MJXc-Node-107\" class=\"mjx-msubsup MJXc-space2\"><span class=\"mjx-base\" style=\"margin-right: -0.007em;\"><span id=\"MJXc-Node-108\" class=\"mjx-mi\"><span class=\"mjx-char MJXc-TeX-math-I\" style=\"padding-top: 0.495em; padding-bottom: 0.495em; padding-right: 0.007em;\">β</span></span></span><span class=\"mjx-sub\" style=\"font-size: 70.7%; vertical-align: -0.212em; padding-right: 0.071em;\"><span id=\"MJXc-Node-109\" class=\"mjx-texatom\" style=\"\"><span id=\"MJXc-Node-110\" class=\"mjx-mrow\"><span id=\"MJXc-Node-111\" class=\"mjx-mn\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.372em; padding-bottom: 0.372em;\">2</span></span></span></span></span></span><span id=\"MJXc-Node-112\" class=\"mjx-mo\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.495em; padding-bottom: 0.618em;\">(</span></span><span id=\"MJXc-Node-113\" class=\"mjx-mi\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.433em; padding-bottom: 0.556em;\">disp</span></span><span id=\"MJXc-Node-114\" class=\"mjx-mo\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.495em; padding-bottom: 0.618em;\">)</span></span><span id=\"MJXc-Node-115\" class=\"mjx-mo MJXc-space2\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.31em; padding-bottom: 0.433em;\">+</span></span><span id=\"MJXc-Node-116\" class=\"mjx-msubsup MJXc-space2\"><span class=\"mjx-base\" style=\"margin-right: -0.007em;\"><span id=\"MJXc-Node-117\" class=\"mjx-mi\"><span class=\"mjx-char MJXc-TeX-math-I\" style=\"padding-top: 0.495em; padding-bottom: 0.495em; padding-right: 0.007em;\">β</span></span></span><span class=\"mjx-sub\" style=\"font-size: 70.7%; vertical-align: -0.212em; padding-right: 0.071em;\"><span id=\"MJXc-Node-118\" class=\"mjx-texatom\" style=\"\"><span id=\"MJXc-Node-119\" class=\"mjx-mrow\"><span id=\"MJXc-Node-120\" class=\"mjx-mn\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.372em; padding-bottom: 0.372em;\">3</span></span></span></span></span></span><span id=\"MJXc-Node-121\" class=\"mjx-mo\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.495em; padding-bottom: 0.618em;\">(</span></span><span id=\"MJXc-Node-122\" class=\"mjx-mi\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.433em; padding-bottom: 0.556em;\">hp</span></span><span id=\"MJXc-Node-123\" class=\"mjx-mo\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.495em; padding-bottom: 0.618em;\">)</span></span><span id=\"MJXc-Node-124\" class=\"mjx-mtext\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"margin-top: -0.304em; padding-bottom: 0.372em;\">&nbsp;</span></span><span id=\"MJXc-Node-125\" class=\"mjx-mo\"><span class=\"mjx-char MJXc-TeX-main-R\" style=\"padding-top: 0.31em; padding-bottom: 0.433em;\">+</span></span><span class=\"mjx-strut\"></span></span></span></span><span id=\"MJXc-Node-126\" class=\"mjx-mtr\" style=\"height: 1.15em;\"><span id=\"MJXc-Node-127\" class=\"mjx-mtd\" style=\"padding: 0.15em 0px 0px; text-align: right;\"><span id=\"MJXc-Node-128\" class=\"mjx-mrow\" style=\"margin-top: -0.2em;\"><span class=\"mjx-strut\"></span></span></span><span id=\"MJXc-Node-129\" class=\"mjx-mtd\" style=\"padding: 0.15em 0px 0px; text-align: left;\"><span id=\"MJXc-Node-130\" class=\"mjx-mrow\" style=\"margin-top: -0.2em;\"><span id=\"MJXc-Node-131\" class=\"mjx-mspace\" style=\"width: 1em; height: 0px;\"></span><span id=\"MJXc-Node-132\" class=\"mjx-mi\"><span class=\"mjx-char MJXc-TeX-math-I\" style=\"padding-top: 0.187em; padding-bottom: 0.31em;\">ϵ</span></span><span class=\"mjx-strut\"></span></span></span></span></span></span></span></span></span></span>", tempfileConn, sep="\n")
											writeLines("```", tempfileConn, sep="\n")
											
											writeLines("```{html}", tempfileConn, sep="\n")
											writeLines(paste("<script type=\"math/tex; mode=display\" id=\"MathJax-Element-3\">",retObjList[[ret_structure_counter]][[1]]$object$tables[[j]],"</script>"), tempfileConn, sep="\n")
											
											#writeLines("```", tempfileConn, sep="\n")
										}
									}
									else
									{
										writeLines(retObjList[[ret_structure_counter]][[1]]$object$tables[[j]], tempfileConn, sep="\n")
									}
								}
								else
								{
									writeLines(retObjList[[ret_structure_counter]][[1]]$object$tables[[j]], tempfileConn, sep="\n")
								}
								
								if(inAppScriptEnv != TRUE)
								{
									writeLines("\n<br/>\n", tempfileConn)
								}
								
								if(inAppScriptEnv == TRUE)
								{
									writeLines("```",tempfileConn, sep="\n")
									#writeLines("\n", tempfileConn)
								}
							}
							else
							{	cat("\n")
								cat(retObjList[[ret_structure_counter]][[1]]$object$tables[[j]])
								cat("\n")
							}
						}
					}
				}
				# else if(!is.null(retObjList[[ret_structure_counter]]$type) && retObjList[[ret_structure_counter]]$type == "BSkyFormat")
				# {
					#[[2]]$object$tables
					# num_tables = length(retObjList[[ret_structure_counter]]$object$tables) - 1
					
					# if(num_tables > 0)
					# {
						# for(j in 1:num_tables)
						# {
							# if(!is.null(tempfileConn))
							# {	
								# writeLines("\n", tempfileConn)
								# writeLines(retObjList[[ret_structure_counter]]$object$tables[[j]], tempfileConn)
								# writeLines("\n<br/>\n", tempfileConn)
							# }
							# else
							# {	cat("\n")
								# cat(retObjList[[ret_structure_counter]]$object$tables[[j]])
								# cat("\n")
							# }
						# }
					# }
				# }

				ret_structure_counter = ret_structure_counter + 1
			}											 
			else if(nchar(sink_line) > 0 && length(grep("BSkyGraphicsFormatInternalSyncFileMarker",sink_line))>0 && ret_structure_counter <= ret_obj_list_len)
			{
				#cat("\n======Inside BSkyGraphicsFormat=======\n")
				#print(sink_line)
				
				if(regular_sink_text_write_flag_on == TRUE)
				{
					if(inAppScriptEnv == TRUE)
					{
						writeLines("```",tempfileConn, sep="\n")
					}
					else
					{
						writeLines("</code></pre>",tempfileConn)
					}
					regular_sink_text_write_flag_on = FALSE
				}
				
				while(ret_structure_counter <= (ret_obj_list_len - 1) && (is.null(retObjList[[ret_structure_counter]]$type) || retObjList[[ret_structure_counter]]$type != "BSkyGraphicsFormat"))
				{
					# cat("\nret_structure_counter : ",ret_structure_counter,"\n")
					# if(!is.null(retObjList[[ret_structure_counter]]$type)) print(retObjList[[ret_structure_counter]]$type)
					# else print(retObjList[[ret_structure_counter]][[1]]$type)
					
					ret_structure_counter = ret_structure_counter + 1
				}
				
				# cat("\n2. ret_structure_counter : ",ret_structure_counter,"\n")
				# if(!is.null(retObjList[[ret_structure_counter]]$type)) print(retObjList[[ret_structure_counter]]$type)
				# else print(retObjList[[ret_structure_counter]][[1]]$type)
				
				if(!is.null(retObjList[[ret_structure_counter]]$type) && retObjList[[ret_structure_counter]]$type == "BSkyGraphicsFormat")
				{
					if(!is.null(tempfileConn))
					{
						if(consecutive_graphics_count > 0)
						{
							if(inAppScriptEnv != TRUE)
							{
								writeLines("<br/> <br/>", tempfileConn, sep="\n")
							}
						}
						consecutive_graphics_count = consecutive_graphics_count + 1
						
						
						#<!-- Image with a relative path -->
						#<img src="path/to/your/image.jpg" alt="Description of the image">
						#<!-- Image with an absolute URL -->
						#<img src="https://example.com/path/to/remote/image.jpg" alt="Description of the remote image">
						
						if(inAppScriptEnv == TRUE)
						{
							svg_content = readLines(full_svg_file_names[plot_file_counter])
						
							# Sample 5 digits from 0 to 9
							rand_number <- paste(sample(0:9, 5, replace = TRUE), collapse='') 
							rand1 = rand_number
							rand2 = rand_number
							
							svg_content = gsub('width="(\\d+)pt"', '', svg_content)
							svg_content = gsub('height="(\\d+)pt"', '', svg_content)
							svg_content = gsub("glyph", paste0("glyph",rand1), svg_content)
							svg_content = gsub('id=\\"clip', paste0("id=\"clip",rand2), svg_content)
							svg_content = gsub("#clip", paste0("#clip",rand2), svg_content)
								
							svg_content = svg_content[-1]
							writeLines("```{html}", tempfileConn, sep="\n")
							writeLines(svg_content, tempfileConn, sep="\n")
							writeLines("```", tempfileConn, sep="\n")
							
							# require(XML)
							# # Read the SVG file
							# svg_tree <- xmlTreeParse(full_svg_file_names[plot_file_counter], useInternalNodes = TRUE)
							# # Save the XML tree as an SVG file
							# svg_xml = saveXML(svg_tree)
							# saveXML(svg_tree, file = paste0("C:/Users/User/Downloads/xmlsvg",plot_file_counter))
							# writeLines("```{html}", tempfileConn, sep="\n")
							# #writeLines(svg_xml, tempfileConn, sep="\n")
							# writeLines("```", tempfileConn, sep="\n")
						}
						else
						{
							writeLines(paste("<img src=", "\"", "plots/", timeStamp, "_", datasetName, "_", codeChunkNum, "_", svg_file_names[plot_file_counter],"\"",">", sep=''), tempfileConn)
							file.copy(full_svg_file_names[plot_file_counter], paste(plot_subdir, "\\", timeStamp, "_", datasetName, "_", codeChunkNum, "_", svg_file_names[plot_file_counter], sep=''))
						}
						
						file.remove(full_svg_file_names[plot_file_counter])
					}
					else
					{
						cat("\n")
						cat("plot file: ")
						cat(svg_file_names[plot_file_counter])
						cat("\n")
					}
					
					plot_file_counter = plot_file_counter + 1
					ret_structure_counter = ret_structure_counter + 1
				}
			}
			else if(nchar(sink_line) > 0 && length(grep("BSkyDataGridRefresh",sink_line))>0)
			{
				#do nothing - skip 
				consecutive_graphics_count = 0
			}
			else if(nchar(sink_line) > 0)
			{
				#cat("\n======Inside text line =======\n")
				#print(sink_line)
				
				consecutive_graphics_count = 0
				
				if(regular_sink_text_write_flag_on == FALSE)
				{
					if(inAppScriptEnv == TRUE)
					{
						writeLines("```{code}", tempfileConn, sep="\n")
					}
					else
					{
						writeLines("<pre class=\"r\"><code>", tempfileConn)
					}
					regular_sink_text_write_flag_on = TRUE
				}
					
				writeLines(sink_line, tempfileConn, sep="\n")
			}
		}
		
		if(regular_sink_text_write_flag_on == TRUE)
		{
			if(inAppScriptEnv == TRUE)
			{
				writeLines("```", tempfileConn, sep="\n")
			}
			else
			{
				writeLines("</code></pre>",tempfileConn)
			}
			regular_sink_text_write_flag_on = FALSE
		}
		
		if(!is.null(sink_file_conn))
		{
			close(sink_file_conn)
			
			if(inAppScriptEnv == FALSE)
			{
				file.copy(sink_file_path, paste(log_subdir, "\\", timeStamp, "_",  datasetName, "_", "BSkysink_", codeChunkNum, ".txt", sep=''))
			}
			
			file.remove(sink_file_path)
		}
		
		if(!is.null(tempfileConn))
		{
			#writeLines("\n</body>\n", fileConn)
			#writeLines("\n</html>\n", fileConn)
			close(tempfileConn)
			
			# Example: Combining two text files line by line
			#file1 <- readLines(paste(graphicsDir, "\\template_pre.html", sep=''))
			file2 <- readLines(tempHTMLfile)
			#file3 <- readLines(paste(graphicsDir, "\\template_post.html", sep=''))

			#combined_lines <- c(file1, file2, file3)

			# Write the combined lines to a new file
			#writeLines(combined_lines, fileName)
			
			file.remove(tempHTMLfile)
			
			outputHTMLfileConn = file(fileName, fileOpenAppendMode)

			# Write the combined lines to a new file
			writeLines(file2, outputHTMLfileConn, sep="\n")
			close(outputHTMLfileConn)
		}
	}

	return(invisible(TRUE))
}


BSkyScriptSystemSinkFileMgmt <- function(bsky_script_system_dir, cur_timestamp, init = FALSE, inAppScriptEnv = FALSE)
{
	##################################################################################################
	#  Open a sink file to redirect the stdout and stderr output to capture any msgs from dataset open
	##################################################################################################
	
	# if(inAppScriptEnv == TRUE)
	# {
		# sinkOpenFile1 = paste(bsky_script_system_dir,"\\","BSkyScriptBmdSink1.txt", sep='')
		# sinkOpenFile2 = paste(bsky_script_system_dir,"\\","BSkyScriptBmdSink2.txt", sep='')
		# sinkOpenFile_names = list.files(path =  paste0(bsky_script_system_dir), pattern=paste0(".*", "sink", ".*\\.txt$"), ignore.case = TRUE, full.names = TRUE)
		# sinkOpenFile_names = sinkOpenFile_names[sinkOpenFile_names %in% c("BSkyScriptBmdSink1.txt", "BSkyScriptBmdSink2.txt")]
	# }
	# else
	{
		sinkOpenFile1 = paste(bsky_script_system_dir,"\\logs\\","BSkyScriptSystemSink1.txt", sep='')
		sinkOpenFile2 = paste(bsky_script_system_dir,"\\logs\\","BSkyScriptSystemSink2.txt", sep='')
		sinkOpenFile_names = list.files(path =  paste0(bsky_script_system_dir,"\\logs"), pattern=paste0(".*", "sink", ".*\\.txt$"), ignore.case = TRUE, full.names = TRUE)
	}
	
	
	if(length(sinkOpenFile_names) > 0)
	{
		# Get file information including creation time
		sinkOpenFiles_info <- file.info(sinkOpenFile_names)

		# Order files based on modified time i.e., process the oldest files first
		sinkOpenFile_names <- sinkOpenFile_names[order(sinkOpenFiles_info$mtime, decreasing = TRUE)]
		
		latest_sinkOpenFile_size = file.info(sinkOpenFile_names[1])$size
		
		#print(sinkOpenFile_names)
		#cat("\n===========================\n")
		
		if(latest_sinkOpenFile_size > (50*(2^10))) 
		{
			file.remove(sinkOpenFile_names[2])
			
			sinkOpenFile_conn = file(sinkOpenFile_names[2])
			sink(file = sinkOpenFile_conn, append = TRUE, type = c("output"), split = FALSE)
			sink(file = sinkOpenFile_conn, append = TRUE, type = c("message"), split = FALSE)
			
			cat("\n===== Timestamp: ",cur_timestamp,"=====\n")
			cat("sink file size limit of(", (50*(2^10)),")exceeded - the old content is erased\n")
		}
		else
		{
			sinkOpenFile_conn = file(sinkOpenFile_names[1], "a")
			sink(file = sinkOpenFile_conn, append = TRUE, type = c("output"), split = FALSE)
			sink(file = sinkOpenFile_conn, append = TRUE, type = c("message"), split = FALSE)
			
			if(init == TRUE)
			{
				cat("\n===== Timestamp: ",cur_timestamp,"=====\n")
			}
		}
	}
	else
	{
		#sinkOpenFile2_conn = file.create(sinkOpenFile2,"w")
		sinkOpenFile2_conn = file.create(sinkOpenFile2)
		#close(sinkOpenFile2_conn)
		
		#sink(file = sinkOpenFile1, append = TRUE, type = c("output", "message"), split = FALSE)
		sinkOpenFile1_conn = file(sinkOpenFile1)
		
		sink(file = sinkOpenFile1_conn, append = TRUE, type = c("output"), split = FALSE)
		sink(file = sinkOpenFile1_conn, append = TRUE, type = c("message"), split = FALSE)
		
		cat("\n===== Timestamp: ",cur_timestamp,"=====\n")
	}
}

# Needed to create copy from the original BSkyEvalRcommand() to avoid the recurssion error to execute R code block from app in BMD script
BSkyEvalRcommandForAppScript <- function (RcommandString, numExprParse = -1, selectionStartpos = 0,
    selectionEndpos = 0, executeSelectOnly = FALSE, currentDatasetName = BSkyGetCurrentDatabaseName(),
    replaceOldDatasetName = c(), currentColumnNames = c(), replaceOldColumnNames = c(),
    echo = BSkyGetRCommandDisplaySetting(), echoInline = BSkyGetRCommandDisplaySetting(),
    ignoreSplitOn = FALSE, graphicsDir = BSkyGetGraphicsDirPath(),
    bskyEvalDebug = FALSE, additionalBskyEvalDebug = FALSE, splitCountDisplay = BSkyGetSplitCountDisplaySetting())
{
    if (bskyEvalDebug == TRUE) {
        if (additionalBskyEvalDebug == TRUE) {
            cat("callStackIndex and callStack\n")
            print(uadatasets.sk$callStackIndex)
            print(uadatasets.sk$callStack)
        }
        cat("\nParameters passed to BSKyEval function\n")
        print(match.call())
        cat("\n")
        cat("\nR command(s) passed\n")
        line_breakdown_RcommandString = data.frame(strsplit(RcommandString,
            "\n"))
        line_breakdown_RcommandString = cbind(line_breakdown_RcommandString,
            lapply(line_breakdown_RcommandString, nchar))
        line_breakdown_RcommandString[, 2] = line_breakdown_RcommandString[,
            2] + 1
        line_breakdown_RcommandString = cbind(line_breakdown_RcommandString,
            cumsum(line_breakdown_RcommandString[, 2]))
        line_breakdown_RcommandString = cbind(seq(1:nrow(line_breakdown_RcommandString)),
            line_breakdown_RcommandString)
        names(line_breakdown_RcommandString) = c("lineNum", "lineTxt",
            "lineTxtCharCount", "lineTxtCumCharCount")
        BSkyFormat(line_breakdown_RcommandString)
    }
    if (additionalBskyEvalDebug == TRUE) {
        cat("\nselectionStartpos ", selectionStartpos, "selectionEndpos ",
            selectionEndpos, "nchar(RcommandString) ", nchar(RcommandString))
        if (selectionStartpos <= nchar(RcommandString)) {
            selectionEndpos_temp = selectionEndpos
            if (selectionStartpos > 0 && (selectionEndpos > nchar(RcommandString) ||
                selectionEndpos == 0 || selectionEndpos < selectionStartpos)) {
                selectionEndpos_temp = nchar(RcommandString)
            }
            cat("\n R commands (possibly partial display) to be executed: \n")
            cat(substr(RcommandString, selectionStartpos, selectionEndpos_temp))
            cat("\n========================\n")
        }
    }
    uadatasets.sk$BSkyEvalErrors = 0
    uadatasets.sk$callStack <- NULL
    uadatasets.sk$callStackIndex = 0
    if (ignoreSplitOn == FALSE) {
        BSkyFunctionInit()
    }
    bsky_Rmarkdown_settings = BSkyGetKableAndRmarkdownFormatting()
    RcommandString_before_any_modification = RcommandString
    if (selectionStartpos > 0 && (selectionEndpos > nchar(RcommandString) ||
        selectionEndpos == 0 || selectionEndpos < selectionStartpos)) {
        selectionEndpos = nchar(RcommandString)
    }
    if (length(RcommandString) > 0 && selectionStartpos <= nchar(RcommandString)) {
        charPosOffsetAdjutment = 0
        linePosOffsetAdjutment = 1
        if (selectionStartpos > 0) {
            linePosOffsetAdjutment = BSkyRCommandLineNumberFromCharCount(RcommandString_before_any_modification,
                selectionStartpos)
            selection_only_first_expr_found = FALSE
            RcommandStringSelect_parse_test = -1
            if (executeSelectOnly == FALSE) {
                RcommandStringSelect = substr(RcommandString,
                  selectionStartpos, selectionEndpos)
                RcommandStringSelect_parse_test = BSkyRCommandParsingTest(RcommandString = RcommandStringSelect,
                  numExprParse = numExprParse, bskyEvalDebug = additionalBskyEvalDebug)
                if (RcommandStringSelect_parse_test == 0) {
                  find_first_expression = BSkyRCommandParsedExprBoundary(RcommandString = RcommandStringSelect,
                    numExprParse = numExprParse, selectionStartpos = 0,
                    selectionEndpos = 0, linePosOffsetAdjutment = linePosOffsetAdjutment,
                    bskyEvalDebug = additionalBskyEvalDebug)
                  if (find_first_expression$firstExprStartPos >
                    0 && find_first_expression$lastExprEndPos >
                    0) {
                    RcommandString = substr(RcommandString, selectionStartpos,
                      selectionEndpos)
                    charPosOffsetAdjutment = selectionStartpos
                    selectionStartpos = 0
                    selectionEndpos = 0
                    selection_only_first_expr_found = TRUE
                  }
                }
                if (selection_only_first_expr_found == FALSE) {
                  if (RcommandStringSelect_parse_test == 0) {
                    find_first_expression = BSkyRCommandParsedExprBoundary(RcommandString = substr(RcommandString,
                      selectionStartpos, nchar(RcommandString)),
                      numExprParse = 1, selectionStartpos = 0,
                      selectionEndpos = 0, linePosOffsetAdjutment = linePosOffsetAdjutment,
                      bskyEvalDebug = additionalBskyEvalDebug)
                    find_first_expression$firstExprStartPos = find_first_expression$firstExprStartPos +
                      selectionStartpos - 1
                    find_first_expression$lastExprEndPos = find_first_expression$lastExprEndPos +
                      selectionStartpos - 1
                  }
                  else {
                    find_first_expression = BSkyRCommandParsedExprBoundary(RcommandString = RcommandString,
                      numExprParse = 1, selectionStartpos = selectionStartpos,
                      selectionEndpos = 0, linePosOffsetAdjutment = 1,
                      bskyEvalDebug = additionalBskyEvalDebug)
                  }
                  if (additionalBskyEvalDebug == TRUE) {
                    cat("\n Printitng find_first_expression returned by BSkyRCommandParsedExprBoundary in BSkyEval\n")
                    print(nchar(RcommandString))
                    print(find_first_expression)
                  }
                  if (find_first_expression$parsingStatus ==
                    -1) {
                    if (ignoreSplitOn == FALSE) {
                      BSkyFunctionWrapUp()
                    }
                    return(invisible(list(executionStatus = find_first_expression$parsingStatus,
                      parsingStatus = find_first_expression$parsingStatus,
                      parsingErrorLineNum = find_first_expression$parsingErrorLineNum,
                      totalCharCount = find_first_expression$totalCharCount,
                      firstExprStartPos = find_first_expression$firstExprStartPos +
                        1, lastExprEndPos = find_first_expression$lastExprEndPos,
                      parsedCommandList = find_first_expression$parsedCommandList)))
                  }
                  else {
                    if (selectionStartpos != find_first_expression$firstExprStartPos ||
                      selectionEndpos != find_first_expression$lastExprEndPos) {
                      additional_char_count = 0
                      if (find_first_expression$lastExprEndPos <
                        nchar(RcommandString) && (substr(RcommandString,
                        find_first_expression$lastExprEndPos,
                        find_first_expression$lastExprEndPos) !=
                        "\n" && substr(RcommandString, find_first_expression$lastExprEndPos,
                        find_first_expression$lastExprEndPos) !=
                        "\r")) {
                        for (j in (find_first_expression$lastExprEndPos +
                          1):nchar(RcommandString)) {
                          if (substr(RcommandString, j, j) !=
                            "\n" && substr(RcommandString, j,
                            j) != "\r") {
                            additional_char_count = additional_char_count +
                              1
                          }
                          else {
                            break
                          }
                        }
                      }
                      if (selectionEndpos < (find_first_expression$lastExprEndPos +
                        additional_char_count)) {
                        selectionEndpos = find_first_expression$lastExprEndPos +
                          additional_char_count
                      }
                    }
                  }
                  if (RcommandStringSelect_parse_test == 0 ||
                    find_first_expression$parsingStatus == 0) {
                    if (selectionStartpos > find_first_expression$firstExprStartPos) {
                      selectionStartpos = find_first_expression$firstExprStartPos
                      linePosOffsetAdjutment = BSkyRCommandLineNumberFromCharCount(RcommandString_before_any_modification,
                        selectionStartpos)
                    }
                    RcommandString = substr(RcommandString, selectionStartpos,
                      nchar(RcommandString))
                    charPosOffsetAdjutment = selectionStartpos
                    selectionEndpos = selectionEndpos - selectionStartpos +
                      1
                    selectionStartpos = 0
                  }
                }
            }
        }
        if (executeSelectOnly == TRUE && selectionStartpos >
            0 && selectionEndpos > 0) {
            RcommandString = substr(RcommandString, selectionStartpos,
                selectionEndpos)
            charPosOffsetAdjutment = selectionStartpos
            selectionStartpos = 0
            selectionEndpos = 0
        }
        no_expresson_to_execute = FALSE
        Rcommands_initial_parse = BSkyRCommandParsedExprBoundary(RcommandString = RcommandString,
            numExprParse = numExprParse, selectionStartpos = selectionStartpos,
            selectionEndpos = selectionEndpos, linePosOffsetAdjutment = linePosOffsetAdjutment,
            bskyEvalDebug = additionalBskyEvalDebug)
        if (additionalBskyEvalDebug == TRUE) {
            cat("\n Printitng Rcommands_initial_parse returned by BSkyRCommandParsedExprBoundary in BSkyEval\n")
            print(nchar(RcommandString))
            print(RcommandString)
            print(Rcommands_initial_parse)
        }
        if (Rcommands_initial_parse$firstExprStartPos > 0 &&
            Rcommands_initial_parse$lastExprEndPos > 0) {
            RcommandString = substr(RcommandString, Rcommands_initial_parse$firstExprStartPos,
                Rcommands_initial_parse$lastExprEndPos)
        }
        else {
            no_expresson_to_execute = TRUE
        }
    }
    else {
        if (ignoreSplitOn == FALSE) {
            BSkyFunctionWrapUp()
        }
        return(invisible(list(executionStatus = -1, parsingStatus = -1,
            parsingErrorLineNum = -1, totalCharCount = 0, firstExprStartPos = 0,
            lastExprEndPos = 0, parsedCommandList = c())))
    }
    if (charPosOffsetAdjutment > 1) {
        Rcommands_initial_parse$totalCharCount = Rcommands_initial_parse$totalCharCount +
            charPosOffsetAdjutment - 1
        Rcommands_initial_parse$firstExprStartPos = Rcommands_initial_parse$firstExprStartPos +
            charPosOffsetAdjutment - 1
        Rcommands_initial_parse$lastExprEndPos = Rcommands_initial_parse$lastExprEndPos +
            charPosOffsetAdjutment - 1
    }
    RcommandString_before_any_modification_length = nchar(RcommandString_before_any_modification)
    if (Rcommands_initial_parse$lastExprEndPos > 0 && Rcommands_initial_parse$lastExprEndPos <
        RcommandString_before_any_modification_length) {
        newline_count = 0
        if (additionalBskyEvalDebug == TRUE) {
            cat("\n prnting ASCII value of the traialing character to skip \\n, \\r and blanks\n")
        }
        for (i in (Rcommands_initial_parse$lastExprEndPos + 1):RcommandString_before_any_modification_length) {
            if (additionalBskyEvalDebug == TRUE) {
                cat(charToRaw(substr(RcommandString_before_any_modification,
                  i, i)))
                cat("\n")
            }
            if (substr(RcommandString_before_any_modification,
                i, i) == "\n" || substr(RcommandString_before_any_modification,
                i, i) == "\r" || trimws(substr(RcommandString_before_any_modification,
                i, i)) == "") {
                newline_count = newline_count + 1
            }
            else {
                break
            }
        }
        Rcommands_initial_parse$totalCharCount = Rcommands_initial_parse$totalCharCount +
            newline_count
        Rcommands_initial_parse$lastExprEndPos = Rcommands_initial_parse$lastExprEndPos +
            newline_count
    }
    if (no_expresson_to_execute == TRUE) {
        Rcommands_initial_parse$firstExprStartPos = Rcommands_initial_parse$firstExprStartPos +
            1
        if (Rcommands_initial_parse$parsingStatus == -1) {
            if (ignoreSplitOn == FALSE) {
                BSkyFunctionWrapUp()
            }
            return(invisible(list(executionStatus = Rcommands_initial_parse$parsingStatus,
                parsingStatus = Rcommands_initial_parse$parsingStatus,
                parsingErrorLineNum = Rcommands_initial_parse$parsingErrorLineNum,
                totalCharCount = Rcommands_initial_parse$totalCharCount,
                firstExprStartPos = Rcommands_initial_parse$firstExprStartPos,
                lastExprEndPos = Rcommands_initial_parse$lastExprEndPos,
                parsedCommandList = Rcommands_initial_parse$parsedCommandList)))
        }
    }
    if (is.null(currentColumnNames) || trimws(currentColumnNames) ==
        "") {
        currentColumnNames = c()
    }
    if (is.null(replaceOldColumnNames) || trimws(replaceOldColumnNames) ==
        "") {
        replaceOldColumnNames = c()
    }
    if (!is.null(graphicsDir) && length(graphicsDir) > 0 && trimws(graphicsDir) !=
        "" && dir.exists(graphicsDir) && bsky_Rmarkdown_settings$doRmarkdownFormatting ==
        FALSE) {
        full_file_names = list.files(path = graphicsDir, pattern = "png|svg",
            full.names = TRUE)
        uadatasets.sk$initial_graphics_file_name = full_file_names[which.max(file.mtime(full_file_names))]
        uadatasets.sk$strating_count_of_bsky_graphics_files = length(full_file_names)
        uadatasets.sk$last_count_of_bsky_graphics_files = uadatasets.sk$strating_count_of_bsky_graphics_files
        plot.new()
    }
    if (class(echo)[1] == "list") {
        echoRcommand = echo$echo
    }
    else {
        echoRcommand = echo
    }
    if (class(echoInline)[1] == "list") {
        echoInlineRcommand = echoInline$echoInline
    }
    else {
        echoInlineRcommand = echoInline
    }
    if (!is.null(currentDatasetName) && length(currentDatasetName) >
        0 && trimws(currentDatasetName) != "" && (exists(currentDatasetName,
        envir = .GlobalEnv) && !(eval(parse(text = paste("is.null(",
        currentDatasetName, ")")))))) {
        BSkySetCurrentDatasetName(currentDatasetName, setDatasetIndex = "y")
        working_datasetName = currentDatasetName
        if (!is.null(replaceOldDatasetName) && length(replaceOldDatasetName) >
            0 && trimws(replaceOldDatasetName) != "") {
            RcommandString_modified = BSkyDatasetNameSubstitute(datasetName = replaceOldDatasetName,
                toDatasetName = currentDatasetName, replaceOldColumnNames = replaceOldColumnNames,
                currentColumnNames = currentColumnNames, RcommandString = RcommandString)
        }
        else {
            RcommandString_modified = RcommandString
        }
        isSplitOn = uaIsDatasetSplit(working_datasetName)
    }
    else {
        RcommandString_modified = RcommandString
        isSplitOn = FALSE
    }
    if (isSplitOn == TRUE && ignoreSplitOn == FALSE) {
        if (echoRcommand == TRUE && echoInlineRcommand == FALSE) {
            cat("\n")
            if (bsky_Rmarkdown_settings$doRmarkdownFormatting ==
                TRUE && bsky_Rmarkdown_settings$doLatexFormatting ==
                FALSE) {
                RcommandString_modified_print = gsub("\\n", "<br>",
                  RcommandString_modified)
                cat("<pre class=\"r\"><code>")
                cat(RcommandString_modified_print)
                cat("<br>")
                cat("</code></pre>")
            }
            else {
                cat(RcommandString_modified)
            }
            cat("\n")
        }
        bSkyVarnamesSplit = dimnames(eval(parse(text = working_datasetName),
            envir = globalenv()))[[2]]
        bSkySplitIterationCount = BSkyComputeSplitdataset(bSkyVarnamesSplit,
            working_datasetName)
        splitDatasetName = "uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]]"
        if (!is.null(replaceOldDatasetName) && length(replaceOldDatasetName) >
            0 && trimws(replaceOldDatasetName) != "") {
            RcommandString_modified_split_dataset = BSkyDatasetNameSubstitute(datasetName = replaceOldDatasetName,
                toDatasetName = splitDatasetName, replaceOldColumnNames = replaceOldColumnNames,
                currentColumnNames = currentColumnNames, RcommandString = RcommandString,
                splitOn = TRUE, preSplitDatasetName = working_datasetName)
        }
        else {
            RcommandString_modified_split_dataset = BSkyDatasetNameSubstitute(datasetName = working_datasetName,
                toDatasetName = splitDatasetName, replaceOldColumnNames = replaceOldColumnNames,
                currentColumnNames = currentColumnNames, RcommandString = RcommandString,
                splitOn = TRUE, preSplitDatasetName = working_datasetName)
        }
        for (bSkySplitIterationCounter in 1:bSkySplitIterationCount) {
            bSkyDatasetSliceIndex = BSkyGetNextDatasetSliceIndex(bSkyVarnamesSplit,
                working_datasetName)
            bSkyGlobalDataSliceIndexToWorkOn = bSkyDatasetSliceIndex$datasetSliceIndex
            assign("bSkyGlobalDataSliceIndexToWorkOn", bSkyDatasetSliceIndex$datasetSliceIndex,
                envir = .GlobalEnv)
            BSkySplit_footer_str = paste(BSkyComputeCurrentVarNamesAndFactorValues(working_datasetName),
                collapse = ",")
            BSkySplit_footer_str = strsplit(substr(BSkySplit_footer_str,
                12, nchar(BSkySplit_footer_str)), ",")
            BSkyFormat_BSkySplit_footer_str = trimws(paste("Split: ",
                paste(BSkySplit_footer_str[[1]][2:length(BSkySplit_footer_str[[1]])],
                  collapse = ",")))
            RcommandString_modified_split_footer = BSkyExpandBSkyFormatWithFooter(RcommandString_modified_split_dataset,
                BSkyFormat_BSkySplit_footer_str)
            if (dim(uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]])[1] >
                0 && dim(uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]])[2] >
                0) {
                if (splitCountDisplay == FALSE) {
                  BSkyFormat(paste("\n", "Begins ", BSkyFormat_BSkySplit_footer_str,
                    "\n"))
                }
                else {
                  BSkySplit_footer_str = paste(BSkySplit_footer_str[[1]],
                    collapse = ",")
                  BSkyFormat(paste("\n", "Begins ", BSkySplit_footer_str,
                    "\n"))
                }
                ret_char_count_array = BSkyEvalRcommandBasicForAppScript(RcommandString = RcommandString_modified_split_footer,
                  origRcommands = RcommandString_modified, echo = echoRcommand,
                  echoInline = echoInlineRcommand, splitOn = TRUE,
                  graphicsDir = graphicsDir, bskyEvalDebug = additionalBskyEvalDebug)
            }
            else {
                if (splitCountDisplay == FALSE) {
                  BSkyFormat(paste("\n", "Begins ", BSkyFormat_BSkySplit_footer_str,
                    "\n", "Split Dataset is empty - skipping"))
                }
                else {
                  BSkySplit_footer_str = paste(BSkySplit_footer_str[[1]],
                    collapse = ",")
                  BSkyFormat(paste("\n", "Begins ", BSkySplit_footer_str,
                    "\n", "Split Dataset is empty - skipping"))
                }
            }
        }
    }
    else {
        ret_char_count_array = BSkyEvalRcommandBasicForAppScript(RcommandString = RcommandString_modified,
            echo = echoRcommand, echoInline = echoInlineRcommand,
            graphicsDir = graphicsDir, bskyEvalDebug = additionalBskyEvalDebug)
    }
    if (ignoreSplitOn == FALSE) {
        BSkyFunctionWrapUp()
    }
    overall_execution_Status = -1
    if (uadatasets.sk$BSkyEvalErrors == 0 && Rcommands_initial_parse$parsingStatus ==
        0) {
        overall_execution_Status = 0
    }
    if (bskyEvalDebug == TRUE) {
        cat("\nTotal eval() execution errors encountered\n")
        print(uadatasets.sk$BSkyEvalErrors)
    }
    if (bskyEvalDebug == TRUE) {
        print("**********Returning from BSky Eval function********")
        print(Rcommands_initial_parse)
    }
    return(invisible(list(executionStatus = overall_execution_Status,
        parsingStatus = Rcommands_initial_parse$parsingStatus,
        parsingErrorLineNum = Rcommands_initial_parse$parsingErrorLineNum,
        totalCharCount = Rcommands_initial_parse$totalCharCount,
        firstExprStartPos = Rcommands_initial_parse$firstExprStartPos,
        lastExprEndPos = Rcommands_initial_parse$lastExprEndPos,
        parsedCommandList = Rcommands_initial_parse$parsedCommandList)))
}

#commented out on 060624
# Needed to create copy from the original BSkyEvalBasicRcommand() to avoid the recurssion error to execute R code block from app in BMD script
BSkyEvalRcommandBasicForAppScript.withTidy.retired <- function (RcommandString, origRcommands = c(), echo = BSkyGetRCommandDisplaySetting(),
    echoInline = BSkyGetRCommandDisplaySetting(), splitOn = FALSE,
    graphicsDir = BSkyGetGraphicsDirPath(), bskyEvalDebug = FALSE)
{
    if (bskyEvalDebug == TRUE) {
        print("printing in BSkyEvalRcommandBasicForAppScript")
        print(RcommandString)
        cat("\n==============origRcommands============= \n")
        print(origRcommands)
        cat("\n=========================== \n")
    }
    parsed_Rcommands = c()
    parsed_orig_Rcommands = c()
    if (is.null(origRcommands) || trimws(origRcommands) == "") {
        origRcommands = c()
    }
    bsky_Rmarkdown_settings = BSkyGetKableAndRmarkdownFormatting()
    first_Graphics_Command_Executed = FALSE
    graphicsDir_exists = FALSE
    if (!is.null(graphicsDir) && length(graphicsDir) > 0 && trimws(graphicsDir) !=
        "" && dir.exists(graphicsDir) && bsky_Rmarkdown_settings$doRmarkdownFormatting ==
        FALSE) {
        graphicsDir_exists = TRUE
    }
    if (class(echo)[1] == "list") {
        echo = echo$echo
    }
    if (class(echoInline)[1] == "list") {
        echoInline = echoInline$echoInline
    }
    parsed_Rcommands_by_R_parse = parse(text = {
        RcommandString
    }, keep.source = TRUE)
    if (bskyEvalDebug == TRUE) {
        print("printing parsed_Rcommands from parse()")
        print(parsed_Rcommands_by_R_parse)
    }
    eval(parse(text = "bsky_rcommand_parsing_an_exception_occured = FALSE"),
        envir = globalenv())
    tryCatch({
        withCallingHandlers({
            parsed_Rcommands = (tidy_source(text = RcommandString,
                output = FALSE))$text.tidy
        }, warning = BSkyRcommandParsingErrWarnHandlerForAppScript, silent = TRUE)
    }, error = BSkyRcommandParsingErrWarnHandlerForAppScript, silent = TRUE)
    if (bsky_rcommand_parsing_an_exception_occured == TRUE) {
        if (bskyEvalDebug == TRUE) {
            cat("\nParsing Error in tidy_source\n")
            cat(RcommandString)
            cat("\n")
        }
        parsed_Rcommands = (tidy_source(text = as.character(parsed_Rcommands_by_R_parse),
            output = FALSE))$text.tidy
        parsed_Rcommands_by_R_parse_srcref = attr(parsed_Rcommands_by_R_parse,
            "srcref")
    }
    if (bskyEvalDebug == TRUE) {
        parsed_Rcommands = (tidy_source(text = as.character(parsed_Rcommands_by_R_parse),
            output = FALSE))$text.tidy
        parsed_Rcommands_by_R_parse_srcref = attr(parsed_Rcommands_by_R_parse,
            "srcref")
        cat("\n============== parsed_Rcommands in parse() and tidy_source() ============= \n")
        print("printing parsed_Rcommands_by_R_parse_srcref from parse()")
        print(parsed_Rcommands_by_R_parse_srcref)
        cat("\n++++++++++++++++++++++++++++++++++ \n")
        print("printing parsed_Rcommands from tidy_source()")
        print(parsed_Rcommands)
        cat("\n=========================== \n")
    }
    if (length(origRcommands) > 0) {
        parsed_orig_Rcommands_by_R_parse = parse(text = {
            origRcommands
        }, keep.source = TRUE)
        if (bsky_rcommand_parsing_an_exception_occured == TRUE) {
            parsed_orig_Rcommands = (tidy_source(text = as.character(parsed_orig_Rcommands_by_R_parse),
                output = FALSE))$text.tidy
            parsed_orig_Rcommands_by_R_parse_srcref = attr(parsed_orig_Rcommands_by_R_parse,
                "srcref")
        }
        else {
            parsed_orig_Rcommands = (tidy_source(text = origRcommands,
                output = FALSE))$text.tidy
        }
        if (bskyEvalDebug == TRUE) {
            parsed_orig_Rcommands = (tidy_source(text = as.character(parsed_orig_Rcommands_by_R_parse),
                output = FALSE))$text.tidy
            parsed_orig_Rcommands_by_R_parse_srcref = attr(parsed_orig_Rcommands_by_R_parse,
                "srcref")
            cat("\n============== parsed_orig_Rcommands in parse() and tidy_source() ============= \n")
            print("printing parsed_orig_Rcommands_by_R_parse_srcref from parse()")
            print(parsed_orig_Rcommands_by_R_parse_srcref)
            cat("\n++++++++++++++++++++++++++++++++++ \n")
            print("printing parsed_orig_Rcommands from tidy_source()")
            print(parsed_orig_Rcommands)
            cat("\n=========================== \n")
        }
    }
    if (bskyEvalDebug == TRUE) {
        if (bsky_Rmarkdown_settings$doRmarkdownFormatting ==
            TRUE && bsky_Rmarkdown_settings$doLatexFormatting ==
            FALSE) {
            cat("<pre class=\"r\"><code>")
        }
        if (length(origRcommands) > 0) {
            print(origRcommands)
            cat("\nExpressions to be executed (orig)\n")
            print(parsed_orig_Rcommands)
        }
        else {
            print(RcommandString)
            cat("\nExpressions to be executed\n")
            print(parsed_Rcommands)
        }
        if (bsky_Rmarkdown_settings$doRmarkdownFormatting ==
            TRUE && bsky_Rmarkdown_settings$doLatexFormatting ==
            FALSE) {
            cat("</code></pre>")
        }
    }
    if (echo == TRUE && echoInline == FALSE && splitOn == FALSE) {
        cat("\n")
        if (bsky_Rmarkdown_settings$doRmarkdownFormatting ==
            TRUE && bsky_Rmarkdown_settings$doLatexFormatting ==
            FALSE) {
            cat("<pre class=\"r\"><code>")
            if (length(origRcommands) > 0) {
                RcommandString_modified_print = gsub("\\n", "<br>",
                  origRcommands)
                cat(RcommandString_modified_print)
                cat("<br>")
            }
            else {
                RcommandString_modified_print = gsub("\\n", "<br>",
                  RcommandString)
                cat(RcommandString_modified_print)
                cat("<br>")
            }
            cat("</code></pre>")
        }
        else {
            if (length(origRcommands) > 0) {
                cat(origRcommands)
            }
            else {
                cat(RcommandString)
            }
        }
        cat("\n")
    }
    for (i in seq_along(parsed_Rcommands)) {
        eval(parse(text = "bsky_rcommand_execution_an_exception_occured1 = FALSE"),
            envir = globalenv())
        if (echo == TRUE && echoInline == TRUE) {
            cat("\n")
            if (bsky_Rmarkdown_settings$doRmarkdownFormatting ==
                TRUE && bsky_Rmarkdown_settings$doLatexFormatting ==
                FALSE) {
                cat("<pre class=\"r\"><code>")
            }
            if (length(origRcommands) > 0) {
                if (bsky_rcommand_parsing_an_exception_occured ==
                  TRUE) {
                  print(parsed_orig_Rcommands_by_R_parse_srcref[[i]])
                }
                else {
                  cat(parsed_orig_Rcommands[[i]])
                }
            }
            else {
                if (bsky_rcommand_parsing_an_exception_occured ==
                  TRUE) {
                  print(parsed_Rcommands_by_R_parse_srcref[[i]])
                }
                else {
                  cat(parsed_Rcommands[[i]])
                }
            }
            if (bsky_Rmarkdown_settings$doRmarkdownFormatting ==
                TRUE && bsky_Rmarkdown_settings$doLatexFormatting ==
                FALSE) {
                cat("</code></pre>")
            }
            cat("\n")
        }
        isCommentOrBlankLine = FALSE
        isHelpCommand = FALSE
        isPkgInstallCommand = FALSE
        HelpOrCommentOrBlankLineStr = trimws(parsed_Rcommands[[i]])
        if (substr(HelpOrCommentOrBlankLineStr, 1, 3) == "`?`" ||
            substr(HelpOrCommentOrBlankLineStr, 1, 5) == "help(" ||
            substr(HelpOrCommentOrBlankLineStr, 1, 5) == "help ") {
            helpfile = c()
            package_only_help_command = FALSE
            package_name = c()
            topic_param_name_found = grep("\\btopic\\b", HelpOrCommentOrBlankLineStr)
            if (length(topic_param_name_found) == 0) {
                package_param_name_found = regexpr("(\\bpackage(\\s)*=)",
                  HelpOrCommentOrBlankLineStr)
                if (package_param_name_found != -1) {
                  y = strsplit(HelpOrCommentOrBlankLineStr, ",")
                  non_topic_param_in_1st_place = grep("=", y[[1]][1])
                  if (length(non_topic_param_in_1st_place) !=
                    0) {
                    substr_start_from_package_param = substr(HelpOrCommentOrBlankLineStr,
                      package_param_name_found, nchar(HelpOrCommentOrBlankLineStr))
                    package_param = strsplit(substr_start_from_package_param,
                      ",")
                    package_name = strsplit(package_param[[1]][1],
                      "=")
                    if (trimws(package_name[[1]][1]) == "package") {
                      package_name = trimws(package_name[[1]][2])
                      nchar_package_name = nchar(package_name)
                      if (nchar_package_name > 0) {
                        package_name = trimws((strsplit(package_name,
                          "\""))[[1]][2])
                      }
                      package_only_help_command = TRUE
                    }
                  }
                }
            }
            if (BSkyGetRHelpHTTPServer() == "R_none" || BSkyGetRHelpParallelHTTPServerPortNumber() ==
                0) {
                options(help_type = "text")
                tryCatch({
                  withCallingHandlers({
                    withAutoprint({
                      {
                        helpfile = eval(parse(text = parsed_Rcommands[[i]]),
                          envir = globalenv())
                      }
                    }, print. = TRUE, echo = FALSE, deparseCtrl = c("keepInteger",
                      "showAttributes", "keepNA"), keep.source = TRUE)
                  }, warning = BSkyRcommandErrWarnHandlerForAppScript, silent = TRUE)
                }, error = BSkyRcommandErrWarnHandlerForAppScript, silent = TRUE)
                if (bsky_rcommand_execution_an_exception_occured1 ==
                  FALSE) {
                  if (bsky_Rmarkdown_settings$doRmarkdownFormatting ==
                    TRUE && bsky_Rmarkdown_settings$doLatexFormatting ==
                    FALSE) {
                    cat("\n")
                    cat("<pre class=\"r\"><code>")
                  }
                  if (class(helpfile)[1] == "help_files_with_topic" &&
                    length(as.character(helpfile)) > 0) {
                    temp_help_file_path = c()
                    pkgname <- basename(dirname(dirname(helpfile)))
                    if (length(pkgname) > 1) {
                      cat("\nhelp topic is found in multiple packages: ",
                        pkgname, "\n")
                      cat("Displaying the help topic from the first package",
                        pkgname[1], "found in the package search path\n")
                      cat("if you want the help(topic) from a specific package, add the package parameter to help(topic, package = 'pkg name')\n")
                    }
                    temp_help_file_path <- tools::Rd2HTML(utils:::.getHelpFile(helpfile[1]),
                      out = tempfile(pattern = "BSkyhelpsink",
                        fileext = ".html"), package = pkgname[1])
                    cat("\n")
                      cat(paste("BSkyHelpCommandMarker ", temp_help_file_path, sep=""))
                    cat("\n")
                  }
                  else if (class(helpfile)[1] == "packageInfo") {
                    title = gettextf("Documentation for package %s",
                      sQuote(helpfile$name))
                    outFile = tempfile(pattern = "BSkyhelpsink",
                      fileext = ".html")
                    content = format(helpfile)
                    cat("<!DOCTYPE html>\n<html>\n<body>\n",
                      file = outFile, append = TRUE)
                    cat("<table>\n", file = outFile, append = TRUE)
                    for (cont in 1:length(content)) {
                      content[cont] = trimws(content[cont])
                      splitline = gregexpr(":|\\s+", content[cont])
                      cat("<tr>\n", file = outFile, append = TRUE)
                      if (splitline[[1]][1] == -1) {
                        cat("<td colspan=\"2\">\n", file = outFile,
                          append = TRUE)
                        cat(content[cont], file = outFile, append = TRUE)
                        cat("\n</td>\n", file = outFile, append = TRUE)
                      }
                      else {
                        cat("<td>\n", file = outFile, append = TRUE)
                        cat(substr(content[cont], 1, (splitline[[1]][1])),
                          file = outFile, append = TRUE)
                        cat("\n</td>\n", file = outFile, append = TRUE)
                        if ((splitline[[1]][1] + 1) <= nchar(content[cont])) {
                          cat("<td>\n", file = outFile, append = TRUE)
                          cat(substr(content[cont], (splitline[[1]][1] +
                            1), nchar(content[cont])), file = outFile,
                            append = TRUE)
                          cat("\n</td>\n", file = outFile, append = TRUE)
                        }
                      }
                      cat("</tr>\n", file = outFile, append = TRUE)
                    }
                    cat("</table>\n", file = outFile, append = TRUE)
                    cat("\n</body>\n</html>", file = outFile,
                      append = TRUE)
                    cat("\n")
						cat(paste("BSkyHelpCommandMarker ", outFile, sep=""))
                    cat("\n")
                  }
                  else {
                    cat("\n", paste("No documentation for the package or the topic in specified packages and libraries :",
                      parsed_Rcommands[[i]]), "\n")
                  }
                  if (bsky_Rmarkdown_settings$doRmarkdownFormatting ==
                    TRUE && bsky_Rmarkdown_settings$doLatexFormatting ==
                    FALSE) {
                    cat("</code></pre>")
                    cat("\n")
                  }
                }
                else {
                  eval(parse(text = "bsky_rcommand_execution_an_exception_occured1 = FALSE"),
                    envir = globalenv())
                }
                options(help_type = "html")
                isHelpCommand = TRUE
            }
            else {
                if (BSkyGetRHelpHTTPServer() == "R_parallel") {
                  options(help_type = "html")
                  port = BSkyGetRHelpParallelHTTPServerPortNumber()
                  if (package_only_help_command == TRUE) {
                    options(help_type = "text")
                    tryCatch({
                      withCallingHandlers({
                        withAutoprint({
                          {
                            helpfile = eval(parse(text = parsed_Rcommands[[i]]),
                              envir = globalenv())
                          }
                        }, print. = TRUE, echo = FALSE, deparseCtrl = c("keepInteger",
                          "showAttributes", "keepNA"), keep.source = TRUE)
                      }, warning = BSkyRcommandErrWarnHandlerForAppScript,
                        silent = TRUE)
                    }, error = BSkyRcommandErrWarnHandlerForAppScript, silent = TRUE)
                    if (bsky_rcommand_execution_an_exception_occured1 ==
                      FALSE) {
                      options(help_type = "html")
                      if (length(package_name) > 0) {
                        browseURL(paste0("http://127.0.0.1:",
                          port, "/library/", package_name, "/html/00Index.html"))
                      }
                    }
                    else {
                      eval(parse(text = "bsky_rcommand_execution_an_exception_occured1 = FALSE"),
                        envir = globalenv())
                    }
                  }
                  else {
                    options(help_type = "text")
                    tryCatch({
                      withCallingHandlers({
                        withAutoprint({
                          {
                            helpfile = eval(parse(text = parsed_Rcommands[[i]]),
                              envir = globalenv())
                          }
                        }, print. = TRUE, echo = FALSE, deparseCtrl = c("keepInteger",
                          "showAttributes", "keepNA"), keep.source = TRUE)
                      }, warning = BSkyRcommandErrWarnHandlerForAppScript,
                        silent = TRUE)
                    }, error = BSkyRcommandErrWarnHandlerForAppScript, silent = TRUE)
                    if (bsky_rcommand_execution_an_exception_occured1 ==
                      FALSE) {
                      options(help_type = "html")
                      if (bsky_Rmarkdown_settings$doRmarkdownFormatting ==
                        TRUE && bsky_Rmarkdown_settings$doLatexFormatting ==
                        FALSE) {
                        cat("\n")
                        cat("<pre class=\"r\"><code>")
                      }
                      if (class(helpfile)[1] == "help_files_with_topic" &&
                        length(as.character(helpfile)) > 0) {
                        pkgname <- basename(dirname(dirname(helpfile)))
                        if (length(pkgname) > 1) {
                          cat("\nhelp topic is found in multiple packages: ",
                            pkgname, "\n")
                          cat("Displaying the help topic from the first package",
                            pkgname[1], "found in the package search path\n")
                          cat("if you want the help(topic) from a specific package, add the package parameter to help(topic, package = 'pkg name')\n")
                        }
                        pkgname = pkgname[1]
                        topic = basename(helpfile[1])
                        browseURL(paste0("http://127.0.0.1:",
                          port, "/library/", pkgname, "/html/",
                          topic, ".html"))
                      }
                    }
                    else {
                      eval(parse(text = "bsky_rcommand_execution_an_exception_occured1 = FALSE"),
                        envir = globalenv())
                    }
                  }
                  isHelpCommand = TRUE
                }
                else {
                  options(help_type = "html")
                  port = tools::startDynamicHelp(NA)
                  if (port <= 0) {
                    cat("\nR HTML help server could not be started\n")
                    cat("\nGo to BlueSky Application configuration menu dialog and uncheck the R help server option and then try help(..) command again\n")
                  }
                  if (package_only_help_command == TRUE) {
                    if (length(package_name) > 0) {
                      browseURL(paste0("http://127.0.0.1:", port,
                        "/library/", package_name, "/html/00Index.html"))
                    }
                    isHelpCommand = TRUE
                  }
                }
            }
        }
        else if (substr(HelpOrCommentOrBlankLineStr, 1, 1) ==
            "#" || substr(HelpOrCommentOrBlankLineStr, 1, 1) ==
            "") {
            isCommentOrBlankLine = TRUE
        }
        else {
            if (length(grep("install\\.packages(\\s*)\\(|update\\.packages|install_github(\\s*)\\(|devtools::install_github(\\s*)\\(|::install|githubinstall|^install_(\\s|\\S)*\\(",
                HelpOrCommentOrBlankLineStr)) > 0) {
                if (length(grep("BSkypackageinstall", HelpOrCommentOrBlankLineStr)) ==
                  0) {
                  isPkgInstallCommand = TRUE
                  cat("\nPLEASE NOTE: For package installation and update, please see triple dot > Install R Package from the top level menu in the BlueSky Statistics application\n")
                }
            }
        }
        if (isCommentOrBlankLine == FALSE && isHelpCommand ==
            FALSE && isPkgInstallCommand == FALSE) {
            tryCatch({
                withCallingHandlers({
                  withAutoprint({
                    {
                      eval(parse(text = parsed_Rcommands[[i]]),
                        envir = globalenv())
                    }
                  }, print. = TRUE, echo = FALSE, deparseCtrl = c("keepInteger",
                    "showAttributes", "keepNA"), keep.source = TRUE)
                }, warning = BSkyRcommandErrWarnHandlerForAppScript, silent = TRUE)
            }, error = BSkyRcommandErrWarnHandlerForAppScript, silent = TRUE)
            if (bsky_rcommand_execution_an_exception_occured1 ==
                TRUE) {
                if (echoInline == FALSE) {
                  if (length(grep("library(\\s*)\\(|require(\\s*)\\(",
                    HelpOrCommentOrBlankLineStr)) == 0) {
                    cat("\n")
                    if (bsky_Rmarkdown_settings$doRmarkdownFormatting ==
                      TRUE && bsky_Rmarkdown_settings$doLatexFormatting ==
                      FALSE) {
                      cat("<pre class=\"r\"><code>")
                    }
                    cat("\n----------------------\n")
                    cat("DIAGNOSTIC MESSAGE: The above R errors and/or warnings are generated by the following R code")
                    cat("\n----------------------\n")
                    if (length(origRcommands) > 0) {
                      cat(parsed_orig_Rcommands[[i]])
                    }
                    else {
                      cat(parsed_Rcommands[[i]])
                    }
                    if (bsky_Rmarkdown_settings$doRmarkdownFormatting ==
                      TRUE && bsky_Rmarkdown_settings$doLatexFormatting ==
                      FALSE) {
                      cat("</code></pre>")
                    }
                    cat("\n")
                  }
                }
                eval(parse(text = "bsky_rcommand_execution_an_exception_occured1 = FALSE"),
                  envir = globalenv())
            }
            if (graphicsDir_exists == TRUE) {
                num_graphics_files = length(list.files(graphicsDir,
                  pattern = "png|svg"))
                if (bskyEvalDebug == TRUE) {
                  cat("\n<br>********* Printing call details within BSkyEvalRcommandBasicForAppScript - num_graphics_files and uadatasets.sk$last_count_of_bsky_graphics_files ******<br>\n")
                  if (length(origRcommands) > 0) {
                    print(parsed_orig_Rcommands[[i]])
                  }
                  else {
                    print(parsed_Rcommands[[i]])
                  }
                  print(list.files(graphicsDir, pattern = "png|svg"))
                  print(num_graphics_files)
                  cat("strating_count_of_bsky_graphics_files\n")
                  print(uadatasets.sk$strating_count_of_bsky_graphics_files)
                  cat("last_count_of_bsky_graphics_files\n")
                  print(uadatasets.sk$last_count_of_bsky_graphics_files)
                  print(num_graphics_files - uadatasets.sk$last_count_of_bsky_graphics_files)
                  cat("first_Graphics_Command_Executed\n")
                  print(first_Graphics_Command_Executed)
                }
                if (num_graphics_files > uadatasets.sk$last_count_of_bsky_graphics_files) {
                  if (uadatasets.sk$last_count_of_bsky_graphics_files ==
                    uadatasets.sk$strating_count_of_bsky_graphics_files &&
                    first_Graphics_Command_Executed == FALSE) {
                    if (file.exists(uadatasets.sk$initial_graphics_file_name)) {
                      file.remove(uadatasets.sk$initial_graphics_file_name)
                      first_Graphics_Command_Executed = TRUE
                      BSkyGraphicsFormat(bSkyFormatAppRequest = FALSE,
                        noOfGraphics = (num_graphics_files -
                          uadatasets.sk$last_count_of_bsky_graphics_files),
                        isRmarkdownOutputOn = bsky_Rmarkdown_settings$doRmarkdownFormatting)
                      uadatasets.sk$last_count_of_bsky_graphics_files = num_graphics_files -
                        1
                    }
                    else {
                      BSkyGraphicsFormat(bSkyFormatAppRequest = FALSE,
                        noOfGraphics = (num_graphics_files -
                          uadatasets.sk$last_count_of_bsky_graphics_files),
                        isRmarkdownOutputOn = bsky_Rmarkdown_settings$doRmarkdownFormatting)
                      uadatasets.sk$last_count_of_bsky_graphics_files = num_graphics_files
                    }
                  }
                  else {
                    BSkyGraphicsFormat(bSkyFormatAppRequest = FALSE,
                      noOfGraphics = (num_graphics_files - uadatasets.sk$last_count_of_bsky_graphics_files),
                      isRmarkdownOutputOn = bsky_Rmarkdown_settings$doRmarkdownFormatting)
                    uadatasets.sk$last_count_of_bsky_graphics_files = num_graphics_files
                  }
                }
            }
        }
    }
    eval(parse(text = "rm(bsky_rcommand_execution_an_exception_occured1)"),
        envir = globalenv())
    eval(parse(text = "bsky_rcommand_parsing_an_exception_occured1 = FALSE"),
        envir = globalenv())
    if (length(origRcommands) > 0) {
        return(invisible(origRcommands))
    }
    else {
        return(invisible(RcommandString))
    }
}


#060624 - BSkyEvalRcommandBasic() copied from BSkyFormat.r and renamed below
# Needed to create copy from the original BSkyEvalBasicRcommand() to avoid the recurssion error to execute R code block from app in BMD script
# All tidy_source() parsing is removed same copy as original BSkyEvalRcommandBasic() to avoid sporadic syntax error for dialog with a lot of code 
BSkyEvalRcommandBasicForAppScript <- function(RcommandString, origRcommands = c(), echo = BSkyGetRCommandDisplaySetting(), echoInline = BSkyGetRCommandDisplaySetting(), splitOn = FALSE, graphicsDir = BSkyGetGraphicsDirPath(), bskyEvalDebug = FALSE)
{
	if(bskyEvalDebug == TRUE)
	{
		print("printing in BSkyEvalRcommandBasicForAppScript")
		print(RcommandString)
		cat("\n==============origRcommands============= \n")
		print(origRcommands)
		cat("\n=========================== \n")
	}
	
	parsed_Rcommands = c()
	parsed_orig_Rcommands = c()
	parsed_Rcommands_by_R_parse_srcref = c()
	parsed_orig_Rcommands_by_R_parse_srcref = c()
	
	if(is.null(origRcommands) || trimws(origRcommands) == "")
	{
		origRcommands = c()
	}
	
	bsky_Rmarkdown_settings = BSkyGetKableAndRmarkdownFormatting()
	
	first_Graphics_Command_Executed = FALSE
	graphicsDir_exists = FALSE
	
	if(!is.null(graphicsDir) && length(graphicsDir) > 0 && trimws(graphicsDir) != "" && dir.exists(graphicsDir) && bsky_Rmarkdown_settings$doRmarkdownFormatting == FALSE)
	{
		graphicsDir_exists = TRUE
		#BSkyFormat("BSky graphics output directory found")
	}
	#else #BSkyFormat("BSky graphics output directory not found") or not needed because it is not C# or Electron app environment 
	
	
	if(class(echo)[1] == "list")
	{
		echo = echo$echo
	}
	
	if(class(echoInline)[1] == "list")
	{
		echoInline = echoInline$echoInline
	}
	
	# No try catch exception block around the parse(), because all the parsing errors should have been caught in the parent function 
	parsed_Rcommands_by_R_parse = parse(text={RcommandString}, keep.source = TRUE)
	
	#Extract each R expression from the parsed expression and create an array of R command strings 
	for (expr_index in 1: length(parsed_Rcommands_by_R_parse)) {
		parsed_Rcommands = c(parsed_Rcommands, (paste(deparse(parsed_Rcommands_by_R_parse[[expr_index]]),collapse="\n")))
	}
	
	parsed_Rcommands_by_R_parse_srcref = parsed_Rcommands
	
	# cat("\n<SK> ===========print each R expression from Parsing by R eval() ================\n")
	# print(parsed_Rcommands)
	
	if(bskyEvalDebug == TRUE)
	{
		print("printing parsed_Rcommands from parse()")
		print(parsed_Rcommands_by_R_parse)
	}

	# eval(parse(text="bsky_rcommand_parsing_an_exception_occured = FALSE"), envir=globalenv())
	
	# tryCatch({
			# withCallingHandlers({
					# parsed_Rcommands = (tidy_source(text = RcommandString, output = FALSE))$text.tidy
			# }, warning = BSkyRcommandParsingErrWarnHandler, silent = TRUE)
			# }, error = BSkyRcommandParsingErrWarnHandler, silent = TRUE)
	
	
	# if(bsky_rcommand_parsing_an_exception_occured == TRUE)
	# {		
		# cat("\n<SK> ===========Parsing Error in tidy_source================\n")
		
		# if(bskyEvalDebug == TRUE)
		# {
			# cat("\nParsing Error in tidy_source\n")
			# cat(RcommandString)
			# cat("\n")
		# }
		
		# parsed_Rcommands = (tidy_source(text = as.character(parsed_Rcommands_by_R_parse), output = FALSE))$text.tidy
		# parsed_Rcommands_by_R_parse_srcref = attr(parsed_Rcommands_by_R_parse, "srcref")
	# }
	
	# cat("\n<SK> ===========Parsing by R eval() ================\n")
	# print(parsed_Rcommands_by_R_parse)
	# cat("\n<SK> ===========print each R expression from Parsing by R eval() ================\n")
	# for (expr_index in 1: length(parsed_Rcommands_by_R_parse)) {
		# #print(parsed_Rcommands_by_R_parse[expr_index])
		# print(paste(deparse(parsed_Rcommands_by_R_parse[[expr_index]]),collapse="\n"))
	# }
	# cat("\n<SK> ===========Parsing tidy_source(() ================\n")
	# print(parsed_Rcommands)
	# if(bsky_rcommand_parsing_an_exception_occured == TRUE)
	# {	
		# cat("\n<SK> =========== parsed_Rcommands_by_R_parse_srcref ================\n")
		# print(parsed_Rcommands_by_R_parse_srcref)
	# }
	
	
	# if(bskyEvalDebug == TRUE)
	# {
		# parsed_Rcommands = (tidy_source(text = as.character(parsed_Rcommands_by_R_parse), output = FALSE))$text.tidy
		# parsed_Rcommands_by_R_parse_srcref = attr(parsed_Rcommands_by_R_parse, "srcref")
		
		# cat("\n============== parsed_Rcommands in parse() and tidy_source() ============= \n")
		# print("printing parsed_Rcommands_by_R_parse_srcref from parse()")
		# print(parsed_Rcommands_by_R_parse_srcref)
		
		# cat("\n++++++++++++++++++++++++++++++++++ \n")
		
		# print("printing parsed_Rcommands from tidy_source()")
		# print(parsed_Rcommands)
		
		# cat("\n=========================== \n")
	# }
	
	
	#Rcommands_initial_parse = BSkyRCommandParsedCharCount(RcommandString = RcommandString, numExprParse = numExprParse)
	#parsed_Rcommands = (tidy_source(text = Rcommands_initial_parse$parsedCommandList, output = FALSE, end.comment="\n"))$text.tidy
	
	if(length(origRcommands) > 0)
	{
		parsed_orig_Rcommands_by_R_parse = parse(text={origRcommands}, keep.source = TRUE)
		
		#Extract each R expression from the parsed expression and create an array of R command strings 
		for (expr_index in 1: length(parsed_orig_Rcommands_by_R_parse)) {
			parsed_orig_Rcommands = c(parsed_orig_Rcommands, (paste(deparse(parsed_orig_Rcommands_by_R_parse[[expr_index]]),collapse="\n")))
		}
		
		parsed_orig_Rcommands_by_R_parse_srcref = parsed_orig_Rcommands
		
		# if(bsky_rcommand_parsing_an_exception_occured == TRUE)
		# {
			# parsed_orig_Rcommands = (tidy_source(text = as.character(parsed_orig_Rcommands_by_R_parse), output = FALSE))$text.tidy
			# parsed_orig_Rcommands_by_R_parse_srcref = attr(parsed_orig_Rcommands_by_R_parse, "srcref")
		# }
		# else
		# {
			# parsed_orig_Rcommands = (tidy_source(text = origRcommands, output = FALSE))$text.tidy
		# }
		
		# #origRcommands_initial_parse = BSkyRCommandParsedCharCount(RcommandString = origRcommands, numExprParse = numExprParse)
		# #parsed_orig_Rcommands = (tidy_source(text = origRcommands_initial_parse$parsedCommandList, output = FALSE, end.comment="\n"))$text.tidy
		
		# if(bskyEvalDebug == TRUE)
		# {
			# parsed_orig_Rcommands = (tidy_source(text = as.character(parsed_orig_Rcommands_by_R_parse), output = FALSE))$text.tidy
			# parsed_orig_Rcommands_by_R_parse_srcref = attr(parsed_orig_Rcommands_by_R_parse, "srcref")
			
			# cat("\n============== parsed_orig_Rcommands in parse() and tidy_source() ============= \n")
			# print("printing parsed_orig_Rcommands_by_R_parse_srcref from parse()")
			# print(parsed_orig_Rcommands_by_R_parse_srcref)
			
			# cat("\n++++++++++++++++++++++++++++++++++ \n")
			
			# print("printing parsed_orig_Rcommands from tidy_source()")
			# print(parsed_orig_Rcommands)
			
			# cat("\n=========================== \n")
		# }
	}
	
	
	if(bskyEvalDebug == TRUE)
	{
		if(bsky_Rmarkdown_settings$doRmarkdownFormatting == TRUE && bsky_Rmarkdown_settings$doLatexFormatting == FALSE)
		{
			cat("<pre class=\"r\"><code>")
		}
		
		if(length(origRcommands) > 0)
		{
			print(origRcommands)
			#cat("\nExpressions parsed (orig)\n")
			#print(origRcommands_initial_parse$parsedCommandList)
			cat("\nExpressions to be executed (orig)\n")
			print(parsed_orig_Rcommands)
		}
		else
		{
			print(RcommandString)
			#cat("\nExpressions parsed\n")
			#print(Rcommands_initial_parse$parsedCommandList)
			cat("\nExpressions to be executed\n")
			print(parsed_Rcommands)
		}
		
		if(bsky_Rmarkdown_settings$doRmarkdownFormatting == TRUE && bsky_Rmarkdown_settings$doLatexFormatting == FALSE)
		{
			cat("</code></pre>")
		}
	}
	
	if(echo == TRUE && echoInline == FALSE && splitOn == FALSE)  
	{
		cat("\n")
		
		if(bsky_Rmarkdown_settings$doRmarkdownFormatting == TRUE && bsky_Rmarkdown_settings$doLatexFormatting == FALSE)
		{
			cat("<pre class=\"r\"><code>")
					 
			if(length(origRcommands) > 0)
			{
				 RcommandString_modified_print = gsub("\\n", "<br>", origRcommands)
				 cat(RcommandString_modified_print)
				 cat("<br>")
			}
			else
			{
				RcommandString_modified_print = gsub("\\n", "<br>", RcommandString)
				cat(RcommandString_modified_print)
				cat("<br>")
			}
			
			cat("</code></pre>")
		}
		else
		{
			if(length(origRcommands) > 0)
			{
				 cat(origRcommands)
			}
			else
			{
				cat(RcommandString)
			}
		}
		
		cat("\n")
	}
	

	for (i in seq_along(parsed_Rcommands)) 
	{
	   eval(parse(text="bsky_rcommand_execution_an_exception_occured = FALSE"), envir=globalenv())
	   
		# tryCatch(        
			# withAutoprint({{eval(ll[[i]])}}, deparseCtrl=c("keepInteger", "showAttributes", "keepNA"), keep.source=TRUE),         
			# error = function(e) message("Error: ", as.character(e))    
			# )
		
		#if(splitOn == FALSE) #Let split iteration also spit our inline RcommandString
		#{
			if(echo == TRUE && echoInline == TRUE)
			{
				cat("\n")
				
				if(bsky_Rmarkdown_settings$doRmarkdownFormatting == TRUE && bsky_Rmarkdown_settings$doLatexFormatting == FALSE)
				{
					cat("<pre class=\"r\"><code>")
				}
				
				if(length(origRcommands) > 0)
				{
					if(bsky_rcommand_parsing_an_exception_occured == TRUE)
					{
						print(parsed_orig_Rcommands_by_R_parse_srcref[[i]])
					}
					else
					{
						#print(parsed_orig_Rcommands[[i]])
						cat(parsed_orig_Rcommands[[i]])
						#cat("\n")
					}
				}
				else
				{
					if(bsky_rcommand_parsing_an_exception_occured == TRUE)
					{
						print(parsed_Rcommands_by_R_parse_srcref[[i]])
					}
					else
					{
						#print(parsed_Rcommands[[i]])
						cat(parsed_Rcommands[[i]])
						#cat("\n")
					}
				}
				
				if(bsky_Rmarkdown_settings$doRmarkdownFormatting == TRUE && bsky_Rmarkdown_settings$doLatexFormatting == FALSE)
				{
					cat("</code></pre>")
				}
				
				cat("\n")
			}
		#}
		
		# https://stat.ethz.ch/R-manual/R-devel/library/base/html/source.html
		# https://stat.ethz.ch/R-manual/R-devel/library/base/html/deparse.html
		# https://stat.ethz.ch/R-manual/R-devel/library/base/html/deparseOpts.html
		
		isCommentOrBlankLine = FALSE
		isHelpCommand = FALSE
		isPkgInstallCommand = FALSE 
		HelpOrCommentOrBlankLineStr = trimws(parsed_Rcommands[[i]])
		
		# print(HelpOrCommentOrBlankLineStr)
		# cat("<br>")
		if(substr(HelpOrCommentOrBlankLineStr,1,3) == "`?`" || substr(HelpOrCommentOrBlankLineStr,1,5) == "help(" || substr(HelpOrCommentOrBlankLineStr,1,5) == "help ")
		{
			
			helpfile = c()
			package_only_help_command = FALSE
			package_name = c()
			
			topic_param_name_found = grep("\\btopic\\b", HelpOrCommentOrBlankLineStr)
			# cat("\ntopic_param_name_found\n")
			# print(topic_param_name_found)
			# return()
			
			if(length(topic_param_name_found) == 0) #regexpr("(\\bpackage(\\s)*=)", x)
			{
				package_param_name_found = regexpr("(\\bpackage(\\s)*=)", HelpOrCommentOrBlankLineStr) #"\\bpackage\\b"
				
				if(package_param_name_found != -1)
				{
					y = strsplit(HelpOrCommentOrBlankLineStr, ",")
					non_topic_param_in_1st_place = grep("=", y[[1]][1])
					
					if(length(non_topic_param_in_1st_place) != 0)
					{
						substr_start_from_package_param = substr(HelpOrCommentOrBlankLineStr, package_param_name_found, nchar(HelpOrCommentOrBlankLineStr))
						package_param = strsplit(substr_start_from_package_param, ",")
						package_name = strsplit(package_param[[1]][1], "=")
						
						if(trimws(package_name[[1]][1]) == "package")
						{
							package_name = trimws(package_name[[1]][2])
							nchar_package_name = nchar(package_name)
							#cat("\n 1. ========= The package name is: ", package_name, "\n")
							
							if(nchar_package_name > 0)
							{
								package_name = trimws((strsplit(package_name, "\""))[[1]][2])
								#cat("\n 2. ========= The package name is: ", package_name, "\n")
							}
							
							
							# options(help_type = 'text')
							# x = help(package = "dplyr") - package only help command
							# str(x)
							# class(x) - "packageInfo"

							package_only_help_command = TRUE
						}
					}
				}
			}
			
			# R_none BSky generates its own HTML file in temp location that app will display instead of calling R help HTTP server
			if(BSkyGetRHelpHTTPServer() == "R_none" || BSkyGetRHelpParallelHTTPServerPortNumber() == 0)
			{
				# temporarily set to text to avoid R popping up browser when command format contains just pkg e.g. help(package = "dplyr")
				options(help_type = 'text') 
				
				tryCatch({
						withCallingHandlers({
								withAutoprint({{helpfile = eval(parse(text = parsed_Rcommands[[i]]), envir=globalenv())}}, print. = TRUE, echo = FALSE, deparseCtrl=c("keepInteger", "showAttributes", "keepNA"), keep.source=TRUE)
						}, warning = BSkyRcommandErrWarnHandler, silent = TRUE)
						}, error = BSkyRcommandErrWarnHandler, silent = TRUE)
				
				
				if(bsky_rcommand_execution_an_exception_occured == FALSE)
				{
					if(bsky_Rmarkdown_settings$doRmarkdownFormatting == TRUE && bsky_Rmarkdown_settings$doLatexFormatting == FALSE)
					{
						cat("\n")
						cat("<pre class=\"r\"><code>")
					}
						
					if(class(helpfile)[1] == "help_files_with_topic" && length(as.character(helpfile)) > 0)
					{
						temp_help_file_path = c()
						
						pkgname <- basename(dirname(dirname(helpfile)))
						
						if(length(pkgname) > 1)
						{
							cat("\nhelp topic is found in multiple packages: ", pkgname, "\n")
							cat("Displaying the help topic from the first package", pkgname[1], "found in the package search path\n")
							cat("if you want the help(topic) from a specific package, add the package parameter to help(topic, package = 'pkg name')\n")
						}
						
						temp_help_file_path <- tools::Rd2HTML(utils:::.getHelpFile(helpfile[1]), out = tempfile(pattern = "BSkyhelpsink", fileext = ".html"), package = pkgname[1])
						
						# For testing purpose to copy the file in the download directory as R removes all temp files upon closing the R session 
						#file.copy(temp_help_file_path, paste("C:/Users/User/Downloads/BSkyTempHelpFile_",i,".html",sep=""), overwrite = TRUE)
						# browseURL(temp_help_file_path)
						
						cat("\n")
						#print(str(helpfile))
						#print(as.character(helpfile))
						cat(paste("BSkyHelpCommandMarker ", temp_help_file_path, sep=""))
						cat("\n")
					}
					else if (class(helpfile)[1] == "packageInfo")
					{	
						title = gettextf("Documentation for package %s", sQuote(helpfile$name))
						
						outFile = tempfile(pattern = "BSkyhelpsink", fileext = ".html")
						
						#print(format(gsub(" ","&nbsp;",helpfile)
						
						content = format(helpfile)
						
						cat("<!DOCTYPE html>\n<html>\n<body>\n", file = outFile, append = TRUE)
						cat("<table>\n", file = outFile, append = TRUE)
						for(cont in 1:length(content))
						{
							content[cont] = trimws(content[cont])
							splitline = gregexpr(":|\\s+", content[cont])
							
							cat("<tr>\n", file = outFile, append = TRUE) 
							if(splitline[[1]][1] == -1)
							{
								cat("<td colspan=\"2\">\n", file = outFile, append = TRUE)
								cat(content[cont],file = outFile, append = TRUE)
								cat("\n</td>\n", file = outFile, append = TRUE)
							}
							else
							{
								cat("<td>\n", file = outFile, append = TRUE)
								cat(substr(content[cont], 1, (splitline[[1]][1])),file = outFile, append = TRUE)
								cat("\n</td>\n", file = outFile, append = TRUE)
								
								if((splitline[[1]][1] + 1) <= nchar(content[cont]))
								{
									cat("<td>\n", file = outFile, append = TRUE)
									cat(substr(content[cont], (splitline[[1]][1] + 1), nchar(content[cont])),file = outFile, append = TRUE)
									cat("\n</td>\n", file = outFile, append = TRUE)
								}
							}
							
							cat("</tr>\n", file = outFile, append = TRUE)
						}
						cat("</table>\n", file = outFile, append = TRUE)
						cat("\n</body>\n</html>", file = outFile, append = TRUE)
						
						# For testing purpose to copy the file in the download directory as R removes all temp files upon closing the R session 
						# file.copy(outFile, paste("C:/Users/User/Downloads/BSkyTempHelpFile_",i,".html",sep=""), overwrite = TRUE)
						# browseURL(outFile)
						
						cat("\n")
						#print(str(helpfile))
						cat(paste("BSkyHelpCommandMarker ", outFile, sep=""))
						cat("\n")
					}
					else
					{
						cat("\n", paste("No documentation for the package or the topic in specified packages and libraries :", parsed_Rcommands[[i]]),"\n")
					}
					
					if(bsky_Rmarkdown_settings$doRmarkdownFormatting == TRUE && bsky_Rmarkdown_settings$doLatexFormatting == FALSE)
					{
						cat("</code></pre>")
						cat("\n")
					}
				}
				else
				{
					eval(parse(text="bsky_rcommand_execution_an_exception_occured = FALSE"), envir=globalenv())
				}
				
				options(help_type = 'html')
				isHelpCommand = TRUE
			}
			else
			{
				if(BSkyGetRHelpHTTPServer() == "R_parallel")
				{
					options(help_type = 'html')
					
					#require(future)
					
					#oplan <- plan(multisession, workers = 2)
					#on.exit(plan(oplan), add = TRUE)
					
					port = BSkyGetRHelpParallelHTTPServerPortNumber()
					
					if(package_only_help_command == TRUE)
					{
					
						options(help_type = 'text')
						
						tryCatch({
						withCallingHandlers({
								withAutoprint({{helpfile = eval(parse(text = parsed_Rcommands[[i]]), envir=globalenv())}}, print. = TRUE, echo = FALSE, deparseCtrl=c("keepInteger", "showAttributes", "keepNA"), keep.source=TRUE)
						}, warning = BSkyRcommandErrWarnHandler, silent = TRUE)
						}, error = BSkyRcommandErrWarnHandler, silent = TRUE)
				
						
						if(bsky_rcommand_execution_an_exception_occured == FALSE)
						{
							options(help_type = 'html')
							if(length(package_name) > 0)
							{
								# fut_help_command <- future(
									# {
										# port = tools::startDynamicHelp(NA);
										# browseURL(paste0("http://127.0.0.1:", port, "/library/", package_name, "/html/00Index.html"));
									# }#, packages = c('base', 'stats', 'tools','dplyr'), globals = 'package_name'
								# )
								
								browseURL(paste0("http://127.0.0.1:", port, "/library/", package_name, "/html/00Index.html"))
							}
						
							#Sys.sleep(60)
						}
						else
						{
							eval(parse(text="bsky_rcommand_execution_an_exception_occured = FALSE"), envir=globalenv())
						}
					}
					else
					{
						# fut_help_command <- future(
							# {
								# withAutoprint({{eval(parse(text = parsed_Rcommands[[i]]), envir=globalenv())}}, print. = TRUE, echo = FALSE, deparseCtrl=c("keepInteger", "showAttributes", "keepNA"), keep.source=TRUE);
							# } #, packages = c('base', 'stats', 'tools', 'dplyr')
						# )
						
						#Sys.sleep(60)
						
						options(help_type = 'text')
						
						tryCatch({
						withCallingHandlers({
								withAutoprint({{helpfile = eval(parse(text = parsed_Rcommands[[i]]), envir=globalenv())}}, print. = TRUE, echo = FALSE, deparseCtrl=c("keepInteger", "showAttributes", "keepNA"), keep.source=TRUE)
						}, warning = BSkyRcommandErrWarnHandler, silent = TRUE)
						}, error = BSkyRcommandErrWarnHandler, silent = TRUE)
				
						
						if(bsky_rcommand_execution_an_exception_occured == FALSE)
						{
							options(help_type = 'html')
							
							if(bsky_Rmarkdown_settings$doRmarkdownFormatting == TRUE && bsky_Rmarkdown_settings$doLatexFormatting == FALSE)
							{
								cat("\n")
								cat("<pre class=\"r\"><code>")
							}
								
							if(class(helpfile)[1] == "help_files_with_topic" && length(as.character(helpfile)) > 0)
							{	
								pkgname <- basename(dirname(dirname(helpfile)))
								
								if(length(pkgname) > 1)
								{
									cat("\nhelp topic is found in multiple packages: ", pkgname, "\n")
									cat("Displaying the help topic from the first package", pkgname[1], "found in the package search path\n")
									cat("if you want the help(topic) from a specific package, add the package parameter to help(topic, package = 'pkg name')\n")
								}
						
								pkgname = pkgname[1]
								topic = basename(helpfile[1])
								
								# http://127.0.0.1:21072/library/stats/html/lm.html
								# http://127.0.0.1:21072/library/dplyr/html/select.html
								# http://127.0.0.1:21072/library/dplyr/html/dplyr-package.html
								
								browseURL(paste0("http://127.0.0.1:", port, "/library/", pkgname, "/html/",topic,".html"));
							}
						}
						else
						{
							eval(parse(text="bsky_rcommand_execution_an_exception_occured = FALSE"), envir=globalenv())
						}
					}
					
					isHelpCommand = TRUE
					
					# check whether the expression executed in the parallel R process is completed successfully 
					# if not completed, the following call will block the current R process waiting for the sub-process to complete
					# fut_value <- value(fut_help_command)
					
					# The following is a check for the subprocess without being blocked till the sub-process resolves/completes 
					# while (!resolved(fut_help_command)) Sys.sleep(5)
				}
				else # The native R html option will not work (blocking R help http server) within BlueSky/Rpy2 app 
				{
					options(help_type = 'html')
					port = tools::startDynamicHelp(NA)
					
					if(port <= 0)
					{
						cat("\nR HTML help server could not be started\n")
						cat("\nGo to BlueSky Application configuration menu dialog and uncheck the R help server option and then try help(..) command again\n")
					}
					# else
					# {
						# cat("\nR HTML help server started on port number:\n", port)
					# }
					
					if(package_only_help_command == TRUE)
					{
						if(length(package_name) > 0)
						{
							browseURL(paste0("http://127.0.0.1:", port, "/library/", package_name, "/html/00Index.html"))
						}
						
						isHelpCommand = TRUE
					}
					
					# if (port > 0L) 
					# {
						# path <- dirname(file)
						# dirpath <- dirname(path)
						# pkgname <- basename(dirpath)
						# browseURL(paste0("http://127.0.0.1:", port, "/library/", pkgname, "/html/", basename(file), ".html"), browser)
					# }
				}
			}
		}
		else if(substr(HelpOrCommentOrBlankLineStr,1,1) == "#" || substr(HelpOrCommentOrBlankLineStr,1,1) == "")
		{
			isCommentOrBlankLine = TRUE
		}
		else
		{
			if(length(grep("install\\.packages(\\s*)\\(|update\\.packages|install_github(\\s*)\\(|devtools::install_github(\\s*)\\(|::install|githubinstall|^install_(\\s|\\S)*\\(", HelpOrCommentOrBlankLineStr)) > 0)
			{
			
				if(length(grep("BSkypackageinstall", HelpOrCommentOrBlankLineStr)) == 0)
				{
					isPkgInstallCommand = TRUE
					#cat("\n")
					#cat(HelpOrCommentOrBlankLineStr)
					cat("\nPLEASE NOTE: For package installation and update, please see triple dot > Install R Package from the top level menu in the BlueSky Statistics application\n")
				}
			}
		}
		
		if(isCommentOrBlankLine == FALSE && isHelpCommand == FALSE && isPkgInstallCommand == FALSE)
		{
			tryCatch({
					withCallingHandlers({
							withAutoprint({{eval(parse(text = parsed_Rcommands[[i]]), envir=globalenv())}}, print. = TRUE, echo = FALSE, deparseCtrl=c("keepInteger", "showAttributes", "keepNA"), keep.source=TRUE)
					}, warning = BSkyRcommandErrWarnHandler, silent = TRUE)
					}, error = BSkyRcommandErrWarnHandler, silent = TRUE)
			
			
			if(bsky_rcommand_execution_an_exception_occured == TRUE)
			{
				#if(splitOn == TRUE || echoInline == FALSE)
				if(echoInline == FALSE)
				{
					if(length(grep("library(\\s*)\\(|require(\\s*)\\(", HelpOrCommentOrBlankLineStr)) == 0)
					{
						cat("\n")
						
						if(bsky_Rmarkdown_settings$doRmarkdownFormatting == TRUE && bsky_Rmarkdown_settings$doLatexFormatting == FALSE)
						{
							cat("<pre class=\"r\"><code>")
						}
						
						cat("\n----------------------\n")
						cat("DIAGNOSTIC MESSAGE: The above R errors and/or warnings are generated by the following R code")
						cat("\n----------------------\n")
					
						if(length(origRcommands) > 0)
						{
							#print(parsed_orig_Rcommands[[i]])
							cat(parsed_orig_Rcommands[[i]])
						}
						else
						{
							#print(parsed_Rcommands[[i]])
							cat(parsed_Rcommands[[i]])
						}
						
						if(bsky_Rmarkdown_settings$doRmarkdownFormatting == TRUE && bsky_Rmarkdown_settings$doLatexFormatting == FALSE)
						{
							cat("</code></pre>")
						}
						
						cat("\n")
					}
				}
				
				eval(parse(text="bsky_rcommand_execution_an_exception_occured = FALSE"), envir=globalenv())
			}
			
			if(graphicsDir_exists == TRUE)
			{
				num_graphics_files = length(list.files(graphicsDir, pattern="png|svg"))
				
				if(bskyEvalDebug == TRUE)
				{
					cat("\n<br>********* Printing call details within BSkyEvalRcommandBasicForAppScript - num_graphics_files and uadatasets.sk$last_count_of_bsky_graphics_files ******<br>\n")
					if(length(origRcommands) > 0)
					{
						print(parsed_orig_Rcommands[[i]])
						#cat(parsed_orig_Rcommands[[i]])
					}
					else
					{
						print(parsed_Rcommands[[i]])
						#cat(parsed_Rcommands[[i]])
					}
					print(list.files(graphicsDir, pattern="png|svg"))
					print(num_graphics_files)
					cat("strating_count_of_bsky_graphics_files\n")
					print(uadatasets.sk$strating_count_of_bsky_graphics_files)
					cat("last_count_of_bsky_graphics_files\n")
					print(uadatasets.sk$last_count_of_bsky_graphics_files)
					print(num_graphics_files - uadatasets.sk$last_count_of_bsky_graphics_files)
					cat("first_Graphics_Command_Executed\n")
					print(first_Graphics_Command_Executed)
				}
			
				if(num_graphics_files > uadatasets.sk$last_count_of_bsky_graphics_files)
				{
					if(uadatasets.sk$last_count_of_bsky_graphics_files == uadatasets.sk$strating_count_of_bsky_graphics_files && first_Graphics_Command_Executed == FALSE)
					{
						# if(bskyEvalDebug == TRUE)
						# {
							# BSkyGraphicsFormat(bSkyFormatAppRequest = FALSE, noOfGraphics= 1, isRmarkdownOutputOn = bsky_Rmarkdown_settings$doRmarkdownFormatting)
						# }
						# else
						if(file.exists(uadatasets.sk$initial_graphics_file_name ))
						{
							file.remove(uadatasets.sk$initial_graphics_file_name)
							first_Graphics_Command_Executed = TRUE
							
							BSkyGraphicsFormat(bSkyFormatAppRequest = FALSE, noOfGraphics= (num_graphics_files - uadatasets.sk$last_count_of_bsky_graphics_files), isRmarkdownOutputOn = bsky_Rmarkdown_settings$doRmarkdownFormatting)
							uadatasets.sk$last_count_of_bsky_graphics_files = num_graphics_files - 1
						}
						else
						{
							BSkyGraphicsFormat(bSkyFormatAppRequest = FALSE, noOfGraphics= (num_graphics_files - uadatasets.sk$last_count_of_bsky_graphics_files), isRmarkdownOutputOn = bsky_Rmarkdown_settings$doRmarkdownFormatting)
							uadatasets.sk$last_count_of_bsky_graphics_files = num_graphics_files
						}
					}
					else
					{
						BSkyGraphicsFormat(bSkyFormatAppRequest = FALSE, noOfGraphics= (num_graphics_files - uadatasets.sk$last_count_of_bsky_graphics_files), isRmarkdownOutputOn = bsky_Rmarkdown_settings$doRmarkdownFormatting)
						uadatasets.sk$last_count_of_bsky_graphics_files = num_graphics_files
					}
				}
			}
		}
		# else
		# {
			# if(bsky_Rmarkdown_settings$doRmarkdownFormatting == TRUE && bsky_Rmarkdown_settings$doLatexFormatting == FALSE)
			# {
				# cat("<br>")
			# }
		# }
	}
	
	eval(parse(text="rm(bsky_rcommand_execution_an_exception_occured)"), envir=globalenv())
	
	eval(parse(text="bsky_rcommand_parsing_an_exception_occured = FALSE"), envir=globalenv())
	
	#return(invisible(RcommandString)) 
	
	if(length(origRcommands) > 0)
	{
		#return(invisible(origRcommands_initial_parse))
		return(invisible(origRcommands))
	}
	else
	{
		#return(invisible(RcommandString_initial_parse))
		return(invisible(RcommandString))
	}
}


# Needed t create copy from the original error/warn handler to avoid the recurssion error to execute R code block from app in BMD script
BSkyRcommandErrWarnHandlerForAppScript <- function(m)
{
	#print(str(m))
	
	eval(parse(text="bsky_rcommand_execution_an_exception_occured1 = TRUE"), envir=globalenv())
	
	if("error" %in% attr(m, "class"))
	{
		cat("\n")
		message("Error: ", as.character(m$message))
		uadatasets.sk$BSkyEvalErrors = uadatasets.sk$BSkyEvalErrors + 1
		
		#print(sys.calls()) #to print the stack trace - not very helpful 
	}
	else if("warning" %in% attr(m, "class"))
	{
		message("Warning: ", as.character(m$message))
	}
	else
	{
		message("Msg: ", as.character(m$message))
	}
}

# Needed t create copy from the original error/warn handler to avoid the recurssion error to execute R code block from app in BMD script
BSkyRcommandParsingErrWarnHandlerForAppScript <- function(m)
{
	#print(str(m))
	
	if("error" %in% attr(m, "class"))
	{
		eval(parse(text="bsky_rcommand_parsing_an_exception_occured1 = TRUE"), envir=globalenv())
		uadatasets.sk$BSkyParsingErrors = c(uadatasets.sk$BSkyParsingErrors, paste("Parsing Error: ", as.character(m$message)))
	}
	else if("warning" %in% attr(m, "class"))
	{
		uadatasets.sk$BSkyParsingErrors = c(uadatasets.sk$BSkyParsingErrors, paste("Parsing Warning: ", as.character(m$message)))
	}
	else
	{
		uadatasets.sk$BSkyParsingErrors = c(uadatasets.sk$BSkyParsingErrors, paste("Parsing Msg: ", as.character(m$message)))
	}
}

# Created to trap any exception with BMD script execution to have the chance to set the Graphics directory path back to original App setting 
BSkyInAppBMDScriptExecuteErrWarnHandler <- function(m)
{	
	#eval(parse(text="bsky_bmd_script_execution_an_exception_occured = TRUE"), envir=globalenv())
	
	if("error" %in% attr(m, "class"))
	{
		cat("\n")
		message("Error: ", as.character(m$message)) 
	}
	else if("warning" %in% attr(m, "class"))
	{
		message("Warning: ", as.character(m$message))
	}
	else
	{
		message("Msg: ", as.character(m$message))
	}
}