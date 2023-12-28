## 12Dec2023 
## New function added for BlueSky script(exported from BSky app) automation. Although it is a generic function
BSkySplitCollapseDatasetWithRepeatingColumns <- function(datasetNameStr = c(), removeColsWithConstant = FALSE, splitDatsetSuffix = '', columnGpLength = 0, outputColumnNames = '', collapseDataset = FALSE, collapseGpIDPrefix ='', collapseGpIDColName="OrigDatasetID", makeCollapseGpIDColFactor = TRUE)
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
		constant_columns <- sapply(datasetObj_non_empty_cols, function(col) length(unique(col)) == 1)
		
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
		
		if(trimws(splitDatsetSuffix) == '')
		{
			names(subdatasets) = paste0(datasetNameStr, "_", seq_along(subdatasets))
		}
		else
		{
			names(subdatasets) = paste0(datasetNameStr, "_", splitDatsetSuffix, "_", seq_along(subdatasets))
		}
		
		collapse_gp_name = ''
		if(trimws(collapseGpIDPrefix) != '')
		{
			collapse_gp_name = paste0(collapseGpIDPrefix, "_")
		}
		
		collapsed_df = data.frame()
		
		for (i in seq_along(subdatasets)) 
		{
			names(subdatasets[[i]]) = cleaned_string[1:no_of_unque_cols]
			
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
			if(trimws(splitDatsetSuffix) == '')
			{
				collapsed_dataset_name = paste0(datasetNameStr, "_1")
			}
			else
			{
				collapsed_dataset_name = paste0(datasetNameStr, "_", splitDatsetSuffix)
			}
			
			if(trimws(collapseGpIDColName) == '')
			{
				collapseGpIDColName="OrigDatasetID"
			}
			
			names(collapsed_df) = c(cleaned_string[1:no_of_unque_cols], collapseGpIDColName)
			
			if(makeCollapseGpIDColFactor == TRUE)
			{
				collapsed_df[collapseGpIDColName] = factor(collapsed_df[, length(names(collapsed_df))])
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
			cat("\nDataset:", datasetNameStr,"has been collapsed with", length(names(subdatasets)),"underlying datasets\n")
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

##27Dec2023
# Add two link breaks e.g. <br><br> between two consecutive graphics to create some separation 
##23Nov2023
##31Mar2021
##11Nov2023 - made changes to handle the output formatting from automated (headless) BSky script run
## fileName = "C:/Users/User/Documents/workfolder/BSky/Rmarkdown/kableoutput1.html"
BSkyWriteKableHtmlOutput <- function(datasetName = c(), dirName = NULL, fileName = NULL, timeStamp = Sys.time(), fileOpenMode = "w", codeChunkNum = 1, codeChunkCmt = c(), bSkyDebug = 0)
{
	retObjList = BSkyGetHoldFormatObjList(bSkyCompatibility=0, bSkyDebug)
	
	svg_file_names = c()
	full_svg_file_names = c()
	graphicsDir = BSkyGetGraphicsDirPath()
	
	sink_file_path = paste(graphicsDir, "\\", "BSkysink.txt", sep='')
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
				#fileConn<-file(fileName, fileOpenMode)
				tempfileConn = file(tempHTMLfile, fileOpenMode)
			}
			else
			{
				return(invisible(FALSE))
			}
			
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
		else
		{
			return(invisible(FALSE))
		}
		
		plot_file_counter = 1
		ret_structure_counter = 1
		regular_sink_text_write_flag_on = FALSE
		
		# Open the file connection
		sink_file_conn <- file(sink_file_path, open = "r")

		if(trimws(codeChunkCmt) != "")
		{
			writeLines(paste("<h4><strong>", codeChunkCmt, "</strong></h4>"), tempfileConn)
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
					writeLines("</code></pre>",tempfileConn)
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
								writeLines("\n", tempfileConn)
								writeLines(retObjList[[ret_structure_counter]][[1]]$object$tables[[j]], tempfileConn)
								writeLines("\n<br/>\n", tempfileConn)
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
					writeLines("</code></pre>",tempfileConn)
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
							writeLines("<br/> <br/>", tempfileConn)
						}
						consecutive_graphics_count = consecutive_graphics_count + 1
						
						
						#<!-- Image with a relative path -->
						#<img src="path/to/your/image.jpg" alt="Description of the image">
						#<!-- Image with an absolute URL -->
						#<img src="https://example.com/path/to/remote/image.jpg" alt="Description of the remote image">
						
						writeLines(paste("<img src=", "\"", "plots/", timeStamp, "_", datasetName, "_", codeChunkNum, "_", svg_file_names[plot_file_counter],"\"",">", sep=''), tempfileConn)
						file.copy(full_svg_file_names[plot_file_counter], paste(plot_subdir, "\\", timeStamp, "_", datasetName, "_", codeChunkNum, "_", svg_file_names[plot_file_counter], sep=''))
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
					writeLines("<pre class=\"r\"><code>", tempfileConn)
					regular_sink_text_write_flag_on = TRUE
				}
					
				writeLines(sink_line, tempfileConn)
			}
		}
		
		if(regular_sink_text_write_flag_on == TRUE)
		{
			writeLines("</code></pre>",tempfileConn)
			regular_sink_text_write_flag_on = FALSE
		}
		
		if(!is.null(sink_file_conn))
		{
			close(sink_file_conn)
			file.copy(sink_file_path, paste(log_subdir, "\\", timeStamp, "_",  datasetName, "_", "BSkysink_", codeChunkNum, ".txt", sep=''))
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
			
			outputHTMLfileConn = file(fileName, "a")

			# Write the combined lines to a new file
			writeLines(file2, outputHTMLfileConn)
			close(outputHTMLfileConn)
		}
		
	}

	return(invisible(TRUE))
}

BSkyScriptSystemSinkFileMgmt <- function(bsky_script_system_dir, cur_timestamp, init = FALSE)
{
	##################################################################################################
	#  Open a sink file to redirect the stdout and stderr output to capture any msgs from dataset open
	##################################################################################################
	sinkOpenFile1 = paste(bsky_script_system_dir,"\\logs\\","BSkyScriptSystemSink1.txt", sep='')
	sinkOpenFile2 = paste(bsky_script_system_dir,"\\logs\\","BSkyScriptSystemSink2.txt", sep='')
	
	sinkOpenFile_names = list.files(path =  paste0(bsky_script_system_dir,"\\logs"), pattern=paste0(".*", "sink", ".*\\.txt$"), ignore.case = TRUE, full.names = TRUE)

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
		sinkOpenFile2_conn = file.create(sinkOpenFile2,"w")
		#close(sinkOpenFile2_conn)
		
		#sink(file = sinkOpenFile1, append = TRUE, type = c("output", "message"), split = FALSE)
		sinkOpenFile1_conn = file(sinkOpenFile1)
		sink(file = sinkOpenFile1_conn, append = TRUE, type = c("output"), split = FALSE)
		sink(file = sinkOpenFile1_conn, append = TRUE, type = c("message"), split = FALSE)
		
		cat("\n===== Timestamp: ",cur_timestamp,"=====\n")
	}
}
