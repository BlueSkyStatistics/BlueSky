##08Oct2021
BSkyFormat <- function(obj, maxOutputTables = BSkyGetTableDisplayLimits(), outputTableIndex = c(), outputColumnIndex = c(), outputTableRenames = c(), outputColumnRenames = c(), outputColumnRenamesRow = c(), maxRowLimit = BSkyGetTableDisplayLimits(), maxColLimit = BSkyGetTableDisplayLimits(), silentFormatting = FALSE, bSkyFormatAppRequest = FALSE, bSkyReturnObj = TRUE,  ftable_change_variable_order = TRUE, sublist_length = 3, remove_rows_with_zero_count = FALSE, no_row_column_headers = FALSE, decimalDigitsRounding = BSkyGetDecimalDigitSetting(), engNotationSetting = BSkyGetEngNotationSetting(), singleTableOutputHeader = "", repeatAllTableFooter = c(), perTableFooter = c(), isRound=BSkyGetRound(), coefConfInt = 0.95, pvalueDisplaySettings = BSkyGetPvalueDisplaySetting(), isKableOutput = TRUE, isLatexOutput = FALSE, isRmarkdownOutput = TRUE, isTextOutput = FALSE, getNonRenderedTables = FALSE, ignoreEnvStyleOverride = FALSE, forceColumnAlign = c(), kableStyleTheme = "kable_styling", textTableFormat = BSkyGetTextTableFormat(), tableStylingOptions = "table_border = F, column_align = r, header_background = \"#bdbdbd\" , more_options = c(bootstrap_options = c(\"striped\", \"hover\", \"condensed\", \"responsive\"), position = \"left\", full_width = F, html_font = \"Helvetica\", fixed_thead = list(enabled = T, background = \"#bdbdbd\"))", RM.SSP = TRUE, RM.SSPE = RM.SSP) # bdbdbd F0F8FF
{
	# cat("\n Parameters passed to BSkyFrequency1\n")
	# print(match.call())
	# cat("\n")
	
	# print(class(obj))
	# print(obj)
	
	doKableFormatting = FALSE
	doLatexFormatting = TRUE
	doRmarkdownFormatting = FALSE
	doTextFormatting = FALSE
	
	
	if(exists("uadatasets.sk") && exists("BSkySigfColPatterns", env=uadatasets.sk) && length(uadatasets.sk$BSkySigfColPatterns) > 0)
	{
		sigfColPatterns = uadatasets.sk$BSkySigfColPatterns
	}
	else
	{
		sigfColPatterns = c("Pr(>F)", "Pr(>|t|)","p.value", "Sig.", "Sig.(2-tailed)", "p-value", "Pr(>|z|)", "Pr(>Chi)", "p.value(z)", "p.value(t)", "Sig.(2-tail)", "Sig.(1-tail, >)", "Sig.(1-tail, <)", "Pr(>Chisq)", "P(>|Chi|)", "Pr(Chi)")
	}
	
	if(exists("uadatasets.sk") && exists("BSkySigFootnote", env=uadatasets.sk) && length(uadatasets.sk$BSkySigFootnote) > 0)
	{
		sigFootnote = uadatasets.sk$BSkySigFootnote
	}
	else
	{
		sigFootnote = c("Signif. codes: 0 ' *** ' 0.001 ' ** ' 0.01 ' * ' 0.05 '.' 0.1 ' ' 1")
	
	}
	
	if(isKableOutput == FALSE || (exists("uadatasets.sk") && exists("BSkyKableFormatting", env=uadatasets.sk) && uadatasets.sk$BSkyKableFormatting == FALSE))
	{
		doKableFormatting = FALSE
	}
	else
	{
		doKableFormatting = TRUE
		
		if(isRmarkdownOutput == FALSE || (exists("uadatasets.sk") && exists("BSkyRmarkdownFormatting", env=uadatasets.sk) && uadatasets.sk$BSkyRmarkdownFormatting == FALSE))
		{
			doRmarkdownFormatting = FALSE
		}
		else
		{
			doRmarkdownFormatting = TRUE
		}
		
		if(isLatexOutput == TRUE || (exists("uadatasets.sk") && exists("BSkyLatexFormatting", env=uadatasets.sk) && uadatasets.sk$BSkyLatexFormatting == TRUE))
		{
			doLatexFormatting = TRUE
		}
		else
		{
			doLatexFormatting = FALSE
		}
	}
	
	#
	
	if(isTextOutput == TRUE || (exists("uadatasets.sk") && exists("BSkyTextFormatting", env=uadatasets.sk) && uadatasets.sk$BSkyTextFormatting == TRUE))
	{
		doTextFormatting = TRUE
		doKableFormatting = FALSE
		doLatexFormatting = FALSE
		doRmarkdownFormatting = FALSE
	}
	else
	{
		doTextFormatting = FALSE
	}
	
	if(exists("uadatasets.sk") && !exists("BSkyTextFormatting", env=uadatasets.sk)) 
	{
		BSkySetKableAndRmarkdownFormatting(BSkyKableFormatting = FALSE, BSkyRmarkdownFormatting = FALSE, BSkyLaTeXFormatting = FALSE, BSkyTextFormatting=TRUE)
		BSky.print.text()
		
		doTextFormatting = TRUE
		doKableFormatting = FALSE
		doLatexFormatting = FALSE
		doRmarkdownFormatting = FALSE
	}
	
	#BSkyGetTableDisplayLimits ()
	
	if(is.null(maxOutputTables) || maxOutputTables == 0)
	{
		maxOutputTables = 99
	}
	else if(class(maxOutputTables)[1] == "list")
	{
		maxOutputTables = as.numeric(maxOutputTables[[1]]) 
	}
	
	if(is.null(maxRowLimit) || maxRowLimit == 0)
	{
		maxRowLimit = 2000
	}
	else if(class(maxRowLimit)[1] == "list")
	{
		maxRowLimit = as.numeric(maxRowLimit[[2]]) 
	}
	
	if(is.null(maxColLimit) || maxColLimit == 0)
	{
		maxColLimit = 99
	}
	else if(class(maxColLimit)[1] == "list")
	{
		maxColLimit = as.numeric(maxColLimit[[3]]) 
	}
	
	
	BSkyFormat_output = NULL
	
	# cat("\n<br> SK -1 Before BSkyFormat2() <br>\n")
	# print(obj)
	# cat("\n<br>========================================<br>\n")
	
	equationInputObj = FALSE
	original_input_equation_obj = c()
	
	##############################################################################################################
	# Prep the equation object from equatiomatic package by converting to a character string before
	# passing through BSkyFormat2() just to get the equation character string within a BSkyFormat return structure
	##############################################################################################################
	
	if(class(obj)[1] == "equation" && length(class(obj)) > 1 && class(obj)[2] == "character")
	{
		original_input_equation_obj = obj
		obj = as.character(obj)
		equationInputObj = TRUE
	}

	#########################################################################################################
	# Handle the LME and LMER multilevel/effects liner models from nlme, lme4, and lmerTest packages
	# Get all the tables extracted and structred from BSkyFormatLmerMod() helper function before
	# sending the table list to BSkyFormat2() just to get the table list within a BSkyFormat return structure 
	#########################################################################################################
	
	if(class(obj)[1] %in% c("lmerMod", "summary.merMod", "lmerModLmerTest", "lme", "summary.lme"))
	{
		obj = BSkyFormatLmerMod(obj, decimalDigits = decimalDigitsRounding)
	}
	
	##############################################################################################
	#Check the obj type is of the BSky Return Structure from BSkyOneSmTTest() and BSkyIndSmTTest()
	##############################################################################################
	obj = BSkyFormatBSkyOneSampleTtest(obj)
	obj = BSkyFormatBSkyIndSampleTtest(obj)
	obj = BSkyFormatBSkyCrossTable(obj)
	
	##############################################################################################
	# check for the "psych" class for summary analysis object from psych::describe()
	###############################################################################################
	if(class(obj)[1] == "psych")
	{
		obj = as.data.frame(obj)
		
		if(singleTableOutputHeader == "")
		{
			singleTableOutputHeader = "Numerical Summaries"
		}
	}
	
	##############################################################################################
	# check for the "summary.manova" class 
	###############################################################################################
	else if(class(obj)[1] == "summary.manova")
	{
		obj = BSky.print.summary.manova(obj)
		
		
	}
	
	##############################################################################################
	# check for the "summary.aov" class 
	###############################################################################################
	else if(class(obj)[1] == "summary.aov")
	{
		obj = BSky.summary.aov(obj)
		
		
	}
	
	
	##############################################################################################
	# check for the "summary.Anova.mlm" class 
	###############################################################################################
	else if(class(obj)[1] == "summary.Anova.mlm")
	{
		obj = BSkyFormatsummary.Anova.mlm(obj)
		
		
	}
	
	##############################################################################################
	# check for the "summary.Anova.mlm" class 
	###############################################################################################
	else if(class(obj)[1] == "linearHypothesis.mlm")
	{
		obj = BSkyprint.linearHypothesis.mlm (obj,SSP =RM.SSP ,SSPE=RM.SSPE  )
		
		
	}
	
	
	# changing bSkyFormatAppRequest=bSkyFormatAppRequest to bSkyFormatAppRequest= FALSE which is the default value(this is to preseve the table footers attributes within BSkyFormat2) 
	BSkyFormat_output = BSkyFormat2(obj, silentFormatting = silentFormatting, bSkyFormatAppRequest= FALSE, bSkyReturnObj = bSkyReturnObj, ftable_change_variable_order =ftable_change_variable_order, sublist_length =sublist_length, remove_rows_with_zero_count= remove_rows_with_zero_count , no_row_column_headers=no_row_column_headers, decimalDigitsRounding=decimalDigitsRounding, engNotationSetting = engNotationSetting, singleTableOutputHeader = singleTableOutputHeader, isRound = isRound, coefConfInt = coefConfInt, isRmarkdownOutputOn = BSkyIsRmarkdownOutputOn())
	
	
	# cat("\n<br> SK -1 after BSkyFormat2() <br>\n")
	# print(BSkyFormat_output$tables[[1]])
	# cat("\n<br>========================================\n<br>")
	
	# C# app i.e. doKableFormatting == FALSE cannot handle equation object (LaTeX string) from equatiomatic package at this time 
	
	if(is.null(BSkyFormat_output) || length(BSkyFormat_output) == 0 || (doKableFormatting == FALSE && equationInputObj == TRUE))
	{
		cat("\n Cannot Format the Object passed to the BSkyFormat() \n")
		return(invisible(BSkyFormat_output))
	}
	
	
	orig_num_tables = length(BSkyFormat_output$tables) - 1
	
	# in else condition - No table to process except check whether there is anything in the BSky error/warning 
	# table in the else condition beofre returning  
	if(orig_num_tables > 0)
	{
		##########################################################################
		# in BSky degital formating function which gets executed before the BSkyformat2() returns,
		# converts an eitre colimn to all decimal points with machting number of decimals points by padding trailing zeros if needed
		# The above mechanism works well for statistical analysis tables. However, Cross have mix of whole number and decimal numbers
		# that need to be kept as is and not convert everthing to decoimal points with trailing zeros after the decimal points
		# Hence the following processing for the cross table is needed to remo
		##########################################################################
		
		if(length(names(BSkyFormat_output$tables)) > 0 && grepl("(\\bMultiway Cross Table\\b)", names(BSkyFormat_output$tables)[1]))
		{
			for(n in 1:orig_num_tables)
			{
				if(grepl("(\\bMultiway Cross Table\\b)", names(BSkyFormat_output$tables)[n]))
				{
					column_index = which(dimnames(BSkyFormat_output$tables[[n]])[[2]] == "count")
					
					if(length(column_index) > 1)
					{
						for(i in 1:length(column_index))
						{
							if(BSkyFormat_output$tables[[n]][2,column_index] == "Count")
							{
								desired_column_index = column_index[i]
							}
						}
						
						column_index = desired_column_index
					}
					
					
					for(i in 1:nrow(BSkyFormat_output$tables[[n]]))
					{
						if(!(BSkyFormat_output$tables[[n]][i,column_index] %in% c("Residual", "Std. Residual", "Adjusted Residual")) && !grepl("% within", BSkyFormat_output$tables[[n]][i,column_index]))
						{
							for(j in (column_index+1):dim(BSkyFormat_output$tables[[n]])[2])
							{
								if(!is.na(suppressWarnings(as.numeric(BSkyFormat_output$tables[[n]][i,j]))))
								{
									BSkyFormat_output$tables[[n]][i,j] = round(as.numeric(BSkyFormat_output$tables[[n]][i,j]), 0)
								}
							}
						}
						else if(grepl("% within", BSkyFormat_output$tables[[n]][i,column_index]))
						{
							for(j in (column_index+1):dim(BSkyFormat_output$tables[[n]])[2])
							{
								if(!is.na(suppressWarnings(as.numeric(BSkyFormat_output$tables[[n]][i,j]))))
								{
									if(as.numeric(BSkyFormat_output$tables[[n]][i,j]) == round(as.numeric(BSkyFormat_output$tables[[n]][i,j]), 0))
									{
										BSkyFormat_output$tables[[n]][i,j] = round(as.numeric(BSkyFormat_output$tables[[n]][i,j]), 0)
									}
								}
							}
						}
					}
				}
			}
		}
	
	
		####################################################
		#Print BSkyFormat2 tables with a for() loop
		####################################################
		
		num_tables = orig_num_tables
		
		if(maxOutputTables <= 0) maxOutputTables = 1
		if(maxRowLimit <= 0) maxRowLimit = 1
		if(maxColLimit <= 0) maxColLimit = 1
		
		#################################################################################################################
		# Process the number of tables to be outputed based on the outputTableIndex passed or maxOutputTables parameters 
		# outputTableIndex cannot have outputTableIndex = c(a=(1:3, 6:8)) instead outputTableIndex = c(1,2,3,6,7,8)
		# or outputTableIndex = c(tableOne = 1, tableTwo = 2, tableThree = 3, tableFour = 6 .......)
		# 
		# example syntax used in the app 
		# BSkyFormat(lm_summary, outputTableIndex = c(tableone = 3, tabletwo = 1), outputColumnIndex = c(tableone = c(3, 
		# 2, 4), tabletwo = c(1:6, 8)), outputColumnRenames = c(tabletwo = c("My Residual", 
        # ".", "My R-Squared")), outputColumnRenamesRow = c(tabletwo = 1))
		##################################################################################################################
		
		if(length(outputTableIndex) > 0 && is.numeric(outputTableIndex))
		{
			outputTableIndex_names = names(outputTableIndex)
			outputTableIndex_names = outputTableIndex_names[!duplicated(outputTableIndex)]
			
			outputTableIndex = unique(outputTableIndex[outputTableIndex > 0])
			names(outputTableIndex) = outputTableIndex_names
			
			outputTableIndex = outputTableIndex[outputTableIndex <= num_tables]
			
			outputTableIndex_names = names(outputTableIndex)
			
			 if(length(grep("[0-9]\\b", outputTableIndex_names)) > 0)
			 {
				#index names cannot end with a digit - this is to avoid pattern matching of names in column index or column renames list 
				outputTableIndex_names = c()
				outputTableIndex = c() 
			 }
		}
		else
		{
			outputTableIndex = c() 
		}
		
		# cat("\n SK -1 \n")
		# print(outputTableIndex)
		# print(outputTableIndex_names)
		# cat("\n========================================\n")
		
		
		if(length(outputTableIndex) > 0)
		{	
			if(num_tables < length(outputTableIndex))
			{
				outputTableIndex = outputTableIndex[1:num_tables]
				outputTableIndex_names = names(outputTableIndex)
			}
			
			temp_output_tbl_list = BSkyFormat_output$tables
			num_output_tables_needed = 0
			
			for(i in 1:length(outputTableIndex))
			{
				if(outputTableIndex[i] <= num_tables)
				{
					BSkyFormat_output$tables[[i]] = temp_output_tbl_list[[outputTableIndex[i]]]
					names(BSkyFormat_output$tables)[i] = names(temp_output_tbl_list[outputTableIndex[i]])
					num_output_tables_needed = num_output_tables_needed + 1
				}
			}
			
			BSkyFormat_output$tables[[num_output_tables_needed + 1]] = temp_output_tbl_list[[length(temp_output_tbl_list)]]
			names(BSkyFormat_output$tables)[num_output_tables_needed + 1] = c("")
			
			if(num_output_tables_needed < num_tables)
			{
				BSkyFormat_output$tables[(num_output_tables_needed+2):(num_tables+1)] = NULL
			}
			
			BSkyFormat_output$nooftables = num_output_tables_needed + 1
			num_tables = num_output_tables_needed
		}
		
		# cat("\n SK -1-1 \n")
		# print(outputTableIndex)
		# print(outputTableIndex_names)
		# print(num_tables)
		# cat("\n========================================\n")
		
		if(num_tables > maxOutputTables) 
		{
			num_tables = maxOutputTables
			BSkyFormat_output$tables = BSkyFormat_output$tables[c(1:num_tables, (orig_num_tables+1))]
		}
		
		outputColumnIndex_names = names(outputColumnIndex)
		outputColumnRenames_names = names(outputColumnRenames)
		outputColumnRenamesRow_names = names(outputColumnRenamesRow)
		
		
		################################################################################################
		#Process renaming the tables as passed through singleTableOutputHeader and/or outputTableRenames
		################################################################################################
		orig_table_names = names(BSkyFormat_output$tables)
		output_table_rename_strings = length(outputTableRenames)
		
		if(output_table_rename_strings > 0)
		{
			if(output_table_rename_strings > num_tables)
			{
				outputTableRenames_adjusted = outputTableRenames[1:num_tables]
			}
			else
			{
				outputTableRenames_adjusted = outputTableRenames
			}
			
			for(n in 1: length(outputTableRenames_adjusted))
			{
				if(outputTableRenames_adjusted[[n]] != ".")
				{
					orig_table_names[[n]] = outputTableRenames_adjusted[n]
				}
				
				# cat("\n SK 1-1 \n")
				# print(orig_table_names)
				# cat("\n=================================\n")
			}
			
			names(BSkyFormat_output$tables) = orig_table_names
		}

		for (i in 1:num_tables) 
		{	
				# cat("\n SK -1 \n")
				# print(i)
				# print(num_tables)
				# cat("\n========================================\n")
				
				orig_all_footnote_attributes = c()
	
				#if(!is.null(attr(BSkyFormat_output$tables[[i]], "BSkyFootnote_BSkySplit")))
				{
					orig_all_footnote_attributes = attributes(BSkyFormat_output$tables[[i]]) #attr(BSkyFormat_output$tables[[i]], "BSkyFootnote_BSkySplit")
					orig_all_footnote_attributes = orig_all_footnote_attributes[grepl("(footnote|Footnote)", names(orig_all_footnote_attributes))] 
				}
		
				unfiltered_table = BSkyFormat_output$tables[[i]]
				
				if(class(BSkyFormat_output$tables[[i]])[1] != "data.frame" && class(BSkyFormat_output$tables[[i]])[1] != "matrix")
				{
					#column_names = dimnames(BSkyFormat_output$tables[[i]])[[2]]
					column_names = names(BSkyFormat_output$tables[[i]])
					temp_tbl = matrix(BSkyFormat_output$tables[[i]], ncol= ncol(BSkyFormat_output$tables[[i]]))
					if(ncol(temp_tbl) == length(column_names))
					{
						dimnames(temp_tbl)[[2]] = list()
						dimnames(temp_tbl)[[2]] = column_names
					}
					BSkyFormat_output$tables[[i]] = temp_tbl
				}
				
				#######################################################################################################
				#Remove empty rows i.e. rows with all c("") values
				#BSkyFormat_output$tables[[i]] = as.data.frame(BSkyFormat_output$tables[[i]][!apply(BSkyFormat_output$tables[[i]] == "", 1, all),])
				#Need the temp table to perform the following opeartions - otherwise R messes up with the dimnames of the tables!!
				########################################################################################################
				
				temp_tbl = BSkyFormat_output$tables[[i]]
				temp_tbl2 = temp_tbl[!apply(temp_tbl == "", 1, all),]
				
				# With the above operation - for a single row charater data.frame becomes a simple character vector e.g. lm() residual table
				# All these temp table is needed because somehow R is messing up with the simple dimnames() operations
			
				if(class(temp_tbl2)[1] != "data.frame" && class(temp_tbl2)[1] != "matrix")
				{
					#new_table_removed_empty_rows = matrix(new_table_removed_empty_rows, ncol= ncol(BSkyFormat_output$tables[[i]]))
					#dimnames(new_table_removed_empty_rows)[[2]] = dimnames(BSkyFormat_output$tables[[i]])[[2]]
					#temp_tbl = BSkyFormat_output$tables[[i]]
					
					temp_tbl2 = matrix(temp_tbl2, ncol= ncol(temp_tbl))
					dimnames(temp_tbl2)[[2]] = list()
					dimnames(temp_tbl2)[[2]] = dimnames(temp_tbl)[[2]]
				}
				
				dimnames(temp_tbl2)[[1]] = list()
				rownames(temp_tbl2) = dimnames(temp_tbl)[[1]][1:nrow(temp_tbl2)]
				
				BSkyFormat_output$tables[[i]] = temp_tbl2
				
				# These are the sizes after removing the empty rows 
				orig_col_names = dimnames(BSkyFormat_output$tables[[i]])[[2]]
				orig_row_names = dimnames(BSkyFormat_output$tables[[i]])[[1]]
				orig_dim_size = dim(BSkyFormat_output$tables[[i]])
				orig_table = BSkyFormat_output$tables[[i]]
				
				
				max_cols = dim(BSkyFormat_output$tables[[i]])[2]
				max_rows = nrow(BSkyFormat_output$tables[[i]])[1]
				max_limit_exceed = FALSE 
				
				if(max_rows > maxRowLimit)
				{
					max_rows = maxRowLimit
					max_limit_exceed = TRUE
				}
				
				if(max_cols > maxColLimit)
				{
					max_cols = maxColLimit
					max_limit_exceed = TRUE
				}
				
				########################################################
				#Trim the table to the max limit of the rows and columns 
				########################################################
				
				row_names_before_table_trimming = dimnames(BSkyFormat_output$tables[[i]])[[1]]
				col_names_before_table_trimming = dimnames(BSkyFormat_output$tables[[i]])[[2]]
				
				BSkyFormat_output$tables[[i]] = BSkyFormat_output$tables[[i]][1:max_rows, 1:max_cols]
				
				# With the above operation - for a single row charater data.frame becomes a simple character vector e.g. lm() residual table
				# All these temp table is needed because somehow R is messing up with the simple dimnames() operations 
				if(class(BSkyFormat_output$tables[[i]])[1] != "data.frame" && class(BSkyFormat_output$tables[[i]])[1] != "matrix")
				{
					temp_tbl = BSkyFormat_output$tables[[i]] 
					temp_tbl = matrix(BSkyFormat_output$tables[[i]], ncol= max_cols)
					
					BSkyFormat_output$tables[[i]] = temp_tbl
				}
				
				if(!is.null(row_names_before_table_trimming) && length(row_names_before_table_trimming) > 0)
				{
					dimnames(BSkyFormat_output$tables[[i]])[[1]] = list()
					#dimnames(BSkyFormat_output$tables[[i]])[[1]] = row_names_before_table_trimming[[1:max_rows]] #R fails to set the row name??
					rownames(BSkyFormat_output$tables[[i]]) = c(row_names_before_table_trimming[1:max_rows])
				}
				
				if(!is.null(col_names_before_table_trimming) && length(col_names_before_table_trimming) > 0)
				{
					dimnames(BSkyFormat_output$tables[[i]])[[2]] = list()
					dimnames(BSkyFormat_output$tables[[i]])[[2]] = c(col_names_before_table_trimming[1:max_cols])
				}
				
				##########################################################################
				# Adjust the columns to be output and also the sequence of the columns
				# based on the outputTableIndex and outputColumnIndex passed 
				##########################################################################
					
				if(length(outputTableIndex) > 0 && length(outputColumnIndex) > 0 && length(outputTableIndex_names) > 0 )
				{
					if(length(outputColumnIndex_names) > 0)
					{
						# Example \\bapple\\b - exact match with the word boundary
						
						desired_output_col_indices = grep(paste("\\b",outputTableIndex_names[i],"[0-9]*", "\\b", sep=""), outputColumnIndex_names)
						
						valid_col_indices = unique(outputColumnIndex[desired_output_col_indices][outputColumnIndex[desired_output_col_indices]<= dim(BSkyFormat_output$tables[[i]])[2]])
						
						# cat("\n SK 0 \n")
						# print(outputTableIndex)
						# print(outputColumnIndex)
						# print(desired_output_col_indices)
						# print(valid_col_indices)
						# cat("\n+++++++++++++++++++++++++++++++++\n")
				
						if(length(valid_col_indices) > 0)
						{
							column_names = dimnames(BSkyFormat_output$tables[[i]])[[2]]
							BSkyFormat_output$tables[[i]] = BSkyFormat_output$tables[[i]][,valid_col_indices] #[,outputColumnIndex[desired_output_col_indices]]
							
							temp_column_names = column_names[valid_col_indices]
							
							if(class(BSkyFormat_output$tables[[i]])[1] != "data.frame" && class(BSkyFormat_output$tables[[i]])[1] != "matrix")
							{
								#column_names = dimnames(BSkyFormat_output$tables[[i]])[[2]]
								temp_tbl = matrix(BSkyFormat_output$tables[[i]], ncol= length(valid_col_indices))
								if(ncol(temp_tbl) == length(temp_column_names))
								{
									dimnames(temp_tbl)[[2]] = list()
									dimnames(temp_tbl)[[2]] = temp_column_names
								}
								BSkyFormat_output$tables[[i]] = temp_tbl
								
								# cat("\n SK 0-1 \n")
								# print(temp_column_names)
								# print(temp_tbl)
								# print(BSkyFormat_output$tables[[i]])
								# cat("\n+++++++++++++++++++++++++++++++++\n")
							}
							
							dimnames(BSkyFormat_output$tables[[i]])[[2]] = temp_column_names
							
							# cat("\n SK 0-1 \n")
							# print(temp_column_names)
							# print(temp_tbl)
							# print(BSkyFormat_output$tables[[i]])
							# cat("\n+++++++++++++++++++++++++++++++++\n")
							
							################################################################
							# Rename the output columns based on outputColumnRenames passed 
							################################################################
							
							if(length(outputColumnRenames_names) > 0)
							{
								desired_output_col_renames_indices = grep(paste("\\b",outputTableIndex_names[i], "[0-9]*", "\\b", sep=""), outputColumnRenames_names)
								
								if(length(desired_output_col_renames_indices) > 0)
								{	
									if(length(desired_output_col_renames_indices) > length(valid_col_indices))
									{
										desired_output_col_renames_indices = desired_output_col_renames_indices[1:length(valid_col_indices)]
									}
									
									
									if(length(desired_output_col_renames_indices) > 0)
									{
										desired_output_col_renames_row_indices = grep(paste("\\b",outputTableIndex_names[i], "[0-9]*", "\\b", sep=""), outputColumnRenamesRow_names)
										total_rows = nrow(BSkyFormat_output$tables[[i]])
										
										# cat("\n SK 1-1 \n")
										# print(desired_output_col_renames_row_indices)
										# print(outputColumnRenamesRow)
										# print(total_rows)
										# cat("\n=================================\n")
										
										if(length(desired_output_col_renames_row_indices) > 0 && outputColumnRenamesRow[desired_output_col_renames_row_indices[1]] > 0 && outputColumnRenamesRow[desired_output_col_renames_row_indices[1]] <= total_rows)
										{
											temp_column_names = BSkyFormat_output$tables[[i]][outputColumnRenamesRow[desired_output_col_renames_row_indices[1]],]
										}
										else
										{
											temp_column_names = dimnames(BSkyFormat_output$tables[[i]])[[2]]
										}
										
										for(m in 1: length(desired_output_col_renames_indices))
										{
											if(outputColumnRenames[desired_output_col_renames_indices[m]] != ".")
											{
												temp_column_names[m] = outputColumnRenames[desired_output_col_renames_indices[m]]
											}
											
											# cat("\n SK 1-1 \n")
											# print(temp_column_names)
											# cat("\n=================================\n")
										}
										
										if(length(desired_output_col_renames_row_indices) > 0 && outputColumnRenamesRow[desired_output_col_renames_row_indices[1]] > 0 && outputColumnRenamesRow[desired_output_col_renames_row_indices[1]] <= total_rows)
										{
											BSkyFormat_output$tables[[i]][outputColumnRenamesRow[desired_output_col_renames_row_indices[1]],] = temp_column_names
										}
										else
										{
											dimnames(BSkyFormat_output$tables[[i]])[[2]] = temp_column_names
										}
										
										# cat("\n=================================\n")
										# print(temp_column_names)
										# print(BSkyFormat_output$tables[[i]])
										# cat("\n+++++++++++++++++++++++++++++++++\n")
									}
									
								}
							}
							else
							{
								dimnames(BSkyFormat_output$tables[[i]])[[2]] = column_names[outputColumnIndex[desired_output_col_indices]]
							}
						}
						
						# cat("\n SK 2 \n")
						# print(outputColumnIndex_names)
						# print(desired_output_col_indices)
						# print(outputColumnIndex[desired_output_col_indices])
						# print(dimnames(BSkyFormat_output$tables[[i]])[[2]])
						# print(class(BSkyFormat_output$tables[[i]]))
						# print(BSkyFormat_output$tables[[i]])
						# cat("\n+++++++++++++++++++++++++++++++++\n")
					}
				}
				
				##################################################################################
				## p-value columns - singnificance code insertion and corresponding Table footnote
				##################################################################################
				
				sig_footnoteNeeded = FALSE
				total_rows = nrow(BSkyFormat_output$tables[[i]])
				
				column_names = dimnames(BSkyFormat_output$tables[[i]])[[2]]
				sigfColIndex = which((column_names %in% sigfColPatterns), TRUE)
				starting_row = 1
				
				if(length(sigfColIndex) == 0 && total_rows > 1)
				{
					#Second_header_row_found
					column_names = BSkyFormat_output$tables[[i]][1,]
					sigfColIndex = which((column_names %in% sigfColPatterns), TRUE)
					starting_row = 2
				}
				
				if(length(sigfColIndex) == 0 && total_rows > 2 )
				{
					#3rd_header_row_found
					column_names = BSkyFormat_output$tables[[i]][2,]
					sigfColIndex = which((column_names %in% sigfColPatterns), TRUE)
					starting_row = 3 
				}

				pvalueColumnAlignmentAdjustmentIndices = c()
				
				if(length(sigfColIndex) > 0)
				{	
					#sig_footnoteNeeded = TRUE
					
					for(k in 1:length(sigfColIndex))
					{
						stringPaddingAdjustment = c()
						
						for(m in starting_row:total_rows)
						{	
							if(!is.na(suppressWarnings(as.numeric(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]]))) && is.finite(suppressWarnings(as.numeric(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]]))))
							{
								if(as.numeric(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]]) >=0 && as.numeric(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]]) <0.001)
								{
									if(pvalueDisplaySettings$showActualPValueInOutput == TRUE)
									{
										if(pvalueDisplaySettings$pvalueDropAsterisk == FALSE)
										{
											BSkyFormat_output$tables[[i]][m,sigfColIndex[k]] = paste(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]],"***")
											stringPaddingAdjustment = c(stringPaddingAdjustment, 3)
										}
									}
									else
									{	
										if(pvalueDisplaySettings$pvalueDropAsterisk == FALSE)
										{
											BSkyFormat_output$tables[[i]][m,sigfColIndex[k]] = paste("< ",".001***") 
											stringPaddingAdjustment = c(stringPaddingAdjustment, 3)
										}
										else
										{
											BSkyFormat_output$tables[[i]][m,sigfColIndex[k]] = paste("< ",".001")
										}
									}
									
									sig_footnoteNeeded = TRUE
								}
								else if(as.numeric(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]]) >=0.001 && as.numeric(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]]) <0.01)
								{
									if(pvalueDisplaySettings$pvalueDropAsterisk == FALSE)
									{
										BSkyFormat_output$tables[[i]][m,sigfColIndex[k]] = paste(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]],"**")
										stringPaddingAdjustment = c(stringPaddingAdjustment, 2)
									}
									# else
									# {
										# BSkyFormat_output$tables[[i]][m,sigfColIndex[k]] = paste(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]])
									# }
									
									sig_footnoteNeeded = TRUE
								}
								else if(as.numeric(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]]) >=0.01 && as.numeric(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]]) <0.05)
								{
									if(pvalueDisplaySettings$pvalueDropAsterisk == FALSE)
									{
										BSkyFormat_output$tables[[i]][m,sigfColIndex[k]] = paste(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]],"*")
										stringPaddingAdjustment = c(stringPaddingAdjustment, 1)
									}
									
									sig_footnoteNeeded = TRUE
								}
								else if(as.numeric(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]]) >=0.05 && as.numeric(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]]) <0.1)
								{
									if(pvalueDisplaySettings$pvalueDropAsterisk == FALSE)
									{
										BSkyFormat_output$tables[[i]][m,sigfColIndex[k]] = paste(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]],".")
										stringPaddingAdjustment = c(stringPaddingAdjustment, 1)
									}
									
									sig_footnoteNeeded = TRUE
								}
								else if(as.numeric(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]]) >=0.01 && as.numeric(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]]) <1)
								{
									BSkyFormat_output$tables[[i]][m,sigfColIndex[k]] = paste(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]]," ")
									
									stringPaddingAdjustment = c(stringPaddingAdjustment, 0)
									
									sig_footnoteNeeded = TRUE
								}
								else
								{
									stringPaddingAdjustment = c(stringPaddingAdjustment, 0)
								}
							}
							else
							{
								stringPaddingAdjustment = c(stringPaddingAdjustment, 0)
							}
						}
						
						if(((length(grep("\\*", as.character(BSkyFormat_output$tables[[i]][,sigfColIndex[k]]))) > 0) || (length(stringPaddingAdjustment) > 0 && max(stringPaddingAdjustment) > 0)) && pvalueDisplaySettings$pvalueDropAsterisk == FALSE)
						{
							maxstars = max(stringPaddingAdjustment)
							
							pvalueColumnAlignmentAdjustmentIndices = c(pvalueColumnAlignmentAdjustmentIndices, sigfColIndex[k])
							
							for(m in starting_row:total_rows)
							{
								BSkyFormat_output$tables[[i]][m,sigfColIndex[k]] = paste(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]],paste(rep(pvalueDisplaySettings$pValueDisplayPaddingCharacter,(maxstars - stringPaddingAdjustment[(m - starting_row) + 1])), collapse=""), sep="")
								#BSkyFormat_output$tables[[i]][m,sigfColIndex[k]] = paste(BSkyFormat_output$tables[[i]][m,sigfColIndex[k]], "x", sep="")
							}
						}
					}
				}

	# sigfColPatterns = c("Pr(>|t|)","p.value", "Sig.", "Sig.(2-tailed)", "p-value", "Pr(>|z|)", "Pr(>Chi)", "p.value(z)", "p.value(t)", "Sig.(2-tail)", "Sig.(1-tail, >)", "Sig.(1-tail, <)", "Pr(>Chisq)", "P(>|Chi|)", "Pr(Chi)")
	# VALUE-FROM	VALUE-TO	APPEND-STR(signif code)
	# 0				0.001		***
	# 0.001			0.01		**
	# 0.01			0.05		*
	# 0.05			0.1			.
	# 0.1				1			(single-space-character)
	# footnote(general = "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
	# For *** range there are following two variations a) append ***, b) replace cell data with <.001*** (preferred default choice) 

				
				#####################################################################################################  
				# Merging/collapsing repeating column names 
				# Remove all repeating column names with only one name and postion it into the center column position 
				#####################################################################################################
				column_names = dimnames(BSkyFormat_output$tables[[i]])[[2]]
				repeated_column_header_found = FALSE 
				pre_merge_column_names = column_names
				
				if(!is.null(column_names))
				{
					rle_columns = rle(column_names)
				}
				
				if(is.null(column_names) || ( (any(rle(column_names)$lengths > 1) == FALSE) || (length(unique(column_names)) == 1 && unique(column_names) == "")))
				{
					repeated_column_header_found = FALSE
				}
				else if((length(rle_columns$values[rle_columns$lengths > 1]) > 0) && all((rle_columns$values[rle_columns$lengths > 1]) == "") == TRUE)
				{
					repeated_column_header_found = FALSE
				}
				else
				{
					repeated_column_header_found = TRUE
				}
				
				# if(!is.null(column_names))
				# {
					# cat("\n ================<br>SK1<br>\n")
					# print(column_names)
					# cat("\n<br>\n")
					# print(rle(column_names)$lengths)
					# cat("\n<br>\n")
					# print(any(rle(column_names)$lengths > 1))
					# cat("\n<br>\n")
					# print(repeated_column_header_found)
					# cat("\n ==============================================<br>\n")
				# }
				
				#repeated_column_header_found = FALSE
				
				if(repeated_column_header_found == TRUE && !is.null(column_names) && ((length(column_names) !=1 && length(unique(column_names)) == 1 && unique(column_names) != "") || (length(column_names) > length(unique(column_names)))))
				#if(!is.null(column_names) && ((length(column_names) !=1 && length(unique(column_names)) == 1 && unique(column_names) != "") || (length(column_names) > length(unique(column_names)))))
				{
					#repeated_column_header_found = TRUE
					
					##########################################################################################################
					# Need to add one extra row back to the table as the trimming of the table did not account for the top row 
					# which is a column header e.g. lm summary table) 
					# May need to add add another row - check later for Independent_Samples_t_Test = t.test()has two rows of pseudo header 
					##########################################################################################################
					if(nrow(orig_table) > max_rows)
					{
						BSkyFormat_output$tables[[i]] = rbind(BSkyFormat_output$tables[[i]], orig_table[max_rows+1,1:max_cols])
						rownames(BSkyFormat_output$tables[[i]]) = c(row_names_before_table_trimming[[1:max_rows+1]])
					}
						
					
					if(length(unique(column_names)) == 1)
					{
						columnHeaderCenterPosition = floor(dim(BSkyFormat_output$tables[[i]])[2]/2)
						if(columnHeaderCenterPosition == 0) columnHeaderCenterPosition = 1
						dimnames(BSkyFormat_output$tables[[i]])[[2]] = rep(c(""),dim(BSkyFormat_output$tables[[i]])[2])
						dimnames(BSkyFormat_output$tables[[i]])[[2]][columnHeaderCenterPosition] = unique(column_names)
						repeted_column_header_df = data.frame(repeat_run_len=dim(BSkyFormat_output$tables[[i]])[2], column_name = unique(column_names), cur_start_pos = 1, stringsAsFactors = FALSE)
					}
					else
					{
						column_names_repeat_index = rle(column_names)
						repeted_column_header_df = data.frame(repeat_run_len=column_names_repeat_index$lengths, column_name = column_names_repeat_index$values, stringsAsFactors = FALSE)
						repeted_column_header_df[,"cur_start_pos"] = cumsum(repeted_column_header_df$repeat_run_len)+1
						#Shift the column cur_start_pos down by a row to get the actuatl starting position for each repeated column name
						repeted_column_header_df$cur_start_pos <- c(1, repeted_column_header_df$cur_start_pos[1:(nrow(repeted_column_header_df) - 1)])
						
						merged_column_header = rep(c(""),dim(BSkyFormat_output$tables[[i]])[2])
						
						for(j in 1:nrow(repeted_column_header_df))
						{
							if(repeted_column_header_df[j, "repeat_run_len"] == 1)
							{
								merged_column_header[[repeted_column_header_df[j, "cur_start_pos"]]] = repeted_column_header_df[j, "column_name"]
							}
							else
							{
								column_header_center_position = repeted_column_header_df[j, "cur_start_pos"] + floor(repeted_column_header_df[j, "repeat_run_len"]/2)
								merged_column_header[[column_header_center_position]] = repeted_column_header_df[j, "column_name"]
							}
						}
						
						dimnames(BSkyFormat_output$tables[[i]])[[2]] = merged_column_header
					}
					
					# cat("\n<br> SK-1 <br>\n")
					# print(dimnames(BSkyFormat_output$tables[[i]])[[2]])
					# cat("<br>")
					# print(repeted_column_header_df)
					# cat("+++++++++++++++++++++++++++++++++<br>")
				}
		  
				new_table_removed_empty_rows = BSkyFormat_output$tables[[i]]
				
				#Just a side note - kableExtra takes the row.names and makes it the 1st column in the data.frame - hence messes up the original col count 
				num_kableExtra_cols = dim(new_table_removed_empty_rows)[2]
				
				  
				#################################################################################################
				# Converting row names as the first column in the table - needed to aviod HTML trouble with row.names containing spacing
				##################################################################################################
				
				row_names_convert_column = FALSE
				
				if(length(dimnames(BSkyFormat_output$tables[[i]])[[1]])> 0)
				{
					row_names_convert_column = TRUE
					#num_kableExtra_cols = dim(BSkyFormat_output$tables[[i]])[2] + 1
					
					new_table_removed_empty_rows = cbind(dimnames(BSkyFormat_output$tables[[i]])[[1]], new_table_removed_empty_rows)
					rownames(new_table_removed_empty_rows) = NULL 
					num_kableExtra_cols = dim(new_table_removed_empty_rows)[2]
					
					if(length(pvalueColumnAlignmentAdjustmentIndices) > 0)
					{
						#Adjust the previously stored pvalue column indices by 1 i.e. push forward by one 
						pvalueColumnAlignmentAdjustmentIndices = pvalueColumnAlignmentAdjustmentIndices + 1
					}
				}
			  
			  
				#KableExtra table styling param syntax add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 2))
				#repeted_column_header_df = data.frame(repeat_run_len = c(1,3,3,2,1), column_name=c("col1","col2","col3","col4","col5"), cur_start_pos = c(1,2,3,4,5))
				
					
				if(repeated_column_header_found == TRUE)
				{
					merged_col_top_header = c() 
					merged_col_header_alignment = c()
					
					for(j in 1:nrow(repeted_column_header_df))
					{
					   merged_col_top_header = paste(merged_col_top_header, "\"",repeted_column_header_df[j,2],"\"","=", repeted_column_header_df[j,1],",", sep="")
					   
					   if(repeted_column_header_df[j,1] > 1)
					   {
							merged_col_header_alignment = c(merged_col_header_alignment, "c")
					   }
					   else
					   {
							merged_col_header_alignment = c(merged_col_header_alignment, "r")
					   }
					}

					merged_col_top_header = substr(merged_col_top_header, 1, nchar(merged_col_top_header) - 1)
					
					#Adjust the comlun length to accomodate an extra column as the row names becomes a new column (i.e. 1st column)
					
					if(row_names_convert_column == TRUE)
					{
						merged_col_top_header = paste("\" \",", merged_col_top_header)
						
						merged_col_header_alignment = c("r", merged_col_header_alignment)
						
						pre_merge_column_names = c(c(" "),pre_merge_column_names)
					}
					
					merged_col_top_header = paste("c(",merged_col_top_header,")")
				}
				
				#####################################################################################################################
				# Prepare the footnote string if one is needed to add to the table later in the code Kable code or in table attribute 
				######################################################################################################################
				
				footnote_string = c() #c("Note:")
				#footnote_string_names = c(" ")
				
				if(sig_footnoteNeeded == TRUE && pvalueDisplaySettings$pvalueDropAsterisk == FALSE && length(pvalueColumnAlignmentAdjustmentIndices) > 0)
				{
					footnote_string = c(footnote_string, sigFootnote)
					
					#p = p %>% footnote(general = c(sigFootnote, "SK - My second footnote"), general_title = c("", ""))
					#p = p %>% footnote(general = "SK - My second footnote", general_title = "")
				}
				
				if(length(perTableFooter) > 0 & i <= length(perTableFooter))
				{
					if(perTableFooter[i] != ".")
					{
						footnote_string = c(footnote_string, perTableFooter[i])
					}
				}
				
				#"BSkyFootnote_BSkySplit"
				if(length(orig_all_footnote_attributes) > 0)
				{
					for(n in 1:length(orig_all_footnote_attributes))
					{
						footnote_string = c(footnote_string, orig_all_footnote_attributes[n]) #c(footnote_string, attr(df, "BSkyFootnote_BSkySplit"))
					}
				}
				
				if(length(repeatAllTableFooter) > 0)
				{
					for(n in 1:length(repeatAllTableFooter))
					{
						footnote_string = c(footnote_string, repeatAllTableFooter[n]) 
					}
				}
				
				if(doKableFormatting == TRUE && doTextFormatting == FALSE)
				{
					###################################
					# Adjusting the Table Caption Text
					###################################
					
					if(doRmarkdownFormatting == FALSE)
					{
						if(max_limit_exceed == FALSE)
						{
							if(length(trimws(names(BSkyFormat_output$tables[i]))) == 0 || (trimws(names(BSkyFormat_output$tables[i]))) == c(""))
							{
								tableCaption = c("")
							}
							else
							{
								if(doLatexFormatting == FALSE)
								{
									#tableCaption = paste("**", trimws(names(BSkyFormat_output$tables[i])), "**", sep="")
									#tableCaption = paste("<strong> ", trimws(names(BSkyFormat_output$tables[i])), " </strong>", sep="")
									tableCaption = paste(trimws(names(BSkyFormat_output$tables[i])))
								}
								else
								{
									tableCaption = paste(trimws(names(BSkyFormat_output$tables[i])))
								}
							}
						}
						else
						{
							tableCaption1 = paste(trimws(names(BSkyFormat_output$tables[i])),"(Table size display limit of", maxRowLimit,"rows and", maxColLimit,"cols exceeded - actual table size:", orig_dim_size[[1]],  "rows and", orig_dim_size[[2]], "cols)")
							
							if(doLatexFormatting == FALSE)
							{
								#tableCaption = paste("<strong> ", trimws(tableCaption1), " </strong>", sep="")
								tableCaption = paste(trimws(tableCaption1))
							}
							else
							{
								tableCaption = paste(trimws(tableCaption1))
							}
						}
					}
					else
					{
						if(max_limit_exceed == FALSE)
						{
							if(length(trimws(names(BSkyFormat_output$tables[i]))) == 0 || (trimws(names(BSkyFormat_output$tables[i]))) == c(""))
							{
								tableCaption = c("")
							}
							else
							{
								if(doLatexFormatting == FALSE)
								{
									tableCaption = paste("**", trimws(names(BSkyFormat_output$tables[i])), "**", sep="")
									#tableCaption = paste(trimws(names(BSkyFormat_output$tables[i])), sep="")
								}
								else
								{
									tableCaption = paste(trimws(names(BSkyFormat_output$tables[i])))
								}
							}
						}
						else
						{
							#tableCaption1 = paste(trimws(names(BSkyFormat_output$tables[i])),"(Table size limit exceeded -", max_rows, "rows and", max_cols, "columns displayed )")
							tableCaption1 = paste(trimws(names(BSkyFormat_output$tables[i])),"(Table size display limit of", maxRowLimit,"rows and", maxColLimit,"cols exceeded - actual table size:", orig_dim_size[[1]],  "rows and", orig_dim_size[[2]], "cols)")
							
							if(doLatexFormatting == FALSE)
							{
								tableCaption = paste("**", trimws(tableCaption1), "**", sep="")
								#tableCaption = paste(trimws(tableCaption1), sep="")
							}
							else
							{
								tableCaption = paste(trimws(tableCaption1))
							}
						}
						
						# cat("\nSK -1\n")
						# print(tableCaption)
						# print(names(BSkyFormat_output$tables[i]))
						# print(length(trimws(names(BSkyFormat_output$tables[i]))))
					}
					
					if(doLatexFormatting == TRUE)
					{
						tableCaption = paste(tableCaption, "\n")
						tableCaption = linebreak(tableCaption)
					}
					
					#############################################################
					# Handle BSkyFormat("plain text", "html text", or LateX model equation printing code)  
					# i.e. table with one row and one column with no cloumn name otherwise for normal table go to the else part 
					##############################################################
					
					empty_header_table_found = FALSE
					
					table_already_printed_to_sync_file = FALSE
					
					# print/cat/writeLines the following special single character string table in place here
					if(dim(unfiltered_table)[1] == 1 && dim(unfiltered_table)[2] == 1 && is.null(dimnames(unfiltered_table)[[1]]) == TRUE && is.null(dimnames(unfiltered_table)[[2]]) == TRUE)
					{
						# Detect whether this is a HTML text chunk, LaTex equation or just plain text with matching of <html </head> <table </table>
					
						if(grepl("(<html)+|(<table)+|(</table>)+|(<p>)+|(</p>)+|(</head>)+",unfiltered_table[1,1]) == TRUE)
						{
							table_already_printed_to_sync_file = TRUE
							
							p = unfiltered_table[1,1]
							
							if(doRmarkdownFormatting == TRUE && doLatexFormatting == FALSE)
							{
								writeLines(p) 
							}
							
							# cat("\n<br> SK 2 <br>\n")
							# cat("\n<br> ============================== <br>\n")
							# print(unfiltered_table)
							# cat("\n<br> ============================== <br>\n")
							# writeLines(as.character(p))
							# #print(p)
							# cat("\n<br> ============================== <br>\n")
						}
						else if(equationInputObj == TRUE)
						{
							table_already_printed_to_sync_file = TRUE
							
							if(doLatexFormatting == FALSE)
							{
								# when itelics is not chosen i.e. equatiomatic::extract_eq(....., ital_vars = FALSE)
								
								if(grepl("\\operatorname",unfiltered_table[1,1]) == TRUE)
								{
									# if(grepl("\\widehat",unfiltered_table[1,1]) == TRUE)
									# {
										p = paste("\\[", as.character(unfiltered_table[1,1]), "\\]")
										
										#To protect the '_' character from getting messed up by HTML output
										#p1 = gsub("\\}_\\{","}\\\\_{", p)
										p = HTMLencode(p, encode.only = c("_")) 
										
										if(doRmarkdownFormatting == TRUE)
										{
											print(unname(as.data.frame(p)),quote = FALSE, row.names = FALSE)
										}
									# }
									# else
									# {
										# p = unfiltered_table[1,1]
										
										# if(doRmarkdownFormatting == TRUE)
										# {
											# cat(p)
										# }
									# }
								}
								else # when itelics is chosen i.e. equatiomatic::extract_eq(....., ital_vars = TRUE)
								{
									p = unfiltered_table[1,1]
									
									if(grepl("\\[",p) == FALSE)
									{
										p = paste("\\[", as.character(unfiltered_table[1,1]), "\\]")
										
										if(doRmarkdownFormatting == TRUE)
										{
											print(unname(as.data.frame(p)),quote = FALSE, row.names = FALSE)
										}
									}
									else
									{
										if(doRmarkdownFormatting == TRUE)
										{
											cat(p)
										}
									}
								}
							}
							else
							{
								p = unfiltered_table[1,1]
								
								print(original_input_equation_obj)
							}
						}	
						else # Simple BSkyFormat("Hello World") style of formatting 
						{
							# Since the string is already prnted out to the sync file
							table_already_printed_to_sync_file = TRUE
								
							if(doKableFormatting == TRUE && doLatexFormatting == FALSE)
							{
								## <br> for newline break <p> for paragraph
								# if(doRmarkdownFormatting == TRUE)
								# {
									unfiltered_table[1,1] = gsub("\\n", "<br>", unfiltered_table[1,1])
									p = paste("<p>", unfiltered_table[1,1], "</p>")
								# }
								
								# For BSky electron app just write to sink file - no need to convert explicitely \n to <br> or put <p> tag around 
								# 08/20/21 SK changed the logic for electron app to not print to sink file - rather keep it in the BSky return structure queue
								if(doRmarkdownFormatting == TRUE)
								{
									writeLines(as.character(unfiltered_table[1,1]))
								}
								
								if(doRmarkdownFormatting == TRUE)
								{
									cat("<br>")
								}
								
								#p = unfiltered_table[1,1] #moved out (see below) of the if clause 
							}
							else
							{
								p = unfiltered_table[1,1]
							}
							
							# else
							# {
								# # \\ double \ is to start a new line in LaTeX
								# unfiltered_table[1,1] = gsub("\\n", "\\\\", unfiltered_table[1,1])
								# p = paste(unfiltered_table[1,1])
							# }
						}
					}
					else
					{
						# KableExtra errors out on table with no column names 
						# so tables with no column names, needed a dummy column header all with c(" ")
						if(length(dimnames(new_table_removed_empty_rows)[[2]]) == 0)
						{
							#kableExtra::kbl will error out if there is no column header 
							dimnames(new_table_removed_empty_rows)[[2]] = c(rep(" ", dim(new_table_removed_empty_rows)[2]))
							
							empty_header_table_found = TRUE
						}
						else
						{
							if(repeated_column_header_found == TRUE)
							{
								dimnames(new_table_removed_empty_rows)[[2]] = rep(c(" "), dim(new_table_removed_empty_rows)[2])
							}
						}
						
						# bootstrap_options = c(\"striped\", \"condensed\", \"responsive\")
						# table_position = \"left\", 
						# full_width = T, 
						# html_font = \"Cambria\", 
						# fixed_thead = list(enabled = T, background = \"#F0F8FF\"))
						
						#tableStylingOptions = "header_background = \"#F0F8FF\", table_border = T, column_align = \"c\",  more_options = c(bootstrap_options = c(\"striped\", \"condensed\", \"responsive\"), table_position = \"left\", full_width = T, html_font = \"Cambria\", fixed_thead = list(enabled = T, background = \"#F0F8FF\"))"
						
						# Default Table Styling and Theme 
						table_border = FALSE
						column_align = "r"
						#header_background_color = "#bdbdbd" # F0F8FF #light blue color bdbdbd light gray color 
						default_table_styling_options = c("table_border = F, column_align = r, more_options = c(bootstrap_options = c(\"striped\", \"condensed\", \"responsive\"), position = \"left\", full_width = T, html_font = \"Cambria\", fixed_thead = list(enabled = T, background = \"#bdbdbd\"))") # bdbdbd F0F8FF
						more_styling_options = c("")
						kable_style = "kable_styling"
						header_background = ""
						
						if(ignoreEnvStyleOverride == FALSE && (exists("uadatasets.sk") && exists("BSkyKableStylingOverride", env=uadatasets.sk) && uadatasets.sk$BSkyKableStylingOverride == TRUE))
						{
							if(exists("BSkykableStyleTheme", env=uadatasets.sk))
							{
								kableStyleTheme = uadatasets.sk$BSkykableStyleTheme
								
								if(length(kableStyleTheme) > 0)
								{
									if(doLatexFormatting == TRUE && exists("BSkyKabletableLatexStylingOptions", env=uadatasets.sk))
									{
										styling_param = uadatasets.sk$BSkyKabletableLatexStylingOptions
									}
									else if(exists("BSkyKabletableStylingOptions", env=uadatasets.sk))
									{
										styling_param = uadatasets.sk$BSkyKabletableStylingOptions
									}
									else
									{
										styling_param = c() #should we choose default_table_styling_options
									}
								}
								else
								{
									styling_param = c() 	
									kableStyleTheme = c() 
									
									table_border = FALSE
									column_align = "r"
									# header_background_color = "" #light blue color 
									header_background = ""
								}
							}
							else
							{
								styling_param = c() 	#should we choose default_table_styling_options
								kableStyleTheme = c() 	#should we choose default kable_style
							}
						}
						else
						{
							#tableStylingOptions is the input parameter from this function call 
							styling_param = tableStylingOptions 
							kable_style = kableStyleTheme
						}
						
						if(length(styling_param) > 0 && styling_param !="" && styling_param != " ")
						{	
							# No need - removed this color option from the parameter list header_i.e. background = \"#F0F8FF\", 
							# if(grepl("header_background", styling_param) == TRUE)
							# {
								# header_background_color = strsplit(strsplit(styling_param, split=",")[[1]][1], split="=")[[1]][2]
							# }
							# else
							# {
								# header_background_color = "#F0F8FF"
							# }
							
							if(grepl("table_border", styling_param) == TRUE)
							{
								table_border = trimws(strsplit(strsplit(styling_param, split=",")[[1]][1], split="=")[[1]][2])
								
								if(table_border == "T")
								{
									table_border = TRUE
								}
								else
								{
									table_border = FALSE
								}
							}
							
							if(grepl("column_align", styling_param) == TRUE)
							{
								column_align = trimws(strsplit(strsplit(styling_param, split=",")[[1]][2], split="=")[[1]][2])
							}
							else
							{
								column_align = "r"
							}
							
							if(grepl("header_background", styling_param) == TRUE)
							{
								header_background = trimws(strsplit(strsplit(styling_param, split=",")[[1]][3], split="=")[[1]][2])
								header_background =substr(header_background, 2, nchar(header_background)-1)
							}
							else
							{
								header_background = ""
							}
							
							if(grepl("more_options", styling_param) == TRUE)
							{
								more_styling_options = gsub("(table_border)((\\d)|(\\D))+(more_options = c\\()", "", substr(styling_param, 1, nchar(styling_param)-1))
							}
							else
							{
								more_styling_options = c("")
							}
						}
						
						
						
						# cat("\n\n SK 1-2 \n\n")
						# print(new_table_removed_empty_rows)
						# print(repeated_column_header_found)
						# print(table_border)
						# print(kableStyleTheme)
						# print(more_styling_options)
						# cat("\n-----------------------------------------\n")
						
						# https://rdrr.io/cran/kableExtra/man/kbl.html - kbl() parameter spec 
						# https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#Overview
						# http://latexcolor.com/
						# https://bookdown.org/yihui/rmarkdown-cookbook/kable.html
						# https://getbootstrap.com/docs/3.4/css/#tables
						# https://haozhu233.github.io/kableExtra/awesome_table_in_pdf.pdf
						# https://getbootstrap.com/docs/3.4/css/#tables
						# https://rdrr.io/cran/kableExtra/man/kable_styling.html
						# https://yihui.org/knitr/options/
						# https://github.com/haozhu233/kableExtra/blob/master/docs/awesome_table_in_pdf.Rmd
						# https://medium.com/swlh/the-students-guide-to-latex-markup-what-it-is-and-why-you-want-it-651e723ce0c8
						
						
						if(length(forceColumnAlign) > 0)
						{
							if(length(forceColumnAlign) >= dim(new_table_removed_empty_rows)[2])
							{
								columnAlignmentPositions = forceColumnAlign[1: dim(new_table_removed_empty_rows)[2]]
							}
							else
							{
								columnAlignmentPositions = c(forceColumnAlign, rep(forceColumnAlign[length(forceColumnAlign)], (dim(new_table_removed_empty_rows)[2] - length(forceColumnAlign))))
							}
						}
						else
						{
							columnAlignmentPositions = c(rep(column_align, dim(new_table_removed_empty_rows)[2]))
							columnHeaderAlignmentPositions = columnAlignmentPositions
							
							#BSkySetPvalueDisplaySetting(showActualPValueInOutput = FALSE, pvalueDropAsterisk = FALSE, pValueDisplayColumnHeaderAlignment = "l", pValueDisplayColumnDataAlignment = "l", pValueDisplayLeftPaddingSpace = 20, pValueDisplayPaddingCharacter = "")
								
							if(length(pvalueColumnAlignmentAdjustmentIndices) > 0)
							{
								# Just for test with conditional column formatting 
								# if(dim(new_table_removed_empty_rows)[2] >= 6)
								# {
									# new_table_removed_empty_rows[,6] = cell_spec(new_table_removed_empty_rows[,6], color = "red", align = "l")
								# }
								
								if(length(pvalueDisplaySettings$pValueDisplayColumnHeaderAlignment) > 0 && trimws(pvalueDisplaySettings$pValueDisplayColumnHeaderAlignment) != "")
								{
									columnAlignmentPositions[pvalueColumnAlignmentAdjustmentIndices] = pvalueDisplaySettings$pValueDisplayColumnHeaderAlignment
								}
							
								if(pvalueDisplaySettings$pValueDisplayColumnDataAlignment == "l")
								{
									pvalue_cell_alignment = paste("text-align:","left", sep="")
								}
								else if(pvalueDisplaySettings$pValueDisplayColumnDataAlignment == "c")
								{
									pvalue_cell_alignment = paste("text-align:","center", sep="")
								}
								else if(pvalueDisplaySettings$pValueDisplayColumnDataAlignment == "r")
								{
									pvalue_cell_alignment = paste("text-align:","right", sep="")
								}
								else 
								{
									pvalue_cell_alignment = paste("text-align:","left", sep="")
								}
								
								if(pvalueDisplaySettings$pValueDisplayLeftPaddingSpace > 0 && pvalueDisplaySettings$pValueDisplayColumnDataAlignment == "l")
								{
									pvalue_cell_alignment = paste(pvalue_cell_alignment, "; padding-left: ", pvalueDisplaySettings$pValueDisplayLeftPaddingSpace,"px",";",sep="")
								}
							}
						}
						
						if(repeated_column_header_found == TRUE)
						{
							if(doLatexFormatting == TRUE)
							{
								if(length(header_background) > 0 && trimws(header_background) != "")
								{
									#p = kableExtra::kbl(new_table_removed_empty_rows, format = "latex", booktabs = T, caption = tableCaption, escape = T, align = c(rep(column_align, dim(new_table_removed_empty_rows)[2]))) %>% 
									p = kableExtra::kbl(new_table_removed_empty_rows, format = "latex", booktabs = T, caption = tableCaption, escape = T, align = columnAlignmentPositions) %>% 		
											add_header_above(eval(parse(text= merged_col_top_header)), background = header_background, align= merged_col_header_alignment) #, background = eval(parse(text=header_background_color))) 
								}
								else
								{
									#p = kableExtra::kbl(new_table_removed_empty_rows, format = "latex", booktabs = T, caption = tableCaption, escape = T, align = c(rep(column_align, dim(new_table_removed_empty_rows)[2]))) %>% 
									p = kableExtra::kbl(new_table_removed_empty_rows, format = "latex", booktabs = T, caption = tableCaption, escape = T, align = columnAlignmentPositions) %>% 		
											add_header_above(eval(parse(text= merged_col_top_header)), align= merged_col_header_alignment) #, background = eval(parse(text=header_background_color))) 
								}
							}
							else
							{
								if(table_border == TRUE && length(kableStyleTheme) > 0)
								{
									if(length(header_background) > 0 && trimws(header_background) != "")
									{
										#p = kableExtra::kbl(new_table_removed_empty_rows, booktabs = T, table.attr = c("class=\"table, th, tr\" style = \"border:1px solid;\""), caption = tableCaption, escape = T, align = c(rep(column_align, dim(new_table_removed_empty_rows)[2]))) %>%
										p = kableExtra::kbl(new_table_removed_empty_rows, booktabs = T, table.attr = c("class=\"table, th, tr\" style = \"border:1px solid;\""), caption = tableCaption, escape = T, align = columnAlignmentPositions) %>%	
											add_header_above(eval(parse(text= merged_col_top_header)), background = header_background, align= merged_col_header_alignment)  #, background = eval(parse(text=header_background_color))) 
									}
									else
									{
										#p = kableExtra::kbl(new_table_removed_empty_rows, booktabs = T, table.attr = c("class=\"table, th, tr\" style = \"border:1px solid;\""), caption = tableCaption, escape = T, align = c(rep(column_align, dim(new_table_removed_empty_rows)[2]))) %>%
										p = kableExtra::kbl(new_table_removed_empty_rows, booktabs = T, table.attr = c("class=\"table, th, tr\" style = \"border:1px solid;\""), caption = tableCaption, escape = T, align = columnAlignmentPositions) %>%
											add_header_above(eval(parse(text= merged_col_top_header)), align= merged_col_header_alignment)
									}
								}
								else
								{
									if(length(header_background) > 0 && trimws(header_background) != "")
									{
										#p = kableExtra::kbl(new_table_removed_empty_rows, booktabs = T, caption = tableCaption, escape = T, align = c(rep(column_align, dim(new_table_removed_empty_rows)[2]))) %>% 
										p = kableExtra::kbl(new_table_removed_empty_rows, booktabs = T, caption = tableCaption, escape = T, align = columnAlignmentPositions) %>% 
											add_header_above(eval(parse(text= merged_col_top_header)), background = header_background, align= merged_col_header_alignment) #, background = eval(parse(text=header_background_color))) 
									}
									else
									{
										#p = kableExtra::kbl(new_table_removed_empty_rows, booktabs = T, caption = tableCaption, escape = T, align = c(rep(column_align, dim(new_table_removed_empty_rows)[2]))) %>% 
										
										if(length(kableStyleTheme) > 0 && kableStyleTheme == "kable_classic")
										{
											p = kableExtra::kbl(new_table_removed_empty_rows, booktabs = T, caption = tableCaption, escape = T, align = columnAlignmentPositions) %>% 
												add_header_above(eval(parse(text= merged_col_top_header)), align= merged_col_header_alignment, extra_css =" border-bottom:1.5px solid black; ")
										}
										else
										{
											p = kableExtra::kbl(new_table_removed_empty_rows, booktabs = T, caption = tableCaption, escape = T, align = columnAlignmentPositions) %>% 
												add_header_above(eval(parse(text= merged_col_top_header)), align= merged_col_header_alignment)
										}
									}
								}
								
								if(length(forceColumnAlign) == 0 && length(pvalueColumnAlignmentAdjustmentIndices) > 0)
								{
									p = column_spec(p, c(pvalueColumnAlignmentAdjustmentIndices), extra_css= pvalue_cell_alignment, include_thead = FALSE) # TRUE is giving error? 
								}
							}
						}
						else
						{
							if(doLatexFormatting == TRUE)
							{
								if(length(header_background) > 0 && trimws(header_background) != "")
								{
									#p = kableExtra::kbl(new_table_removed_empty_rows, format = "latex", booktabs = T, caption = tableCaption, escape = T, align = c(rep(column_align, dim(new_table_removed_empty_rows)[2]))) %>% 
									p = kableExtra::kbl(new_table_removed_empty_rows, format = "latex", booktabs = T, caption = tableCaption, escape = T, align = columnAlignmentPositions) %>% 
										row_spec(0, background = header_background)	
								}
								else
								{
									#p = kableExtra::kbl(new_table_removed_empty_rows, format = "latex", booktabs = T, caption = tableCaption, escape = T, align = c(rep(column_align, dim(new_table_removed_empty_rows)[2])))
									p = kableExtra::kbl(new_table_removed_empty_rows, format = "latex", booktabs = T, caption = tableCaption, escape = T, align = columnAlignmentPositions)		
								}
							}
							else
							{
								if(table_border == TRUE && length(kableStyleTheme) > 0)
								{
									if(length(header_background) > 0 && trimws(header_background) != "")
									{
										#p = kableExtra::kbl(new_table_removed_empty_rows, booktabs = T, table.attr = c("class=\"table, th, tr\" style = \"border:1px solid;\""), caption = tableCaption, escape = T, align = c(rep(column_align, dim(new_table_removed_empty_rows)[2]))) %>%
										p = kableExtra::kbl(new_table_removed_empty_rows, booktabs = T, table.attr = c("class=\"table, th, tr\" style = \"border:1px solid;\""), caption = tableCaption, escape = T, align = columnAlignmentPositions) %>%
											row_spec(0, background = header_background)
									}
									else
									{
										#p = kableExtra::kbl(new_table_removed_empty_rows, booktabs = T, table.attr = c("class=\"table, th, tr\" style = \"border:1px solid;\""), caption = tableCaption, escape = T, align = c(rep(column_align, dim(new_table_removed_empty_rows)[2])))
										p = kableExtra::kbl(new_table_removed_empty_rows, booktabs = T, table.attr = c("class=\"table, th, tr\" style = \"border:1px solid;\""), caption = tableCaption, escape = T, align = columnAlignmentPositions)
									}
								}		
								else
								{
									if(length(header_background) > 0 && trimws(header_background) != "")
									{
										#p = kableExtra::kbl(new_table_removed_empty_rows, booktabs = T, caption = tableCaption, escape = T, align = c(rep(column_align, dim(new_table_removed_empty_rows)[2]))) %>%
										p = kableExtra::kbl(new_table_removed_empty_rows, booktabs = T, caption = tableCaption, escape = T, align = columnAlignmentPositions) %>%
										row_spec(0, background = header_background) 
									}
									else
									{
										#new_table_removed_empty_rows[0,4] = cell_spec(new_table_removed_empty_rows[0,4], extra_css= "padding-left: 60px")
										#p = kableExtra::kbl(new_table_removed_empty_rows, booktabs = T, caption = tableCaption, escape = T, align = c(rep(column_align, dim(new_table_removed_empty_rows)[2])))
										p = kableExtra::kbl(new_table_removed_empty_rows, booktabs = T, caption = tableCaption, escape = T, align = columnAlignmentPositions)
									}
								}
								
								# p value columns are left justified only if asterisks are involved in the data cell to display nicely 
								
								if(length(forceColumnAlign) == 0 && length(pvalueColumnAlignmentAdjustmentIndices) > 0)
								{
									p = column_spec(p, c(pvalueColumnAlignmentAdjustmentIndices), extra_css= pvalue_cell_alignment, include_thead = FALSE) # TRUE is giving error?
									
									# Since Kable package has an defect now that include_thead = TRUE gives an error for including the column header, 
									# the following logic is needed to handle the header position correctly with the left padding space as the cell data
									# Otherwise pvalue column header is fully left justified without any left padding space - it displays bad on the HTML output
									
									if(length(pvalueDisplaySettings$pValueDisplayColumnHeaderAlignment) > 0 && trimws(pvalueDisplaySettings$pValueDisplayColumnHeaderAlignment) == "l")
									{
										if(pvalueDisplaySettings$pValueHeaderLeftPaddingOffsetSpace >= 0)
										{
											headerPaddingOffset = pvalueDisplaySettings$pValueHeaderLeftPaddingOffsetSpace
										}
										else
										{
											headerPaddingOffset = 0
										}
										
										pvalue_header_alignment = paste("padding-left: ", pvalueDisplaySettings$pValueDisplayLeftPaddingSpace + headerPaddingOffset,"px",";",sep="")
										p = row_spec(p, 0, extra_css = pvalue_header_alignment)
									}
									
								}
							}
						}
						
						
						if(length(kableStyleTheme) > 0 && kableStyleTheme != "" && kableStyleTheme != " ")
						{
							#styling_text = paste(kable_style, "(p,  bootstrap_options = c(\"striped\", \"condensed\", \"responsive\"), position = \"left\", full_width = T, html_font = \"Cambria\", fixed_thead = list(enabled = T, background = \"#F0F8FF\"))")
							styling_text = paste(kableStyleTheme, "(p,", more_styling_options, ")")
							
							p = eval(parse(text=styling_text))
						}
					
						#########################################
						# Add the footnote to the table if needed 
						#########################################
						
						if(length(footnote_string) > 0)
						{
							p = p %>% footnote(general = footnote_string, threeparttable = FALSE) #, general_title = footnote_string_names)
							
							# remove the colspan = 100% to colspan = num of column to avoid JS copy to clip board failing to copy footnote
							# format from web browser page to copy/paste to Excel
							
							# y = gregexpr("(?s)+(<tfoot>)(.*)(</tfoot>)", myhtml, ignore.case=TRUE, perl=TRUE) #DOTALL modifier (?s) so that . could match line breaks
							myhtml = p
							
							tfoot_substr_pos = gregexpr("(<tfoot>)[\\S\\s]*(</tfoot>)", myhtml, ignore.case=TRUE, perl=TRUE)

							if(tfoot_substr_pos[[1]] > 0)
							{
								num_col = dim(new_table_removed_empty_rows)[2]
								colspan_sub_str = paste("colspan=\"", num_col, "\"", sep="")
								tfoot_substr = substr(myhtml, tfoot_substr_pos[[1]][1], (tfoot_substr_pos[[1]][1] + attr(tfoot_substr_pos[[1]], "match.length")-1))
								tfoot_substr = gsub("colspan=\"100%\"", colspan_sub_str, tfoot_substr)
								myhtml = paste(substr(myhtml, 1, tfoot_substr_pos[[1]][1] - 1), tfoot_substr, substr(myhtml, (tfoot_substr_pos[[1]][1] + attr(tfoot_substr_pos[[1]], "match.length")), nchar(myhtml)))
							}
							
							p = myhtml
						}
						
						
						########################################################################################
						# remove the column header with all empty (" ") values from the generated HTML and LaTeX 
						########################################################################################
						
						# http://uc-r.github.io/regex
						# https://stat.ethz.ch/R-manual/R-devel/library/base/html/grep.html
						# p =  gsub("<tr>\\D+ (<th\\s+style=\"text((\\D)|(\\d))+>\\s+</th>)+ \\D+</tr>", "", p)
						# x1 = regexpr ("<tr>\\D+ (<th\\s+style=\"text((\\D)|(\\d))+>\\s+</th>)+ \\D+</tr>\\D+</thead>", p)
						# str(x1)
						# y = substr(p, 542, (542+1926))
						# cat(y)
						# nchar(as.character(p))
						
						if(repeated_column_header_found == TRUE || empty_header_table_found == TRUE)
						{
							if(doLatexFormatting == TRUE)
							{
								# x = grepl("cmidrule(\\D|\\d)+\\&\\D+\\midrule", p)
								# y = regexpr("cmidrule(\\D|\\d)+\\&\\D+\\midrule", p)
								# cat("\n\n SK - 7\n\n")
								# print(x)
								# print(y)
								# cat("\n===========================\n")
								
								p =  gsub("cmidrule(\\D|\\d)+\\&\\D+\\midrule","\\midrule", p)
							}
							else
							{
								p =  gsub("<tr>\\D+(<th\\s+style=\"text((\\D)|(\\d))+>\\s+</th>)+\\D+</tr>\\D+</thead>", "</thead>", p)
							}
						}
						
						
						# if(sig_footnoteNeeded == TRUE)
						# {
							# # https://stackoverflow.com/questions/58302255/remove-bordering-from-tfoot-element
							# #p = footnote(p, general = "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
							# #p = paste(p, "\n<p>", sigFootnote, "</p>","\n<br>")
							# if(doLatexFormatting == FALSE)
							# {
								# p = paste(p, "<p>", sigFootnote, "</p>")
								# #p = paste(p, "<br>")
							# }
							# else
							# {
								# p = paste(p, sigFootnote)
								# #p = paste(p, "\\\\", "\\", sigFootnote)
							# }
							
						# }
					}
					
					if(doRmarkdownFormatting == TRUE && table_already_printed_to_sync_file == FALSE)
					{	
						cat(p)
						
						if(doLatexFormatting == FALSE)
						{
							cat("\n\n<!-- -->\n\n")
						}
						else
						{
							cat("\n\n")
						}
					}
					
					# if(doRmarkdownFormatting == FALSE && doLatexFormatting == FALSE && length(kableStyleTheme) > 0 && kableStyleTheme == "kable_classic")
					# {
						# p = paste("<div class=\"bskyAPADiv\">\n", p, "\n</div>\n")
					# }

					BSkyFormat_output$tables[[i]] = c(p)
				}
				else
				{
					if(doTextFormatting == FALSE)
					{
						BSkyFormat_output$tables[[i]] = new_table_removed_empty_rows
					}
					else
					{
						#print(class(new_table_removed_empty_rows))
						cat("\n")
						tableCaption1 = paste(trimws(names(BSkyFormat_output$tables[i])))
						
						if(textTableFormat == "rprint")
						{
							cat(tableCaption1)
							cat("\n")
							cat(paste(rep("-", nchar(tableCaption1)), collapse=""))
							cat("\n")
							
							# # print(new_table_removed_empty_rows)
							# # cat("\n")
							new_table_removed_empty_rows_df = as.data.frame((new_table_removed_empty_rows))
							names(new_table_removed_empty_rows_df) = dimnames(new_table_removed_empty_rows)[[2]]
							# #formals(print.data.frame)$row.names <- FALSE
							# #print.data.frame(row.names = FALSE)
							
							print(new_table_removed_empty_rows_df, row.names = FALSE)
							cat("\n")
						}
						else
						{
							if(!(textTableFormat %in% c("simple", "pipe", "rst")))
							{
								textTableFormat = "simple" 
							}
							#abc = knitr::kable(new_table_removed_empty_rows, caption = tableCaption1, align = 'r', format="pipe")
							textTableOutput = kableExtra::kbl(new_table_removed_empty_rows, caption = tableCaption1, align = 'r', format= textTableFormat)
							print(textTableOutput)
							cat("\n")
						}
					}
				}
				
				# cat("\n\n SK 1-3 \n\n")
				# print(sig_footnoteNeeded)
				# print(repeated_column_header_found)
				# print(doKableFormatting)
				# print(BSkyFormat_output$tables[[i]])
				# cat("\n-----------------------------------------\n")
				
				# if(sig_footnoteNeeded == TRUE)
				# {
					# attr(BSkyFormat_output$tables[[i]], "BSkyFootnote_sig") = sigFootnote
				# }
				
				# if(length(perTableFooter) > 0 & i <= length(perTableFooter))
				# {
					# if(perTableFooter[i] != ".")
					# {
						# footnote_attr_name = paste("BSkyFootnote_perTableFooter_no_", i, sep="")
						# attr(BSkyFormat_output$tables[[i]], footnote_attr_name) = perTableFooter[i] 
					# }
				# }
				
				# if(length(repeatAllTableFooter) > 0)
				# {
					# for(n in 1:length(repeatAllTableFooter))
					# {
						# footnote_attr_name = paste("BSkyFootnote_repeatAllTableFooter_no_", n, sep="")
						# attr(BSkyFormat_output$tables[[i]], footnote_attr_name) = repeatAllTableFooter[n] 
					# }
				# }
				
				###################################################
				# Add the footnote to the table attribute if needed 
				###################################################
				
				if(length(footnote_string) > 0)
				{
					attr(BSkyFormat_output$tables[[i]], "BSkyFootnote_Combined") = footnote_string
					
					if(doTextFormatting == TRUE)
					{
						cat("Note:\n")
						#cat("-----\n")
						
						for(foot_note_item in 1:length(footnote_string))
						{
							#print(footnote_string, row.names = FALSE)
							#print(class(footnote_string[foot_note_item]))
							#print(footnote_string[foot_note_item], row.names = FALSE)
							cat(footnote_string[[foot_note_item]])
							cat("\n")
						}
					}
				}
				
				if(repeated_column_header_found == TRUE)
				{			
					attr(BSkyFormat_output$tables[[i]], "premerge_column_names") = pre_merge_column_names
					
					attr(BSkyFormat_output$tables[[i]], "colspan_column_names") = trimws(gsub("(\")+|(c\\()+|(\\))+","",merged_col_top_header))
					
					if(doKableFormatting == FALSE && doTextFormatting == FALSE)
					{
						dimnames(BSkyFormat_output$tables[[i]])[[2]] = pre_merge_column_names
					}
					
					# y = trimws(gsub("(\")+|(c\\()+|(\\))+","",merged_col_top_header))
					# y1 = unlist(strsplit(y, ","))
					# y1[1]
				}
		}
		
		#Print (to sync) file all the BSky and R error and warning msgs captured in the BSky return structure reurned from BSkyFormat2() 
		if(doKableFormatting == TRUE && doLatexFormatting == FALSE)
		{
			last_table_index = length(BSkyFormat_output$tables)
			if(BSkyFormat_output$tables[[last_table_index]]$nometadatatables > 0)
			{
				for(ewtablemsgs in 1: BSkyFormat_output$tables[[last_table_index]]$nometadatatables)
				{
					if(("BSkyMsg") %in%  names(BSkyFormat_output$tables[[last_table_index]]$metadatatable[[ewtablemsgs]]))
					{
						if(doRmarkdownFormatting == TRUE)
						{
							BSkyFormat_output$tables[[last_table_index]]$metadatatable[[ewtablemsgs]]$BSkyMsg = gsub("\\n", "<br>", BSkyFormat_output$tables[[last_table_index]]$metadatatable[[ewtablemsgs]]$BSkyMsg)
							#p = paste("<p>", unfiltered_table[1,1], "</p>")
						}
						
						writeLines(as.character(BSkyFormat_output$tables[[last_table_index]]$metadatatable[[ewtablemsgs]]$BSkyMsg))
						#cat("\n")
					}
					
					if(doRmarkdownFormatting == TRUE)
					{
						cat("<br>")
					}
					
					if(("RMsg") %in%  names(BSkyFormat_output$tables[[last_table_index]]$metadatatable[[ewtablemsgs]]))
					{
						if(doRmarkdownFormatting == TRUE)
						{
							BSkyFormat_output$tables[[last_table_index]]$metadatatable[[ewtablemsgs]]$RMsg = gsub("\\n", "<br>", BSkyFormat_output$tables[[last_table_index]]$metadatatable[[ewtablemsgs]]$RMsg)
							#p = paste("<p>", unfiltered_table[1,1], "</p>")
						}
						
						writeLines(as.character(BSkyFormat_output$tables[[last_table_index]]$metadatatable[[ewtablemsgs]]$RMsg))
						#cat("\n")
					}
				}
			}
		}
		
		if(exists("uadatasets.sk") && exists("holdBSkyFormatObjectNew", env=uadatasets.sk) && !is.null(uadatasets.sk$holdBSkyFormatObjectNew))
		{
			latest_bsky_obj_index = length(uadatasets.sk$holdBSkyFormatObjectNew)
			BSkyFormat_output$nooftables = length(BSkyFormat_output$tables)
			uadatasets.sk$holdBSkyFormatObjectNew[[latest_bsky_obj_index]] = list(list(type=c("BSkyFormat"), object = BSkyFormat_output))
		}
		
		if(exists("uadatasets.sk") && exists("holdBSkyFormatObject", env=uadatasets.sk) && !is.null(uadatasets.sk$holdBSkyFormatObject))
		{
			latest_bsky_obj_index = length(uadatasets.sk$holdBSkyFormatObject)
			BSkyFormat_output$nooftables = length(BSkyFormat_output$tables)
			uadatasets.sk$holdBSkyFormatObject[[latest_bsky_obj_index]] = BSkyFormat_output ## fix to clean up - unintentionally this became list within list -> list(BSkyFormat_output)
		}
		
		
		if(getNonRenderedTables == TRUE && doKableFormatting == TRUE)
		{
			return(invisible(BSkyFormat_output$tables[1:(BSkyFormat_output$nooftables -1)]))
		}
		else
		{
			return(invisible(BSkyFormat_output))
		}
	}
	else
	{
		# No table to process except check whether there is anything in the BSky error/warning table in the else condition beofre returning
		# Print (to sync) file all the BSky and R error and warning msgs captured in the BSky return structure reurned from BSkyFormat2() 
		if(doKableFormatting == TRUE && doLatexFormatting == FALSE)
		{
			last_table_index = length(BSkyFormat_output$tables)
			if(BSkyFormat_output$tables[[last_table_index]]$nometadatatables > 0)
			{
				for(ewtablemsgs in 1: BSkyFormat_output$tables[[last_table_index]]$nometadatatables)
				{
					if(("BSkyMsg") %in%  names(BSkyFormat_output$tables[[last_table_index]]$metadatatable[[ewtablemsgs]]))
					{
						if(doRmarkdownFormatting == TRUE)
						{
							BSkyFormat_output$tables[[last_table_index]]$metadatatable[[ewtablemsgs]]$BSkyMsg = gsub("\\n", "<br>", BSkyFormat_output$tables[[last_table_index]]$metadatatable[[ewtablemsgs]]$BSkyMsg)
							#p = paste("<p>", unfiltered_table[1,1], "</p>")
						}
						writeLines(as.character(BSkyFormat_output$tables[[last_table_index]]$metadatatable[[ewtablemsgs]]$BSkyMsg))
						#cat("\n")
					}
					
					if(doRmarkdownFormatting == TRUE)
					{
						cat("<br>")
					}
					
					if(("RMsg") %in%  names(BSkyFormat_output$tables[[last_table_index]]$metadatatable[[ewtablemsgs]]))
					{
						if(doRmarkdownFormatting == TRUE)
						{
							BSkyFormat_output$tables[[last_table_index]]$metadatatable[[ewtablemsgs]]$RMsg = gsub("\\n", "<br>", BSkyFormat_output$tables[[last_table_index]]$metadatatable[[ewtablemsgs]]$RMsg)
							#p = paste("<p>", unfiltered_table[1,1], "</p>")
						}
						writeLines(as.character(BSkyFormat_output$tables[[last_table_index]]$metadatatable[[ewtablemsgs]]$RMsg))
						#cat("\n")
					}
				}
			}
		}
		
		return(invisible(BSkyFormat_output))
	}
}


#08Oct2021
BSkyIsRmarkdownOutputOn <- function()
{
	
	if(exists("uadatasets.sk") && exists("BSkyKableFormatting", env=uadatasets.sk) && uadatasets.sk$BSkyKableFormatting == FALSE)
	{
		doKableFormatting = FALSE
		doRmarkdownFormatting = FALSE
	}
	else
	{
		doKableFormatting = TRUE
		
		if(exists("uadatasets.sk") && exists("BSkyRmarkdownFormatting", env=uadatasets.sk) && uadatasets.sk$BSkyRmarkdownFormatting == FALSE)
		{
			doRmarkdownFormatting = FALSE
		}
		else
		{
			doRmarkdownFormatting = TRUE
		}
		
		# if(isLatexOutput == TRUE || (exists("uadatasets.sk") && exists("BSkyLatexFormatting", env=uadatasets.sk) && uadatasets.sk$BSkyLatexFormatting == TRUE))
		# {
			# doLatexFormatting = TRUE
		# }
		# else
		# {
			# doLatexFormatting = FALSE
		# }
	}
	
	if(exists("uadatasets.sk") && !exists("BSkyTextFormatting", env=uadatasets.sk))
	{
		doRmarkdownFormatting = TRUE
	}
	else if(exists("uadatasets.sk") && exists("BSkyTextFormatting", env=uadatasets.sk) && uadatasets.sk$BSkyTextFormatting == TRUE)
	{
		doRmarkdownFormatting = TRUE
	}
	
	# print("in BSkyIsRmarkdownOutputOn()")
	# print(BSkyGetKableAndRmarkdownFormatting())
	# print(list(doRmarkdownFormatting = doRmarkdownFormatting))
	
	return(invisible(doRmarkdownFormatting))
}


##08Oct2021
BSky.print.text <- function(textFormat = "simple")
{
	BSkySetKableAndRmarkdownFormatting (BSkyKableFormatting = FALSE, BSkyRmarkdownFormatting = FALSE, BSkyLaTeXFormatting = FALSE, BSkyTextFormatting = TRUE)
	BSkySetTextTableFormat (textTableFormat = textFormat)
}

# #08Oct2021
BSky.print.html <- function(app = "rmarkdown", html_style = "APA")
{
	if(app == "rmarkdown")
	{
		BSkySetKableAndRmarkdownFormatting (BSkyKableFormatting = TRUE, BSkyRmarkdownFormatting = TRUE, BSkyLaTeXFormatting = FALSE, BSkyTextFormatting = FALSE)
	}
	else
	{
		BSkySetKableAndRmarkdownFormatting (BSkyKableFormatting = TRUE, BSkyRmarkdownFormatting = FALSE, BSkyLaTeXFormatting = FALSE, BSkyTextFormatting = FALSE)
	}
	
	BSkySetHtmlStylingSetting()

	if(html_style == "APA")
	{
		BSkySetHtmlStylingSetting(tableTheme = "kable_classic", fontFamily = "Helvetica", tableHeaderBackgroundColor = "", columHeaderScrollFixed = FALSE, fullWidthDisplay = FALSE)
	}
	else
	{
		BSkySetHtmlStylingSetting(fontFamily = "Helvetica", fullWidthDisplay = FALSE, tableHeaderBackgroundColor = "#B2BEB5")
	}
}

# #08Oct2021
BSky.print.latex <- function()
{
	BSkySetKableAndRmarkdownFormatting (BSkyKableFormatting = TRUE, BSkyRmarkdownFormatting = TRUE, BSkyLaTeXFormatting = TRUE, BSkyTextFormatting = FALSE)
	
	BSkySetHtmlStylingSetting()
	BSkySetHtmlStylingSetting(tableTheme = "kable_styling", latex_hold_position = FALSE, latex_scale_down = TRUE, fullWidthDisplay = FALSE, tableHeaderBackgroundColor = "", striped = FALSE)
}


##21May2021
BSkyGetTableDisplayLimits <- function()
{
	if(exists("tableDisplayLimits", env=uadatasets.sk))
	{
		# Defaults
		# maxOutputTables = 99
		# maxRowLimit = 2000
		# maxColLimit = 99
		return(invisible(uadatasets.sk$tableDisplayLimits))
	}
	else
	{
		return(invisible(NULL))
	}
}

##21May2021
BSkySetTableDisplayLimits <- function(maxOutputTables = 99, maxRowLimit = 2000, maxColLimit = 99)
{
		uadatasets.sk$tableDisplayLimits = list(maxOutputTables = maxOutputTables, maxRowLimit = maxRowLimit, maxColLimit = maxColLimit)
		return(invisible(uadatasets.sk$tableDisplayLimits))
}


##08Oct2021
BSkySetKableAndRmarkdownFormatting <- function(BSkyKableFormatting = FALSE, BSkyRmarkdownFormatting = FALSE, BSkyLaTeXFormatting = FALSE, BSkyTextFormatting = TRUE)
{
	if(exists("uadatasets.sk"))
	{
		uadatasets.sk$BSkyKableFormatting = BSkyKableFormatting
		uadatasets.sk$BSkyRmarkdownFormatting = BSkyRmarkdownFormatting
		uadatasets.sk$BSkyLatexFormatting = BSkyLaTeXFormatting
		uadatasets.sk$BSkyTextFormatting = BSkyTextFormatting
		
		return (invisible(list(uadatasets.sk$BSkyKableFormatting, uadatasets.sk$BSkyRmarkdownFormatting, uadatasets.sk$BSkyLatexFormatting, uadatasets.sk$BSkyTextFormatting)))
	}
	else
	{
		return(invisible(NULL))
	}
}

#08Oct2021
BSkyGetKableAndRmarkdownFormatting <- function()
{
	if(exists("uadatasets.sk") && !exists("BSkyTextFormatting", env=uadatasets.sk))
	{
		doKableFormatting = FALSE
		doRmarkdownFormatting = FALSE
		doLatexFormatting = FALSE
		doTextFormatting = TRUE
	}
	else if(exists("uadatasets.sk") && exists("BSkyTextFormatting", env=uadatasets.sk) && uadatasets.sk$BSkyTextFormatting == TRUE)
	{
		doKableFormatting = FALSE
		doRmarkdownFormatting = FALSE
		doLatexFormatting = FALSE
		doTextFormatting = TRUE
	}
	else if(exists("uadatasets.sk") && exists("BSkyKableFormatting", env=uadatasets.sk) && uadatasets.sk$BSkyKableFormatting == FALSE)
	{
		doKableFormatting = FALSE
		doRmarkdownFormatting = FALSE
		doLatexFormatting = FALSE
		doTextFormatting = FALSE
	}
	else
	{
		doKableFormatting = TRUE
		doTextFormatting = FALSE
		
		if(exists("uadatasets.sk") && exists("BSkyRmarkdownFormatting", env=uadatasets.sk) && uadatasets.sk$BSkyRmarkdownFormatting == FALSE)
		{
			doRmarkdownFormatting = FALSE
		}
		else
		{
			doRmarkdownFormatting = TRUE
		}
		
		if(exists("uadatasets.sk") && exists("BSkyLatexFormatting", env=uadatasets.sk) && uadatasets.sk$BSkyLatexFormatting == TRUE)
		{
			doLatexFormatting = TRUE
		}
		else
		{
			doLatexFormatting = FALSE
		}
		
	}
	
	return(invisible(list(doKableFormatting = doKableFormatting, doRmarkdownFormatting = doRmarkdownFormatting, doLatexFormatting = doLatexFormatting, doTextFormatting = doTextFormatting)))
}


##08Oct2021
BSkySetHtmlStylingSetting <- function(tableTheme = "kable_styling", fontFamily = "Helvetica", tableHeaderBackgroundColor = "#bdbdbd", ColumnAlign = "Right", tablePosition = "Left", fullWidthDisplay = TRUE, tableOuterBorder = FALSE, tableGridLines = FALSE, striped = TRUE, hover = TRUE, latex_scale_down = FALSE, latex_hold_position = FALSE, columHeaderScrollFixed = TRUE, fontSize = 0, overrideStylingSettings = TRUE)
{
	function_name = "BSkySetHtmlStylingSetting"
	
	if(length((as.list(match.call()))) == 1)
	{
		globalOptionReset = TRUE
	}
	else
	{
		globalOptionReset = FALSE
		
		priorOptions = strsplit(uadatasets.sk$BSkyKabletableStylingOptions, ",")
		
		prior_tableOuterBorder = trimws(priorOptions[[1]][1])
		
		 # [[1]]
		 # [1] " table_border = F "                                    " column_align = r "                                   
		 # [3] " more_options = c(bootstrap_options = c(\"condensed\"" " \"responsive\"  "                                    
		 # [5] " \"striped\"  "                                        " \"hover\" ) "                                        
		 # [7] " position = \"left\" "                                 " full_width = T "                                     
		 # [9] " html_font = \"Helvetica\" "                           " font_size = 10 "                                     
		 # [11] " fixed_thead = list(enabled = F "                      " background = \"#F0F8FF\" ) )"
	}
	
	if(exists("uadatasets.sk"))
	{
		if(globalOptionReset == FALSE && !missing(overrideStylingSettings))
		{
			if(overrideStylingSettings == TRUE)
			{
				uadatasets.sk$BSkyKableStylingOverride = TRUE
			}
			else
			{
				uadatasets.sk$BSkyKableStylingOverride = FALSE
			}
		}
		else
		{
			if(globalOptionReset == TRUE)
			{
				uadatasets.sk$BSkyKableStylingOverride = formals(function_name)$overrideStylingSettings
			}
		}
		
		
		if(globalOptionReset == FALSE && !missing(tableTheme))
		{
			tableTheme = tolower(tableTheme)
			
			if(length(tableTheme) > 0 && (tableTheme %in% c("kable_styling", "kable_classic", "kable_classic_2", "kable_styling", "kable_minimal", "kable_paper", "kable_material", "kable_material_dark")))
			{
				uadatasets.sk$BSkykableStyleTheme = tableTheme
			}
			else
			{
				if( length(tableTheme) == 0)
				{ 
					uadatasets.sk$BSkykableStyleTheme = c()
				}
				else
				{
					uadatasets.sk$BSkykableStyleTheme = "kable_styling"
				}
			}
		}
		else
		{
			if(globalOptionReset == TRUE)
			{
				uadatasets.sk$BSkykableStyleTheme = formals(function_name)$tableTheme
			}
		}
		
		uadatasets.sk$BSkyKabletableStylingOptions = c()
		uadatasets.sk$BSkyKabletableLatexStylingOptions = c()
		
		
		if(tableOuterBorder == TRUE && length(tableTheme)> 0 && tableTheme == "kable_styling")
		{
			uadatasets.sk$BSkyKabletableStylingOptions = paste(uadatasets.sk$BSkyKabletableStylingOptions, "table_border = T")
		}
		else
		{
			uadatasets.sk$BSkyKabletableStylingOptions = paste(uadatasets.sk$BSkyKabletableStylingOptions, "table_border = F")
		}
		
		uadatasets.sk$BSkyKabletableLatexStylingOptions = paste(uadatasets.sk$BSkyKabletableLatexStylingOptions, "table_border = F")
		
		ColumnAlign = tolower(ColumnAlign)
		
		if(ColumnAlign == "center")
		{
			uadatasets.sk$BSkyKabletableStylingOptions = paste(uadatasets.sk$BSkyKabletableStylingOptions, "," , "column_align = c")
			uadatasets.sk$BSkyKabletableLatexStylingOptions = paste(uadatasets.sk$BSkyKabletableLatexStylingOptions, "," , "column_align = c")
		}
		else if(ColumnAlign == "left")
		{
			uadatasets.sk$BSkyKabletableStylingOptions = paste(uadatasets.sk$BSkyKabletableStylingOptions, "," , "column_align = l")
			uadatasets.sk$BSkyKabletableLatexStylingOptions = paste(uadatasets.sk$BSkyKabletableLatexStylingOptions, "," , "column_align = l")
		}
		else
		{
			uadatasets.sk$BSkyKabletableStylingOptions = paste(uadatasets.sk$BSkyKabletableStylingOptions, "," , "column_align = r")
			uadatasets.sk$BSkyKabletableLatexStylingOptions = paste(uadatasets.sk$BSkyKabletableLatexStylingOptions, "," , "column_align = r")
		}
		
		tableHeaderBackgroundColor = toupper(tableHeaderBackgroundColor)
		
		if(nchar(tableHeaderBackgroundColor) != 7 && substr(tableHeaderBackgroundColor,1,1) != "#")
		{
			tableHeaderBackgroundColor = paste("\"", "\"", sep = "")
		}
		else
		{
			tableHeaderBackgroundColor = paste("\"", tableHeaderBackgroundColor, "\"", sep = "")
		}
		
		uadatasets.sk$BSkyKabletableStylingOptions = paste(uadatasets.sk$BSkyKabletableStylingOptions, "," , "header_background =", tableHeaderBackgroundColor)
		uadatasets.sk$BSkyKabletableLatexStylingOptions = paste(uadatasets.sk$BSkyKabletableLatexStylingOptions, "," , "header_background =", tableHeaderBackgroundColor)
		
		
		if(striped == TRUE)
		{
			striped = ", \"striped\""
		}
		else
		{
			striped = c()
		}
		
		if(tableGridLines == TRUE)
		{
			tableGridLines = ", \"bordered\""
		}
		else
		{
			tableGridLines = c()
		}
		
		if(hover == TRUE)
		{
			hover = ", \"hover\""
		}
		else
		{
			hover = c() 
		}
		
		# Latex only option "scale_down", hold_position, "repeat_header"
		if(latex_scale_down == TRUE)
		{
			latex_scale_down = ", \"scale_down\""
		}
		else
		{
			latex_scale_down = c() 
		}
		
		if(latex_hold_position == TRUE)
		{
			latex_hold_position = ", \"hold_position\""
		}
		else
		{
			latex_hold_position = c() 
		}
		
		uadatasets.sk$BSkyKabletableStylingOptions = paste(uadatasets.sk$BSkyKabletableStylingOptions, "," , "more_options = c(bootstrap_options = c(\"condensed\", \"responsive\" ")
		
		#uadatasets.sk$BSkyKabletableStylingOptions = paste(uadatasets.sk$BSkyKabletableStylingOptions, "," , "more_options = c(bootstrap_options = c(\"responsive\" ")
		
		if(length(striped) > 0 || length(tableGridLines) > 0 || length(hover) > 0)
		{
			uadatasets.sk$BSkyKabletableStylingOptions = paste(uadatasets.sk$BSkyKabletableStylingOptions, striped, tableGridLines, hover, ")" )
		}
		else
		{
			uadatasets.sk$BSkyKabletableStylingOptions = paste(uadatasets.sk$BSkyKabletableStylingOptions, ")")
		}
		
		if(length(striped) > 0)
		{
			uadatasets.sk$BSkyKabletableLatexStylingOptions = paste(uadatasets.sk$BSkyKabletableLatexStylingOptions, "," , "more_options = c(latex_options = c(\"striped\"", latex_scale_down, latex_hold_position, ",\"repeat_header\")" )
		}
		else if(length(latex_scale_down) > 0)
		{
			uadatasets.sk$BSkyKabletableLatexStylingOptions = paste(uadatasets.sk$BSkyKabletableLatexStylingOptions, "," , "more_options = c(latex_options = c(\"scale_down\"", latex_hold_position, ",\"repeat_header\")" )
		}
		else if(length(latex_hold_position) > 0)
		{
			uadatasets.sk$BSkyKabletableLatexStylingOptions = paste(uadatasets.sk$BSkyKabletableLatexStylingOptions, "," , "more_options = c(latex_options = c(\"hold_position\"", ",\"repeat_header\")" )
		}
		else
		{
			uadatasets.sk$BSkyKabletableLatexStylingOptions = paste(uadatasets.sk$BSkyKabletableLatexStylingOptions, "," , "more_options = c(latex_options = c(\"repeat_header\")" )
		}
		
		tablePosition = tolower(tablePosition)
		
		if(!(tablePosition %in% c("right", "float_right", "left", "float_left", "center", "float_center")))
		{
			tablePosition = "\"left\"" 
		}
		else
		{
			tablePosition = paste("\"", tablePosition, "\"", sep="")
		}
		
		uadatasets.sk$BSkyKabletableStylingOptions = paste(uadatasets.sk$BSkyKabletableStylingOptions, ",", "position =", tablePosition)
		uadatasets.sk$BSkyKabletableLatexStylingOptions = paste(uadatasets.sk$BSkyKabletableLatexStylingOptions, ",", "position =", tablePosition)
		
		
		if(fullWidthDisplay == TRUE)
		{
			uadatasets.sk$BSkyKabletableStylingOptions = paste(uadatasets.sk$BSkyKabletableStylingOptions, ",", "full_width = T")
			uadatasets.sk$BSkyKabletableLatexStylingOptions = paste(uadatasets.sk$BSkyKabletableLatexStylingOptions, ",", "full_width = T")
		}
		else
		{
			uadatasets.sk$BSkyKabletableStylingOptions = paste(uadatasets.sk$BSkyKabletableStylingOptions, ",", "full_width = F")
			uadatasets.sk$BSkyKabletableLatexStylingOptions = paste(uadatasets.sk$BSkyKabletableLatexStylingOptions, ",", "full_width = F")
		}
		
		
		if(length(fontFamily) <= 0 )
		{
			fontFamily = "\"Helvetica\""
		}
		else
		{
			fontFamily = paste("\"", fontFamily, "\"", sep="")
		}
		
		uadatasets.sk$BSkyKabletableStylingOptions = paste(uadatasets.sk$BSkyKabletableStylingOptions, "," , "html_font =", fontFamily)
		
		if(fontSize > 0)
		{
			uadatasets.sk$BSkyKabletableStylingOptions = paste(uadatasets.sk$BSkyKabletableStylingOptions, "," , "font_size =", fontSize)
			uadatasets.sk$BSkyKabletableLatexStylingOptions = paste(uadatasets.sk$BSkyKabletableLatexStylingOptions, "," , "font_size =", fontSize)
		}
		
		# uadatasets.sk$BSkyKabletableStylingOptions = c("table_border = T, column_align = r, more_options = c(bootstrap_options = c(\"striped\", \"condensed\", \"responsive\", \"bordered\", \"hover\"), position = \"left\", full_width = T, html_font = \"Cambria\", fixed_thead = list(enabled = T, background = \"#F0F8FF\"))" ) #Background color code examples #FFBF00 or #F0F8FF etc. 
		# tableTheme = "kable_style", 
		# fontFamily = "Helvetica", 
		# tableHeaderBackgroundColor = "#F0F8FF", 
		# ColumnAlign = "Right", 
		# TablePosition = "Left", 
		# fullWidthDisplay = TRUE, 
		# tableOuterBorder = FALSE, 
		# tableGridLines = FALSE, 
		# striped = TRUE, 
		# hover = TRUE, 
		# columHeaderScrollFixed = FALSE, 
		# overrideStylingSettings = TRUE
		
		if(columHeaderScrollFixed == TRUE)
		{
			uadatasets.sk$BSkyKabletableStylingOptions = paste(uadatasets.sk$BSkyKabletableStylingOptions, "," , "fixed_thead = list(enabled = T" )
		}
		else
		{
			uadatasets.sk$BSkyKabletableStylingOptions = paste(uadatasets.sk$BSkyKabletableStylingOptions, "," , "fixed_thead = list(enabled = F" )
		}
		
		
		uadatasets.sk$BSkyKabletableStylingOptions = paste(uadatasets.sk$BSkyKabletableStylingOptions, "," , "background =", tableHeaderBackgroundColor, ")" )
		
		uadatasets.sk$BSkyKabletableStylingOptions = paste(uadatasets.sk$BSkyKabletableStylingOptions, ")")
		uadatasets.sk$BSkyKabletableLatexStylingOptions = paste(uadatasets.sk$BSkyKabletableLatexStylingOptions, ")")
		
		return(invisible(c(kableOverride = uadatasets.sk$BSkyKableStylingOverride, kableTheme = uadatasets.sk$BSkykableStyleTheme, kableThemeStyleOptions = uadatasets.sk$BSkyKabletableStylingOptions, kableThemeLatexStyleOptions = uadatasets.sk$BSkyKabletableLatexStylingOptions)))
	}
	else
	{
		return(invisible(FALSE))
	}
}

##08Oct2021
BSkySetTextTableFormat <- function(textTableFormat = "simple")
{
	if(exists("uadatasets.sk"))
	{
		uadatasets.sk$BSkyKabletableTextTableFormat = textTableFormat
	}
	
	return(invisible(textTableFormat))
}

##08Oct2021
BSkyGetTextTableFormat <- function()
{
	textTableFormat = "simple"
	
	if(exists("uadatasets.sk") && exists("BSkyKabletableTextTableFormat", env=uadatasets.sk))
	{
		textTableFormat = uadatasets.sk$BSkyKabletableTextTableFormat
	}
	
	return(invisible(textTableFormat))
}


##31Mar2021
## fileName = "C:/Users/User/Documents/workfolder/BSky/Rmarkdown/kableoutput1.html"
BSkyWriteKableHtmlOutput <- function(fileName, fileOpenMode = "a", bSkyDebug = 1)
{
	retObjList = BSkyGetHoldFormatObjList(bSkyCompatibility=0, bSkyDebug)
	
	if(is.null(retObjList))
	{
		return(invisible(NULL))
	}
	
	len = length(retObjList)
	
	if(len > 0)
	{
		fileConn = NULL
		
		if(!is.null(fileName) && fileName != "" && file.exists(fileName))
		{
			fileConn<-file(fileName, fileOpenMode)
		}
		
		for(i in 1:len)
		{
			if(retObjList[[i]][[1]]$type == "BSkyFormat")
			{
				num_tables = length(retObjList[[i]][[1]]$object$tables) - 1
				
				if(num_tables > 0)
				{
					for(j in 1:num_tables)
					{
						#write(x, file = "data", ncolumns = if(is.character(x)) 1 else 5, append = FALSE, sep = " ")
						#write(retObjList[[i]][[1]]$object$tables[[j]], file = fileName)
						#sink("outfile.txt")
						#cat("world")
						#sink()
						if(!is.null(fileConn))
						{	
							writeLines("\n\n", fileConn)
							writeLines(retObjList[[i]][[1]]$object$tables[[j]], fileConn)
							writeLines("\n<br/>\n", fileConn)
						}
						else
						{	cat("\n")
							cat(retObjList[[i]][[1]]$object$tables[[j]])
							cat("\n")
						}
					}
				}
			}
		}
		
		# writeLines("\n</body>\n", fileConn)
		# writeLines("\n</html>\n", fileConn)
		if(!is.null(fileConn))
		{
			close(fileConn)
		}
	}

	return(invisible(TRUE))
}

#21May2021. 
BSkyGetPvalueDisplaySetting <- function()
{
	if(!exists("pValueDisplayColumnDataAlignment", env=uadatasets.sk))
	{
		if(!exists("showActualPValueInOutput", env=uadatasets.sk))
		{
			uadatasets.sk$showActualPValueInOutput = FALSE
		}
		uadatasets.sk$pvalueDropAsterisk = FALSE
		uadatasets.sk$pValueDisplayColumnHeaderAlignment = "c"
		uadatasets.sk$pValueDisplayColumnDataAlignment = "l"
		uadatasets.sk$pValueDisplayLeftPaddingSpace = 20
		uadatasets.sk$pValueHeaderLeftPaddingOffsetSpace = 0
		uadatasets.sk$pValueDisplayPaddingCharacter = ""
	}
	
	return(invisible(list(showActualPValueInOutput = uadatasets.sk$showActualPValueInOutput, pvalueDropAsterisk = uadatasets.sk$pvalueDropAsterisk, pValueDisplayColumnHeaderAlignment = uadatasets.sk$pValueDisplayColumnHeaderAlignment, pValueDisplayColumnDataAlignment = uadatasets.sk$pValueDisplayColumnDataAlignment, pValueDisplayLeftPaddingSpace = uadatasets.sk$pValueDisplayLeftPaddingSpace, pValueHeaderLeftPaddingOffsetSpace = uadatasets.sk$pValueHeaderLeftPaddingOffsetSpace, pValueDisplayPaddingCharacter = uadatasets.sk$pValueDisplayPaddingCharacter)))
}




#24Apr2021.
BSkySetPvalueDisplaySetting <- function(showActualPValueInOutput = uadatasets.sk$showActualPValueInOutput, pvalueDropAsterisk = uadatasets.sk$pvalueDropAsterisk, pValueDisplayColumnHeaderAlignment = uadatasets.sk$pValueDisplayColumnHeaderAlignment, pValueDisplayColumnDataAlignment = uadatasets.sk$pValueDisplayColumnDataAlignment, pValueDisplayLeftPaddingSpace = uadatasets.sk$pValueDisplayLeftPaddingSpace, pValueHeaderLeftPaddingOffsetSpace = uadatasets.sk$pValueHeaderLeftPaddingOffsetSpace, pValueDisplayPaddingCharacter = uadatasets.sk$pValueDisplayPaddingCharacter)
{
	if(exists("uadatasets.sk"))
	{
		uadatasets.sk$showActualPValueInOutput = showActualPValueInOutput
		uadatasets.sk$pvalueDropAsterisk = pvalueDropAsterisk
		uadatasets.sk$pValueDisplayColumnHeaderAlignment = pValueDisplayColumnHeaderAlignment
		uadatasets.sk$pValueDisplayColumnDataAlignment = pValueDisplayColumnDataAlignment
		uadatasets.sk$pValueDisplayLeftPaddingSpace = pValueDisplayLeftPaddingSpace
		uadatasets.sk$pValueHeaderLeftPaddingOffsetSpace = pValueHeaderLeftPaddingOffsetSpace
		uadatasets.sk$pValueDisplayPaddingCharacter = pValueDisplayPaddingCharacter		
	}
	
	return(invisible(list(showActualPValueInOutput, pvalueDropAsterisk, pValueDisplayColumnHeaderAlignment, pValueDisplayColumnDataAlignment, pValueDisplayLeftPaddingSpace, pValueHeaderLeftPaddingOffsetSpace, pValueDisplayPaddingCharacter)))
}


#14Jun2021
#https://data.princeton.edu/r/linearmodels
BSkyFormatLmerMod <- function(obj, decimalDigits = BSkyGetDecimalDigitSetting())
{
	if(class(obj)[1] == "lmerMod" || class(obj)[1] == "lmerModLmerTest" || class(obj)[1] == "lme")
	{
		obj = summary(obj)
	}
	
	if("summary.merMod" %in% class(obj))
	{
		#summary_table = data.frame(obj$AICtab, obj$logLik, length(obj$residuals), obj$sigma, t(obj$ngrps))
		#names(summary_table) = c(names(obj$AICtab), "LogLik", "Num Obs", "Sigma", names(obj$ngrps))
		
		if("logLik" %in% names(obj$AICtab))
		{
			summary_table = data.frame(t(obj$AICtab), length(obj$residuals), obj$sigma, t(obj$ngrps))
			names(summary_table) = c(names(obj$AICtab), "Num Obs", "Sigma", names(obj$ngrps))
		}
		else
		{
			summary_table = data.frame(t(obj$AICtab), obj$logLik, length(obj$residuals), obj$sigma, t(obj$ngrps))
			names(summary_table) = c(names(obj$AICtab), "LogLik", "Num Obs", "Sigma", names(obj$ngrps))
		}
		names(summary_table)[1] = "AIC"
        row.names(summary_table) = c()
		
		table_list = list(summary_table)
		table_list_names = c(paste(obj$methTitle, ":", paste(deparse(obj$call), collapse="")))    
		
		table_list_names = BSkyReplaceSplitDatasetName(table_list_names)
		
		names(table_list) = table_list_names
		
		scaled_residual = data.frame(t(unclass(summary(obj$residuals))))
        scaled_residual = scaled_residual[,c(1:3,5:6)]
        names(scaled_residual) = c("Min", "1Q", "Median", "3Q", "Max")
		
        table_list = c(table_list, list(scaled_residual))
		table_list_names = c(table_list_names, "Scaled Residuals")
		names(table_list) = table_list_names
		
		random_effects = formatVC(obj$varcor, digits = decimalDigits, comp = c("Variance", "Std.Dev."), useScale = obj$useScale )
		table_list = c(table_list, list(random_effects))
        table_list_names = c(table_list_names, "Random Effects")

		names(table_list) = table_list_names

		table_list = c(table_list, list(obj$coefficients))
        table_list_names = c(table_list_names, "Fixed Effects") #(Coefficients)

		names(table_list) = table_list_names 
		
		# Correlation matrix not shown by default, as p = 16 > 12.
		# Use print(cl_long3_summary, correlation=TRUE)  or
		# vcov(cl_long3_summary)        if you need it
		
		if(!is.null(obj$vcov) && !is.null(obj$vcov@factors$correlation))
		{
			cor_fixed_effect = as.matrix(obj$vcov@factors$correlation)
			
			if(dim(cor_fixed_effect)[1] != 1 || dim(cor_fixed_effect)[2] != 1)
			{
				table_list = c(table_list, list(cor_fixed_effect))
				table_list_names = c(table_list_names, "Correlation of Fixed Effects")
				names(table_list) = table_list_names 
			}
		}
		 
        
        info1 = paste("optimizer", "(", obj$optinfo$optimizer, ")", "convergence code:", obj$optinfo$control )
		
        additional_info = data.frame(info1, stringsAsFactors = FALSE)
        #dimnames(additional_info)[[2]] = "Optional Information"
        names(additional_info) = "Optional Information"
		
		if(length(obj$optinfo$conv$lme4) > 0)
		{
			additional_info = rbind(additional_info, obj$optinfo$conv$lme4$messages)
		}
		
		if(length(obj$optinfo$message) > 0)
		{
			additional_info = rbind(additional_info, obj$optinfo$message)
		}
		
		if(length(obj$optinfo$warnings) > 0)
		{
			additional_info = rbind(additional_info, obj$optinfo$warnings)
		}
		
		if(!is.null(obj$coefficients) && nrow(obj$coefficients) > 0)
		{
			coeff_row_count = nrow(obj$coefficients)
			cor.max <- getOption("lme4.summary.cor.max")
			
				if (coeff_row_count > cor.max) 
				{
					nam <- deparse(substitute(obj))
					if (length(nam) > 1 || nchar(nam) >= 32) 
					{
					  nam <- "...."
					}
					
					cor_rel_matrix_msg = paste("Correlation matrix not shown by default, as p =", coeff_row_count, ">", cor.max,". ","Run vcov(model) if needed") 
					
					#cor_rel_matrix_msg = sprintf(paste("\nCorrelation matrix not shown by default, as p = %d > %d.", "Use print(%s, correlation=TRUE)  or", "vcov(%s) if you need it\n", sep = "\n"), coeff_row_count, cor.max, nam, nam)
					
					additional_info = rbind(additional_info, cor_rel_matrix_msg)
				}
		}
        
		row.names(additional_info) = c()
		
        table_list = c(table_list, list(additional_info))
        table_list_names = c(table_list_names, "Additional Details")
        names(table_list) = table_list_names

		obj = table_list
	}
	else if("summary.lme" %in% class(obj))
	{
		summary_table = data.frame(obj$AIC, obj$BIC, obj$logLik, obj$dim$N, obj$sigma, obj$dims$ngrps[[1]])
		names(summary_table) = c("AIC", "BIC", "LogLik", "Num Obs", "Sigma", "Num Groups")
        row.names(summary_table) = c()
		table_list = list(summary_table)
		#table_list_names = c(paste(obj$methTitle, ":", paste(deparse(obj$call), collapse=""))) 
		table_list_names = paste(deparse(obj$call), collapse="")
		
		table_list_names = BSkyReplaceSplitDatasetName(table_list_names)
		
		names(table_list) = table_list_names
		
        table_list = c(table_list, list(obj$residuals))
		table_list_names = c(table_list_names, "Standardized Within-Group Residuals")
		names(table_list) = table_list_names

		
		random_effects = data.frame(unclass(VarCorr(obj))) #random effect
		table_list = c(table_list, list(random_effects))
        table_list_names = c(table_list_names, "Random Effects")

		names(table_list) = table_list_names

		table_list = c(table_list, list(obj$tTable))
        table_list_names = c(table_list_names, "Fixed Effects") #(Coefficients)

		names(table_list) = table_list_names 

		if(dim(obj$corFixed)[1] != 1 || dim(obj$corFixed)[2] != 1)
		{
			table_list = c(table_list, list(obj$corFixed))
			table_list_names = c(table_list_names, "Correlation of Fixed Effects")
			names(table_list) = table_list_names 
		}
		
		obj = table_list
	}
	
	return(invisible(obj))
}



#11May2021 formatting of BSkyOneSmTTest() and BSkyIndSmTTest() to eliminate the two xml templates for formatting
#16May2021, 21May2021, 26May2021, 08Jun2021, 30Jul2021

BSkyFormatBSkyOneSampleTtest <- function(obj)
{
	# "BSkyOneSmTTest( alternative=c('two.sided' ), bSkyHandleSplit=c(TRUE ), conf.level=c(0.95 ), 
	# datasetNameOrDatasetGlobalIndex=c('mtcars' ), missing=c(1 ), mu=c(10 ), 
	# varNamesOrVarGlobalIndices=c('mpg','cyl','disp'))"
	
	# BSky_One_Sample_T_Test = BSkyOneSmTTest(varNamesOrVarGlobalIndices = c("disp", "hp"),mu = 0, conf.level = 0.95, 
	# alternative = "two.sided", cohens_d = TRUE,cohensd_correction = TRUE, hedges_g = TRUE, 
	# hedgesg_correction = TRUE, glass_d = TRUE, glassd_correction = FALSE,
	# datasetNameOrDatasetGlobalIndex = "mtcars", missing = 0)
	
	if( (typeof(obj) == "list") || (class(obj)[1]== "list"))
	{		
		if(length(obj) >= 7 && (c("BSkySplit") %in% names(obj)) && obj$nooftables > 1 ) ## [[2]] will contain the name of BSkyfunction. which is the same as the name of the output template.
		{
		   funcNamePosition = regexpr("BSkyOneSmTTest", obj$uasummary[[7]])
		   
		   if(funcNamePosition > 0)
		   {
				conf_level = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "conf.level")
				mu = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "mu")
				var_names = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "varNamesOrVarGlobalIndices")
				alternative = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "alternative")
				
				cohens_d = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "cohens_d")
				hedges_g = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "hedges_g")
				glass_d = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "glass_d")
						
				table_list = list()
				table_list_names = c()
				
				num_tables = obj$nooftables - 1
				n = 1 
				
				while(n <= num_tables)
				{	
					if(!is.null(obj$tables[[n]]$metadatatable[[1]]) && (dim(obj$tables[[n]]$metadatatable[[1]])[1] == 0 || (dim(obj$tables[[n]]$metadatatable[[1]])[1] > 0 && obj$tables[[n]]$metadatatable[[1]]$type[1] != -2)))
					{
						if(!is.null(obj$tables[[n]]$datatable) && dim(obj$tables[[n]]$datatable)[1] > 0 && dim(obj$tables[[n]]$datatable)[2] > 0 )
						{  
							X_has_been_printed = FALSE
							
							if(dim(obj$tables[[n]]$datatable)[2] < 4)
							{
								X_has_been_printed = TRUE
								
								filler_column = rep("X",dim(obj$tables[[n]]$datatable)[1])
										
								for(add_col in 1: (4 - dim(obj$tables[[n]]$datatable)[2]))
								{
									obj$tables[[n]]$datatable = cbind(obj$tables[[n]]$datatable, filler_column)
								}
							}
							
							obj$tables[[n]]$datatable = matrix(obj$tables[[n]]$datatable[,c(1:4)], ncol = 4)
							dimnames(obj$tables[[n]]$datatable)[[2]] = c("N", "Mean", "Std Deviation", "Std Error Mean")
							
							if(length(var_names) == dim(obj$tables[[n]]$datatable)[1])
							{
								row.names(obj$tables[[n]]$datatable) = var_names
							}
							
							if(obj$BSkySplit == 1)
							{
								#table_list_names = c(table_list_names, paste("One Sample Statistics -", obj$tables[[n]]$cartlevel))
								
								#attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel))
								attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = paste("Split:", strsplit(substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel)),",")[[1]][2], sep="")
								
								split_iteration_headline_str = paste("Begins Split:", strsplit(substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel)),",")[[1]][2], sep="")
								
								split_iteration_headline = matrix(split_iteration_headline_str, ncol=1)
								dimnames(split_iteration_headline)[[1]] = NULL
								dimnames(split_iteration_headline)[[2]] = NULL
								
								table_list_names = c(table_list_names, "Split Headline")
								table_list = c(table_list, list(split_iteration_headline))
								names(table_list) = table_list_names
								
								table_list_names = c(table_list_names, "One Sample Statistics")
							}
							else
							{
								table_list_names = c(table_list_names, "One Sample Statistics")
							}
							
							if(X_has_been_printed == TRUE)
							{
								attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkyXExplain") = c("X indicates incomplete result due to errors")
							}
							
							num_additional_info = dim(obj$tables[[n]]$metadatatable[[1]])[1]
								
							if(num_additional_info > 0)
							{
								index = 0 
								
								for(addl_msg in 1:num_additional_info)
								{
									if(obj$tables[[n]]$metadatatable[[1]]$type[addl_msg] == -1)
									{
										index = index + 1
										
										if(trimws(obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg]) != c(""))
										{
											additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " BSky Msg: ", obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg], sep="")
											attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyAppMsg_",index, sep="")) = additional_info_str
										}
										
										if(trimws(obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg]) != c(""))
										{
											additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " R Msg: ", obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg], sep="")
											attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyRMsg_",index, sep="")) = additional_info_str
										}
									}
								}
							}
						
							table_list = c(table_list, list(obj$tables[[n]]$datatable))
							names(table_list) = table_list_names
						}
						
						n = n+1
						
						if(n <= num_tables && !is.null(obj$tables[[n]]$datatable) && dim(obj$tables[[n]]$datatable)[1] > 0 && dim(obj$tables[[n]]$datatable)[2] > 0 )
						{
							X_has_been_printed = FALSE
							
							col_header_string = paste("Test Value =", mu)
							col_header = rep(col_header_string,6)
							conf_header = paste("confidence:", format(as.numeric(conf_level), nsmall=2))
							col_header_row1 = c(" ", " ", " ", " ", conf_header, conf_header)
							
							if(alternative == "less")
							{
								sig_col_header = "Sig.(1-tail, <)"
							}
							else if(alternative == "greater")
							{
								sig_col_header = "Sig.(1-tail, >)"
							}
							else
							{
								sig_col_header = "Sig.(2-tail)"
							}
							
							stat_header_row2 = c("t", "df", sig_col_header, "mean difference", "lower", "upper") 
							
							if(!is.null(obj$tables[[n]]$datatable))
							{
								if(dim(obj$tables[[n]]$datatable)[2] == length(stat_header_row2))
								{
									obj$tables[[n]]$datatable = rbind(col_header_row1, stat_header_row2, obj$tables[[n]]$datatable)
									dimnames(obj$tables[[n]]$datatable)[[2]] = col_header
								}
								else if(dim(obj$tables[[n]]$datatable)[2] < length(stat_header_row2))
								{
									filler_column = rep("X",dim(obj$tables[[n]]$datatable)[1])
									
									X_has_been_printed = TRUE
											
									for(add_col in 1: (length(stat_header_row2) - dim(obj$tables[[n]]$datatable)[2]))
									{
										obj$tables[[n]]$datatable = cbind(obj$tables[[n]]$datatable, filler_column)
									}
									
									obj$tables[[n]]$datatable = rbind(col_header_row1, stat_header_row2, obj$tables[[n]]$datatable)
									dimnames(obj$tables[[n]]$datatable)[[2]] = col_header
								}
								
								row_names = c(" ", " ", var_names)
								
								if(dim(obj$tables[[n]]$datatable)[1] == length(row_names))
								{
									row.names(obj$tables[[n]]$datatable) = row_names
								}
								
								if(obj$BSkySplit == 1)
								{
									table_list_names = c(table_list_names, "One Sample Test")
									#table_list_names = c(table_list_names, paste("One Sample Statistics -", obj$tables[[n]]$cartlevel))
									
									#attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel))
									attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = paste("Split:", strsplit(substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel)),",")[[1]][2], sep="")
							
								}
								else
								{
									table_list_names = c(table_list_names, "One Sample t-test")
								}
								
								if(X_has_been_printed == TRUE)
								{
									attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkyXExplain") = c("X indicates incomplete result due to errors")
								}
								
								num_additional_info = dim(obj$tables[[n]]$metadatatable[[1]])[1]
								
								if(num_additional_info > 0)
								{
									index = 0 
									
									for(addl_msg in 1:num_additional_info)
									{
										if(obj$tables[[n]]$metadatatable[[1]]$type[addl_msg] == -1)
										{
											index = index + 1
											
											if(trimws(obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg]) != c(""))
											{
												additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " BSky Msg: ", obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg], sep="")
												attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyAppMsg_",index, sep="")) = additional_info_str
											}
											
											if(trimws(obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg]) != c(""))
											{
												additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " R Msg: ", obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg], sep="")
												attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyRMsg_",index, sep="")) = additional_info_str
											}
										}
									}
								}
								
								table_list = c(table_list, list(obj$tables[[n]]$datatable))
								names(table_list) = table_list_names
							}
						}
						
						n = n+1
						
						if(n <= num_tables && cohens_d == TRUE)
						{
							if(!is.null(obj$tables[[n]]$datatable) && dim(obj$tables[[n]]$datatable)[1] > 0 && dim(obj$tables[[n]]$datatable)[2] > 0 )
							{  
								X_has_been_printed = FALSE
								
								if(dim(obj$tables[[n]]$datatable)[2] < 4)
								{
									X_has_been_printed = TRUE
									
									filler_column = rep("X",dim(obj$tables[[n]]$datatable)[1])
											
									for(add_col in 1: (4 - dim(obj$tables[[n]]$datatable)[2]))
									{
										obj$tables[[n]]$datatable = cbind(obj$tables[[n]]$datatable, filler_column)
									}
								}
								
								obj$tables[[n]]$datatable = matrix(obj$tables[[n]]$datatable[,c(1:4)], ncol = 4)
								dimnames(obj$tables[[n]]$datatable)[[2]] = c("Cohens_d", "CI", "CI_low", "CI_high")
								
								if(length(var_names) == dim(obj$tables[[n]]$datatable)[1])
								{
									row.names(obj$tables[[n]]$datatable) = var_names
								}
								
								if(obj$BSkySplit == 1)
								{
									table_list_names = c(table_list_names, "Cohen's d")
									#table_list_names = c(table_list_names, paste("One Sample Statistics -", obj$tables[[n]]$cartlevel))
									
									#attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel))
									attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = paste("Split:", strsplit(substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel)),",")[[1]][2], sep="")
								}
								else
								{
									table_list_names = c(table_list_names, "Cohen's d")
								}
								
								if(X_has_been_printed == TRUE)
								{
									attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkyXExplain") = c("X indicates incomplete result due to errors")
								}
								
								num_additional_info = dim(obj$tables[[n]]$metadatatable[[1]])[1]
									
								if(num_additional_info > 0)
								{
									index = 0 
									
									for(addl_msg in 1:num_additional_info)
									{
										if(obj$tables[[n]]$metadatatable[[1]]$type[addl_msg] == -1)
										{
											index = index + 1
											
											if(trimws(obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg]) != c(""))
											{
												additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " BSky Msg: ", obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg], sep="")
												attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyAppMsg_",index, sep="")) = additional_info_str
											}
											
											if(trimws(obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg]) != c(""))
											{
												additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " R Msg: ", obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg], sep="")
												attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyRMsg_",index, sep="")) = additional_info_str
											}
										}
									}
								}
							
								table_list = c(table_list, list(obj$tables[[n]]$datatable))
								names(table_list) = table_list_names
							}
						
							n = n+1
						}
						
						if(n <= num_tables && hedges_g == TRUE)
						{
							if(!is.null(obj$tables[[n]]$datatable) && dim(obj$tables[[n]]$datatable)[1] > 0 && dim(obj$tables[[n]]$datatable)[2] > 0 )
							{  
								X_has_been_printed = FALSE
								
								if(dim(obj$tables[[n]]$datatable)[2] < 4)
								{
									X_has_been_printed = TRUE
									
									filler_column = rep("X",dim(obj$tables[[n]]$datatable)[1])
											
									for(add_col in 1: (4 - dim(obj$tables[[n]]$datatable)[2]))
									{
										obj$tables[[n]]$datatable = cbind(obj$tables[[n]]$datatable, filler_column)
									}
								}
								
								obj$tables[[n]]$datatable = matrix(obj$tables[[n]]$datatable[,c(1:4)], ncol = 4)
								dimnames(obj$tables[[n]]$datatable)[[2]] = c("Hedges_g", "CI", "CI_low", "CI_high")
								
								if(length(var_names) == dim(obj$tables[[n]]$datatable)[1])
								{
									row.names(obj$tables[[n]]$datatable) = var_names
								}
								
								if(obj$BSkySplit == 1)
								{
									table_list_names = c(table_list_names, "Hedges' g")
									#table_list_names = c(table_list_names, paste("One Sample Statistics -", obj$tables[[n]]$cartlevel))
									
									#attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel))
									attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = paste("Split:", strsplit(substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel)),",")[[1]][2], sep="")
								}
								else
								{
									table_list_names = c(table_list_names, "Hedges' g")
								}
								
								if(X_has_been_printed == TRUE)
								{
									attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkyXExplain") = c("X indicates incomplete result due to errors")
								}
								
								num_additional_info = dim(obj$tables[[n]]$metadatatable[[1]])[1]
									
								if(num_additional_info > 0)
								{
									index = 0 
									
									for(addl_msg in 1:num_additional_info)
									{
										if(obj$tables[[n]]$metadatatable[[1]]$type[addl_msg] == -1)
										{
											index = index + 1
											
											if(trimws(obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg]) != c(""))
											{
												additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " BSky Msg: ", obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg], sep="")
												attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyAppMsg_",index, sep="")) = additional_info_str
											}
											
											if(trimws(obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg]) != c(""))
											{
												additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " R Msg: ", obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg], sep="")
												attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyRMsg_",index, sep="")) = additional_info_str
											}
										}
									}
								}
							
								table_list = c(table_list, list(obj$tables[[n]]$datatable))
								names(table_list) = table_list_names
							}
						
							n = n+1
						}
						
						if(n <= num_tables && glass_d == TRUE)
						{
							if(!is.null(obj$tables[[n]]$datatable) && dim(obj$tables[[n]]$datatable)[1] > 0 && dim(obj$tables[[n]]$datatable)[2] > 0 )
							{  
								X_has_been_printed = FALSE
								
								if(dim(obj$tables[[n]]$datatable)[2] < 4)
								{
									X_has_been_printed = TRUE
									
									filler_column = rep("X",dim(obj$tables[[n]]$datatable)[1])
											
									for(add_col in 1: (4 - dim(obj$tables[[n]]$datatable)[2]))
									{
										obj$tables[[n]]$datatable = cbind(obj$tables[[n]]$datatable, filler_column)
									}
								}
								
								obj$tables[[n]]$datatable = matrix(obj$tables[[n]]$datatable[,c(1:4)], ncol = 4)
								dimnames(obj$tables[[n]]$datatable)[[2]] = c("Glass_delta", "CI", "CI_low", "CI_high")
								
								if(length(var_names) == dim(obj$tables[[n]]$datatable)[1])
								{
									row.names(obj$tables[[n]]$datatable) = var_names
								}
								
								if(obj$BSkySplit == 1)
								{
									table_list_names = c(table_list_names, "Glass' delta")
									#table_list_names = c(table_list_names, paste("One Sample Statistics -", obj$tables[[n]]$cartlevel))
									
									#attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel))
									attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = paste("Split:", strsplit(substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel)),",")[[1]][2], sep="")
								}
								else
								{
									table_list_names = c(table_list_names, "Glass' delta")
								}
								
								if(X_has_been_printed == TRUE)
								{
									attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkyXExplain") = c("X indicates incomplete result due to errors")
								}
								
								num_additional_info = dim(obj$tables[[n]]$metadatatable[[1]])[1]
									
								if(num_additional_info > 0)
								{
									index = 0 
									
									for(addl_msg in 1:num_additional_info)
									{
										if(obj$tables[[n]]$metadatatable[[1]]$type[addl_msg] == -1)
										{
											index = index + 1
											
											if(trimws(obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg]) != c(""))
											{
												additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " BSky Msg: ", obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg], sep="")
												attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyAppMsg_",index, sep="")) = additional_info_str
											}
											
											if(trimws(obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg]) != c(""))
											{
												additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " R Msg: ", obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg], sep="")
												attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyRMsg_",index, sep="")) = additional_info_str
											}
										}
									}
								}
							
								table_list = c(table_list, list(obj$tables[[n]]$datatable))
								names(table_list) = table_list_names
							}
						
							n = n+1
						}
					}
					else
					{
						n = n + 1
					}
				}
				
				table_list = c(table_list, list(obj$tables[[obj$nooftables]]))
				table_list_names = c(table_list_names, "")
				names(table_list) = table_list_names
				
				obj$tables = table_list
				obj$nooftables = length(obj$tables)
		   }
		}
	}
	
	return(invisible(obj))
}

BSkyFormatBSkyIndSampleTtest <- function(obj)
{
	# BSkyIndSmTTest( alternative=c('less' ), bSkyHandleSplit=c(TRUE ), conf.level=c(0.9 ), 
	# datasetNameOrDatasetGlobalIndex=c('mtcars' ), excludeEnvPrefix=c(FALSE ), group=c('transmission' ), 
	# missing=c(1 ), varNamesOrVarGlobalIndices=c('mpg','cyl','disp','hp','drat'))
	
	if( (typeof(obj) == "list") || (class(obj)[1]== "list"))
	{		
		if(length(obj) >= 7 && (c("BSkySplit") %in% names(obj)) && obj$nooftables > 1 ) ## [[2]] will contain the name of BSkyfunction. which is the same as the name of the output template.
		{
		   funcNamePosition = regexpr("BSkyIndSmTTest", obj$uasummary[[7]])
		   
		   if(funcNamePosition > 0)
		   {
				conf_level = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "conf.level")
				#mu = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "mu")
				var_names = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "varNamesOrVarGlobalIndices")
				alternative = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "alternative")
				group_factor_var = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "group")
				database_name = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "datasetNameOrDatasetGlobalIndex")
				
				cohens_d = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "cohens_d")
				hedges_g = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "hedges_g")
				glass_d = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "glass_d")
				
				group_factor_var_values = levels(eval(parse(text= paste(database_name, "$", group_factor_var, sep = "")), envir=globalenv()))
				
				#return(list(database_name, group_factor_var, (eval(parse(text=paste(database_name, "$", group_factor_var, sep = "")))), group_factor_var_values))
				
				table_list = list()
				table_list_names = c()
				
				num_tables = obj$nooftables - 1
				n = 1 
				
				while(n <= num_tables)
				{
					if(!is.null(obj$tables[[n]]$metadatatable[[1]]) && (dim(obj$tables[[n]]$metadatatable[[1]])[1] == 0 || (dim(obj$tables[[n]]$metadatatable[[1]])[1] > 0 && obj$tables[[n]]$metadatatable[[1]]$type[1] != -2)))
					{
						if(!is.null(obj$tables[[n]]$datatable) && dim(obj$tables[[n]]$datatable)[1] > 0 && dim(obj$tables[[n]]$datatable)[2] > 0 )
						{
							X_has_been_printed = FALSE
							
							colA = c()
							colB = c()
							
							for(i in 1:length(var_names))
							{
								colA = c(colA, var_names[i], rep(" ", length(group_factor_var_values) - 1))
								colB = c(colB, group_factor_var_values)
							}
							
							
							if(dim(obj$tables[[n]]$datatable)[2] < 4)
							{
								filler_column = rep("X",dim(obj$tables[[n]]$datatable)[1])
								
								X_has_been_printed = TRUE
										
								for(add_col in 1: (4 - dim(obj$tables[[n]]$datatable)[2]))
								{
									obj$tables[[n]]$datatable = cbind(obj$tables[[n]]$datatable, filler_column)
								}
							}
					
							obj$tables[[n]]$datatable = matrix(obj$tables[[n]]$datatable[,c(1:4)], ncol = 4)
							
							# The following condition will happens when when too many errors were generated in BSkyIndSmTTest()
							if(dim(obj$tables[[n]]$datatable)[1] < length(colA))
							{
								filler_row = rep("X", dim(obj$tables[[n]]$datatable)[2])
								
								X_has_been_printed = TRUE
								
								for(i in 1: (length(colA) - nrow(obj$tables[[n]]$datatable)))
								{
									obj$tables[[n]]$datatable = rbind(obj$tables[[n]]$datatable, filler_row)
								}
							}
							
							obj$tables[[n]]$datatable = cbind(colA, colB, obj$tables[[n]]$datatable)
							
							col_header = c(" ", group_factor_var, "N", "Mean", "Std Deviation", "Std Error Mean")
							
							if(dim(obj$tables[[n]]$datatable)[2] == length(col_header))
							{	
								dimnames(obj$tables[[n]]$datatable)[[2]] = col_header
							}
							
							row.names(obj$tables[[n]]$datatable) = c()
							
							if(obj$BSkySplit == 1)
							{
								
								#table_list_names = c(table_list_names, paste("Group Statistics -", obj$tables[[n]]$cartlevel))
								
								#attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel))
								attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = paste("Split:", strsplit(substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel)),",")[[1]][2], sep="")	
								
								split_iteration_headline_str = paste("Begins Split:", strsplit(substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel)),",")[[1]][2], sep="")
								
								split_iteration_headline = matrix(split_iteration_headline_str, ncol=1)
								dimnames(split_iteration_headline)[[1]] = NULL
								dimnames(split_iteration_headline)[[2]] = NULL
								
								table_list_names = c(table_list_names, "Split Headline")
								table_list = c(table_list, list(split_iteration_headline))
								names(table_list) = table_list_names
								
								table_list_names = c(table_list_names, "Group Statistics")
							}
							else
							{
								table_list_names = c(table_list_names, "Group Statistics")
							}
							
							if(X_has_been_printed == TRUE)
							{
								attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkyXExplain") = c("X indicates incomplete result due to errors")
							}
							
							num_additional_info = dim(obj$tables[[n]]$metadatatable[[1]])[1]
								
							if(num_additional_info > 0)
							{
								index = 0 
								
								for(addl_msg in 1:num_additional_info)
								{
									if(obj$tables[[n]]$metadatatable[[1]]$type[addl_msg] == -1)
									{
										index = index + 1
										
										if(trimws(obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg]) != c(""))
										{
											additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " BSky Msg: ", obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg], sep="")
											attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyAppMsg_",index, sep="")) = additional_info_str
										}
										
										if(trimws(obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg]) != c(""))
										{
											additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " R Msg: ", obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg], sep="")
											attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyRMsg_",index, sep="")) = additional_info_str
										}
									}
								}
							}
							
							table_list = c(table_list, list(obj$tables[[n]]$datatable))
							names(table_list) = table_list_names
						}
						
						n = n+1
						
						if(n <= num_tables && !is.null(obj$tables[[n]]$datatable) && dim(obj$tables[[n]]$datatable)[1] > 0 && dim(obj$tables[[n]]$datatable)[2] > 0 )
						{
							X_has_been_printed = FALSE
							
							col_header_string1 = paste("Levene's Test for Equality")
							col_header1 = rep(col_header_string1,3)
							col_header_string2 = paste("t-test Equality of Means")
							col_header2 = rep(col_header_string2,7)
							
							col_header_combined = c(" ", col_header1, col_header2)
							
							conf_header = paste("confidence interval of the diffs:", format(as.numeric(conf_level), nsmall=2))
							col_header_row1 = c(" ", " ", " ", " ", " ", " ", " ", " ", " ", conf_header, conf_header)
							
							if(alternative == "less")
							{
								sig_col_header = "Sig.(1-tail, <)"
							}
							else if(alternative == "greater")
							{
								sig_col_header = "Sig.(1-tail, >)"
							}
							else
							{
								sig_col_header = "Sig.(2-tail)"
							}
							
							stat_header_row2 = c(" ", " ","Sig.", "F", "t", "df", sig_col_header, "mean difference", "std. error difference", "lower", "upper") 
											
							colA = c()
							colB = c()
							
							for(i in 1:length(var_names))
							{
								colA = c(colA, var_names[i], " ")
								colB = c(colB, "Equal variance assumed", "Equal variances not assumed")
							}
							
							if(!is.null(obj$tables[[n]]$datatable))
							{
								# The following condition will happens when when too many errors were generated in BSkyIndSmTTest()
								if(dim(obj$tables[[n]]$datatable)[1] < length(colA))
								{
									filler_row = rep("X", dim(obj$tables[[n]]$datatable)[2])
									
									X_has_been_printed = TRUE
									
									for(i in 1: (length(colA) - dim(obj$tables[[n]]$datatable)[1]))
									{
										obj$tables[[n]]$datatable = rbind(obj$tables[[n]]$datatable, filler_row)
									}
								}
								
								if(dim(obj$tables[[n]]$datatable)[2] < (length(stat_header_row2) - 2))
								{
									filler_column = rep("X",dim(obj$tables[[n]]$datatable)[1])
									
									X_has_been_printed = TRUE
											
									for(add_col in 1: ((length(stat_header_row2) -2) - dim(obj$tables[[n]]$datatable)[2]))
									{
										obj$tables[[n]]$datatable = cbind(obj$tables[[n]]$datatable, filler_column)
									}
								}
								else if(dim(obj$tables[[n]]$datatable)[2] > ((length(stat_header_row2) - 2)))
								{
									obj$tables[[n]]$datatable = matrix(obj$tables[[n]]$datatable[1:length(stat_header_row2)], ncol = length(stat_header_row2) - 2)
								}
							
								obj$tables[[n]]$datatable = cbind(colA, colB, obj$tables[[n]]$datatable)
								
								obj$tables[[n]]$datatable = rbind(col_header_row1, stat_header_row2, obj$tables[[n]]$datatable)
								dimnames(obj$tables[[n]]$datatable)[[2]] = col_header_combined
								
								row.names(obj$tables[[n]]$datatable) = c()
								
								if(obj$BSkySplit == 1)
								{
									table_list_names = c(table_list_names, "Independent Samples Test")
									#table_list_names = c(table_list_names, paste("Group Statistics -", obj$tables[[n]]$cartlevel))
									
									#attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel))
									attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = paste("Split:", strsplit(substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel)),",")[[1]][2], sep="")
								}
								else
								{
									table_list_names = c(table_list_names, "Independent Samples t-test")
								}
								
								if(X_has_been_printed == TRUE)
								{
									attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkyXExplain") = c("X indicates incomplete result due to errors")
								}
								
								num_additional_info = dim(obj$tables[[n]]$metadatatable[[1]])[1]
								
								if(num_additional_info > 0)
								{
									index = 0 
									
									for(addl_msg in 1:num_additional_info)
									{
										if(obj$tables[[n]]$metadatatable[[1]]$type[addl_msg] == -1)
										{
											index = index + 1
											
											if(trimws(obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg]) != c(""))
											{
												additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " BSky Msg: ", obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg], sep="")
												attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyAppMsg_",index, sep="")) = additional_info_str
											}
											
											if(trimws(obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg]) != c(""))
											{
												additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " R Msg: ", obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg], sep="")
												attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyRMsg_",index, sep="")) = additional_info_str
											}
										}
									}
								}
								
								table_list = c(table_list, list(obj$tables[[n]]$datatable))
								names(table_list) = table_list_names
							}
						}
						
						n = n + 1
						
						if(n <= num_tables && cohens_d == TRUE)
						{
							if(!is.null(obj$tables[[n]]$datatable) && dim(obj$tables[[n]]$datatable)[1] > 0 && dim(obj$tables[[n]]$datatable)[2] > 0 )
							{  
								X_has_been_printed = FALSE
								
								if(dim(obj$tables[[n]]$datatable)[2] < 4)
								{
									X_has_been_printed = TRUE
									
									filler_column = rep("X",dim(obj$tables[[n]]$datatable)[1])
											
									for(add_col in 1: (4 - dim(obj$tables[[n]]$datatable)[2]))
									{
										obj$tables[[n]]$datatable = cbind(obj$tables[[n]]$datatable, filler_column)
									}
								}
								
								obj$tables[[n]]$datatable = matrix(obj$tables[[n]]$datatable[,c(1:4)], ncol = 4)
								dimnames(obj$tables[[n]]$datatable)[[2]] = c("Cohens_d", "CI", "CI_low", "CI_high")
								
								if(length(var_names) == dim(obj$tables[[n]]$datatable)[1])
								{
									row.names(obj$tables[[n]]$datatable) = var_names
								}
								
								if(obj$BSkySplit == 1)
								{
									table_list_names = c(table_list_names, "Cohen's d")
									#table_list_names = c(table_list_names, paste("One Sample Statistics -", obj$tables[[n]]$cartlevel))
									
									#attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel))
									attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = paste("Split:", strsplit(substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel)),",")[[1]][2], sep="")
								}
								else
								{
									table_list_names = c(table_list_names, "Cohen's d")
								}
								
								if(X_has_been_printed == TRUE)
								{
									attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkyXExplain") = c("X indicates incomplete result due to errors")
								}
								
								num_additional_info = dim(obj$tables[[n]]$metadatatable[[1]])[1]
									
								if(num_additional_info > 0)
								{
									index = 0 
									
									for(addl_msg in 1:num_additional_info)
									{
										if(obj$tables[[n]]$metadatatable[[1]]$type[addl_msg] == -1)
										{
											index = index + 1
											
											if(trimws(obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg]) != c(""))
											{
												additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " BSky Msg: ", obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg], sep="")
												attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyAppMsg_",index, sep="")) = additional_info_str
											}
											
											if(trimws(obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg]) != c(""))
											{
												additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " R Msg: ", obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg], sep="")
												attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyRMsg_",index, sep="")) = additional_info_str
											}
										}
									}
								}
							
								table_list = c(table_list, list(obj$tables[[n]]$datatable))
								names(table_list) = table_list_names
							}
						
							n = n+1
						}
						
						if(n <= num_tables && hedges_g == TRUE)
						{
							if(!is.null(obj$tables[[n]]$datatable) && dim(obj$tables[[n]]$datatable)[1] > 0 && dim(obj$tables[[n]]$datatable)[2] > 0 )
							{  
								X_has_been_printed = FALSE
								
								if(dim(obj$tables[[n]]$datatable)[2] < 4)
								{
									X_has_been_printed = TRUE
									
									filler_column = rep("X",dim(obj$tables[[n]]$datatable)[1])
											
									for(add_col in 1: (4 - dim(obj$tables[[n]]$datatable)[2]))
									{
										obj$tables[[n]]$datatable = cbind(obj$tables[[n]]$datatable, filler_column)
									}
								}
								
								obj$tables[[n]]$datatable = matrix(obj$tables[[n]]$datatable[,c(1:4)], ncol = 4)
								dimnames(obj$tables[[n]]$datatable)[[2]] = c("Hedges_g", "CI", "CI_low", "CI_high")
								
								if(length(var_names) == dim(obj$tables[[n]]$datatable)[1])
								{
									row.names(obj$tables[[n]]$datatable) = var_names
								}
								
								if(obj$BSkySplit == 1)
								{
									table_list_names = c(table_list_names, "Hedges' g")
									#table_list_names = c(table_list_names, paste("One Sample Statistics -", obj$tables[[n]]$cartlevel))
									
									#attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel))
									attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = paste("Split:", strsplit(substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel)),",")[[1]][2], sep="")
								}
								else
								{
									table_list_names = c(table_list_names, "Hedges' g")
								}
								
								if(X_has_been_printed == TRUE)
								{
									attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkyXExplain") = c("X indicates incomplete result due to errors")
								}
								
								num_additional_info = dim(obj$tables[[n]]$metadatatable[[1]])[1]
									
								if(num_additional_info > 0)
								{
									index = 0 
									
									for(addl_msg in 1:num_additional_info)
									{
										if(obj$tables[[n]]$metadatatable[[1]]$type[addl_msg] == -1)
										{
											index = index + 1
											
											if(trimws(obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg]) != c(""))
											{
												additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " BSky Msg: ", obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg], sep="")
												attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyAppMsg_",index, sep="")) = additional_info_str
											}
											
											if(trimws(obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg]) != c(""))
											{
												additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " R Msg: ", obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg], sep="")
												attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyRMsg_",index, sep="")) = additional_info_str
											}
										}
									}
								}
							
								table_list = c(table_list, list(obj$tables[[n]]$datatable))
								names(table_list) = table_list_names
							}
						
							n = n+1
						}
						
						if(n <= num_tables && glass_d == TRUE)
						{
							if(!is.null(obj$tables[[n]]$datatable) && dim(obj$tables[[n]]$datatable)[1] > 0 && dim(obj$tables[[n]]$datatable)[2] > 0 )
							{  
								X_has_been_printed = FALSE
								
								if(dim(obj$tables[[n]]$datatable)[2] < 4)
								{
									X_has_been_printed = TRUE
									
									filler_column = rep("X",dim(obj$tables[[n]]$datatable)[1])
											
									for(add_col in 1: (4 - dim(obj$tables[[n]]$datatable)[2]))
									{
										obj$tables[[n]]$datatable = cbind(obj$tables[[n]]$datatable, filler_column)
									}
								}
								
								obj$tables[[n]]$datatable = matrix(obj$tables[[n]]$datatable[,c(1:4)], ncol = 4)
								dimnames(obj$tables[[n]]$datatable)[[2]] = c("Glass_delta", "CI", "CI_low", "CI_high")
								
								if(length(var_names) == dim(obj$tables[[n]]$datatable)[1])
								{
									row.names(obj$tables[[n]]$datatable) = var_names
								}
								
								if(obj$BSkySplit == 1)
								{
									table_list_names = c(table_list_names, "Glass' delta")
									#table_list_names = c(table_list_names, paste("One Sample Statistics -", obj$tables[[n]]$cartlevel))
									
									#attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel))
									attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkySplit") = paste("Split:", strsplit(substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel)),",")[[1]][2], sep="")
								}
								else
								{
									table_list_names = c(table_list_names, "Glass' delta")
								}
								
								if(X_has_been_printed == TRUE)
								{
									attr(obj$tables[[n]]$datatable, "BSkyFootnote_BSkyXExplain") = c("X indicates incomplete result due to errors")
								}
								
								num_additional_info = dim(obj$tables[[n]]$metadatatable[[1]])[1]
									
								if(num_additional_info > 0)
								{
									index = 0 
									
									for(addl_msg in 1:num_additional_info)
									{
										if(obj$tables[[n]]$metadatatable[[1]]$type[addl_msg] == -1)
										{
											index = index + 1
											
											if(trimws(obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg]) != c(""))
											{
												additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " BSky Msg: ", obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg], sep="")
												attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyAppMsg_",index, sep="")) = additional_info_str
											}
											
											if(trimws(obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg]) != c(""))
											{
												additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " R Msg: ", obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg], sep="")
												attr(obj$tables[[n]]$datatable, paste("BSkyFootnote_BSkyRMsg_",index, sep="")) = additional_info_str
											}
										}
									}
								}
							
								table_list = c(table_list, list(obj$tables[[n]]$datatable))
								names(table_list) = table_list_names
							}
						
							n = n+1
						}
					}
					else
					{
						n = n + 1
					}
				}
				
				table_list = c(table_list, list(obj$tables[[obj$nooftables]]))
				table_list_names = c(table_list_names, "")
				names(table_list) = table_list_names
				
				obj$tables = table_list
				obj$nooftables = length(obj$tables)
		   }
		}
	}
	
	return(invisible(obj))
}

BSkyFormatBSkyCrossTable <- function(obj)
{
	#BSkyCrossTable( asresid=c(TRUE ), bSkyHandleSplit=c(TRUE ), chisq=c(FALSE ), datasetname=c('mtcars' ), 
	#digits=c(3 ), dnn=NULL, expected=c(TRUE ), fisher=c(FALSE ), layers=c('am' ,'cyl' ), max.width=c(5 ), 
	#mcnemar=c(FALSE ), missing.include=c(TRUE ), prop.c=c(FALSE ), prop.chisq=c(FALSE ), prop.r=c(FALSE ), 
	#prop.t=c(FALSE ), resid=c(TRUE ), sresid=c(TRUE ), weight=c(NA ), x=c('transmission' ), y=c('region' ) )
	# resid=TRUE, sresid=TRUE, asresid=TRUE Residual, Std. Residual, Adjusted Residual
	
	if( (typeof(obj) == "list") || (class(obj)[1]== "list"))
	{		
		if(length(obj) >= 7 && (c("BSkySplit") %in% names(obj)) && obj$nooftables > 1 ) ## [[2]] will contain the name of BSkyfunction. which is the same as the name of the output template.
		{
		   funcNamePosition = regexpr("BSkyCrossTable", obj$uasummary[[7]])
		   
		   if(funcNamePosition > 0)
		   {
				#return(obj$tables[[1]]$metadatatable[[3]])
				
				database_name = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "datasetname")
				
				param_expected = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "expected")
				param_row_prop = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "prop.r")
				param_col_prop = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "prop.c")
				
				param_row = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "x")
				param_col = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "y")
				param_layers = trimws(BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "layers"))
				
				param_resid = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "resid")
				param_sresid = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "sresid")
				param_asresid = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "asresid")
				
				# cat("<br> resid, sresid, asresid <br>")
				# cat(param_resid, param_sresid, param_asresid)
				# cat("<br>")
				
				param_chisq = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "chisq")
				param_mcnemar = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "mcnemar")
				param_fisher = BSkyFormatBSkyFunctionParamParsing(obj$uasummary[[7]], "fisher")
				
				dataset_nrow = nrow(eval(parse(text= paste(database_name)), envir=globalenv()))
				
				tabulation = c("Count")
				
				num_count_elements = 0
				
				if(toupper(param_expected) == "TRUE")
				{
					tabulation = c(tabulation, "Expected Count")
					num_count_elements = num_count_elements + 1
				}
				
				if(toupper(param_row_prop) == "TRUE")
				{
					tabulation = c(tabulation, paste("% within", param_row))
					num_count_elements = num_count_elements + 1
				}
				
				if(toupper(param_col_prop) == "TRUE")
				{
					tabulation = c(tabulation, paste("% within", param_col))
					num_count_elements = num_count_elements + 1
				}
				
				num_extra_count_elements = 0
				
				if(toupper(param_resid) == "TRUE")
				{
					tabulation = c(tabulation, "Residual")
					num_count_elements = num_count_elements + 1
					num_extra_count_elements = num_extra_count_elements + 1
				}
				
				if(toupper(param_sresid) == "TRUE")
				{
					tabulation = c(tabulation, "Std. Residual")
					num_count_elements = num_count_elements + 1
					num_extra_count_elements = num_extra_count_elements + 1
				}
				
				if(toupper(param_asresid) == "TRUE")
				{
					tabulation = c(tabulation, "Adjusted Residual")
					num_count_elements = num_count_elements + 1
					num_extra_count_elements = num_extra_count_elements + 1
				}
				
				additional_tests_count = 0
				additional_tests = c()
					
				if(toupper(param_chisq) == "TRUE")
				{
					additional_tests = c(additional_tests, "Pearson Chi Square")
					additional_tests_count = additional_tests_count + 1
				}
				
				if(toupper(param_mcnemar) == "TRUE")
				{
					additional_tests = c(additional_tests, "Mcnemar's test")
					additional_tests_count = additional_tests_count + 1
				}
				
				if(toupper(param_fisher) == "TRUE")
				{
					additional_tests = c(additional_tests, "Fisher's test")
					additional_tests_count = additional_tests_count + 1
				}
					
				
				tabulation_levels = tabulation
				
				tabulation = c(tabulation, rep("Count", dataset_nrow - length(tabulation)))
				
				tabulation_ordered_levels = factor(tabulation, ordered = TRUE, levels = tabulation_levels)
				
				modified_param_row = factor(eval(parse(text= paste(database_name, "$", param_row, sep = "")), envir=globalenv()), ordered = TRUE, levels = c(levels(eval(parse(text= paste(database_name, "$", param_row, sep = "")), envir=globalenv())), "Total"))
				
				modified_param_col = factor(eval(parse(text= paste(database_name, "$", param_col, sep = "")), envir=globalenv()), ordered = TRUE, levels = c(levels(eval(parse(text= paste(database_name, "$", param_col, sep = "")), envir=globalenv())), "Total"))
				
				param_layers = rev(param_layers)
				
				if(length(param_layers) > 1 || ( length(param_layers) == 1 && param_layers != "NA"))
				{
					param_layers_values = c()
					for (i in 1:length(param_layers))
					{
						param_layers_values = paste(param_layers_values, ",")
						param_layers_values = paste(param_layers_values, param_layers[i], "=", database_name, "$", param_layers[i], sep = "")
					}
					
					#table_exe_string = paste("tabulation = tabulation_ordered_levels,", param_col, "=", database_name, "$", param_col, ",", param_row, "=", database_name, "$", param_row, param_layers_values, sep = "")
					table_exe_string = paste("count = tabulation_ordered_levels,", param_col, "=", "modified_param_col,", param_row, "=", "modified_param_row", param_layers_values, sep = "")
				}
				else
				{
					param_layers = c()
					table_exe_string = paste("count = tabulation_ordered_levels,", param_col, "=", "modified_param_col,", param_row, "=", "modified_param_row", sep = "")
				}
				
				table_exe_string = paste("table(", table_exe_string, ")")
				
				# cat("\n=======================\n")
				# print(table_exe_string)
				# cat("\n=======================\n")
				
				cross_table_skeleton_non_formatted = eval(parse(text= table_exe_string))

				# Table to create the combination for factor levels for all all layers, row, and column 
				cross_table_skeleton = BSkyTableObjectFormat(cross_table_skeleton_non_formatted)
				
				param_row_col_index = length(param_layers) + 1
				
				###################################################################################
				# Remove extra rows if present from the "Total" section of the skeleton table like 
				# Residual, Std. Residual, Adjusted Residual
				###################################################################################
				extra_Total_rows_to_be_deleted = c()
				
				if(num_extra_count_elements > 0)
				{
					for(i in 1: nrow(cross_table_skeleton))
					{
						if(cross_table_skeleton[i,param_row_col_index] == "Total")
						{
							extra_Total_rows_to_be_deleted = c(extra_Total_rows_to_be_deleted, (i+ num_count_elements - num_extra_count_elements + 1):(i+ num_count_elements))
						}
					}
					
					cross_table_skeleton = cross_table_skeleton[- extra_Total_rows_to_be_deleted,]
				}
				
				######################################################################################
				# set it up for split iteration if there is a split - otherwise it will just loop once 
				#######################################################################################
				
				table_list = list()
				table_list_names = c()
				orig_cross_table_skeleton = cross_table_skeleton
				additional_tests_orig = additional_tests
				
				num_tables = obj$nooftables - 1
				n = 1 
				
				while(n <= num_tables)
				{
					if(obj$tables[[n]]$nometadatatables > 1) # || dim($tables[[n]]$metadatatable[[1]])[1] == 0 || (dim($tables[[n]]$metadatatable[[1]])[1] > 0 && $tables[[n]]$metadatatable[[1]]$severityType[1] != -2))
					{
						if(obj$tables[[n]]$metadatatabletype == "crosstab" && !is.null(obj$tables[[n]]$datatable) && dim(obj$tables[[n]]$datatable)[1] > 0 && dim(obj$tables[[n]]$datatable)[2] > 0)
						{
							X_has_been_printed = FALSE
							orig_datatable_dim = dim(obj$tables[[n]]$datatable)
							param_layers_values_df = NULL 
							
							cross_table_skeleton = orig_cross_table_skeleton
							additional_tests = additional_tests_orig
							
							#return(list(cross_table_skeleton))
							
							##################################################################################
							# Remove the combination of layer values that have only zero counts
							##################################################################################
							
							if(length(param_layers) > 1 || (length(param_layers) == 1 && param_layers != "NA"))
							{
								param_layers_values = c()
								
								for (i in 1:length(param_layers))
								{
									param_layers_values = paste(param_layers_values, param_layers[i], "=", database_name, "$", param_layers[i], sep = "")
									
									if( i != length(param_layers))
									{
										param_layers_values = paste(param_layers_values, ",")
									}
								}
								
								param_layers_values = paste("table(", param_layers_values, ")")
							
								param_layers_values_df = (as.data.frame(eval(parse(text= param_layers_values)),stringsAsFactors = FALSE))[length(param_layers):1]
								
								
								# cat("<br>", nrow(param_layers_values_df), nrow(obj$tables[[n]]$metadatatable[[3]]), "<br>")
								# return(list(param_layers_values_df, obj$tables[[n]]$metadatatable[[3]]))
								
								
								if(length(obj$tables[[n]]$metadatatable) > 2 && !is.null(obj$tables[[n]]$metadatatable[[3]]) && nrow(param_layers_values_df) > dim(obj$tables[[n]]$metadatatable[[3]])[1])
								{
									number_of_rows_per_layer_value_combination = eval(parse(text= paste("length(levels(", database_name, "$", param_row,"))", sep = ""))) * (num_count_elements +1) + ((num_count_elements + 1) - num_extra_count_elements)
									
									start_row_num = c()
									end_row_num = c()
									start_row = 1
									
									dimnames(obj$tables[[n]]$metadatatable[[3]])[[2]] = rev(param_layers)
									row.names(obj$tables[[n]]$metadatatable[[3]]) = c()
									
									
									to_be_deleted_row_indices = c()
									
									for (i in 1:nrow(param_layers_values_df))
									{
										# cat("<br>")
										# print(as.character(param_layers_values_df[i,]))
										# cat("<br>")
											
										found_all_layer_value_match = FALSE
										
										for(j in 1: nrow(obj$tables[[n]]$metadatatable[[3]]))
										{
											# print(obj$tables[[1]]$metadatatable[[3]][j,])
											# cat("<br>")
											# print(param_layers_values_df[i,] %in% obj$tables[[1]]$metadatatable[[3]][j,])
											# cat("<br>")
											
											#if(all(as.character(param_layers_values_df[i,]) %in% as.character(obj$tables[[1]]$metadatatable[[3]][j,])) == TRUE)
											#if(length(intersect(as.character(param_layers_values_df[i,]), as.character(obj$tables[[1]]$metadatatable[[3]][j,]))) == ncol(param_layers_values_df))
											cell_matched = 0
											for(m in 1:ncol(param_layers_values_df))
											{
												if(as.character(param_layers_values_df[i,m]) == as.character(obj$tables[[n]]$metadatatable[[3]][j,m]))
												{
													cell_matched = cell_matched + 1
												}
											}
											
											if(cell_matched == ncol(param_layers_values_df))
											{
												# cat("<br>")
												# print("Found")
												# cat("<br>")
												# print(as.character(obj$tables[[1]]$metadatatable[[3]][j,]))
												# cat("<br>")
												# print(i)
												# cat("<br>")
												
												found_all_layer_value_match = TRUE
											}
										}
										
										start_row_num = c(start_row_num, start_row)
										end_row_num = c(end_row_num, start_row + number_of_rows_per_layer_value_combination - 1)
										
										
										if(found_all_layer_value_match == FALSE)
										{
											to_be_deleted_row_indices = c(to_be_deleted_row_indices, c((start_row + 1):(start_row + 1 + number_of_rows_per_layer_value_combination - 1)))
											
												# cat("<br>")
												# print("start_row")
												# print(start_row)
												# cat("<br>")
												# print("number_of_rows_per_layer_value_combination")
												# print(number_of_rows_per_layer_value_combination)
												# cat("<br>")
											
											if(length(param_layers) > 1)
											{
												for(k in 1: (length(param_layers) - 1))
												{
													if(trimws(cross_table_skeleton[start_row + 1,k]) != "")
													{
														if((start_row + 1 + number_of_rows_per_layer_value_combination) <= nrow(cross_table_skeleton))
														{
															if(trimws(cross_table_skeleton[(start_row + 1 + number_of_rows_per_layer_value_combination),k]) == "")
															{
																cross_table_skeleton[(start_row + 1 + number_of_rows_per_layer_value_combination),k] = cross_table_skeleton[start_row + 1, k]
															}
														}
													}
												}
											}
										}
										
										start_row = start_row + number_of_rows_per_layer_value_combination
									}
									
									# cat("<br>")
									# cat(length(to_be_deleted_row_indices))
									# cat("<br>")
									# cat(to_be_deleted_row_indices)
									# cat("<br>")
									
									param_layers_values_df = cbind(param_layers_values_df, start_row_number = start_row_num, end_row_number = end_row_num)
									#cross_table_skeleton = cross_table_skeleton[- as.numeric(to_be_deleted_row_indices),] #for some odd reason this syntax did not work
									
									#return(list(param_layers_values_df, cross_table_skeleton))
									
									# remove the to to_be_deleted_rows 
									new_temp_table = c()
									for(i in 1:nrow(cross_table_skeleton))
									{
										if(!(i %in% as.numeric(to_be_deleted_row_indices)))
										{
											new_temp_table = rbind(new_temp_table, cross_table_skeleton[i,])
										}
									}
									
									names(new_temp_table) = names(cross_table_skeleton)
									cross_table_skeleton = new_temp_table
									
									# cat("\n<br>======================<br>\n")
									# print(nrow(cross_table_skeleton))
									# print(nrow(obj$tables[[n]]$metadatatable[[2]]))
									# #print(cross_table_skeleton)
									# cat("\n<br>======================<br>\n")
									
									# return(list(param_layers_values_df, cross_table_skeleton))
								}
							}
							
							# cat("<br>")
							# cat("<br> iteration number,", n, "<br>")
							# print(nrow(cross_table_skeleton))
							# cat("<br>")
							# print(nrow(obj$tables[[n]]$metadatatable[[2]]))
							
							# now delete the invidual rows within the layer combinations filtered from the above step
							
							if(length(obj$tables[[n]]$metadatatable) > 1 && !is.null(obj$tables[[n]]$metadatatable[[2]]) && dim(obj$tables[[n]]$metadatatable[[2]])[1]>0 && dim(obj$tables[[n]]$metadatatable[[2]])[2]>0 && dim(obj$tables[[n]]$metadatatable[[2]])[1] == (dim(cross_table_skeleton)[1] - 1))
							{
								#cat("<br> ===================<br>")
								row_filter = c(1,obj$tables[[n]]$metadatatable[[2]][,2])
								cross_table_skeleton = cbind(cross_table_skeleton, row_filter)
							}
							else
							{
								row_filter = c(rep(1, nrow(cross_table_skeleton)))
								cross_table_skeleton = cbind(cross_table_skeleton, row_filter)
							}
							
							
							# cat("<br>nrow(cross_table_skeleton)<br>")
							# print(nrow(cross_table_skeleton))
							# cat("<br>ncol(cross_table_skeleton)<br>")
							# print(ncol(cross_table_skeleton))
							# cat("<br> param_row_col_index - 1 :", (param_row_col_index - 1),"<br>")
							# return(list(cross_table_skeleton)) #, t(row_filter)))
							
							#remove the individual row based on 0 (delete) or 1(keep) value of the very last column 
							
							last_col_index = ncol(cross_table_skeleton)
							
							# adjust the layer values to shift down as needed if the values were on the individual row to be deleted 
							if(length(param_layers) > 1 || ( length(param_layers) == 1 && param_layers != "NA"))
							{
								for(i in 2:nrow(cross_table_skeleton))
								{
									if(cross_table_skeleton[i,last_col_index] == 0)
									{
										# print(i)
										# print("found 1")
									
										if(trimws(cross_table_skeleton[i,param_row_col_index - 1]) != "")
										{
											# print(i)
											# print("found 2")
											
											if( (i+ length(tabulation_levels)) <= nrow(cross_table_skeleton))
											{
												if(trimws(cross_table_skeleton[(i + length(tabulation_levels)),param_row_col_index - 1]) == "")
												{
													# print(i)
													# print("found 3")
													
													layer_values_to_be_copied = cross_table_skeleton[i, c(1:(param_row_col_index - 1))]
													# print(layer_values_to_be_copied)
													# print(param_row_col_index)
													# print(c(1:(param_row_col_index - 1)))
													
													cross_table_skeleton[(i + length(tabulation_levels)), c(1:(param_row_col_index - 1))] = layer_values_to_be_copied
												}
											}
										}
									}
								}
							}
							
							# remove the individual row based on the last column value 0 or 1
							
							col_header = names(cross_table_skeleton)
							cross_table_skeleton1 = (cross_table_skeleton[cross_table_skeleton[,last_col_index] != 0,]) 
							cross_table_skeleton2 = cross_table_skeleton1[,1:(last_col_index-1)]
							names(cross_table_skeleton2) = col_header[1:(length(col_header) -1)]
							row.names(cross_table_skeleton2) = c()
						
							#return(list(cross_table_skeleton2))
						
							if(dim(cross_table_skeleton2)[1] > 0 && dim(cross_table_skeleton2)[2] > 0 && !is.null(obj$tables[[n]]$datatable) && dim(obj$tables[[n]]$datatable)[1] > 0 && dim(obj$tables[[n]]$datatable)[2] > 0)
							{
								####################################################################################
								#If needed, turn this back off - convert NAs to blank space in the output table display
								####################################################################################
								obj$tables[[n]]$datatable[is.na(obj$tables[[n]]$datatable)] = c("")
								
								# cat("\n<br> iteration num: ",n, "<br>\n")
								# cat("\n<br>dim(cross_table_skeleton2)<br>\n")
								# print(dim(cross_table_skeleton2))
								# print(length(levels(modified_param_col)))
								# cat("\n<br> dim(obj$tables[[n]]$datatable)<br>\n")
								# print(dim(obj$tables[[n]]$datatable))
								# print(obj$tables[[n]]$datatable)
								# cat("\n<br>", "1 ====================", "<br>\n")
								
								if(dim(obj$tables[[n]]$datatable)[2] > length(levels(modified_param_col)))
								{
									obj$tables[[n]]$datatable = matrix(obj$tables[[n]]$datatable[,1:length(levels(modified_param_col))], ncol = length(levels(modified_param_col)))
								}
								else if(dim(obj$tables[[n]]$datatable)[2] < length(levels(modified_param_col)))
								{
									col_levels = levels(modified_param_col)
									filler_column = rep(" ",dim(obj$tables[[n]]$datatable)[1])
									temp_datatable = matrix(filler_column, ncol = 1)
										
									if(!is.null(obj$tables[[n]]$columnNames) && length(obj$tables[[n]]$columnNames) > 0 && length(obj$tables[[n]]$columnNames) < (length(col_levels) - 1))
									{	
										j = 1
										
										for(add_col in 1: (length(col_levels) - 1))
										{
											
											if(j <= length(obj$tables[[n]]$columnNames))
											{
												if(col_levels[add_col] != obj$tables[[n]]$columnNames[j])
												{
													temp_datatable = cbind(temp_datatable, filler_column)
												}
												else
												{
													temp_datatable = cbind(temp_datatable, obj$tables[[n]]$datatable[,j])
													j = j + 1
												}
											}
											else
											{
												temp_datatable = cbind(temp_datatable, filler_column)
											}
										}
										
										temp_datatable = cbind(temp_datatable, obj$tables[[n]]$datatable[,dim(obj$tables[[n]]$datatable)[2]])
										
										#return(list(obj$tables[[n]]$datatable, temp_datatable))
										obj$tables[[n]]$datatable = temp_datatable[,-c(1)]
									}
									else if(is.null(obj$tables[[n]]$columnNames))
									{
										filler_column = rep("X",dim(obj$tables[[n]]$datatable)[1])
											
										if(length(col_levels) > dim(obj$tables[[n]]$datatable)[2])
										{
											X_has_been_printed = TRUE
											
											for(add_col in 1: (length(levels(modified_param_col)) - dim(obj$tables[[n]]$datatable)[2]))
											{
												obj$tables[[n]]$datatable = cbind(obj$tables[[n]]$datatable, filler_column)
											}
										}
									}
								}
								
								# cat("\n<br> iteration num: ",n, "<br>\n")
								# cat("\n<br> dim(obj$tables[[n]]$datatable)<br>\n")
								# print(dim(obj$tables[[n]]$datatable))
								# print(obj$tables[[n]]$datatable)
								# # cat("\n<br>dim(cross_table_skeleton2)<br>\n")
								# # print(dim(cross_table_skeleton2))
								# cat("\n<br>", "2 ====================", "<br>\n")
								
								if((nrow(cross_table_skeleton2) -1) > dim(obj$tables[[n]]$datatable)[1])
								{
									filler_row = rep("X",dim(obj$tables[[n]]$datatable)[2])
									
									X_has_been_printed = TRUE
									
									for(add_row in 1: ((nrow(cross_table_skeleton2) -1 - dim(obj$tables[[n]]$datatable)[1])))
									{
										obj$tables[[n]]$datatable = rbind(obj$tables[[n]]$datatable, filler_row)
									}
								}
								else if((nrow(cross_table_skeleton2) -1) < dim(obj$tables[[n]]$datatable)[1])
								{
									obj$tables[[n]]$datatable = obj$tables[[n]]$datatable[1:nrow(cross_table_skeleton2)-1,]
								}
								
								#dimnames(obj$tables[[n]]$datatable)[[2]] = paste("C",seq(1:dim(obj$tables[[n]]$datatable)[2]), sep="")
								# return(list(cross_table_skeleton2, obj$tables[[n]]$datatable))
				
								# cat("\n<br> iteration num: ",n, "<br>\n")
								# cat("\n<br> dim(obj$tables[[n]]$datatable)<br>\n")
								# print(dim(obj$tables[[n]]$datatable))
								# print(obj$tables[[n]]$datatable)
								# cat("\n<br>dim(cross_table_skeleton2)<br>\n")
								# print(dim(cross_table_skeleton2))
								# print(cross_table_skeleton2)
								# cat("\n<br>", "3 ====================", "<br>\n")
								
								########################################################################
								# Need to fix this in BSkyFormat after BSkyFormat2 returns so all the whole number can be 
								# restored by removing decimal point along with trailing zeros  
								########################################################################
								#obj$tables[[1]]$datatable = BSkyDecimalDigitFormating(obj$tables[[1]]$datatable, decimalDigits = 0)
								
								cross_table_skeleton2[c(2:nrow(cross_table_skeleton2)),c((ncol(cross_table_skeleton2)- dim(obj$tables[[n]]$datatable)[2] + 1): ncol(cross_table_skeleton2))] = obj$tables[[n]]$datatable
							
								# cat("\n<br> iteration num: ",n, "<br>\n")
								# cat("\n<br>dim(cross_table_skeleton2)<br>\n")
								# print(dim(cross_table_skeleton2))
								# print(cross_table_skeleton2)
								# print(obj$tables[[n]]$datatable)
								# cat("\n<br>", "4 ====================", "<br>\n")
							}
							
							if(dim(cross_table_skeleton2)[1] > 0 && dim(cross_table_skeleton2)[2] > 0)
							{
								#Convert to matrix to get rid of row numbers - looks there is no way to get rid of rown numbers for data.frame - strange!!
								cross_table_skeleton3 = as.matrix(cross_table_skeleton2)
								dimnames(cross_table_skeleton3)[[2]] = names(cross_table_skeleton2)
								row.names(cross_table_skeleton3) = c()
								
								#cat("<br> Iteration number: ", n, "<br>")
								#return(cross_table_skeleton3)
								
								if(obj$BSkySplit == 1)
								{
									#table_list_names = c(table_list_names, paste("Multiway Cross Table -", obj$tables[[n]]$cartlevel))
									
									#attr(cross_table_skeleton3, "BSkyFootnote_BSkySplit") = substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel))
									attr(cross_table_skeleton3, "BSkyFootnote_BSkySplit") = paste("Split:", strsplit(substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel)),",")[[1]][2], sep="")
								
									split_iteration_headline_str = paste("Begins Split:", strsplit(substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel)),",")[[1]][2], sep="")
								
									split_iteration_headline = matrix(split_iteration_headline_str, ncol=1)
									dimnames(split_iteration_headline)[[1]] = NULL
									dimnames(split_iteration_headline)[[2]] = NULL
									
									table_list_names = c(table_list_names, "Split Headline")
									table_list = c(table_list, list(split_iteration_headline))
									names(table_list) = table_list_names
									
									table_list_names = c(table_list_names, "Multiway Cross Table")
								}
								else
								{
									table_list_names = c(table_list_names, "Multiway Cross Table")
								}
								
								if(X_has_been_printed == TRUE)
								{
									attr(cross_table_skeleton3, "BSkyFootnote_BSkyXExplain") = c("X indicates incomplete result due to errors")
								}
								
								num_additional_info = dim(obj$tables[[n]]$metadatatable[[1]])[1]
								
								if(num_additional_info > 0)
								{
									index = 0 
									
									for(addl_msg in 1:num_additional_info)
									{
										if(obj$tables[[n]]$metadatatable[[1]]$type[addl_msg] == 2)
										{
											index = index + 1
											
											if(!is.na(obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg]))
											{
												additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " BSky Msg: ", obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg], sep="")
												attr(cross_table_skeleton3, paste("BSkyFootnote_BSkyAppMsg_",index, sep="")) = additional_info_str
											}
											
											if(!is.na(obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg]))
											{
												additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " R Msg: ", obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg], sep="")
												attr(cross_table_skeleton3, paste("BSkyFootnote_BSkyRMsg_",index, sep="")) = additional_info_str
											}
										}
									}
								}
								
								######################################################
								# SK - Do not forget to remove before the final builld 
								######################################################
								# if(!is.null(obj$tables[[n]]$columnNames))
								# {
									# col_filter_hint = paste(obj$tables[[n]]$columnNames, collapse=",")
									# data_table_dim = paste("Datatable Dim: ", orig_datatable_dim[1], "x", orig_datatable_dim[2], sep="")
									# row_filter_hint = paste("Number of non zero row hint: ", length(obj$tables[[n]]$metadatatable[[2]][,2][(obj$tables[[n]]$metadatatable[[2]][,2] == 1)]))
									
									# attr(cross_table_skeleton3, "BSkyFootnote_Debug_info") = paste("BSky Debug_info: ", "Col filter hint: ", col_filter_hint, data_table_dim, row_filter_hint)
								# }
								
								table_list = c(table_list, list(cross_table_skeleton3))
								names(table_list) = table_list_names
								
								######################################################
								# SK - Do not forget toremove before the final builld 
								######################################################
								# if(length(obj$tables[[n]]$metadatatable) > 2 && !is.null(obj$tables[[n]]$metadatatable[[3]]) && dim(obj$tables[[n]]$metadatatable[[3]])[1] > 0 && !is.null(param_layers_values_df) && (nrow(param_layers_values_df) > dim(obj$tables[[n]]$metadatatable[[3]])[1]) )
								# {
									# table_list = c(table_list, list(obj$tables[[n]]$metadatatable[[3]]))
									# table_list_names = c(table_list_names, "BSky Debug Info: Non Zero Layer Tuple Table")
									# names(table_list) = table_list_names
								# }
							}
						}
					
						#if(!is.null(obj$tables[[n]]$datatable))
						{
							#increase the table counter 
							n = n + 1
						}
						
							
						####################################################################
						#"Pearson Chi Square" "Mcnemar's test" "Fisher's test"
						####################################################################
						if(n <= num_tables && additional_tests_count > 0 && obj$tables[[n]]$nometadatatables == 2 && !is.null(obj$tables[[n]]$datatable) && dim(obj$tables[[n]]$datatable)[1] > 0 && dim(obj$tables[[n]]$datatable)[2] > 0)
						{	
							X_has_been_printed = FALSE
							
							additional_tests = additional_tests_orig
							additional_tests_levels = additional_tests
						
							additional_tests = c(additional_tests, rep(additional_tests[1], dataset_nrow - length(additional_tests)))
							
							additional_tests_ordered_levels = factor(additional_tests, ordered = TRUE, levels = additional_tests_levels)
					  
							##################################################################################
							# Remove the combination of layer values that have only zero counts
							##################################################################################
							
							if(length(param_layers) > 0 || ( length(param_layers) == 1 && param_layers != "NA"))
							{
								param_layers_values = c()
								
								for (i in 1:length(param_layers))
								{
									param_layers_values = paste(param_layers_values, param_layers[i], "=", database_name, "$", param_layers[i], sep = "")
									
									if( i != length(param_layers))
									{
										param_layers_values = paste(param_layers_values, ",")
									}
								}
								
								tests_cross_table_skeleton_string = paste("test = additional_tests_ordered_levels,", "test_dummy_col = additional_tests_ordered_levels,", param_layers_values) 
								tests_cross_table_skeleton_string = paste("table(", tests_cross_table_skeleton_string, ")")
								tests_cross_table_skeleton_non_formatted = eval(parse(text= tests_cross_table_skeleton_string))
								tests_cross_table_skeleton = BSkyTableObjectFormat(tests_cross_table_skeleton_non_formatted)
								
								
								param_layers_values = paste("table(", param_layers_values, ")")
							
								param_layers_values_df = (as.data.frame(eval(parse(text= param_layers_values)),stringsAsFactors = FALSE))[length(param_layers):1]
								
								#return(list(tests_cross_table_skeleton,param_layers_values_df))
								
								# cat("\n<br> Iteration num :", n, "<br>\n")
								# print(param_layers)
								# print(param_layers_values)
								# print(param_layers_values_df)
								# print(obj$tables[[n]]$metadatatable[[2]])
								
								# cat("\n<br>\n")
								# print(obj$tables$metadatatable)
								# cat("<br> rev(param_layers) :", rev(param_layers), "<br>")
								#return(param_layers_values_df)
								
								if(length(obj$tables[[n]]$metadatatable) > 1 && !is.null(obj$tables[[n]]$metadatatable[[2]]) && dim(obj$tables[[n]]$metadatatable[[2]])[1] > 0 && nrow(param_layers_values_df) > dim(obj$tables[[n]]$metadatatable[[2]])[1] && ncol(param_layers_values_df) == dim(obj$tables[[n]]$metadatatable[[2]])[2])
								{
									#number_of_rows_per_layer_value_combination = eval(parse(text= paste("length(levels(", database_name, "$", param_row,"))", sep = ""))) * (num_count_elements +1) + ((num_count_elements + 1) - num_extra_count_elements)
									
									number_of_rows_per_layer_value_combination = additional_tests_count
									
									start_row_num = c()
									end_row_num = c()
									start_row = 1
									
									if(dim(obj$tables[[n]]$metadatatable[[2]])[2] == length(rev(param_layers)))
									{
										dimnames(obj$tables[[n]]$metadatatable[[2]])[[2]] = rev(param_layers)
									}
									
									row.names(obj$tables[[n]]$metadatatable[[2]]) = c()
									
									
									to_be_deleted_row_indices = c()
									
									for (i in 1:nrow(param_layers_values_df))
									{
										found_all_layer_value_match = FALSE
										
										for(j in 1: dim(obj$tables[[n]]$metadatatable[[2]])[1])
										{
											#if(all(param_layers_values_df[i,] %in% obj$tables[[2]]$metadatatable[[2]][j,]) == TRUE)
											cell_matched = 0
											
											for(m in 1:ncol(param_layers_values_df))
											{
												if(m <= dim(obj$tables[[n]]$metadatatable[[2]])[2])
												{
													if(as.character(param_layers_values_df[i,m]) == as.character(obj$tables[[n]]$metadatatable[[2]][j,m]))
													{
														cell_matched = cell_matched + 1
													}
												}
											}
											
											if(cell_matched == ncol(param_layers_values_df))
											{
												# cat("<br>")
												# print("Found")
												# cat("<br>")
												# print(as.character(obj$tables[[2]]$metadatatable[[2]][j,]))
												# cat("<br>")
												# print(i)
												# cat("<br>")
												
												found_all_layer_value_match = TRUE
											}
										}
										
										start_row_num = c(start_row_num, start_row)
										end_row_num = c(end_row_num, start_row + number_of_rows_per_layer_value_combination - 1)
										
										if(found_all_layer_value_match == FALSE)
										{
											to_be_deleted_row_indices = c(to_be_deleted_row_indices, c((start_row + 1):(start_row + 1 + number_of_rows_per_layer_value_combination - 1)))
											
												# cat("<br>")
												# print("start_row")
												# print(start_row)
												# cat("<br>")
												# print("number_of_rows_per_layer_value_combination")
												# print(number_of_rows_per_layer_value_combination)
												# cat("<br>")
											
											if(length(param_layers) > 1)
											{
												for(k in 1: (length(param_layers) - 1))
												{
													if(trimws(tests_cross_table_skeleton[start_row + 1,k]) != "")
													{
														if((start_row + 1 + number_of_rows_per_layer_value_combination) <= nrow(tests_cross_table_skeleton))
														{
															if(trimws(tests_cross_table_skeleton[(start_row + 1 + number_of_rows_per_layer_value_combination),k]) == "")
															{
																tests_cross_table_skeleton[(start_row + 1 + number_of_rows_per_layer_value_combination),k] = tests_cross_table_skeleton[start_row + 1, k]
															}
														}
													}
												}
											}
										}
										
										start_row = start_row + number_of_rows_per_layer_value_combination
									}
									
									# cat("<br>")
									# print("to_be_deleted_row_indices")
									# cat(to_be_deleted_row_indices)
									# cat("<br>")
									
									param_layers_values_df = cbind(param_layers_values_df, start_row_number = start_row_num, end_row_number = end_row_num)
									#tests_cross_table_skeleton = tests_cross_table_skeleton[- as.numeric(to_be_deleted_row_indices),] #for some odd reason this syntax did not work
									
									new_temp_table = c()
									for(i in 1:nrow(tests_cross_table_skeleton))
									{
										if(!(i %in% as.numeric(to_be_deleted_row_indices)))
										{
											new_temp_table = rbind(new_temp_table, tests_cross_table_skeleton[i,])
										}
									}
									
									names(new_temp_table) = names(tests_cross_table_skeleton)
									
									#return(list(param_layers_values_df, tests_cross_table_skeleton, new_temp_table))
									
									tests_cross_table_skeleton = new_temp_table
									
									# cat("\n<br>======================<br>\n")
									# print(nrow(tests_cross_table_skeleton))
									# print(nrow(obj$tables[[2]]$datatable))
									# #print(tests_cross_table_skeleton)
									# cat("\n<br>======================<br>\n")
									
									#return(list(param_layers_values_df, tests_cross_table_skeleton))
								}
								
								tests_cross_table_skeleton = tests_cross_table_skeleton[-c(1), 1:(length(param_layers)+1)]
								
								if(!is.null(obj$tables[[n]]$datatable) && dim(obj$tables[[n]]$datatable)[1] > 0 && dim(obj$tables[[n]]$datatable)[2] > 0)
								{
									if(nrow(tests_cross_table_skeleton) == dim(obj$tables[[n]]$datatable)[1])
									{
										tests_cross_table_skeleton = cbind(tests_cross_table_skeleton, obj$tables[[n]]$datatable)
									}
									else if(nrow(tests_cross_table_skeleton) > dim(obj$tables[[n]]$datatable)[1])
									{
										filler_row = rep("X",ncol(obj$tables[[n]]$datatable))
										
										X_has_been_printed = TRUE
										
										for(add_row in 1: (nrow(tests_cross_table_skeleton) - dim(obj$tables[[n]]$datatable)[1]))
										{
											obj$tables[[n]]$datatable = rbind(obj$tables[[n]]$datatable, filler_row)
										}
										
										tests_cross_table_skeleton = cbind(tests_cross_table_skeleton, obj$tables[[n]]$datatable)
									}
									else if(nrow(tests_cross_table_skeleton) < dim(obj$tables[[n]]$datatable)[1])
									{
										obj$tables[[n]]$datatable = matrix(obj$tables[[n]]$datatable[1:nrow(tests_cross_table_skeleton),], ncol = dim(obj$tables[[n]]$datatable)[2])
										tests_cross_table_skeleton = cbind(tests_cross_table_skeleton, obj$tables[[n]]$datatable)
									}
									
									if(dim(obj$tables[[n]]$datatable)[2] < 6)
									{
										filler_column = rep("X",nrow(tests_cross_table_skeleton))
										
										X_has_been_printed = TRUE
										
										for(add_col in 1: (6 - dim(obj$tables[[n]]$datatable)[2]))
										{
											tests_cross_table_skeleton = cbind(tests_cross_table_skeleton, filler_column)
										}
									}
									else if(dim(obj$tables[[n]]$datatable)[2] > 6)
									{
										tests_cross_table_skeleton = tests_cross_table_skeleton[,1:(ncol(tests_cross_table_skeleton) - (dim(obj$tables[[n]]$datatable)[2] - 6))]
									}
								}
								
								
								tests_cross_table_skeleton = as.matrix(tests_cross_table_skeleton)
								dimnames(tests_cross_table_skeleton)[[2]] = c(rev(param_layers), "Test", "Value", "df", "Asyp. Sig", "Odds ratio", "95% Confidence interval", "95% Confidence interval")
								row.names(tests_cross_table_skeleton) = c()
								
							}
							else
							{
								if(!is.null(obj$tables[[n]]$datatable) && dim(obj$tables[[n]]$datatable)[1] > 0 && dim(obj$tables[[n]]$datatable)[2] > 0)  
								{
									tests_cross_table_skeleton = matrix(additional_tests_levels, ncol = 1)
									
									#tests_cross_table_skeleton = cbind(tests_cross_table_skeleton, obj$tables[[n]]$datatable)
									
									if(nrow(tests_cross_table_skeleton) == dim(obj$tables[[n]]$datatable)[1])
									{
										tests_cross_table_skeleton = cbind(tests_cross_table_skeleton, obj$tables[[n]]$datatable)
									}
									else if(nrow(tests_cross_table_skeleton) > dim(obj$tables[[n]]$datatable)[1])
									{
										filler_row = rep("X", dim(obj$tables[[n]]$datatable)[2])
										
										X_has_been_printed = TRUE
										
										for(add_row in 1: (nrow(tests_cross_table_skeleton) - dim(obj$tables[[n]]$datatable)[1]))
										{
											obj$tables[[n]]$datatable = rbind(obj$tables[[n]]$datatable, filler_row)
										}
										
										tests_cross_table_skeleton = cbind(tests_cross_table_skeleton, obj$tables[[n]]$datatable)
									}
									else if(nrow(tests_cross_table_skeleton) < dim(obj$tables[[n]]$datatable)[1])
									{
										obj$tables[[n]]$datatable = matrix(obj$tables[[n]]$datatable[1:nrow(tests_cross_table_skeleton),], ncol = dim(obj$tables[[n]]$datatable)[2])
										tests_cross_table_skeleton = cbind(tests_cross_table_skeleton, obj$tables[[n]]$datatable)
									}
									
									if(dim(obj$tables[[n]]$datatable)[2] < 6)
									{
										filler_column = rep("X",nrow(tests_cross_table_skeleton))
										
										X_has_been_printed = TRUE
										
										for(add_col in 1: (6 - ncol(obj$tables[[n]]$datatable)))
										{
											tests_cross_table_skeleton = cbind(tests_cross_table_skeleton, filler_column)
										}
									}
									else if(dim(obj$tables[[n]]$datatable)[2] > 6)
									{
										tests_cross_table_skeleton = tests_cross_table_skeleton[,1:(ncol(tests_cross_table_skeleton) - (dim(obj$tables[[n]]$datatable)[2] - 6))]
									}
									
									#return(list(tests_cross_table_skeleton, obj$tables[[2]]$datatable))
									
									dimnames(tests_cross_table_skeleton)[[2]] = c("Tests", "Value", "df", "Asyp. Sig", "Odds ratio", "95% Confidence interval", "95% Confidence interval")
								}
							}
							
							if(obj$BSkySplit == 1)
							{
								table_list_names = c(table_list_names, "Statistical Tests")
								#table_list_names = c(table_list_names, paste("Multiway Cross Table -", obj$tables[[n]]$cartlevel))
								
								#attr(tests_cross_table_skeleton, "BSkyFootnote_BSkySplit") = substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel))
								attr(tests_cross_table_skeleton, "BSkyFootnote_BSkySplit") = paste("Split:", strsplit(substr(obj$tables[[n]]$cartlevel, 12, nchar(obj$tables[[n]]$cartlevel)),",")[[1]][2], sep="")
							}
							else
							{
								table_list_names = c(table_list_names, "Statistical Tests")
							}
							
							if(X_has_been_printed == TRUE)
							{
								attr(tests_cross_table_skeleton, "BSkyFootnote_BSkyXExplain") = c("X indicates incomplete result due to errors")
							}
							
							num_additional_info = dim(obj$tables[[n]]$metadatatable[[1]])[1]
								
							if(num_additional_info > 0)
							{
								index = 0 
								
								for(addl_msg in 1:num_additional_info)
								{
									if(obj$tables[[n]]$metadatatable[[1]]$type[addl_msg] == 2)
									{
										index = index + 1
										
										if(!is.na(obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg]))
										{
											additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " BSky Msg: ", obj$tables[[n]]$metadatatable[[1]]$BSkyMsg[addl_msg], sep="")
											attr(tests_cross_table_skeleton, paste("BSkyFootnote_BSkyAppMsg_",index, sep="")) = additional_info_str
										}
										
										if(!is.na(obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg]))
										{
											additional_info_str = paste("Row: ", obj$tables[[n]]$metadatatable[[1]]$dataTableRow[addl_msg], " R Msg: ", obj$tables[[n]]$metadatatable[[1]]$RMsg[addl_msg], sep="")
											attr(tests_cross_table_skeleton, paste("BSkyFootnote_BSkyRMsg_",index, sep="")) = additional_info_str
										}
									}
								}
							}
								
							table_list = c(table_list, list(tests_cross_table_skeleton))
							names(table_list) = table_list_names
						}

						
						if(additional_tests_count > 0 && obj$tables[[n]]$nometadatatables == 2)
						{
							#increase the table counter 
							n = n + 1
						}
					}
					else
					{
						#increase the table counter
						n = n + 1
					}
				}
					
				################################################################
				# Add back the BSky error/warning table from the return structure
				################################################################
				
				table_list = c(table_list, list(obj$tables[[obj$nooftables]]))
				table_list_names = c(table_list_names, "")
				names(table_list) = table_list_names
				
				obj$tables = table_list
				obj$nooftables = length(obj$tables)
		   }
		}
	}
	
	return(invisible(obj))
}








#17May2021
BSkyFormatBSkyFunctionParamParsing <- function(functionCallString=c(), paramName = c())
{
	paramName = paste("(\\b", paramName, "\\b)", sep="")
	
	paramNamePosition = regexpr(paramName, functionCallString)
	endParamValuePosition = regexpr(")", substr(functionCallString,(paramNamePosition+attr(paramNamePosition,"match.length")), nchar(functionCallString)))
	paramValuesString = substr(functionCallString, paramNamePosition+attr(paramNamePosition,"match.length"), paramNamePosition+attr(paramNamePosition,"match.length")+endParamValuePosition-1)
	  
	paramValuesString = gsub(" ","", paramValuesString)
	paramValuesString = gsub("\'","", paramValuesString)
	paramValuesString = gsub("\"","", paramValuesString)
	  
	paramValueList = strsplit(strsplit(strsplit(gsub("(\\(|\\))+","X", paramValuesString), "X")[[1]][2],",")[[1]], ",")
	
	return(invisible(unlist(paramValueList)))
}


#25Sep2021
BSkyEvalRcommand <- function(RcommandString, numExprParse = -1, selectionStartpos = 0, selectionEndpos = 0, executeSelectOnly = FALSE, currentDatasetName = BSkyGetCurrentDatabaseName(), replaceOldDatasetName = c(), currentColumnNames = c(), replaceOldColumnNames = c(), echo = BSkyGetRCommandDisplaySetting(), echoInline = BSkyGetRCommandDisplaySetting(), ignoreSplitOn = FALSE, graphicsDir = BSkyGetGraphicsDirPath(), bskyEvalDebug = FALSE, splitCountDisplay = BSkyGetSplitCountDisplaySetting())
{
	if(bskyEvalDebug == TRUE)
	{
		cat("callStackIndex and callStack\n")
		print(uadatasets.sk$callStackIndex)
		print(uadatasets.sk$callStack)
		
		cat("\nParameters passed to BSKyEval function\n")
		print(match.call())
		cat("\n")
		
		cat("\nR command(s) passed\n")
		
		line_breakdown_RcommandString = data.frame(strsplit(RcommandString, "\n"))
		line_breakdown_RcommandString = cbind(line_breakdown_RcommandString, lapply(line_breakdown_RcommandString, nchar))
		line_breakdown_RcommandString[,2] = line_breakdown_RcommandString[,2] + 1
		line_breakdown_RcommandString = cbind(line_breakdown_RcommandString, cumsum(line_breakdown_RcommandString[,2]))
		line_breakdown_RcommandString = cbind(seq(1:nrow(line_breakdown_RcommandString)), line_breakdown_RcommandString)
		names(line_breakdown_RcommandString) = c("lineNum", "lineTxt", "lineTxtCharCount", "lineTxtCumCharCount")
		
		BSkyFormat(line_breakdown_RcommandString)
	}
	
	uadatasets.sk$BSkyEvalErrors = 0
	# just in case as a safety net any uncleaned callstack leftover from the previous BSkyEvalRcommand run 
	uadatasets.sk$callStack <- NULL
	uadatasets.sk$callStackIndex = 0
	
	# if(numExprParse == 0)
	# {
		# return(invisible(BSkyRCommandParsedCharCount(RcommandString = RcommandString, numExprParse = numExprParse)))
	# }
			
	if(ignoreSplitOn == FALSE)
	{
		BSkyFunctionInit()
	}
	
		bsky_Rmarkdown_settings = BSkyGetKableAndRmarkdownFormatting()
		
		RcommandString_before_any_modification = RcommandString
		
		if(selectionStartpos> 0 && (selectionEndpos > nchar(RcommandString) || selectionEndpos == 0 || selectionEndpos < selectionStartpos ))
		{
			selectionEndpos = nchar(RcommandString)
		}
		
		if(length(RcommandString) > 0) # && (numExprParse > -1 || selectionStartpos > 0 || selectionEndpos > 0))
		{	
			charPosOffsetAdjutment = 0
			linePosOffsetAdjutment = 1
			
			if(selectionStartpos> 0)
			{
				linePosOffsetAdjutment = BSkyRCommandLineNumberFromCharCount(RcommandString_before_any_modification, selectionStartpos)
				
				selection_only_first_expr_found = FALSE
				RcommandStringSelect_parse_test = -1
				
				if(executeSelectOnly == FALSE)
				{
					RcommandStringSelect = substr(RcommandString, selectionStartpos, selectionEndpos)
					
					#If seelction is parse-able, just execute the selection - do not expand the selection boundary  
					RcommandStringSelect_parse_test = BSkyRCommandParsingTest(RcommandString = RcommandStringSelect, numExprParse = numExprParse, bskyEvalDebug = bskyEvalDebug)
					
					if(RcommandStringSelect_parse_test == 0)
					{
						find_first_expression = BSkyRCommandParsedExprBoundary(RcommandString = RcommandStringSelect, numExprParse = numExprParse , selectionStartpos = 0, selectionEndpos = 0, linePosOffsetAdjutment = linePosOffsetAdjutment, bskyEvalDebug = bskyEvalDebug)
					
						#if(find_first_expression$parsingStatus == 0 && find_first_expression$firstExprStartPos > 0 && find_first_expression$lastExprEndPos > 0)
						if(find_first_expression$firstExprStartPos > 0 && find_first_expression$lastExprEndPos > 0)
						{
							RcommandString = substr(RcommandString, selectionStartpos, selectionEndpos)
							charPosOffsetAdjutment = selectionStartpos
							selectionStartpos = 0
							selectionEndpos = 0
							
							selection_only_first_expr_found = TRUE
						}
					}
					
					if(selection_only_first_expr_found == FALSE)
					{
						if(RcommandStringSelect_parse_test == 0)
						{
							find_first_expression = BSkyRCommandParsedExprBoundary(RcommandString = substr(RcommandString, selectionStartpos, nchar(RcommandString)), numExprParse = 1 , selectionStartpos = 0, selectionEndpos = 0, linePosOffsetAdjutment = linePosOffsetAdjutment, bskyEvalDebug = bskyEvalDebug)
							find_first_expression$firstExprStartPos = find_first_expression$firstExprStartPos + selectionStartpos - 1
							find_first_expression$lastExprEndPos = find_first_expression$lastExprEndPos + selectionStartpos - 1
						}
						else
						{
							find_first_expression = BSkyRCommandParsedExprBoundary(RcommandString = RcommandString, numExprParse = 1 , selectionStartpos = selectionStartpos, selectionEndpos = 0, linePosOffsetAdjutment = 1, bskyEvalDebug = bskyEvalDebug)
						}
						
						if(bskyEvalDebug == TRUE)
						{
							cat("\n Printitng find_first_expression returned by BSkyRCommandParsedExprBoundary in BSkyEval\n")
							print(nchar(RcommandString))
							#print(RcommandString)
							print(find_first_expression)
						}
						
						if(find_first_expression$parsingStatus == -1)
						{
							if(ignoreSplitOn == FALSE)
							{
								BSkyFunctionWrapUp()
							}
							
							return(invisible(list(executionStatus = find_first_expression$parsingStatus, parsingStatus = find_first_expression$parsingStatus, parsingErrorLineNum = find_first_expression$parsingErrorLineNum, totalCharCount = find_first_expression$totalCharCount, firstExprStartPos = find_first_expression$firstExprStartPos + 1, lastExprEndPos = find_first_expression$lastExprEndPos, parsedCommandList= find_first_expression$parsedCommandList)))
						}
						else
						{
							if(selectionStartpos != find_first_expression$firstExprStartPos || selectionEndpos != find_first_expression$lastExprEndPos)
							{
								# Count additional characters till the end of line to expand the find_first_expression$lastExprEndPos
								
								additional_char_count = 0
								
								if(find_first_expression$lastExprEndPos < nchar(RcommandString) && (substr(RcommandString,find_first_expression$lastExprEndPos, find_first_expression$lastExprEndPos) != '\n' && substr(RcommandString,find_first_expression$lastExprEndPos, find_first_expression$lastExprEndPos) != '\r'))
								{
									for(j in (find_first_expression$lastExprEndPos+1):nchar(RcommandString))
									{
										if(substr(RcommandString, j, j) != '\n' && substr(RcommandString, j, j) != '\r' )
										{
											additional_char_count = additional_char_count + 1
										}
										else
										{
											break 
										}
									}
								}
								
								# if(selectionStartpos > find_first_expression$firstExprStartPos)
								# {
									# selectionStartpos = find_first_expression$firstExprStartPos
								# }
								
								if(selectionEndpos < (find_first_expression$lastExprEndPos + additional_char_count))
								{
									selectionEndpos = find_first_expression$lastExprEndPos + additional_char_count
								}
							}
						}
						
						if(RcommandStringSelect_parse_test == 0 || find_first_expression$parsingStatus == 0)
						{
							if(selectionStartpos > find_first_expression$firstExprStartPos)
							{
								selectionStartpos = find_first_expression$firstExprStartPos
								linePosOffsetAdjutment = BSkyRCommandLineNumberFromCharCount(RcommandString_before_any_modification, selectionStartpos)
							}
							
							RcommandString = substr(RcommandString, selectionStartpos, nchar(RcommandString))
							charPosOffsetAdjutment = selectionStartpos
							selectionEndpos = selectionEndpos - selectionStartpos + 1
							selectionStartpos = 0
						}
					}
				}
			}
			
			
			if(executeSelectOnly == TRUE && selectionStartpos > 0 && selectionEndpos > 0)
			{
				RcommandString = substr(RcommandString, selectionStartpos, selectionEndpos)
				charPosOffsetAdjutment = selectionStartpos
				selectionStartpos = 0
				selectionEndpos = 0
			}
			
			no_expresson_to_execute = FALSE
			
			Rcommands_initial_parse = BSkyRCommandParsedExprBoundary(RcommandString = RcommandString, numExprParse = numExprParse , selectionStartpos = selectionStartpos, selectionEndpos = selectionEndpos, linePosOffsetAdjutment = linePosOffsetAdjutment, bskyEvalDebug = bskyEvalDebug)
			
			if(bskyEvalDebug == TRUE)
			{
				cat("\n Printitng Rcommands_initial_parse returned by BSkyRCommandParsedExprBoundary in BSkyEval\n")
				print(nchar(RcommandString))
				print(RcommandString)
				print(Rcommands_initial_parse)
			}
			
			#if(Rcommands_initial_parse$parsingStatus == 0 && Rcommands_initial_parse$firstExprStartPos > 0 && Rcommands_initial_parse$lastExprEndPos > 0)
			if(Rcommands_initial_parse$firstExprStartPos > 0 && Rcommands_initial_parse$lastExprEndPos > 0)
			{
				RcommandString = substr(RcommandString, Rcommands_initial_parse$firstExprStartPos, Rcommands_initial_parse$lastExprEndPos)
			}
			else
			{
				no_expresson_to_execute = TRUE
			}
		}
		else
		{
			if(ignoreSplitOn == FALSE)
			{
				BSkyFunctionWrapUp()
			}
				
			return(invisible(list(executionStatus = -1, parsingStatus = -1, parsingErrorLineNum = -1, totalCharCount = 0, firstExprStartPos = 0, lastExprEndPos = 0, parsedCommandList=c())))
		}
		
		if(charPosOffsetAdjutment > 1)
		{
			Rcommands_initial_parse$totalCharCount = Rcommands_initial_parse$totalCharCount + charPosOffsetAdjutment -1
			Rcommands_initial_parse$firstExprStartPos = Rcommands_initial_parse$firstExprStartPos + charPosOffsetAdjutment -1
			Rcommands_initial_parse$lastExprEndPos = Rcommands_initial_parse$lastExprEndPos + charPosOffsetAdjutment -1
		}
		
		RcommandString_before_any_modification_length = nchar(RcommandString_before_any_modification)
		
		if(Rcommands_initial_parse$lastExprEndPos > 0 && Rcommands_initial_parse$lastExprEndPos < RcommandString_before_any_modification_length )
		{
			newline_count = 0
			
			if(bskyEvalDebug == TRUE)
			{
				cat("\n prnting ASCII value of the trainling character to skip \\n, \\r and blanks\n")
			}
			
			for(i in (Rcommands_initial_parse$lastExprEndPos + 1):RcommandString_before_any_modification_length)
			{
				if(bskyEvalDebug == TRUE)
				{
					cat(charToRaw(substr(RcommandString_before_any_modification, i, i)))
					cat("\n")
				}
				
				if(substr(RcommandString_before_any_modification, i, i) == '\n' || substr(RcommandString_before_any_modification, i, i) == '\r' || trimws(substr(RcommandString_before_any_modification, i, i)) == "")
				{
					newline_count = newline_count + 1
				}
				else
				{
					break 
				}
			}
			
			Rcommands_initial_parse$totalCharCount = Rcommands_initial_parse$totalCharCount + newline_count
			Rcommands_initial_parse$lastExprEndPos = Rcommands_initial_parse$lastExprEndPos + newline_count
		}
		
		# if comments or other statements are the only one passed - then parse() will return empty expression() - so nothing to execute
		if(no_expresson_to_execute == TRUE)
		{
			Rcommands_initial_parse$firstExprStartPos = Rcommands_initial_parse$firstExprStartPos + 1
			
			if(Rcommands_initial_parse$parsingStatus == -1)
			{
				if(ignoreSplitOn == FALSE)
				{
					BSkyFunctionWrapUp()
				}
				
				#return(invisible(Rcommands_initial_parse))
				return(invisible(list(executionStatus = Rcommands_initial_parse$parsingStatus, parsingStatus = Rcommands_initial_parse$parsingStatus, parsingErrorLineNum = Rcommands_initial_parse$parsingErrorLineNum, totalCharCount = Rcommands_initial_parse$totalCharCount, firstExprStartPos = Rcommands_initial_parse$firstExprStartPos, lastExprEndPos = Rcommands_initial_parse$lastExprEndPos, parsedCommandList= Rcommands_initial_parse$parsedCommandList)))
			}
		}
		
		
		
		if(is.null(currentColumnNames) || trimws(currentColumnNames) == "")
		{
			currentColumnNames = c() 
		}
		
		if(is.null(replaceOldColumnNames) || trimws(replaceOldColumnNames) == "")
		{
			replaceOldColumnNames = c() 
		}
		
		# the following needed for C# and new Electron app and not for Rstudio markdown 
		if(!is.null(graphicsDir) && length(graphicsDir) > 0 && trimws(graphicsDir) != "" && dir.exists(graphicsDir) && bsky_Rmarkdown_settings$doRmarkdownFormatting == FALSE)
		{
			full_file_names = list.files(path = graphicsDir, pattern="png|svg", full.names = TRUE)
			
			# capture the file name with path for the very first graphics device file open automatically 
			uadatasets.sk$initial_graphics_file_name = full_file_names[which.max(file.mtime(full_file_names))]
			
			#uadatasets.sk$initial_graphics_file_mtime = file.mtime(uadatasets.sk$initial_graphics_file_name) 
			
			uadatasets.sk$strating_count_of_bsky_graphics_files = length(full_file_names) #0
			uadatasets.sk$last_count_of_bsky_graphics_files = uadatasets.sk$strating_count_of_bsky_graphics_files
			
			#inserting a dummy plot graphics into the already open first graphics device image file 
			plot.new()
		}
	
		if(class(echo)[1] == "list")
		{
			echoRcommand = echo$echo
		}
		else
		{
			echoRcommand = echo  # means echo is paased as boolean - TRUE or FALSE
		}
		
		if(class(echoInline)[1] == "list")
		{
			echoInlineRcommand = echoInline$echoInline
		}
		else
		{
			echoInlineRcommand = echoInline # means echoInline is paased as boolean - TRUE or FALSE
		}
		
		
		if(!is.null(currentDatasetName) && length(currentDatasetName) > 0 && trimws(currentDatasetName) != "")
		{
			BSkySetCurrentDatasetName(currentDatasetName, setDatasetIndex = "y")
			working_datasetName = currentDatasetName
			
			if(!is.null(replaceOldDatasetName) && length(replaceOldDatasetName) > 0 && trimws(replaceOldDatasetName) != "")
			{														
				RcommandString_modified = BSkyDatasetNameSubstitute(datasetName = replaceOldDatasetName, toDatasetName = currentDatasetName, replaceOldColumnNames = replaceOldColumnNames, currentColumnNames = currentColumnNames, RcommandString = RcommandString)
			}
			else
			{
				RcommandString_modified = RcommandString
			}
			
			# cat("<br>============<br>")
			# print(BSkyGetRCommandDisplaySetting())
			# cat("<br>")
			# print(uadatasets.sk$echoRcommand)
			# print(uadatasets.sk$echoInlineRcommand)
			# print(echoRcommand)
			# print(echoInlineRcommand)
			# cat("<br>============<br>")
			
			isSplitOn = uaIsDatasetSplit(working_datasetName)
		}
		else
		{
			RcommandString_modified = RcommandString
			isSplitOn = FALSE
		}
		
		if(isSplitOn == TRUE && ignoreSplitOn == FALSE)
		{
		
			if(echoRcommand == TRUE && echoInlineRcommand == FALSE)
			{
				cat("\n")
				
				if(bsky_Rmarkdown_settings$doRmarkdownFormatting == TRUE && bsky_Rmarkdown_settings$doLatexFormatting == FALSE)
				{
					 RcommandString_modified_print = gsub("\\n", "<br>", RcommandString_modified)
					 ## RcommandString_modified_print = gsub("$", "\\\\$", RcommandString_modified_print) #does not work to show $ in Rstudio html output
					 
					 cat("<pre class=\"r\"><code>")
					 cat(RcommandString_modified_print)
					 cat("<br>")
					 cat("</code></pre>")
					 
					 #cat("\n")
					 #print(tidy_source(text = RcommandString_modified_print)) #formatR package 
				}
				else
				{
					cat(RcommandString_modified)
				}
				
				cat("\n")
			}
			
			bSkyVarnamesSplit = dimnames(eval(parse(text=working_datasetName), envir=globalenv()))[[2]] #get all variable (i.e. column) names
		
			bSkySplitIterationCount = BSkyComputeSplitdataset(bSkyVarnamesSplit, working_datasetName)
			
			splitDatasetName = "uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]]"
			
			if(!is.null(replaceOldDatasetName) && length(replaceOldDatasetName) > 0 && trimws(replaceOldDatasetName) != "")
			{																	   
				RcommandString_modified_split_dataset = BSkyDatasetNameSubstitute(datasetName = replaceOldDatasetName, toDatasetName = splitDatasetName, replaceOldColumnNames = replaceOldColumnNames, currentColumnNames = currentColumnNames, RcommandString = RcommandString, splitOn = TRUE, preSplitDatasetName = working_datasetName)
			}
			else
			{
				RcommandString_modified_split_dataset = BSkyDatasetNameSubstitute(datasetName = working_datasetName, toDatasetName = splitDatasetName, replaceOldColumnNames = replaceOldColumnNames, currentColumnNames = currentColumnNames, RcommandString = RcommandString, splitOn = TRUE, preSplitDatasetName = working_datasetName)
			}
			
			# cat("<pre class=\"r\"><code>")
			# cat("<br>============<br>")
			# print(bSkySplitIterationCount)
			# print(bSkyVarnamesSplit)
			# cat("\n<br>\n")
			# print(splitDatasetName)
			# cat("\n<br>\n")
			# print(RcommandString_modified_split_dataset)
			# cat("\n<br>\n")
			# cat("<br>============<br>")
			# cat("</code></pre>")
			
			for (bSkySplitIterationCounter in 1:bSkySplitIterationCount) 
			{
				#cat("\n +++++++++++++++++++++++++ Split Counter START :", bSkySplitIterationCounter, "++++++++++++++++++++++++ \n")
				bSkyDatasetSliceIndex = BSkyGetNextDatasetSliceIndex(bSkyVarnamesSplit, working_datasetName)
			
				bSkyGlobalDataSliceIndexToWorkOn = bSkyDatasetSliceIndex$datasetSliceIndex
				assign("bSkyGlobalDataSliceIndexToWorkOn", bSkyDatasetSliceIndex$datasetSliceIndex, envir = .GlobalEnv)	
				
				BSkySplit_footer_str = paste(BSkyComputeCurrentVarNamesAndFactorValues(working_datasetName), collapse = ",")
				BSkySplit_footer_str = strsplit(substr(BSkySplit_footer_str, 12, nchar(BSkySplit_footer_str)), ",")
				
				
				BSkyFormat_BSkySplit_footer_str =  trimws(paste("Split: ", paste(BSkySplit_footer_str[[1]][2:length(BSkySplit_footer_str[[1]])], collapse=",")))
				
				
				RcommandString_modified_split_footer = BSkyExpandBSkyFormatWithFooter(RcommandString_modified_split_dataset, BSkyFormat_BSkySplit_footer_str)
				
				# cat("\n Dataset Slice \n")
				# cat("\n Dim - ", dim(uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]]), "\n")
				# print(head(uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]]))
				# cat("\n")
				# print(BSkySplit_footer_str)
				# cat("\n")
				
				# Example slice dataset substitution 
				#"BSky_Independent_Samples_t_Test = t.test( mtcarsmodified$mpg, mtcarsmodified$hp, alternative='two.sided', conf.level=0.95, mu=0.0, var.equal=FALSE, data=mtcarsmodified)"
				# BSky_Independent_Samples_t_Test = t.test(uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]][,1], uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]][,2], alternative='two.sided', conf.level=0.95, mu=0.0, var.equal=FALSE, data=uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]])
				# RcommandString = "BSky_Independent_Samples_t_Test = t.test( mtcarsmodified$mpg, mtcarsmodified$hp, alternative='two.sided', conf.level=0.95, mu=0.0, var.equal=FALSE, data=mtcarsmodified) ; BSkyFormat(BSky_Independent_Samples_t_Test)"
				
				# print(RcommandString_modified_split_footer)
				# cat("\n")
				
				#RcommandString_modified = "print(bSkyGlobalDataSliceIndexToWorkOn); mydf = data.frame(A=c(1,2,3), B=c(\"a\",\"b\",\"c\")); BSkyFormat(mydf)"
				
				if(dim(uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]])[1] > 0 && dim(uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]])[2] > 0)
				{
					#cat("\n",BSkySplit_footer_str, "\n")
					if(splitCountDisplay == FALSE)
					{
						BSkyFormat(paste("\n", "Begins ", BSkyFormat_BSkySplit_footer_str, "\n"))
					}
					else
					{
						BSkySplit_footer_str = paste(BSkySplit_footer_str[[1]], collapse=",")
						BSkyFormat(paste("\n", "Begins ", BSkySplit_footer_str,"\n"))
					}
					#cat("\n")
					ret_char_count_array = BSkyEvalRcommandBasic(RcommandString = RcommandString_modified_split_footer, origRcommands = RcommandString_modified, echo = echoRcommand, echoInline = echoInlineRcommand, splitOn = TRUE, graphicsDir = graphicsDir, bskyEvalDebug = bskyEvalDebug) #, numExprParse = numExprParse, selectionStartpos = selectionStartpos, selectionEndpos = selectionEndpos)
				}
				else
				{
					#cat("\n",BSkySplit_footer_str, "\n")
					#cat("\nSplit Dataset is empty - skipping")
					#cat("\n")
					
					if(splitCountDisplay == FALSE)
					{
						BSkyFormat(paste("\n", "Begins ", BSkyFormat_BSkySplit_footer_str, "\n", "Split Dataset is empty - skipping"))
					}
					else
					{
						BSkySplit_footer_str = paste(BSkySplit_footer_str[[1]], collapse=",")
						BSkyFormat(paste("\n", "Begins ", BSkySplit_footer_str, "\n", "Split Dataset is empty - skipping"))
					}
					
					# BSkyErrMsgwithSplit  = paste("Split Dataset Size is 0 - skipping :", BSkySplit_footer_str)
					# BSkyWarnMsgWithSplit = paste("Split Dataset Size is 0 - skipping :", BSkySplit_footer_str) 
					# BSkyStoreApplicationWarnErrMsg(BSkyWarnMsgWithSplit, BSkyErrMsgwithSplit)
				}
			}
		}
		else
		{
			ret_char_count_array = BSkyEvalRcommandBasic(RcommandString = RcommandString_modified, echo = echoRcommand, echoInline = echoInlineRcommand, graphicsDir = graphicsDir, bskyEvalDebug = bskyEvalDebug) #, numExprParse = numExprParse, selectionStartpos = selectionStartpos, selectionEndpos = selectionEndpos)
		}
		
	if(ignoreSplitOn == FALSE)
	{
		BSkyFunctionWrapUp()
	}
	
	# return(invisible(RcommandString_modified))
	#return(invisible(ret_char_count_array))
	#return(invisible(Rcommands_initial_parse))
	
	overall_execution_Status = -1
	if(uadatasets.sk$BSkyEvalErrors == 0 && Rcommands_initial_parse$parsingStatus == 0)
	{
		overall_execution_Status = 0
	}
	
	if(bskyEvalDebug == TRUE)
	{
		cat("\nTotal eval() execution errors encountered\n")
		print(uadatasets.sk$BSkyEvalErrors)
	}

	if(bskyEvalDebug == TRUE)
	{
		print("**********Returning from BSky Eval function********")
		print(Rcommands_initial_parse)
	}
	
	return(invisible(list(executionStatus = overall_execution_Status, parsingStatus = Rcommands_initial_parse$parsingStatus, parsingErrorLineNum = Rcommands_initial_parse$parsingErrorLineNum, totalCharCount = Rcommands_initial_parse$totalCharCount, firstExprStartPos = Rcommands_initial_parse$firstExprStartPos, lastExprEndPos = Rcommands_initial_parse$lastExprEndPos, parsedCommandList= Rcommands_initial_parse$parsedCommandList)))
}



#13Sep2021
BSkyEvalRcommandBasic <- function(RcommandString, origRcommands = c(), numExprParse = -1, selectionStartpos = 0, selectionEndpos = 0, echo = BSkyGetRCommandDisplaySetting(), echoInline = BSkyGetRCommandDisplaySetting(), splitOn = FALSE, graphicsDir = BSkyGetGraphicsDirPath(), bskyEvalDebug = FALSE)
{
	parsed_Rcommands = c()
	parsed_orig_Rcommands = c()
	
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
	
	
	parsed_Rcommands = parse(text={RcommandString})
	parsed_Rcommands = (tidy_source(text = RcommandString, output = FALSE))$text.tidy
	
	#Rcommands_initial_parse = BSkyRCommandParsedCharCount(RcommandString = RcommandString, numExprParse = numExprParse)
	#parsed_Rcommands = (tidy_source(text = Rcommands_initial_parse$parsedCommandList, output = FALSE, end.comment="\n"))$text.tidy
	
	if(length(origRcommands) > 0)
	{
		parsed_orig_Rcommands = parse(text={origRcommands})
		parsed_orig_Rcommands = (tidy_source(text = origRcommands, output = FALSE))$text.tidy
		
		#origRcommands_initial_parse = BSkyRCommandParsedCharCount(RcommandString = origRcommands, numExprParse = numExprParse)
		#parsed_orig_Rcommands = (tidy_source(text = origRcommands_initial_parse$parsedCommandList, output = FALSE, end.comment="\n"))$text.tidy
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
					#print(parsed_orig_Rcommands[[i]])
					cat(parsed_orig_Rcommands[[i]])
					#cat("\n")
					
				}
				else
				{
					#print(parsed_Rcommands[[i]])
					cat(parsed_Rcommands[[i]])
					#cat("\n")
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
		CommentOrBlankLineStr = trimws(parsed_Rcommands[[i]])
		
		if(substr(CommentOrBlankLineStr,1,1) == "#" || substr(CommentOrBlankLineStr,1,1) == "")
		{
			isCommentOrBlankLine = TRUE
		}
		
		if(isCommentOrBlankLine == FALSE)
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
					cat("\n")
					
					if(bsky_Rmarkdown_settings$doRmarkdownFormatting == TRUE && bsky_Rmarkdown_settings$doLatexFormatting == FALSE)
					{
						cat("<pre class=\"r\"><code>")
					}
				
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
				
				eval(parse(text="bsky_rcommand_execution_an_exception_occured = FALSE"), envir=globalenv())
			}
			
			if(graphicsDir_exists == TRUE)
			{
				num_graphics_files = length(list.files(graphicsDir, pattern="png|svg"))
				
				if(bskyEvalDebug == TRUE)
				{
					cat("\n<br>********* SK debug Printing call details within BSkyEvalRcommandBasic - num_graphics_files and uadatasets.sk$last_count_of_bsky_graphics_files ******<br>\n")
					print(num_graphics_files)
					print(uadatasets.sk$last_count_of_bsky_graphics_files)
					print(num_graphics_files - uadatasets.sk$last_count_of_bsky_graphics_files)
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
						{
							file.remove(uadatasets.sk$initial_graphics_file_name)
							first_Graphics_Command_Executed = TRUE
						}
						
						# if(bskyEvalDebug == TRUE)
						# {
							# BSkyGraphicsFormat(bSkyFormatAppRequest = FALSE, noOfGraphics= (num_graphics_files - uadatasets.sk$last_count_of_bsky_graphics_files - 1), isRmarkdownOutputOn = bsky_Rmarkdown_settings$doRmarkdownFormatting)
							# uadatasets.sk$last_count_of_bsky_graphics_files = num_graphics_files
						# }
						# else
						{
							BSkyGraphicsFormat(bSkyFormatAppRequest = FALSE, noOfGraphics= (num_graphics_files - uadatasets.sk$last_count_of_bsky_graphics_files), isRmarkdownOutputOn = bsky_Rmarkdown_settings$doRmarkdownFormatting)
							uadatasets.sk$last_count_of_bsky_graphics_files = num_graphics_files - 1
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



#22Sep2021
BSkyRCommandParsedExprBoundary <- function(RcommandString, numExprParse = -1, selectionStartpos = 0, selectionEndpos = 0, linePosOffsetAdjutment = 0, bskyEvalDebug = FALSE)
{
	uadatasets.sk$BSkyParsingErrors = c()
	total_error_log_index = 0
	first_reportable_error_log_index = 0
	
	char_count = 0
	first_expr_start_char_count = 0
	last_expr_end_char_count = 0
	
	firstExpressionDetermind = FALSE
	lastExpressionDetermind = FALSE 
	totalExprDetermind = 0
	n = 0
	
	bsky_Rmarkdown_settings = BSkyGetKableAndRmarkdownFormatting()
	
	if(bskyEvalDebug == TRUE)
	{
		if(bsky_Rmarkdown_settings$doRmarkdownFormatting == TRUE && bsky_Rmarkdown_settings$doLatexFormatting == FALSE)
		{
			cat("<pre class=\"r\"><code>")
		}
	}
	
	#if(nchar(trimws(RcommandString)) == 0 || selectionStartpos > nchar(RcommandString))
	if(nchar(RcommandString) == 0 || selectionStartpos > nchar(RcommandString))
	{
		#return(invisible(list(parsingStatus = 0, parsingErrorLineNum =0, totalCharCount = nchar(RcommandString), firstExprStartPos = first_expr_start_char_count, lastExprEndPos = last_expr_end_char_count, parsedCommandList=c())))
		return(invisible(list(parsingStatus = -1, parsingErrorLineNum =-1, totalCharCount = nchar(RcommandString), firstExprStartPos = first_expr_start_char_count, lastExprEndPos = last_expr_end_char_count, parsedCommandList=c())))
	}
	
	if(numExprParse == 0)
	{
		#return(invisible(list(parsingStatus = 0, parsingErrorLineNum =0, totalCharCount = char_count, firstExprStartPos = first_expr_start_char_count, lastExprEndPos = last_expr_end_char_count, parsedCommandList = c())))
		return(invisible(list(parsingStatus = -1, parsingErrorLineNum =-1, totalCharCount = char_count, firstExprStartPos = first_expr_start_char_count, lastExprEndPos = last_expr_end_char_count, parsedCommandList = c())))
	}
	
	if(numExprParse != 0 && selectionStartpos <= 0)
	{
		selectionStartpos = 1
	}
	
	line_breakdown_RcommandString = data.frame(strsplit(RcommandString, "\n"))
	line_breakdown_RcommandString = cbind(line_breakdown_RcommandString, lapply(line_breakdown_RcommandString, nchar))
	line_breakdown_RcommandString[,2] = line_breakdown_RcommandString[,2] + 1
	line_breakdown_RcommandString = cbind(line_breakdown_RcommandString, cumsum(line_breakdown_RcommandString[,2]))
	line_breakdown_RcommandString = cbind(seq(1:nrow(line_breakdown_RcommandString)), line_breakdown_RcommandString)
	names(line_breakdown_RcommandString) = c("lineNum", "lineTxt", "lineTxtCharCount", "lineTxtCumCharCount")
	
	if(bskyEvalDebug == TRUE)
	{
		if(bsky_Rmarkdown_settings$doRmarkdownFormatting == TRUE && bsky_Rmarkdown_settings$doLatexFormatting == FALSE)
		{
			cat("<pre class=\"r\"><code>")
		}
		
		cat("\nline_breakdown_RcommandString table\n")
		print(line_breakdown_RcommandString)
		
		if(bsky_Rmarkdown_settings$doRmarkdownFormatting == TRUE && bsky_Rmarkdown_settings$doLatexFormatting == FALSE)
		{
			cat("</code></pre>")
		}
	}
	
	if(selectionStartpos > 0)
	{
		selctionStartLineNumber = min((line_breakdown_RcommandString[line_breakdown_RcommandString$lineTxtCumCharCount >= selectionStartpos,])$lineNum)
	}
	
	
	while(lastExpressionDetermind == FALSE && char_count < nchar(RcommandString))
	{
		cur_end_line_num_parsed = min((line_breakdown_RcommandString[line_breakdown_RcommandString$lineTxtCumCharCount >= (char_count+1),])$lineNum)
		
		if(bskyEvalDebug == TRUE)
		{
			cat("\nAbsolute current line number:", linePosOffsetAdjutment, "\n")
			cat("current line number:", cur_end_line_num_parsed, "\n")
			cat("command to parse\n", substr(RcommandString, char_count+1, nchar(RcommandString)), "\n")
		}
		
		parsed_Rcommands = BSkyRCommandParsedCharCount(RcommandString = substr(RcommandString, char_count+1, nchar(RcommandString)), numExprParse = 1)
		
		if(bskyEvalDebug == TRUE)
		{
			cat("\n****** parsed_Rcommands from BSkyRCommandParsedCharCount *******\n")
			print(parsed_Rcommands)
			cat("\n+++++++\n")
		}
		
		if(parsed_Rcommands$parsingStatus == -1)
		{
			total_error_log_index = total_error_log_index + 1
			#uadatasets.sk$BSkyParsingErrors[total_error_log_index] = paste("Line number:", (cur_end_line_num_parsed + parsed_Rcommands$totalCharCount - 1), uadatasets.sk$BSkyParsingErrors[total_error_log_index])
			uadatasets.sk$BSkyParsingErrors[total_error_log_index] = paste("Line number:", (cur_end_line_num_parsed + (linePosOffsetAdjutment -1) + parsed_Rcommands$totalCharCount - 1), uadatasets.sk$BSkyParsingErrors[total_error_log_index])
			
			# if there is parsing error parsed_Rcommands$totalCharCount returns the offending line number not the total char count
			if(selctionStartLineNumber > (cur_end_line_num_parsed + parsed_Rcommands$totalCharCount - 1)) #min((line_breakdown_RcommandString[line_breakdown_RcommandString$lineTxtCumCharCount >= char_count,])$lineNum)))
			{
				if(bskyEvalDebug == TRUE)
				{
					cat("\n\n******Parsing error - offending line number: ", cur_end_line_num_parsed + parsed_Rcommands$totalCharCount - 1, "\n")
					cat("Starting Position Line Number: ", selctionStartLineNumber, " -continuing parsing(higher than the offending line number)\n\n")
					# cat("Parsing error :", parsed_Rcommands$totalCharCount, "-continuing parsing... \n")
					# print(uadatasets.sk$BSkyParsingErrors)
				}
			}
			else
			{
				if(first_reportable_error_log_index == 0)
				{
					first_reportable_error_log_index = total_error_log_index
				}
				
				if(bskyEvalDebug == TRUE)
				{
					cat("\n\n******Parsing error - offending line number: ", cur_end_line_num_parsed + parsed_Rcommands$totalCharCount - 1, "\n")
					cat("Starting Position Line Number: ", selctionStartLineNumber, " -halting parsing(within the offending line number)\n")
					# cat("Parsing error :", parsed_Rcommands$totalCharCount, "-halting parsing... \n")
					# print(uadatasets.sk$BSkyParsingErrors)
				}
				
				if(bskyEvalDebug == TRUE)
				{
					start_index = 1
				}
				else
				{
					start_index = first_reportable_error_log_index
				}
				
				for(i in start_index:total_error_log_index)
				{
					cat("\n")
					cat(uadatasets.sk$BSkyParsingErrors[i])
				}
				cat("\n")
				
				parsed_expr_list = BSkyRCommandParsedCharCount(RcommandString = substr(RcommandString, first_expr_start_char_count, last_expr_end_char_count)) 
	
				return(invisible(list(parsingStatus = -1, parsingErrorLineNum = (cur_end_line_num_parsed + (linePosOffsetAdjutment -1) + parsed_Rcommands$totalCharCount - 1), totalCharCount = line_breakdown_RcommandString[(cur_end_line_num_parsed + parsed_Rcommands$totalCharCount - 1),"lineTxtCumCharCount"], firstExprStartPos = first_expr_start_char_count, lastExprEndPos = last_expr_end_char_count, parsedCommandList= parsed_expr_list$parsedCommandList)))
			}
			
			char_count = line_breakdown_RcommandString[line_breakdown_RcommandString$lineNum == (cur_end_line_num_parsed + parsed_Rcommands$totalCharCount - 1),"lineTxtCumCharCount"]
		}
		else
		{
			char_count = char_count + parsed_Rcommands$totalCharCount
			
			if(selectionStartpos <= char_count && firstExpressionDetermind == FALSE)
			{
				if(selectionStartpos > (char_count - parsed_Rcommands$totalCharCount + parsed_Rcommands$firstExprStartPos))
				{
					first_expr_start_char_count = char_count - parsed_Rcommands$totalCharCount + parsed_Rcommands$firstExprStartPos
				}
				else
				{
					first_expr_start_char_count = char_count - parsed_Rcommands$totalCharCount + 1 #parsed_Rcommands$firstExprStartPos #selectionStartpos
				}
				
				firstExpressionDetermind = TRUE 
			}
			
			if(selectionEndpos > 0 && selectionEndpos > selectionStartpos && selectionEndpos <= char_count && lastExpressionDetermind == FALSE)
			{
				if(selectionEndpos <= (char_count - parsed_Rcommands$totalCharCount + parsed_Rcommands$lastExprEndPos))
				{
					#last_expr_end_char_count = char_count - parsed_Rcommands$totalCharCount + parsed_Rcommands$lastExprEndPos
					lastExpressionDetermind = TRUE
				} 
				
				if(selectionEndpos <= (char_count - parsed_Rcommands$totalCharCount + parsed_Rcommands$firstExprStartPos -1))
				{
					lastExpressionDetermind = TRUE
					char_count = char_count - parsed_Rcommands$totalCharCount + parsed_Rcommands$firstExprStartPos - 1
				}
			}
			
			if(firstExpressionDetermind == TRUE)
			{
				last_expr_end_char_count = char_count #- parsed_Rcommands$totalCharCount + parsed_Rcommands$lastExprEndPos
				totalExprDetermind = totalExprDetermind + 1
			}
			
			if((numExprParse > 0 && totalExprDetermind == numExprParse) || (selectionEndpos > 0 && selectionEndpos <= char_count))
			{
				lastExpressionDetermind = TRUE
			}
		}
	}
	
	if(first_expr_start_char_count > 0 && last_expr_end_char_count == 0)
	{
		last_expr_end_char_count = char_count
	}
	
	if(bskyEvalDebug == TRUE && total_error_log_index > 0)
	{
		for(i in 1:total_error_log_index)
		{
			cat("\n")
			cat(uadatasets.sk$BSkyParsingErrors[i])
		}
		cat("\n")
	}
	
	if(bskyEvalDebug == TRUE)
	{
		cat("\nfirst_expr_start_char_count: ", first_expr_start_char_count, "last_expr_end_char_count: ", last_expr_end_char_count, "\n")
	}
	
	if(bskyEvalDebug == TRUE)
	{
		if(bsky_Rmarkdown_settings$doRmarkdownFormatting == TRUE && bsky_Rmarkdown_settings$doLatexFormatting == FALSE)
		{
			cat("</code></pre>")
		}
	}
	
	parsed_expr_list = BSkyRCommandParsedCharCount(RcommandString = substr(RcommandString, first_expr_start_char_count, last_expr_end_char_count)) 
	return(invisible(list(parsingStatus = 0, parsingErrorLineNum = 0, totalCharCount = char_count, firstExprStartPos = first_expr_start_char_count, lastExprEndPos = last_expr_end_char_count, parsedCommandList= parsed_expr_list$parsedCommandList)))
}

#22Sep2021
BSkyRCommandLineNumberFromCharCount <- function(RcommandString, charCount)
{
	RCommandLineNumber = 1
	
	if(charCount > 0)
	{
		line_breakdown_RcommandString = data.frame(strsplit(RcommandString, "\n"))
		line_breakdown_RcommandString = cbind(line_breakdown_RcommandString, lapply(line_breakdown_RcommandString, nchar))
		line_breakdown_RcommandString[,2] = line_breakdown_RcommandString[,2] + 1
		line_breakdown_RcommandString = cbind(line_breakdown_RcommandString, cumsum(line_breakdown_RcommandString[,2]))
		line_breakdown_RcommandString = cbind(seq(1:nrow(line_breakdown_RcommandString)), line_breakdown_RcommandString)
		names(line_breakdown_RcommandString) = c("lineNum", "lineTxt", "lineTxtCharCount", "lineTxtCumCharCount")
		
		RCommandLineNumber = min((line_breakdown_RcommandString[line_breakdown_RcommandString$lineTxtCumCharCount >= charCount,])$lineNum)
	}
	
	return(invisible(RCommandLineNumber))
}



#05Sep2021
BSkyRCommandParsedCharCount <- function(RcommandString, numExprParse = -1)
{
	char_count = 0
	first_expr_start_char_count = 0
	last_expr_end_char_count = 0
	found_first_expr = FALSE
	leading_comments_newlines_and_parsed_expr = list()
	
	eval(parse(text="bsky_rcommand_parsing_an_exception_occured = FALSE"), envir=globalenv())
	partial_parsed_txt = srcfile("RcommandString") #("C:/Users/User/Documents/workfolder/BSky/Rmarkdown/srctxt.r")
	
	tryCatch({
			withCallingHandlers({
					parsed_Rcommands = parse(text = RcommandString, n = numExprParse, keep.source= TRUE)
			}, warning = BSkyRcommandParsingErrWarnHandler, silent = TRUE)
			}, error = BSkyRcommandParsingErrWarnHandler, silent = TRUE)
	
	
	if(bsky_rcommand_parsing_an_exception_occured == TRUE)
	{
		tryCatch({
			withCallingHandlers({
					parsed_Rcommands = parse(text = RcommandString, n = numExprParse, keep.source= TRUE, srcfile = partial_parsed_txt)
			}, warning = BSkyDoNothingErrWarnHandler, silent = TRUE)
			}, error = BSkyDoNothingErrWarnHandler, silent = TRUE)
			
		eval(parse(text="bsky_rcommand_parsing_an_exception_occured = FALSE"), envir=globalenv())
		return(invisible(list(parsingStatus = -1, totalCharCount = max(getParseData(partial_parsed_txt)$line1), firstExprStartPos = first_expr_start_char_count, lastExprEndPos = last_expr_end_char_count, parsedCommandList = leading_comments_newlines_and_parsed_expr)))
	}
	
			
	#if(length(parsed_Rcommands) > 0)
	{
		# print(parsed_Rcommands)
		# print(str(parsed_Rcommands))
		
		leading_comments_newlines_and_parsed_expr = as.character(attr(parsed_Rcommands, "wholeSrcref"))
		#return(leading_comments_newlines_and_parsed_expr)
		
		parsed_source_array_length = length(leading_comments_newlines_and_parsed_expr)
		
		for(i in 1:parsed_source_array_length)
		{
			if(i > 1)
			{
				char_count = char_count + 1 # accounting for \n character 
			}
			
			if(found_first_expr == FALSE && (substr(trimws(leading_comments_newlines_and_parsed_expr[i]),1,1) != "#" && substr(trimws(leading_comments_newlines_and_parsed_expr[i]),1,1) != ""))
			{
				found_first_expr = TRUE
				first_expr_start_char_count = char_count + 1
			}
			
			char_count = char_count + nchar(leading_comments_newlines_and_parsed_expr[i])
		}
		
		#parsed_token_df = getParseData(parsed_Rcommands)
		#line_num_first_expr = unique(parsed_token_df[parsed_token_df$token == "SYMBOL",min(parsed_token_df$line1)])
		
		if(found_first_expr == TRUE)
		{
			last_expr_end_char_count = char_count
		}
	}

	return(invisible(list(parsingStatus = 0, totalCharCount = char_count, firstExprStartPos = first_expr_start_char_count, lastExprEndPos = last_expr_end_char_count, parsedCommandList = leading_comments_newlines_and_parsed_expr)))
}

#14Sep2021
BSkyRCommandParsingTest <- function(RcommandString, numExprParse = -1, bskyEvalDebug = FALSE)
{
	eval(parse(text="bsky_rcommand_parsing_an_exception_occured = FALSE"), envir=globalenv())
	
	tryCatch({
			withCallingHandlers({
					parsed_Rcommands = parse(text = RcommandString, n = numExprParse, keep.source= TRUE)
			}, warning = BSkyRcommandParsingErrWarnHandler, silent = TRUE)
			}, error = BSkyRcommandParsingErrWarnHandler, silent = TRUE)
	
	
	if(bsky_rcommand_parsing_an_exception_occured == TRUE)
	{		
		if(bskyEvalDebug == TRUE)
		{
			cat("\nParsing Error in Selected Text Area\n")
			cat(RcommandString)
			cat("\n")
		}
		eval(parse(text="bsky_rcommand_parsing_an_exception_occured = FALSE"), envir=globalenv())
		return(invisible(c(-1)))
	}
	
	if(bskyEvalDebug == TRUE)
	{
		cat("\nNo Parsing Error in Selected Text Area\n")
		cat(RcommandString)
		cat("\n")
	}
	
	return(invisible(c(0)))
}


#15Sep2021
BSkyRcommandErrWarnHandler <- function(m)
{
	#print(str(m))
	
	eval(parse(text="bsky_rcommand_execution_an_exception_occured = TRUE"), envir=globalenv())
	
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



BSkyRcommandParsingErrWarnHandler <- function(m)
{
	#print(str(m))
	
	if("error" %in% attr(m, "class"))
	{
		eval(parse(text="bsky_rcommand_parsing_an_exception_occured = TRUE"), envir=globalenv())
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


BSkyDoNothingErrWarnHandler <- function(m)
{
}



BSkyGetGraphicsDirPath <- function()
{
	bskyGraphicsDirPath = c()
	
	if(exists("uadatasets.sk"))
	{
		bskyGraphicsDirPath = uadatasets.sk$bskyGraphicsDirPath
	}
	
	return(invisible(bskyGraphicsDirPath))
}


BSkySetGraphicsDirPath <- function(bskyGraphicsDirPath = c())
{
	if(length(bskyGraphicsDirPath) > 0)
	{
		if(exists("uadatasets.sk"))
		{
			uadatasets.sk$bskyGraphicsDirPath = bskyGraphicsDirPath
		}
	}
	return(invisible(bskyGraphicsDirPath))
}


#14Jun2021 for setting the environment variables to control the echo and inline echo of the R commands being executed by BSkyExecuteRcommand()
BSkySetRCommandDisplaySetting <- function(echo = TRUE, echoInline = TRUE)
{

	if(exists("uadatasets.sk"))
	{
		uadatasets.sk$echoRcommand = echo
		uadatasets.sk$echoInlineRcommand = echoInline		
	}
	
	return(invisible(list(echo = echo, echoInline = echoInline)))
}


#14Jun2021 for getting the environment variables to control the echo and inline echo of the R commands being executed by BSkyExecuteRcommand()
BSkyGetRCommandDisplaySetting <- function()
{
	echoRcommand = TRUE
	echoInlineRcommand = TRUE
	
	if(exists("uadatasets.sk"))
	{
		if(exists("echoRcommand", env=uadatasets.sk))
		{
			echoRcommand = uadatasets.sk$echoRcommand
		}
		else
		{
			echoRcommand = TRUE
		}
	
		if(exists("echoInlineRcommand", env=uadatasets.sk))
		{
			echoInlineRcommand = uadatasets.sk$echoInlineRcommand
		}
		else
		{
			echoInlineRcommand = TRUE
		}
	}
	
	return(invisible(list(echo = echoRcommand, echoInline = echoInlineRcommand)))
}


BSkySetSplitCountDisplaySetting <- function(splitIterationCountDisplay = FALSE)
{
	if(exists("uadatasets.sk"))
	{
		uadatasets.sk$splitIterationCountDisplay = splitIterationCountDisplay		
	}
	
	return(invisible(splitIterationCountDisplay))
}


BSkyGetSplitCountDisplaySetting <- function()
{
	splitIterationCountDisplay = FALSE
	
	if(exists("uadatasets.sk"))
	{
		if(exists("splitIterationCountDisplay", env=uadatasets.sk))
		{
			splitIterationCountDisplay = uadatasets.sk$splitIterationCountDisplay
		}
		else
		{
			uadatasets.sk$splitIterationCountDisplay = FALSE
		}
	}
	
	return(invisible(splitIterationCountDisplay))
}

#21Jun2021
BSkyDatasetNameSubstitute <- function(datasetName, toDatasetName, replaceOldColumnNames = c(), currentColumnNames = c(), RcommandString, splitOn = FALSE, preSplitDatasetName = c())
{
	#grep("(\\bdata=mtcarsmodified)\\b)|(\\bdata=\\s+mtcarsmodified)\\b)|(\\bdata\\s+=mtcarsmodified)\\b)|(\\bdata\\s+=\\s+mtcarsmodified)\\b)", abc)
	#grep("((\\bdata=mtcarsmodified\\b)|(\\bdata=\\s+mtcarsmodified\\b)|(\\bdata\\s+=mtcarsmodified\\b)|(\\bdata\\s+=\\s+mtcarsmodified\\b))", abc)
	
	#grep_str = paste("(\\bdata=",datasetName,")\\b)|(\\bdata=\\s+",datasetName,")\\b)|\\bdata\\s+=\\s+",datasetName, "|(\\bdata\\s+=",datasetName,")\\b)", sep="")

	grep_dataset_str0 = paste("((\\b",datasetName,"\\b))", sep="")
	grep_dataset_str1 = paste("((\\bdata=",datasetName,"\\b)|(\\bdata=\\s+",datasetName,"\\b)|(\\bdata\\s+=",datasetName,"\\b)|(\\bdata\\s+=\\s+",datasetName,"\\b))", sep="")
	grep_dataset_str2 = paste("((",datasetName,"\\[))", sep = "")
	
	for(n in 1:length(RcommandString))
	{
		#replace the datasetName with toDatasetName
		RcommandString[n] = gsub(grep_dataset_str0, toDatasetName , RcommandString[n])
		
		#replace the data=datasetName with data=toDatasetName
		sub_str1 = paste("data=", toDatasetName) 
		RcommandString[n] = gsub(grep_dataset_str1, sub_str1, RcommandString[n])
	
		#substitute datasetName such as datasetName[index] that uses the convention to refer to its columns with toDatasetName[index]
		sub_str2 = paste(toDatasetName,"[",sep="")
		RcommandString[n] = gsub(grep_dataset_str2, sub_str2, RcommandString[n])
		
		#substitute the column names 
		if(length(replaceOldColumnNames) > 0 && length(currentColumnNames) > 0)
		{
			fromDatsetColumnNames = dimnames(eval(parse(text = datasetName), envir = globalenv()))[[2]]
			
			if(splitOn == TRUE)
			{
				toDatsetColumnNames = dimnames(eval(parse(text = preSplitDatasetName), envir = globalenv()))[[2]]
				currentDatasetName = preSplitDatasetName
			}
			else
			{
				toDatsetColumnNames = dimnames(eval(parse(text = toDatasetName), envir = globalenv()))[[2]] 
				currentDatasetName = toDatasetName
			}
			
			if(length(replaceOldColumnNames) <= length(currentColumnNames))
			{
				loop_count = length(replaceOldColumnNames)
			}
			else
			{
				loop_count = length(currentColumnNames)
			}
			
			for(j in 1:loop_count)
			{
				# cat("<pre class=\"r\"><code>")
				# cat("\n======= Replacing =======\n")
				# print(datasetName)
				# print(toDatasetName)
				# print(fromDatsetColumnNames)
				# print(replaceOldColumnNames[j])
				# print(toDatsetColumnNames)
				# print(currentColumnNames[j])
				
				replaceOldColumnNamesClass = class(eval(parse(text = paste(datasetName, "$", replaceOldColumnNames[j], sep="")), envir = globalenv()))
				currentColumnNamesClass = class(eval(parse(text = paste(currentDatasetName, "$", currentColumnNames[j], sep="")), envir = globalenv()))
				
				if((replaceOldColumnNames[j] %in% fromDatsetColumnNames) && (currentColumnNames[j] %in% toDatsetColumnNames) && replaceOldColumnNamesClass == currentColumnNamesClass)
				{	
					# cat("\n======= col map exists =======\n")
					# print(RcommandString[n])
					RcommandString[n] = gsub(replaceOldColumnNames[j], currentColumnNames[j], RcommandString[n])
					# cat("\n")
					# print(RcommandString[n])
					
				}
				
				#cat("</code></pre>")
			}
		}
	}
	
	return(invisible(RcommandString))
}





#14Jun2021
BSkyExpandBSkyFormatWithFooter <- function(RcommandString, BSkySplitFooterStr)
{
	paramName = "BSkyFormat"
	paramName = paste("(\\b", paramName, "\\b)", sep="")
	
	for(n in 1:length(RcommandString))
	{
		# Look for all BSkyFormat commands in the R command string passed 
		paramNamePosition = gregexpr(paramName, RcommandString[n])
		
		if(paramNamePosition[[1]][1] > 0)
		{
			all_BSkyformat_str = c()
			
			for(i in 1:length(paramNamePosition[[1]]))
			{
				#endParamValuePosition = regexpr(")", substr(RcommandString[n],(paramNamePosition[[1]][i] +attr(paramNamePosition[[1]],"match.length")[i]), nchar(RcommandString[n])))
				
				final_closing_parentheses_matched = gregexpr("\\((?>[^()]|(?R))*\\)", substr(RcommandString[n], paramNamePosition[[1]][i], nchar(RcommandString[n])), perl = T)
				
				#BSkyformat_str1 = substr(RcommandString[n], paramNamePosition[[1]][i], paramNamePosition[[1]][i]+attr(paramNamePosition[[1]],"match.length")[i]+endParamValuePosition-1)
				
				BSkyformat_str1 = substr(RcommandString[n], paramNamePosition[[1]][i], paramNamePosition[[1]][i]+final_closing_parentheses_matched[[1]][1]+attr(final_closing_parentheses_matched[[1]],"match.length")[1] - 1 -1)
				
				all_BSkyformat_str = c(all_BSkyformat_str, BSkyformat_str1)
				
				#print(BSkyformat_str1)
				#cat("===================\n")
			}
			
			for(i in 1:length(all_BSkyformat_str))
			{
				BSkyformat_str_modified = paste(substr(all_BSkyformat_str[i], 1, nchar(all_BSkyformat_str[i]) -1), " ,repeatAllTableFooter = ", "\"", BSkySplitFooterStr, "\"", ")", sep = "")
				
				#print(BSkyformat_str_modified)
				#cat("===================\n")
				
				all_BSkyformat_str[i] = gsub("\\(", "\\\\(", all_BSkyformat_str[i])
				#all_BSkyformat_str[i] = paste("(\\b", all_BSkyformat_str[i], "\\b)", sep="")
				RcommandString[n] = gsub(all_BSkyformat_str[i], BSkyformat_str_modified, RcommandString[n])
				
				#print(RcommandString[n])
			}
		}
	}
	
	return(invisible(RcommandString))
}


#23May2021
BSkySplitIterationHandling <- function(datasetName, varNames, RcommandString)
{
	BSkyFunctionInit()
		
		BSkySetCurrentDatasetName(datasetName, setDatasetIndex = "y")
		
		bSkyDatasetname = BSkyGetDatasetName(datasetName)
		
		bSkyVarnamesSplit = varNames
		
		bSkySplitIterationCount = BSkyComputeSplitdataset(bSkyVarnamesSplit, bSkyDatasetname)
		
		for (bSkySplitIterationCounter in 1:bSkySplitIterationCount) 
		{
			#cat("\n +++++++++++++++++++++++++ Split Counter START :", bSkySplitIterationCounter, "++++++++++++++++++++++++ \n")
			bSkyDatasetSliceIndex = BSkyGetNextDatasetSliceIndex(bSkyVarnamesSplit, bSkyDatasetname)
			
			bSkyGlobalDataSliceIndexToWorkOn = bSkyDatasetSliceIndex$datasetSliceIndex
			assign("bSkyGlobalDataSliceIndexToWorkOn", bSkyDatasetSliceIndex$datasetSliceIndex, envir = .GlobalEnv)
			
			# cat("\n Dataset Slice \n")
			# cat("\n Dim - ", dim(uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]]), "\n")
			# print(head(uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]]))
			# cat("\n")
						  
			#if (bSkySplitIterationCount > 1) 
			{
				#cat("\n")
				BSkySplit_footer_str = paste(BSkyComputeCurrentVarNamesAndFactorValues(bSkyDatasetname), collapse = ",")
				#print(BSkySplit_footer_str)
				#cat("\n")
			}
			
			# Example slice dataset substitution 
			#"BSky_Independent_Samples_t_Test = t.test( mtcarsmodified$mpg, mtcarsmodified$hp, alternative='two.sided', conf.level=0.95, mu=0.0, var.equal=FALSE, data=mtcarsmodified)"
			# BSky_Independent_Samples_t_Test = t.test(uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]][,1], uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]][,2], alternative='two.sided', conf.level=0.95, mu=0.0, var.equal=FALSE, data=uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]])
			#RcommandString = "BSky_Independent_Samples_t_Test = t.test( mtcarsmodified$mpg, mtcarsmodified$hp, alternative='two.sided', conf.level=0.95, mu=0.0, var.equal=FALSE, data=mtcarsmodified) ; BSkyFormat(BSky_Independent_Samples_t_Test)"
			
			if(bSkySplitIterationCount > 1)
			{
				#print(RcommandString)
				RcommandString_modified = BSkySplitVarsDatasetStringSub(datasetName = datasetName, varNames = varNames, RcommandString = RcommandString, BSkySplitFooterStr = BSkySplit_footer_str)
				#print(RcommandString_modified)
			}
			else
			{
				RcommandString_modified = RcommandString
			}
			
			#cat(RcommandString_modified)
			
			#RcommandString_modified = "print(bSkyGlobalDataSliceIndexToWorkOn); mydf = data.frame(A=c(1,2,3), B=c(\"a\",\"b\",\"c\")); BSkyFormat(mydf)"
			
			if(dim(uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]])[1] > 0 && dim(uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]])[2] > 0)
			{
				tryCatch({
					 withCallingHandlers({
						# eval.parent(expr, n) is a shorthand for eval(expr, parent.frame(n))
						#R_Output = eval.parent(parse(text= RcommandString_modified), n=pEnv)
						R_Output = eval(parse(text= RcommandString_modified)) #, envir=globalenv()) #env = caller_env(pEnv))
						#print(R_Output)
				# # }, warning = BSkyDummyErrorWarningMuncher) #UAwarnHandlerFn, silent = TRUE)
				# # }, error = BSkyDummyErrorWarningMuncher) #UAerrHandlerFn, silent = TRUE)
				}, warning = UAwarnHandlerFn, silent = TRUE)
				}, error = UAerrHandlerFn, silent = TRUE)
			}
			else
			{
				BSkyErrMsgwithSplit  = paste("Split Dataset Size is 0 - skipping :", BSkySplit_footer_str)
                BSkyWarnMsgWithSplit = paste("Split Dataset Size is 0 - skipping :", BSkySplit_footer_str) 
                BSkyStoreApplicationWarnErrMsg(BSkyWarnMsgWithSplit, BSkyErrMsgwithSplit)
			}
		}
	
	BSkyFunctionWrapUp()
	
	return(invisible(RcommandString_modified))
}

#23May2021
BSkySplitVarsDatasetStringSub <- function(datasetName, varNames, RcommandString, BSkySplitFooterStr)
{
	#grep("(\\bdata=mtcarsmodified)\\b)|(\\bdata=\\s+mtcarsmodified)\\b)|(\\bdata\\s+=mtcarsmodified)\\b)|(\\bdata\\s+=\\s+mtcarsmodified)\\b)", abc)
	#grep("((\\bdata=mtcarsmodified\\b)|(\\bdata=\\s+mtcarsmodified\\b)|(\\bdata\\s+=mtcarsmodified\\b)|(\\bdata\\s+=\\s+mtcarsmodified\\b))", abc)
	
	#grep_str = paste("(\\bdata=",datasetName,")\\b)|(\\bdata=\\s+",datasetName,")\\b)|\\bdata\\s+=\\s+",datasetName, "|(\\bdata\\s+=",datasetName,")\\b)", sep="")

	grep_dataset_str1 = paste("((\\bdata=",datasetName,"\\b)|(\\bdata=\\s+",datasetName,"\\b)|(\\bdata\\s+=",datasetName,"\\b)|(\\bdata\\s+=\\s+",datasetName,"\\b))", sep="")
	grep_dataset_str2 = paste("((\\b",datasetName,"\\b)|(",datasetName,"\\[))", sep = "")

	
	paramName = "BSkyFormat"
	paramName = paste("(\\b", paramName, "\\b)", sep="")
	
	for(n in 1:length(RcommandString))
	{
		#replace the dataset name with the split dataset name 
		RcommandString[n] = gsub(grep_dataset_str1, "data=uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]]", RcommandString[n])
		
		#substitute dataset parameter with dataset$var convention to refer to its var columns 
		for(i in 1:length(varNames))
		{
			grep_str = paste("\\b",datasetName,"\\$", varNames[i], sep="")
			sub_str = paste("uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]]","$", varNames[i], sep="")
			RcommandString[n] = gsub(grep_str, sub_str, RcommandString[n])
		}
	
		#substitute dataset parameter with dataset[index] convention to refer to its var columns 
		RcommandString[n] = gsub(grep_dataset_str2, "data=uadatasets$lst[[bSkyGlobalDataSliceIndexToWorkOn]][", RcommandString[n])
		
		# Look for all BSkyFormat commands in the R command string passed 
		paramNamePosition = gregexpr(paramName, RcommandString[n])
		
		if(paramNamePosition[[1]][1] > 0)
		{
			all_BSkyformat_str = c()
			
			for(i in 1:length(paramNamePosition[[1]]))
			{
				#endParamValuePosition = regexpr(")", substr(RcommandString[n],(paramNamePosition[[1]][i] +attr(paramNamePosition[[1]],"match.length")[i]), nchar(RcommandString[n])))
				
				final_closing_parentheses_matched = gregexpr("\\((?>[^()]|(?R))*\\)", substr(RcommandString[n], paramNamePosition[[1]][i], nchar(RcommandString[n])), perl = T)
				
				#BSkyformat_str1 = substr(RcommandString[n], paramNamePosition[[1]][i], paramNamePosition[[1]][i]+attr(paramNamePosition[[1]],"match.length")[i]+endParamValuePosition-1)
				
				BSkyformat_str1 = substr(RcommandString[n], paramNamePosition[[1]][i], paramNamePosition[[1]][i]+final_closing_parentheses_matched[[1]][1]+attr(final_closing_parentheses_matched[[1]],"match.length")[1] - 1 -1)
				
				all_BSkyformat_str = c(all_BSkyformat_str, BSkyformat_str1)
				
				#print(BSkyformat_str1)
				#cat("===================\n")
			}
			
			for(i in 1:length(all_BSkyformat_str))
			{
				BSkyformat_str_modified = paste(substr(all_BSkyformat_str[i], 1, nchar(all_BSkyformat_str[i]) -1), ",repeatAllTableFooter =", "\"", BSkySplitFooterStr, "\"", ")")
				
				#print(BSkyformat_str_modified)
				#cat("===================\n")
				
				all_BSkyformat_str[i] = gsub("\\(", "\\\\(", all_BSkyformat_str[i])
				#all_BSkyformat_str[i] = paste("(\\b", all_BSkyformat_str[i], "\\b)", sep="")
				RcommandString[n] = gsub(all_BSkyformat_str[i], BSkyformat_str_modified, RcommandString[n])
				
				#print(RcommandString[n])
			}
		}
	}
	
	return(invisible(RcommandString))
}

#23May2021
BSkyDummyErrorWarningMuncher <- function(...)
{
	# Contnue the execution and munch (i.e. do nothing) all error and wanings silently without spitting out 
}

