BSkyMakeFirstRowAsHeader <- function(rowNum = 1, datasetName = BSkyGetCurrentDatabaseName())
{
	if(is.na(datasetName) || is.null(datasetName) || trimws(datasetName) == '')
	{
		# Do nothing
		return(invisible(datasetName))
	}

	datasetObj = get(datasetName)
	datasetObj = as.data.frame(datasetObj)

	bsky_first_row_values = make.names(datasetObj[1,])
  
    bsky_first_row_values = make.unique(bsky_first_row_values)

	datasetObj = datasetObj[-c(1),]
	names(datasetObj) = bsky_first_row_values

	datasetObj = datasetObj %>% dplyr::select_if(~any(!is.na(.) & .!=""))

	datasetObj =  datasetObj %>% dplyr::filter(apply(., 1, function(row){any(!is.na(row) & row != "")}))

    datasetObj = as.data.frame(datasetObj)
  
	bsky_num_of_cols_convert = dim(datasetObj)[2]

	for(i in 1:bsky_num_of_cols_convert){
		datasetObj[,i] = type.convert(datasetObj[,i], as.is = TRUE, dec =Sys.localeconv()[["mon_decimal_point"]] )
	}

	if(dim(datasetObj)[1] > 0 && dim(datasetObj)[2] > 0)
	{
		datasetObj = as.data.frame(datasetObj)
		#print(datasetObj)
		eval(parse(text = paste0(".GlobalEnv$", datasetName, "= datasetObj")))
		BSkyLoadRefresh(datasetName)
	}
} 

# this also creates top row header and also keeping the empty rows cols
BSkyMakeFirstRowAsHeaderKeepingEmptyRC <- function(rowNum = 1, datasetName = BSkyGetCurrentDatabaseName())
{
	if(is.na(datasetName) || is.null(datasetName) || trimws(datasetName) == '')
	{
		# Do nothing
		return(invisible(datasetName))
	}

	datasetObj = get(datasetName)
	datasetObj = as.data.frame(datasetObj)

	# Current column names
	new_names <- colnames(datasetObj)
	# Values in the first row
	first_row <- as.character(datasetObj[1, ])
	# Replace only cells that are neither NA nor empty strings
	idx <- !is.na(first_row) & trimws(first_row) != ""	
	new_names[idx] <- first_row[idx]

	bsky_first_row_values = make.names(new_names)
  
    bsky_first_row_values = make.unique(bsky_first_row_values)

	datasetObj = datasetObj[-c(1),]
	names(datasetObj) = bsky_first_row_values


	# you can introduce a flag and execute following 2 commented lines to clean the empty row/col area of the grid
	# when you execute empty are gets dropped if you dont execute then the empty grid area is retained and empty 
	# portion is only dropped while excuting a dialog
	#
	#datasetObj = datasetObj %>% dplyr::select_if(~any(!is.na(.) & .!=""))
	#datasetObj =  datasetObj %>% dplyr::filter(apply(., 1, function(row){any(!is.na(row) & row != "")}))

    datasetObj = as.data.frame(datasetObj)
  
	bsky_num_of_cols_convert = dim(datasetObj)[2]

	for(i in 1:bsky_num_of_cols_convert){
		datasetObj[,i] = type.convert(datasetObj[,i], as.is = TRUE, dec =Sys.localeconv()[["mon_decimal_point"]] )
	}

	if(dim(datasetObj)[1] > 0 && dim(datasetObj)[2] > 0)
	{
		datasetObj = as.data.frame(datasetObj)
		#print(datasetObj)
		eval(parse(text = paste0(".GlobalEnv$", datasetName, "= datasetObj")))
		BSkyLoadRefresh(datasetName)
	}
} 