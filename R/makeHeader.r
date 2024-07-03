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
		datasetObj[,i] = type.convert(datasetObj[,i], as.is = TRUE)
	}

	if(dim(datasetObj)[1] > 0 && dim(datasetObj)[2] > 0)
	{
		datasetObj = as.data.frame(datasetObj)
		#print(datasetObj)
		eval(parse(text = paste0(".GlobalEnv$", datasetName, "= datasetObj")))
		BSkyLoadRefresh(datasetName)
	}
} 