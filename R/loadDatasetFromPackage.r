BSkyGetDatasetNameTitle <-function(package ="")
{
    if (package == ""|| package =="All_Installed_Packages") {
        uadatasets.sk$BSkyDataFramePackageDetails <- as.data.frame(data(package = .packages(all.available = TRUE))$results)
		uadatasets.sk$BSkyDataFramePackageDetails  <-uadatasets.sk$BSkyDataFramePackageDetails %>% dplyr::filter(BSkyisValidName(Item)) %>% dplyr::arrange(tolower(Item))
    }
    else {
        uadatasets.sk$BSkyDataFramePackageDetails <- as.data.frame(data(package = package)$results)
		uadatasets.sk$BSkyDataFramePackageDetails <-uadatasets.sk$BSkyDataFramePackageDetails %>% dplyr::filter(BSkyisValidName(Item)) %>% dplyr::arrange(tolower(Item))
    }
    uadatasets.sk$BSkyDataFramePackageDetails <- uadatasets.sk$BSkyDataFramePackageDetails %>%
        mutate(Keys = paste(Item, "-[", Title, "]", "-", Package,
            sep = ""))
    return(uadatasets.sk$BSkyDataFramePackageDetails[, c("Keys")])

}

BSkyisValidName <- function(string) {
    grepl("^((([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*)|[.])$", string)
}

### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title  Get dataset name from package dataset list
#'
#' @description 
#'
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param
#' @param
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param
#' @param
#'
#' @return
#'
#' @examples
# BSkyGetDatasetNameFromPackageDatasetList <-function(datasetAndPackagedetails)
# {
# return(uadatasets.sk$BSkyDataFramePackageDetails[which(uadatasets.sk$BSkyDataFramePackageDetails$Keys ==datasetAndPackagedetails),3])
# }

# BSkyGetPackageNameFromPackageDatasetList <-function(datasetAndPackagedetails)
# {
# return(uadatasets.sk$BSkyDataFramePackageDetails[which(uadatasets.sk$BSkyDataFramePackageDetails$Keys ==datasetAndPackagedetails),1])
# }


## 06Dec2023
#The following functon is modifed locally, but should be in the original file in the BSky package
BSkyGetDatasetNameFromPackageDatasetList <- function (datasetAndPackagedetails)
{
	if(is.null(uadatasets.sk$BSkyDataFramePackageDetails) || (is.data.frame(uadatasets.sk$BSkyDataFramePackageDetails) && dim(uadatasets.sk$BSkyDataFramePackageDetails)[1] < 1))
	{
		input_string = datasetAndPackagedetails #example: "pistonrings-[Piston rings data]-qcc"
		# Specify the character to match
		character_to_match <- "-"

		# Use sub() to extract characters before the first occurrence of the matching character '-'
		datasetName <- sub(paste0(character_to_match, ".*"), "", input_string)
		
		# Use sub() to extract characters after the last occurrence of the matching character '-'
		pkgName <- sub(paste0(".*", character_to_match), "", input_string)
		
		eval(parse(text=paste("require(",pkgName,")")))
		
		invisible(return(datasetName))
	}
	else
	{
		return(uadatasets.sk$BSkyDataFramePackageDetails[which(uadatasets.sk$BSkyDataFramePackageDetails$Keys ==
			datasetAndPackagedetails), 3])
	}
}

## 06Dec2023
#The following functon is modifed locally, but should be in the original file in the BSky package
BSkyGetPackageNameFromPackageDatasetList <- function (datasetAndPackagedetails)
{
	if(is.null(uadatasets.sk$BSkyDataFramePackageDetails) || (is.data.frame(uadatasets.sk$BSkyDataFramePackageDetails) && dim(uadatasets.sk$BSkyDataFramePackageDetails)[1] < 1))
	{
		input_string = datasetAndPackagedetails #example: "pistonrings-[Piston rings data]-qcc"
		# Specify the character to match
		character_to_match <- "-"

		# Use sub() to extract characters before the first occurrence of the matching character '-'
		datasetName <- sub(paste0(character_to_match, ".*"), "", input_string)
		
		# Use sub() to extract characters after the last occurrence of the matching character '-'
		pkgName <- sub(paste0(".*", character_to_match), "", input_string)
		
		eval(parse(text=paste("require(",pkgName,")")))
		
		cat("\n", paste("Loading dataset:", datasetName, "from R Package:", pkgName), "\n")
		invisible(return(pkgName))
	}
	else
	{
		return(uadatasets.sk$BSkyDataFramePackageDetails[which(uadatasets.sk$BSkyDataFramePackageDetails$Keys ==
			datasetAndPackagedetails), 1])
	}
}
