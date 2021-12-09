BSkyGetDatasetNameTitle <-function(package ="")
{
    if (package == ""|| package =="All_Installed_Packages") {
        uadatasets.sk$BSkyDataFramePackageDetails <- as.data.frame(data(package = .packages(all.available = TRUE))$results)
		uadatasets.sk$BSkyDataFramePackageDetails  <-uadatasets.sk$BSkyDataFramePackageDetails %>% dplyr::arrange(tolower(Item))
    }
    else {
        uadatasets.sk$BSkyDataFramePackageDetails <- as.data.frame(data(package = package)$results)
		uadatasets.sk$BSkyDataFramePackageDetails <-uadatasets.sk$BSkyDataFramePackageDetails %>% dplyr::filter(isValidName(Item)) %>% dplyr::arrange(tolower(Item))
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
BSkyGetDatasetNameFromPackageDatasetList <-function(datasetAndPackagedetails)
{
return(uadatasets.sk$BSkyDataFramePackageDetails[which(uadatasets.sk$BSkyDataFramePackageDetails$Keys ==datasetAndPackagedetails),3])
}

BSkyGetPackageNameFromPackageDatasetList <-function(datasetAndPackagedetails)
{
return(uadatasets.sk$BSkyDataFramePackageDetails[which(uadatasets.sk$BSkyDataFramePackageDetails$Keys ==datasetAndPackagedetails),1])
}


