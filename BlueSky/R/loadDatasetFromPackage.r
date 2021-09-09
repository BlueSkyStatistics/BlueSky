BSkyGetDatasetNameTitle <-function(package ="")
{
    if (package == "") {
        uadatasets.sk$BSkyDataFramePackageDetails <- as.data.frame(data(package = .packages(all.available = TRUE))$results)
		uadatasets.sk$BSkyDataFramePackageDetails  <-uadatasets.sk$BSkyDataFramePackageDetails %>% dplyr::arrange(tolower(Item))
    }
    else {
        uadatasets.sk$BSkyDataFramePackageDetails <- as.data.frame(data(package = package)$results)
		uadatasets.sk$BSkyDataFramePackageDetails <-uadatasets.sk$BSkyDataFramePackageDetails %>% dplyr::arrange(tolower(Item))
    }
    uadatasets.sk$BSkyDataFramePackageDetails <- uadatasets.sk$BSkyDataFramePackageDetails %>%
        mutate(Keys = paste(Item, "-[", Title, "]", "-", Package,
            sep = ""))
    return(uadatasets.sk$BSkyDataFramePackageDetails[, c("Keys")])

}


BSkyGetDatasetNameFromPackageDatasetList <-function(datasetAndPackagedetails)
{
return(uadatasets.sk$BSkyDataFramePackageDetails[which(uadatasets.sk$BSkyDataFramePackageDetails$Keys ==datasetAndPackagedetails),3])
}

BSkyGetPackageNameFromPackageDatasetList <-function(datasetAndPackagedetails)
{
return(uadatasets.sk$BSkyDataFramePackageDetails[which(uadatasets.sk$BSkyDataFramePackageDetails$Keys ==datasetAndPackagedetails),1])
}


