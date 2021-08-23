BSkyGetDatasetNamesfunction <-function(package ="")
{
    if (package=="")
    {
    uadatasets.sk$BSkyDataFramePackageDetails <-as.data.frame(data(package = .packages(all.available = TRUE))$results)
    }
    else
    {
    uadatasets.sk$BSkyDataFramePackageDetails <-as.data.frame(data(package = package)$results)
    }
    uadatasets.sk$BSkyDataFramePackageDetails <-uadatasets.sk$BSkyDataFramePackageDetails %>%
         mutate( Keys =paste(Item , "-[", Title, "]","-",Package, sep="" ))
    return(BSkyDataFramePackageDetails[,c("Keys")])

}


BSkyGetDatasetName <-function(datasetAndPackagedetails)
{
return(uadatasets.sk$BSkyDataFramePackageDetails[which(uadatasets.sk$BSkyDataFramePackageDetails$Keys ==datasetAndPackagedetails),2])
}

BSkyGetPackageName <-function(datasetAndPackagedetails)
{
return(uadatasets.sk$BSkyDataFramePackageDetails[which(uadatasets.sk$BSkyDataFramePackageDetails$Keys ==datasetAndPackagedetails),1])
}


