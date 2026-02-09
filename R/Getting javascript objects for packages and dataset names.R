

BSkyGetDatasetNameTitle <- function(package = "") {
  if (package == "" || package == "All_Installed_Packages") {
    uadatasets.sk$BSkyDataFramePackageDetails <- as.data.frame(data(package = .packages(all.available = TRUE))$results)
    uadatasets.sk$BSkyDataFramePackageDetails <- uadatasets.sk$BSkyDataFramePackageDetails %>%
      dplyr::filter(BSkyisValidName(Item)) %>%
      dplyr::arrange(tolower(Item))
  } else {
    uadatasets.sk$BSkyDataFramePackageDetails <- as.data.frame(data(package = package)$results)
    uadatasets.sk$BSkyDataFramePackageDetails <- uadatasets.sk$BSkyDataFramePackageDetails %>%
      dplyr::filter(BSkyisValidName(Item)) %>%
      dplyr::arrange(tolower(Item))
  }

  uadatasets.sk$BSkyDataFramePackageDetails <- uadatasets.sk$BSkyDataFramePackageDetails %>%
    mutate(Keys = paste(Item, "-[", Title, "]-", Package, sep = ""))

  # return a character vector (not a data.frame)
  uadatasets.sk$BSkyDataFramePackageDetails$Keys
}

BSkyCreatePkgDatasetJson <- function(jsonfilepath = "C:/pkgdataset.json") {
library(dplyr)
library(jsonlite)
BlueSky::BSkyGetDatasetNameTitle() 
# ---- Packages ----
AllPackages <- c("All_Installed_Packages", installed.packages()[, 1])
AllPackages <- sort(unique(as.character(AllPackages)))

# ---- Datasets (as character vector) ----
DatasetsWithDescandPackage <- BSkyGetDatasetNameTitle()

# (optional) ensure character + drop NA/empty
DatasetsWithDescandPackage <- as.character(DatasetsWithDescandPackage)
DatasetsWithDescandPackage <- DatasetsWithDescandPackage[nzchar(DatasetsWithDescandPackage)]

# ---- Build packageToDataset map ----
# Your dataset strings end with "-<PackageName>" so extract text after last "-"
pkgs_for_ds <- sub(".*-([^\\-]+)$", "\\1", DatasetsWithDescandPackage)

# split datasets by extracted package name
packageToDataset <- split(DatasetsWithDescandPackage, pkgs_for_ds)

# (optional) keep stable ordering
packageToDataset <- packageToDataset[order(tolower(names(packageToDataset)))]

# ---- Build final object ----
packages_and_datasets <- list(
  packages = AllPackages,
  datasets = DatasetsWithDescandPackage,
  packageToDataset = packageToDataset
)

# ---- Write JSON ----
packages_and_datasets_json <- jsonlite::toJSON(
  packages_and_datasets,
  auto_unbox = TRUE,
  pretty = TRUE
)

writeLines(packages_and_datasets_json, jsonfilepath)
           #"C:/Users/aaron.rangel/Documents/packages_and_datasets3.json")
}