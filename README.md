# Overview

The BlueSky package contains all of the functions that are unique to BlueSky Statistics 10 and later. It is installed automatically along with BlueSky Statistics. BlueSky output files use extension ".Bmd". If you export those to R Markdown files (".Rmd"), users of RStudio, or similar software that runs R Markdown, can execute that code if they install the BlueSky package.

# Installation in R or RStudio

To install the BlueSky package, use the following steps:

```{r}
install.packages("remotes")
remotes::install_github("BlueSkyStatistics/BlueSky")
```

# Installation in BlueSky Statistics

The BlueSky R package is installed automatically with the BlueSky Statistics application. It is essential that the application use a version of the package that matches it. Therefore, you should rarely be a need to replace it manually. However, if you are *sure* that is what you need to do, Use the BlueSky menu item, "Triple-dot> Update BlueSKy R Package," or follow these steps:

```{r}
# Are you sure you want to do this?
install.packages("remotes")
remotes::install_github("BlueSkyStatistics/BlueSky", repos = "https://cloud.r-project.org/", upgrade="always")
```

# Usage

The `BlueSky` package is used like any other R package. After installing it, load the package before executing any R Markdown code exported from BlueSky Statistics:

```{r}
library("BlueSky")
```

# Getting Help

For details, see the chapter, "Working with R," in the *BlueSky 10 User Guide*.
