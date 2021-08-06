# BlueSkyRpkg
BlueSky R package

# Overview

The BlueSky package contains all of the functions that are unique to BlueSky Statistics 10 and later. It is installed automatically along with BlueSky Statistics. BlueSky output files use extension ".Bmd". If you export those to R Markdown files (".Rmd"), users of RStudio, or similar software that runs R Markdown, can execute that code if they install BlueSkyRpkg. 

# Installation

To install the BlueSky package, use the following steps:

```{r}
install.packages("devtools")
devtools::install_github("BlueSkyStatistics/BlueSkyRpkg")
```

# Usage

`BlueSkyRpkg` is used like any other R package. After installing it, load the package before executing any R Markdown code exported from BlueSky Statistics:

```{r}
library("BlueSkyRpkg")
```

# Getting Help

For details, see the chapter, "Working with R," in the *BlueSky 10 User Guide*.
