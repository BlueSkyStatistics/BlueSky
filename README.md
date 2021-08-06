# BlueSkyRpkg
BlueSky R package

# Overview

The BlueSky package contains all of the functions that BlueSky Statistics 10 and later use. It is installed automatically when BlueSky Statistics is installed. BlueSky output files use extension ".Bmd". If you export those to R Markdown files (".Rmd"), users of RStudio, or similar software that run R Markdown, can execute that code if they install the BlueSky package. 

# Installation

To install the BlueSky package, use the following steps:

```{r}
install.packages("devtools")
devtools::install_github("BlueSkyStatistics/BlueSkyRpkg")
```
