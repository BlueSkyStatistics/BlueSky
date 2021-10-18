############################################################
# Just Last Modification Date. Time can also be included
# ver: 2011.w.d  #W starts on Mon=1
############################################################
### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title BlueSky package adn other details
#'
#' @description Get BlueSky package details along with R session related details.
#'
#'
#' @return prints details about system, R session etc.
#'
#' @examples BSkyVersion()
BSkyVersion<-function()
{
bskyver= "Version: 7.77"
bskydate="Date: 2021-10-17"
bskytime="10:46AM"
rver = R.Version()
print("------ BlueSky R package version ------")
print(bskyver) 
print(bskydate)
print(bskytime) 

cat("\n\n------ R Home ------\n")
print(R.home())

cat("\n\n------ R library paths ------\n")
print(.libPaths()) 

cat("\n\n------ R version ------\n")
print(rver)

cat("\n\n------ R SessionInfo ------\n")
print(sessionInfo())

cat("\n\n------ Encoding ------\n")
print(getOption("encoding"))

cat("\n\n------ Internationalization ------\n")
print(l10n_info())

cat("\n\n------ System Info ------\n")
print(Sys.info()[c(1:3,5)])

# location of R's own temp directory
cat("\n\n------ R's temp directory ------\n")
print(tempdir())

# R's working directory
cat("\n\n------ R's working directory ------\n")
print(getwd())

# finding .Rprofile file path 
candidates <- c( Sys.getenv("R_PROFILE"),
                 file.path(Sys.getenv("R_HOME"), "etc", "Rprofile.site"),
                  Sys.getenv("R_PROFILE_USER"),
                  file.path(getwd(), ".Rprofile") )
cat("\n\n------ Rprofile details ------\n")
Filter(file.exists, candidates)
}

# "bskyfrmtobj <- BSkyFormat(bskytempvarname, bSkyFormatAppRequest = TRUE, singleTableOutputHeader = \"c(\"a\")\" )"
# "bskyfrmtobj <- BSkyFormat(bskytempvarname, bSkyFormatAppRequest = TRUE, singleTableOutputHeader = \"c(\\\"a\\\")\" )"


# To find the version of the currently installed, required R packages.
BSkyInstalledReqPkgVer  <-function () 
{
 Rver <- paste("R Ver :", R.Version())

 installedpackages <- installed.packages()[,1]
  
 v1=character(0)
 if(is.element("openxlsx", installedpackages))
	v1 <- paste("openxlsx :", packageVersion("openxlsx"))
 v2=character(0)
 if(is.element("dplyr", installedpackages))	
	v2 <- paste("dplyr :", packageVersion("dplyr"))
	
 v3=character(0)
 if(is.element("haven", installedpackages))		
	v3 <- paste("haven :", packageVersion("haven"))
 
  v4=character(0)
 if(is.element("readr", installedpackages))	
	v4 <- paste("readr :", packageVersion("readr"))
 
  v5=character(0)
 if(is.element("readxl", installedpackages))	
	v5 <- paste("readxl :", packageVersion("readxl"))
 
  v6=character(0)
 if(is.element("data.table", installedpackages))	
	v6 <- paste("data.table :", packageVersion("data.table"))
 
 v7=character(0)
 if(is.element("foreign", installedpackages))	
	v7 <- paste("foreign :", packageVersion("foreign"))

 v8=character(0)
 if(is.element("BlueSky", installedpackages))	
	v8 <- paste("BlueSky :", packageVersion("BlueSky"))
 
  v9=character(0)
 if(is.element("gmodels", installedpackages))	
	v9  <- paste("gmodels :", packageVersion("gmodels"))
 
  v10=character(0)
 if(is.element("GPArotation", installedpackages))	
	v10 <- paste("GPArotation :", packageVersion("GPArotation"))
 
  v11=character(0)
 if(is.element("RcmdrMisc", installedpackages))	
	v11 <- paste("RcmdrMisc :", packageVersion("RcmdrMisc"))
 
  v12=character(0)
 if(is.element("corrplot", installedpackages))	
	v12 <- paste("corrplot :", packageVersion("corrplot"))
 
  v13=character(0)
 if(is.element("ggplot2", installedpackages))	
	v13 <- paste("ggplot2 :", packageVersion("ggplot2"))
 
  v14=character(0)
 if(is.element("ggthemes", installedpackages))	
	v14 <- paste("ggthemes :", packageVersion("ggthemes"))
 
  v15=character(0)
 if(is.element("gplots", installedpackages))	
	v15 <- paste("gplots :", packageVersion("gplots"))
 
  v16=character(0)
 if(is.element("aplpack", installedpackages))	
	v16 <- paste("aplpack :", packageVersion("aplpack"))
 
  v17=character(0)
 if(is.element("car", installedpackages))	
	v17 <- paste("car :", packageVersion("car"))
 
  v18=character(0)
 if(is.element("Rmisc", installedpackages))	
	v18 <- paste("Rmisc :", packageVersion("Rmisc"))
 
  v19=character(0)
 if(is.element("choroplethrMaps", installedpackages))	
	v19 <- paste("choroplethrMaps :", packageVersion("choroplethrMaps"))
 
   v20=character(0)
 if(is.element("choroplethr", installedpackages))	
	v20 <- paste("choroplethr :", packageVersion("choroplethr"))
	
  v21=character(0)
 if(is.element("stringr", installedpackages))	
	v21 <- paste("stringr :", packageVersion("stringr"))
 
  v22=character(0)
 if(is.element("gdata", installedpackages))	
	v22 <- paste("gdata :", packageVersion("gdata"))
	##### added more below
  v23=character(0)
 if(is.element("forecast", installedpackages))	
	v23 <- paste("forecast :", packageVersion("forecast"))

  v24=character(0)
 if(is.element("ggthemes", installedpackages))	
	v24 <- paste("ggthemes :", packageVersion("ggthemes"))

  v25=character(0)
 if(is.element("gplots", installedpackages))	
	v25 <- paste("gplots :", packageVersion("gplots"))	
	
  v26=character(0)
 if(is.element("randomForest", installedpackages))	
	v26 <- paste("randomForest :", packageVersion("randomForest"))

  v27=character(0)
 if(is.element("caret", installedpackages))	
	v27 <- paste("caret :", packageVersion("caret"))

  v28=character(0)
 if(is.element("klaR", installedpackages))	
	v28 <- paste("klaR :", packageVersion("klaR"))

  v29=character(0)
 if(is.element("rpart", installedpackages))	
	v29 <- paste("rpart :", packageVersion("rpart"))

  v30=character(0)
 if(is.element("rpart.plot", installedpackages))	
	v30 <- paste("rpart.plot :", packageVersion("rpart.plot"))

  v31=character(0)
 if(is.element("rattle", installedpackages))	
	v31 <- paste("rattle :", packageVersion("rattle"))

  v32=character(0)
 if(is.element("RJDBC", installedpackages))	
	v32 <- paste("RJDBC :", packageVersion("RJDBC"))

  v33=character(0)
 if(is.element("DescTools", installedpackages))	
	v33 <- paste("DescTools :", packageVersion("DescTools"))

  v34=character(0)
 if(is.element("arules", installedpackages))	
	v34 <- paste("arules :", packageVersion("arules"))

  v35=character(0)
 if(is.element("arulesViz", installedpackages))	
	v35 <- paste("arulesViz :", packageVersion("arulesViz"))

  v36=character(0)
 if(is.element("iplots", installedpackages))	
	v36 <- paste("iplots :", packageVersion("iplots"))

  v37=character(0)
 if(is.element("rJava", installedpackages))	
	v37 <- paste("rJava :", packageVersion("rJava"))

  v38=character(0)
 if(is.element("fastAdaboost", installedpackages))	
	v38 <- paste("fastAdaboost :", packageVersion("fastAdaboost"))

  v39=character(0)
 if(is.element("monomvn", installedpackages))	
	v39 <- paste("monomvn :", packageVersion("monomvn"))

  v40=character(0)
 if(is.element("kernlab", installedpackages))	
	v40 <- paste("kernlab :", packageVersion("kernlab"))




 
 #allver <- c( v1,'\n',v2,'\n',v3,'\n',v4,'\n',v5,'\n',v6,'\n',v7,'\n',v8)
 allver <- list(Rver, v1,v2,v3,v4,v5,v6,v7,v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31, v32, v33, v34, v35, v36, v37, v38, v39, v40)
 return(allver)
}

