#The function BSkyInstallPkg checks if a package is installed, then checks if its a valid R package and then installs it from CRAN
# The call x %in% rownames(available.packages() takes a few secs, I need to think whether its worthwhile
Old.BSkyInstallPkg <- function(x)
{
	mustloadpkgs <- c("data.table", "foreign", "readxl","readr", "haven")

    if(x %in% rownames(installed.packages())==FALSE) 
    {
	#cat("\n Installing from CRAN")
    #if(x %in% rownames(available.packages())==FALSE) 
    #{
    #    retval = paste(x,"is not a valid package - please check again...")
    #} 
    #else 
    #{
    #trycatch(
      install.packages(x,repos ="http://cran.us.r-project.org")  

	  #after installing we are checking if its installed or not
	  if(isPackageInstalled(x))
	  {
		  #cat("\n Successfully installed from CRAN")
		  retval =paste("Package",x,"Installed successfully" )
		  if(x %in% mustloadpkgs)
		  {
			#cat("\n Must Load : Trying to load")
			if(!isPackageloaded(x)) # load if not loaded, but is installed. and is one of the mustloadpkgs
				eval(parse(text=paste('library(',x,')'))) 
			if(isPackageloaded(x))
			{
				#cat("\n Success Loading")
				paste("Package",x,"loaded successfully" )
			}
			else
			{
				#cat("\n Loading Failed")
				paste("Package",x,"loading failed" )
			}
		  }
	  }
	  else
	  {
		#cat("\n Install from CRAN Failed")
		retval =paste("Package",x,"Installation failed." )
	  }
     #   }
    } 
	else 
	{
		#cat("\n Already installed;")
		retval =paste(x,"package already installed...")
	}
}

# Find if R package is installed or not
isPackageInstalled <- function(pkgname)
{
	if(pkgname %in% rownames(installed.packages())== TRUE) 
	{
		return(TRUE) # insalled
	}
	else
	{
		return(FALSE) # not installed
	}
}

# Find if R package is loaded in memory or not
isPackageloaded <- function(pkgname)
{
	fullpkgname <- paste("package:", pkgname, sep='', collapse='')

	if(fullpkgname %in% search() == TRUE) 
	{
		return(TRUE) # package loaded
	}
	else
	{
		return(FALSE) # not loaded
	}
}


#01Oct2015 Improved version from Aaron.
BSkyInstallPkg  <-function (x, lib) 
{
    mustloadpkgs <- c("data.table", "foreign", "readxl", "readr", "haven","stringr", "gdata", "openxlsx")
    if (x %in% rownames(installed.packages()) == FALSE) {
        install.packages(x, lib=lib)
        if (isPackageInstalled(x)) {
            retval = paste("Package", x, "Installed successfully")
            if (x %in% mustloadpkgs) {
                if (!isPackageloaded(x)) {
                  eval(parse(text = paste("library(", x, ")")))
               }
                if (isPackageloaded(x)) {
                  paste("Package", x, "loaded successfully")
                }
                else {
                  paste("Package", x, "loading failed")
                }
            }
        }
        else {
            retval = paste("Package", x, "Installation failed.")
        }
    }
    else {
        retval = paste(x, "package already installed...")
    }
}

### Initial list : 27Aug2015 ###
###############
#Installs BlueSky  packages
###############
# BSkyInstallPkg("data.table")
# BSkyInstallPkg("foreign")
# BSkyInstallPkg("gmodels")
# BSkyInstallPkg("GPArotation")
# BSkyInstallPkg("RcmdrMisc")
# BSkyInstallPkg("corrplot")
# BSkyInstallPkg("forecast")
# BSkyInstallPkg("car")
# BSkyInstallPkg("ggplot2")
# BSkyInstallPkg("ggthemes")
# BSkyInstallPkg("ggthemes")
# BSkyInstallPkg("dplyr")
# BSkyInstallPkg("gplots")
# BSkyInstallPkg("aplpack")
###############