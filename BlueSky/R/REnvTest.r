getEnv <- function(x) 
{
  xobj <- deparse(substitute(x))
  gobjects <- ls(envir=.GlobalEnv)
  envirs <- gobjects[sapply(gobjects, function(x) is.environment(get(x)))]
  envirs <- c('.GlobalEnv', envirs)
  xin <- sapply(envirs, function(e) xobj %in% ls(envir=get(e)))
  envirs[xin] 
}

BSkyEnvTest <- function(varname)
{
	varenv <- eval( parse(text=paste('getEnv(varname)',sep='')))
	print(varenv)
	
	varenv2 <- eval( parse(text=paste('getEnv(summary)',sep='')))
	print(varenv2)	
	
}