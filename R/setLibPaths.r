#get Sys.getenv('R_LIBS_USER')
#check if it exists
#create the path if it doesn't exist.
#set new path in .libPath()
# use trycatch in your code

# uber function : adds new userlib path (creates it if it does not exist)
# prints a message on failure and returns success flag.
setNewLibPath <- function(newlibpath="")
{
	#cat("\nStart setNewLibPath")
	success = FALSE
	if(TRUE)#(BSkyIsRmarkdownOutputOn()== FALSE)## if its BlueSky app environment
	{
		cat("\nUser lib to be added:\n")
		userlib = checkNcreateUserLibs(newlibpath)
		print(userlib)
		if(nchar(userlib)>2) #min ./x
		{
			# cat("\n inside inner if")
			success = addNewUserLib(userlib)
		}
		if(!success)
		{
			cat("\nFailed to add user lib path: ")
			cat(newlibpath)
			cat("\n")
		}
	}
	# cat("\n returning")
	return(success)
}

checkNcreateUserLibs <- function(newlib="")
{
	success = FALSE
	userlib = newlib
	if(nchar(newlib)==0)##is.null(newlib) || is.na(newlib)) #is newlib not passed use R default
	{
		#get R default
		userlib = Sys.getenv('R_LIBS_USER')
	}
	
	#check if path exists else create it
	dirExist = dir.exists(paths=userlib)
	
	if(!dirExist) #create path if it does not exist
	{
		#reset global error-warning flag
		eval(parse(text="bsky_opencommand_execution_an_exception_occured = FALSE"), envir=globalenv())
		
		#trying to create new user lib path
		tryCatch({
		
				withCallingHandlers({					
					dir.create(path=userlib, recursive=TRUE)
					success = TRUE
				}, warning = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
		}, error = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
		
		if(bsky_opencommand_execution_an_exception_occured == FALSE)## Success
		{
			success = TRUE
			## maybe return 0 for success
			cat("\nUser library path created.") 
		}
		else ## Failure
		{
			cat("\nFailed to create user lib path: ")
			## maybe return -1 for failure
			success = FALSE;
		}
	}
	else 
	{
		success=TRUE
	}
	return(userlib)
}

#add new lib path the libPaths
addNewUserLib <- function(newlib = "")
{
	success = FALSE
	if(nchar(newlib) > 0)
	{
		#check if path exists else no need to set it
		dirExist = dir.exists(paths=newlib)
		if(dirExist) 
		{	
			.libPaths( c(.libPaths(), newlib) )
			success = TRUE
		}
	}
	return(success)
}


