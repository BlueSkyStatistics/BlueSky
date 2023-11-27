BSkySaveProjectSpace <- function(save = "all", filedir='C:/Users/User/Downloads') 
{
	if(!file.exists(filedir))
	{
		#BSkyFormat(paste("The file directory does not exist", filedir))
		invisible(return())
	}
		
	if(save == "all")
	{
		# Get a list of loaded libraries
		loaded_libraries <- search()

		# Filter the list to extract only the library names
		loaded_library_names <- loaded_libraries[grep("^package:", loaded_libraries)]

		# Remove the "package:" prefix to get clean library names
		.GlobalEnv$loaded_library_names <- rev(sub("^package:", "", loaded_library_names))

		# All the currently open dataset names on the UI grid
		.GlobalEnv$grid_dataset_names = uadatasets$name
		
		#print(.GlobalEnv$grid_dataset_names)

		# Save everything in the current user's workspace i.e. envir = .GlobalEnv
		#save(list = ls(all.names = TRUE), file = paste(filedir, "user_workspace.RData", sep='/'), envir = .GlobalEnv)
		suppressWarnings(save.image(file = paste(filedir, "user_workspace.RData", sep='/')))
		
		# Save all the BSky environments that do not get saved by R workspace save
		save(list=ls(name = uadatasets), envir = uadatasets, file = paste(filedir,"bsky_env1.RData", sep='/'))
		save(list=ls(name = uadatasets.sk), envir = uadatasets.sk, file = paste(filedir, "bsky_env2.RData", sep='/'))
		save(list = ls(name = ualog), envir = ualog, file = paste(filedir, "bsky_env3.RData", sep='/'))
		
		# Save all data.frame objects into a RData file eventhough saving workspace save all the UI=grid datasets
		#save(list = uadatasets$name, envir = .GlobalEnv, file = paste(filedir, "grid_data_frames.RData", sep='/'))
		
		#BSkyFormat(paste("Workspace and Datasets saved succesfully at", filedir))
		
	}else
	{
		# Save all data frame objects into an RData file
		save(list = uadatasets$name, envir = .GlobalEnv, file = paste(filedir, "grid_data_frames.RData", sep='/'))
		
		#BSkyFormat(paste("Datasets saved succesfully at", filedir))
	}
}


BSkyLoadProjectSpace <- function(load = "all", filedir='C:/Users/User/Downloads')
{
	if(load == "all") 
	{
		if(!file.exists(paste(filedir,"bsky_env1.RData", sep='/')))
		{
			#BSkyFormat(paste("Project workspace cannot be restored. Required files are missing in", filedir))
			invisible(return())
		}
		
		if(!file.exists(paste(filedir,"bsky_env2.RData", sep='/')))
		{
			#BSkyFormat(paste("Project workspace cannot be restored. Required files are missing in", filedir))
			invisible(return())
		}
		
		if(!file.exists(paste(filedir,"bsky_env3.RData", sep='/')))
		{
			#BSkyFormat(paste("Project workspace cannot be restored. Required files are missing in", filedir))
			invisible(return())
		}
		
		if(!file.exists(paste(filedir, "user_workspace.RData", sep='/')))
		{
			#BSkyFormat(paste("Project workspace cannot be restored. Required files are missing in", filedir))
			invisible(return())
		}
		
		# Load data into all three BSky environments
		#load(envir = asNamespace('BlueSky'), file = paste(filedir,"bsky_env1.RData", sep='/'))
		#load(envir = asNamespace('BlueSky'), file = paste(filedir,"bsky_env2.RData", sep='/'))
		#load(envir = asNamespace('BlueSky'), file = paste(filedir,"bsky_env3.RData", sep='/'))
		
		#load(envir = uadatasets, verbose = TRUE, file = paste(filedir,"bsky_env1.RData", sep='/'))
		#load(envir = uadatasets.sk, verbose = TRUE, file = paste(filedir,"bsky_env2.RData", sep='/'))
		#load(envir = ualog, verbose = TRUE, file = paste(filedir,"bsky_env3.RData", sep='/'))
		
		load(envir = uadatasets, file = paste(filedir,"bsky_env1.RData", sep='/'))
		load(envir = uadatasets.sk, file = paste(filedir,"bsky_env2.RData", sep='/'))
		load(envir = ualog, file = paste(filedir,"bsky_env3.RData", sep='/'))
		
		
		# Load the current user's workspace i.e. envir = .GlobalEnv
		load(envir = .GlobalEnv, file = paste(filedir, "user_workspace.RData", sep='/'))
		
		# Load libraries in sequence from the list of libraries loaded previously 
		dummy = lapply(.GlobalEnv$loaded_library_names, function(libname) eval(parse(text=paste("require(",libname,")"))))

		# Load all UI grid data frame objects
		dummy = lapply(.GlobalEnv$grid_dataset_names, BSkyLoadRefresh, createAttr=FALSE)
		
		#BSkyFormat(paste("Workspace and Datasets loaded succesfully from", filedir))
	
	}
	else
	{
		if(!file.exists(paste(filedir, "grid_data_frames.RData", sep='/')))
		{
			#BSkyFormat(paste("Datasets cannot be loaded. Required file is missing in", filedir))
			invisible(return())
		}
		
		# Load all UI grid data frame objects
		.GlobalEnv$grid_dataset_names = load(file = paste(filedir, "grid_data_frames.RData", sep='/'), verbose = TRUE, envir = .GlobalEnv)
		dummy = lapply(.GlobalEnv$grid_dataset_names, BSkyLoadRefresh, createAttr=FALSE)
		
		#BSkyFormat(paste("Datasets loaded succesfully from", filedir))
	}	
}