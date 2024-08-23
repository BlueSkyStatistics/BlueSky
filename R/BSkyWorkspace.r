BSkySaveProjectSpace <- function(save = "all", filedir='C:/Users/User/Downloads/bskyproject.bsp') 
{
	library(zip)
	
	#if(!file.exists(filedir))
	#{
		#BSkyFormat(paste("The file directory does not exist", filedir))
		#invisible(return())
	#}
		
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
		
		# Step 1: Save the objects to different temporary files
		#temp_file1 <- tempfile(fileext = ".RData")
		temp_file1 <- tempfile()
		temp_file2 <- tempfile()
		temp_file3 <- tempfile()
		temp_file4 <- tempfile()
		
		temp_file1 = paste0(temp_file1, "_user_workspace.RData")
		temp_file2 = paste0(temp_file2, "_bsky_env1.RData")
		temp_file3 = paste0(temp_file3, "_bsky_env2.RData")
		temp_file4 = paste0(temp_file4, "_bsky_env3.RData")
		
		#suppressWarnings(save.image(file = paste(filedir, "user_workspace.RData", sep='/')))
		suppressWarnings(save.image(file = temp_file1))
		
		# Save all the BSky environments that do not get saved by R workspace save
		#save(list=ls(name = uadatasets), envir = uadatasets, file = paste(filedir,"bsky_env1.RData", sep='/'))
		#save(list=ls(name = uadatasets.sk), envir = uadatasets.sk, file = paste(filedir, "bsky_env2.RData", sep='/'))
		#save(list = ls(name = ualog), envir = ualog, file = paste(filedir, "bsky_env3.RData", sep='/'))
		
		save(list=ls(name = uadatasets), envir = uadatasets, file = temp_file2)
		save(list=ls(name = uadatasets.sk), envir = uadatasets.sk, file = temp_file3)
		save(list = ls(name = ualog), envir = ualog, file = temp_file4)
		
		# Save all data.frame objects into a RData file eventhough saving workspace save all the UI=grid datasets
		#save(list = uadatasets$name, envir = .GlobalEnv, file = paste(filedir, "grid_data_frames.RData", sep='/'))
		
		#BSkyFormat(paste("Workspace and Datasets saved succesfully at", filedir))
		
		# Add all temp files to the zip archive - utils::zip() requires external zip software installed on the machine first
		#utils::zip(filedir, files = c(temp_file1, temp_file2, temp_file3, temp_file4))
		
		# Add all temp files to the zip archive - zip::zipr() does not need any external zip software installed on the machine
		
		if(!file.exists(filedir))
		{
		zip::zipr(filedir, files = c(temp_file1, temp_file2, temp_file3, temp_file4),
					compression_level = 9, 
					include_directories = FALSE)
		}
		else
		{
			zip::zipr_append(filedir, files = c(temp_file1, temp_file2, temp_file3, temp_file4),
					compression_level = 9, 
					include_directories = FALSE)
		}

		# Delete the temporary files
		unlink(temp_file1)
		unlink(temp_file2)
		unlink(temp_file3)
		unlink(temp_file4)
		
	}else
	{
		# Save all data frame objects into an RData file
		save(list = uadatasets$name, envir = .GlobalEnv, file = paste(filedir, "grid_data_frames.RData", sep='/'))
		
		#BSkyFormat(paste("Datasets saved succesfully at", filedir))
	}
}


BSkyLoadProjectSpace <- function(load = "all", filedir='C:/Users/User/Downloads/bskyproject.bsp')
{
	currentGraphicsDir = BSkyGetGraphicsDirPath()
	if(load == "all") 
	{
		# Load data into all three BSky environments
		# load(envir = uadatasets, file = paste(filedir,"bsky_env1.RData", sep='/'))
		# load(envir = uadatasets.sk, file = paste(filedir,"bsky_env2.RData", sep='/'))
		# load(envir = ualog, file = paste(filedir,"bsky_env3.RData", sep='/'))
		
		# Load the current user's workspace i.e. envir = .GlobalEnv
		#load(envir = .GlobalEnv, file = paste(filedir, "user_workspace.RData", sep='/'))
		
		# List the contents of the zip file
		zip_contents <- utils::unzip(filedir, list = TRUE)
		
		############################################
		# Load data into all three BSky environments
		############################################
			
		# Find the specific file name that matches the pattern
		matched_file <- zip_contents$Name[grep("bsky_env1", zip_contents$Name)]

		# Check if a file was found
		if (length(matched_file) > 0) {
		  
			# Open a connection to the matched file using `unz()`
			file_connection <- unz(filedir, matched_file)

			# Load the data from the connection
			load(envir = uadatasets, file_connection)
			
			cat("\nLoaded envir = uadatasets successfully\n")

			# Close the connection
			close(file_connection)
		}
		
		# Find the specific file name that matches the pattern
		matched_file <- zip_contents$Name[grep("bsky_env2", zip_contents$Name)]

		# Check if a file was found
		if (length(matched_file) > 0) {
		  
			# Open a connection to the matched file using `unz()`
			file_connection <- unz(filedir, matched_file)

			# Load the data from the connection
			load(envir = uadatasets.sk, file_connection)
			
			cat("\nLoaded envir = uadatasets.sk successfully\n")

			# Close the connection
			close(file_connection)
		}
		
		# Find the specific file name that matches the pattern
		matched_file <- zip_contents$Name[grep("bsky_env3", zip_contents$Name)]

		# Check if a file was found
		if (length(matched_file) > 0) {
		  
			# Open a connection to the matched file using `unz()`
			file_connection <- unz(filedir, matched_file)

			# Load the data from the connection
			load(envir = ualog, file_connection)
			
			cat("\nLoaded envir = ualog successfully\n")

			# Close the connection
			close(file_connection)
		}
		
		# Find the specific file name that matches the pattern
		matched_file <- zip_contents$Name[grep("user_workspace", zip_contents$Name)]

		# Check if a file was found
		if (length(matched_file) > 0) {
		  
			# Open a connection to the matched file using `unz()`
			file_connection <- unz(filedir, matched_file)

			# Load the data from the connection
			load(envir = .GlobalEnv, file_connection)
			
			cat("\nLoaded envir = .GlobalEnv with user_workspace successfully\n")

			# Close the connection
			close(file_connection)
		}
		
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
	invisible(return(BSkySetGraphicsDirPath(currentGraphicsDir)))
}


#############################################################################################################
# Older version of the two functions before switching to .bsp zip file on disk to manage project at all times
#############################################################################################################

BSkySaveProjectSpace.OLD <- function(save = "all", filedir='C:/Users/User/Downloads') 
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


BSkyLoadProjectSpace.OLD <- function(load = "all", filedir='C:/Users/User/Downloads')
{
	currentGraphicsDir = BSkyGetGraphicsDirPath()
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
	invisible(return(BSkySetGraphicsDirPath(currentGraphicsDir)))
}