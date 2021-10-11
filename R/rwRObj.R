######## list of methods in this file; in sequence (few may not be ready and unused ) ###########
# UAreadRObj
# UAwriteRObj
#################################################################################################

###################################################################################################################
#Function: UAreadRObj(RObjfileName, datasetname, replace=FALSE)									
#              																			
#Parameters:  
#		RObjfileName			- full path filename of R Object file on disk 
#		datasetname		     	- new refrence name for later use               																
#		replace					- TRUE if you want to overwrite exisiting dataset in UA memory space
#																				
#Description:	
#		Reads R object file from disk and loads in UA memory space. You can refer this dataset later 
#		with provided datasetname
#																						
#Example: UAreadRObj("C:/Data/test.robj", "myrobj", replace=FALSE)																		
###################################################################################################################
UAreadRObj <- function(RObjfileName, datasetname, replace=FALSE) 
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(datasetname)

	BSkyErrMsg = paste("UAreadRObj: Error reading R Obj file : ", "DataSetName :", datasetname," ", "R Obj Filename  :", paste(RObjfileName, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAreadRObj: Warning reading R Obj file : ", "DataSetName :", datasetname," ", "R Obj Filename :", paste(RObjfileName, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)

				##loading the dbf file from disk to uadatasets array
				# New Dataset name is only added if you want to replace existing, Or you will check that it should not already present
				curidx <- UAgetIndexOfDataSet(datasetname) # current index of dataset if its already loaded ( before )
				#cat("\nCur Index of RObj:",curidx)
				if((replace == TRUE) || (replace == FALSE && curidx == -1)){
					# if i>0 then datasetname already exists && you want to replace it.
					if(replace == TRUE && curidx > 0){
						# Delete the existing dataset from the global list first. 
						# if dataset already exists, its index is in i.
						# uadatasets$lst[i]<-NULL
						#UAcloseDataset(datasetname)
						BSkyCloseForReloading(datasetname)
						#cat("DS Closed")						
					}
	
					filetype="RDATA"
					if(length(grep(".RData", RObjfileName, ignore.case = TRUE)==0))
						filetype = "RDA"
			#cat("Before actual function call load():",datasetname)
			#print(Sys.time())			
					# library(RODBC) #no need
					bskysig=NULL ##support for old .RData format where data.fram was stored in UAObj
					UAObj <- NULL
					UAObj$info <- NULL
					UAObj$obj <- NULL
					bsky.class <- ""
					isDFname=FALSE
					
					
					bsky.rdataname <- load(file = RObjfileName)#, uadatasets$lst[Newindex])
					if( length(bsky.rdataname) == 1 && bsky.rdataname=="UAObj")##support old format (BSky format) #D:/BSky/Misc/Datasets/rdata/empfull.rdata
					{
						bskysig <- UAObj$info 
					}
					else
					{
						bsky.class <- class(get(bsky.rdataname))
						
						if(length(bsky.class) > 1)
						{
							#warning("UAreadRObj: .RData file has multiple objects.");
							
							objidxs = which(bsky.class == 'tbl_df') #there may be more than one datasets
							if( length(objidxs) == 0)#19Oct2019
								objidxs = which(bsky.class == 'data.frame')
							if(length(objidxs) > 1)
							{
								#warning("UAreadRObj: .RData file has multiple data.frame(s). Loading the first one only.");
								firstdfidx = objidxs[1]
							}
							else
							{
								firstdfidx = objidxs
							}
							bsky.rdataname <-  get(bsky.rdataname[firstdfidx] )
							bsky.class=class(bsky.rdataname)  #get class for the selected item from the RDATA
							if(length(bsky.class) > 1)
								bsky.class = bsky.class[1]
							
							isDFname=FALSE # no need to set this. It is default
						}
						else
						{
							isDFname=TRUE
							## no need of this block because while running load() above we assigned to bsky.rdataname
							## which works when there is just one data.frame in RData file.
						}
						
					}
					# print(bsky.rdataname);
					if(bsky.class=="tbl_df" || bsky.class=="data.frame")
					{
						#eval( parse(text=paste( datasetname,' <<- get(bsky.rdataname)' , sep=''   )))
						if(isDFname)
						{
							eval( parse(text=paste( '.GlobalEnv$',datasetname,' <- as.data.frame(',bsky.rdataname,')' , sep=''   )))
						}
						else
						{
							eval( parse(text=paste( '.GlobalEnv$',datasetname,' <- as.data.frame(bsky.rdataname)' , sep=''   )))
						}

						uadatasets$name <- c(uadatasets$name, datasetname)

						#Creating extra attributes at column level
						UAcreateExtraAttributes(datasetname, filetype)
					}
					else if(!is.null(bskysig) && bskysig$ver==1.2 && bskysig$name=="BSky")##supporting old format in which we saved dat frame in our UAObj
					{
						eval( parse(text=paste( '.GlobalEnv$',datasetname,' <- UAObj$obj' , sep=''   ))) ## Working
						uadatasets$name <- c(uadatasets$name, datasetname)
						#Creating extra attributes at column level
						UAcreateExtraAttributes(datasetname, filetype)
					}
					else
					{
						BSkyErrMsg =paste("UAreadRObj: .RData file should only contain one data.frame object"," File Name:", RObjfileName)
						warning("UAreadRObj: .RData file should only contain one data.frame object");
						# cat("\nNot a Blue Sky file version.\n")
					}
				}		
				else
				{
					BSkyErrMsg =paste("UAreadRObj: Dataset with the same name already on the global list ."," Dataset Name:", datasetname)
					warning("UAreadRObj: Dataset with the same name already on the global list ")
				}			
			# if(objInfo$ver == 1.2)
			# {
				# cat("Internal Obj...\n")
			# }
			# else
			# {
				# cat("External Object...\n")
			# }
			#cat("Wrapup:",datasetname)
			#print(Sys.time())	
			BSkyFunctionWrapUp()
}




###################################################################################################################
#Function: UAwriteRObj(RObjfileName,dataSetNameOrIndex)							
#              																			
#Parameters:  
#		RObjfileName			- full path filename of new R object file to which particular dataset element 
#								  of uadatasets$lst is to be written
#		dataSetNameOrIndex     	- Dataset Name or Index (of uadatasets$lst array) which is to be 
#								  written to disk file as new R object file. 
#																						
#Description:	
#		Write UA memory space dataset to a R object file format
#																						
#Example: UAwriteRObj("C:/Data/new.robj", "dietstudy")																				
###################################################################################################################

UAwriteRObj <- function(RObjfileName,dataSetNameOrIndex) ##  index of dataset and file
{
	BSkyFunctionInit()
	BSkySetCurrentDatasetName(dataSetNameOrIndex)
	
	BSkyErrMsg = paste("UAwriteRObj: Error writing R Obj file : ", "DataSetName :", dataSetNameOrIndex," ", "R Obj Filename  :", paste(RObjfileName, collapse = ","),sep="")
	BSkyWarnMsg = paste("UAwriteRObj: Warning writing R Obj file : ", "DataSetName :", dataSetNameOrIndex," ", "R Obj Filename :", paste(RObjfileName, collapse = ","),sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
datasetname <- BSkyValidateDataset(dataSetNameOrIndex)
#cat("\nDS obj:", datasetname," ::\n")
			if(!is.null(datasetname))
			{		
				datasetname <- ExtractDatasetNameFromGlobal(datasetname)
				
				extnPattern = ".RData"
				
				DatasetNameSameAsFilename = TRUE #TRUE means save the dataset with the same name as the name of the file
				# first make a copy of 'Dataset1' Dataset2 with the same name as the filename (provided by the user)
				# then save this newly created dataset in a file instead of the 'Dataset1', 'Dataset2' etc.
				# not sure what to do with 'Dataset1', 'Dataset2'. Maybe make them null.
				if(DatasetNameSameAsFilename)
				{
					#get filename for the fullpath fileName. Get Dir name using: dirname("C:/some_dir/a.ext")
					onlyfilenamewithextension = basename(RObjfileName)
					
					if(length(grep(".RData", onlyfilenamewithextension, ignore.case = TRUE))==0)
						extnPattern = ".RDa"
						
					#get filename without extension
					onlyfilename = stringr::str_replace(onlyfilenamewithextension, regex(extnPattern, ignore_case = TRUE), "")
					
					#remove spaces or specialchars in filename, else eval/parse below will not work.
  				    onlyfilename =str_replace_all(onlyfilename, "[^[:alnum:]]", "")

					if(onlyfilename != datasetname)# if they re not exactly same name (case sensitive)
					{
						####01Feb2021 Following 2 eval-pasrse modified. 
						####Double arrow dropped and .GlobalEnv$ introduced.
						#creating a copy of the new dataset (Dataset1, Dataset2, etc.) with the same name as the filename in RObjFilename
						eval(parse(text=paste('.GlobalEnv$',onlyfilename, '<- .GlobalEnv$', datasetname, sep='')))
						
						#Make 'Dataset1', 'Dataset2' etc. NULL. For releasing the memory.
						eval(parse(text=paste('.GlobalEnv$',datasetname, '<- NULL', sep='')))
						
						#replace 'Dataset1', 'Dataset2' etc in dataset with whatever name is in the onlyfilename (which is the filename user provided)
						datasetname = onlyfilename 
					}
				}
				
				
				#eval(parse(text=paste('save(',datasetname,', file = RObjfileName)',sep='')))
				eval(parse(text=paste('save(',datasetname,', file = RObjfileName)',sep='')))
				#save(datasetname, file = RObjfileName)
				# save(uadatasets$lst[1],"F:/myuads")
				
				# # # eval( parse(text= paste("save(",name,",file=",filename,")") ) )
				#paste("save(",name,",file=",filename,")")
				#output "save( myobj ,file= myfile )"
				# no one else could
				#1.look for sig. If not found "not BlueSky" Continue loading? Show list to choose. dF1, df2.
				#2.''''''''''''', read DF, flag in sig (read anyway), Name of DF filename-d1....4,
			}
			else
			{
				# cat("\nError: Cannot write RData. Dataset name or index not found\n")
				BSkyErrMsg =paste("UAwriteRObj:  Cannot write RData. Dataset name or index not found."," Dataset Name:", dataSetNameOrIndex)
				warning("UAwriteRObj:  Cannot write RData. Dataset name or index not found.")
			}		
		BSkyFunctionWrapUp()			

}

#ReturnObjectName will contain the contents of RData file after loading
BSky.GetDataframeObjNames <- function (RObjFilename, ReturnObjectName) 
{
	BSkyFunctionInit()
	#BSkySetCurrentDatasetName(datasetname)
	datasetname =ReturnObjectName
	BSkyErrMsg = paste("BSky.GetDatasets: Error reading R Obj file : ", "DataSetName :", datasetname," ", "R Obj Filename  :", paste(RObjFilename, collapse = ","),sep="")
	BSkyWarnMsg = paste("BSky.GetDatasets: Warning reading R Obj file : ", "DataSetName :", datasetname," ", "R Obj Filename :", paste(RObjFilename, collapse = ","),sep="")
	
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)

			#cat("Before actual function call load():",datasetname)
			#print(Sys.time())			
					# library(RODBC) #no need
					bskysig=NULL ##support for old .RData format where data.fram was stored in UAObj
					UAObj <- NULL
					UAObj$info <- NULL
					UAObj$obj <- NULL
					bsky.class <- ""
					DFrobjsnames = c()
					
					eval(parse(text=paste( ReturnObjectName, '<<- load(file = "',RObjFilename,'", envir = .GlobalEnv)',sep='')))
					
					eval(parse(text=paste('objlen <- length(',ReturnObjectName,')', sep='' )))
					
					eval(parse(text=paste('isUAObj <- ',ReturnObjectName,'== "UAObj" ', sep='')))
					#isUAObj <- (ReturnObjectName == "UAObj")
					#<- load("D:/BSky/Misc/Datasets/rdata/empfull.rdata")
					if( objlen == 1 && isUAObj)##support old format (BSky format)
					{
						method1=FALSE
						
						if(method1) # This needs some fix
						{
							# filename without extension. BSkyLoadrefresh fails with extension.
							datasetname <-  tools::file_path_sans_ext(basename(RObjFilename)) 
							eval( parse(text=paste( datasetname,' <<- as.data.frame(UAObj$obj)' , sep=''   ))) ##This doesn't work so use the one below.
							#assign(datasetname, UAObj$obj, envir=.GlobalEnv)
							DFrobjsnames = datasetname
						}
						else
						{
							DFrobjsnames = "UAObj$obj"
						}
					}
					else
					{
						
						bsky.class <- eval(parse(text=paste('class(get(',ReturnObjectName,'))',sep='')))#it only returns the class of the first obj.
						
						if(length(bsky.class) >= 1)
						{
							#warning("BSky.GetDatasets: .RData file has multiple objects.");
							idx=1
							for (i in 1 : objlen)
							{
								#if(bsky.class[i] == 'tbl_df' || bsky.class[i] == 'data.frame')# || bsky.class[i] == 'matrix') 
								{
									classnamesEachObjHas = eval(parse(text=paste('class(get(',ReturnObjectName,'[',i,'],))',sep=''))) # An object can belong to multiple classess
									
									#check if it belogs to tbl_df class
									tbl_df_index = which(classnamesEachObjHas == 'tbl_df')
									
									#check if it belogs to data.frame class
									data.frame_index = which(classnamesEachObjHas == 'data.frame')
									
									#check if it belogs to data.frame class. For first ver Aaron dont want matrix. So not used in 'if' below
									matrix_index = which(classnamesEachObjHas == 'matrix')
									
									if( length(tbl_df_index) >=1  || length(data.frame_index) >= 1 ) # if object is tbl_df or data.frame class then add to list.
									{									
										DFrobjsnames[idx] = eval(parse(text=paste(ReturnObjectName,'[',i,']', sep='')))
										idx = idx + 1
									}
								}
							}
							# adding a extra space if there were multiple R Obj but just one data.frame
							# This will differentiate RData file with single data.frame object vs 
							# RData file with single data frame plus one or more other objects
							if(objlen>1 && length(DFrobjsnames) == 1)
							{
								#idx is already incremented
								DFrobjsnames[idx] = ""
							}
						}
						else
						{
							BSkyErrMsg =paste("BSky.GetDatasets: No objects found in RData file")
							warning("BSky.GetDatasets: No objects found in RData file ")
						}
					}
					
					if(length(DFrobjsnames) >= 1)
					{
						#nothing to do here. May be in future we can.
					}
					else
					{
							BSkyErrMsg =paste("BSky.GetDatasets: No data.frame or tbl_df class objects found in RData file")
							warning("BSky.GetDatasets: No data.frame or tbl_df class objects found in RData file ")					
					}
					
					
			#cat("Wrapup:",datasetname)
			#print(Sys.time())	
			BSkyFunctionWrapUp()
			invisible(DFrobjsnames)
}

# Not in use  and not tested
#check and convert single tbl_df object to data.frame
Convert.tbl_df.to.dataframe <- function(tbl_dframe)
{

	dfclass = class(tbl_dframe)
	tbl_df_index = which(dfclass == 'tbl_df')
	if( length(tbl_df_index) > 0)
	{
		tbl_dframe <<- as.data.frame(tbl_dframe)
	}
}