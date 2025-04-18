BSkyExportOutputPro <- function(pandocPath, exportDirPath, exportFilename, exportFormat) #
{
	BSkyFunctionInit()
	
	BSkyErrMsg = paste("BSkyExportOutputPro: Error exporting output : ",sep="")
	BSkyWarnMsg = paste("BSkyExportOutputPro: Warning exporting output : ",sep="")
	BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
	success = FALSE

	tryCatch({
	
			withCallingHandlers({
			
				#Check pandoc path. This is in JS code
				
				##normalizePath(file.path("folder", "subfolder", "file.txt"), mustWork = FALSE)
				RMDfilename = normalizePath(file.path(exportDirPath, paste(exportFilename, ".Rmd", sep='')), mustWork = FALSE)
				HTMLfilename = normalizePath(file.path(exportDirPath, paste(exportFilename, ".html", sep='')), mustWork = FALSE)
				DOCXfilename = paste(exportFilename, ".docx", sep='')			
			
				library(rmarkdown);
				library(pagedown);
				#Need to add the pandoc installation directory path
				Sys.setenv(PATH = paste(Sys.getenv("PATH"), '"',pandocPath,'"', sep = .Platform$path.sep));

				#Save the BSky App's current Kable output table styling settings
				bsky_orig_BSkyGetKableAndRmarkdownFormatting = BSkyGetKableAndRmarkdownFormatting();
				bsky_orig_BSkyKabletableStylingOptions = uadatasets.sk$BSkyKabletableStylingOptions;

				# DOCX, PDF requires HTML so no need of 'if' because it is always needed
				#if(exportFormat == 'HTML' || exportFormat == 'DOCX'){ 
					output_html = rmarkdown::render(RMDfilename, output_format = c("html_document"));
				#	success = TRUE
				#}

				# Use do.call to unpack the list of four values and pass arguments in the correct order automatically
				do.call(BSkySetKableAndRmarkdownFormatting, unname(bsky_orig_BSkyGetKableAndRmarkdownFormatting));
				uadatasets.sk$BSkyKabletableStylingOptions = bsky_orig_BSkyKabletableStylingOptions;

				if(exportFormat == 'DOCX'){
					rmarkdown::pandoc_convert(HTMLfilename, to = "docx", output = DOCXfilename);
					success = TRUE
				}
				if(exportFormat == 'PDF'){
					Sys.setenv(R_HOME = normalizePath(Sys.getenv("R_HOME")) )
					pagedown::chrome_print(input = HTMLfilename) # , wait = 10, timeout = 300);
					success = TRUE
				}
				if(exportFormat == 'PPTX'){
					#output_html = rmarkdown::render(RMDfilename, output_format = c("powerpoint_presentation"));
					success = TRUE
				}
			
			}, warning = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
		}, error = BSkyOpenDatafileCommandErrWarnHandler, silent = TRUE)
		

	BSkyFunctionWrapUp()	
	return(success)
}