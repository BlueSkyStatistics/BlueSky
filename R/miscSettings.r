
# Setting for turning ON/OFF the message that we get when a library is 
# loaded from UI and some messages are shown in the output area.
BSkySetLibLoadMsgPrintSetting <- function(libLoadMsgPrintSetting = TRUE)
{
	uadatasets.sk$BSkyLibLoadMsgPrintSetting = libLoadMsgPrintSetting
	return(invisible(uadatasets.sk$BSkyLibLoadMsgPrintSetting))
}

BSkyGetLibLoadMsgPrintSetting <- function()
{
	if(exists("BSkyLibLoadMsgPrintSetting", env=uadatasets.sk))
	{
		return(invisible(uadatasets.sk$BSkyLibLoadMsgPrintSetting))
	}
	else
	{
		return(invisible((uadatasets.sk$BSkyLibLoadMsgPrintSetting = TRUE)))
	}
}