######## list of methods in this file; in sequence  ###########
# uagetindex.Anil
# uagetindex
# uainit
# .Last 
# uavectortostring
# uavectortostringnew
#################################################################################################

uagetindex <-function(ualistofvars=NA,uaform=NA,uaindex)
{
	if(!is.na(ualistofvars[1]))
	{
	uaarr=names(uadatasets$lst[[uaindex]])
	len =length(ualistofvars)
	uavarindex =rep(-1:-1,len)
	for (i in 1:len)
		{
		uavarindex[i] =which(uaarr==ualistofvars[i])
		}
	}
	if (!is.na(uaform))
	{	
		ualistofvars=all.vars(uaform, functions="FALSE")
		uaarr =names(uadatasets$lst[[uaindex]])
 		len =length(ualistofvars)
 		uavarindex =rep(-1:-1,len)
 		for (i in 1:len)
 			{
				uavarindex[i] =which(uaarr==ualistofvars[i])
			}
	}
 	uavarindex
}

#I have not removed the objects. This can be done on exit of R using the .Last, see help(savehistory)

uainit <- function()
{
	if(exists("temp", envir=uadatasets)) uadatasets$temp=NULL
	# rm(temp,envir=uadatasets)
	if(exists("temppairs",envir=uadatasets)) uadatasets$temppairs=NULL
	#rm(temppairs,envir =uadatasets)
	if(exists("ostarttime", envir=uadatasets))uadatasets$ostarttime =NULL
	if(exists("starttime", envir=uadatasets))uadatasets$starttime=NULL
	if(exists("funtname", envir=uadatasets))uadatasets$funtname=NULL
	if(exists("initialmem", envir=uadatasets))uadatasets$initialmem=NULL
	#if(exists("totaltime", envir=uadatasets))uadatasets$totaltime=NULL
	if(exists("totalcputime", envir=uadatasets))uadatasets$totalcputime=NULL
	if(exists("elapsedtime", envir=uadatasets))uadatasets$elapsedtime=NULL
	if(exists("warning", envir=uadatasets))uadatasets$warning=0
	if(exists("error", envir=uadatasets))uadatasets$error=0	
	if(exists("warnindex", envir=uadatasets))uadatasets$warnindex=0	
	if(exists("warnindex", envir=uadatasets))uadatasets$uawarnfn=0	
	if(exists("warnmsg", envir=uadatasets))uadatasets$warnmsg=NULL
	if(exists("errmsg", envir=uadatasets))uadatasets$errmsg=NULL
	if(exists("logindex", envir=uadatasets))uadatasets$logindex=1
	if(exists("df", envir=ualog))ualog$df=NULL
	if(exists("errorindex", envir=uadatasets))uadatasets$errorindex=0
	if(exists("split", envir=uadatasets))uadatasets$split=NULL
	#Globals for simplifying the try catch loop
	if(exists("functname"))uadatasets$functname=NULL
	if(exists("uberfunct"))uadatasets$uberfunct=NULL
	if(exists("uaerrmsgdis"))uadatasets$uaerrmsgdis=NULL
	if(exists("uawarnmsgdis"))uadatasets$uawarnmsgdis=NULL
	if(exists("uavar"))uadatasets$uavar=NULL
		
}


#This is the function that will remove all objects, not sure if we need it though as we are exiting R
.Last =function()
{
	if(exists("temp", envir=uadatasets)) rm(temp,envir=uadatasets)
	if(exists("temppairs",envir=uadatasets)) rm(temppairs,envir =uadatasets)
	if(exists("ostarttime", envir=uadatasets))rm(ostarttime,envir =uadatasets)
	if(exists("starttime", envir=uadatasets))rm(starttime,envir =uadatasets)
	if(exists("funtname", envir=uadatasets))rm(funtname,envir =uadatasets)
	if(exists("initialmem", envir=uadatasets))rm(initialmem,envir =uadatasets)
	#if(exists("totaltime", envir=uadatasets)) rm(totaltime,envir=uadatasets)
	if(exists("elapsedtime", envir=uadatasets)) rm(elapsedtime,envir=uadatasets)
	if(exists("totalcputime", envir=uadatasets))rm(uadatasets$totalcputime,envir=uadatasets)
	if(exists("warning", envir=uadatasets)) rm(warning,envir=uadatasets)
	if(exists("error", envir=uadatasets))rm(uadatasets$error,envir=uadatasets)
	if(exists("warnindex", envir=uadatasets))rm(uadatasets$warnindex,envir=uadatasets)
	if(exists("warnmsg", envir=uadatasets)) rm(uadatasets$warnmsg,envir=uadatasets)
	if(exists("errmsg", envir=uadatasets))rm(uadatasets$errmsg,envir=uadatasets)
	if(exists("split", envir=uadatasets))rm(uadatasets$split,envir=uadatasets)
	#Globals for simplifying the try catch loop
	if(exists("functname"))rm (uadatasets$functname,envir=uadatasets)
	if(exists("uberfunct")) rm(uadatasets$uberfunct, envir=uadatasets)
	if(exists("uaerrmsgdis"))rm(uadatasets$uaerrmsgdis,envir=uadatasets)
	if(exists("uawarnmsgdis"))rm(uadatasets$uawarnmsgdis,envir=uadatasets)
	if(exists("uavar"))rm(uadatasets$uavar,,envir=uadatasets)
}


# This code takes a character vector and returns a string
# For example the vector is test=c("tg0","tg1","tg2"), this function returns the following string "tg0\","tg1\","tg2\"
uavectortostring <-function(uapairs)
{
	i=1
	len=length(uapairs)
	uatemp=uapairs[1]
	
	for(i in 1:len) uatemp =c(uatemp,",",uapairs[i]) 
	uatemp=paste(uatemp,collapse="\"")
	return(uatemp)
}	

uavectortostringnew <-function(uapairs)
{
	i=1
	len=length(uapairs)
	uatemp=NULL
	for(i in 1:len) 
	{
			if (i==1)	uatemp =paste(uatemp, uapairs[i]) 
			else uatemp =paste(uatemp, "|",uapairs[i])
	}
	return(uatemp)
}	

#Parameters to uaprocdesc are
#index -The index number of the dataset
#missing -This captures whether missing values are handled analysis by analysis or listwise. This is an optional parameter


#Here is what uaprocdesc returns
# 1st Full path of the dataset being analyzed
# 2nd The dataset name
# 3rd Information on whether there is a filter applied
# 4th Information on whether there are weights
# 5th splits
# 6th total number of cases
# 7th The function echoed
# 8th Total CPU time
# Total elapsed time
# uaprocdesc <-function(index,missing =0)
# {
	# i=1
	# uasplitvars=NULL
	# uasplitindex=NULL
	# uafilter=NA
	# uaweights=NA
	
	# # Total number of rows in dataset
	# uatotalrows =nrow(uadatasets$lst[[index]])
	# # Ckecking whether there any any splits involved with the dataset
	# if (!is.null(uadatasets$split))
	# {
		# uasplitindex=uadatasets$split
		# len =length(as.numeric(uasplitindex))
		# for (i in 1:len)
		# {
			# uasplitvars =c(uasplitvars, names(uadatasets$lst[[index]][uasplitindex[i]]))
		# }
	# }
	# else
	# {
		# uasplitvars=NULL
	# }
	# return(list(uadatasets$fullpath[index],uadatasets$name[index],uafilter,uaweights,uasplitvars,uatotalrows,uadatasets$rproc,uadatasets$totalcputime,uadatasets$elapsedtime))
# }	
