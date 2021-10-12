######## list of methods in this file; in sequence (few may not be ready and unused ) ###########
# uaopendataset
#################################################################################################

#We should support 2 datasets with the same name
#How will we handle 2 datasets with the same name?
#we could add the directory path and file name




#read.spss is the R call made by the uaopendataset function
#This call is weak because
# a. The use.missing =TRUE converts all user defined missing values to NA or if FALSE brings these values in but treats these
# place holders for user defined missing values as true values in statistical calculations
# b. When use.value.labels =FALSE the names of the value labels are not brought in as attributes of that variable
# We use the following defaults use.value.labels=FALSE,use.missings=FALSE

#uadatasets$fullpath holds the path of the dataset

#uadatasets$error
#uadatasers$warning
#uadatasers$warnindex NOT SURE WE NEED THIS AS IT RETURNS THE TOTAL NUMBER OF WARNINGS
#uadatasets$warnmsg NOT SURE WHETHER WE NEED THIS
#uadatasets$warncallfn  NOTE SURE WE NEED THIS
#uadatasets$errcallfn  NOTE SURE WE NEED THIS





uaopendataset <- function(typeoffile, directory,filename,uaperformance)
{  
	i=1;
	uadatasets$errmsg =NULL
	uadatasets$warning=NA
	uadatasets$warncallfn=NULL
	uadatasets$warnindex=0
	uadatasets$warnmsg=NULL
	datasetpath =paste(directory,sep= "/",filename)
		
	if(uaperformance==1)
	{
	ostarttime =date()
	starttime=proc.time()
	initialmem=gc()
	}
	if(typeoffile==1)
	{
	tryCatch(
	{
		withCallingHandlers(
				{
				uadatasets$lst[[uadatasets$index]] <- read.spss(datasetpath,use.value.labels=FALSE,max.value.labels=Inf, to.data.frame=TRUE,use.missings=FALSE);
				},
			        warning = function(ex) 
				{
				if(is.null(ex)) {ex <- warnings();}
				uadatasets$warning =-1;
				uadatasets$warnmsg =c(uadatasets$warnmsg, conditionMessage(ex));
				uadatasets$warnmsgdis ="Warning(s) in opening the SPSS data file";
				uadatasets$warncallfn =c(uadatasets$warncallfn,conditionCall(ex));
				uawritelog(type="Warning", functionName="read.spss", uberFunct="uaopendataset", RMessage =conditionMessage(ex),BSkyMessage =uadatasets$warnmsgdis);
				uadatasets$warnindex=uadatasets$warnindex+1;
				invokeRestart("muffleWarning");
				}
			         ) # end of withCallingHandlers for catching warnings and continuing execution
	},

	error = function(ex) 
		        {
		        uadatasets$error =-1;
				uadatasets$errmsgdis ="Error in opening the SPSS data file";			
		        uawritelog(type="Error", functionName="read.spss",uberFunct="uaopendataset",RMessage =conditionMessage(ex),BSkyMessage=uadatasets$errmsgdis);
				uadatasets$errmsg =conditionMessage(ex);
                uadatasets$errcallfn=conditionCall(ex);
				uadatasets$lst[[uadatasets$index]]=ex;
		        },
	silent =TRUE)
	if (inherits(uadatasets$lst[[uadatasets$index]],"error")) return(list(uadatasets$lst[[uadatasets$index]],uadatasets$errmsg,uadatasets$warning))
	else
	{
		if (uaperformance==1)
		{
		oendtime=date()
		endtime=proc.time()
		time=endtime-starttime
		uawritelog(type ="CPU time", functionName ="read.spss",uberFunct="uaopendataset",startTime =ostarttime,endTime=oendtime,userTime=time[1],systemTime=time[2],elapsed=time[3],totalCpuTime=time[1]+time[2])
		finalmem=gc()
		uawritelog(type="Memory", functionName="read.spss",uberFunct="uaopendataset",startTime=ostarttime,endTime=oendtime, initNCells=initialmem[1,2],
		initVCells=initialmem[2,2],initTotal=initialmem[1,2]+initialmem[2,2], finNCells=finalmem[1,2], finVCells=finalmem[2,2], finTotal=finalmem[1,2]+finalmem[2,2],maxNCells=finalmem[1,6],maxVCells=finalmem[2,6],maxTotal=finalmem[1,6]+finalmem[2,6])
		}
	uadatasets$name[uadatasets$index]=filename
	uadatasets$fullpath[uadatasets$index]=datasetpath
	uadatasets$index =uadatasets$index +1
	return(list(uadatasets$lst[[uadatasets$index-1]],uadatasets$errmsg,uadatasets$warning))
	}
	}
	#the above closes type of file =1
}
