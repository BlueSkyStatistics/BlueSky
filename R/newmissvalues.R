######## list of methods in this file; in sequence (few may not be ready and unused ) ###########
# uaaretheremissval
# uabihandlemissvalsnew1
# uauserdefinedmissvals
# uahandlemissvalsnew
# uagetsubsetcriterianew
# uahandlemissvalspasm
# uagetsubsetcriteriapasm
# uahandlemissvalskrel
# uagetsubsetcriteriakrel
# uapasmhandlemissvals
# uahandlemissvalskind
#################################################################################################

# This function checks the dataset for userdefined or system defined missing values
# If there are any user missing values or NA in the variables being analyzed, we return 1
# This function does not remove missing values
# If there are missing values, return 1
# If there are no missing values return 0
# The user defined missing values are stored in the attributes of the variable
#THIS FUNCTION DOES NOT HANDLE A BLANK CHARACTER IN A FACTOR, WHEN WE CREATE THE DATASET "Employee data with miss vals.sav"
# BY IMPORTING A DATASET FROM R, GENDER IN THE SPSS DATASET IS A BLANK STRING THAT I CREATED IN SPSS AS A MISSING VALUE
# ON IMPORT THIS COMES IN AS A BLANK STRING AND THIS IS NOT TREATED AS A MISSING VALUE. RECODING GENDER WORKS.


uaaretheremissval <-function(uaindex, uavarindex)
{
	i=1
	noofvars =length(uavarindex)
	#This is for handling system missing values
	usermissvals=c(NA)
	# This allows us to append the user missing values to the system missing values
	# and then checks the entire column for any system missing and user missing values
	for(i in 1:noofvars) 
	{
		if (!is.null(attributes(uadatasets$lst[[uaindex]][,uavarindex[i]])$values))
		{
			usermissvals =c(usermissvals, attributes(uadatasets$lst[[uaindex]][,uavarindex[i]])$values)
		}
		if(TRUE %in% (uadatasets$lst[[uaindex]][,uavarindex[i]] %in% usermissvals))return(1)
		usermissvals=c(NA)
	}
	#There are no missing values
	return(0)
}



#This function is used to handle missing values for Binomial and one sample t.test
#Note:Since binomial and one sample t.test operate on one variable at a time, we don't have to be worried about
# system missing values as the mean, variance procedures automatically handle this
# The t.test and binom.test also automatically handle  this
# The algorithm checks if there are missing values (Both system or user defined)
# If there are missing values, we check if the option for missing values is set to list wise, if so, we remove rows where a single column contains
# NA or user defined missing values
# we them check if there are user defined missing values, if true, we process them
# IN ALL OTHER CASES WE ARE PASSING THE ORIGINAL DATASET W/O DOING ANY PROCESSING


uabihandlemissvalsnew1 <-function(index,uavarindex,noofvars,missing)
{
uamissvalsinfo =NULL
uadatasets$temppairs=NULL
usermissvalsinfo=NULL
cindex=NULL	
uauserdefmissvals =uauserdefinedmissvals (index,uavarindex,noofvars)
uamissvalindex =uauserdefmissvals[[2]]
if (uaaretheremissval(index,uavarindex))
	{
	#Handling missing values list wise
	#If there is a user defined or system defined missing value, all cases that contain a missing values
	#have to be deleted
	#temppairs have to be modified
	if (missing ==1)
		{
			cindex[1:noofvars] =index
			uatemp=uahandlemissvalsnew(index,index,uavarindex,uauserdefmissvals[[2]],missing)
			usermissvalsinfo =c(usermissvalsinfo,uatemp)
			uaplaceholder =c(expression(uadatasets$temp[[cindex[i]]][,i])) 
			uadatasets$temppairs =rep(uaplaceholder,noofvars)
			return(cindex)
			#uadatasets$temp[[index]] =na.omit(uadatasets$lst[[index]][,uavarindex])
			#uaplaceholder =c(expression(uadatasets$temp[[cindex[i]]][,i]))
			#uadatasets$temppairs =rep(uaplaceholder,noofvars)
		}
	# if there are user defined missing values, we have to search and replace all user defined missing values by NA
	#We have to iterate through all the variables to check whether there are user defined missing values
	# Temppairs needs to be modified	
	else if (uauserdefmissvals[[1]]==TRUE)
		{
			#This is the old code that did not handle user defined missing values
			#cindex[1:noofvars]=index
			#uaplaceholder =c(expression(uadatasets$lst[[cindex[i]]][,uavarindex[i]]))
			#uadatasets$temppairs =rep(uaplaceholder,noofvars)
		
			#iterate through all the variables
			l=1
			
			while( l<= noofvars)
			{
				if (uavarindex[l] %in% uamissvalindex)
					{
						cindex[l]=l
						#uadatasets$temp[[cindex[l]]] =na.omit(uadatasets$lst[[index]][,c(uavarindex[l], uavarindex[l+1])]) 
						uatemp =uahandlemissvalsnew(index,cindex[l],c(uavarindex[l]),uauserdefmissvals[[2]],missing)	
						usermissvalsinfo =c(usermissvalsinfo,uatemp)					
						uadatasets$temppairs =c(uadatasets$temppairs,expression(uadatasets$temp[[cindex[i]]][,1]))
						l=l+1
					}
				else
					{
						cindex[l]=l
						usermissvalsinfo =c(usermissvalsinfo,NA)
						
						##fix for tibble
uadatasets$temppairs =c(uadatasets$temppairs,expression( as.data.frame( uadatasets$lst[[index]][,uavarindex[i]])[[1]]))										

						l=l+1
					}	
			} #While
		return(cindex)
		#}#There are no user defined missing values
	
		}#Else if
		
	}#Are there missing values

#for missing values analysis by analysis we will just pass the original dataset in uadatasets 
#We will handle the missing values in the mean
cindex[1:noofvars]=index

##fix for tibble
 uaplaceholder =c(expression(as.data.frame( uadatasets$lst[[cindex[i]]][,uavarindex[i]])[[1]]))


uadatasets$temppairs =rep(uaplaceholder,noofvars)
return(cindex)
}


uauserdefinedmissvals <-function(index,uavarindex,noofvars)
{
	i=1
	uaarethereuserdefmisval =FALSE
	uauserdefmissvalindex =NULL
	for (i in 1:noofvars)
	{
		if (!is.null(attributes(uadatasets$lst[[index]][,uavarindex[i]])$values))
 		{
		uauserdefmissvalindex=c(uadatasets$usedefmissvalsindex,i)
		uaarethereuserdefmisval =TRUE
		}
	}
	return(list(uaarethereuserdefmisval,uauserdefmissvalindex))
}


	
uahandlemissvalsnew <-function(index,indextemp, uavarindex,uauserdefmissvalsindex,missing)
{
	i=0
	uasubsetcriteria =	uagetsubsetcriterianew(index,uavarindex, uauserdefmissvalsindex,missing)
	uadatasets$temp[[indextemp]] =subset(uadatasets$lst[[index]],subset=eval(parse(text =uasubsetcriteria)), select=uavarindex)
	return(1)
}	


uagetsubsetcriterianew <-function(index,uavarindex,uauserdefmissvalsindex,missing)
{
	i=1
	noofvars =length(uavarindex)
	uasubsetcriteria=NULL
	uanouserdefmisvals=length(uauserdefmissvalsindex)
	if(missing ==1)
	{
		uasubsetcriteria ="!uadatasets$lst[[index]][,uavarindex[(i=i+1)]] %in% c(NA,attributes(uadatasets$lst[[index]][,uavarindex[i]])$values)"
		while (i < noofvars)
		{
			uasubsetcriteria =paste(uasubsetcriteria,"&","!uadatasets$lst[[index]][,uavarindex[(i=i+1)]] %in% c(NA,attributes(uadatasets$lst[[index]][,uavarindex[i]])$values)")
			i=i+1
		}
	}
	# We will come to this position in the code ONLY when there are user defined missing values
	if(missing ==0)
	{
		uasubsetcriteria==NULL
		while (i < uanouserdefmisvals)
		{
		if (uasubsetcriteria==NULL) uasubsetcriteria ="!uadatasets$lst[[index]][,uauserdefmissvalsindex[(i=i+1)]] %in% attributes(uadatasets$lst[[index]][,uausermissvalsindex[i])$values"				
		else uasubsetcriteria =paste(uasubsetcriteria,"&","!uadatasets$lst[[index]][,uauserdefmissvalsindex[(i=i+1)]] %in% attributes(uadatasets$lst[[index]][,uausermissvalsindex[i]])$values")
		i=i+1
		}
	}
	uasubsetcriteria
}


uahandlemissvalspasm <-function(index,indextemp, uavarindex)
{
	i=0
	noofvars =length(uavarindex)
	uasubsetcriteria =	uagetsubsetcriteriapasm(index,uavarindex,noofvars)
	uadatasets$temp[[indextemp]] =subset(uadatasets$lst[[index]],subset=eval(parse(text =uasubsetcriteria)), select=uavarindex)
	return(1)
}


uagetsubsetcriteriapasm <-function(index,uavarindex,noofvars)
{
	i=1
	
	uasubsetcriteria=NULL
	{
		uasubsetcriteria ="!uadatasets$lst[[index]][,uavarindex[(i=i+1)]] %in% c(NA,attributes(uadatasets$lst[[index]][,uavarindex[i]])$values)"
		while (i < noofvars)
		{
			uasubsetcriteria =paste(uasubsetcriteria,"&","!uadatasets$lst[[index]][,uavarindex[(i=i+1)]] %in% c(NA,attributes(uadatasets$lst[[index]][,uavarindex[i]])$values)")
			i=i+1
		}
	}
	uasubsetcriteria
}


uahandlemissvalskrel <-function(index,indextemp, uavarindex)
{
	i=0
	uasubsetcriteria =	uagetsubsetcriteriakrel(index,uavarindex)
	uadatasets$temp[[indextemp]] =subset(uadatasets$lst[[index]],subset=eval(parse(text =uasubsetcriteria)), select=uavarindex)
	return(1)
}		


uagetsubsetcriteriakrel <-function(index,uavarindex)
{
	i=1
	noofvars =length(uavarindex)
	uasubsetcriteria=NULL
	uasubsetcriteria==NULL
	while (i < noofvars)
		{
			if (is.null(uasubsetcriteria)) uasubsetcriteria ="!uadatasets$lst[[index]][,uavarindex[(i=i+1)]] %in% c(NA,attributes(uadatasets$lst[[index]][,uavarindex[i]])$values)"
			uasubsetcriteria =paste(uasubsetcriteria,"&","!uadatasets$lst[[index]][,uavarindex[(i=i+1)]] %in% c(NA,attributes(uadatasets$lst[[index]][,uavarindex[i]])$values)")
			i=i+1
		}
	uasubsetcriteria
}



uapasmhandlemissvals <-function(index,uavarindex,noofvars,missing=0)
{
uamissvalsinfo =NULL
uadatasets$temppairs=NULL
cindex=NULL
usermissvalsinfo=NULL
j=1
# if a case contains a single variable with a NA, the case is ignored across all variables. 
# This corresponds to exclude cases listwise in SPSS
#uaaretheremissval tell me if there are missing values
if (uaaretheremissval(index,uavarindex))
	{
		# There are missing values
		if (missing ==1)
		{
			cindex[1:noofvars] =index
			uatemp=uahandlemissvalspasm(index,index,uavarindex)
			#uatemp=uahandlemissvals(index,index,uavarindex)
			#usermissvalsinfo =c(usermissvalsinfo,uatemp)
			uaplaceholder =c(expression(uadatasets$temp[[cindex[i]]][,i]),expression(uadatasets$temp[[cindex[j]]][,j]))
			uadatasets$temppairs =rep(uaplaceholder,noofvars/2)
		}
# This corresponds to Exclude cases analysis by analysis in SPSS. If a case contains a single variable with a NA, 
# the case is ignored for all pairs of variables
		if (missing ==0)
		{
			#iterate through all the variables
			l=1
			#reflects the fact that we create a dataset per 2 variables
			k=1
			while( l< noofvars)
			{
				if (uaaretheremissval(index,uavarindex[l])|uaaretheremissval(index,uavarindex[l+1]))
					{
						cindex[l:(l+1)]=k
						#uadatasets$temp[[cindex[l]]] =na.omit(uadatasets$lst[[index]][,c(uavarindex[l], uavarindex[l+1])]) 
						#uatemp =uahandlemissvals(cindex[l],index,c(uavarindex[l], uavarindex[l+1]))
						uatemp =uahandlemissvalspasm(index,cindex[l],c(uavarindex[l], uavarindex[l+1]))						
						#usermissvalsinfo =c(usermissvalsinfo,temp)
						uadatasets$temppairs =c(uadatasets$temppairs,expression(uadatasets$temp[[cindex[i]]][,1]),expression(uadatasets$temp[[cindex[j]]][,2]))
						l=l+2
						k=k+1
					}
				else
					{
						cindex[l:(l+1)]=k
						usermissvalsinfo =c(usermissvalsinfo,NA)
						uadatasets$temppairs =c(uadatasets$temppairs,expression(uadatasets$lst[[index]][,uavarindex[i]]),expression(uadatasets$lst[[index]][,uavarindex[j]]))										
						l=l+2
						k=k+1
					}	
			}
		 }
	return(cindex)
	}
#if there are no missing values, I can run my test on uadatasets itself
#Aaron you need to correct this for more than 2 pairs
	else
	{
	cindex[1:noofvars]=index
	# uaplaceholder =c(expression(uadatasets$lst[[index]][,uavarindex[i]]),expression(uadatasets$lst[[index]][,uavarindex[j]]))
	##fix for tibble. Converting above line to following
		uaplaceholder =c(expression(as.data.frame(uadatasets$lst[[index]][,uavarindex[i]])[[1]]),expression( as.data.frame(uadatasets$lst[[index]][,uavarindex[j]])[[1]]))
	
	uadatasets$temppairs =rep(uaplaceholder,noofvars/2)
	return(cindex)
	}
}


#########################################################################################################################

uahandlemissvalskind <-function(index,uavarindex,groupindex,noofvars,missing)
{
uamissvalsinfo =NULL
uadatasets$temppairs=NULL
usermissvalsinfo=NULL
cindex=NULL	
uauserdefmissvals =uauserdefinedmissvals (index,c(uavarindex,groupindex),noofvars)
uamissvalindex =uauserdefmissvals[[2]]
if (uaaretheremissval(index,c(uavarindex,groupindex)))
	{
	#Handling missing values list wise
	#If there is a user defined or system defined missing value, all cases that contain a missing values
	#have to be deleted
	#temppairs have to be modified
	if (missing ==1)
		{
			cindex[1:noofvars] =index
			uatemp=uahandlemissvalsnew(index,index,c(uavarindex,groupindex),uauserdefmissvals[[2]],missing)
			usermissvalsinfo =c(usermissvalsinfo,uatemp)
			#uaplaceholder =c(expression(uadatasets$temp[[cindex[i]]][,i]),expression(uadatasets$temp[[cindex[i]]][,(noofvars+1)]))
########fix for tibble. Converting above line to following########
uaplaceholder =c(expression(as.data.frame(uadatasets$temp[[cindex[i]]][,i])[[1]]),expression(as.data.frame(uadatasets$temp[[cindex[i]]][,(noofvars+1)])[[1]]))	
			
			uadatasets$temppairs =rep(uaplaceholder,noofvars)
			#uadatasets$tempdataset=c(uadatasets$tempdataset,uadatasets$temp[[cindex[i]]][c(i,(noofvars+1))])				
			#This is added to get the group index required for K independent samples
			return(cindex)
			#uadatasets$temp[[index]] =na.omit(uadatasets$lst[[index]][,uavarindex])
			#uaplaceholder =c(expression(uadatasets$temp[[cindex[i]]][,i]))
			#uadatasets$temppairs =rep(uaplaceholder,noofvars)
		}
	# if there are user defined missing values, we have to search and replace all user defined missing values by NA
	#We have to iterate through all the variables to check whether there are user defined missing values
	# Temppairs needs to be modified	
	# 03/26/2011 else if (uauserdefmissvals[[1]]==TRUE)
	else
		{
			#This is the old code that did not handle user defined missing values
			#cindex[1:noofvars]=index
			#uaplaceholder =c(expression(uadatasets$lst[[cindex[i]]][,uavarindex[i]]))
			#uadatasets$temppairs =rep(uaplaceholder,noofvars)
		
			#iterate through all the variables
			k=1
			l=1
			
			while( l<= noofvars)
			{
				
				if (uavarindex[l] %in% uamissvalindex)
					{
						cindex[l]=l
						#uadatasets$temp[[cindex[l]]] =na.omit(uadatasets$lst[[index]][,c(uavarindex[l], uavarindex[l+1])]) 
						uatemp =uahandlemissvalsnew(index,cindex[l],c(uavarindex[l],groupindex),uauserdefmissvals[[2]],missing)	
						usermissvalsinfo =c(usermissvalsinfo,uatemp)					
						#uaplaceholder =c(expression(uadatasets$temp[[cindex[i]]][,i]),expression(uadatasets$temp[[cindex[i]]][,(i+1)]))
########fix for tibble. Converting above line to following########
uaplaceholder =c(expression(as.data.frame(uadatasets$temp[[cindex[i]]][,i])[[1]]),expression(as.data.frame(uadatasets$temp[[cindex[i]]][,(i+1)])[[1]]))
												
						uadatasets$temppairs =c(uadatasets$temppairs,uaplaceholder)
						#uadatasets$tempdataset=c(uadatasets$tempdataset,uadatasets$temp[[cindex[i]]])
						l=l+1
					}
				else
					{
						cindex[l]=l
						usermissvalsinfo =c(usermissvalsinfo,NA)
						#uaplaceholder =c(expression(uadatasets$lst[[index]][,uavarindex[i]]),expression(uadatasets$lst[[index]][,groupindex]))
########fix for tibble. Converting above line to following########
uaplaceholder =c(expression(as.data.frame(uadatasets$lst[[index]][,uavarindex[i]])[[1]]),expression(as.data.frame(uadatasets$lst[[index]][,groupindex])[[1]]))

						uadatasets$temppairs =c(uadatasets$temppairs,uaplaceholder)
						#uadatasets$tempdataset=c(uadatasets$tempdataset,uadatasets$lst[[index]][c(uavarindex[i],groupindex)])										
						l=l+1						
					}	
			} #While
		return(cindex)
		#}#There are no user defined missing values
	
		}#Else if
		
	}#Are there missing values

#for missing values analysis by analysis we will just pass the original dataset in uadatasets 
#We will handle the missing values in the mean
cindex[1:noofvars]=index

#uaplaceholder =c(expression(uadatasets$lst[[index]][,uavarindex[i]]),expression(uadatasets$lst[[index]][,groupindex]))
########fix for tibble. Converting above line to following########
uaplaceholder =c(expression(as.data.frame(uadatasets$lst[[index]][,uavarindex[i]])[[1]]),expression(as.data.frame(uadatasets$lst[[index]][,groupindex])[[1]]))

uadatasets$temppairs =rep(uaplaceholder,noofvars)
return(cindex)
}


