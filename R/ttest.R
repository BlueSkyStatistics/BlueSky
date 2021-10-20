######## list of methods in this file; in sequence (few may not be ready and unused ) ###########
# uaindsmt.test
# uaindsm
# uacombinemat
# uaindsmdesc
# ualevene.test
# uaindsmttest
# uaonesmt.test
# newBskycheckempty
# Bskycheckempty
# uaonesample
# uaonesamttest
# uaonesmhandlemissvals
# uapairedsmt.test
# uapairedsample
# uapairedsmdesc
# uaonesmdesc
# uagetcorr
# uapasmttest
# uarettest
# levenemod.test
#################################################################################################
#Things to do
#Error handling
#The standard error is not returned with the t.test. I need to modify the t.test so that the standard error is returned
#You cannot split and group by the same variable in an independent sample t.test
#Grouping by a continuous variable where you specify the 2 values that you should group by needs to be implemented.
#The dialog should allow you to specify which 2 values you want to group by and cutpoints

#group -The variable to group by. GROUP NEEDS TO BE A FACTOR WITH ONLY 2 LEVELS
# EVEN IF THERE ARE NO SYSTEM MISSING VALUES IN THE DATA AND THERE ARE 2 UNIQUE VALUES, THIS CODE WILL NOT WORK, THIS IS 
# BECAUSE THERE ARE 3 LEVELS AND THE 3 LEVEL IS THE SYSTEM MISSING VALUE, , THIS IS HOW LEVELS WORK IN R
#THERE NEED TO BE 2 LEVELS FOR THIS CODE TO WORK

#****************************************************************
#Things to do
# Post question to the R community about x$yyy amd x$y

# Verify how logging is handled with splits
# uastartlog accepts the uber function name and the function name, ualogcommand does not accept any parameters
# Now I can call a uastartlog with uastartlog("t.test","uaindsmt.test") and forget to do the log command
#While it is fine to call uastartlog consecutively, you may erroneously call uastartlog consecutively because you forgot to log a command
#This would result in erroneous entries
#To minimize the risk, we should pass the function name parameter to logcommand
#If the function name parameter is not the same as the most recent logcommand, we should flag an error
#When there are errors in the logs with splits, its impossible to tell which split the error belongs to, 
#we should add this in one of the fields
#The datasetname needs to be added to the logs



# Handle error handling for date fields

#think about no of decimals
#We should code alternate i.e. single sided or two sided



#################################################################
#INDEPENDENT SAMPLES T.TEST
#################################################################
#The arguments
#vars -The variables that need to be passed
#group -The variable to group by. GROUP NEEDS TO BE A FACTOR WITH ONLY 2 LEVELS
# EVEN IF THERE ARE NO SYSTEM MISSING VALUES IN THE DATA AND THERE ARE 2 UNIQUE VALUES, THIS CODE WILL NOT WORK, THIS IS 
# BECAUSE THERE ARE 3 LEVELS AND THE 3RD LEVEL IS THE SYSTEM MISSING VALUE, THIS IS HOW LEVELS WORK IN R
#THERE NEED TO BE 2 LEVELS FOR THIS CODE TO WORK
#conf.level -This is the confidence interval
#Datasetname -The name of the dataset
#missing -0 indicates that missing values will be handled analysis by analysis
#missing -1 indicates missing values will be handled listwise


#Return values
#The 1st return value tells us whether there is a split or not (1=Split, 0=No split)
#The 2nd return value tells us whether there is an error or not (-1 if there is an error, 0 if there is no error)
# The 3rd return value gives us the number of errors
#The 4th return value tells us how many warnings there are
#The 5th return value gives us all the log entries
#The 6th return value gives us the summary of the function
#The 7th return value if there is no split returns the descriptive statistics of all the variables in the independent sample t.test, 
# called (uadesc)
#The 8th return value, if there is no split, returns the results of the independent sample t.test for all variables,called (uamat)
#The 9th variable, if there is no splits tells us what to display i.e. what variables should we display the results for
#and the variables for which we should display nothing as there are errors and warnings
#Note: we will display the errors and warnings at the top of the table
#If there is a split, the 6th variable is a list containing the (uadesc), (uamat),(uamatdis),variables you are splitting by, and the variables involved in the analysis
###################################################################################################################



uaindsmt.test<- function(vars, group,conf.level=0.95,datasetname, missing=0)
{
	#The function below captures the complete call to the function
	#It must be stored in a global so that the function that creates the notes item, uaprocdesc can access it
	uadatasets$rproc =uaLogFunArgsDetails()$RuntimeParamValues
	###Functions to call at start
	uainit()
	uastartlog("uaindsmt.test","uaindsmt.test")
	###
	index=which(datasetname==uadatasets$name)
	uavarindex =uagetindex(vars, uaindex=index)
	groupindex=uagetindex(group, uaindex=index)
	uasplitinfo=uagetsplitinfo(index,c(uavarindex,groupindex))
	uaresults =list(NULL)
	if (uasplitinfo[[1]]==1)
	{
		uacartlevels=uasplitinfo[[4]]
		uasplitvarindex=uasplitinfo[[5]]
		# This is the index of the split dataset in uadatasets
		uasplitindex=uadatasets$index
		uadatasets$index=uadatasets$index+1
		i=0
		for (j in 1:uasplitinfo[[2]])
		{
			uadatasets$lst[[uasplitindex]]=subset(uadatasets$lst[[index]],subset=eval(parse(text =uasplitinfo[[3]])), select=c(uavarindex,groupindex))
			# Vars corresponds to the variables in the split dataset
			vars =1:length(uavarindex)
			groups=length(vars)+1
			uatemp=uaindsm(vars, groups,conf.level,index=uasplitindex, missing)
			uaresults[[j]] <-list(uatemp[[1]],uatemp[[2]],uatemp[[3]],uacartlevels[j,],uavarindex)
			i=0
		}
		###Functions to call at end
		ualogcommand()
		uasummary =uaprocdesc(index,missing)
		ualog=uaretlog("uaindsmt.test")
		###
		#The 1st return value tells us whether there is a split or not
		#The 2nd return value tells us whether there is an error or not
		#The 3rd return value tells us how many warnings there are
		#The 4th return value gives us all the log values
		#The 5th return value gives us the summary of the function
		#The 6th return value, if there is a split, gives us a list containing the uadesc, uamat and uamatdis for all the splits
		return(list(uasplitinfo[[1]],uadatasets$error,uadatasets$errorindex, uadatasets$warnindex,ualog,uasummary,uaresults))
	}	
	else
	{
		uamat=uaindsm(uavarindex, groupindex,conf.level,index, missing)
		###Functions to call at end
		ualogcommand()
		uasummary =uaprocdesc(index,missing)
		ualog=uaretlog("uaindsmt.test")
		###
		return(list(0,uadatasets$error, uadatasets$errorindex,uadatasets$warnindex,ualog,uasummary,uamat[[1]],uamat[[2]],uamat[[3]]))
	}
}
#
#uaindsm<-function(uavarindex, groupindex,conf.level,index, missing)
#{
	#m=1
	#n=1
	#noofvars=length(vars)
	## This is the number of rows returned by the independent sample t.test
	##The leven test is performed only where variances are assumed to be equal
	#noofrows=noofvars*2
	##Get the index of all the paired variables in the dataset for which the ttest needs to be computed
	##cindex =uaonesmhandlemissvals(index,uavarindex,noofvars,missing)
  	#cindex =uahandlemissvalskind (index,uavarindex,groupindex,noofvars,missing)
	#uamat= uaindsmttest(cindex, uavarindex, groupindex,noofvars,"two.sided", conf.level, FALSE, index)
	#uadesc =uaindsmdesc(cindex,uavarindex,groupindex,noofvars, index)
	#ualevene =ualevene.test(cindex,uavarindex,groupindex,noofvars, index)
	##Combining the results of the levenes test into the independent samples t.test matrix
	#while (m <=noofrows)
	#{
		#uamat[[1]][m,1:2]=ualevene[[1]][n,1:2]	
		#m=m+2
		#n=n+1
	#}	
	##Combining the results of the uamatdis from the levenes test into the independent samples t.test usmatdis
	#uamat[[2]]=uacombinemat(uamat[[2]],ualevene[[2]])
	#return(list(uadesc,uamat[[1]],uamat[[2]]))
#}

#uacombinemat <-function(mat1,mat2)
#{
	#len1 =nrow(mat1)
	#len2=nrow(mat2)
	#newmat=matrix(nrow=0,ncol=5)
	#i=1
	#j=1
	#while (j<=len2)
	#{
		#newmat=rbind(newmat,mat2[j,])
		#i=1
		#while (i<=len1)		
		#{
			#if(mat2[j,1] ==mat1[i,1])	newmat=rbind(newmat,mat1[i,])
			#i=i+1	
		#}
		#j=j+1	
	#}
	#newmat
#}
		#
#
#uaindsmdesc <-function(cindex,uavarindex,groupindex,noofvars,index)
#{
	#k=1
	#j=1
	#i=1
	#len=noofvars*2
	#uamat =matrix(nrow=len,ncol=4)
	#while (k < len)
		#{
			##We are getting the counts only with the variable that we are doing an independent samples t.test on
			##We are not doing counts on the grouping variable
			##y <- y[!is.na(y)]
			#temp1 =!is.na(eval(uadatasets$temppairs[k]))
			#temp2=eval(uadatasets$temppairs[k+1])
			#uatemp <-table(temp2[temp1])
			##uatemp <-table(eval(uadatasets$temppairs[k+1])[!is.na(eval(uadatasets$temppairs[k]))])
			##uatemp=table(eval(uadatasets$temppairs[k]))
			#uamat[j,1] =uatemp[1]
			#uamat[j+1,1]=uatemp[2]
			#uatemp=tapply(eval(uadatasets$temppairs[k]), eval(uadatasets$temppairs[k+1]), mean, na.rm=TRUE)
			#uamat[j,2] =uatemp[1]
			#uamat[j+1,2]=uatemp[2]
			#uatemp=tapply(eval(uadatasets$temppairs[k]), eval(uadatasets$temppairs[k+1]), sd, na.rm=TRUE)
			#uamat[j,3] =uatemp[1]
			#uamat[j+1,3]=uatemp[2]
			#uatemp=tapply(eval(uadatasets$temppairs[k]), eval(uadatasets$temppairs[k+1]), std.error, na.rm=TRUE)
			#uamat[j,4] =uatemp[1]
			#uamat[j+1,4]=uatemp[2]
			#k=k+2
			#j=j+2
			#i=i+1
		#}
	#uamat
#}
#
#ualevene.test <-function(cindex,uavarindex,groupindex,noofvars, index)
#{
	#i=1
	#p=1
	#j=1
	#uamat =matrix(nrow=noofvars,ncol=2)
	#uamatdis=matrix(nrow=0,ncol=5)
#
	#while (i <=noofvars)
	#{
		#if (!is.na(uavarindex[i]))
		#{ 
			#if(uaperformance==2)
			#{
			#uastartlog("levene.test","uaindsmt.test")	
			#}
		#
			#tryCatch(
			#{
			##NOTE: THE CODE HANDLES THE FACT THAT THERE CAN BE AN ERROR AND ONE OR MORE WARNINGS
			##ON RUNNING A ONE SAMPLE T.TEST ON A SINGLE VARIABLE. WE ALWAYS RETURN THE ERROR FIRST EVEN THOUGH THE WARNING OCCURED FIRST
			#withCallingHandlers(
				#{
				#uatemp <-levenemod.test(eval(uadatasets$temppairs[p]),eval(uadatasets$temppairs[p+1]))
				#},
			#warning = function(ex) 
				#{
				#if(is.null(ex)) {ex <- warnings();}
				#uadatasets$warning =-1;
				#uadatasets$uawarnfn=-1
				#uadatasets$warnindex=uadatasets$warnindex+1;
				#uawarnvar =names(uadatasets$lst[[index]][uavarindex[i]])
				#uarwarnmsg =conditionMessage(ex)
				#uawarnmsgdis =sprintf("levenes test on variable %s generated a warning",uawarnvar)
				#uadatasets$uawarnvar =c(uadatasets$uawarnvar,uawarnvar)
				#uadatasets$uawarnmsgdis =c(uadatasets$uawarnmsgdis,uawarnmsgdis)
				#uadatasets$uarwarnmsg =c(uadatasets$uarwarnmsg,uarwarnmsg );
				#uawritelog(type="Warning", functionname="t.test", uberfunct="uaindsmt.test",message =uarwarnmsg,callfn =conditionCall(ex),uamessage =uadatasets$warnmsgdis);
				#invokeRestart("muffleWarning");
				#}
			         #) # end of withCallingHandlers for catching warnings and continuing execution
			#},
#
			#error = function(ex) 
		        #{
					#uadatasets$error =-1;
					#uadatasets$errorindex =uadatasets$errorindex +1
					#uadatasets$errorfn=-1
					#uaerrvar =names(uadatasets$lst[[index]][uavarindex[i]])
					#uaerrmsgdis =sprintf("Levenes test on variable %s generated an error",uaerrvar) 		
					#uarerrmsg=conditionMessage(ex)
					#uawritelog(type="Error", functionname="levene.test",uberfunct="uaindsmt.test",message =uarerrmsg,callfn =conditionCall(ex),uamessage=uaerrmsgdis);
					#uadatasets$results=c(as.character(i),"-1",uaerrvar,uaerrmsgdis, uarerrmsg)		        
		        #},
		#silent =TRUE) #end of try catch
		#
			#if(uaperformance==2)
			#{
			#ualogcommand()	
			#}
			#if( uadatasets$errorfn ==-1)
			#{
				#uamatdis =rbind(uamatdis,uadatasets$results)
				## AS the levene test for the variable failed, the entries in the uamat for that variable should be NA
				## we need to advance the uamat to the next variable
				#j=j+1
			#}	
			#if(uadatasets$uawarnfn ==-1)
			#{
				#len1 =length(uadatasets$uawarnvar)
				## k is used for indexing the warning variables
				#k=1
				#for (k in 1:len1)
				#{
					##1st position is the index of the variable in uavarindex
					##2nd position is whether everything is OK (1), its an error(-1) or warning(-2)
					##3rd position is the variable name
					##4th position is the warning message that should be displayed
					##5th position is the R warning message
					#uamatdis= rbind(uamatdis, c(as.character(i),"1",uadatasets$uawarnvar[k],uadatasets$uawarnmsgdis[k],uadatasets$uarwarnmsg[k]))
				#}	
				#uadatasets$uawarnvar=NULL
				#uadatasets$uawarnmsgdis=NULL
				#uadatasets$uarwarnmsg=NULL
			#}
			#if (uadatasets$errorfn != -1)
			#{			
				#uadatasets$errorfn =0
				#uadatasets$warning=0
				#uamatdis =rbind(uamatdis,c(as.character(i), "0",NA,NA,NA))
				#uamat[j,1] <-uatemp[[1]]
				#uamat[j,2] <-uatemp[[2]]
				#j<-j+1				
			#}
			#uadatasets$errorfn=0
			#uadatasets$uawarnfn=0
		#}#End of if
	#i=i+1
	#p=p+2
	#}#End of while
#return(list(uamat,uamatdis))
#}	
#
#uaindsmttest <-function(cindex, uavarindex, groupindex,noofvars,uaopt1pass ="two.sided", uacipass, uatyoftestpass =FALSE, index)
#{
#i=1
#j=1
#p=1
#q=1
#uamat =matrix(nrow=(noofvars*2),ncol=9)
#uamatdis=matrix(nrow=0,ncol=5)
#while (i <=noofvars)
#{
   #if (!is.na(uavarindex[i]))
   #{ 
		#while (q<=2)
		#{
			#if (q==1) uavarequal =TRUE
			#else uavarequal =FALSE
		#
			#if(uaperformance==2)
			#{
			#uastartlog("t.test","uaindsmt.test")	
			##ostarttime =date()
			##starttime=proc.time()
			##initialmem=gc()
			#}
	#tryCatch(
	#{
		##NOTE: THE CODE HANDLES THE FACT THAT THERE CAN BE AN ERROR AND ONE OR MORE WARNINGS
		##ON RUNNING A ONE SAMPLE T.TEST ON A SINGLE VARIABLE. WE ALWAYS RETURN THE ERROR FIRST EVEN THOUGH THE WARNING OCCURED FIRST
		#withCallingHandlers(
				#{
				#uatemp <-t.test(eval(uadatasets$temppairs[p])~eval(uadatasets$temppairs[p+1]), alternative=uaopt1pass, conf.level=uacipass, paired=uatyoftestpass,var.equal=uavarequal)
				#},
			    #warning = function(ex) 
				#{
				#if(is.null(ex)) {ex <- warnings();}
				#uadatasets$warning =-1;
				#uadatasets$uawarnfn=-1
				#uadatasets$warnindex=uadatasets$warnindex+1;
				#uawarnvar =names(uadatasets$lst[[index]][uavarindex[i]])
				#uarwarnmsg =conditionMessage(ex)
				#uawarnmsgdis =sprintf("Independent sample T test on variable %s generated a warning",uawarnvar)
				#uadatasets$uawarnvar =c(uadatasets$uawarnvar,uawarnvar)
				#uadatasets$uawarnmsgdis =c(uadatasets$uawarnmsgdis,uawarnmsgdis)
				#uadatasets$uarwarnmsg =c(uadatasets$uarwarnmsg,uarwarnmsg );
				#uawritelog(type="Warning", functionname="t.test", uberfunct="uaindsmt.test",message =uarwarnmsg,callfn =conditionCall(ex),uamessage =uadatasets$warnmsgdis);
				#invokeRestart("muffleWarning");
				#}
			         #) # end of withCallingHandlers for catching warnings and continuing execution
	#},
#
	#error = function(ex) 
		        #{
					#uadatasets$error =-1;
					#uadatasets$errorindex =uadatasets$errorindex +1
					#uadatasets$errorfn=-1
					#uaerrvar =names(uadatasets$lst[[index]][uavarindex[i]])
					#uaerrmsgdis =sprintf("Independent sample T test on variable %s generated an error",uaerrvar) 		
					#uarerrmsg=conditionMessage(ex)
					#uawritelog(type="Error", functionname="t.test",uberfunct="uaindsmt.test",message =uarerrmsg,callfn =conditionCall(ex),uamessage=uaerrmsgdis);
					#uadatasets$results=c(as.character(i),"-1",uaerrvar,uaerrmsgdis, uarerrmsg)		        
		        #},
	#silent =TRUE)
		#
	#if(uaperformance==2)
	#{
	#ualogcommand()	
	#}
	#if( uadatasets$errorfn ==-1)
			#{
				#uamatdis =rbind(uamatdis,uadatasets$results)
				## AS the t test for the variable failed, the entries in the uamat for that variable should be NA
				## we need to advance the uamat to the next variable
				#j=j+1
			#}	
	#if(uadatasets$uawarnfn ==-1)
			#{
				#len1 =length(uadatasets$uawarnvar)
				## k is used for indexing the warning variables
				#k=1
				#for (k in 1:len1)
				#{
					##1st position is the index of the variable in uavarindex
					##2nd position is whether everything is OK (1), its an error(-1) or warning(-2)
					##3rd position is the variable name
					##4th position is the warning message that should be displayed
					##5th position is the R warning message
					#uamatdis= rbind(uamatdis, c(as.character(i),"1",uadatasets$uawarnvar[k],uadatasets$uawarnmsgdis[k],uadatasets$uarwarnmsg[k]))
				#}	
				#uadatasets$uawarnvar=NULL
				#uadatasets$uawarnmsgdis=NULL
				#uadatasets$uarwarnmsg=NULL
			#}
	#if (uadatasets$errorfn != -1)
			#{			
				##equal variances are assumeed
				#if (q==1)
					#{
						#uadatasets$errorfn =0
						#uadatasets$warning=0
						#uamatdis =rbind(uamatdis,c(as.character(i), "0",NA,NA,NA))
						#uamat[j,3] <-uatemp$statistic
						#uamat[j,4] <-uatemp$parameter
						#uamat[j,5] <-uatemp$p.value
						#uamat[j,6]<-uatemp$estimate[1]-uatemp$estimate[2]
						##uamat[j,7] <-uatemp$standarderr[1]-uatemp$standarderr[2]
						#uamat[j,8] <-uatemp$conf.int[1]
						#uamat[j,9] <-uatemp$conf.int[2]
						#j<-j+1
					#}
				##We have not assumed equal variances
				#if (q==2)
					#{
						#uadatasets$errorfn =0
						#uadatasets$warning=0
						#uamatdis =rbind(uamatdis,c(as.character(i), "0",NA,NA,NA))
						#uamat[j,3] <-uatemp$statistic
						#uamat[j,4] <-uatemp$parameter
						#uamat[j,5] <-uatemp$p.value
						#uamat[j,6]<-uatemp$estimate[1]-uatemp$estimate[2]
						##uamat[j,7] <-uatemp$standarderr[1]-uatemp$standarderr[2]
						#uamat[j,8] <-uatemp$conf.int[1]
						#uamat[j,9] <-uatemp$conf.int[2]
						#j<-j+1
					#}
			#}
			#uadatasets$errorfn=0
			#uadatasets$uawarnfn=0
			#q=q+1
		  #}# End of while for performing the t.test for both equal and not equal variances assumed
		#}#End of if
	#i=i+1
	#p=p+2
	#q=1
	#}#End of while
#return(list(uamat,uamatdis))
#}

# Do I need a call ualog=uaretlog("uaonesmt.test")


#****************************************************************
#Things to do
#Add calling function for debug logging. Post the  question of getting the calling function as a string
#Add calling function for errors and warnings back to Clive's program
#think about no of decimals
#We should code alternate i.e. single sided or two sided

#################################################################
#ONE SAMPLE T.TEST
#################################################################
#The arguments
#vars -The pairs of values that need to be passed
#mu -The value the mean needs to be compared against
#conf.level -This is the confidence interval
#Datasetname -The name of the dataset
#missing -0 indicates that missing values will be handled analysis by analysis
#missing -1 indicates missing values will be handled listwise

#################################################################
#ONE SAMPLE T.TEST
#################################################################
#The arguments
#vars -The variables that need to be passed
#mu -The value the mean needs to be compared against
#conf.level -This is the confidence interval
#Datasetname -The name of the dataset
#missing -0 indicates that missing values will be handled analysis by analysis
#missing -1 indicates missing values will be handled listwise




#################################################################################
#The return structure
#################################################################################



#The 1st return value tells us whether there is a split or not. The object name is split
#The 2nd return value tells us whether there is an error or not. The object name is error.
#The 3rd return value tells us how many errors there are. The object name is nooferrors.
#The 4rd return value tells us how many warnings there are. The object name is noofwarnings
#The 5th return value gives us all the log values. The object name is log
#The 6th return value gives us the summary of the function. The object name is summary.
#The 7th return value gives us the number of tables we need to display. The object name is nooftables.
#The 8th return value onwards returns a list for every table or graph we need to display

 
 
#Each list contains the following elements
#1-the type, this is either table or graph. The object name is type
#2 -metadata, this is either yes or no. This tells us whether there is an accompanying meta data table. The object name is metadata
#3 -The number of meta data tables. The object name is nometadatatables
#4 -The type of the meta data tables. There is a type associated with each meta data table returned.Currently we support
# normal and crosstab types. The object name is metadatatabletype
#5 -metadatatable, this is a list containing all the meta data tables. The tables must be listed according to the type 
#specified in 4. The object name is metadatatable
#6 -datatable, this contains the data. The object name is datatable
#7 -cartlevel, If there are splits, this gives us the value of the levels we are splitting by. If there are no splits,
# this object is not returned. Used for trouble shooting purposes. The object name is cartlevel.
#8 -varindex, if there are splits, this tells us the variables involved in the split. Used for trouble shooting purposes. The object name is
#varindex








#uatemplist is a temporary variable used to hold the results fo that we can append the lists that
#represent the tables to display
#See code uasplitinfo[[2]]*2, we multiply by 2 as we are returning 2 list of display structures for every table that 
#needs to be displayed


##################################################################
#Sanjay: This is the uber function
uaonesmt.test<- function(vars, mu=0,conf.level=0.95, cohens_d=FALSE, cohensd_correction=FALSE,hedges_g =FALSE, hedgesg_correction=FALSE,glass_d=FALSE, glassd_correction=FALSE, datasetname, missing=0)
{
	#The function below captures the complete call to the function
	#It must be stored in a global so that the function that creates the notes item, uaprocdesc can access it
	uadatasets$rproc =uaLogFunArgsDetails()$RuntimeParamValues
	###Functions to call at start
	uainit()
	uastartlog("uaonesmt.test","uaonesmt.test")
	###	
	index=which(datasetname==uadatasets$name)
	uavarindex =uagetindex(vars, uaindex=index)
	uasplitinfo=uagetsplitinfo(index,uavarindex)
	uaresults =list()
	#used to track the number of tables returned
	nooftablestodis =0
	if (uasplitinfo[[1]]==1)
	{
		uacartlevels=uasplitinfo[[4]]
		uasplitvarindex=uasplitinfo[[5]]
		# This is the index of the split dataset in uadatasets
		# added a plus 1 below to accomodate Anils code
		#uadatasets always show the no of datasets open
		uasplitindex=uadatasets$index+1
		#Also commented the line below
		#When Anil opens another dataset, the split dataset is overwritten
		#uadatasets$index=uadatasets$index+1
		i=0
		j=1
		for (j in 1:uasplitinfo[[2]])
		{
			uadatasets$lst[[uasplitindex]]=subset(uadatasets$lst[[index]],subset=eval(parse(text =uasplitinfo[[3]])), select=uavarindex)
			# Vars corresponds to the variables in the split dataset
			vars =1:length(uavarindex)
			uatemp=uaonesample(vars, mu,conf.level,cohens_d=FALSE, cohensd_correction=FALSE,hedges_g =FALSE, hedgesg_correction=FALSE,glass_d=FALSE, glassd_correction=FALSE,index=uasplitindex, missing)
			# This is the case of am empty dataset
			if (length(uatemp) ==1)
			{
				uatemp[[1]]$cartlevel=uacartlevels[j,]
				uatemp[[1]]$varindex=uavarindex
				#uaresults[[j]] <-list(uatemp[[1]],uatemp[[2]])
				#uaresults= c(uaresults,uatemp[[1]],uatemp[[2]])
				uaresults <- c(uaresults, list(uatemp[[1]]))
				#This variable calculates the number of tables as there could be empty datasets for splits where there is no data
				#In the case of an empty dataset, we are only returning a single table per split
				nooftablestodis=nooftablestodis+1
			}
			else
			{
				#This is for the typical case when the one sample t.test returns 2 tables, one for descriptives 
				#and the other for the results of the t.test
				uatemp[[1]]$cartlevel=uacartlevels[j,]
				uatemp[[1]]$varindex=uavarindex
				uatemp[[2]]$cartlevel=uacartlevels[j,]
				uatemp[[2]]$varindex=uavarindex
				#uaresults[[j]] <-list(uatemp[[1]],uatemp[[2]])
				#uaresults= c(uaresults,uatemp[[1]],uatemp[[2]])
				uaresults <- c(uaresults, list(uatemp[[1]]), list(uatemp[[2]]))
				nooftablestodis=nooftablestodis+2
			}
			j=j+1
			i=0
		}
		###Functions to call at end
		ualogcommand()
		uasummary =uaprocdesc(index,missing)
		ualog=uaretlog("uaonesmt.test")
		###
		#The 1st return value tells us whether there is a split or not. The object name is split
		#The 2nd return value tells us whether there is an error or not. The object name is error.
		#The 3rd return value tells us how many errors there are. The object name is nooferrors.
		#The 4rd return value tells us how many warnings there are. The object name is noofwarnings
		#The 5th return value gives us all the log values. The object name is log
		#The 6th return value gives us the summary of the function. The object name is summary.
		#The 7th return value gives us the number of tables we need to display. The object name is nooftables.
		#The 8th return value onwards returns a list for every table or graph we need to display
		
		#Each list contains the following elements
		#1-the type, this is either table or graph. The object name is type
		#2 -metadata, this is either yes or no. This tells us whether there is an accompanying meta data table. The object name is metadata
		#3 -The number of meta data tables. The object name is nometadatatables
		#4 -The type of the meta data tables. There is a type associated with each meta data table returned.Currently we support
		# normal and crosstab types. The object name is metadatatabletype
		#5 -metadatatable, this is a list containing all the meta data tables. The tables must be listed according to the type 
		#specified in 4.
		#6 -datatable, this contains the data
		#7 -cartlevel, If there are splits, this gives us the value of the levels we are splitting by. If there are no splits,
		# this object is not returned. Used for trouble shooting purposes. The object name is cartlevel.
		#8 -varindex, if there are splits, this tells us the variables involved in the split. Used for trouble shooting purposes. The object name is
		#varindex
		
		#uatemplist is a temporary variable used to hold the results fo that we can append the lists that
		#represent the tables to display
		# See code uasplitinfo[[2]]*2, we multiply by 2 as we are returning 2 list of display structures for every table that 
		#needs to be displayed
		uatemplist=list(split =uasplitinfo[[1]],error =uadatasets$error,nooferrors=uadatasets$errorindex, noofwarnings=uadatasets$warnindex,log=ualog,uasummary,nooftables =uasplitinfo[[2]]*2)
		# This code merges the results of the one sample t.test run for every split into a return struction where
		#the number of tables is known and we return a single list element to represent every table
		j=8
		k=1
		len=nooftablestodis
		#working code below
		#for (k in 1:len)
		#	{
		#	uatemplist[[j]]<-uaresults[[k]]
		#	j=j+1
		
		#new code added
		for (k in 1:len) uatemplist= c(uatemplist, list(uaresults[[k]]))
		return(uatemplist)		
	}	
	else
	{
		uamat=uaonesample(uavarindex, mu,conf.level,cohens_d=FALSE, cohensd_correction=FALSE,hedges_g =FALSE, hedgesg_correction=FALSE,glass_d=FALSE, glassd_correction=FALSE,index, missing)
		
		###Functions to call at end
		ualogcommand()
		uasummary =uaprocdesc(index,missing)
		ualog=uaretlog("uaonesmt.test")
		###if length(uamat)==1, there is a critical error
		if (length(uamat)==1) 
		{
			nooftablestodis =1
			return(list(split=0,error=uadatasets$error, nooferrors=uadatasets$errorindex,noofwarnings=uadatasets$warnindex,log=ualog,uasummary,nooftables =nooftablestodis,uamat[[1]]))			
		}
		else
		{
			#no of tables represents the number of tables we are going to display
			nooftablestodis =2
			return(list(split=0,error=uadatasets$error, nooferrors=uadatasets$errorindex,noofwarnings=uadatasets$warnindex,log=ualog,uasummary,nooftables =nooftablestodis,uamat[[1]],uamat[[2]]))
		}
	}
}

newBskycheckempty<-function(uavarindex,index)
{
	
	
	tryCatch(
		{	
			withCallingHandlers(
			{
				if (nrow(uadatasets$lst[[index]])==0 && ncol(uadatasets$lst[[index]])==0)
				{
					stop("The dataset is empty")
				}
			},
				
			warning = UAwarnHandlerFn

			) # end of withCallingHandlers for catching warnings and continuing execution	
		},
		error = UAerrHandlerFn,
		silent =TRUE		
		)
}

Bskycheckempty<-function(uavarindex,index)
{
	
	if (nrow(uadatasets$lst[[index]])==0 && ncol(uadatasets$lst[[index]])==0)
	{
		uamatdis =rbind(uamatdis,c(NA,"-3",NA,"The dataset is empty", NA))
		uaretstructure[[1]]$type="table"
		uaretstructure[[1]]$metadata="yes"
		uaretstructure[[1]]$nometadatatables=1
		uaretstructure[[1]]$metadatatabletype="normal"
		uaretstructure[[1]]$metadatatable=uamatdis
		uaretstructure[[1]]$datatable=NULL
		return(TRUE)
	}
	else 
	{
	return(FALSE)
	}
		
}


#30Jul2018	
# Changes to BSkyOneSmTTest,  uaonesample, uaonesamttest to support alternative	
 uaonesample<- function (uavarindex, mu, conf.level, alternative,cohens_d=FALSE, cohensd_correction=FALSE,hedges_g =FALSE, hedgesg_correction=FALSE,glass_d=FALSE, glassd_correction=FALSE,index, missing) 
{
    BSkyFunctionInit()
    BSkyErrMsg = "Error in One Sample T.test"
    BSkyWarnMsg = "Warning in One Sample T.test"
    BSkyStoreApplicationWarnErrMsg(BSkyWarnMsg, BSkyErrMsg)
    bskyNoofTables = length(uadatasets$retstructure)
    if (nrow(uadatasets$lst[[index]]) == 0) {
        uawritelog(type = "Error", BSkyMessage = "One sample T test cannot be run as the dataset is empty")
        if (bskyNoofTables == 0) {
            uadatasets$retstructure[[1]] <- list()
            uadatasets$retstructure[[1]]$metadatatable[[1]] = data.frame()
            uadatasets$retstructure[[1]]$type = "table"
            uadatasets$retstructure[[1]]$metadata = "yes"
            uadatasets$retstructure[[1]]$nometadatatables = 1
            uadatasets$retstructure[[1]]$metadatatabletype = "normal"
            uadatasets$retstructure[[1]]$metadatatable = list()
            uadatasets$retstructure[[1]]$metadatatable[[1]] = data.frame(varIndex = NA, 
                type = -2, varName = NA, dataTableRow = NA, startCol = NA, 
                endCol = NA, BSkyMsg = "One sample T test cannot be run as the dataset is empty", 
                Rmsg = NA)
            uadatasets$retstructure[[1]]$datatable = NULL
            uadatasets$error = -1
            uadatasets$errorindex = uadatasets$errorindex + 1
        }
        else {
            uadatasets$retstructure[[bskyNoofTables]]$metadatatable[[1]] = rbind(uadatasets$retstructure[[bskyNoofTables]]$metadatatable[[1]], 
                data.frame(varIndex = NA, type = -2, varName = NA, 
                  dataTableRow = NA, startCol = NA, endCol = NA, 
                  BSkyMsg = "One sample T test cannot be run as the dataset is empty", 
                  Rmsg = NA))
        }
        BSkyFunctionWrapUp()
        return(TRUE)
    }
    noofvars = length(uavarindex)
    cindex = uabihandlemissvalsnew1(index, uavarindex, noofvars, 
        missing)
    if (bskyNoofTables == 0) {
        uadatasets$retstructure[[1]] <- list()
        uadatasets$retstructure[[1]]$type = "table"
        uadatasets$retstructure[[1]]$metadata = "yes"
        uadatasets$retstructure[[1]]$nometadatatables = 1
        uadatasets$retstructure[[1]]$metadatatabletype = c("normal")
        uadatasets$retstructure[[1]]$metadatatable = list()
        uadatasets$retstructure[[1]]$metadatatable[[1]] = data.frame()
    }
    uaonesmdesc(cindex, uavarindex, noofvars, index)
    uadatasets$retstructure[[2]] <- list()
    uadatasets$retstructure[[2]]$type = "table"
    uadatasets$retstructure[[2]]$metadata = "yes"
    uadatasets$retstructure[[2]]$nometadatatables = 1
    uadatasets$retstructure[[2]]$metadatatabletype = c("normal")
    uadatasets$retstructure[[2]]$metadatatable = list()
    uadatasets$retstructure[[2]]$metadatatable[[1]] = data.frame()
    uaonesamttest(cindex, uavarindex, noofvars,  
      alternative,  conf.level, FALSE, index, mu)
	  
	indexInReturnStructure=3		  
	if (cohens_d ||cohensd_correction )
	{
	
	 uadatasets$retstructure[[indexInReturnStructure]] <- list()
    uadatasets$retstructure[[indexInReturnStructure]]$type = "table"
    uadatasets$retstructure[[indexInReturnStructure]]$metadata = "yes"
    uadatasets$retstructure[[indexInReturnStructure]]$nometadatatables = 1
    uadatasets$retstructure[[indexInReturnStructure]]$metadatatabletype = c("normal")
    uadatasets$retstructure[[indexInReturnStructure]]$metadatatable = list()
    uadatasets$retstructure[[indexInReturnStructure]]$metadatatable[[1]] = data.frame()
	
	cohensd (cindex,uavarindex, noofvars,correction=cohensd_correction, uacipass=conf.level, index,mu, indexInReturnStructure)
	indexInReturnStructure =indexInReturnStructure+1
	
	}
	if (hedges_g || hedgesg_correction)
	{
	 uadatasets$retstructure[[indexInReturnStructure]] <- list()
    uadatasets$retstructure[[indexInReturnStructure]]$type = "table"
    uadatasets$retstructure[[indexInReturnStructure]]$metadata = "yes"
    uadatasets$retstructure[[indexInReturnStructure]]$nometadatatables = 1
    uadatasets$retstructure[[indexInReturnStructure]]$metadatatabletype = c("normal")
    uadatasets$retstructure[[indexInReturnStructure]]$metadatatable = list()
    uadatasets$retstructure[[indexInReturnStructure]]$metadatatable[[1]] = data.frame()
	hedgesg (cindex,uavarindex, noofvars,correction=hedgesg_correction, uacipass=conf.level, index,mu,indexInReturnStructure)
	indexInReturnStructure =indexInReturnStructure+1
	}
	if (glass_d || glassd_correction)
	{
	 uadatasets$retstructure[[indexInReturnStructure]] <- list()
    uadatasets$retstructure[[indexInReturnStructure]]$type = "table"
    uadatasets$retstructure[[indexInReturnStructure]]$metadata = "yes"
    uadatasets$retstructure[[indexInReturnStructure]]$nometadatatables = 1
    uadatasets$retstructure[[indexInReturnStructure]]$metadatatabletype = c("normal")
    uadatasets$retstructure[[indexInReturnStructure]]$metadatatable = list()
    uadatasets$retstructure[[indexInReturnStructure]]$metadatatable[[1]] = data.frame()
	glassd (cindex,uavarindex, noofvars,correction=glassd_correction, uacipass=conf.level, index,mu,indexInReturnStructure)
	}
	  
	
    BSkyFunctionWrapUp()
    return(TRUE)
}




#30Jul2018	
# Changes to BSkyOneSmTTest,  uaonesample, uaonesamttest to support alternative
uaonesamttest <-function(cindex,uavarindex, len,uaopt1pass ="two.sided", uacipass=.95, uatyoftestpass =FALSE,index,valuetocompare)
{
	# every ua sub function must call the following two 
    # initislization call at the begining of the function as follows 
   BSkyFunctionInit()

	#uadatasets$retstructure[[1]]$metadatatable=list(BSkyonesmdesc[[2]])
	uadatasets$retstructure[[2]]$datatable=matrix(nrow=len,ncol=6)
	i=1
	j=1
	#warning ("Valerie");

while (i <=len)
{
  if (!is.na(uavarindex[i]))
  { 
	if(uaperformance==2)
	{
	uastartlog("t.test","uaonesmt.test")	
	#ostarttime =date()
	#starttime=proc.time()
	#initialmem=gc()
	}
	uavar =names(uadatasets$lst[[index]][uavarindex[i]])
	uadatasets$uawarnmsgdis =sprintf("One sample T test on variable %s generated a warning",uavar)
	uadatasets$uaerrmsgdis =sprintf("One sample T test on variable %s generated an error",uavar)		
	tryCatch(
	{
		#NOTE: THE CODE HANDLES THE FACT THAT THERE CAN BE AN ERROR AND ONE OR MORE WARNINGS
		#ON RUNNING A ONE SAMPLE T.TEST ON A SINGLE VARIABLE. WE ALWAYS RETURN THE ERROR FIRST EVEN THOUGH THE WARNING OCCURED FIRST
		withCallingHandlers(
				{
				uatemp <-uarettest(eval(uadatasets$temppairs[i]), mu=valuetocompare, alternative=uaopt1pass, conf.level=uacipass, paired=uatyoftestpass)
				},
			        warning = UAwarnHandlerFn
				           ) # end of withCallingHandlers for catching warnings and continuing execution
	},

	error = UAerrHandlerFn,
	silent =TRUE)
	if(uaperformance==2)
	{
	ualogcommand()	
	}
	
	if( uadatasets$errorfn ==-1)
			{
				uadatasets$retstructure[[2]]$metadatatable[[1]] =rbind(uadatasets$retstructure[[2]]$metadatatable[[1]],data.frame(varIndex=i,type=-1,varName=uavar,dataTableRow=i,startCol=1,endCol=6,BSkyMsg=uadatasets$uaerrmsgdis, RMsg=uadatasets$uarerrmsg))
				# AS the t test for the variable failed, the entries in the uamat for that variable should be NA
				# we need to advance the uamat to the next variable
				j=j+1
			}	
	if(uadatasets$uawarnfn ==-1)
			{
				len1 =length(uadatasets$uarwarnmsg)
				# k is used for indexing the warning variables
				k=1
				for (k in 1:len1)
				{
					#1st position is the index of the variable in uavarindex
					#2nd position is whether everything is OK (1), its an error(-1) or warning(-2)
					#3rd position is the variable name
					#4th position is the warning message that should be displayed
					#5th position is the R warning message
					uadatasets$retstructure[[2]]$metadatatable[[1]] = rbind(uadatasets$retstructure[[2]]$metadatatable[[1]] , data.frame(varIndex=i,type=1,varName=uavar,dataTableRow=i,startCol=1,endCol=6,BSkyMsg=uadatasets$uawarnmsgdis,RMsg=uadatasets$uarwarnmsg[k]))
				}	
				uadatasets$uawarnvar=NULL
				uadatasets$uawarnmsgdis=NULL
				uadatasets$uarwarnmsg=NULL
			}
	if (uadatasets$errorfn != -1)
			{			
				uadatasets$errorfn =0
				uadatasets$warning=0
				uadatasets$retstructure[[2]]$datatable[j,1] <-uatemp$statistic
				uadatasets$retstructure[[2]]$datatable[j,2] <-uatemp$parameter
				uadatasets$retstructure[[2]]$datatable[j,3] <-uatemp$p.value
				uadatasets$retstructure[[2]]$datatable[j,4]<-uatemp$estimate -valuetocompare
				uadatasets$retstructure[[2]]$datatable[j,5] <-uatemp$conf.int[1]
				uadatasets$retstructure[[2]]$datatable[j,6] <-uatemp$conf.int[2]
				j<-j+1
			}
			uadatasets$errorfn=0
			uadatasets$uawarnfn=0
		}#End of if
	i=i+1
	}#End of while
BSkyFunctionWrapUp()
return(TRUE)
}



##############################################################
# This function is no longer used, we use uabihandlemissvalsnew1
###############################################################


uaonesmhandlemissvals <-function(index,uavarindex,noofvars,missing=0)
{
BSkyFunctionInit()	
cindex=NULL	
uadatasetstemppairs=NULL
if (uaaretheremissval(index,uavarindex))
	{
	if (missing ==1)
		{
			#Corresponds to missing values listwise
			cindex[1:noofvars] =index
			uadatasets$temp[[index]] =na.omit(uadatasets$lst[[index]][,uavarindex])
			uaplaceholder =c(expression(uadatasets$temp[[cindex[i]]][,i]))
			uadatasets$temppairs =rep(uaplaceholder,noofvars)
		}
	else
		{
			cindex[1:noofvars]=index
			uaplaceholder =c(expression(uadatasets$lst[[cindex[i]]][,uavarindex[i]]))
			uadatasets$temppairs =rep(uaplaceholder,noofvars)
		}
	}
#for missing values analysis by analysis we will just pass the original dataset in usdatasets 
#We will handle the missing values in the mean
else
{
cindex[1:noofvars]=index
uaplaceholder =c(expression(uadatasets$lst[[cindex[i]]][,uavarindex[i]]))
uadatasets$temppairs =rep(uaplaceholder,noofvars)
}
BSkyFunctionWrapUp()
return(cindex)
}


##########################################################################
# To Do
##########################################################################
#The 2 variables being passed to the error and waring messages need to be converted to a string
#We should allow the person to specify one sided or 2 sided



##########################################################################
#Paired sample t.test
##########################################################################
#The arguments
#pairs -The pairs of values that need to be passed
#conf.level -This is the confidence interval
#missing -0 indicates that missing values will be handled analysis by analysis
#missing -1 indicates missing values will be handled listwise
#Datasetname -The name of the dataset

#Return values
#A list containing 
#uadesc descriptive statistics
#uacorr correlations and the significance
#uamat results of the ttest run between each pairs of variables
###########################################################################

#Type of t.test for paired sample is hard coded to two.sided
#Paired =TRUE is hard coded. Paired =TRUE performs a paired T-test similiar to SPSS, when PAIRED =FALSE a
#Welch 2 sample T-test is performed. The code below us not tested for PAIRED =False
#We cannot guarantee results when paired = FALSE 
#I have compared results with spss and numbers match for Paired =TRUE
#See uapasmttest for more details on Paired =TRUE


#datasetreference is first parameter
#list of variables -dependent variable list
#independent variable list
#options list
#cindex holds the dataset in the temporary space


uapairedsmt.test<- function(pairs,conf.level=0.95,datasetname,missing =0)
{
	#The function below captures the complete call to the function
	#It must be stored in a global so that the function that creates the notes item, uaprocdesc can access it
	uadatasets$rproc =uaLogFunArgsDetails()$RuntimeParamValues
	###Functions to call at start
	uainit()
	uastartlog("uapairedsmt.test","uapairedsmt.test")
	###	
	index=which(datasetname==uadatasets$name)
	uavarindex =uagetindex(pairs, uaindex=index)
	uasplitinfo=uagetsplitinfo(index,uavarindex)
	uaresults =list(NULL)
	if (uasplitinfo[[1]]==1)
	{
		uacartlevels=uasplitinfo[[4]]
		uasplitvarindex=uasplitinfo[[5]]
		# This is the index of the split dataset in uadatasets
		uasplitindex=uadatasets$index
		uadatasets$index=uadatasets$index+1
		i=0
		for (j in 1:uasplitinfo[[2]])
		{
			uadatasets$lst[[uasplitindex]]=subset(uadatasets$lst[[index]],subset=eval(parse(text =uasplitinfo[[3]])), select=uavarindex)
			# Vars corresponds to the variables in the split dataset
			vars =1:length(uavarindex)
			uatemp=uapairedsample(vars,conf.level=0.95,index=uasplitindex, missing)
			uaresults[[j]] <-list(uatemp[[1]],uatemp[[2]],uatemp[[3]],uatemp[[4]],uatemp[[5]],uacartlevels[j,],uavarindex)
			i=0
		}
		###Functions to call at end
		ualogcommand()
		uasummary =uaprocdesc(index,missing)
		ualog=uaretlog("uapairedsmt.test")
		###
		#The 1st return value tells us whether there is a split or not
		#The 2nd return value tells us whether there is an error or not
		#The 3rd return value tells us how many warnings there are
		#The 4th return value gives us all the log values
		#The 5th return value gives us the summary of the function
		#The 6th return value, if there is a split, gives us a list containing the uadesc, uamat and uamatdis for all the splits
		return(list(uasplitinfo[[1]],uadatasets$error,uadatasets$errorindex, uadatasets$warnindex,ualog,uasummary,uaresults))
	}	
	else
	{
		uamat=uapairedsample(uavarindex, conf.level=0.95,index, missing)
		###Functions to call at end
		ualogcommand()
		uasummary =uaprocdesc(index,missing)
		ualog=uaretlog("uapairedsmt.test")
		###
		return(list(0,uadatasets$error, uadatasets$errorindex,uadatasets$warnindex,ualog,uasummary,uamat[[1]],uamat[[2]],uamat[[3]],uamat[[4]],uamat[[5]]))
	}
}


uapairedsample<- function(uavarindex,conf.level =0.95,index,missing =0)
{
uacommand=NULL
#if( uaperformance ==1) 
#{
#	uastartlog("uapairedsample")
#	uacommand =paste("uapairedsample(pairs=c(\"", uavectortostring(pairs),"\")", ",conf.level=",conf.level,",datasetname=\"",uavectortostring(datasetname),"\"",",missing=",missing,")",sep="")
#	uawritelog(type="Functioncall",functionname="uapairedsample",functcommand=uacommand,starttime=uadatasets$ostarttime)
#}
noofvars=length(uavarindex)
cindex =uapasmhandlemissvals(index,uavarindex,noofvars,missing)
#Run the t.test
uamat= uapasmttest(cindex,uavarindex,noofvars,"two.sided", conf.level, TRUE,index)
#Get descriptive statistics for all the variables
uadesc =uapairedsmdesc(cindex,uavarindex,noofvars,index)
#Get the correlation between the variables and measure the significance of the correlation
uacorr=uagetcorr(cindex,uavarindex,noofvars,uadatasets,index)
uaretlist =list(uadesc,uacorr[[1]],uacorr[[2]],uamat[[1]],uamat[[2]])
#if(exists("temp", envir=uadatasets)) rm(temp,envir=uadatasets)
#if( uaperformance ==1)ualogcommand()
#uacleanup()
return(uaretlist)
}


#This function returns the descriptives. This is table 2

uapairedsmdesc <-function(cindex,uavarindex,len,index)
{
uamat =matrix(nrow=len,ncol=4)
j=1
for (i in 1:len)
	{
	j=i
	uamat[i,1]=mean(x=eval(uadatasets$temppairs[i]),na.rm=FALSE)
	uamat[i,2]=length(eval(uadatasets$temppairs[i]))
	uamat[i,3]=sd(eval(uadatasets$temppairs[i]),na.rm=FALSE)
	#we could optimize the std error to use the std dev above
	uamat[i,4]=sqrt((var(eval(uadatasets$temppairs[i]),na.rm="FALSE")/uamat[i,2]))
	}
uamat
}


uaonesmdesc <-function(cindex,uavarindex,len,index)
{
BSkyFunctionInit()

#uadatasets$retstructure[[1]]$metadatatable=data.frame()
uadatasets$retstructure[[1]]$datatable=matrix(nrow=len,ncol=4)
	
BSkyerrvar=NULL
BSkyerrmsgdis=NULL
BSkyfootervar=NULL
BSkyfootermsgdis=NULL
BSkyvalcases=NULL
		#cat("\n Aaron's Entry uaonesmdesc:\n")
		#print(uadatasets$retstructure)
		#cat("\n##########Entry#################\n")	
		#cat("\n Len:",len,"\n")
		#cat("\n",cindex,"\n")
		#cat("\n",uavarindex,"\n")
#j is used for the indirection within temppairs
j=1
for (i in 1:len)
{
	#cat("\n uaonesmdesc -1")
	j=i
	BSkyvalcases =!is.na(eval(uadatasets$temppairs[i]))
	#cat("\n uaonesmdesc0", BSkyvalcases,"\n")
	#print(eval(uadatasets$temppairs[i]))
	#cat("\n----+")
	#abc = eval(uadatasets$temppairs[i])
	#print(class(abc) )
	#cat("\n=-+")
	#print(typeof(abc) )
	#cat("\n+=-+")
	
		#print(class(BSkyvalcases) )
	#cat("\n++=-+")
	
		#print(typeof(BSkyvalcases) )
	#cat("\n=++++-+")
	#BSkynoofcases=length(eval(uadatasets$temppairs[i])[!is.na(eval(uadatasets$temppairs[i]))])
	###BSkynoofcases=length( as.data.frame( eval(uadatasets$temppairs[i]))[BSkyvalcases])
	#x=abc[BSkyvalcases]
	#print(x)
	#cat("\n++--\n")
	BSkynoofcases=length( eval(uadatasets$temppairs[i])[BSkyvalcases])
	#BSkynoofcases=100
	#cat("\nBSkynoofcases:",BSkynoofcases,"\n")
	if (is.factor(eval(uadatasets$temppairs[i])))
	{
		BSkyerrvar =names(uadatasets$lst[[index]][uavarindex[i]])
		BSkyerrmsgdis =sprintf("Error: variable %s is a factor, descriptive statistics is not supported for factors in One Sample t.test ",BSkyerrvar)
		uadatasets$errorindex =uadatasets$errorindex +1
		uadatasets$retstructure[[1]]$metadatatable[[1]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[1]], data.frame(varIndex=i,type=-1,varName=BSkyerrvar,dataTableRow=i,startCol=1,endCol=4,BSkyMsg=BSkyerrmsgdis,RMsg=""))		
		uawritelog(type="Error", functionName="uaonesmdesc", BSkyMessage =BSkyerrmsgdis )
		#cat("\n uaonesmdesc1")
	}
		
	else if (BSkynoofcases==0)
	{
		#cat("\n uaonesmdesc2")
		BSkyerrvar =names(uadatasets$lst[[index]][uavarindex[i]])
		BSkyerrmsgdis =sprintf("Error: there are no valid cases for variable %s",BSkyerrvar)
		uadatasets$errorindex =uadatasets$errorindex +1
		uadatasets$retstructure[[1]]$metadatatable[[1]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[1]], data.frame(varIndex=i,type=-1,varName=BSkyerrvar,dataTableRow=i,startCol=1,endCol=4,BSkyMsg=BSkyerrmsgdis,RMsg=""))
		uawritelog(type="Error", functionName="uaonesmdesc",BSkyMessage =BSkyerrmsgdis )
	}
	else
	{	
		uadatasets$retstructure[[1]]$datatable[i,1]=sum(is.na(eval(uadatasets$temppairs[i]))==FALSE)
		uadatasets$retstructure[[1]]$datatable[i,2]=mean(x=eval(uadatasets$temppairs[i]),na.rm="TRUE")
		
		#If there is a single case, we cannot calculate standard dev and standard error. We display appropriate messages in the footer
		#cat("\n uaonesmdesc3")
		if (BSkynoofcases==1)
		{
			#cat("\n uaonesmdesc3.1")
			BSkyfootervar =names(uadatasets$lst[[index]][uavarindex[i]])
			BSkyfootermsgdis =sprintf("Std. deviation cannot be calculated as variable %s has only 1 valid case",BSkyfootervar)
			uadatasets$retstructure[[1]]$metadatatable[[1]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[1]], data.frame(varIndex=i,type=2,varName=BSkyfootervar,dataTableRow=i,startCol=3,endCol=3,BSkyMsg=BSkyfootermsgdis,RMsg=""))
			BSkyfootermsgdis =sprintf("Std. error cannot be calculated as variable %s has only 1 valid case",BSkyfootervar)
			uadatasets$retstructure[[1]]$metadatatable[[1]] =rbind(uadatasets$retstructure[[1]]$metadatatable[[1]], data.frame(varIndex=i,type=2,varName=BSkyfootervar,dataTableRow=i,startCol=4,endCol=4,BSkyMsg=BSkyfootermsgdis,RMsg=""))
			#cat("\n uaonesmdesc3.2")
		}
		else
		{
			#cat("\n uaonesmdesc4")
			#print(uadatasets$temppairs[i])
			
			#cat("\n---\n")
			#print(eval(uadatasets$temppairs[i]))
			#cat("\n+++\n")
			uadatasets$retstructure[[1]]$datatable[i,3]=sd(eval(uadatasets$temppairs[i]),na.rm="TRUE")
			#print(uadatasets$retstructure[[1]]$datatable[i,3])
			#cat("\n uaonesmdesc4")
			#we could optimize the std error to use the std dev above
			uadatasets$retstructure[[1]]$datatable[i,4]=sqrt((var(eval(uadatasets$temppairs[i]),na.rm="TRUE")/uadatasets$retstructure[[1]]$datatable[i,2]))
			#cat("\n uaonesmdesc4")
		}
	}
}
		#cat("\n Aaron's Exit uaonesmdesc:\n")
		#print(uadatasets$retstructure)
		#cat("\n############Exit###############\n")	
  # every ua sub function must call the following function to write the log
    BSkyFunctionWrapUp()
	return(TRUE)
}



uagetcorr <-function(cindex,uavarindex,len1,uadatasets,index)
{
	uamat =matrix(nrow=len1/2,ncol=3)
	uamatdis=matrix(nrow=0,ncol=6)
	#i indexes the pairs of variables and is incremented by 2
	i=1
	#k indexes the statistics for a pair of variables in uamat
	k=1
	while (i<len1)
		{
		j=i+1
		uamat[k,1]<-length(eval(uadatasets$temppairs[i]))
		uamat[k,2]<-cor(eval(uadatasets$temppairs[i]),eval(uadatasets$temppairs[i+1]))
		if(uaperformance==2)
			{
			uastartlog("cor.test","uapairedsmt.test")	
			}

		tryCatch(
				{
		withCallingHandlers(
							{
							temp <-cor.test(eval(uadatasets$temppairs[i]),eval(uadatasets$temppairs[i+1]),method = c("pearson"),conf.level =0.95)
							},
						warning = function(ex) 
							{
							if(is.null(ex)) {ex <- warnings();}
							uadatasets$warning =-1;
							uadatasets$uawarnfn=-1
							uawarnvar =names(uadatasets$lst[[index]][uavarindex[i]:uavarindex[i+1]])
							uarwarnmsg =conditionMessage(ex)
							uawarnmsgdis =sprintf("Correlation test on variables %s generated a warning",uawarnvar)
							uadatasets$uawarnvar =c(uadatasets$uawarnvar,uawarnvar)
							uadatasets$uawarnmsgdis =c(uadatasets$uawarnmsgdis,uawarnmsgdis)
							uadatasets$uarwarnmsg =c(uadatasets$warnmsg,uawarnmsg );
							uawritelog(type="Warning", functionName="cor.test", uberFunct="uapairedsm.test",RMessage =uarwarnmsg,BSkyMessage =uadatasets$warnmsgdis, );
							uadatasets$warnindex=uadatasets$warnindex+1;
							invokeRestart("muffleWarning");
							}
						  ) # end of withCallingHandlers for catching warnings and continuing execution
				},

				error = function(ex) 
		        {
					uadatasets$error =-1;
					uadatasets$errorindex =uadatasets$errorindex +1
					uadatasets$errorfn=-1
					uaerrvar =names(uadatasets$lst[[index]][uavarindex[i]:uavarindex[i+1]])
					uaerrmsgdis =sprintf("Correlation test on variables %s failed",uaerrvar) 		
					uarerrmsg=conditionMessage(ex)
					uawritelog(type="Error", functionName="cor.test",uberFunct="uapairedsm.test",RMessage =uarerrmsg,BSkyMessage=uaerrmsgdis);
					#1st position is the index of the variable in uavarindex
					#2nd position is whether everything is OK (0), its an error(-1) or a warning (1)
					#3rd position is the variable name 
					#4th position is the variable name
					#5th position is the warning message that should be displayed
					#6th position is the R warning message
					uadatasets$results=c(as.character(m),"-1",uaerrvar,uaerrmsgdis, uarerrmsg)
				},
				silent =TRUE)
		
		if(uaperformance==2)
		{
		ualogcommand()	
		}
		if( uadatasets$errorfn ==-1)
		{
			uamatdis =rbind(uamatdis,uadatasets$results)
			# As the cor test for the variable failed, the entries in the uamat for the cor test should be NA
			# we need to advance the uamatdis to the next variable
			#m=m+1
			k=k+1
		}	
		if(uadatasets$uawarnfn ==-1)
		{
			len1 =length(uadatasets$uawarnvar)
			n=1
			o=1
			while (n < len1)
			{
				#1st position is the index of the variable in uavarindex
				#2nd position is whether everything is OK (0), its an error(-1) or a warning (1)
				#3rd position is the variable name 
				#4th position is the variable name
				#5th position is the warning message that should be displayed
				#6th position is the R warning message
				uamatdis= rbind(uamatdis, c(as.character(k),"1",uadatasets$uawarnvar[n:n+1],uadatasets$uawarnmsgdis[o],uadatasets$uarwarnmsg[o]))
				n=n+2
				o=o+1
			}	
			uadatasets$uawarnvar=NULL
			uadatasets$uawarnmsgdis=NULL
			uadatasets$uarwarnmsg=NULL
		}
		if (uadatasets$errorfn != -1)
		{			
				uamat[k,3]<-temp$p.value
				uamatdis =rbind(uamatdis,c(as.character(k), "0",NA,NA,NA,NA))
				k<-k+1
		}
		uadatasets$errorfn=0
		uadatasets$uawarnfn=0
		i=i+2
	} # end of while
	list(uamat,uamatdis)
}


#uamatdis helps direct what data from uamat should be displayed and also gives us the errors and warnings associated with
#every variable in uamat
#It must accomodate the fact that several rows in uamat contain info for a single variable, e.g. a Binomial test on a single variable
#produces 3 rows, in one sample t.test, a single row in uamat represents a single variable, in paired sample t.test a single row represents
# 2 variables
#It must accomodate the fact that a single row can be associated with more than 1 error and warning message
# in onesample t.test a single row in uamat is the result of one t.test
# in paired sample t.test, the correlation table, one row corresponds to 2 tests, a cor and a cor.test
#uamat should tell us whether to display the row or not
#The following is returned by uamatdis
#as.character(m) -> The row in uamat
#"-1" Don't display the row
#"0" All is well
#"1" display the row, but there are errors and warnings
#Whether there is an error or warning
#uaerrvar The variables generating the error or uawarnvar, the variables generating the warning
#The error/warning message that should be displayed
#The R eror/warning message



uapasmttest <-function(cindex,uavarindex, len,ua.opt1pass ="two.sided", ua.cipass=.95, ua.tyoftestpass =TRUE, index)
{
i=1
# J is used for indexing temppairs
j=1
# k is used for indexing the matrix that is returned
k=1
#m is used as an index into uamatdis
m=1
#n is used for indexing the warning variables
n=1
uamat =matrix(nrow=len/2,ncol=8)
uamatdis=matrix(nrow=0,ncol=6)
while (i <=len)
{
  #j is used for the indirection within temppairs
	j=i+1
  if (!is.na(uavarindex[i]) & !is.na(uavarindex[i+1]))
  { 
	if(uaperformance==2)
			{
			uastartlog("t.test","uapairedsmt.test")	
			}
	
	tryCatch(
	{
		#NOTE: THE CODE HANDLES THE FACT THAT THERE CAN BE AN ERROR AND ONE OR MORE WARNINGS
		#ON RUNNING A ONE SAMPLE T.TEST ON A SINGLE VARIABLE. WE ALWAYS RETURN THE ERROR FIRST EVEN THOUGH THE WARNING OCCURED FIRST
		withCallingHandlers(
				{
				uatemp <-t.test(eval(uadatasets$temppairs[i]),eval(uadatasets$temppairs[i+1]) , alternative=ua.opt1pass, conf.level=ua.cipass, paired=ua.tyoftestpass)
				},
			        warning = function(ex) 
				{
				if(is.null(ex)) {ex <- warnings();}
				uadatasets$warning =-1;
				uadatasets$uawarnfn=-1
				uawarnvar =names(uadatasets$lst[[index]][uavarindex[i]:uavarindex[i+1]])
				uarwarnmsg =conditionMessage(ex)
				uawarnmsgdis =sprintf("Paired T test on variables %s generated a warning",uawarnvar)
				uadatasets$uawarnvar =c(uadatasets$uawarnvar,uawarnvar)
				uadatasets$uawarnmsgdis =c(uadatasets$uawarnmsgdis,uawarnmsgdis)
				uadatasets$uarwarnmsg =c(uadatasets$uarwarnmsg,uarwarnmsg );
				uawritelog(type="Warning", functionName="t.test", uberFunct="uapairedsm.test",RMessage =uarwarnmsg,BSkyMessage =uadatasets$warnmsgdis, );
				uadatasets$warnindex=uadatasets$warnindex+1;
				invokeRestart("muffleWarning");
				}
			         ) # end of withCallingHandlers for catching warnings and continuing execution
	},

	error = function(ex) 
		        {
		        uadatasets$error =-1;
				uadatasets$errorindex =uadatasets$errorindex +1
				uadatasets$errorfn=-1
				uaerrvar =names(uadatasets$lst[[index]][uavarindex[i]:uavarindex[i+1]])
				uaerrmsgdis =sprintf("Paired sample T.test variables %s failed",uaerrvar) 		
				uarerrmsg=conditionMessage(ex)
		        uawritelog(type="Error", functionName="t.test",uberFunct="uapairedsm.test",RMessage =uarerrmsg,BSkyMessage=uaerrmsgdis);
				uadatasets$results=c(as.character(m),"-1",uaerrvar,uaerrmsgdis, uarerrmsg)
				},
	silent =TRUE)
	
	if(uaperformance==2)
	{
	ualogcommand()	
	}
	
	if( uadatasets$errorfn ==-1)
			{
				uamatdis =rbind(uamatdis,uadatasets$results)
				# As the t test for the variable failed, the entries in the uamat for that variable should be NA
				# we need to advance the uamatdis to the next variable
				m=m+1
				k=k+1
			}	
	if(uadatasets$uawarnfn ==-1)
			{
				len1 =length(uadatasets$uawarnvar)
				n=1
				o=1
				while (n < len1)
				{
					#1st position is the index of the variable in uavarindex
					#2nd position is whether everything is OK (1), its an error(-1) or warning(-2)
					#3rd position is the 1st variable name
					#4th position is the 2nd variable name
					#5th position is the warning message that should be displayed
					#6th position is the R warning message
					uamatdis= rbind(uamatdis, c(as.character(m),"1",uadatasets$uawarnvar[n:n+1],uadatasets$uawarnmsgdis[o],uadatasets$uarwarnmsg[o]))
					n=n+2
					o=o+1
				}	
				uadatasets$uawarnvar=NULL
				uadatasets$uawarnmsgdis=NULL
				uadatasets$uarwarnmsg=NULL
			}
	if (uadatasets$errorfn != -1)
			{			
				uamat[k,1] <-uatemp$estimate
				uamat[k,2] <-uatemp$standarddev
				uamat[k,3] <-uatemp$standarderr
				uamat[k,4] <-uatemp$conf.int[1]
				uamat[k,5] <-uatemp$conf.int[2]
				uamat[k,6] <-uatemp$statistic
				uamat[k,7] <-uatemp$parameter
				uamat[k,8] <-uatemp$p.value
				uamatdis =rbind(uamatdis,c(as.character(m), "0",NA,NA,NA,NA))
				k<-k+1
				m<-m+1
			}
			uadatasets$errorfn=0
			uadatasets$warnfn=0
		}#End of if
	
	i=i+2
	} # end of while
	return(list(uamat,uamatdis))
}
	

#I made very very minor modifications to the way t.test in R returns values
#I saved the changes to a new function

uarettest <- function(x, y = NULL, alternative = c("two.sided", "less", "greater"), 
    mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95, 
    ...) 
{
    alternative <- match.arg(alternative)
    if (!missing(mu) && (length(mu) != 1 || is.na(mu))) 
        stop("'mu' must be a single number")
    if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) || 
        conf.level < 0 || conf.level > 1)) 
        stop("'conf.level' must be a single number between 0 and 1")
    if (!is.null(y)) {
        dname <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
        if (paired) 
            xok <- yok <- complete.cases(x, y)
        else {
            yok <- !is.na(y)
            xok <- !is.na(x)
        }
        y <- y[yok]
    }
    else {
        dname <- deparse(substitute(x))
        if (paired) 
            stop("'y' is missing for paired test")
        xok <- !is.na(x)
        yok <- NULL
    }
    x <- x[xok]
    if (paired) {
        x <- x - y
        y <- NULL
    }
    nx <- length(x)
    mx <- mean(x)
    vx <- var(x)
    estimate <- mx
    if (is.null(y)) {
        if (nx < 2) 
            stop("not enough 'x' observations")
        df <- nx - 1
        stderr <- sqrt(vx/nx)
        if (stderr < 10 * .Machine$double.eps * abs(mx)) 
            stop("data are essentially constant")
        tstat <- (mx - mu)/stderr
        method <- ifelse(paired, "Paired t-test", "One Sample t-test")
        names(estimate) <- ifelse(paired, "mean of the differences", 
            "mean of x")
    }
    else {
        ny <- length(y)
        if (nx < 1 || (!var.equal && nx < 2)) 
            stop("not enough 'x' observations")
        if (ny < 1 || (!var.equal && ny < 2)) 
            stop("not enough 'y' observations")
        if (var.equal && nx + ny < 3) 
            stop("not enough observations")
        my <- mean(y)
        vy <- var(y)
        method <- paste(if (!var.equal) 
            "Welch", "Two Sample t-test")
        estimate <- c(mx, my)
        names(estimate) <- c("mean of x", "mean of y")
        if (var.equal) {
            df <- nx + ny - 2
            v <- 0
            if (nx > 1) 
                v <- v + (nx - 1) * vx
            if (ny > 1) 
                v <- v + (ny - 1) * vy
            v <- v/df
            stderr <- sqrt(v * (1/nx + 1/ny))
        }
        else {
            stderrx <- sqrt(vx/nx)
            stderry <- sqrt(vy/ny)
            stderr <- sqrt(stderrx^2 + stderry^2)
            df <- stderr^4/(stderrx^4/(nx - 1) + stderry^4/(ny - 
                1))
        }
        if (stderr < 10 * .Machine$double.eps * max(abs(mx), 
            abs(my))) 
            stop("data are essentially constant")
        tstat <- (mx - my - mu)/stderr
    }
    if (alternative == "less") {
        pval <- pt(tstat, df)
        cint <- c(-Inf, tstat + qt(conf.level, df))
    }
    else if (alternative == "greater") {
        pval <- pt(tstat, df, lower.tail = FALSE)
        cint <- c(tstat - qt(conf.level, df), Inf)
    }
    else {
        pval <- 2 * pt(-abs(tstat), df)
        alpha <- 1 - conf.level
        cint <- qt(1 - alpha/2, df)
        cint <- tstat + c(-cint, cint)
    }
    cint <- mu + cint * stderr
    names(tstat) <- "t"
    names(df) <- "df"
    names(mu) <- if (paired || !is.null(y)) 
        "difference in means"
    else "mean"
    attr(cint, "conf.level") <- conf.level
    rval <- list(statistic = tstat, parameter = df, p.value = pval, 
        conf.int = cint, estimate = estimate, null.value = mu, 
        alternative = alternative, method = method, data.name = dname,  
	standarderr=stderr,variationx=vx, standarddev=sd(x))
    class(rval) <- "ahtest"
    return(rval)
}


levenemod.test <-function (y, group, location = c("median", "mean", "trim.mean"), 
    trim.alpha = 0.25, bootstrap = FALSE, num.bootstrap = 1000, 
    kruskal.test = FALSE, correction.method = c("none", "correction.factor", 
        "zero.removal", "zero.correction")) 
{
    if (length(y) != length(group)) {
        stop("the length of the data (y) does not match the length of the group")
    }
    location <- match.arg(location)
    correction.method <- match.arg(correction.method)
    DNAME = deparse(substitute(y))
    group <- group[!is.na(y)]
	y <- y[!is.na(y)]
    if ((location == "trim.mean") & (trim.alpha == 1)) {
        stop("trim.alpha value of 0 to 0.5 should be provided for the trim.mean location")
    }
    reorder <- order(group)
    group <- group[reorder]
    y <- y[reorder]
    gr <- group
    group <- as.factor(group)
    if (location == "mean") {
        means <- tapply(y, group, mean)
        METHOD <- "classical Levene's test based on the absolute deviations from the mean"
    }
    else if (location == "median") {
        means <- tapply(y, group, median)
        METHOD = "modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median"
    }
    else {
        location = "trim.mean"
        trimmed.mean <- function(y) mean(y, trim = trim.alpha)
        means <- tapply(y, group, trimmed.mean)
        METHOD <- "modified robust Levene-type test based on the absolute deviations from the trimmed mean"
    }
    n <- tapply(y, group, length)
    resp.mean <- abs(y - means[group])
    ngroup <- n[group]
    if (location != "median" && correction.method != "correction.factor") {
        METHOD <- paste(METHOD, "(", correction.method, "not applied because the location is not set to median", 
            ")")
        correction.method <- "none"
    }
    if (correction.method == "correction.factor") {
        METHOD <- paste(METHOD, "with correction factor")
        correction <- sqrt(ngroup/(ngroup - 1))
        resp.mean <- correction * resp.mean
    }
    if (correction.method == "zero.removal" || correction.method == 
        "zero.correction") {
        if (correction.method == "zero.removal") {
            METHOD <- paste(METHOD, "with Hines-Hines structural zero removal method")
        }
        if (correction.method == "zero.correction") {
            METHOD <- paste(METHOD, "with modified structural zero removal method and correction factor")
        }
        resp.mean <- y - means[group]
        k <- length(n)
        temp <- double()
        endpos <- double()
        startpos <- double()
        for (i in 1:k) {
            group.size <- n[i]
            j <- i - 1
            if (i == 1) 
                start <- 1
            else start <- sum(n[1:j]) + 1
            startpos <- c(startpos, start)
            end <- sum(n[1:i])
            endpos <- c(endpos, end)
            sub.resp.mean <- resp.mean[start:end]
            sub.resp.mean <- sub.resp.mean[order(sub.resp.mean)]
            if (group.size%%2 == 1) {
                mid <- (group.size + 1)/2
                temp2 <- sub.resp.mean[-mid]
                if (correction.method == "zero.correction") {
                  ntemp <- length(temp2) + 1
                  correction <- sqrt((ntemp - 1)/ntemp)
                  temp2 <- correction * temp2
                }
            }
            if (group.size%%2 == 0) {
                mid <- group.size/2
                if (correction.method == "zero.removal") {
                  denom <- sqrt(2)
                }
                else {
                  denom <- 1
                }
                replace1 <- (sub.resp.mean[mid + 1] - sub.resp.mean[mid])/denom
                temp2 <- sub.resp.mean[c(-mid, -mid - 1)]
                temp2 <- c(temp2, replace1)
                if (correction.method == "zero.correction") {
                  ntemp <- length(temp2) + 1
                  correction <- sqrt((ntemp - 1)/ntemp)
                  temp2 <- correction * temp2
                }
            }
            temp <- c(temp, temp2)
        }
        resp.mean <- abs(temp)
        zero.removal.group <- group[-endpos]
    }
    else {
        correction.method = "none"
    }
    if (correction.method == "zero.removal" || correction.method == 
        "zero.correction") {
        d <- zero.removal.group
    }
    else {
        d <- group
    }
    if (kruskal.test == FALSE) {
        statistic <- anova(lm(resp.mean ~ d))[1, 4]
        p.value <- anova(lm(resp.mean ~ d))[1, 5]
    }
    else {
        METHOD <- paste("rank-based (Kruskal-Wallis)", METHOD)
        ktest <- kruskal.test(resp.mean, d)
        statistic <- ktest$statistic
        p.value = ktest$p.value
    }
    non.bootstrap.p.value <- p.value
    if (bootstrap == TRUE) {
        METHOD <- paste("bootstrap", METHOD)
        R <- 0
        N <- length(y)
        frac.trim.alpha <- 0.2
        b.trimmed.mean <- function(y) {
            nn <- length(y)
            wt <- rep(0, nn)
            y2 <- y[order(y)]
            lower <- ceiling(nn * frac.trim.alpha) + 1
            upper <- floor(nn * (1 - frac.trim.alpha))
            if (lower > upper) 
                stop("frac.trim.alpha value is too large")
            m <- upper - lower + 1
            frac <- (nn * (1 - 2 * frac.trim.alpha) - m)/2
            wt[lower - 1] <- frac
            wt[upper + 1] <- frac
            wt[lower:upper] <- 1
            return(weighted.mean(y2, wt))
        }
        b.trim.means <- tapply(y, group, b.trimmed.mean)
        rm <- y - b.trim.means[group]
        for (j in 1:num.bootstrap) {
            sam <- sample(rm, replace = TRUE)
            boot.sample <- sam
            if (min(n) < 10) {
                U <- runif(1) - 0.5
                means <- tapply(y, group, mean)
                v <- sqrt(sum((y - means[group])^2)/N)
                boot.sample <- ((12/13)^(0.5)) * (sam + v * U)
            }
            if (location == "mean") {
                boot.means <- tapply(boot.sample, group, mean)
            }
            else if (location == "median") {
                boot.means <- tapply(boot.sample, group, median)
            }
            else {
                location = "trim.mean"
                trimmed.mean.2 <- function(boot.sample) mean(boot.sample, 
                  trim = trim.alpha)
                boot.means <- tapply(boot.sample, group, trimmed.mean.2)
            }
            resp.boot.mean <- abs(boot.sample - boot.means[group])
            if (correction.method == "correction.factor") {
                correction <- sqrt(ngroup/(ngroup - 1))
                resp.mean <- correction * resp.boot.mean
            }
            if (correction.method == "zero.removal" || correction.method == 
                "zero.correction") {
                resp.mean <- boot.sample - boot.means[group]
                k <- length(n)
                temp <- double()
                endpos <- double()
                startpos <- double()
                for (i in 1:k) {
                  group.size <- n[i]
                  j <- i - 1
                  if (i == 1) 
                    start <- 1
                  else start <- sum(n[1:j]) + 1
                  startpos <- c(startpos, start)
                  end <- sum(n[1:i])
                  endpos <- c(endpos, end)
                  sub.resp.mean <- resp.mean[start:end]
                  sub.resp.mean <- sub.resp.mean[order(sub.resp.mean)]
                  if (group.size%%2 == 1) {
                    mid <- (group.size + 1)/2
                    temp2 <- sub.resp.mean[-mid]
                    if (correction.method == "zero.correction") {
                      ntemp <- length(temp2) + 1
                      correction <- sqrt((ntemp - 1)/ntemp)
                      temp2 <- correction * temp2
                    }
                  }
                  if (group.size%%2 == 0) {
                    mid <- group.size/2
                    if (correction.method == "zero.removal") {
                      denom <- sqrt(2)
                    }
                    else {
                      denom <- 1
                    }
                    replace1 <- (sub.resp.mean[mid + 1] - sub.resp.mean[mid])/denom
                    temp2 <- sub.resp.mean[c(-mid, -mid - 1)]
                    temp2 <- c(temp2, replace1)
                    if (correction.method == "zero.correction") {
                      ntemp <- length(temp2) + 1
                      correction <- sqrt((ntemp - 1)/ntemp)
                      temp2 <- correction * temp2
                    }
                  }
                  temp <- c(temp, temp2)
                }
                resp.boot.mean <- abs(temp)
                zero.removal.group <- group[-endpos]
            }
            if (correction.method == "zero.removal" || correction.method == 
                "zero.correction") {
                d <- zero.removal.group
            }
            else {
                d <- group
            }
            if (kruskal.test == FALSE) {
                statistic2 = anova(lm(resp.boot.mean ~ d))[1, 
                  4]
            }
            else {
                bktest <- kruskal.test(resp.boot.mean, d)
                statistic2 <- bktest$statistic
            }
            if (statistic2 > statistic) 
                R <- R + 1
        }
        p.value <- R/num.bootstrap
    }
    STATISTIC = statistic
    names(STATISTIC) = "Test Statistic"
    structure(list(statistic = STATISTIC, p.value = p.value, 
        method = METHOD, data.name = DNAME, non.bootstrap.p.value = non.bootstrap.p.value), 
        class = "htest")
}




