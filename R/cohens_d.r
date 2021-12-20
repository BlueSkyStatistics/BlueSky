
cohenTest <-function()
{
print("you are here")
}



cohensd <-function(cindex,uavarindex, len,correction=cohensd_correction, uacipass=.95, index,valuetocompare,indexInReturnStructure)
{
	# every ua sub function must call the following two 
    # initislization call at the begining of the function as follows 
   BSkyFunctionInit()

	#uadatasets$retstructure[[1]]$metadatatable=list(BSkyonesmdesc[[2]])
	uadatasets$retstructure[[indexInReturnStructure]]$datatable=matrix(nrow=len,ncol=4)
	i=1
	j=1
	#warning ("Valerie");

while (i <=len)
{
  if (!is.na(uavarindex[i]))
  { 
	if(uaperformance==2)
	{
	uastartlog("t.test","cohensd")	
	#ostarttime =date()
	#starttime=proc.time()
	#initialmem=gc()
	}
	uavar =names(uadatasets$lst[[index]][uavarindex[i]])
	uadatasets$uawarnmsgdis =sprintf("Cohen's d on variable %s generated a warning",uavar)
	uadatasets$uaerrmsgdis =sprintf("Cohen's d on variable %s generated an error",uavar)		
	tryCatch(
	{
		#NOTE: THE CODE HANDLES THE FACT THAT THERE CAN BE AN ERROR AND ONE OR MORE WARNINGS
		#ON RUNNING A ONE SAMPLE T.TEST ON A SINGLE VARIABLE. WE ALWAYS RETURN THE ERROR FIRST EVEN THOUGH THE WARNING OCCURED FIRST
		withCallingHandlers(
				{
				
				
				uatemp<-cohens_d(eval(uadatasets$temppairs[i])~1, mu=valuetocompare, correction =correction, ci = uacipass)
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
				uadatasets$retstructure[[indexInReturnStructure]]$metadatatable[[1]] =rbind(uadatasets$retstructure[[indexInReturnStructure]]$metadatatable[[1]],data.frame(varIndex=i,type=-1,varName=uavar,dataTableRow=i,startCol=1,endCol=1,BSkyMsg=uadatasets$uaerrmsgdis, RMsg=uadatasets$uarerrmsg))
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
					uadatasets$retstructure[[indexInReturnStructure]]$metadatatable[[1]] = rbind(uadatasets$retstructure[[indexInReturnStructure]]$metadatatable[[1]] , data.frame(varIndex=i,type=1,varName=uavar,dataTableRow=i,startCol=1,endCol=4,BSkyMsg=uadatasets$uawarnmsgdis,RMsg=uadatasets$uarwarnmsg[k]))
				}	
				uadatasets$uawarnvar=NULL
				uadatasets$uawarnmsgdis=NULL
				uadatasets$uarwarnmsg=NULL
			}
	if (uadatasets$errorfn != -1)
			{			
				uadatasets$errorfn =0
				uadatasets$warning=0
				uadatasets$retstructure[[indexInReturnStructure]]$datatable[j,1] <-uatemp$Cohens_d
				uadatasets$retstructure[[indexInReturnStructure]]$datatable[j,2] <-uatemp$CI
				uadatasets$retstructure[[indexInReturnStructure]]$datatable[j,3] <-uatemp$CI_low
				uadatasets$retstructure[[indexInReturnStructure]]$datatable[j,4]<-uatemp$CI_high
				
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





cohensdIndSmTTest <-function (cindex, uavarindex, groupindex, noofvars,  correction,
    uacipass, index, indexInReturnStructure) 
{
    BSkyFunctionInit()
    i = 1
    j = 1
    p = 1
	uatemp=NULL
    uadatasets$retstructure[[indexInReturnStructure]]$datatable = matrix(nrow = noofvars, ncol = 4)
   
    while (i <= noofvars) 
	{
		uavar = names(uadatasets$lst[[index]][uavarindex[i]])
        uadatasets$uawarnmsgdis = sprintf("Cohen's d on variable '%s' generated a warning", uavar)
        uadatasets$uaerrmsgdis = sprintf("Cohen's d on variable '%s' generated an error", uavar)
        if (!is.na(uavarindex[i])) 
		{
            if (uaperformance == 2) 
			{
                 uastartlog("t.test", "cohensdIndSmTTest")
            }
				tryCatch(
				{
                  withCallingHandlers({
				  
					temp1 = !is.na(eval(uadatasets$temppairs[p]))
                    temp2 = eval(uadatasets$temppairs[p + 1])
                    uatempcounts <- table(temp2[temp1])
                    if (uatempcounts[1] <= 1 || uatempcounts[2] <= 1) 
					{
						BSkywarnmsgdis = sprintf("Warning: Cohen's d for variable '%s' cannot be computed as there are too few cases for the t.test to run", 
							  names(uadatasets$lst[[index]][uavarindex[i]]))
						uadatasets$warnindex = uadatasets$warnindex +  1
						uadatasets$retstructure[[indexInReturnStructure]]$metadatatable[[1]] = rbind(uadatasets$retstructure[[indexInReturnStructure]]$metadatatable[[1]], 
							  data.frame(varIndex = i, type = -1, 
								varName = names(uadatasets$lst[[index]][uavarindex[i]]), 
								dataTableRow = i, startCol = 2, endCol = 2	, 
								BSkyMsg = BSkywarnmsgdis, RMsg = ""))
							uawritelog(type = "Warning", functionName = "cohensdIndSmTTest", 
							  BSkyMessage = BSkywarnmsgdis)
							uatemp$Cohens_d =NA
							uatemp$CI = NA
							uatemp$CI_low = NA
							uatemp$CI_high =NA
                      }
                      else 
					  {
                        uatemp <- cohens_d(eval(uadatasets$temppairs[p]) ~ 
                          eval(uadatasets$temppairs[p + 1]), 
                            correction =correction, ci = uacipass)
                      }
                    
                  }
				  , warning = UAwarnHandlerFn)
                }, error = UAerrHandlerFn, silent = TRUE)
                if (uaperformance == 2) 
				{
                  ualogcommand()
                }
                if (uadatasets$errorfn == -1) 
				{
                  uadatasets$retstructure[[indexInReturnStructure]]$metadatatable[[1]] = rbind(uadatasets$retstructure[[indexInReturnStructure]]$metadatatable[[1]], 
                        data.frame(varIndex = i, type = -1, varName = uavar, 
                        dataTableRow = (i ), startCol = 2, 
                        endCol = 2, BSkyMsg = uadatasets$uaerrmsgdis, 
                        RMsg = uadatasets$uarerrmsg))
                  j = j + 1
                }
                if (uadatasets$uawarnfn == -1) 
				{
                  len1 = length(uadatasets$uawarnvar)
                  k = 1
                  for (k in 1:len1) {
                    uadatasets$retstructure[[indexInReturnStructure]]$metadatatable[[1]] = rbind(uadatasets$retstructure[[indexInReturnStructure]]$metadatatable[[1]], 
                      data.frame(varIndex = i, type = 1, varName = uavar, 
                        dataTableRow = (i ), startCol = 2, 
                        endCol = 4, BSkyMsg = uadatasets$uawarnmsgdis, 
                        RMsg = uadatasets$uarwarnmsg[k]))
                  }
                  uadatasets$uawarnvar = NULL
                  uadatasets$uawarnmsgdis = NULL
                  uadatasets$uarwarnmsg = NULL
                }
                if (uadatasets$errorfn != -1) 
				{
                    uadatasets$errorfn = 0
                    uadatasets$warning = 0
					uadatasets$retstructure[[indexInReturnStructure]]$datatable[j,1] <-uatemp$Cohens_d
					uadatasets$retstructure[[indexInReturnStructure]]$datatable[j,2] <-uatemp$CI
					uadatasets$retstructure[[indexInReturnStructure]]$datatable[j,3] <-uatemp$CI_low
					uadatasets$retstructure[[indexInReturnStructure]]$datatable[j,4]<-uatemp$CI_high                
					j <- j + 1
                }
				
			uadatasets$errorfn = 0
			uadatasets$uawarnfn = 0
                
		}
       i = i + 1
     p = p + 2 
            
    }
    
      BSkyFunctionWrapUp()
    return(list(uadatasets$retstructure[[2]]$datatable, uadatasets$retstructure[[2]]$metadatatable[[1]]))
   
}

