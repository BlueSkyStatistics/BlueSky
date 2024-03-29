##test cases for MANOVA repeated measures
	# https://www.rdocumentation.org/packages/car/versions/1.2-4/topics/Anova
# library(afex)
# library(carData)
# library(car)
# phase <- factor(rep(c("pretest", "posttest", "followup"), c(5, 5, 5)),
# levels=c("pretest", "posttest", "followup"))
# hour <- ordered(rep(1:5, 3))
# idata <- data.frame(phase, hour)
# idata

# mod.ok <- lm(cbind(pre.1, pre.2, pre.3, pre.4, pre.5, 
                     # post.1, post.2, post.3, post.4, post.5, 
                     # fup.1, fup.2, fup.3, fup.4, fup.5) ~  treatment*gender, 
                # data=OBrienKaiser)
# (av.ok <- Anova(mod.ok, idata=idata, idesign=~phase*hour)) 
# test <-summary(av.ok, multivariate=TRUE)
  # best<-  BSkyFormatsummary.Anova.mlm(test)
                           # length(best)
                           # names(best)
                           # BSkyFormat(best)
                           # test <-summary(av.ok, multivariate=FALSE)
	
#For class = summary.Anova.mlm	
	BSkyFormatsummary.Anova.mlm  <- function(obj)
	{
	table_list =NULL
	table_list_names =NULL
	 if("summary.Anova.mlm" %in% class(obj))
	 {
		SSP = TRUE
		SSPE = SSP
		if (!is.null(obj$multivariate.tests)) 
		{
		   
		   tableHeader = paste("Type ", obj$type, if (obj$repeated)
				" Repeated Measures", " MANOVA Tests:\n", sep = "")
			if ((!obj$repeated) && SSPE) {
				#cat("\nSum of squares and products for error:\n")
				#print(obj$SSPE, digits = digits, ...)
				
				class(obj$SSPE ) <-"matrix"
				
				header = "Sum of squares and products for error:"
				if (is.null(table_list))
				{
					table_list = list(obj$SSPE)
					table_list_names <- header
				}
				else {
				table_list = c(table_list, list(obj$SSPE))
				table_list_names <- c(table_list_names,header)
				}

			}
		    for (term in 1:length(obj$multivariate.tests)) {
				# cat(paste("\n------------------------------------------\n",
					# "\nTerm:", names(obj$multivariate.tests)[term],
					# "\n"))
				
				header = paste("Term:", names(obj$multivariate.tests)[term])
				contents = data.frame(Message = "Review statistics for the above mentioned term below")
				
				if (is.null(table_list))
				{
					table_list = list(contents)
					table_list_names <- header
				}
				else {
				table_list = c(table_list, list(contents))
				table_list_names <- c(table_list_names,header)
				}
				
				
				#print(x$multivariate.tests[[term]], digits = digits,
				#	SSP = SSP, SSPE = FALSE, ...)
				resultsLinearHypothesis <-BSkyprint.linearHypothesis.mlm(obj$multivariate.tests[[term]], digits = digits,
					SSP = SSP, SSPE = FALSE)
				noOfResults = length(resultsLinearHypothesis)
				for(j in 1:noOfResults)
				{
					if (is.null(table_list))
					{
						table_list <-resultsLinearHypothesis[j]
						table_list_names <-names(resultsLinearHypothesis[j])
					} else
					{
						table_list <-c(table_list, resultsLinearHypothesis[j])
						table_list_names <-c(table_list_names, names(resultsLinearHypothesis[j]))				
					}
				}
				
		   }
		}
		if (!is.null(obj$univariate.tests))
	{
			tableHeader1 <-paste("Univariate Type ", obj$type, " Repeated-Measures ANOVA Assuming Sphericity", sep="")
			
			class (obj$univariate.tests) <-"matrix"
			if (is.null(table_list))
			{
				table_list = list(obj$univariate.tests)
				table_list_names <- tableHeader1
			}
			else {
			table_list = c(table_list, list(obj$univariate.tests))
			table_list_names <- c(table_list_names,tableHeader1)
			}
			#print(obj$univariate.tests)
			if (nrow(obj$sphericity.tests) > 0) {
				tableHeader2 ="Mauchly Tests for Sphericity"
				table_list_names = c(table_list_names, tableHeader2)
				class (obj$sphericity.tests) <-"matrix"
				table_list = c(table_list, list(obj$sphericity.tests))
				
				#print(x$sphericity.tests)
				tableHeader3 <-"Greenhouse-Geisser Corrections for Departure from Sphericity"
				table_list_names = c(table_list_names, tableHeader3)
				table <- obj$pval.adjustments[, 1:2, drop = FALSE]
				class(table) <- "matrix"
				table_list = c(table_list, list(table))
				
				#print(table, ...)
				
				
			   # cat("\n")
				table <- obj$pval.adjustments[, 3:4, drop = FALSE]
				class(table) <- "matrix"
				tableHeader4 <-"Huynh-Feldt Corrections for Departure from Sphericity"
				table_list = c(table_list, list(table))
				table_list_names = c(table_list_names, tableHeader4)
				#print(table, ...)
				
			}
		}
		if (!is.null(obj$univaov))
		{
			#print(x$univaov, ...)
			if (is.null(table_list))
			{
				table_list = list(obj$univaov)
				tableHeader5 <-""
				table_list_names <- tableHeader5
			}
			else {
			table_list = c(table_list, list(obj$univaov))
			table_list_names <- c(table_list_names,tableHeader5)
			}
		
		}
		names(table_list) = table_list_names
		invisible(table_list)

	 }
	}