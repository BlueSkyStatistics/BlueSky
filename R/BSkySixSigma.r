#######################
#MSA Attribute Analysis
#######################
modified.kappam.fleiss <- function (ratings, exact = FALSE, detail = FALSE, levels =c())
{
    ratings <- as.matrix(na.omit(ratings))
    ns <- nrow(ratings)
    nr <- ncol(ratings)
    
	if(length(levels) > 0)
	{
		# this is added to modify the kappam.fleiss() to include an optional levels parameter
		# to pass the ordered factor list for the response and reference variables. Without this
		# factor levels are displayed in alphabetical orders in the kappam.fleiss output table
		
		lev = levels 
	}
	else
	{
		lev <- levels(as.factor(ratings))
	}
    
	for (i in 1:ns) {
        frow <- factor(ratings[i, ], levels = lev)
        if (i == 1)
            ttab <- as.numeric(table(frow))
        else ttab <- rbind(ttab, as.numeric(table(frow)))
    }
    ttab <- matrix(ttab, nrow = ns)
    agreeP <- sum((apply(ttab^2, 1, sum) - nr)/(nr * (nr - 1))/ns)
    if (!exact) {
        method <- "Fleiss' Kappa for m Raters"
        chanceP <- sum(apply(ttab, 2, sum)^2)/(ns * nr)^2
    }
    else {
        method <- "Fleiss' Kappa for m Raters (exact value)"
        for (i in 1:nr) {
            rcol <- factor(ratings[, i], levels = lev)
            if (i == 1)
                rtab <- as.numeric(table(rcol))
            else rtab <- rbind(rtab, as.numeric(table(rcol)))
        }
        rtab <- rtab/ns
        chanceP <- sum(apply(ttab, 2, sum)^2)/(ns * nr)^2 - sum(apply(rtab,
            2, var) * (nr - 1)/nr)/(nr - 1)
    }
    value <- (agreeP - chanceP)/(1 - chanceP)
    if (!exact) {
        pj <- apply(ttab, 2, sum)/(ns * nr)
        qj <- 1 - pj
        varkappa <- (2/(sum(pj * qj)^2 * (ns * nr * (nr - 1)))) *
            (sum(pj * qj)^2 - sum(pj * qj * (qj - pj)))
        SEkappa <- sqrt(varkappa)
        u <- value/SEkappa
        p.value <- 2 * (1 - pnorm(abs(u)))
        if (detail) {
            pj <- apply(ttab, 2, sum)/(ns * nr)
            pjk <- (apply(ttab^2, 2, sum) - ns * nr * pj)/(ns *
                nr * (nr - 1) * pj)
            kappaK <- (pjk - pj)/(1 - pj)
            varkappaK <- 2/(ns * nr * (nr - 1))
            SEkappaK <- sqrt(varkappaK)
            uK <- kappaK/SEkappaK
            p.valueK <- 2 * (1 - pnorm(abs(uK)))
            tableK <- as.table(round(cbind(kappaK, SEkappaK, uK, p.valueK),
                digits = 3))
            rownames(tableK) <- lev
            colnames(tableK) <- c("Kappa", "SE Kappa", "z", "p.value")
        }
    }
    if (!exact) {
        if (!detail) {
            rval <- list(method = method, subjects = ns, raters = nr,
                irr.name = "Kappa", value = value)
        }
        else {
            rval <- list(method = method, subjects = ns, raters = nr,
                irr.name = "Kappa", value = value, detail = tableK)
        }
        rval <- c(rval, stat.name = "z", statistic = u, p.value = p.value)
    }
    else {
        rval <- list(method = method, subjects = ns, raters = nr,
            irr.name = "Kappa", value = value)
    }
    class(rval) <- "irrlist"
    return(rval)
}

disagreementStat <- function(response_df)
{
	#print(response_df)
	
	first_level_matched = 0
	second_level_matched = 0
	
	#N is sample size
	N = dim(response_df)[1]
	
	level_reference = levels(response_df$Reference)

	#matched_rows = response_df[apply(response_df, 1, function(row) length(unique(row)) == 1),]
	
	#Rows with all matched
	
	apply(response_df, 1, 
		function(row) 
		{
			if((length(unique(row[1:(length(row)-1)])) == 1) && unique(row[1:(length(row)-1)]) == level_reference[1] && row[length(row)] != level_reference[1] )
			{
				first_level_matched <<- first_level_matched + 1
			}
			
			if((length(unique(row[1:(length(row)-1)])) == 1) && unique(row[1:(length(row)-1)]) == level_reference[2] && row[length(row)] != level_reference[2])
			{
				second_level_matched <<- second_level_matched + 1
			}
		} 
	)
	
	
	mixed_disageement_rows = response_df[apply(response_df, 1, function(row) length(unique(row[1:(length(row)-1)])) != 1),]

	if(!is.matrix(mixed_disageement_rows) && !is.data.frame(mixed_disageement_rows))
	{
		m = 0
		if(length(mixed_disageement_rows) > 0)
			m = 1
	}
	else
	{
		m = dim(mixed_disageement_rows)[1]
	}
	
	#print(c(first_level_matched, (first_level_matched/N)*100, second_level_matched, (second_level_matched/N)*100, m, (m/N)*100 ))															

	invisible(return(c(first_level_matched, (first_level_matched/N)*100, second_level_matched, (second_level_matched/N)*100, m, (m/N)*100 )))															

}

summaryDisagreement <- function(response_list, reference)
{
	reference = as.character(reference)
	samples = length(reference)
	
	#print(response_list)
	#print(reference)
	
	disagreement_df = data.frame(Sample = seq(1:samples), Standard = reference)
	
	lapply(response_list, 
		function(op_response)
		{
			all_count = c()
			all_percent = c()
			
			for(i in 1:samples)
			{
				count = length(which(op_response[i,] != reference[i]))
				percent = count/(length(op_response[i,]))*100
				all_count = c(all_count, count)
				all_percent = c(all_percent, percent)
			}
			
			disagreement_df <<- cbind(disagreement_df, all_count, all_percent)
		}
	)
	
	count_pct_columns = rep(c("Count", "Percent"), length(response_list))
	disagreement_df = rbind(c("Sample", "Standard", count_pct_columns), disagreement_df)
	 
	col_names = c(" ", " ", rep(names(response_list), each=2))
	dimnames(disagreement_df)[[2]] = col_names
	rownames(disagreement_df) = NULL 
	
	invisible(return(disagreement_df))
}

agreementCI <- function(response_df, alpha = 0.95)
{
	#Rows with all matched

	matched_rows = response_df[apply(response_df, 1, function(row) length(unique(row)) == 1),]

	if(!is.matrix(matched_rows) && !is.data.frame(matched_rows))
	{
		# m = dim(matrix(matched_rows, ncol = dim(response_df)[2]))[1]
		m = 1
	}
	else
	{
		m = dim(matched_rows)[1]
	}

		#N is sample size
		N = dim(response_df)[1]

		v1 = 2*m
		v2 = 2*(N - m + 1)

	if(m == N)
	{
		alphaL = (1-alpha)
		alphaU = alpha

		qfl = qf(alphaL, df1=v1, df2=v2)

		LL = (v1* qfl)/(v2 + v1*qfl)
	}
	else if(m !=0)
	{
		alphaL = (1-alpha)/2
		alphaU = alpha + (1-alpha)/2

		qfl = qf(alphaL, df1=v1, df2=v2)

		LL = (v1* qfl)/(v2 + v1*qfl)
	}
	else
	{
		LL = 0
	}


		v1 = 2*(m + 1)
		v2 = 2*(N - m)

	if(m == N)
	{
		UL = 1
	}
	else if(m == 0)
	{
		alphaL = (1-alpha)
		alphaU = alpha
		qfu = qf(alphaU, df1=v1, df2=v2)
		UL = (v1* qfu)/(v2 + v1*qfu)
	}
	else
	{
		alphaL = (1-alpha)/2
		alphaU = alpha + (1-alpha)/2
		qfu = qf(alphaU, df1=v1, df2=v2)
		UL = (v1* qfu)/(v2 + v1*qfu)
	}

	invisible(return(c(m=m,N=N,LL=LL,UL=UL)))
}

plotAgreemnt <- function(df, main_title = c())
{
	ggplot(df, aes(x=Appraiser, y=Percentage, group = Appraiser, label= Percentage, color=Appraiser )) + 
		  geom_line(size = 2) + 
		  geom_point(size = 8, shape = 19) +
		  geom_text(hjust=1.6, vjust=0, size = 6) + 
		  scale_y_continuous(breaks=seq((min(df$Percentage)- (min(df$Percentage)%%10)),100,5))+ 
		  theme_grey(base_size = 20) +
		  theme(plot.title = element_text(size = 20))+
		  ggtitle(main_title) +
		  xlab("Appraiser") + 
		  ylab("Percentage")
}

BSkyAttributeAgreementAnalysis.NOT.IN.USE <- function(part, operator, response, reference = c(), trial = c(), showMisclassificationStat = FALSE, alpha = 0.95)
{
	orig_part = part
	orig_operator = operator
	orig_response = response
	orig_reference = reference 
	
	if(is.factor(response)){
		response_levels = levels(response)
	}else{
		all_reponse = as.character(response)
		response_levels = levels(factor(all_reponse))
		#response = sort(unique(all_reponse))
	}
	

	# Optional reference value for response if given
	reference_given = FALSE
	if(length(reference) > 0)
	{
		reference_given = TRUE
		
		if(is.factor(reference)){
			reference = levels(reference)
			orig_reference_levels = levels(orig_reference)
		}else{
			all_reference = as.character(reference)
			reference = levels(factor(all_reference))
			#reference = sort(unique(all_reference))
			orig_reference = factor(all_reference)
			orig_reference_levels = levels(orig_reference)
		}
	}
	
	all_part = as.character(part)
	part = unique(all_part)
	part_len = length(part)
	
	all_operator = as.character(operator)
	operator = unique(all_operator)


	resp_mat = list()
	resp_mat_no_reference = list()
	resp_mat_names = c()
	j = 1
	withinAgreement = c()
	withinAgreement_reference = c()
	
	disagreement_reference = c()
	summaryDisagreement_reference = c()
	
	kappam_fleiss_mat = c()
	kappam_fleiss_mat_reference = c()
	
	observations_per_op = c()

	for(i in 1:length(operator))
	{
		observations_per_op = length(which(orig_operator == operator[i]))

# print(operator[i])
# print(observations_per_op)
# print(part_len)
# print(orig_response[j:(j + observations_per_op - 1)])
		
		Op_resp_mat = matrix(orig_response[j:(j + observations_per_op - 1)], nrow = part_len, byrow = TRUE)
						  
		dimnames(Op_resp_mat)[[2]] = c(paste(operator[i], seq(1:dim(Op_resp_mat)[2]), sep=""))
						  
		resp_mat = c(resp_mat, list(Op_resp_mat))
		resp_mat_no_reference = c(resp_mat_no_reference, list(Op_resp_mat))
		  
		resp_mat_names = c(resp_mat_names, operator[i])
		names(resp_mat) = resp_mat_names
		names(resp_mat_no_reference) = resp_mat_names
		
		#dimnames(resp_mat[[i]])[[2]] = c(paste(operator[i], seq(1:dim(resp_mat[[i]])[2]), sep="")) 

# print("resp_mat without reference for")
# print(operator[i])
# BSkyFormat(resp_mat[[i]])

		
		CI_stat = agreementCI(response_df = resp_mat[[i]], alpha = alpha)
		
		withinAgreement = rbind(withinAgreement, c(operator[i], CI_stat["m"], CI_stat["N"], (CI_stat["m"]/CI_stat["N"])*100, CI_stat["LL"]*100, CI_stat["UL"]*100))
		
		dimnames(withinAgreement)[[2]] = c("Operator", "Agreement", "Inspected", "%Aggreement", paste(format(round(alpha, 2), nsmall = 2),"CI (lower)"), paste(format(round(alpha, 2), nsmall = 2),"CI (upper)"))
		
		kappam_fleiss = (modified.kappam.fleiss(resp_mat[[i]], detail=TRUE, levels = response_levels))$detail
		kappam_fleiss = cbind(Operator=c(operator[i], rep("",dim(kappam_fleiss)[1]-1)), Response = dimnames(kappam_fleiss)[[1]], kappam_fleiss)
		rownames(kappam_fleiss) = NULL
		kappam_fleiss_mat = rbind(kappam_fleiss_mat, kappam_fleiss )
		
		if(reference_given == TRUE)
		{
			tries_by_op = paste(operator[i], seq(1:dim(resp_mat[[i]])[2]), sep="")

#print(orig_reference[seq(j, (j + observations_per_op -1), by = (observations_per_op/part_len))])

			#resp_mat[[i]] = cbind(resp_mat[[i]], orig_reference[j:(j + part_len -1)]) 
			resp_mat[[i]] = cbind(resp_mat[[i]], orig_reference[seq(j, (j + observations_per_op -1), by = (observations_per_op/part_len))])
			resp_mat[[i]] = as.data.frame(resp_mat[[i]])
			resp_mat[[i]][,dim(resp_mat[[i]])[2]] = factor(resp_mat[[i]][,dim(resp_mat[[i]])[2]])
			levels(resp_mat[[i]][,dim(resp_mat[[i]])[2]]) = orig_reference_levels
			
			dimnames(resp_mat[[i]])[[2]] = c(tries_by_op, "Reference")

# print("resp_mat without reference for")
# print(operator[i])
# BSkyFormat(resp_mat[[i]])
			
			CI_stat = agreementCI(response_df = resp_mat[[i]], alpha = alpha)
		
			withinAgreement_reference = rbind(withinAgreement_reference, c(operator[i], CI_stat["m"], CI_stat["N"], (CI_stat["m"]/CI_stat["N"])*100, CI_stat["LL"]*100, CI_stat["UL"]*100))
			dimnames(withinAgreement_reference)[[2]] = c("Operator", "Agreement", "Inspected", "%Aggreement", paste(format(round(alpha, 2), nsmall = 2),"CI (lower)"), paste(format(round(alpha, 2), nsmall = 2),"CI (upper)"))
			
			kappam_fleiss = (modified.kappam.fleiss(resp_mat[[i]], detail=TRUE, levels = orig_reference_levels))$detail
			kappam_fleiss = cbind(Operator=c(operator[i], rep("",dim(kappam_fleiss)[1]-1)), Response = dimnames(kappam_fleiss)[[1]], kappam_fleiss)
			rownames(kappam_fleiss) = NULL
			kappam_fleiss_mat_reference = rbind(kappam_fleiss_mat_reference, kappam_fleiss )
			
			if(length(orig_reference_levels) == 2)
			{
				disagreement_stat = disagreementStat(response_df = resp_mat[[i]])
				disagreement_reference = rbind(disagreement_reference, c(operator[i], disagreement_stat))
				dimnames(disagreement_reference)[[2]] = c("Operator", 
																paste("# ",orig_reference_levels[1],"/",orig_reference_levels[2], sep=""), 
																"Percent", 
																paste("# ",orig_reference_levels[2],"/",orig_reference_levels[1], sep=""),
																"Percent",
																"# Mixed", "Percent")
			}
		}
		
		j = j + observations_per_op
	}
	
	
#print(resp_mat)

	between_agreement_response_mat = c()                        
	x = lapply(resp_mat_no_reference, function(x){between_agreement_response_mat <<- cbind(between_agreement_response_mat,x)})

# print("between_agreement_response_mat with no reference")
# BSkyFormat(between_agreement_response_mat)

	CI_stat = agreementCI(response_df = between_agreement_response_mat, alpha = alpha)
	between_agreement_mat = matrix(c("All", CI_stat["m"], CI_stat["N"], (CI_stat["m"]/CI_stat["N"])*100, CI_stat["LL"]*100, CI_stat["UL"]*100), nrow = 1)
	dimnames(between_agreement_mat)[[2]] = c("Operator", "Agreement", "Inspected", "%Aggreement", paste(format(round(alpha, 2), nsmall = 2),"CI (lower)"), paste(format(round(alpha, 2), nsmall = 2),"CI (upper)"))

	kappam_fleiss_all = (modified.kappam.fleiss(between_agreement_response_mat, detail=TRUE, levels = response_levels))$detail
	kappam_fleiss_mat_all = cbind(Operator=c("All", rep("",dim(kappam_fleiss_all)[1]-1)), Response = dimnames(kappam_fleiss_all)[[1]], kappam_fleiss_all)
	rownames(kappam_fleiss_mat_all) = NULL

	if(reference_given == TRUE)
	{
		between_agreement_response_mat = c()                        
		x = lapply(resp_mat_no_reference, function(x){between_agreement_response_mat <<- cbind(between_agreement_response_mat,x)})
		
		between_agreement_response_mat = as.data.frame(between_agreement_response_mat)
		#between_agreement_response_mat = cbind(between_agreement_response_mat, Reference = orig_reference[1:part_len])

# print("between_agreement_response_mat")
# BSkyFormat(between_agreement_response_mat)
# print(observations_per_op)
# print(orig_reference[seq(1, (1 + observations_per_op -1), by = (observations_per_op/part_len))])

		between_agreement_response_mat = cbind(between_agreement_response_mat, Reference = orig_reference[seq(1, (1 + observations_per_op -1), by = (observations_per_op/part_len))])
		
		#between_agreement_response_mat = cbind(between_agreement_response_mat, Reference = orig_reference[1:part_len])
  
		between_agreement_response_mat[,dim(between_agreement_response_mat)[2]] = factor(between_agreement_response_mat[,dim(between_agreement_response_mat)[2]])
		levels(between_agreement_response_mat[,dim(between_agreement_response_mat)[2]]) = orig_reference_levels

# print("between_agreement_response_mat with reference")
# BSkyFormat(between_agreement_response_mat)

		CI_stat = agreementCI(response_df = between_agreement_response_mat, alpha = alpha)
		between_agreement_mat_reference = matrix(c("All", CI_stat["m"], CI_stat["N"], (CI_stat["m"]/CI_stat["N"])*100, CI_stat["LL"]*100, CI_stat["UL"]*100), nrow = 1)
		dimnames(between_agreement_mat_reference)[[2]] = c("Operator", "Agreement", "Inspected", "%Aggreement", paste(format(round(alpha, 2), nsmall = 2),"CI (lower)"), paste(format(round(alpha, 2), nsmall = 2),"CI (upper)"))

		kappam_fleiss_all_reference = (modified.kappam.fleiss(between_agreement_response_mat, detail=TRUE, levels = orig_reference_levels))$detail
		kappam_fleiss_mat_all_reference = cbind(Operator=c("All", rep("",dim(kappam_fleiss_all_reference)[1]-1)), Response = dimnames(kappam_fleiss_all_reference)[[1]], kappam_fleiss_all_reference)
		rownames(kappam_fleiss_mat_all_reference) = NULL
		
		if(length(orig_reference_levels) == 2)
		{
			#summaryDisagreement_reference = summaryDisagreement(response_list = resp_mat_no_reference, reference = orig_reference[1:part_len])
			summaryDisagreement_reference = summaryDisagreement(response_list = resp_mat_no_reference, reference = orig_reference[seq(1, (1 + observations_per_op -1), by = (observations_per_op/part_len))])
		}
	}
	

	BSkyFormat(withinAgreement, outputTableRenames = c("Within Appraiser Agreement")) 	
	BSkyFormat(kappam_fleiss_mat, outputTableRenames = c("Within Appraiser Fleiss Kappa Statistic"))
	BSkyFormat(between_agreement_mat, outputTableRenames = c("Between Appraiser Agreement"))
	BSkyFormat(kappam_fleiss_mat_all, outputTableRenames = c("Between Appraiser Fleiss Kappa Statistic"))

	if(reference_given == TRUE)
	{
		BSkyFormat(withinAgreement_reference, outputTableRenames = c("Each Appraiser Agreement Vs Standard"))
		
		#if(length(disagreement_reference) > 0)  
		#{
		#	BSkyFormat(disagreement_reference, outputTableRenames = c("Each Appraiser Disagreement Vs Standard"))
		#}
		
		BSkyFormat(kappam_fleiss_mat_reference, outputTableRenames = c("Each Appraiser Vs Standard Fleiss Kappa Statistic"))
		BSkyFormat(between_agreement_mat_reference, outputTableRenames = c("All Appraisers Agreement Vs Standard"))
		
		#if(length(summaryDisagreement_reference) > 0)  
		#{
		#	BSkyFormat(summaryDisagreement_reference, outputTableRenames = c("Summary of Appraiser Disagreement Vs Standard"))
		#}
		
		BSkyFormat(kappam_fleiss_mat_all_reference, outputTableRenames = c("All Appraisers Vs Standard Fleiss Kappa Statistic"))
	
		if(length(disagreement_reference) > 0)  
		{
			BSkyFormat(disagreement_reference, outputTableRenames = c("Each Appraiser Disagreement Vs Standard"))
		}
		
		if(length(summaryDisagreement_reference) > 0)  
		{
			BSkyFormat(summaryDisagreement_reference, outputTableRenames = c("Summary of Appraiser Disagreement Vs Standard"))
		}

		
		if(showMisclassificationStat == TRUE)
		{
			cat("\nOverall Accuracy and Misclassification Rates\n")
			
			#Overall accuracy
			bsky_total_observations = length(orig_response)
			bsky_overall_accuracy = sum(orig_response == orig_reference)/bsky_total_observations
			cat(paste0("\nOverall accuracy rate: ", round(bsky_overall_accuracy, BSkyGetDecimalDigitSetting()), "\n"))
			
			#Accuracy for each appraiser
			bsky_accuracy_by_appraiser <- as.data.frame(({{dataset.name}} %>%
				group_by({{selected.variableOpSelcted | safe}}) %>%
					summarise(
						Accuracy_count = sum({{selected.variableRespSelcted | safe}} == {{selected.variableRefSelcted | safe}}),
						Obs_count = n(),
						Accuracy = sum({{selected.variableRespSelcted | safe}} == {{selected.variableRefSelcted | safe}})/n()
					)))
			BSkyFormat(bsky_accuracy_by_appraiser, outputTableRenames = "Accuracy rate for each {{selected.variableOpSelcted | safe}}",
						perTableFooter = paste0("{{selected.variableOpSelcted | safe}} classified accurately compared to its {{selected.variableRefSelcted | safe}} value"))
			
			#Accuracy by standard
			bsky_accuracy_by_standard <- as.data.frame(({{dataset.name}} %>%
				group_by({{selected.variableRefSelcted | safe}}) %>%
					summarise(
						Accuracy_count = sum({{selected.variableRespSelcted | safe}} == {{selected.variableRefSelcted | safe}}),
						Obs_count = n(),
						Accuracy = sum({{selected.variableRespSelcted | safe}} == {{selected.variableRefSelcted | safe}})/n()
					)))
			BSkyFormat(bsky_accuracy_by_standard, outputTableRenames = "Accuracy rate comapred to {{selected.variableRefSelcted | safe}}")
			
			if(length(trial) > 0)
			{
				#Accuracy by trial
				bsky_accuracy_by_trial <- as.data.frame(({{dataset.name}} %>%
					group_by({{selected.variableTrialSelcted | safe}}) %>%
						summarise(
							Accuracy_count = sum({{selected.variableRespSelcted | safe}} == {{selected.variableRefSelcted | safe}}),
							Obs_count = n(),
							Accuracy = sum({{selected.variableRespSelcted | safe}} == {{selected.variableRefSelcted | safe}})/n()
						)))
				BSkyFormat(bsky_accuracy_by_trial, outputTableRenames = "Accuracy rate by {{selected.variableTrialSelcted | safe}} comapred to {{selected.variableRefSelcted | safe}}")
			}	
			
			#Accuracy by appraiser and standard
			bsky_accuracy_by_appraiser_standard <- as.data.frame(({{dataset.name}} %>%
				group_by({{selected.variableOpSelcted | safe}}, {{selected.variableRefSelcted | safe}}) %>%
					summarise(
						Accuracy_count = sum({{selected.variableRespSelcted | safe}} == {{selected.variableRefSelcted | safe}}),
						Obs_count = n(),
						Accuracy = sum({{selected.variableRespSelcted | safe}} == {{selected.variableRefSelcted | safe}})/n(),
						.groups = 'drop'
					)))
			BSkyFormat(bsky_accuracy_by_appraiser_standard, outputTableRenames = "Accuracy rate by {{selected.variableOpSelcted | safe}} and {{selected.variableRefSelcted | safe}}",
						perTableFooter = paste0("{{selected.variablePartSelcted | safe}} classified correctly compared to its {{selected.variableRefSelcted | safe}} value by {{selected.variableOpSelcted | safe}}"))
			
			#Misclassification rates
			bsky_overall_error_rate = 1 - bsky_overall_accuracy
			cat(paste0("\nOverall misclassification error rate: ", round(bsky_overall_error_rate, BSkyGetDecimalDigitSetting()),"\n"))
			
			bsky_misclassification_by_standard <- as.data.frame(({{dataset.name}} %>%
				group_by({{selected.variableRefSelcted | safe}}) %>%
					summarise(
						Misclassification_count = sum({{selected.variableRespSelcted | safe}} != {{selected.variableRefSelcted | safe}}),
						Obs_count = n(),
						Misclassification = sum({{selected.variableRespSelcted | safe}} != {{selected.variableRefSelcted | safe}})/n(),
						.groups = 'drop'
					)))
			BSkyFormat(bsky_misclassification_by_standard, outputTableRenames = "Misclassification rate compared to {{selected.variableRefSelcted | safe}}")
			
			#Overall appraiser misclassification rate
			bsky_combined_resp_mat = c()
			lapply(resp_mat, function(x){names(x) = seq(1:dim(x)[2]); bsky_combined_resp_mat <<- rbind(bsky_combined_resp_mat, x)})
			not_all_equal_rows_count <- sum(apply(bsky_combined_resp_mat[,c(1:dim(bsky_combined_resp_mat)[2]-1)], 1, function(row){!all(row == row[1])}))
			cat(paste0("\nOverall {{selected.variableOpSelcted | safe}} misclassification rate: ",(not_all_equal_rows_count/dim(bsky_combined_resp_mat)[1]),"\n"))
			
			#Appraiser misclassification rates
			bsky_misclassification_by_standard_appraiser <- as.data.frame(({{dataset.name}} %>%
				group_by({{selected.variableRefSelcted | safe}}, {{selected.variableOpSelcted | safe}}) %>%
					summarise(
						Misclassification_count = sum({{selected.variableRespSelcted | safe}} != {{selected.variableRefSelcted | safe}}),
						Obs_count = n(),
						Misclassification = sum({{selected.variableRespSelcted | safe}} != {{selected.variableRefSelcted | safe}})/n(),
						.groups = 'drop'
					)))
			BSkyFormat(bsky_misclassification_by_standard_appraiser, outputTableRenames = "Misclassification rate by {{selected.variableOpSelcted | safe}} comapred to {{selected.variableRefSelcted | safe}}", 
						perTableFooter = paste0("{{selected.variablePartSelcted | safe}} misclassified compared to its {{selected.variableRefSelcted | safe}} value by {{selected.variableOpSelcted | safe}}"))

			#Appraiser misclassification rate
			not_all_equal_rows_count_by_appraiser <- lapply(resp_mat, function(df){sum(apply(df[,c(1:dim(df)[2]-1)], 1, function(row){!all(row == row[1])}))})
			bsky_appraiser_misclassification_df = t(data.frame(not_all_equal_rows_count_by_appraiser))
			bsky_appraiser_misclassification_df = cbind(bsky_appraiser_misclassification_df, rep(dim(resp_mat[[1]])[1], length(operator)))
			bsky_appraiser_misclassification_df = cbind(bsky_appraiser_misclassification_df, bsky_appraiser_misclassification_df[,1]/bsky_appraiser_misclassification_df[,2])
			dimnames(bsky_appraiser_misclassification_df)[[2]] = c("Misclassification_count", "Obs_count", "Misclassification")
			dimnames(bsky_appraiser_misclassification_df)[[1]] = operator 
			BSkyFormat(bsky_appraiser_misclassification_df, outputTableRenames = "Misclassification rate by {{selected.variableOpSelcted | safe}}",
						perTableFooter = paste0("{{selected.variableOpSelcted | safe}} classified the same {{selected.variablePartSelcted | safe}} correctly and incorrectly across multiple trials"))

			
			#Misclassification by part
			bsky_misclassification_by_part <- as.data.frame(({{dataset.name}} %>%
				group_by({{selected.variablePartSelcted | safe}}, {{selected.variableRefSelcted | safe}}) %>%
					summarise(
						Misclassification_count = sum({{selected.variableRespSelcted | safe}} != {{selected.variableRefSelcted | safe}}),
						Obs_count = n(),
						Misclassification = sum({{selected.variableRespSelcted | safe}} != {{selected.variableRefSelcted | safe}})/n(),
						.groups = 'drop'
					)))
			BSkyFormat(bsky_misclassification_by_part, outputTableRenames = "Misclassification rate for {{selected.variablePartSelcted | safe}}", 
						perTableFooter = paste0("{{selected.variablePartSelcted | safe}} misclassified compared to its {{selected.variableRefSelcted | safe}} value"))
		
		}
		
	}
	
	# Plot Graphs
	
	ggplot_df1 = NULL
	y_percentage = round(as.numeric(c(t(withinAgreement[,c(4:6)]))), digit=2)
	x_appraiser = c(sapply(operator, function(x) rep(x,3)))
	ggplot_df1 = data.frame(Appraiser = x_appraiser, Percentage = y_percentage)
	
	#plotAgreemnt(df = ggplot_df1, main_title = c("Confidence Intervals Within Appraisers"))
	
	ggplot_df2 = NULL 
	if(reference_given == TRUE)
	{	
		y_percentage = round(as.numeric(c(t(withinAgreement_reference[,c(4:6)]))), digit=2)
		x_appraiser = c(sapply(operator, function(x) rep(x,3)))
		ggplot_df2 = data.frame(Appraiser = x_appraiser, Percentage = y_percentage)
		
		#plotAgreemnt(df = ggplot_df2, main_title = c("Confidence Intervals Against Reference"))
	}
	
	invisible(return(list(ggplot_df1, ggplot_df2)))
}


###################
#MSA Gage Analysis
###################
ss.rr.modified <- function (var, part, appr, lsl = NA, usl = NA, sigma = 6, tolerance = usl -
    lsl, data, main = "Six Sigma Gage R&R Study", sub = "", alphaLim = 0.05,
    errorTerm = "interaction", digits = 4, method = "crossed",
    print_plot = TRUE, signifstars = FALSE)
{
    curr_stars <- getOption("show.signif.stars")
    if (signifstars) {
        options(show.signif.stars = TRUE)
    }
    else {
        options(show.signif.stars = FALSE)
    }
    if (is.data.frame(data)) {
        if (deparse(substitute(var)) %in% names(data)) {
            var <- deparse(substitute(var))
        }
        if (!(var %in% names(data))) {
            stop(var, "is not a valid column name for", deparse(substitute(data)))
        }
        if (deparse(substitute(part)) %in% names(data)) {
            part <- deparse(substitute(part))
        }
        if (deparse(substitute(appr)) %in% names(data)) {
            appr <- deparse(substitute(appr))
        }
        if (part %in% names(data)) {
            data[[part]] <- factor(data[[part]])
        }
        else {
            stop(part, "is not a valid column name for", data)
        }
        if (appr %in% names(data)) {
            data[[appr]] <- factor(data[[appr]])
        }
        else {
            stop(appr, "is not a valid column name for", data)
        }
    }
    else {
        stop("A data.frame object is needed as data argument")
    }
    if (method == "nested") {
        data[part] = paste(data[[appr]], data[[part]], sep = "-")
        data[[part]] <- factor(data[[part]])
    }
    a <- nlevels(data[[part]])
    b <- nlevels(data[[appr]])
    if (method == "crossed") {
        n <- nrow(data)/(a * b)
    }
    else {
        n <- nrow(data)/a
    }
    if (abs(n - round(n)) != 0 && method == "crossed") {
        stop("The design is not balanced.")
    }
    if (n < 2 && method == "crossed") {
        stop("The design has no replications.")
    }
    if (method == "crossed") {
        if (b == 1) {
            modelf <- as.formula(paste(var, "~", part))
            model <- aov(modelf, data = data)
            modelm <- summary(model)
            rownames(modelm[[1]])[2] <- "Repeatability"
            modelm[[1]] <- rbind(modelm[[1]], c(colSums(modelm[[1]][,
                1:2]), rep(NA, 3)))
            rownames(modelm[[1]])[3] <- "Total"
            #cat("One-way ANOVA (single appraiser):\n\n")
            #print(modelm)
            modelrm <- NULL
        }
        else {
            modelf <- as.formula(paste(var, "~", part, "*", appr))
            modelfm <- as.formula(paste(var, "~", part, "*",
                appr, "+ Error(", part, "/", appr, ")"))
            model <- aov(modelf, data = data)
            modelm <- summary(model)
            if (errorTerm == "interaction") {
                modelm[[1]][1:2, 4] <- modelm[[1]][1:2, 3]/modelm[[1]][3,
                  3]
                modelm[[1]][1:2, 5] <- pf(modelm[[1]][1:2, 4],
                  modelm[[1]][1:2, 1], modelm[[1]][3, 1], lower.tail = FALSE)
            }
            rownames(modelm[[1]])[4] <- "Repeatability"
            modelm[[1]] <- rbind(modelm[[1]], c(colSums(modelm[[1]][,
                1:2]), rep(NA, 3)))
            rownames(modelm[[1]])[5] <- "Total"
            #cat("Complete model (with interaction):\n\n")
            #print(modelm)
            #cat("\nalpha for removing interaction:", alphaLim,
            #   "\n")
            pint <- modelm[[1]][3, 5]
            if (pint > alphaLim) {
                modelfr <- as.formula(paste(var, "~", part, "+",
                  appr))
                modelr <- aov(modelfr, data = data)
                modelrm <- summary(modelr)
                rownames(modelrm[[1]])[3] <- "Repeatability"
                modelrm[[1]] <- rbind(modelrm[[1]], c(colSums(modelrm[[1]][,
                  1:2]), rep(NA, 3)))
                rownames(modelrm[[1]])[4] <- "Total"
                #cat("\n\nReduced model (without interaction):\n\n")
                #print(modelrm)
            }
            else modelrm <- NULL
        }
        varComp <- matrix(ncol = 6, nrow = 7)
        rownames(varComp) <- c("Total Gage R&R", "  Repeatability",
            "  Reproducibility", paste0("    ", appr), paste0(part,
                ":", appr), "Part-To-Part", "Total Variation")
        colnames(varComp) <- c("VarComp", "%Contrib", "StdDev",
            "StudyVar", "%StudyVar", "%Tolerance")
        if (b == 1) {
            varComp[2, 1] <- modelm[[1]][2, 3]
            varComp[4, 1] <- NA
            varComp[5, 1] <- NA
            varComp[3, 2] <- NA
            varComp[6, 1] <- max(c((modelm[[1]][1, 3] - modelm[[1]][2,
                3])/(b * n), 0))
            varComp[1, 1] <- varComp[2, 1]
            varComp[7, 1] <- varComp[1, 1] + varComp[6, 1]
        }
        else {
            if (pint > alphaLim) {
                varComp[2, 1] <- modelrm[[1]][3, 3]
                varComp[4, 1] <- max(c((modelrm[[1]][2, 3] -
                  modelrm[[1]][3, 3])/(a * n), 0))
                varComp[5, 1] <- NA
                varComp[3, 1] <- varComp[4, 1]
                varComp[6, 1] <- max(c((modelrm[[1]][1, 3] -
                  modelrm[[1]][3, 3])/(b * n), 0))
                varComp[1, 1] <- varComp[2, 1] + varComp[3, 1]
                varComp[7, 1] <- varComp[1, 1] + varComp[6, 1]
            }
            else {
                varComp[2, 1] <- modelm[[1]][4, 3]
                varComp[4, 1] <- max(c((modelm[[1]][2, 3] - modelm[[1]][3,
                  3])/(a * n), 0))
                varComp[5, 1] <- max(c((modelm[[1]][3, 3] - modelm[[1]][4,
                  3])/n, 0))
                varComp[3, 1] <- varComp[4, 1] + varComp[5, 1]
                varComp[6, 1] <- max(c((modelm[[1]][1, 3] - modelm[[1]][3,
                  3])/(b * n), 0))
                varComp[1, 1] <- varComp[2, 1] + varComp[3, 1]
                varComp[7, 1] <- varComp[1, 1] + varComp[6, 1]
            }
        }
        varComp[, 2] <- round(100 * (varComp[, 1]/varComp[7,
            1]), 2)
        varComp[, 3] <- sqrt(varComp[, 1])
        varComp[, 4] <- varComp[, 3] * sigma
        varComp[, 5] <- round(100 * (varComp[, 3]/varComp[7,
            3]), 2)
        varComp[, 6] <- round(100 * (varComp[, 4]/(tolerance)),
            2)
        ncat <- max(c(1, floor((varComp[6, 4]/varComp[1, 4]) *
            1.41)))
        if (b == 1) {
            varComp <- varComp[-c(3:5), ]
        }
        else {
            if (pint > alphaLim) {
                varComp <- varComp[-c(5), ]
            }
        }
        #cat(paste("\nGage R&R\n\n"))
        #print(varComp[, 1:2])
        #cat("\n")
        if ((!is.na(usl) && !is.na(lsl)) || !is.na(tolerance)) {
            #print(varComp[, c(1, 3:6)])
        }
        else {
            #print(varComp[, 3:5])
        }
        cat(paste("\nNumber of Distinct Categories =", ncat,
            "\n"))
    }
    else if (method == "nested") {
        if (b == 1) {
            modelf <- as.formula(paste(var, "~", part))
            model <- aov(modelf, data = data)
            modelm <- summary(model)
            rownames(modelm[[1]])[2] <- "Repeatability"
            modelm[[1]] <- rbind(modelm[[1]], c(colSums(modelm[[1]][,
                1:2]), rep(NA, 3)))
            rownames(modelm[[1]])[3] <- "Total"
            #cat("One-way ANOVA (single appraiser):\n\n")
            #print(modelm)
            modelrm <- NULL
        }
        else {
            modelf <- as.formula(paste(var, "~", appr, "/", part))
            modelfm <- as.formula(paste(var, "~", appr, "/",
                part, "+ Error(", appr, "/", part, ")"))
            model <- aov(modelf, data = data)
            modelm <- summary(model)
            if (errorTerm == "interaction") {
                modelm[[1]][1, 4] <- modelm[[1]][1, 3]/modelm[[1]][2,
                  3]
                modelm[[1]][1, 5] <- pf(modelm[[1]][1, 4], modelm[[1]][1,
                  1], modelm[[1]][3, 1], lower.tail = FALSE)
            }
            rownames(modelm[[1]])[3] <- "Repeatability"
            modelm[[1]] <- rbind(modelm[[1]], c(colSums(modelm[[1]][,
                1:2]), rep(NA, 3)))
            rownames(modelm[[1]])[4] <- "Total"
            #cat("Complete model (with interaction):\n\n")
            #print(modelm)
            #cat("\nalpha for removing interaction:", alphaLim,
            #   "\n")
            modelrm <- NULL
        }
        varComp <- matrix(ncol = 6, nrow = 5)
        rownames(varComp) <- c("Total Gage R&R", "  Repeatability",
            "  Reproducibility", "Part-To-Part", "Total Variation")
        colnames(varComp) <- c("VarComp", "%Contrib", "StdDev",
            "StudyVar", "%StudyVar", "%Tolerance")
        if (b == 1) {
            varComp[2, 1] <- modelm[[1]][3, 3]
            varComp[3, 2] <- NA
            varComp[4, 1] <- max(c((modelm[[1]][2, 3] - modelm[[1]][3,
                3])/n, 0))
            varComp[1, 1] <- varComp[2, 1]
            varComp[5, 1] <- varComp[1, 1] + varComp[4, 1]
        }
        else {
            varComp[2, 1] <- modelm[[1]][3, 3]
            varComp[3, 1] <- max(c((modelm[[1]][1, 1] * (modelm[[1]][1,
                3] - modelm[[1]][2, 3])/(a * n)), 0))
            varComp[4, 1] <- max(c((modelm[[1]][2, 3] - modelm[[1]][3,
                3])/n, 0))
            varComp[1, 1] <- varComp[2, 1] + varComp[3, 1]
            varComp[5, 1] <- varComp[1, 1] + varComp[4, 1]
        }
        varComp[, 2] <- round(100 * (varComp[, 1]/varComp[5,
            1]), 2)
        varComp[, 3] <- sqrt(varComp[, 1])
        varComp[, 4] <- varComp[, 3] * sigma
        varComp[, 5] <- round(100 * (varComp[, 3]/varComp[5,
            3]), 2)
        varComp[, 6] <- round(100 * (varComp[, 4]/(tolerance)),
            2)
        ncat <- max(c(1, floor((varComp[4, 4]/varComp[1, 4]) *
            1.41)))
        if (b == 1) {
            varComp <- varComp[-c(3:5), ]
        }
        #cat(paste("\nGage R&R\n\n"))
        #print(varComp[, 1:2])
        #cat("\n")
        if ((!is.na(usl) && !is.na(lsl)) || !is.na(tolerance)) {
            #print(varComp[, c(1, 3:6)])
        }
        else {
            #print(varComp[, 3:5])
        }
        cat(paste("\nNumber of Distinct Categories =", ncat,
            "\n"))
        pint <- 0
    }
    if (print_plot) {	
        #.ss.prepCanvas(main, sub)
		SixSigma:::.ss.prepCanvas(paste("Components of Variation (", main,")"), sub)
        
		#vp.plots <- grid::viewport(name = "plots", layout = grid::grid.layout(3,2))
		vp.plots <- grid::viewport(name = "plots", layout = grid::grid.layout(1,1))
		
		# vp.plots <- grid::viewport(name = "plots", layout = grid::grid.layout(3,
            # 2, widths=unit(4, "inches"),
                          # heights=unit(6, "npc"),))
        grid::pushViewport(vp.plots)
        vp.bar <- grid::viewport(name = "barplot", layout.pos.row = 1,
            layout.pos.col = 1)
        grid::pushViewport(vp.bar)
        if (b == 1) {
            rowstoplot <- c(1, 2, 3)
        }
        else if (method == "nested") {
            rowstoplot <- c(1, 2, 3, 4)
        }
        else if (method == "crossed") {
            if (pint > alphaLim) {
                rowstoplot <- c(1, 2, 3, 5)
            }
            else {
                rowstoplot <- c(1, 2, 3, 6)
            }
        }
        if ((!is.na(usl) && !is.na(lsl)) || !is.na(tolerance)) {
            colstoplot <- c(2, 5, 6)
            klabels <- c("%Contribution", "%Study Var", "%Tolerance")
        }
        else {
            colstoplot <- c(2, 5)
            klabels <- c("%Contribution", "%Study Var")
        }
        databar <- varComp[rowstoplot, colstoplot]
        if (b == 1) {
            rownames(databar) <- c("G.R&R", "Repeat", "Part2Part")
        }
        else {
            rownames(databar) <- c("G.R&R", "Repeat", "Reprod",
                "Part2Part")
        }
		
        plot <- lattice::barchart(databar, freq = FALSE, grid = TRUE,
            scales = list(x = list(rot = 45)), par.settings = list(axis.text = list(cex = 0.6),
                par.ylab.text = list(cex = 0.8), par.main.text = list(cex = 0.85)),
            ylab = list("Percent", fontsize = 8), panel = function(...) {
                lattice::panel.barchart(...)
                lattice::panel.abline(h = 0)
                lattice::panel.abline(h = c(10, 30), lty = 2,
                  col = "gray")
            }, auto.key = list(text = klabels, cex = 0.8, columns = length(colstoplot),
                space = "bottom", rectangles = TRUE, points = FALSE,
                adj = 1, rep = FALSE), stack = FALSE, horizontal = FALSE
            #main = list("Components of Variation", fontsize = 14)
			)
        print(plot, newpage = FALSE)
        grid::popViewport()
		
		SixSigma:::.ss.prepCanvas(paste(var, "by", part,"(", main,")"), sub)
		vp.plots <- grid::viewport(name = "plots2", layout = grid::grid.layout(1,1))
		vp.varByPart <- grid::viewport(name = "varByPart", layout.pos.row = 1,
            layout.pos.col = 1)
			
        #vp.varByPart <- grid::viewport(name = "varByPart", layout.pos.row = 1,
        #    layout.pos.col = 2)
		
        grid::pushViewport(vp.varByPart)
        plot <- lattice::stripplot(as.formula(paste(var, "~",
            part)), data = data, grid = TRUE, scales = list(x = list(rot = 45)),
            par.settings = list(axis.text = list(cex = 0.6),
                par.xlab.text = list(cex = 0.8), par.ylab.text = list(cex = 0.8),
                par.main.text = list(cex = 0.9)),
				#main = paste(var, "by", part), 
				type = c("p", "a")
				)
        print(plot, newpage = FALSE)
        grid::popViewport()
		
		SixSigma:::.ss.prepCanvas(paste(var, "by", appr, "(", main,")"), sub)
		vp.plots <- grid::viewport(name = "plots3", layout = grid::grid.layout(1,1))
        vp.varByAppr <- grid::viewport(name = "varByAppr", layout.pos.row = 1,
            layout.pos.col = 1)
			
		#vp.varByAppr <- grid::viewport(name = "varByAppr", layout.pos.row = 2,
        #   layout.pos.col = 2)
		
        grid::pushViewport(vp.varByAppr)
        plot <- lattice::stripplot(as.formula(paste(var, "~",
            appr)), data = data, grid = TRUE, scales = list(x = list(rot = 45)),
            par.settings = list(axis.text = list(cex = 0.6),
                par.xlab.text = list(cex = 0.8), par.ylab.text = list(cex = 0.8),
                par.main.text = list(cex = 0.9)),
				#main = paste(var, "by", appr), 
				type = c("p", "a")
				)
        print(plot, newpage = FALSE)
        grid::popViewport()
		
		
		SixSigma:::.ss.prepCanvas(paste(paste0(part, ":", appr, " Interaction"), "(", main,")"), sub)
		vp.plots <- grid::viewport(name = "plots4", layout = grid::grid.layout(1,1))
		
        if (method == "crossed") {
			
			vp.Interact <- grid::viewport(name = "Interact",
                layout.pos.row = 1, layout.pos.col = 1)
				
            #vp.Interact <- grid::viewport(name = "Interact",
            #    layout.pos.row = 3, layout.pos.col = 2)
            
			grid::pushViewport(vp.Interact)
            data.xbar <- aggregate(as.formula(paste(var, "~",
                appr, "+", part)), data = data, mean)
            plot <- lattice::stripplot(as.formula(paste(var,
                "~", part)), groups = get(appr), data = data.xbar,
                pch = 16, grid = TRUE, par.settings = list(par.main.text = list(cex = 0.9)),
                #main = paste0(part, ":", appr, " Interaction"),
                type = c("p", "a"), auto.key = list(text = levels(data[[appr]]),
                  columns = nlevels(data[[appr]]), space = "bottom",
                  cex = 0.5, lines = TRUE, points = FALSE, adj = 1))
            print(plot, newpage = FALSE)
            grid::popViewport()
        }
        data.xbar <- aggregate(as.formula(paste(var, "~", appr,
            "+", part)), data = data, mean)
        data.xrange <- aggregate(as.formula(paste(var, "~", appr,
            "+", part)), data = data, function(x) {
            max(x) - min(x)
        })
		
		
		SixSigma:::.ss.prepCanvas(paste("Xbar Chart by", appr, "(", main,")"), sub)
		vp.plots <- grid::viewport(name = "plots5", layout = grid::grid.layout(1,1))
		vp.ccMean <- grid::viewport(name = "ccMean", layout.pos.row = 1,
            layout.pos.col = 1)
		
        ar <- mean(data.xrange[[var]])
		
        #vp.ccMean <- grid::viewport(name = "ccMean", layout.pos.row = 3,
        #    layout.pos.col = 1)
		
        grid::pushViewport(vp.ccMean)
        xbar <- mean(data[[var]], na.rm = TRUE)
        ucl <- xbar + (3/(ss.cc.getd2(n) * sqrt(n))) * ar
        lcl <- xbar - (3/(ss.cc.getd2(n) * sqrt(n))) * ar
        glimits <- c(min(range(data.xbar[[var]])[1], lcl), max(range(data.xbar[[var]])[2],
            ucl)) + c(-1, 1) * 0.1 * diff(range(data.xbar[[var]]))
        plot <- lattice::xyplot(as.formula(paste(var, "~", part,
            "|", appr)), data = data.xbar, pch = 16, par.settings = list(axis.text = list(cex = 0.6),
            par.xlab.text = list(cex = 0.8), par.ylab.text = list(cex = 0.8),
            par.main.text = list(cex = 0.9)), par.strip.text = list(cex = 0.6),
            #main = bquote(bold(bar(x) * " Chart by " * .(appr))),
            grid = TRUE, layout = c(b, 1), type = "b", axs = "r",
            ylim = glimits, scales = list(alternating = FALSE,
                x = list(relation = "free", rot = 45)), panel = function(...) {
                lattice::panel.xyplot(...)
                lattice::panel.abline(h = xbar, lty = 2)
                lattice::panel.abline(h = ucl, col = "red3")
                lattice::panel.abline(h = lcl, col = "red3")
            })
        print(plot, newpage = FALSE)
        grid::popViewport()
		
		SixSigma:::.ss.prepCanvas(paste("R Chart by", appr, "(", main,")"), sub)
		vp.plots <- grid::viewport(name = "plots6", layout = grid::grid.layout(1,1))
		vp.ccRange <- grid::viewport(name = "ccRange", layout.pos.row = 2,
            layout.pos.col = 1)
		
        #vp.ccRange <- grid::viewport(name = "ccRange", layout.pos.row = 2,
        #    layout.pos.col = 1)
        grid::pushViewport(vp.ccRange)
        this.d3 <- ss.cc.getd3(n)
        this.d2 <- ss.cc.getd2(n)
        rlimits <- c(max(ar * (1 - 3 * (this.d3/(this.d2))),
            0), ar * (1 + 3 * (this.d3/(this.d2))))
        glimits <- c(min(range(data.xrange[[var]])[1], rlimits[1]),
            max(range(data.xrange[[var]])[2], rlimits[2])) +
            c(-1, 1) * 0.1 * diff(range(data.xrange[[var]]))
        if (all(glimits == 0)) {
            glimits <- c(0, max(data[[var]]))
        }
        plot <- lattice::xyplot(as.formula(paste(var, "~", part,
            "|", appr)), data = data.xrange, pch = 16, par.settings = list(axis.text = list(cex = 0.6),
            par.xlab.text = list(cex = 0.8), par.ylab.text = list(cex = 0.8),
            par.main.text = list(cex = 0.9), layout.widths = list(axis.panel = c(1,
                0, 0))), par.strip.text = list(cex = 0.6), 
				#main = paste("R Chart by", appr), 
			grid = TRUE, layout = c(b, 1), type = "b",
            axs = "r", ylim = glimits, scales = list(alternating = FALSE,
                x = list(relation = "free", rot = 45)), panel = function(...) {
                lattice::panel.xyplot(...)
                lattice::panel.abline(h = ar, lty = 2)
                lattice::panel.abline(h = rlimits[1], col = "red3")
                lattice::panel.abline(h = rlimits[2], col = "red3")
            })
        print(plot, newpage = FALSE)
        grid::popViewport()
    }
	
    options(show.signif.stars = curr_stars)
    
	invisible(list(anovaTable = modelm, anovaRed = modelrm, varComp = varComp[,
        1:2], studyVar = varComp[, 3:6], ncat = ncat))
}


############################
#Process Capability Analysis
############################
process.capability.nonNormal <- function (data, data.name = c(), fun_name = c(), spec.limits, target, 
    breaks = "scott", add.stats = TRUE, print = TRUE, capability.type = "overall", digits = getOption("digits"), 
    restore.par = TRUE) 
{
	if ((missing(data)) || (length(data)==0) || !is.numeric(data)) 
        stop("Data to be analyzed is missing or not numeric")
    if ((missing(fun_name)) || (length(fun_name)==0)) 
        stop("A non-normal function name is missing")
    # if (missing(args)) || (length(args)==0)) 
        # stop("Parameter for the non-normal function is missing")
		
    x <- as.vector(data)
    x <- x[!is.na(x)]
	
	fitg_dist = NULL
	suppressWarnings(
		fitg_dist <- fitdist(data = as.numeric(x), 
						distr = fun_name,
						method = 'mle',
						keepdata = TRUE
						)
	)
	
	if(is.null(fitg_dist))
		stop("Could compute using the distribution function chosen")
	
	center = NULL
	
	#print(fitg_dist$estimate)
	#print(names(fitg_dist$estimate))
	
	#exp_rate = fitg_dist$estimate
	
    # sizes <- object$sizes
    # center <- object$center
    # if (missing(std.dev)) 
        # std.dev <- object$std.dev
    n <- length(x)
	
	# changed to incorporate overall vs potential (P vs. C conventions that was missing in qcc package)
	if(capability.type == "overall")
	{
		#title <- paste("Overall Process Capability Analysis ( sample size of", max(sizes), ")\nfor", 
		title <- paste("Overall Process Capability Analysis for", data.name,"based on",fun_name,"dist model")
	}
	# else
	# {
		# #title <- paste("Potential (Within) Process Capability Analysis ( sample size of", max(sizes), ")\nfor", 
		# title <- paste("Potential (Within) Process Capability Analysis\nfor",
			# object$data.name)
	# }
	
    if (missing(spec.limits)) 
        stop("specification limits must be provided")
		
    spec.limits <- as.vector(spec.limits)[1:2]
	
    LSL <- spec.limits[1]
    if (!(is.numeric(LSL) & is.finite(LSL))) 
        LSL <- NA
    
	USL <- spec.limits[2]
    if (!(is.numeric(USL) & is.finite(USL))) 
        USL <- NA
    
	if (is.na(LSL) & is.na(USL)) 
        stop("invalid specification limits - at least one of LSL or USL must be specified")
    
	has.target <- (!missing(target))
    if (!has.target) {
        target <- mean(spec.limits, na.rm = TRUE)
        if (!is.na(LSL) & !is.na(USL)) 
            has.target <- TRUE
    }
    
	if (is.na(LSL)) {
        if (target > USL) 
            warning("target value larger than one-sided specification limit...")
    }
    
	if (is.na(USL)) {
        if (target < LSL) 
            warning("target value smaller than one-sided specification limit...")
    }
    
	if (!is.na(LSL) & !is.na(USL)) {
        if (target < LSL || target > USL) 
            warning("target value is not within specification limits...")
    }
	
    # if (missing(nsigmas)) 
        # if (is.null(object$nsigmas)) 
            # stop("nsigmas not available in the 'qcc' object. Please provide nsigmas.")
        # else nsigmas <- object$nsigmas
    # if (confidence.level < 0 | confidence.level > 1) 
        # stop("the argument confidence.level must be a value between 0 and 1")
	
	# X0.99865	The 99.865th percentile for the specified distribution
	# X0.00135	The 0.135th percentile for the specified distribution
	# X0.5	The 50th percentile for the specified distribution
	
	# denscomp(fitg_dist, xlim, ylim, probability = TRUE, main, xlab, ylab, datacol, fitlty, 
    # fitcol, fitlwd, addlegend = TRUE, legendtext, xlegend = "topright", ylegend = NULL, 
    # demp = FALSE, dempcol = "black", plotstyle = "graphics", 
    # discrete, fitnbpts = 101, fittype="l", ...)

	
	quantile_fit = quantile(fitg_dist, probs = c(0.00135, 0.5, 0.99865))
	
	# quantile_00135 <- quantile(x, 0.00135)
	# quantile_99865 <- quantile(x, 0.99865)
	# quantile_05 <- quantile(x, 0.5)
	# print(quantile_fit)
	# cat("\n",quantile_00135,quantile_05,quantile_99865,"\n")
	
	quantile_00135 <- quantile_fit$quantiles[1,1]
	quantile_05 <- quantile_fit$quantiles[1,2]
	quantile_99865 <- quantile_fit$quantiles[1,3]
	
	
	Pp 	= (USL - LSL) / (quantile_99865 - quantile_00135)
	Ppl = (quantile_05 - LSL) / (quantile_05 - quantile_00135)
	Ppu = (USL - quantile_05) / (quantile_99865 - quantile_05)
	Ppk = min(Ppl,Ppu)
	
	exp.LSL <- 0
	exp.USL <- 0
	# if (is.na(LSL)) 
        # exp.LSL <- NA
    # else {
        # exp.LSL <- pnorm((LSL - center)/std.dev) * 100
        # if (exp.LSL < 0.01) 
            # exp.LSL <- 0
    # }
    # if (is.na(USL)) 
        # exp.USL <- NA
    # else {
        # exp.USL <- (1 - pnorm((USL - center)/std.dev)) * 100
        # if (exp.USL < 0.01) 
            # exp.USL <- 0
    # }
	
	obs.LSL <- sum(x < LSL)/n * 100
    obs.USL <- sum(x > USL)/n * 100
	
	#h <- hist(x, breaks = breaks, plot = FALSE)
    #ylim <- range(h$density, dx)
	#ylim = range(min(h$counts), max(h$counts))
    #ylim <- ylim + diff(ylim) * c(0, 0.05)
	#ylim = range(0,1)
	
	
    xlim <- range(x, USL, LSL, target, na.rm = TRUE)
    #xlim <- xlim + diff(xlim) * c(-0.1, 0.1)
    xx <- seq(min(xlim), max(xlim), length = 250)
	
	tab <- rbind(Pp, Ppl, Ppu, Ppk)
	
	if(capability.type == "overall")
	{
		rownames(tab) <- c("Pp", "Pp_l", "Pp_u", "Pp_k")
	}
	
	#colnames(tab) <- c("Value", names(Cp.limits))
    oldpar <- par(no.readonly = TRUE)
    if (restore.par) 
        on.exit(par(oldpar))
    mar <- c(4.1, 2.1, 3.6, 2.1)
    #par(bg = qcc.options("bg.margin"), cex = oldpar$cex * 
	par(cex = oldpar$cex *
        qcc.options("cex"), mar = if (add.stats) 
        pmax(mar, c(8.6 + is.null(center) * -1, 0, 0, 0))
    else mar)
	#plot(0, 0, type = "n", xlim = xlim, ylim = ylim, axes = FALSE, ylab = "", xlab = "")
	denscomp(fitg_dist, demp = TRUE, main =title, xlim = xlim, xlab = "") #, type = "n", xlim = xlim, ylim = ylim, axes = FALSE, ylab = "", xlab = "") #, main = "Hello", addlegend=FALSE, xlab = "mydata", ylab="my Y data")
    usr <- par()$usr
    #rect(usr[1], usr[3], usr[2], usr[4], col = qcc.options("bg.figure"))
    #axis(1)
    # box()
    
	#top.line <- par("mar")[3] - length(capture.output(cat(title))) - 
        0.5
    #mtext(title, side = 3, line = top.line, font = par("font.main"), 
    #   cex = qcc.options("cex"), col = par("col.main"))
		
		
	abline(v = c(LSL, USL), col = 2, lty = 3, lwd = 2)
    text(LSL, usr[4], "LSL", pos = 3, offset = 0.2, cex = 0.8, 
        xpd = TRUE)
    text(USL, usr[4], "USL", pos = 3, offset = 0.2, cex = 0.8, 
        xpd = TRUE)
    if (has.target) {
        abline(v = target, col = 2, lty = 2, lwd = 2)
        text(target, usr[4], "Target", pos = 3, offset = 0.2, 
            cex = 0.8, xpd = TRUE)
    }
	
	if (add.stats) {
        plt <- par()$plt
        px <- diff(usr[1:2])/diff(plt[1:2])
        xfig <- c(usr[1] - px * plt[1], usr[2] + px * (1 - plt[2]))
        at.col <- xfig[1] + diff(xfig[1:2]) * c(0.07, 0.35, 0.56, 
            0.75)
        top.line <- 3
        mtext(paste("Number of obs = ", n, sep = ""), 
            side = 1, line = top.line, adj = 0, at = at.col[1], 
            font = qcc.options("font.stats"), cex = par("cex") * 
                qcc.options("cex.stats"))
		
		mtext(paste("Dist. used:", fun_name), 
            side = 1, line = top.line+1, adj = 0, at = at.col[1], 
            font = qcc.options("font.stats"), cex = par("cex") * 
                qcc.options("cex.stats"))
		
		# mtext(paste("Dist. Estimates", sep = ""), 
            # side = 1, line = top.line+2, adj = 0, at = at.col[1], 
            # font = qcc.options("font.stats"), cex = par("cex") * 
                # qcc.options("cex.stats"))
		
		if(length(fitg_dist$estimate)>0)
		{
			for(est in 1:length(fitg_dist$estimate))
			{
				mtext(paste(names(fitg_dist$estimate)[est], "=", signif(as.numeric(fitg_dist$estimate[est],digits)), sep = ""), 
					side = 1, line = top.line+est+1, adj = 0, at = at.col[1], 
					font = qcc.options("font.stats"), cex = par("cex") * 
						qcc.options("cex.stats"))
			}
		}
				
        mtext(ifelse(has.target, paste("Target = ", signif(target, 
            digits), sep = ""), paste("Target = ")), 
            side = 1, line = top.line, adj = 0, at = at.col[2], 
            font = qcc.options("font.stats"), cex = par("cex") * 
                qcc.options("cex.stats"))
        mtext(paste("LSL = ", ifelse(is.na(LSL), "", 
            signif(LSL, digits)), sep = ""), side = 1, 
            line = top.line + 1, adj = 0, at = at.col[2], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
        mtext(paste("USL = ", ifelse(is.na(USL), "", 
            signif(USL, digits)), sep = ""), side = 1, 
            line = top.line + 2, adj = 0, at = at.col[2], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
			
		# changed to incorporate overall vs potential (P vs. C conventions that was missing in qcc package)
		if(capability.type == "overall")
		{
			mtext(paste("Pp     = ", ifelse(is.na(Pp), "", 
				signif(Pp, 3)), sep = ""), side = 1, line = top.line, 
				adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			mtext(paste("Pp_l  = ", ifelse(is.na(Ppl), "", 
				signif(Ppl, 3)), sep = ""), side = 1, line = top.line + 
				1, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			mtext(paste("Pp_u = ", ifelse(is.na(Ppu), "", 
				signif(Ppu, 3)), sep = ""), side = 1, line = top.line + 
				2, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			mtext(paste("Pp_k = ", ifelse(is.na(Ppk), "", 
				signif(Ppk, 3)), sep = ""), side = 1, line = top.line + 
				3, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			# mtext(paste("Ppm  = ", ifelse(is.na(Cpm), "", 
				# signif(Cpm, 3)), sep = ""), side = 1, line = top.line + 
				# 4, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				# cex = par("cex") * qcc.options("cex.stats"))
		}
		# else
		# {
						# mtext(paste("Cp     = ", ifelse(is.na(Cp), "", 
				# signif(Cp, 3)), sep = ""), side = 1, line = top.line, 
				# adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				# cex = par("cex") * qcc.options("cex.stats"))
			# mtext(paste("Cp_l  = ", ifelse(is.na(Cp.l), "", 
				# signif(Cp.l, 3)), sep = ""), side = 1, line = top.line + 
				# 1, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				# cex = par("cex") * qcc.options("cex.stats"))
			# mtext(paste("Cp_u = ", ifelse(is.na(Cp.u), "", 
				# signif(Cp.u, 3)), sep = ""), side = 1, line = top.line + 
				# 2, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				# cex = par("cex") * qcc.options("cex.stats"))
			# mtext(paste("Cp_k = ", ifelse(is.na(Cp.k), "", 
				# signif(Cp.k, 3)), sep = ""), side = 1, line = top.line + 
				# 3, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				# cex = par("cex") * qcc.options("cex.stats"))
			# mtext(paste("Cpm  = ", ifelse(is.na(Cpm), "", 
				# signif(Cpm, 3)), sep = ""), side = 1, line = top.line + 
				# 4, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				# cex = par("cex") * qcc.options("cex.stats"))
		# }
		
        # mtext(paste("Exp<LSL ", ifelse(is.na(exp.LSL), 
            # "", paste(signif(exp.LSL, 2), "%", sep = "")), 
            # sep = ""), side = 1, line = top.line, adj = 0, 
            # at = at.col[4], font = qcc.options("font.stats"), 
            # cex = par("cex") * qcc.options("cex.stats"))
        # mtext(paste("Exp>USL ", ifelse(is.na(exp.USL), 
            # "", paste(signif(exp.USL, 2), "%", sep = "")), 
            # sep = ""), side = 1, line = top.line + 1, adj = 0, 
            # at = at.col[4], font = qcc.options("font.stats"), 
            # cex = par("cex") * qcc.options("cex.stats"))
        mtext(paste("Obs<LSL ", ifelse(is.na(obs.LSL), 
            "", paste(signif(obs.LSL, 2), "%", sep = "")), 
            sep = ""), side = 1, line = top.line , adj = 0, 
            at = at.col[4], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
        mtext(paste("Obs>USL ", ifelse(is.na(obs.USL), 
            "", paste(signif(obs.USL, 2), "%", sep = "")), 
            sep = ""), side = 1, line = top.line + 1, adj = 0, 
            at = at.col[4], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
    }
   
	center = 0
	std.dev = 0
	
	invisible(list(nobs = n, center = center, std.dev = std.dev, 
        target = target, spec.limits = {
            sl <- c(LSL, USL)
            names(sl) <- c("LSL", "USL")
            sl
        }, indices = tab, exp = {
            exp <- c(exp.LSL, exp.USL)/100
            names(exp) <- c("Exp < LSL", "Exp > USL")
            exp
        }, obs = {
            obs <- c(obs.LSL, obs.USL)/100
            names(obs) <- c("Obs < LSL", "Obs > USL")
            obs
    }))
	

	
    # Cp <- (USL - LSL)/(2 * nsigmas * std.dev)
    # Cp.u <- (USL - center)/(nsigmas * std.dev)
    # Cp.l <- (center - LSL)/(nsigmas * std.dev)
    # Cp.k <- min(Cp.u, Cp.l)
    # Cpm <- Cp/sqrt(1 + ((center - target)/std.dev)^2)
    # alpha <- 1 - confidence.level
    # Cp.limits <- Cp * sqrt(qchisq(c(alpha/2, 1 - alpha/2), n - 
        # 1)/(n - 1))
    # Cp.u.limits <- Cp.u * (1 + c(-1, 1) * qnorm(confidence.level) * 
        # sqrt(1/(9 * n * Cp.u^2) + 1/(2 * (n - 1))))
    # Cp.l.limits <- Cp.l * (1 + c(-1, 1) * qnorm(confidence.level) * 
        # sqrt(1/(9 * n * Cp.l^2) + 1/(2 * (n - 1))))
    # Cp.k.limits <- Cp.k * (1 + c(-1, 1) * qnorm(1 - alpha/2) * 
        # sqrt(1/(9 * n * Cp.k^2) + 1/(2 * (n - 1))))
    # df <- n * (1 + ((center - target)/std.dev)^2)/(1 + 2 * ((center - 
        # target)/std.dev)^2)
    # Cpm.limits <- Cpm * sqrt(qchisq(c(alpha/2, 1 - alpha/2), 
        # df)/df)
		
    # names(Cp.limits) <- names(Cp.k.limits) <- names(Cpm.limits) <- c(paste(round(100 * 
        # alpha/2, 1), "%", sep = ""), paste(round(100 * 
        # (1 - alpha/2), 1), "%", sep = ""))
		
    # if (is.na(LSL)) 
        # exp.LSL <- NA
    # else {
        # exp.LSL <- pnorm((LSL - center)/std.dev) * 100
        # if (exp.LSL < 0.01) 
            # exp.LSL <- 0
    # }
    # if (is.na(USL)) 
        # exp.USL <- NA
    # else {
        # exp.USL <- (1 - pnorm((USL - center)/std.dev)) * 100
        # if (exp.USL < 0.01) 
            # exp.USL <- 0
    # }
	
    # obs.LSL <- sum(x < LSL)/n * 100
    # obs.USL <- sum(x > USL)/n * 100
    # xlim <- range(x, USL, LSL, target, na.rm = TRUE)
    # xlim <- xlim + diff(xlim) * c(-0.1, 0.1)
    # xx <- seq(min(xlim), max(xlim), length = 250)
	
    # dx <- dnorm(xx, center, std.dev)
    # h <- hist(x, breaks = breaks, plot = FALSE)
    # ylim <- range(h$density, dx)
    # ylim <- ylim + diff(ylim) * c(0, 0.05)
    # tab <- cbind(c(Cp, Cp.l, Cp.u, Cp.k, Cpm), rbind(Cp.limits, 
        # Cp.l.limits, Cp.u.limits, Cp.k.limits, Cpm.limits))
	
	# # changed to incorporate overall vs potential (P vs. C conventions that was missing in qcc package)
	# if(capability.type == "overall")
	# {
		# rownames(tab) <- c("Pp", "Pp_l", "Pp_u", 
			# "Pp_k", "Ppm")
	# }
	# else
	# {
		# rownames(tab) <- c("Cp", "Cp_l", "Cp_u", 
			# "Cp_k", "Cpm")
	# }
	
    # colnames(tab) <- c("Value", names(Cp.limits))
    # oldpar <- par(no.readonly = TRUE)
    # if (restore.par) 
        # on.exit(par(oldpar))
    # mar <- c(4.1, 2.1, 3.6, 2.1)
    # par(bg = qcc.options("bg.margin"), cex = oldpar$cex * 
        # qcc.options("cex"), mar = if (add.stats) 
        # pmax(mar, c(8.6 + is.null(center) * -1, 0, 0, 0))
    # else mar)
    # plot(0, 0, type = "n", xlim = xlim, ylim = ylim, axes = FALSE, 
        # ylab = "", xlab = "")
    # usr <- par()$usr
    # rect(usr[1], usr[3], usr[2], usr[4], col = qcc.options("bg.figure"))
    # axis(1)
    # box()
    # top.line <- par("mar")[3] - length(capture.output(cat(title))) - 
        # 0.5
    # mtext(title, side = 3, line = top.line, font = par("font.main"), 
        # cex = qcc.options("cex"), col = par("col.main"))
    # plot(h, add = TRUE, freq = FALSE)
    # abline(v = c(LSL, USL), col = 2, lty = 3, lwd = 2)
    # text(LSL, usr[4], "LSL", pos = 3, offset = 0.2, cex = 0.8, 
        # xpd = TRUE)
    # text(USL, usr[4], "USL", pos = 3, offset = 0.2, cex = 0.8, 
        # xpd = TRUE)
    # if (has.target) {
        # abline(v = target, col = 2, lty = 2, lwd = 2)
        # text(target, usr[4], "Target", pos = 3, offset = 0.2, 
            # cex = 0.8, xpd = TRUE)
    # }
    # lines(xx, dx, lty = 2)
    # if (add.stats) {
        # plt <- par()$plt
        # px <- diff(usr[1:2])/diff(plt[1:2])
        # xfig <- c(usr[1] - px * plt[1], usr[2] + px * (1 - plt[2]))
        # at.col <- xfig[1] + diff(xfig[1:2]) * c(0.07, 0.35, 0.56, 
            # 0.75)
        # top.line <- 3
        # mtext(paste("Number of obs = ", n, sep = ""), 
            # side = 1, line = top.line, adj = 0, at = at.col[1], 
            # font = qcc.options("font.stats"), cex = par("cex") * 
                # qcc.options("cex.stats"))
        # mtext(paste("Center = ", signif(center, digits), 
            # sep = ""), side = 1, line = top.line + 1, adj = 0, 
            # at = at.col[1], font = qcc.options("font.stats"), 
            # cex = par("cex") * qcc.options("cex.stats"))
        # mtext(paste("StdDev = ", signif(std.dev, digits), 
            # sep = ""), side = 1, line = top.line + 2, adj = 0, 
            # at = at.col[1], font = qcc.options("font.stats"), 
            # cex = par("cex") * qcc.options("cex.stats"))
        # mtext(ifelse(has.target, paste("Target = ", signif(target, 
            # digits), sep = ""), paste("Target = ")), 
            # side = 1, line = top.line, adj = 0, at = at.col[2], 
            # font = qcc.options("font.stats"), cex = par("cex") * 
                # qcc.options("cex.stats"))
        # mtext(paste("LSL = ", ifelse(is.na(LSL), "", 
            # signif(LSL, digits)), sep = ""), side = 1, 
            # line = top.line + 1, adj = 0, at = at.col[2], font = qcc.options("font.stats"), 
            # cex = par("cex") * qcc.options("cex.stats"))
        # mtext(paste("USL = ", ifelse(is.na(USL), "", 
            # signif(USL, digits)), sep = ""), side = 1, 
            # line = top.line + 2, adj = 0, at = at.col[2], font = qcc.options("font.stats"), 
            # cex = par("cex") * qcc.options("cex.stats"))
			
		# # changed to incorporate overall vs potential (P vs. C conventions that was missing in qcc package)
		# if(capability.type == "overall")
		# {
			# mtext(paste("Pp     = ", ifelse(is.na(Cp), "", 
				# signif(Cp, 3)), sep = ""), side = 1, line = top.line, 
				# adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				# cex = par("cex") * qcc.options("cex.stats"))
			# mtext(paste("Pp_l  = ", ifelse(is.na(Cp.l), "", 
				# signif(Cp.l, 3)), sep = ""), side = 1, line = top.line + 
				# 1, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				# cex = par("cex") * qcc.options("cex.stats"))
			# mtext(paste("Pp_u = ", ifelse(is.na(Cp.u), "", 
				# signif(Cp.u, 3)), sep = ""), side = 1, line = top.line + 
				# 2, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				# cex = par("cex") * qcc.options("cex.stats"))
			# mtext(paste("Pp_k = ", ifelse(is.na(Cp.k), "", 
				# signif(Cp.k, 3)), sep = ""), side = 1, line = top.line + 
				# 3, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				# cex = par("cex") * qcc.options("cex.stats"))
			# mtext(paste("Ppm  = ", ifelse(is.na(Cpm), "", 
				# signif(Cpm, 3)), sep = ""), side = 1, line = top.line + 
				# 4, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				# cex = par("cex") * qcc.options("cex.stats"))
		# }
		# else
		# {
						# mtext(paste("Cp     = ", ifelse(is.na(Cp), "", 
				# signif(Cp, 3)), sep = ""), side = 1, line = top.line, 
				# adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				# cex = par("cex") * qcc.options("cex.stats"))
			# mtext(paste("Cp_l  = ", ifelse(is.na(Cp.l), "", 
				# signif(Cp.l, 3)), sep = ""), side = 1, line = top.line + 
				# 1, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				# cex = par("cex") * qcc.options("cex.stats"))
			# mtext(paste("Cp_u = ", ifelse(is.na(Cp.u), "", 
				# signif(Cp.u, 3)), sep = ""), side = 1, line = top.line + 
				# 2, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				# cex = par("cex") * qcc.options("cex.stats"))
			# mtext(paste("Cp_k = ", ifelse(is.na(Cp.k), "", 
				# signif(Cp.k, 3)), sep = ""), side = 1, line = top.line + 
				# 3, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				# cex = par("cex") * qcc.options("cex.stats"))
			# mtext(paste("Cpm  = ", ifelse(is.na(Cpm), "", 
				# signif(Cpm, 3)), sep = ""), side = 1, line = top.line + 
				# 4, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				# cex = par("cex") * qcc.options("cex.stats"))
		# }
		
        # mtext(paste("Exp<LSL ", ifelse(is.na(exp.LSL), 
            # "", paste(signif(exp.LSL, 2), "%", sep = "")), 
            # sep = ""), side = 1, line = top.line, adj = 0, 
            # at = at.col[4], font = qcc.options("font.stats"), 
            # cex = par("cex") * qcc.options("cex.stats"))
        # mtext(paste("Exp>USL ", ifelse(is.na(exp.USL), 
            # "", paste(signif(exp.USL, 2), "%", sep = "")), 
            # sep = ""), side = 1, line = top.line + 1, adj = 0, 
            # at = at.col[4], font = qcc.options("font.stats"), 
            # cex = par("cex") * qcc.options("cex.stats"))
        # mtext(paste("Obs<LSL ", ifelse(is.na(obs.LSL), 
            # "", paste(signif(obs.LSL, 2), "%", sep = "")), 
            # sep = ""), side = 1, line = top.line + 2, adj = 0, 
            # at = at.col[4], font = qcc.options("font.stats"), 
            # cex = par("cex") * qcc.options("cex.stats"))
        # mtext(paste("Obs>USL ", ifelse(is.na(obs.USL), 
            # "", paste(signif(obs.USL, 2), "%", sep = "")), 
            # sep = ""), side = 1, line = top.line + 3, adj = 0, 
            # at = at.col[4], font = qcc.options("font.stats"), 
            # cex = par("cex") * qcc.options("cex.stats"))
    # }
    # if (print) {
        # cat("\nProcess Capability Analysis\n")
        # cat("\nCall:\n", deparse(match.call()), "\n\n", 
            # sep = "")
        # cat(paste(formatC("Number of obs = ", width = 16), 
            # formatC(n, width = 12, flag = "-"), formatC("Target = ", 
                # width = 10), ifelse(has.target, formatC(signif(target, 
                # digits = digits), flag = "-"), ""), 
            # "\n", sep = ""))
        # cat(paste(formatC("Center = ", width = 16), formatC(signif(center, 
            # digits = digits), width = 12, flag = "-"), 
            # formatC("LSL = ", width = 10), ifelse(is.na(LSL), 
                # "", formatC(signif(LSL, digits = digits), 
                  # flag = "-")), "\n", sep = ""))
        # cat(paste(formatC("StdDev = ", width = 16), formatC(signif(std.dev, 
            # digits = digits), width = 12, flag = "-"), 
            # formatC("USL = ", width = 10), ifelse(is.na(USL), 
                # "", formatC(signif(USL, digits = digits), 
                  # flag = "-")), "\n", sep = ""))
        # cat("\nCapability indices:\n\n")
        # print(tab, digits = 4, na.print = "", print.gap = 2)
        # cat("\n")
        # cat(paste("Exp<LSL", ifelse(is.na(exp.LSL), "\t", 
            # paste(format(exp.LSL, digits = 2), "%\t", sep = "")), 
            # "Obs<LSL", ifelse(is.na(obs.LSL), "", 
                # paste(format(obs.LSL, digits = 2), "%\n", 
                  # sep = ""))))
        # cat(paste("Exp>USL", ifelse(is.na(exp.USL), "\t", 
            # paste(format(exp.USL, digits = 2), "%\t", sep = "")), 
            # "Obs>USL", ifelse(is.na(obs.USL), "", 
                # paste(format(obs.USL, digits = 2), "%\n", 
                  # sep = ""))))
    # }
    # invisible(list(nobs = n, center = center, std.dev = std.dev, 
        # target = target, spec.limits = {
            # sl <- c(LSL, USL)
            # names(sl) <- c("LSL", "USL")
            # sl
        # }, indices = tab, exp = {
            # exp <- c(exp.LSL, exp.USL)/100
            # names(exp) <- c("Exp < LSL", "Exp > USL")
            # exp
        # }, obs = {
            # obs <- c(obs.LSL, obs.USL)/100
            # names(obs) <- c("Obs < LSL", "Obs > USL")
            # obs
        # }))
}



process.capability.enhanced <- function (object, spec.limits, target, std.dev, nsigmas, confidence.level = 0.95, 
    breaks = "scott", add.stats = TRUE, print = TRUE, capability.type = "potential", digits = getOption("digits"), 
    restore.par = TRUE) 
{
    if ((missing(object)) | (!inherits(object, "qcc"))) 
        stop("an object of class 'qcc' is required")
    if (!(object$type == "xbar" | object$type == "xbar.one")) 
        stop("Process Capability Analysis only available for charts type \"xbar\" and \"xbar.one\" charts")
    x <- as.vector(object$data)
    x <- x[!is.na(x)]
    sizes <- object$sizes
    center <- object$center
    if (missing(std.dev)) 
        std.dev <- object$std.dev
    n <- length(x)
	
	# changed to incorporate overall vs potential (P vs. C conventions that was missing in qcc package)
	if(capability.type == "overall")
	{
		#title <- paste("Overall Process Capability Analysis ( sample size of", max(sizes), ")\nfor", 
		title <- paste("Overall Process Capability Analysis\nfor",
			object$data.name)
	}
	else
	{
		#title <- paste("Potential (Within) Process Capability Analysis ( sample size of", max(sizes), ")\nfor", 
		title <- paste("Potential (Within) Process Capability Analysis\nfor",
			object$data.name)
	}
	
    if (missing(spec.limits)) 
        stop("specification limits must be provided")
    spec.limits <- as.vector(spec.limits)[1:2]
    LSL <- spec.limits[1]
    if (!(is.numeric(LSL) & is.finite(LSL))) 
        LSL <- NA
    USL <- spec.limits[2]
    if (!(is.numeric(USL) & is.finite(USL))) 
        USL <- NA
    if (is.na(LSL) & is.na(USL)) 
        stop("invalid specification limits")
    has.target <- (!missing(target))
    if (!has.target) {
        target <- mean(spec.limits, na.rm = TRUE)
        if (!is.na(LSL) & !is.na(USL)) 
            has.target <- TRUE
    }
    if (is.na(LSL)) {
        if (target > USL) 
            warning("target value larger than one-sided specification limit...")
    }
    if (is.na(USL)) {
        if (target < LSL) 
            warning("target value smaller than one-sided specification limit...")
    }
    if (!is.na(LSL) & !is.na(USL)) {
        if (target < LSL || target > USL) 
            warning("target value is not within specification limits...")
    }
    if (missing(nsigmas)) 
        if (is.null(object$nsigmas)) 
            stop("nsigmas not available in the 'qcc' object. Please provide nsigmas.")
        else nsigmas <- object$nsigmas
    if (confidence.level < 0 | confidence.level > 1) 
        stop("the argument confidence.level must be a value between 0 and 1")
    Cp <- (USL - LSL)/(2 * nsigmas * std.dev)
    Cp.u <- (USL - center)/(nsigmas * std.dev)
    Cp.l <- (center - LSL)/(nsigmas * std.dev)
    Cp.k <- min(Cp.u, Cp.l)
    Cpm <- Cp/sqrt(1 + ((center - target)/std.dev)^2)
    alpha <- 1 - confidence.level
    Cp.limits <- Cp * sqrt(qchisq(c(alpha/2, 1 - alpha/2), n - 
        1)/(n - 1))
    Cp.u.limits <- Cp.u * (1 + c(-1, 1) * qnorm(confidence.level) * 
        sqrt(1/(9 * n * Cp.u^2) + 1/(2 * (n - 1))))
    Cp.l.limits <- Cp.l * (1 + c(-1, 1) * qnorm(confidence.level) * 
        sqrt(1/(9 * n * Cp.l^2) + 1/(2 * (n - 1))))
    Cp.k.limits <- Cp.k * (1 + c(-1, 1) * qnorm(1 - alpha/2) * 
        sqrt(1/(9 * n * Cp.k^2) + 1/(2 * (n - 1))))
    df <- n * (1 + ((center - target)/std.dev)^2)/(1 + 2 * ((center - 
        target)/std.dev)^2)
    Cpm.limits <- Cpm * sqrt(qchisq(c(alpha/2, 1 - alpha/2), 
        df)/df)
    names(Cp.limits) <- names(Cp.k.limits) <- names(Cpm.limits) <- c(paste(round(100 * 
        alpha/2, 1), "%", sep = ""), paste(round(100 * 
        (1 - alpha/2), 1), "%", sep = ""))
    if (is.na(LSL)) 
        exp.LSL <- NA
    else {
        exp.LSL <- pnorm((LSL - center)/std.dev) * 100
        if (exp.LSL < 0.01) 
            exp.LSL <- 0
    }
    if (is.na(USL)) 
        exp.USL <- NA
    else {
        exp.USL <- (1 - pnorm((USL - center)/std.dev)) * 100
        if (exp.USL < 0.01) 
            exp.USL <- 0
    }
    obs.LSL <- sum(x < LSL)/n * 100
    obs.USL <- sum(x > USL)/n * 100
    xlim <- range(x, USL, LSL, target, na.rm = TRUE)
    xlim <- xlim + diff(xlim) * c(-0.1, 0.1)
    xx <- seq(min(xlim), max(xlim), length = 250)
    dx <- dnorm(xx, center, std.dev)
    h <- hist(x, breaks = breaks, plot = FALSE)
    ylim <- range(h$density, dx)
    ylim <- ylim + diff(ylim) * c(0, 0.05)
    tab <- cbind(c(Cp, Cp.l, Cp.u, Cp.k, Cpm), rbind(Cp.limits, 
        Cp.l.limits, Cp.u.limits, Cp.k.limits, Cpm.limits))
	
	# changed to incorporate overall vs potential (P vs. C conventions that was missing in qcc package)
	if(capability.type == "overall")
	{
		rownames(tab) <- c("Pp", "Pp_l", "Pp_u", 
			"Pp_k", "Ppm")
	}
	else
	{
		rownames(tab) <- c("Cp", "Cp_l", "Cp_u", 
			"Cp_k", "Cpm")
	}
	
    colnames(tab) <- c("Value", names(Cp.limits))
    oldpar <- par(no.readonly = TRUE)
    if (restore.par) 
        on.exit(par(oldpar))
    mar <- c(4.1, 2.1, 3.6, 2.1)
    par(bg = qcc.options("bg.margin"), cex = oldpar$cex * 
        qcc.options("cex"), mar = if (add.stats) 
        pmax(mar, c(8.6 + is.null(center) * -1, 0, 0, 0))
    else mar)
    plot(0, 0, type = "n", xlim = xlim, ylim = ylim, axes = FALSE, 
        ylab = "", xlab = "")
    usr <- par()$usr
    rect(usr[1], usr[3], usr[2], usr[4], col = qcc.options("bg.figure"))
    axis(1)
    box()
    top.line <- par("mar")[3] - length(capture.output(cat(title))) - 
        0.5
    mtext(title, side = 3, line = top.line, font = par("font.main"), 
        cex = qcc.options("cex"), col = par("col.main"))
    plot(h, add = TRUE, freq = FALSE)
    abline(v = c(LSL, USL), col = 2, lty = 3, lwd = 2)
    text(LSL, usr[4], "LSL", pos = 3, offset = 0.2, cex = 0.8, 
        xpd = TRUE)
    text(USL, usr[4], "USL", pos = 3, offset = 0.2, cex = 0.8, 
        xpd = TRUE)
    if (has.target) {
        abline(v = target, col = 2, lty = 2, lwd = 2)
        text(target, usr[4], "Target", pos = 3, offset = 0.2, 
            cex = 0.8, xpd = TRUE)
    }
    lines(xx, dx, lty = 2)
    if (add.stats) {
        plt <- par()$plt
        px <- diff(usr[1:2])/diff(plt[1:2])
        xfig <- c(usr[1] - px * plt[1], usr[2] + px * (1 - plt[2]))
        at.col <- xfig[1] + diff(xfig[1:2]) * c(0.07, 0.35, 0.56, 
            0.75)
        top.line <- 3
        mtext(paste("Number of obs = ", n, sep = ""), 
            side = 1, line = top.line, adj = 0, at = at.col[1], 
            font = qcc.options("font.stats"), cex = par("cex") * 
                qcc.options("cex.stats"))
        mtext(paste("Center = ", signif(center, digits), 
            sep = ""), side = 1, line = top.line + 1, adj = 0, 
            at = at.col[1], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
        mtext(paste("StdDev = ", signif(std.dev, digits), 
            sep = ""), side = 1, line = top.line + 2, adj = 0, 
            at = at.col[1], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
        mtext(ifelse(has.target, paste("Target = ", signif(target, 
            digits), sep = ""), paste("Target = ")), 
            side = 1, line = top.line, adj = 0, at = at.col[2], 
            font = qcc.options("font.stats"), cex = par("cex") * 
                qcc.options("cex.stats"))
        mtext(paste("LSL = ", ifelse(is.na(LSL), "", 
            signif(LSL, digits)), sep = ""), side = 1, 
            line = top.line + 1, adj = 0, at = at.col[2], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
        mtext(paste("USL = ", ifelse(is.na(USL), "", 
            signif(USL, digits)), sep = ""), side = 1, 
            line = top.line + 2, adj = 0, at = at.col[2], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
			
		# changed to incorporate overall vs potential (P vs. C conventions that was missing in qcc package)
		if(capability.type == "overall")
		{
			mtext(paste("Pp     = ", ifelse(is.na(Cp), "", 
				signif(Cp, 3)), sep = ""), side = 1, line = top.line, 
				adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			mtext(paste("Pp_l  = ", ifelse(is.na(Cp.l), "", 
				signif(Cp.l, 3)), sep = ""), side = 1, line = top.line + 
				1, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			mtext(paste("Pp_u = ", ifelse(is.na(Cp.u), "", 
				signif(Cp.u, 3)), sep = ""), side = 1, line = top.line + 
				2, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			mtext(paste("Pp_k = ", ifelse(is.na(Cp.k), "", 
				signif(Cp.k, 3)), sep = ""), side = 1, line = top.line + 
				3, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			mtext(paste("Ppm  = ", ifelse(is.na(Cpm), "", 
				signif(Cpm, 3)), sep = ""), side = 1, line = top.line + 
				4, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
		}
		else
		{
						mtext(paste("Cp     = ", ifelse(is.na(Cp), "", 
				signif(Cp, 3)), sep = ""), side = 1, line = top.line, 
				adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			mtext(paste("Cp_l  = ", ifelse(is.na(Cp.l), "", 
				signif(Cp.l, 3)), sep = ""), side = 1, line = top.line + 
				1, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			mtext(paste("Cp_u = ", ifelse(is.na(Cp.u), "", 
				signif(Cp.u, 3)), sep = ""), side = 1, line = top.line + 
				2, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			mtext(paste("Cp_k = ", ifelse(is.na(Cp.k), "", 
				signif(Cp.k, 3)), sep = ""), side = 1, line = top.line + 
				3, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
			mtext(paste("Cpm  = ", ifelse(is.na(Cpm), "", 
				signif(Cpm, 3)), sep = ""), side = 1, line = top.line + 
				4, adj = 0, at = at.col[3], font = qcc.options("font.stats"), 
				cex = par("cex") * qcc.options("cex.stats"))
		}
		
        mtext(paste("Exp<LSL ", ifelse(is.na(exp.LSL), 
            "", paste(signif(exp.LSL, 2), "%", sep = "")), 
            sep = ""), side = 1, line = top.line, adj = 0, 
            at = at.col[4], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
        mtext(paste("Exp>USL ", ifelse(is.na(exp.USL), 
            "", paste(signif(exp.USL, 2), "%", sep = "")), 
            sep = ""), side = 1, line = top.line + 1, adj = 0, 
            at = at.col[4], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
        mtext(paste("Obs<LSL ", ifelse(is.na(obs.LSL), 
            "", paste(signif(obs.LSL, 2), "%", sep = "")), 
            sep = ""), side = 1, line = top.line + 2, adj = 0, 
            at = at.col[4], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
        mtext(paste("Obs>USL ", ifelse(is.na(obs.USL), 
            "", paste(signif(obs.USL, 2), "%", sep = "")), 
            sep = ""), side = 1, line = top.line + 3, adj = 0, 
            at = at.col[4], font = qcc.options("font.stats"), 
            cex = par("cex") * qcc.options("cex.stats"))
    }
    if (print) {
        cat("\nProcess Capability Analysis\n")
        cat("\nCall:\n", deparse(match.call()), "\n\n", 
            sep = "")
        cat(paste(formatC("Number of obs = ", width = 16), 
            formatC(n, width = 12, flag = "-"), formatC("Target = ", 
                width = 10), ifelse(has.target, formatC(signif(target, 
                digits = digits), flag = "-"), ""), 
            "\n", sep = ""))
        cat(paste(formatC("Center = ", width = 16), formatC(signif(center, 
            digits = digits), width = 12, flag = "-"), 
            formatC("LSL = ", width = 10), ifelse(is.na(LSL), 
                "", formatC(signif(LSL, digits = digits), 
                  flag = "-")), "\n", sep = ""))
        cat(paste(formatC("StdDev = ", width = 16), formatC(signif(std.dev, 
            digits = digits), width = 12, flag = "-"), 
            formatC("USL = ", width = 10), ifelse(is.na(USL), 
                "", formatC(signif(USL, digits = digits), 
                  flag = "-")), "\n", sep = ""))
        cat("\nCapability indices:\n\n")
        print(tab, digits = 4, na.print = "", print.gap = 2)
        cat("\n")
        cat(paste("Exp<LSL", ifelse(is.na(exp.LSL), "\t", 
            paste(format(exp.LSL, digits = 2), "%\t", sep = "")), 
            "Obs<LSL", ifelse(is.na(obs.LSL), "", 
                paste(format(obs.LSL, digits = 2), "%\n", 
                  sep = ""))))
        cat(paste("Exp>USL", ifelse(is.na(exp.USL), "\t", 
            paste(format(exp.USL, digits = 2), "%\t", sep = "")), 
            "Obs>USL", ifelse(is.na(obs.USL), "", 
                paste(format(obs.USL, digits = 2), "%\n", 
                  sep = ""))))
    }
    invisible(list(nobs = n, center = center, std.dev = std.dev, 
        target = target, spec.limits = {
            sl <- c(LSL, USL)
            names(sl) <- c("LSL", "USL")
            sl
        }, indices = tab, exp = {
            exp <- c(exp.LSL, exp.USL)/100
            names(exp) <- c("Exp < LSL", "Exp > USL")
            exp
        }, obs = {
            obs <- c(obs.LSL, obs.USL)/100
            names(obs) <- c("Obs < LSL", "Obs > USL")
            obs
        }))
}


##03Sep2022
BSkySetSixSigmaTestOptions <- function(test1 = FALSE, one.point.k.stdv = 3, 
										test2 = FALSE, k.run.same.side = 9, 
										test3 = FALSE, k.run.increase.decrease = 6, 
										test4 = FALSE, k.run.alternating = 14,
										test5 = FALSE, k.plusone.run.beyond.2dev = 2, 
										test6 = FALSE, k.plusone.run.beyond.1dev = 4, 
										test7 = FALSE, k.run.within.1dev = 15, 
										test8 = FALSE, k.run.beyond.1dev = 8,  
										print.summary = FALSE, print.detail = FALSE, 
										digits = 4, 
										optional.data.names = c(), optional.newdata.names = c(),
										either.side = TRUE,
										debug = FALSE)
{
	if(exists("uadatasets.sk"))
	{
		uadatasets.sk$BSkySixSigmaTestOptions = list(test1 = test1, one.point.k.stdv = one.point.k.stdv, 
													test2 = test2, k.run.same.side = k.run.same.side, 
													test3 = test3, k.run.increase.decrease = k.run.increase.decrease, 
													test4 = test4, k.run.alternating = k.run.alternating,
													test5 = test5, k.plusone.run.beyond.2dev = k.plusone.run.beyond.2dev, 
													test6 = test6, k.plusone.run.beyond.1dev = k.plusone.run.beyond.1dev, 
													test7 = test7, k.run.within.1dev = k.run.within.1dev, 
													test8 = test8, k.run.beyond.1dev = k.run.beyond.1dev, 
													either.side = either.side, 
													print.summary = print.summary, print.detail = print.detail,
													digits = digits, 
													optional.data.names = c(), optional.newdata.names = c(),
													debug = debug)
	
	
		return(invisible(uadatasets.sk$BSkySixSigmaTestOptions))
	}
	else
	{
		return(invisible(list()))
	}
}

##03Sep2022
BSkyGetSixSigmaTestOptions <- function()
{
	
	if(exists("uadatasets.sk") && exists("BSkySixSigmaTestOptions", env=uadatasets.sk))
	{
		return(invisible(uadatasets.sk$BSkySixSigmaTestOptions))
	}
	else
	{
		return(invisible(list()))
	}
}

BSkyGetSixSigmaTestsToPerform <- function()
{
	tests = c()
	
	if(uadatasets.sk$BSkySixSigmaTestOptions$test1 == TRUE) tests = c(tests, test1 = TRUE)
	if(uadatasets.sk$BSkySixSigmaTestOptions$test2== TRUE) tests = c(tests, test2 = TRUE)
	if(uadatasets.sk$BSkySixSigmaTestOptions$test3 == TRUE) tests = c(tests, test3 = TRUE)
	if(uadatasets.sk$BSkySixSigmaTestOptions$test4 == TRUE) tests = c(tests, test4 = TRUE)
	if(uadatasets.sk$BSkySixSigmaTestOptions$test5 == TRUE) tests = c(tests, test5 = TRUE)
	if(uadatasets.sk$BSkySixSigmaTestOptions$test6 == TRUE) tests = c(tests, test6 = TRUE)
	if(uadatasets.sk$BSkySixSigmaTestOptions$test7 == TRUE) tests = c(tests, test7 = TRUE)
	if(uadatasets.sk$BSkySixSigmaTestOptions$test8 == TRUE) tests = c(tests, test8 = TRUE)
	
	return(invisible(tests))
}


BSkyResetSixSigmaTestOptionDefaults <- function()
{
	if(exists("uadatasets.sk"))
	{
		uadatasets.sk$BSkySixSigmaTestOptions = list(test1 = FALSE, one.point.k.stdv = 3, 
														test2 = FALSE, k.run.same.side = 9, 
														test3 = FALSE, k.run.increase.decrease = 6, 
														test4 = FALSE, k.run.alternating = 14,
														test5 = FALSE, k.plusone.run.beyond.2dev = 2, 
														test6 = FALSE, k.plusone.run.beyond.1dev = 4, 
														test7 = FALSE, k.run.within.1dev = 15, 
														test8 = FALSE, k.run.beyond.1dev = 8, 
														print.summary = FALSE, print.detail = FALSE, 
														digits = 4, 
														optional.data.names = c(), optional.newdata.names = c(),
														either.side = TRUE, 
														debug = FALSE)
	
	
		return(invisible(uadatasets.sk$BSkySixSigmaTestOptions))
	}
	else
	{
		return(invisible(list()))
	}
}



get.print.violation.indices <- function(object, print.summary = FALSE, print.detail = FALSE)
{
	violationIndices = list()
	
	testOptions = BSkyGetSixSigmaTestOptions()
	
	if(print.summary == FALSE)
	{
		print.summary = testOptions$print.summary
	}
	
	if(print.detail == FALSE)
	{
		print.detail = testOptions$print.detail
	}
	
	
	if(length(testOptions) > 0)
	{
		violationIndices = test.special.causes(object, 
												print.summary = print.summary, 
												print.detail = print.detail,
												test1 = testOptions$test1, one.point.k.stdv = testOptions$one.point.k.stdv, 
												test2 = testOptions$test2, k.run.same.side = testOptions$k.run.same.side, 
												test3 = testOptions$test3, k.run.increase.decrease = testOptions$k.run.increase.decrease, 
												test4 = testOptions$test4, k.run.alternating = testOptions$k.run.alternating,
												test5 = testOptions$test5, k.plusone.run.beyond.2dev = testOptions$k.plusone.run.beyond.2dev, 
												test6 = testOptions$test6, k.plusone.run.beyond.1dev = testOptions$k.plusone.run.beyond.1dev, 
												test7 = testOptions$test7, k.run.within.1dev = testOptions$k.run.within.1dev, 
												test8 = testOptions$test8, k.run.beyond.1dev = testOptions$k.run.beyond.1dev, 
												either.side = testOptions$either.side, 
												digits = testOptions$digits, 
												optional.data.names = testOptions$optional.data.names, optional.newdata.names = testOptions$optional.newdata.names,
												debug = testOptions$debug)
	}
	
	return(invisible(violationIndices))
}


test.special.causes <- function(object, test1 = FALSE, one.point.k.stdv = 3, 
										test2 = FALSE, k.run.same.side = 9, 
                                        test3 = FALSE, k.run.increase.decrease = 6, 
										test4 = FALSE, k.run.alternating = 14,
										test5 = FALSE, k.plusone.run.beyond.2dev = 2, 
										test6 = FALSE, k.plusone.run.beyond.1dev = 4, 
										test7 = FALSE, k.run.within.1dev = 15, 
										test8 = FALSE, k.run.beyond.1dev = 8, 
										print.summary = TRUE, print.detail = TRUE, 
										digits = 4, 
										optional.data.names = c(), optional.newdata.names = c(),
										either.side = TRUE, 
										debug = FALSE)
{
		# if ((missing(object)) | (!inherits(object, "qcc"))) 
			# stop("an object of class `qcc' is required")
		
		if (missing(object))
			stop("an object parameter is required")
			
		violations = list()
		
		if(length(optional.data.names) > 0 && (length(object$statistics) == length(optional.data.names)))
		{
			names(object$statistics) = optional.data.names
		}
		else
		{
			if(length(names(object$statistics)) == 0)
			{
				names(object$statistics) = seq(1:length(object$statistics))
			}
		}
		
		if(length(optional.newdata.names) > 0 && (length(object$newstats) == length(optional.newdata.names)))
		{
			names(object$newstats) = optional.newdata.names
		}
		
		# BSkyFormat(object$statistics)
		
		# if(length(optional.newdata.names) > 0)
		# {
			# BSkyFormat(object$newstats)
		# }
		
		# print("###########################")
		# cat("\nprint\n")
		# print(print)
		# print(str(object))
		# print("=====================")
		
		type <- object$type
		sizes <- object$sizes
		std.dev <- object$std.dev
		center <- object$center
		stats <- object$statistics
		newstats <- object$newstats
		statistics <- c(stats, newstats)
		
		
		violators = violating.runs.indices(object, test1=test1, test2= test2, test3=test3, test4= test4,
												   test5=test5, test6= test6, test7=test7, test8= test8,
												   beyond.kdev.one.point = one.point.k.stdv, run.length = k.run.same.side, 
												   increase.decrease.run.length = k.run.increase.decrease,
		                                           alternating.run.length = k.run.alternating, 
												   beyond.plusone.2dev.run.length = k.plusone.run.beyond.2dev, 
												   beyond.plusone.1dev.run.length = k.plusone.run.beyond.1dev,
												   within.1dev.run.length = k.run.within.1dev, beyond.1dev.run.length = k.run.beyond.1dev,
												   either.side = either.side)
		
		selcted_tests_choices = c("Test 1", "Test 2", "Test 3", "Test 4", "Test 5", "Test 6", "Test 7", "Test 8")
		selcted_tests = selcted_tests_choices[c(test1, test2, test3, test4, test5, test6, test7, test8)]
		selected_test_str = paste(selcted_tests, collapse = ', ')
		
		##############################################################################################
		# preparing the indices for plot.qcc() to color orange (violating.runs) or red (beyond limits)
		##############################################################################################
		
		statistics <- c(object$statistics, object$newstats)
		
		bl <- sort(beyond.limits(object, limits = object$limits))
		#BSkyFormat(bl)
		
		#beyond_limits <- as.numeric(names(statistics)[sort(beyond.limits(object, limits = object$limits))])
		beyond_limits <- as.numeric(names(statistics)[bl])
		# BSkyFormat(beyond_limits)
		# BSkyFormat(which(names(statistics) %in%beyond_limits))
		
		# BSkyFormat(as.numeric(violators$combined_violation_indices))
		# BSkyFormat(which(names(statistics) %in% violators$combined_violation_indices))
		
		#vr_named_indices = sort(setdiff(as.numeric(violators$combined_violation_indices), beyond_limits))
		#vr <- sort(setdiff(as.numeric(which(names(statistics) %in% violators$combined_violation_indices)), bl))
		#vr <- sort(as.numeric(which(names(statistics) %in% violators$combined_violation_indices)))
		vr <- (as.numeric(which(names(statistics) %in% violators$combined_violation_indices)))
		#print(vr)
		
		violations = list(beyond.limits = bl, beyond.limits.named.indices = beyond_limits, 
							violating.runs = vr, 
							violating.runs.named.indices = violators$combined_violation_indices)
		
		#BSkyFormat(rbind(vr, names(violators$combined_violation_indices), violators$combined_violation_indices))
		
		if(print.summary == TRUE)
		{
			if(length(selcted_tests) > 0)
			{
				BSkyFormat(paste("Summary for the selected tests (", selected_test_str ,") performed for special causes in control charts"))
			}
			
			if(length(beyond_limits) > 0)
			{
				BSkyFormat(paste("Beyond", object$nsigmas, "σ limits - violating samples (", paste(violations$beyond.limits.named.indices, collapse=', '), ")"))
			}
			else
			{
				BSkyFormat(paste("Beyond", object$nsigmas, "σ limits - no violating sample found"))
			}
			
			if(length(selcted_tests) > 0)
			{
				if(length(violators$combined_violation_indices) > 0)
				{
					BSkyFormat(paste("Combined sample indices from all special cause tests performed - violating samples (", paste(violators$combined_violation_indices, collapse=', '), ")"))
				}
				else
				{
					BSkyFormat(paste("Combined sample indices from all special cause tests performed - no violating sample found"))
				}
			}
			
			if(debug == TRUE)
			{
				cat("\n====for debug only====\n")
				print(violations)
			}
		}
		
		if(print.detail == TRUE)
		{
			if(length(selcted_tests) > 0)
			{
				BSkyFormat(paste("Details for the selected tests (", selected_test_str ,") performed for special causes in control charts"))
			}
			
			###################
			
			if(print.summary == FALSE)
			{
				if(length(beyond_limits) > 0)
				{
					BSkyFormat(paste("Beyond", object$nsigmas, "σ limits - violating samples (", paste(violations$beyond.limits.named.indices, collapse=', '), ")"))
				}
				else
				{
					BSkyFormat(paste("Beyond", object$nsigmas, "σ limits - no violating sample found"))
				}
			}
			
			###################
			
			if(test1)
			{
				BSkyFormat(paste("Test 1: One point more than", one.point.k.stdv, "σ from center line"))
				if(length(violators$beyond.kdev.one.point.index) == 1)
				{
				  cat("\nonly sample", dimnames(violators$beyond.kdev.one.point.index)[[2]], ">",one.point.k.stdv,"standard deviation from the center line\n")
				  BSkyFormat(violators$beyond.kdev.one.point.index, decimalDigitsRounding = digits)
				}
				else if(length(violators$beyond.kdev.one.point.index) == 0)
				{
					cat("\nno sample found >",one.point.k.stdv,"standard deviation from the center line\n")
				}
				else
				{
					cat("\ninfo only: more than one sample found (", dimnames(violators$beyond.kdev.one.point.index)[[2]], ") >",one.point.k.stdv,"standard deviation from the center line\n")
					BSkyFormat(violators$beyond.kdev.one.point.index, decimalDigitsRounding = digits)
				}
			}
			
			##################
			
			if(test2)
			{
				BSkyFormat(paste("Test 2:", k.run.same.side, "points in a row on the same side of the center line"))
				if(length(violators$run.above.indices) > 0)
				{
					cat("\n",k.run.same.side, "points in a row above the center line - violating samples (", dimnames(violators$run.above.indices)[[2]], ")\n")
					BSkyFormat(violators$run.above.indices, decimalDigitsRounding = digits)
				}
				
				if(length(violators$run.below.indices) > 0)
				{
					cat("\n",k.run.same.side, "points in a row below the center line - violating samples (", dimnames(violators$run.below.indices)[[2]], ")\n")
					BSkyFormat(violators$run.below.indices, decimalDigitsRounding = digits)
				}
				
				if(length(violators$run.below.indices) == 0 && length(violators$run.below.indices) == 0)
				{
					cat("\n",k.run.same.side, "points in a row above or below the center line - no violating sample found\n")
				}
			}
			 
			
			###############
			
			if(test3)
			{
				BSkyFormat(paste("Test 3:", k.run.increase.decrease, "points in a row, all increasing or all decreasing"))
				if(length(violators$increase.decrease.run.indices) > 0)
				{
					cat("\n",k.run.increase.decrease, "points in a row, increasing or decreasing - violating samples (", dimnames(violators$increase.decrease.run.indices)[[2]], ")\n")
					BSkyFormat(violators$increase.decrease.run.indices, decimalDigitsRounding = digits)
				}
				else
				{
					cat("\n",k.run.increase.decrease, "points in a row, increasing or decreasing - no violating sample found\n")
				}
			}
			
			############
			
			if(test4)
			{
				BSkyFormat(paste("Test 4:", k.run.alternating, "points in a row, alternating up and down"))
				if(length(violators$alternate.run.indices) > 0)
				{
					cat("\n",k.run.alternating, "points in a row, alternating up and down - violating samples (", dimnames(violators$alternate.run.indices)[[2]], ")\n")
					BSkyFormat(violators$alternate.run.indices, decimalDigitsRounding = digits) #, outputTableRenames = c(rep("",length(violators$alternate.run.indices))))
				}
				else
				{
					cat("\n",k.run.alternating, "points in a row, alternating up and down - no violating sample found\n")
				}
			}
			
			##################
			
			if(test5)
			{
				BSkyFormat(paste("Test 5:", k.plusone.run.beyond.2dev, "out of", k.plusone.run.beyond.2dev, "+ 1 points more than 2σ from the center line (same side)"))
				if(length(violators$beyond.plusone.2dev.above.indices) > 0)
				{
					cat("\n",k.plusone.run.beyond.2dev, "out of", k.plusone.run.beyond.2dev, "+ 1 points > 2 standard deviation above the center line  - violating samples (", dimnames(violators$beyond.plusone.2dev.above.indices)[[2]], ")\n")
					BSkyFormat(violators$beyond.plusone.2dev.above.indices, decimalDigitsRounding = digits)
				}
				
				if(length(violators$beyond.plusone.2dev.below.indices) > 0)
				{
					cat("\n",k.plusone.run.beyond.2dev, "out of", k.plusone.run.beyond.2dev, "+ 1 points > 2 standard deviation below the center line  - violating samples (", dimnames(violators$beyond.plusone.2dev.below.indices)[[2]], ")\n")
					BSkyFormat(violators$beyond.plusone.2dev.below.indices, decimalDigitsRounding = digits)
				}
				
				if(length(violators$beyond.plusone.2dev.above.indices) == 0 && length(violators$beyond.plusone.2dev.below.indices) == 0)
				{
					cat("\n",k.plusone.run.beyond.2dev, "out of", k.plusone.run.beyond.2dev, "+ 1 points > 2 standard deviation above or below the center line  - no violating sample found\n")
				}
			}
			
			##################
			
			if(test6)
			{
				BSkyFormat(paste("Test 6:", k.plusone.run.beyond.1dev, "out of", k.plusone.run.beyond.1dev, "+ 1 points more than 1σ from the center line (same side)"))
				if(length(violators$beyond.plusone.1dev.above.indices) > 0)
				{
					cat("\n",k.plusone.run.beyond.1dev, "out of", k.plusone.run.beyond.1dev, "+ 1 points > 1 standard deviation above the center line  - violating samples (", dimnames(violators$beyond.plusone.1dev.above.indices)[[2]], ")\n")
					BSkyFormat(violators$beyond.plusone.1dev.above.indices, decimalDigitsRounding = digits)
				}
				
				if(length(violators$beyond.plusone.1dev.below.indices) > 0)
				{
					cat("\n",k.plusone.run.beyond.1dev, "out of", k.plusone.run.beyond.1dev, "+ 1 points > 1 standard deviation below the center line  - violating samples (", dimnames(violators$beyond.plusone.1dev.below.indices)[[2]], ")\n")
					BSkyFormat(violators$beyond.plusone.1dev.below.indices, decimalDigitsRounding = digits)
				}
				
				if(length(violators$beyond.plusone.1dev.above.indices) == 0 && length(violators$beyond.plusone.1dev.below.indices) == 0)
				{
					cat("\n",k.plusone.run.beyond.2dev, "out of", k.plusone.run.beyond.2dev, "+ 1 points > 1 standard deviation above or below the center line  - no violating sample found\n")
				}
			}
			
			################## 
			
			if(test7)
			{
				if(either.side == TRUE)
				{
					BSkyFormat(paste("Test 7:", k.run.within.1dev, "points in a row within 1σ of center line (either side)"))
					if(length(violators$within.1dev.above.indices) > 0)
					{
						cat("\n",k.run.within.1dev, "points in a row, within 1 standard deviation on either side of the center line  - violating samples (", dimnames(violators$within.1dev.above.indices)[[2]], ")\n")
						BSkyFormat(violators$within.1dev.above.indices, decimalDigitsRounding = digits)
					}
					
					# if(length(violators$within.1dev.below.indices) > 0)
					# {
						# cat("\n",k.run.within.1dev, "points in a row, within 1 standard deviation below the center line  - violating samples (", dimnames(violators$within.1dev.below.indices)[[2]], ")\n")
						# BSkyFormat(violators$within.1dev.below.indices)
					# }
					
					if(length(violators$within.1dev.above.indices) == 0 && length(violators$within.1dev.below.indices) == 0)
					{
						cat("\n",k.run.within.1dev, "points in a row, within 1 standard deviation above or below the center line  - no violating sample found\n")
					}
				}
				else
				{
					BSkyFormat(paste("Test 7:", k.run.within.1dev, "points in a row within 1σ of center line (on the same side)"))
					if(length(violators$within.1dev.above.indices) > 0)
					{
						cat("\n",k.run.within.1dev, "points in a row, within 1 standard deviation above the center line  - violating samples (", dimnames(violators$within.1dev.above.indices)[[2]], ")\n")
						BSkyFormat(violators$within.1dev.above.indices, decimalDigitsRounding = digits)
					}
					
					if(length(violators$within.1dev.below.indices) > 0)
					{
						cat("\n",k.run.within.1dev, "points in a row, within 1 standard deviation below the center line  - violating samples (", dimnames(violators$within.1dev.below.indices)[[2]], ")\n")
						BSkyFormat(violators$within.1dev.below.indices, decimalDigitsRounding = digits)
					}
					
					if(length(violators$within.1dev.above.indices) == 0 && length(violators$within.1dev.below.indices) == 0)
					{
						cat("\n",k.run.within.1dev, "points in a row, within 1 standard deviation on either side of the center line  - no violating sample found\n")
					}
				}
			}
				
			###################
			
			if(test8)
			{
				if(either.side == TRUE)
				{
					BSkyFormat(paste("Test 8:", k.run.beyond.1dev, "points in a row more than 1σ from center line (either side)"))
					if(length(violators$beyond.1dev.above.indices) > 0)
					{
						cat("\n",k.run.beyond.1dev, "points in a row, beyond 1 standard deviation on either side of the center line  - violating samples (", dimnames(violators$beyond.1dev.above.indices)[[2]], ")\n")
						BSkyFormat(violators$beyond.1dev.above.indices, decimalDigitsRounding = digits)
					}
					
					# if(length(violators$beyond.1dev.below.indices) > 0)
					# {
						# cat("\n",k.run.beyond.1dev, "points in a row, beyond 1 standard deviation below the center line  - violating samples (", BSkyFormat(violators$beyond.1dev.below.indices)[[2]], ")\n")
						# BSkyFormat(violators$beyond.1dev.below.indices)
					# }
					
					if(length(violators$beyond.1dev.above.indices) == 0 && length(violators$beyond.1dev.below.indices) == 0)
					{
						cat("\n",k.run.beyond.1dev, "points in a row, beyond 1 standard deviation on either side of the center line  - no violating sample found\n")
					}
				}
				else
				{
					BSkyFormat(paste("Test 8:", k.run.beyond.1dev, "points in a row more than 1σ from center line (on the same side)"))
					if(length(violators$beyond.1dev.above.indices) > 0)
					{
						cat("\n",k.run.beyond.1dev, "points in a row, beyond 1 standard deviation above the center line  - violating samples (", dimnames(violators$beyond.1dev.above.indices)[[2]], ")\n")
						BSkyFormat(violators$beyond.1dev.above.indices, decimalDigitsRounding = digits)
					}
					
					if(length(violators$beyond.1dev.below.indices) > 0)
					{
						cat("\n",k.run.beyond.1dev, "points in a row, beyond 1 standard deviation below the center line  - violating samples (", BSkyFormat(violators$beyond.1dev.below.indices)[[2]], ")\n")
						BSkyFormat(violators$beyond.1dev.below.indices, decimalDigitsRounding = digits)
					}
					
					if(length(violators$beyond.1dev.above.indices) == 0 && length(violators$beyond.1dev.below.indices) == 0)
					{
						cat("\n",k.run.beyond.1dev, "points in a row, beyond 1 standard deviation above or below the center line  - no violating sample found\n")
					}
				}
			}
		}

	return(invisible(violations))
}


violating.runs.indices <- function (object, 
									test1 = TRUE, beyond.kdev.one.point = 3, 
									test2 = TRUE, run.length = 9, 
									test3 = TRUE, increase.decrease.run.length = 6, 
									test4 = TRUE, alternating.run.length = 14, 
									test5 = TRUE, beyond.plusone.2dev.run.length = 2, 
									test6 = TRUE, beyond.plusone.1dev.run.length = 4,
									test7 = TRUE, within.1dev.run.length = 15, 
									test8 = TRUE, beyond.1dev.run.length = 8, 
									either.side = TRUE)
{
    
	# if ((missing(object)) | (!inherits(object, "qcc"))) 
			# stop("an object of class `qcc' is required")
	
	combined_violation_indices = c()
	violators <- list()
	
	type = object$type
	limits.func <- paste("limits.", type, sep = "")
	
    center <- object$center
    statistics <- c(object$statistics, object$newstats)
	
	# Test 1: One point more than 3σ from center line
	################################################################################################

	if(test1)
	{
		conf = beyond.kdev.one.point
		limits <- do.call(limits.func, list(center = object$center, std.dev = object$std.dev, 
						sizes = c(object$sizes, object$newsizes), conf = conf))
		lcl <- limits[, 1]
		ucl <- limits[, 2]
		index.above.ucl <- seq(along = statistics)[statistics > ucl]
		index.below.lcl <- seq(along = statistics)[statistics < lcl]
		#violators$beyond.kdev.one.point.index = names(statistics)[c(index.above.ucl,index.below.lcl)]
		
		above_below_indices = sort(c(index.above.ucl,index.below.lcl))
		
		if(length(above_below_indices) == 1)
		{
			old.names = names(combined_violation_indices)
			combined_violation_indices = c(combined_violation_indices, names(statistics)[c(index.above.ucl,index.below.lcl)])
			names(combined_violation_indices) = c(old.names, rep('T1', length(c(index.above.ucl,index.below.lcl))))
		}
		
		violators$beyond.kdev.one.point.index = matrix(statistics[above_below_indices], nrow =1)
		dimnames(violators$beyond.kdev.one.point.index)[[2]] = names(statistics)[above_below_indices]
		dimnames(violators$beyond.kdev.one.point.index)[[1]] = "sample value"
	}
	
	
	# Test 2: Nine points in a row on the same side of the center line
    ################################################################################################

	if(test2)
	{
		diffs <- statistics - center
		diffs[diffs > 0] <- 1
		diffs[diffs < 0] <- -1
		runs <- rle(diffs)
		
		vruns <- rep(runs$lengths >= run.length, runs$lengths)
		vruns.above <- (vruns & (diffs > 0))
		vruns.below <- (vruns & (diffs < 0))
		rvruns.above <- rle(vruns.above)
		rvruns.below <- rle(vruns.below)
		
		vbeg.above <- cumsum(rvruns.above$lengths)[rvruns.above$values] -
			(rvruns.above$lengths - run.length)[rvruns.above$values]
		vend.above <- cumsum(rvruns.above$lengths)[rvruns.above$values]
		
		vbeg.below <- cumsum(rvruns.below$lengths)[rvruns.below$values] -
			(rvruns.below$lengths - run.length)[rvruns.below$values]
		vend.below <- cumsum(rvruns.below$lengths)[rvruns.below$values]
		
		
		run.above.indices = numeric()
		run.below.indices = numeric()
		violators$run.above.indices = numeric()
		violators$run.below.indices = numeric()
		
		if (length(vbeg.above)) 
		{
			for (i in 1:length(vbeg.above)) run.above.indices <- c(run.above.indices,
				vbeg.above[i]:vend.above[i])
		}
		
		if (length(vbeg.below)) 
		{
			for (i in 1:length(vbeg.below)) run.below.indices <- c(run.below.indices,
				vbeg.below[i]:vend.below[i])
		}
		
		old.names = names(combined_violation_indices)
		combined_violation_indices = c(combined_violation_indices, names(statistics)[run.above.indices])
		names(combined_violation_indices) = c(old.names, rep('T2', length(run.above.indices)))
		
		violators$run.above.indices = matrix(statistics[run.above.indices], nrow = 1)
		dimnames(violators$run.above.indices)[[2]] = names(statistics)[run.above.indices]
		dimnames(violators$run.above.indices)[[1]] = "sample value (above)"
		
		old.names = names(combined_violation_indices)
		combined_violation_indices = c(combined_violation_indices, names(statistics)[run.below.indices])
		names(combined_violation_indices) = c(old.names, rep('T2', length(run.below.indices)))
		
		violators$run.below.indices = matrix(statistics[run.below.indices], nrow = 1)
		dimnames(violators$run.below.indices)[[2]] = names(statistics)[run.below.indices]
		dimnames(violators$run.below.indices)[[1]] = "sample value (below)"
	}
	
	
	# Test 3: Six points in a row, all increasing or all decreasing
	################################################################################################
	
	if(test3)
	{
		start.point.padded.statistics = c(statistics, (statistics[length(statistics)]+1) )
		diffs = diff(start.point.padded.statistics)
		#diffs[(diffs > 0 | diffs == 0)]<- 1
		diffs[diffs > 0]<- 1
		diffs[diffs < 0] <- -1
		
		increase.decrease.runs = diffs
		names(increase.decrease.runs) = names(statistics)
		
		# BSkyFormat(statistics)
		# BSkyFormat(increase.decrease.runs)
		
		violators$increase.decrease.runs = numeric()
		increase.decrease.run.indices = numeric()
		
		num.stat.points = length(diffs)
		
		if(num.stat.points >= increase.decrease.run.length)
		{
			for(i in 1:(length(increase.decrease.runs)-(increase.decrease.run.length -2)))
			{
				runs <- rle(increase.decrease.runs[i:(i+ (increase.decrease.run.length -2))])
				
				if(all(runs$lengths >= (increase.decrease.run.length -1)))
				{
					increase.decrease.run.indices = c(increase.decrease.run.indices, (i+ (increase.decrease.run.length -1)))
				}
			}
		
			increase.decrease.run.indices = increase.decrease.run.indices[increase.decrease.run.indices <= length(increase.decrease.runs)]
		}
		
		# BSkyFormat(increase.decrease.run.indices)
		# BSkyFormat(names(statistics)[increase.decrease.run.indices])
		
		old.names = names(combined_violation_indices)
		combined_violation_indices = c(combined_violation_indices, names(statistics)[increase.decrease.run.indices])
		names(combined_violation_indices) = c(old.names, rep('T3', length(increase.decrease.run.indices)))
		
		violators$increase.decrease.run.indices = matrix(statistics[increase.decrease.run.indices], nrow = 1)
		dimnames(violators$increase.decrease.run.indices)[[2]] = names(statistics)[increase.decrease.run.indices]
		dimnames(violators$increase.decrease.run.indices)[[1]] = "sample value"
	}


	# Test 4: Fourteen points in a row, alternating up and down
	#########################################################################################################
	
	if(test4)
	{
		start.point.padded.statistics = c(statistics, (statistics[length(statistics)]+1) )
		diffs = diff(start.point.padded.statistics)
		#diffs[(diffs > 0 | diffs == 0)]<- 1
		diffs[diffs > 0]<- 1
		diffs[diffs < 0] <- -1
		#runs <- rle(diffs)
		
		# BSkyFormat(start.point.padded.statistics)
		# BSkyFormat(diffs)
		
		alternating.runs = diffs
		names(alternating.runs) = names(statistics)
		
		# BSkyFormat(statistics)
		# BSkyFormat(alternating.runs)
		
		violators$alternate.run.indices = numeric()
		alternate.run.indices = numeric()
		
		num.stat.points = length(diffs)
		
		if(num.stat.points >= alternating.run.length)
		{
			for(i in 1:(length(alternating.runs)-(alternating.run.length -2)))
			{
				runs <- rle(alternating.runs[i:(i+ (alternating.run.length -2))])
				
				if(all(runs$lengths == 1))
				{
						alternate.run.indices = c(alternate.run.indices, (i+ (alternating.run.length -1)))
				}
			}
		}
		
		alternate.run.indices = alternate.run.indices[alternate.run.indices <= length(alternating.runs)]
		
		# BSkyFormat(alternate.run.indices)
		# BSkyFormat(names(statistics)[alternate.run.indices])
		
		old.names = names(combined_violation_indices)
		combined_violation_indices = c(combined_violation_indices, names(statistics)[alternate.run.indices])
		names(combined_violation_indices) = c(old.names, rep('T4', length(alternate.run.indices)))
		
		violators$alternate.run.indices = matrix(statistics[alternate.run.indices], nrow = 1)
		dimnames(violators$alternate.run.indices)[[2]] = names(statistics)[alternate.run.indices]
		dimnames(violators$alternate.run.indices)[[1]] = "sample value"
		
		# print(dim(violators$alternate.run.indices))
		# print(dimnames(violators$alternate.run.indices))
		# BSkyFormat(violators$alternate.run.indices)
	}

	# Test 5: Two out of three points more than 2σ from the center line (same side)
	################################################################################################
	
	if(test5)
	{
		conf = 2
		limits <- do.call(limits.func, list(center = object$center, std.dev = object$std.dev, 
						sizes = c(object$sizes, object$newsizes), conf = conf))
		lcl <- limits[, 1]
		ucl <- limits[, 2]
		
		diffs <- statistics - center
		diffs[diffs > 0 & statistics > ucl ] <- 1
		diffs[diffs > 0 & statistics <= ucl ] <- 9
		diffs[diffs < 0 & statistics < lcl ] <- -1
		diffs[diffs < 0 & statistics >= lcl ] <- -9
		
		names(diffs) = names(statistics)
		
		plusone.2dev.above.indices = numeric()
		plusone.2dev.below.indices = numeric()
		violators$beyond.plusone.2dev.above.indices = numeric()
		violators$beyond.plusone.2dev.below.indices = numeric()
		
		num.stat.points = length(diffs)
		
		for(i in 1:num.stat.points)
		{
			if((i+ beyond.plusone.2dev.run.length) <= num.stat.points)
			{
				beyond.plusone.2dev.above.indices = which(diffs[i:(i+beyond.plusone.2dev.run.length)] == 1)
				beyond.plusone.2dev.below.indices = which(diffs[i:(i+beyond.plusone.2dev.run.length)] == -1)
				
				if(length(beyond.plusone.2dev.above.indices) >= beyond.plusone.2dev.run.length)
				{
					plusone.2dev.above.indices = c(plusone.2dev.above.indices, names(diffs[i:(i+beyond.plusone.2dev.run.length)])[beyond.plusone.2dev.above.indices])
				}
				
				if(length(beyond.plusone.2dev.below.indices) >= beyond.plusone.2dev.run.length)
				{
					#plusone.2dev.below.indices = c(plusone.2dev.below.indices, beyond.plusone.2dev.below.indices)
					plusone.2dev.below.indices = c(plusone.2dev.below.indices, names(diffs[i:(i+beyond.plusone.2dev.run.length)])[beyond.plusone.2dev.below.indices])
				
				}
			}
		}

		plusone.2dev.above.indices = unique(plusone.2dev.above.indices)
		plusone.2dev.below.indices = unique(plusone.2dev.below.indices)
		
		old.names = names(combined_violation_indices)
		combined_violation_indices = c(combined_violation_indices, plusone.2dev.above.indices)
		names(combined_violation_indices) = c(old.names, rep('T5', length(plusone.2dev.above.indices)))
		
		violators$beyond.plusone.2dev.above.indices = matrix(statistics[plusone.2dev.above.indices], nrow = 1)
		dimnames(violators$beyond.plusone.2dev.above.indices)[[2]] = plusone.2dev.above.indices
		dimnames(violators$beyond.plusone.2dev.above.indices)[[1]] = "sample value (above)"
		
		old.names = names(combined_violation_indices)
		combined_violation_indices = c(combined_violation_indices, plusone.2dev.below.indices)
		names(combined_violation_indices) = c(old.names, rep('T5', length(plusone.2dev.below.indices)))
		
		violators$beyond.plusone.2dev.below.indices = matrix(statistics[plusone.2dev.below.indices], nrow = 1)
		dimnames(violators$beyond.plusone.2dev.below.indices)[[2]] = plusone.2dev.below.indices
		dimnames(violators$beyond.plusone.2dev.below.indices)[[1]] = "sample value (below)"
	}
		

	# Test 6: Four out of five points more than 1σ from center line (same side)
	#########################################################################################################
	
	if(test6)
	{
		conf = 1
		limits <- do.call(limits.func, list(center = object$center, std.dev = object$std.dev, 
						sizes = c(object$sizes, object$newsizes), conf = conf))
		lcl <- limits[, 1]
		ucl <- limits[, 2]
		
		diffs <- statistics - center
		diffs[diffs > 0 & statistics > ucl ] <- 1
		diffs[diffs > 0 & statistics <= ucl ] <- 9
		diffs[diffs < 0 & statistics < lcl ] <- -1
		diffs[diffs < 0 & statistics >= lcl ] <- -9
		
		names(diffs) = names(statistics)
		
		plusone.1dev.above.indices = numeric()
		plusone.1dev.below.indices = numeric()
		
		violators$beyond.plusone.1dev.above.indices = numeric()
		violators$beyond.plusone.1dev.below.indices = numeric()
		
		num.stat.points = length(diffs)
		
		for(i in 1:num.stat.points)
		{
			if((i+ beyond.plusone.1dev.run.length) <= num.stat.points)
			{
				beyond.plusone.1dev.above.indices = which(diffs[i:(i+beyond.plusone.1dev.run.length)] == 1)
				beyond.plusone.1dev.below.indices = which(diffs[i:(i+beyond.plusone.1dev.run.length)] == -1)
				
				if(length(beyond.plusone.1dev.above.indices) >= beyond.plusone.1dev.run.length)
				{
					plusone.1dev.above.indices = c(plusone.1dev.above.indices, names(diffs[i:(i+beyond.plusone.1dev.run.length)])[beyond.plusone.1dev.above.indices])
				}
				
				if(length(beyond.plusone.1dev.below.indices) >= beyond.plusone.1dev.run.length)
				{
					plusone.1dev.below.indices = c(plusone.1dev.below.indices, names(diffs[i:(i+beyond.plusone.1dev.run.length)])[beyond.plusone.1dev.below.indices])
				}
			}
		}

		plusone.1dev.above.indices = unique(plusone.1dev.above.indices)
		plusone.1dev.below.indices = unique(plusone.1dev.below.indices)
		
		old.names = names(combined_violation_indices)
		combined_violation_indices = c(combined_violation_indices, plusone.1dev.above.indices)
		names(combined_violation_indices) = c(old.names, rep('T6', length(plusone.1dev.above.indices)))
		
		violators$beyond.plusone.1dev.above.indices = matrix(statistics[plusone.1dev.above.indices], nrow = 1)
		dimnames(violators$beyond.plusone.1dev.above.indices)[[2]] = plusone.1dev.above.indices
		dimnames(violators$beyond.plusone.1dev.above.indices)[[1]] = "sample value (above)"
		
		old.names = names(combined_violation_indices)
		combined_violation_indices = c(combined_violation_indices, plusone.1dev.below.indices)
		names(combined_violation_indices) = c(old.names, rep('T6', length(plusone.1dev.below.indices)))
		
		violators$beyond.plusone.1dev.below.indices = matrix(statistics[plusone.1dev.below.indices], nrow = 1)
		dimnames(violators$beyond.plusone.1dev.below.indices)[[2]] = plusone.1dev.below.indices
		dimnames(violators$beyond.plusone.1dev.below.indices)[[1]] = "sample value (below)"
	}
		
	
	# Test 7: Fifteen points in a row within 1σ of center line (either side)
	##################################################################################################
	
	if(test7)
	{
		conf = 1
		limits <- do.call(limits.func, list(center = object$center, std.dev = object$std.dev, 
						sizes = c(object$sizes, object$newsizes), conf = conf))
		lcl <- limits[, 1]
		ucl <- limits[, 2]
		
		if(either.side == TRUE)
		{
			diffs <- statistics - center
			diffs[diffs > 0 & statistics > ucl ] <- 9
			diffs[diffs < 0 & statistics < lcl ] <- -9
			diffs[statistics <= ucl & statistics >= lcl] <- 1
		}
		else
		{
			diffs <- statistics - center
			diffs[diffs > 0 & statistics <= ucl ] <- 1
			diffs[diffs > 0 & statistics > ucl ] <- 9
			diffs[diffs < 0 & statistics >= lcl ] <- -1
			diffs[diffs < 0 & statistics < lcl ] <- -9
		}

		runs <- rle(diffs)
			
		vruns <- rep((runs$lengths >= within.1dev.run.length & abs(runs$values) != 9), runs$lengths)
		
		if(either.side == TRUE)
		{
			vruns.above <- (vruns & (diffs > 0 & abs(diffs) != 9))
		}
		else
		{
			vruns.above <- (vruns & (diffs > 0 & diffs != 9))
		}
		
		vruns.below <- (vruns & (diffs < 0 & diffs != -9))
		rvruns.above <- rle(vruns.above)
		rvruns.below <- rle(vruns.below)
		
		vbeg.above <- cumsum(rvruns.above$lengths)[rvruns.above$values] -
				(rvruns.above$lengths - within.1dev.run.length)[rvruns.above$values]
		vend.above <- cumsum(rvruns.above$lengths)[rvruns.above$values]
		vbeg.below <- cumsum(rvruns.below$lengths)[rvruns.below$values] -
				(rvruns.below$lengths - within.1dev.run.length)[rvruns.below$values]
		vend.below <- cumsum(rvruns.below$lengths)[rvruns.below$values]
			
		within.1dev.above.indices = numeric()
		within.1dev.below.indices = numeric()
		
		violators$within.1dev.above.indices = numeric()
		violators$within.1dev.below.indices = numeric()
			
		if (length(vbeg.above)) 
		{
				for (i in 1:length(vbeg.above)) within.1dev.above.indices <- c(within.1dev.above.indices,
					vbeg.above[i]:vend.above[i])
		}
		
		if (length(vbeg.below)) 
		{
				for (i in 1:length(vbeg.below)) within.1dev.below.indices <- c(within.1dev.below.indices,
					vbeg.below[i]:vend.below[i])
		}
		
		old.names = names(combined_violation_indices)
		combined_violation_indices = c(combined_violation_indices, names(statistics)[within.1dev.above.indices])
		names(combined_violation_indices) = c(old.names, rep('T7', length(within.1dev.above.indices)))
		
		violators$within.1dev.above.indices = matrix(statistics[within.1dev.above.indices], nrow = 1)
		dimnames(violators$within.1dev.above.indices)[[2]] = names(statistics)[within.1dev.above.indices]
		dimnames(violators$within.1dev.above.indices)[[1]] = "sample value"
		
		# print(within.1dev.above.indices)
		# print(violators$within.1dev.above.indices)
		# print(dim(violators$within.1dev.above.indices))
		
		old.names = names(combined_violation_indices)
		combined_violation_indices = c(combined_violation_indices, names(statistics)[within.1dev.below.indices])
		names(combined_violation_indices) = c(old.names, rep('T7', length(within.1dev.below.indices)))
		
		violators$within.1dev.below.indices = matrix(statistics[within.1dev.below.indices], nrow = 1)
		dimnames(violators$within.1dev.below.indices)[[2]] = names(statistics)[within.1dev.below.indices]
		dimnames(violators$within.1dev.below.indices)[[1]] = "sample value"
	}
		
	
	# Test 8: Eight points in a row more than 1σ from center line (either side)
	################################################################################################
	
	if(test8)
	{
		conf = 1
		limits <- do.call(limits.func, list(center = object$center, std.dev = object$std.dev, 
						sizes = c(object$sizes, object$newsizes), conf = conf))
		
		lcl <- limits[, 1]
		ucl <- limits[, 2]
		
		if(either.side == TRUE)
		{
			diffs <- statistics - center
			diffs[diffs > 0 & statistics <= ucl ] <- 9
			diffs[diffs < 0 & statistics >= lcl ] <- -9
			diffs[statistics > ucl | statistics < lcl] <- 1
		}
		else
		{
			diffs <- statistics - center
			diffs[diffs > 0 & statistics > ucl ] <- 1
			diffs[diffs > 0 & statistics <= ucl ] <- 9
			diffs[diffs < 0 & statistics < lcl ] <- -1
			diffs[diffs < 0 & statistics >= lcl ] <- -9
		}

		runs <- rle(diffs)
		
		# BSkyFormat(diffs)
		# BSkyFormat(rbind(runs$lengths, runs$values))
		
		vruns <- rep((runs$lengths >= beyond.1dev.run.length & abs(runs$values) != 9), runs$lengths)
		
		#BSkyFormat(as.character(vruns))
		
		if(either.side == TRUE)
		{
			vruns.above <- (vruns & (diffs > 0 & abs(diffs) != 9))
		}
		else
		{
			vruns.above <- (vruns & (diffs > 0 & diffs != 9))
		}
		
		vruns.below <- (vruns & (diffs < 0 & diffs != -9))
		rvruns.above <- rle(vruns.above)
		rvruns.below <- rle(vruns.below)
		vbeg.above <- cumsum(rvruns.above$lengths)[rvruns.above$values] -
			(rvruns.above$lengths - beyond.1dev.run.length)[rvruns.above$values]
		vend.above <- cumsum(rvruns.above$lengths)[rvruns.above$values]
		vbeg.below <- cumsum(rvruns.below$lengths)[rvruns.below$values] -
			(rvruns.below$lengths - beyond.1dev.run.length)[rvruns.below$values]
		vend.below <- cumsum(rvruns.below$lengths)[rvruns.below$values]
		
		beyond.1dev.above.indices = numeric()
		beyond.1dev.below.indices = numeric()
		
		violators$beyond.1dev.above.indices = numeric()
		violators$beyond.1dev.below.indices = numeric()
		
		if (length(vbeg.above)) 
		{
			for (i in 1:length(vbeg.above)) beyond.1dev.above.indices <- c(beyond.1dev.above.indices,
				vbeg.above[i]:vend.above[i])
		}
		
		if (length(vbeg.below)) 
		{
			for (i in 1:length(vbeg.below)) beyond.1dev.below.indices <- c(beyond.1dev.below.indices,
				vbeg.below[i]:vend.below[i])
		}
		
		old.names = names(combined_violation_indices)
		combined_violation_indices = c(combined_violation_indices, names(statistics)[beyond.1dev.above.indices])
		names(combined_violation_indices) = c(old.names, rep('T8', length(beyond.1dev.above.indices)))
		
		violators$beyond.1dev.above.indices = matrix(statistics[beyond.1dev.above.indices], nrow = 1)
		dimnames(violators$beyond.1dev.above.indices)[[2]] = names(statistics)[beyond.1dev.above.indices]
		dimnames(violators$beyond.1dev.above.indices)[[1]] = "sample value"
		
		old.names = names(combined_violation_indices)
		combined_violation_indices = c(combined_violation_indices, names(statistics)[beyond.1dev.below.indices])
		names(combined_violation_indices) = c(old.names, rep('T8', length(beyond.1dev.below.indices)))
		
		violators$beyond.1dev.below.indices = matrix(statistics[beyond.1dev.below.indices], nrow = 1)
		dimnames(violators$beyond.1dev.below.indices)[[2]] = names(statistics)[beyond.1dev.below.indices]
		dimnames(violators$beyond.1dev.below.indices)[[1]] = "sample value"
	}
		
	
	#########################################################################################################
	
	
	#violators$combined_violation_indices = sort(as.numeric(unique(combined_violation_indices)))
	#violators$combined_violation_indices = sort(as.numeric(combined_violation_indices[!duplicated(combined_violation_indices)]))
	old.names = names(combined_violation_indices)
	combined_violation_indices = as.numeric(combined_violation_indices)
	names(combined_violation_indices) = old.names
	
	violators$combined_violation_indices = sort(combined_violation_indices[!duplicated(combined_violation_indices)])
	
	# print(names(combined_violation_indices))
	# print(violators$combined_violation_indices)
	
    return(invisible(violators))
}


		
plot.qcc.spc.phases <- function(data, data.name = c(), sizes = c(), newdata=c(), newdata.name = c(), newsizes = c(), 
								phases.data.list = list(), phase.names = c(), 
								type = "xbar", chart.title.name = c(), size.title = c(), xlab = c(), ylab = c(),
                                nsigmas = 3, confidence.level= NA, std.dev = NA, 
								additional.sigma.lines = c(), spec.limits = list(lsl=c(), usl= c()),
								digits =2, 
								print.stats = FALSE, print.test.summary = FALSE, print.test.detail = FALSE,
								print.qcc.object.summary = FALSE,
								mark.test.number = TRUE,
								restore.par = TRUE, extend.plot.range =c(NA,NA))
{
	if (missing(data)) 
        stop("data is required")
	
	orig_type = type[1]
	if(type[1] == 'MR')
	{
		type[1] = 'R'
	}
	
	if(length(chart.title.name) == 0)
	{
		chart.title.name = type[1]
	}
		
	if (!(type[1] %in% c("xbar", "R", "S", "xbar.one", "p", "np", "c", "u"))) 
        stop("Chart type allowed - xbar, R, S, xbar.one, p, np, c, u")
	
	if(length(additional.sigma.lines) > 0)
	{
		additional.sigma.lines = additional.sigma.lines[additional.sigma.lines > 0 & additional.sigma.lines <= 3 & additional.sigma.lines != nsigmas]
	}
	
	#cat("\n Sigma Lines ", additional.sigma.lines, "\n")
	
	if(length(phase.names) == 1 && trimws(phase.names) == '')
	{
		phase.names = c()
	}
	
	oldpar <- par(no.readonly = TRUE)
    if (restore.par) 
        on.exit(par(oldpar))
		
	mar <- pmax(oldpar$mar, c(4.1, 4.1, 3.1, 3.1)) #c(bottom, left, top, right) The default is c(5, 4, 4, 2) + 0.1
    #par(bg = qcc.options("bg.margin"), cex = oldpar$cex * qcc.options("cex"), mar = if (print.stats) pmax(mar, c(11.6, 0, 0, 0)) else mar)
	par(bg = qcc.options("bg.margin"), cex = oldpar$cex * qcc.options("cex"), mar = mar)
	
	zero.or.one.phase = TRUE
	
	if(class(data)[1] %in% c("matrix", "data.frame"))
	{
		max_rows = dim(data)[1]
	}	
	else
	{
		max_rows = length(data)
	}
	
	if(length(newdata) > 0)
	{
		if(orig_type == 'MR' && length(newdata) > 1)
		{
			newdata = newdata[1:(length(newdata) - 1)]
		}
			
		newdata = newdata[newdata <= max_rows]
			
		if(length(newsizes) > 0 && length(newsizes) > length(newdata))
		{
			newsizes = newsizes[1: length(newdata)]
		}
	}
	
	if(length(phases.data.list) > 0)
	{
		phases.data.list = phases.data.list[!unlist(lapply(phases.data.list,is.null))]
		
		if(orig_type == 'MR' && length(phases.data.list) > 0)
		{
			#phases.data.list = lapply(phases.data.list,function(x){x[length(x)]= x[length(x)] - 1; x})
			phases.data.list = lapply(phases.data.list,function(x){if(length(x) > 1){x[1:(length(x)-1)]}else x})
			#phases.data.list = lapply(phases.data.list,function(x){if(max(x) < max_rows) {x[1:(length(x)-1)]} else x})
		}
		
		phases.data.list = lapply(phases.data.list,function(x){x[x <= max_rows]})
		
		#print(phases.data.list)
		 
		if(length(phases.data.list) > 1)
		{
			zero.or.one.phase = FALSE
			
			if(length(newdata) > 0)
			{
				cat('\n\nFor multi phase chart - the newdata will be ignored. newdata is supported for chart with no additional phase\n')
				newdata = c()
				newsizes = c()
			}
		} 
	}
	
	if(length(data.name) == 0)
	{
		data.name = deparse(substitute(data)) 
	}
	
	if(length(newdata) > 0 && length(newdata.name) == 0)
	{
		newdata.name = deparse(substitute(newdata)) 
	}
	
	if(class(data)[1] %in% c("matrix", "data.frame"))
	{	
		if(length(phases.data.list) == 0)
		{
			phases.data.list = list(c(1:dim(data)[1]))
			all.samples = unique(Reduce(c, phases.data.list))
			
			if(length(dimnames(data)[1]) > 0)
			{
				names(all.samples) = as.numeric(dimnames(data)[[1]])
				names(phases.data.list[[1]]) = names(all.samples)
			}
			else
			{
				names(all.samples) = all.samples
			}
		}
		else
		{
			all.samples = unique(Reduce(c, phases.data.list))
			names(all.samples) = all.samples
		}
        
		all.data = data[c(all.samples),]
		
		if(length(sizes) > 0)
		{
			all.sizes = sizes[c(all.samples)]
		}
		else #if(type == 'c')
		{
			all.sizes = apply(all.data, 1, function(x) sum(!is.na(x))) #sizes <- apply(data, 1, function(x) sum(!is.na(x)))
		}
		# else
		# {
			# all.sizes = dim(all.data)[2]
		# }
	}
	else
	{
		if(length(phases.data.list) == 0)
		{
			phases.data.list = list(c(1:length(data)))
		}
		
		all.samples = unique(Reduce(c, phases.data.list))
		names(all.samples) = all.samples
		
		all.data = data[c(all.samples)]
		
		if(length(sizes) > 0)
		{
			all.sizes = sizes[c(all.samples)]
		}
		else #if(type == 'c')
		{
			all.sizes <- rep(1,length(all.data))
		}
		# else
		# {
			# all.sizes <- rep(1,length(all.data))
		# }
	}
	
	if(length(unique(all.sizes)) == 1)
	{
		if(length(newdata) == 0)
		{
			#if(chart.title.name == 'MR') 
			if(length(size.title) > 0 && trimws(size.title) == "")
			{
				main.title <- paste(chart.title.name, "Chart\nfor", data.name)
			}
			else
			{
				main.title <- paste(chart.title.name, "Chart\nfor", data.name, "( sample size of", all.sizes[1],")")
			}
		}
		else
		{
			#if(chart.title.name == 'MR')
			if(length(size.title) > 0 && trimws(size.title) == "")
			{
				main.title <- paste(chart.title.name, "Chart\nfor", data.name, "with new data")
			}
			else
			{
				main.title <- paste(chart.title.name, "Chart\nfor", data.name, "with new data ( sample size of", all.sizes[1],")")
			}
		}
	}
	else
	{
		if(length(newdata) == 0)
		{
			if(length(size.title) > 0 && trimws(size.title) == "")
			{
				main.title <- paste(chart.title.name, "Chart\nfor", data.name)
			}
			else
			{
				main.title <- paste(chart.title.name, "Chart\nfor", data.name, "( variable sample sizes between", min(all.sizes),"and", max(all.sizes), ")")
			}
		}
		else
		{
			if(length(size.title) > 0 && trimws(size.title) == "")
			{
				main.title <- paste(chart.title.name, "Chart\nfor", data.name, "with new data")
			}
			else
			{
				main.title <- paste(chart.title.name, "Chart\nfor", data.name, "with new data( variable sample sizes between", min(all.sizes),"and", max(all.sizes), ")")
			}
		}
	}
	
	
	all.statistics = c()
	all.limits = c()
	all.center = c()
	
	for(i in 1:length(phases.data.list))
	{
		if(length(phases.data.list[[i]]) > 0)
		{
			if(class(data)[1] %in% c("matrix", "data.frame"))
			{
				data.i = data[c(phases.data.list[[i]]),]
				
				if(length(sizes) > 0)
				{
					sizes.i = sizes[c(phases.data.list[[i]])]
				}
				else #if(type == 'c')
				{
					sizes.i <- apply(data.i, 1, function(x) sum(!is.na(x)))
				}
			}
			else
			{
				data.i = data[c(phases.data.list[[i]])]
				
				if(length(sizes) > 0)
				{
					sizes.i = sizes[c(phases.data.list[[i]])]
				}
				else #if(type == 'c')
				{
					sizes.i <- rep(1, length(data.i))
				}
			}
		
			if(length(newdata) > 0 && i ==1)
			{
				if(class(data)[1] %in% c("matrix", "data.frame"))
				{
					newdata.i = data[c(newdata),]
					
					if(length(newsizes) == 0)
					{
						newsizes.i = apply(newdata.i, 1, function(x) sum(!is.na(x)))
					}
				}
				else
				{	
					newdata.i = data[c(newdata)]
					
					if(length(newsizes) > 0)
					{
						newsizes.i = newsizes
					}
					if(length(sizes) == 0)
					{
						newsizes.i = rep(1, length(newdata.i))
					}
					else
					{
						newsizes.i = sizes[c(newdata)]
					}
				}
				
				qcc.object.i = compute.qcc.statistics(data = data.i, sizes = sizes.i, newdata = newdata.i, newsizes = newsizes.i,
												type = type, digits = digits,
												nsigmas = nsigmas, confidence.level= confidence.level, std.dev = std.dev)
			}
			else
			{
				qcc.object.i = compute.qcc.statistics(data = data.i, sizes = sizes.i, type = type, digits = digits,
												nsigmas = nsigmas, confidence.level= confidence.level, std.dev = std.dev)
			}
			
			all.statistics = c(all.statistics, qcc.object.i$statistics, qcc.object.i$newstats)
			all.limits = c(all.limits, qcc.object.i$limits)
			all.center = c(all.center, qcc.object.i$center)
		}
	}
	
	#cat("\n",all.statistics, "\nLimits\n",all.limits, "Center\n", all.center,"\n")
	
	axes.las = 0
	
	ylim.range = range(all.statistics, all.limits, all.center, Reduce(c, spec.limits), na.rm = TRUE) 
	
	if(!is.na(extend.plot.range[1]) && extend.plot.range[1] < ylim.range[1]) ylim.range[1] = extend.plot.range[1]
	if(!is.na(extend.plot.range[2]) && extend.plot.range[2] > ylim.range[2]) ylim.range[2] = extend.plot.range[2]

	#ylim.range[1] = ylim.range[1]* 0.75 # - (ylim.range[2] - ylim.range[1])* 0.75
	#ylim.range[2] = ylim.range[2]* 0.75  #+ (ylim.range[2] - ylim.range[1])* 0.75
	
	
	all.samples.names = names(all.samples)
	all.samples = c(all.samples, newdata)
	
	names(all.statistics) = c(all.samples.names, newdata)
	all.indices = 1:length(all.samples)
	names(all.indices) = c(all.samples.names, newdata)
	
	if(length(xlab) == 0)
	{
		xlab = "Group"
	}
	
	if(length(ylab) == 0)
	{
		ylab = "Group summary statistics"
	}
	
    plot(all.indices, all.statistics, type = "n", 
		ylim = ylim.range, 
		ylab = ylab, 
		xlab = xlab, axes = FALSE)
		
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = qcc.options("bg.figure")) # qcc.options("bg.figure") is white
	
    #axis(1, at = all.indices, las = axes.las, 
	#	labels = if (is.null(names(all.statistics))) as.character(all.indices) else names(all.statistics))
	
	axis(1, at = all.indices, las = axes.las, labels = c(all.samples.names, newdata)) #all.samples
		
    axis(2, las = axes.las)
	
    box()
	
    top.line <- par("mar")[3] - length(capture.output(cat(main.title)))
	
    top.line <- top.line -  0.5
	
    mtext(main.title, side = 3, line = top.line, font = par("font.main"), 
        cex = qcc.options("cex"), col = par("col.main"))
		
	if(length(spec.limits) > 0 && !is.null(spec.limits$lsl) && !is.na(spec.limits$lsl))
	{
		abline(h = spec.limits$lsl, lty = 3, lwd = 2, col = gray(0.3))
		
		mtext("Lspec", side = 4, at = spec.limits$lsl, 
			las = 1, line = 0.1, col = gray(0.3), cex = par("cex"))
		
		text(x = length(all.samples)/2, y = spec.limits$lsl, label = paste("Lower Spec","\n",round(spec.limits$lsl, digits),sep=""),
					col = gray(0.3),   # Color of the text
					font = 2,      # Bold face
					cex = par("cex") * 0.8)     # Size
	}
	
	if(length(spec.limits) > 0 && !is.null(spec.limits$usl) && !is.na(spec.limits$usl))
	{
		abline(h = spec.limits$usl, lty = 3, lwd = 2, col = gray(0.3))
		
		mtext("Uspec", side = 4, at = spec.limits$usl, 
			las = 1, line = 0.1, col = gray(0.3), cex = par("cex"))
		
		text(x = length(all.samples)/2, y = spec.limits$usl, label = paste("Upper Spec","\n",round(spec.limits$usl, digits),sep=""),
					col = gray(0.3),   # Color of the text
					font = 2,      # Bold face
					cex = par("cex") * 0.8)     # Size
	}
	
	
	# plt <- par()$plt
	# usr <- par()$usr
	# px <- diff(usr[1:2])/diff(plt[1:2])
	# xfig <- c(usr[1] - px * plt[1], usr[2] + px * (1 - plt[2]))
	# at.col <- xfig[1] + diff(xfig[1:2]) * c(0.1, 0.4, 0.65)
	# top.line <- 4.5
	
	phase.summary.info = c()
	
	object.list = list()
				
	cum.indices = c()
	
	for(i in 1:length(phases.data.list))
	{
		if(length(phases.data.list[[i]]) > 0)
		{
			if(class(data)[1] %in% c("matrix", "data.frame"))
			{
				data.i = data[c(phases.data.list[[i]]),]
				
				if(length(sizes) > 0)
				{
					sizes.i = sizes[c(phases.data.list[[i]])]
				}
				else if(type == 'c')
				{
					sizes.i <- apply(data.i, 1, function(x) sum(!is.na(x)))
				}
			}
			else
			{
				data.i = data[c(phases.data.list[[i]])]
				
				if(length(sizes) > 0)
				{
					sizes.i = sizes[c(phases.data.list[[i]])]
				}
				else #if(type == 'c')
				{
					sizes.i <- rep(1, length(data.i))
				}
			}
			
			if(length(newdata) > 0 && i ==1)
			{
				if(class(data)[1] %in% c("matrix", "data.frame"))
				{
					newdata.i = data[c(newdata),]
					
					if(length(newsizes) == 0)
					{
						newsizes.i = apply(newdata.i, 1, function(x) sum(!is.na(x)))
					}
				}
				else
				{	
					newdata.i = data[c(newdata)]
					
					if(length(newsizes) > 0)
					{
						newsizes.i = newsizes
					}
					if(length(sizes) == 0)
					{
						newsizes.i = rep(1, length(newdata.i))
					}
					else
					{
						newsizes.i = sizes[c(newdata)]
					}
				}
				
				qcc.object.i = compute.qcc.statistics(data = data.i, sizes = sizes.i, newdata = newdata.i, newsizes = newsizes.i,
												type = type, digits = digits,
												nsigmas = nsigmas, confidence.level= confidence.level, std.dev = std.dev)
				
				names(qcc.object.i$newdata)= newdata
			}
			else
			{
				qcc.object.i = compute.qcc.statistics(data = data.i, sizes = sizes.i, type = type, digits = digits,
												nsigmas = nsigmas, confidence.level= confidence.level, std.dev = std.dev)
			}
			
			#cat("\nChart type: ", type, "\n")
			if(length(names(phases.data.list[[i]])) > 0)
			{
				names(qcc.object.i$statistics)= names(phases.data.list[[i]])
			}
			else
			{
				names(qcc.object.i$statistics)= phases.data.list[[i]]
			}
			#plot(qcc.object.i)
			
			object <- qcc.object.i
			
			type <- object$type
			object.std.dev <- object$std.dev
			data.name <- object$data.name
			center <- object$center
			stats <- c(object$statistics, object$newstats)
			limits <- object$limits
	
			lcl <- limits[, 1] #unique(limits[, 1])
			ucl <- limits[, 2] #unique(limits[, 2])
			violations <- object$violations
			#violations$violating.runs = c() #Null all other viaolations except hightlight the points in red beyond the LCL and UCL 
			
			indices = (length(cum.indices) + 1): (length(cum.indices) + length(stats))
			
			# cat("\n", length(indices), "\n")
			# BSkyFormat(indices)
			# cat("\n", length(stats), "\n")
			# BSkyFormat(stats)
			
			lines(indices, stats, type = "b", pch = 20)
			
			if (length(violations$violating.runs)) 
			{
				v <- violations$violating.runs
				points(indices[v], stats[v], col = qcc.options("violating.runs")$col, 
					pch = qcc.options("violating.runs")$pch)
					
				#print(names(violations$violating.runs.named.indices))
				
				if(mark.test.number == TRUE)
				{
					text(indices[v]+0.5, stats[v], label = names(violations$violating.runs.named.indices),
						col = gray(0.3),   # Color of the text
						font = 2,      # Bold face
						cex = par("cex") * 0.8)
				}
			}
	
			if (length(violations$beyond.limits)) 
			{
				v <- violations$beyond.limits
				points(indices[v], stats[v], col = qcc.options("beyond.limits")$col, 
					pch = qcc.options("beyond.limits")$pch)
			}
			
			if(zero.or.one.phase == FALSE)
			{
				lcl.label = paste("LCL.",i,sep="")
				ucl.label = paste("UCL.",i,sep="")
			}
			else
			{
				lcl.label = paste("LCL",sep="")
				ucl.label = paste("UCL",sep="")
			}
			
			label.limits = c(lcl.label, ucl.label)
			
			if (length(unique(lcl)) == 1) 
			{
				lcl = unique(lcl)
				
				#abline(h = lcl, lty = 2, col = "red")
				segments(x0= length(cum.indices), y0=lcl, x1= length(cum.indices) + length(stats), y1= lcl, col= 'red') #x0, y0, x1 = x0, y1 = y0
				
				#mtext(label.limits, side = 4, at = c(rev(lcl)[1], rev(ucl)[1]), 
			#	las = 1, line = 0.1, col = gray(0.3), cex = par("cex"))
			
				text(x = length(cum.indices)+length(stats)/4, y = lcl, label = paste(lcl.label,"\n",round(lcl,digits),sep=""),
						col = gray(0.3),   # Color of the text
						font = 2,      # Bold face
						cex = par("cex") * 0.8)     # Size
			}
			else 
			{
				#lines(indices, lcl[indices], type = "s", lty = 2, col= 'red')
				lines(indices, lcl, type = "s", lty = 2, col= 'red')
				
				text(x = length(cum.indices)+length(stats)/4, y = min(lcl), label = paste(lcl.label,"\nvariable",sep=""),
						col = gray(0.3),   # Color of the text
						font = 2,      # Bold face
						cex = par("cex") * 0.8)     # Size
			}
			
			if (length(unique(ucl)) == 1) 
			{
				ucl = unique(ucl)
				
				##abline(h = ucl, lty = 2, col = "red")
				segments(x0= length(cum.indices), y0=ucl, x1= length(cum.indices) + length(stats), y1= ucl, col= 'red') #x0, y0, x1 = x0, y1 = y0
			
				#mtext(label.limits, side = 4, at = c(rev(lcl)[1], rev(ucl)[1]), 
			#	las = 1, line = 0.1, col = gray(0.3), cex = par("cex"))
						
				text(x = length(cum.indices)+length(stats)/4, y = ucl, label = paste(ucl.label,"\n",round(ucl,digits),sep=""),
						col = gray(0.3),   # Color of the text
						font = 2,      # Bold face
						cex = par("cex") * 0.8)     # Size
			}
			else 
			{
				
				#lines(indices, ucl[indices], type = "s", lty = 2, col= 'red')
				lines(indices, ucl, type = "s", lty = 2, col= 'red')
						
				text(x = length(cum.indices)+length(stats)/4, y = max(ucl), label = paste(ucl.label,"\nvariable",sep=""),
						col = gray(0.3),   # Color of the text
						font = 2,      # Bold face
						cex = par("cex") * 0.8)     # Size
			}
			
			if(length(additional.sigma.lines) > 0)
			{
				for(x in 1:length(additional.sigma.lines))
				{
					limits = paste("limits.", type, sep = "")
			
					warn.limits = do.call(limits, list(center = center, std.dev = object.std.dev, sizes = c(object$sizes, object$newsizes), conf = additional.sigma.lines[x]))
					
					warn.lcl <- warn.limits[, 1]
					warn.ucl <- warn.limits[, 2]
					
					
					if(zero.or.one.phase == FALSE)
					{
						warn.lcl.label = paste("LCL ",additional.sigma.lines[x],"s",sep="")
						warn.ucl.label = paste("UCL ",additional.sigma.lines[x],"s",sep="")
					}
					else
					{
						warn.lcl.label = paste("LCL ",additional.sigma.lines[x],"s",sep="")
						warn.ucl.label = paste("UCL ",additional.sigma.lines[x],"s",sep="")
					}
					
					if (length(unique(warn.lcl)) == 1) 
					{
						warn.lcl = unique(warn.lcl)
						
						segments(x0= length(cum.indices), y0=warn.lcl, x1= length(cum.indices) + length(stats), y1= warn.lcl, lty = 3, col= 'black') #x0, y0, x1 = x0, y1 = y0
						
						text(x = length(cum.indices)+length(stats)/8, y = warn.lcl, label = paste(warn.lcl.label,"\n",round(warn.lcl,digits),sep=""),
								col = gray(0.3),   # Color of the text
								font = 2,      # Bold face
								cex = par("cex") * 0.8)     # Size
					}
					else 
					{
						lines(indices, warn.lcl, type = "s", lty = 2, col= 'black')
						
						text(x = length(cum.indices)+length(stats)/8, y = min(warn.lcl), label = paste(warn.lcl.label), #"\nvariable",sep=""),
								col = gray(0.3),   # Color of the text
								font = 2,      # Bold face
								cex = par("cex") * 0.8)     # Size
					}
					
					if (length(unique(warn.ucl)) == 1) 
					{
						warn.ucl = unique(warn.ucl)
						
						segments(x0= length(cum.indices), y0=warn.ucl, x1= length(cum.indices) + length(stats), y1= warn.ucl, lty = 3, col= 'black') 
								
						text(x = length(cum.indices)+length(stats)/8, y = warn.ucl, label = paste(warn.ucl.label,"\n",round(warn.ucl,digits),sep=""),
								col = gray(0.3),   # Color of the text
								font = 2,      # Bold face
								cex = par("cex") * 0.8)     # Size
					}
					else 
					{
						lines(indices, warn.ucl, type = "s", lty = 2, col= 'black')
								
						text(x = length(cum.indices)+length(stats)/8, y = max(warn.ucl), label = paste(warn.ucl.label), #"\nvariable",sep=""),
								col = gray(0.3),   # Color of the text
								font = 2,      # Bold face
								cex = par("cex") * 0.8)     # Size
					}
				}
			}
			
			if(zero.or.one.phase == FALSE)
			{
				center.label = paste("CL.",i, sep="")
			}
			else
			{
				center.label = paste("CL",sep="")
			}
			
			if (length(unique(center)) == 1)
			{
				center = unique(center)
				
				#abline(h = center)
				segments(x0= length(cum.indices), y0=center, x1= length(cum.indices) + length(stats), y1= center, col= 'green') #x0, y0, x1 = x0, y1 = y0
				
				text(x = length(cum.indices)+length(stats)/2, y = (center+ min(ucl))/2, label = paste(center.label,"\n",round(center,digits),sep=""),
					col = gray(0.3),   # Color of the text
					font = 2,      # Bold face
					cex = par("cex") * 0.8)     # Size
			}
			else 
			{
				#lines(indices, center[indices], type = "s", col= 'green')
				lines(indices, center, type = "s", col= 'green')
				
				text(x = length(cum.indices)+length(stats)/2, y = (max(center) + min(ucl))/2, label = paste(center.label,"\nvariable"),
					col = gray(0.3),   # Color of the text
					font = 2,      # Bold face
					cex = par("cex") * 0.8)     # Size
			}
			
			#mtext(paste("CL.",i,sep=""), side = 4, at = rev(center)[1], las = 1, 
			#	line = 0.1, col = gray(0.3), cex = par("cex"))
			
			
			if(zero.or.one.phase == FALSE)
			{
				if(length(phase.names) == 0 || i > length(phase.names))
				{
					phase.name = paste("Phase ", i)
				}
				else 
				{
					phase.name = phase.names[i]
				}
				
				
				if(i < length(phases.data.list))
				{
					abline(v = length(c(cum.indices, indices)) + 0.1, lty = 3, col="blue")
					
					mtext(phase.name, cex = par("cex") * 
							0.8, at = length(cum.indices)+length(stats)/2, line = 0, adj = 0.5)
				}
				else
				{
					mtext(phase.name, cex = par("cex") * 
						0.8, at = length(cum.indices)+length(stats)/2, line = 0, adj = 0.5)
				}
			}
			
			if(i == 1 && length(newdata) > 0)
			{
				abline(v = length(object$statistics) + 0.5, lty = 3, lwd = 1.5, col="brown")
				
				mtext("Calibration data", cex = par("cex") * 
					0.8, at = length(object$statistics)/8, line = -1, adj = 0.5)
				
				mtext("New data", cex = par("cex") * 0.8, 
					at = length(object$statistics) + length(object$newstats)/2, line = -1, adj = 0.5)
			}
			
			#if (print.stats) 
			{	
				# mtext(paste("Number of groups = ", length(stats), 
					# sep = ""), side = 1, line = top.line, adj = 0, 
					# at = at.col[i], font = qcc.options("font.stats"), 
					# cex = par("cex") * qcc.options("cex.stats"))
					
				num.groups.info = length(stats)
				
				center <- object$center
				
				if (length(center) == 1) 
				{
					# mtext(paste("Center = ", signif(center[1], 
						# digits), sep = ""), side = 1, line = top.line + 
						# 1, adj = 0, at = at.col[i], font = qcc.options("font.stats"), 
						# cex = par("cex") * qcc.options("cex.stats"))
						
						center.info = center
				}
				else 
				{
					# mtext("Center is variable", side = 1, line = top.line + 
						# 1, adj = 0, at = at.col[i], font = qcc.options("font.stats"), 
						# cex = par("cex") * qcc.options("cex.stats"))
						
						center.info = "Center is variable"
				}
				
				if (length(object.std.dev) == 1) 
				{
					# mtext(paste("StdDev = ", signif(std.dev, digits), 
						# sep = ""), side = 1, line = top.line + 
						# 2, adj = 0, at = at.col[i], font = qcc.options("font.stats"), 
						# cex = par("cex") * qcc.options("cex.stats"))
						
						std.info = object.std.dev
				}
				else 
				{
					# mtext("StdDev is variable", side = 1, line = top.line + 
						# 2, adj = 0, at = at.col[i], font = qcc.options("font.stats"), 
						# cex = par("cex") * qcc.options("cex.stats"))
						
						std.info = "StdDev is variable"
				}
				
				if (length(lcl) == 1) 
				{
					# mtext(paste("LCL = ", signif(lcl[1], digits), 
						# sep = ""), side = 1, line = top.line + 
						# 3, adj = 0, at = at.col[i], font = qcc.options("font.stats"), 
						# cex = par("cex") * qcc.options("cex.stats"))
						
						lcl.info = lcl
				}
				else 
				{
					# mtext("LCL is variable", side = 1, line = top.line + 
						# 3, adj = 0, at = at.col[i], font = qcc.options("font.stats"), 
						# cex = par("cex") * qcc.options("cex.stats"))
						
						lcl.info = "LCL is variable"
				}
				
				if (length(ucl) == 1) 
				{
					# mtext(paste("UCL = ", signif(ucl[1], digits), 
						# sep = ""), side = 1, line = top.line + 
						# 4, adj = 0, at = at.col[i], font = qcc.options("font.stats"), 
						# cex = par("cex") * qcc.options("cex.stats"))
						
						ucl.info = ucl
				}
				else 
				{
					# mtext("UCL is variable", side = 1, line = top.line + 
						# 4, adj = 0, at = at.col[i], font = qcc.options("font.stats"), 
						# cex = par("cex") * qcc.options("cex.stats"))
						
						ucl.info = "UCL is variable"
				}
				
				if (!is.null(violations)) 
				{
					# mtext(paste("Number beyond limits =", length(unique(violations$beyond.limits))), 
						# side = 1, line = top.line + 5, adj = 0, at = at.col[i], 
						# font = qcc.options("font.stats"), cex = par("cex") * 
						  # qcc.options("cex.stats"))
						
						beyond.limits.info = length(violations$beyond.limits)
						  
					# mtext(paste("Number violating runs =", length(unique(violations$violating.runs))), 
						# side = 1, line = top.line + 6, adj = 0, at = at.col[i], 
						# font = qcc.options("font.stats"), cex = par("cex") * 
						  # qcc.options("cex.stats"))
						 
						violating.runs.info = length(violations$violating.runs)
				}
				
				phase.summary.info = cbind(phase.summary.info, c(num.groups.info, center.info, std.info, lcl.info, ucl.info, beyond.limits.info, violating.runs.info ))
			}
    
			object.list = c(object.list, list(object))
		
			cum.indices = c(cum.indices, indices)
		}
	}
	
	if(length(phase.names) > 0) 
	{
		updated.phase.names = phase.names
		
		if(length(phase.names) < dim(phase.summary.info)[2])
		{
			updated.phase.names = paste("Phase",c((length(phase.names)+1):length(phases.data.list)))
			updated.phase.names = c(phase.names, updated.phase.names)
		}
		
		dimnames(phase.summary.info)[[2]] = updated.phase.names[1:dim(phase.summary.info)[2]]
	}
	else
	{
		dimnames(phase.summary.info)[[2]] = paste("Phase",c(1:length(phases.data.list)))
	}
	
	dimnames(phase.summary.info)[[1]] = c("Number of groups", "Center", "StdDev", "LCL", "UCL", "# of samples beyond limits", "# of samples violating tests")
	
	
	# if(print.stats || print.test.summary == TRUE || print.test.detail == TRUE)
	# {
		# #BSkyGraphicsFormat(bSkyFormatAppRequest = FALSE, noOfGraphics= 1)
	# }
	
	if (print.stats) 
	{	
		if(zero.or.one.phase == TRUE)
		{
			dimnames(phase.summary.info)[[2]] = c()
		}
		
		BSkyFormat(phase.summary.info, decimalDigitsRounding = digits, outputTableRenames = paste("Summary Stats for", type, "chart"))
	}
		
	if(print.test.summary == TRUE || print.test.detail == TRUE || print.qcc.object.summary == TRUE)
	{
		#if(length(object.list) > 0 && length(BSkyGetSixSigmaTestsToPerform()) > 0 && (print.test.summary == TRUE || print.test.detail == TRUE) )
		if(length(object.list) > 0 )
		{
			for(x in 1:length(object.list))
			{
				if(print.test.summary == TRUE || print.test.detail == TRUE || print.qcc.object.summary == TRUE)
				{
					if(zero.or.one.phase == FALSE)
					{
						if(length(phase.names) > 0 && length(phase.names) >= x)
						{
							cat("\n", phase.names[x], "\n")
						}
						else
						{
							cat(paste("\nPhase", x, "\n"))
						}
					}
					
					if(print.qcc.object.summary == TRUE)
					{
						summary(object.list[[x]])
					}
					
					get.print.violation.indices(object.list[[x]], print.summary = print.test.summary, print.detail = print.test.detail)
				}
			}
		}
	}
			
	return(invisible(list(summary.table = phase.summary.info, qcc.objects = object.list)))
}


print.qcc.spc.phases <- function (qcc.spc.phases.obects = list(), qcc.objects = list(),
									print.stats = FALSE, print.test.summary = FALSE, print.test.detail = FALSE,
									print.qcc.object.summary = FALSE,
									digits = 2, 
									phase.names = c(),
									stat.table.name = "Summary Stats",
									chart.title.name = c())
{

	zero.or.one.phase = FALSE
	
	if((length(qcc.spc.phases.obects) == 0 && length(qcc.objects) == 0) || 
		(print.stats == TRUE && length(qcc.spc.phases.obects) > 0 && length(qcc.spc.phases.obects$summary.table) == 0) || 
		((print.test.summary == TRUE || print.test.detail == TRUE) && ((length(qcc.spc.phases.obects) > 0 && length(qcc.spc.phases.obects$qcc.objects) == 0) && length(qcc.objects) == 0)))
	{
		stop("One or more qcc objects are needed or return value from plot.qcc.spc.phase() is needed as parameter") 
	}
	
	if (print.stats && length(qcc.spc.phases.obects$summary.table) > 0) 
	{	
		if(length(qcc.spc.phases.obects$qcc.objects) == 1)
		{
			dimnames(qcc.spc.phases.obects$summary.table)[[2]] = c()
		}
		
		if(length(qcc.spc.phases.obects$qcc.objects) > 0)
		{
			if(length(chart.title.name) == 0)
			{
				stat.table.name = paste(stat.table.name, "for", qcc.spc.phases.obects$qcc.objects[[1]]$type, "chart")
			}
			else
			{
				stat.table.name = paste(stat.table.name, "for", chart.title.name, "chart")
			}
		}
		
		BSkyFormat(qcc.spc.phases.obects$summary.table, decimalDigitsRounding = digits, outputTableRenames = stat.table.name)
	}
	
	
	if(print.test.summary == TRUE || print.test.detail == TRUE || print.qcc.object.summary == TRUE)
	{
		if(length(qcc.spc.phases.obects$qcc.objects) > 0)
		{
			object.list = qcc.spc.phases.obects$qcc.objects
		}
		else if(length(qcc.objects) > 0)
		{
			object.list =  qcc.objects
		}
		
		if(length(object.list) == 1)
		{
			zero.or.one.phase = TRUE
		}
		
		#if(length(object.list) > 0 && length(BSkyGetSixSigmaTestsToPerform()) > 0 && (print.test.summary == TRUE || print.test.detail == TRUE) )
		if(length(object.list) > 0 )
		{
			for(x in 1:length(object.list))
			{
				if(print.test.summary == TRUE || print.test.detail == TRUE || print.qcc.object.summary == TRUE)
				{
					if(zero.or.one.phase == FALSE)
					{
						if(length(phase.names) > 0 && length(phase.names) >= x)
						{
							#cat("\n", phase.names[x], "\n")
							BSkyFormat(phase.names[x])
						}
						else
						{
							#cat(paste("\nPhase", x, "\n"))
							BSkyFormat(paste("Phase", x))
						}
					}
					
					if(print.qcc.object.summary == TRUE)
					{
						summary(object.list[[x]])
					}
					
					get.print.violation.indices(object.list[[x]], print.summary = print.test.summary, print.detail = print.test.detail)
				}
			}
		}
	}
	
	return(invisible())
}
			


compute.qcc.statistics <- function(data, sizes = c(), newdata = c(), newsizes = c(), type = "xbar", nsigmas = 3, confidence.level= NA, std.dev = NA, digits = 2)
{	
	
	if(length(confidence.level) == 0) confidence.level = NA
	if(length(std.dev) == 0) std.dev = NA
	
	if(type[1] %in% c("xbar", "R", "S", "xbar.one"))
	{
		if(!is.na(std.dev) && !is.na(confidence.level))
		{
			if(length(newdata) == 0)
			{
				qcc.object = qcc::qcc(type = type, plot = FALSE, digits = digits,
										rules = get.print.violation.indices,
										data = data, 
										std.dev = std.dev, 
										nsigmas = nsigmas,
										confidence.level = confidence.level)
			}
			else
			{
				qcc.object = qcc::qcc(type = type, plot = FALSE, digits = digits,
										rules = get.print.violation.indices,
										data = data, 
										newdata = newdata,
										std.dev = std.dev, 
										nsigmas = nsigmas,
										confidence.level = confidence.level)
			}
		}
		else if(!is.na(std.dev))
		{
			if(length(newdata) == 0)
			{
				qcc.object = qcc::qcc(type = type, plot = FALSE, digits = digits,
										rules = get.print.violation.indices,
										data = data, 
										std.dev = std.dev, 
										nsigmas = nsigmas)
			}
			else
			{
				qcc.object = qcc::qcc(type = type, plot = FALSE, digits = digits,
										rules = get.print.violation.indices,
										data = data,
										newdata = newdata,									
										std.dev = std.dev, 
										nsigmas = nsigmas)
			}						
		}
		else if(!is.na(confidence.level))
		{
			if(length(newdata) == 0)
			{
				qcc.object = qcc::qcc(type = type, plot = FALSE, digits = digits,
										rules = get.print.violation.indices,
										data = data, 
										nsigmas = nsigmas,
										confidence.level = confidence.level)
			}
			else
			{
				qcc.object = qcc::qcc(type = type, plot = FALSE, digits = digits,
										rules = get.print.violation.indices,
										data = data,
										newdata = newdata, 
										nsigmas = nsigmas,
										confidence.level = confidence.level)
			}
		}
		else
		{
			if(length(newdata) == 0)
			{
				qcc.object = qcc::qcc(type = type, plot = FALSE, digits = digits,
										rules = get.print.violation.indices,
										data = data, 
										nsigmas = nsigmas)	
			}
			else
			{
				qcc.object = qcc::qcc(type = type, plot = FALSE, digits = digits,
										rules = get.print.violation.indices,
										data = data, 
										newdata = newdata,
										nsigmas = nsigmas)
			}
		}
	}
	else if(type[1] %in% c("p", "np", "c", "u"))
	{
		if(!is.na(std.dev) && !is.na(confidence.level))
		{
			if(length(newdata) == 0)
			{
				qcc.object = qcc::qcc(type = type, plot = FALSE, digits = digits,
										rules = get.print.violation.indices,
										data = data, 
										sizes = sizes,
										std.dev = std.dev, 
										nsigmas = nsigmas,
										confidence.level = confidence.level)
			}
			else
			{
				qcc.object = qcc::qcc(type = type, plot = FALSE, digits = digits,
										rules = get.print.violation.indices,
										data = data,										
										sizes = sizes,
										newdata	= newdata,
										newsizes = newsizes,
										std.dev = std.dev, 
										nsigmas = nsigmas,
										confidence.level = confidence.level)
			}
		}
		else if(!is.na(std.dev))
		{
			if(length(newdata) == 0)
			{
				qcc.object = qcc::qcc(type = type, plot = FALSE, digits = digits,
										rules = get.print.violation.indices,
										data = data, 
										sizes = sizes,
										std.dev = std.dev, 
										nsigmas = nsigmas)
			}
			else
			{
				qcc.object = qcc::qcc(type = type, plot = FALSE, digits = digits,
										rules = get.print.violation.indices,
										data = data, 
										sizes = sizes,
										newdata	= newdata,
										newsizes = newsizes,
										std.dev = std.dev, 
										nsigmas = nsigmas)
			}
										
		}
		else if(!is.na(confidence.level))
		{
			if(length(newdata) == 0)
			{
				qcc.object = qcc::qcc(type = type, plot = FALSE, digits = digits,
										rules = get.print.violation.indices,
										data = data, 
										sizes = sizes,
										nsigmas = nsigmas,
										confidence.level = confidence.level)
			}
			else
			{
				qcc.object = qcc::qcc(type = type, plot = FALSE, digits = digits,
										rules = get.print.violation.indices,
										data = data, 
										sizes = sizes,
										newdata	= newdata,
										newsizes = newsizes,
										nsigmas = nsigmas,
										confidence.level = confidence.level)
			}
		}
		else
		{
			if(length(newdata) == 0)
			{
				qcc.object = qcc::qcc(type = type, plot = FALSE, digits = digits,
										rules = get.print.violation.indices,
										data = data, 
										sizes = sizes,
										nsigmas = nsigmas)
			}
			else
			{
				qcc.object = qcc::qcc(type = type, plot = FALSE, digits = digits,
										rules = get.print.violation.indices,
										data = data, 
										sizes = sizes,
										newdata	= newdata,
										newsizes = newsizes,
										nsigmas = nsigmas)
			}
		}
	}
	
	return(invisible(qcc.object))
}