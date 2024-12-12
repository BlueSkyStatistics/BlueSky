# Define the function with column names as parameters
twoPropTestBothSamplesSingleColMini <- function (dataset, col1_name, col2_name, p = 0, alternate = "two.sided",
    conf.level = 0.95, testMethod = "Estimate proportions separately")
{
    dataset <- base::get(dataset)[, c(col1_name, col2_name)]
    unique_col1 <- stats::na.omit(base::unique(dataset[[col1_name]]))
    if (base::length(unique_col1) != 2) {
        stop(base::paste("Error:", col1_name, "does not have exactly 2 unique values (excluding NA)."))
    }
    most_frequent_col1_value <- base::names(base::sort(base::table(dataset[[col1_name]]),
        decreasing = TRUE))[1]
    unique_col2 <- stats::na.omit(base::unique(dataset[[col2_name]]))
    if (base::length(unique_col2) != 2) {
        stop(base::paste("Error:", col2_name, "does not have exactly 2 unique values (excluding NA)."))
    }
    col1Values = dataset[, c(col1_name)]
    col2Values = dataset[, c(col2_name)]
    x1 <- base::sum(col1Values == most_frequent_col1_value, na.rm = TRUE)
    x2 = base::sum(col2Values == most_frequent_col1_value, na.rm = TRUE)
    n1 = length(na.omit(col1Values))
    n2 = length(na.omit(col2Values))
    eventString = base::paste("Event: ", col1_name, " = ", most_frequent_col1_value,
        "                             ", sep = "")
    proportion1String = base::paste("p1: proportion where ",
        col1_name, " =", most_frequent_col1_value)
    proportion2String = base::paste("p2: proportion where ",
        col2_name, " =", most_frequent_col1_value)
    differenceString = base::paste("Difference: p1-p2 = ", p,
        sep = "")
    testMethodMatrix = base::matrix(c(eventString, proportion1String,
        proportion2String, differenceString), nrow = 4, ncol = 1)
    base::colnames(testMethodMatrix) = "Test details"
    BSkyFormat(testMethodMatrix)
    BSky2SampleProportionMT(x1, x2, n1, n2, p, alternate, conf.level,
        testMethod, FALSE)
}
# Example usage:
# df <- data.frame(column1 = c(1, 1, 2, 2, NA), column2 = c("A", "B", "A", "B", "A"))
# result <- process_data(df, "column1", "column2")
# print(result)




BSky2SampleProportionMT <- function( x1, x2,n1,n2, p=0, alternate="two.sided", conf.level=0.95, testMethod ="Estimate proportions separately", generateDetails =TRUE)
{
	
	warningForPooledEstimateNotUsed = FALSE
  	p_null <- p # hypothesized proportion (p0)
  	alpha = 1-conf.level
	z_value=0
    # Proportion in each sample
    p1 <- x1 / n1
    p2 <- x2 / n2
    # Observed difference
	p_hat <- p1-p2
    #Proportion test to calculate the confidence interval
	
	if (generateDetails)
	{
		 #eventString = base::paste("Event: ", col1_name, " = ", most_frequent_col1_value, "                             ",sep ="" )
		proportion1String = "p1: proportion where Sample 1 = Event"
		proportion2String = "p2: proportion where Sample 2 = Event"
		differenceString = "Difference: p1-p2" 
  
		  testMethodMatrix = base::matrix(c( proportion1String, proportion2String, differenceString), nrow = 3, ncol = 1)
		  base::colnames(testMethodMatrix) ="Test details"
		  BSkyFormat(testMethodMatrix)
	}
	
	
	
	res <- prop.test(x = c(x1, x2), n = c(n1, n2), correct=FALSE,alternative=alternate, conf.level=conf.level)
    #  prop.test(x = c(x1, x2), n = c(n1, n2), correct=FALSE,alternative=alternate, conf.level=conf.level)
	ci_lower = round (res$conf.int[1], digits =BSkyGetDecimalDigitSetting())
    ci_upper = round(res$conf.int[2], digits= BSkyGetDecimalDigitSetting())
    # Variance of each sample
    var1 <- p1 * (1 - p1) / n1
    var2 <- p2 * (1 - p2) / n2
  	if (testMethod == "Estimate proportions separately" )
    {
      	# Standard error based on separate proportions
		se <- sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2))
        # Z-value for the test
        z_value <- (p1 - p2 - p_null) / se
	} else if (testMethod != "Estimate proportions separately" && p !=0) 
    {
    	warningForPooledEstimateNotUsed = TRUE
        pooled_p <- (x1 + x2) / (n1 + n2)
        #z_value <- (p1 - p2 - p_null) / sqrt(pooled_p * (1 - pooled_p) * (1 / n1 + 1 / n2))
        se <- sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2))
        #Z-value calculation
		z_value <- (p1 - p2 - p_null) / se
    } else {
        pooled_p <- (x1 + x2) / (n1 + n2)
        #z_value <- (p1 - p2 - p_null) / sqrt(pooled_p * (1 - pooled_p) * (1 / n1 + 1 / n2))
        se_pooled <- sqrt(pooled_p * (1 - pooled_p) * (1 / n1 + 1 / n2))
		# Z-value calculation
        z_value <- (p1 - p2 - p_null) / se_pooled
    }
  	if (alternate == "two.sided")
    {
  	    # To calculate p-value from z-value two tailed test
        p_value <- 2 * pnorm(-abs(z_value))  # two-tailed test
        #z_alpha <- qnorm(1 - alpha / 2)  # critical value for 95% confidence
        #ci_lower <- p_hat - z_alpha * sqrt(p_hat * (1 - p_hat) / n)
        #ci_upper <- p_hat + z_alpha * sqrt(p_hat * (1 - p_hat) / n)
		NullHypothesis = base::paste("H0: p1 - p2 =", p_null, sep=" ")
		AlterHypothesis	= base::paste("H1: p1 - p2 !=", p_null, sep = " ")
    }
	if (alternate == "greater")
    {
        # To calculate p-value from z-value greater
        p_value <- 1 - pnorm(z_value)
        ci_upper="+Inf"
	    #z_alpha <- qnorm(1 - alpha)  # critical value for 95% confidence
        #ci_lower <- p_hat - z_alpha * sqrt(p_hat * (1 - p_hat) / n)
		NullHypothesis = base::paste("H0: p1 - p2 =", p_null, sep=" ")
		AlterHypothesis	=base::paste("H1: p1 - p2 >", p_null, sep = " ")
    }
	if (alternate =="less")
    {
        # To calculate p-value from z-value less
        p_value <- pnorm(z_value)
        ci_lower ="-Inf"
		NullHypothesis = base::paste("H0: p1 - p2 =", p_null, sep=" ")
		AlterHypothesis	=base::paste("H1: p1 - p2 <", p_null, sep = " ")
	    #z_alpha <- qnorm(1 - alpha)  # critical value for 95% confidence
        #ci_upper <- p_hat + z_alpha * sqrt(p_hat * (1 - p_hat) / n)
		
    }
    #Descriptive statistics
    BSkyDescStats <- data.frame(
    Column1 = c("Sample1", "Sample2"),
    Column2 = c(n1, n2),
    Column3 =c(x1,x2) , 
    Column4 = c(p1, p2))
    base::colnames(BSkyDescStats) <- c("Sample", "N", "Event", "Sample p")
    BSkyFormat(BSkyDescStats, singleTableOutputHeader = "Descriptive statistics")
    #Estimation for difference
    result_diff_df <- data.frame(Column1 =p1-p2, Column2 =ci_lower, Column3 =ci_upper)
    base::colnames(result_diff_df) <- c("Difference", base::paste("Lower CI", conf.level, sep=":"),base::paste("Upper CI", conf.level, sep=":") )
    attr(result_diff_df, "BSkyFootnote_BSkySplit") = "CI based on normal approximation"
    BSkyFormat(result_diff_df, singleTableOutputHeader = "Estimation for difference")
    #result_matrix <- matrix(c(z_value, p_value), nrow = 1, ncol = 2)
    #
    ## Add column names
    #base::colnames(result_matrix) <- c("z-value", "p-value")
    #
    #	return (list(htest_obj, result_matrix))
    #
    #Run Fishers test
    if (p ==0)
    {  
        # Perform Fisher's Exact Test
      	contingency_table = base::matrix( c(x1, x2, n1-x1, n2-x2),nrow = 2, ncol = 2,byrow=TRUE)
        fisher_test_result <- stats::fisher.test(contingency_table)
        result_stats_df <- data.frame(Column1 =c("Normal approximation", "Fisher's exact"), Column2 =c(z_value, NA ), Column3 =c(p_value, fisher_test_result$p.value))
        base::colnames(result_stats_df) <- c("Method", "Z-value","P-Value" )
    } else    {
        result_stats_df <- data.frame(Column1 ="Normal approximation", Column2 =z_value, Column3 =p_value)
        base::colnames(result_stats_df) <- c("Method", "Z-Value","P-Value" )
    }
    finalFootNote="The normal approximation may be inaccurate for small samples"
	footnote2=""
	if (warningForPooledEstimateNotUsed)
	{
		footnote2= "Pooled estimate of the rate cannot be used in the denominator of the test when hypothesized difference is not 0"
	}
	if (footnote2 !="")
	{
		finalFootNote =base::paste(finalFootNote, footnote2, sep="\n")
	}
	if (testMethod !="Estimate proportions separately")
	{
		poooledString = base::paste ("The test based on normal approximation uses the pooled estimate of the proportion:", base::round(pooled_p, digits =BSkyGetDecimalDigitSetting()), sep="")
		finalFootNote =base::paste(finalFootNote, poooledString, sep="\n")
	}
	hypoMatrix = base::matrix(c("Null hypothesis", NullHypothesis, "Alternative hypothesis", AlterHypothesis), nrow = 2, ncol = 2,byrow=TRUE)
	#BSkyFormat(hypoMatrix, singleTableOutputHeader = "Test summary") 
  base::colnames(hypoMatrix) = c("Test summary","")
  BSkyFormat(hypoMatrix)
    BSkyFormat(result_stats_df, singleTableOutputHeader = "Test results",perTableFooter =finalFootNote) 
}