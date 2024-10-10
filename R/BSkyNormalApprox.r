BSkyNormalApprox <- function( x, n, p, alternative="two.sided", conf.level=0.95)
{
	p_null <- p # hypothesized proportion (p0)
	# Observed sample proportion
	p_hat <- x / n
	# Z-value calculation (normal approximation)
	z_value <- (p_hat - p_null) / sqrt(p_null * (1 - p_null) / n)
	alpha = 1-conf.level
	if (alternative =="two.sided")
	{
	# P-value for a two-tailed test
		p_value <- 2 * (1 - pnorm(abs(z_value)))
			# Confidence Interval Calculation (95% confidence interval)
	z_alpha <- qnorm(1 - alpha / 2)  # critical value for 95% confidence
	ci_lower <- p_hat - z_alpha * sqrt(p_hat * (1 - p_hat) / n)
	ci_upper <- p_hat + z_alpha * sqrt(p_hat * (1 - p_hat) / n)
	
		
	}
	if (alternative =="greater")
	{
	# P-value for one-tailed upper test (H1: p > p0)
		p_value <- 1 - pnorm(z_value)
		ci_upper="+Inf"
		z_alpha <- qnorm(1 - alpha)  # critical value for 95% confidence
		ci_lower <- p_hat - z_alpha * sqrt(p_hat * (1 - p_hat) / n)
	
	}
	# P-value for one-tailed lower test (H1: p < p0)
	if (alternative =="less")
	{
		p_value <- pnorm(z_value)
		ci_lower ="-Inf"
		z_alpha <- qnorm(1 - alpha)  # critical value for 95% confidence
		ci_upper <- p_hat + z_alpha * sqrt(p_hat * (1 - p_hat) / n)
	
	}
	# Confidence interval
	ci <- c(ci_lower, ci_upper)
	# Create the htest object manually
	htest_obj <- list(
    
	  statistic = c("number of sucesses" =x),  # Test statistic (Z-value)
	  p.value = p_value,           # P-value of the test
	  parameter = c("number of trials" = n),        # Sample size
	  estimate = c("Sample p" = p_hat), # Estimated proportion
	  null.value = c(proportion = p_null), # Null hypothesis proportion
	  alternative = alternative,   # Type of alternative hypothesis
	  method = "1-sample proportion test", # Test method
	  data.name = "Sample Proportion Data", # Description of the data
	  conf.int =ci
	)
 	# Assign the class htest to the object
	class(htest_obj) <- "htest"
	
	result_matrix <- matrix(c(z_value, p_value), nrow = 1, ncol = 2)

# Add column names
colnames(result_matrix) <- c("z-value", "p-value")

	return (list(htest_obj, result_matrix))
}