BSky_variance_analysis (
    data_format = c("wide" )
    cols_to_compare = c('A','B','C')
    response_col = NULL
    group_col = NULL
    conf_level = 0.95
    alt_hypothesis = "two.sided"
    p_adjust_method = "holm"
    normal_distribution =FALSE
    Fligner_Killeen =FALSE
  pairwiseComparison=FALSE
    data_name="AandB"







BSky_variance_analysis <- function( 
    data_name,
    data_format = c("wide", "long"),
    cols_to_compare = NULL,
    response_col = NULL,
    group_col = NULL,     # can be one or more grouping columns
    conf_level = 0.95,
    alt_hypothesis = "two.sided",
    p_adjust_method = "bonferroni",
  	normal_distribution =FALSE,
  Fligner_Killeen =FALSE,
  pairwiseComparison=FALSE
  
) {
  bartlett_res=NULL
  alpha = 1-conf_level
  subtitle_string =""
  if (!normal_distribution)
  {
		subtitle_string = paste("Multiple comparison intervals for the standard deviation, alpha = ", alpha, sep="")
  }
  pairwise_df  =NULL
  # Load data
    data <- get(data_name, envir = .GlobalEnv)
    #data_format <- match.arg(data_format)

    # === 1. Convert to long format ===
   # if (data_format == "wide") {
  	if (is.null(group_col)) {
        if (is.null(cols_to_compare)) stop("Must specify cols_to_compare for wide format")
        #This code removes nas which is not necessary
      	#data <- data %>% drop_na(all_of(cols_to_compare))
        long_data <- data %>%
            select(all_of(cols_to_compare)) %>%
            pivot_longer(
                cols = everything(),
                names_to = "group",
                values_to = "value"
            )
        long_data$group <- factor(long_data$group)
        grouping_vars <- "group"

    } else {
        if (is.null(response_col) || is.null(group_col)) {
            stop("Must specify response_col and group_col for long format")
        }

        # Ensure characters
          # Ensure both are character
        group_col <- as.character(group_col)
        response_col <- as.character(response_col)

        # Combine into a single vector of column names
        cols_needed <- c(group_col, response_col)

        # Drop missing
        data <- data %>% drop_na(all_of(cols_needed))
        # Keep grouping + response cols
        long_data <- data %>%
            select(all_of(cols_needed)) %>%
            rename(value = !!sym(response_col))

        # Combined group factor for tests
        long_data <- long_data %>%
            mutate(group = interaction(!!!syms(group_col), sep = "_", drop = TRUE))

        long_data$group <- factor(long_data$group)
        grouping_vars <- group_col
    }

    # === 2. Overall variance tests ===
	groups <- levels(long_data$group)
	if (normal_distribution && length(groups) >2)
	{
		bartlett_res <- bartlett.test(x = long_data$value, g = long_data$group)
	}
    levene_res   <- leveneTest(y = long_data$value, group = long_data$group)
  	
   if (pairwiseComparison)
   {
    BSkyFormat(as.data.frame(levene_res),
              singleTableOutputHeader = "Levene's test for homogeneity of variances (center = median)",
              decimalDigitsRounding = BSkyGetDecimalDigitSetting())
   }
   # BSkyFormat(bartlett_res, decimalDigitsRounding = BSkyGetDecimalDigitSetting())
  fligner_res=NULL
    if (Fligner_Killeen)
    {
    	fligner_res  <- fligner.test(x = long_data$value, g = long_data$group)
       BSkyFormat(fligner_res, singleTableOutputHeader = "Fligner-Killeen test",
               decimalDigitsRounding = BSkyGetDecimalDigitSetting())
		}

    # === 3. Pairwise Levene’s tests ===
    
    pairwise_list <- list()
  
  if (pairwiseComparison)
    {
  
    for (i in 1:(length(groups)-1)) {
        for (j in (i+1):length(groups)) {
            subset_df <- subset(long_data, group %in% c(groups[i], groups[j]))
            lt <- leveneTest(value ~ group, data = subset_df, center = mean)
            pval <- lt$`Pr(>F)`[1]

            pairwise_list[[length(pairwise_list)+1]] <- data.frame(
                Group1 = groups[i],
                Group2 = groups[j],
                PValue = pval
            )
        }
    }
    pairwise_df <- bind_rows(pairwise_list)
    pairwise_df$P_adj <- p.adjust(pairwise_df$PValue, method = p_adjust_method)

    # If multiple grouping vars, split Group1/Group2 back into columns
    if (length(grouping_vars) > 1) {
        pairwise_df <- pairwise_df %>%
            separate(Group1, into = paste0("G1_", grouping_vars), sep = "_", remove = FALSE) %>%
            separate(Group2, into = paste0("G2_", grouping_vars), sep = "_", remove = FALSE)
    }

    BSkyFormat(pairwise_df,
               singleTableOutputHeader = paste("Pairwise Levene’s tests with center = mean, adjustment: ", p_adjust_method))
}


	# Create a data frame with 2 variables and 3 rows
	df_mini_method <- data.frame(
	  Test = c("Null hypothesis", "Alternative hypothesis", "Significance level"),
	  Description = c("All variances are equal",
					  "At least one variance is different",
					  paste("alpha = ", alpha, sep="")),
		stringsAsFactors = FALSE
		)
	BSkyFormat(df_mini_method,
               singleTableOutputHeader = "Method")

    # === 4. Optional F-test if exactly 2 groups ===
    if (normal_distribution && length(groups) == 2) {
        ftest_res <- var.test(value ~ group, data = long_data,
                              conf.level = conf_level,
                              alternative = alt_hypothesis)
        # BSkyFormat(ftest_res,
                   # singleTableOutputHeader = "F-test",
                   # decimalDigitsRounding = BSkyGetDecimalDigitSetting())
    }

    # === 5. Bonferroni CIs ===
    alpha <- 1 - conf_level
    K <- length(groups)
    sided <- 2
  	
  	 K1 = alpha / (sided * K)
  	K2 = 1 - K1
  #K2 = round(K2, digits =BSkyGetDecimalDigitSetting())
  individualCI =1-alpha / ( K)

    bonf_df <- long_data %>%
      group_by(across(all_of(grouping_vars)), group) %>%
      summarise(
        result = list(
          minitab(
            value,
            alpha = .env$alpha,
            K = .env$K,
            sided = .env$sided,
            type = "sample", normal_distribution
          )
        ),
        .groups = "drop"
      ) %>%
      unnest_wider(result)
  bonf_df<-as.data.frame(bonf_df)
		attr(bonf_df, "BSkyFootnote_BSkySplit") = paste("Individual confidence level =", round(individualCI*100, digits =BSkyGetDecimalDigitSetting()), "%", sep="")
    BSkyFormat(as.data.frame(bonf_df),
               singleTableOutputHeader = paste(conf_level*100, "% Bonferroni Confidence Intervals for Standard Deviations", sep=""))

    # === 6. Plot ===
   # levene_p <- signif(levene_res$`Pr(>F)`[1], 3)
  levene_p <- round(levene_res$`Pr(>F)`[1],digits =BSkyGetDecimalDigitSetting() )
  
  if (base::nlevels(long_data$group) ==2 && !normal_distribution)
  {
    	group_names <- levels(long_data$group)
			group_name1 <- group_names[1]
			group_name2 <- group_names[2]

		# 2. Extract the values for each group using filter()
			group1_values <- long_data %>%
  				filter(group == group_name1) %>%
  				pull(value)

			group2_values <- long_data %>%
  				filter(group == group_name2) %>%
  					pull(value)
    
    	testResults<-bonett_two_variances (x=group1_values, y=group2_values, rho0 = 1, alpha = 0.05)
    min_mc_p =round (testResults$pval, digits =BSkyGetDecimalDigitSetting())
    testStatistic =round (testResults$T2, digits =BSkyGetDecimalDigitSetting())
	
	mcp_result <- mc_sd_intervals_k2(group1_values, group2_values)
	
	bonf_df$CI_Lower <- mcp_result$CI_Lower
	bonf_df$CI_Upper <- mcp_result$CI_Upper
	bonf_df$group <- factor(bonf_df$group, levels = rev(unique(bonf_df$group)))
	
	
	
    
    dfForTestStats <- data.frame(
				  Method  = c("Multiple comparisons", "Levene"),
				  "Test Statistic"    = c(testStatistic, levene_res$`F value`[1]),
				  P_value = c(min_mc_p, levene_p)
					)
    BSkyFormat(dfForTestStats, singleTableOutputHeader = "Tests")
   
  } else if (base::nlevels(long_data$group) !=2 && !normal_distribution){
    
    data_list <- long_data %>%
  	group_split(group) %>%                # split tibble by group
  	lapply(function(df) df$value)         # extract "value" column as numeric vector
		# Now call mc_pvalue_from_data
		out <- mc_pvalue_from_dataNEW(data_list, rel.tol = 1e-8)      
    min_mc_p =round (out$overall_p, digits =BSkyGetDecimalDigitSetting())    
         
         dfForTestStats <- data.frame(
  Method  = c("Multiple comparisons", "Levene"),
  "Test Statistic"    = c("--", levene_res$`F value`[1]),
  P_value = c(min_mc_p, levene_p)
)
    BSkyFormat(dfForTestStats, singleTableOutputHeader = "Tests")
	
	samples_mcp <- long_data %>%
  group_split(group, .keep = TRUE) %>%
  setNames(unique(long_data$group)) %>%
  lapply(function(df) df$value)
	
	 
	result_mcp <- mc_intervals_graphical_kgt2_modified(samples_mcp,alpha)
	
	bonf_df$CI_Lower <- result_mcp$CI_dataframe$CI_Lower
	bonf_df$CI_Upper <- result_mcp$CI_dataframe$CI_Upper
	bonf_df$group <- factor(bonf_df$group, levels = rev(unique(bonf_df$group)))
	
	
    } else if (base::nlevels(long_data$group) ==2 &&normal_distribution)
      {
      #attr(ftest_res$statistic, "label") <- NULL
      names(ftest_res$statistic) <- NULL
      dfForTestStats <- data.frame(
  Method  = c("F"),
  "Test Statistic"    = c( ftest_res$statistic ),
        P_value = c(ftest_res$p.value))
      min_mc_p=round (ftest_res$p.value, digits =BSkyGetDecimalDigitSetting())
         BSkyFormat(dfForTestStats, singleTableOutputHeader = "Tests")
      bonf_df$group <- factor(bonf_df$group, levels = rev(unique(bonf_df$group)))
      
      } else if (base::nlevels(long_data$group) !=2 &&normal_distribution){
        dfForTestStats <- data.frame(
  Method  = c("Bartlett"),
  "Test Statistic"    = c( round(bartlett_res$statistic,digits =BSkyGetDecimalDigitSetting())),
  P_value = c(bartlett_res$p.value))
      min_mc_p=round (bartlett_res$p.value, digits =BSkyGetDecimalDigitSetting())
           BSkyFormat(dfForTestStats, singleTableOutputHeader = "Tests")
      bonf_df$group <- factor(bonf_df$group, levels = rev(unique(bonf_df$group)))
      }
  
  
  
    #min_mc_p <- signif(min(pairwise_df$P_adj), 3)
		
    if (!normal_distribution)   {
      xlabel="If intervals don't overlap, the corresponding stdevs are significantly different"
    legend_df <- data.frame(
      x = max(bonf_df$CI_Upper) * 1.05,
      y = c(length(unique(bonf_df$group)) + 0.9,
            length(unique(bonf_df$group)) + 0.75,
      			length(unique(bonf_df$group)) + 0.6,
            length(unique(bonf_df$group)) + 0.45),
      label = c(
                paste0("Multiple Comparisons"),
      					paste0("P-value = ", min_mc_p),
        				paste0("Levene's Test"),
                paste0("P-value = ", levene_p)       
               ),
      col = c("black", "black", "black", "black"),
      size = c(4, 4, 4,4)
    )
      } else{
      xlabel=paste(conf_level*100, "% Bonferroni Confidence Intervals for Standard Deviations", sep="")
      if (base::nlevels(long_data$group) ==2)  
      {
          legend_df <- data.frame(
          x = max(bonf_df$CI_Upper) * 1.05,
          y = c(length(unique(bonf_df$group)) + 0.9,
                length(unique(bonf_df$group)) + 0.75
                ),
          label = c("F Test",
                    paste0("P-value ", min_mc_p)),
          col = c("black", "black"),
          size = c(4, 4)
        )
      } else{
        
        legend_df <- data.frame(
          x = max(bonf_df$CI_Upper) * 1.05,
          y = c(length(unique(bonf_df$group)) + 0.9,
                length(unique(bonf_df$group)) + 0.75
                ),
          label = c("Bartlett's  Test",
                    paste0("P-value ", min_mc_p)),
          col = c("black", "black"),
          size = c(4, 4)
        )
        
      }
      
      
      }
 		# Appending string to the title
           if (!is.null(cols_to_compare))
           {
					StrintToAppendToTitle <- paste(cols_to_compare, collapse = ",")
            } else
             {
            
             
             StrintToAppendToTitle <- paste(response_col, " grouped by ", paste(group_col, collapse = ","), sep="")
             }
	
    plot <- ggplot(bonf_df, aes(x = StDev, y = group)) +
      geom_point(size = 3, color = "blue") +
      geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2, color = "blue") +
      xlab(xlabel) +
      ylab("Group") +
      ggtitle(paste("Test of Equal Variances ",StrintToAppendToTitle, sep=""), subtitle = subtitle_string) +
      theme_minimal(base_size = 12) +
      geom_text(
        data = legend_df,
        aes(x = x, y = y, label = label),
        inherit.aes = FALSE,
        hjust = 0,
        color = legend_df$col,
        size = legend_df$size
      ) +
      theme(plot.margin = margin(10, 150, 10, 10),plot.subtitle = element_text(hjust = 0.5,     
      size = 14 ),plot.title = element_text(hjust = 0.5,     
      size = 16 )) +
      coord_cartesian(clip = "off")

    print(plot)

    return(list(
        bartlett = bartlett_res,
        levene = levene_res,
        fligner = fligner_res,
        pairwise_levene = pairwise_df,
        bonferroni_ci = bonf_df
    ))
}

           
           
           
minitab_var <- function(x, type = "sample") {
  # ensure numeric vector
  if (!is.numeric(x)) stop("x must be numeric")
  
  #type <- match.arg(type)
  n <- length(x)
  mean_x <- mean(x)
  
  if (type == "sample") {
    # sample variance (denominator n - 1)
    return(sum((x - mean_x)^2) / (n - 1))
  } else {
    # population variance (denominator n)
    return(sum((x - mean_x)^2) / n)
  }
}

minitab <- function(x, alpha=0.05, K=2, sided=2, type ="sample", normal_distribution=FALSE){
  x =na.omit(x)
  K1 = alpha / (sided * K)
  K2 = 1 - K1
  sampleSize = length(x)
  df = sampleSize - 1
  
  if (type == "sample") {
    varALL = minitab_var(x, type)
  } else {
    varALL = minitab_var(x, type)
  }
  
  sdALL = sd(x)
  
  if (!normal_distribution)
  {
	results =bonett_sd_ci(x, alpha, K, sided)
	 LowerL1= results[1]
	 UpperL1= results[2]
	
  }else
  {
  U1 = qchisq(K1, df)
  L1 = qchisq(K2, df)
  UpperL1 = sqrt((df * varALL) / U1)
  LowerL1 = sqrt((df * varALL) / L1)
  }
  # return results as a tibble row
  return(tibble(
    N = sampleSize,
    StDev = sdALL,
    CI_Lower = LowerL1,
    CI_Upper = UpperL1
  ))
}

bonett_sd_ci <- function(x, alpha=0.05, K=2, sided=2) {
  n <- length(x)
  s <- sd(x)                    # Sample standard deviation
  alpha =alpha/K
  #alpha <- 1 - conf.level
  z <- qnorm(1 - alpha/2)
  
  # ---- Step 1: Compute trimmed mean (or fallback to sample mean) ----
  # Bonett’s method requires a trimmed mean for kurtosis estimation.
  # If n <= 5, trimming is not feasible, so we use the sample mean.
  if (n > 5) {
    trim_prop <- 1 / (2 * sqrt(n - 4))
    m_trim <- mean(x, trim = trim_prop)
  } else {
    m_trim <- mean(x)
  }
  
  # Trimmed mean with trim proportion = 1/(2*sqrt(n-4))
  #trim_prop <- 1 / (2 * sqrt(n - 4))
  #m_trim <- mean(x, trim = trim_prop)
  
  # Numerator for gamma4
  numerator <- n * sum((x - m_trim)^4)
  # Denominator: squared sum of squared deviations from sample mean
  denom <- (sum((x - mean(x))^2))^2
  gamma4_hat <- numerator / denom
  
  c_const <- n / (n - z)
  se <- c_const * sqrt((gamma4_hat - (n - 3) / n) / (n - 1))
  
  log_variance <- log(c_const * s^2)
  lower_var <- exp(log_variance - z * se)
  upper_var <- exp(log_variance + z * se)
  
  # Convert variance CI to SD CI
  lower_sd <- sqrt(lower_var)
  upper_sd <- sqrt(upper_var)
  
  #return(c(lower_sd = lower_sd, upper_sd = upper_sd))
  return(c(lower_sd, upper_sd))
}






  #### MC multiple-comparisons p-value (Banga & Fox 2013) - R implementation ####
# Requires base R only (uses integrate()).

# 1) helper: trimmed mean with the paper's trim proportion 1/(2*sqrt(n) - 4)
trimmed_mean_paper <- function(x) {
  n <- length(x)
  trim_prop <- if (n <= 2) 0 else 1 / (2*sqrt(n) - 4)
  trim_prop <- max(0, min(0.5, trim_prop))  # safe bounds
  mean(x, trim = trim_prop)
}

# 2) pooled kurtosis estimator gamma_hat_ij per paper (uses trimmed means)
gamma_hat_pair <- function(x, y) {
  ni <- length(x); nj <- length(y)
  mi <- trimmed_mean_paper(x)
  mj <- trimmed_mean_paper(y)
  num <- sum((x - mi)^4) + sum((y - mj)^4)
  denom <- ( sum((x - mean(x))^2) + sum((y - mean(y))^2) )^2
  if (denom <= 0) return(3.0) # fallback
  (ni + nj) * num / denom
}

# 3) distribution function of the range R of k i.i.d. standard normals:
#    P(R <= r) = k * integral_{-Inf}^{Inf} phi(x) * [Phi(x + r) - Phi(x)]^(k-1) dx
# so tail Pr(R > r) = 1 - that integral. (Barnard 1978 discussed algorithms for normal range.)
pr_range_gt <- function(r, k, rel.tol = 1e-8) {
  if (r <= 0) return(1.0)
  integrand <- function(x) {
    dnorm(x) * (pnorm(x + r) - pnorm(x))^(k - 1)
  }
  # numeric integration (vectorized integrand OK)
  val <- try(integrate(integrand, lower = -Inf, upper = Inf,
                       rel.tol = rel.tol, subdivisions = 1000)$value,
             silent = TRUE)
  if (inherits(val, "try-error") || is.nan(val)) {
    warning("range integral did not converge; returning NA")
    return(NA_real_)
  }
  cdf <- k * val
  p_tail <- max(0, 1 - cdf)
  p_tail
}

# 4) L(z, n1, n2, S1, S2, se) function from the paper (z < min(n1,n2))
Lfun <- function(z, n1, n2, S1, S2, se) {
  if (!is.finite(z) || z >= min(n1, n2)) return(NA_real_)
  log(n1 / n2) + log((n2 - z) / (n1 - z)) - z * se + log((S1^2) / (S2^2))
}

# 5) Root-finding for z_L or z_U following the algorithm (uses finite brackets)
find_root_L <- function(n1, n2, S1, S2, se) {
  eps <- 1e-8
  minnz <- min(n1, n2)
  # z_m formula (guard for negative inside sqrt)
  disc <- (n1 - n2) * (n1 - n2 - 4 / se)
  if (disc < 0) {
    # paper: if disc < 0 no root in that case for the z_m branch
    z_m <- (n1 + n2) / 2  # fallback
  } else {
    z_m <- (n1 + n2 - sqrt(disc)) / 2
  }
  if (n1 < n2) {
    Lzm <- Lfun(z_m, n1, n2, S1, S2, se)
    if (!is.finite(Lzm)) return(NA_real_)
    if (Lzm > 0) {
      return(NA_real_)  # paper says set alpha_L = 0
    }
    # find lower bracket (a large negative number where L>0); expand if needed
    lower <- -1e3
    Llower <- Lfun(lower, n1, n2, S1, S2, se)
    tries <- 0
    while (!is.finite(Llower) || Llower <= 0) {
      lower <- lower * 2
      Llower <- Lfun(lower, n1, n2, S1, S2, se)
      tries <- tries + 1
      if (tries > 60) break
    }
    if (!is.finite(Llower)) return(NA_real_)
    # now L(lower) >0 and L(z_m) <= 0 => bracket found
    root <- try(uniroot(function(z) Lfun(z, n1, n2, S1, S2, se),
                        lower = lower, upper = z_m, tol = 1e-8)$root, silent = TRUE)
    if (inherits(root, "try-error")) return(NA_real_)
    return(root)
  } else { # n1 > n2
    L0 <- Lfun(0, n1, n2, S1, S2, se)
    if (!is.finite(L0)) return(NA_real_)
    if (L0 >= 0) {
      # find root in [0, n2)
      upper <- n2 - eps
      # check sign change between 0 and upper
      Lup <- Lfun(upper, n1, n2, S1, S2, se)
      if (!is.finite(Lup)) return(NA_real_)
      if (L0 * Lup > 0) return(NA_real_)  # no root
      root <- try(uniroot(function(z) Lfun(z, n1, n2, S1, S2, se),
                          lower = 0, upper = upper, tol = 1e-8)$root, silent = TRUE)
      if (inherits(root, "try-error")) return(NA_real_)
      return(root)
    } else {
      # find root in (-Inf, 0): bracket with negative lower bound
      lower <- -1e3
      Llower <- Lfun(lower, n1, n2, S1, S2, se)
      tries <- 0
      while (!is.finite(Llower) || Llower <= 0) {
        lower <- lower * 2
        Llower <- Lfun(lower, n1, n2, S1, S2, se)
        tries <- tries + 1
        if (tries > 60) break
      }
      if (!is.finite(Llower)) return(NA_real_)
      root <- try(uniroot(function(z) Lfun(z, n1, n2, S1, S2, se),
                          lower = lower, upper = 0, tol = 1e-8)$root, silent = TRUE)
      if (inherits(root, "try-error")) return(NA_real_)
      return(root)
    }
  }
}

# 6) Primary function: input = list of numeric vectors (one per group)
mc_pvalue_from_data <- function(data_list, nsim = NULL, rel.tol = 1e-8) {
  k <- length(data_list)
  if (k < 3) stop("This function is for k > 2 groups (MC method).")
  n <- sapply(data_list, length)
  S <- sapply(data_list, sd)
  # compute pairwise pooled kurtosis gamma_hat_ij
  gamma_hat <- matrix(NA, k, k)
  for (i in 1:(k-1)) {
    for (j in (i+1):k) {
      gamma_hat[i,j] <- gamma_hat[j,i] <- gamma_hat_pair(data_list[[i]], data_list[[j]])
    }
  }
  # compute kappa constants
  kappa <- (n - 3) / n
  # compute b_ij
  b <- matrix(0, k, k)
  for (i in 1:(k-1)) {
    for (j in (i+1):k) {
      v <- (gamma_hat[i,j] - kappa[i]) / (n[i] - 1) + (gamma_hat[i,j] - kappa[j]) / (n[j] - 1)
      b[i,j] <- b[j,i] <- sqrt(max(0, v))
    }
  }
  # total sum of all unique b_jl
  total_bsum <- sum(b[upper.tri(b)])
  # compute V_i per paper
  V <- numeric(k)
  for (i in 1:k) {
    sum_j_not_i <- sum(b[i, -i])
    V[i] <- ((k - 1) * sum_j_not_i - total_bsum) / ((k - 1) * (k - 2))
  }
  # pairwise p_ij
  Pmat <- matrix(NA, k, k)
  for (i in 1:(k-1)) {
    for (j in (i+1):k) {
      se_ij <- V[i] + V[j]
      # if n_i == n_j balanced case:
      if (n[i] == n[j]) {
        z0 <- (log(S[i]^2) - log(S[j]^2)) / se_ij
        thresh <- abs(z0) * sqrt(2)
        Pmat[i,j] <- Pmat[j,i] <- pr_range_gt(thresh, k, rel.tol = rel.tol)
      } else {
        # unbalanced: find z_L and z_U via L function adapted with se = V_i + V_j
        zL <- find_root_L(n[i], n[j], S[i], S[j], se_ij)
        zU <- find_root_L(n[j], n[i], S[j], S[i], se_ij)
        alphaL <- if (is.na(zL)) 0 else pr_range_gt(zL * sqrt(2), k, rel.tol = rel.tol)
        alphaU <- if (is.na(zU)) 0 else pr_range_gt(zU * sqrt(2), k, rel.tol = rel.tol)
        Pmat[i,j] <- Pmat[j,i] <- min(alphaL, alphaU)
      }
    }
  }
  # overall p-value = minimum pairwise p
  overall_p <- min(Pmat[upper.tri(Pmat)], na.rm = TRUE)
  list(Ppairwise = Pmat, overall_p = overall_p, V = V, b = b, n = n, S = S)
}

           
           
mc_pvalue_from_dataNEW <- function(data_list, nsim = NULL, rel.tol = 1e-8, conf.level = 0.95) {
  k <- length(data_list)
  if (k < 3) stop("This function is for k > 2 groups (MC method).")
  n <- sapply(data_list, length)
  S <- sapply(data_list, sd)
  
  # compute pairwise pooled kurtosis gamma_hat_ij
  gamma_hat <- matrix(NA, k, k)
  for (i in 1:(k-1)) {
    for (j in (i+1):k) {
      gamma_hat[i,j] <- gamma_hat[j,i] <- gamma_hat_pair(data_list[[i]], data_list[[j]])
    }
  }
  
  # compute kappa constants
  kappa <- (n - 3) / n
  
  # compute b_ij
  b <- matrix(0, k, k)
  for (i in 1:(k-1)) {
    for (j in (i+1):k) {
      v <- (gamma_hat[i,j] - kappa[i]) / (n[i] - 1) + (gamma_hat[i,j] - kappa[j]) / (n[j] - 1)
      b[i,j] <- b[j,i] <- sqrt(max(0, v))
    }
  }
  
  # total sum of all unique b_jl
  total_bsum <- sum(b[upper.tri(b)])
  
  # compute V_i
  V <- numeric(k)
  for (i in 1:k) {
    sum_j_not_i <- sum(b[i, -i])
    V[i] <- ((k - 1) * sum_j_not_i - total_bsum) / ((k - 1) * (k - 2))
  }
  
  # pairwise p_ij and intervals
  Pmat <- matrix(NA, k, k)
  CI_low <- matrix(NA, k, k)
  CI_high <- matrix(NA, k, k)
  
  for (i in 1:(k-1)) {
    for (j in (i+1):k) {
      se_ij <- V[i] + V[j]
      diff_ij <- log(S[i]^2) - log(S[j]^2)
      
      if (n[i] == n[j]) {
        z0 <- diff_ij / se_ij
        thresh <- abs(z0) * sqrt(2)
        Pmat[i,j] <- Pmat[j,i] <- pr_range_gt(thresh, k, rel.tol = rel.tol)
      } else {
        zL <- find_root_L(n[i], n[j], S[i], S[j], se_ij)
        zU <- find_root_L(n[j], n[i], S[j], S[i], se_ij)
        alphaL <- if (is.na(zL)) 0 else pr_range_gt(zL * sqrt(2), k, rel.tol = rel.tol)
        alphaU <- if (is.na(zU)) 0 else pr_range_gt(zU * sqrt(2), k, rel.tol = rel.tol)
        Pmat[i,j] <- Pmat[j,i] <- min(alphaL, alphaU)
      }
      
      # Confidence interval (currently using normal quantile)
      crit <- qnorm(1 - (1 - conf.level) / 2)
      CI_low[i,j] <- CI_low[j,i] <- diff_ij - crit * se_ij
      CI_high[i,j] <- CI_high[j,i] <- diff_ij + crit * se_ij
    }
  }
  
  overall_p <- min(Pmat[upper.tri(Pmat)], na.rm = TRUE)
  
  list(
    Ppairwise = Pmat,
    overall_p = overall_p,
    V = V,
    b = b,
    n = n,
    S = S,
    CI_low = CI_low,
    CI_high = CI_high,
    conf.level = conf.level
  )
}

#Final k=2 that handles NAs
mc_sd_intervals_k2 <- function(x, y, alpha = 0.05) {
  z <- qnorm(1 - alpha/2)  # Minitab uses z_{alpha/2} here
  
  f <- function(v) {
    v <- v[!is.na(v)]  # Remove NAs
    n <- length(v)
    
    if (n < 2) {
      # Not enough data to compute sd or interval
      return(c(lower = NA, upper = NA))
    }
    
    s  <- sd(v)
    c  <- n / (n - z)
    Vi <- 1 / (n - 1)
    center <- s * sqrt(c)
    m <- z * sqrt(Vi) / 2
    
    c(lower = center * exp(-m),
      upper = center * exp( m))
  }
  
  # Compute intervals for both x and y
  Ix <- f(x)
  Iy <- f(y)
  
  # Combine into a data frame
  result <- data.frame(                     # <-- changed
    CI_Lower = c(Ix["lower"], Iy["lower"]), # <-- changed
    CI_Upper = c(Ix["upper"], Iy["upper"])  # <-- changed
  )
  
  #rownames(result) <- c("x", "y")           # <-- changed
  
  return(result)                            # <-- changed
}


#Final k> 2 10/18/2025 NA handling

.q_range_normal <- function(alpha, k, rel.tol = 1e-7) {
  stopifnot(k >= 2, alpha > 0, alpha < 1)
  # CDF of the normal-range at t:  F_R(t) = k ∫ φ(u) [Φ(u+t) - Φ(u)]^{k-1} du
  cdf_R <- function(t) {
    if (t <= 0) return(0)
    integrand <- function(u) k * dnorm(u) * (pnorm(u + t) - pnorm(u))^(k - 1)
    integrate(integrand, lower = -Inf, upper = Inf, rel.tol = rel.tol)$value
  }
  target <- 1 - alpha
  f <- function(t) cdf_R(t) - target
  uniroot(f, interval = c(1e-8, 12), tol = 1e-7)$root
}


# Graphical MC intervals for standard deviations, k > 2, handles na
#and has small sample adjustment
mc_intervals_graphical_kgt2_modified <- function(samples, alpha = 0.05) {
  stopifnot(is.list(samples), length(samples) >= 3L)
  k <- length(samples)

  # basic summaries
  n <- sapply(samples, function(x) sum(!is.na(x)))        # <-- changed
  S <- sapply(samples, function(x) sd(x, na.rm = TRUE))   # <-- changed
  ybar <- sapply(samples, function(x) mean(x, na.rm = TRUE))  # <-- changed

    small_n <- n <= 6             
                 
  # trimmed means m_i with trim proportion 1 / (2*sqrt(n_i) - 4); clamp to [0, 0.49]
  #trim_prop <- 1 / (2 * sqrt(n - 4))
  #trim_prop[!is.finite(trim_prop) | trim_prop < 0] <- 0
  #trim_prop <- pmin(trim_prop, 0.49)
                 
 trim_prop <- ifelse(small_n, 0, 1 / (2 * sqrt(n) - 4))
trim_prop[!is.finite(trim_prop) | trim_prop < 0] <- 0
trim_prop <- pmin(trim_prop, 0.49)                

  tmean <- function(x, prop) {
    x <- x[!is.na(x)]                                     # <-- changed
    if (prop <= 0) return(mean(x))
    g <- floor(prop * length(x))
    if (g <= 0) return(mean(x))
    sx <- sort(x)
    mean(sx[(g + 1):(length(x) - g)])
  }
  m <- mapply(tmean, samples, trim_prop)

  # pooled kurtosis estimator γ̂_ij (Bonett-modified Layard)
  sum4 <- function(x, mu) sum((x[!is.na(x)] - mu)^4)      # <-- changed
  sum4_i <- mapply(sum4, samples, m)

  gamma_hat <- matrix(NA_real_, k, k)
  for (i in 1:(k - 1)) for (j in (i + 1):k) {
    num <- (n[i] + n[j]) * (sum4_i[i] + sum4_i[j])
    den <- ((n[i] - 1) * S[i]^2 + (n[j] - 1) * S[j]^2)^2
    gh  <- num / den
    gamma_hat[i, j] <- gamma_hat[j, i] <- gh
  }

  # r_i = (n_i - 3)/n_i
  r <- (n - 3) / n

  # b_ij = sqrt{ (γ̂_ij - r_i)/(n_i-1) + (γ̂_ij - r_j)/(n_j-1) }
  b <- matrix(0, k, k)
  for (i in 1:k) for (j in 1:k) if (i != j) {
    #b[i, j] <- sqrt((gamma_hat[i, j] - r[i]) / (n[i] - 1) +
     #               (gamma_hat[i, j] - r[j]) / (n[j] - 1))
    scale_fac <- ifelse(small_n[i] | small_n[j], 0.5, 1)
b[i, j] <- scale_fac * sqrt((gamma_hat[i, j] - r[i]) / (n[i] - 1) +
                            (gamma_hat[i, j] - r[j]) / (n[j] - 1))
  }

  # V_i chosen to minimize Σ_{i≠j} (V_i + V_j - b_ij)^2
  sum_pairs <- sum(b[upper.tri(b)])
  V <- sapply(1:k, function(i) {
    ((k - 1) * sum(b[i, -i]) - sum_pairs) / ((k - 1) * (k - 2))
  })

  # bias correction
  z <- qnorm(1 - alpha/2)
  c_i <- n / (n - z)

  # normal-range critical q_{α,k}
  #q_alpha_k <- .q_range_normal(alpha, k)
  q_norm <- .q_range_normal(alpha, k)
if (any(small_n)) {
  n_eff <- min(n)
  z <- qnorm(1 - alpha / 2)
  q_alpha_k <- 0.78 * q_norm * sqrt((n_eff - 1)/(n_eff - 3)) *
               (qt(1 - alpha/2, df = n_eff - 1) / z)
} else {
  q_alpha_k <- q_norm
}

  # Final MC intervals for SDs
  intervals <- lapply(seq_len(k), function(i) {
    c(lower =  S[i] * sqrt(c_i[i] * exp(-q_alpha_k * V[i] / sqrt(2))),
      upper =  S[i] * sqrt(c_i[i] * exp( q_alpha_k * V[i] / sqrt(2))))
  })

  names(intervals) <- names(samples) %||% paste0("Group", seq_len(k))
                 
  CI_df <- data.frame(
  CI_Lower = sapply(intervals, function(x) as.numeric(x[1])),  # <-- lower values
  CI_Upper = sapply(intervals, function(x) as.numeric(x[2]))   # <-- upper values
)

# (optional) add group names as a column
#CI_df <- tibble::rownames_to_column(CI_df, "Group")
  
  # Create dataframe with all confidence intervals  # <-- changed
  #CI_df <- data.frame(                              # <-- changed
#    CI_Lower = sapply(intervals, function(x) x["lower"]),  # <-- changed
#    CI_Upper = sapply(intervals, function(x) x["upper"])   # <-- changed
#  ) %>%                                             # <-- changed
#    tibble::rownames_to_column("Group")  

  list(
    intervals = intervals,
     CI_dataframe = CI_df,      
    details = list(
      n = n, S = S, m = m, r = r, V = V, c_i = c_i, z = z, q_alpha_k = q_alpha_k,
      gamma_hat = gamma_hat, b = b
    )
  )
}
                    
 bonett_two_variances <- function(x, y, rho0 = 1, alpha = 0.05,gv = c("sum_M4"),md =c("n"),ut=c(TRUE),uc = c(TRUE ), ag=c(TRUE),tau_adj=c(FALSE)) {
  n1 <- length(x); n2 <- length(y)
  S1 <- sd(x); S2 <- sd(y)
  m1 <- mean(x); m2 <- mean(y)
  
  
  
  # candidate settings
  #gamma_variants <- c("sum_M4", "sum_M4_rho")
  # m4_divisors    <- c("n", "n_minus1")
  # use_trim_opts  <- c(TRUE, FALSE)
  # use_corr_opts  <- c(TRUE, FALSE)
  # apply_g_opts   <- c(TRUE, FALSE)
  # tau_opts       <- c(TRUE, FALSE)   # NEW toggle for τ² adjustment
  
  # loop over all combinations
  # for (gv in gamma_variants) {
    # for (md in m4_divisors) {
      # for (ut in use_trim_opts) {
        # for (uc in use_corr_opts) {
          # for (ag in apply_g_opts) {
            # for (tau_adj in tau_opts) {
              
              # trimming if requested
              if (ut) {
                
                
                trim_prop1 <- 1/(2*sqrt(n1-4))
                trim_prop2 <- 1/(2*sqrt(n2-4))
                m1 <- mean(x, trim = trim_prop1)
                m2 <- mean(y, trim = trim_prop2)
              }
              
            
              
              # 4th moments
              M4_1 <- sum((x - m1)^4)
              M4_2 <- sum((y - m2)^4)
              
              # denominator
              d1 <- if (md == "n") n1 else (n1-1)
              d2 <- if (md == "n") n2 else (n2-1)
              
              # gammaP
              if (gv == "sum_M4") {
                num <- M4_1 + M4_2
              } else if (gv == "sum_M4_rho") {
                num <- M4_1 + rho0^4 * M4_2
              }
              denom <- ((n1 - 1) * S1^2 + rho0^2 * (n2 - 1) * S2^2)^2
              gammaP <- (n1 + n2) * num / denom
              
              # correction g_i
              g1 <- if (ag) (n1 - 3)/n1 else 0
              g2 <- if (ag) (n2 - 3)/n2 else 0
              
              # correction factors (c1,c2) if used
              if (uc) {
                z_alpha2 <- qnorm(1 - alpha/2)
                c1 <- n1 / (n1 - z_alpha2)
                c2 <- n2 / (n2 - z_alpha2)
              } else {
                c1 <- c2 <- 1
              }
              C <- c1/c2
              
              # log ratio
              log_term <- log(C * (S1^2 / S2^2)) - log(rho0^2)
              
              # denominator (se^2)
              se2 <- (gammaP - g1)/(n1 - 1) + (gammaP - g2)/(n2 - 1)
              
              # tau^2 adjustment if requested
              if (tau_adj) {
                # Bonett/Shoemaker adjustment
                tau2 <- 2 + (1 - 1/n1)*(gammaP - 3)
                tau2_hat <- gammaP - g1
                se2 <- se2 * (tau2 / tau2_hat)
              }
              
              se <- sqrt(abs(se2))
              Z_signed <- log_term / se
              Z_abs <- abs(Z_signed)
              pval <- 2 * (1 - pnorm(Z_abs))
              
              # Two equivalent ways to get p-value (report both)
               p_two_sided_norm <- 2 * (1 - pnorm(Z_abs))           # two-sided normal/Z p
  						T2 <- Z_abs^2
  					p_chisq1 <- pchisq(T2, df = 1, lower.tail = FALSE)  # chi-square(1) p
 
              
              results <- data.frame(
                gamma_variant = gv,
                m4_divisor = md,
                use_trim = ut,
                use_correction = uc,
                apply_g = ag,
                tau_adjust = tau_adj,
                n1 = n1, n2 = n2,
                S1 = S1, S2 = S2,
                gammaP = gammaP,
                g1 = g1, g2 = g2,
                c1 = c1, c2 = c2,
                se2 = se2,
                se = se,
                Z_signed = Z_signed,
                Z_abs = Z_abs,
                pval = pval,
                 T2 = T2,
    p_norm = round(p_two_sided_norm, digits=4),
    p_chisq1 = p_chisq1
              )
            # }
          # }
        # }
      # }
    # }
  # }
  #do.call(rbind, results)
}

#This is the original , it handles nas but not small samples                  
# Graphical MC intervals for standard deviations, k > 2
mc_intervals_graphical_kgt2 <- function(samples, alpha = 0.05) {
  stopifnot(is.list(samples), length(samples) >= 3L)
  k <- length(samples)

  # basic summaries
  n <- sapply(samples, function(x) sum(!is.na(x)))        # <-- changed
  S <- sapply(samples, function(x) sd(x, na.rm = TRUE))   # <-- changed
  ybar <- sapply(samples, function(x) mean(x, na.rm = TRUE))  # <-- changed

  # trimmed means m_i with trim proportion 1 / (2*sqrt(n_i) - 4); clamp to [0, 0.49]
  trim_prop <- 1 / (2 * sqrt(n - 4))
  trim_prop[!is.finite(trim_prop) | trim_prop < 0] <- 0
  trim_prop <- pmin(trim_prop, 0.49)

  tmean <- function(x, prop) {
    x <- x[!is.na(x)]                                     # <-- changed
    if (prop <= 0) return(mean(x))
    g <- floor(prop * length(x))
    if (g <= 0) return(mean(x))
    sx <- sort(x)
    mean(sx[(g + 1):(length(x) - g)])
  }
  m <- mapply(tmean, samples, trim_prop)

  # pooled kurtosis estimator γ̂_ij (Bonett-modified Layard)
  sum4 <- function(x, mu) sum((x[!is.na(x)] - mu)^4)      # <-- changed
  sum4_i <- mapply(sum4, samples, m)

  gamma_hat <- matrix(NA_real_, k, k)
  for (i in 1:(k - 1)) for (j in (i + 1):k) {
    num <- (n[i] + n[j]) * (sum4_i[i] + sum4_i[j])
    den <- ((n[i] - 1) * S[i]^2 + (n[j] - 1) * S[j]^2)^2
    gh  <- num / den
    gamma_hat[i, j] <- gamma_hat[j, i] <- gh
  }

  # r_i = (n_i - 3)/n_i
  r <- (n - 3) / n

  # b_ij = sqrt{ (γ̂_ij - r_i)/(n_i-1) + (γ̂_ij - r_j)/(n_j-1) }
  b <- matrix(0, k, k)
  for (i in 1:k) for (j in 1:k) if (i != j) {
    b[i, j] <- sqrt((gamma_hat[i, j] - r[i]) / (n[i] - 1) +
                    (gamma_hat[i, j] - r[j]) / (n[j] - 1))
  }

  # V_i chosen to minimize Σ_{i≠j} (V_i + V_j - b_ij)^2
  sum_pairs <- sum(b[upper.tri(b)])
  V <- sapply(1:k, function(i) {
    ((k - 1) * sum(b[i, -i]) - sum_pairs) / ((k - 1) * (k - 2))
  })

  # bias correction
  z <- qnorm(1 - alpha/2)
  c_i <- n / (n - z)

  # normal-range critical q_{α,k}
  q_alpha_k <- .q_range_normal(alpha, k)

  # Final MC intervals for SDs
  intervals <- lapply(seq_len(k), function(i) {
    c(lower =  S[i] * sqrt(c_i[i] * exp(-q_alpha_k * V[i] / sqrt(2))),
      upper =  S[i] * sqrt(c_i[i] * exp( q_alpha_k * V[i] / sqrt(2))))
  })

  names(intervals) <- names(samples) %||% paste0("Group", seq_len(k))
                 
  CI_df <- data.frame(
  CI_Lower = sapply(intervals, function(x) as.numeric(x[1])),  # <-- lower values
  CI_Upper = sapply(intervals, function(x) as.numeric(x[2]))   # <-- upper values
)

# (optional) add group names as a column
#CI_df <- tibble::rownames_to_column(CI_df, "Group")
  
  # Create dataframe with all confidence intervals  # <-- changed
  #CI_df <- data.frame(                              # <-- changed
#    CI_Lower = sapply(intervals, function(x) x["lower"]),  # <-- changed
#    CI_Upper = sapply(intervals, function(x) x["upper"])   # <-- changed
#  ) %>%                                             # <-- changed
#    tibble::rownames_to_column("Group")  

  list(
    intervals = intervals,
     CI_dataframe = CI_df,      
    details = list(
      n = n, S = S, m = m, r = r, V = V, c_i = c_i, z = z, q_alpha_k = q_alpha_k,
      gamma_hat = gamma_hat, b = b
    )
  )
}

