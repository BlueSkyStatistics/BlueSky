#parameters

#tvarbox1: The variables in format NoPrefix|UseComma|Enclosed
#correlationType gpbox2: Pearson or Spearman
#missingValues gpbox1: complete.obs or pairwise.complete.obs
#pValue gpbox3: adjP or unAdjP or bothP
#visualizeCorrelation: TRUE FALSE
#plotweb: TRUE FALSE
#require(RcmdrMisc);
#require(corrplot);
#require(DescTools); 
#BSkyCorrelationMatrix (data=Iris_Train, vars=c("Sepal.Length","Sepal.Width"), correlationType = "Pearson", missingValues = "complete.obs", pValue = "adjP")
#BSkyPlotCorrelationMatrix  (data, vars=c("Sepal.Length","Sepal.Width"), correlationType = "Pearson", missingValues = "complete.obs", visualizeCorrelation = TRUE, plotWeb = TRUE)
#

#########################################################
# BSky Correlation Test Script
#########################################################
# library(RcmdrMisc)
# abc = BSkyCorrelationMatrix(data=mtcars, vars=c("mpg", "hp", "disp"))
# BSkyFormat(abc)

# library(DescTools)
# BSkyPlotCorrelationMatrix(data=mtcars, vars=c("mpg", "hp", "disp"))

# mtcars %>%
  # select(mpg, hp, disp) %>%
    # BSkyCorrelationMatrix() %>%
      # BSkyFormat()

# mtcars %>%
  # select(mpg, hp, disp) %>%
    # BSkyPlotCorrelationMatrix()


# library(magrittr)

# mtcars %>%
  # select(mpg, hp) %T>%
    # BSkyPlotCorrelationMatrix() %>%
    # BSkyCorrelationMatrix()

#13Oct2021
BSkyCorrelationMatrix <-function  (data, vars=NULL, correlationType = "Pearson", missingValues = "complete.obs", pValue = "adjP")
{
	if(class(data)[1] != "character")
	{
		dataset_name_str = deparse(substitute(data))
		
		if(dataset_name_str == ".")
		{
			dataset_name_str = "data" 
		}
		
		#print(head(data))
	}
	else
	{
		dataset_name_str = data
		data = eval(parse(text=data), envir = globalenv())
	}
	
	if(is.null(vars) || length(trimws(vars)) == 0)
	{
		vars = dimnames(data)[[2]]
	}
	
	BSky_Correlation_Matrix_Type = rcorr.adjust(data[,vars], use=missingValues, type = tolower(correlationType))
	showPvalue = pValue
	results <- BSkyFormatRcorr_adjust(BSky_Correlation_Matrix_Type, showPvalue)
	row.names(results) <- NULL
	results = list (results)
	names(results) = paste(correlationType, " correlation", sep="")
	
	if(BSkyIsRmarkdownOutputOn() == TRUE)
	{
		return((results))
	}
	else
	{
		return(invisible(results))
	}
}



### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title Correlation Analysis
#'
#' @description This function uses the rcorr function in the Hmisc package to compute matrices of Pearson or Spearman correlations along with the pair wise p-values among the correlations. The p-values are corrected for multiple inference using Holm's method (see p.adjust). Observations are filtered for missing data, and only complete observations are used.
#'
#' @param data a numeric matrix or data frame, or an object of class "rcorr.adjust" to be printed.
#' @param vars
#' @param correlationType "pearson" or "spearman", depending upon the type of correlations desired; the default is "pearson".
#' @param missingValues how to handle missing data: "complete.obs", the default, use only complete cases; "pairwise.complete.obs", use all cases with valid data for each pair.
#' @param visualizeCorrelation prints PlotCorr()
#' @param plotWeb prints PlotWeb()
#'
#' @return Returns an object of class "rcorr.adjust", which is normally just printed.
#'
#' @examples
BSkyPlotCorrelationMatrix <-function  (data, vars=NULL, correlationType = "Pearson", missingValues = "complete.obs", visualizeCorrelation = TRUE, plotWeb = TRUE)
{
	if(class(data)[1] != "character")
	{
		dataset_name_str = deparse(substitute(data))
		
		if(dataset_name_str == ".")
		{
			dataset_name_str = "data" 
		}
		
		#print(head(data))
	}
	else
	{
		dataset_name_str = data
		data = eval(parse(text=data), envir = globalenv())
	}
	
	# if(class(data)[1] != "data.frame" && class(data)[1] != "matrix")
	# {
		# return(invisible(NULL))
	# }
	
	if(is.null(vars) || length(trimws(vars)) == 0)
	{
		vars = dimnames(data)[[2]]
	}
	
	BSky_Correlation_Matrix_Type = rcorr.adjust(data[,vars], use=missingValues, type = tolower(correlationType))
	m=BSky_Correlation_Matrix_Type[[1]]$r
	
	if (visualizeCorrelation)
	{
		 DescTools::PlotCorr(x=m, cols = colorRampPalette(c("cyan3","white","chartreuse2"), space = "rgb")(20),breaks=seq(-1, 1, length=21), border="black", args.colorlegend = list(labels=sprintf("%.1f", seq(-1, 1, length = 11)), frame=TRUE), main ="Web plot of the Correlation Matrix")
		 title(main="Correlation Matrix", line=3)
		 text(x=rep(1:ncol(m),ncol(m)), y=rep(1:ncol(m),each=ncol(m)), label=sprintf("%0.2f", m[,ncol(m):1]), cex=0.8, xpd=TRUE)
	}
	if (plotWeb)
	{
		DescTools::PlotWeb(m, col = c(hred, hblue), lty = par("lty"), lwd = NULL, args.legend=NULL,
			pch = 21, pt.cex = 2, pt.col = "black", pt.bg = "darkgrey",
			cex.lab = 1, las = 1, adj = NULL, dist = 0.5)
	}
}


BSkyFormatRcorr_adjust <- function(a, showPvalue=c("adjP"))
{
	 if(class(a) != "rcorr.adjust") return (NULL)

	 df = data.frame(t(rep(c(""), nrow(a$R$r))), stringsAsFactors = FALSE)
	 statCol = c("dummy")
	 rowNames = c("dummy")

	 for(i in 1:nrow(a$R$r))
	 {
		  df = rbind(df, a$R$r[i,] )
		  
		  if(showPvalue=="adjP")
		  {
			 df = rbind(df, a$P[i,] )
			 statCol = c(statCol, c("Correlation", "Adj-P"))
			 repCount = 1
		  }
		  else if(showPvalue=="unAdjP")
		  {
			 df = rbind(df, a$P.unadj[i,] )
			 statCol = c(statCol, c("Correlation", "Un-adj-P"))
			 repCount = 1
		  }
		  else
		  {
			 df = rbind(df, a$P[i,] )
			 df = rbind(df, a$P.unadj[i,] )
			 statCol = c(statCol,c("Correlation","Adj-P", "Un-adj-P"))
			 repCount = 2
		  }
		  
		  df = rbind(df, a$R$n[i,] )
		  statCol = c(statCol, c("n"))
		  rowNames = c(rowNames, dimnames(a$R$r)[[1]][i], rep(c(""),repCount+1))
	}

	 df = df[-1,]
	 statCol = statCol[-1]
	 rowNames = rowNames[-1]
	 df = data.frame(rowNames, statCol, df)
	 names(df) = c("", "", dimnames(a$R$r)[[2]])

 return(invisible(df))
}

#13Oct2021
### title should fit on one line, be written in sentence case, but not end in a full stop
### to print @ in the documentation, escape with one more @ (e.g. @@ prints @)
#' @title BSky_options
#'
#' @description Get current BlueSky options
#'
#' @return current BlueSky options
#'
#' @examples BSky_options()s
BSky_options <- function()
{
	fmt = BSkyGetKableAndRmarkdownFormatting()
	
	if(fmt$doTextFormatting == TRUE)
	{
		print_mode = "print = text"
		print_style = paste("text_style =", BSkyGetTextTableFormat())
	}
	else if(fmt$doLatexFormatting == TRUE)
	{
		print_mode = "print = latex"
		print_style = c(" ")
	}
	else if(fmt$doRmarkdownFormatting == TRUE || fmt$doKableFormatting == TRUE)
	{
		print_mode = "print = html"
		
		if(uadatasets.sk$BSkykableStyleTheme == "kable_classic")
		{
			print_style = "html_style = APA"
		}
		else
		{
			print_style = "html_style = non-APA"
		}
	}
	
	p_value_show = paste("p_value_show =", as.character(uadatasets.sk$showActualPValueInOutput))
	p_value_asterix = paste("p_value_drop_asterisk =", as.character(uadatasets.sk$pvalueDropAsterisk))
	
	bsky_digits = paste("bsky_digits =", BSkyGetDecimalDigitSetting())
	
	bsky_scipen = paste("bsky_scipen_on =", as.character(BSkyGetEngNotationSetting()))
	
	bsky_rounding = paste("bsky_rounding_on =", as.character(BSkyGetRound()))
	
	cat("\n", print_mode, "and", print_style, "\n")
	cat("\n", p_value_show, "and", p_value_asterix, "\n")
	cat("\n", bsky_digits, "\n")
	cat("\n", bsky_scipen, "\n")
	cat("\n", bsky_rounding, "\n")
}
