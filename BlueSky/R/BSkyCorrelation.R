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

BSkyCorrelationMatrix <-function  (data, vars=NULL, correlationType = "Pearson", missingValues = "complete.obs", pValue = "adjP")
{

	BSky_Correlation_Matrix_Type = rcorr.adjust(data[,vars], use=missingValues, type = tolower(correlationType))
	showPvalue = pValue
	results <- BSkyFormatRcorr_adjust(BSky_Correlation_Matrix_Type, showPvalue)
	row.names(results) <- NULL
	results = list (results)
	names(results) = paste(correlationType, " correlation", sep="")
	return(invisible(results))
	
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

BSkyPlotCorrelationMatrix <-function  (data, vars=NULL, correlationType = "Pearson", missingValues = "complete.obs", visualizeCorrelation = TRUE, plotWeb = TRUE)
{
	BSky_Correlation_Matrix_Type = rcorr.adjust(data[,vars], use=missingValues, type = tolower(correlationType))
	m=BSky_Correlation_Matrix_Type[[1]]$r
	if (visualizeCorrelation)
	{
	 DescTools::PlotCorr(x=m, cols = colorRampPalette(c("cyan3","white","chartreuse2"), space = "rgb")(20),breaks=seq(-1, 1, length=21), border="black", args.colorlegend = list(labels=sprintf("%.1f", seq(-1, 1, length = 11)), frame=TRUE))
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

