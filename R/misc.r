#Used in ModelMatchesDataset to strip out 0's when adding statistics to a dataset using a model that is build with ignore intercept

BSkyStripNumberVariableNames <- function(variablenames)
{
	novars =length(variablenames)
  strippedNumerics =NULL
  i=1
  for (i in 1:novars)
  {
    if (is.na( suppressWarnings(as.numeric(variablenames[i]))))
      {
              strippedNumerics<- c(strippedNumerics, variablenames[i])
      }
  }
  return (strippedNumerics)
}

#This is a copy of binVariable in RcmdrMisc
#The issue is the bin ranges were displayed in scientific notation, setting dig.lab =12 ensures that for most ranges scientific notation is not used
BSkybinVariable <-
function (x, bins = 4, method = c("intervals", "proportions",
    "natural"), labels = FALSE, dig.lab =12)
{
    method <- match.arg(method)
    if (length(x) < bins) {
        stop("The number of bins exceeds the number of data values")
    }
    x <- if (method == "intervals")
        cut(x, bins, labels = labels, dig.lab =dig.lab)
    else if (method == "proportions")
        cut(x, quantile(x, probs = seq(0, 1, 1/bins), na.rm = TRUE),
            include.lowest = TRUE, labels = labels, dig.lab =dig.lab)
    else {
        xx <- na.omit(x)
        breaks <- c(-Inf, tapply(xx, KMeans(xx, bins)$cluster,
            max))
        cut(x, breaks, labels = labels, dig.lab =dig.lab)
    }
    as.factor(x)
}