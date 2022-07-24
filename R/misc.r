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