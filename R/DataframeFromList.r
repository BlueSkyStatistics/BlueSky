createDataFrameFromList <- function(data) {
  ListCols <- sapply(data, is.list)
  cbind(t(apply(data[ListCols], 1, unlist)),data[!ListCols] )
}

missingValues <- function (data)
{
if (sum( sapply(data, function(x) sum(is.na(x)))) ==0)
{
return(FALSE)
}
else
{
return (TRUE)
}
}




