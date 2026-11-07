BSkyDownSample <- function(data, dependent)
{
	BSkyResampleByClass(data, dependent, up = FALSE)
}

BSkyUpSample <- function(data, dependent)
{
	BSkyResampleByClass(data, dependent, up = TRUE)
}

#Balances the classes of a dependent variable by sampling rows: down-sampling keeps as many rows of each class
#as the smallest class has, up-sampling repeats random rows of each class until it has as many as the largest.
#Replaces caret::downSample/upSample, which group by any column named "y" instead of the dependent variable
#and drop that column from the result. Rows where the dependent variable is NA are left out. Call set.seed() first
#for reproducible results.
BSkyResampleByClass <- function(data, dependent, up = FALSE)
{
	if (!is.data.frame(data)) stop("data must be a data frame")
	if (!(dependent %in% names(data))) stop(paste0("Variable '", dependent, "' was not found in the dataset"))
	cls <- data[[dependent]]
	nMissing <- sum(is.na(cls))
	if (nMissing > 0) warning(paste0(nMissing, " row(s) with a missing value for '", dependent, "' were left out"))
	#Row numbers of each class, only classes that occur (empty factor levels would make every class 0 rows)
	rowsByClass <- split(seq_along(cls), cls, drop = TRUE)
	if (length(rowsByClass) == 0) stop(paste0("Variable '", dependent, "' has no non-missing values"))
	sizes <- lengths(rowsByClass)
	if (up)
	{
		target <- max(sizes)
		#All rows of a class, then random repeats of them; sample.int avoids sample()'s trap when a class has 1 row
		idx <- unlist(lapply(rowsByClass, function(i) c(i, i[sample.int(length(i), target - length(i), replace = TRUE)])))
	}
	else
	{
		target <- min(sizes)
		idx <- unlist(lapply(rowsByClass, function(i) i[sample.int(length(i), target)]))
	}
	out <- data[sort(idx), , drop = FALSE]
	rownames(out) <- NULL
	out
}
