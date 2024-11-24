
BSkyTwoPropTestSingleColMini <- function (dataset, col1_name, col2_name, p = 0, alternate = "two.sided",
    conf.level = 0.95, testMethod = "Estimate proportions separately")
{
    dataset <- base::get(dataset)[, c(col1_name, col2_name)]
  	unique_col1 <- stats::na.omit(base::unique(dataset[[col1_name]]))
    if (base::length(unique_col1) > 2) {
        stop(base::paste("Error:", col1_name, "has more than 2 unique values (excluding NA). Unique values found are:",base::paste(unique_col2, collapse =",")))
    }
   
    unique_col2 <- stats::na.omit(base::unique(dataset[[col2_name]]))
    if (base::length(unique_col2) > 2) {
        stop(base::paste("Error:", col2_name, "has more than 2 unique values (excluding NA). Unique values found are:",base::paste(unique_col2, collapse =",")))
    }
		split_values <- base::split(dataset[,col1_name], dataset[,col2_name])
		most_frequent_col1_value <- base::names(base::sort(base::table(dataset[,col1_name]),
        decreasing = TRUE))[1]
	n1 = length(split_values[[1]])
	n2 = length(split_values[[2]])
	x1 = sum(split_values[[1]] == most_frequent_col1_value)
  x2 = sum(split_values[[2]] == most_frequent_col1_value)
  eventString = base::paste("Event: ", col1_name, " = ", most_frequent_col1_value,
        "                             ", sep = "")
    proportion1String = base::paste("p1: proportion where ",
        col1_name, " =", most_frequent_col1_value, " for ", col2_name,
        " = ", base::levels(dataset[,col2_name])[1])
    proportion2String = base::paste("p2: proportion where ",
        col1_name, " =", most_frequent_col1_value, " for ", col2_name,
        " = ", base::levels(dataset[,col2_name])[2])
    differenceString = base::paste("Difference: p1-p2 = ", p,
        sep = "")
    testMethodMatrix = base::matrix(c(eventString, proportion1String,
        proportion2String, differenceString), nrow = 4, ncol = 1)
    base::colnames(testMethodMatrix) = "Test details"
    BSkyFormat(testMethodMatrix)
    BSky2SampleProportionMT(x1, x2, n1, n2, p, alternate, conf.level,
        testMethod, FALSE)
}

 
