\name{BSkyDownSample}
\alias{BSkyDownSample}
\alias{BSkyUpSample}
\alias{BSkyResampleByClass}
\title{Down-sample or up-sample a dataset to balance the classes of a variable}
\usage{
BSkyDownSample(data, dependent)

BSkyUpSample(data, dependent)

BSkyResampleByClass(data, dependent, up = FALSE)
}
\arguments{
\item{data}{A data frame.}

\item{dependent}{The name of the variable whose classes (levels or distinct values) are balanced, as a character string.}

\item{up}{If FALSE, down-sample; if TRUE, up-sample. \code{BSkyDownSample} and \code{BSkyUpSample} set this for you.}
}
\value{
A data frame with the same columns, in the same order, as \code{data}. Rows keep their original order and the row names are reset to 1 to n.
}
\description{
Balances an imbalanced dataset so that every class of \code{dependent} has the same number of rows.

\code{BSkyDownSample} randomly keeps, from each class, as many rows as the smallest class has. Rows of the smallest class are all kept.

\code{BSkyUpSample} keeps all rows and adds randomly chosen repeats of rows (sampling with replacement) to each class until it has as many rows as the largest class.
}
\details{
Rows where \code{dependent} is missing (NA) are left out, with a warning. Factor levels that no row has are ignored. \code{dependent} can be a factor, character, logical, or numeric variable; each distinct value is a class.

Sampling is random. Call \code{set.seed} first to get the same rows every time.

These functions replace \code{downSample} and \code{upSample} from the caret package, which group the rows by any column named \code{y} in the data instead of by the dependent variable, and drop that column from the result.
}
\examples{
df <- data.frame(y = rnorm(10), sex = factor(c(rep("Female", 7), rep("Male", 3))))
set.seed(12345)
balanced <- BSkyDownSample(data = df, dependent = "sex")
table(balanced$sex)

set.seed(12345)
balanced <- BSkyUpSample(data = df, dependent = "sex")
table(balanced$sex)
}
\seealso{
\code{\link[base]{sample}}, \code{\link[base]{set.seed}}
}
