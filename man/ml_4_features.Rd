% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ml_4_features.R
\name{ml_4_features}
\alias{ml_4_features}
\title{Specify features for a machine learning model}
\usage{
ml_4_features(data_sets_list, features, output = "list", add = FALSE)
}
\arguments{
\item{data_sets_list}{A list containing at least one data frame \code{.train}
and possibly other data frames \code{.test1} and \code{.test2}, as well as 
other dot prefixed elements specified earlier in the pipeline.}

\item{features}{A character vector or NSE of feature names contained in all
training and test sets within \code{data_sets_list}. Alternatively a 
right hand side formula can be supplied. when a complete formula is set, 
the left hand side is automaitcally known as .target and a call to 
\code{ml_target} is not necessary anymore. The latter one and NSE support is
not implemented yet.}

\item{output}{A character vector specifying the output type. One can choose between
list, tibble, data.table and data.frame. Other formats like sparse matrices
might be implemented in the future.}

\item{add}{Not implemented yet. Per default new features by iterative calls will overwrite the first
call. If you instead want to add additional features, set \code{add} to
\code{TRUE}.}
}
\value{
A list containing the train and test sets as well as a .features
element and other elements prefixed by a dot, if supplied earlier in the
pipeline.
}
\description{
Specify features for a machine learning model
}
\examples{
ml_4_features(data_sets_list = list(.train = iris), features = c("Sepal.Length", "Sepal.Width"))

}
\seealso{
\href{https://github.com/Tazinho/ml}{ml on github}.
}
\author{
Malte Grosser, \email{malte.grosser@gmail.com}
}
\keyword{utilities}
