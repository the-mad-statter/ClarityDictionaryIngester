% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{map_no_yes_to_0_1}
\alias{map_no_yes_to_0_1}
\title{Map No/Yes to 0/1}
\usage{
map_no_yes_to_0_1(x)
}
\arguments{
\item{x}{"No" or "Yes" (case sensitive)}
}
\value{
0 for "No", 1 for "Yes", NA otherwise
}
\description{
Map No/Yes to 0/1
}
\examples{
ClarityDictionaryIngester:::map_no_yes_to_0_1("No")

ClarityDictionaryIngester:::map_no_yes_to_0_1("Yes")

ClarityDictionaryIngester:::map_no_yes_to_0_1("Maybe")
}
