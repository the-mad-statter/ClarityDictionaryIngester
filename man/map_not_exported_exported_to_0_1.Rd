% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{map_not_exported_exported_to_0_1}
\alias{map_not_exported_exported_to_0_1}
\title{Map "Not Exported"/"Exported" to 0/1}
\usage{
map_not_exported_exported_to_0_1(x)
}
\arguments{
\item{x}{"Not Exported" or "Exported" (case sensitive)}
}
\value{
0 for "Not Exported", 1 for "Exported", NA otherwise
}
\description{
Map "Not Exported"/"Exported" to 0/1
}
\examples{
ClarityDictionaryIngester:::map_not_exported_exported_to_0_1("Not Exported")

ClarityDictionaryIngester:::map_not_exported_exported_to_0_1("Exported")

ClarityDictionaryIngester:::map_not_exported_exported_to_0_1("Imported")
}
