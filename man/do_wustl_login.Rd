% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{do_wustl_login}
\alias{do_wustl_login}
\title{Do WUSTL Login}
\usage{
do_wustl_login(
  b,
  user = Sys.getenv("WUSTL_KEY_USER"),
  pass = Sys.getenv("WUSTL_KEY_PASS")
)
}
\arguments{
\item{b}{chromote session}

\item{user}{WUSTL Key username}

\item{pass}{WUSTL Key password}
}
\description{
Do WUSTL Login
}
