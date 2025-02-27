% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app.R
\name{checkRequiredColumns}
\alias{checkRequiredColumns}
\title{checkRequiredColumns}
\usage{
checkRequiredColumns(functionName, dataset, required_names)
}
\arguments{
\item{functionName}{name of the function wheree we are validating a dataset column}

\item{dataset}{the dataset we want to validate}

\item{required_names}{The required column names we will be using and must be in the dataset}
}
\value{
boolean value and/or error message if it catches an inconsistency
}
\description{
Allows us to validate if the required columns exist in the dataframe we will use and gives a helpful error message if they dont. This helps us notice issues that could show up in many different error messages that would be less specific
}
