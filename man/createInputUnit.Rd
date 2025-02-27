% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app.R
\name{createInputUnit}
\alias{createInputUnit}
\title{Create Input Unit}
\usage{
createInputUnit(header, ..., button_label, button_id)
}
\arguments{
\item{header}{General title for all the information needed in this unit}

\item{button_label}{label on the submission button. Note this should be the second to last parameter}

\item{button_id}{ID to call  the submitted information in this input field/s. Note this should be the last parameter}
}
\value{
creates and displays the input units components wanted in the UI of shiny
}
\description{
This app needs to query different type of information to create its tools such as income, expenses, disposable income, debts etc they are all different data but all need a header title and a certain number of input fields of certain types. This function generalizes those "units". See wireframe for visual example.  Function to create input fields that contain a header, paragraph, and variable number of input elements.

Required Parameters:
}
