% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_demographics.R
\name{read_demographics}
\alias{read_demographics}
\title{read_demographics}
\usage{
read_demographics(
  directory,
  person = c("participants", "controls"),
  event_sequence = NULL,
  simplify = FALSE
)
}
\arguments{
\item{directory}{A path name containing the downloaded ABC-DS data from the
University of South Carolina Laboratory of Neuro Imaging's (LONI) Image
and Data Archive}

\item{person}{The specific group for which to read in the data, Default: c("participants", "controls")}

\item{event_sequence}{Harmonized event sequence referring to the data collection time point, Default: NULL}

\item{simplify}{Reduce the demographic variables to only include age,
gender, race, and ethnicity, Default: FALSE}
}
\value{
A data frame of the demographics of the participants or controls or
a list containing the demographics of the participants and controls.
}
\description{
Reads the file containing the demographics from the directory
for the participants and controls.
}
\details{
Reads the file containing the demographics from the directory
for the participants and controls. End users also have an option to simplify
the demographics included using the \code{simplify} argument or return the
demographics at a specific time point using \code{event_sequence}. Currently, the
\code{simplify} argument returns the \code{age_at_visit}, \code{de_gender}, \code{de_race}, and
\code{de_ethnicity} variables.
}
