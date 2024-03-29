% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_survey_choices.R
\name{get_survey_choices}
\alias{get_survey_choices}
\title{Get choices from survey name (and concatenate if you want!)}
\usage{
get_survey_choices(survey, choices, col, conc = TRUE, label = FALSE, sep = "_")
}
\arguments{
\item{survey}{A survey sheet from Kobo (already split with columns list_name and name present).}

\item{choices}{A choices sheet from Kobo (with column list_name, label and name).}

\item{col}{A column.}

\item{conc}{Boolean. Should choices be concatenated to column name? Default to TRUE. Can only be used together with label = FALSE.}

\item{label}{Boolean. Should the labels be returned? It returns a data.frame with two columns: "name" and "label".}

\item{sep}{Separator for choices' concatenation.}
}
\value{
A character vector of choices or pasted to \code{col} choices with a separator.
}
\description{
Get choices from survey name (and concatenate if you want!)
}
