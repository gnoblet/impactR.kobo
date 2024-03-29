% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_columns_dictionary.R
\name{get_columns_dictionary}
\alias{get_columns_dictionary}
\title{Get column names as a dictionary.}
\usage{
get_columns_dictionary(df, survey, name_as_label = FALSE)
}
\arguments{
\item{df}{Some Kobo data.}

\item{survey}{Some survey sheet, with a split 'type' column, e.g. with \code{split_survey(type)}. It must have columns 'list_name', 'type', 'label', and 'name'.}

\item{name_as_label}{Default to TRUE. Should the variable name be used as the label if label is missing?}
}
\value{
A dictionary.
}
\description{
Get column names as a dictionary.
}
