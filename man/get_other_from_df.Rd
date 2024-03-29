% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_other_from_df.R
\name{get_other_from_df}
\alias{get_other_from_df}
\alias{get_other_from_survey}
\title{Guess other columns from child's pattern.}
\usage{
get_other_from_df(df, other)

get_other_from_survey(survey, other)
}
\arguments{
\item{df}{A data frame.}

\item{other}{A character vector of the start of all other column names. E.g., other = "other_".}

\item{survey}{Some survey sheet, with a split 'type' column, e.g. with \code{split_survey(survey, type)}. It must have columns 'list_name', 'type', 'label', and 'name'.}
}
\value{
A character vector of other columns.
}
\description{
\code{get_other_from_df} assumes that the parent-to-child other answers were designed as 'parent' and 'other_parent' ('other_' being a pattern preceeding) and retrieves the other columns from the dataset. \code{get_other_from_survey()} has the same rational though it retrieves column names from the survey sheet.
}
