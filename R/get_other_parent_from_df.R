#' Guess other parent columns from child's pattern.
#'
#' `get_other_parent_from_df()` assumes that the parent-to-child other answers were designed as 'parent' and 'other_parent' ('other_' being a pattern preceeding) and retrieves the other parent parent columns from the dataset. `get_other_parent_from_survey()` has the same rational though it retrieves column names from the survey sheet.
#'
#' @inheritParams get_other_from_df
#'
#' @return A character vector of other parent columns.
#'
#' @export
get_other_parent_from_df <- function(df, other){
  UseMethod("get_other_parent_from_child")
}

#' @export
get_other_parent_from_df.data.frame <- function(df, other){

  other_cols <- get_other_from_df(df, other)

  other_parent_cols <- stringr::str_remove(other_cols, other_cols)

  if (length(other_parent_cols) == 0) {
    rlang::warn("Did you provide the right `other` pattern or was the tool designed that way? There is no other parent column.")
  }

  return(other_parent_cols)
}



#' @export
#'
#' @inheritParams get_other_from_survey
#'
#' @rdname get_other_parent_from_df
get_other_parent_from_survey <- function(survey, other) {
  UseMethod("get_other_parent_from_survey")
}

#' @export
get_other_parent_from_survey.data.frame <- function(survey, other) {

  other_cols <- get_other_from_survey(survey, other)

  # Get parent
  other_parent_cols <- stringr::str_remove(other_cols, other)

  if (length(other_parent_cols) == 0) {
    rlang::warn("Did you provide the right `other` pattern or was the tool designed that way? There is no other parent column.")
  }

  return(other_parent_cols)
}

