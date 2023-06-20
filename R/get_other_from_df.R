#' Guess other columns from child's pattern.
#'
#' `get_other_from_df` assumes that the parent-to-child other answers were designed as 'parent' and 'other_parent' ('other_' being a pattern preceeding) and retrieves the other columns from the dataset. `get_other_from_survey()` has the same rational though it retrieves column names from the survey sheet.
#'
#' @param df A data frame.
#' @param other  A character vector of the start of all other column names. E.g., other = "other_".
#'
#' @return A character vector of other columns.
#'
#' @export
get_other_from_df <- function(df, other){
  UseMethod("get_other_from_df")
}

#' @export
get_other_from_df.data.frame <- function(df, other){

  other_cols <- colnames(df)[startsWith(colnames(df), other)]

  if (length(other_cols) == 0) {
    rlang::warn("Did you provide the right `other` pattern or was the tool designed that way? There is no other column.")
  }

  return(other_cols)
}




#' @export
#'
#' @param survey Some survey sheet, with a split 'type' column, e.g. with `split_survey(survey, type)`. It must have columns 'list_name', 'type', 'label', and 'name'.
#'
#' @rdname get_other_from_df
get_other_from_survey <- function(survey, other) {
  UseMethod("get_other_parent_from_survey")
}

#' @export
get_other_from_survey.data.frame <- function(survey, other) {

  check_survey(survey)

  # Filter rows of type "text" and starting with "other" pattern
  filtered <- dplyr::filter(survey,
                            rlang::.data$type == "text" ,
                            stringr::str_starts(rlang::.data$name, other))

  # Pull names
  other_parent_cols <- dplyr::pull(filtered, "name")

  if (length(other_parent_cols) == 0) {
    rlang::warn("Did you provide the right `other` pattern or was the tool designed that way? There is no other parent column.")
  }

  return(other_parent_cols)
}

