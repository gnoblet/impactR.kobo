#' Get survey label from survey name
#'
#' @param survey A survey sheet from Kobo (already split with columns label and name present).
#' @param col A column.
#'
#' @return The label or NA if it does not exist.
#'
#' @export
get_survey_label <- function(survey, col){

  col_name <- rlang::as_name(rlang::enquo(col))

  to_return <- dplyr::filter(survey, rlang::.data$name == col_name)
  to_return <- dplyr::pull(to_return, "label")

  if (length(to_return) == 0) {

    rlang::warn(glue::glue("Column '{col_name}' is not in survey$name.", "An empty character string is returned.", .sep = "\n"))
  }

  # If there are more than one row, throw a warning but continue keeping the 1st row
  if (length(to_return) > 1) {

    rlang::warn(glue::glue(sep = "\n", "There are more than one line in the survey sheet for col '{col_name}'.", "The head was sliced to go on, but please check."))

    to_return <- to_return[1]
  }

  return(to_return)
}
