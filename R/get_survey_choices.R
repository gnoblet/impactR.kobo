#' Get choices from survey name (and concatenate if you want!)
#'
#' @param survey A survey sheet from Kobo (already split with columns list_name and name present).
#' @param choices A choices sheet from Kobo (with column list_name, label and name).
#' @param col A column.
#' @param conc Boolean. Should choices be concatenated to column name? Default to TRUE. Can only be used together with label = FALSE.
#' @param label Boolean. Should the labels be returned? It returns a data.frame with two columns: "name" and "label".
#' @param sep Separator for choices' concatenation.
#'
#' @return A character vector of choices or pasted to `col` choices with a separator.
#'
#' @export
get_survey_choices <- function(survey, choices, col, conc = TRUE, label = FALSE, sep = "_"){

  col_name <- rlang::as_name(rlang::enquo(col))

  to_return <- dplyr::filter(survey, rlang::.data$name == col_name)
  to_return <- dplyr::pull(to_return, "list_name")

  if (length(to_return) == 0) {

    rlang::warn(glue::glue("Col: '{col_name}' is not in survey$name.", "An empty vector or an empty tibble is returned.", .sep = "\n"))

    if (label) {
      return(tibble::tibble(
        name = character(),
        label = character()
      ))
    } else if (!label) {
      return(character())
    }
  }

  # If there are more than one row, throw a warning but continue keeping the 1st row
  if (length(to_return) > 1) {

    rlang::warn(glue::glue(sep = "\n", "There are more than one line in the survey sheet for col '{col_name}'.", "The head was sliced to go on, but please check."))

    to_return <- to_return[1]
  }

  if (is.na(to_return)) {

    rlang::warn(glue::glue("There is no list_name listed in survey for col: '{col_name}'.", "An empty vector or an empty tibble is returned, please check.", .sep = "\n"))

    if (label) {
      return(tibble::tibble(
        name = character(),
        label = character()
      ))
    } else if (!label) {
      return(character())
    }
  }

  if (length(impactR.utils::subvec_in(to_return, choices[["list_name"]])) == 0) {

    rlang::warn(glue::glue("There is no corresponding list_name in choices for col: '{col_name}'.", "An empty vector or an empty tibble is returned.", .sep = "\n"))

    if (label) {
      return(tibble::tibble(
        name = character(),
        label = character()
      ))
    } else if (!label) {
      return(character())
    }
  }

  to_return <- dplyr::filter(choices, rlang::.data$list_name == to_return)

  if (!label) {
    to_return <- dplyr::pull(to_return, "name")

    if (rlang::is_true(conc)) {
      to_return <- stringr::str_c(col_name, to_return, sep = sep)
    }} else {
      to_return <- to_return |>
        dplyr::select("name", "label")
    }


  return(to_return)
}
