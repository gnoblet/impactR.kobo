#' Get survey labels from survey names
#'
#' @param survey A survey sheet from Kobo (already split with columns label and name present).
#' @param ... Column names, quoted or unquoted.
#' @param output_df Boolean. Should the output be a data frame or a vector? Default to TRUE.
#'
#' @return The labels as a vectir.
#'
#' @export
get_survey_labels <- function(survey, ..., output_df = TRUE) {

  col_names <- purrr::map_chr(rlang::enquos(...), rlang::as_name)

  to_return <- dplyr::filter(survey, rlang::.data$name %in% col_names)
  to_return <- dplyr::select(to_return, "name", "label")

  if (nrow(to_return) == 0) {

    rlang::warn(glue::glue("None of the column names were found in survey$name.", "An empty vector or an empty tibble is returned.", .sep = "\n"))

    if (output_df) {
      return(tibble::tibble(
        name = character(),
        label = character()
      ))
    } else if (!output_df) {
      return(character())
    }
  }

  to_return_distinct <- dplyr::distinct(to_return, !!rlang::sym("label"), !!rlang::sym("name"), .keep_all = TRUE)

  if (nrow(to_return_distinct) < nrow(to_return)) {

    rlang::warn(glue::glue(sep = "\n", "There are some duplicated lines.", "The duplicates were removed, but please check."))
    to_return <- to_return_distinct
  }

  if (output_df) {

    return(to_return)

  } else {

    to_return <- dplyr::pull(to_return, "label")

    return(to_return)
  }

}

