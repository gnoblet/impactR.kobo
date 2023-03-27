#' @title Split survey type and list name
#'
#' @param survey A survey sheet from Kobo
#' @param col_to_split Usually `type`
#' @param into Vector of columns names to split to. Default to c("type", "list_name" )
#' @param sep The separator. Default to " ".
#' @param fill How to fill. Default to NA on the right.
#' @param ... Params to pass to `tidyr::separate()`.
#'
#' @return  A survey tibble, split
#'
#' @export
split_survey <- function(survey, col_to_split, into = c("type", "list_name"),
                         sep = " ", fill = "right", ...) {

  # Check if the column to split is in survey
  col_to_split_name <- rlang::as_name(rlang::enquo(col_to_split))
  if_not_in_stop(survey, col_to_split_name, "survey", "col_to_split")

  # Split
  survey <- tidyr::separate(survey, {{ col_to_split }},
    into = into,
    sep = sep,
    fill = fill,
    ...
  )
  return(survey)
}
