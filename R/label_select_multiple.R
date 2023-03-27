#' Label a select_multiple question
#'
#' `label_select_multiple()` labels one variable. `label_all_select_multiple()` label all select_multiple variables that exists in the survey sheet and in the data set.
#'
#' @param df Some kobo data.
#' @param survey Some survey sheet, with a split 'type' column, e.g. with `split_survey(type)` and at least one 'label' column.
#' @param choices The corresponding choices sheet; it must have columns name and label.
#' @param id_col The id column; usually uuid.
#' @param col The select_one column to label.
#' @param return_df NULL returns the updated data frame; "id_col" returns a data frame with only col and id_col; and, "vector" returns.
#'
#' @return A labeled data frame, sub-data frame or vector for `label_select_multiple()`; a labeled data frame for `label_all_select_multiple()`.
#'
#' @export
label_select_multiple <- function(df, survey, choices, id_col, col, return_df = NULL){

  # Check if survey as the right column names
  check_survey(survey)

  # Get col_name
  col_name <- rlang::as_name(rlang::enquo(col))

  # Check if variable is a select_one
  select_multiples <- get_survey_select_multiple(survey)
  if (!(col_name) %in% select_multiples) rlang::abort(glue::glue(col_name, " does not exist in survey with type 'select_multiples', please check."))

  # A dictionary by construct
  dict <- get_survey_choices(survey, choices, {{ col_name }}, label = TRUE)

  # Deframe to get a named vector
  dict <- tibble::deframe(dict)

  if (length(dict) == 0) {
    recoded <- df

    rlang::warn(paste0(
      "There was no choices value to recode for column: ",
      rlang::as_name(rlang::enquo(col))
    ))
  } else {

    if (all(is.na(df[[col_name]]))) {
      recoded <- df
    } else {

      recoded <- tidyr::separate_rows(df, {{ col }}, sep = " ")
      recoded <- dplyr::mutate(recoded, "{col_name}" := as.character(!!rlang::sym(rlang::ensym(col))))

      recoded <- dplyr::mutate(recoded, "{col_name}" := dplyr::recode(!!rlang::sym(rlang::ensym(col)), !!!dict))

      recoded <- dplyr::group_by(recoded, {{ id_col }})

      recoded <- dplyr::mutate(recoded, "{{ col }}" := paste0({{ col }}, collapse = " "))

      recoded <- dplyr::ungroup(dplyr::distinct(recoded))

      recoded <- impactR::recode_values(recoded, "NA", NA, {{ col }})
    }
  }

  if (!is.null(return_df)) {
    if (return_df == "vector"){
      recoded <- dplyr::pull(recoded, {{ col }})
    } else if (return_df == "id_col") {
      recoded <- dplyr::select(recoded, {{ id_col }}, {{ col }})
    }
  }
  return(recoded)

}


#' @rdname label_select_multiple
#' @export
label_all_select_multiple <- function(df, survey, choices, id_col){

  id_col_name <- rlang::as_name(rlang::enquo(id_col))
  col_names <- colnames(df)

  select_multiples <- get_survey_select_multiple(survey)

  recoded <- purrr::map(
    select_multiples,
    \(x) label_select_multiple(df, survey, choices, {{ id_col }}, {{ x }}, return_df = "id_col")
  )

  recoded <- impactR::left_joints(recoded, {{ id_col }})

  df <- impactR::diff_tibbles(df, recoded, {{ id_col }})

  recoded <- dplyr::left_join(df, recoded, by = id_col_name)
  recoded <- dplyr::relocate(recoded, dplyr::all_of(col_names))
  return(recoded)
}
