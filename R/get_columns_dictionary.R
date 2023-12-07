#' Get column names as a dictionary.
#'
#' @param df Some Kobo data.
#' @param survey Some survey sheet, with a split 'type' column, e.g. with `split_survey(type)`. It must have columns 'list_name', 'type', 'label', and 'name'.
#' @param name_as_label Default to TRUE. Should the variable name be used as the label if label is missing?
#'
#' @return A dictionary.
#'
#' @export
get_columns_dictionary <- function(df, survey, name_as_label = FALSE){

  rlang::check_installed("labelled", reason = "to use `get_columns_dictionary()`")

  labelled_columns_data <- label_columns(df, survey, name_as_label)

  dictionary <- labelled::generate_dictionary(labelled_columns_data, labels = FALSE, values = FALSE)

  dictionary <- dplyr::select(dictionary, !dplyr::all_of(c("levels", "value_labels")))

  return(dictionary)
}
