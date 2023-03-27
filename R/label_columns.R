#' Label Kobo data column names as attributes
#'
#' @param df Some Kobo data.
#' @param survey Some survey sheet, with a split 'type' column, e.g. with `split_survey(type)`. It must have columns 'list_name', 'type', 'label', and 'name'.
#' @param name_as_label Default to TRUE. Should the variable name be used as the label if label is missing?
#' @param as_attribute Default to FALSE. Should the names be as attributes or column names?
#'
#' @return Labeled columns as attributes.
#'
#' @export
label_columns <- function(df, survey, name_as_label = TRUE){

  rlang::check_installed("labelled", reason = "to use `label_columns()`")

  survey <- tidyr::drop_na(survey, "name")

  if (name_as_label) {
    survey <-  dplyr::mutate(survey, label = ifelse(is.na(.data$label), .data$name, .data$label))
  }

  added_cols <- subvec_not_in(colnames(data), survey$name)

  var_labels <- purrr::set_names(survey[["label"]], survey[["name"]])

  var_labels <- as.list(var_labels)

  if(length(added_cols) > 0) {

    var_labels_added <- purrr::set_names(added_cols, added_cols)

    var_labels_added <- as.list(var_labels_added)

    var_labels <- c(var_labels, var_labels_added)
  }

  df <- labelled::set_variable_labels(df, .labels = var_labels, .strict = FALSE)

  return(df)

}
