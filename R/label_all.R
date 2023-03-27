#' Label all select_one and select_multiple questions
#'
#' It uses `label_all_select_one()` and `label_all_select_multiple()`in the background.
#'
#' @param df Some kobo data.
#' @param survey Some survey sheet, with a split 'type' column, e.g. with `split_survey(survey, type)`. It must have columns 'list_name', 'type', 'label', and 'name'.
#' @param choices The corresponding choices sheet; it must have columns 'name' and 'label'.
#' @param id_col The survey id column; usually uuid.
#'
#' @return A labeled data frame.
#'
#' @export
label_all <- function(df, survey, choices, id_col) {

  df <- label_all_select_multiple(df, survey, choices, {{ id_col }})
  df <- label_all_select_one(df, survey, choices, {{ id_col }})

  return(df)

}
