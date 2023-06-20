#' Get questions by type from the survey sheet
#'
#' `get_survey_type()` is a generic; the other functions get the specified obvious type from the survey sheet.
#'
#' @param survey The survey sheet from Kobo (with column "type" split). See `split_survey()`
#' @param question_type Usually "integer", "select_one" or "select_multiple".
#'
#' @details `survey` should have a split type column with types of variables such as "select_one", "select_multiple", etc.
#'
#' @return A character vector of select_one questions.
#'
#' @export
get_survey_type <- function(survey, question_type){
  UseMethod("get_survey_type")
}

#' @export
get_survey_type.data.frame <- function(survey, question_type){

  # Check if type is in survey
  if_not_in_stop(survey, "type", "survey")

  # Check if type exists https://support.kobotoolbox.org/question_types.html
  if (!(question_type %in% c(
    "integer",
    "decimal",
    "range",
    "text",
    "select_one",
    "select_multiple",
    "select_one_from_file",
    "select_mutiple_from_file",
    "rank",
    "note",
    "geopoint",
    "geotrace",
    "geoshape",
    "date",
    "time",
    "dateTime",
    "image",
    "audio",
    "background-audio",
    "video",
    "file",
    "barcode",
    "calculate",
    "acknowledge",
    "hidden",
    "xml-external"
  ))) rlang::abort("Did you mean the right type? Please check: https://support.kobotoolbox.org/question_types.html.")

  got <- dplyr::filter(survey, !!rlang::sym("type") == question_type)

  got <- dplyr::pull(got, rlang::.data$name)

  # Warning if yielding zero row
  if (length(got) == 0) rlang::warn("The output is empty.")

  return(got)
}


#' @export
#' @rdname get_survey_type
get_survey_select_one <- function(survey) {
  get_survey_type(survey, "select_one")
}

#' @export
#' @rdname get_survey_type
get_survey_select_multiple <- function(survey) {
  get_survey_type(survey, "select_multiple")
}

#' @export
#' @rdname get_survey_type
get_survey_calculate <- function(survey) {
  get_survey_type(survey, "calculate")
}

#' @export
#' @rdname get_survey_type
get_survey_integer <- function(survey) {
  get_survey_type(survey, "integer")
}
