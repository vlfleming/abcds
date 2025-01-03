#' @title apply_labels
#' @description Apply labels to an existing data set from the ABC-DS data dictionary
#' @param data An data set existing in the user's global environment for
#'   which to apply the labels
#' @return A data set that labels all variables existing in the ABC-DS data dictionary
#' @details Apply labels to an existing data set from the ABC-DS data dictionary.
#'   This function must be used before the `apply_factor_labels` function or the labels
#'   will be removed from the data set.
#' @rdname apply_labels
#' @export

apply_labels <- function(data){

  load(system.file("extdata/data_dictionary.RData", package = "abcds"))

  for(i in names(data)){
    attr(data[[i]], "label") <- data_dictionary$field_question[which(data_dictionary$field_name == i)]
  }

  return(data)
}
