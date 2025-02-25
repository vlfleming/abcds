#' @title apply_factor_labels
#' @description Apply factor labels to an existing data set from the ABC-DS codebook
#' @param data An data set existing in the user's global environment for
#'   which to apply the factor labels
#' @return A data set that converts character variables to factors
#' @details Apply factor labels to an existing data set from the ABC-DS codebook.
#'   This function must be used before the `apply_labels` function or the labels
#'   will be removed from the data set.
#' @rdname apply_factor_labels
#' @export

apply_factor_labels <- function(data){

  load(system.file("extdata/codebook.RData", package = "abcds"))

  for(i in names(data)){
    if(i %in% codebook$field_name){
      data[[i]] <- factor(
        data[[i]],
        levels = codebook$field_code[which(codebook$field_name == i)],
        labels = codebook$field_code_label[which(codebook$field_name == i)])
    }
  }

  return(data)
}

