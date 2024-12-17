#' @title expand_lookup
#' @description Expands the look-up table in cases where there are ranges in the scores
#' @param data The data set containing the variable with the ranges
#' @param variable The variable containing the range as indicated by a dash between two numeric values
#' @return The expanded look-up table with the variable in descending order
#' @details Expands the look-up table in cases where there are ranges in the scores
#' @seealso
#'  \code{\link[data.table]{:=}}
#'  \code{\link[tibble]{tibble}}
#' @rdname expand_lookup
#' @export
#' @importFrom data.table `:=`
#' @importFrom tibble tibble


expand_lookup <- function(data, variable){

  `:=` <- data.table::`:=`

  lookupTable <-
    do.call(rbind, lapply(seq_along(data[[variable]]), FUN = function(i){
      if(grepl("-", data[[variable]][i])){
        bounds <- as.numeric(strsplit(data[[variable]][i], "-")[[1]])
        tibble::tibble(!!variable := seq(bounds[1], bounds[2]), data[i, -which(colnames(data) == variable)])
        } else {
          tibble::tibble(!!variable := as.numeric(data[[variable]][i]), data[i, -which(colnames(data) == variable)])
          }
      })
    )

  lookupTable[order(lookupTable[[variable]], decreasing = TRUE), ]

  }
