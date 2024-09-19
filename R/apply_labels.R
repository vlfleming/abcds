apply_labels <- function(data){

  load(system.file("extdata/data_dictionary.RData", package = "abcds"))

  for(i in names(data)){
    attr(data[[i]], "label") <- data_dictionary$field_question[which(data_dictionary$field_name == i)]
  }

  return(data)
}
