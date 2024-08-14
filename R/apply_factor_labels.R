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

