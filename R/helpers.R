.detect_person <- function(file){
  if(grepl("Control", file)){
    return("control")
  } else {
    return("participant")
  }
}

.create_meta_information <- function(files, remove_date = FALSE){
  file <- basename(files[grepl("Demographics", files) & !grepl("Controls", files)])
  fileMeta <- unlist(strsplit(gsub(".csv$", "", file), split = "_"))
  dateRaw <- fileMeta[length(fileMeta)]
  dateFormatted <- format(as.Date(dateRaw, format = "%d%b%Y"), "%B %d, %Y")

  if(remove_date){
    for(i in list.files(dirname(files)[1], full.names = TRUE)){
      if(grepl(dateRaw, basename(i))){
        file.rename(from = i, to = gsub("_|-", "", gsub(dateRaw, "", i)))
      }
    }
  }

  new_files <- list.files(dirname(files)[1], full.names = TRUE, pattern = ".pdf$|.csv$")

  fileCon <- file(file.path(dirname(files)[1], "abcdsMeta.txt"))
  writeLines(sprintf("Date Updated: %s", dateFormatted), fileCon)
  close(fileCon)

  return(new_files)
}


.create_new_directories <- function(directory){

  new_directories <- c("download", "documents", "controls", "codebook", "participants")

  for(i in new_directories){
    if(!dir.exists(file.path(directory, i))){
      dir.create(file.path(directory, i))
    }
  }
}

.remove_all_na <- function(data){
  # Filter out rows and columns with all missing data
  data[rowSums(is.na(data) | data == "") < ncol(data), colSums(is.na(data) | data == "") < nrow(data)]
}

check_variables <- function(...){
  
  variables <- list(...)
  
  load(system.file("extdata/data_dictionary.RData", package = "abcds"))
  
  unlist(variables[variables %in% data_dictionary$field_name])

}



.print_description <- function(){
  message(
  "  ADDS: Alzheimer's Disease Down Syndrome
  - New York, NY (Columbia University/New York State Institute for Basic Research in Developmental Disabilities); 
  - Boston, MA (Massachusetts General Hospital, Harvard University); 
  - Irvine, CA (the University of California, Irvine)\n
  NiAD: Neurodegeneration in Aging Down Syndrome
  - Pittsburgh, PA (University of Pittsburgh); 
  - Madison, WI (University of Wisconsin); 
  - St. Louis, MO (Washington University); 
  - Cambridge, England (University of Cambridge)"
  )
}


