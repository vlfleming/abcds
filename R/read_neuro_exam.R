read_neuro_exam <- function(directory, include_demographics = FALSE, ...){
  
  variables <- list(...)
  
  files <- list.files(directory, pattern = "Physical_and_Neurological", full.names = TRUE)
  
  person = "participants" # Setting this for similarities to other functions (combine at some point)
  
  person_types <- unname(sapply(files, .detect_person))
  
  if("participants" %in% person & "participant" %in% person_types){
    participants <- read.csv(files[!grepl("Controls", files)])
    if("update_stamp" %in% colnames(participants)) participants$update_stamp <- NULL
    if(include_demographics){
      participants <- merge(
        read_demographics(
          directory,
          person = "participants"),
        participants, by = c("subject_label", "event_sequence", "language_code", "language_label"))
    }
    class(participants) <- c("tbl_df", "tbl", "data.frame")
  }
  
  if(length(variables) != 0){
    participants <- participants[, c("subject_label", "event_sequence", unlist(variables))]
  }
  
  if(exists("participants", inherits = FALSE)){
    return(participants)
  } else {
    stop("Did not find any physical and neurological exam files")
  }
  
}