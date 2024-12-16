#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param directory PARAM_DESCRIPTION
#' @param include_demographics PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname read_neuro_exam
#' @export

read_neuro_exam <- function(directory, include_demographics = FALSE, ...){
  
  variables <- check_variables(...)
  
  files <- list.files(directory, pattern = "Physical_and_Neurological", full.names = TRUE)
  
  person = "participants" # Setting this for similarities to other functions (combine at some point)
  
  person_types <- unname(sapply(files, .detect_person))
  
  if("participants" %in% person & "participant" %in% person_types){
    participants <- utils::read.csv(files[!grepl("Controls", files)])
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
