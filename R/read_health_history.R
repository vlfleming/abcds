#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param directory PARAM_DESCRIPTION
#' @param person PARAM_DESCRIPTION, Default: c("participants", "controls")
#' @param include_demographics PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname read_health_history
#' @export 

read_health_history <- function(directory, person = c("participants", "controls"), include_demographics = FALSE){
  
  if(length(person) == 1) person <- match.arg(person)
  
  files <- list.files(directory, pattern = "Health_History", full.names = TRUE)
  
  files <- files[!grepl("Followup|Medications", files)]
  
  person_types <- unname(sapply(files, .detect_person))
  
  if("participants" %in% person & "participant" %in% person_types){
    participants <- utils::read.csv(files[!grepl("Controls", files)])
    if("update_stamp" %in% colnames(participants)) participants$update_stamp <- NULL
    if(include_demographics){
      participants <- merge(
        read_demographics(
          directory,
          person = "participants"),
        participants, by = c("subject_label", "event_sequence", "language_code", "language_label")
        )
    }
    class(participants) <- c("tbl_df", "tbl", "data.frame")
  }
  
  if("controls" %in% person & "control" %in% person_types){
    controls <- utils::read.csv(files[grepl("Controls", files)])
    if("update_stamp" %in% colnames(controls)) controls$update_stamp <- NULL
    if(include_demographics){
      controls <- merge(
        read_demographics(
          directory,
          person = "controls"),
        controls, by = c("subject_label", "event_sequence", "language_code", "language_label")
      )
    }
    class(controls) <- c("tbl_df", "tbl", "data.frame")
  }
  
  if(exists("participants", inherits = FALSE) & exists("controls", inherits = FALSE)){
    return(list(participants = participants, controls = controls))
  } else if(exists("participants", inherits = FALSE)){
    return(participants)
  } else if(exists("controls", inherits = FALSE)){
    return(controls)
  } else{
    stop("Did not find any health history files")
  }
}





