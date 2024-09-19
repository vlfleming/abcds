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
#' @rdname read_apoe
#' @export 

read_apoe <- function(directory, person = c("participants", "controls"), include_demographics = FALSE){

  if(length(person) == 1) person <- match.arg(person)

  files <- list.files(directory, pattern = "ApoE", full.names = TRUE)

  person_types <- unname(sapply(files, .detect_person))

  if("participants" %in% person & "participant" %in% person_types){
    participants <- read.csv(files[!grepl("Controls", files)])
    if("update_stamp" %in% colnames(participants)) participants$update_stamp <- NULL
    if(include_demographics){
      participants <- merge(
        read_demographics(
          directory,
          person = "participants",
          event_sequence = 1),
        participants, by = "subject_label")
    }
    class(participants) <- c("tbl_df", "tbl", "data.frame")
  }

  if("controls" %in% person & "control" %in% person_types){
    controls <- read.csv(files[grepl("Controls", files)])
    if("update_stamp" %in% colnames(controls)) controls$update_stamp <- NULL
    if(include_demographics){
      controls <- merge(
        read_demographics(
          directory,
          person = "controls",
          event_sequence = 1),
        controls, by = "subject_label"
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
    stop("Did not find any ApoE files")
  }
}


# Create a read demographics and latency to auto-merge those data points


#' read_apoe
#' read_cognition
#' read_amyloid_pet
#' read_consensus
#' read_meta?? Like consent tracking, enrollment verification, registration, etc.
#' read_family_history
#' read_health_history
#' karyotyping
#' medications
#' mri scan
#' neuropathology
#' partner concerns
#' physical and neurological exam
#' premorbid functioning
#' blood
#' csf




