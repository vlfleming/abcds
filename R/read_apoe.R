#' @title read_apoe
#' @description Reads the file containing the APOE results from the directory
#'   for the participants and controls.
#' @param directory A path name containing the downloaded ABC-DS data from the
#'   University of South Carolina Laboratory of Neuro Imaging's (LONI) Image
#'   and Data Archive
#' @param person The specific group for which to read in the data, Default: c("participants", "controls")
#' @param include_demographics An optional parameter to merge demographic information
#'   before returning the data, Default: FALSE
#' @return A data frame of the APOE results of the participants or controls or
#'   a list containing the APOE results of the participants and controls.
#' @details Reads the file containing the APOE results from the directory
#'   for the participants and controls. End users also have an option to
#'   include the demographics with the `include_demographics` argument.
#' @rdname read_apoe
#' @export

read_apoe <- function(directory, person = c("participants", "controls"), include_demographics = FALSE){

  if(length(person) == 1) person <- match.arg(person)

  files <- list.files(directory, pattern = "ApoE", full.names = TRUE)

  person_types <- unname(sapply(files, .detect_person))

  if("participants" %in% person & "participant" %in% person_types){
    participants <- utils::read.csv(files[!grepl("Controls", files)])
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
    controls <- utils::read.csv(files[grepl("Controls", files)])
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




