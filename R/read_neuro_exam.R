#' @title read_neuro_exam
#' @description Reads the file containing the physical and neurological exam
#'   from the directory for the participants and controls.
#' @param directory A path name containing the downloaded ABC-DS data from the
#'   University of South Carolina Laboratory of Neuro Imaging's (LONI) Image
#'   and Data Archive
#' @param include_demographics An optional parameter to merge demographic information
#'   before returning the data, Default: FALSE
#' @param ... An optional list of variables in the Physical and Neurological Exam file
#'   to include if the end user does not want to return all variables. Commonly used
#'   variables include `wt` (weight), `ht` (height), `bpsys` (systolic blood pressure), \
#'   `bpdia` (diastolic blood pressure), `hdcirc` (head circumference),
#'   `nkcirc` (neck circumference), and `gait` (gait abnormalities).
#' @return A data frame of the physical and neurological exam of the participants or
#'   controls or a list containing the physical and neurological exam of the
#'   participants and controls.
#' @details Reads the file containing the physical and neurological exam from
#'   the directory for the participants and controls. End users also have an option
#'   to include the demographics with the `include_demographics` argument.
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
