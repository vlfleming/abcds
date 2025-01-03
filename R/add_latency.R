#' @title add_latency
#' @description Reads the file containing the latency variables from the directory
#'   for the participants and controls and optionally adds the latency variable
#'   to a data set.
#' @param directory A path name containing the downloaded ABC-DS data from the
#'   University of South Carolina Laboratory of Neuro Imaging's (LONI) Image
#'   and Data Archive
#' @param data An optional data set existing in the user's global environment for
#'   which to add the latency variable(s), Default: NULL
#' @param person The specific group for which to read in the data, Default: c("participants", "controls")
#' @param visit The latency variable to read in or add to the existing data set. Options can include any or
#'   all of the following: `clinical` for the clinic visit, `amy` for the amyloid PET scan, `tau` for the
#'   tau PET scan, `fdg` for the fdg PET scan, `mri` for the structural MRI, and `csf` for the cerebral
#'   spinal fluid draw, Default: c("clinical", "amy", "tau", "fdg", "mri", "csf")
#' @return A data frame of the demographics of the participants or controls or
#'   a list containing the demographics of the participants and controls.
#' @details Reads the file containing the latency variables from the directory
#'   for the participants and controls and optionally adds the latency variable
#'   to a data set.
#' @rdname add_latency
#' @importFrom utils read.csv
#' @export

add_latency <- function(directory, data = NULL,
                        person = c("participants", "controls"),
                        visit = c("clinical", "amy", "tau", "fdg", "mri", "csf")){

  if(length(person) == 1) person <- match.arg(person)

  if(length(visit) == 1) visit <- match.arg(visit)

  files <- list.files(directory, pattern = "Latency", full.names = TRUE)

  person_types <- unname(sapply(files, .detect_person))

  if("participants" %in% person & "participant" %in% person_types){
    participants <- utils::read.csv(files[!grepl("Controls", files)])
    if("update_stamp" %in% colnames(participants)) participants$update_stamp <- NULL
    participants <- participants[, c("subject_label", "event_sequence", paste0(visit, "_latency_in_days"))]
    if(!is.null(data)){
      if(all(c("subject_label", "event_sequence") %in% colnames(data))){
        participants <- merge(
          data, participants,
          by = c("subject_label", "event_sequence")
        )
      }
    }
    participants <- participants[order(participants$subject_label, participants$event_sequence), ]
    class(participants) <- c("tbl_df", "tbl", "data.frame")
  }

  if("controls" %in% person & "control" %in% person_types){
    controls <- utils::read.csv(files[grepl("Controls", files)])
    if("update_stamp" %in% colnames(controls)) controls$update_stamp <- NULL
    controls <- controls[, c("subject_label", "event_sequence", paste0(visit, "_latency_in_days"))]
    if(!is.null(data)){
      if(all(c("subject_label", "event_sequence") %in% colnames(data))){
        controls <- merge(
          data, controls,
          by = c("subject_label", "event_sequence")
        )
      }
    }
    controls <- controls[order(controls$subject_label, controls$event_sequence), ]
    class(controls) <- c("tbl_df", "tbl", "data.frame")
  }

  if(exists("participants", inherits = FALSE) & exists("controls", inherits = FALSE)){
    return(list(participants = participants, controls = controls))
  } else if(exists("participants", inherits = FALSE)){
    return(participants)
  } else if(exists("controls", inherits = FALSE)){
    return(controls)
  } else{
    stop("Did not find any demographic files")
  }

}



