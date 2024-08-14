add_latency <- function(directory, data = NULL,
                        person = c("participants", "controls"),
                        visit = c("clinical", "amy", "tau", "fdg", "mri", "csf")){

  if(length(person) == 1) person <- match.arg(person)

  if(length(visit) == 1) visit <- match.arg(visit)

  files <- list.files(directory, pattern = "Latency", full.names = TRUE)

  person_types <- unname(sapply(files, .detect_person))

  if("participants" %in% person & "participant" %in% person_types){
    participants <- read.csv(files[!grepl("Controls", files)])
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
    controls <- read.csv(files[grepl("Controls", files)])
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



