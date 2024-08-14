read_demographics <- function(directory,
                              person = c("participants", "controls"),
                              event_sequence = NULL,
                              simplify = FALSE){

  if(length(person) == 1) person <- match.arg(person)

  demographic_files <- list.files(directory, pattern = "Demographics", full.names = TRUE)

  age_and_latency_files <- list.files(directory, pattern = "Age_at_Event", full.names = TRUE)

  person_types <- unname(sapply(demographic_files, .detect_person))

  simplified_demographics <- c("age_at_visit", "de_gender", "de_race", "de_ethnicity")

  if("participants" %in% person & "participant" %in% person_types){
    participants <- read.csv(demographic_files[!grepl("Controls", demographic_files)])
    if("update_stamp" %in% colnames(participants)) participants$update_stamp <- NULL
    participants_age <- read.csv(age_and_latency_files[!grepl("Controls", age_and_latency_files)])
    participants <- merge(
      participants,
      participants_age[, c("subject_label", "event_sequence", "age_at_visit")],
      by = c("subject_label", "event_sequence")
      )
    if(is.null(event_sequence)) event_sequence <- max(participants$event_sequence)
    participants <- participants[participants$event_sequence <= event_sequence, ]
    participants <- participants[order(participants$subject_label, participants$event_sequence), ]
    class(participants) <- c("tbl_df", "tbl", "data.frame")
    if(simplify) participants <- participants[, c("subject_label", "event_sequence", simplified_demographics)]
  }

  if("controls" %in% person & "control" %in% person_types){
    controls <- read.csv(demographic_files[grepl("Controls", demographic_files)])
    if("update_stamp" %in% colnames(controls)) controls$update_stamp <- NULL
    controls_age <- read.csv(age_and_latency_files[grepl("Controls", age_and_latency_files)])
    controls <- merge(
      controls,
      controls_age[, c("subject_label", "event_sequence", "age_at_visit")],
      by = c("subject_label", "event_sequence")
    )
    if(is.null(event_sequence)) event_sequence <- max(controls$event_sequence)
    controls <- controls[controls$event_sequence <= event_sequence, ]
    controls <- controls[order(controls$subject_label, controls$event_sequence), ]
    class(controls) <- c("tbl_df", "tbl", "data.frame")
    if(simplify) controls <- controls[, c("subject_label", "event_sequence", simplified_demographics)]
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
