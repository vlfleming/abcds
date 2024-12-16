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
#' @rdname read_cognitive
#' @export 

read_cognitive <- function(directory,
                           person = c("participants", "controls"),
                           include_demographics = FALSE){

  if(length(person) == 1) person <- match.arg(person)

  load(system.file("extdata/data_dictionary.RData", package = "abcds"))

  cog_labels <- gsub("-| |:|/", "_", unique(data_dictionary[startsWith(data_dictionary$dd_crf_name, "cog"), "dd_crf_label"]))

  if("participants" %in% person){
    participants <- data.frame()
    for(i in cog_labels[!grepl("Montreal", cog_labels)]){
      file <- list.files(directory, pattern = i, full.names = TRUE)
      if(length(file) != 0) temp_df <- utils::read.csv(file)
      if("update_stamp" %in% colnames(temp_df)) temp_df$update_stamp <- NULL
      if(all(dim(participants) == 0)){
        participants <- temp_df
      } else{
        participants <- merge(
          participants,
          temp_df,
          by = c("subject_label", "event_sequence", "language_code", "language_label"),
          all = TRUE)
      }
    }
    if(include_demographics){
      participants <- merge(
        read_demographics(
          directory,
          person = "participants"),
        participants, by = c("subject_label", "event_sequence", "language_code", "language_label"))
    }
    participants <- add_latency(directory, data = participants, person = "participants", visit = "clinical")
    participants <- participants[order(participants$subject_label, participants$event_sequence), ]
    class(participants) <- c("tbl_df", "tbl", "data.frame")
  }


  if("controls" %in% person){
    controls <- utils::read.csv(
      list.files(
        directory,
        pattern = cog_labels[grepl("Montreal", cog_labels)],
        full.names = TRUE)
      )
    if("update_stamp" %in% colnames(controls)) controls$update_stamp <- NULL
    if(include_demographics){
      controls <- merge(
        read_demographics(
          directory,
          person = "controls"),
        participants, by = c("subject_label", "event_sequence", "language_code", "language_label"))
    }
    controls <- add_latency(directory, data = controls, person = "controls", visit = "clinical")
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
    stop("Did not find any cognitive files")
  }


}
