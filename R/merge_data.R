

merge_data <- function(directory){

  if(dir.exists(file.path(directory, "participants"))){
    p <- list.files(file.path(directory, "participants"), full.names = TRUE)
  } else{
    f <- list.files(directory, recursive = TRUE, full.names = TRUE)
    p <- f[-grep(pattern = ".pdf$|Controls|Data_Dictionary|Codebook", f)]
  }

  files2merge <- p[!grepl("Demographics|biospecimen|Metabolomics|ApoE|MRI", p)]

  demo <- p[grepl("Demographics", p)] |>
    read.csv() |>
    dplyr::arrange(subject_label, event_sequence)

  data <- demo

  for(i in files2merge){
    print(basename(i))
    temp <- read.csv(i)
    temp$update_stamp <- NULL
    if(grepl("MRI", basename(i))){ # MRI seems to have duplicated IDs
      temp <- temp[!duplicated(temp), ]
    }
    if("site_id" %in% colnames(temp)){
      data <- dplyr::left_join(data, temp, by = c("subject_label", "site_id", "event_sequence"))
    } else{
      data <- dplyr::left_join(data, temp, by = c("subject_label", "event_sequence"))
    }
  }

  write.csv(data, file = file.path(directory, "merged_data.csv"), row.names = FALSE)

}














