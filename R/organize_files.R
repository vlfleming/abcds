
organize_files <- function(directory, copy = TRUE, remove_date = FALSE){

  original_files <- list.files(directory, full.names = TRUE)

  .create_new_directories(directory)

  if(copy){
    for(i in original_files){
      newFile <- file.path(directory, "download", basename(i))
      if(!exists(newFile)) file.copy(from = i, to = newFile)
    }
  }

  new_files <- .create_meta_information(original_files, remove_date = TRUE)


  d <- new_files[grep(".pdf$", new_files)]
  invisible(file.copy(d, file.path(directory, "documents", basename(d))))
  invisible(file.remove(d))

  c <- new_files[grep("Controls", new_files)]
  invisible(file.copy(c, file.path(directory, "controls", basename(c))))
  invisible(file.remove(c))

  cb <- new_files[grep("DataDictionary|Codebook", new_files)]
  invisible(file.copy(cb, file.path(directory, "codebook", basename(cb))))
  invisible(file.remove(cb))

  p <- list.files(directory, pattern = ".csv$", full.names = TRUE)
  invisible(file.copy(p, file.path(directory, "participants", basename(p))))
  invisible(file.remove(p))

}

restore_original_files <- function(directory){

  download_files <- list.files(file.path(directory, "download"), full.names = TRUE)

  if(file.exists(file.path(directory, "abcdsMeta.txt"))) file.remove(file.path(directory, "abcdsMeta.txt"))

  invisible(file.copy(from = download_files, to = file.path(directory, basename(download_files))))

  sub_directories <- c("download", "documents", "controls", "codebook", "participants")

  for(i in sub_directories){
    unlink(file.path(directory, i), recursive = TRUE)
  }

}



# organize_files(directory, copy = TRUE, remove_date = TRUE)
#
# restore_original_files(directory)
