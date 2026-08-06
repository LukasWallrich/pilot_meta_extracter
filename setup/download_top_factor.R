# Download TOP Factor data from OSF
# Using the OSF download function

download.OSF.file <- function(GUID, Access_Token = NULL, file_name) {
  pacman::p_load(httr)
  pacman::p_load(rjson)

  # search for file private/public status
  GETurl <- paste0("https://api.osf.io/v2/files/", GUID)
  tempfile_path <- tempfile()
  req <- GET(GETurl, write_disk(tempfile_path, overwrite = T))
  json_data <- fromJSON(file = tempfile_path)

  if (length(json_data$data) > 0) {
    req1 <- GET(
      json_data$data$links$download,
      write_disk(file_name, overwrite = TRUE)
    )
    print(paste0(
      "The file has been downloaded to your working directory as: ",
      file_name
    ))
    return(TRUE)
  } else if (length(Access_Token) == 1) {
    if (grepl("https://osf.io", Access_Token) == TRUE) {
      req1 <- GET(
        paste0("https://api.osf.io/v2/files/", GUID, "/", gsub(".*/", "", Access_Token)),
        write_disk(tempfile_path, overwrite = TRUE)
      )
      json_data <- fromJSON(file = tempfile_path)
      if (length(json_data$data) > 0) {
        req1 <- GET(
          json_data$data$links$download,
          write_disk(file_name, overwrite = TRUE)
        )
        print(paste0(
          "The file has been downloaded to your working directory as: ",
          file_name
        ))
        return(TRUE)
      } else {
        print(json_data$errors[[1]]$detail[1])
        return(FALSE)
      }
    } else if (grepl("https://osf.io", Access_Token) == FALSE) {
      req1 <- GET(
        paste0("https://api.osf.io/v2/files/", GUID),
        write_disk(tempfile_path, overwrite = TRUE),
        add_headers("Authorization" = paste0("Bearer ", Access_Token))
      )
      json_data <- fromJSON(file = tempfile_path)
      if (length(json_data$data) > 0) {
        req1 <- GET(
          json_data$data$links$download,
          write_disk(file_name, overwrite = TRUE),
          add_headers("Authorization" = paste0("Bearer ", Access_Token))
        )
        print(paste0(
          "The file has been downloaded to your working directory as: ",
          file_name
        ))
        return(TRUE)
      } else {
        print(json_data$errors[[1]]$detail[1])
        return(FALSE)
      }
    } else {
      print(json_data$errors[[1]]$detail[1])
      return(FALSE)
    }
  } else {
    print(json_data$errors[[1]]$detail[1])
    return(FALSE)
  }
}

# Try to download TOP Factor data
# The GUID 'qatkz' refers to the OSF storage

message("Attempting to download TOP Factor data from OSF...")

# Try the file GUID
success <- download.OSF.file(GUID = "qatkz", file_name = "top_factor_data.csv")

if (success) {
  # Load the data
  top_factor_data <- read.csv("top_factor_data.csv")
  message("TOP Factor data loaded successfully!")
  message("Number of journals: ", nrow(top_factor_data))
  message("Column names: ", paste(names(top_factor_data), collapse = ", "))

  # Show first few rows
  print(head(top_factor_data))

  # Save as RData for easy loading
  save(top_factor_data, file = "top_factor_data.RData")
  message("\nData saved as top_factor_data.RData")
} else {
  message("Could not download TOP Factor data.")
  message("Please visit https://osf.io/qatkz/ and download manually.")
}
