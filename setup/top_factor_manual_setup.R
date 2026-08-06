# Manual TOP Factor Data Setup
# ============================================================================
# The TOP Factor data from COS needs to be downloaded manually if automatic
# download fails. Follow these steps:
# ============================================================================

# Step 1: Download the data
# Visit: https://osf.io/qatkz/
# Download the TOP Factor dataset (likely a CSV or Excel file)

# Step 2: Load the data into R
# Replace 'path/to/file.csv' with the actual path to your downloaded file

# Example for CSV:
# top_factor_data <- read.csv("path/to/TOP_Factor_data.csv")

# Example for Excel:
# library(readxl)
# top_factor_data <- read_excel("path/to/TOP_Factor_data.xlsx")

# Step 3: Verify the structure
# str(top_factor_data)

# Step 4: The data should have columns like:
# - Journal name
# - ISSN
# - TOP Factor score
# You may need to rename columns to match the expected format:

# Example renaming (adjust based on actual column names):
# names(top_factor_data)[names(top_factor_data) == "actual_journal_col"] <- "Journal"
# names(top_factor_data)[names(top_factor_data) == "actual_issn_col"] <- "ISSN"
# names(top_factor_data)[names(top_factor_data) == "actual_score_col"] <- "TOP_Factor"

# Step 5: Save as RData for easy loading in the main script
# save(top_factor_data, file = "top_factor_data.RData")

# ============================================================================
# Alternative: Use the osfr package to download programmatically
# ============================================================================

library(osfr)

# Try to download from OSF project
# The project node is qatkz
tryCatch({
  # Authenticate if needed (for private projects, not needed for public)
  # osf_auth()

  # Retrieve the project
  project <- osf_retrieve_node("qatkz")

  # List files in the project
  files <- osf_ls_files(project)
  print(files)

  # Download the file (adjust name based on what you see)
  # Find the data file
  data_file <- files[grep("csv|xlsx|xls", files$name, ignore.case = TRUE), ]

  if (nrow(data_file) > 0) {
    # Download first matching file
    osf_download(data_file[1, ], path = ".", conflicts = "overwrite")

    # Load the downloaded file
    file_name <- data_file$name[1]

    if (grepl("\\.csv$", file_name, ignore.case = TRUE)) {
      top_factor_data <- read.csv(file_name)
    } else if (grepl("\\.xlsx?$", file_name, ignore.case = TRUE)) {
      library(readxl)
      top_factor_data <- read_excel(file_name)
    }

    # Save for future use
    save(top_factor_data, file = "top_factor_data.RData")
    message("TOP Factor data downloaded and saved successfully!")

    # Display structure
    str(top_factor_data)
    head(top_factor_data)
  } else {
    message("No CSV or Excel files found in the OSF project")
  }

}, error = function(e) {
  message("Error downloading from OSF: ", e$message)
  message("Please download manually from https://osf.io/qatkz/")
})

# ============================================================================
# Once you have the data loaded, you can test the matching:
# ============================================================================

# Example: Search for a specific journal
# test_journal <- "Psychological Science"
# top_factor_data[grep(test_journal, top_factor_data$Journal, ignore.case = TRUE), ]

# Example: Search by ISSN
# test_issn <- "1745-6916"
# top_factor_data[top_factor_data$ISSN == test_issn, ]
