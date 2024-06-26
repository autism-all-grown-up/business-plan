library(googlesheets4)
library(tidyverse)
library(gargle)

# Deauthorize and reauthorize interactively
# gs4_deauth()
# gs4_auth()

# Check where the token is saved
# gargle::gargle_oauth_sitrep()

# Construct the full path to the token file
token_dir <- "C:\\Users\\ariel\\AppData\\Local\\gargle\\gargle\\Cache"
token_file <- list.files(token_dir, full.names = TRUE)[1]

# Replace backslashes with forward slashes in the full path
token_file <- str_replace_all(token_file, "\\\\", "/")

# Print the fixed path
print(token_file)

# Check if the file exists
if (file.exists(token_file)) {
  # Authenticate using the cached token
  gs4_auth(token = token_file)
} else {
  stop("Token file not found.")
}

# Example to read a Google Sheet
sheet_url <- "https://docs.google.com/spreadsheets/d/1tDWDwJANCWrrx1dn4g40qG0ejlE5WFwfOb9HnWsCmto/edit?usp=sharing"
data <- read_sheet(sheet_url)

# Print the data
print(data)
