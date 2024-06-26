library(httr)
library(jsonlite)
library(tidyverse)

# Function to get salary information from ZipRecruiter API
get_ziprecruiter_salary_info <- function(job_title, location, api_key) {
  base_url <- "https://api.ziprecruiter.com/jobs/v1"

  response <- GET(
    url = base_url,
    query = list(
      search = job_title,
      location = location,
      api_key = api_key,
      page = 1,
      jobs_per_page = 1
    )
  )

  # Check for successful request
  if (status_code(response) == 200) {
    content <- content(response, as = "text", encoding = "UTF-8")
    json_content <- fromJSON(content)
    if (length(json_content$jobs) > 0) {
      job <- json_content$jobs[[1]]
      tibble(
        JobTitle = job_title,
        Location = location,
        SalaryMin = job$salary_min,
        SalaryMax = job$salary_max,
        AverageSalary = job$average_salary
      )
    } else {
      tibble(
        JobTitle = job_title,
        Location = location,
        SalaryMin = NA,
        SalaryMax = NA,
        AverageSalary = NA
      )
    }
  } else {
    stop("Failed to fetch data from ZipRecruiter API")
  }
}

# Your ZipRecruiter API key
api_key <- "p7ark7v2nzpzat6r38zhwuftm5p22x2m"

# Job titles and location
job_titles <- c("Data Analyst", "Research Associate")
location <- "Portland, OR"

# Fetch salary information for each job title
salary_info <- map_df(job_titles, ~ get_ziprecruiter_salary_info(.x, location, api_key))

# Print the salary information
print(salary_info)
