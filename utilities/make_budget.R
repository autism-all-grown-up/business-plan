# Load necessary libraries
library(tidyverse)
library(knitr)
library(kableExtra)

# Load data
data <- read_csv('data/jobs_data.csv') %>% tibble()

# Function to format the staffing table
formatStaffingTable <- function(data) {
  total_fte <- sum(data$FTE)
  total_row <- data.frame(Responsibility = "Total", FTE = total_fte, Description = "")
  data <- rbind(data, total_row)
  data[is.na(data)] <- ""
  kable(data, format = "html", col.names = c("Responsibility", "FTE", "Description"), align = c("l", "r", "l"), escape = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
    row_spec(0, bold = TRUE, background = "lightgrey", align = "c") %>%
    row_spec(nrow(data), extra_css = "border-top: 1px solid black;") %>%
    row_spec(nrow(data), extra_css = "border-bottom: 2px solid black;")
}

# Function to format the budget table
formatBudgetTable <- function(data) {
  data <- data %>%
    mutate(`Weekly Rate` = FTE * Rate, Overhead = `Weekly Rate` * 0.1)
  subtotal <- data %>%
    summarise(Responsibility = "Subtotals per week", FTE = sum(FTE), Rate = NA, `Weekly Rate` = sum(`Weekly Rate`), Overhead = sum(Overhead))
  total_per_week <- data %>%
    summarise(Responsibility = "Total per week", FTE = sum(FTE), Rate = NA, `Weekly Rate` = sum(`Weekly Rate`) + sum(Overhead), Overhead = NA)
  total_for_8_weeks <- data %>%
    summarise(Responsibility = "Total for 8 weeks", FTE = sum(FTE), Rate = NA, `Weekly Rate` = 8 * (sum(`Weekly Rate`) + sum(Overhead)), Overhead = NA)
  data <- bind_rows(data, subtotal, total_per_week, total_for_8_weeks)
  data <- data %>%
    mutate(Rate = ifelse(is.na(Rate), 0, Rate), `Weekly Rate` = ifelse(is.na(`Weekly Rate`), 0, `Weekly Rate`), Overhead = ifelse(is.na(Overhead), 0, Overhead))
  data <- data %>%
    mutate(Rate = scales::dollar(Rate, prefix = "$", accuracy = 0.01), `Weekly Rate` = scales::dollar(`Weekly Rate`, prefix = "$", accuracy = 0.01), Overhead = scales::dollar(Overhead, prefix = "$", accuracy = 0.01))
  data[is.na(data)] <- ""
  kable(data, format = "html", col.names = c("Responsibility", "FTE", "Rate", "Weekly Rate", "Overhead"), align = c("l", "r", "r", "r", "r"), escape = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
    row_spec(0, bold = TRUE, background = "lightgrey", align = "c") %>%
    row_spec(nrow(data) - 2, extra_css = "border-top: 1px solid black;") %>%
    row_spec(nrow(data), extra_css = "border-bottom: 2px solid black;")
}

# Initialize the content of the Quarto document
quarto_content <- c(
  "---",
  "title: \"Budget and Staffing Tables\"",
  "format:",
  "  html",
  "  pdf",
  "---",
  "",
  "# Staffing and Budget Tables",
  ""
)

# Loop through phases and generate sections and tables
for (phase in c("seed", "sprout", "grow")) {
  phase_title <- paste0("## ", str_to_title(phase), " Phase")
  staffing_section <- paste0("### Staffing\n\n", knitr::kable(formatStaffingTable(data %>% filter(Phase == !!phase) %>% arrange(FTE) %>% select(Responsibility, FTE, Description)), format = "html"))
  budget_section <- paste0("### Budget\n\n", knitr::kable(formatBudgetTable(data %>% filter(Phase == !!phase) %>% select(Responsibility, FTE, Rate)), format = "html"))
  quarto_content <- c(quarto_content, phase_title, staffing_section, "", budget_section, "\n\n")
}

# Write the content to a Quarto file
writeLines(quarto_content, "generated_document.qmd")
