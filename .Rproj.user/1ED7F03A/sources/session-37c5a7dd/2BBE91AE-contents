# library(gt)
# library(dplyr)
#
# formatBudgetTable <- function(data) {
#
#   # Calculate Weekly Rate and Overhead
#   data <- data %>%
#     mutate(
#       `Weekly Rate` = FTE * Rate,
#       Overhead = `Weekly Rate` * 0.1
#     )
#
#   # Summarize totals
#   subtotal <- data %>%
#     summarise(
#       Responsibility = "Subtotals per week",
#       FTE = sum(FTE),
#       Rate = NA,
#       `Weekly Rate` = sum(`Weekly Rate`),
#       Overhead = sum(Overhead)
#     )
#
#   total_per_week <- data %>%
#     summarise(
#       Responsibility = "Total per week",
#       FTE = sum(FTE),
#       Rate = NA,
#       `Weekly Rate` = sum(`Weekly Rate`) + sum(Overhead),
#       Overhead = NA
#     )
#
#   total_for_8_weeks <- data %>%
#     summarise(
#       Responsibility = "Total for 8 weeks",
#       FTE = NA,
#       Rate = NA,
#       `Weekly Rate` = 8 * (sum(`Weekly Rate`) + sum(Overhead)),
#       Overhead = NA
#     )
#
#   # Combine all rows
#   data <- bind_rows(data, subtotal, total_per_week, total_for_8_weeks)
#
#   # Create the gt table
#   gt_table <- gt(data) %>%
#     # Include this line in your function to format NA values as blanks
#     sub_missing(
#       columns = everything(),
#       missing_text = ""
#     ) %>%
#
#     cols_label(
#       Responsibility = "Responsibility",
#       FTE = "FTE",
#       Rate = "Rate",
#       `Weekly Rate` = "Weekly Rate",
#       Overhead = "Overhead"
#     ) %>%
#     tab_style(
#       style = list(
#         cell_text(weight = "bold", align = "center", size = "larger"),
#         cell_fill(color = "lightgrey")
#       ),
#       locations = cells_column_labels(everything())
#     ) %>%
#     cols_align(
#       align = "left",
#       columns = c(Responsibility)
#     ) %>%
#     cols_align(
#       align = "right",
#       columns = c(FTE, Rate, `Weekly Rate`, Overhead)
#     ) %>%
#     fmt_currency(
#       columns = c(Rate, `Weekly Rate`, Overhead),
#       currency = "USD"
#     ) %>%
#     tab_style(
#       style = list(
#         cell_text(weight = "bold")
#       ),
#       locations = cells_body(columns = c(Responsibility))
#     ) %>%
#     tab_options(
#       table.font.size = 12,
#       heading.align = "center"
#     )
#
#   return(gt_table)
# }


library(knitr)
library(kableExtra)
library(dplyr)
library(scales)

formatBudgetTable <- function(data) {

  # Calculate Weekly Rate and Overhead
  data <- data %>%
    mutate(
      `Weekly Rate` = FTE * Rate,
      Overhead = `Weekly Rate` * 0.1
    )

  # Summarize totals
  subtotal <- data %>%
    summarise(
      Responsibility = "Subtotals per week",
      FTE = sum(FTE),
      Rate = NA,
      `Weekly Rate` = sum(`Weekly Rate`),
      Overhead = sum(Overhead)
    )

  total_per_week <- data %>%
    summarise(
      Responsibility = "Total per week",
      FTE = sum(FTE),
      Rate = NA,
      `Weekly Rate` = sum(`Weekly Rate`) + sum(Overhead),
      Overhead = NA
    )

  total_for_8_weeks <- data %>%
    summarise(
      Responsibility = "Total for 8 weeks",
      FTE = sum(FTE),
      Rate = NA,
      `Weekly Rate` = 8 * (sum(`Weekly Rate`) + sum(Overhead)),
      Overhead = NA
    )

  # Combine all rows
  data <- bind_rows(data, subtotal, total_per_week, total_for_8_weeks)

  # Replace NA values with zeros for numeric columns before formatting
  data <- data %>%
    mutate(
      Rate = ifelse(is.na(Rate), 0, Rate),
      `Weekly Rate` = ifelse(is.na(`Weekly Rate`), 0, `Weekly Rate`),
      Overhead = ifelse(is.na(Overhead), 0, Overhead)
    )

  # Format the currency columns
  data <- data %>%
    mutate(
      Rate = scales::dollar(Rate, prefix = "$", accuracy = 0.01),
      `Weekly Rate` = scales::dollar(`Weekly Rate`, prefix = "$", accuracy = 0.01),
      Overhead = scales::dollar(Overhead, prefix = "$", accuracy = 0.01)
    )

  # Replace NA values with blanks for non-numeric columns
  data[is.na(data)] <- ""

  # Determine format based on the output format
  output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")

  # Create the kable table
  kable_table <- kable(
    data,
    format = ifelse(output_format == "latex", "latex", "html"),
    col.names = c("Responsibility", "FTE", "Rate", "Weekly Rate", "Overhead"),
    align = c("l", "r", "r", "r", "r"),
    escape = FALSE,
    booktabs = output_format == "latex"
  ) %>%
    kable_styling(
      bootstrap_options = ifelse(output_format == "html", c("striped", "hover", "condensed"), NULL),
      latex_options = ifelse(output_format == "latex", c("striped", "hold_position"), NULL),
      full_width = FALSE
    ) %>%
    row_spec(0, bold = TRUE, background = "lightgrey", align = "c") %>%
    row_spec(nrow(data) - 2, extra_css = "border-top: 1px solid black;") %>%
    row_spec(nrow(data), extra_css = "border-bottom: 2px solid black;")

  return(kable_table)
}
