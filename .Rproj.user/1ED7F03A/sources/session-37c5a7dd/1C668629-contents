# library(gt)
#
# formatStaffingTable <- function(data) {
#   total_fte <- sum(data$FTE)
#
#   # Add the total row
#   total_row <- data.frame(
#     Responsibility = "Total",
#     FTE = total_fte,
#     Description = ""
#   )
#
#   data <- rbind(data, total_row)
#
#   # Create the gt table
#   gt_table <- gt(data) %>%
#     sub_missing(
#       columns = everything(),
#       missing_text = ""
#     ) %>%
#
#     tab_style(
#       style = list(
#         cell_text(weight = "bold", align = "center", size = "larger"),
#         cell_fill(color = "lightgrey")
#       ),
#       locations = cells_title(groups = "title")
#     ) %>%
#     tab_style(
#       style = list(
#         cell_text(weight = "bold"),
#         cell_fill(color = "lightgrey"),
#         cell_text(align = "center")
#       ),
#       locations = cells_column_labels(everything())
#     ) %>%
#     cols_align(
#       align = "left",
#       columns = c(Responsibility, Description)
#     ) %>%
#     cols_align(
#       align = "right",
#       columns = c(FTE)
#     ) %>%
#     tab_style(
#       style = list(
#         cell_text(weight = "bold")
#       ),
#       locations = cells_body(columns = Responsibility)
#     ) %>%
#     tab_style(
#       style = list(
#         cell_borders(sides = "top", color = "black", weight = px(1))
#       ),
#       locations = cells_body(
#         columns = everything(),
#         rows = nrow(data)
#       )
#     ) %>%
#     tab_style(
#       style = list(
#         cell_borders(sides = "bottom", color = "black", weight = px(2))
#       ),
#       locations = cells_body(
#         columns = FTE,
#         rows = nrow(data)
#       )
#     ) %>%
#     tab_options(
#       table.font.size = 12,
#       table.width = pct(80)
#     )
#
#   return(gt_table)
# }


library(knitr)
library(kableExtra)
library(dplyr)

formatStaffingTable <- function(data) {
  total_fte <- sum(data$FTE)

  # Add the total row
  total_row <- data.frame(
    Responsibility = "Total",
    FTE = total_fte,
    Description = ""
  )

  data <- rbind(data, total_row)

  # Replace NA values with blanks
  data[is.na(data)] <- ""

  # Determine format based on the output format
  output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")

  # Create the kable table
  kable_table <- kable(
    data,
    format = ifelse(output_format == "latex", "latex", "html"),
    col.names = c("Responsibility", "FTE", "Description"),
    align = c("l", "r", "l"),
    escape = FALSE,
    booktabs = output_format == "latex"
  ) %>%
    kable_styling(
      bootstrap_options = ifelse(output_format == "html", c("striped", "hover", "condensed"), NULL),
      latex_options = ifelse(output_format == "latex", c("striped", "hold_position"), NULL),
      full_width = FALSE
    ) %>%
    row_spec(0, bold = TRUE, background = "lightgrey", align = "c") %>%
    row_spec(nrow(data), extra_css = "border-top: 1px solid black;") %>%
    row_spec(nrow(data), extra_css = "border-bottom: 2px solid black;")

  return(kable_table)
}
