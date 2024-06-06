library(flextable)
library(dplyr)
library(officer)
library(scales)

printFteTable <- function(data, output_format) {
  # Calculate the total FTE
  total_fte <- sum(data$FTE, na.rm = TRUE)

  # Add the total row
  total_row <- data.frame(
    Responsibility = "Total",
    FTE = total_fte,
    Description = ""
  )

  # Combine the original data with the total row
  final_data <- bind_rows(data, total_row)

  # Define border style
  border_style <- fp_border(color = "black", width = 1)
  bottom_border_style <- fp_border(color = "black", width = 2)
  no_border_style <- fp_border(color = "white", width = 0)


  # Create flextable
  ft <- flextable(final_data) %>%
    # Style header
    bold(part = "header") %>%
    fontsize(part = "header", size = 12) %>%
    bg(part = "header", bg = "grey") %>%
    # Style body
    bold(j = ~ Responsibility, part = "body") %>%
    fontsize(part = "body", size = 10) %>%
    align(
      j = c("FTE", "Description"),
      align = "left",
      part = "body"
    ) %>%
    align(
      j = c("FTE", "Description"),
      align = "left",
      part = "header"
    ) %>%
    # Borders
    border_outer(border = border_style) %>%
    border_inner_h(border = border_style) %>%
    border_inner_v(border = border_style) %>%
    hline_bottom(border = bottom_border_style) %>%
    hline(i = nrow(final_data), border = bottom_border_style) %>%
    # Autofit columns
    autofit() %>%
    align(align = "left", part = "all") %>%
    width(j = 1, width = 2, unit = "in") %>%
    width(j = 2, width = 0.5, unit = "in") %>%
    width(j = 3, width = 3, unit = "in") %>%
    set_table_properties(width = 1, layout = "autofit")

  # Adjustments for HTML
  if (!is.null(output_format) && output_format == "html") {
    ft <- ft
  }

  # Adjustments for PDF
  if (!is.null(output_format) && output_format == "latex") {
    ft <- ft %>%
      set_table_properties(layout = "fixed")
  }

  # Adjustments for DOCX
  if (!is.null(output_format) && output_format == "docx") {
    ft <- ft %>%
      set_table_properties(layout = "fixed")
  }

  # Default for interactive sessions
  if (is.null(output_format)) {
    ft <- ft
  }

  return(ft)

  return(ft)
}
