library(flextable)
library(tidyverse)
library(knitr)

printFteTable <- function(data) {
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
    font(part = "header", fontname = "Times") %>%
    bg(part = "header", bg = "grey") %>%
    # Style body
    bold(j = ~ Responsibility, part = "body") %>%
    font(j = ~ Responsibility, fontname = "Arial", part = "body") %>%
    font(j = ~ -Responsibility, fontname = "Arial", part = "body") %>%
    fontsize(part = "body", size = 10) %>%
    align(j = c("FTE", "Description"), align = "left", part = "body") %>%
    align(j = c("FTE", "Description"), align = "left", part = "header") %>%
    # Bold total row
    bold(i = nrow(final_data), part = "body") %>%
    # Borders
    border_outer(border = border_style) %>%
    border_inner_h(border = border_style) %>%
    border_inner_v(border = border_style) %>%
    hline_bottom(border = bottom_border_style) %>%
    hline(i = nrow(final_data), border = bottom_border_style) %>%
    # Autofit columns
    autofit()

  # Detect output format
  output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")

  # Adjustments for HTML
  if (!is.null(output_format) && output_format == "html") {
    ft <- ft %>%
      width(j = 1, width = 2.5, unit = "in") %>%
      width(j = 2, width = 5.5, unit = "in")
  }

  # Adjustments for PDF
  if (!is.null(output_format) && output_format == "latex") {
    ft <- ft %>%
      width(j = 1, width = 2.5, unit = "in") %>%
      width(j = 2, width = 5.5, unit = "in") %>%
      set_table_properties(layout = "autofit")
  }

  # Adjustments for DOCX
  if (!is.null(output_format) && output_format == "docx") {
    ft <- ft %>%
      width(j = 1, width = 2.5, unit = "in") %>%
      width(j = 2, width = 5.5, unit = "in") %>%
      set_table_properties(layout = "fixed")
  }

  # Default for interactive sessions
  if (is.null(output_format)) {
    ft <- ft %>%
      width(j = 1, width = 2.5, unit = "in") %>%
      width(j = 2, width = 5.5, unit = "in")
  }

  return(ft)
}

printBudgetTable <- function(data) {
  data <- data %>%
    mutate(Total = FTE * Rate)

  # Define border style
  border_style <- fp_border(color = "black", width = 1)
  bottom_border_style <- fp_border(color = "black", width = 2)
  no_border_style <- fp_border(color = "white", width = 0)

  # Create flextable
  ft <- flextable(data) %>%
    # Style header
    bold(part = "header") %>%
    fontsize(part = "header", size = 12) %>%
    font(part = "header", fontname = "Times") %>%
    bg(part = "header", bg = "grey") %>%
    # Style body
    bold(j = ~ Responsibility, part = "body") %>%
    font(j = ~ Responsibility, fontname = "Arial", part = "body") %>%
    font(j = ~ -Responsibility, fontname = "Arial", part = "body") %>%
    fontsize(part = "body", size = 10) %>%
    align(j = c("FTE", "Rate", "Total"), align = "right", part = "body") %>%
    align(j = c("FTE", "Rate", "Total"), align = "right", part = "header") %>%
    # Borders
    border_outer(border = border_style) %>%
    border_inner_h(border = border_style) %>%
    border_inner_v(border = border_style) %>%
    hline_bottom(border = bottom_border_style) %>%
    hline(i = nrow(data), border = bottom_border_style) %>%
    # Autofit columns
    autofit()

  # Detect output format
  output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")

  # Adjustments for HTML
  if (!is.null(output_format) && output_format == "html") {
    ft <- ft %>%
      width(j = 1, width = 2.5, unit = "in") %>%
      width(j = 2, width = 2.5, unit = "in") %>%
      width(j = 3, width = 2.5, unit = "in")
  }

  # Adjustments for PDF
  if (!is.null(output_format) && output_format == "latex") {
    ft <- ft %>%
      width(j = 1, width = 2.5, unit = "in") %>%
      width(j = 2, width = 2.5, unit = "in") %>%
      width(j = 3, width = 2.5, unit = "in") %>%
      width(j = 4, width = 2.5, unit = "in") %>%
      set_table_properties(layout = "autofit")
  }

  # Adjustments for DOCX
  if (!is.null(output_format) && output_format == "docx") {
    ft <- ft %>%
      width(j = 1, width = 2.5, unit = "in") %>%
      width(j = 2, width = 2.5, unit = "in") %>%
      width(j = 3, width = 2.5, unit = "in") %>%
      width(j = 4, width = 2.5, unit = "in") %>%
      set_table_properties(layout = "fixed")
  }

  # Default for interactive sessions
  if (is.null(output_format)) {
    ft <- ft %>%
      width(j = 1, width = 2.5, unit = "in") %>%
      width(j = 2, width = 2.5, unit = "in") %>%
      width(j = 3, width = 2.5, unit = "in") %>%
      width(j = 4, width = 2.5, unit = "in")
  }

  return(ft)
}

printGlossary <- function(data) {
  # Define border style
  border_style <- fp_border(color = "black", width = 1)
  bottom_border_style <- fp_border(color = "black", width = 2)
  no_border_style <- fp_border(color = "white", width = 0)

  # Create flextable
  ft <- flextable(data) %>%
    # Style header
    bold(part = "header") %>%
    fontsize(part = "header", size = 12) %>%
    font(part = "header", fontname = "Times") %>%
    bg(part = "header", bg = "grey") %>%
    # Style body
    font(part = "body", fontname = "Arial") %>%
    fontsize(part = "body", size = 10) %>%
    valign(valign = "top", part = "all") %>%
    # Borders
    border_outer(border = border_style) %>%
    border_inner_h(border = border_style) %>%
    border_inner_v(border = border_style) %>%
    hline_bottom(border = bottom_border_style) %>%
    # Autofit columns
    autofit()

  # Detect output format
  output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")

  # Adjustments for HTML
  if (!is.null(output_format) && output_format
