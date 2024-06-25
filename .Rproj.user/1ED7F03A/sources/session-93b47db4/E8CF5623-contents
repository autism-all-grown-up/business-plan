library(flextable)
library(tidyverse)
library(knitr)
library(officer)

printGlossary <- function(data, outpout_format) {
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
    autofit() %>%
    width(j = 1, width = 2, unit = "in") %>%
    width(j = 2, width = 4, unit = "in")

  # # Detect output format
  # output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")

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
}
