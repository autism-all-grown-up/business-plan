library(flextable)
library(tidyverse)
library(knitr)

printBudgetTable <- function(data, output_format) {
  data <-
    tibble(data) %>%
    mutate(
      `Weekly Total` = FTE * Rate * 40,
      Overhead = `Weekly Total` * 0.1
    )

  # colnames(data) <- c(`Responsibility`, `FTE`, `Rate`, `Weekly Total`, `Overhead`)

  # Adding subtotal and total rows
  subtotal <- data %>%
    summarize(
      Responsibility = "Subtotal per week",
      FTE = sum(FTE),
      Rate = NA,
      `Weekly Total` = sum(`Weekly Total`),
      Overhead = sum(Overhead)
    )

  total_per_week <-
    subtotal %>%
    mutate(
      Responsibility = "Total per week",
      `Weekly Total` = `Weekly Total` + Overhead
    )

  total_for_8_weeks <- total_per_week %>%
    mutate(Responsibility = "Total for 8 weeks", `Weekly Total` = 8 * `Weekly Total`, Overhead = NA)

  final_data <- bind_rows(
    data,
    tibble(Responsibility = "Totals", FTE = NA, Rate = NA, `Weekly Total` = NA, Overhead = NA),
    subtotal,
    total_per_week,
    total_for_8_weeks
  )

  # Format columns
  final_data <- final_data %>%
    mutate(
      Rate = ifelse(is.na(Rate), NA, dollar(Rate)),
      `Weekly Total` = ifelse(is.na(`Weekly Total`), NA, dollar(`Weekly Total`)),
      Overhead = ifelse(is.na(Overhead), NA, dollar(Overhead))
    )

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
      j = c("FTE", "Rate", "Weekly Total", "Overhead"),
      align = "right",
      part = "body"
    ) %>%
    align(
      j = c("FTE", "Rate", "Weekly Total", "Overhead"),
      align = "right",
      part = "header"
    ) %>%
    # Borders
    border_outer(border = border_style) %>%
    border_inner_h(border = border_style) %>%
    border_inner_v(border = border_style) %>%
    hline_bottom(border = bottom_border_style) %>%
    hline(i = nrow(final_data) - 3, border = bottom_border_style) %>%
    hline(i = nrow(final_data) - 2, border = bottom_border_style) %>%
    hline(i = nrow(final_data) - 1, border = bottom_border_style) %>%
    hline(i = nrow(final_data), border = bottom_border_style) %>%
    # Style custom row
    bg(i = nrow(data) + 1, bg = "lightgrey", part = "body") %>%
    border(i = nrow(data) + 1, border.top = no_border_style, border.bottom = no_border_style, border.left = no_border_style, border.right = no_border_style, part = "body") %>%
    # Autofit columns
    autofit() %>%
    align(align = "left", part = "all") %>%
    width(j = 1, width = 2.5, unit = "in") %>%
    width(j = 2, width = 0.5, unit = "in") %>%
    width(j = 3, width = 0.5, unit = "in") %>%
    width(j = 4, width = 1.5, unit = "in") %>%
    width(j = 5, width = 1.5, unit = "in") %>%
    set_table_properties(width = 1, layout = "autofit")

  # Detect output format
  # output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")

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

  return(ft)
}
