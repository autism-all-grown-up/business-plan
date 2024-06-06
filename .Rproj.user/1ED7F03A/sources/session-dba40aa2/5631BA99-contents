library(tidyverse)
library(flextable)

# Your data
data <- tribble(
  ~Responsibility, ~FTE, ~Rate, ~`Weekly Total`, ~`Overhead (10%)`,
  "Organizing and Directing", 0.75, "$50.00", 1500.00, 150.00,
  "Data engineering", 0.50, "$45.00", 900.00, 90.00,
  "Coworking space manager", 0.50, "$30.00", 600.00, 60.00,
  "Research support", 0.50, "$30.00", 600.00, 60.00
)

# Calculate subtotals and totals
subtotal <- data %>%
  summarize(
    Responsibility = "Subtotal per week",
    FTE = NA,
    Rate = NA,
    `Weekly Total` = sum(`Weekly Total`),
    `Overhead (10%)` = sum(`Overhead (10%)`)
  )

total_per_week <- subtotal %>%
  mutate(Responsibility = "Total per week", `Weekly Total` = sum(`Weekly Total`), `Overhead (10%)` = sum(`Overhead (10%)`))

total_for_8_weeks <- total_per_week %>%
  mutate(Responsibility = "Total for 8 weeks", `Weekly Total` = 8 * `Weekly Total`, `Overhead (10%)` = NA)

# Combine all data
final_data <- bind_rows(data, subtotal, total_per_week, total_for_8_weeks)

# Create a flextable
ft <- flextable(final_data)

# Style the flextable
ft <- ft %>%
  add_header_lines(values = "Potential Funders") %>%
  bold(part = "header") %>%
  fontsize(size = 12, part = "header") %>%
  bg(bg = "grey", part = "header") %>%
  bold(j = 1, part = "body") %>%
  fontsize(size = 10, part = "body") %>%
  font(fontname = "Arial", part = "body") %>%
  border(border = fp_border(width = 1)) %>%
  hline_bottom(part = "body", border = fp_border(width = 2)) %>%
  add_footer_lines(values = "Subtotal per week") %>%
  add_footer_lines(values = "Total per week") %>%
  add_footer_lines(values = "Total for 8 weeks") %>%
  merge_at(j = 1, i = nrow(final_data) + 1:nrow(final_data) + 3, part = "body")

# Save as Word document
save_as_docx(ft, path = "table.docx")
