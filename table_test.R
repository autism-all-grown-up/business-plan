library(tidyverse)
library(kableExtra)

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

# Display the table
final_data %>%
  kbl(booktabs = TRUE) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = F
  ) %>%
  row_spec(0, bold = TRUE, font_size = 12, background = "#D3D3D3", extra_css = "font-family: 'Times New Roman';") %>%
  row_spec(1:nrow(data), extra_css = "font-family: 'Arial'; font-size: 10px; border: 1px solid;") %>%
  row_spec(nrow(data) + 1, bold = TRUE, extra_css = "font-family: 'Arial'; font-size: 10px; border: 1px solid;") %>%
  row_spec(nrow(data) + 2, bold = TRUE, extra_css = "font-family: 'Arial'; font-size: 10px; border: 2px solid;") %>%
  row_spec(nrow(data) + 3, bold = TRUE, extra_css = "font-family: 'Arial'; font-size: 10px; border: 2px solid;") %>%
  column_spec(1, bold = TRUE, extra_css = "font-family: 'Arial'; font-size: 10px;") %>%
  column_spec(2:5, extra_css = "font-family: 'Arial'; font-size: 10px;") %>%
  kable_classic() %>%
  kable_styling(latex_options = c("hold_position"))
