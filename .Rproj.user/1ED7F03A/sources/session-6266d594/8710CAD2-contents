library(flextable)
library(dplyr)
library(officer)
library(scales)

# Sample data
df <- data.frame(
  Responsibility = c("Organizing and Directing", "Data engineering", "Coworking space manager", "Research support", "Web Development"),
  FTE = c(0.75, 0.50, 0.50, 0.50, 0.5),
  Rate = c(50.00, 45.00, 30.00, 30.00, 30)
)

# Calculate Overhead
df <- df %>%
  mutate(
    Weekly_Total = FTE * Rate * 40,
    Overhead = Weekly_Total * 0.1
  )

# Adding subtotal and total rows
subtotal <- df %>%
  summarize(
    Responsibility = "Subtotal per week",
    FTE = NA,
    Rate = NA,
    Weekly_Total = sum(Weekly_Total),
    Overhead = sum(Overhead)
  )

total_per_week <- subtotal %>%
  mutate(Responsibility = "Total per week", Weekly_Total = sum(Weekly_Total), Overhead = sum(Overhead))

total_for_8_weeks <- total_per_week %>%
  mutate(Responsibility = "Total for 8 weeks", Weekly_Total = 8 * Weekly_Total, Overhead = NA)

# Combine all data with "Totals" row before the subtotals
final_data <- bind_rows(
  df,
  data.frame(Responsibility = "Totals", FTE = NA, Rate = NA, Weekly_Total = NA, Overhead = NA),
  subtotal,
  total_per_week,
  total_for_8_weeks
)

# Rename columns
colnames(final_data) <- c("Responsibility", "FTE", "Rate", "Weekly Total", "Overhead")

# Define border style
border_style <- fp_border(color = "black", width = 1)
bottom_border_style <- fp_border(color = "black", width = 2)
no_border_style <- fp_border(color = "white", width = 0)

# Format columns
final_data <- final_data %>%
  mutate(
    Rate = ifelse(is.na(Rate), NA, dollar(Rate)),
    `Weekly Total` = ifelse(is.na(`Weekly Total`), NA, dollar(`Weekly Total`)),
    Overhead = ifelse(is.na(Overhead), NA, dollar(Overhead))
  )

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
  align(j = c("Rate", "Weekly Total", "Overhead"), align = "right", part = "body") %>%
  align(j = c("Rate", "Weekly Total", "Overhead"), align = "right", part = "header") %>%
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
  bg(i = nrow(df) + 1, bg = "lightgrey", part = "body") %>%
  border(i = nrow(df) + 1, border.top = no_border_style, border.bottom = no_border_style, border.left = no_border_style, border.right = no_border_style, part = "body") %>%
  # Autofit columns
  autofit()

# Display the table
ft
