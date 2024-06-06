library(flextable)
library(dplyr)
library(officer)

# Define the function to format the table and add the total row
format_table_with_total <- function(data) {
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

  return(ft)
}

# Sample data
df <- data.frame(
  Responsibility = c("Organizing and Directing", "Data engineering", "Coworking space manager", "Research support", "Web Development"),
  FTE = c(0.75, 0.50, 0.50, 0.50, 0.50),
  Description = c(
    "Complete current information product projects. Investigate access gaps. Locate resources. Continue building relationships. Oversee and participate in research on community resources and funding opportunities.",
    "Create databases. Research portal design.",
    "Research how coworking spaces run. Solicit community feedback. Run trials.",
    "Collect and organize information. Writing.",
    "Design and build website."
  )
)

# Format the table using the custom function
ft <- format_table_with_total(df)

# Display the table
ft
