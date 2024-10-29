library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)  # For number formatting

# New dataset structure
all_data <- tribble(
  ~Category, ~Counts, ~start, ~end,
  "General Adult Population",    3407770,      0,   3407770,
  "IDD",                           22755,      0,     22755,
  "-Autistic + IDD",              16483,      0,     16383,
  "Autistic + IDD",                 6371,  16384,     22755,
  "Autistic",                      75993,  16384,     75993,
  "Autistic - IDD",                69622,  22756,     75993,
  "-Autistic - IDD",             3331777,  75994,   3407770
) %>%
  mutate(
    Category = factor(Category, levels=c("General Adult Population", "Autistic - IDD", "Autistic + IDD", "-Autistic + IDD"))
  )

# Define the colors for the plot
colors <- c(
  "General Adult Population" = "#000000",  # Black for General Adult Population
  "Autistic - IDD" = "#E69F00",
  "Autistic + IDD" = "#56B4E9",
  "-Autistic + IDD" = "#009E73"
)

# Prepare the data for the left-hand stacked bar plot (filter the relevant categories)
data_left <- all_data %>%
  filter(Category %in% c("General Adult Population", "Autistic - IDD", "Autistic + IDD", "-Autistic + IDD"))

# Create the left-hand stacked bar plot
p_left <- ggplot(data_left, aes(x = "General Adult Population", y = Counts, fill = Category)) +
  geom_bar(stat = "identity", width = 0.4) +  # Stacked bar plot
  scale_fill_manual(values = colors) +  # Apply the correct colors
  scale_y_continuous(labels = scales::comma, limits = c(0, max(all_data$end))) +  # Scale to the full population
  labs(
    x = NULL,
    y = "Number",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "right",  # Position the legend on the right
    legend.text = element_text(size = 12),  # Increase the legend text size
    legend.title = element_text(size = 14),  # Increase the legend title size
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    plot.title = element_text(size = 16, face = "bold")  # Increase the font size of the plot title
  )

# Print the left-hand stacked bar plot
print(p_left)
