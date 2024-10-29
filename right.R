library(tidyverse)
library(ggplot2)

# Use the new dataset with manually set factor levels for Category
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
    Category = factor(Category, levels=c("General Adult Population", "IDD", "-Autistics + IDD", "Autistic + IDD", "Autistic", "Autistic - IDD", "-Autistic - IDD"))
  )

# Define the colors for the plot
colors <- c(
  "IDD" = "#E69F00",
  "Autistic + IDD" = "#56B4E9",
  "Autistic" = "#009E73"
)

data_right =
  all_data %>%
  filter(Category %in% c("IDD", "Autistic + IDD", "Autistic")) %>%
  mutate(
    Category = factor(Category, levels = c("IDD", "Autistic + IDD", "Autistic")),
    x = as.numeric(Category)
  )

# Create the plot for the relevant categories
p_right <- data_right %>%
  ggplot(aes(x = Category)) +
  geom_rect(aes(xmin = x - 0.4,
                xmax = x + 0.4,
                ymin = start, ymax = end, fill = Category)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 80000)) +  # Apply appropriate y-axis limits
  scale_fill_manual(values = colors) +  # Apply the correct colors
  labs(
    x = NULL,
    y = "Number",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    legend.position = "right",  # Position the legend on the right
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    plot.title = element_blank()
  )

# Print the right-hand plot
print(p_right)
