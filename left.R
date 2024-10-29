library(tidyverse)
library(ggplot2)
library(scales)  # For formatting

# Use the working dataset you provided
all_data <- tribble(
  ~Category, ~Counts, ~start, ~end,
  "General Adult Population",    3407770,      0,   3407770,
  "IDD",                           22755,      0,     22755,
  "-Autistic + IDD",              16483,      0,     16383,
  "Autistic + IDD",                 6371,  16384,     22755,
  "Autistic",                      75993,  16384,     75993,
  "Autistic - IDD",                69622,  22756,     75993,
  "-Autistic - IDD",             3331777,  75994,   3407770
)

data_left =
  all_data %>%
  arrange(start) %>%
  filter(Category %in% c("General Adult Population", "Autistic - IDD", "Autistic + IDD", "-Autistic + IDD")) %>%
  mutate(
    Category = factor(Category, levels=Category)
  )

# Define the colors for the plot
colors <- c(
  "General Adult Population" = "#000000",  # General adult population in grey
  "Autistic - IDD" = "#E69F00",
  "Autistic + IDD" = "#56B4E9",
  "-Autistic + IDD" = "#009E73"
)

data_left = data_left %>% mutate(color = colors)

# Create the stacked bar plot
p_left <- ggplot(data_left, aes(x = "General Adult Population", y = Counts, fill = Category)) +
  geom_bar(stat = "identity", width = 0.4) +  # Stacked bar plot
  scale_fill_manual(values = colors) +  # Apply the colors from data_left
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
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
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    plot.title = element_blank()
  )

# Print the stacked bar plot
print(p_left)
