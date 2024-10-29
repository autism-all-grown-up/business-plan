library(tidyverse)
library(ggplot2)
library(scales)
library(patchwork)

# Colors for both plots
colors <- c(
  "Not Autistic + not Recv. I/DD Svcs." = "#AAAAAA",
  "Autistic + not Recv. I/DD Svcs." = "#E69F00",
  "Autistic + Recv. I/DD Svcs." = "#009E73",
  "Not Autistic, Recv. I/DD Svcs." = "#56B4E9",
  "Autistic" = "#F0E442",  # Shared with right-hand plot
  "Recv. I/DD Svcs." = "#D55E00"        # Shared with right-hand plot
)

# Data for the left-hand plot
all_data <-
  tribble(
    ~Category, ~Counts, ~start, ~end,
    "General Adult Population",    3407770,      0,   3407770,
    "Recv. I/DD Svcs.",                           22755,      0,     22755,
    "Not Autistic, Recv. I/DD Svcs.",              16483,      0,     16383,
    "Autistic + Recv. I/DD Svcs.",                 6371,  16384,     22755,
    "Autistic",                      75993,  16384,     75993,
    "Autistic + not Recv. I/DD Svcs.",                69622,  22756,     75993,
    "Not Autistic + not Recv. I/DD Svcs.",             3331777,  75994,   3407770
  ) %>%
  arrange(start) %>%
  mutate(color = colors[Category])

# Prepare data for left plot
data_left <- all_data %>%
  filter(Category %in% c("Autistic + not Recv. I/DD Svcs.", "Autistic + Recv. I/DD Svcs.", "Not Autistic, Recv. I/DD Svcs.", "Not Autistic + not Recv. I/DD Svcs.")) %>%
  mutate(Category = factor(Category, levels = Category))

# Create the left-hand stacked bar plot with the total population above the bar
p_left <- ggplot(data_left, aes(x = "General Adult Population", y = Counts, fill = Category)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = data_left$color) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 3500000)) +  # Increase y-axis limits
  annotate("text", x = 1, y = 3500000, label = "3,407,770", size = 3, vjust = 0) +  # Match font size and avoid cutoff
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right",
    legend.box.just = "left",  # Left-align the legend box
    legend.title = element_text(hjust = 0),  # Left-align legend title
    legend.text = element_text(hjust = 0),   # Left-align legend text
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    plot.title = element_blank()
  )

# Data for right-hand plot
data_right <- all_data %>%
  filter(Category %in% c("Recv. I/DD Svcs.", "Autistic + Recv. I/DD Svcs.", "Autistic")) %>%
  mutate(Category = factor(Category, levels = c("Recv. I/DD Svcs.", "Autistic + Recv. I/DD Svcs.", "Autistic")),
         x = as.numeric(Category))

# Create the right-hand plot with rotated x-tick labels
p_right <- ggplot(data_right, aes(x = Category)) +
  geom_rect(aes(xmin = x - 0.4, xmax = x + 0.4, ymin = start, ymax = end, fill = Category)) +
  geom_text(aes(label = scales::comma(Counts), y = Counts), vjust = -2, size = 3) +  # Add total counts above bars
  scale_y_continuous(labels = scales::comma, limits = c(0, 80000)) +
  scale_x_discrete(labels = c(
    "Recv. I/DD Svcs.",
    "Autistic + \nRecv. I/DD Svcs.",  # Use newline in the label
    "Autistic"
  )) +  # Modify x-tick labels
  scale_fill_manual(values = data_right$color) +
  labs(x = NULL, y = "Number", fill = "Category") +
  theme_minimal() +
  theme(
    # axis.text.x = element_text(angle = -45, vjust = -1, hjust = 0.1),  # Rotate x-tick labels
    axis.title.y = element_blank(),
    legend.position = "none",  # No legend here, use the one from left plot
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    plot.title = element_blank()
  )

# Combine the two plots using patchwork, keeping the legend from p_left
combined_plot <- p_right + p_left + plot_layout(ncol = 2, widths = c(2, 1))

# Add a title to the combined plot with increased font size and centered
combined_plot <- combined_plot + plot_annotation(
  title = "Adult Population of Oregon: Autistic and/or Receiving I/DD Services",
  theme = theme(plot.title = element_text(size = 16, hjust = 0.5))
)

# Print the combined plot with the title
print(combined_plot)
