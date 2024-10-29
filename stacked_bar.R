# Install and load the required packages
library(ggplot2)
library(patchwork)

# Data for the left bar plot (All Oregon Adults)
data_left <- data.frame(
  Category = c("Adults Receiving I/DD", "Autistic Adults",
               "Intersection (I/DD & Autistic)", "Remaining Adults"),
  Count = c(22755, 75993, 6371, 3407770 - (75993 + 22755 - 6371)),
  Position = rep("All Oregon Adults", 4)
)

# Data for the right bar plot (Blow up of special categories)
data_right <- data.frame(
  Category = c("Adults Receiving I/DD", "Autistic Adults",
               "Intersection (I/DD & Autistic)"),
  Count = c(22755 - 6371, 75993 - 6371, 6371),
  Position = rep("Autistic/I/DD Adults", 3)
)

# Define the colors
colors <- c("Adults Receiving I/DD" = "orange",
            "Autistic Adults" = "skyblue",
            "Intersection (I/DD & Autistic)" = "green",
            "Remaining Adults" = "grey")

# Create the left bar plot
plot_left <- ggplot(data_left, aes(x = Position, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(x = NULL, y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = 14))

# Create the right bar plot
plot_right <- ggplot(data_right, aes(x = Position, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(x = NULL, y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")

# Combine the plots and include the legend
combined_plot <- plot_left + plot_right +
  plot_annotation(
    title = "Oregon Adults and Special Categories",
    subtitle = "The right-hand side magnifies the special categories on the left"
  ) &
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12))

# Display the combined plot with a legend
combined_plot + plot_layout(guides = "collect") & theme(legend.position = "bottom")
