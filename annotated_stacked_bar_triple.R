# Install and load the required packages
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)  # For number formatting

counts = c(
  "General Adult Population" = 3407770,
  "Autistic Adults" = 75993,
  "Adults Receiving I/DD Services" = 22755,
  "Autistic Adults Receiving I/DD Services" = 6371,
  "Autistic Adults not Receiving I/DD Services" = 75993 - 6371,
  "Not Autistic Receiving I/DD Services" = 22755 - 6371
)

data = tribble(
  ~Category, ~Counts, ~start, ~end,
  "General Adult Population",    3407770,      0,   3407770,
  "IDD",                           22755,      0,     22755,
  "-Autistics + IDD",              16483,      0,     16383,
  "Autistic + IDD",                 6371,  16384,     22755,
  "Autistic",                      75993,  16384,     75993,
  "Autistic - IDD",                69622,  22756,     75993,
  "-Autistic - IDD",             3331777,  75994,   3407770
) %>%
  arrange(start) %>%
  mutate(
    Category = factor(Category, levels=Category)
  )

# Data for the left bar plot (All Oregon Adults)
data_full = data.frame(
  Category = factor(names(counts)[c(1,4,5,6)], levels = names(counts)[c(1,4,5,6)]),
  Count = counts[c(1,4,5,6)],
  Position = rep("General Adult Population", 4)
)

# Data for the right bar plot (Blow up of special categories)
data_idd = data.frame(
  Category = factor(names(counts)[c(3)], levels = names(counts)[c(1,4,5,6)]),
  Count = counts[c(3)],
  Position = rep("Receiving I/DD", 1)
)

# Data for the right bar plot (Blow up of special categories)
data_autistic = data.frame(
  Category = factor(names(counts)[c(2)], levels = names(counts)[c(1,4,5,6)]),
  Count = counts[c(2)],
  Position = rep("Autistic", 1)
)

data_autistic_and_idd =  data.frame(
  Category = factor(names(counts)[c(4)], levels = names(counts)[c(1,4,5,6)]),
  Count = counts[c(4)],
  Position = rep("Autistic and Receiving I/DD", 1)
)

# Combine the datasets
data = rbind(data_full, data_idd, data_autistic, data_autistic_and_idd) %>%
  mutate(
    # Position = factor(Position, levels=c("General Adult Population", "Autistic and/or Receiving I/DD")),
    Category = factor(Category, levels=c("General Adult Population", "Autistic Adults not Receiving I/DD Services", "Autistic Adults Receiving I/DD Services", "Not Autistic Receiving I/DD Services"))
  )

colors = palette("Okabe-Ito")[1:4]  # Using your original palette
names(colors) = names(counts)[c(1,4,5,6)]

# Create the plot
p <- data %>%
  ggplot(aes(x = 1, y = Count, fill = Category)) +  # Use x = 1 for uniform alignment
  geom_bar(stat = "identity", width = 0.4) +  # Make bars narrower by adjusting width
  scale_fill_manual(values = colors) +  # Apply your original colors
  scale_y_continuous(labels = scales::comma, breaks=trans_breaks(identity, identity, n = 5)) +  # Format y-axis numbers with commas
  labs(
    title = "Number of Oregon Adults who are Autistic and/or Receive I/DD Services",
    x = NULL,
    y = "Number",
    fill = "Category"
  ) +  # Add legend title
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "right",  # Position the legend to the right
    legend.text = element_text(size = 12),  # Increase the legend text size
    legend.title = element_text(size = 14),  # Increase the legend title size
    strip.text = element_text(size = 14, hjust = 0.5),  # Center the facet labels and increase font size
    panel.spacing = unit(1, "lines"),  # Adjust the spacing between panels
    panel.grid.major.x = element_blank(),  # Remove x grid
    panel.grid.major.y = element_line(color = "grey80"),  # Add y grid
    plot.title = element_text(size = 16, face = "bold")  # Increase the font size of the plot title
  ) +
  facet_wrap(~Position, scales = "free_y") +
  coord_cartesian(clip = "off")  # Allow for consistent positioning

# Print the plot
print(p)
