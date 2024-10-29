# Load necessary libraries
library(ComplexUpset)
library(ggplot2)

# Define the counts vector
counts <- c(
  "Adults" = 3407770,
  "Autistic Adults" = 75993,
  "Adults Receiving I/DD" = 22755,
  "Adults & Autistic Adults" = 75993,
  "Adults & Adults Receiving I/DD" = 22755,
  "Autistic Adults & Adults Receiving I/DD" = 6371,
  "Adults & Autistic Adults & Adults Receiving I/DD" = 6371
)

# Create the data frame for the UpSet plot
upset_data <- data.frame(
  'Adults' = c(1, 1, 1, 1, 1, 1, 1),
  'Autistic_Adults' = c(1, 1, 0, 1, 0, 1, 1),
  'Adults_Receiving_IDD' = c(1, 0, 1, 0, 1, 1, 1),
  'Count' = c(
    counts["Adults & Autistic Adults & Adults Receiving I/DD"],  # Intersection A&B&C
    counts["Autistic Adults"] - counts["Adults & Autistic Adults & Adults Receiving I/DD"],  # B only
    counts["Adults Receiving I/DD"] - counts["Adults & Autistic Adults & Adults Receiving I/DD"],  # C only
    counts["Adults & Autistic Adults"] - counts["Adults & Autistic Adults & Adults Receiving I/DD"],  # A & B only
    counts["Adults & Adults Receiving I/DD"] - counts["Adults & Autistic Adults & Adults Receiving I/DD"],  # A & C only
    counts["Autistic Adults & Adults Receiving I/DD"] - counts["Adults & Autistic Adults & Adults Receiving I/DD"],  # B & C only
    counts["Adults"] - counts["Autistic Adults"] - counts["Adults Receiving I/DD"] + counts["Adults & Autistic Adults & Adults Receiving I/DD"]  # A only
  )
)

# Create the UpSet plot
upset_plot <- upset(
  upset_data,
  intersect = c('Adults', 'Autistic_Adults', 'Adults_Receiving_IDD'),
  base_annotations = list(
    'Intersection size' = intersection_size(
      counts = TRUE
    )
  )
)

# Display the UpSet plot
print(upset_plot)
