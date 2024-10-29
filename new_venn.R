# Install and load the VennDiagram package (if not already installed)
install.packages("VennDiagram")
library(VennDiagram)

plot.new()

# Counts as provided
adults_receiving_idd <- 22755
autistic_adults <- 75993
intersection <- 6371

# Create the Venn diagram with customized colors
venn.plot <- draw.pairwise.venn(
  area1 = autistic_adults,
  area2 = adults_receiving_idd,
  cross.area = intersection,
  category = c("Autistic Adults", "Adults Receiving I/DD"),
  fill = c("skyblue", "orange"),
  lty = "blank", # No line type around the circles
  cex = 2, # Text size for category labels
  cat.cex = 2, # Text size for set labels
  cat.pos = c(-20, 20), # Adjusted position of set labels
  cat.dist = c(0.025, 0.025), # Distance of set labels from the diagram
  scaled = TRUE
)

# Open a new plot area with extra space for the legend
png("proportional_venn_with_legend.png", width = 800, height = 800)
par(mar = c(5, 5, 5, 12)) # Extra space on the right for the legend
grid.draw(venn.plot)

# Add a legend outside the plot area
legend("right", inset = c(-0.4, 0),
       legend = c("Autistic Adults", "Adults Receiving I/DD", "Autistic Adults & Adults Receiving I/DD"),
       fill = c("skyblue", "orange", "green"),
       cex = 1.5,
       xpd = TRUE) # Allows the legend to be drawn outside the plot area

# Save the plot
dev.off()
