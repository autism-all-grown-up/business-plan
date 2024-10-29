library(networkD3)

# Nodes (Unique categories in your data)
nodes <- data.frame(name = c("All Adults",
                             "Autistic Adults",
                             "Adults Receiving I/DD",
                             "Autistic Adults & Adults Receiving I/DD"))

# Links (Flows between nodes with their respective counts)
links <- data.frame(source = c(0, 0, 0, 1, 2),
                    target = c(1, 2, 3, 3, 3),
                    value = c(75993, 22755, 6371, 6371, 6371))

# Create the Sankey diagram
sankey <- sankeyNetwork(Links = links, Nodes = nodes,
                        Source = "source", Target = "target",
                        Value = "value", NodeID = "name",
                        sinksRight = TRUE, # Places the target on the right
                        fontSize = 12, nodeWidth = 30)

# Save the Sankey diagram as an HTML file
saveNetwork(sankey, file = "sankey_diagram.html")

# Display the Sankey diagram in the viewer (if running in RStudio)
sankey

