library(ggtree)
library(ggplot2)
library(grid)  # To adjust the size of the keys in the legend

# Define the colors for the locations, including 'KR'
location_colors <- c(
  "AO" = "red", "BE" = "blue", "CI" = "green", "CM" = "purple",
  "DE" = "orange", "ES" = "pink", "FR" = "cyan", "GB" = "magenta",
  "GH" = "#00853F", "GW" = "brown", "KP" = "yellow", "PK" = "darkgray",
  "RU" = "black", "SE" = "darkblue", "SN" = "lightgreen", "US" = "lightblue",
  "RK" = "lightcoral",  # Color for 'RK'
  "KR" = "darkorange"  # Color for 'KR'
)

# Make sure the number of nodes and locations match
data <- data.frame(
  node = 1:12,  # Ensure the number of nodes matches the number of locations
  location = c("AO", "BE", "CI", "CM", "DE", "ES", "FR", "GB", "GH", "US", "RK", "KR")  # Replace with actual locations
)

# Generate a fan-shaped tree with gray branches ("gray30") and thinner lines
ggtree(beast, layout="fan") +
  geom_tree(color="gray30", size=0.5) +  # All branches in gray ("gray30") and thinner
  geom_range(range='location.rate_0.95_HPD', color='red', alpha=.6, size=2) +
  geom_point(aes(fill=location), shape=21, color="black", size=3) +  # Points on internal nodes based on 'location'
  geom_tiplab(aes(label=location), size=3, align=TRUE, linetype="dashed") +
  scale_fill_manual(values=location_colors) +  # Colors for points on internal nodes
  theme(legend.position="right",  # Move the legend to the right
        legend.title=element_blank(),  # Remove the legend title
        legend.text=element_text(size=10),  # Adjust the legend text size
        legend.key.size=unit(0.5, "cm")) +  # Adjust the size of the keys in the legend
  geom_hilight(node=which(data$location == "GH"), fill="#00853F", alpha=0.2)  # Highlight clades and branches for Ghana
