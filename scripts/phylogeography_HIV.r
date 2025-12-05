library(ggtree)
library(ggplot2)
library(grid)  # To adjust legend key size

# Define colors for the locations, including 'KR'
location_colors <- c(
  "AO" = "red", "BE" = "blue", "CI" = "green", "CM" = "purple",
  "DE" = "orange", "ES" = "pink", "FR" = "cyan", "GB" = "magenta",
  "GH" = "#00853F", "GW" = "brown", "KP" = "yellow", "PK" = "darkgray",
  "RU" = "black", "SE" = "darkblue", "SN" = "lightgreen", "US" = "lightblue",
  "RK" = "lightcoral",  # Color for 'RK'
  "KR" = "darkorange"   # Color for 'KR'
)

# Example of dummy data. Replace with your real data.
# Make sure the number of nodes matches the number of locations
data <- data.frame(
  node = 1:12,
  location = c("AO", "BE", "CI", "CM", "DE", "ES", "FR", "GB", "GH", "US", "RK", "KR")
)

# Generate the fan tree with gray branches (“gray30”) and thinner line width
ggtree(beast, layout="fan") +
  geom_tree(color="gray30", size=0.5) +
  geom_range(range='location.rate_0.95_HPD', color='red', alpha=.6, size=2) +
  geom_point(aes(fill=location), shape=21, color="black", size=3) +
  geom_tiplab(aes(label=location), size=3, align=TRUE, linetype="dashed") +
  scale_fill_manual(values=location_colors) +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.5, "cm")
  ) +
  geom_hilight(node = which(data$location == "GH"), fill = "#00853F", alpha = 0.2)


## ============================================================
## BEAST NEXUS TREE colored by country (rectangular layout)
## Files: virus_vih.tre + virus_vih.csv
## ============================================================

library(ggtree)
library(treeio)
library(ggplot2)
library(grid)
library(dplyr)
library(RColorBrewer)

## ------------------------------------------------------------
## 1. LOAD FILES
## ------------------------------------------------------------

beast <- read.beast("/Users/hugo/Documents/virus_vih/virus_vih.tre")
metadata <- read.csv("/Users/hugo/Documents/virus_vih/virus_vih.csv")

## ------------------------------------------------------------
## 2. CLEAN AND STANDARDIZE DATA
## ------------------------------------------------------------

metadata <- metadata %>%
  mutate(
    ID = trimws(as.character(ID)),
    locations = trimws(toupper(as.character(locations)))
  )

## ------------------------------------------------------------
## 3. CREATE AUTOMATIC COLOR PALETTE
## ------------------------------------------------------------

base_colors <- c(
  "AO" = "red", "BE" = "blue", "CI" = "green", "CM" = "purple",
  "DE" = "orange", "ES" = "pink", "FR" = "cyan", "GB" = "magenta",
  "GH" = "#00853F", "GW" = "brown", "KP" = "yellow", "PK" = "darkgray",
  "RU" = "black", "SE" = "darkblue", "SN" = "lightgreen", "US" = "lightblue",
  "RK" = "lightcoral", "KR" = "darkorange"
)

unique_locs <- unique(metadata$locations)
missing <- setdiff(unique_locs, names(base_colors))

if (length(missing) > 0) {
  set.seed(1)
  extra_colors <- setNames(
    colorRampPalette(brewer.pal(8, "Set2"))(length(missing)),
    missing
  )
  location_colors <- c(base_colors, extra_colors)
} else {
  location_colors <- base_colors
}

cat("\nDetected location codes:\n")
print(unique_locs)

## ------------------------------------------------------------
## 4. MATCH TREE TIPS WITH METADATA
## ------------------------------------------------------------

metadata <- metadata %>%
  rename(tip_label = ID, location = locations)

tip_labels <- beast@phylo$tip.label

metadata <- metadata %>%
  filter(tip_label %in% tip_labels)

beast_annot <- full_join(beast, metadata, by = c("label" = "tip_label"))

## ------------------------------------------------------------
## 5. DRAW RECTANGULAR TREE WITH READABLE LABELS
## ------------------------------------------------------------

p <- ggtree(beast_annot, layout = "rectangular", size = 0.5, color = "gray30") +
  geom_tippoint(aes(fill = location), shape = 21, size = 3, color = "black") +
  geom_tiplab(
    aes(label = location),
    size = 1.8,
    align = TRUE,
    linetype = "dashed",
    offset = 0.3,
    hjust = 0
  ) +
  scale_fill_manual(values = location_colors, na.value = "gray80") +
  theme_tree2() +
  theme(
    text = element_text(size = 11),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"),
    plot.title = element_text(face = "bold", size = 12)
  ) +
  ggtitle("HIV-1 CRF02_AG — BEAST tree colored by country (rectangular layout)")

## ------------------------------------------------------------
## 6. HIGHLIGHT GHANA CLADE
## ------------------------------------------------------------

if ("GH" %in% metadata$location) {
  gh_tips <- beast@phylo$tip.label[
    beast@phylo$tip.label %in% metadata$tip_label[metadata$location == "GH"]
  ]

  if (length(gh_tips) > 1) {
    node_gh <- getMRCA(beast@phylo, gh_tips)
    if (!is.null(node_gh)) {
      p <- p + geom_hilight(node = node_gh, fill = "#00853F", alpha = 0.2)
      cat("\n✅ Ghana clade highlighted successfully.\n")
    }
  }
}

## ------------------------------------------------------------
## 7. EXPORT FIGURES
## ------------------------------------------------------------

out_dir <- "/Users/hugo/Documents/virus_vih/"
pdf_labels <- paste0(out_dir, "virus_vih_tree_labels.pdf")
pdf_clean  <- paste0(out_dir, "virus_vih_tree_nolabels.pdf")
pdf_ids    <- paste0(out_dir, "virus_vih_tree_fullID.pdf")
png_labels <- paste0(out_dir, "virus_vih_tree_labels.png")
png_clean  <- paste0(out_dir, "virus_vih_tree_nolabels.png")
png_ids    <- paste0(out_dir, "virus_vih_tree_fullID.png")

# Version 1: with labels (already created)
ggsave(pdf_labels, plot = p, width = 16, height = 10)
ggsave(png_labels, plot = p, width = 16, height = 10, dpi = 300)

# Version 2: without labels (remove GeomTextGGtree layers)
p_no_labels <- p
p_no_labels$layers <- Filter(function(l) !"GeomTextGGtree" %in% class(l$geom), p$layers)
p_no_labels <- p_no_labels +
  labs(title = "HIV-1 CRF02_AG — BEAST tree colored by country (without labels)") +
  theme(legend.position = "right")

ggsave(pdf_clean, plot = p_no_labels, width = 16, height = 10)
ggsave(png_clean, plot = p_no_labels, width = 16, height = 10, dpi = 300)

# Version 3: full IDs as tip labels
p_fullid <- ggtree(beast_annot, layout = "rectangular", size = 0.5, color = "gray30") +
  geom_tippoint(aes(fill = location), shape = 21, size = 3, color = "black") +
  geom_tiplab(
    aes(label = label),
    size = 1.3,
    align = TRUE,
    offset = 0.3,
    hjust = 0
  ) +
  scale_fill_manual(values = location_colors, na.value = "gray80") +
  theme_tree2() +
  theme(
    text = element_text(size = 10),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 9)
  ) +
  ggtitle("HIV-1 CRF02_AG — BEAST tree with full IDs")

ggsave(pdf_ids, plot = p_fullid, width = 18, height = 12)
ggsave(png_ids, plot = p_fullid, width = 18, height = 12, dpi = 300)

cat("\n Trees exported successfully to:\n", out_dir, "\n")

## ------------------------------------------------------------
## 8. DISPLAY ON SCREEN
## ------------------------------------------------------------
print(p)
