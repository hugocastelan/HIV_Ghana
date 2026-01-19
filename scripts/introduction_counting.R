############################################################
#  introduction_counting.R
#
#  Count HIV-1 CRF02_AG introduction events into Ghana (GH)
#
#  Adapted from Simon Dellicour's SARS-CoV-2 introduction
#  counting framework, modified for HIV phylogeography.
############################################################

## ---------------------------
## 1. Libraries
## ---------------------------
library(seraphim)
library(ape)
library(lubridate)

## ---------------------------
## 2. User-defined inputs
## ---------------------------
analysis <- "virus_vih"      # prefix for input files
burnIn  <- 101               # number of posterior trees to discard
ghana_code <- "GH"

tree_file  <- paste0(analysis, ".tre")    # annotated MCC tree (optional)
trees_file <- paste0(analysis, ".trees")  # posterior trees
meta_file  <- paste0(analysis, ".csv")    # metadata (optional)

## ---------------------------
## 3. Read posterior trees
## ---------------------------
trees_raw <- scan(trees_file,
                  what = "",
                  sep = "\n",
                  quiet = TRUE,
                  blank.lines.skip = FALSE)

tree_lines  <- which(grepl("^tree STATE_", trees_raw))
header_lines <- which(!grepl("^tree STATE_", trees_raw))

if (length(tree_lines) <= burnIn) {
  stop("Not enough trees after burn-in.")
}

## ---------------------------
## 4. Extract sampling dates
##    (assumes date is last '_' element in tip labels)
## ---------------------------
example_tree <- readAnnotatedNexus(tree_file)
tip_labels <- example_tree$tip.label

collection_dates <- sapply(tip_labels, function(x) {
  parts <- unlist(strsplit(x, "_"))
  as.numeric(parts[length(parts)])
})

mostRecentSamplingDate <- max(collection_dates, na.rm = TRUE)

## ---------------------------
## 5. Initialize containers
## ---------------------------
ghanaIntroductions_list <- rep(NA, length(tree_lines))
ghana_tMRCAs_list <- vector("list", length(tree_lines))

## ---------------------------
## 6. Loop through posterior trees
## ---------------------------
for (i in (burnIn + 1):length(tree_lines)) {

  cat("Processing tree", i, "of", length(tree_lines), "\n")

  selected_lines <- c(
    header_lines[1:(length(header_lines) - 1)],
    tree_lines[i],
    header_lines[length(header_lines)]
  )

  temp_file <- paste0("TEMP_tree_", i, ".tree")
  write(trees_raw[selected_lines], temp_file)

  tree <- tryCatch(
    readAnnotatedNexus(temp_file),
    error = function(e) NULL
  )

  if (is.null(tree)) {
    file.remove(temp_file)
    next
  }

  ghanaIntroductions <- 0
  ghana_tMRCAs <- c()

  ## ---------------------------
  ## Definition of introduction:
  ## Parent location != GH
  ## Child location  == GH
  ## ---------------------------
  for (j in seq_len(nrow(tree$edge))) {

    ann_child <- tree$annotations[[j]]
    if (is.null(ann_child)) next

    if (ann_child$location == ghana_code) {

      parent_index <- which(tree$edge[,2] == tree$edge[j,1])

      if (length(parent_index) > 0) {
        ann_parent <- tree$annotations[[parent_index]]

        if (!is.null(ann_parent) &&
            ann_parent$location != ghana_code) {

          ghanaIntroductions <- ghanaIntroductions + 1

          tMRCA <- mostRecentSamplingDate -
                   nodeheight(tree, tree$edge[j,1])

          ghana_tMRCAs <- c(ghana_tMRCAs, tMRCA)
        }
      }
    }
  }

  ghanaIntroductions_list[i] <- ghanaIntroductions
  ghana_tMRCAs_list[[i]] <- ghana_tMRCAs

  file.remove(temp_file)
}

## ---------------------------
## 7. Posterior summary
## ---------------------------
ghanaIntroductions_clean <- ghanaIntroductions_list[
  !is.na(ghanaIntroductions_list)
]

median_introductions <- median(ghanaIntroductions_clean)
HPD <- quantile(ghanaIntroductions_clean, probs = c(0.025, 0.975))

cat(
  "Estimated number of HIV-1 CRF02_AG introductions into Ghana:\n",
  "Median =", median_introductions, "\n",
  "95% HPD =", HPD[1], "-", HPD[2], "\n"
)

## ---------------------------
## 8. Optional: save outputs
## ---------------------------
results <- data.frame(
  introductions = ghanaIntroductions_clean
)

write.csv(results,
          file = "ghana_introductions_posterior.csv",
          row.names = FALSE)
