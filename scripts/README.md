# Molecular epidemiology, evolution and transmission dynamics of HIV-1 in Ghana, West Africa: Scripts

## Distribution of the data 
* `disstribution_of_data.R` -  This R script reads a metadata file for an HIV study and generates multiple descriptive epidemiological visualizations. It explores how different demographic and behavioral variables vary across years and how HIV subtypes are distributed across the population.

## Detecting introduccions of HIV to Ghana

* `phylogenetic_analysis_hiv_ghana.R` - This R script estimates the number of HIV-1 subtype CRF02_AG introductions into Ghana using results from a discrete phylogeographic analysis. It is a modified version of Simon Dellicour’s original script for SARS-CoV-2 introduction counting.
Requieres the next libraries in R:
`seraphim,
lubridate,
diagram,
treeio,
ape` 

## Plot the phylogeography reconstruction 

* `phylogeography_HIV.r` - This script loads a BEAST phylogenetic tree and associated metadata, assigns geographic locations to tree tips, creates custom color palettes for each country, and generates multiple publication-ready visualizations of the HIV-1 CRF02_AG phylogeny. It highlights the Ghanaian clade, annotates tips with country labels, and exports several versions of the tree.
