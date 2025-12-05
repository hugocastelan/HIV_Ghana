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
