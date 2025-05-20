
# Rastello, Kévan

# website of the project: 
# https://salmonprize.com/

# brood return data
# https://drive.google.com/file/d/14Wn7BXlfkJua5Wmr5o1ZKLRm7jCwp2Ca/view

# env covariates:
# https://docs.google.com/spreadsheets/d/1HPL6P1ujpgGmzHmLg6b1mlHs6E-FkYvSB-JHN0Hyq9k/edit?gid=1627873802#gid=1627873802

# supplementary info:
# https://docs.google.com/document/d/1ESGdHN4xQVpbRghNNITFseGlILpS55ekrpureaN3Hl8/edit?tab=t.0

# Brood table analysis example:
#https://nceas.github.io/sasap-training/materials/reproducible_research_in_r_fairbanks/example-brood-table-analysis.html

# Set the wd --------------------------------------------------------------
setwd("C:/Users/keked/OneDrive - University of Victoria/PhD/PROJECT/#Github-MODELS/sockeye_prediction")

# Load the libraries ------------------------------------------------------
library(MPBforecasting)
library(tidyverse)
library(data.table)
# remotes::install_github("pbs-assess/pacea")
library(pacea)

# Load and merge the data -------------------------------------------------

merge_brood_return_by_ageclass <- function(brood_file, return_file) {
  # Read in brood and return tables
  brood <- fread(brood_file)
  ret <- fread(return_file)
  
  # Find AgeClass columns
  age_cols_brood <- grep("^AgeClass_", names(brood), value = TRUE)
  age_cols_ret   <- grep("^AgeClass_", names(ret), value = TRUE)
  
  # Coerce AgeClass columns to numeric
  brood[, (age_cols_brood) := lapply(.SD, as.numeric), .SDcols = age_cols_brood]
  ret[, (age_cols_ret)     := lapply(.SD, as.numeric), .SDcols = age_cols_ret]
  
  # Melt to long format
  brood_long <- melt(
    brood,
    id.vars = c("System", "River", "BroodYear"),
    measure.vars = age_cols_brood,
    variable.name = "AgeClass",
    value.name = "Recruits"
  )
  ret_long <- melt(
    ret,
    id.vars = c("System", "River", "ReturnYear"),
    measure.vars = age_cols_ret,
    variable.name = "AgeClass",
    value.name = "Returns"
  )
  
  # Clean AgeClass names
  brood_long[, AgeClass := gsub("AgeClass_", "", AgeClass)]
  ret_long[, AgeClass   := gsub("AgeClass_", "", AgeClass)]
  
  # Split AgeClass into freshwater and ocean years
  brood_long[, c("FW_yrs", "OC_yrs") := tstrsplit(AgeClass, "\\.")]
  brood_long[, ExpectedReturnYear := BroodYear + as.integer(FW_yrs) + as.integer(OC_yrs)]
  
  # Merge on matching keys
  merged <- merge(
    brood_long,
    ret_long,
    by.x = c("System", "River", "ExpectedReturnYear", "AgeClass"),
    by.y = c("System", "River", "ReturnYear", "AgeClass"),
    all.x = TRUE
  )
  
  # Clean up the merged data, unsetting sorting and keys
  setkey(merged, NULL)
  
  return(merged)
}

# Merge data --------------------------------------------------------------

rivers <- c("Bristol_Bay", "Columbia", "Fraser") #

merged_by_river <- lapply(rivers, function(r) {
  brood_file  <- paste0( "data/", r, "_Brood_Table.csv")
  return_file <- paste0("data/", r, "_Return_Table.csv")
  merge_brood_return_by_ageclass(brood_file, return_file)
})
names(merged_by_river) <- rivers

merged_by_river

# In salmon population dynamics, a recruit is a salmon that survives to return as a spawning adult.

# BroodYear = the year salmon were born (hatched/eclosed).
# AgeClass_X.Y = number of recruits (salmon that survied and return to spawn) 
# from that brood that returned after X years in freshwater and Y years in ocean.

# BroodYear = 2000
# AgeClass_1.2 = 100,000
# → means: 100,000 salmon from brood year 2000 returned in 2003 (2000 + 1 + 2)

# ReturnYear = the year salmon were observed returning to spawn.
# AgeClass_X.Y = number of returning salmon in that year with that age class

# ReturnYear = 2003
# AgeClass_1.2 = 95,000
# → means: 95,000 returning salmon in 2003 were age 1.2 (born in 2000)


# Add the env covariates --------------------------------------------------

merged_by_river$Fraser


# Vizualize the data ------------------------------------------------------


