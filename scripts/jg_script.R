

# 1. Create Data Package
# -- All data
# Geo only
# 2. Use case package
# -- Parameters:  5

require(sf)
require(tidyverse)
require(FactoMineR)
require(factoextra)

#install.packages("remotes")
#remotes::install_github("cities-lab/SLD")

setwd("ENTER YOUR WORKING DRIVE HERE")
setwd('~/documents/jsd')

#Input files needed in your working drive:
#1. SLD_sf folder containing shapefile and metadata for the current version of the SLD

#outputs that will save in your working drive:
#1. US CBG PCA Results - Correlation.csv : Table of correlatoins between each input variable and the first 5 factors. Allows you to qualitatively define each factor (1 through 5)
#2. US CBG PCA Results - Percent Contribution.csv : % Contribution of each individual input variable to each factor
#3. RData file with data table of every CBG FIPS Code (key for matching) and value of each factor for both National- and (if applicable for a CBG) CSA-scale PCA.
#For input variable definitions, consult SLD data dictionary
#Both of the above are for the overall national US PCA (such files are not generated for each individual market)

sld <- st_read(file.path(getwd(), "input data/SLD_sf/SmartLocationDb.shp"))
sld_meta <- sld %>%
  dplyr::mutate(geometry = NULL)  %>%
  tibble::as_tibble() %>%
  dplyr::select(-TRFIPS,-CFIPS,-SFIPS,-CBSA_EMP,-CBSA_POP,-CBSA_WRK,-Shape_Leng,-Shape_Area,
                -D5br_Flag,-D5be_Flag,-D1_flag)

#identifying missing data using placeholders/irrational values
sld_meta[sld_meta==-99999] <- NA
sld_meta$CBSA[sld_meta$CBSA == 0] <- NA

#creating a master spreadsheet with every CBG and CSA
sld_pca_master <- sld_meta %>%
  dplyr::select(GEOID10, CSA, CSA_Name)

#creating sub-sets based on data availability
#E_* variables (Employment data not available in MA or PR)
employment_subset <- colnames(select(sld_meta,starts_with("E_")))

#GTFS variables (excluding D4a, which is not widely available as it is restricted to "TOD Database Locations")
gtfs_subset <- c("D4c","D4d","D5br","D5be","D5ce","D5dr","D5dri","D5de","D5dei")

#1. all SLD variables with no missing data (i.e. all CBGs)
sld_all <- sld_meta %>%
  select(which(colSums(is.na(.)) %in% 0))

#2. subset that includes the E_* employment variables (federal employment + wages). Supposedly "Entire U.S. (except MA and PR)" according to SLD User Guide"
sld_full_employment <- sld_meta %>%
  drop_na(employment_subset) %>%
  select(which(colSums(is.na(.)) %in% 0))

#3. subset that includes D5ce (Regional access - Auto) and E_* employment variables.
#D5ce is only available for CBGs located in CBSAs, so it's also all CBSA CBGs (with employment data)
sld_regaccess_auto <- sld_meta %>%
  drop_na(employment_subset) %>%
  drop_na(D5ce) %>%
  select(which(colSums(is.na(.)) %in% 0))

#4. subset of all CBGs in CSAs (#1 filtered for CSAs)
sld_csa_all <- sld_meta %>%
  drop_na(CSA) %>%
  select(which(colSums(is.na(.)) %in% 0))

#5. subset of #4 (all CSAs) with E_* employment variables
sld_csa_employment <- sld_meta %>%
  drop_na(employment_subset) %>%
  drop_na(CSA) %>%
  select(which(colSums(is.na(.)) %in% 0))

#6. subset of all CBGs with GTFS & E_* Employment data (excluding D4a, which is only available in "participating TOD database locations")
sld_GTFS_employment <- sld_meta %>%
  drop_na(employment_subset) %>%
  drop_na(gtfs_subset) %>%
  select(which(colSums(is.na(.)) %in% 0))




######################
#PCA - NATIONAL SCALE#
######################

#The PCA code below is designed to be cumulative and build a "master dataset" with factor scores for each subset
#The default is sld_all

#Code Written for "sld_all", but can substitute any subset from 2-6 above by changing below definition
sld_pca <- sld_all

#running PCA (using FactoMineR), limited to 5 components. If any NA, all NA REPLACED with column means.
sld_pca_output  <-  PCA(select(sld_pca,-GEOID10), scale.unit = TRUE, ncp = 5)
barplot(sld_pca_output$eig[,1],main="Eigenvalues",names.arg=1:nrow(sld_pca_output$eig)) #plot of eigenvalues
sld_pca_results <- get_pca_ind(sld_pca_output)

#extracting data from the pca
sld_pca_factorscores  <-  sld_pca_results$coord #extracting factor scores for each observation
sld_pca_eigen  <-  get_eigenvalue(sld_pca_output) #extracting eigenvalues for each factor
sld_pca_contrib  <-  get_pca_var(sld_pca_output) #extracting contributions for each factor

#exporting data to csv to qualitatively assess each principal component
write.csv(sld_pca_contrib$coord, "US CBG PCA Results - Correlation.csv")
write.csv(sld_pca_contrib$contrib, "US CBG PCA Results - Percent Contribution.csv")

#merging factor scores with GEOID10
#1. create dataframe of GEOID for chosen subsample
sld_factorscores_CBG <- sld_pca %>%
  select(GEOID10)

#2. merge with factor scores
sld_pca_factorscores  <-  as.data.frame(sld_pca_factorscores)
sld_factorscores_CBG <- bind_cols(sld_factorscores_CBG,sld_pca_factorscores) %>%
  setNames(c("GEOID10","Factor_1_National","Factor_2_National", "Factor_3_National", "Factor_4_National", "Factor_5_National"))

#3. bind to master dataframe
sld_pca_master <- left_join(sld_pca_master,sld_factorscores_CBG, by="GEOID10")

#################
#PCA - CSA SCALE#
#################

#identify subsample that retains CSA column. sld_csa_all is default.
sld_pca_csa <- sld_csa_all

#get unique list of CSAs in the dataset
unique_CSA <- sld_pca_csa %>%
  select(CSA,CSA_Name) %>%
  mutate(CSA = as.integer(CSA)) %>%
  distinct() %>%
  drop_na()

#start a spreadsheet to enter data
sld_factorscores_csa_CBG <- sld_pca_csa %>%
  select(GEOID10) %>%
  add_column(Factor_1_CSA = "", Factor_2_CSA="", Factor_3_CSA="", Factor_4_CSA="", Factor_5_CSA="")

#first CSA
market  <-  unique_CSA$CSA[[1]]
sld_pca_temp <- sld_pca_csa %>%
  mutate(CSA = as.integer(CSA)) %>%
  filter(CSA == market)

sld_pca_csa_output <- PCA(select(sld_pca_temp,-GEOID10,-CSA,-CSA_Name,-CBSA,-CBSA_Name), scale.unit = TRUE, ncp = 5)
sld_pca_csa_results <- get_pca_ind(sld_pca_csa_output)
sld_pca_csa_factorscores <- as.data.frame(sld_pca_csa_results$coord)
temp_csa_CBG <- sld_pca_temp %>%
  select(GEOID10)
temp_csa_CBG <- bind_cols(temp_csa_CBG,sld_pca_csa_factorscores)

#start the spreadsheet that will accumulate all CSAs
temp_csa_CBG_all <- temp_csa_CBG

#Loop to calculate each CSA.
#Only factor scores are extracted.
#Must qualitatively assess factors independently (i.e. creating a subsample with a single CSA & running National code)
for (i in 2:length(unique_CSA$CSA)) {
  market  <-  unique_CSA$CSA[[i]]
  sld_pca_temp <- sld_pca_csa %>%
    mutate(CSA = as.integer(CSA)) %>%
    filter(CSA == market)

  sld_pca_csa_output <- PCA(select(sld_pca_temp,-GEOID10,-CSA,-CSA_Name,-CBSA,-CBSA_Name), scale.unit = TRUE, ncp = 5)
  sld_pca_csa_results <- get_pca_ind(sld_pca_csa_output)
  sld_pca_csa_factorscores <- as.data.frame(sld_pca_csa_results$coord)
  temp_csa_CBG <- sld_pca_temp %>%
    select(GEOID10)
  temp_csa_CBG <- bind_cols(temp_csa_CBG,sld_pca_csa_factorscores)

  temp_csa_CBG_all <- full_join(temp_csa_CBG_all,temp_csa_CBG)
}

#merging CSA-specific factor scores into master spreadhseet
#first rename columns
temp_csa_CBG_all <- temp_csa_CBG_all %>%
  plyr::rename(c("Dim.1" = "Factor_1_CSA", "Dim.2"="Factor_2_CSA", "Dim.3"="Factor_3_CSA", "Dim.4"="Factor_4_CSA", "Dim.5"="Factor_5_CSA"))

#joining
sld_pca_master <- left_join(sld_pca_master,temp_csa_CBG_all, by="GEOID10")

#saving
#default here. 2nd part of filename should be manually changed to whatever SLD data scope was used (default is #1. sld_all)
save(sld_pca_master,file=paste0("~/PCA_Factors_by_CBG_","sld_all_",Sys.Date(),".Rdata"))

