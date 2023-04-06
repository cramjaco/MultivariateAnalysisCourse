## Essentially, everything here is copied from `03_Similarity.qmd` or `O2_Regression.qmd`

library(tidyverse)
library(here)
library(vegan) # lots of useful functions for analyis of communities -- was originally for vegetation data, hense the name
library(cowplot)



## ARISA Biological Data
bio00 <- read_csv(here("Data", "arisa_latlon_sort_vess_bio.csv"), na = "nd")

arisa_fragments <- bio00 %>%
  select(date_local, depth_n, arisa_frag, rel_abund) %>%
  filter(!is.na(rel_abund))
arisa_community <- arisa_fragments %>%
  pivot_wider(names_from = arisa_frag, values_from = rel_abund, values_fn = median)

arisa_community2 <- arisa_fragments %>%
  select(date_local, depth_n, arisa_frag, rel_abund) %>%
  pivot_wider(names_from = arisa_frag, values_from = rel_abund, values_fn = median)

arisa_community3 <- arisa_community2 %>%
  mutate(sampleID = paste(date_local, depth_n, sep = "_")) %>%
  select(sampleID, starts_with("ARISA"))

arisa_community_4 <- arisa_community3 %>% as.data.frame() %>% .[,-1]
rownames(arisa_community_4) <- arisa_community3$sampleID

arisa_community_mtx <- arisa_community_4 %>% # throw away the column with the sampleIDs
  as.matrix() # turn into matrix
#rownames(arisa_community_mtx) <- arisa_community3$sampleID # use the sampleID column as names

## Environmental Data

env00 <- read_csv(here("Data", "arisa_latlon_sort_vess_phys.csv"), na = c("nd"))

env01 <- env00 %>% 
  mutate(sampleID = paste(as.character(date_local), depth_n, sep = "_"))

env02 <- env01 %>%
  filter(sampleID %in% arisa_community3$sampleID)

env03 <- env02 %>%
  filter(sampleID != "2006-10-15_5" | !is.na(cruise_id))

## Taxonomoy Data

tax00 <- read_csv(here("Data", "bins_taxonomy.csv"), na = "nd")

tax00_surface <- tax00 %>%
  # just use the taxonomy data for the surface
  filter(nodeDepths == 5) %>%
  # make a column called arisa_frag that is comperable to the arisa_frag colum in arisa_fragments_surface
  # I'm using str_remove and regular expressions: I highly recommend this cheat sheet
  # https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_strings.pdf
  mutate(arisa_frag = str_remove(nodeIDs,"^[^_]*_")) %>%
  # just select the arisa_frag column and the primary Domain through genus level IDs for each fragment
  select(arisa_frag, Domain:Genus)

subset_arisa_by_depth<- function(target_depth_n){
  ac3 <- arisa_community2 %>% filter(depth_n == target_depth_n) %>% select(-depth_n)
  # ac <- ac %>% rename(sampleID = date_local)
  
  ac4 <- ac3 %>% as.data.frame() %>% column_to_rownames("date_local")
  
  ac4
}

# test
test_subset_arisa <- subset_arisa_by_depth("5")

subset_env_by_depth <- function(target_depth_n){
  es0 <- env03 %>% filter(depth_n == target_depth_n)
  es0
}

test_subset_env <- subset_env_by_depth("5")
