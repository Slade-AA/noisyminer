library(tidyverse)

sites <- gsub(".RDS", "", list.files(path = "G:/_combinedIndices/", pattern = "*.RDS$"))
sites = sites[!sites=="BS2"]

for (site in sites) {
  og <- readRDS(paste0("G:/_combinedIndices/", site, ".RDS"))
  new <- readRDS(paste0("G:/_newRun/_combinedIndices/", site, ".RDS"))
  
  combined <- list(Indices_Summary = bind_rows(og$Indices_Summary, new$Indices_Summary),
                   Indices_SpectralAggregated = bind_rows(og$Indices_SpectralAggregated, new$Indices_SpectralAggregated),
                   Indices_FeatureReduction = bind_rows(og$Indices_FeatureReduction, new$Indices_FeatureReduction),
                   Indices_R = bind_rows(og$Indices_R, new$Indices_R))
  
  saveRDS(combined, paste0("G:/_combinedIndices_Total/", site, ".RDS"))
  
  remove(og, new)
}