# Fit models using indices from multiple day periods (e.g. dawn, solarNoon and dusk) ----


Indices_Summary_wider <- Indices_Summary %>% 
  filter(type %in% c("dawn", "solarNoon", "dusk")) %>% 
  dplyr::select(c("Site", "SurveyIDR12", "NMPresent", "Threshold20m", "Threshold40m", "MeanMiner20m", "MeanMiner40m", "type", "audioDays", ends_with(c("median")))) %>% 
  pivot_wider(names_from = type, values_from = ends_with(c("median"))) %>% 
  drop_na()


#MeanMiner20m
rf_data_MeanMiner20m <- Indices_Summary_wider %>% 
  filter(audioDays == 1) %>% 
  dplyr::select(c("MeanMiner20m", contains("dawn"), contains("dusk"), contains("solarNoon")))

rf_model_MeanMiner20m <- train(MeanMiner20m~.,
                               data = rf_data_MeanMiner20m,
                               method = "cforest",
                               metric = "RMSE",
                               tuneGrid = tunegrid_cforest,
                               trControl = control_cforest,
                               controls = cforest_unbiased(ntree = 1000))

#MeanMiner40m
rf_data_MeanMiner40m <- Indices_Summary_wider %>% 
  filter(audioDays == 1) %>% 
  dplyr::select(c("MeanMiner40m", contains("dawn"), contains("dusk"), contains("solarNoon")))

rf_model_MeanMiner40m <- train(MeanMiner40m~.,
                               data = rf_data_MeanMiner40m,
                               method = "cforest",
                               metric = "RMSE",
                               tuneGrid = tunegrid_cforest,
                               trControl = control_cforest,
                               controls = cforest_unbiased(ntree = 1000))