
library(sf)
library(CAST)
library(caret)
library(tidymodels)
library(tidyverse)
library(doParallel)
library(parallel)
library(ggtext)
source('utils.R')
pibo_ml <- read_csv('pibo_ml.csv')
class(pibo_ml)
#clean
pibo_ml <- pibo_ml %>% select(1:30, mean_bf,mgmt, MAP_UNIT_N, utm1, utm2) %>% janitor::clean_names() %>% as.data.frame()

cluster10 <- kmeans(pibo_ml[,c('utm1','utm2')], (nrow(pibo_ml)/30))
# add the new variable back to your dataframe here
pibo_ml$spatial_cluster = cluster10$cluster

pibo_ml <- pibo_ml %>% na.omit() %>% mutate(across(is.character, factor))

library(caret)
set.seed(1234)
#split data into training and test data. 75% for training is the goal.
index <- createDataPartition(pibo_ml$mean_bf, list = FALSE, p=0.75)

train <- pibo_ml[index,]

test <- pibo_ml[-index,]

library(CAST)
set.seed(1234)
indices10 <- CreateSpacetimeFolds(train, spacevar = "spatial_cluster", k = 10)


#for balanced data
rec_sel <-
  recipe(mean_bf ~ ., data = train) %>%
  update_role(utm1, utm2, spatial_cluster,site_id,  new_role = "bring along") %>%
  step_center(contains("swe_"))  %>%
  step_scale(contains("swe_")) %>%
  step_pca(contains("swe_"), prefix = "pca_swe", threshold = 0.9)%>%
  step_center(contains("hydrogrp"))  %>%
  step_scale(contains("hydrogrp")) %>%
  step_pca(contains("hydrogrp"), prefix = "pca_hydrogrp", threshold = 0.9)


library(doParallel)
library(parallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

#add to controls
rfeCtrl <- rfeControl(
  functions = rfFuncs,
  method = "repeatedcv",
  repeats = 5,
  returnResamp = "all",
  verbose = FALSE,
  allowParallel = TRUE,
  index = indices10$index
)

set.seed(1234)
library(randomForest)
rfe10 <- rfe(rec_sel,
             data = train, metric = "RMSE",
             rfeControl = rfeCtrl,
             sizes = c(2:18))

#now remove cluster. consistency...
stopCluster(cluster)
registerDoSEQ()

varImp(rfe10)

plot(rfe10)
set.seed(1234)
rfe10tune <- train(rec_tune, data = train,
                   method = 'rf',
                   trControl = trainControl(method = "repeatedcv",
                                            repeats = 5,
                                            savePredictions = 'all',
                                            index = indices10$index))
c1 <- ggplot(rfe10) + ggtitle("Predicting Mean Bankfull Width")
c2 <- xyplot(rfe10,
             type = c("g", "p", "smooth"),
             ylab = "RMSE")
c3 <- densityplot(rfe10,
                  subset = Variables %in% c(1:6),
                  adjust = 1.25,
                  as.table = TRUE,
                  xlab = "RMSE CV",
                  pch = "|")

toltune(rfe10, 10, "Rsquared", title = "RFE 12th HUC Random Forest")
oneR <- oneSE(rfe10$results, metric = "Rsquared", num = 10 , maximize = TRUE)
rfe10$bestSubset
rfe10$variables
library(gridExtra)
grid.arrange(c1,c2,c3,ncol=2)

set.seed(134)
bf_split <- pibo_ml %>% select(fac_taudem_17all_int,
                               us_precip_1981_2010_cpg_all,
                               slope_percent_int_cpg_all,
                               mean_bf,
                               spatial_cluster,
                               contains('swe_')) %>%
  filter(!is.na(mean_bf)) %>%
  initial_split()

bf_train <- training(bf_split)
bf_test <- testing(bf_split)

rec_tune <- recipe(mean_bf ~ ., data = bf_train) %>%
  step_center(contains("swe_"))  %>%
  step_scale(contains("swe_")) %>%
  step_pca(contains("swe_"), prefix = "pca_swe", num_comp = 1) %>%
  update_role(spatial_cluster, new_role = 'strata')

set.seed(1234)

rf_spec <- rand_forest(mode = 'regression',
                       mtry = tune(),
                       trees = tune(),
                       min_n = tune()) %>%
  set_engine(engine = 'randomForest')

cv_10 <- vfold_cv(bf_train, strata = spatial_cluster)
set.seed(1234)
rf_grid <- tune_grid(rf_spec,
                     rec_tune,
                     resamples = cv_10)
rf_grid %>%
  collect_metrics()
#just show the best
rf_grid %>%
  show_best('rsq')
rf_grid %>%
  select_best('rsq')
rf_wf <- workflow() %>%
  add_recipe(rec_tune) %>%
  add_model(rf_spec)
final_wf <- finalize_workflow(rf_wf,parameters = tibble(mtry = 3,
                                                     trees = 800,
                                                     min_n = 11))
final_cv <- final_wf %>%
  fit_resamples(cv_10)

rf_fit <- fit(final_wf, bf_train)

final_cv %>% collect_metrics()

bf_test %>%
  bind_cols(predict(rf_fit, bf_test)) %>%
  metrics(mean_bf, .pred)


#spec is the model spec or workflow
lm_spec <- linear_reg(mode = 'regression') %>%
  set_engine(engine = 'lm')
# recipe
lm_rec <- recipe(mean_bf ~ ., data = bf_train) %>%
  update_role(spatial_cluster, contains('swe_'), new_role = 'strata') %>%
  step_log(all_predictors())

lm_wf <- workflow() %>%
  add_recipe(lm_rec) %>%
  add_model(lm_spec)

lm_cv <- lm_wf %>%
  fit_resamples(cv_10)

collect_metrics(lm_cv)

lm_fit <- fit(lm_wf, bf_train)

bf_test %>%
  bind_cols(predict(lm_fit, bf_test)) %>%
  metrics(mean_bf, .pred)

autoplot(lm_grid)

