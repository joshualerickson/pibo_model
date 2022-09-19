library(targets)

source('R/functions.R')
options(tidyverse.quiet = TRUE)

# Set target-specific options such as packages.
tar_option_set(tidy_eval = TRUE, packages = c("dplyr", "broom",
                            "sf", "tidyverse",
                            "plyr", "car", "CAST",
                            "caret", "performance"))
# End this file with a list of target objects.
list(
  tar_target(raw_ancova_data, "data/gr_95.csv", format = 'file'),
  tar_target(data_ancova, read_csv(raw_ancova_data)),
  tar_target(raw_pibo_over_time, "data/gr_95_ts.csv", format = 'file'),
  tar_target(data_ts, read_csv(raw_pibo_over_time)),
  tar_target(correlations_to_bf, cortest_cols(data_ancova, mean_bf, c(drain_density:pct_mtbs_high,
                                                               aug_et_2000_2015_cpg_all:us_tmax_1981_2010_int_cpg_all))),
  tar_target(correlations_to_ba, cortest_cols(data_ancova, mean_bank_an, c(drain_density:pct_mtbs_high,
                                                               aug_et_2000_2015_cpg_all:us_tmax_1981_2010_int_cpg_all))),
  tar_target(correlations_to_wd, cortest_cols(data_ancova, mean_wd_2009, c(drain_density:pct_mtbs_high,
                                                               aug_et_2000_2015_cpg_all:us_tmax_1981_2010_int_cpg_all))),
  tar_target(correlations_to_check_bf,  correlations_to_bf %>%
               filter(pvalue <= 0.05)),
  tar_target(correlations_to_check_angle,  correlations_to_ba %>%
               filter(pvalue <= 0.05)),
  tar_target(correlations_to_check_wd,  correlations_to_wd %>%
               filter(pvalue <= 0.05)),
  tar_target(bf_var_collinearity, collinearity(data_ancova, c('fac_taudem_17all_int','us_precip_1981_2010_cpg_all',
                                                              'us_tmax_1981_2010_int_cpg_all', 'swe_avg_0301_cpg_all',
                                                              'clay_100_cpg_all', 'slope_percent_int_cpg_all'))),
  tar_target(ba_var_collinearity, collinearity(data_ancova, c('fac_taudem_17all_int','streamslope_percent_int_cpg_all',
                                                              'ave_basin_elev', 'twi_100_int_cpg_all', 'silt_100_cpg_all'))),
  tar_target(wd_var_collinearity, collinearity(data_ancova, c('fac_taudem_17all_int','us_precip_1981_2010_cpg_all', 'ave_ann_cwd',
                                                              'swe_avg_0301_cpg_all', 'clay_100_cpg_all', 'slope_percent_int_cpg_all',
                                                              'twi_100_int_cpg_all'))),
  tar_target(collinearity_to_check_bf, filter_collinearity(bf_var_collinearity)),
  tar_target(collinearity_to_check_ba, filter_collinearity(ba_var_collinearity)),
  tar_target(collinearity_to_check_wd, filter_collinearity(wd_var_collinearity)),
  tar_target(multcollinearity_bf, multi_collin(data_ancova, 'bf')),
  tar_target(multcollinearity_ba, multi_collin(data_ancova, 'ba')),
  tar_target(multcollinearity_wd, multi_collin(data_ancova, 'wd')),
  tar_target(lasso_model_bf, feature_selection(data_ancova, 'bf')),
  tar_target(lasso_model_ba, feature_selection(data_ancova, 'ba')),
  tar_target(lasso_model_wd, feature_selection(data_ancova, 'wd')),
  tar_target(var_imp_bf, varImp(lasso_model_bf$finalModel, lasso_model_bf$bestTune$lambda)),
  tar_target(var_imp_ba, varImp(lasso_model_ba$finalModel, lasso_model_ba$bestTune$lambda)),
  tar_target(var_imp_wd, varImp(lasso_model_wd$finalModel, lasso_model_wd$bestTune$lambda)),
  tar_target(bf_assumption, check_if_normal_hetero(data_ancova, 'bf', var_imp = var_imp_bf)),
  tar_target(ba_assumption, check_if_normal_hetero(data_ancova, 'ba', var_imp = var_imp_ba)),
  tar_target(wd_assumption, check_if_normal_hetero(data_ancova, 'wd', var_imp = var_imp_wd))




)
