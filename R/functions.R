
cortest_cols <- function(data, dv, cols){

  data_cov <- data %>% dplyr::select({{cols}})
  data_dv <- data %>% dplyr::select({{dv}}) %>% pull()

  final_data <- data.frame()

  for(i in 1:length(data_cov)){

    y <- data_cov[i] %>% pull()
    corT <- broom::tidy(cor.test(as.numeric(data_dv), as.numeric(y)))
    corT$estimate <- corT$estimate
    corT$pvalue <- corT$p.value
    corT <- corT %>% mutate(var = names(data_cov[i]))
    final_data <- plyr::rbind.fill(final_data, corT)

  }

  final_data
}

collinearity <- function(data, cols){

  collinearity <- data.frame()
  for(i in cols){
    cor <- cortest_cols(data, i, cols )
    cor <- cor %>% mutate(voi = paste(i))

    collinearity <- bind_rows(collinearity, cor)

  }

  collinearity

  }

filter_collinearity <- function(data) {
  data %>% filter(pvalue != 0) %>%
    filter(estimate >= abs(0.7))
}


multi_collin <- function(data, type){
 lm_results <- switch(type,
  'bf' = {lm(log(mean_bf)~log(fac_taudem_17all_int)+
                 log(us_precip_1981_2010_cpg_all)+
                 log(us_tmax_1981_2010_int_cpg_all) +
                 log(swe_avg_0301_cpg_all) +
                 log(clay_100_cpg_all)+
                 log(slope_percent_int_cpg_all)+
                 MAP_UNIT_N +
                 mgmt, data = data)},
  'ba' = {lm(log(mean_bank_an)~log(fac_taudem_17all_int)+
               log(ave_basin_elev)+
               log(streamslope_percent_int_cpg_all) +
               log(silt_100_cpg_all) +
               log(twi_100_int_cpg_all) +
               MAP_UNIT_N +
               mgmt, data = data)},
  'wd' = {lm(log(mean_wd_2009)~log(fac_taudem_17all_int)+
               log(us_precip_1981_2010_cpg_all)+
               log(ave_ann_cwd) +
               log(clay_100_cpg_all) +
               log(swe_avg_0301_cpg_all) +
               log(twi_100_int_cpg_all) +
               log(slope_percent_int_cpg_all) +
               MAP_UNIT_N +
               mgmt, data = data)}
  )

 car::vif(lm_results)

}

linear_plot <- function(data, x, y, iv = NULL, log_y = NULL, log_x = NULL){


mapping <- ggplot2::aes(.data[[x]], .data[[y]], color = .data[[iv]])

  if(is.null(iv)){

   r2 <- NULL
   mapping$color <- NULL

  } else {

    r2 <- Add_R2()
  }

  if(!is.null(log_y)){
    logy <- ggplot2::scale_y_log10()
  } else {
    logy <- NULL
  }

  if (!is.null(log_x)){
    logx <- ggplot2::scale_x_log10()
  } else {
    logx <- NULL
  }


  data %>%
    ggplot(mapping) +
    geom_point() +
    r2 +
    custom_theme() +
    logy +
    logx
}

### from Anatolii at https://github.com/atsyplenkov/atslib/tree/master/R
Add_R2 <- function(method = "lm",
                   formula = y ~ x,
                   add_line = T,
                   lty = "dashed",
                   conf_int = F,
                   ...) {

  if(add_line == TRUE){


      list(
        ggpmisc::stat_poly_eq(aes(label =  paste(stat(eq.label),
                                                 stat(rr.label),
                                                 sep = "~~~~")),
                              formula = formula,
                              rr.digits = 2,
                              # coef.digits = 2,
                              parse = TRUE,
                              ...),
        ggplot2::geom_smooth(formula = formula,
                             method = method,
                             linetype = lty,
                             se = conf_int)
      )

  } else {

    ggpmisc::stat_poly_eq(aes(label =  paste(stat(eq.label),
                                             stat(rr.label),
                                             sep = "~~~~")),
                          formula = formula,
                          rr.digits = 2,
                          # coef.digits = 2,
                          parse = TRUE,
                          ...)
  }
}
