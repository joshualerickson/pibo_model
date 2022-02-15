#' Custom Theme
#'
#' @param font_size
#' @param font_family
#' @param line_size
#' @param rel_small
#' @param rel_tiny
#' @param rel_large
#' @param color
#'
#' @return
#' @export
#'
#' @examples
custom_theme <-  function (font_size = 14, font_family = "", line_size = 0.5,
                           rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14, color = "grey85",
                           em = TRUE) {
  half_line <- font_size/2
  small_size <- rel_small * font_size


  theme_bw(base_size = font_size, base_family = font_family) %+replace%
    theme(line = element_line(color = "black", size = line_size,
                              linetype = 1, lineend = "butt"), rect = element_rect(fill = NA,
                                                                                   color = NA, size = line_size, linetype = 1), text = element_text(family = font_family,
                                                                                                                                                    face = "plain", color = "black", size = font_size,
                                                                                                                                                    hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9,
                                                                                                                                                    margin = margin(), debug = FALSE), axis.line = element_line(color = "black",
                                                                                                                                                                                                                size = line_size, lineend = "square"), axis.line.x = NULL,
          axis.line.y = NULL, axis.text = element_text(color = "black",
                                                       size = small_size), axis.text.x = element_markdown(margin = margin(t = small_size/4),
                                                                                                          vjust = 1), axis.text.x.top = element_markdown(margin = margin(b = small_size/4),
                                                                                                                                                         vjust = 0), axis.text.y = element_markdown(margin = margin(r = small_size/4),
                                                                                                                                                                                                    hjust = 1), axis.text.y.right = element_markdown(margin = margin(l = small_size/4),
                                                                                                                                                                                                                                                     hjust = 0), axis.ticks = element_line(color = "black",
                                                                                                                                                                                                                                                                                           size = line_size), axis.ticks.length = unit(half_line/2,
                                                                                                                                                                                                                                                                                                                                       "pt"), axis.title.x = element_markdown(margin = margin(t = half_line/2),
                                                                                                                                                                                                                                                                                                                                                                              vjust = 1), axis.title.x.top = element_markdown(margin = margin(b = half_line/2),
                                                                                                                                                                                                                                                                                                                                                                                                                              vjust = 0), axis.title.y = element_markdown(angle = 90,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                          margin = margin(r = half_line/2), vjust = 1),
          axis.title.y.right = element_markdown(angle = -90, margin = margin(l = half_line/2),
                                                vjust = 0), legend.background = element_blank(),
          legend.spacing = unit(font_size, "pt"), legend.spacing.x = NULL,
          legend.spacing.y = NULL, legend.margin = margin(0,
                                                          0, 0, 0), legend.key = element_blank(), legend.key.size = unit(1.1 *
                                                                                                                           font_size, "pt"), legend.key.height = NULL,
          legend.key.width = NULL, legend.text = element_text(size = rel(rel_small)),
          legend.text.align = NULL, legend.title = element_markdown(hjust = 0),
          legend.title.align = NULL, legend.position = "right",
          legend.direction = NULL, legend.justification = c("left",
                                                            "center"), legend.box = NULL, legend.box.margin = margin(0,
                                                                                                                     0, 0, 0), legend.box.background = element_blank(),
          legend.box.spacing = unit(font_size, "pt"),
          panel.background = element_blank(), panel.border = element_blank(),
          panel.grid = element_line(color = color, size = line_size), panel.grid.major = NULL,
          panel.grid.minor = NULL,
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = NULL,
          panel.grid.minor.x = NULL,
          panel.grid.minor.y = NULL,
          panel.spacing = unit(half_line,
                               "pt"), panel.spacing.x = NULL, panel.spacing.y = NULL,
          panel.ontop = FALSE, strip.background = element_rect(fill = "grey80"),
          strip.text = element_text(size = rel(rel_small),
                                    margin = margin(half_line/2, half_line/2, half_line/2,
                                                    half_line/2)), strip.text.x = NULL, strip.text.y = element_text(angle = -90),
          strip.placement = "inside", strip.placement.x = NULL,
          strip.placement.y = NULL, strip.switch.pad.grid = unit(half_line/2,
                                                                 "pt"), strip.switch.pad.wrap = unit(half_line/2,
                                                                                                     "pt"), plot.background = element_blank(),
          plot.title = element_markdown(face = "bold", size = rel(rel_large),
                                        hjust = 0, vjust = 1, margin = margin(b = half_line)),
          plot.subtitle = element_text(size = rel(rel_small),hjust = 0, vjust = 1, margin = margin(b = half_line)),
          plot.caption = element_markdown(size = rel(rel_tiny),
                                          hjust = 1, vjust = 1, margin = margin(t = half_line)),
          plot.tag = element_text(face = "bold", hjust = 0,
                                  vjust = 0.7), plot.tag.position = c(0, 1), plot.margin = margin(half_line,half_line, half_line, half_line), complete = TRUE)
}



#' Facet function
#'
#' @param data
#' @param dv
#' @param color
#' @param cut
#' @param scales
#'
#' @return
#' @export
#'
#' @examples
facet_func <- function(data, dv, color, cut, scales = NULL){

 scale_color <-  if(is.numeric(data %>% pull({{color}}))){

    scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous')))

  } else {

    NULL
  }

  data %>%
    ggplot(aes(samp_date, {{dv}}, group = site_id, color = {{color}})) +
    geom_point(alpha = 0.3) +
    geom_line(aes(group = site_id))  +
    scale_color +
    facet_wrap(vars({{cut}}), scales = scales)
}


#' One Standard Deviation Method
#'
#' @param objlist
#' @param metric
#' @param num
#' @param maximize
#'
#' @return
#' @export
#'
#' @examples
oneReturn <- function(objlist, metric = "", num, maximize = TRUE){

  oneStan <- data.frame()
  y <- NULL
  for (i in 1:length(objlist)){

    object <- objlist[[i]]

    object <- object$results

    a <- oneSE(object, metric = "Kappa", num = 10 , maximize = TRUE)

    a <- object[a,]
    y <- rbind(y, a)

  }
  return(y)
}

#' Tolerance Tuner
#'
#' @param object
#' @param len
#' @param metric
#' @param title
#' @param plot
#'
#' @return
#' @export
#'
#' @examples
toltune <- function(object, len, metric = "", title = "", plot = TRUE){

  object <- object$results
  res = integer()
  for (i in 1:len){

    res[i] <- tolerance(object, metric = metric, tol = i,maximize = TRUE)

  }
  tolit <- object[res,1:6]
  tolit$tolerance <- seq(1,len, 1)

  if (plot == "TRUE"){

    pp <-  tolit %>% ggplot(aes_string("tolerance",metric)) +
      geom_point(aes(color = factor(Variables)), size = 3) + geom_line() +
      ggtitle(paste(title),"statistic against tolerance values") +
      scale_x_continuous(breaks = 1:len) +
      labs(color = "# Variables")



  } else {

  }


  return(list(tolit, pp))

}


cortest_cols <- function(data, dv, cols){

  data_cov <- data %>% select({{cols}})
  data_dv <- data %>% select({{dv}}) %>% pull()

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


pibo_sites <- read_sf('pibo_sites.gpkg', 'pibo_sites')

pibo_ml <- read_csv('pibo_ml.csv') %>%
  janitor::clean_names()

gr_95_new <- read_xlsx('R1WestPIBO110521.xlsx') %>%
  clean_names()

gr_95 <- read_csv('SummaryStatsKendall_gr_95.csv')[,1:36] %>%
  na.omit() %>%
  clean_names() %>%
  right_join(gr_95_new %>% select(site_id), by = 'site_id') %>%
         mutate(mgmt = factor(mgmt),
                precip_cut = cut_interval(ave_annual_precip,n=3),
                 shed_cut = cut_number(shed_areakm2, n = 2))

gr_95 <- gr_95 %>% left_join(pibo_sites %>% st_drop_geometry() %>%
                               select(MAP_UNIT_N, site_id),
                             by = 'site_id')
gr_95 <- pibo_ml %>%
  pivot_longer(contains('hydrogrp')) %>%  select(name, value, site_id:us_tmax_1981_2010_int_cpg_all) %>%
  group_by(site_id) %>% slice_max(value, with_ties = F) %>% select(site_id:us_tmax_1981_2010_int_cpg_all,hydro_group = 'name') %>%
  ungroup() %>%
  right_join(gr_95) %>%
  mutate(MAP_UNIT_N = factor(MAP_UNIT_N))

library(tidymodels)

pca_pibo <- recipe(mean_bf ~ ., data = pibo_ml %>%
                     select(contains("SWE_"),site_id,) %>%
                     right_join(gr_95 %>% select(mean_bf, site_id), by = 'site_id')) %>%
  update_role(site_id,new_role = 'bring_along') %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_pca(all_predictors())

pca_swe <- prep(pca_pibo) %>% juice()

gr_95 <- gr_95 %>% left_join(pca_swe %>% select(site_id, PC1, PC2))

gr_95_all <- read_xlsx('R1WestDividePIBO.xlsx',
                       sheet = 'R1WestHabitat') %>% clean_names()

gr_95_ts <- gr_95_all %>%
  right_join(gr_95 %>% select(site_id:us_tmax_1981_2010_int_cpg_all, drain_density:pct_nfs, mean_bf:shed_cut,MAP_UNIT_N, PC1,PC2,  -mgmt)) %>%
  mutate(impact_fire = pct_mtbs_mod+pct_mtbs_high,
         impact_harvest = pct_high_harv_int+pct_mod_harv_int,
         harv_group = ifelse(impact_harvest <= 5, 'Low', ifelse(impact_harvest > 5 & impact_harvest <= 18, 'Mod', 'High')),
         harv_group = factor(harv_group, levels = c('Low', 'Mod', 'High')),
         fire_group = ifelse(impact_fire <= 5, 'Low', ifelse(impact_fire > 5 & impact_fire <= 18, 'Mod', 'High')),
         fire_group = factor(fire_group, levels = c('Low', 'Mod', 'High')),
         site_id = factor(site_id))

gr_95_ts <- gr_95_ts %>%
  group_by(site_id) %>% summarise(sd_bf = sd(bf, na.rm = T),
                                  meanbf = mean(bf, na.rm = T),
                                  coef_bf = sd_bf/meanbf,
                                  coef_bfl = sqrt(exp(sd(log(bf), na.rm = TRUE)^2)-1),
                                  sd_ba = sd(bank_angle, na.rm = T),
                                  meanba = mean(bank_angle, na.rm = T),
                                  coef_ba = sd_ba/meanba,
                                  coef_bal = sqrt(exp(sd(log(bank_angle), na.rm = TRUE)^2)-1)) %>%
  ungroup() %>%
  left_join(gr_95_ts, by = 'site_id')

gr_95 <- gr_95_ts %>% group_by(site_id) %>% slice(n = 1) %>% ungroup()
