#pca

library(tidymodels)
library(tidyverse)
library(wesanderson)
library(janitor)
library(patchwork)
library(GGally)
library(ggtext)
library(sasLM)
library(janitor)
library(car)
library(broom)
library(ggpubr)
library(rstatix)

gr_95_new <- read_csv('R1WestPIBO102621.csv') %>%
  clean_names()

gr_95 <- read_csv('SummaryStatsKendall_gr_95.csv')[,1:36] %>%
  na.omit() %>%
  clean_names() %>%
  right_join(gr_95_new %>% select(site_id), by = 'site_id')

gr_95 <- gr_95 %>% mutate(all_harvest = pct_high_harv_int+pct_mod_harv_int,
                          all_fire = pct_mtbs_high+pct_mtbs_mod)

resourceviz::cairo_view()

pibo <- new_pibo_data_df %>% select(contains('SWE_'),mean_bf, mgmt)

pca_rec <- recipe(~., data = pibo) %>%
  update_role(mgmt,mean_bf,new_role = 'bring_along') %>%
  step_naomit(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

pca_prep <- prep(pca_rec)
pca_prep
juice(pca_prep)

tidied_pca <- tidy(pca_prep,3)


pca_pibo <- tidied_pca %>%
  filter(component %in% paste0("PC", 1:6)) %>%
  mutate(component = fct_inorder(component))

pca_pibo %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = F) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL) +
  theme_bw()

pca_wider <- pca_pibo %>%
  tidyr::pivot_wider(names_from = component, id_cols = terms)

pca_plot <- juice(pca_prep) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(size = 2.25, aes(color = mean_bf)) +
  scale_color_distiller(palette = 'RdBu') +
  theme_dark()

pca_plot_34 <- juice(pca_prep) %>%
  ggplot(aes(PC3, PC4)) +
  geom_point(size = 2.25, aes(color = mean_bf))  +
  scale_color_distiller(palette = 'RdBu') +
  theme_dark()

arrow_style <- arrow(length = unit(.05, "inches"),
                       type = "closed")

pca_plot + labs(color = 'Mean Bankfull (ft)')+
  geom_segment(data = pca_wider,
               aes(xend = PC1, yend = PC2),
               x = 0,
               y = 0,
               arrow = arrow_style) +
  ggrepel::geom_text_repel(data = pca_wider,
            aes(x = PC1, y = PC2, label = terms),
            size = 5, force = 100, max.overlaps = 20)
pca_plot

pca_plot_34 +
  geom_segment(data = pca_wider,
               aes(xend = PC3, yend = PC4),
               x = 0,
               y = 0,
               arrow = arrow_style) +
  ggrepel::geom_text_repel(data = pca_wider,
                           aes(x = PC3, y = PC4, label = terms),
                           size = 5, force = 100, max.overlaps = 20)
juice(pca_prep) %>%
  ggplot(aes(PC5, PC6)) +
  geom_point(size = 2.25, aes(color = mgmt)) +
  labs(color = 'Group')+ theme_bw() +
  geom_segment(data = pca_wider,
               aes(xend = PC1, yend = PC2),
               x = 0,
               y = 0,
               arrow = arrow_style)
sdev <- pca_prep$steps[[3]]$res$sdev
percent_variation <- sdev^2/sum(sdev^2)
tibble(component = unique(tidied_pca$component),
       percent_var = percent_variation) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(component, percent_var)) +
  geom_col() + theme_bw()



gr_95 %>%
  ggplot(aes(shed_area_floor, fill = mgmt)) +
  geom_histogram(position = 'dodge')

gr_95 %>%
  group_by(shed_area_floor, mgmt) %>%
  summarise(mean_bf = mean(mean_bf)) %>%
  ungroup() %>%
  pivot_wider(names_from = mgmt, values_from = mean_bf) %>%
mutate(diff = abs(Managed-Reference)) %>%
  ggplot(aes(shed_area_floor, diff)) +
  geom_point()
gr_95 %>%
  count(shed_area_floor,mgmt, sort = T)
gr_95 %>% filter(shed_area_floor_50 <100) %>%
  ggplot(aes(factor(shed_area_floor_50), mean_bf, color = mgmt))+
  geom_boxplot() +
  geom_point(position=position_jitterdodge(), aes(color = mgmt))+
  facet_wrap(~cut_number(ave_annual_precip, 3))

comb <- gr_95%>%
  filter(shed_area_floor_50 <100)
comb %>% count(shed_area_floor, mgmt, sort = T) %>% view()
library(modelr)
final_resamp_means <- comb   %>%
  filter(shed_area_floor == 40) %>%
  group_by(shed_area_floor_50,mgmt) %>%
  nest %>%
  mutate(boot = map(data, ~ bootstrap(.x, n = 1000, id = 'mean_bf') %>%
                      pull('strap') %>%
                      map_dbl(~ as_tibble(.x) %>%
                                pull('mean_bf') %>%
                                mean))) %>% select(-data) %>% unnest(boot)

comb <- comb %>% group_by(shed_area_floor_50,mgmt) %>% mutate(point_estimate = mean(mean_bf))

ci.resamp_se <- final_resamp_means %>% left_join(comb, by = c('shed_area_floor_50','mgmt')) %>%  rename(stat = "boot") %>% select(-mean_bf) %>% nest() %>% mutate(confidence_intervals = map(data,~ get_confidence_interval(.x, type = "se", point_estimate = .$point_estimate))) %>% select(shed_area_floor_50,mgmt, confidence_intervals) %>% unnest(confidence_intervals)
#and
ci.resamp_per <- final_resamp_means %>% rename(stat = "boot") %>%  group_by(shed_area_floor_50,mgmt) %>% nest() %>% mutate(confidence_intervals = map(data,~ get_confidence_interval(.x))) %>% select(shed_area_floor_50,mgmt, confidence_intervals) %>% unnest(confidence_intervals)

ci.resamp_per %>% mutate(diff = max(upper_ci) - min(lower_ci)) %>%
  ggplot() + geom_col(aes(fct_reorder(mgmt, diff, max), diff, fill = mgmt), show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~shed_area_floor_50) +
  coord_flip() +
  labs(x = "Model", y = "Difference in 97.5% to 2.5% (percentiles)", title = "Comparing Bootstrapped Distribution 95% CI (Perc.) of Model Means", subtitle = "80 cluster 10-fold CV")

ci.resamp_se %>%
  mutate(diff = max(upper_ci) - min(lower_ci)) %>%
  ggplot() + geom_col(aes(fct_reorder(mgmt, diff, max), diff, fill = mgmt), show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~shed_area_floor_50) +
  coord_flip() + labs(x = "Model", y = "Difference in  lower and upper Standard Error (95%)", title = "Comparing Bootstrapped Distribution 95% CI (SE) of Model Means", subtitle = "80 cluster 10-fold CV")

final_resamp_means %>%
  ggplot(aes(boot,color = mgmt)) +
  geom_density() +
  geom_rug()

gr_95 %>%
  ggplot(aes(mean_grad, fill = mgmt)) +
  geom_histogram(position = 'dodge') +
  scale_y_continuous(breaks = seq(1,10,1)) +
  scale_x_continuous(breaks = seq(0,4,.5)) +
  theme_bw() +
  labs(x = 'Mean Gradient (%)',
       title = 'Histogram of Mean Gradient by Group',
       subtitle = paste0('There are ', gr_95 %>%
                           filter(mean_grad >=3) %>%
                           nrow(), ' observations above or equal to 3% (9 Managed, 7 Reference)',
                         ' and\n',gr_95 %>%
                           filter(mean_grad >=2.5) %>%
                           nrow(), ' above 2.5% (26 Managed, 20 Reference)'),
       fill = 'Group') + theme(axis.text.x = element_text(size=8),
                               axis.text.y = element_text(size=8))

gr_95 %>%
  filter(mean_grad >=3) %>%
  count(mgmt)

gr_95 %>%
  filter(mean_grad >=3) %>% view()
  pull(mean_grad)
