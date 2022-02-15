### plotting
library(patchwork)
library(tidyverse)
library(GGally)
library(ggtext)
library(sasLM)
library(janitor)
library(readxl)
thematic::thematic_on(font = 'Montserrat')
source('utils.R')
resourceviz::cairo_view()
##### Pre-Proccesing
library(sf)
pibo_sites <- read_sf('pibo_sites.gpkg', 'pibo_sites')

gr_95_new <- read_xlsx('C:/Users/joshualerickson/Box/PIBO and Water Yield/Data/R1WestPIBO110521.xlsx') %>%
  clean_names()

gr_95 <- read_csv('SummaryStatsKendall_gr_95.csv')[,1:36] %>%
  na.omit() %>%
  clean_names() %>%
  right_join(gr_95_new %>% select(site_id), by = 'site_id') %>%
  mutate(precip_cut = cut_interval(ave_annual_precip,n=6),
         shed_cut = cut_interval(shed_areakm2, n = 6),
         mgmt = factor(mgmt))
gr_95 <- gr_95 %>% left_join(pibo_sites %>% st_drop_geometry() %>%
                               select(MAP_UNIT_N, site_id), by = 'site_id')

gr_95_all <- read_xlsx('C:/Users/joshualerickson/Box/PIBO and Water Yield/Data/R1WestDividePIBO.xlsx',
                       sheet = 'R1WestHabitat') %>% clean_names()

gr_95_ts <- gr_95_all %>%
  right_join(gr_95 %>% select(site_id, drain_density:pct_nfs, mean_bf:shed_cut, -mgmt)) %>%
  mutate(impact_fire = pct_mtbs_mod+pct_mtbs_high,
         impact_harvest = pct_high_harv_int+pct_mod_harv_int)

gr_95_ts <- gr_95_ts %>%
  group_by(site_id) %>% summarise(sd_bf = sd(bf, na.rm = T),
                                  meanbf = mean(bf, na.rm = T),
                                  coef_bf = sd_bf/meanbf) %>%
  ungroup() %>%
  left_join(gr_95_ts, by = 'site_id') %>%
  mutate(scaled_bf = (bf - meanbf)/sd_bf)

gr_95_ts <- gr_95_ts %>% group_by(site_id) %>%
  summarise(sd_of_scale = sd(scaled_bf, na.rm = T)) %>%
  ungroup() %>%
  left_join(gr_95_ts, by = 'site_id')

gr_95_ts_everything <- read_csv('SummaryStatsKendall_gr_95.csv')[,1:36] %>%
  na.omit() %>%
  clean_names() %>%
  right_join(gr_95_new %>% select(site_id), by = 'site_id') %>%
  mutate(precip_cut = cut_interval(ave_annual_precip,n=15),
         shed_cut = cut_interval(shed_areakm2, n = 15),
         mgmt = factor(mgmt)) %>%
  select(site_id, drain_density:pct_nfs, mean_bf:shed_cut, -mgmt) %>%
  left_join(gr_95_all)


bf_sd_ts <- gr_95_ts %>% group_by(site_id, mgmt) %>%
  summarise(across(bf:lw_vol, ~sd(.,na.rm = T))) %>%
  left_join(gr_95 %>% select(site_id, drain_density:pct_nfs,stream,mean_bf:shed_cut) %>%
                             group_by(site_id) %>%
              slice(n=1), by = c('site_id', 'mgmt')) %>%
              ungroup() %>%
  mutate(impact = if_else(mgmt == 'Reference', pct_mtbs_mod+pct_mtbs_high,
                          pct_high_harv_int+pct_mod_harv_int))



gr_95_ts %>%
  top_n(50, impact_harvest) %>%
  facet_func(dv = bf, color = impact_harvest)+
  custom_theme()

b1 <- gr_95_ts %>% filter(mgmt == "Reference") %>%
  group_by(site_id) %>% mutate(sd_ba = sd(bank_angle, na.rm = T)) %>%
  ungroup() %>%
  mutate(sd_cut = cut_number(coef_bf,6)) %>%
  facet_func(dv = bf, color = ave_annual_precip, cut = sd_cut) +
  scale_color_gradientn(colours = hcl.colors('Zissou1', n = 11, rev = T),
                        breaks = seq(600, 2000, 400)) +
  custom_theme() + labs(x = 'Sample Date', y = 'Bankfull Width (m)',
                        title = 'Bankfull Width over Time: faceted by Coefficient of Variation',
                        color = 'Avg. Annual Precip (mm)',
                        caption = 'Reference Sites Only') +
  ylim(0, 35)

b2 <- gr_95_ts %>% filter(mgmt == "Managed") %>%
  group_by(site_id) %>% mutate(sd_ba = sd(bank_angle, na.rm = T)) %>%
  ungroup() %>%
  mutate(sd_cut = cut_number(coef_bf,6)) %>%
  facet_func(dv = bf, color = ave_annual_precip, cut = sd_cut) +
  scale_color_gradientn(colours = hcl.colors('Zissou1', n = 11, rev = T),
                        breaks = seq(600, 2000, 400))  +
  custom_theme() + labs(x = 'Sample Date', y = '',
                        title = '',
                        color = 'Avg. Annual Precip (mm)',
                        caption = 'Managed Sites Only')+
  ylim(0, 35)

(b1+b2) + plot_layout(guides = 'collect')

gr_95_ts %>% filter(mgmt == "Reference") %>%
  group_by(site_id) %>% mutate(sd_ba = sd(bank_angle, na.rm = T)) %>%
  ungroup() %>%
  mutate(sd_cut = cut_number(sd_ba,6)) %>%
  facet_func(dv = bank_angle, color = impact_fire, cut = sd_cut) +
  custom_theme() + labs(x = 'Sample Date', y = 'Bank Angle (deg)',
                        title = 'Bank Angle over Time: faceted by standard deviation',
                        subtitle = 'Fire Impact is % of fire in that watershed since 2011',
                        color = 'Fire Impact %',
                        caption = 'Reference Sites Only')

gr_95_ts %>%
  mutate(precip_cut = cut_interval(ave_annual_precip, 6)) %>%
  facet_func(dv = scaled_bf, color = sd_of_scale, precip_cut, scales = NULL)

gr_95_ts %>%
  mutate(precip_cut = cut_interval(shed_areakm2, 6)) %>%
  ggplot(aes(samp_date, bf, group = site_id, color = mgmt)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(group = site_id))  +
  #scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous')))+
  facet_wrap(forest~precip_cut, scales = 'free')

######## Plotting and EDA

bf_sd_ts %>% count(site_id, mgmt, sort = T)

bf_sd_ts %>% count(mgmt)

bf_sd_ts %>%
  ggplot(aes(mgmt, bf, mgmt, fill = mgmt)) +
  geom_boxplot() +
  labs(x = 'Groups', y = 'Bankfull Width Standard Deviation (ft)',
       fill = 'Groups',
       title = 'Comparing Reference vs Managed by Standard Deviation of Bankfull Widths')

performance::check_normality(lm(bf~mean_bf, data = bf_sd_ts))

bf_sd_ts %>%
  ggplot(aes(mean_pool_pc, mean_bf, color = mgmt)) +
  geom_point() + scale_x_log10()+
  geom_smooth(method = 'lm', se = F)

summary(lm(bf_sd_bc~mgmt*mean_bf, data = bf_sd_ts))

model <- lm(mean_bf~mgmt, data = bf_sd_ts)
summary(model)
car::qqPlot(model_ancova, distribution = 'norm')

plotly::plot_ly(car::qqPlot(model_ancova))
model_ancova <- lm(mean_bf~shed_areakm2+log(ave_annual_precip)+log(mean_pool_fr)+log(mean_pool_pc)+mgmt, data = bf_sd_ts %>% filter(mean_pool_fr>0) %>% .[-c(30,300,234,306,236,237,294),])
car::Anova(model_ancova, type = "III")

performance::check_normality(model_ancova)

gr_95_ts %>% group_by(site_id, mgmt) %>%
  summarise(bf_sd = sd(bf, na.rm = T),
            count = n()) %>% ungroup() %>%
  mutate(cut_sd = cut_number(bf_sd, n = 6)) %>%
  left_join(gr_95_ts %>% select(site_id, drain_density:pct_nfs, samp_date, bf,stream,
                                mgmt), by = c('site_id', 'mgmt')) %>%
  mutate(impact = if_else(mgmt == 'Reference', pct_mtbs_mod+pct_mtbs_high,
                          pct_high_harv_int+pct_mod_harv_int),
         fire_impact = pct_mtbs_mod+pct_mtbs_high) %>%
  filter(mgmt != 'Reference') %>%
  ggplot(aes(samp_date, bf,group = site_id, color = impact, label = stream)) +
  geom_point(alpha = 0.3) +
  geom_line() +
  scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous'))) +
  facet_wrap(~cut_sd) +
  labs(title = 'Managed Sites Only Faceted by SD of Bankfull Width',
       subtitle = 'colored by Impact and DV is Bankfull Width\nimpact is the addition of high and mod fire since 2011',
       x = 'Date', y = 'Bankfull Width (ft)')

gr_95_ts %>%
  mutate(cut_sd = cut_number(bf, n = 6)) %>%
  mutate(impact = if_else(mgmt == 'Reference', pct_mtbs_mod+pct_mtbs_high,
                          pct_high_harv_int+pct_mod_harv_int)) %>%
  filter(mgmt != 'Managed') %>% filter(impact >= 10) %>%
  ggplot(aes(samp_date, bf,group = site_id, color = impact, label = stream)) +
  geom_point(alpha = 0.3) +
  geom_line() +
  scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous'))) +
  facet_wrap(~cut_sd) +
  labs(title = 'Reference Sites Only Faceted by SD of Bankfull Width',
       subtitle = 'colored by Impact and DV is Bankfull Width\nimpact is the addition of high and mod fire since 2011',
       x = 'Date', y = 'Bankfull Width (ft)')

facet_func <- function(data, dv){
  data %>%
  group_by(site_id) %>%
  mutate(sd_var = scale({{dv}})) %>%
    ungroup() %>%
    mutate(cut_sd = cut_number(sd_var, n =6)) %>%
  mutate(impact = if_else(mgmt == 'Reference', pct_mtbs_mod+pct_mtbs_high,
                          pct_high_harv_int+pct_mod_harv_int)) %>%
  filter(mgmt == 'Managed') %>%
  ggplot(aes(samp_date, {{dv}}, group = site_id, color = impact)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(group = site_id)) +
  scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous'))) +
  facet_wrap(~cut_sd) +
  labs(x = 'Sample Date', color = 'Percent Harvest',
                                           title = 'Faceted by Standard deviations (ft)')
}

resourceviz::cairo_view()
facet_func(gr_95_ts, bf_ft)

gr_95_ts %>%
  group_by(site_id) %>%
  summarise(scaled_bf = sd(bf_ft)) %>%
  ungroup() %>%
  mutate(cut_sd = cut_number(scaled_bf, n =6)) %>%
  cbind(gr_95_ts %>% select(-site_id)) %>%
  mutate(impact = if_else(mgmt == 'Reference', pct_mtbs_mod+pct_mtbs_high,
                          pct_high_harv_int+pct_mod_harv_int)) %>%
  filter(mgmt == 'Managed') %>%
  ggplot(aes(samp_date, bf_ft, group = site_id, color = impact)) +
  geom_point(alpha = 0.3) +
  geom_line() +
  scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous'))) +
  facet_wrap(~cut_sd) +
  labs(x = 'Sample Date', color = 'Percent Harvest',
       title = 'Faceted by Standard deviations (ft)')

gr_95_ts %>%
  mutate(cut_sd = cut_number(bf, n = 6)) %>%
  mutate(impact = if_else(mgmt == 'Reference', pct_mtbs_mod+pct_mtbs_high,
                          pct_high_harv_int+pct_mod_harv_int)) %>%
  filter(mgmt != 'Managed') %>% filter(road_dens_km2 < 4) %>%
  ggplot(aes(samp_date, bf,group = site_id, color = impact, label = stream)) +
  geom_point(alpha = 0.3) +
  geom_line() +
  scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous'))) +
  facet_wrap(~cut_sd) +
  labs(title = 'Reference Sites Only Faceted by SD of Bankfull Width',
       subtitle = 'colored by Impact and DV is Bankfull Width\nimpact is the addition of high and mod fire since 2011',
       x = 'Date', y = 'Bankfull Width (ft)')

gr_95_ts %>% group_by(site_id, mgmt) %>%
  summarise(bf_sd = sd(bf, na.rm = T),
            count = n()) %>% ungroup() %>%
  mutate(cut_sd = cut_number(bf_sd, n = 6)) %>%
  left_join(gr_95_ts %>% select(site_id, drain_density:pct_nfs, samp_date, bf,stream,
                                mgmt), by = c('site_id', 'mgmt')) %>%
  mutate(impact = if_else(mgmt == 'Reference', pct_mtbs_mod+pct_mtbs_high,
                          pct_high_harv_int+pct_mod_harv_int))  %>%
  ggplot(aes(impact, fill = mgmt)) + geom_histogram(position = 'dodge')

performance::check_normality(lm(bf_sd_bc~shed_areakm2, data = bf_sd_ts))
summary(lm(bf_sd_bc~mgmt*shed_areakm2, data = bf_sd_ts))
effectsize::effectsize(lm(bf_sd_bc~mgmt+ave_annual_precip, data = bf_sd_ts))



pt <- gr_95_ts_everything %>% group_by(site_id, mgmt) %>%
  summarise(bf_sd = sd(bf, na.rm = T),
            count = n()) %>% ungroup() %>%
  mutate(cut_sd = cut_number(bf_sd, n = 6)) %>%
  left_join(gr_95_ts %>% select(site_id, drain_density:pct_nfs, samp_date, bf,stream,
                                mgmt), by = c('site_id', 'mgmt')) %>%
  mutate(impact = if_else(mgmt == 'Reference', pct_mtbs_mod+pct_mtbs_high,
                          pct_high_harv_int+pct_mod_harv_int)) %>%
  filter(mgmt != 'Managed') %>%
  ggplot(aes(samp_date, bf,group = site_id, color = impact, label = stream)) +
  geom_point(alpha = 0.3) +
  geom_line() +
  scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous'))) +
  facet_wrap(~cut_sd) +
  labs(title = 'Managed Sites Only Faceted by SD of Bankfull Width',
       subtitle = 'colored by Impact and DV is Bankfull Width\nimpact is the addition of high and mod fire since 2011',
       x = 'Date', y = 'Bankfull Width (ft)')

gr_95_ts_everything %>% group_by(site_id, mgmt) %>%
  summarise(bf_sd = sd(bf, na.rm = T),
            count = n()) %>% ungroup() %>%
  mutate(cut_sd = cut_number(bf_sd, n = 6)) %>%
  left_join(gr_95_ts %>% select(site_id, drain_density:pct_nfs, samp_date, bf,stream,
                                mgmt), by = c('site_id', 'mgmt')) %>%
  mutate(impact = if_else(mgmt == 'Reference', pct_mtbs_mod+pct_mtbs_high,
                          pct_high_harv_int+pct_mod_harv_int)) %>%
  filter(mgmt == 'Managed') %>%
  ggplot(aes(samp_date, bf,group = site_id, color = ave_annual_precip, label = stream)) +
  geom_point(alpha = 0.3) +
  geom_line() +
  scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous'))) +
  facet_wrap(~cut_sd) +
  labs(title = 'Managed Sites Only Faceted by SD of Bankfull Width',
       subtitle = 'colored by Impact and DV is Bankfull Width\nimpact is the addition of high and mod fire since 2011',
       x = 'Date', y = 'Bankfull Width (ft)')

library(plotly)

ggplotly(pt)
gr_95_ts %>% group_by(site_id, mgmt) %>%
  summarise(bf_sd = sd(bf, na.rm = T),
            count = n()) %>% ungroup() %>%
  mutate(cut_sd = cut_number(bf_sd, n = 6)) %>%
  add_count(mgmt,cut_sd) %>%
  left_join(gr_95_ts %>% select(site_id, drain_density:pct_nfs, samp_date, bf,stream,
                                mgmt), by = c('site_id', 'mgmt')) %>%
  mutate(impact = if_else(mgmt == 'Reference', pct_mtbs_mod+pct_mtbs_high,
                          pct_high_harv_int+pct_mod_harv_int)) %>%
  filter(mgmt %in% 'Managed') %>%
  ggplot(aes(samp_date, bf,group = site_id, color = impact, label = stream)) +
  geom_point(alpha = 0.3) +
  geom_line() +
  scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous'))) +
  facet_wrap(~cut_sd) +
  labs(title = 'Managed Sites Only Faceted by SD of Bankfull Width',
       subtitle = 'colored by Impact and DV is Bankfull Width\nimpact is the addition of high and mod fire since 2011',
       x = 'Date', y = 'Bankfull Width (ft)') + custom_theme()



gr_95_ts %>% group_by(site_id, mgmt) %>%
  summarise(bf_sd = sd(bf, na.rm = T),
            count = n()) %>% ungroup() %>%
  mutate(cut_sd = cut_number(bf_sd, n = 6)) %>%
  add_count(mgmt,cut_sd) %>%
  ggplot(aes(cut_sd, n, fill = mgmt)) +
  geom_col(position = 'dodge') +
  labs(x = 'Standard Deviation Bins (ft)',
       fill = 'Groups', title = 'Comparing Counts between groups within SD Bins')


gr_95_ts %>% group_by(site_id, mgmt) %>%
  summarise(bf_sd = sd(bf, na.rm = T),
            count = n()) %>% ungroup() %>%
  ggplot(aes(bf_sd, color = mgmt)) +
  geom_density()


gr_95_ts %>% group_by(site_id) %>% nest() %>%
  mutate(mk = map(data, safely(~Kendall::MannKendall(.$bf)))) %>%
  unnest()
mk_test <- gr_95_ts %>%
  split(.$site_id) %>%
  purrr::map(safely(~Kendall::MannKendall(.$bf))) %>%
  purrr::keep(~length(.) != 0) %>%
  purrr::map(~.x[['result']]) %>%
  map(~.x[['sl']]) %>%
  map(~data.frame(.)) %>%
  plyr::rbind.fill()
colnames(mk_test)[1] <- 'pvalue'
mk_site_id <- gr_95_ts %>%
  split(.$site_id) %>%
  purrr::map(safely(~Kendall::MannKendall(.$bf))) %>%
  purrr::keep(~length(.) != 0) %>%
  purrr::map(~.x[['result']]) %>%
  keep(~!is.null(.)) %>%
  names(.)

mk_test$site_id <- parse_number(mk_site_id)

mk_test %>%
  left_join(gr_95_ts %>% select(bf,site_id), by='site_id') %>%
  add_count(site_id) %>%
  group_by(site_id) %>%
  slice(n=1) %>% ungroup() %>%
  left_join(gr_95, by = 'site_id') %>%
  ggplot(aes(pvalue,fill = factor(n))) +
  geom_histogram(binwidth = 0.01) +
  geom_vline(xintercept = 0.05, size = 1, color = 'red') +
  labs(title = 'Mann-Kendall Test for Monotonic Trend',
       subtitle = 'p value < 0.05 is a significant trend\nobservations must have greater than 3 observations\nonly 4 had < 3')

gr_95_ts %>%
  count(site_id)


gr_95 %>%
  ggplot(aes(mean_wd_tran, fill = mgmt)) +
  geom_boxplot()

model <- aov(mean_bf_ft~precip_in+shed_areakm2+mean_pool_pc+mgmt, data = gr_95)

car::Anova(model, type = 'III')

summary(lm(mean_bf_area~ave_annual_precip, data = gr_95))

performance::check_normality(lm(mean_wd_tran~precip_in+shed_areami2+mean_pool_pc+mgmt, data = gr_95))
summary(lm(mean_wd_tran~precip_in+shed_areami2+mean_pool_pc+mgmt, data = gr_95))
ggplot(gr_95, aes(mean_pool_pc, mean_wd_tran, color = mgmt)) +
  geom_point() + geom_smooth(method = 'lm', se = F)
summary(lm(mean_wd_tran~mean_pool_pc*mgmt, data = gr_95))

min_date <- gr_95_ts %>%
  group_by(site_id) %>% slice_min(samp_date) %>% ungroup %>%
  mutate(dategroup = 'min')

  max_date <- gr_95_ts %>%
  group_by(site_id) %>% slice_max(samp_date) %>%  ungroup%>%
    mutate(dategroup2 = 'max') %>% rename_with(~paste0(.,'_max'),contains(c('bf', 'veg_stab', 'bank_angle', "d50")))

left_join(min_date, max_date %>% select(contains(c('bf_max', 'veg_stab_max', 'bank_angle_max', "d50_max")), site_id), by = 'site_id') %>%
  ggplot(aes(bf, bf_max, color = mgmt)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  labs()


cov_test <- left_join(min_date, max_date %>% select(contains(c('bf_max', 'veg_stab_max', 'bank_angle_max', "d50_max")), site_id), by = 'site_id')

m1 <- lm(bf_max~bf+mgmt, data = cov_test)
m2 <- lm(bf_max~bf*mgmt, data = cov_test)

anova(m1, m2)

thematic::thematic_on(font = 'Montserrat')

bf <- cov_test %>%
  filter(bf_max < 50) %>%
  ggplot(aes(bf, bf_max, color = mgmt)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  geom_abline(linetype = 2,show.legend = T) +
  scale_y_continuous(breaks = seq(0, 100, 20))+
  scale_x_continuous(breaks = seq(0, 100, 20))+
  labs(color = 'Group', x = 'First Bankfull Width Measurement (m)',
       y = 'Last Bankfull Width Measurement (m)')
bf


veg <- cov_test %>% ggplot(aes(veg_stab, veg_stab_max, color = mgmt)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  geom_abline(linetype = 2,show.legend = T) +
  labs(color = 'Group', x = 'First Vegetation Stability Measurement',
       y = 'Last Vegetation Stability Measurement')
veg

ba <- cov_test %>% ggplot(aes(bank_angle, bank_angle_max, color = mgmt)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  geom_abline(linetype = 2,show.legend = T) +
  scale_y_continuous(breaks = seq(0, 160, 20))+
  scale_x_continuous(breaks = seq(0, 160, 20))+
  labs(color = 'Group', x = 'First Bank Angle Measurement (deg)',
       y = 'Last Bank Angle Measurement (deg)')
ba

d50 <- cov_test %>% ggplot(aes(d50, d50_max, color = mgmt)) +
  geom_point() +
  geom_abline(linetype = 2,show.legend = T) +
  geom_smooth(method = 'lm', se = F) +
  scale_x_log10() +
  scale_y_log10() +
  labs(color = 'Group', x = 'First D50 Measurement',
       y = 'Last D50 Measurement')
d50

library(patchwork)

(bf|veg)/(ba|d50) + plot_layout(guides = 'collect') + plot_annotation(title = 'Comparing first measurement of PIBO survey with the most recent',
                                                                      subtitle = 'dotted line is a 1:1 fit')



bf <- cov_test %>%
  mutate(impact_harvest = pct_high_harv_int+pct_mod_harv_int,
         impact_fire = pct_mtbs_high+pct_mtbs_mod) %>%
  filter(mgmt == 'Managed', impact_harvest >= 20) %>%
  ggplot(aes(bf, bf_max, color = impact_harvest)) +
  geom_point(size = 3) +
  geom_smooth(method = 'lm', se = F) +
  scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous'))) +
  geom_abline(linetype = 2,show.legend = T) +
  scale_y_continuous(breaks = seq(0, 100, 20))+
  scale_x_continuous(breaks = seq(0, 100, 20))+
  labs(color = '% Harvest', x = 'First Bankfull Width Measurement (m)',
       y = 'Last Bankfull Width Measurement (m)')
bf


veg <-cov_test %>%
  mutate(impact_harvest = pct_high_harv_int+pct_mod_harv_int,
         impact_fire = pct_mtbs_high+pct_mtbs_mod) %>%
  filter(mgmt == 'Managed', impact_harvest >= 20) %>%
  ggplot(aes(veg_stab, veg_stab_max, color = impact_harvest)) +
  geom_point(size = 3) +
  geom_smooth(method = 'lm', se = F) +
  scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous'))) +
  geom_abline(linetype = 2,show.legend = T) +
  labs(color = '% Harvest', x = 'First Vegetation Stability Measurement',
       y = 'Last Vegetation Stability Measurement')
veg

ba <- cov_test  %>%
  mutate(impact_harvest = pct_high_harv_int+pct_mod_harv_int,
         impact_fire = pct_mtbs_high+pct_mtbs_mod) %>%
  filter(mgmt == 'Managed', impact_harvest >= 20) %>%
  ggplot(aes(bank_angle, bank_angle_max, color = impact_harvest)) +
  geom_point(size = 3) +
  geom_smooth(method = 'lm', se = F) +
  scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous'))) +
  geom_abline(linetype = 2,show.legend = T) +
  scale_y_continuous(breaks = seq(0, 160, 20))+
  scale_x_continuous(breaks = seq(0, 160, 20))+
  labs(color = '% Harvest', x = 'First Bank Angle Measurement (deg)',
       y = 'Last Bank Angle Measurement (deg)')
ba

d50 <- cov_test %>%
  mutate(impact_harvest = pct_high_harv_int+pct_mod_harv_int,
         impact_fire = pct_mtbs_high+pct_mtbs_mod) %>%
  filter(mgmt == 'Managed', impact_harvest >= 20) %>%
  ggplot(aes(d50, d50_max, color = impact_harvest)) +
  geom_point(size = 3) +
  geom_smooth(method = 'lm', se = F) +
  scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous'))) +
  geom_smooth(method = 'lm', se = F) +
  scale_x_log10() +
  scale_y_log10() +
  labs(color = '% Harvest', x = 'First D50 Measurement',
       y = 'Last D50 Measurement')

d50

library(patchwork)

(bf|veg)/(ba|d50) + plot_layout(guides = 'collect') + plot_annotation(title = 'Comparing first measurement of PIBO survey with the most recent.\nOnly including Manged areas greater than 20% Harvest',
                                                                      subtitle = 'dotted line is a 1:1 fit')



cov_test %>% ggplot(aes(bank_angle, bank_angle_max, color = mean_d50)) +
  geom_point(size = 3) +
  geom_smooth(method = 'lm', se = F) +
  geom_abline(linetype = 2,show.legend = T) +
  scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous'))) +
  scale_y_continuous(breaks = seq(0, 160, 20))+
  scale_x_continuous(breaks = seq(0, 160, 20))+
  labs(color = 'Watershed Area km2', x = 'First Bank Angle Measurement (deg)',
       y = 'Last Bank Angle Measurement (deg)')


cov_test %>%
  mutate(impact_harvest = pct_high_harv_int+pct_mod_harv_int,
         impact_fire = pct_mtbs_high+pct_mtbs_mod,
         fire_cut = cut(impact_fire,breaks = c(-Inf,10,20,Inf))) %>%
  filter(mgmt == 'Managed', impact_harvest >= 30) %>%
  ggplot(aes(bf, bf_max, color = impact_harvest)) +
  geom_point(size = 3) +
  geom_smooth(method = 'lm', se = F) +
  scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous'))) +
  geom_abline(linetype = 2,show.legend = T) +
  scale_y_continuous(breaks = seq(0, 100, 20))+
  scale_x_continuous(breaks = seq(0, 100, 20))+
  labs(color = '% Harvest', x = 'First Bankfull Width Measurement (m)',
       y = 'Last Bankfull Width Measurement (m)') +
  facet_wrap(~fire_cut)


bf <- cov_test %>%
  mutate(impact_harvest = pct_high_harv_int+pct_mod_harv_int,
         impact_fire = pct_mtbs_high+pct_mtbs_mod) %>%
  filter(mgmt == 'Reference', impact_fire >= 20) %>%
  ggplot(aes(bf, bf_max, color = impact_fire)) +
  geom_point(size = 3) +
  geom_smooth(method = 'lm', se = F) +
  scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous'))) +
  geom_abline(linetype = 2,show.legend = T) +
  scale_y_continuous(breaks = seq(0, 100, 20))+
  scale_x_continuous(breaks = seq(0, 100, 20))+
  labs(color = '% Harvest', x = 'First Bankfull Width Measurement (m)',
       y = 'Last Bankfull Width Measurement (m)')
bf


veg <-cov_test %>%
  mutate(impact_harvest = pct_high_harv_int+pct_mod_harv_int,
         impact_fire = pct_mtbs_high+pct_mtbs_mod) %>%
  filter(mgmt == 'Reference', impact_fire >= 20) %>%
  ggplot(aes(veg_stab, veg_stab_max, color = impact_fire)) +
  geom_point(size = 3) +
  geom_smooth(method = 'lm', se = F) +
  scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous'))) +
  geom_abline(linetype = 2,show.legend = T) +
  labs(color = '% Harvest', x = 'First Vegetation Stability Measurement',
       y = 'Last Vegetation Stability Measurement')
veg

ba <- cov_test  %>%
  mutate(impact_harvest = pct_high_harv_int+pct_mod_harv_int,
         impact_fire = pct_mtbs_high+pct_mtbs_mod) %>%
  filter(mgmt == 'Reference', impact_fire >= 20) %>%
  ggplot(aes(bank_angle, bank_angle_max, color = impact_fire)) +
  geom_point(size = 3) +
  geom_smooth(method = 'lm', se = F) +
  scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous'))) +
  geom_abline(linetype = 2,show.legend = T) +
  scale_y_continuous(breaks = seq(0, 160, 20))+
  scale_x_continuous(breaks = seq(0, 160, 20))+
  labs(color = '% Harvest', x = 'First Bank Angle Measurement (deg)',
       y = 'Last Bank Angle Measurement (deg)')
ba

d50 <- cov_test %>%
  mutate(impact_harvest = pct_high_harv_int+pct_mod_harv_int,
         impact_fire = pct_mtbs_high+pct_mtbs_mod) %>%
  filter(mgmt == 'Reference', impact_fire >= 20) %>%
  ggplot(aes(d50, d50_max, color = impact_fire)) +
  geom_point(size = 3) +
  geom_smooth(method = 'lm', se = F) +
  scale_color_gradientn(colors = rev(wesanderson::wes_palette('Darjeeling1', n = 3, type = 'continuous'))) +
  geom_smooth(method = 'lm', se = F) +
  scale_x_log10() +
  scale_y_log10() +
  labs(color = '% Harvest', x = 'First D50 Measurement',
       y = 'Last D50 Measurement')

d50

library(patchwork)

(bf|veg)/(ba|d50) + plot_layout(guides = 'collect') + plot_annotation(title = 'Comparing first measurement of PIBO survey with the most recent.\nOnly including Reference areas greater than 20% Fire Impact',
                                                                      subtitle = 'dotted line is a 1:1 fit')
