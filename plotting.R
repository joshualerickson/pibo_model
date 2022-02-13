### plotting
library(patchwork)
library(tidyverse)
library(GGally)
library(ggtext)
library(sasLM)
library(janitor)
library(resourceviz)
gr_95_new <- read_csv('R1WestPIBO102621.csv') %>%
  clean_names()

gr_95 <- read_csv('SummaryStatsKendall_gr_95.csv')[,1:36] %>%
  na.omit() %>%
  clean_names() %>%
  right_join(gr_95_new %>% select(site_id), by = 'site_id')


set.seed(13466)
gr_95m <- slice_sample(gr_95 %>% filter(mgmt == 'Managed'), n = 122)

gr_95 <- rbind(gr_95m, gr_95 %>% filter(mgmt == 'Reference'))
gr_95 <- gr_95 %>%  mutate(mean_bf_bc = bcPower(mean_bf, 0.42),
                           mean_wd_tran_bc = bcPower(mean_wd_tran, 0.42),
                           shed_round = round(shed_areakm2, 0),
                           shed_area_floor = (shed_round-shed_round %% 10),
                           shed_area_floor_50 = (shed_round-shed_round %% 50))

resourceviz::cairo_view()
thematic::thematic_on(font = 'Montserrat')

ggplot(gr_95, aes(x = mgmt, y = mean_bf)) +
  ggdist::stat_halfeye(
    adjust = .5,
    width = .6,
    .width = 0,
    justification = -.3,
    point_colour = NA, aes(fill = mgmt)) +
  geom_boxplot(
    width = .25,
    outlier.shape = NA, size = .75,
    aes(color = mgmt)
  ) +
  geom_point(
    size = 3.3,
    alpha = .43,
    position = position_jitter(
      seed = 1, width = .1
    ), aes(color = mgmt)
  ) +
  custom_theme() +
  coord_cartesian(xlim = c(1.2, NA), clip = "off") +
  labs(y = 'Mean Bankfull Width (m)',
       x = 'Type',
       title = 'Raincloud plots of Mean Bankfull Width split by Managed vs. Reference') + theme(legend.position = 'none')

p1 <- gr_95 %>% filter(!is.na(mean_stab), !is.na(mgmt))%>%
  ggplot(aes(mgmt, mean_stab, fill = mgmt)) +
  geom_boxplot(show.legend = F)  +
  theme_bw() + labs(subtitle = paste0('Mean Bank Stability<br>**p-value = ', p_adjust[4], '**'),
                    y = 'Mean Bank Stability (%)', x = '')+
  cowplot::theme_minimal_hgrid(12, rel_small = 1)+
  theme(
    plot.subtitle = element_markdown())

p2 <- gr_95 %>% filter(!is.na(mean_bf), !is.na(mgmt))%>%
  mutate(mean_bf = mean_bf)%>%
  ggplot(aes(mgmt, mean_bf, fill = mgmt))+
  geom_boxplot(show.legend = F)  +
  theme_bw() + labs(subtitle = paste0('Mean Bankfull Width<br>**p-value = ', p_adjust[2],'**'),
                    y = 'Mean Bankfull Width (m)', x = '')+
  cowplot::theme_minimal_hgrid(12, rel_small = 1)+
  theme(
    plot.subtitle = element_markdown())

p3 <- gr_95 %>% filter(!is.na(mean_wd_tran), !is.na(mgmt))%>%
  ggplot(aes(mgmt, mean_wd_tran, fill = mgmt)) +
  geom_boxplot(show.legend = F) +
  theme_bw() + labs(subtitle = paste0('Mean Width to Depth (WD) at Transects<br>**p-value = ', p_adjust[1], '**'),
                    y = 'Mean WD (ratio)', x = '')+
  cowplot::theme_minimal_hgrid(12, rel_small = 1)+
  theme(
    plot.subtitle = element_markdown())

p4 <- gr_95 %>% filter(!is.na(mean_bank_an), !is.na(mgmt))%>%
  ggplot(aes(mgmt, mean_bank_an, fill = mgmt)) +
  geom_boxplot(show.legend = F) +
  theme_bw() + labs(subtitle = paste0('Mean Bank Angle <br>**p-value =', p_adjust[3], '**'),
                    y = 'Mean Bank Angle (%)', x = '')+
  cowplot::theme_minimal_hgrid(12, rel_small = 1) +
  theme(
    plot.subtitle = element_markdown())

(p1|p2)/(p3|p4) +
plot_annotation(
  title = 'Multiple Comparison p value results (Benjamini & Hochberg (1995))',
  caption = paste('Disclaimer: assuming iid')
)

p1 <- gr_95 %>% filter(!is.na(mean_stab), !is.na(mgmt))%>%
  ggplot(aes( mean_stab, color = mgmt)) +
  geom_density(show.legend = T, alpha = 0.75)  +
  geom_rug() +
  theme_bw() + labs(subtitle = paste0('Mean Bank Stability<br>**p-value = ', p_adjust[4], '**'),
                    y = 'Mean Bank Stability (%)', x = '')+
  cowplot::theme_minimal_hgrid(12, rel_small = 1)+
  theme(
    plot.subtitle = element_markdown())

p2 <- gr_95 %>% filter(!is.na(mean_bf), !is.na(mgmt))%>%
  ggplot(aes(mean_bf, color = mgmt))+
  geom_density(show.legend = T, alpha = 0.75)  +
  geom_rug() +
  theme_bw() + labs(subtitle = paste0('Mean Bankfull Width<br>**p-value = ', p_adjust[2],'**'),
                    y = 'Mean Bankfull Width (m)', x = '')+
  cowplot::theme_minimal_hgrid(12, rel_small = 1)+
  theme(
    plot.subtitle = element_markdown())

p3 <- gr_95 %>% filter(!is.na(mean_wd_tran), !is.na(mgmt))%>%
  ggplot(aes(mean_wd_tran, color = mgmt)) +
  geom_density(show.legend = T, alpha = 0.75) +
  geom_rug() +
  theme_bw() + labs(subtitle = paste0('Mean Width to Depth (WD) at Transects<br>**p-value = ', p_adjust[1], '**'),
                    y = 'Mean WD (ratio)', x = '')+
  cowplot::theme_minimal_hgrid(12, rel_small = 1)+
  theme(
    plot.subtitle = element_markdown())

p4 <- gr_95 %>% filter(!is.na(mean_bank_an), !is.na(mgmt))%>%
  ggplot(aes(mean_bank_an, color = mgmt)) +
  geom_density(show.legend = T, alpha = 0.75) +
  geom_rug() +
  theme_bw() + labs(subtitle = paste0('Mean Bank Angle <br>**p-value =', p_adjust[3], '**'),
                    y = 'Mean Bank Angle (%)', x = '')+
  cowplot::theme_minimal_hgrid(12, rel_small = 1) +
  theme(
    plot.subtitle = element_markdown())

(p1|p2)/(p3|p4) + plot_layout(guides = 'collect') +
  plot_annotation(
    title = 'Multiple Comparison p value results (Benjamini & Hochberg (1995))',
    caption = paste('Disclaimer: assuming iid'))


GGally::ggpairs(t %>% select(mean_bf,drain_density:harv_intensit_med, mgmt),mapping = aes(color = mgmt),
                lower = list(continuous = wrap("smooth", method = "lm", se = F,
                                               alpha = 0.6)))

GGally::ggpairs(t %>% select(mean_bf,harv_intensit_l:pct_mtbs_high, mgmt),mapping = aes(color = mgmt),
                lower = list(continuous = wrap("smooth", method = "lm", se = F,
                                               alpha = 0.6)))

GGally::ggpairs(t %>% filter(!is.na(mgmt)) %>%
                  select(`Mean Bankfull Width` = mean_bf,
                         `Avg Annual Precip` = ave_annual_precip,
                         `Watershed Area (km^2)` = shed_areakm2,
                         Management = mgmt),mapping = aes(color = Management),
                lower = list(continuous = wrap("smooth", method = "lm", se = F,
                                               alpha = 0.6))
               ) + labs(title = 'Mean Bankfull Width with Highest Covariates',
                        subtitle = 'Ave Annual Precip and Watershed Area (km^2)') +
  cowplot::theme_minimal_hgrid(12, rel_small = 1,font_size = 8)

GGally::ggpairs(t %>% filter(!is.na(mgmt)) %>%
                  select(`Mean WD` = mean_wd_tran,
                         `Avg Annual Precip` = ave_annual_precip,
                         `Watershed Area (km^2)` = shed_areakm2,
                         Management = mgmt),mapping = aes(color = Management),
                lower = list(continuous = wrap("smooth", method = "lm", se = F,
                                               alpha = 0.6))
) + labs(title = 'Mean Bankfull Width with Highest Covariates',
         subtitle = 'Ave Annual Precip and Watershed Area (km^2)') +
  cowplot::theme_minimal_hgrid(12, rel_small = 1,font_size = 8)

GGally::ggpairs(gr_95 %>% select(mean_wd_tran,drain_density:harv_intensit_med, mgmt),mapping = aes(color = mgmt),
                lower = list(continuous = wrap("smooth", method = "lm", se = F,
                                               alpha = 0.6)))

GGally::ggpairs(gr_95 %>% select(mean_wd_tran,harv_intensit_l:pct_mtbs_high, mgmt),mapping = aes(color = mgmt),
                lower = list(continuous = wrap("smooth", method = "lm", se = F,
                                               alpha = 0.6)))

GGally::ggpairs(gr_95 %>% filter(!is.na(mgmt)) %>%
                  select(`Mean W/D` = mean_wd_tran,
                         `Avg Annual Precip` = ave_annual_precip,
                         `Watershed Area (km^2)` = shed_areakm2,
                         Management = mgmt),mapping = aes(color = Management),
                lower = list(continuous = wrap("smooth", method = "lm", se = F,
                                               alpha = 0.6))
) + labs(title = 'Mean W/D with Highest Covariates',
         subtitle = 'Ave Annual Precip and Watershed Area (km^2)') +
  cowplot::theme_minimal_hgrid(12, rel_small = 1,font_size = 8)
GGally::ggpairs(gr_95 %>% filter(!is.na(mgmt)) %>%
                  select(`Mean lw Freq` = mean_lw_frq,
                         `Avg Annual Precip` = ave_annual_precip,
                         `Watershed Area (km^2)` = shed_areakm2,
                         Management = mgmt),mapping = aes(color = Management),
                lower = list(continuous = wrap("smooth", method = "lm", se = F,
                                               alpha = 0.6))
) + labs(title = 'Mean W/D with Highest Covariates',
         subtitle = 'Ave Annual Precip and Watershed Area (km^2)') +
  cowplot::theme_minimal_hgrid(12, rel_small = 1,font_size = 8)

library(car)

fit_wd <- aov(mean_wd_tran~mgmt+ave_annual_precip+shed_areakm2,
              data = gr_95 %>% filter(!is.na(mgmt),
                                      !is.na(mean_bf),
                                      !is.na(ave_annual_precip),
                                      !is.na(shed_areakm2)) )
Anova(fit_wd, type = 'III')

fit_ran <- aov(mean_bf~mgmt+ave_annual_precip+shed_areakm2,
              data = fire_harv)
Anova(fit_ran, type = 'III')

fire_harv %>% ggplot(aes(ave_annual_precip, mean_bf, color = mgmt)) +
  geom_point() + geom_smooth(method = 'lm', se = F)

sasLM::GLM(mean_bf~shed_areakm2+ave_annual_precip+mgmt,
           fire_harv)
sasLM::GLM(mean_wd_tran~ave_annual_precip+shed_areakm2+mgmt,
           gr_95 %>% filter(!is.na(mgmt),
                            !is.na(mean_bf),
                            !is.na(ave_annual_precip),
                            !is.na(shed_areakm2)))

#histograms

histos <- function(data,cov, iv, bw, position){

  ggplot(data = data, aes(.data[[cov]], fill = .data[[iv]])) +
    geom_histogram(binwidth = bw, position = position) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8)) +
    labs(subtitle = paste0('binwidth = ', bw))

}
set.seed(13466)
gr_95m <- gr_95 %>% filter(ave_annual_precip >= 800,
                                        mgmt == 'Managed')

gr_95 <- rbind(gr_95m, gr_95 %>% filter(mgmt == 'Reference'))
dsamp <- gr_95 %>% mutate(all_harvest = cut_interval(ave_annual_precip,n=5),
                          mgmt = factor(mgmt)) %>%
  group_by(precip_cut) %>%
  do(caret::downSample(., .$mgmt)) %>% ungroup()
histos(gr_95 %>% filter(mgmt=='Managed', all_harvest>10),
       'all_harvest', 'mgmt', bw = 1, position = 'dodge') +
  labs(x = 'Average Annual Precipitation (mm)',
       fill = 'Group',
       title = 'Histogram of Average Annual Precipitation\nPRISM 800m; 1981-2010 ')

histos(gr_95, 'mean_pool_fr', 'mgmt', bw = 5, position = 'dodge') +
  labs(x = "Pools/km",
       fill = 'Group',
       title = 'Histogram of Mean Pool Frequency')


library(rstatix)
  library(ggpubr)
  ggscatter(
    gr_95,
    x = 'mean_pt_fine', y = 'mean_bank_an',
    color = 'mgmt', add = "reg.line"
  )+
    stat_regline_equation(
      aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"),
          color = mgmt)
    ) +
    labs(x = 'Mean Percent Fines (fines < 6mm)',
         y = "Mean Bank Angle",
         color = 'Group')

  GGally::ggpairs(gr_95 %>%
                    select(mean_wd_tran,ave_annual_precip,
                           shed_areakm2, mean_wd_tran, mean_bank_an, mean_grad,
                           mean_pool_dp, mean_pool_fr,
                           mean_d50, mean_pool_pc, mean_pt_fine,mgmt),
                  mapping = aes(color = mgmt, alpha = 0.2),
                  upper = list(continuous = wrap("cor", size = 2)),
                  lower = list(continuous = wrap("smooth",
                                                 method = "lm",
                                                 se = F,
                                                 alpha = 0.6)))
car::vif(lm(mean_bf~ave_annual_precip+
            shed_areakm2+mean_wd_tran+ mean_bank_an+ mean_grad+
            mean_pool_dp+mean_pool_fr+
            mean_d50+ mean_pool_pc+mean_pt_fine, data = gr_95))
