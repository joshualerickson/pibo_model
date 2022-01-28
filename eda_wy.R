library(tidyverse)
library(janitor)
library(car)
library(evaluate)
thematic::thematic_on(font = 'Montserrat')
gr_95 <- read_csv('SummaryStatsKendall_gr_95.csv')[,1:36] %>%
  na.omit() %>%
  clean_names()

resourceviz::cairo_view()

set.seed(13466)
gr_95m <- slice_sample(gr_95 %>% filter(mgmt == 'Managed'), n = 122)

gr_95 <- rbind(gr_95m, gr_95 %>% filter(mgmt == 'Reference'))

gr_95 <- gr_95 %>%  mutate(mean_bf_bc = bcPower(mean_bf, 0.42),
                                       mean_wd_tran_bc = bcPower(mean_wd_tran, 0.42),
                                       shed_round = round(shed_areakm2, 0),
                                       shed_area_floor = (shed_round-shed_round %% 10),
                                       shed_area_floor_50 = (shed_round-shed_round %% 50))


t_wd_tran <- t.test(mean_wd_tran~mgmt, data = gr_95 %>% filter(!is.na(mean_wd_tran), !is.na(mgmt)))
t_mean_bf <- t.test(mean_bf~mgmt, data = gr_95 %>% filter(!is.na(mean_bf), !is.na(mgmt)))
t_mean_bank_an<-t.test(mean_bank_an ~mgmt, data = gr_95 %>% filter(!is.na(mean_bank_an), !is.na(mgmt)))
t_mean_stab <- t.test(mean_stab ~mgmt, data = gr_95 %>% filter(!is.na(mean_stab), !is.na(mgmt)))

p <- c(t_wd_tran$p.value,t_mean_bf$p.value, t_mean_bank_an$p.value,t_mean_stab$p.value)

p_adjust <- round(p.adjust(p, "BH"), 3)

gr_95 %>% filter(!is.na(mean_wd_tran), !is.na(mgmt))%>%
  ggplot(aes(mean_bf, fill = mgmt)) +
  geom_density() +
  geom_rug(aes(color = mgmt), size = 2) +
  theme_bw()

gr_95 %>% filter(!is.na(mgmt)) %>%
  ggplot(aes(shed_areakm2,fill = mgmt)) +
  geom_histogram(position = 'dodge')

gr_95 %>% filter(!is.na(mgmt)) %>%
  ggplot(aes(ave_annual_precip,fill = mgmt)) +
  geom_histogram(position = 'dodge')

  m <- lm(mean_bf~mgmt, data = gr_95)
  summary(m)
qqPlot(m)
performance::check_normality(m)

leveneTest(m)
