library(gt)
library(tidyverse)
resourceviz::cairo_view()
tc <- read_csv('table_cov.csv')
source('utils.R')
#set up boxplots

glimpse(gr_95)
tab <- tc %>%
  mutate(across(where(is.numeric), round, 1)) %>%
  group_by(type, mgmt) %>%
  pivot_wider(names_from = mgmt, values_from = c("Mean", "SD")) %>%
  ungroup() %>% mutate(ggplot = NA) %>%
  gt(rowname_col = 'covariates') %>%
  tab_header(
    title = "Potential Covariates for Reference and Managed Sites",
    subtitle = "with associated summary statistics"
  ) %>%
  tab_row_group(label = md("**_Management and Fire_**"),
                rows = type %in% 'Management and Fire' ) %>%
  tab_row_group(label = md('**_Geoclimatic Variables_**'),
                rows = type %in% 'Geoclimatic Variables' )  %>%
  cols_hide('type') %>%
  # gt::tab_style(
  #   style = list(
  #     gt::cell_fill(color = "green", alpha = 0.2)
  #   ),
  #   locations = gt::cells_body(
  #     columns = c(`Mean_Reference (n=116)`,`SD_Reference (n=116)`)
  #   )
  # )%>%
  # gt::tab_style(
  #   style = list(
  #     gt::cell_fill(color = "orange", alpha = 0.5)
  #   ),
  #   locations = gt::cells_body(
  #     columns = c(`Mean_Managed (n=189)`,`SD_Managed (n=189)`)
  #   )
  # )%>%
  cols_label(
    `Mean_Reference (n=116)` = md("<span style='color:green'>*Reference*</span>"),
    `Mean_Managed (n=189)` = md("<span style='color:#FFAF42'>*Managed* </span>"),
    `SD_Reference (n=116)` = md("<span style='color:green'>*Reference* </span>"),
    `SD_Managed (n=189)` = md("<span style='color:#FFAF42'>*Managed*</span>")
  ) %>%
  opt_align_table_header(align = 'center') %>%
  tab_spanner(
    label = md("**Potential Covariates**"),
    columns = c(1)
  ) %>%
  tab_spanner(
    label = md("**Mean**"),
    columns = c(2:4)
  )%>%
  tab_spanner(
    label = md("**Standard Deviation**"),
    columns = c(5:6)
  ) %>%
  tab_source_note(
    source_note = md('**Table 2.**')
    ) %>%
  text_transform(
    locations = cells_body(ggplot),
    fn = function(x) {
      map(tct$plot, ggplot_image, height = px(50))
    }
  ) %>%
  cols_label(
    ggplot = ''
  )

tab %>% gtsave('table_plot.png')


mt_id <- USAboundaries::us_states(states = c('Montana', 'Idaho', 'Washington'))

northwest <- USAboundaries::us_states(states = c('Montana', 'Idaho', 'Washington',
                                                 'Wyoming', 'Oregon'))
bbox <- mapedit::drawFeatures(sf = pibo_sites)
mt_id <- mt_id %>% st_crop(bbox)
admin_units <- read_sf("T:/DataCenter/Citrix/Home01/joshualerickson/My Documents/ArcGIS/Default.gdb", layer = 'Export_Output_18')
admin_units <- admin_units %>%
  mutate(
    FORESTNAME = factor(FORESTNAME),
    FORESTNAME = fct_relevel(FORESTNAME, c('Idaho Panhandle National Forests', "Kootenai National Forest", "Flathead National Forest", "Nez Perce-Clearwater National Forest",
                                           "Bitterroot National Forest",
                                           "Lolo National Forest", "Beaverhead-Deerlodge National Forest","Helena-Lewis and Clark National Forest", "Custer Gallatin National Forest", "Dakota Prairie Grasslands")))
adu <- admin_units %>%
  filter(FORESTNAME %in% c('Idaho Panhandle National Forests', "Kootenai National Forest", "Flathead National Forest", "Nez Perce-Clearwater National Forest",
                           "Bitterroot National Forest",
                           "Lolo National Forest","Beaverhead-Deerlodge National Forest"))
library(mapsf)
library(sf)
library(tidyverse)

pibo_sites <- pibo_sites %>% st_transform(crs = 4326)

adu <- adu %>% st_make_valid() %>% st_transform(crs = 4326) %>% st_crop(mt_id)
adu <- adu %>% st_transform(32612)
mt_id <- mt_id %>% st_transform(crs = 4326)
northwest <- northwest %>% st_transform(4326)

nw_map <- plyr::rbind.fill(northwest, st_cast(bbox,'MULTIPOLYGON')) %>% st_as_sf()

nw_map %>% ggplot() + geom_sf()

mf_init(pibo_sites, theme = 'brutal')

mf_map(mt_id, col = NA, lwd= 2)



mf_shadow(adu,
          add = TRUE)

mf_map(adu,
       add = TRUE)

mf_map(pibo_sites,var = 'mgmt',
       type = 'typo',
       add = TRUE,
       pch = 16,
       cex = 1,
       pal = c('#00BA38', 'orange'),
       leg_pos = c(-119.15, 49),
       leg_title = 'PIBO Sites')

mf_inset_on(nw_map, pos = "bottomleft",cex = .275)

mf_map(nw_map, col = NA, lwd= 2)

mf_inset_off()
mf_layout(title = "PIBO Sites in the Study Area",
          credits = 'Figure 2.',
          arrow = F,
          scale = F,
          frame = F)

mf_arrow(pos = 'bottomright')


mf_export(x = mtq, filename = "fixed_width.png", width = 500)

tc %>% pivot_longer(everything()) %>% view()

gr_95 %>%
  select(mgmt, c(drain_density:pct_mtbs_high,aug_et_2000_2015_cpg_all:us_tmax_1981_2010_int_cpg_all)) %>%
  pivot_longer(-mgmt) %>% pull(name) %>% unique() %>% data.frame(longnames = .) %>% mutate(rowid = row_number()) %>%
  dplyr::filter(!rowid %in% c(3,5:16)) %>%
  cbind(tc %>% pivot_longer(everything()) %>% select(value) %>% na.omit()) %>%
  write_csv('col_names.csv')

name_change <- read_csv('col_names.csv')

name_change <- name_change %>% tibble()


var_names <- deframe(name_change)

library(ggh4x)

facet_covs <- gr_95 %>%
  select(mgmt, c(drain_density:pct_mtbs_high,aug_et_2000_2015_cpg_all:us_tmax_1981_2010_int_cpg_all)) %>%
  pivot_longer(-mgmt) %>% left_join(name_change, by = c('name')) %>% na.omit() %>%
  left_join(tc %>% pivot_longer(everything()), by = c('clean_name' = 'value')) %>%
  mutate(name.y = ifelse(clean_name %in% c('FA Mean Annual Precipitation'), paste('Hydroclimatic'), name.y),
         name.y = ifelse(clean_name %in% c('FA Temperature Max'), paste('Hydroclimatic'), name.y))

facet_covs %>%
  ggplot(aes(mgmt, value, fill = mgmt)) +
  stat_summary(fun.data = calc_boxplot_stat, geom="boxplot") +
  facet_nested_wrap(~ name.y+clean_name, ncol = 5,
                    nest_line = element_line(linetype = 1, size = 1.25),
                    scales = 'free',
                    axes = "all", remove_labels = "x",
                    strip  = strip_nested(bleed = TRUE))+
  scale_y_continuous(labels = scales::comma) +
  custom_theme()+
  theme(
        strip.background = element_blank(),
        #strip.text = element_text(size = 18),
        ggh4x.facet.nestline = element_line(colour = c("black")),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x = '', y = '', fill = '')



