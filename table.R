library(gt)
library(tidyverse)
resourceviz::cairo_view()
tc <- read_csv('table_cov.csv')

#set up boxplots


tct <- gr_95 %>%
  mutate(total_fire = pct_mtbs_mod+pct_mtbs_mod,
         total_harvest = pct_high_harv_int+pct_mod_harv_int) %>%
  select(shed_areakm2,
                        ave_annual_precip,
                        ave_ann_cwd,
                        ave_basin_elev,
                        drain_density,
                        grad,
                        mean_d50,
                        road_dens_km2,
                        pct_high_harv_int,
                        pct_mod_harv_int,
                        total_harvest,
                        pct_mod_harv_int,
                        pct_mtbs_high,
                        pct_mtbs_mod,
                        total_fire,
                        mgmt) %>%
  pivot_longer(-mgmt) %>%
  group_by(name) %>%
  nest() %>%
  mutate(plot = map(data,
                    function(df) df %>%
                      ggplot(aes(mgmt, value)) +
                      geom_boxplot(
                        width = .25,
                        outlier.shape = NA,
                        aes(fill = mgmt), show.legend = F
                      ) +
                      labs(fill = '', x = '', y = '') +
                      theme_void())) %>%
  select(-data)


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






