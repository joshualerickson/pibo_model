library(sf)
library(terra)
library(tidyverse)
library(GGally)


pibo_sites_corrected <- read_sf('pibo_sites_corrected_final.shp')
pibo_sites_corrected <- st_transform(pibo_sites_corrected, crs = terra::crs(cpg_rasters))
pb_vect <- vect(pibo_sites_corrected)
filelist <- list.files(path = "D:/documents/Project File/water_yield_paper/water_yield_R/rasters", pattern = '*\\.tif')

new_pibo_data <- data.frame(site_id = pibo_sites_corrected$site_id)

for(i in 1:length(filelist)){

  cpg_rasters <- rast(paste0(getwd(),'/rasters/',filelist[i]))
  pibo_sites_corrected <- st_transform(pibo_sites_corrected, crs = terra::crs(cpg_rasters))
  pb_vect <- vect(pibo_sites_corrected)
  extracted_data <- terra::extract(cpg_rasters, pb_vect) %>% select(2)

  new_pibo_data <- cbind.data.frame(new_pibo_data, extracted_data)
}

pibo_sites_corrected <- pibo_sites_corrected %>%
  mutate(utm1 = unlist(map(pibo_sites_corrected$geometry,1)),
         utm2 = unlist(map(pibo_sites_corrected$geometry,2)))

new_pibo_data <- new_pibo_data %>% left_join(pibo_sites_corrected, by = 'site_id') %>% st_as_sf()

new_pibo_data_df <- new_pibo_data %>% st_drop_geometry()
class(new_pibo_data_df) <- c("tbl_df", "tbl", "data.frame")
write_csv(new_pibo_data_df, 'pibo_ml.csv')
ggpairs(new_pibo_data_df %>% select(mean_bf,mgmt, 2:10),
        mapping = aes(color = mgmt), upper = list(continuous = wrap("cor", size = 6)),
        lower = list(continuous = wrap("smooth", method = "lm", se = F,
                                       alpha = 0.6)))

ggpairs(new_pibo_data_df %>% select(mean_bf,mgmt, 11:20),
        mapping = aes(color = mgmt), upper = list(continuous = wrap("cor", size = 6)),
        lower = list(continuous = wrap("smooth", method = "lm", se = F,
                                       alpha = 0.6)))


ggpairs(new_pibo_data_df %>% select(mean_bf,mgmt, 21:30),
        mapping = aes(color = mgmt), upper = list(continuous = wrap("cor", size = 6)),
        lower = list(continuous = wrap("smooth", method = "lm", se = F,
                                       alpha = 0.6)))
new_pibo_data_df$pc1 <- juice(pca_prep)['PC1'] %>% pull(PC1)

ggpairs(new_pibo_data_df %>% select(mean_bf,mgmt, fac_taudem_17all_int,
                                    slope_percent_int_CPG_all, streamslope_percent_int_CPG_all,
                                    pc1, `US_Precip_1981-2010_CPG_all`, `US_TMAX_1981-2010_int_CPG_all`),
        mapping = aes(color = mgmt), upper = list(continuous = wrap("cor", size = 6)),
        lower = list(continuous = wrap("smooth", method = "lm", se = F,
                                       alpha = 0.6)))


pca_juiced <- juice(pca_prep)
new_pibo_data_df$residuals <- mod$residuals
mod <- lm(mean_bf~log(fac_taudem_17all_int)+log(`US_Precip_1981-2010_CPG_all`)+pc1+mgmt,
          data = new_pibo_data_df %>% filter(residuals < 6))
summary(mod)

new_pibo_data_df %>%
  ggplot(aes(`US_Precip_1981-2010_CPG_all`, mean_bf, color = mgmt)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_x_log10()

new_pibo_data_df %>%
  ggplot(aes(pc1, mean_bf, color = mgmt)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_x_log10()

cor.test(new_pibo_data_df$fac_taudem_17all_int, new_pibo_data_df$`US_Precip_1981-2010_CPG_all`)
shapiro.test(mod$residuals)
car::qqPlot(mod)
