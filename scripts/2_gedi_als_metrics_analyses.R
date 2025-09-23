library(sf)
library(tidyverse)
library(Metrics)
library(dunn.test)

g = rbind(g_do, g_pn, g_rr)
f_species_metrics <- function(df) {
  df = df %>%
    mutate(bias_rh100_max = rh_100-max,
           bias_rh100_p99 = rh_100-p99,
           bias_rh100_p98 = rh_100-p98,
           bias_rh100_p95 = rh_100-p95,
           bias_rh99_max = rh_99-max,
           bias_rh99_p99 = rh_99-p99,
           bias_rh99_p98 = rh_99-p98,
           bias_rh99_p95 = rh_99-p95,
           bias_rh98_p99 = rh_98-p99,
           bias_rh98_p98 = rh_98-p98,
           bias_rh98_p95 = rh_98-p95,
           bias_rh95_p95 = rh_95-p95,
           bias_rh95_p90 = rh_95-p90,
           bias_rh90_p90 = rh_90-p90,
           GEDI_nmt = elev_lowestmode-diff_geo) %>% 
    mutate(acq_date = as.Date(acq_date, format = "%Y.%m.%d"),
           spec_age = as.numeric(spec_age),
           part_cd = as.numeric(part_cd),
           CANOPY_CV = case_when(ccv <= 0.1 ~ "0-10 CV%",
                                 ccv <= 0.2 ~ "10-20 CV%",
                                 ccv <= 0.3 ~ "20-30 CV%",
                                 ccv <= 0.4 ~ "30-40 CV%",
                                 ccv <= 0.5 ~ "40-50 CV%",
                                 ccv <= 0.6 ~ "50-60 CV%",
                                 ccv <= 0.7 ~ "60-70 CV%",
                                 ccv <= 0.8 ~ "70-80 CV%",
                                 ccv <= 0.9 ~ "80-90 CV%",
                                 ccv <= 1 ~ "90-100 CV%"),
           VCI = case_when(vcp <= 0.1 ~ "0-10 VCI%",
                           vcp <= 0.2 ~ "10-20 VCI%",
                           vcp <= 0.3 ~ "20-30 VCI%",
                           vcp <= 0.4 ~ "30-40 VCI%",
                           vcp <= 0.5 ~ "40-50 VCI%",
                           vcp <= 0.6 ~ "50-60 VCI%",
                           vcp <= 0.7 ~ "60-70 VCI%",
                           vcp <= 0.8 ~ "70-80 VCI%",
                           vcp <= 0.9 ~ "80-90 VCI%",
                           vcp <= 1 ~ "90-100 VCI%"),
           SILP_AGE = case_when(spec_age <= 20 ~ "1: 0-20",
                                spec_age <= 40 ~ "2: 20-40",
                                spec_age <= 60 ~ "3: 40-60",
                                spec_age <= 80 ~ "4: 60-80",
                                spec_age <= 100 ~ "5: 80-100",
                                spec_age <= 120 ~ "6: 100-120",
                                spec_age <= 300 ~ "7: 120+"),
           HGT_CLASS = case_when(p99 <= 5 ~ "1: 0-5 m",
                                 p99 <= 10 ~ "2: 5-10 m",
                                 p99 <= 15 ~ "3: 10-15 m",
                                 p99 <= 20 ~ "4: 15-20 m",
                                 p99 <= 25 ~ "5: 20-25 m",
                                 p99 <= 30 ~ "6: 25-30 m",
                                 p99 <= 35 ~ "7: 30-35 m",
                                 p99 <= 40 ~ "8: 35-40 m")) %>% 
    drop_na()
  return(df)
}
g = f_species_metrics(g)

do = st_read("so_poly_do_review.gpkg") #("D:/phd/GEDI/simulator/do/simulation/do_so_simZfix_col.gpkg")
pn = st_read("so_poly_pn_review.gpkg") #("D:/phd/GEDI/simulator/pn/simulation/pn_so_simZfix_col.gpkg")
rr = st_read("so_poly_rr_review.gpkg") #("D:/phd/GEDI/simulator/rr/simulation/rr_so_simZfix_col.gpkg")

g = rbind(do, pn, rr)

#dodać kreskę RMSE
g = g %>% 
  group_by(BEAM_TYPE)
beam_stats = g %>% 
  st_drop_geometry %>% 
  summarise(rmse = rmse(p98, rh_98),
            bias = mean(bias_rh98_p98),
            stddev = sd(bias_rh98_p98),
            n = n())
ggplot(data = g, aes(x = BEAM_TYPE, y = bias_rh98_p98, fill = BEAM_TYPE))+
  geom_hline(yintercept = 0, colour = "grey30")+
  geom_boxplot(outlier.alpha = 0.25)+
  ylim(-10, 10)+
  xlab("GEDI beam type")+
  ylab(expression('GEDI rh'[G]*'98 - p98 [m]'))+
  scale_fill_manual(values = c("#94bceb", "#2865ad"))+
  theme_light()+
  theme(legend.position = "none",
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 6))
ggsave("appendix_figure_2.tiff", plot = last_plot(), path = "C:/users/wojciech.krawczyk/OneDrive - Uniwersytet Rolniczy im. Hugona Kołłątaja w Krakowie/GEDI_accuracy/review_20240815/",
       units = "cm", width = 7, height = 9, dpi = 300, bg = "white")

g = g %>% 
  group_by(DAY_NIGHT)
solar_stats = g %>% 
  st_drop_geometry %>% 
  summarise(rmse = rmse(p98, rh_98),
            bias = mean(bias_rh98_p98),
            stddev = sd(bias_rh98_p98),
            n = n())
ggplot(data = g, aes(x = DAY_NIGHT, y = bias_rh98_p98, fill = DAY_NIGHT))+
  geom_hline(yintercept = 0, colour = "grey30")+
  geom_boxplot(outlier.alpha = 0.25)+
  ylim(-10, 10)+
  xlab("time of GEDI measurement acqusition")+
  ylab(expression('GEDI rh'[G]*'98 - p98 [m]'))+
  scale_fill_manual(values = c("#ffe596", "#142259"))+
  theme_light()+
  theme(legend.position = "none",
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 6))
ggsave("appendix_figure_3.tiff", plot = last_plot(), path = "C:/users/wojciech.krawczyk/OneDrive - Uniwersytet Rolniczy im. Hugona Kołłątaja w Krakowie/GEDI_accuracy/review_20240815/",
       units = "cm", width = 7, height = 9, dpi = 300, bg = "white")

g = g %>% 
  group_by(SVTY_CLASS)
svty_stats = g %>% 
  st_drop_geometry %>% 
  summarise(rmse = rmse(p98, rh_98),
            bias = mean(bias_rh98_p98),
            stddev = sd(bias_rh98_p98),
            n = n())
ggplot(data = g, aes(x = SVTY_CLASS, y = bias_rh98_p98, fill = SVTY_CLASS))+
  geom_hline(yintercept = 0, colour = "grey30")+
  geom_boxplot(outlier.alpha = 0.25)+
  ylim(-10, 10)+
  xlab("GEDI sensitivity class")+
  ylab(expression('GEDI rh'[G]*'98 - p98 [m]'))+
  scale_fill_brewer(palette = "Greys")+
  theme_light()+
  theme(legend.position = "none",
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 6))
ggsave("appendix_figure_1.tiff", plot = last_plot(), path = "C:/users/wojciech.krawczyk/OneDrive - Uniwersytet Rolniczy im. Hugona Kołłątaja w Krakowie/GEDI_accuracy/review_20240815/",
       units = "cm", width = 10, height = 9, dpi = 300, bg = "white")

ggplot(g, aes(x = sensitivity, y = bias_rh98_p98))+
  geom_hline(yintercept = 0)+
  geom_point(alpha = 0.15)+
  xlim(0.9, 1)+
  ylim(-10, 10)+
  xlab("GEDI sensitivity")+
  ylab(expression('GEDI rh'[G]*'98 - p98 [m]'))+
  theme_light()+
  theme(legend.position = "none",
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 6))
ggsave("appendix_figure_1_1.tiff", plot = last_plot(), path = "C:/users/wojciech.krawczyk/OneDrive - Uniwersytet Rolniczy im. Hugona Kołłątaja w Krakowie/GEDI_accuracy/review_20240815/",
       units = "cm", width = 10, height = 9, dpi = 300, bg = "white")

svty_kw = dunn.test(g$bias_rh98_p98, g$SVTY_CLASS, kw = TRUE)
svty_kw

solar_kw = kruskal.test(bias_rh98_p98 ~ DAY_NIGHT, data = g)
solar_kw

beam_kw = kruskal.test(bias_rh98_p98 ~ BEAM_TYPE, data = g)
beam_kw

hgt_svty = g %>%
  st_drop_geometry() %>% 
  group_by(HGT_CLASS, SVTY_CLASS) %>% 
  summarise(n = n())
hgt_solar = g %>%
  st_drop_geometry() %>% 
  group_by(HGT_CLASS, DAY_NIGHT) %>% 
  summarise(n = n())
hgt_beam = g %>%
  st_drop_geometry() %>% 
  group_by(HGT_CLASS, BEAM_TYPE) %>% 
  summarise(n = n())

age_svty = g %>%
  st_drop_geometry() %>% 
  group_by(SILP_AGE, SVTY_CLASS) %>% 
  summarise(n = n())
age_solar = g %>%
  st_drop_geometry() %>% 
  group_by(SILP_AGE, DAY_NIGHT) %>% 
  summarise(n = n())
age_beam = g %>%
  st_drop_geometry() %>% 
  group_by(SILP_AGE, BEAM_TYPE) %>% 
  summarise(n = n())

ccv_svty = g %>%
  st_drop_geometry() %>% 
  group_by(CANOPY_CV, SVTY_CLASS) %>% 
  summarise(n = n())
ccv_solar = g %>%
  st_drop_geometry() %>% 
  group_by(CANOPY_CV, DAY_NIGHT) %>% 
  summarise(n = n())
ccv_beam = g %>%
  st_drop_geometry() %>% 
  group_by(CANOPY_CV, BEAM_TYPE) %>% 
  summarise(n = n())

vci_svty = g %>%
  st_drop_geometry() %>% 
  group_by(VCI, SVTY_CLASS) %>% 
  summarise(n = n())
vci_solar = g %>%
  st_drop_geometry() %>% 
  group_by(VCI, DAY_NIGHT) %>% 
  summarise(n = n())
vci_beam = g %>%
  st_drop_geometry() %>% 
  group_by(VCI, BEAM_TYPE) %>% 
  summarise(n = n())