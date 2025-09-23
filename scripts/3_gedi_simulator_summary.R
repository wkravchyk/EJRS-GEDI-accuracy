library(sf)
library(tidyverse)
library(here)
library(yardstick)

do_db = st_read("D:/phd/GEDI/simulator/do/simulation/do_so_sim_review.gpkg") %>% 
  st_drop_geometry()
do_percentyle = st_read("percentyle_do_so_review.gpkg") %>% 
  st_drop_geometry()
do_db = left_join(do_percentyle, do_db, join_by(shot_number_x == shot_number_x))
pn_db = st_read("D:/phd/GEDI/simulator/pn/simulation/pn_so_sim_review.gpkg") %>% 
  st_drop_geometry()
pn_percentyle = st_read("percentyle_pn_so_review.gpkg") %>% 
  st_drop_geometry()
pn_db = left_join(pn_percentyle, pn_db, join_by(shot_number_x == shot_number_x))
rr_db = st_read("D:/phd/GEDI/simulator/rr/simulation/rr_so_sim_review.gpkg") %>% 
  st_drop_geometry()
rr_percentyle = st_read("percentyle_rr_so_review.gpkg") %>% 
  st_drop_geometry()
rr_db = left_join(rr_percentyle, rr_db, join_by(shot_number_x == shot_number_x))

do = do_db %>% 
  select(BEAM:rh_100.x, sensitivity.x:rumple_idx.x, SLOPE_CL.x:RMPL_IDX.x, p100:p010,
         rhGauss_10, rhGauss_20, rhGauss_25, rhGauss_30, rhGauss_40, rhGauss_50, rhGauss_60, rhGauss_70, rhGauss_75, rhGauss_80, rhGauss_90:rhGauss_100, 
         rhMax_10, rhMax_20, rhMax_25, rhMax_30, rhMax_40, rhMax_50, rhMax_60, rhMax_70, rhMax_75, rhMax_80, rhMax_90:rhMax_100, 
         rhInfl_10, rhInfl_20, rhInfl_25, rhInfl_30, rhInfl_40, rhInfl_50, rhInfl_60, rhInfl_70, rhInfl_75, rhInfl_80, rhInfl_90:rhInfl_100, area.x) %>% 
  rename_with(~ gsub("[.]x$", "", .x))
pn = pn_db %>% 
  select(BEAM:rh_100.x, sensitivity.x:rumple_idx.x, SLOPE_CL.x:RMPL_IDX.x, p100:p010,
         rhGauss_10, rhGauss_20, rhGauss_25, rhGauss_30, rhGauss_40, rhGauss_50, rhGauss_60, rhGauss_70, rhGauss_75, rhGauss_80, rhGauss_90:rhGauss_100, 
         rhMax_10, rhMax_20, rhMax_25, rhMax_30, rhMax_40, rhMax_50, rhMax_60, rhMax_70, rhMax_75, rhMax_80, rhMax_90:rhMax_100, 
         rhInfl_10, rhInfl_20, rhInfl_25, rhInfl_30, rhInfl_40, rhInfl_50, rhInfl_60, rhInfl_70, rhInfl_75, rhInfl_80, rhInfl_90:rhInfl_100, area.x) %>% 
  rename_with(~ gsub("[.]x$", "", .x))
rr = rr_db %>% 
  select(BEAM:rh_100.x, sensitivity.x:rumple_idx.x, SLOPE_CL.x:RMPL_IDX.x, p100:p010,
         rhGauss_10, rhGauss_20, rhGauss_25, rhGauss_30, rhGauss_40, rhGauss_50, rhGauss_60, rhGauss_70, rhGauss_75, rhGauss_80, rhGauss_90:rhGauss_100, 
         rhMax_10, rhMax_20, rhMax_25, rhMax_30, rhMax_40, rhMax_50, rhMax_60, rhMax_70, rhMax_75, rhMax_80, rhMax_90:rhMax_100, 
         rhInfl_10, rhInfl_20, rhInfl_25, rhInfl_30, rhInfl_40, rhInfl_50, rhInfl_60, rhInfl_70, rhInfl_75, rhInfl_80, rhInfl_90:rhInfl_100, area.x) %>% 
  rename_with(~ gsub("[.]x$", "", .x))

w = rbind(do, pn, rr) %>% 
  relocate(p100, .after = p099) %>% 
  relocate(p098, .before = p099) %>%
  relocate(p097, .before = p098) %>%
  relocate(p096, .before = p097) %>%
  relocate(p095, .before = p096) %>%
  relocate(p094, .before = p095) %>%
  relocate(p093, .before = p094) %>%
  relocate(p092, .before = p093) %>%
  relocate(p091, .before = p092) %>%
  relocate(p090, .before = p091) %>% 
  relocate(p080, .before = p090) %>% 
  relocate(p075, .before = p080) %>% 
  relocate(p070, .before = p075) %>%
  relocate(p060, .before = p070) %>%
  relocate(p050, .before = p060) %>%
  relocate(p040, .before = p050) %>%
  relocate(p030, .before = p040) %>%
  relocate(p025, .before = p030) %>%
  relocate(p020, .before = p025) %>%
  relocate(p010, .before = p020) %>% 
  drop_na()
do = w %>% 
  filter(area == "Odra Valley")
pn = w %>% 
  filter(area == "Niepołomice Forest")
rr = w %>% 
  filter(area == "Rudy Raciborskie")

scplt = ggplot(data = w, mapping = aes(x = rhGauss_60, y = rh_60, color = area))+
  geom_abline()+
  geom_point(alpha = 0.2)+
  xlim(0,30)+
  ylim(0,30)+
  facet_grid(rows = vars(area))
scplt

w_stat = w %>% 
  group_by(VCI) %>% 
  summarise(n_samples = n())
w = w %>% 
  filter(p099 >= 5) %>% 
  filter(vcp >= 0.4) %>% 
  filter(vcp <= 0.9)

#zmien parametr .fns w funkcji across aby obliczyc rozne statystyki
#mozliwe porownania: vs ALS: list(p100 = ~.-p100, p099 = ~.-p099, p098 = ~.-p098, p097 = ~.-p097, p096 = ~.-p096, p095 = ~.-p095, p094 = ~.-p094, p093 = ~.-p093, p092 = ~.-p092, p091 = ~.-p091, p090 = ~.-p090))) 
#                    vs sim GEDImax: list(p100 = ~.-rhMax_100, p099 = ~.-rhMax_99, p098 = ~.-rhMax_98, p097 = ~.-rhMax_97, p096 = ~.-rhMax_96, p095 = ~.-rhMax_95, p094 = ~.-rhMax_94, p093 = ~.-rhMax_93, p092 = ~.-rhMax_92, p091 = ~.-rhMax_91, p090 = ~.-rhMax_90)))
#                    vs sim GEDIinfl: list(p100 = ~.-rhInfl_100, p099 = ~.-rhInfl_99, p098 = ~.-rhInfl_98, p097 = ~.-rhInfl_97, p096 = ~.-rhInfl_96, p095 = ~.-rhInfl_95, p094 = ~.-rhInfl_94, p093 = ~.-rhInfl_93, p092 = ~.-rhInfl_92, p091 = ~.-rhInfl_91, p090 = ~.-rhInfl_90)))
#                    vs sim GEDIgauss: list(p100 = ~.-rhGauss_100, p099 = ~.-rhGauss_99, p098 = ~.-rhGauss_98, p097 = ~.-rhGauss_97, p096 = ~.-rhGauss_96, p095 = ~.-rhGauss_95, p094 = ~.-rhGauss_94, p093 = ~.-rhGauss_93, p092 = ~.-rhGauss_92, p091 = ~.-rhGauss_91, p090 = ~.-rhGauss_90)))
grouping = as.name("CANOPY_CV")

w_dif_ALS = w %>% 
  mutate(across(.cols = c(rh_100:rh_90),
                .fns = list(p100 = ~.-p100, p099 = ~.-p099, p098 = ~.-p098, p097 = ~.-p097, p096 = ~.-p096, p095 = ~.-p095, p094 = ~.-p094, p093 = ~.-p093, p092 = ~.-p092, p091 = ~.-p091, p090 = ~.-p090))) %>% 
  group_by({{ grouping }}) %>% 
  drop_na()
w_dif_SIM = w %>% 
  mutate(across(.cols = c(rh_100:rh_90),
                .fns = list(p100 = ~.-rhGauss_100, p099 = ~.-rhGauss_99, p098 = ~.-rhGauss_98, p097 = ~.-rhGauss_97, p096 = ~.-rhGauss_96, p095 = ~.-rhGauss_95, p094 = ~.-rhGauss_94, p093 = ~.-rhGauss_93, p092 = ~.-rhGauss_92, p091 = ~.-rhGauss_91, p090 = ~.-rhGauss_90))) %>% 
  group_by({{ grouping }}) %>% 
  drop_na()


f_bias_table = function(w_dif) {
  w_dif_mean_max = w_dif %>%
    st_drop_geometry() %>%
    summarise(across(ends_with("_p100"), .fns = mean))
  w_dif_mean_p99 = w_dif %>%
    st_drop_geometry() %>%
    summarise(across(ends_with("_p099"), .fns = mean))
  w_dif_mean_p98 = w_dif %>%
    st_drop_geometry() %>%
    summarise(across(ends_with("_p098"), .fns = mean))
  w_dif_mean_p97 = w_dif %>%
    st_drop_geometry() %>%
    summarise(across(ends_with("_p097"), .fns = mean))
  w_dif_mean_p96 = w_dif %>%
    st_drop_geometry() %>%
    summarise(across(ends_with("_p096"), .fns = mean))
  w_dif_mean_p95 = w_dif %>%
    st_drop_geometry() %>%
    summarise(across(ends_with("_p095"), .fns = mean))
  w_dif_mean_p94 = w_dif %>%
    st_drop_geometry() %>%
    summarise(across(ends_with("_p094"), .fns = mean))
  w_dif_mean_p93 = w_dif %>%
    st_drop_geometry() %>%
    summarise(across(ends_with("_p093"), .fns = mean))
  w_dif_mean_p92 = w_dif %>%
    st_drop_geometry() %>%
    summarise(across(ends_with("_p092"), .fns = mean))
  w_dif_mean_p91 = w_dif %>%
    st_drop_geometry() %>%
    summarise(across(ends_with("_p091"), .fns = mean))
  w_dif_mean_p90 = w_dif %>%
    st_drop_geometry() %>%
    summarise(across(ends_with("_p090"), .fns = mean))
   
  if (dim(w_dif_mean_max)[1] != 1) {
    w_dif_mean_max = w_dif_mean_max %>%
    pivot_longer(cols = !1, names_to = "stats", values_to = "p100")
  } else {
    w_dif_mean_max = w_dif_mean_max %>%
    pivot_longer(cols = colnames(.), names_to = "stats", values_to = "p100")
  }
  if (dim(w_dif_mean_p99)[1] != 1) {
    w_dif_mean_p99 = w_dif_mean_p99 %>% 
      pivot_longer(cols = !1, values_to = "p099") %>% 
      select(-name, -grouping)
  } else {
    w_dif_mean_p99 = w_dif_mean_p99 %>% 
      pivot_longer(cols = colnames(.), values_to = "p099") %>% 
      select(-name)
  }
  if (dim(w_dif_mean_p98)[1] != 1) {
    w_dif_mean_p98 = w_dif_mean_p98 %>% 
      pivot_longer(cols = !1, values_to = "p098") %>% 
      select(-name, -grouping)
  } else {
    w_dif_mean_p98 = w_dif_mean_p98 %>% 
      pivot_longer(cols = colnames(.), values_to = "p098") %>% 
      select(-name)
  }
  if (dim(w_dif_mean_p97)[1] != 1) {
    w_dif_mean_p97 = w_dif_mean_p97 %>% 
      pivot_longer(cols = !1, values_to = "p097") %>% 
      select(-name, -grouping)
  } else {
    w_dif_mean_p97 = w_dif_mean_p97 %>% 
      pivot_longer(cols = colnames(.), values_to = "p097") %>% 
      select(-name)
  }
  if (dim(w_dif_mean_p96)[1] != 1) {
    w_dif_mean_p96 = w_dif_mean_p96 %>% 
      pivot_longer(cols = !1, values_to = "p096") %>% 
      select(-name, -grouping)
  } else {
    w_dif_mean_p96 = w_dif_mean_p96 %>% 
      pivot_longer(cols = colnames(.), values_to = "p096") %>% 
      select(-name)
  }
  if (dim(w_dif_mean_p95)[1] != 1) {
    w_dif_mean_p95 = w_dif_mean_p95 %>% 
      pivot_longer(cols = !1, values_to = "p095") %>% 
      select(-name, -grouping)
  } else {
    w_dif_mean_p95 = w_dif_mean_p95 %>% 
      pivot_longer(cols = colnames(.), values_to = "p095") %>% 
      select(-name)
  }
  if (dim(w_dif_mean_p94)[1] != 1) {
    w_dif_mean_p94 = w_dif_mean_p94 %>% 
      pivot_longer(cols = !1, values_to = "p094") %>% 
      select(-name, -grouping)
  } else {
    w_dif_mean_p94 = w_dif_mean_p94 %>% 
      pivot_longer(cols = colnames(.), values_to = "p094") %>% 
      select(-name)
  }
  if (dim(w_dif_mean_p93)[1] != 1) {
    w_dif_mean_p93 = w_dif_mean_p93 %>% 
      pivot_longer(cols = !1, values_to = "p093") %>% 
      select(-name, -grouping)
  } else {
    w_dif_mean_p93 = w_dif_mean_p93 %>% 
      pivot_longer(cols = colnames(.), values_to = "p093") %>% 
      select(-name)
  }
  if (dim(w_dif_mean_p92)[1] != 1) {
    w_dif_mean_p92 = w_dif_mean_p92 %>% 
      pivot_longer(cols = !1, values_to = "p092") %>% 
      select(-name, -grouping)
  } else {
    w_dif_mean_p92 = w_dif_mean_p92 %>% 
      pivot_longer(cols = colnames(.), values_to = "p092") %>% 
      select(-name)
  }
  if (dim(w_dif_mean_p91)[1] != 1) {
    w_dif_mean_p91 = w_dif_mean_p91 %>% 
      pivot_longer(cols = !1, values_to = "p091") %>% 
      select(-name, -grouping)
  } else {
    w_dif_mean_p91 = w_dif_mean_p91 %>% 
      pivot_longer(cols = colnames(.), values_to = "p091") %>% 
      select(-name)
  }
  if (dim(w_dif_mean_p90)[1] != 1) {
    w_dif_mean_p90 = w_dif_mean_p90 %>% 
      pivot_longer(cols = !1, values_to = "p090") %>% 
      select(-name, -grouping)
  } else {
    w_dif_mean_p90 = w_dif_mean_p90 %>% 
      pivot_longer(cols = colnames(.), values_to = "p090") %>% 
      select(-name)
  }

  w_dif_bias = cbind(w_dif_mean_max, w_dif_mean_p99, w_dif_mean_p98, w_dif_mean_p97, 
                     w_dif_mean_p96, w_dif_mean_p95, w_dif_mean_p94, w_dif_mean_p93, 
                     w_dif_mean_p92, w_dif_mean_p91, w_dif_mean_p90)
  return(w_dif_bias)
}
w_dif_ALS_bias = f_bias_table(w_dif_ALS)
w_dif_SIM_bias = f_bias_table(w_dif_SIM)
#write.table(w_dif_bias, "G:/new/pn_bias_ALS_GEDI.csv", col.names = T, row.names = F, sep = ",")


#zmien ostatni zestaw pozycji kolumn aby obliczyc rozne statystyki
#mozliwe porownania vs ALS: 155:165
#                   vs sim GEDImax: 175:188
#                   vs sim GEDIinfl: 197:207
#                   vs sim GEDIgauss: 176:186
#zmien pierwszy zestaw pozycji kolumn aby obliczyc rozne statystki
#dla porownania percentyli 90-100: 105:115

#grouping = as.name("area")
GEDIrh_cols_list = list(rh100 = 11, rh099 = 10, rh098 = 9, rh097 = 8, rh096 = 7, rh095 = 6, rh094 = 5, rh093 = 4, rh092 = 3, rh091 = 2, rh090 = 1)
cols_ALS = 155:165
cols_SIM = 176:186

f_rmse_table = function(w, sel_columns, grouping = NULL) {
  if (is.null(grouping)) {
    tmp = w %>% 
      select(105:115, all_of(sel_columns))
  } else {
    tmp = w %>% 
      group_by( {{ grouping }}) %>% 
      select(105:115, all_of(sel_columns), {{ grouping }})
  }
  w_dif_rmse_max = tmp %>% 
    st_drop_geometry() %>% 
    lapply(GEDIrh_cols_list, rmse, data = ., truth = 22)
  w_dif_rmse_max = as.data.frame(w_dif_rmse_max) %>% 
    select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
  w_dif_rmse_p99 = tmp %>% 
    st_drop_geometry() %>% 
    lapply(GEDIrh_cols_list, rmse, data = ., truth = 21)
  w_dif_rmse_p99 = as.data.frame(w_dif_rmse_p99) %>% 
    select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
  w_dif_rmse_p98 = tmp %>% 
    st_drop_geometry() %>% 
    lapply(GEDIrh_cols_list, rmse, data = ., truth = 20)
  w_dif_rmse_p98 = as.data.frame(w_dif_rmse_p98) %>% 
    select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
  w_dif_rmse_p97 = tmp %>% 
    st_drop_geometry() %>% 
    lapply(GEDIrh_cols_list, rmse, data = ., truth = 19)
  w_dif_rmse_p97 = as.data.frame(w_dif_rmse_p97) %>% 
    select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
  w_dif_rmse_p96 = tmp %>% 
    st_drop_geometry() %>% 
    lapply(GEDIrh_cols_list, rmse, data = ., truth = 18)
  w_dif_rmse_p96 = as.data.frame(w_dif_rmse_p96) %>% 
    select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
  w_dif_rmse_p95 = tmp %>% 
    st_drop_geometry() %>% 
    lapply(GEDIrh_cols_list, rmse, data = ., truth = 17)
  w_dif_rmse_p95 = as.data.frame(w_dif_rmse_p95) %>% 
    select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
  w_dif_rmse_p94 = tmp %>% 
    st_drop_geometry() %>% 
    lapply(GEDIrh_cols_list, rmse, data = ., truth = 16)
  w_dif_rmse_p94 = as.data.frame(w_dif_rmse_p94) %>% 
    select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
  w_dif_rmse_p93 = tmp %>% 
    st_drop_geometry() %>% 
    lapply(GEDIrh_cols_list, rmse, data = ., truth = 15)
  w_dif_rmse_p93 = as.data.frame(w_dif_rmse_p93) %>% 
    select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
  w_dif_rmse_p92 = tmp %>% 
    st_drop_geometry() %>% 
    lapply(GEDIrh_cols_list, rmse, data = ., truth = 14)
  w_dif_rmse_p92 = as.data.frame(w_dif_rmse_p92) %>% 
    select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
  w_dif_rmse_p91 = tmp %>% 
    st_drop_geometry() %>% 
    lapply(GEDIrh_cols_list, rmse, data = ., truth = 13)
  w_dif_rmse_p91 = as.data.frame(w_dif_rmse_p91) %>% 
    select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
  w_dif_rmse_p90 = tmp %>% 
    st_drop_geometry() %>% 
    lapply(GEDIrh_cols_list, rmse, data = ., truth = 12)
  w_dif_rmse_p90 = as.data.frame(w_dif_rmse_p90) %>% 
    select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
  
  w_dif_rmse_max = w_dif_rmse_max %>% 
    pivot_longer(cols = ends_with(".estimate"), values_to = "p100", names_to = "stat")
  w_dif_rmse_p99 = w_dif_rmse_p99 %>% 
    pivot_longer(cols = ends_with(".estimate"), values_to = "p099") %>% 
    select(p099)
  w_dif_rmse_p98 = w_dif_rmse_p98 %>% 
    pivot_longer(cols = ends_with(".estimate"), values_to = "p098") %>% 
    select(p098)
  w_dif_rmse_p97 = w_dif_rmse_p97 %>% 
    pivot_longer(cols = ends_with(".estimate"), values_to = "p097") %>% 
    select(p097)
  w_dif_rmse_p96 = w_dif_rmse_p96 %>% 
    pivot_longer(cols = ends_with(".estimate"), values_to = "p096") %>% 
    select(p096)
  w_dif_rmse_p95 = w_dif_rmse_p95 %>% 
    pivot_longer(cols = ends_with(".estimate"), values_to = "p095") %>% 
    select(p095)
  w_dif_rmse_p94 = w_dif_rmse_p94 %>% 
    pivot_longer(cols = ends_with(".estimate"), values_to = "p094") %>% 
    select(p094)
  w_dif_rmse_p93 = w_dif_rmse_p93 %>% 
    pivot_longer(cols = ends_with(".estimate"), values_to = "p093") %>% 
    select(p093)
  w_dif_rmse_p92 = w_dif_rmse_p92 %>% 
    pivot_longer(cols = ends_with(".estimate"), values_to = "p092") %>% 
    select(p092)
  w_dif_rmse_p91 = w_dif_rmse_p91 %>% 
    pivot_longer(cols = ends_with(".estimate"), values_to = "p091") %>% 
    select(p091)
  w_dif_rmse_p90 = w_dif_rmse_p90 %>% 
    pivot_longer(cols = ends_with(".estimate"), values_to = "p090") %>% 
    select(p090)
  w_rmse = cbind(w_dif_rmse_max, w_dif_rmse_p99, w_dif_rmse_p98, w_dif_rmse_p97, 
                 w_dif_rmse_p96, w_dif_rmse_p95, w_dif_rmse_p94, w_dif_rmse_p93,
                 w_dif_rmse_p92, w_dif_rmse_p91, w_dif_rmse_p90)

  return(w_rmse)
}
w_ALS_rmse = f_rmse_table(w, sel_columns = cols_ALS, grouping = grouping)
ALS_rmse_cols = colnames(w_ALS_rmse)
ALS_rmse_cols[1] = as.character(grouping)
colnames(w_ALS_rmse) = ALS_rmse_cols
w_SIM_rmse = f_rmse_table(w, sel_columns = cols_SIM, grouping = grouping)
SIM_rmse_cols = colnames(w_SIM_rmse)
SIM_rmse_cols[1] = as.character(grouping)
colnames(w_SIM_rmse) = SIM_rmse_cols
#write.table(w_rmse, "G:/new/all_rmse_simGEDIGauss_GEDI.csv", col.names = T, row.names = F, sep = ",")

### %RMSE ###
f_PCTrmse_table = function(w, rmse_table, sel_columns, grouping = NULL) {
  if (is.null(grouping)) {
    tmp = w %>% 
      select(105:115, all_of(sel_columns))
    mean_observed_value = tmp %>% 
      st_drop_geometry() %>% 
      lapply(., mean)
    mean_observed_value = mean_observed_value[12:22] %>% 
      rev()
    stat = select(rmse_table, 1)
    rmse_table = select(rmse_table, -1)
    w_PCTrmse = mapply(FUN = "/", rmse_table, mean_observed_value) %>% 
      as.data.frame()
    w_PCTrmse = cbind(stat, w_PCTrmse)
  } else {
    tmp = w %>% 
      group_by( {{ grouping }}) %>%
      select(105:115, all_of(sel_columns), {{ grouping }})
    mean_observed_value = tmp %>% 
      st_drop_geometry() %>% 
      summarise(across(12:22, mean)) %>% 
      slice(rep(1:n(), each = 11)) %>% 
      select(rev(colnames(.)))
    fstrws = select(rmse_table, 1,2)
    w_PCTrmse = mapply(FUN = "/", select(rmse_table, 3:13), select(mean_observed_value, 1:11)) %>% 
      as.data.frame()
    w_PCTrmse = cbind(fstrws, w_PCTrmse)
  }
  return(w_PCTrmse)
}
w_ALS_PCTrmse = f_PCTrmse_table(w, rmse_table = w_ALS_rmse, sel_columns = cols_ALS, grouping = grouping)
w_SIM_PCTrmse = f_PCTrmse_table(w, rmse_table = w_SIM_rmse, sel_columns = cols_SIM, grouping = grouping)
###  ---  ###

#wykresy porównawcze dla tych samych percentyli
#BIAS
#ALS: p
#simGEDIGauss: rhGauss_

bias_ten_sam_ALS = w %>%
  drop_na() %>% 
  rename(p0100 = p100) %>% 
  mutate(across(matches("^p\\d"),
                ~. -pick(sub("p0", "rh_", cur_column())), 
                .names = "{sub('p', 'diff', col)}")) %>%
  mutate(across(230:250, ~  as.numeric(unlist(.)))) %>%
  rename(p100 = p0100) %>%
  group_by(get(grouping)) %>% 
  dplyr::summarise(across(matches("^diff\\d"), mean)) %>% 
  select(-4, -10, -(13:21)) %>%  #-3, -9, -(12:20)
  pivot_longer(cols = 2:11, names_to = "rh", values_to = "value")#1:10

bias_ten_sam_SIM = w %>%
  drop_na() %>% 
  rename(p0100 = p100) %>% 
  mutate(across(matches("^rhGauss_\\d"),
                ~. -pick(sub("rhGauss_", "rh_", cur_column())), 
                .names = "{sub('rhGauss_', 'diff', col)}")) %>%
  mutate(across(230:250, ~  as.numeric(unlist(.)))) %>%
  rename(p100 = p0100) %>%
  st_drop_geometry() %>% 
  group_by(get(grouping)) %>% 
  dplyr::summarise(across(matches("^diff\\d"), mean)) %>% 
  select(-4, -10, -(13:21)) %>%  #-3, -9, -(12:20)
  pivot_longer(cols = 2:11, names_to = "rh", values_to = "value")#1:10


bias_ten_sam = cbind(bias_ten_sam_ALS, bias_ten_sam_SIM$value) %>% 
  rename(ALS = value, SIM = `bias_ten_sam_SIM$value`) %>% 
  pivot_longer(cols = 3:4, names_to = "stat") %>% #2:3
  mutate(rh = str_replace(rh, "diff0100", "diff100")) %>% 
  mutate(rh = str_replace(rh, "diff", "p")) %>% 
  mutate(error = "Bias")

#RMSE
#ALS: p
#simGEDIGauss: rhGauss_

rmse_ten_sam_ALS = w %>%
  drop_na() %>% 
  rename(p0100 = p100) %>% 
  st_drop_geometry() %>%
  group_by(get(grouping)) %>% 
  summarise(across(matches("^p\\d"),
                   ~ Metrics::rmse(as.numeric(unlist(pick(sub("p0", "rh_", cur_column())))), as.numeric(unlist(pick(cur_column())))), 
                   .names = "{sub('p', 'rmse', col)}")) %>% 
  rename(rmse100 = rmse0100) %>% 
  select(-4, -10, -(13:21)) %>%  #-3, -9, -(12:20)
  pivot_longer(cols = 2:11, names_to = "rh", values_to = "value")#1:10

rmse_ten_sam_SIM = w %>%
  drop_na() %>% 
  rename(p0100 = p100) %>% 
  st_drop_geometry() %>%
  group_by(get(grouping)) %>% 
  summarise(across(matches("^rhGauss_\\d"),
                   ~ Metrics::rmse(as.numeric(unlist(pick(sub("rhGauss_", "rh_", cur_column())))), as.numeric(unlist(pick(cur_column())))), 
                   .names = "{sub('rhGauss_', 'rmse', col)}")) %>% 
  select(-4, -10, -(13:21)) %>%  #-3, -9, -(12:20)
  pivot_longer(cols = 2:11, names_to = "rh", values_to = "value")#1:10

rmse_ten_sam = cbind(rmse_ten_sam_ALS, rmse_ten_sam_SIM$value) %>% 
  rename(ALS = value, SIM = `rmse_ten_sam_SIM$value`) %>% 
  pivot_longer(cols = 3:4, names_to = "stat") %>% #2:3
  mutate(rh = str_replace(rh, "rmse", "p")) %>% 
  mutate(error = "RMSE")
test = rbind(bias_ten_sam, rmse_ten_sam) %>% 
  mutate(staterror = paste0(stat, error))

percentile_names = c(p090 = expression("rh"[G]*"90"), p010 = expression("rh"[G]*"10"), p020 = expression("rh"[G]*"20"), p030 = expression("rh"[G]*"30"), p040 = expression("rh"[G]*"40"), p050 = expression("rh"[G]*"50"), p060 = expression("rh"[G]*"60"), p070 = expression("rh"[G]*"70"), p080 = expression("rh"[G]*"80"), p100 = expression("rh"[G]*"100"))
##wykres liniowy dla bias i rmse - porównanie tych samych percentylii
ggplot(test, aes(x = rh, y = value, group = staterror))+
  geom_hline(yintercept = 0)+
  geom_point(aes(colour = staterror, shape = staterror), size = 1.5, alpha = 0.8)+
  geom_line(aes(colour = staterror,), linewidth = 0.5, alpha = 0.8)+
  scale_x_discrete(labels = percentile_names)+
  scale_colour_manual(labels = c("Bias -\nALS p", "RMSE -\nALS p", expression('Bias -\nGEDI rh'[SW]), expression('RMSE -\nGEDI rh'[SW])), 
                      values = c("#fdb138", "#009E73", "#648FFF", "#F599CC"))+
  scale_shape_manual(labels = c("Bias -\nALS p", "RMSE -\nALS p", expression('Bias -\nGEDI rh'[SW]), expression('RMSE -\nGEDI rh'[SW])),
                     values = c(1, 16, 2, 17))+
  coord_flip()+
  facet_grid(cols = vars(`get(grouping)`))+
  labs(x = expression('GEDI rh'[G]*'metric'), y = "Error value [m]",
       colour = "Error -\nreference:", shape = "Error -\nreference:")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 7),
        axis.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.text.align = 0, legend.key.size = unit(0.75, "cm"),
        legend.key = element_rect(color = NA, fill = NA),
        panel.grid.major = element_line(linewidth = 0.25), panel.grid.minor = element_line(linewidth = 0.1), strip.text = element_text(size = 8), strip.background = element_rect(colour = "white", fill = "#f2f2f0")
        )
#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8), legend.text.align = 0)

ggsave("fig8d_review.tiff", plot = last_plot(), path = "C:/Users/wojci/OneDrive - Uniwersytet Rolniczy im. Hugona Kołłątaja w Krakowie/GEDI_accuracy/review_20240528/analizy_review/",
       dpi = 300, units = "cm", width = 20, height = 8, bg = "white") #width = 8, height = 8


#### WYKRESY - PORÓWNANIE PERCENTYLI p90-p100 ####
#wykresy kolumnowe bez stratyfikacji
w_dif_bias_long = pivot_longer(w_dif_ALS_bias, cols = 2:12, names_to = "truth", values_to = "bias") %>% 
  mutate(across("stats", ~str_replace(., "_p100", ""))) %>% 
  mutate(across("stats", ~str_replace(., "rh_", "rh"))) %>% 
  rename(estimate = stats)
w_rmse_long = pivot_longer(w_ALS_rmse, cols = 2:12, names_to = "truth", values_to = "rmse")
w_bias_rmse = cbind(w_dif_bias_long, w_rmse_long) %>%
  select(-(4:5)) %>% 
  pivot_longer(cols = 3:4, names_to = "error", values_to = "value") %>% 
  mutate(estimate = str_replace(estimate, "rh", "\"rh\"[G]*\"")) %>% 
  mutate(estimate = paste0(estimate, "\""))
w_PCTrmse_long = pivot_longer(w_SIM_PCTrmse, cols = 2:12, names_to = "truth", values_to = "PCTrmse") %>% 
  mutate(stat = str_replace(stat, "rh0", "rh")) %>% 
  mutate(stat = str_replace(stat, "rh", "\"rh\"[G]*\"")) %>% 
  mutate(stat = str_replace(stat, "..estimate", "\"")) %>% 
  rename(estimate = stat, value = PCTrmse) %>% 
  mutate(error = "%RMSE", .before = value) %>% 
  mutate(across(where(is.numeric), ~.*100))
w_PCTrmse_long$estimate = factor(w_PCTrmse_long$estimate, levels = c("\"rh\"[G]*\"90\"", "\"rh\"[G]*\"91\"", "\"rh\"[G]*\"92\"", "\"rh\"[G]*\"93\"", "\"rh\"[G]*\"94\"", "\"rh\"[G]*\"95\"", "\"rh\"[G]*\"96\"", "\"rh\"[G]*\"97\"", "\"rh\"[G]*\"98\"", "\"rh\"[G]*\"99\"", "\"rh\"[G]*\"100\""))

#bias+rmse na jednym wykresie kolumnowym
w_bias_rmse$estimate = factor(w_bias_rmse$estimate, levels = c("\"rh\"[G]*\"90\"", "\"rh\"[G]*\"91\"", "\"rh\"[G]*\"92\"", "\"rh\"[G]*\"93\"", "\"rh\"[G]*\"94\"", "\"rh\"[G]*\"95\"", "\"rh\"[G]*\"96\"", "\"rh\"[G]*\"97\"", "\"rh\"[G]*\"98\"", "\"rh\"[G]*\"99\"", "\"rh\"[G]*\"100\""))
percentile_names = c(p090 = "p90", p091 = "p91", p092 = "p92", p093 = "p93", p094 = "p94", p095 = "p95", p096 = "p96", p097 = "p97", p098 = "p98", p099 = "p99", p100 = "p100")
#percentile_names = c(p090 = expression("rh"[SW]*"90"), p091 = expression("rh"[SW]*"91"), p092 = expression("rh"[SW]*"92"), p093 = expression("rh"[SW]*"93"), p094 = expression("rh"[SW]*"94"), p095 = expression("rh"[SW]*"95"), p096 = expression("rh"[SW]*"96"), p097 = expression("rh"[SW]*"97"), p098 = expression("rh"[SW]*"98"), p099 = expression("rh"[SW]*"99"), p100 = expression("rh"[SW]*"100"))
ggplot(w_bias_rmse, aes(x = truth, y = value, fill = error))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_point(data = w_PCTrmse_long, aes(x = truth, y = value/5, group = estimate), size = 0.5)+
  geom_line(data = w_PCTrmse_long, aes(x = truth, y = value/5, group = estimate), linewidth = 0.25)+
  scale_fill_manual(labels = (c("%RMSE", "Bias", "RMSE")), values=c("black", "#648FFF", "#F599CC"))+#"#fdb138", "#009E73"
  scale_x_discrete(labels = percentile_names)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 5),
        axis.text.y = element_text(size = 6),
        axis.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(linewidth = 0.25), panel.grid.minor = element_line(linewidth = 0.1), strip.text = element_text(size = 6), strip.background = element_rect(colour = "white", fill = "#f2f2f0")
        )+
  facet_grid(.~estimate, labeller = label_parsed)+
  scale_y_continuous(breaks = seq(-4, 5, 1),
                     sec.axis = sec_axis(transform = ~.*5, name = "%RMSE [%]"))+
  #labs(x = "ALS percentile", y = expression('GEDI rh'[G]*' - ALS percentile'*' [m]'), fill = "Error:")
  labs(x = expression('GEDI rh'[SW]), y = expression('GEDI rh'[G]*' - GEDI rh'[SW]*' [m]'), fill = "Error:")
ggsave("fig3_review.tiff", plot = last_plot(), path = "C:/Users/wojci/OneDrive - Uniwersytet Rolniczy im. Hugona Kołłątaja w Krakowie/GEDI_accuracy/review_20240528/analizy_review/",
       dpi = 300, units = "cm", width = 24, height = 6, bg = "white")

#wykresy liniowe ze stratyfikacją
facet_names_bias = c(rh_90 = "rh90", rh_91 = "rh91", rh_92 = "rh92", rh_93 = "rh93", rh_94 = "rh94", rh_95 = "rh95", rh_96 = "rh96", rh_97 = "rh97", rh_98 = "rh98", rh_99 = "rh99", rh_100 = "rh100")
w_ALS_bias_long = w_dif_ALS_bias %>% 
  pivot_longer(cols = 3:13, names_to = "truth", values_to = "bias") %>% 
  mutate(across("stats", ~str_replace(., "_p100", ""))) %>% 
  mutate(across("stats", ~str_replace(., "rh_", "rh"))) %>% 
  rename(estimate = stats, bias_ALS = bias)
w_SIM_bias_long = w_dif_SIM_bias %>% 
  pivot_longer(cols = 3:13, names_to = "truth", values_to = "bias") %>% 
  mutate(across("stats", ~str_replace(., "_p100", ""))) %>% 
  mutate(across("stats", ~str_replace(., "rh_", "rh"))) %>% 
  rename(estimate = stats, bias_SIM = bias)
w_bias_long = cbind(w_ALS_bias_long, w_SIM_bias_long$bias_SIM) %>% 
  rename(bias_SIM = `w_SIM_bias_long$bias_SIM`) %>% 
  pivot_longer(cols = 4:5, names_to = "stat") %>% 
  mutate(estimate = str_replace(estimate, "rh", "\"rh\"[G]*\"")) %>% 
  mutate(estimate = paste0(estimate, "\"")) %>% 
  mutate(across(grouping, ~paste0("\"", get(grouping), "\"")))
w_bias_long$estimate = factor(w_bias_long$estimate, levels = c("\"rh\"[G]*\"90\"", "\"rh\"[G]*\"91\"", "\"rh\"[G]*\"92\"", "\"rh\"[G]*\"93\"", "\"rh\"[G]*\"94\"", "\"rh\"[G]*\"95\"", "\"rh\"[G]*\"96\"", "\"rh\"[G]*\"97\"", "\"rh\"[G]*\"98\"", "\"rh\"[G]*\"99\"", "\"rh\"[G]*\"100\""))
percentile_names_long = c(p090 = "0.90", p091 = "0.91", p092 = "0.92", p093 = "0.93", p094 = "0.94", p095 = "0.95", p096 = "0.96", p097 = "0.97", p098 = "0.98", p099 = "0.99", p100 = "1")

w_ALS_rmse_long = w_ALS_rmse %>% 
  pivot_longer(cols = 3:13, names_to = "truth", values_to = "rmse") %>% 
  mutate(across("stat", ~str_replace(., "..estimate", ""))) %>% 
  mutate(across("stat", ~str_replace(., "rh0", "rh"))) %>% 
  rename(estimate = stat, rmse_ALS = rmse)
w_SIM_rmse_long = w_SIM_rmse %>% 
  pivot_longer(cols = 3:13, names_to = "truth", values_to = "rmse") %>% 
  mutate(across("stat", ~str_replace(., "..estimate", ""))) %>% 
  mutate(across("stat", ~str_replace(., "rh0", "rh"))) %>% 
  rename(estimate = stat, rmse_SIM = rmse)
w_rmse_long = cbind(w_ALS_rmse_long, w_SIM_rmse_long$rmse_SIM) %>% 
  rename(rmse_SIM = `w_SIM_rmse_long$rmse_SIM`) %>% 
  pivot_longer(cols = 4:5, names_to = "stat") %>% 
  mutate(estimate = str_replace(estimate, "rh", "\"rh\"[G]*\"")) %>% 
  mutate(estimate = paste0(estimate, "\"")) %>% 
  mutate(across(grouping, ~paste0("\"", get(grouping), "\"")))
w_rmse_long$estimate = factor(w_rmse_long$estimate, levels = c("\"rh\"[G]*\"90\"", "\"rh\"[G]*\"91\"", "\"rh\"[G]*\"92\"", "\"rh\"[G]*\"93\"", "\"rh\"[G]*\"94\"", "\"rh\"[G]*\"95\"", "\"rh\"[G]*\"96\"", "\"rh\"[G]*\"97\"", "\"rh\"[G]*\"98\"", "\"rh\"[G]*\"99\"", "\"rh\"[G]*\"100\""))

w_ALS_PCTrmse_long_bar = w_ALS_PCTrmse %>% 
  pivot_longer(cols = 3:13, names_to = "truth", values_to = "rmse") %>% 
  mutate(across("stat", ~str_replace(., "..estimate", ""))) %>% 
  mutate(across("stat", ~str_replace(., "rh0", "rh"))) %>% 
  rename(estimate = stat, PCTrmse_ALS = rmse)
w_SIM_PCTrmse_long_bar = w_SIM_PCTrmse %>% 
  pivot_longer(cols = 3:13, names_to = "truth", values_to = "rmse") %>% 
  mutate(across("stat", ~str_replace(., "..estimate", ""))) %>% 
  mutate(across("stat", ~str_replace(., "rh0", "rh"))) %>% 
  rename(estimate = stat, PCTrmse_SIM = rmse)
w_PCTrmse_long_bar = cbind(w_ALS_PCTrmse_long_bar, w_SIM_PCTrmse_long_bar$PCTrmse_SIM) %>% 
  rename(PCTrmse_SIM = `w_SIM_PCTrmse_long_bar$PCTrmse_SIM`) %>% 
  pivot_longer(cols = 4:5, names_to = "stat") %>% 
  mutate(estimate = str_replace(estimate, "rh", "\"rh\"[G]*\"")) %>% 
  mutate(estimate = paste0(estimate, "\"")) %>% 
  mutate(across(grouping, ~paste0("\"", get(grouping), "\""))) %>% 
  mutate(value = value*100)
w_PCTrmse_long_bar$estimate = factor(w_PCTrmse_long_bar$estimate, levels = c("\"rh\"[G]*\"90\"", "\"rh\"[G]*\"91\"", "\"rh\"[G]*\"92\"", "\"rh\"[G]*\"93\"", "\"rh\"[G]*\"94\"", "\"rh\"[G]*\"95\"", "\"rh\"[G]*\"96\"", "\"rh\"[G]*\"97\"", "\"rh\"[G]*\"98\"", "\"rh\"[G]*\"99\"", "\"rh\"[G]*\"100\""))

test_long = rbind(w_bias_long, w_rmse_long)

ggplot(test_long, aes(x = truth, y = value, group = stat))+
  geom_hline(yintercept = 0)+
  geom_bar(stat = "identity", position = "dodge", aes(fill = stat), colour = "white", linewidth = 0.1)+
  #geom_point(aes(colour = stat, shape = stat), size = 0.75, alpha = 0.8)+
  #geom_line(aes(colour = stat, linetype = stat), linewidth = 0.25, alpha = 0.8)+
  #scale_colour_discrete(labels = c("ALS\npercentile",expression('GEDI rh'[SW])))+
  #scale_shape_discrete(labels = c("ALS\npercentile",expression('GEDI rh'[SW])))+
  geom_point(data = w_PCTrmse_long_bar, aes(x = truth, y = value/5, group = stat, colour = stat, shape = stat), size = 0.5, alpha = 0.75)+
  geom_line(data = w_PCTrmse_long_bar, aes(x = truth, y = value/5, group = stat, colour = stat), linewidth = 0.1, alpha = 0.75)+
  scale_fill_manual(labels = c('Bias -\nALS p', expression('Bias -\nGEDI rh'[SW]), 'RMSE -\nALS p', expression('RMSE -\nGEDI rh'[SW])), 
                      values = c("#fdb138", "#648FFF", "#009E73", "#F599CC"))+
  scale_colour_manual(labels = c('%RMSE - \nALS p', expression('%RMSE -\nGEDI rh'[SW])), values = c("black", "gray40"))+
  scale_shape_manual(labels = c('%RMSE - \nALS p', expression('%RMSE -\nGEDI rh'[SW])), values = c(3, 4))+
  coord_flip(ylim = c(-5, 5), clip = "off")+
  facet_grid(factor(get(grouping), levels = sort(unique(test_long[[1]]), decreasing = TRUE))~estimate, labeller = label_parsed)+
  scale_x_discrete(labels = percentile_names_long)+
  scale_y_continuous(breaks = seq(-4,4,2),
                     sec.axis = sec_axis(transform = ~.*5, name = "%RMSE [%]", breaks = seq(-15,15,15)))+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 9),
        strip.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "top", 
        legend.direction = "horizontal", 
        legend.box = "horizontal",
        legend.text.align = 0,
        legend.key = element_rect(color = NA, fill = NA),
        panel.grid.major = element_line(linewidth = 0.25),
        panel.grid.minor = element_line(linewidth = 0.1),
        strip.background = element_rect(colour = "white", fill = "#f2f2f0"))+
  labs(x =  expression('GEDI rh'[G]*' - reference metric [m]'), y = "Error value [m]", fill = "", colour = "", shape = "")
ggsave("fig6_review.tiff", plot = last_plot(), path = "C:/Users/wojci/OneDrive - Uniwersytet Rolniczy im. Hugona Kołłątaja w Krakowie/GEDI_accuracy/review_20240528/analizy_review/",
       dpi = 300, units = "cm", width = 19, height = 21, bg = "white")

ggplot(w_rmse_long, aes(x = truth, y = value, group = stat))+
  geom_hline(yintercept = 0)+
  geom_point(aes(colour = stat, shape = stat), size = 1)+
  geom_line(aes(colour = stat), linewidth = 0.25)+
  scale_colour_discrete(labels = c("ALS\npercentile",expression('GEDI rh'[SW])))+
  scale_shape_discrete(labels = c("ALS\npercentile",expression('GEDI rh'[SW])))+
  facet_grid(factor(get(grouping), levels = sort(unique(test_long[[1]]), decreasing = TRUE))~estimate, labeller = label_parsed)+
  scale_x_discrete(labels = percentile_names_long)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        axis.text.y = element_text(size = 8),
        legend.text.align = 0,
        strip.background = element_rect(colour = "white", fill = "#f2f2f0"))+
  labs(x =  expression('GEDI rh'[G]*' - reference metric'), y = "RMSE [m]", colour = "Reference\nmetrics:", shape = "Reference\nmetrics:")
ggsave("ALS_gauss_rmse_vci_so.png", plot = last_plot(), device = "png", path = "C:/Users/wojci/OneDrive - Uniwersytet Rolniczy im. Hugona Kołłątaja w Krakowie/GEDI_accuracy/figures",
       dpi = 300, units = "px", width = 3500, height = 1800)

### proba z wykresami pokazujacymi porownanie najbliższych 4 metryk
#musi być wykonane wcześniej: test_long
for (i in seq(90, 100, 1)) {
  print(i)
  fi = function(i) {
    if(i == 100){
      c(i, i-1, i-2, i-3, i-4)
    } else if(i == 99) {
      c(i+1, i, i-1, i-2, i-3)
    } else if(i <= 98 & i >= 92) {
      c(i+2, i+1, i, i-1, i-2)
    } else if(i == 91) {
      c(i+3, i+2, i+1, i, i-1)
    } else if(i == 90) {
      c(i+4, i+3, i+2, i+1, i) 
    } else {
      print("Coś nie tak kolego.")
    }
  }
  j = fi(i)
  percentile_names_long = c(p090 = "0.90", p091 = "0.91", p092 = "0.92", p093 = "0.93", p094 = "0.94", p095 = "0.95", p096 = "0.96", p097 = "0.97", p098 = "0.98", p099 = "0.99", p100 = "1")
  fp = function(j, str_list) {
    tmp_j = j-90
    tmp_j = tmp_j+1
    str_list = str_list[tmp_j]
    return(str_list)
  }
  percentile_names_long_sub = fp(j, percentile_names_long)
  
  test_long_sub = test_long %>% 
    filter(grepl(i, estimate))
  test_long_sub = lapply(j, function(j, x){x = filter(x, grepl(j, truth))}, x = test_long_sub)
  test_long_sub = rbind(test_long_sub[[1]], test_long_sub[[2]], test_long_sub[[3]], test_long_sub[[4]], test_long_sub[[5]])
  
  ggplot(test_long_sub, aes(x = truth, y = value, group = stat))+
    geom_bar(stat = "identity", position = "dodge", aes(fill = stat), colour = "white", linewidth = 0.25)+
    scale_fill_manual(labels = c('Bias -\nALS p', expression('Bias -\nGEDI rh'[SW]), 'RMSE -\nALS p', expression('RMSE -\nGEDI rh'[SW])), 
                      values = c("#fdb138", "#648FFF", "#009E73", "#F599CC"))+
    coord_flip()+
    facet_grid(factor(get(grouping), levels = sort(unique(test_long_sub[[1]]), decreasing = TRUE))~estimate, labeller = label_parsed)+
    scale_x_discrete(labels = percentile_names_long_sub)+
    ylim(-3.25,3.7)+
    theme_minimal()+
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          legend.position = "top", 
          legend.direction = "horizontal", 
          legend.box = "horizontal",
          legend.text.align = 0,
          legend.key = element_rect(color = NA, fill = NA),
          strip.background = element_rect(colour = "white", fill = "#f2f2f0"))+
    labs(x =  expression('GEDI rh'[G]*' - reference metric'), y = "Error value [m]", fill = "")
  ggsave(paste0("so_hgt_", i, ".png"), plot = last_plot(), device = "png", path = "C:/Users/wojci/OneDrive - Uniwersytet Rolniczy im. Hugona Kołłątaja w Krakowie/GEDI_accuracy/figures",
         dpi = 300, units = "px", width = 700, height = 2500, bg = "white")
}

### funkcja do obliczania MAPE - nieuzywana
# f_mape_table = function(w, sel_columns, grouping = NULL) {
#   if (is.null(grouping)) {
#     tmp = w %>% 
#       select(105:115, all_of(sel_columns))
#   } else {
#     tmp = w %>% 
#       group_by( {{ grouping }}) %>% 
#       select(105:115, all_of(sel_columns), {{ grouping }})
#   }
#   w_dif_mape_max = tmp %>% 
#     st_drop_geometry() %>% 
#     lapply(GEDIrh_cols_list, mape, data = ., truth = 22)
#   w_dif_mape_max = as.data.frame(w_dif_mape_max) %>% 
#     select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
#   w_dif_mape_p99 = tmp %>% 
#     st_drop_geometry() %>% 
#     lapply(GEDIrh_cols_list, mape, data = ., truth = 21)
#   w_dif_mape_p99 = as.data.frame(w_dif_mape_p99) %>% 
#     select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
#   w_dif_mape_p98 = tmp %>% 
#     st_drop_geometry() %>% 
#     lapply(GEDIrh_cols_list, mape, data = ., truth = 20)
#   w_dif_mape_p98 = as.data.frame(w_dif_mape_p98) %>% 
#     select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
#   w_dif_mape_p97 = tmp %>% 
#     st_drop_geometry() %>% 
#     lapply(GEDIrh_cols_list, mape, data = ., truth = 19)
#   w_dif_mape_p97 = as.data.frame(w_dif_mape_p97) %>% 
#     select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
#   w_dif_mape_p96 = tmp %>% 
#     st_drop_geometry() %>% 
#     lapply(GEDIrh_cols_list, mape, data = ., truth = 18)
#   w_dif_mape_p96 = as.data.frame(w_dif_mape_p96) %>% 
#     select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
#   w_dif_mape_p95 = tmp %>% 
#     st_drop_geometry() %>% 
#     lapply(GEDIrh_cols_list, mape, data = ., truth = 17)
#   w_dif_mape_p95 = as.data.frame(w_dif_mape_p95) %>% 
#     select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
#   w_dif_mape_p94 = tmp %>% 
#     st_drop_geometry() %>% 
#     lapply(GEDIrh_cols_list, mape, data = ., truth = 16)
#   w_dif_mape_p94 = as.data.frame(w_dif_mape_p94) %>% 
#     select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
#   w_dif_mape_p93 = tmp %>% 
#     st_drop_geometry() %>% 
#     lapply(GEDIrh_cols_list, mape, data = ., truth = 15)
#   w_dif_mape_p93 = as.data.frame(w_dif_mape_p93) %>% 
#     select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
#   w_dif_mape_p92 = tmp %>% 
#     st_drop_geometry() %>% 
#     lapply(GEDIrh_cols_list, mape, data = ., truth = 14)
#   w_dif_mape_p92 = as.data.frame(w_dif_mape_p92) %>% 
#     select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
#   w_dif_mape_p91 = tmp %>% 
#     st_drop_geometry() %>% 
#     lapply(GEDIrh_cols_list, mape, data = ., truth = 13)
#   w_dif_mape_p91 = as.data.frame(w_dif_mape_p91) %>% 
#     select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
#   w_dif_mape_p90 = tmp %>% 
#     st_drop_geometry() %>% 
#     lapply(GEDIrh_cols_list, mape, data = ., truth = 12)
#   w_dif_mape_p90 = as.data.frame(w_dif_mape_p90) %>% 
#     select(-ends_with("metric"), -ends_with("estimator"), -matches("^rh0\\d\\d\\.\\w"))
#   
#   w_dif_mape_max = w_dif_mape_max %>% 
#     pivot_longer(cols = ends_with(".estimate"), values_to = "p100", names_to = "stat")
#   w_dif_mape_p99 = w_dif_mape_p99 %>% 
#     pivot_longer(cols = ends_with(".estimate"), values_to = "p099") %>% 
#     select(p099)
#   w_dif_mape_p98 = w_dif_mape_p98 %>% 
#     pivot_longer(cols = ends_with(".estimate"), values_to = "p098") %>% 
#     select(p098)
#   w_dif_mape_p97 = w_dif_mape_p97 %>% 
#     pivot_longer(cols = ends_with(".estimate"), values_to = "p097") %>% 
#     select(p097)
#   w_dif_mape_p96 = w_dif_mape_p96 %>% 
#     pivot_longer(cols = ends_with(".estimate"), values_to = "p096") %>% 
#     select(p096)
#   w_dif_mape_p95 = w_dif_mape_p95 %>% 
#     pivot_longer(cols = ends_with(".estimate"), values_to = "p095") %>% 
#     select(p095)
#   w_dif_mape_p94 = w_dif_mape_p94 %>% 
#     pivot_longer(cols = ends_with(".estimate"), values_to = "p094") %>% 
#     select(p094)
#   w_dif_mape_p93 = w_dif_mape_p93 %>% 
#     pivot_longer(cols = ends_with(".estimate"), values_to = "p093") %>% 
#     select(p093)
#   w_dif_mape_p92 = w_dif_mape_p92 %>% 
#     pivot_longer(cols = ends_with(".estimate"), values_to = "p092") %>% 
#     select(p092)
#   w_dif_mape_p91 = w_dif_mape_p91 %>% 
#     pivot_longer(cols = ends_with(".estimate"), values_to = "p091") %>% 
#     select(p091)
#   w_dif_mape_p90 = w_dif_mape_p90 %>% 
#     pivot_longer(cols = ends_with(".estimate"), values_to = "p090") %>% 
#     select(p090)
# 
#   w_mape = cbind(w_dif_mape_max, w_dif_mape_p99, w_dif_mape_p98, w_dif_mape_p97, 
#                  w_dif_mape_p96, w_dif_mape_p95, w_dif_mape_p94, w_dif_mape_p93,
#                  w_dif_mape_p92, w_dif_mape_p91, w_dif_mape_p90)
#   
#   return(w_mape)
# }
# #w_ALS_mape = f_mape_table(w, sel_columns = cols_ALS, grouping = grouping)
# #w_SIM_mape = f_mape_table(w, sel_columns = cols_SIM, grouping = grouping)
# #write.table(w_mape, "G:/new/all_mape_ALS_GEDI.csv", col.names = T, row.names = F, sep = ",")