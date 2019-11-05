

# ejercicio:
# identificar cutoff optimo para cada metrica
opt_cutoff = function(obs, pred) {
  dat = seq(0.01,0.98,0.01) %>%
    map_dfr(function(c) metricas_cutoff(c, obs, pred)$metricas)
  dat %>% 
    dplyr::filter(metrica %in% c("accuracy","precision","ganancia")) %>% 
    group_by(metrica) %>% 
    dplyr::filter(valor %in% max(valor, na.rm=T)) %>% 
    ungroup()
}
opt_cutoff(dat_test$target, pred)

# ejercicio: 
# aplicar el modelo a la eph mas reciente
dat_new = indiv %>%
  dplyr::filter(periodo=="2019-1") %>% 
  dplyr::filter(estado==1) %>% 
  left_join(emae_ia, by="periodo")
pred_new = pred_logistic(mod_l, dat_new, receta_trained)
