

library(knitr)

## QC PLOTS FOR EXCEL AND FOR RAW

# the plots can be found in this order:  
## long access 
## 
## 



## see objects from the raw vs excel qc 
### long access 

## uniform variables: session_length_hours, reinforcer, bolus_volume_ml, dose_ug_kg_infusion, time_out_seconds
pdf("kalivas_longaccess_verified.pdf", onefile = T)
for (i in 1:(length(kalivas_lga_measures)/2)){
  g_cohort <-lga_allsubjects_tograph %>% 
    dplyr::filter(active_lever_excel == active_lever_raw,
                  inactive_lever_excel == inactive_lever_raw,
                  infusions_excel == infusions_raw) %>% 
    dplyr::filter(resolution != "FLAG_EXPERIMENT"|is.na(resolution) ) %>% 
    mutate(session = as.numeric(session) %>% as.factor) %>% 
    ggplot(aes(x = session, group = session)) + 
    geom_boxplot(aes_string(y = kalivas_lga_measures[i])) + 
                   facet_grid(~ cohort_number) +
                   labs(title = paste0(gsub("_(?!excel)", " ", gsub("_excel", "", kalivas_lga_measures[i]), perl = T), "_Verified_Data_U01_Kalivas", "\n", "By cohort"),
                        y = gsub("_(?!excel)", " ", gsub("_excel", "", kalivas_lga_measures[i]), perl = T)) + 
                   theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
                 
  g_room <-lga_allsubjects_tograph %>% 
    dplyr::filter(active_lever_excel == active_lever_raw,
                  inactive_lever_excel == inactive_lever_raw,
                  infusions_excel == infusions_raw) %>% 
    mutate(session = as.numeric(session) %>% as.factor) %>%
    dplyr::filter(resolution != "FLAG_EXPERIMENT"|is.na(resolution)  ) %>% 
    ggplot(aes(x = session, group = session)) + 
    geom_boxplot(aes_string(y = kalivas_lga_measures[i])) + 
    facet_grid(~ self_administration_room) +
    labs(title = paste0(gsub("_(?!excel)", " ", gsub("_excel", "", kalivas_lga_measures[i]), perl = T), "_Verified_Data_U01_Kalivas", "\n", "By self admin room"),
         y = gsub("_(?!excel)", " ", gsub("_excel", "", kalivas_lga_measures[i]), perl = T)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  g_box <-lga_allsubjects_tograph %>% 
    dplyr::filter(active_lever_excel == active_lever_raw,
                  inactive_lever_excel == inactive_lever_raw,
                  infusions_excel == infusions_raw) %>% 
    mutate(session = as.numeric(session) %>% as.factor) %>%
    dplyr::filter(resolution != "FLAG_EXPERIMENT"|is.na(resolution)  ) %>% 
    ggplot(aes(x = session, group = session)) + 
    geom_boxplot(aes_string(y = kalivas_lga_measures[i])) + 
    facet_grid(~ self_administration_box) +
    labs(title = paste0(gsub("_(?!excel)", " ", gsub("_excel", "", kalivas_lga_measures[i]), perl = T), "_Verified_Data_U01_Kalivas", "\n", "By self admin box"),
         y = gsub("_(?!excel)", " ", gsub("_excel", "", kalivas_lga_measures[i]), perl = T)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  g_box_x <-lga_allsubjects_tograph %>% 
    dplyr::filter(active_lever_excel == active_lever_raw,
                  inactive_lever_excel == inactive_lever_raw,
                  infusions_excel == infusions_raw) %>% 
    mutate(session = as.numeric(session) %>% as.factor,
           self_administration_box = as.numeric(self_administration_box) %>% as.factor) %>%
    dplyr::filter(resolution != "FLAG_EXPERIMENT"|is.na(resolution)  ) %>% 
    ggplot(aes(x = self_administration_box, group = self_administration_box)) + 
    geom_boxplot(aes_string(y = kalivas_lga_measures[i])) + 
    # facet_grid(~ self_administration_box) +
    labs(title = paste0(gsub("_(?!excel)", " ", gsub("_excel", "", kalivas_lga_measures[i]), perl = T), "_Verified_Data_U01_Kalivas", "\n", "By self admin box"),
         y = gsub("_(?!excel)", " ", gsub("_excel", "", kalivas_lga_measures[i]), perl = T)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  g_rein <-lga_allsubjects_tograph %>% 
    dplyr::filter(active_lever_excel == active_lever_raw,
                  inactive_lever_excel == inactive_lever_raw,
                  infusions_excel == infusions_raw) %>% 
    mutate(session = as.numeric(session) %>% as.factor) %>% 
    dplyr::filter(resolution != "FLAG_EXPERIMENT"|is.na(resolution)) %>% 
    ggplot(aes(x = session, group = session, fill = reinforcement_schedule)) + 
    geom_boxplot(aes_string(y = kalivas_lga_measures[i])) + 
    # facet_grid(~ reinforcement_schedule) +
    labs(title = paste0(gsub("_(?!excel)", " ", gsub("_excel", "", kalivas_lga_measures[i]), perl = T), "_Verified_Data_U01_Kalivas", "\n", "By reinforcement schedule"),
        y = gsub("_(?!excel)", " ", gsub("_excel", "", kalivas_lga_measures[i]), perl = T)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  g_saline <-lga_allsubjects_tograph %>% 
    dplyr::filter(active_lever_excel == active_lever_raw,
                  inactive_lever_excel == inactive_lever_raw,
                  infusions_excel == infusions_raw) %>% 
    dplyr::filter(resolution != "FLAG_EXPERIMENT"|is.na(resolution) ) %>% 
    mutate(session = as.numeric(session) %>% as.factor) %>% 
    ggplot(aes(x = session, group = session)) + 
    geom_boxplot(aes_string(y = kalivas_lga_measures[i])) + 
    facet_grid(~ heroin_or_saline) +
    labs(title = paste0(gsub("_(?!excel)", " ", gsub("_excel", "", kalivas_lga_measures[i]), perl = T), "_Verified_Data_U01_Kalivas", "\n", "By heroin/saline"),
         y = gsub("_(?!excel)", " ", gsub("_excel", "", kalivas_lga_measures[i]), perl = T)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  # lga_allsubjects_tograph %>% select(cohort_number, self_administration_room) %>% table %>% kable 
  # lga_allsubjects_tograph %>% select(cohort_number, reinforcement_schedule) %>% table %>% kable 
  # lga_allsubjects_tograph %>% select(sex, self_administration_box) %>% table %>% kable 
  # lga_allsubjects_tograph %>% select(cohort_number, self_administration_box) %>% table %>% kable 
  # 
  
  print(g_cohort)
  print(g_room)
  print(g_box)
  print(g_box_x)
  print(g_rein)
  print(g_saline)
  
}

dev.off()

# for above tables
# can also use this dataframe:
lga_allsubjects_tograph %>% 
  dplyr::filter(active_lever_excel == active_lever_raw,
                inactive_lever_excel == inactive_lever_raw,
                infusions_excel == infusions_raw) %>% 
  dplyr::filter(resolution != "FLAG_EXPERIMENT"|is.na(resolution) ) 


# ## for email graphics: 
# lga_allsubjects_tograph %>% 
#   dplyr::filter(active_lever_excel == active_lever_raw,
#                 inactive_lever_excel == inactive_lever_raw,
#                 infusions_excel == infusions_raw) %>% 
#   dplyr::filter(resolution != "FLAG_EXPERIMENT"|is.na(resolution) ) %>% 
#   mutate(session = as.numeric(session) %>% as.factor) %>% dplyr::filter(session %in% c(11, 13), active_lever_excel > 300) %>% select(internal_id, session, active_lever_excel)
# # for clarifying the non-kal subject id 
# lga_allsubjects_tograph %>% dplyr::filter(session == 13, is.na(active_lever_raw))




############################
# Exp 2: OPEN FIELD TASK
############################
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC")
pdf("plot_kalivas_openfieldtask_excel", onefile = T)

kalivas_oft_measures <- c("center_time_seconds", "number_of_rears", "number_of_sterotypies", "total_cm_traveled", "total_time_traveled_seconds")
kalivas_oft_allcohorts_excel_processed_tograph <- kalivas_oft_allcohorts_excel_processed %>% 
  mutate_at(vars(one_of(kalivas_oft_measures)), as.numeric)
# check if resolution is just ignore or na (pass)


pdf("plot_kalivas_oft_excel.pdf", onefile = T)
for (i in 1:(length(kalivas_oft_measures))){
  
  g_cohort <- kalivas_oft_allcohorts_excel_processed_tograph %>% 
    mutate(session = factor(session, levels = c("before_SA", "after_SA"))) %>% 
    ggplot(aes(x = session, fill = heroin_or_saline)) + 
    geom_boxplot(aes_string(y = kalivas_oft_measures[i])) + 
    facet_grid(~ cohort_number)
  
  print(g_cohort)
}

dev.off()



setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC")

kalivas_oft_measures <- c("center_time_seconds", "number_of_rears", "number_of_sterotypies", "total_cm_traveled", "total_time_traveled_seconds")
kalivas_oft_raw_tograph <- openfieldtask_raw_df_total %>% 
  mutate_at(vars(one_of(kalivas_oft_measures)), as.numeric)
# check if resolution is just ignore or na (pass)


pdf("plot_kalivas_openfieldtask_raw.pdf", onefile = T)
for (i in 1:(length(kalivas_oft_measures))){
  
  g_cohort <- kalivas_oft_raw_tograph %>%
    mutate(session = factor(session, levels = c("before_SA", "after_SA"))) %>%
    ggplot(aes(x = cohort, fill = session)) +
    geom_boxplot(aes_string(y = kalivas_oft_measures[i])) +
    labs(title = paste0(kalivas_oft_measures[i], "Before and After Self Admin By Cohort"))
  
  g_individual <- kalivas_oft_raw_tograph %>% 
    mutate(session = factor(session, levels = c("before_SA", "after_SA"))) %>% 
    ggplot(aes(x = session, group = labanimalid)) + 
    geom_line(aes_string(y = kalivas_oft_measures[i])) + 
    facet_grid(~ cohort) + 
    labs(title = paste0(kalivas_oft_measures[i], "Before and After Self Admin By Cohort and By Individual"))
  
  print(g_cohort)
  print(g_individual)
}

dev.off()


############################
# Exp 3: TAIL FLICK
############################

## NOTE THAT TAIL FLICK WILL NOT HAVE RAW DATA TO QC AGAINST 

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC")

kalivas_tf_measures <- grep("_rt", names(kalivas_tf_allcohorts_excel_processed), value = T)
kalivas_tf_allcohorts_excel_processed_tograph <- kalivas_tf_allcohorts_excel_processed %>% 
  mutate_at(vars(one_of(kalivas_tf_measures)), as.numeric)
# check if resolution is just ignore or na (pass)


pdf("plot_kalivas_tailflick_excel.pdf", onefile = T)
for (i in 1:(length(kalivas_tf_measures))){
  
  g_cohort <- kalivas_tf_allcohorts_excel_processed_tograph %>% 
    mutate(session = factor(session, levels = c("before_SA", "after_SA"))) %>% 
    ggplot(aes(x = session, fill = heroin_or_saline)) + 
    geom_boxplot(aes_string(y = kalivas_tf_measures[i])) + 
    facet_grid(~ cohort_number)
  g_sex <- kalivas_tf_allcohorts_excel_processed_tograph %>% 
    mutate(session = factor(session, levels = c("before_SA", "after_SA"))) %>% 
    ggplot(aes(x = session, color = heroin_or_saline, linetype = sex)) + 
    geom_boxplot(aes_string(y = kalivas_tf_measures[i])) + 
    facet_grid(~ cohort_number)
  
  print(g_cohort)  
  print(g_sex)

}

dev.off()

kalivas_tf_allcohorts_excel_processed %>% subset(treatment_rt4_seconds %>% is.na)



