

library(knitr)

## QC PLOTS FOR EXCEL AND FOR RAW

# the plots can be found in this order:  
## long access 
## open field task
## tail flick



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

lga_allsubjects_tograph %>% 
  subset(internal_id %in% head(unique(lga_allsubjects_tograph$internal_id), 4)) %>% 
  dplyr::filter(active_lever_excel == active_lever_raw,
                inactive_lever_excel == inactive_lever_raw,
                infusions_excel == infusions_raw) %>% 
  dplyr::filter(resolution != "FLAG_EXPERIMENT"|is.na(resolution) ) %>% 
  mutate(session = as.numeric(session) %>% as.factor) %>% 
  ggplot() + 
  geom_path(aes(x = session, y = active_lever_excel, group = internal_id)) + 
  facet_grid(~ sex) + 
  labs(title = "Active Lever for Validated Data")

lga_allsubjects_tograph %>% dplyr::filter_at(vars(matches("_raw")), is.na)




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
# Exp 1: OPEN FIELD TASK
############################
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC")

kalivas_oft_measures <- c("center_time_seconds", "number_of_rears", "number_of_sterotypies", "total_cm_traveled", "total_time_traveled_seconds")

# check if resolution is just ignore or na (pass)

pdf("plot_kalivas_openfieldtask_excel.pdf", onefile = T)
for (i in 1:(length(kalivas_oft_measures))){
  
  g_heroin_saline <- kalivas_oft_allcohorts_excel_processed_c01_08_df %>%
    mutate_at(vars(one_of(kalivas_oft_measures)), as.numeric) %>% 
    mutate(session = factor(session, levels = c("before_SA", "after_SA"))) %>% 
    ggplot(aes(x = session, fill = heroin_or_saline)) + 
    geom_boxplot(aes_string(y = kalivas_oft_measures[i])) + 
    facet_grid(~ cohort)+ theme(axis.text.x = element_text(angle = 45))
  
  g_cohort <- kalivas_oft_allcohorts_excel_processed_c01_08_df %>%
    mutate_at(vars(one_of(kalivas_oft_measures)), as.numeric) %>%
    mutate(session = factor(session, levels = c("before_SA", "after_SA"))) %>%
    ggplot(aes(x = cohort, fill = session)) +
    geom_boxplot(aes_string(y = kalivas_oft_measures[i])) +
    labs(title = paste0(kalivas_oft_measures[i], "Before and After Self Admin By Cohort"))
  
  g_cohort_sex <- kalivas_oft_allcohorts_excel_processed_c01_08_df %>%
    mutate_at(vars(one_of(kalivas_oft_measures)), as.numeric) %>%
    mutate(session = factor(session, levels = c("before_SA", "after_SA"))) %>%
    ggplot(aes(x = sex, fill = session)) +
    geom_boxplot(aes_string(y = kalivas_oft_measures[i])) +
    facet_grid(~ cohort) + 
    labs(title = paste0(kalivas_oft_measures[i], "Before and After Self Admin By Cohort and Sex"))
  
  g_individual <- kalivas_oft_allcohorts_excel_processed_c01_08_df %>%
    mutate_at(vars(one_of(kalivas_oft_measures)), as.numeric) %>% 
    mutate(session = factor(session, levels = c("before_SA", "after_SA"))) %>% 
    ggplot(aes(x = session, group = rfid)) + 
    geom_line(aes_string(y = kalivas_oft_measures[i])) + 
    facet_grid(~ cohort) + 
    labs(title = paste0(kalivas_oft_measures[i], "Before and After Self Admin By Cohort and By Individual")) + 
    theme(axis.text.x = element_text(angle = 45))
  
  print(g_cohort)
  print(g_heroin_saline)
  print(g_cohort_sex)
  print(g_individual)
  
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
    labs(title = paste0(kalivas_oft_measures[i], "Before and After Self Admin By Cohort")) + 
    theme(axis.text.x = element_text(angle = 45))
  
  g_individual <- kalivas_oft_raw_tograph %>% 
    mutate(session = factor(session, levels = c("before_SA", "after_SA"))) %>% 
    ggplot(aes(x = session, group = labanimalid)) + 
    geom_line(aes_string(y = kalivas_oft_measures[i])) + 
    facet_grid(~ cohort) + 
    labs(title = paste0(kalivas_oft_measures[i], "Before and After Self Admin By Cohort and By Individual")) + 
    theme(axis.text.x = element_text(angle = 45))
  
  print(g_cohort)
  print(g_individual)
}

dev.off()


############################
# Exp 2: TAIL FLICK
############################

## NOTE THAT TAIL FLICK WILL NOT HAVE RAW DATA TO QC AGAINST 



kalivas_tf_measures <- grep("_rt", names(kalivas_tf_allcohorts_excel_processed_c01_08_df), value = T)
kalivas_tf_allcohorts_excel_processed_tograph <- kalivas_tf_allcohorts_excel_processed_c01_08_df %>% 
  mutate_at(vars(one_of(kalivas_tf_measures)), as.numeric) %>% 
  mutate_at(vars(one_of(kalivas_tf_measures)), ~ replace(., resolution == "FLAG_EXPERIMENT", NA))

data <- kalivas_tf_allcohorts_excel_processed_c01_08_df
measures <- kalivas_tf_measures


pdf("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC/plot_kalivas_tailflick_excel.pdf", onefile = T)
for (i in 1:(length(measures))){
  
  
  g_cohort <- data %>%
    mutate_at(vars(one_of(measures)), as.numeric) %>%
    mutate(session = factor(session, levels = c("before_SA", "after_SA"))) %>%
    ggplot(aes(x = cohort, fill = session)) +
    geom_boxplot(aes_string(y = measures[i])) +
    labs(title = paste0(measures[i], "Before and After Self Admin By Cohort"))
  
  g_heroin_saline <- data %>%
    mutate_at(vars(one_of(measures)), as.numeric) %>% 
    mutate(session = factor(session, levels = c("before_SA", "after_SA"))) %>% 
    ggplot(aes(x = session, fill = heroin_or_saline)) + 
    geom_boxplot(aes_string(y = measures[i])) + 
    facet_grid(~ cohort)+ theme(axis.text.x = element_text(angle = 45))
  
  g_cohort_sex <- data %>%
    mutate_at(vars(one_of(measures)), as.numeric) %>%
    mutate(session = factor(session, levels = c("before_SA", "after_SA"))) %>%
    ggplot(aes(x = sex, fill = session)) +
    geom_boxplot(aes_string(y = measures[i])) +
    facet_grid(~ cohort) + 
    labs(title = paste0(measures[i], "Before and After Self Admin By Cohort and Sex"))
  
  g_individual <- data %>%
    mutate_at(vars(one_of(measures)), as.numeric) %>% 
    mutate(session = factor(session, levels = c("before_SA", "after_SA"))) %>% 
    ggplot(aes(x = session, group = rfid)) + 
    geom_line(aes_string(y = measures[i])) + 
    facet_grid(~ cohort) + 
    labs(title = paste0(measures[i], "Before and After Self Admin By Cohort and By Individual")) + 
    theme(axis.text.x = element_text(angle = 45))
  
  print(g_cohort)
  print(g_heroin_saline)
  print(g_cohort_sex)
  print(g_individual)
  
}

dev.off()




############################
# Exp 3: EPM 
############################

kalivas_epm_measures <- c("closed_arm_entries", "closed_arm_percent_time", "closed_arm_time_seconds", "open_arm_entries", "open_arm_percent_time", "open_arm_time_seconds")
kalivas_epm_allcohorts_excel_processed_c01_08_df_tograph <- kalivas_epm_allcohorts_excel_processed_c01_08_df %>% 
  mutate_at(vars(one_of(kalivas_epm_measures)), as.numeric) %>% 
  mutate_at(vars(one_of(kalivas_epm_measures)), ~ replace(., resolution == "FLAG_EXPERIMENT", NA))



pdf("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC/plot_kalivas_epm_excel.pdf", onefile = T)
for (i in 1:(length(kalivas_epm_measures))){
  
  g_cohort <- kalivas_epm_allcohorts_excel_processed_c01_08_df_tograph %>%
    mutate(session = factor(session, levels = c("before_SA", "after_SA"))) %>%
    ggplot(aes(x = cohort, fill = session)) +
    geom_boxplot(aes_string(y = kalivas_epm_measures[i])) +
    labs(title = paste0(kalivas_epm_measures[i], "Before and After Self Admin By Cohort"))
  
  g_heroin_saline <- kalivas_epm_allcohorts_excel_processed_c01_08_df_tograph %>%
    mutate_at(vars(one_of(kalivas_epm_measures)), as.numeric) %>% 
    mutate(session = factor(session, levels = c("before_SA", "after_SA"))) %>% 
    ggplot(aes(x = session, fill = heroin_or_saline)) + 
    geom_boxplot(aes_string(y = kalivas_epm_measures[i])) + 
    facet_grid(~ cohort)+ theme(axis.text.x = element_text(angle = 45))
  
  
  g_cohort_sex <- kalivas_epm_allcohorts_excel_processed_c01_08_df_tograph %>%
    mutate(session = factor(session, levels = c("before_SA", "after_SA"))) %>%
    ggplot(aes(x = sex, fill = session)) +
    geom_boxplot(aes_string(y = kalivas_epm_measures[i])) +
    facet_grid(~ cohort) + 
    labs(title = paste0(kalivas_epm_measures[i], "Before and After Self Admin By Cohort and Sex"))
  
  g_individual <- kalivas_epm_allcohorts_excel_processed_c01_08_df_tograph %>% 
    mutate(session = factor(session, levels = c("before_SA", "after_SA"))) %>% 
    ggplot(aes(x = session, group = rfid)) + 
    geom_line(aes_string(y = kalivas_epm_measures[i])) + 
    facet_grid(~ cohort) + 
    labs(title = paste0(kalivas_epm_measures[i], "Before and After Self Admin By Cohort and By Individual")) + 
    theme(axis.text.x = element_text(angle = 45))
  
  print(g_cohort)
  print(g_heroin_saline)
  print(g_cohort_sex)
  print(g_individual)
}

dev.off()





############################
# LGA 12H 
############################

kalivas_lga_measures <- c("active_lever", "inactive_lever", "infusions")
kalivas_lga_allcohorts_excel_processed_c01_08_df_tograph <- kalivas_lga_allcohorts_excel_processed_c01_08_df %>% 
  mutate_at(vars(one_of(kalivas_lga_measures)), as.numeric) %>% 
  mutate_at(vars(one_of(kalivas_lga_measures)), ~ replace(., resolution == "FLAG_EXPERIMENT", NA))



pdf("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC/plot_kalivas_lga_excel.pdf", onefile = T)
for (i in 1:(length(kalivas_lga_measures))){
  
  g_cohort <- kalivas_lga_allcohorts_excel_processed_c01_08_df_tograph %>%
    ggplot(aes(x = cohort, fill = session)) +
    geom_boxplot(aes_string(y = kalivas_lga_measures[i])) +
    labs(title = paste0(kalivas_lga_measures[i], "Before and After Self Admin By Cohort"))
  
  g_heroin_saline <- kalivas_lga_allcohorts_excel_processed_c01_08_df_tograph %>%
    mutate_at(vars(one_of(kalivas_lga_measures)), as.numeric) %>% 
    ggplot(aes(x = session, fill = heroin_or_saline)) + 
    geom_boxplot(aes_string(y = kalivas_lga_measures[i])) + 
    facet_grid(~ cohort)+ theme(axis.text.x = element_text(angle = 45))
  
  
  g_cohort_sex <- kalivas_lga_allcohorts_excel_processed_c01_08_df_tograph %>%
    ggplot(aes(x = sex, fill = session)) +
    geom_boxplot(aes_string(y = kalivas_lga_measures[i])) +
    facet_grid(~ cohort) + 
    labs(title = paste0(kalivas_lga_measures[i], "Before and After Self Admin By Cohort and Sex"))
  
  g_individual <- kalivas_lga_allcohorts_excel_processed_c01_08_df_tograph %>% 
    ggplot(aes(x = session, group = rfid)) + 
    geom_line(aes_string(y = kalivas_lga_measures[i])) + 
    facet_grid(~ cohort) + 
    labs(title = paste0(kalivas_lga_measures[i], "Before and After Self Admin By Cohort and By Individual")) + 
    theme(axis.text.x = element_text(angle = 45))
  
  print(g_cohort)
  print(g_heroin_saline)
  print(g_cohort_sex)
  print(g_individual)
}

dev.off()


############################
# PR 
############################

kalivas_pr_measures <- c("pr_step", "infusions", "total_session_minutes", "active_lever", "inactive_lever", "current_ratio")
kalivas_pr_allcohorts_excel_processed_c01_08_df_tograph <- kalivas_pr_allcohorts_excel_processed_c01_08_df %>% 
  mutate_at(vars(one_of(kalivas_pr_measures)), as.numeric) %>% 
  mutate_at(vars(one_of(kalivas_pr_measures)), ~ replace(., resolution == "FLAG_EXPERIMENT", NA))



pdf("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC/plot_kalivas_pr_excel.pdf", onefile = T)
for (i in 1:(length(kalivas_pr_measures))){
  
  g_cohort <- kalivas_pr_allcohorts_excel_processed_c01_08_df_tograph %>%
    ggplot(aes(x = cohort)) +
    geom_boxplot(aes_string(y = kalivas_pr_measures[i])) +
    labs(title = paste0(kalivas_pr_measures[i], "Before and After Self Admin By Cohort"))
  
  g_heroin_saline <- kalivas_pr_allcohorts_excel_processed_c01_08_df_tograph %>%
    mutate_at(vars(one_of(kalivas_pr_measures)), as.numeric) %>% 
    ggplot(aes(x = cohort, fill = heroin_or_saline)) + 
    geom_boxplot(aes_string(y = kalivas_pr_measures[i])) + 
    theme(axis.text.x = element_text(angle = 45))
  
  
  g_cohort_sex <- kalivas_pr_allcohorts_excel_processed_c01_08_df_tograph %>%
    ggplot(aes(x = cohort, fill = sex)) +
    geom_boxplot(aes_string(y = kalivas_pr_measures[i])) +
    labs(title = paste0(kalivas_pr_measures[i], "Before and After Self Admin By Cohort and Sex"))
  
  
  print(g_cohort)
  print(g_heroin_saline)
  print(g_cohort_sex)
}

dev.off()





############################
# EXTINCTION PRIME 
############################

kalivas_expr_measures <- c("hour_1", "hour_2", "hour_3", "hour_4", "hour_5", "hour_6")
kalivas_expr_allcohorts_excel_processed_c01_08_df_tograph <- kalivas_expr_allcohorts_excel_processed_c01_08_df %>% 
  mutate_at(vars(one_of(kalivas_expr_measures)), as.numeric) %>% 
  mutate_at(vars(one_of(kalivas_expr_measures)), ~ replace(., resolution == "FLAG_EXPERIMENT", NA))



pdf("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC/plot_kalivas_expr_excel.pdf", onefile = T)
for (i in 1:(length(kalivas_expr_measures))){
  
  g_cohort <- kalivas_expr_allcohorts_excel_processed_c01_08_df_tograph %>%
    ggplot(aes(x = cohort, fill = lever)) +
    geom_boxplot(aes_string(y = kalivas_expr_measures[i])) +
    labs(title = paste0(kalivas_expr_measures[i], "Before and After Self Admin By Cohort"))
  
  g_heroin_saline <- kalivas_expr_allcohorts_excel_processed_c01_08_df_tograph %>%
    mutate_at(vars(one_of(kalivas_expr_measures)), as.numeric) %>% 
    ggplot(aes(x = lever, fill = heroin_or_saline)) + 
    geom_boxplot(aes_string(y = kalivas_expr_measures[i])) + 
    facet_grid(~ cohort)+ theme(axis.text.x = element_text(angle = 45))
  
  
  g_cohort_sex <- kalivas_expr_allcohorts_excel_processed_c01_08_df_tograph %>%
    ggplot(aes(x = sex, fill = lever)) +
    geom_boxplot(aes_string(y = kalivas_expr_measures[i])) +
    facet_grid(~ cohort) + 
    labs(title = paste0(kalivas_expr_measures[i], "Before and After Self Admin By Cohort and Sex"))
  
  g_individual <- kalivas_expr_allcohorts_excel_processed_c01_08_df_tograph %>% 
    ggplot(aes(x = lever, group = rfid)) + 
    geom_line(aes_string(y = kalivas_expr_measures[i])) + 
    facet_grid(~ cohort) + 
    labs(title = paste0(kalivas_expr_measures[i], "Before and After Self Admin By Cohort and By Individual")) + 
    theme(axis.text.x = element_text(angle = 45))
  
  print(g_cohort)
  print(g_heroin_saline)
  print(g_cohort_sex)
  print(g_individual)
}

dev.off()



############################
# EXTINCTION  
############################

kalivas_ex_measures <- c("active_lever", "inactive_lever")
kalivas_ex_allcohorts_excel_processed_c01_08_df_tograph <- kalivas_ex_allcohorts_excel_processed_c01_08_df %>% 
  mutate_at(vars(one_of(kalivas_ex_measures)), as.numeric) %>% 
  mutate_at(vars(one_of(kalivas_ex_measures)), ~ replace(., resolution == "FLAG_EXPERIMENT", NA))



pdf("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC/plot_kalivas_ex_excel.pdf", onefile = T)
for (i in 1:(length(kalivas_ex_measures))){
  
  g_cohort <- kalivas_ex_allcohorts_excel_processed_c01_08_df_tograph %>%
    ggplot(aes(x = cohort, fill = session)) +
    geom_boxplot(aes_string(y = kalivas_ex_measures[i])) +
    labs(title = paste0(kalivas_ex_measures[i], "Before and After Self Admin By Cohort"))
  
  g_heroin_saline <- kalivas_ex_allcohorts_excel_processed_c01_08_df_tograph %>%
    mutate_at(vars(one_of(kalivas_ex_measures)), as.numeric) %>% 
    ggplot(aes(x = session, fill = heroin_or_saline)) + 
    geom_boxplot(aes_string(y = kalivas_ex_measures[i])) + 
    facet_grid(~ cohort)+ theme(axis.text.x = element_text(angle = 45))
  
  
  g_cohort_sex <- kalivas_ex_allcohorts_excel_processed_c01_08_df_tograph %>%
    ggplot(aes(x = sex, fill = session)) +
    geom_boxplot(aes_string(y = kalivas_ex_measures[i])) +
    facet_grid(~ cohort) + 
    labs(title = paste0(kalivas_ex_measures[i], "Before and After Self Admin By Cohort and Sex"))
  
  g_individual <- kalivas_ex_allcohorts_excel_processed_c01_08_df_tograph %>% 
    ggplot(aes(x = session, group = rfid)) + 
    geom_line(aes_string(y = kalivas_ex_measures[i])) + 
    facet_grid(~ cohort) + 
    labs(title = paste0(kalivas_ex_measures[i], "Before and After Self Admin By Cohort and By Individual")) + 
    theme(axis.text.x = element_text(angle = 45))
  
  print(g_cohort)
  print(g_heroin_saline)
  print(g_cohort_sex)
  print(g_individual)
}

dev.off()





############################
# CUED REINSTATEMENT
############################

kalivas_cued_measures <- c("active_lever", "inactive_lever")
kalivas_cued_allcohorts_excel_processed_c01_08_df_tograph <- kalivas_cued_allcohorts_excel_processed_c01_08_df %>% 
  mutate_at(vars(one_of(kalivas_cued_measures)), as.numeric) %>% 
  mutate_at(vars(one_of(kalivas_cued_measures)), ~ replace(., resolution == "FLAG_EXPERIMENT", NA))



pdf("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC/plot_kalivas_cued_excel.pdf", onefile = T)
for (i in 1:(length(kalivas_cued_measures))){
  
  g_cohort <- kalivas_cued_allcohorts_excel_processed_c01_08_df_tograph %>%
    ggplot(aes(x = cohort)) +
    geom_boxplot(aes_string(y = kalivas_cued_measures[i])) +
    labs(title = paste0(kalivas_cued_measures[i], "Before and After Self Admin By Cohort"))
  
  g_heroin_saline <- kalivas_cued_allcohorts_excel_processed_c01_08_df_tograph %>%
    mutate_at(vars(one_of(kalivas_cued_measures)), as.numeric) %>% 
    ggplot(aes(x = cohort, fill = heroin_or_saline)) + 
    geom_boxplot(aes_string(y = kalivas_cued_measures[i])) + 
    theme(axis.text.x = element_text(angle = 45))
  
  
  g_cohort_sex <- kalivas_cued_allcohorts_excel_processed_c01_08_df_tograph %>%
    ggplot(aes(x = cohort, fill = sex)) +
    geom_boxplot(aes_string(y = kalivas_cued_measures[i])) +
    labs(title = paste0(kalivas_cued_measures[i], "Before and After Self Admin By Cohort and Sex"))
  
  
  print(g_cohort)
  print(g_heroin_saline)
  print(g_cohort_sex)
}

dev.off()


