# QC RAW VS EXCEL KALIVAS DATA

# using objects created in Raw folders 

####### long access 
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC")
# merge and graph 
# lga_allsubjects_tograph <- lga_allsubjects %>% head

lga_allsubjects_tograph <- left_join(kalivas_lga_allcohorts_excel_processed, lga_allsubjects[,c("internal_id", "session", "inactive_lever", "active_lever", "infusions", "filename")] , by = c("internal_id", "session"))
names(lga_allsubjects_tograph) <- mgsub::mgsub(names(lga_allsubjects_tograph), c("\\.x", "\\.y"), c("_excel", "_raw")) %>% gsub(" ", "", .)

# kalivas_lga_measures <- names(lga_allsubjects_tograph)[grepl("raw|excel", names(lga_allsubjects_tograph) )] 
kalivas_lga_measures <- c("active_lever_excel", "inactive_lever_excel", "infusions_excel", "active_lever_raw", "inactive_lever_raw", "infusions_raw") # for the correct order
lga_allsubjects_tograph <- lga_allsubjects_tograph %>% 
  mutate_at(kalivas_lga_measures, as.numeric)
# create plots 

pdf("kalivas_longaccess.pdf", onefile = T)
for (i in 1:(length(kalivas_lga_measures)/2)){
  g <-  ggplot(lga_allsubjects_tograph, aes_string(x = kalivas_lga_measures[i], y = kalivas_lga_measures[i+3])) + 
    geom_point() + 
    labs(title = paste0(kalivas_lga_measures[i], "_Raw_VS_Excel_U01_Kalivas", "\n")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  # g_cohort <-  ggplot(lga_allsubjects_tograph, aes_string(x = kalivas_lga_measures[i], y = kalivas_lga_measures[i+3])) + 
  #   geom_point(aes(color = cohort_number)) + 
  #   facet_grid(~ cohort_number)
  #   labs(title = paste0(kalivas_lga_measures[i], "_Raw_VS_Excel_U01_Kalivas", "\n")) + 
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  print(g)
  # print(g_cohort)
}

dev.off()

lga_allsubjects_tograph %>% 
  dplyr::filter(active_lever_excel != active_lever_raw) %>% select(rfid, sex, cohort_number, internal_id, session, active_lever_excel, active_lever_raw, filename)
lga_allsubjects_tograph %>% 
  dplyr::filter(inactive_lever_excel != inactive_lever_raw)
lga_allsubjects_tograph %>% 
  dplyr::filter(infusions_excel != infusions_raw)


lga_allsubjects_tocompare <- kalivas_lga_measures %>% gsub("_(raw|excel)", "", .) %>% unique %>% 
  map(~ lga_allsubjects_tograph %>% 
        select(matches(.x)) %>%
        reduce(`==`)) %>%
  set_names(paste0("isequal_", kalivas_lga_measures %>% gsub("_(raw|excel)", "", .) %>% unique)) %>%
  bind_cols(lga_allsubjects_tograph, .) 
# add the following line to subset data that don't match in all of the measures 
# %>% filter_at(vars(starts_with("isequal")), any_vars(. == FALSE))



####### progressive ratio 
# uniform variable session_length_hours, reinforcer, bolus_volume,ml, dose_ug_kg_infusion, reinforcement_schedule, time_out_seconds, discrete_stimulus

kalivas_pr_allcohorts_excel_processed_c01_09_df_tograph <- full_join(pr_raw_c01_09_df, kalivas_pr_allcohorts_excel_processed_c01_09_df, by = c("cohort", "internal_id"))
names(kalivas_pr_allcohorts_excel_processed_c01_09_df_tograph) <- mgsub::mgsub(names(kalivas_pr_allcohorts_excel_processed_c01_09_df_tograph), c("\\.x", "\\.y"), c("_raw", "_excel")) %>% gsub(" ", "", .)

kalivas_pr_measures <- names(kalivas_pr_allcohorts_excel_processed_c01_09_df_tograph)[grepl("raw|excel", names(kalivas_pr_allcohorts_excel_processed_c01_09_df_tograph) )] %>% sort()
kalivas_pr_allcohorts_excel_processed_c01_09_df_tograph <- kalivas_pr_allcohorts_excel_processed_c01_09_df_tograph %>% 
  mutate_at(kalivas_pr_measures, as.numeric)
# create plots 

pdf("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC/kalivas_pr.pdf", onefile = T)
for (i in seq(1, length(kalivas_pr_measures), 2)) {
  g <-  kalivas_pr_allcohorts_excel_processed_c01_09_df_tograph %>% 
    ggplot(aes_string(x = kalivas_pr_measures[i], y = kalivas_pr_measures[i+1])) +
    geom_point() +
    geom_abline(intercept = 0 , slope = 1) +
    labs(title = paste0(kalivas_pr_measures[i], "_Raw_VS_Excel_U01_Kalivas", "\n")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  # g_cohort <-  ggplot(lga_allsubjects_tograph, aes_string(x = kalivas_lga_measures[i], y = kalivas_lga_measures[i+3])) + 
  #   geom_point(aes(color = cohort_number)) + 
  #   facet_grid(~ cohort_number)
  #   labs(title = paste0(kalivas_lga_measures[i], "_Raw_VS_Excel_U01_Kalivas", "\n")) + 
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  print(g)
  # print(g_cohort)
}

dev.off()


pr_allsubjects_tocompare <- kalivas_pr_measures %>% gsub("_(raw|excel)", "", .) %>% unique %>% 
  map(~ kalivas_pr_allcohorts_excel_processed_c01_09_df_tograph %>% 
        select(matches(.x)) %>%
        reduce(`==`)) %>%
  set_names(paste0("isequal_", kalivas_pr_measures %>% gsub("_(raw|excel)", "", .) %>% unique)) %>%
  bind_cols(kalivas_pr_allcohorts_excel_processed_c01_09_df_tograph, .) 
# add the following line to subset data that don't match in all of the measures 
# %>% filter_at(vars(starts_with("isequal")), any_vars(. == FALSE))


## create excel to be qc'ed 
pr_qc <- pr_allsubjects_tocompare %>% 
  select(cohort, internal_id, room, compbox, matches("_(raw|excel)")) %>%
  pivot_longer(
    cols = inactive_lever_raw:current_ratio_excel,
    names_to = c("measurement", "value"),
    names_pattern = "(.*)_(raw|excel)",
    values_to = "count"
  ) %>% 
  spread(value, count) %>% 
  mutate(QC_diff = excel - raw,
         QC = ifelse(QC_diff == 0 | is.na(QC_diff), "pass", "fail"))
# (is.na(excel)&is.na(raw)&is.na(QC_diff)), "pass", ifelse(is.na(QC_diff)&(!is.na(excel)|!is.na(raw)), "fail", "fail"))) # XX come back to figure out 

pr_qc %>% 
  select(QC) %>% table(exclude = NULL) %>% prop.table()

pr_qc %>% 
  subset(QC == "fail") %>% 
  left_join(pr_allsubjects_tocompare %>%
              select(internal_id, filename, matches("comments")), by = c("internal_id")) %>% 
  openxlsx::write.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/QC files/pr_tobeqc_c01_09.xlsx")







####### extinction prime 

kalivas_expr_allcohorts_excel_processed_c01_09_df_tograph <- full_join(expr_raw_c01_09_df, kalivas_expr_allcohorts_excel_processed_c01_09_df_wide , by = c("cohort", "internal_id"))
names(kalivas_expr_allcohorts_excel_processed_c01_09_df_tograph) <- mgsub::mgsub(names(kalivas_expr_allcohorts_excel_processed_c01_09_df_tograph), c("\\.x", "\\.y"), c("_raw", "_excel")) %>% gsub(" ", "", .)

kalivas_expr_measures <- names(kalivas_expr_allcohorts_excel_processed_c01_09_df_tograph)[grepl("raw|excel", names(kalivas_expr_allcohorts_excel_processed_c01_09_df_tograph) )] %>% sort()
kalivas_expr_allcohorts_excel_processed_c01_09_df_tograph <- kalivas_expr_allcohorts_excel_processed_c01_09_df_tograph %>% 
  mutate_at(kalivas_expr_measures, as.numeric)
# create plots 

pdf("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC/kalivas_expr.pdf", onefile = T)
for (i in seq(1, length(kalivas_expr_measures), 2)) {
  g <-  kalivas_expr_allcohorts_excel_processed_c01_09_df_tograph %>% 
    ggplot(aes_string(x = kalivas_expr_measures[i], y = kalivas_expr_measures[i+1])) + 
    geom_point() + 
    geom_abline(intercept = 0 , slope = 1) +
    labs(title = paste0(kalivas_expr_measures[i], "_Raw_VS_Excel_U01_Kalivas", "\n")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  # g_cohort <-  ggplot(lga_allsubjects_tograph, aes_string(x = kalivas_lga_measures[i], y = kalivas_lga_measures[i+3])) + 
  #   geom_point(aes(color = cohort_number)) + 
  #   facet_grid(~ cohort_number)
  #   labs(title = paste0(kalivas_lga_measures[i], "_Raw_VS_Excel_U01_Kalivas", "\n")) + 
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  print(g)
  # print(g_cohort)
}

dev.off()


expr_allsubjects_tocompare <- kalivas_expr_measures %>% gsub("_(raw|excel)", "", .) %>% unique %>% 
  map(~ kalivas_expr_allcohorts_excel_processed_c01_09_df_tograph %>% 
        select(matches(.x)) %>%
        reduce(`==`)) %>%
  set_names(paste0("isequal_", kalivas_expr_measures %>% gsub("_(raw|excel)", "", .) %>% unique)) %>%
  bind_cols(kalivas_expr_allcohorts_excel_processed_c01_09_df_tograph, .) 
# add the following line to subset data that don't match in all of the measures 
# %>% filter_at(vars(starts_with("isequal")), any_vars(. == FALSE))

# pr_unverified <- list()
# for (i in seq(1, length(kalivas_pr_measures), 2)) {
#   pr_unverified <- pr_allsubjects_tograph %>% 
#     dplyr::filter(kalivas_pr_measures[i] != kalivas_pr_measures[i+1])
# }



## create excel to be qc'ed 
expr_qc <- expr_allsubjects_tocompare %>% 
  # left_join() %>% 
  select(cohort, internal_id, room, compbox, matches("hour"), -matches("isequal")) %>%
  pivot_longer(
    cols = hour_1_inactive_raw:hour_6_inactive_excel,
    names_to = c("day", "active_lever", "inactive_lever"),
    names_pattern = "(.*_.*)_(inactive|active)_(raw|excel)",
    values_to = "count"
  ) %>% 
  rename("value" = "inactive_lever") %>% 
  # mutate(value = paste0(day, "_", value)) %>% 
  # select(-day) %>% 
  rename("lever" = "day",
         "day" = "active_lever") %>% 
  spread(value, count) %>% 
  mutate(QC_diff = excel - raw,
         QC = ifelse(QC_diff == 0 | is.na(QC_diff), "pass", "fail"))
                       # (is.na(excel)&is.na(raw)&is.na(QC_diff)), "pass", ifelse(is.na(QC_diff)&(!is.na(excel)|!is.na(raw)), "fail", "fail"))) # XX come back to figure out 

expr_qc %>% 
  select(QC) %>% table(exclude = NULL) %>% prop.table()

expr_qc %>% 
  subset(QC == "fail") %>% 
  mutate(day = as.character(day)) %>% 
  left_join(expr_allsubjects_tocompare %>%
              select(internal_id, filename, matches("comments")), by = c("internal_id")) %>% 
  openxlsx::write.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/QC files/expr_tobeqc_c01_09.xlsx")







### EXTINCTION

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC")
 
# excel = kalivas_ex_allcohorts_excel_processed_c01_09_df_wide 
# raw = ext_raw_c01_09_df_wide

# uniform variable session_length_hours, reinforcer, bolus_volume,ml, dose_ug_kg_infusion, reinforcement_schedule, time_out_seconds, discrete_stimulus

# ex_allsubjects_tograph <- left_join( kalivas_ex_allcohorts_excel_processed_c01_09_df_wide, ext_raw_c01_09_df_wide[,c("internal_id", "inactive_lever", "active_lever", "session", "filename")] , by = c("internal_id")) ## doesn't look right because of the multiple inactive levers... and inaccurate
ex_allsubjects_tograph <- full_join(ext_raw_c01_09_df_wide, kalivas_ex_allcohorts_excel_processed_c01_09_df_wide, by = c("cohort", "internal_id"))
names(ex_allsubjects_tograph) <- mgsub::mgsub(names(ex_allsubjects_tograph), c("\\.x", "\\.y"), c("_raw", "_excel")) %>% gsub(" ", "", .)

kalivas_ex_measures <- names(ex_allsubjects_tograph)[grepl("raw|excel", names(ex_allsubjects_tograph) )] %>% sort()
ex_allsubjects_tograph <- ex_allsubjects_tograph %>% 
  mutate_at(kalivas_ex_measures, as.numeric)
# create plots 

pdf("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC/kalivas_ex.pdf", onefile = T)
for (i in seq(1, length(kalivas_ex_measures), 2)) {
  g <-  ex_allsubjects_tograph %>% 
    # dplyr::filter(!grepl("separate", comments)) %>% 
    ggplot(aes_string(x = kalivas_ex_measures[i], y = kalivas_ex_measures[i+1])) + 
    geom_point() + 
    geom_abline(intercept = 0 , slope = 1) +
    labs(title = paste0(kalivas_ex_measures[i], "_Raw_VS_Excel_U01_Kalivas", "\n")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  # g_cohort <-  ggplot(lga_allsubjects_tograph, aes_string(x = kalivas_lga_measures[i], y = kalivas_lga_measures[i+3])) + 
  #   geom_point(aes(color = cohort_number)) + 
  #   facet_grid(~ cohort_number)
  #   labs(title = paste0(kalivas_lga_measures[i], "_Raw_VS_Excel_U01_Kalivas", "\n")) + 
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  print(g)
  # print(g_cohort)
}

dev.off()


ex_allsubjects_tocompare <- kalivas_ex_measures %>% gsub("_(raw|excel)", "", .) %>% unique %>% 
  map(~ ex_allsubjects_tograph %>% 
        select(matches(.x)) %>%
        reduce(`==`)) %>%
  set_names(paste0("isequal_", kalivas_ex_measures %>% gsub("_(raw|excel)", "", .) %>% unique)) %>%
  bind_cols(ex_allsubjects_tograph, .)

# add the following line to subset data that don't match in all of the measures 
ex_allsubjects_tocompare %>% filter_at(vars(starts_with("isequal")), any_vars(. == FALSE))


## create excel to be qc'ed 
ex_qc <- ex_allsubjects_tocompare %>% 
  # left_join() %>% 
  select(cohort, internal_id, room, compbox, matches("lever"), -matches("isequal")) %>%
  pivot_longer(
    cols = inactive_lever_1_raw:inactive_lever_6_excel,
    names_to = c("day", "active_lever", "inactive_lever"),
    names_pattern = "(.*_.*)_(\\d+)_(.*)",
    values_to = "count"
  ) %>% 
  rename("value" = "inactive_lever") %>% 
  # mutate(value = paste0(day, "_", value)) %>% 
  # select(-day) %>% 
  rename("lever" = "day",
         "day" = "active_lever") %>% 
  spread(value, count) %>% 
  mutate(QC_diff = excel - raw,
         QC = ifelse(QC_diff == 0 | is.na(QC_diff), "pass", "fail"))
  
ex_qc %>% 
  select(QC) %>% table(exclude = NULL) %>% prop.table()

ex_qc %>% 
  subset(QC == "fail") %>% 
  mutate(day = as.character(day)) %>% 
  left_join(ext_raw_c01_09_df %>% 
              mutate(day = as.character(day)) %>% 
              select(internal_id, day, filename), by = c("internal_id", "day")) %>% 
  left_join(ex_allsubjects_tocompare %>% 
              select(internal_id, matches("comment")), by = "internal_id") %>% 
  openxlsx::write.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/QC files/ex_tobeqc_c01_09.xlsx")













##  Cued reinstatement

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC")

full_join(kalivas_cued_allcohorts_excel_processed_c01_09_df, cued_rein_raw_c01_09_df, by = "internal_id") %>% 
  naniar::vis_miss()

full_join(kalivas_cued_allcohorts_excel_processed_c01_09_df, cued_rein_raw_c01_09_df, by = "internal_id") %>% 
  subset(is.na(inactive_lever.y)&is.na(resolution)) %>% View()

rein_allsubjects

# uniform variable session_length_hours, reinforcer, bolus_volume,ml, dose_ug_kg_infusion, reinforcement_schedule, time_out_seconds, discrete_stimulus
## note that we are joining excel TO raw
cuedrein_allsubjects_tograph <- full_join(cued_rein_raw_c01_09_df, kalivas_cued_allcohorts_excel_processed_c01_09_df, by = c("internal_id", "cohort"))
names(cuedrein_allsubjects_tograph) <- mgsub::mgsub(names(cuedrein_allsubjects_tograph), c("\\.x", "\\.y"), c("_raw", "_excel")) %>% gsub(" ", "", .)

kalivas_rein_measures <- names(cuedrein_allsubjects_tograph)[grepl("raw|excel", names(cuedrein_allsubjects_tograph) )] %>% sort()
cuedrein_allsubjects_tograph <- cuedrein_allsubjects_tograph %>% 
  mutate_at(kalivas_rein_measures, as.numeric)
# create plots 

pdf("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC/kalivas_rein.pdf", onefile = T)
for (i in seq(1, length(kalivas_rein_measures), 2)) {
  g <-  cuedrein_allsubjects_tograph %>% 
    # dplyr::filter(!grepl("separate", comments)) %>% 
    ggplot(aes_string(x = kalivas_rein_measures[i], y = kalivas_rein_measures[i+1])) + 
    geom_point() + 
    geom_abline(intercept = 0 , slope = 1) +
    labs(title = paste0(kalivas_rein_measures[i], "_Raw_VS_Excel_U01_Kalivas", "\n")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  # g_cohort <-  ggplot(lga_allsubjects_tograph, aes_string(x = kalivas_lga_measures[i], y = kalivas_lga_measures[i+3])) + 
  #   geom_point(aes(color = cohort_number)) + 
  #   facet_grid(~ cohort_number)
  #   labs(title = paste0(kalivas_lga_measures[i], "_Raw_VS_Excel_U01_Kalivas", "\n")) + 
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  print(g)
  # print(g_cohort)
}

dev.off()


cued_rein_allsubjects_tocompare <- kalivas_rein_measures %>% gsub("_(raw|excel)", "", .) %>% unique %>% 
  map(~ cuedrein_allsubjects_tograph %>% 
        select(matches(.x)) %>%
        reduce(`==`)) %>%
  set_names(paste0("isequal_", kalivas_rein_measures %>% gsub("_(raw|excel)", "", .) %>% unique)) %>%
  bind_cols(cuedrein_allsubjects_tograph, .) 
# add the following line to subset data that don't match in all of the measures 
# %>% filter_at(vars(starts_with("isequal")), any_vars(. == FALSE))

# pr_unverified <- list()
# for (i in seq(1, length(kalivas_pr_measures), 2)) {
#   pr_unverified <- pr_allsubjects_tograph %>% 
#     dplyr::filter(kalivas_pr_measures[i] != kalivas_pr_measures[i+1])
# }



## create excel to be qc'ed 
cuedrein_qc <- cuedrein_allsubjects_tograph %>% 
  # left_join() %>% 
  select(cohort, internal_id, room, compbox, matches("lever")) %>%
  pivot_longer(
    cols = inactive_lever_raw:active_lever_excel,
    names_to = c("active_lever", "inactive_lever"),
    names_pattern = "(.*_.*)_(.*)",
    values_to = "count"
  ) %>% 
  rename("value" = "inactive_lever") %>% 
  # mutate(value = paste0(day, "_", value)) %>% 
  # select(-day) %>% 
  rename("lever" = "active_lever") %>% 
  spread(value, count) %>% 
  mutate(QC_diff = excel - raw,
         QC = ifelse(QC_diff == 0 | is.na(QC_diff), "pass", "fail"))

cuedrein_qc %>% 
  select(QC) %>% table(exclude = NULL) %>% prop.table()

cuedrein_qc %>% 
  subset(QC == "fail") %>% 
  left_join(cued_rein_allsubjects_tocompare %>% 
              select(internal_id, filename, matches("comment")), by = c("internal_id")) %>% 
  openxlsx::write.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/QC files/cuedrein_tobeqc_c01_09.xlsx")



































############################
# Exp 2: OPEN FIELD TASK
############################
# combine the data

kalivas_oft_graph_merge <- left_join(kalivas_oft_allcohorts_excel_processed, openfieldtask_raw_df_total, by = c("internal_id" = "labanimalid", "cohort_number" = "cohort", "session", "date"))
# note dim differences openfieldtask_excel_df_data 2053, raw 2147
# use eeptools::isid(openfieldtask_excel_df_data,  c("subject_id", "sample", "cage","date","time")) to find unique identifiers 
# verified TRUE FOR  eeptools::isid(openfieldtask_excel_df_data, c("subject_id", "sample", "cage","date","time"))
# verified TRUE FOR  eeptools::isid(as.data.frame(openfieldtask_raw_df),  c("subject_id", "sample", "cage","date","time"))
names(kalivas_oft_graph_merge) <- mgsub::mgsub(names(kalivas_oft_graph_merge),
                                                c("\\.x", "\\.y"),
                                                c("_excel", "_raw"))
kalivas_oft_measures <- kalivas_oft_graph_merge %>% select_if(is.numeric) %>% names()

# create plots 

pdf("kalivas_opentailflick.pdf", onefile = T)
for (i in 1:(length(kalivas_oft_measures)/2)){

  g <- ggplot(kalivas_oft_graph_merge, aes_string(x = kalivas_oft_measures[i], y = kalivas_oft_measures[i+length(kalivas_oft_measures)/2])) + 
    geom_point() + 
    labs(title = paste0(kalivas_oft_measures[i], "_Raw_VS_Excel_U01_Kalivas", "\n")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  print(g)
}

dev.off()

#### find the discrepancies 
# the raw data cannot be found for the excel files
naniar::vis_miss(kalivas_oft_graph_merge)
kalivas_oft_noraw <- kalivas_oft_graph_merge %>% 
  dplyr::filter(is.na(actfilename)) 

## send to kalivas lab
writexl::write_xlsx(kalivas_oft_noraw, "oft_noraw.xlsx") ## clarified on 11/26-- will wait for readme and excel files (before thxgi, xmas) # to do: MUSC (Analyse) email (11/26/19) 'n some of the ACT files the subject ID was either mislabeled or unlabeled when the experimental session was originally set up, due to either operator error or the animal needing to switch cages at the last minute after it was too late to change the subject ID. So every animals data is there but the subject ID number does not match the cage it was run in--we know which cage each animal is ultimately run in because we take notes of during each session and write down any unexpected changes or errors. Find information in This information is clarified in the README files and the comments section in the Excel book."

