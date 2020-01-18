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
  dplyr::filter(active_lever_excel != active_lever_raw)
lga_allsubjects_tograph %>% 
  dplyr::filter(inactive_lever_excel != inactive_lever_raw)
lga_allsubjects_tograph %>% 
  dplyr::filter(infusions_excel != infusions_raw)


####### progressive ratio 
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC")

# uniform variable session_length_hours, reinforcer, bolus_volume,ml, dose_ug_kg_infusion, reinforcement_schedule, time_out_seconds, discrete_stimulus

pr_allsubjects_tograph <- left_join(kalivas_pr_allcohorts_excel_processed, pr_allsubjects[,c("internal_id", "pr_step", "total_session_minutes", "inactive_lever", "active_lever", "infusions", "current_ratio", "filename")] , by = "internal_id")
names(pr_allsubjects_tograph) <- mgsub::mgsub(names(pr_allsubjects_tograph), c("\\.x", "\\.y"), c("_excel", "_raw")) %>% gsub(" ", "", .)

kalivas_pr_measures <- names(pr_allsubjects_tograph)[grepl("raw|excel", names(pr_allsubjects_tograph) )] %>% sort()
pr_allsubjects_tograph <- pr_allsubjects_tograph %>% 
  mutate_at(kalivas_pr_measures, as.numeric)
# create plots 

pdf("kalivas_pr.pdf", onefile = T)
for (i in seq(1, length(kalivas_pr_measures), 2)) {
  g <-  pr_allsubjects_tograph %>% 
    mutate(is_same_total_mins = if_else(total_session_minutes_excel == total_session_minutes_raw, "yes", "no")) %>% 
    ggplot(aes_string(x = kalivas_pr_measures[i], y = kalivas_pr_measures[i+1])) + 
    geom_point(aes(color = is_same_total_mins)) + 
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
  map(~ pr_allsubjects_tograph %>% 
        select(matches(.x)) %>%
        reduce(`==`)) %>%
  set_names(paste0("isequal_", kalivas_pr_measures %>% gsub("_(raw|excel)", "", .) %>% unique)) %>%
  bind_cols(pr_allsubjects_tograph, .) 
# add the following line to subset data that don't match in all of the measures 
# %>% filter_at(vars(starts_with("isequal")), any_vars(. == FALSE))

# pr_unverified <- list()
# for (i in seq(1, length(kalivas_pr_measures), 2)) {
#   pr_unverified <- pr_allsubjects_tograph %>% 
#     dplyr::filter(kalivas_pr_measures[i] != kalivas_pr_measures[i+1])
# }



####### extinction prime 
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC")

# XX NOT UPDATED uniform variable session_length_hours, reinforcer, bolus_volume,ml, dose_ug_kg_infusion, reinforcement_schedule, time_out_seconds, discrete_stimulus

expr_allsubjects_tograph <- left_join(kalivas_expr_allcohorts_excel_processed, expr_allsubjects[,c("internal_id", "lever", paste0("hour_", 1:6), "filename")] , by = c("internal_id", "lever"))
names(expr_allsubjects_tograph) <- mgsub::mgsub(names(expr_allsubjects_tograph), c("\\.x", "\\.y"), c("_excel", "_raw")) %>% gsub(" ", "", .)

kalivas_expr_measures <- names(expr_allsubjects_tograph)[grepl("raw|excel", names(expr_allsubjects_tograph) )] %>% sort()
expr_allsubjects_tograph <- expr_allsubjects_tograph %>% 
  mutate_at(kalivas_expr_measures, as.numeric)
# create plots 

pdf("kalivas_expr.pdf", onefile = T)
for (i in seq(1, length(kalivas_expr_measures), 2)) {
  g <-  expr_allsubjects_tograph %>% 
    dplyr::filter(!grepl("separate", comments)) %>% 
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
  map(~ expr_allsubjects_tograph %>% 
        select(matches(.x)) %>%
        reduce(`==`)) %>%
  set_names(paste0("isequal_", kalivas_expr_measures %>% gsub("_(raw|excel)", "", .) %>% unique)) %>%
  bind_cols(expr_allsubjects_tograph, .) 
# add the following line to subset data that don't match in all of the measures 
# %>% filter_at(vars(starts_with("isequal")), any_vars(. == FALSE))

# pr_unverified <- list()
# for (i in seq(1, length(kalivas_pr_measures), 2)) {
#   pr_unverified <- pr_allsubjects_tograph %>% 
#     dplyr::filter(kalivas_pr_measures[i] != kalivas_pr_measures[i+1])
# }


##  Extinction

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Kalivas_U01DA045300/QC")

kalivas_expr_allcohorts_excel_processed 
expr_allsubjects

# uniform variable session_length_hours, reinforcer, bolus_volume,ml, dose_ug_kg_infusion, reinforcement_schedule, time_out_seconds, discrete_stimulus

expr_allsubjects_tograph <- left_join(kalivas_expr_allcohorts_excel_processed, expr_allsubjects[,c("internal_id", "lever", paste0("hour_", 1:6), "filename")] , by = c("internal_id", "lever"))
names(expr_allsubjects_tograph) <- mgsub::mgsub(names(expr_allsubjects_tograph), c("\\.x", "\\.y"), c("_excel", "_raw")) %>% gsub(" ", "", .)

kalivas_expr_measures <- names(expr_allsubjects_tograph)[grepl("raw|excel", names(expr_allsubjects_tograph) )] %>% sort()
expr_allsubjects_tograph <- expr_allsubjects_tograph %>% 
  mutate_at(kalivas_expr_measures, as.numeric)
# create plots 

pdf("kalivas_expr.pdf", onefile = T)
for (i in seq(1, length(kalivas_expr_measures), 2)) {
  g <-  expr_allsubjects_tograph %>% 
    dplyr::filter(!grepl("separate", comments)) %>% 
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
  map(~ expr_allsubjects_tograph %>% 
        select(matches(.x)) %>%
        reduce(`==`)) %>%
  set_names(paste0("isequal_", kalivas_expr_measures %>% gsub("_(raw|excel)", "", .) %>% unique)) %>%
  bind_cols(expr_allsubjects_tograph, .) 
# add the following line to subset data that don't match in all of the measures 
# %>% filter_at(vars(starts_with("isequal")), any_vars(. == FALSE))

# pr_unverified <- list()
# for (i in seq(1, length(kalivas_pr_measures), 2)) {
#   pr_unverified <- pr_allsubjects_tograph %>% 
#     dplyr::filter(kalivas_pr_measures[i] != kalivas_pr_measures[i+1])
# }





















############################
# Exp 2: OPEN FIELD TASK
############################
# combine the data
# XX temporarily add the subject_id
openfieldtask_excel_df_data[which(is.na(openfieldtask_excel_df_data$subject_id)),]$subject_id <- "KAL056"

kalivas_oft_graph_merge <- left_join(openfieldtask_excel_df_data, openfieldtask_raw_df[,-c("actfilename", "vmx")], by = c("subject_id", "sample", "cage","date","time"))
# note dim differences openfieldtask_excel_df_data 2053, raw 2147
# use eeptools::isid(openfieldtask_excel_df_data,  c("subject_id", "sample", "cage","date","time")) to find unique identifiers 
# verified TRUE FOR  eeptools::isid(openfieldtask_excel_df_data, c("subject_id", "sample", "cage","date","time"))
# verified TRUE FOR  eeptools::isid(as.data.frame(openfieldtask_raw_df),  c("subject_id", "sample", "cage","date","time"))
names(kalivas_oft_graph_merge) <- mgsub::mgsub(names(kalivas_oft_graph_merge),
                                                c("\\.x", "\\.y"),
                                                c("_excel", "_raw"))
kalivas_oft_measures <- names(kalivas_oft_graph_merge)[grepl("_", names(kalivas_oft_graph_merge) )] 
kalivas_oft_measures <- grep("[^subject_id]", kalivas_oft_measures, value = T, perl  =T)

# move the vector to make the same order
kalivas_oft_measures <- setdiff(kalivas_oft_measures, "cohort_raw") ## Use `match` to find the point at which to insert "cohort_raw" ## Use `append` to insert "cohort_raw" at the relevant point
kalivas_oft_measures <- append(kalivas_oft_measures, values = "cohort_raw", after = match("pri_samples_excel", kalivas_oft_measures))

# create plots 

pdf("kalivas_opentailflick.pdf", onefile = T)
for (i in 1:(length(kalivas_oft_measures)/2)){

  g <- ggplot(kalivas_oft_graph_merge, aes_string(x = kalivas_oft_measures[i], y = kalivas_oft_measures[i+33])) + 
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
  dplyr::filter(is.na(cohort_raw)) %>% 
  mutate(date = as.character(date),
         time = as.character(time)) %>% 
  select(filename_excel, subject_id, sample, cage, date, time, actfilename) 

## send to kalivas lab
writexl::write_xlsx(kalivas_oft_noraw, "oft_noraw.xlsx") ## clarified on 11/26-- will wait for readme and excel files (before thxgi, xmas) # to do: MUSC (Analyse) email (11/26/19) 'n some of the ACT files the subject ID was either mislabeled or unlabeled when the experimental session was originally set up, due to either operator error or the animal needing to switch cages at the last minute after it was too late to change the subject ID. So every animals data is there but the subject ID number does not match the cage it was run in--we know which cage each animal is ultimately run in because we take notes of during each session and write down any unexpected changes or errors. Find information in This information is clarified in the README files and the comments section in the Excel book."



## compare the total values by grouping by actfile and by cage
openfieldtask_excel_df_total_forcompare <- openfieldtask_excel_df_total %>% 
  rename("actfilename" = "actfile")
names(openfieldtask_excel_df_total_forcompare) <- gsub("\\.\\d", "", names(openfieldtask_excel_df_total_forcompare))
openfieldtask_raw_df_total <- openfieldtask_raw_df %>% group_by(actfilename, cage, date, time) %>%
  summarise_at(c("totdist", "movtime", "strno", "ctrtime","rmovno"), sum) %>% 
  ungroup() %>% 
  select(-c(date, time)) %>% 
  mutate(actfilename = str_match(actfilename, "/.*/(.*?)_raw*.")[,2])
openfieldtask_excel_df_total_forcompare
openfieldtask_raw_df_total

# openfieldtask_raw_df_total has 188 observations vs 181 from excel
kalivas_oft_total_compare <- left_join(openfieldtask_raw_df_total, openfieldtask_excel_df_total_forcompare, by = c("actfilename", "cage"))
kalivas_oft_total_compare %>% 
  dplyr::filter(is.na(totdist.excel))
names(kalivas_oft_total_compare) <- mgsub::mgsub(names(kalivas_oft_total_compare), c("\\.x", "\\.y"), c("\\.raw", "\\.excel"))

