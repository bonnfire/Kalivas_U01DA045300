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


## raw vs excel escalation
kalivas_lga_rawvsxl <- kalivas_lga_excel_c01_09_df %>% 
  subset(session %in% c("01", "02", "03", "10", "11", "12")) %>% 
  select(cohort, rfid, internal_id, session, infusions, date) %>% 
  full_join(lga_raw_c01_09_df %>% 
              subset(day %in% c("01", "02", "03", "10", "11", "12")) %>% ## remove if duplicates, come back afterwards
              rename("session" = "day") %>% 
              add_count(internal_id, session) %>%
              subset(n == 1) %>% 
              select(internal_id, session, infusions), by = c("internal_id", "session")) %>% 
  rename("raw" = "infusions.x",
         "xl" = "infusions.y") %>% 
  mutate(raw = as.numeric(raw)) 

kalivas_lga_rawvsxl <- kalivas_lga_rawvsxl %>% 
  mutate(QC_diff = xl - raw,
         QC = ifelse(QC_diff == 0&!is.na(QC_diff), "pass", "fail")) 

## calculate the gwas traits
kalivas_us_esc_gwas <- kalivas_lga_rawvsxl %>%
  mutate(infusions = ifelse(QC == "pass", raw, NA),
         group = ifelse(session %in% c("01", "02", "03"), "start", "end"),
         lga_01_date = ifelse(session == "01", date, NA)) %>%
  group_by(internal_id) %>% 
  fill(lga_01_date) %>% 
  ungroup() %>% 
  group_by(internal_id, group) %>% 
  mutate(mean_infusions = mean(infusions, na.rm = F)) %>% 
  ungroup() %>% 
  subset(session %in% c("01", "10")) %>% 
  distinct(cohort, rfid, internal_id, lga_01_date, group, mean_infusions) %>% 
  spread(group, mean_infusions) %>% 
  rowwise() %>% 
  mutate(lga_escalation = end - start) %>% ungroup %>% 
  distinct(cohort, rfid, internal_id, lga_01_date, lga_escalation)



# for total consumption
kalivas_lga_rawvsxl_2 <- kalivas_lga_excel_c01_09_df %>% 
  select(cohort, rfid, internal_id, session, infusions, date) %>% 
  full_join(lga_raw_c01_09_df %>% 
              rename("session" = "day") %>% 
              add_count(internal_id, session) %>%
              subset(n == 1) %>% 
              select(internal_id, session, infusions), by = c("internal_id", "session")) %>% 
  rename("raw" = "infusions.x",
         "xl" = "infusions.y") %>% 
  mutate(raw = as.numeric(raw)) 

kalivas_lga_rawvsxl_2 <- kalivas_lga_rawvsxl_2 %>% 
  mutate(QC_diff = xl - raw,
         QC = ifelse(QC_diff == 0&!is.na(QC_diff), "pass", "fail")) 

## calculate the gwas traits
kalivas_us_consu_gwas <- kalivas_lga_rawvsxl_2 %>%
  mutate(infusions = ifelse(QC == "pass", raw, NA)) %>%
  group_by(internal_id) %>% 
  mutate(lga_total_consumption = sum(infusions, na.rm = F)) %>% 
  ungroup() %>% 
  distinct(cohort, rfid, internal_id, lga_total_consumption) 


## raw vs excel consumption


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
         QC = ifelse(QC_diff == 0&!is.na(QC_diff), "pass", "fail")) 
# (is.na(excel)&is.na(raw)&is.na(QC_diff)), "pass", ifelse(is.na(QC_diff)&(!is.na(excel)|!is.na(raw)), "fail", "fail"))) # XX come back to figure out 

pr_qc %>% 
  select(QC) %>% table(exclude = NULL) %>% prop.table()

pr_qc %>% 
  subset(QC == "fail") %>% 
  left_join(pr_allsubjects_tocompare %>%
              select(internal_id, filename, matches("comments")), by = c("internal_id")) %>% 
  openxlsx::write.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/QC files/pr_tobeqc_c01_09.xlsx")



## raw vs excel 
kalivas_pr_rawvsxl <- kalivas_pr_excel_c01_09_df %>% 
  select(cohort, rfid, internal_id, pr_step, date) %>% 
  full_join(pr_raw_c01_09_df %>% 
              add_count(internal_id) %>%
              subset(n == 1) %>% 
              select(internal_id, pr_step), by = c("internal_id")) %>% 
  rename("raw" = "pr_step.x",
         "xl" = "pr_step.y") %>% 
  mutate(raw = as.numeric(raw),
         xl = as.numeric(xl)) 

kalivas_pr_rawvsxl <- kalivas_pr_rawvsxl %>% 
  mutate(QC_diff = xl - raw,
         QC = ifelse(QC_diff == 0&!is.na(QC_diff), "pass", "fail")) 

## calculate the gwas traits
kalivas_us_pr_gwas <- kalivas_pr_rawvsxl %>%
  mutate(pr_breakpoint = ifelse(QC == "pass", raw, NA)) %>%
  rename("pr_date" = "date") %>% 
  distinct(cohort, rfid, internal_id, pr_date, pr_breakpoint)





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




## raw vs excel 
# focus only on the hours 5 and 6 active lever presses
expr_raw_c01_09_df_long <-  expr_raw_c01_09_df %>% 
  select(cohort, internal_id, matches("([56])_active")) %>%
  gather("session", "active_presses", -cohort, -internal_id) %>% 
  mutate(session = gsub("_active", "", session))

kalivas_expr_excel_c01_09_df_long <- kalivas_expr_excel_c01_09_df %>% 
  subset(grepl("^active", lever)) %>% 
  select(cohort, rfid, internal_id, date, hour_5, hour_6) %>% 
  gather("session", "active_presses", -cohort, -rfid, -internal_id, -date)

  
kalivas_prime_rawvsxl <- kalivas_expr_excel_c01_09_df_long %>% 
  full_join(expr_raw_c01_09_df_long %>% 
              add_count(internal_id, session) %>%
              subset(n == 1) %>% 
              select(-n, -cohort), by = c("internal_id", "session")) %>% 
  rename("raw" = "active_presses.y",
         "xl" = "active_presses.x") %>% 
  mutate(raw = as.numeric(raw),
         xl = as.numeric(xl)) 

kalivas_prime_rawvsxl <- kalivas_prime_rawvsxl %>% 
  mutate(QC_diff = xl - raw,
         QC = ifelse(QC_diff == 0&!is.na(QC_diff), "pass", "fail")) 

## calculate the gwas traits
kalivas_us_prime_gwas <- kalivas_prime_rawvsxl %>%
  mutate(presses = ifelse(QC == "pass", raw, NA)) %>%
  rename("prime_date" = "date") %>% 
  distinct(cohort, rfid, internal_id, prime_date, presses, session) %>% 
  spread(session, presses) %>% 
  rowwise() %>% 
  mutate(prime_active = sum(hour_5, hour_6, na.rm = F)) %>% 
  ungroup() %>% 
  distinct(cohort, rfid, internal_id, prime_date, prime_active)




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





## raw vs excel 
# focus only on the day 6 active lever presses
kalivas_ex_rawvsxl <- kalivas_ex_excel_c01_09_df %>% 
  subset(session == "6") %>% 
  select(cohort, rfid, internal_id, active_lever, date) %>% 
  full_join(ext_raw_c01_09_df %>% 
              subset(day == 6) %>% 
              add_count(internal_id) %>%
              subset(n == 1) %>% 
              select(internal_id, active_lever), by = c("internal_id")) %>% 
  rename("raw" = "active_lever.y",
         "xl" = "active_lever.x") %>% 
  mutate(raw = as.numeric(raw),
         xl = as.numeric(xl)) 

kalivas_ex_rawvsxl <- kalivas_ex_rawvsxl %>% 
  mutate(QC_diff = xl - raw,
         QC = ifelse(QC_diff == 0&!is.na(QC_diff), "pass", "fail")) 

## calculate the gwas traits
kalivas_us_ex_gwas <- kalivas_ex_rawvsxl %>%
  mutate(ext_active_presses_day6 = ifelse(QC == "pass", raw, NA)) %>%
  rename("ext_date" = "date") %>% 
  distinct(cohort, rfid, internal_id, ext_date, ext_active_presses_day6)










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





## raw vs excel 
kalivas_cued_rawvsxl <- kalivas_cued_excel_c01_09_df %>%
  select(cohort, rfid, internal_id, active_lever, date) %>% 
  full_join( cued_rein_raw_c01_09_df %>% 
              add_count(internal_id) %>%
              subset(n == 1) %>% 
              select(internal_id, active_lever), by = c("internal_id")) %>% 
  rename("raw" = "active_lever.y",
         "xl" = "active_lever.x") %>% 
  mutate(raw = as.numeric(raw),
         xl = as.numeric(xl)) 

kalivas_cued_rawvsxl <- kalivas_cued_rawvsxl %>% 
  mutate(QC_diff = xl - raw,
         QC = ifelse(QC_diff == 0&!is.na(QC_diff), "pass", "fail")) 

## calculate the gwas traits
kalivas_us_cued_gwas <- kalivas_cued_rawvsxl %>%
  mutate(cued_active_presses = ifelse(QC == "pass", raw, NA)) %>%
  rename("cued_date" = "date") %>% 
  distinct(cohort, rfid, internal_id, cued_date, cued_active_presses) 





## create gwas object for all (using raw vs excel data)

# join to postgresql database 
# library("RPostgreSQL") 
# library("DBI")
con <- dbConnect(dbDriver("PostgreSQL"), dbname="PalmerLab_Datasets",user="postgres",password="postgres")
kalivas_us_metadata_db <- dbGetQuery(con, "select * from \"u01_peter_kalivas_us\".\"wfu_master\"")


us_gwas_traits <- kalivas_oft_excel_c01_09_df %>% 
  subset(grepl("before", session)) %>% 
  distinct(rfid, internal_id, total_cm_traveled, total_time_traveled_seconds, number_of_rears, date) %>% 
  rename("date_oft" = "date") %>% 
  left_join(kalivas_epm_excel_c01_09_df %>% 
              subset(grepl("before", session)) %>% 
              distinct(rfid, open_arm_time_seconds, date) %>% 
              rename("date_openarm" = "date"), by = "rfid") %>% 
  left_join(kalivas_us_esc_gwas %>% distinct(rfid, lga_01_date, lga_escalation), by = c("rfid")) %>% #left join when number of rfid's match
  left_join(kalivas_us_consu_gwas %>% distinct(rfid, lga_total_consumption), by = c("rfid")) %>% #left join when number of rfid's match
  left_join(kalivas_us_cued_gwas %>% distinct(rfid, cued_date, cued_active_presses), by = c("rfid")) %>% #left join when number of rfid's match
  left_join(kalivas_us_ex_gwas %>% distinct(rfid, ext_date, ext_active_presses_day6), by = c("rfid")) %>% #left join when number of rfid's match
  left_join(kalivas_us_pr_gwas %>% distinct(rfid, pr_date, pr_breakpoint), by = c("rfid")) %>% #left join when number of rfid's match
  left_join(kalivas_us_prime_gwas %>% distinct(rfid, prime_date, prime_active), by = c("rfid")) %>% #left join when number of rfid's match
  rename("oft_distancetraveled_cm" = "total_cm_traveled",
         "oft_timetraveled_sec" = "total_time_traveled_seconds",
         "oft_rears" = "number_of_rears", 
         "oft_date" = "date_oft", 
         "epm_totaltimeopenarm_sec" = "open_arm_time_seconds",
         "epm_date" = "date_openarm", 
         "labanimalid" = "internal_id") %>% 
  left_join(kalivas_us_metadata_db %>% 
              select(rfid, cohort, sex, coatcolor, dob), by = "rfid") %>% 
  mutate(site = "us") %>%  
  mutate_at(vars(matches("date")), ~difftime(., dob, units = c("days")) %>% as.numeric() %>% round) %>% # calculate the age at exp
  setNames(gsub("date", "age", names(.))) %>%
  left_join(kalivas_us_box_weight, by = "rfid") %>% 
  subset(parse_number(cohort) < 10) %>% 
  select(site, cohort, rfid, labanimalid, sex, saroom, sabox, coatcolor, prime_age, ext_age, lga_01_age, pr_age, cued_age, oft_age, epm_age, surgery_weight, ext_active_presses_day6, prime_active, lga_total_consumption, pr_breakpoint, lga_escalation, cued_active_presses, oft_distancetraveled_cm, oft_timetraveled_sec, oft_rears, epm_totaltimeopenarm_sec)


us_gwas_traits %>% naniar::vis_miss()
us_gwas_traits %>% get_dupes(labanimalid)
us_gwas_traits %>% get_dupes(rfid)
write.csv(us_gwas_traits, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/us/generated/gwas_ustraits_n320_v2.csv", row.names = F)
















## combine to Beverly's to create full traits for Italy GWAS 
italy_gwas_traits <- oft_c01_10 %>% mutate(internal_id = parse_number(internal_id) %>% str_pad(3, "left", "0") %>% paste0("IT", .)) %>% 
  distinct(rfid, internal_id, total_cm_traveled, total_time_traveled_seconds, number_of_rears, date) %>% 
  rename("date_oft" = "date") %>% 
  left_join(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/italy/generated/Italy_Gwas_Traits.csv", stringsAsFactors =  F) %>% 
              mutate(rfid=as.numeric(rfid) %>% as.character) %>% 
              naniar::replace_with_na_all(condition = ~.x %in% c("None")) %>% 
              select_if(function(x) any(!is.na(x))), by = "internal_id") %>% # remove columns that have all na's
  mutate(rfid = ifelse(rfid.x != rfid.y&!is.na(rfid.y), rfid.y, rfid.x)) %>% 
  rename("labanimalid" = "internal_id") %>% 
  left_join(kalivas_italy_prime_gwas %>% distinct(rfid, prime_active, prime_date, saroom, sabox), by = c("rfid")) %>% #left join when number of rfid's match
  left_join(kalivas_italy_cued_gwas %>% distinct(rfid, cued_active_presses, cued_date), by = c("rfid")) %>% # full join when number of rfid's don't match
  # join to the excel only data
  left_join(openarm_epm_c01_10 %>%
              mutate(rfid=as.numeric(rfid) %>% as.character) %>%
              left_join(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/italy/generated/Italy_Gwas_Traits.csv", stringsAsFactors =  F) %>% 
                          mutate(rfid=as.numeric(rfid) %>% as.character) %>% 
                          naniar::replace_with_na_all(condition = ~.x %in% c("None")) %>% 
                          select_if(function(x) any(!is.na(x))), by = "internal_id") %>% # remove columns that have all na's
              mutate(rfid = ifelse(rfid.x != rfid.y&!is.na(rfid.y), rfid.y, rfid.x)) %>% 
              distinct(rfid, totaltimeopenarm_sec, date_openarm), by = c("rfid")) %>% 
  rename("oft_distancetraveled_cm" = "total_cm_traveled",
         "oft_timetraveled_sec" = "total_time_traveled_seconds",
         "oft_rears" = "number_of_rears", 
         "oft_date" = "date_oft", 
         "epm_totaltimeopenarm_sec" = "totaltimeopenarm_sec",
         "epm_date" = "date_openarm") %>% 
  select(-sex, -dob, -rfid.x, -rfid.y, -cohort, -site, -coatcolor) %>% 
  left_join(kalivas_metadata_db %>% 
              select(rfid, cohort, sex, coatcolor, dob), by = "rfid") %>% 
  mutate(site = "italy") %>%  
  mutate_at(vars(matches("date")), ~difftime(., dob, units = c("days")) %>% as.numeric() %>% round) %>% # calculate the age at exp
  setNames(gsub("date", "age", names(.))) %>% 
  select(site, cohort, rfid, labanimalid, sex, saroom, sabox, coatcolor, prime_age, ext_age, lga_01_age, pr_age, cued_age, oft_age, epm_age, surgery_weight, ext_active_presses_day6, prime_active, lga_total_consumption, pr_breakpoint, lga_escalation, cued_active_presses, oft_distancetraveled_cm, oft_timetraveled_sec, oft_rears, epm_totaltimeopenarm_sec)

italy_gwas_traits %>% naniar::vis_miss()
italy_gwas_traits %>% get_dupes(labanimalid)
write.csv(italy_gwas_traits, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/italy/generated/gwas_italytraits_n400_v2.csv", row.names = F)














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

