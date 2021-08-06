### qc raw vs italy data 

#####################
##### PRIMING ####### 
#####################

# join the raw vs excel data 
# sha_raw_df is saved as read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Olivier_George_U01DA043799 (Cocaine)/excel_and_csv_files/cocaine_sha_raw_c01_11_oldnewdirs.csv", stringsAsFactors = F)
primed_raw_df_long <- primed_presses_byhour_5_6_df_id %>% select(-box, -num_seq) %>% pivot_longer(cols = where(is.numeric), names_to = "measurement", values_to = "raw")

primed_xl_df_long <- kalivas_italy_priming_excel_c01_10_df %>% 
  subset(session %in% c("5", "6")) %>% 
  select(cohort, rfid, internal_id, heroin_salineyoked, saroom, measurement, session, value) %>%
  mutate(measurement = paste0(gsub("lever", "", measurement), "_hour", session)) %>% 
  select(-session) %>% 
  rename("xl" = "value")

kalivas_italy_prime_rawvsxl <- primed_raw_df_long %>% full_join(primed_xl_df_long, by = c("internal_id", "measurement")) %>% 
  mutate(xl = as.numeric(xl)) %>% 
  group_by(internal_id) %>% 
  fill(cohort) %>% # fill in info if some columns are missing from the excel
  fill(rfid) %>% 
  fill(heroin_salineyoked) %>% 
  fill(saroom) %>% 
  ungroup()

kalivas_italy_prime_rawvsxl %>% get_dupes(internal_id, measurement) %>% openxlsx::write.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/italy/QC files/prime_dupe_tobeqc_c03_09.xlsx")


kalivas_italy_prime_rawvsxl <- kalivas_italy_prime_rawvsxl %>% 
  mutate(QC_diff = xl - raw,
         QC = ifelse(QC_diff == 0&!is.na(QC_diff), "pass", "fail")) 
# %>% 
# naniar::vis_miss() # overall missingness
# select(QC) %>% table() %>% prop.table() # proportion of pass vs fail
# select(QC_diff) %>% summary # spread of differences

# most of the fails are from missing raw and excels
kalivas_italy_prime_rawvsxl %>% subset(QC == "fail") %>% naniar::vis_miss()
# kalivas_italy_prime_rawvsxl %>% ggplot(aes(x = raw, y = xl)) + geom_point() 
kalivas_italy_prime_rawvsxl %>% subset(!is.na(cohort)&cohort!="C10") %>% ggplot(aes(x = cohort, y = QC_diff)) + geom_boxplot()

# subset from the ones that failed to only include those that have QC diff values to ask the team to go over and check 
kalivas_italy_prime_rawvsxl %>% subset(QC == "fail"&!is.na(QC_diff)) %>% pivot_wider(names_from = measurement, values_from = c("raw", "xl", "QC_diff", "QC")) %>% 
  openxlsx::write.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/italy/QC files/prime_tobeqc_c03_09.xlsx")


kalivas_italy_prime_rawvsxl <- primed_presses_byhour_5_6_df_id %>% 
  select_all(~gsub("((inactive|active).*)", "\\1_raw", .)) %>% 
  full_join(kalivas_italy_priming_excel_c01_10_df %>% 
              subset(session %in% c("5", "6")) %>% 
              select(cohort, rfid, internal_id, heroin_salineyoked, saroom, measurement, session, value) %>%
              mutate(measurement = paste0(gsub("lever", "", measurement), "_hour", session, "_xl")) %>% 
              select(-session) %>% 
              spread(measurement, value), by = "internal_id") %>% 
  mutate_at(vars(matches("hour")), as.numeric) %>% 
  select(cohort, internal_id, rfid, sex, saroom, sabox, active_hour5_xl, active_hour5_raw, active_hour6_xl, active_hour6_raw, inactive_hour5_xl, inactive_hour5_raw, inactive_hour6_xl, inactive_hour6_raw, filename)

kalivas_italy_prime_rawvsxl %>% subset(cohort  == "C09") %>% tail()
kalivas_italy_prime_rawvsxl %>% subset(internal_id == "IT224")
kalivas_italy_prime_rawvsxl %>% subset(active_hour5_xl!=active_hour5_raw) 

# create long version
kalivas_italy_prime_rawvsxl_long <- kalivas_italy_prime_rawvsxl %>% 
  pivot_longer(active_hour5_xl:inactive_hour6_raw,
               names_to = c("session", "source"),
               names_pattern = "(active_hour\\d+)_(.*)",
               values_to = c("presses"))
  
  
  
  