### qc raw vs italy data 

#####################
##### PRIMING ####### 
#####################

# join the raw vs excel data 
primed_raw_df_long <- primed_presses_byhour_5_6_df_id %>% select(-box, -num_seq) %>% pivot_longer(cols = where(is.numeric), names_to = "measurement", values_to = "raw")

primed_xl_df_long <- kalivas_italy_priming_excel_c01_10_df %>% 
  subset(session %in% c("5", "6")) %>% 
  select(cohort, rfid, internal_id, heroin_salineyoked, saroom, measurement, session, date, value) %>%
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
  fill(date, .direction = "downup") %>% 
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


## calculate the gwas traits
kalivas_italy_prime_gwas <- kalivas_italy_prime_rawvsxl %>%
  subset(grepl("^active", measurement)) %>% 
  rename("labanimalid" = "internal_id") %>% 
  add_count(labanimalid, measurement) %>% 
  mutate(active_presses = ifelse(QC == "pass"&n==1, raw, NA)) %>% 
  distinct(cohort, rfid, labanimalid, sex, saroom, sabox, measurement, active_presses, date) %>% 
  spread(measurement, active_presses) %>% 
  rowwise() %>% 
  mutate(prime_active = sum(active_hour5, active_hour6, na.rm = F)) %>% 
  ungroup() %>% 
  distinct(cohort, rfid, labanimalid, sex, saroom, sabox, prime_active, date) %>% 
  rename("prime_date" = "date")
  

  
#####################
### CUED REIN ####### 
#####################

# join the raw vs excel data 
cued_raw_df_long <- relapse_raw_c03_09_df_id %>% select(-box, -num_seq) %>% pivot_longer(cols = where(is.numeric), names_to = "measurement", values_to = "raw")

# excel is already in long form  

kalivas_italy_cued_rawvsxl <- cued_raw_df_long %>% full_join(kalivas_italy_cued_excel_c01_10_df, by = c("labanimalid" = "internal_id", "cohort", "sex", "sabox")) %>% 
  mutate(xl = as.numeric(xl)) %>% 
  group_by(labanimalid) %>% 
  fill(cohort) %>% # fill in info if some columns are missing from the excel
  fill(rfid) %>% 
  fill(heroin_salineyoked) %>% 
  fill(saroom) %>% 
  ungroup()

kalivas_italy_cued_rawvsxl %>% get_dupes(labanimalid) %>% 
  openxlsx::write.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/italy/QC files/cued_dupe_tobeqc_c03_09.xlsx")


kalivas_italy_cued_rawvsxl <- kalivas_italy_cued_rawvsxl %>% 
  mutate(QC_diff = xl - raw,
         QC = ifelse(QC_diff == 0&!is.na(QC_diff), "pass", "fail")) 
# %>% 
# naniar::vis_miss() # overall missingness
# select(QC) %>% table() %>% prop.table() # proportion of pass vs fail
# select(QC_diff) %>% summary # spread of differences

# most of the fails are from missing raw and excels
kalivas_italy_cued_rawvsxl %>% subset(QC == "fail") %>% naniar::vis_miss()
# kalivas_italy_cued_rawvsxl %>% ggplot(aes(x = raw, y = xl)) + geom_point() 
# kalivas_italy_cued_rawvsxl %>% subset(!is.na(cohort)&cohort!="C10") %>% ggplot(aes(x = cohort, y = QC_diff)) + geom_boxplot()

# subset from the ones that failed to only include those that have QC diff values to ask the team to go over and check 
kalivas_italy_cued_rawvsxl %>% subset(QC == "fail"&!is.na(QC_diff)) %>% pivot_wider(names_from = measurement, values_from = c("raw", "xl", "QC_diff", "QC")) %>% 
  openxlsx::write.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/italy/QC files/cued_tobeqc_c03_09.xlsx")


## calculate the gwas traits
kalivas_italy_cued_gwas <- kalivas_italy_cued_rawvsxl %>%
  add_count(labanimalid) %>% 
  mutate(cued_active_presses = ifelse(QC == "pass"&n==1, raw, NA)) %>% 
  select(cohort, rfid, labanimalid, sex, saroom, sabox, cued_active_presses, date) %>% 
  rename("cued_date" = "date")


## combine to Beverly's to create full traits for Italy GWAS 
italy_gwas_traits <- oft_c01_10 %>% mutate(internal_id = parse_number(internal_id) %>% str_pad(3, "left", "0") %>% paste0("IT", .)) %>% 
  distinct(rfid, internal_id, distancetraveled_cm, timetraveled, rears, date_oft) %>% 
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
  rename("oft_distancetraveled_cm" = "distancetraveled_cm",
         "oft_timetraveled_sec" = "timetraveled",
         "oft_rears" = "rears", 
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
  
