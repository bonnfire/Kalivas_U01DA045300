## SAVE CSV'S
# save master table files
write.csv(WFU_Kalivas_test_df, file = "~/Desktop/Database/csv files/u01_peter_kalivas_us/kalivasus_01_06_wfu_metadata.csv", row.names = F) 
write.csv(kalivas_07_wfu_metadata, file = "~/Desktop/Database/csv files/u01_peter_kalivas_us/kalivasus_07_wfu_metadata.csv", row.names = F)
write.csv(kalivas_08_wfu_metadata, file = "~/Desktop/Database/csv files/u01_peter_kalivas_us/kalivasus_08_wfu_metadata.csv", row.names = F)
write.csv(kalivas_09_wfu_metadata, file = "~/Desktop/Database/csv files/u01_peter_kalivas_us/kalivasus_09_wfu_metadata.csv", row.names = F)
write.csv(kalivas_10_wfu_metadata, file = "~/Desktop/Database/csv files/u01_peter_kalivas_us/kalivasus_10_wfu_metadata.csv", row.names = F)
write.csv(kalivas_11_wfu_metadata, file = "~/Desktop/Database/csv files/u01_peter_kalivas_us/kalivasus_11_wfu_metadata.csv", row.names = F)
write.csv(kalivas_12_wfu_metadata, file = "~/Desktop/Database/csv files/u01_peter_kalivas_us/kalivasus_12_wfu_metadata.csv", row.names = F)
write.csv(kalivas_13_wfu_metadata, file = "~/Desktop/Database/csv files/u01_peter_kalivas_us/kalivasus_13_wfu_metadata.csv", row.names = F)


## create csv for gwas rppr both us and italy

read.csv( "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/preliminary GWAS files/gwas_ustraits_rppr_n320.csv", colClasses = "character") %>%
  mutate(site = "us") %>% 
  bind_rows(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/preliminary GWAS files/gwas_italytraits_rppr_n400.csv", colClasses = "character") %>%
              mutate(site = "italy")) %>% 
  rename("labanimalid" = "internal_id",
         "extinction_active_presses_day6" = "active_presses_day6",
         "extinction_age" = "age_at_extinction",
         "exprimerein_active_presses_hour5_6" = "prime_active",
         "selfadmin_total_consumption" = "total_consumption",
         "selfadmin_age" = "age_at_consumption",
         "pr_breakpoint" = "breakpoint", 
         "pr_age" = "age_at_prbreakpoint", 
         "selfadmin_escalation" = "esc", 
         "cuedrein_activepresses" = "active_presses",
         "cuedrein_age" = "age_at_cued",
         "oft_distancetraveled_cm" = "distancetraveled_cm",
         "oft_timetraveled" = "timetraveled",
         "oft_rears" = "rears",
         "oft_age" = "age_at_oft",
         "epm_totaltimeopenarm_sec" = "totaltimeopenarm_sec",
         "epm_age" = "age_at_openarm") %>% 
  select(-age_at_esc) %>% 
  write.csv(file = "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/preliminary GWAS files/gwas_italyandustraits_rppr_n720.csv", row.names = F)
  
