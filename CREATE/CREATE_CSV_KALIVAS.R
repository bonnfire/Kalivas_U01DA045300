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

# prelim gwas data
write.csv(us_gwas_traits, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/us/generated/gwas_ustraits_n320_v2.csv", row.names = F)

## create csv for gwas rppr both us and italy
rbind(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/us/generated/gwas_ustraits_n320_v2.csv",stringsAsFactors = F) %>% 
        mutate(rfid = as.numeric(rfid) %>% as.character),
      read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/italy/generated/gwas_italytraits_n400_v2.csv",stringsAsFactors = F) %>% 
        mutate(rfid = as.numeric(rfid) %>% as.character)) %>% 
  write.csv(file = "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/preliminary GWAS files/gwas_italyandustraits_n720_v2.csv", row.names = F)

rbind(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/us/generated/gwas_ustraits_n320_v3.csv",stringsAsFactors = F) %>% 
        mutate(rfid = as.numeric(rfid) %>% as.character),
      read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/italy/generated/gwas_italytraits_n400_v3.csv",stringsAsFactors = F) %>% 
        mutate(rfid = as.numeric(rfid) %>% as.character)) %>% 
  write.csv(file = "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/preliminary GWAS files/gwas_italyandustraits_n720_v3.csv", row.names = F)
  
