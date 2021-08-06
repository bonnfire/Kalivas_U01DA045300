### create summer temp files
compromised_rats_us <- kalivas_us_metadata_c01_09_df %>% subset(!(is.na(comments_behavunit)|is.na(resolution_behavunit))) %>% 
  select(rfid, comments_behavunit, resolution_behavunit)

write.csv(compromised_rats_us, "~/Desktop/Database/csv files/u01_peter_kalivas_us/compromised_rats_summer.csv", row.names = F)


# create compromised rats for italy 
kalivas_italy_surgery <- lapply(all_excel_fnames_c01_10, kalivas_italy_extract_shipping_metadata, "surgery") %>% sapply(.,'[')
kalivas_italy_surgery_df <- kalivas_italy_surgery %>% rbindlist(fill = T)

# fix compromised table 
kalivas_italy_surgery_df_summer <- kalivas_italy_surgery_df %>% 
  subset(grepl("DEA(TH|D)", notes)) %>% 
  mutate(rat_internal_id = coalesce(rat_internal_id, animal_id)) %>% 
  mutate(rat_internal_id = ifelse(!grepl("IT", rat_internal_id)&nchar(rat_internal_id)==2, gsub("(.*)", "IT0\\1", rat_internal_id), rat_internal_id)) %>% 
  mutate(rat_internal_id = ifelse(!grepl("IT", rat_internal_id)&nchar(rat_internal_id)==3, gsub("(.*)", "IT\\1", rat_internal_id), rat_internal_id)) %>% 
  rename("labanimalid" = "rat_internal_id") %>% 
  left_join(kalivas_italy_metadata_c01_10_df %>% 
              select(rfid, animal_id), by = c("labanimalid" = "animal_id")) %>% 
  select(rfid, notes)

write.csv(kalivas_italy_surgery_df_summer, "~/Desktop/Database/csv files/u01_peter_kalivas_italy/compromised_rats_summer.csv", row.names = F)

