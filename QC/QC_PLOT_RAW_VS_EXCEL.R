# QC RAW VS EXCEL KALIVAS DATA

# using objects created in Raw folders 

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
writexl::write_xlsx(kalivas_oft_noraw, "oft_noraw.xlsx")


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
kalivas_oft_total_compare <- left_join(openfieldtask_raw_df_total, openfieldtask_excel_df_total_forcompare, by = "actfilename")
kalivas_oft_total_compare %>% 
  dplyr::filter(is.na(cage.y))
