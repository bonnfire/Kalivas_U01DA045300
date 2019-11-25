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


onlymins <-  grep(pattern = "^(min)", names(rawfiles_locomotor_wide_graph), perl = T, value = T)[-31]
onlymins_excel <- paste0(onlymins, "_excel")
onlymins_raw <- paste0(onlymins, "_raw")


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

