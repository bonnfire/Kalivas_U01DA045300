library(lubridate)
#############################
# Protocol 
#############################

# setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/behavioral_tasks") #old code

# import all and then separate by tab
setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/addiction_related_behaviors/Raw_data_files")
u01.importxlsx <- function(xlname){
  path_sheetnames <- excel_sheets(xlname)
  df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname)
  names(df) <- path_sheetnames
  return(df)
} 
all_excel_fnames <- list.files(full.names = T, recursive = T)
kalivas_cohort2_excel <- u01.importxlsx(all_excel_fnames[1])
kalivas_cohort3_excel <- u01.importxlsx(all_excel_fnames[2])

# mapping files processing
kalivas_cohort2_mapping <- kalivas_cohort2_excel$info_from_breeder

names(kalivas_cohort2_mapping) <- c("dames", "sires", kalivas_cohort2_mapping[2,3:25]) %>% tolower()

datecols <- c("dob", "dow", "shipmentdate")
datefunction <- function(x){
  if(is.POSIXct(x) == F){
    as.POSIXct(as.numeric(x) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")
  } else x
}

kalivas_cohort2_mapping <- kalivas_cohort2_mapping[-c(1:4),] %>% 
  rename("dob" =  "date_of_birth", 
         "dow" = "date_of_wean", 
         "shipmentdate" = "date_of_ship") %>%  # date of ship = date of delivery so remove (kalivas_cohort2_mapping %>% subset(date_of_ship != date_of_delivery))
  select(-date_of_delivery) %>% 
  mutate_at(.vars = vars(datecols), .funs = datefunction)


kalivas_cohort3_mapping <- kalivas_cohort3_excel$info_from_breeder
names(kalivas_cohort3_mapping) <- c("dames", "sires", kalivas_cohort3_mapping[2,3:25]) %>% tolower()

kalivas_cohort3_mapping <- kalivas_cohort3_mapping[-c(1:4),] %>% 
  rename("dob" =  "date_of_birth", 
         "dow" = "date_of_wean", 
         "shipmentdate" = "date_of_ship") %>%  # date of ship = date of delivery so remove (kalivas_cohort3_mapping %>% subset(date_of_ship != date_of_delivery))
  select(-date_of_delivery) %>% 
  mutate_at(.vars = vars(datecols), .funs = datefunction)

kalivas_allcohorts <- rbind(kalivas_cohort2_mapping, kalivas_cohort3_mapping) %>% 
  mutate(cohort_number = gsub("MUSC_", "", cohort_number)) %>%
  rename("rfid" = "microchip")


############### extract for raw vs excel comparison
make_unique = function(x, sep='_'){
  ave(x, x, FUN=function(a){if(length(a) > 1){paste(a, 1:length(a), sep=sep)} else {a}})
}


### long access
kalivas_cohort2_lga_xl <- kalivas_cohort2_excel$LgA_SA[10:nrow(kalivas_cohort2_excel$LgA_SA),]
names(kalivas_cohort2_lga_xl) <- tolower(kalivas_cohort2_excel$LgA_SA[9,]) %>% make_unique()
kalivas_cohort2_lga_xl_sessiondosage <- kalivas_cohort2_excel$LgA_SA[1:8,]
kalivas_cohort2_lga_xl_long <- gather(kalivas_cohort2_lga_xl, var, value, inactive_lever:"resolution.14", factor_key=F) %>% 
  mutate(session = str_extract(var, "\\d+") %>% as.numeric(),
         session = ifelse(is.na(session), 1, session + 1))

kalivas_cohort2_lga_xl_long <- kalivas_cohort2_lga_xl %>%
  dplyr::filter(!is.na(microchip)) %>%  
  gather(var, value, -microchip, -sex, -bx_unit, -cohort_number, -internal_id, -group, -heroin_or_saline, -self_administration_room, -self_administration_box) %>%
  extract(var, c("measurement", "session"), "(\\D+_?)_(\\d+)") %>%
  spread(measurement, value) 


# function(x){
#   df <- 
#   return(df)
# }




# ############################
# # Exp 1: ELEVATED PLUS MAZE
# ############################
# setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/behavioral_tasks/elevated_plus_maze")
# 
# # all wmv files
# 
# 
# ############################
# # Exp 2: OPEN FIELD TASK
# ############################
# setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/behavioral_tasks/open_field_task")
# 
# # cohort 2 and 3 excel and ACT files
# 
# # read in the excel files 
# openfieldtask_files_excel <- list.files(path = ".", pattern = "*.xlsx", full.names = T, recursive = T) 
# openfieldtask_files_excel <- grep("cohort", openfieldtask_files_excel, ignore.case = T, value = T) # ignore the other xlsx file
# 
# # for(i in 1:length(openfieldtask_files)){
# #     assign(gsub("\\.xlsx", "", sapply(strsplit(openfieldtask_files_excel, "/"), "[", 3))[i], u01.importxlsx(openfieldtask_files[i]))
# # } # creates multiple objects for every excel file
# 
# ls()[sapply(ls(), function(i) class(get(i))) == "list"] # use this vector of these objects to make them into df's to be bound together in to a data frame cohort 2
# 
# 
# # create df with all excel values
# # not needed sapply(strsplit(openfieldtask_files_excel, "/"), "[", 3)
# openfieldtask_excel_df <- list()
# openfieldtask_excel_list <- lapply(openfieldtask_files_excel, read_excel)
# # since sapply(openfieldtask_excel_df, length) is all 1's, we won't need to use the u01.importxlsx function; sapply(openfieldtask_excel_list, dim)[2,] %>% summary shows that all dataframes have 46 vars
# names(openfieldtask_excel_list) <- str_match(openfieldtask_files_excel, "/.*/(.*?)\\.")[,2] ## XX what's up with this case "cohort03_subject_104_and_108_to_110)_OF2"
# 
# # naniar::vis_miss(rbindlist(openfieldtask_excel_list, fill = T)) there is some missingness that we should probably address but we can leave that for another time
# openfieldtask_excel_list_test <- lapply(openfieldtask_excel_list, function(x){
#   names(x) <-ifelse(grepl("^[[:alpha:]].*\\d$", names(x)), gsub("[.].*", "", names(x)), names(x)) # remove trailing dots and variable numbers
#   names(x) <- mgsub::mgsub(names(x),c("-| "), c("_")) %>% 
#     tolower() %>% 
#     make.unique(sep = ".")
#   x %<>% mutate(date = as.POSIXct(strptime(date,format="%d-%b-%Y")),
#          time = chron::chron(times = time))
#   return(x)
# })
# 
# # create the total tables extracted from the excel files
# openfieldtask_excel_df_total <- lapply(openfieldtask_excel_list_test, function(x){
#  x <- x %>% 
#     select(ends_with(".1")) %>% 
#     dplyr::filter(complete.cases(.))
#  return(x)
# }) %>% rbindlist(fill = T, idcol = "actfile")
# 
# openfieldtask_excel_df_data <- lapply(openfieldtask_excel_list_test, function(x){
#   x <- x %>% 
#     select(-matches("\\d$")) %>% 
#     dplyr::filter(grepl("^\\d", x$cage))
#   return(x)
# }) %>% rbindlist(fill = T, idcol = "cohort") %>% 
#   mutate(cohort = gsub("_.*", "", cohort),
#          cage = as.numeric(cage),
#          tempdegc = as.logical(tempdegc),
#          wheelrot = as.logical(wheelrot))
# 
# openfieldtask_excel_df_data[which(is.na(openfieldtask_excel_df_data$subject_id)),] ## XX found missing labanimalid - easy fix though, just need confirmation that i can
# 
# 
# 
# 
# 
# 
# # test for one case, eventually use this to create the cohort objects above 
# # change the variable names # extract the data and create the summary as a separate list item
# names(cohort02_group1_OF1$C2Group1OF1_C2G1OF1) <- ifelse(grepl("^[[:alpha:]].*\\d$", names(cohort02_group1_OF1$C2Group1OF1_C2G1OF1)), gsub("[.].*", "", names(cohort02_group1_OF1$C2Group1OF1_C2G1OF1)), names(cohort02_group1_OF1$C2Group1OF1_C2G1OF1)) 
# 
# names(cohort02_group1_OF1$C2Group1OF1_C2G1OF1) <- mgsub::mgsub(names(cohort02_group1_OF1$C2Group1OF1_C2G1OF1),c("-| "), c("_")) %>% 
#   tolower() %>% 
#   make.unique(sep = ".")
#   
# cohort02_group1_OF1_test <- list()
# cohort02_group1_OF1_test[[1]] <- cohort02_group1_OF1$C2Group1OF1_C2G1OF1 %>% 
#                            select(ends_with(".1")) %>% # creates the "total" table
#                             dplyr::filter(complete.cases(.)) 
# cohort02_group1_OF1_test[[2]] <- cohort02_group1_OF1$C2Group1OF1_C2G1OF1 %>% 
#   select(-matches("\\d$")) %>% 
#   mutate(date = as.Date(date, format='%d-%b-%Y') ,
#          time = chron::chron(times = time)) 
#   
# naniar::vis_miss(cohort02_group1_OF1_test[[2]]) # rather than visualizing the graph every time, # consider coding this ifelse in the future: return a comment if the percentages diff
# 
# cohort02_group1_OF1_test[[2]] <- cohort02_group1_OF1$C2Group1OF1_C2G1OF1 %>% 
#   select(-matches("\\d$")) %>%  # matches is the only select helper that allows for regular expressions
#   dplyr::filter(complete.cases(.)) %>% 
#   select(subject_id, everything()) %>%  # reorder the columns 
#   mutate(date = as.Date(date, format='%d-%b-%Y') ,
#          time = chron::chron(times = time))
# 
# nrow(cohort02_group1_OF1_test[[2]]) == n_distinct(cohort02_group1_OF1_test[[2]]$subject_id)
# # check the number of unique id matches expected values.
# 
# 
# 
#   
# ############################
# # Exp 3: TAIL FLICK
# ############################
# 
# 
# ############################
# # Exp 4: LONG ACCESS
# ############################
# longaccess_cohort2 <- kalivas_cohort2_excel[[6]] #27 rfid
# longaccess_cohort3 <- kalivas_cohort3_excel[[6]] #33 rfid
