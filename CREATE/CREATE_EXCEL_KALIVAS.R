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

# attempt to create function, but maybe we don't need to create this mapping files because WFU_Kalivas_test_df %>% sample_n(10) %>% select(cohort, labanimalid, accessid, sex, rfid) suggests that we might actually have this information from the wfu sheets
extract_kalivas_mapping <- function(files, sheet){
  data_breeder_list <-  lapply(files, function(i) {
    data_allsheets = u01.importxlsx(i)
    # data_breeder <- data_allsheets$info_from_breeder
    data_breeder <- data_allsheets[[sheet]] # made to extract any of the sheets
    
    names(data_breeder) <- c("dames", "sires", data_breeder[2,3:25]) %>% tolower()

    datecols <- c("dob", "dow", "shipmentdate")
    datefunction <- function(x){
      if(is.POSIXct(x) == F){
        as.POSIXct(as.numeric(x) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")
      } else x
    }

    data_breeder <- data_breeder[-c(1:4),] %>%
      rename("dob" =  "date_of_birth",
             "dow" = "date_of_wean",
             "shipmentdate" = "date_of_ship",
             "cohort" = "cohort_number",
             "rfid" = "microchip") %>%  # date of ship = date of delivery so remove (kalivas_cohort2_mapping %>% subset(date_of_ship != date_of_delivery))
      select(-date_of_delivery) %>%
      mutate_at(.vars = vars(datecols), .funs = datefunction) %>% 
      mutate(cohort = gsub("MUSC_", "", cohort))

    return(data_breeder)
    
  })
  return(data_breeder_list)
}

kalivas_cohort_xl <- extract_kalivas_mapping(all_excel_fnames, "info_from_breeder") %>% 
  rbindlist()


### PICK UP HERE

extract_kalivas_weights <- function(files, sheet){
  data_weight_list <-  lapply(files, function(i) {
    data_allsheets = u01.importxlsx(i)
    data_weight <- data_allsheets[[sheet]] # made to extract any of the sheets
    #data_weight <- data_allsheets$body_weights # used for troubleshooting
    names(data_weight) <- c(data_weight[1, 1:7], data_weight[2, 8:ncol(data_weight)]) %>% tolower()
    
    # datecols <- c("dob", "dow", "shipmentdate")
    # datefunction <- function(x){
    #   if(is.POSIXct(x) == F){
    #     as.POSIXct(as.numeric(x) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")
    #   } else x
    # }
    # 
    
    data_weight <- data_weight[-c(1:2),] %>%
      gather(var, value, -microchip, -sex, -bx_unit, -cohort_number, -internal_id, -group, -heroin_or_saline, -comment, -resolution) %>% 
      rename("cohort" = "cohort_number",
             "rfid" = "microchip") %>%  # date of ship = date of delivery so remove (kalivas_cohort2_mapping %>% subset(date_of_ship != date_of_delivery))
      mutate(cohort = gsub("MUSC_", "", cohort)) %>% 
      separate(var, into = c("date", "date_comment"), sep = "_", extra = "merge") %>% # warning messages about missing pieces filling with NA is expected and wanted
      mutate(date = ifelse(grepl("^\\d{5}$", date), as.POSIXct(as.numeric(date) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d") %>% as.character,
                           as.character(date)), 
             date = ifelse(grepl("^\\d{,2}/", date), lubridate::mdy(date) %>% as.character, as.character(date))) # warning messages about na's from the dates also expected
    
    return(data_weight)
    
  })
  return(data_weight_list)
}


kalivas_weights_xl <- extract_kalivas_weights(all_excel_fnames, "body_weights") %>% 
  rbindlist()



############### extract for raw vs excel comparison


## create cohort 2 and 3 objects



### long access 
extract_process_excel_lapply <- function(files, sheet){
  data_breeder_list <-  lapply(files, function(i) {
    data_allsheets = u01.importxlsx(i)
    data_breeder <- data_allsheets[[sheet]] # made to extract any of the sheets
    
    databegins_index <- which(data_breeder[,1] == "Microchip")
    
    df_values <- data_breeder[(databegins_index + 1):nrow(data_breeder),]
    
    make_unique = function(x, sep='_'){
      ave(x, x, FUN=function(a){if(length(a) > 1){paste(a, 1:length(a), sep=sep)} else {a}})
    }
    names(df_values) <- data_breeder[databegins_index,] %>% gsub(" ", "", .) %>% tolower() %>% make_unique()
    
    df_values <- df_values %>%
      dplyr::filter(!is.na(microchip)) %>%  ## okay doing this bc all other data na
      gather(var, value, -microchip, -sex, -bx_unit, -cohort_number, -internal_id, -group, -heroin_or_saline, -self_administration_room, -self_administration_box) %>%
      extract(var, c("measurement", "session"), "(\\D+_?)_(\\d+)") %>%
      spread(measurement, value) %>% 
      mutate(self_administration_box = self_administration_box %>% as.numeric %>% round(2) %>% as.character)
    
    df_sessiondosage <- data_breeder[1:(databegins_index-1),]
    
    df_sessiondosage <- df_sessiondosage %>%
      select_if(function(x) all(!is.na(x))) %>% # only select columns that have no na
      t() %>%
      cbind(rownames(.), ., row.names = NULL) %>%
      as.data.frame(row.names = NULL) %>%
      mutate_all(str_trim) %>%
      magrittr::set_colnames(.[1, ] %>% unlist() %>% as.character %>% tolower) %>%
      dplyr::filter(row_number() != 1) %>%
      mutate(date = openxlsx::convertToDateTime(date, origin = "1900-01-01") %>% as.character,
             session = stringr::str_extract(session, "\\d+"))
    
    
    df <- left_join(df_values, df_sessiondosage, by = "session") %>%
      mutate(cohort_number = gsub("MUSC_", "", cohort_number)) %>%
      rename("rfid" = "microchip")
    
    return(df)
    
  })
  return(data_breeder_list)
}

kalivas_lga_allcohorts_excel_processed <- extract_process_excel_lapply(all_excel_fnames, "LgA_SA") %>% rbindlist() %>% 
  select(-`...8`)

# *****************
##  PR_test

extract_process_excel_shortened_lapply <- function(files, sheet){
  data_breeder_list <-  lapply(files, function(i) {
    data_allsheets = u01.importxlsx(i)
    # data_breeder <- data_allsheets$info_from_breeder
    data_breeder <- data_allsheets[[sheet]] # made to extract any of the sheets
    
    databegins_index <- which(data_breeder[,1] == "Microchip")
    
    df_values <- data_breeder[(databegins_index + 1):nrow(data_breeder),]
    
    make_unique = function(x, sep='_'){
      ave(x, x, FUN=function(a){if(length(a) > 1){paste(a, 1:length(a), sep=sep)} else {a}})
    }
    names(df_values) <- data_breeder[databegins_index,] %>% gsub(" ", "", .) %>% tolower() %>% make_unique()
    
    df_values <- df_values %>%
      dplyr::filter(!is.na(microchip)) %>% ## okay doing this bc all other data na
      mutate(self_administration_box = self_administration_box %>% as.numeric %>% round(2) %>% as.character) 
    
    df_sessiondosage <- data_breeder[1:(databegins_index-1),]
    
    df_sessiondosage <- df_sessiondosage %>%
      select_if(function(x) all(!is.na(x))) %>% # only select columns that have no na
      t() %>%
      cbind(rownames(.), ., row.names = NULL) %>%
      as.data.frame(row.names = NULL) %>%
      mutate_all(str_trim) %>%
      magrittr::set_colnames(.[1, ] %>% unlist() %>% as.character %>% tolower) %>%
      dplyr::filter(row_number() != 1) %>%
      mutate(date = openxlsx::convertToDateTime(date, origin = "1900-01-01"))
    
    
    df <- cbind(df_values, df_sessiondosage) %>% 
      rename("cohort" = "cohort_number") %>% 
      mutate(cohort = gsub("MUSC_", "", cohort)) %>%
      rename("rfid" = "microchip")
    return(df)
    
  })
  return(data_breeder_list)
}

kalivas_pr_allcohorts_excel_processed <- extract_process_excel_shortened_lapply(all_excel_fnames, "PR_test") %>% rbindlist()


# *****************
##  Extinction_prime_test (Is it primed reinstatement?)

extract_process_excel_expr_lapply <- function(files, sheet){
  data_breeder_list <-  lapply(files, function(i) {
    data_allsheets = u01.importxlsx(i)
    data_breeder <- data_allsheets[[sheet]] # made to extract any of the sheets
    
    databegins_index <- which(data_breeder[,1] == "Microchip")
    
    df_values <- data_breeder[(databegins_index + 1):nrow(data_breeder),]
    
    make_unique = function(x, sep='_'){
      ave(x, x, FUN=function(a){if(length(a) > 1){paste(a, 1:length(a), sep=sep)} else {a}})
    }
    names(df_values) <- data_breeder[databegins_index,] %>% gsub(" ", "", .) %>% tolower() %>% make_unique()
    
    df_values <- df_values %>%
      dplyr::filter(!is.na(microchip)) %>%  ## okay doing this bc all other data na
      gather(var, value, -microchip, -sex, -bx_unit, -cohort_number, -internal_id, -group, -heroin_or_saline, -self_administration_room, -self_administration_box) %>%
      extract(var, c("measurement", "session"), "(\\D+_?\\d?)_(\\d)") %>% 
      mutate(session = ifelse(session == 1, "inactive", "active"))  %>%
      rename("lever" = "session") %>% 
      spread(measurement, value) %>% 
      mutate(self_administration_box = self_administration_box %>% as.numeric %>% round(2) %>% as.character)
    
    df_sessiondosage <- data_breeder[1:(databegins_index-1),]

    df_sessiondosage <- df_sessiondosage %>%
      select_if(function(x) all(!is.na(x))) %>% # only select columns that have no na
      t() %>%
      cbind(rownames(.), ., row.names = NULL) %>%
      as.data.frame(row.names = NULL) %>%
      mutate_all(str_trim) %>%
      magrittr::set_colnames(.[1, ] %>% unlist() %>% as.character %>% tolower) %>%
      dplyr::filter(row_number() != 1) %>%
      select(-lever) %>% 
      mutate(date = openxlsx::convertToDateTime(date, origin = "1900-01-01"))
    
    
    df <- cbind(df_values, df_sessiondosage) %>%
      mutate(cohort_number = gsub("MUSC_", "", cohort_number)) %>%
      rename("rfid" = "microchip")
    
    return(df)
    
  })
  return(data_breeder_list)
}



kalivas_expr_allcohorts_excel_processed <- extract_process_excel_expr_lapply(all_excel_fnames, "extinction_prime_test") %>% rbindlist()

# *****************
##  Extinction
kalivas_ex_allcohorts_excel_processed <- extract_process_excel_lapply(all_excel_fnames, "extinction") %>% rbindlist()

# *****************
##  Cued reinstatement
kalivas_cued_allcohorts_excel_processed <- extract_process_excel_shortened_lapply(all_excel_fnames, "cued_reinstatement") %>% rbindlist()
# names(kalivas_cued_allcohorts_excel_processed) <- paste0(names(kalivas_cued_allcohorts_excel_processed), "_xl")



## test 

## use this code to create something similar to italy's data
############# self admin (lga/pr) XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

kalivas_lga_allcohorts_excel_processed %>% 
  subset((rfid == "933000320046651"|rfid =="933000320046468")&session %in% c("1", "2", "3", "10", "11", "12")) %>% 
  select(rfid, session, infusions) %>%  ### don't know how to convert these values to the escalation of heroin intake
  mutate(intake = 20 * as.numeric(infusions)) # total consumption in ug/kg then it’s (# of infusions)(20). Our infusions are 20 ug/kg. 
    
kalivas_lga_allcohorts_excel_processed %>%
  # subset((rfid == "933000320046651"|rfid =="933000320046468")&session %in% c("1", "2", "3", "10", "11", "12")) %>%
  subset(session %in% c("1", "2", "3", "10", "11", "12")) %>%
  select(rfid, session, infusions) %>%
  pivot_wider(names_from = session, values_from = infusions, names_prefix = "session_") %>%
  mutate_at(vars(-matches("rfid")), as.numeric) %>% 
  mutate(mean_firstsessions = rowMeans(.[grep("session_[123]$", names(.))], na.rm =TRUE),
         mean_latersessions = rowMeans(.[grep("session_1[012]$", names(.))], na.rm =TRUE),
         escalation = mean_latersessions - mean_firstsessions) %>% 
  select(-matches("mean"))
  
## XX ASK ITALY TEAM IF THIS SHOULD BE ACTIVE LEVERS, INACTIVE, OR INFUSIONS?? HOW TO TRANSLATE TO HEROIN INTAKE
## missing escalation during 1st hour 
## missing total heroin consumption by body weight 
## missing total heroin consumption 


## XX CHECK THAT BREAKPOINT IS THE PR_STEP, IF SO, THEN THIS IS DONE 
kalivas_lga_allcohorts_excel_processed %>% 
  subset((rfid == "933000320046651"|rfid =="933000320046468")&session %in% c("1", "2", "3", "10", "11", "12")) %>% 
  select(rfid, pr_step, infusions) %>%  ### don't know how to convert these values to the escalation of heroin intake
  mutate_at(vars(-matches("rfid")), as.numeric) 



  
############# prime extinction
kalivas_expr_allcohorts_excel_processed %>% 
  # subset((rfid == "933000320046651"|rfid =="933000320046468")) %>%
  mutate_at(vars(matches("hour_")), as.numeric) %>% 
  group_by(rfid, lever) %>% 
  summarize(prime_sum_5_6 = hour_5 + hour_6,
            ext_prime_sum_3_4 = hour_3 + hour_4,
            context_sum_1_2 = hour_1 + hour_2) %>% 
  pivot_wider(names_from = lever, values_from = c("context_sum_1_2", "ext_prime_sum_3_4", "prime_sum_5_6")) # update from tidyr, more flexibility 


## use this code to create something similar to italy's data
############ extinction
kalivas_ex_allcohorts_excel_processed %>% 
  # subset((rfid == "933000320046651"|rfid =="933000320046468")&session%in%c("1", "6")) %>% 
  subset(session%in%c("1", "6")) %>% 
  mutate(active_lever = as.numeric(active_lever)) %>% 
  group_by(rfid) %>% 
  summarize(deescalation = active_lever[2]-active_lever[1]) %>% select(rfid, deescalation)































# ############################
# # Exp 1: ELEVATED PLUS MAZE
# ############################
setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/addiction_related_behaviors/Raw_data_files")

kalivas_epm_allcohorts_excel_processed <- extract_process_excel_repeatedmeasures2_lapply(all_excel_fnames, "elevated_plus_maze") %>% rbindlist()

data_breeder <- data_allsheets$elevated_plus_maze

############################
# Exp 2: OPEN FIELD TASK
############################

setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/addiction_related_behaviors/Raw_data_files")

extract_process_excel_repeatedmeasures2_lapply <- function(files, sheet){ # for use on before self admin and after self admin
  data_breeder_list <-  lapply(files, function(i) {
    
    u01.importxlsx <- function(xlname){
      path_sheetnames <- excel_sheets(xlname)
      df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname, col_names = F) ## note the difference here, bc we don't want headers 
      names(df) <- path_sheetnames
      return(df)
    }
    
    data_allsheets = u01.importxlsx(i)
    data_breeder <- data_allsheets[[sheet]] # made to extract any of the sheets
    
    datavaluesbegins_index <- which(data_breeder[, 1] == "Microchip")
    databegins_index <- which(data_breeder[, 8] == "Date")
    
    names(data_breeder)[1:8] <- data_breeder[datavaluesbegins_index, 1:8] %>% unlist() %>% as.character()
    names(data_breeder)[8:ncol(data_breeder)] <- data_breeder[databegins_index, 8:ncol(data_breeder)] %>% unlist() %>% as.character()
    make_unique = function(x, sep='_'){
      ave(x, x, FUN=function(a){if(length(a) > 1){paste(a, 1:length(a), sep=sep)} else {a}})
    }
    names(data_breeder) <- names(data_breeder) %>% gsub(" ", "", .) %>% tolower() %>% make_unique()
    df_values <- data_breeder[(databegins_index + 1):nrow(data_breeder),]
    df <- df_values %>%
      dplyr::filter(!is.na(microchip)) %>%  ## okay doing this bc all other data_breeder na
      mutate_at(vars(matches("date")), openxlsx::convertToDateTime, origin = "1900-01-01") %>% 
      mutate_at(vars(matches("date")), as.character) %>% 
      gather(var, value, -microchip, -sex, -bx_unit, -cohort_number, -internal_id, -group, -heroin_or_saline) %>%
      extract(var, c("measurement", "session"), "(.*)_(\\d)") %>% 
      mutate(session = ifelse(session == 1, "before_SA", "after_SA"))  %>%
      spread(measurement, value) %>% 
      mutate(cohort_number = gsub("MUSC_", "", cohort_number)) %>%
      rename("rfid" = "microchip")
    return(df)
    
  })
  return(data_breeder_list)
}

kalivas_oft_allcohorts_excel_processed <- extract_process_excel_repeatedmeasures2_lapply(all_excel_fnames, "open_field") %>% rbindlist() %>% 
  mutate_at(vars(one_of("center_time_seconds", "number_of_rears", "number_of_sterotypies", "total_cm_traveled", "total_time_traveled_seconds")), as.numeric)

############################
# Exp 3: TAIL FLICK
############################
setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/addiction_related_behaviors/Raw_data_files")

kalivas_tf_allcohorts_excel_processed <- extract_process_excel_repeatedmeasures2_lapply(all_excel_fnames, "tail_flick") %>% rbindlist()
