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
             "shipmentdate" = "date_of_ship") %>%  # date of ship = date of delivery so remove (kalivas_cohort2_mapping %>% subset(date_of_ship != date_of_delivery))
      select(-date_of_delivery) %>%
      mutate_at(.vars = vars(datecols), .funs = datefunction)

    return(data_breeder)
    
  })
  return(data_breeder_list)
}

kalivas_cohort_xl <- extract_kalivas_mapping(all_excel_fnames, "info_from_breeder") %>% 
  rbindlist() %>% 
  rename("cohort" = "cohort_number") %>% 
  mutate(cohort = gsub("MUSC_", "", cohort)) %>%
  rename("rfid" = "microchip") 


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
      mutate(date = openxlsx::convertToDateTime(date, origin = "1900-01-01"))
    
    
    df <- left_join(df_values, df_sessiondosage, by = "session") %>%
      mutate(cohort_number = gsub("MUSC_", "", cohort_number)) %>%
      rename("rfid" = "microchip")
    
    return(df)
    
  })
  return(data_breeder_list)
}

kalivas_lga_allcohorts_excel_processed <- extract_process_excel_lapply(all_excel_fnames, "LgA_SA") %>% rbindlist()

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




































# ############################
# # Exp 1: ELEVATED PLUS MAZE
# ############################
# setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/behavioral_tasks/elevated_plus_maze")
# 
# # all wmv files
# 
# 

############################
# Exp 2: OPEN FIELD TASK
############################


extract_process_excel_repeatedmeasures2_lapply <- function(files, sheet){ # for use on before self admin and after self admin
  data_breeder_list <-  lapply(files, function(i) {
    data_allsheets = u01.importxlsx(i)
    data_breeder <- data_allsheets[[sheet]] # made to extract any of the sheets
    
    databegins_index <- which(data_breeder[, 8] == "Date")
    
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
      extract(var, c("measurement", "session"), "(\\D+_?\\d?)_(\\d)") %>% 
      mutate(session = ifelse(session == 1, "before_SA", "after_SA"))  %>%
      spread(measurement, value) %>% 
      mutate(cohort_number = gsub("MUSC_", "", cohort_number)) %>%
      rename("rfid" = "microchip")
    return(df)
    
  })
  return(data_breeder_list)
}

kalivas_oft_allcohorts_excel_processed <- extract_process_excel_repeatedmeasures2_lapply(all_excel_fnames, "open_field") %>% rbindlist()
















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
