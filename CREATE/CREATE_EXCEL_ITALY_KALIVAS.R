## CREATE EXCEL ITALY AND U.S. 

## EXTRACT KALIVAS ITALY EXCEL 
# 
# setwd("~/Dropbox (Palmer Lab)/Roberto_Ciccocioppo_U01/raw data")
# italy_filenames_xl <- list.files(recursive = T)


# extract_italy_tissue_notes <- function(files, sheet){
#   data_breeder_list <-  lapply(files, function(i) {
#     data_allsheets = u01.importxlsx(i)
#     # data_breeder <- data_allsheets$info_from_breeder
#     data_breeder <- data_allsheets[[sheet]] # made to extract any of the sheets
#     
#     names(data_breeder) <- c("dames", "sires", data_breeder[2,3:25]) %>% tolower()
#     
#     datecols <- c("dob", "dow", "shipmentdate")
#     datefunction <- function(x){
#       if(is.POSIXct(x) == F){
#         as.POSIXct(as.numeric(x) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")
#       } else x
#     }
#     
#     data_breeder <- data_breeder[-c(1:4),] %>%
#       rename("dob" =  "date_of_birth",
#              "dow" = "date_of_wean",
#              "shipmentdate" = "date_of_ship",
#              "cohort" = "cohort_number",
#              "rfid" = "microchip") %>%  # date of ship = date of delivery so remove (kalivas_cohort2_mapping %>% subset(date_of_ship != date_of_delivery))
#       select(-date_of_delivery) %>%
#       mutate_at(.vars = vars(datecols), .funs = datefunction) %>%
#       mutate(cohort = parse_number(cohort) %>% str_pad(., 2, side = "left", pad = "0"),
#              coat_color = gsub("([A-Z]+)(HOOD)", "\\1 \\2", mgsub::mgsub(coat_color, 
#                                                                          c("BRN|[B|b]rown", "BLK|[B|b]lack", "HHOD|[H|h]ood|[H|h]hod", "[A|a]lbino"), 
#                                                                          c("BROWN", "BLACK", "HOOD", "ALBINO"))) 
#       ) %>% 
#       rename_at(vars(behavioral_unit:resolution) ,function(x){paste0(x, "_behavunit")})
#     
#     return(data_breeder)
#     
#   })
#   return(data_breeder_list)
# }
# 
# kalivas_italy_cohort_xl <- extract_italy_tissue_notes(italy_filenames_xl[2], "General info") %>%
#   rbindlist()


# import all and then separate by tab
setwd("~/Dropbox (Palmer Lab)/Roberto_Ciccocioppo_U01/raw data/")
u01.importxlsx <- function(xlname){
  path_sheetnames <- excel_sheets(xlname)
  df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname)
  names(df) <- path_sheetnames
  return(df)
} 
all_excel_fnames_c01_10 <- list.files(path = "~/Dropbox (Palmer Lab)/Roberto_Ciccocioppo_U01/raw data/", full.names = T, recursive = T) %>% grep("batch[- ]?(0[1-9]|10)", ., value = T, ignore.case = T)


# extract shipping information for metadata 
kalivas_italy_extract_shipping_metadata <- function(files, sheet){
  data_breeder_list <-  lapply(files, function(i) {
    
    u01.importxlsx <- function(xlname){
      path_sheetnames <- excel_sheets(xlname)
      df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname, col_names = F) ## note the difference here, bc we don't want headers 
      names(df) <- path_sheetnames
      return(df)
    }
    
    data_allsheets = u01.importxlsx(i)
    
    names(data_allsheets) = names(data_allsheets) %>% 
      tolower() %>%
      str_trim() %>%
      gsub(" ", "", .) %>% 
      gsub("^general.*|^italy$|^shipping", "generalinfo", .) %>%  # make names of sheets uniform across all cohorts
      gsub("of", "openfield", .) %>% 
      gsub(".*12h.*", "heroin_sa_12h", .) %>% 
      gsub(".*1(h|st).*", "heroin_sa_1h", .) %>% 
      gsub(".priming.*", "priming", .) %>% 
      gsub(".*extinction.*", "extinction", .)
    
    data_breeder <- data_allsheets[[grep(sheet, names(data_allsheets), ignore.case = T)]] # made to extract any of the sheets
    
    df <- data_breeder 
    names(df) <- df[1, ] %>% unlist() %>% as.character %>% make_clean_names() 
    df <- df[-1, ]
    
    while(grepl("^na|^info", names(df)[1])&grepl("^na", names(df)[2])){
      names(df) <- df[1, ] %>% unlist() %>% as.character %>% make_clean_names() 
      df <- df[-1, ]
    }
    
    return(df)
  })
  return(data_breeder_list)
}

kalivas_italy_shipping_metadata_c01_10 <- lapply(all_excel_fnames_c01_10, kalivas_italy_extract_shipping_metadata, "generalinfo") %>% sapply(.,'[')
names(kalivas_italy_shipping_metadata_c01_10) <- all_excel_fnames_c01_10
kalivas_italy_shipping_metadata_c01_10_df <- kalivas_italy_shipping_metadata_c01_10 %>% 
  rbindlist(fill = T, idcol = "file")

# extracting relevant covariates
kalivas_italy_shipping_metadata_c01_10_df_ids <- kalivas_italy_shipping_metadata_c01_10_df %>% 
  mutate(cohort = gsub(".*batch[- ](\\d+).*", "\\1", file, ignore.case = T) %>% parse_number(),
         cohort = paste0("C", str_pad(as.character(cohort), 2, "left", "0"))) %>% 
  mutate(transponder_id = gsub("([.]|E14)", "", transponder_id),
         transponder_id = ifelse(nchar(transponder_id) == 14, paste0(transponder_id, "0"), transponder_id)) %>%
  subset(!is.na(transponder_id)) %>% 
  mutate(d_o_b = as.numeric(d_o_b) %>% as.character() %>% openxlsx::convertToDate() %>% as.POSIXct(format="%Y-%m-%d")) %>% 
  select(cohort, transponder_id, animal_id, sex, d_o_b)


# join to the metadata here to verify rfid's and fill in dob's 
kalivas_italy_metadata_c01_10_df <- kalivas_italy_shipping_metadata_c01_10_df_ids %>% 
  full_join(WFU_KalivasItaly %>% mutate(cohort = ifelse(!grepl("^C", cohort), paste0("C", cohort), cohort)) %>% select(cohort, rfid, sex, dob, comment), by = c("cohort", "transponder_id" = "rfid")) %>% 
  mutate(sex.x = coalesce(sex.x, sex.y),
         d_o_b = coalesce(d_o_b, dob)) %>%
  rename("sex" = "sex.y",
         "rfid" = "transponder_id") %>% 
  mutate(dob = as.POSIXct(dob, format="%Y-%m-%d")) %>%
  select(cohort, rfid, animal_id, sex, dob, comment)

# check for incorrectly labelled sexes
kalivas_italy_metadata_c01_10_df %>% subset(sex.x != sex.y)
kalivas_italy_metadata_c01_10_df %>% subset(d_o_b != dob)






############################
# ELEVATED PLUS MAZE
############################

extract_process_excel_repeatedmeasures2_lapply_italy <-  function(files, sheet){ # for use on before self admin and after self admin
  data_breeder_list <-  lapply(files, function(i) {
    
    u01.importxlsx <- function(xlname){
      path_sheetnames <- excel_sheets(xlname)
      df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname, col_names = F) ## note the difference here, bc we don't want headers 
      names(df) <- path_sheetnames
      return(df)
    }
    
    data_allsheets = u01.importxlsx(i)
    
    # data_allsheets = u01.importxlsx(all_excel_fnames_c01_10[1])
    
    
    names(data_allsheets) = names(data_allsheets) %>% 
      tolower() %>%
      str_trim() %>%
      gsub(" ", "", .) %>% 
      gsub("^general.*|^italy$", "generalinfo", .) %>%  # make names of sheets uniform across all cohorts
      gsub("of", "openfield", .) %>% 
      gsub(".*12h.*", "heroin_sa_12h", .) %>% 
      gsub(".*1(h|st).*", "heroin_sa_1h", .) %>% 
      gsub(".priming.*", "priming", .) %>% 
      gsub(".*extinction.*", "extinction", .)
    
    # data_breeder <- data_allsheets[[sheet]] # made to extract any of the sheets
    data_breeder <- data_allsheets[[grep(sheet, names(data_allsheets), ignore.case = T)]] # made to extract any of the sheets
    
    if(length(grep("Transponder ID", as.character(data_breeder[1, ]), value = T, ignore.case = T))==2|length(grep("Transponder ID", names(data_breeder), value = T, ignore.case = T))==2){
      data_breeder <- data_breeder[, -1]
    }
    
    if(!any(grepl("transponder", data_breeder[1,] %>% unlist() %>% as.character, ignore.case = T))){ # if the data doesn't have the transponder id column, add a placeholder
      data_breeder <- data_breeder %>% 
        tibble::add_column(transponderid = "transponder id", .before = 1) # chose index location instead of name bc maybe inconsistent naming
    }
    
    datavaluesbegins_index <- grep("transponder id", data_breeder[,1] %>% unlist() %>% as.character, ignore.case = T)[1]
    
    
    if(!any(grepl("sex", data_breeder[1,] %>% unlist() %>% as.character, ignore.case = T))){ # if the data doesn't have the sex id column, add a placeholder
      data_breeder <- data_breeder %>% 
        tibble::add_column(sex = "sex", .after = 1) # chose index location instead of name bc maybe inconsistent naming
    }
    
    
    if(!any(grepl("cohort|batch", data_breeder[1,] %>% unlist() %>% as.character, ignore.case = T))){ # if the data doesn't have the batch column, add a placeholder
      data_breeder <- data_breeder %>% 
        tibble::add_column(batchnumber = "batchnumber", .after = 3) # chose index location instead of name bc maybe inconsistent naming
    }
    
    if(!any(grepl("Behavioral", data_breeder[1,] %>% unlist() %>% as.character, ignore.case = T))){ # if the data doesn't have the behavioral characterization column, add a placeholder
      data_breeder <- data_breeder %>% 
        tibble::add_column(behavioralcharacterizationunit = "behavioralcharacterizationunit", .after = 4) # chose index location instead of name bc maybe inconsistent naming
    }
    
    if(!any(grepl("heroin|yoked", data_breeder[1,] %>% unlist() %>% as.character, ignore.case = T))){ # if the data doesn't have the heroin/yoked column, add a placeholder XX should check if this value is shared across phenotypes in the same cohort
      data_breeder <- data_breeder %>% 
        tibble::add_column(heroin_salineyoked = "heroin_salineyoked", .after = 5) # chose index location instead of name bc maybe inconsistent naming
    }
    
    databegins_index <- grep("date", data_breeder[,7] %>% unlist() %>% as.character, ignore.case = T)
    
    names(data_breeder)[1:6] <- data_breeder[datavaluesbegins_index, 1:6] %>% unlist() %>% as.character()
    names(data_breeder)[7:ncol(data_breeder)] <- data_breeder[databegins_index, 7:ncol(data_breeder)] %>% unlist() %>% as.character()
    make_unique = function(x, sep='_'){
      ave(x, x, FUN=function(a){if(length(a) > 1){paste(a, 1:length(a), sep=sep)} else {a}})
    }
    names(data_breeder) <- names(data_breeder) %>% gsub(" ", "", .) %>% tolower() %>% make_unique() %>% make_clean_names()
    df_values <- data_breeder[(databegins_index + 1):nrow(data_breeder),]
    
    not_all_na <- function(x) any(!is.na(x))
    
    
    df <- df_values %>%
      rename("microchip" = "transponderid",
             "behavioralcharacterizationunit" = "behavioralcarachterizationunit") %>%
      rename_all(~ stringr::str_replace_all(., '.*internal.*', 'ratinternalid')) %>%    
      # rename_all(~ stringr::str_replace_all(., '.*saline*', 'heroin_salineyoked')) %>%       
      dplyr::filter(!is.na(microchip)) %>%  ## okay doing this bc all other data_breeder na
      mutate_at(vars(matches("date")), openxlsx::convertToDateTime, origin = "1900-01-01") %>% 
      mutate_at(vars(matches("date")), as.character) %>% 
      mutate(batchnumber = replace(batchnumber, batchnumber == "batchnumber", parse_number(str_match(i, "cohort \\d+"))),
             behavioralcharacterizationunit = replace(behavioralcharacterizationunit, behavioralcharacterizationunit == "behavioralcharacterizationunit", NA),
             heroin_salineyoked = replace(heroin_salineyoked, heroin_salineyoked == "heroin_salineyoked", NA)) %>% 
      gather(var, value, -microchip, -sex, -behavioralcharacterizationunit, -batchnumber, -ratinternalid, -heroin_salineyoked) %>%
      extract(var, c("measurement", "session"), "(.*)_(\\d)") %>% 
      mutate(session = ifelse(session == 1, "before_SA", "after_SA"))  %>%
      spread(measurement, value) %>% 
      rename("cohort_number" = "batchnumber") %>% 
      mutate(cohort_number = gsub("UNICAM_", "", cohort_number)) %>%
      rename("rfid" = "microchip")  %>%
      # select_if(not_all_na) %>%
      mutate(heroin_salineyoked = tolower(heroin_salineyoked)) %>%
      mutate(heroin_salineyoked = ifelse(grepl("saline|yoke", heroin_salineyoked), "saline", heroin_salineyoked)) %>% 
      rename_all(~ stringr::str_replace_all(., '.*time.*?center', 'totaltimeincenter')) %>%    
      rename_all(~ stringr::str_replace_all(., '.*distancetraveled.*', 'distancetraveled_cm')) %>% 
      select(-matches("^<?NA>?$"))
    
    return(df) 
    
  })
  return(data_breeder_list)
}


kalivas_italy_epm_excel_processed_c01_10 <- extract_process_excel_repeatedmeasures2_lapply_italy(all_excel_fnames_c01_10, "epm") 
names(kalivas_italy_epm_excel_processed_c01_10) <- all_excel_fnames_c01_10
kalivas_italy_epm_excel_processed_c01_10_df <- kalivas_italy_epm_excel_processed_c01_10 %>% rbindlist(idcol = "file")

kalivas_italy_epm_excel_processed_c01_10_df <- kalivas_italy_epm_excel_processed_c01_10_df %>% 
  rename("cohort" = "cohort_number") %>% 
  mutate(cohort = paste0("C", str_pad(str_match(tolower(file), "cohort \\d+") %>% parse_number() %>% as.character(), 2, "left", "0"))) %>% 
  naniar::replace_with_na_all(condition = ~.x %in% c("N/A", "NA", ""))




############################
# OPEN FIELD TASK
############################


kalivas_italy_oft_excel_processed_c01_10 <- extract_process_excel_repeatedmeasures2_lapply_italy(all_excel_fnames_c01_10, "openfield") # not sure why heroin yoked error
names(kalivas_italy_oft_excel_processed_c01_10) <- all_excel_fnames_c01_10
kalivas_italy_oft_excel_processed_c01_10_df <- kalivas_italy_oft_excel_processed_c01_10 %>% rbindlist(idcol = "file", fill = T) ## XX temporary 

kalivas_italy_oft_excel_processed_c01_10_df <- kalivas_italy_oft_excel_processed_c01_10_df %>%
  mutate(cohort = paste0("C", str_pad(str_match(tolower(file), "cohort \\d+") %>% parse_number() %>% as.character(), 2, "left", "0"))) %>% 
  select(-cohort_number) %>% 
  naniar::replace_with_na_all(condition = ~.x %in% c("N/A", "NA", ""))

kalivas_italy_oft_excel_processed_c01_10_df_wide <- kalivas_italy_oft_excel_processed_c01_10_df %>% 
  pivot_wider(names_from = session, values_from = center_time_seconds:total_time_traveled_seconds) 


############################
# TAIL FLICK 
############################
tailflick_italy <- function(files, sheet){ # for use on before self admin and after self admin
  data_breeder_list <- lapply(files, function(i){
    u01.importxlsx <- function(xlname){
      path_sheetnames <- excel_sheets(xlname)
      df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname, col_names = F) ## note the difference here, bc we don't want headers 
      names(df) <- path_sheetnames
      return(df)
    }
    
    data_allsheets = u01.importxlsx(i)
    # data_allsheets = u01.importxlsx(all_excel_fnames_c01_10[1])
    names(data_allsheets) = names(data_allsheets) %>% 
      tolower() %>%
      str_trim() %>%
      gsub(" ", "", .) %>% 
      gsub("^general.*|^italy$", "generalinfo", .) %>%  # make names of sheets uniform across all cohorts
      gsub("of", "openfield", .) %>% 
      gsub(".*12h.*", "heroin_sa_12h", .) %>% 
      gsub(".*1(h|st).*", "heroin_sa_1h", .) %>% 
      gsub(".priming.*", "priming", .) %>% 
      gsub(".*extinction.*", "extinction", .)
    
    # data_breeder <- data_allsheets[[sheet]] # made to extract any of the sheets
    data_breeder <- data_allsheets[[grep(sheet, names(data_allsheets), ignore.case = T)]] # made to extract any of the sheets
    # data_breeder <- data_allsheets[[grep("tailflick", names(data_allsheets), ignore.case = T)]] # made to extract any of the sheets
    
    if(!any(grepl("transponder", data_breeder[,1] %>% unlist() %>% as.character, ignore.case = T))){ # if the data doesn't have the transponder id column, add a placeholder
      data_breeder <- data_breeder %>% 
        tibble::add_column(transponderid = "transponder id", .before = 1) # chose index location instead of name bc maybe inconsistent naming
    }
    

    if(length(grep("Transponder ID", as.character(data_breeder[1, ]), value = T, ignore.case = T))==2|length(grep("Transponder ID", names(data_breeder), value = T, ignore.case = T))==2){
      data_breeder <- data_breeder[, -1]
    }
  
    datavaluesbegins_index <- grep("transponder id", data_breeder[,1] %>% unlist() %>% as.character, ignore.case = T)[1]
    
    
    if(!any(grepl("sex", data_breeder[datavaluesbegins_index,] %>% unlist() %>% as.character, ignore.case = T))){ # if the data doesn't have the sex id column, add a placeholder
      data_breeder <- data_breeder %>% 
        tibble::add_column(sex = "sex", .after = 1) # chose index location instead of name bc maybe inconsistent naming
    }
    
    
    if(!any(grepl("cohort|batch", data_breeder[datavaluesbegins_index,] %>% unlist() %>% as.character, ignore.case = T))){ # if the data doesn't have the batch column, add a placeholder
      data_breeder <- data_breeder %>% 
        tibble::add_column(batchnumber = "batchnumber", .after = 3) # chose index location instead of name bc maybe inconsistent naming
    }
    
    if(!any(grepl("Behavioral", data_breeder[datavaluesbegins_index,] %>% unlist() %>% as.character, ignore.case = T))){ # if the data doesn't have the behavioral characterization column, add a placeholder
      data_breeder <- data_breeder %>% 
        tibble::add_column(behavioralcharacterizationunit = "behavioralcharacterizationunit", .after = 4) # chose index location instead of name bc maybe inconsistent naming
    }
    
    if(!any(grepl("heroin|yoked", data_breeder[datavaluesbegins_index,] %>% unlist() %>% as.character, ignore.case = T))){ # if the data doesn't have the heroin/yoked column, add a placeholder XX should check if this value is shared across phenotypes in the same cohort
      data_breeder <- data_breeder %>% 
        tibble::add_column(heroin_salineyoked = "heroin_salineyoked", .after = 5) # chose index location instead of name bc maybe inconsistent naming
    }
    
    databegins_index <- grep("date", data_breeder[,7] %>% unlist() %>% as.character, ignore.case = T)
    
    names(data_breeder)[1:6] <- data_breeder[datavaluesbegins_index, 1:6] %>% unlist() %>% as.character()
    names(data_breeder)[7] <- "date"
    names(data_breeder)[8:ncol(data_breeder)] <- data_breeder[databegins_index+1, 8:ncol(data_breeder)] %>% unlist() %>% as.character %>% as.data.frame() %>% rename("varname" = ".") %>% fill(varname) %>% unlist() %>% as.character
    make_unique = function(x, sep='_'){
      ave(x, x, FUN=function(a){if(length(a) > 1){paste(a, 1:length(a), sep=sep)} else {a}})
    }
    names(data_breeder) <- names(data_breeder) %>% gsub(" ", "", .) %>% tolower() %>% make_unique() %>% make_clean_names()
    df_values <- data_breeder[(databegins_index + 1):nrow(data_breeder),]
    
    not_all_na <- function(x) any(!is.na(x))
    diff_fxn <- function(x, y) x-y
    
    df <- df_values %>%
      rename("microchip" = "transponderid",
             "behavioralcharacterizationunit" = "behavioralcarachterizationunit") %>%
      rename_all(~ stringr::str_replace_all(., '.*internal.*', 'ratinternalid')) %>%    
      # rename_all(~ stringr::str_replace_all(., '.*saline*', 'heroin_salineyoked')) %>%       
      dplyr::filter(!is.na(microchip)) %>%  ## okay doing this bc all other data_breeder na
      mutate_at(vars(matches("date")), openxlsx::convertToDateTime, origin = "1900-01-01") %>% 
      mutate_at(vars(matches("date")), as.character) %>% 
      mutate(batchnumber = replace(batchnumber, batchnumber == "batchnumber", parse_number(str_match(i, "cohort \\d+"))),
             behavioralcharacterizationunit = replace(behavioralcharacterizationunit, behavioralcharacterizationunit == "behavioralcharacterizationunit", NA),
             heroin_salineyoked = replace(heroin_salineyoked, heroin_salineyoked == "heroin_salineyoked", NA)) %>% 
      gather(var, value, -microchip, -sex, -behavioralcharacterizationunit, -batchnumber, -ratinternalid, -heroin_salineyoked) %>%
      mutate(var = replace(var, var == "date", "date_2"),
             var = replace(var, var == "average_3", "date_1")) %>% 
      extract(var, c("measurement", "session"), "(.*)_(\\d+)$", remove = F) %>% 
      mutate(session = case_when(
        grepl("trial_ir", measurement)&session %in% c(1:8)|grepl("average", measurement)&session%in%c(1:2)|grepl("date", measurement)&session==1 ~ "before_SA", 
        grepl("trial_ir", measurement)&session %in% c(9:16)|grepl("average", measurement)&session%in%c(4:5)|grepl("date", measurement)&session==2 ~ "after_SA")) %>% 
      mutate(var = case_when(
        grepl("trial_ir", var)&grepl("_[1-4]$", var) ~ gsub("trial_ir50", "trial_ir50_veh", var), 
        grepl("trial_ir", var)&grepl("_(9|1[0-2])$", var) ~ gsub("trial_ir50_(\\d+)", "\\1", var) %>% as.numeric() %>% diff_fxn(., 8) %>% as.character %>% paste0("trial_ir50_veh_", .),
        grepl("trial_ir", var)&grepl("_([5-8])$", var) ~ gsub("trial_ir50_(\\d+)", "\\1", var) %>% as.numeric() %>% diff_fxn(., 4) %>% as.character %>% paste0("trial_ir50_trt_", .),
        grepl("trial_ir", var)&grepl("_(1[3-6])$", var) ~ gsub("trial_ir50_(\\d+)", "\\1", var) %>% as.numeric() %>% diff_fxn(., 12) %>% as.character %>% paste0("trial_ir50_trt_", .),
        grepl("average_1|4", var) ~ "average_veh", 
        grepl("average_2|5", var) ~ "average_trt",
        grepl("date_1|4", var) ~ "date", 
        grepl("date_2|5", var) ~ "date", 
        TRUE ~ as.character(var))) %>%
      select(-measurement) %>% 
      spread(var, value) %>% 
      rename("cohort_number" = "batchnumber") %>% 
      mutate(cohort_number = gsub("UNICAM_", "", cohort_number)) %>%
      rename("rfid" = "microchip")  %>%
      # select_if(not_all_na) %>%
      mutate(heroin_salineyoked = tolower(heroin_salineyoked)) %>%
      mutate(heroin_salineyoked = ifelse(grepl("saline|yoke", heroin_salineyoked), "saline", heroin_salineyoked)) %>% 
      mutate(date = ifelse(grepl("\\d{5}", date), as.character(openxlsx::convertToDate(date)), date)) %>% 
      rename_all(~ stringr::str_replace_all(., '.*time.*?center', 'totaltimeincenter')) %>%    
      rename_all(~ stringr::str_replace_all(., '.*distancetraveled.*', 'distancetraveled_cm')) %>% 
      select(-matches("^<?NA>?$"))
    
    return(df)
    
  })
  return(data_breeder_list)
  
}

kalivas_italy_tf_excel_processed_c01_10 <- tailflick_italy(all_excel_fnames_c01_10, "tailflick") 
names(kalivas_italy_tf_excel_processed_c01_10) <- all_excel_fnames_c01_10
kalivas_italy_tf_excel_processed_c01_10_df <- kalivas_italy_tf_excel_processed_c01_10 %>% rbindlist(fill = T, id = "file")



############################
# LGA
############################

extract_process_excel_manysessions_lapply_italy <- function(files, sheet){
  data_breeder_list <-  lapply(files, function(i) {
    u01.importxlsx <- function(xlname){
      path_sheetnames <- excel_sheets(xlname)
      df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname, col_names = F) ## note the difference here, bc we don't want headers 
      names(df) <- path_sheetnames
      return(df)
    }
    
    data_allsheets = u01.importxlsx(i) # data_allsheets = u01.importxlsx(all_excel_fnames_c01_10[2]) ## XX remove, just here for troubleshooting 
    
    names(data_allsheets) = names(data_allsheets) %>% 
      tolower() %>%
      str_trim() %>%
      gsub(" ", "", .) %>% 
      gsub("^general.*|^italy$", "generalinfo", .) %>%  # make names of sheets uniform across all cohorts
      gsub("of", "openfield", .) %>% 
      gsub("^(?!infusions).*(12)h.*", "heroin_sa_12h", ., perl = T) %>% 
      gsub("^(?!infusions).*1(h|st).*", "heroin_sa_1h", ., perl = T) %>% 
      gsub(".priming.*", "priming", .) %>% 
      gsub(".*extinction.*", "extinction", .)
    
    if(any(grepl(sheet, names(data_allsheets)))){
      data_breeder <- data_allsheets[[grep(sheet, names(data_allsheets), ignore.case = T)]] # made to extract any of the sheets
      # data_breeder <- data_allsheets[[grep("heroin_sa_12h", names(data_allsheets), ignore.case = T)]] # made to extract any of the sheets
      
      ## reorder columns in specific cohorts
      if(grepl("06", i)&grepl("_12h", sheet)){ # make fix when they leave out the headers 
        data_breeder[, 1:4] <- data_breeder[, c(3, 2, 1, 4)] # transponder id, sex, batch number, internal id
      }
      
      if(grepl("batch-01", i, ignore.case = T)&grepl("_12h", sheet)){ # make fix when they leave out the headers 
        data_breeder[, 1:5] <- data_breeder[, c(2, 3, 5, 1, 4)]
      }
      
      if(grepl("batch-03", i, ignore.case = T)&grepl("_1h|priming", sheet)){ # make fix when they leave out the headers 
        data_breeder <- data_breeder[, -1]
      }
      
      if(grepl("batch-06", i, ignore.case = T)&grepl("_1h", sheet)){ # make fix when they leave out the headers 
        data_breeder[, 1:3] <- data_breeder[, c(3, 2, 1)]
      }
      
      if(grepl("batch-01", i, ignore.case = T)&grepl("_1h", sheet)){ # make fix when they leave out the headers 
        data_breeder[, 1:3] <- data_breeder[, c(2, 3, 1)]
      }
      
      # add columns
      if(!any(grepl("tra[n]?sponder", data_breeder[, 1] %>% unlist() %>% as.character, ignore.case = T))){ # if the data doesn't have the transponder id column, add a placeholder
        data_breeder <- data_breeder %>% 
          tibble::add_column(transponderid = "transponder id", .before = 1) # chose index location instead of name bc maybe inconsistent naming
      }
      
      if(any(grepl("trasponder", data_breeder[, 1] %>% unlist() %>% as.character, ignore.case = T))){ # fix transponder column id typo
        data_breeder[grep("trasponder", data_breeder[, 1] %>% unlist() %>% as.character(), ignore.case = T), 1] <- "Transponder ID"
      }
      
      if(unique(data_breeder[, 1] %>% unlist() %>% as.character) %>% length == 1 & unique(data_breeder[, 1] %>% unlist() %>% as.character) == "transponder id"){
        datavaluesbegins_index <- grep("id", data_breeder[,2] %>% unlist() %>% as.character, ignore.case = T)[1]
      } else{
        datavaluesbegins_index <- grep("transponder id", data_breeder[,1] %>% unlist() %>% as.character, ignore.case = T)[1]
      }
            
      if(length(grep("Tran[s]?ponder ID", as.character(data_breeder[datavaluesbegins_index, ]), value = T, ignore.case = T))==2|length(grep("Tran[s]?ponder ID", names(data_breeder), value = T, ignore.case = T))==2){
        data_breeder <- data_breeder[, -1]
      }
      
      
      if(!any(grepl("sex", data_breeder[datavaluesbegins_index,] %>% unlist() %>% as.character, ignore.case = T))){ # if the data doesn't have the sex id column, add a placeholder
        data_breeder <- data_breeder %>% 
          tibble::add_column(sex = "sex", .after = 1) # chose index location instead of name bc maybe inconsistent naming
      }
      
      
      if(!any(grepl("cohort|batch", data_breeder[datavaluesbegins_index,] %>% unlist() %>% as.character, ignore.case = T))){ # if the data doesn't have the batch column, add a placeholder
        data_breeder <- data_breeder %>% 
          tibble::add_column(batchnumber = "batchnumber", .after = 3) # chose index location instead of name bc maybe inconsistent naming
      }
      
      if(!any(grepl("Behavioral", data_breeder[datavaluesbegins_index,] %>% unlist() %>% as.character, ignore.case = T))){ # if the data doesn't have the behavioral characterization column, add a placeholder
        data_breeder <- data_breeder %>% 
          tibble::add_column(behavioralcharacterizationunit = "behavioralcharacterizationunit", .after = 4) # chose index location instead of name bc maybe inconsistent naming
      }
      
      if(!any(grepl("heroin|yoked", data_breeder[datavaluesbegins_index,] %>% unlist() %>% as.character, ignore.case = T))){ # if the data doesn't have the heroin/yoked column, add a placeholder XX should check if this value is shared across phenotypes in the same cohort
        data_breeder <- data_breeder %>% 
          tibble::add_column(heroin_salineyoked = "heroin_salineyoked", .after = 5) # chose index location instead of name bc maybe inconsistent naming
      }

      databegins_index <- grep("date", data_breeder[,7] %>% unlist() %>% as.character, ignore.case = T)
      if(grepl("05", i)&grepl("extinction", sheet)){
        databegins_index <- grep("date", data_breeder[,8] %>% unlist() %>% as.character, ignore.case = T)
      }
            
      # names(data_breeder)[1:8] <- data_breeder[datavaluesbegins_index, 1:8] %>% unlist() %>% as.character()
      # names(data_breeder)[9:ncol(data_breeder)] <- data_breeder[databegins_index, 9:ncol(data_breeder)] %>% unlist() %>% as.character()
      make_unique = function(x, sep='_'){
        ave(x, x, FUN=function(a){if(length(a) > 1){paste(a, 1:length(a), sep=sep)} else {a}})
      }

      df_values <- data_breeder[(datavaluesbegins_index + 1):nrow(data_breeder),]
      
      if(grepl("03", i)&grepl("_12h", sheet)){ # make fix when they leave out the headers 
        
        data_breeder <- data_breeder %>% 
          mutate_if(is.logical, as.character)
        data_breeder[datavaluesbegins_index, 32] <- "infusions"
        
        data_breeder[datavaluesbegins_index, 33] <- "activelever"
        
        data_breeder[datavaluesbegins_index, 34] <- "inactivelever"
      }
      
      names(df_values) <- data_breeder[datavaluesbegins_index,] %>% gsub(" ", "", .) %>% tolower() %>% make_unique() %>% make_clean_names() %>% make_unique()  
      
      
      df_values <- df_values %>%
        rename("microchip" = "transponderid",
               "cohort_number" = "batchnumber") %>% 
        rename_all(~ stringr::str_replace_all(., 'behavioralcarachterizationunit', 'behavioralcharacterizationunit')) %>% 
        rename_all(~ stringr::str_replace_all(., '.*(rat|animal).*?id', 'internal_id')) %>%     
        rename_all(~ stringr::str_replace_all(., '.*room$', 'saroom')) %>%       
        rename_all(~ stringr::str_replace_all(., '.*box$', 'sabox')) %>%       
        dplyr::filter(!is.na(internal_id)) %>% 
        gather(var, value, -microchip, -sex, -cohort_number, -internal_id, -behavioralcharacterizationunit, -heroin_salineyoked, -saroom, -sabox) %>%
        extract(var, c("measurement", "session"), "(\\D+_?)_(\\d+)", remove = F) %>%
        mutate(measurement = coalesce(measurement, var))
      
      
      if(grepl("_12h|_1h", sheet)){
        df_values <- df_values %>% 
        rowwise() %>% 
        mutate(session = replace(session, session == "13"|measurement == "bp", "pr"),
               session = replace(session, session %in% c("14", "15", "16"), as.character(as.numeric(session) - 1))) %>% 
        ungroup() %>% 
        select(-var) %>% 
        subset(!(is.na(measurement)|measurement == "na"))
      }
      
      if(grepl("extinction", sheet)){
        df_values <- df_values %>% 
          rowwise() %>% 
          mutate(session = replace(session, session == "7"|measurement == "infusions", "relapse")) %>% 
          ungroup() %>% 
          select(-var) %>% 
          subset(!(is.na(measurement)|measurement == "na"))
      }
      
      if(grepl("priming", sheet)){
        df_values <- df_values %>% 
          rowwise() %>% 
          mutate(session = replace(session, session == "1", "priming"),
                 session = replace(session, session %in% as.character(c(2:7)), as.character(as.numeric(session) - 1))) %>% 
          ungroup() %>% 
          select(-var) %>% 
          subset(!(is.na(measurement)|measurement == "na"))
      }
      
      
      # join to the session dosage information
      df_sessiondosage <- data_breeder[1:(datavaluesbegins_index-1),7:ncol(data_breeder)]
      
      not_all_na <- function(x) any(!is.na(x))
      if(grepl("_12h|_1h", sheet)){
        df_sessiondosage <- df_sessiondosage %>%
          # select_if(function(x) all(!is.na(x))) %>% # only select columns that have no na
          select_if(not_all_na) %>% 
          t() %>%
          # cbind(rownames(.), ., row.names = NULL) %>%
          as.data.frame(row.names = NULL) %>%
          mutate_all(str_trim) %>%
          magrittr::set_colnames(.[1, ] %>% unlist() %>% as.character %>% tolower %>% make_clean_names()) %>%
          dplyr::filter(row_number() != 1) %>%
          mutate(date = openxlsx::convertToDateTime(date, origin = "1900-01-01") %>% as.character,
                 session = ifelse(grepl("\\d+", session), stringr::str_extract(session, "\\d+"), "pr")) %>% 
          rowwise() %>% 
          mutate(session = replace(session, session == "13", "pr"),
                 session = replace(session, session %in% c("14", "15", "16"), as.character(as.numeric(session) - 1))) %>% 
          ungroup()
      }
      
      if(grepl("extinction", sheet)){
        df_sessiondosage <- df_sessiondosage %>%
          # select_if(function(x) all(!is.na(x))) %>% # only select columns that have no na
          select_if(not_all_na) %>% 
          t() %>%
          # cbind(rownames(.), ., row.names = NULL) %>%
          as.data.frame(row.names = NULL) %>%
          mutate_all(str_trim) %>%
          magrittr::set_colnames(.[1, ] %>% unlist() %>% as.character %>% tolower %>% make_clean_names()) %>%
          dplyr::filter(row_number() != 1) %>% 
          rename_all(~ stringr::str_replace_all(., '^na$', 'session')) %>%       
          mutate(date = openxlsx::convertToDateTime(date, origin = "1900-01-01") %>% as.character,
                 session = ifelse(grepl("\\d+", session), stringr::str_extract(session, "\\d+"), "relapse")) %>% 
          rowwise() %>% 
          mutate(session = replace(session, session == "7", "relapse")) %>% 
          ungroup()
      }
      
      if(grepl("priming", sheet)){
        df_sessiondosage <- df_sessiondosage %>%
          # select_if(function(x) all(!is.na(x))) %>% # only select columns that have no na
          select_if(not_all_na) %>% 
          t() %>%
          # cbind(rownames(.), ., row.names = NULL) %>%
          as.data.frame(row.names = NULL) %>%
          mutate_all(str_trim) %>%
          magrittr::set_colnames(.[1, ] %>% unlist() %>% as.character %>% tolower %>% make_clean_names()) %>%
          dplyr::filter(row_number() != 1) %>% 
          rename_all(~ stringr::str_replace_all(., '^na$', 'session')) %>%   
          rename_all(~ stringr::str_replace_all(., '^session_hour', 'session_length')) %>%  
          rename_all(~ stringr::str_replace_all(., '^session_phase', 'reinforcement_schedule')) %>%    
          rowwise() %>% 
          mutate(date = if(exists('date', where = .)) openxlsx::convertToDateTime(date, origin = "1900-01-01") %>% as.character else NA,
                 session = replace(session, !grepl("^prim", session, ignore.case = T), stringr::str_extract(session_length, "\\d+"))) %>% 
          mutate(session = replace(session, grepl("^prim", session, ignore.case = T), "priming")) %>% 
          ungroup()
      }
      
      
      
      df <- left_join(df_values, df_sessiondosage, by = "session") %>%
        mutate(cohort_number = gsub("MUSC_", "", cohort_number)) %>%
        rename("rfid" = "microchip", 
               "cohort" = "cohort_number")
      
      return(df)
    }  
    else if(!grepl(sheet, names(data_allsheets))){
      df <- data.frame(rfid = NA, 
                       sex = NA, 
                       cohort = i,
                       internal_id = NA, 
                       behavioralcharacterizationunit = NA, 
                       heroin_salineyoked = NA,
                       saroom = NA,
                       sabox = NA, 
                       measurement = NA, 
                       session = NA, 
                       value = NA, 
                       date = NA, 
                       session_length = NA, 
                       reinforcer = NA, 
                       bolus_volume = NA, 
                       dose = NA, 
                       reinforcement_schedule = NA, 
                       time_out = NA, 
                       contextual_stimulus = NA, 
                       discrete_stimulus = NA)
      return(df)
    }
  })
  
  return(data_breeder_list)
}



kalivas_italy_lga_excel_c01_10 <- extract_process_excel_manysessions_lapply_italy(all_excel_fnames_c01_10, "heroin_sa_12h") 
names(kalivas_italy_lga_excel_c01_10) <- all_excel_fnames_c01_10
kalivas_italy_lga_excel_c01_10_df <- kalivas_italy_lga_excel_c01_10 %>% 
  rbindlist(fill = T, idcol = "file")


## clean df 
kalivas_italy_lga_excel_processed_c01_10_df <- kalivas_italy_lga_excel_c01_10_df %>% 
  mutate(cohort = gsub(".*batch-(\\d+).*", "\\1", file, ignore.case = T) %>% parse_number(),
         cohort = paste0("C", str_pad(as.character(cohort), 2, "left", "0"))) %>%
  left_join()

kalivas_italy_shipping_metadata_c01_10_df_ids


############################
### SHA 
############################
kalivas_italy_sha_excel_c01_10 <- extract_process_excel_manysessions_lapply_italy(all_excel_fnames_c01_10, "heroin_sa_1h") 
names(kalivas_italy_sha_excel_c01_10) <- all_excel_fnames_c01_10
kalivas_italy_sha_excel_c01_10_df <- kalivas_italy_sha_excel_c01_10 %>% 
  rbindlist(fill = T, idcol = "file")


############################
### Extinction
############################
kalivas_italy_extinction_excel_c01_10 <- extract_process_excel_manysessions_lapply_italy(all_excel_fnames_c01_10, "extinction") 
names(kalivas_italy_extinction_excel_c01_10) <- all_excel_fnames_c01_10
kalivas_italy_extinction_excel_c01_10_df <- kalivas_italy_extinction_excel_c01_10 %>% 
  rbindlist(fill = T, idcol = "file")

kalivas_italy_extinction_excel_c01_10_df <- kalivas_italy_extinction_excel_c01_10_df %>% 
  mutate(saroom = parse_number(saroom) %>% as.character(),
         sabox = tolower(sabox) %>% gsub(" ", "", .) %>% gsub("(\\d+)(\\D+)", "\\2\\1", .)) 
## XX come back to fix cohort 03/03/2021

############################
### Priming
############################
kalivas_italy_priming_excel_c01_10 <- extract_process_excel_manysessions_lapply_italy(all_excel_fnames_c01_10, "priming") 
names(kalivas_italy_priming_excel_c01_10) <- all_excel_fnames_c01_10
kalivas_italy_priming_excel_c01_10_df <- kalivas_italy_priming_excel_c01_10 %>% 
  rbindlist(fill = T, idcol = "file")







############################
#### CREATE GWAS VARIABLES
############################

total_consumption_c01_10 <- kalivas_italy_lga_excel_c01_10_df %>% 
  mutate(rfid = gsub("([.]|E14)", "", rfid),
         rfid = ifelse(nchar(rfid) == 14, paste0(rfid, "0"), rfid)) %>% 
  subset(measurement == "infusions"& as.numeric(session) <= 12) %>% 
  group_by(rfid) %>% 
  mutate(total_consumption=sum(as.numeric(value), na.rm = T)*20) %>% 
  ungroup() %>%
  mutate(cohort = gsub(".*batch[- ](\\d+).*", "\\1", file, ignore.case = T) %>% parse_number(),
         cohort = paste0("C", str_pad(as.character(cohort), 2, "left", "0"))) %>% 
  subset(!is.na(rfid)) %>% # checked in the Excel, no rfid's, completely empty rows  
  select(cohort, rfid, total_consumption)


escalation <- kalivas_italy_lga_excel_c01_10_df %>% 
  mutate(rfid = gsub("([.]|E14)", "", rfid),
         rfid = ifelse(nchar(rfid) == 14, paste0(rfid, "0"), rfid)) %>% 
  subset(measurement == "infusions"& session %in% c("1", "2", "3", "10", "11", "12")) %>%
  mutate(session_cat = ifelse(session %in% c("1", "2", "3"), "start", "end")) %>%  # label the sessions by start of the experiment or end of the experiment
  group_by(rfid, session_cat) %>% 
  mutate(session_mean=mean(as.numeric(value), na.rm = T)) %>% 
  ungroup() %>%
  mutate(cohort = gsub(".*batch[- ](\\d+).*", "\\1", file, ignore.case = T) %>% parse_number(),
         cohort = paste0("C", str_pad(as.character(cohort), 2, "left", "0"))) %>% 
  subset(!is.na(rfid)) %>% # checked in the Excel, no rfid's, completely empty rows  
  distinct(cohort, rfid, session_cat, session_mean) %>% 
  mutate_if(is.numeric, ~.*20) %>% 
  spread(session_cat, session_mean) %>% 
  mutate(esc = end - start) %>% 
  select(-end, -start)
  
  

  






