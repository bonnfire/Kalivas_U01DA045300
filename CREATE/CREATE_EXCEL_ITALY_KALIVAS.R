## CREATE EXCEL ITALY AND U.S. 

## EXTRACT KALIVAS ITALY EXCEL 

setwd("~/Dropbox (Palmer Lab)/Roberto_Ciccocioppo_U01/raw data")
italy_filenames_xl <- list.files(recursive = T)


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
all_excel_fnames_c01_06 <- list.files(path = "~/Dropbox (Palmer Lab)/Roberto_Ciccocioppo_U01/raw data/", full.names = T, recursive = T) 



############################
# ELEVATED PLUS MAZE
############################


all_excel_fnames_c01_06, "tailflick"

extract_process_excel_repeatedmeasures2_lapply_italy <-  function(files, sheet){ # for use on before self admin and after self admin
  data_breeder_list <-  lapply(files, function(i) {
    
    u01.importxlsx <- function(xlname){
      path_sheetnames <- excel_sheets(xlname)
      df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname, col_names = F) ## note the difference here, bc we don't want headers 
      names(df) <- path_sheetnames
      return(df)
    }
    
    data_allsheets = u01.importxlsx(i)
    
    # data_allsheets = u01.importxlsx(all_excel_fnames_c01_06[1])
    
    
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


kalivas_italy_epm_excel_processed_c01_06 <- extract_process_excel_repeatedmeasures2_lapply_italy(all_excel_fnames_c01_06, "epm") 
names(kalivas_italy_epm_excel_processed_c01_06) <- all_excel_fnames_c01_06
kalivas_italy_epm_excel_processed_c01_06_df <- kalivas_italy_epm_excel_processed_c01_06 %>% rbindlist(idcol = "file")

kalivas_italy_epm_excel_processed_c01_06_df <- kalivas_italy_epm_excel_processed_c01_06_df %>% 
  rename("cohort" = "cohort_number") %>% 
  mutate(cohort = paste0("C", str_pad(str_match(tolower(file), "cohort \\d+") %>% parse_number() %>% as.character(), 2, "left", "0"))) %>% 
  naniar::replace_with_na_all(condition = ~.x %in% c("N/A", "NA", ""))




############################
# OPEN FIELD TASK
############################


kalivas_italy_oft_excel_processed_c01_06 <- extract_process_excel_repeatedmeasures2_lapply_italy(all_excel_fnames_c01_06, "openfield") # not sure why heroin yoked error
names(kalivas_italy_oft_excel_processed_c01_06) <- all_excel_fnames_c01_06
kalivas_italy_oft_excel_processed_c01_06_df <- kalivas_italy_oft_excel_processed_c01_06 %>% rbindlist(idcol = "file", fill = T) ## XX temporary 

kalivas_italy_oft_excel_processed_c01_06_df <- kalivas_italy_oft_excel_processed_c01_06_df %>%
  mutate(cohort = paste0("C", str_pad(str_match(tolower(file), "cohort \\d+") %>% parse_number() %>% as.character(), 2, "left", "0"))) %>% 
  select(-cohort_number) %>% 
  naniar::replace_with_na_all(condition = ~.x %in% c("N/A", "NA", ""))

kalivas_italy_oft_excel_processed_c01_06_df_wide <- kalivas_italy_oft_excel_processed_c01_06_df %>% 
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
    # data_allsheets = u01.importxlsx(all_excel_fnames_c01_06[1])
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

kalivas_italy_tf_excel_processed_c01_06 <- tailflick_italy(all_excel_fnames_c01_06, "tailflick") 
names(kalivas_italy_tf_excel_processed_c01_06) <- all_excel_fnames_c01_06
kalivas_italy_tf_excel_processed_c01_06_df <- kalivas_italy_tf_excel_processed_c01_06 %>% rbindlist(fill = T, id = "file")



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
    
    data_allsheets = u01.importxlsx(i) # data_allsheets = u01.importxlsx(all_excel_fnames_c01_06[2]) ## XX remove, just here for troubleshooting 
    
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
      # data_breeder <- data_allsheets[[grep(sheet, names(data_allsheets), ignore.case = T)]] # made to extract any of the sheets
      data_breeder <- data_allsheets[[grep("heroin_sa_12h", names(data_allsheets), ignore.case = T)]] # made to extract any of the sheets
      
      ## reorder columns in specific cohorts
      if(grepl("06", i)){ # make fix when they leave out the headers 
        data_breeder[1:4] <- data_breeder[, c(3, 2, 1, 4)]
      }
      
      if(grepl("batch-01", i, ignore.case = T)){ # make fix when they leave out the headers 
        data_breeder[1:5] <- data_breeder[, c(2, 3, 5, 1, 4)]
      }
      
      if(!any(grepl("tra[n]?sponder", data_breeder[, 1] %>% unlist() %>% as.character, ignore.case = T))){ # if the data doesn't have the transponder id column, add a placeholder
        data_breeder <- data_breeder %>% 
          tibble::add_column(transponderid = "transponder id", .before = 1) # chose index location instead of name bc maybe inconsistent naming
      }
      
      if(any(grepl("trasponder", data_breeder[, 1] %>% unlist() %>% as.character, ignore.case = T))){ # fix transponder column id typo
        data_breeder[grep("trasponder", data_breeder[, 1] %>% unlist() %>% as.character(), ignore.case = T), 1] <- "Transponder ID"
      }
      
      datavaluesbegins_index <- grep("transponder id", data_breeder[,1] %>% unlist() %>% as.character, ignore.case = T)[1]
      
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
      
      # names(data_breeder)[1:8] <- data_breeder[datavaluesbegins_index, 1:8] %>% unlist() %>% as.character()
      # names(data_breeder)[9:ncol(data_breeder)] <- data_breeder[databegins_index, 9:ncol(data_breeder)] %>% unlist() %>% as.character()
      make_unique = function(x, sep='_'){
        ave(x, x, FUN=function(a){if(length(a) > 1){paste(a, 1:length(a), sep=sep)} else {a}})
      }

      df_values <- data_breeder[(datavaluesbegins_index + 1):nrow(data_breeder),]
      
      if(grepl("03", i)){ # make fix when they leave out the headers 
        
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
        mutate(measurement = coalesce(measurement, var)) %>% 
        rowwise() %>% 
        mutate(session = replace(session, session == "13"|measurement == "bp", "pr"),
               session = replace(session, session %in% c("14", "15", "16"), as.character(as.numeric(session) - 1))) %>% 
        ungroup() %>% 
        select(-var) %>% 
        subset(!(is.na(measurement)|measurement == "na"))
        # spread(measurement, value) %>% 
        # mutate(sabox = sabox %>% as.numeric %>% round(2) %>% as.character)
      
      df_sessiondosage <- data_breeder[1:(datavaluesbegins_index-1),7:ncol(data_breeder)]
      
      not_all_na <- function(x) any(!is.na(x))
    
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



kalivas_italy_lga_excel_processed_c01_06 <- extract_process_excel_manysessions_lapply_italy(all_excel_fnames_c01_06, "heroin_sa_12h") 
names(kalivas_italy_lga_excel_processed_c01_06) <- all_excel_fnames_c01_06
kalivas_italy_lga_excel_processed_c01_06_df <- kalivas_italy_lga_excel_processed_c01_06 %>% 
  rbindlist(fill = T, idcol = "file")




