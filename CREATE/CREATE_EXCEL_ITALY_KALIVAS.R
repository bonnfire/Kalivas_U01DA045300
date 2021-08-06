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
u01.importxlsx <- function(xlname){
  path_sheetnames <- excel_sheets(xlname)
  df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname)
  names(df) <- path_sheetnames
  return(df)
} 
all_excel_fnames_c01_10 <- list.files(path = "~/Dropbox (Palmer Lab)/Roberto_Ciccocioppo_U01/raw data/", full.names = T, recursive = T) %>% grep("batch[- ]?(0[1-9]|10)", ., value = T, ignore.case = T)


# extract shipping information for metadata 
kalivas_italy_extract_shipping_metadata <- function(files, sheet){
  setwd("~/Dropbox (Palmer Lab)/Roberto_Ciccocioppo_U01/raw data/")
  
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
      gsub(".*extinction.*", "extinction", .) %>% 
      gsub(".*surgery.*", "surgery", .)
    
    if(any(grepl(sheet, names(data_allsheets)))){
    data_breeder <- data_allsheets[[grep(sheet, names(data_allsheets), ignore.case = T)]] # made to extract any of the sheets
    
    df <- data_breeder 
    names(df) <- df[1, ] %>% unlist() %>% as.character %>% make_clean_names() 
    df <- df[-1, ]
    
    # go through the rows to find the row with the column names and clean it up
    while(grepl("^na|^info", names(df)[1])&grepl("^na", names(df)[2])){
      names(df) <- df[1, ] %>% unlist() %>% as.character %>% make_clean_names() 
      df <- df[-1, ]
    }
    
    return(df)
    }
    else
      df <- data.frame()
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
         transponder_id = ifelse(nchar(transponder_id) == 14, paste0(transponder_id, "0"), transponder_id),
         animal_id = coalesce(animal_id, animal_number)) %>%
  subset(!is.na(transponder_id)) %>% 
  mutate(d_o_b = as.numeric(d_o_b) %>% as.character() %>% openxlsx::convertToDate() %>% as.POSIXct(format="%Y-%m-%d")) %>% 
  select(cohort, transponder_id, animal_id, sex, d_o_b, coat_color)


# join to the metadata here to verify rfid's and fill in dob's 

# join to postgresql database 
# library("RPostgreSQL") 
# library("DBI")
con <- dbConnect(dbDriver("PostgreSQL"), dbname="PalmerLab_Datasets",user="postgres",password="postgres")
kalivas_metadata_db <- dbGetQuery(con, "select * from \"u01_peter_kalivas_italy\".\"wfu_master\"")

kalivas_italy_excel_metadata_c01_10_df <- kalivas_italy_shipping_metadata_c01_10_df_ids %>% 
  mutate(d_o_b = as.Date(d_o_b)) %>%
  full_join(kalivas_metadata %>%
              select(cohort, rfid, sex, dob, coatcolor, comments), by = c("cohort", "transponder_id" = "rfid")) %>% # created with WFU EXCEL R ### WFU_KalivasItaly <- WFU_KalivasItaly_test_df %>% bind_rows(kalivasitaly_07_wfu_metadata %>% mutate_at(vars(one_of("dob", "dow", "shipmentdate")), as.Date)) %>% bind_rows(kalivasitaly_08_wfu_metadata%>% mutate_at(vars(one_of("dob", "dow", "shipmentdate")), as.Date) %>% mutate_at(vars(one_of("litternumber", "littersize", "shipmentbox", "shipmentage", "weanage")), as.numeric)) %>% bind_rows(kalivasitaly_09_wfu_metadata%>% mutate_at(vars(one_of("dob", "dow", "shipmentdate")), as.Date) %>% mutate_at(vars(one_of("litternumber", "littersize", "shipmentbox", "shipmentage", "weanage")), as.numeric))%>% bind_rows(kalivasitaly_10_wfu_metadata%>% mutate_at(vars(one_of("dob", "dow", "shipmentdate")), as.Date) %>% mutate_at(vars(one_of("litternumber", "littersize", "shipmentbox", "shipmentage", "weanage")), as.numeric))
  mutate(sex.x = coalesce(sex.x, sex.y),
         d_o_b = coalesce(d_o_b, dob)) %>% 
  rename("sex" = "sex.y",
         "rfid" = "transponder_id") %>% 
  mutate(rfid = as.character(rfid))
  

write.csv(kalivas_italy_excel_metadata_c01_10_df, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/italy/generated/kalivas_italy_excel_metadata_c01_10_df.csv", row.names = F)
kalivas_italy_metadata_c01_10_df <- read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/italy/generated/kalivas_italy_excel_metadata_c01_10_df.csv", colClasses = "character") %>% 
  select(cohort, rfid, animal_id, sex, dob, coatcolor, comments)

# check for incorrectly labelled sexes
kalivas_italy_metadata_c01_10_df %>% subset(sex.x != sex.y)
kalivas_italy_metadata_c01_10_df %>% subset(d_o_b != dob)
kalivas_italy_metadata_c01_10_df %>% get_dupes(rfid)
kalivas_italy_metadata_c01_10_df %>% subset(is.na(animal_id)) # they all have comments explaining why 


rm(list = ls(pattern = '(kalivas_italy_shipping_metadata_c01_10)'))



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
      gsub("of.*", "openfield", .) %>% 
      gsub(".*12h.*", "heroin_sa_12h", .) %>% 
      gsub(".*1(h|st).*", "heroin_sa_1h", .) %>% 
      gsub(".priming.*", "priming", .) %>% 
      gsub(".*extinction.*", "extinction", .)
    
    
    if(any(grepl(sheet, names(data_allsheets)))){
    # data_breeder <- data_allsheets[[sheet]] # made to extract any of the sheets
    data_breeder <- data_allsheets[[grep(sheet, names(data_allsheets), ignore.case = T)]] # made to extract any of the sheets
    
    # if(any(data_breeder[,3] %>% unique == "unicam_07")&sheet == "openfield"){ ## if this data_breeder is cohort 7's
    #   data_breeder <- rbind(names(data_breeder), data_breeder) 
    # }
    
    
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
    
    ## check with team why 
    # if(any(data_breeder[,3] %>% unique == "unicam_07")&sheet == "openfield"){ ## if this data_breeder is cohort 7's
    #   data_breeder <- rbind(names(data_breeder), data_breeder) 
    # }
    
    if(any(grepl("HR/LR", data_breeder[2, ] %>% unlist() %>% as.character()))){
      data_breeder <- data_breeder[, -(grep("HR/LR", data_breeder[2, ] %>% unlist() %>% as.character))]
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
      rename_at(vars(matches("^transponderid$")), function(x) "microchip") %>% 
      rename_at(vars(matches("^behavioralcarachterizationunit$")), function(x) "behavioralcharacterizationunit") %>% 
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


kalivas_italy_epm_excel_processed_c01_10 <- extract_process_excel_repeatedmeasures2_lapply_italy(all_excel_fnames_c01_10, "epm") 
names(kalivas_italy_epm_excel_processed_c01_10) <- all_excel_fnames_c01_10
kalivas_italy_epm_excel_processed_c01_10_df <- kalivas_italy_epm_excel_processed_c01_10 %>% rbindlist(idcol = "file", use.names = T)

kalivas_italy_epm_excel_processed_c01_10_df <- kalivas_italy_epm_excel_processed_c01_10_df %>% 
  rename("cohort" = "cohort_number") %>% 
  mutate(cohort = paste0("C", str_pad(str_match(tolower(file), "cohort \\d+") %>% parse_number() %>% as.character(), 2, "left", "0"))) %>% 
  naniar::replace_with_na_all(condition = ~.x %in% c("N/A", "NA", ""))



############################
# OPEN FIELD TASK
############################


kalivas_italy_oft_excel_processed_c01_10 <- extract_process_excel_repeatedmeasures2_lapply_italy(all_excel_fnames_c01_10, "openfield") # not sure why heroin yoked error
names(kalivas_italy_oft_excel_processed_c01_10) <- all_excel_fnames_c01_10
kalivas_italy_oft_excel_processed_c01_10_df <- kalivas_italy_oft_excel_processed_c01_10 %>% rbindlist(idcol = "file", fill = T)

kalivas_italy_oft_excel_processed_c01_10_df <- kalivas_italy_oft_excel_processed_c01_10_df %>%
  mutate(cohort = paste0("C", str_pad(str_match(tolower(file), "cohort \\d+") %>% parse_number() %>% as.character(), 2, "left", "0"))) %>% 
  select(-cohort_number) %>% 
  naniar::replace_with_na_all(condition = ~.x %in% c("N/A", "NA", "")) %>% 
  subset(!is.na(session)) %>% # remove because all columns are na's
  subset(!is.na(date))
# 
# kalivas_italy_oft_excel_processed_c01_10_df_wide <- kalivas_italy_oft_excel_processed_c01_10_df %>% 
#   pivot_wider(names_from = session, values_from = date:ambulatorytime_sec) 


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
  setwd("~/Dropbox (Palmer Lab)/Roberto_Ciccocioppo_U01/raw data/")
  
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
# kalivas_italy_lga_excel_processed_c01_10_df <- kalivas_italy_lga_excel_c01_10_df %>% 
#   mutate(cohort = gsub(".*batch-(\\d+).*", "\\1", file, ignore.case = T) %>% parse_number(),
#          cohort = paste0("C", str_pad(as.character(cohort), 2, "left", "0"))) %>%
#   left_join()
# 
# kalivas_italy_shipping_metadata_c01_10_df_ids


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

############################
### Priming
############################
kalivas_italy_priming_excel_c01_10 <- extract_process_excel_manysessions_lapply_italy(all_excel_fnames_c01_10, "priming") 
names(kalivas_italy_priming_excel_c01_10) <- all_excel_fnames_c01_10
kalivas_italy_priming_excel_c01_10_df <- kalivas_italy_priming_excel_c01_10 %>% 
  rbindlist(fill = T, idcol = "file")
## see gwas code below to correct sessions in cohorts

kalivas_italy_priming_excel_c01_10_df <- kalivas_italy_priming_excel_c01_10_df %>% 
  mutate(cohort = gsub(".*batch[- ](\\d+).*", "\\1", file, ignore.case = T) %>% parse_number(),
         cohort = paste0("C", str_pad(as.character(cohort), 2, "left", "0"))) %>% 
  mutate(sabox = toupper(sabox) %>% gsub("(\\d+) (LEFT|RIGHT)", "\\2 \\1", .) %>% gsub("[.]0", "", .)) %>% 
  mutate(internal_id = parse_number(internal_id) %>% str_pad(3, "left", "0") %>% paste0("IT", .)) %>%  # reformat the lab animal id   
  mutate(saroom = as.numeric(saroom) %>% as.character)

## check if transponder/id's is different 
kalivas_italy_priming_excel_c01_10_df %>% full_join(kalivas_italy_metadata_c01_10_df, by = "rfid") %>% naniar::vis_miss()
# check if the sheet id's are different from the general info page 
kalivas_italy_priming_excel_c01_10_df %>% full_join(kalivas_italy_metadata_c01_10_df, by = "rfid") %>% subset(internal_id != animal_id|cohort.x!=cohort.y|sex.x!=sex.y) %>% select(cohort.x, cohort.y, rfid, sex.x, sex.y, internal_id, animal_id) %>% View()
kalivas_italy_priming_excel_c01_10_df %>% full_join(kalivas_italy_metadata_c01_10_df, by = "rfid") %>% subset(internal_id!=animal_id&!cohort.x %in% c("C06", "C08")&!cohort.y %in% c("C06", "C08")) %>% select(cohort.x, cohort.y, rfid, sex.x, sex.y, internal_id, animal_id) %>% distinct %>% View()

kalivas_italy_priming_excel_c01_10_df <- kalivas_italy_priming_excel_c01_10_df %>% 
  select(-rfid, -sex) %>% 
  left_join(kalivas_metadata_db %>% 
              mutate(labanimalid = parse_number(labanimalid) %>% str_pad(3, "left", "0") %>% paste0("IT", .)) %>%  # reformat the lab animal id   
              select(labanimalid, rfid, sex), by = c("internal_id" = "labanimalid")) %>% 
  subset(internal_id != "ITNA") %>% 
  subset(!cohort %in% c("C01", "C02"))


############################
### Box allocation
############################
italy_rat_boxes <- u01.importxlsx("~/Dropbox (Palmer Lab)/Roberto_Ciccocioppo_U01/MedPC_data file/RAT-BOX ALLOCATION.xlsx") %>% 
  lapply(function(x){
    x <- x %>% 
      clean_names %>% 
      select_all(~gsub(".*batch.*", "cohort", tolower(.))) %>% 
      select_all(~gsub(".*(internal|animal).*", "labanimalid", tolower(.))) %>% 
      select_all(~gsub(".*room.*", "room", tolower(.))) %>% 
      select_all(~gsub(".*box.*", "box", tolower(.))) %>% 
      select_all(~gsub(".*ponder_id.*", "rfid", tolower(.))) %>% 
      select_all(~gsub(".*heroin.*", "heroin_saline_yoked", tolower(.))) 
    return(x)
    })
italy_rat_boxes_df <- italy_rat_boxes %>% 
  rbindlist(fill = T) %>% 
  rename('comments' = 'x8') %>% 
  select(cohort, sex, labanimalid, rfid, room, box, heroin_saline_yoked, comments) %>% 
  mutate(labanimalid = ifelse(rfid == "933000320048660", "IT280", labanimalid)) %>% 
  mutate(labanimalid_num = parse_number(labanimalid)) %>% 
  subset(!is.na(cohort)) %>%
  subset(!is.na(rfid)) %>% 
  mutate(cohort = parse_number(cohort) %>% str_pad(2, side = "left", "0") %>% paste0("C", .)) %>% 
  mutate(box = toupper(box) %>% gsub("(\\d+) (LEFT|RIGHT)", "\\2 \\1", .)) %>% 
  mutate(heroin_saline_yoked = toupper(heroin_saline_yoked))


# check across files 
## XX 08/04/2021
kalivas_italy_priming_excel_c01_10_df %>% select(cohort, saroom, sabox, internal_id, sex) %>% 
  left_join(italy_rat_boxes_df %>% 
              mutate(room = as.character(room)), by = c("cohort", "saroom" = "room", "sabox" = "box")) %>% subset(!is.na(labanimalid))




############################
#### CREATE GWAS VARIABLES
############################

# total heroin comsumption
# sum of all infusions from self admin sessions 1-12
total_consumption_c01_10 <- kalivas_italy_lga_excel_c01_10_df %>% 
  mutate(rfid = gsub("([.]|E14)", "", rfid),
         rfid = ifelse(nchar(rfid) == 14, paste0(rfid, "0"), rfid)) %>% 
  subset(measurement == "infusions"& as.numeric(session) <= 12) %>% 
  group_by(rfid, internal_id) %>% 
  mutate(total_consumption=sum(as.numeric(value), na.rm = T)*20) %>% 
  mutate(date_consumption = max(lubridate::ymd(date), na.rm = T)) %>% # find the date of the last session
  ungroup() %>%
  mutate(cohort = gsub(".*batch[- ](\\d+).*", "\\1", file, ignore.case = T) %>% parse_number(),
         cohort = paste0("C", str_pad(as.character(cohort), 2, "left", "0"))) %>% 
  subset(!is.na(rfid)) %>% # checked in the Excel, no rfid's, completely empty rows  
  distinct(cohort, rfid, internal_id, total_consumption, date_consumption)

# write csv for beverly 
write.csv(kalivas_italy_lga_excel_c01_10_df, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/preliminary GWAS files/italy_lga_sessions_xl.csv", row.names = F)
write.csv(total_consumption_c01_10, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/preliminary GWAS files/italy_consumption_xl.csv", row.names = F)

# escalation 
# (mean of sessions 10-12)*20 - (mean of sessions 1-3)*20
escalation_c01_10 <- kalivas_italy_lga_excel_c01_10_df %>% 
  mutate(rfid = gsub("([.]|E14)", "", rfid),
         rfid = ifelse(nchar(rfid) == 14, paste0(rfid, "0"), rfid)) %>% 
  subset(measurement == "infusions"& session %in% c("1", "2", "3", "10", "11", "12")) %>%
  mutate(session_cat = ifelse(session %in% c("1", "2", "3"), "start", "end")) %>%  # label the sessions by start of the experiment or end of the experiment
  group_by(rfid, internal_id, session_cat) %>% 
  mutate(session_mean=mean(as.numeric(value), na.rm = T)) %>% 
  ungroup() %>%
  mutate(cohort = gsub(".*batch[- ](\\d+).*", "\\1", file, ignore.case = T) %>% parse_number(),
         cohort = paste0("C", str_pad(as.character(cohort), 2, "left", "0"))) %>% 
  subset(!is.na(rfid)) %>% # checked in the Excel, no rfid's, completely empty rows
  group_by(rfid, internal_id) %>%
  mutate(date_esc = max(lubridate::ymd(date), na.rm = T)) %>% # find the date of the last session
  ungroup() %>% 
  distinct(cohort, rfid, internal_id, session_cat, session_mean, date_esc) %>% 
  mutate_if(is.numeric, ~.*20) %>% 
  spread(session_cat, session_mean) %>% 
  mutate(esc = end - start) %>% 
  select(-end, -start)
  


# breakpoint
# breakpoint value in the PR session after first set of self admin sessions
## missing cohorts 2 

breakpoint_c01_10 <- kalivas_italy_lga_excel_c01_10_df %>% 
  mutate(rfid = gsub("([.]|E14)", "", rfid),
         rfid = ifelse(nchar(rfid) == 14, paste0(rfid, "0"), rfid)) %>% 
  mutate(measurement = ifelse(measurement == "breakpoint", "bp", measurement)) %>% 
  subset(measurement == "bp"& session == "pr") %>% 
  group_by(rfid, internal_id) %>%
  mutate(date_min = min(lubridate::ymd(date), na.rm = T)) %>% # find the date of the last session
  ungroup() %>% 
  subset(date == date_min) %>% 
  mutate(cohort = gsub(".*batch[- ](\\d+).*", "\\1", file, ignore.case = T) %>% parse_number(),
         cohort = paste0("C", str_pad(as.character(cohort), 2, "left", "0"))) %>%
  rename("breakpoint" = "value",
         "date_prbreakpoint" = "date") %>% 
  select(cohort, rfid, internal_id, breakpoint, date_prbreakpoint) 

## Day 1 extinction 
# exclude from this analysis run 

# heroin prime seeking 
# sum of the active lever during the 5th and 6th hour of the session

prime_seeking_c01_10 <- kalivas_italy_priming_excel_c01_10_df %>%
  mutate(rfid = gsub("([.]|E14)", "", rfid),
         rfid = ifelse(nchar(rfid) == 14, paste0(rfid, "0"), rfid)) %>%
  rowwise() %>%
  mutate(session = replace(session, session == "1"&cohort %in% c("C03", "C04", "C06", "C07", "C08", "C09", "C10"), "session_total"),
         session = replace(session, session %in% as.character(c(2:7))&cohort%in% c("C03", "C04", "C06", "C07", "C08", "C09", "C10"), as.character(as.numeric(session) - 1))) %>%
  ungroup() %>%
  subset(measurement == "activelever"&session %in% c("5", "6")) %>%
  mutate(session = paste0("session", session),
         value = as.numeric(value)) %>% 
  select(cohort, rfid, internal_id, session, value) %>% 
  spread(session, value) %>% 
  mutate(prime_active = rowSums((.[,c("session5", "session6")]), na.rm = T)) %>% 
  select(cohort, rfid, internal_id, prime_active) ## date column has a lot of NA's


# Day 6 extinction 
# active lever presses

extinction_6_c01_10 <- kalivas_italy_extinction_excel_c01_10_df %>% 
  mutate(rfid = gsub("([.]|E14)", "", rfid),
         rfid = ifelse(nchar(rfid) == 14, paste0(rfid, "0"), rfid)) %>%
  mutate(cohort = gsub(".*batch[- ](\\d+).*", "\\1", file, ignore.case = T) %>% parse_number(),
         cohort = paste0("C", str_pad(as.character(cohort), 2, "left", "0"))) %>%
  subset(measurement == "activelever"&session == "6") %>% 
  mutate(session = paste0("session", session),
         value = as.numeric(value)) %>% 
  rename("active_presses" = "value") %>% 
  distinct(cohort, rfid, internal_id, session, active_presses, date) %>% 
  spread(session, active_presses) %>% 
  rename("date_extinction" = "date",
         "active_presses_day6"= "session6")


# cued seeking
# active lever presses on cued reinstatement
cuedseeking_c01_10 <- kalivas_italy_extinction_excel_c01_10_df %>% 
  mutate(rfid = gsub("([.]|E14)", "", rfid),
         rfid = ifelse(nchar(rfid) == 14, paste0(rfid, "0"), rfid)) %>%
  mutate(cohort = gsub(".*batch[- ](\\d+).*", "\\1", file, ignore.case = T) %>% parse_number(),
         cohort = paste0("C", str_pad(as.character(cohort), 2, "left", "0"))) %>%
  subset(measurement == "activelever"&session=="relapse") %>% 
  mutate(session = paste0("session", session),
         value = as.numeric(value)) %>% 
  rename("active_presses" = "value") %>% 
  distinct(cohort, rfid, internal_id, session, active_presses, date) %>% 
  spread(session, active_presses) %>% 
  rename("active_presses" = "sessionrelapse", 
         "date_cued" = "date")


## open field behaviors
# distance travelled, time travelled and rears

oft_c01_10 <- kalivas_italy_oft_excel_processed_c01_10_df %>% 
  subset(session == "before_SA") %>% 
  rename("internal_id" = "ratinternalid") %>% 
  mutate(timetraveled = coalesce(timetraveled, ambulatorytime_sec)) %>% 
  mutate_at(vars(one_of("distancetraveled_cm", "timetraveled", "rears")), as.numeric) %>% 
  rename("date_oft" = "date") %>% 
  select(cohort, rfid, internal_id, distancetraveled_cm, timetraveled, rears, date_oft)

## Plus maze
# Time point 1, total time open arm

openarm_epm_c01_10 <- kalivas_italy_epm_excel_processed_c01_10_df %>% 
  subset(session == "before_SA") %>% 
  rename("internal_id" = "ratinternalid") %>% 
  rename("date_openarm" = "date") %>% 
  select(cohort, rfid, internal_id, totaltimeopenarm_sec, date_openarm)

## joining all columns

gwas_kalivas_italy_c01_10 <- full_join(extinction_6_c01_10, prime_seeking_c01_10, by = c("cohort", "internal_id", "rfid")) %>% 
  full_join(total_consumption_c01_10, by = c("cohort", "internal_id", "rfid")) %>% 
  full_join(breakpoint_c01_10, by = c("cohort", "internal_id", "rfid")) %>% 
  full_join(escalation_c01_10, by = c("cohort", "internal_id", "rfid")) %>%  
  full_join(cuedseeking_c01_10, by = c("cohort", "internal_id", "rfid")) %>% 
  full_join(oft_c01_10, by = c("cohort", "internal_id", "rfid")) %>% 
  full_join(openarm_epm_c01_10, by = c("cohort", "internal_id", "rfid")) %>% 
  mutate(internal_id = ifelse(!grepl("^IT\\d+", internal_id), paste0("IT", str_pad(internal_id, 3, "left", "0")), internal_id))
  
gwas_kalivas_italy_c01_10_df <- gwas_kalivas_italy_c01_10 %>% left_join(kalivas_italy_metadata_c01_10_df, by = c("cohort", "internal_id"="animal_id"))
  
gwas_kalivas_italy_c01_10_df %>% subset(rfid.x != rfid.y)
gwas_kalivas_italy_c01_10_df %>% naniar::vis_miss()


gwas_kalivas_italy_c01_10_df_final <- gwas_kalivas_italy_c01_10_df %>% 
  mutate(rfid.x = as.numeric(rfid.x),
         rfid.x = ifelse(is.na(rfid.x)|rfid.x != rfid.y, rfid.y, rfid.x)) %>% 
  mutate(rfid.x = as.character(rfid.x)) %>% 
  subset(!grepl("average", internal_id, ignore.case = T)) %>% 
  group_by(rfid.x) %>% fill(-rfid.x, .direction = "downup") %>% ungroup() %>% distinct() %>% 
  rename("rfid" = "rfid.x") %>% 
  mutate_at(vars(matches("date")), ~difftime(., dob, units = c("days")) %>% as.numeric() %>% round) %>% # calculate the age at exp
  setNames(gsub("date", "age_at", names(.))) %>% 
  select(-rfid.y)
  
  
gwas_kalivas_italy_c01_10_df_final %>% get_dupes(rfid)

write.csv(gwas_kalivas_italy_c01_10_df_final, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300/preliminary GWAS files/gwas_italytraits_rppr_n400.csv", row.names = F)



