## CREATE EXCEL ITALY AND U.S. 

## EXTRACT KALIVAS ITALY EXCEL 

setwd("~/Dropbox (Palmer Lab)/Roberto_Ciccocioppo_U01/raw data")
italy_filenames_xl <- list.files(recursive = T)


extract_italy_tissue_notes <- function(files, sheet){
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
      mutate(cohort = parse_number(cohort) %>% str_pad(., 2, side = "left", pad = "0"),
             coat_color = gsub("([A-Z]+)(HOOD)", "\\1 \\2", mgsub::mgsub(coat_color, 
                                                                         c("BRN|[B|b]rown", "BLK|[B|b]lack", "HHOD|[H|h]ood|[H|h]hod", "[A|a]lbino"), 
                                                                         c("BROWN", "BLACK", "HOOD", "ALBINO"))) 
      ) %>% 
      rename_at(vars(behavioral_unit:resolution) ,function(x){paste0(x, "_behavunit")})
    
    return(data_breeder)
    
  })
  return(data_breeder_list)
}

kalivas_cohort_xl <- extract_italy_tissue_notes(all_excel_fnames, "info_from_breeder") %>%
  rbindlist()