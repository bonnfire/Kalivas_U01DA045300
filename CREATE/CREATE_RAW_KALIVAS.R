#############################
# Protocol 
#############################

setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/behavioral_tasks")


# self defined functions
uniform.var.names.testingu01 <- function(df){
  lapply(seq_along(df), function(i) {
    if(grepl("Parent", names(df[[i]])) %>% any()){
      names(df[[i]])[1:2] <- df[[i]][1,][1:2] %>% as.character()
      df[[i]] <- df[[i]][-1, ]
    }
    names(df[[i]]) <- mgsub::mgsub(names(df[[i]]),
                                   c(" |\\.", "#", "Transponder ID", "Date of Wean|Wean Date","Animal", "Shipping|Ship", "Dams"),
                                   c("", "Number", "RFID", "DOW","LabAnimal", "Shipment", "Dames"))
    # names(df[[i]]) <- mgsub::mgsub(names(df[[i]]),
    #                                c("DateofShipment", "LabAnimalID"), 
    #                                c("ShipmentDate", "LabAnimalNumber"))
    names(df[[i]]) <- mgsub::mgsub(names(df[[i]]),
                                   c("DateofShipment", "LabAnimalNumber"), 
                                   c("ShipmentDate", "LabAnimalID")) # actually keep the column named lab animal number
    names(df[[i]]) <- tolower(names(df[[i]]))
    df[[i]]
  })
}

############################
# Exp 1: ELEVATED PLUS MAZE
############################
setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/behavioral_tasks/elevated_plus_maze")

# all wmv files


############################
# Exp 2: OPEN FIELD TASK
############################
setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/behavioral_tasks/open_field_task")
# cohort 2 and 3 excel and ACT files
openfieldtask_files_raw <- list.files(path = ".", pattern = "*.ACT", full.names = T, recursive = T) 

read_oft <- function(x){
  read_oft <- fread(x, fill = T)
  read_oft$actfilename <- x
  return(read_oft)
}

openfieldtask_raw_list <- lapply(openfieldtask_files_raw, read_oft)
names(openfieldtask_raw_list) = openfieldtask_files_raw

openfieldtask_raw_list <- lapply(openfieldtask_raw_list, function(df){
  df <- df %>% 
    select(-V38) %>%   
    mutate(vmx = head(grep("^\\D", df$CAGE,value = T), 1)) %>% 
    dplyr::filter(grepl("(?=)^\\d", df$CAGE, perl = T)) %>% 
    mutate(CAGE = as.numeric(CAGE),
           DATE = as.POSIXct(strptime(DATE,format="%d-%B-%Y")),
           TIME = chron::chron(times=TIME),
           cohort = str_match(actfilename, "/(.*?)/")[,2]) %>%
    arrange(CAGE)
  names(df) <- mgsub::mgsub(names(df),c("-| "), c("_")) %>% 
    tolower() %>% 
    make.unique(sep = ".")
  return(df)
  })
# naniar::vis_miss(rbindlist(openfieldtask_raw_list, fill = T)) nothing abnormal; remove V38 bc all are empty; check why some columns are 100% empty and if the kalivas lab kept these # all na cycle lines contain the C:\\ extension
openfieldtask_raw_df <- rbindlist(openfieldtask_raw_list, fill = T) # vis_miss only those two columns are empty now 


# todo: check that the names are the same as the excel file

############################
# Exp 3: TAIL FLICK
############################


