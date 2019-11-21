#############################
# Protocol 
#############################

setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/behavioral_tasks")

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

# read in the excel files 
openfieldtask_files <- list.files(path = ".", pattern = "*.xlsx", full.names = T, recursive = T) 
openfieldtask_files <- grep("cohort", openfieldtask_files, ignore.case = T, value = T) # ignore the other xlsx file

u01.importxlsx <- function(xlname){
  path_sheetnames <- excel_sheets(xlname)
  df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname)
  names(df) <- path_sheetnames
  return(df)
}


for(i in 1:length(openfieldtask_files)){
    assign(gsub("\\.xlsx", "", sapply(strsplit(openfieldtask_files, "/"), "[", 3))[i], u01.importxlsx(openfieldtask_files[i]))
} # creates multiple objects for every excel file





############################
# Exp 3: TAIL FLICK
############################


