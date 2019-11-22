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
openfieldtask_files_raw <- list.files(path = ".", pattern = "*.ACT", full.names = T, recursive = T) 

read_oft <- function(x){
  read_oft <- fread(x, fill = T)
  read_oft$filename <- x
  return(read_oft)
}

openfieldtask_raw <- lapply(openfieldtask_files_raw, read_oft)
# cohort 2 and 3 excel and ACT files


############################
# Exp 3: TAIL FLICK
############################


