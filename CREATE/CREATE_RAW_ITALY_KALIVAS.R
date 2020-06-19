## CREATE RAW ITALY KALIVAS
setwd("~/Dropbox (Palmer Lab)/Roberto_Ciccocioppo_U01/MedPC_data file")
raw_filenames_italy_kalivas <- list.files(recursive = T, full.names = T)

readsubject <- function(x){
  subject <- fread(paste0("grep -i 'subject:' ", "'", x, "'"), header = F)
  subject$filename <- x
  return(subject)
} 
all_subjects <- lapply(raw_filenames_italy_kalivas, readsubject) %>% 
  rbindlist(fill = T) %>% 
  rename("subjectid"= "V1") %>% 
  group_by(filename) %>% 
  mutate(numseq = row_number()) %>% 
  ungroup() 

# ", " | grep -Po ': \\K(w|KAL)?[0-9]*'"

### function to extract lever presses
read.active <- function(x){
  R_array <- fread(paste0("awk '/R:/{flag=1;next}/5:/{flag=0}flag' ", "'", x, "' | awk '/0:/{print NR \"_\" $2}'"), header = F, fill = T)
  R_array$filename <- x
  return(R_array)
} 
read.inactive <- function(x){
  R_array <- fread(paste0("awk '/L:/{flag=1;next}/5:/{flag=0}flag' ", "'", x, "' | awk '/0:/{print NR \"_\" $2}'"), header = F, fill = T)
  R_array$filename <- x
  return(R_array)
} 


##################################################
##################################################
## PRIMED REINSTATEMENT
##################################################
##################################################

########### ACTIVE LEVER PRESSES

primedrein_active <- lapply(grep("primed rein", raw_filenames_italy_kalivas, value = T, ignore.case = T), read.active) %>% 
  rbindlist(fill = T) %>% 
  rename("row_number"="V1") %>% 
  select(row_number, filename) %>% 
  separate(row_number, c("row_number", "lever_presses"), sep = "_") %>% 
  mutate(lever_presses = gsub(",", ".", lever_presses))


########### INACTIVE LEVER PRESSES

primedrein_inactive <- lapply(grep("primed rein", raw_filenames_italy_kalivas, value = T, ignore.case = T), read.inactive) %>% 
  rbindlist(fill = T) %>% 
  rename("row_number"="V1") %>% 
  select(row_number, filename) %>% 
  separate(row_number, c("row_number", "lever_presses"), sep = "_") %>% 
  mutate(lever_presses = gsub(",", ".", lever_presses))


##################################################
##################################################
## EXTINCTION 
##################################################
##################################################

########### ACTIVE LEVER PRESSES

extinction_active <- lapply(grep("ext", raw_filenames_italy_kalivas, value = T, ignore.case = T), read.active) %>% 
  rbindlist(fill = T) %>% 
  rename("row_number"="V1") %>% 
  select(row_number, filename) %>% 
  separate(row_number, c("row_number", "lever_presses"), sep = "_") %>% 
  mutate(lever_presses = gsub(",", ".", lever_presses))


########### INACTIVE LEVER PRESSES

extinction_inactive <- lapply(grep("ext", raw_filenames_italy_kalivas, value = T, ignore.case = T), read.inactive) %>% 
  rbindlist(fill = T) %>% 
  rename("row_number"="V1") %>% 
  select(row_number, filename) %>% 
  separate(row_number, c("row_number", "lever_presses"), sep = "_") %>% 
  mutate(lever_presses = gsub(",", ".", lever_presses))



