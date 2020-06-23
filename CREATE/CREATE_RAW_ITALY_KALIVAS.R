## CREATE RAW ITALY KALIVAS
setwd("~/Dropbox (Palmer Lab)/Roberto_Ciccocioppo_U01/MedPC_data file")
raw_filenames_italy_kalivas <- list.files(recursive = T, full.names = T)


## read in excel assigning subjects based on boxes 

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

### function to expand the subject names in filename 
## functions for wrangling
## create new rows to expand on the projects 
expand.project.dash <- function(txt) {
  
  if((grepl("\\d[[:space:]]?-[[:space:]]?\\d", txt) & !grepl("Subject", txt, ignore.case = T))){
    str <- gsub('[(]\\d+\\s?-\\s?\\d+[)](.txt)?', '%s', txt)
    dashed_str <- gsub('[a-zA-Z ()]+', '', txt)

    expand.dash <- function(dashed) {
      limits <- as.numeric(unlist(strsplit(dashed, '-')))
      first <- seq(limits[1], limits[2], 4)
      second <- seq(limits[1], limits[2], 4) + 1
      c(rbind(first, second))
    }

    paste0(sprintf(str, expand.dash(dashed_str)), sep = "", collapse = ",")
  }
  else if(grepl("\\d[[:space:]]?-[[:space:]]?\\d", txt) & grepl("Subject", txt, ignore.case = T)){
    paste0(gsub("([MF])[(].*", "\\1", txt), parse_number(gsub(".*Subject ", "", txt)))
  } 
  else{
    paste0(txt)
  }
}
expand.project.dash <- Vectorize(expand.project.dash)

##################################################
##################################################
## LONG ACCESS SELF ADMIN
##################################################

########### INACTIVE LEVER PRESSES (LP), ACTIVE LP, AND INFUSIONS
read.lga <- function(x){
  lga <- fread(paste0("awk '/Group:/{flag=1;next}/^$/{flag=0}flag' ", "'", x, "'", " | awk '/Box:| 0:/{print $1, $2}'"), header = F, fill = T)
  lga_split <- split(lga, findInterval(1:nrow(lga), grep("Box:", lga$V1)))
  maxlength <- lga_split %>% sapply(nrow) %>% max #get the max number of "arrays"
  lga_merge <- lapply(lga_split, function(x, maxlength){
    arrays <- x$V2
    length(arrays) <- maxlength 
    as.data.frame(matrix(arrays, ncol = maxlength, byrow = T))
    }, maxlength = maxlength) %>% 
    rbindlist(fill = T) %>%
    mutate(filename = x)

  # lga_df <- lga_df %>% 
  #   mutate(subject = str_extract(filename, "[MF](.*+)")) %>% 
  #   mutate(subject = expand.project.dash(subject))
  
  return(lga_merge)
  # return(lga)
  }

grep("C3_LgA1-F\\(83-99\\)\\.txt", raw_filenames_italy_kalivas, value = T, ignore.case = T)
lga_raw <- lapply(grep("long-access", raw_filenames_italy_kalivas, value = T, ignore.case = T), read.lga) %>%
  rbindlist(fill = T) 
names(lga_raw) <- c("box", "A", "inactive_presses", "active_presses", "infusions", "filename")
lga_raw_df <- lga_raw %>% 
  mutate_all(as.character) %>%
  mutate_at(vars(-matches("filename")), ~ gsub(",", ".", .))

lga_raw_df %>% mutate(box = as.numeric(box)) %>% subset(box>10) %>% dim


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



