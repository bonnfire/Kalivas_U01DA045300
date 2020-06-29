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
expand.project.dash <- function(txt_original) {
  txt <- txt_original %>% gsub(" ", "", .)
  if((grepl("\\d[[:space:]]?-[[:space:]]?\\d", txt) & !grepl("Subject", txt, ignore.case = T))){
    
    str <- paste0(gsub("[^MF]", "", str_extract_all(txt, "[MF][(]")[[1]], ignore.case = T), "%s")
    dashed_nums <- stringr::str_extract_all(txt, "\\([^()]+\\)")[[1]] %>% gsub("[()]", "", .) %>% strsplit('_') ## get rid of spaces and parentheses
    
    numbers <- lapply(dashed_nums, function(x){
      str_split(x, "-")}) %>% 
      lapply(function(x){ 
        lapply(x, function(x){
          seq(as.numeric(x[[1]]), as.numeric(x[[2]]))}) 
      }) %>% 
      lapply(function(x){
        unlist(x, recursive = F)
      })
    
    ids_list <- list()
    for(i in 1:length(str)){
      ids_list[i] <- paste0(sprintf(str[i], numbers[[i]]), separate = "", collapse = ",")
    }
    
    ids_df <- unlist(ids_list) %>% paste0(collapse  = ",")
    return(ids_df)
  }
  # else if(grepl("\\d[[:space:]]?-[[:space:]]?\\d", txt) & grepl("Subject", txt, ignore.case = T)){
  #   paste0(gsub("([MF])[(].*", "\\1", txt), parse_number(gsub(".*Subject ", "", txt)))
  # } 
  else{
    paste0(txt)
  }
}
expand.project.dash <- Vectorize(expand.project.dash)

# dashed_str <- stringr::str_extract_all(txt, "\\([^()]+\\)")[[1]] %>% gsub("[()]", "", .)
# test_list <- strsplit(dashed_str, '_')



## xx pick up from here and fix the code for getting rid of numeric characters


# any of these should work
txt_original <- ("./unicam_cohort_06/Long-access self-administration/U01-C6 ROOM 47 LGA9 M(209-210_237-238) F( 235-240)
> txt")
txt <- gsub("[[:space:]]", "", txt_original)
paste0(gsub("[^MF]", "", str_extract_all(txt, "[MF][(]")[[1]], ignore.case = T), "%s")


messyids <- lga_firsthour_raw_df %>% subset(grepl("[MF][(].*[MF][(].*", filename)) %>% select(filename) %>% distinct() 
# %>% unlist() %>% as.character
messyids %>% mutate(potentialids = expand.project.dash(filename))


gsub("[^MF]", "", str_extract_all("LGA9 M(209-210_237-238) F( 235-240)", "[MF][(]")[[1]], ignore.case = T)


##################################################
##################################################
## LONG ACCESS SELF ADMIN (12 HOUR AND 1 HOUR)
##################################################


## 12 HOURS 
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

lga_raw_df <- lga_raw_df %>% 
  mutate(sex = ifelse(grepl("[MF][(]", filename, ignore.case = T), gsub(".*([MF])[(].*", "\\1", filename), NA),
         cohort = str_pad(parse_number(gsub(".*unicam_cohort_(\\d+)[/].*", "\\1", filename)), 2, "left", "0"),
         exp = gsub(".*(LgA\\d+).*", "\\1", filename, ignore.case = T),
         saline = ifelse(grepl("saline", filename, ignore.case = T), "Saline", NA))


#### FIRST(1) HOUR 

## extract the infusions, active, and inactive lever (first 12 5 minute bins)
read.selfadmin.firsthour <- function(x){
  lga_firsthour <- fread(paste0("grep -A 3 -E \"(Box|L|R|W):\" --no-group-separator ", "'", x, "'", "| grep -v \"MSN:\" "), header = F, fill = T)
  lga_firsthour_split <- split(lga_firsthour, findInterval(1:nrow(lga_firsthour), grep("Box:", lga_firsthour$V2)))
  # maxlength <- lga_firsthour_split %>% sapply(nrow) %>% max #get the max number of "arrays"
  lga_firsthour_merge <- lapply(lga_firsthour_split, function(x){
    x <- x %>% 
      mutate(V2 = replace(V2, V1=="0:", NA)) %>%  # remove the total 12h value, remove any 5 min bins after an hour (12th bin)
      mutate_at(vars(matches("V[56]")), ~ replace(., V1=="10:", NA) ) 
    
    # splitting the array 
    x_split <- split(x, findInterval(1:nrow(x), grep("(L|R|W):", x$V1)))
    
    # wrangle the data to get the total number of infusions and presses in the first hour
    x_split_2 <- lapply(x_split, function(x){
      x <- x %>% 
        mutate(V1 = replace(V1, grepl("(0|5|10):", V1), NA)) %>% 
        fill(V1) %>% 
        mutate_all(as.character) %>%
        mutate_at(vars(-matches("filename")), ~ gsub(",", ".", .)) %>%
        rowwise() %>% 
        mutate(V7 = paste0(c(V2, V3, V4, V5, V6), collapse = ",") ) %>%
        # subset(!grepl("^$|Time", V2)|grepl("Box", V1)) %>% 
        group_by(V1) %>%
        mutate(V8 = paste0(V7, collapse = ",") ) %>%
        ungroup() %>%
        subset(grepl("^$|Time", V2)|grepl("Box", V1)) %>%
          select(-V7) %>%
        mutate(V8 = gsub("[,]{2,}", "", V8)) %>%
        separate(V8, into = c(paste0("C", 1:15)), sep = ",") %>%
        mutate_at(vars(matches("C\\d+")), ~ replace(., grepl("Box|Start|End", V1), NA)) %>%
      #   mutate_at(vars(matches("C([3-9]|1[0-5])")), ~ replace(., V1 == "W:", NA)) %>%
        mutate_at(vars(matches("C\\d+")), as.numeric) %>%
        mutate(sum = rowSums(.[grep("C\\d+", names(.))], na.rm = TRUE) %>% as.character) %>% 
        mutate_all(~ replace(., grepl("^$", .), NA)) %>% 
        rowwise() %>%
        mutate(V1 = replace(V1, grepl("Time", V2, ignore.case = T), paste0(V1, "_Time")),  # move the time character to the first column
               V2 = replace(V2, grepl("Time", V2, ignore.case = T), NA),
               sum = replace(sum, grepl("Box|Start|End", V1), NA),
               V1 = gsub("[:]", "", V1)) %>%
        mutate(sum = coalesce(sum, V3)) %>% 
        mutate(sum = coalesce(sum, V2)) %>%
        select(-matches("C\\d+|V[2-6]")) %>% 
        ungroup() 
      return(x)
    }) %>% rbindlist(fill = T) 
    
    return(x_split_2)
    }) %>%
    rbindlist(fill = T)
  

  ## create a wide dataframe from long by first assigning the boxes within each file
  lga_firsthour_merge <- lga_firsthour_merge %>% 
    mutate(box = ifelse(grepl("box", V1, ignore.case = T), paste0(sum, "_",lead(sum, 1)), NA)) %>%
    fill(box) %>%
    spread(V1, sum) %>%
    separate(box, into = c("box", "start_time"), sep = "_") %>% 
    mutate(filename = x) %>%
    mutate(exp = str_extract(toupper(filename), "LGA\\d+"),
           cohort = str_extract(toupper(filename), "C\\d+"),
           room = str_extract(toupper(filename), "ROOM[[:space:]]?\\d+")) %>% 
    clean_names() %>% 
    select(-matches("_[2-9]$"))  # remove duplicated columns (here, they are the ones that were used to spread)
  
  return(lga_firsthour_merge)

  }

# trial with grep("long-access", raw_filenames_italy_kalivas, value = T, ignore.case = T)[280]
# grep("long-access", raw_filenames_italy_kalivas, value = T, ignore.case = T)[39] file doesn't have unique boxes and sessions in the 
lga_firsthour_raw <- lapply(grep("long-access", raw_filenames_italy_kalivas, value = T, ignore.case = T), read.selfadmin.firsthour) %>%
  rbindlist(fill = T) 
  
lga_firsthour_raw_df <- lga_firsthour_raw %>% 
  rename("active" = "r", 
         "inactive" = "l", 
         "infusions" = "w") %>% 
  mutate_at(vars(one_of("active","inactive", "infusions")), as.numeric) %>% 
  mutate(sex = ifelse(grepl("[MF][(]", filename, ignore.case = T), gsub(".*([MF])[(].*", "\\1", filename), NA))

# assign the subjects by box (differentiate by sex and room)
# work with just cohort 3

# maybe install.packages("fuzzyjoin")

# lga_firsthour_raw_df_expand <- 
test <-  lga_firsthour_raw_df %>% 
  subset(cohort == "C3") %>% 
  mutate(possible_subjects = expand.project.dash(filename) %>% as.character) %>% fuzzyjoin::fuzzy_inner_join(boxes_xl %>% 
  subset(cohort == "C03") %>%
  mutate_at(vars(one_of("rat_internal_id")), as.numeric) %>% 
  mutate(labanimalid = paste0(sex, rat_internal_id),
         box = as.character(box)), by = c("box", "possible_subjects" = "labanimalid"), match_fun = stringr::str_detect)


lga_firsthour_raw_df %>% subset(grepl("[MF][(].*[MF][(].*", filename)) %>% distinct(filename)





test %>% get_dupes(labanimalid, exp, start_time)

  


lga_firsthour_raw_df %>% 
  ggplot(aes(x = active)) + 
  geom_histogram() + 
  facet_grid(~ cohort) 

lga_firsthour_raw_df %>% 
  ggplot(aes(x = inactive)) + 
  geom_histogram() + 
  facet_grid(~ cohort) 

lga_firsthour_raw_df %>% 
  ggplot(aes(x = infusions)) + 
  geom_histogram() + 
  facet_grid(~ cohort) 

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
##  CUED REINSTATEMENT
##################################################
##################################################

########### INACTIVE LEVER PRESSES (LP), ACTIVE LP, AND PUMP ACTIVATIONS
read.relapse <- function(x){
  cued_rein <- fread(paste0("awk '/Group:/{flag=1;next}/^$/{flag=0}flag' ", "'", x, "'", " | awk '/Box:| 0:/{print $1, $2}'"), header = F, fill = T)
  cued_rein_split <- split(cued_rein, findInterval(1:nrow(cued_rein), grep("Box:", cued_rein$V1)))
  maxlength <- cued_rein_split %>% sapply(nrow) %>% max #get the max number of "arrays"
  cued_rein_merge <- lapply(cued_rein_split, function(x, maxlength){
    arrays <- x$V2
    length(arrays) <- maxlength 
    as.data.frame(matrix(arrays, ncol = maxlength, byrow = T))
  }, maxlength = maxlength) %>% 
    rbindlist(fill = T) %>%
    mutate(filename = x)
  
  # cued_rein_df <- cued_rein_df %>% 
  #   mutate(subject = str_extract(filename, "[MF](.*+)")) %>% 
  #   mutate(subject = expand.project.dash(subject))
  
  return(cued_rein_merge)
  # return(cued_rein)
}

relapse_raw <- lapply(grep("cued", raw_filenames_italy_kalivas, value = T, ignore.case = T), read.relapse) %>%
  rbindlist(fill = T) 
names(relapse_raw) <- c("box", "A", "inactive_presses", "active_presses", "pump_activation", "filename")
relapse_raw_df <- relapse_raw %>% 
  mutate_all(as.character) %>%
  mutate_at(vars(-matches("filename")), ~ gsub(",", ".", .))

relapse_raw_df %>% mutate(box = as.numeric(box)) %>% subset(box>10) %>% dim

relapse_raw_df <- relapse_raw_df %>% 
  mutate(sex = ifelse(grepl("[MF][(]", filename, ignore.case = T), gsub(".*([MF])[(].*", "\\1", filename), NA),
         cohort = str_pad(parse_number(gsub(".*unicam_cohort_(\\d+)[/].*", "\\1", filename)), 2, "left", "0"),
         exp = gsub(".*(relapse\\d+).*", "\\1", filename, ignore.case = T),
         saline = ifelse(grepl("saline", filename, ignore.case = T), "Saline", NA))






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



