## CREATE RAW ITALY KALIVAS
raw_filenames_italy_kalivas_c03_09 <- list.files(path = "~/Dropbox (Palmer Lab)/Roberto_Ciccocioppo_U01/MedPC_data file", recursive = T, full.names = T) %>% grep("cohort_0[3-9]", ., value = T)
# raw_filenames_italy_kalivas[which(raw_filenames_italy_kalivas == "./unicam_cohort_06/Long-access self-administration/U01-C6 ROOM 47 LGA14 M(209-210_237-238) F( 235-236) e PR( F 239-240)")] <- "./unicam_cohort_06/Long-access self-administration/U01-C6 ROOM 47 LGA14 M(209-210_237-238) F( 235-236) e PR F(239-240)"

## read in excel assigning subjects based on boxes 

sheets_c03_06 <- openxlsx::getSheetNames("~/Dropbox (Palmer Lab)/Roberto_Ciccocioppo_U01/MedPC_data file/rat-box_allocation.xlsx") %>% grep("[3-6]", ., value = T)
italy_kalivas_xl_boxes_c03_06 <- lapply(sheets_c03_06, openxlsx::read.xlsx, xlsxFile="~/Dropbox (Palmer Lab)/Roberto_Ciccocioppo_U01/MedPC_data file/rat-box_allocation.xlsx") %>% 
  lapply(., function(x){
    x <- x %>% 
      clean_names() %>% 
      rename_all(~ stringr::str_replace_all(., '.*animal_id|internal*', 'internal_id')) %>%    
      rename_all(~ stringr::str_replace_all(., '.*heroin.*', 'heroin_salineyoked')) %>%    
      rename_all(~ stringr::str_replace_all(., '.*room.*', 'saroom')) %>%    
      rename_all(~ stringr::str_replace_all(., '.*box.*', 'sabox')) %>%    
      rename_all(~ stringr::str_replace_all(., '^tra.*id.*', 'rfid')) %>%    
      rename_all(~ stringr::str_replace_all(., '.*batch.*', 'cohort')) %>% 
      mutate_all(~gsub(" ", "", .))
  
      return(x)
  })

italy_kalivas_xl_boxes_c03_06_df <- italy_kalivas_xl_boxes_c03_06 %>% rbindlist(fill = T) %>% 
  rename("comments" = "x8") %>% 
  mutate(cohort = parse_number(cohort) %>% str_pad(., 2, "left", "0") %>% paste0("C", .)) %>% 
  rowwise() %>% 
  mutate(sabox = paste0(str_extract(tolower(saroom), "left|right"), sabox),
         saroom = parse_number(saroom) %>% as.character) %>% 
  ungroup() ## XX where are the saroom 47 data






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

# maybe install.packages("fuzzyjoin")
  
lga_firsthour_raw_df_expand <- lga_firsthour_raw_df  %>% 
  mutate(filename =
           replace(filename, filename == "./unicam_cohort_06/Long-access self-administration/U01-C6 ROOM 47 LGA14 M(209-210_237-238) F( 235-236) e PR( F 239-240)",
                   "./unicam_cohort_06/Long-access self-administration/U01-C6 ROOM 47 LGA14 M(209-210_237-238) F( 235-236) e PR F(239-240)")) %>% 
  mutate(possible_subjects = expand.project.dash(filename) %>% as.character) %>% 
  fuzzyjoin::fuzzy_left_join(., boxes_xl %>% 
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


primed_presses_byhour_5_6 <- lapply(grep("primed rein", raw_filenames_italy_kalivas_c03_09, value = T, ignore.case = T), function(x){
  setwd("~/Dropbox (Palmer Lab)/Roberto_Ciccocioppo_U01/MedPC_data file/")
  
  box = fread(paste0("awk '/Box:/{print $2}' '", x, "'"), header = F, fill = T)
  
  inactive_lever =  fread(paste0("awk '/^L:/{flag=1;next}/^R:/{flag=0}flag' ", "'",  x, "'"), header = F, fill = T)
  active_lever =  fread(paste0("awk '/^R:/{flag=1;next}/^W:/{flag=0}flag' ", "'",  x, "' | awk '/0:/{print $3 + $4}'"), header = F, fill = T)
  
  # inactive 
  # find each subject and split 
  inactive_indices <- grep("^0:", inactive_lever$V1) 
  split_data_inactive <- split(inactive_lever, cumsum(1:nrow(inactive_lever) %in% inactive_indices))
  
  inactive_split_data_process <- lapply(split_data_inactive, function(x){
    x <- x %>% separate(V1, c("row", "V1"), sep = ":") %>% 
      select(-matches("V6")) %>% mutate_all(~gsub(" ", "", .)) %>% 
      mutate_all(as.numeric) %>% 
      select(-row)
    vector <- data.frame(rewards = as.vector(t(data.matrix(x)))) # transpose to get by row
    vector <- vector[-1,] %>% # remove total 
      as.data.frame() %>% rename("rewards" = ".") %>% 
      mutate(bin = 1:n()) # add bin number 
    
    vector_sum <- vector %>% 
      mutate(group = ifelse(between(bin, 49, 61), "inactive_hour5", 
                            ifelse(between(bin, 62, 73), "inactive_hour6", NA))) %>% 
      subset(!is.na(group)) %>% 
      group_by(group) %>% 
      mutate(sum = sum(rewards, na.rm = T))
    
    vector_sum_wide <- vector_sum %>% 
      distinct(group, sum) %>% 
      spread(group, sum)
    
    return(vector_sum_wide)
  }) %>% 
    rbindlist(fill = T)
  
  
  # active 
  # find each subject and split 
  active_indices <- grep("^0:", active_lever$V1) 
  split_data_active <- split(active_lever, cumsum(1:nrow(active_lever) %in% active_indices))
  
  active_split_data_process <- lapply(split_data_active, function(x){
    x <- x %>% separate(V1, c("row", "V1"), sep = ":") %>% 
      select(-matches("V6")) %>% mutate_all(~gsub(" ", "", .)) %>% 
      mutate_all(as.numeric) %>% 
      select(-row)
    vector <- data.frame(rewards = as.vector(t(data.matrix(x)))) # transpose to get by row
    vector <- vector[-1,] %>% # remove total 
      as.data.frame() %>% rename("rewards" = ".") %>% 
      mutate(bin = 1:n()) # add bin number 
    
    vector_sum <- vector %>% 
      mutate(group = ifelse(between(bin, 49, 61), "active_hour5", 
                            ifelse(between(bin, 62, 73), "active_hour6", NA))) %>% 
      subset(!is.na(group)) %>% 
      group_by(group) %>% 
      mutate(sum = sum(rewards, na.rm = T))
    
    vector_sum_wide <- vector_sum %>% 
      distinct(group, sum) %>% 
      spread(group, sum)
    
    return(vector_sum_wide)
  }) %>% 
    rbindlist(fill = T)

  data <- cbind(box, inactive_split_data_process) %>% cbind(active_split_data_process) %>% 
    mutate(filename = x) %>% 
    rename("box" = "V1")
  
  return(data)
  
})

primed_presses_byhour_5_6_df <- primed_presses_byhour_5_6 %>% rbindlist(fill = T)

# file dictionary
primed_filename_c03_09_df <- primed_presses_byhour_5_6_df %>% 
  distinct(filename) %>% 
  mutate(filename2 = gsub(" ", "", filename)) %>%
  rowwise() %>% 
  mutate(num = stringr::str_extract_all(filename2, "([MF]|YOKED)\\([^()]+\\)") %>% gsub("[()]", "", .)) %>% 
  ungroup %>% 
  mutate(num2 = strsplit(as.character(num), "\", \"|_")) %>% 
  unnest(num2) %>% 
  mutate(num2 = gsub("c\"|\"", "", num2)) %>% 
  mutate(sex = str_match(num2, "[MF]|YOKED")) %>% 
  mutate(num2 = ifelse(num2 == "character0", "0-0", num2)) %>% 
  mutate(num3 = gsub("(\\D+)?([0-9]+)-?.*", "\\2", num2) %>% parse_number() %>% as.numeric,
         num4 = gsub(".*-", "", num2) %>% parse_number() %>% as.numeric) %>% 
  mutate(num_seq = map2(num3, num4, `:`)) %>% 
  unnest(num_seq, keep_empty = T) %>% 
  distinct(filename, sex, num_seq)

# join file dictionary to box allocation
kalivas_italy_priming_excel_c01_10_df %>% distinct(internal_id, sabox, sex) %>% 
  mutate(num_seq = parse_number(internal_id)) %>% 
  full_join(primed_filename_c03_09_df, by = c("num_seq", "sex")) %>%
  naniar::vis_miss() 

##XX potential internal_id 
priming_id <- kalivas_italy_priming_excel_c01_10_df %>% distinct(internal_id, sabox, sex) %>% 
  mutate(num_seq = parse_number(internal_id)) %>% 
  full_join(primed_filename_c03_09_df, by = c("num_seq", "sex"))%>%
  subset(!is.na(internal_id)) 

anti_join(primed_filename_c03_09_df, priming_id, by = "filename") ## XX 
anti_join(priming_id, primed_filename_c03_09_df, by = "filename")


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

relapse_raw_c03_09 <- lapply(grep("cued", raw_filenames_italy_kalivas_c03_09, value = T, ignore.case = T), read.relapse) 
relapse_raw_c03_09_df <- relapse_raw_c03_09 %>%
  rbindlist(fill = T) 
names(relapse_raw_c03_09_df) <- c("box", "A", "inactive_presses", "active_presses", "infusions", "filename")
relapse_raw_c03_09_df <- relapse_raw_c03_09_df %>% 
  mutate_all(as.character) %>%
  mutate_at(vars(-matches("filename")), ~ gsub(",", ".", .))

relapse_raw_c03_09_df %>% mutate(box = as.numeric(box)) %>% subset(box>10) %>% dim

relapse_raw_c03_09_df <- relapse_raw_c03_09_df %>% 
  mutate(sex = ifelse(grepl("[MF][(]", filename, ignore.case = T), gsub(".*([MF])[(].*", "\\1", filename), NA),
         cohort = str_pad(parse_number(gsub(".*unicam_cohort_(\\d+)[/].*", "\\1", filename)), 2, "left", "0") %>% paste0("C", .),
         exp = "cued_rein",
         heroin_or_saline = ifelse(grepl("saline", filename, ignore.case = T), "saline", "heroin"))

# filename dictionary
relapse_c03_09_filename <- relapse_raw_c03_09_df %>% 
  distinct(filename) %>% 
  mutate(filename2 = gsub(" ", "", filename)) %>%
  rowwise() %>% 
  mutate(num = stringr::str_extract_all(filename2, "([MF]|YOKED)\\([^()]+\\)") %>% gsub("[()]", "", .)) %>% 
  ungroup %>% 
  mutate(num2 = strsplit(as.character(num), "\", \"|_")) %>% 
  unnest(num2) %>% 
  mutate(num2 = gsub("c\"|\"", "", num2)) %>% 
  mutate(sex = str_match(num2, "[MF]|YOKED")) %>% 
  mutate(num2 = ifelse(num2 == "character0", "0-0", num2)) %>% 
  mutate(num3 = gsub("(\\D+)?([0-9]+)-?.*", "\\2", num2) %>% parse_number() %>% as.numeric,
         num4 = gsub(".*-", "", num2) %>% parse_number() %>% as.numeric) %>% 
  mutate(num_seq = map2(num3, num4, `:`)) %>% 
  unnest(num_seq, keep_empty = T) %>% 
  distinct(filename, sex, num_seq)
  

# join to data and by boxes
relapse_raw_c03_09_df_id <- relapse_raw_c03_09_df %>% 
  left_join(relapse_c03_09_filename, by = c("filename", "sex")) %>% 
  left_join(italy_rat_boxes_df %>% 
              rename("box_fullname" = "box"), by = c("cohort", "sex", "num_seq" = "labanimalid_num")) %>% 
  subset(!is.na(labanimalid))

# missing 
relapse_raw_c03_09_df[!relapse_raw_c03_09_df$filename %in% unique(relapse_raw_c03_09_df_id$filename),]$filename %>% unique







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



