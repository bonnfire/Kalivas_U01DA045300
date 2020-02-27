#############################
# Protocol 
#############################

## use dates or confirm with them 

# First body weight 
# Tail flick (Before SA)
# EPM (Before SA)
# Open field (Before SA)
# LGA1-12 # use end date (12 hours sessions)
# PR 
# LGA13-15 
# Extinction prime
# Extinction 1-3
# Cued reinstatement 
# Tail flick (After SA)
# EPM (After SA)
# Open field (After SA)




#### 
setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/addiction_related_behaviors/MedPC_raw_data_files")
allcohorts_allexp_filenames <- list.files(full.names = T, recursive = T) #295 files

## in addiction tasks 



#####  LONG ACCESS

# all infusions are mg/kg/infusion
# behavioral testing occurred 4 days/wk (sun-thurs) from 19:00 to 7:00 EST
# syringe refill calculation based on the most recent weight from the animal 
# active lever = always right (20ug/kg/infusion; infusion volume is 100 ul; fixed ratio 1)


## cohort xx : after the session ended, rats received 0.05 ml of a mixture of Heparin and 1 mg/ml Gentamicin Sulfate (antibiotic) to maintain catheter patency and animal health
allcohorts_longaccess_fnames <- grep("long", allcohorts_allexp_filenames, ignore.case = T, value = T)
# Extract subject information
readsubject <- function(x){
  subject <- fread(paste0("grep -i 'subject' ", "'", x, "'", " | grep -Po ': \\K(w|KAL)?[0-9]*'"), header = F)
  subject$filename <- x
  return(subject)
} 
lga_subjects <- lapply(allcohorts_longaccess_fnames, readsubject) %>% 
  rbindlist(fill = T) %>% 
  rename("subjectid"= "V1") %>% 
  group_by(filename) %>% 
  mutate(numseq = row_number()) %>% 
  ungroup() 

lga_subjects %>% dplyr::filter(is.na(subjectid)) 
  

# B array contains the inactive lever, active lever, and infusion information
readBarray <- function(x){
  Barray <- fread(paste0("grep -a1 --no-group-separator -n 'B:' ", "'", x, "'", " | grep -E ' 0:'"), header = F, fill = T)
  Barray$filename <- x
  return(Barray)
} 

lga_Barray <- lapply(allcohorts_longaccess_fnames, readBarray) %>% rbindlist(fill = T) %>% 
  dplyr::select(-c(V2, V6)) %>% 
  dplyr::rename("rownum" = "V1",
         "inactive_lever" = "V3",
         "active_lever" = "V4", 
         "infusions" = "V5") %>% 
  mutate(rownum = gsub("-", "", rownum) %>% as.numeric) %>% 
  arrange(filename, rownum) %>% 
  group_by(filename) %>% 
  mutate(numseq = row_number()) %>% 
  ungroup() 
lga_Barray %>% naniar::vis_miss()

if(nrow(lga_Barray) == nrow(lga_subjects)){
  lga_merge <- merge(lga_subjects, lga_Barray) %>% 
    mutate(subjectid = str_extract(subjectid, "\\d+") %>% as.numeric,
           subjectid = paste0("KAL", str_pad(subjectid, 3, "left", "0")))
  print("LGA_merge object created")
  } else {
  lga_Barray %>% 
    group_by(filename) %>% 
    add_count(filename) %>% 
    merge(., lga_subjects %>% 
            count(filename), by = "filename") %>%   
    rename("numberofBarrays" = "n.x", "numberofsubjects" = "n.y") %>% 
    select(numberofBarrays, numberofsubjects, filename) %>% 
    group_by(filename) %>% 
    slice(1) %>% 
    dplyr::filter(numberofBarrays != numberofsubjects) 
  print("LGA DATA COULD NOT BE JOINED")}

lga_merge %>% naniar::vis_miss() #complete cases all 1506 observations

lga_allsubjects <- left_join(kalivas_allcohorts[,c("cohort_number", "sex", "rfid", "dob", "internal_id")], lga_merge, by = c("internal_id"= "subjectid")) %>% 
  mutate(filename = gsub(".*MUSC_", "", filename)) %>% 
  left_join(., allcohorts_df[, c("startdate", "filename")]) %>% 
  mutate(startdate = unlist(startdate) %>% as.character %>% gsub('([0-9]+/[0-9]+/)', '\\120', .) %>% as.POSIXct(format="%m/%d/%Y"),
         experimentage = (startdate - dob) %>% as.numeric %>% round) %>% 
  distinct() %>% 
  arrange(internal_id, startdate) %>% 
  select(-c(numseq, rownum, dob)) %>%  ## only the 80 in the mapping excel information
  mutate(session = gsub(".*day ", "", filename))

lga_allsubjects %>% naniar::vis_miss()

# *****************
# create missingness table for kalivas team
setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/addiction_related_behaviors/MedPC_raw_data_files")
allcohorts <- system("grep -ir -b4 'subject: ' . | grep -iE '(end date|subject|box):' ", intern = TRUE)

allcohorts_df <- data.frame(enddate = allcohorts %>% gsub("\r", "", .)%>% grep(".*Date:", ., value = T) %>% sub(".*Date:", "", .) %>% gsub(" ", "", .), 
                            subject = allcohorts %>% gsub("\r", "", .) %>% grep(".*Subject:", ., value = T) %>% sub(".*Subject:", "", .)%>% gsub(" ", "", .),
                            cohort = allcohorts %>% gsub("\r", "", .) %>% grep(".*Subject:", ., value = T) %>% str_match("Cohort \\d+") %>% unlist() %>% as.character(),
                            box = allcohorts %>% gsub("\r", "", .)%>% grep(".*Box:", ., value = T) %>% sub(".*Box:", "", .) %>% gsub(" ", "", .), 
                            experiment = sapply(strsplit(allcohorts %>% gsub("\r", "", .)%>% grep(".*Date:", ., value = T), "[_]"), "[", 4) %>% 
                              gsub("-.*", "",.) %>% 
                              gsub("day ", "", .) %>% 
                              gsub("ction3", "ction 3", .) %>% 
                              gsub("Prime test", "Prime rein", .),
                            filename = allcohorts %>% gsub("\r", "", .) %>% grep(".*Subject:", ., value = T) %>% str_match("MUSC_(.*?):") %>% as.data.frame() %>% 
                              select(V2) %>% unlist() %>% as.character()) %>% 
  arrange(subject, enddate, cohort) %>% 
  mutate(subject = str_extract(subject, "\\d+") %>% as.numeric,
         subject = paste0("KAL", str_pad(subject, 3, "left", "0"))) %>% 
  rename("date" = "enddate")

allcohorts_df_nodupes <- allcohorts_df[!duplicated(allcohorts_df), ] %>% mutate_all(as.character) # all from one file Cohort 2_L room_Extinction 6 because the sessions were run too short the first time and then regular times the second time 


# library(reshape2)
# # to spread and get tally 
# allcohorts_files <- dcast(allcohorts_df_nodupes, formula = subject ~ experiment, fun.aggregate = length) %>% 
#   left_join(allcohorts_df_nodupes[c("subject", "cohort")], ., by = "subject") %>% 
#   arrange(cohort, subject) %>% 
#   select(subject, cohort, "LgA 1", "LgA 2", "LgA 3", "LgA 4", "LgA 5", "LgA 6",
#          "LgA 7", "LgA 8", "LgA 9", "LgA 10", "LgA 11", "LgA 12", "LgA 13", "LgA 14", 
#          "LgA 15", "PR", "Prime rein", "Extinction 1", "Extinction 2", "Extinction 3", 
#          "Extinction 4", "Extinction 5", "Extinction 6", "Cued rein")
# write.xlsx(allcohorts_files, "allcohorts_df_count.xlsx")

# *****************


##  PR_test

# behavioral testing occurred Sunday following 3 weeks of self admin training; 7 est   
# mg/kg/infusion
# active lever = always right (20 ug/kg/infusion; infusion volume is 100 ul); inactive lever = always left
# terminate sessions after 12 hours, or after 1 hour of the next ratio not being achieveed  (seems like most can use start or end date bc the sessions are generally only a few hours long)

# starting cohort xx, immediately after session ended, 

allcohorts_pr_fnames <- grep("progressive", allcohorts_allexp_filenames, ignore.case = T, value = T) #13 files

# Extract subject information
pr_subjects <- lapply(allcohorts_pr_fnames, readsubject) %>% 
  rbindlist(fill = T) %>% 
  rename("subjectid"= "V1") %>% 
  group_by(filename) %>% 
  mutate(numseq = row_number()) %>% 
  ungroup() %>% 
  arrange(filename, numseq)
pr_subjects %>% dplyr::filter(is.na(subjectid)) # check for no na

# P array contains the break points and the O value contains the PR step at which the rat terminated at to give PR_step; 
# M contains the totaL_session_minutes; and B array contains  inactive lever, active lever, infusion, and current ratio information
readParray <- function(x){
  Parray <- fread(paste0("awk '/P:/{flag=1;next}/S:/{flag=0; exit}flag' ", "'", x, "'"), header = F, fill = T)
  return(Parray)
} ## checkin with Apurva
readBarray <- function(x){
  Barray <- fread(paste0("grep -a1 --no-group-separator -En '(B):' ", "'", x, "'", " | grep -E '( 0):'"), header = F, fill = T)
  Barray$filename <- x
  return(Barray)
} 
readM_O <- function(x){
  M_O <- fread(paste0("grep -a1 --no-group-separator -En '(M|O):' ", "'", x, "'", " | grep -E '(M|O):'"), header = F, fill = T)
  M_O$filename <- x
  return(M_O)
} 


pr_Parray <- lapply(allcohorts_pr_fnames[1], readParray) %>% rbindlist(fill = T) %>%    # since this array is the same for all files; you only need one copy and then use the  O value to extract
  select(-V1) %>% 
  data.matrix() %>% 
  t() %>% 
  as.vector() %>% 
  prepend(1)
pr_Parray <- prepend(pr_Parray, 0)

pr_Barray <- lapply(allcohorts_pr_fnames, readBarray) %>% rbindlist(fill = T) %>% 
  select(-c(V2, V7)) %>% 
  dplyr::rename("rownum" = "V1",
              "inactive_lever" = "V3",
              "active_lever" = "V4", 
              "infusions" = "V5",
              "current_ratio" = V6) %>% 
  mutate(rownum = gsub("-", "", rownum) %>% as.numeric) %>% 
  arrange(filename, rownum) %>% 
  cbind(pr_subjects$subjectid) %>% 
  rename("subjectid" = "pr_subjects$subjectid") %>% 
  mutate(subjectid = str_extract(subjectid, "\\d+") %>% as.numeric,
         subjectid = paste0("KAL", str_pad(subjectid, 3, "left", "0")))
  
pr_M_O <- lapply(allcohorts_pr_fnames, readM_O) %>% rbindlist(fill = T) %>% 
  tidyr::separate(V1, c("rownum", "var"), sep = ":") %>% # ignore warning message about 2 pieces bc colon is found twice and the default behavior to remove it is okay
  rename("value" = "V2") 

pr_O_vals <- pr_M_O %>% 
  dplyr::filter(var == "O") %>% 
  cbind(pr_subjects$subjectid) %>% 
  dplyr::select(-c("rownum", "var")) %>% 
  mutate(value = value + 1)  %>% # prevent the 0 digit issue
  rename("subjectid" = "pr_subjects$subjectid") %>% 
  mutate(subjectid = str_extract(subjectid, "\\d+") %>% as.numeric,
         subjectid = paste0("KAL", str_pad(subjectid, 3, "left", "0")))

pr_O_vals$pr_step <- pr_Parray[pr_O_vals$value]

pr_M_vals <- pr_M_O %>% 
  dplyr::filter(var == "M") %>% 
  arrange(filename, rownum) %>% 
  cbind(pr_subjects$subjectid) %>% 
  rename("subjectid" = "pr_subjects$subjectid",
         "total_session_minutes" = "value")

pr_allsubjects <- merge(pr_O_vals[c("pr_step", "subjectid")], pr_M_vals[c("total_session_minutes", "subjectid")]) %>% 
  merge(pr_Barray) %>% 
  select(-c(rownum)) %>%
  mutate(subjectid = as.character(subjectid),
    subjectid = if_else(grepl("KAL", subjectid), subjectid, paste0("KAL", str_pad(subjectid, 3, "left", 0)))) %>%
  # mutate(cohort = str_match(filename, "/(.*?)/")[,2] %>% gsub("^.*([0-9]+).*", "\\1", .) %>% str_pad(., 2, pad = "0")) %>% 
  left_join(kalivas_allcohorts[,c("cohort_number", "sex", "rfid", "dob", "internal_id", "comments", "resolution")], ., by = c("internal_id"= "subjectid")) %>% 
  mutate(filename = gsub(".*MUSC_", "", filename)) %>% 
  left_join(., allcohorts_df_nodupes[, c("startdate", "filename")]) %>% 
  mutate(startdate = unlist(startdate) %>% as.character %>% gsub('([0-9]+/[0-9]+/)', '\\120', .) %>% as.POSIXct(format="%m/%d/%Y"),
         experimentage = (startdate - dob) %>% as.numeric %>% round) %>% 
  distinct() %>% 
  arrange(cohort_number, internal_id) %>% 
  select(-c("dob")) %>%  
  select(cohort_number, sex, rfid, internal_id, startdate, everything())

# *****************
##  Extinction_prime_test (Is it primed reinstatement?)


# the day after 3 days of heroin re-establishment (lga 13-15) from 1900 to 1est
# no syringes
# sessions terminate after 6 hours
# active lever (right lever) no longer results in heroin infusion, presentation of cue light, or the time out period
# first 4 hours is extinction training; 2 hours left = receive injection of 0.25 mg/kg of heroin subcutaneous (heroin prime) XX ASKING OKSANA IS THIS SEEMS STANDARD
# assess how the drug being "on board" affects drug-seeking behavior

allcohorts_expr_fnames <- grep("Primed reinstatement", allcohorts_allexp_filenames, ignore.case = T, value = T) #72 files


# Extract subject information
expr_subjects <- lapply(allcohorts_expr_fnames, readsubject) %>% 
  rbindlist(fill = T) %>% 
  rename("subjectid"= "V1")
expr_subjects %>% dplyr::filter(is.na(subjectid)) # check for no na


# get A (inactive) lever presses 
readAarray <- function(x){
  Aarray <- fread(paste0("awk '/A:/{flag=1;next}/B:/{flag=0}flag' ", "'", x, "'"), header = F, fill = T)
  return(Aarray)
} ## checkin with Apurva

expr_Aarray <- lapply(allcohorts_expr_fnames, readAarray) %>% rbindlist(fill = T)
expr_Aarray_indices <- grep("^0:$", expr_Aarray$V1)
split_Aarray_expr <- split(expr_Aarray, cumsum(1:nrow(expr_Aarray) %in% expr_Aarray_indices))
processedAdata_expr <- lapply(split_Aarray_expr, function(x){
  indexremoved <- x %>% select(-V1)
  Aarray <- as.vector(t(data.matrix(indexremoved)))
  Aarray <- Aarray[!is.na(Aarray)]
  Aarray <- colSums(matrix(Aarray, nrow=2)) # take the sumes of every other row by forming a matrix and taking the column sums
  # Aarray <- data.frame(inactive_leverpresses = Aarray[1:6], time_bin = paste0("inactive_hour_", 1:6))
  Aarray <- data.frame(leverpresses = Aarray[1:6], time_bin = paste0("hour_", paste0(1:6)), lever = "inactive")
  return(Aarray)
})
names(processedAdata_expr) <- expr_subjects$subjectid
processedAdata_expr_wide <- processedAdata_expr %>% rbindlist(idcol = "subjectid") %>% spread(time_bin, leverpresses)

# get D (active) lever presses
readDarray <- function(x){
  Darray <- fread(paste0("awk '/D:/{flag=1;next}/E/{flag=0}flag' ", "'", x, "'"), header = F, fill = T)
  return(Darray)
} ## checkin with Apurva

expr_Darray <- lapply(allcohorts_expr_fnames, readDarray) %>% rbindlist(fill = T)
expr_Darray_indices <- grep("^0:$", expr_Darray$V1)
split_Darray_expr <- split(expr_Darray, cumsum(1:nrow(expr_Darray) %in% expr_Darray_indices))
processedDdata_expr <- lapply(split_Darray_expr, function(x){
  indexremoved <- x %>% select(-V1)
  Darray <- as.vector(t(data.matrix(indexremoved)))
  Darray <- Darray[!is.na(Darray)]
  Darray <- colSums(matrix(Darray, nrow=2)) # take the sumes of every other row by forming a matrix and taking the column sums
  # Darray <- data.frame(active_leverpresses = Darray[1:6], time_bin = paste0("active_hour_", 1:6))
  Darray <- data.frame(leverpresses = Darray[1:6], time_bin = paste0("hour_", paste0(1:6)), lever = "active")
  return(Darray)
})
names(processedDdata_expr) <- expr_subjects$subjectid
processedDdata_expr_wide <- processedDdata_expr %>% rbindlist(idcol = "subjectid") %>% spread(time_bin, leverpresses)

expr_allsubjects <- rbind(processedAdata_expr_wide, processedDdata_expr_wide) %>%  
  merge(expr_subjects)  %>%
  mutate(subjectid = as.character(subjectid),
         subjectid = if_else(grepl("KAL", subjectid), subjectid, paste0("KAL", str_pad(subjectid, 3, "left", 0)))) %>%
  left_join(kalivas_allcohorts[,c("cohort_number", "sex", "rfid", "dob", "internal_id", "comments", "resolution")], ., by = c("internal_id"= "subjectid")) %>% 
  mutate(filename = gsub(".*MUSC_", "", filename)) %>% 
  left_join(., allcohorts_df[, c("startdate", "filename")]) %>% 
  mutate(startdate = unlist(startdate) %>% as.character %>% gsub('([0-9]+/[0-9]+/)', '\\120', .) %>% as.POSIXct(format="%m/%d/%Y"),
         experimentage = (startdate - dob) %>% as.numeric %>% round) %>% 
  distinct() %>% 
  arrange(cohort_number, internal_id) %>% 
  select(-c("dob")) %>%  
  select(cohort_number, sex, rfid, internal_id, startdate, everything())


ex_allsubjects <- merge(ex_W[, c("subjectid", "inactive_lever")], ex_X[, c("subjectid", "active_lever", "filename")]) %>%
  mutate(subjectid = str_extract(subjectid, "\\d+") %>% as.numeric,
         subjectid = paste0("KAL", str_pad(subjectid, 3, "left", "0"))) %>% 
  left_join(WFU_Kalivas_test_df[,c("cohort", "sex", "rfid", "dob", "labanimalid")], ., by = c("labanimalid" = "subjectid")) %>% # get rat basic info
  left_join(., kalivas_cohort_xl[, c("internal_id", "rfid", "comments", "resolution")], by = c("labanimalid" = "internal_id", "rfid")) %>% # join comments to explain missing data
  mutate(filename = gsub(".*MUSC_", "", filename)) %>% # shorten the filename
  left_join(., allcohorts_df_nodupes[, c("startdate", "filename")], by = "filename") %>% # get file date 
  rename("date" = "startdate") %>% 
  mutate(date = unlist(date) %>% as.character %>% gsub('([0-9]+/[0-9]+/)', '\\120', .) %>% as.POSIXct(format="%m/%d/%Y"),
         experimentage = (date - dob) %>% as.numeric %>% round,
         session = gsub(".*Extinction ", "", filename)) %>%
  distinct() %>% 
  arrange(cohort, labanimalid) %>% 
  select(-c("dob")) %>%  
  select(cohort, sex, rfid, labanimalid, date, inactive_lever, active_lever, experimentage, filename, comments, resolution)



# *****************
##  Extinction

# start the day after prime reinstatement test; occurs for 6 straight days from 1900 to 2100est (friday-wednesday)
# syringes were not needed 
# food, water bottle, and metal toy were not placed in cage
# session runs for 2 hours

allcohorts_ex_fnames <- grep("extinction", allcohorts_allexp_filenames, ignore.case = T, value = T) #72 files

# Extract subject information
ex_subjects <- lapply(allcohorts_ex_fnames, readsubject) %>% 
  rbindlist(fill = T) %>% 
  rename("subjectid"= "V1") %>% 
  group_by(filename) %>% 
  mutate(numseq = row_number()) %>% 
  ungroup() %>% 
  arrange(filename, numseq)
ex_subjects %>% dplyr::filter(is.na(subjectid)) # check for no na

read_WX <- function(x){
  WX <- fread(paste0("grep -irEn '^(W|X):' ", "'", x, "'"), header = F, fill = T)
  WX$filename <- x 
  return(WX)
}

ex_WX <- lapply(allcohorts_ex_fnames, read_WX) %>% rbindlist(fill = T) %>%
  tidyr::separate(V1, c("rownum", "var"), sep = ":") %>% 
  rename("value" = "V2") %>%
  mutate(rownum = as.numeric(rownum)) %>% 
  arrange(filename, rownum)   
ex_W <- ex_WX %>% 
  dplyr::filter(var == "W") %>% 
  cbind(ex_subjects$subjectid, .) %>% 
  rename("subjectid" = "ex_subjects$subjectid",
         "inactive_lever" = "value") 
ex_X <- ex_WX %>% 
  dplyr::filter(var == "X") %>% 
  cbind(ex_subjects$subjectid, .) %>% 
  rename("subjectid" = "ex_subjects$subjectid",
         "active_lever" = "value")
ex_allsubjects <- merge(ex_W[, c("subjectid", "inactive_lever")], ex_X[, c("subjectid", "active_lever", "filename")]) %>%
  mutate(subjectid = str_extract(subjectid, "\\d+") %>% as.numeric,
         subjectid = paste0("KAL", str_pad(subjectid, 3, "left", "0"))) %>% 
  left_join(WFU_Kalivas_test_df[,c("cohort", "sex", "rfid", "dob", "labanimalid")], ., by = c("labanimalid" = "subjectid")) %>% # get rat basic info
  left_join(., kalivas_cohort_xl[, c("internal_id", "rfid", "comments", "resolution")], by = c("labanimalid" = "internal_id", "rfid")) %>% # join comments to explain missing data
  mutate(filename = gsub(".*MUSC_", "", filename)) %>% # shorten the filename
  left_join(., allcohorts_df_nodupes[, c("startdate", "filename")], by = "filename") %>% # get file date 
  rename("date" = "startdate") %>% 
  mutate(date = unlist(date) %>% as.character %>% gsub('([0-9]+/[0-9]+/)', '\\120', .) %>% as.POSIXct(format="%m/%d/%Y"),
         experimentage = (date - dob) %>% as.numeric %>% round,
         session = gsub(".*Extinction ", "", filename)) %>%
  distinct() %>% 
  arrange(cohort, labanimalid) %>% 
  select(-c("dob")) %>%  
  select(cohort, sex, rfid, labanimalid, date, inactive_lever, active_lever, experimentage, filename, comments, resolution)



#   mutate(subjectid = as.character(subjectid),
#          subjectid = if_else(grepl("KAL", subjectid), subjectid, paste0("KAL", str_pad(subjectid, 3, "left", 0)))) %>%
#   left_join(kalivas_allcohorts[,c("cohort_number", "sex", "rfid", "dob", "internal_id")], ., by = c("internal_id"= "subjectid")) %>% 
#   mutate(filename = gsub(".*MUSC_", "", filename)) %>% 
#   left_join(., allcohorts_df[, c("startdate", "filename")]) %>% 
#   mutate(startdate = unlist(startdate) %>% as.character %>% gsub('([0-9]+/[0-9]+/)', '\\120', .) %>% as.POSIXct(format="%m/%d/%Y"),
#          experimentage = (startdate - dob) %>% as.numeric %>% round,
#          session = gsub(".*Extinction ", "", filename)) %>% 
#   distinct() %>% 
#   arrange(cohort_number, internal_id) %>% 
#   select(-c("dob")) %>%  
#   select(cohort_number, sex, rfid, internal_id, session, startdate, inactive_lever, active_lever, experimentage, filename) 
# ### most of the NA are dead animals BUT check because there might be exceptions


# *****************
##  Cued reinstatement

# occured the day after the last extinction training session (thursday) 
# sessions terminated after 2 hours
# *** XX typo in the time, should be 
# example from cohort 2 -- 
# Start Time: 18:09:56
# End Time: 20:47:26
# again, lack of food, water, and toy
# instead of heroin; presented with cue light and tone (5 sec) and house light off (20 sec)
# objective is to assess the conditioned reinforcing properties of cue light and tone that was paired with the heroin infusion during self admin training 


allcohorts_rein_fnames <- grep("cued reinstatement", allcohorts_allexp_filenames, ignore.case = T, value = T) #12 files

# Extract subject information
rein_subjects <- lapply(allcohorts_rein_fnames, readsubject) %>% 
  rbindlist(fill = T) %>% 
  rename("subjectid"= "V1") %>% 
  group_by(filename) %>% 
  mutate(numseq = row_number()) %>% 
  ungroup() %>% 
  arrange(filename, numseq)
rein_subjects %>% dplyr::filter(is.na(subjectid)) # check for no na

rein_W <- lapply(allcohorts_rein_fnames, function(x){
  WX <- fread(paste0("grep -iEn '^(W|X):' ", "'", x, "'"), header = F, fill = T)
  WX$filename <- x 
  return(WX)
}) %>% rbindlist(fill = T) %>%
  tidyr::separate(V1, c("rownum", "var"), sep = ":") %>% 
  rename("value" = "V2") %>%
  mutate(rownum = as.numeric(rownum)) %>% 
  arrange(filename, rownum) %>% 
  dplyr::filter(var == "W") %>% 
  arrange(filename, rownum) %>% 
  cbind(rein_subjects$subjectid, .) %>% 
  rename("subjectid" = "rein_subjects$subjectid",
         "inactive_lever" = "value") 

rein_X <- lapply(allcohorts_rein_fnames, function(x){
  WX <- fread(paste0("grep -iEn '^(W|X):' ", "'", x, "'"), header = F, fill = T)
  WX$filename <- x 
  return(WX)
}) %>% rbindlist(fill = T) %>%
  tidyr::separate(V1, c("rownum", "var"), sep = ":") %>% 
  rename("value" = "V2") %>%
  mutate(rownum = as.numeric(rownum)) %>% 
  arrange(filename, rownum) %>% 
  dplyr::filter(var == "X") %>% 
  cbind(rein_subjects$subjectid, .) %>% 
  rename("subjectid" = "rein_subjects$subjectid",
         "active_lever" = "value")

rein_allsubjects <- merge(rein_W[, c("subjectid", "inactive_lever")], rein_X[, c("subjectid", "active_lever", "filename")]) %>% 
  mutate(subjectid = str_extract(subjectid, "\\d+") %>% as.numeric,
         subjectid = paste0("KAL", str_pad(subjectid, 3, "left", "0"))) %>% 
  # left_join(kalivas_allcohorts[,c("cohort_number", "sex", "rfid", "dob", "internal_id")], ., by = c("internal_id"= "subjectid")) %>% 
  left_join(WFU_Kalivas_test_df[,c("cohort", "sex", "rfid", "dob", "labanimalid")], ., by = c("labanimalid" = "subjectid")) %>% # get rat basic info
  left_join(., kalivas_cohort_xl[, c("internal_id", "rfid", "comments", "resolution")], by = c("labanimalid" = "internal_id", "rfid")) %>% # join comments to explain missing data
  mutate(filename = gsub(".*MUSC_", "", filename)) %>% # shorten the filename
  # left_join(., allcohorts_df[, c("startdate", "filename")]) %>% # get file info
  left_join(., allcohorts_df_nodupes[, c("startdate", "filename")], by = "filename") %>% # get file date 
  rename("date" = "startdate") %>% 
  mutate(date = unlist(date) %>% as.character %>% gsub('([0-9]+/[0-9]+/)', '\\120', .) %>% as.POSIXct(format="%m/%d/%Y"),
         experimentage = (date - dob) %>% as.numeric %>% round) %>% 
  distinct() %>% 
  # arrange(cohort_number, internal_id) %>% 
  arrange(cohort, labanimalid) %>% 
  select(-c("dob")) %>%  
  # select(cohort_number, sex, rfid, internal_id, date, inactive_lever, active_lever, experimentage, filename)
  select(cohort, sex, rfid, labanimalid, date, inactive_lever, active_lever, experimentage, filename, comments, resolution)
  
### most of the NA are dead animals BUT check because there might be exceptions # make excel vs raw graph esp for KAL043 because there seem to be wrong data














# ################# old code; before they uploaded the text files 
# 
# setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/behavioral_tasks")
# 
# 
# # self defined functions
# uniform.var.names.testingu01 <- function(df){
#   lapply(seq_along(df), function(i) {
#     if(grepl("Parent", names(df[[i]])) %>% any()){
#       names(df[[i]])[1:2] <- df[[i]][1,][1:2] %>% as.character()
#       df[[i]] <- df[[i]][-1, ]
#     }
#     names(df[[i]]) <- mgsub::mgsub(names(df[[i]]),
#                                    c(" |\\.", "#", "Transponder ID", "Date of Wean|Wean Date","Animal", "Shipping|Ship", "Dams"),
#                                    c("", "Number", "RFID", "DOW","LabAnimal", "Shipment", "Dames"))
#     # names(df[[i]]) <- mgsub::mgsub(names(df[[i]]),
#     #                                c("DateofShipment", "LabAnimalID"), 
#     #                                c("ShipmentDate", "LabAnimalNumber"))
#     names(df[[i]]) <- mgsub::mgsub(names(df[[i]]),
#                                    c("DateofShipment", "LabAnimalNumber"), 
#                                    c("ShipmentDate", "LabAnimalID")) # actually keep the column named lab animal number
#     names(df[[i]]) <- tolower(names(df[[i]]))
#     df[[i]]
#   })
# }
# 
# ############################
# # Exp 1: ELEVATED PLUS MAZE
# ############################
# setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/behavioral_tasks/elevated_plus_maze")
# 
# # all wmv files
# 
# 
# ############################
# # Exp 2: OPEN FIELD TASK
# ############################
setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/behavioral_tasks/open_field_task")
# cohort 2-4 excel and ACT files
openfieldtask_files_raw <- list.files(path = ".", pattern = "*.ACT", full.names = T, recursive = T) #38 files

read_oft <- function(x){
  read_oft <- fread(x, fill = T)
  read_oft$actfilename <- x
  return(read_oft)
}

openfieldtask_raw_list <- lapply(openfieldtask_files_raw, read_oft)
names(openfieldtask_raw_list) <- openfieldtask_files_raw

openfieldtask_raw_df <- lapply(openfieldtask_raw_list, function(df){
  df <- df %>%
    select(-V38) %>%
    dplyr::filter(grepl("(?=)^\\d", CAGE, perl = T)) %>%
    mutate(CAGE = as.numeric(CAGE),
           DATE = lubridate::dmy(strptime(DATE,format="%d-%B-%Y")),
           TIME = chron::chron(times=TIME),
           cohort = str_match(actfilename, "/(.*?)/")[,2],
           cohort = str_extract(cohort, "\\d+")) %>%
    arrange(CAGE) 
  # %>%
  #   select(-vmx) # Analyse's email (11/26/19) -- restrictive software for naming the file, so they change the filename immediately after the exp is run; no set protocol for how the files are named upfront bc they change them so quickly to be "more useful and descriptive"; " therefore, I would not be too concerned about the VMX filenames in general"
  names(df) <- mgsub::mgsub(names(df),c("-| "), c("_")) %>%
    tolower() %>%
    make.unique(sep = ".")
  return(df)
  }) %>% rbindlist()

# # note: the raw "total" summary stats are created in QC_PLOT_RAW_VS_EXCEL.R
# # to do: MUSC (Analyse) email (11/26/19) 'n some of the ACT files the subject ID was either mislabeled or unlabeled when the experimental session was originally set up, due to either operator error or the animal needing to switch cages at the last minute after it was too late to change the subject ID. So every animals data is there but the subject ID number does not match the cage it was run in--we know which cage each animal is ultimately run in because we take notes of during each session and write down any unexpected changes or errors. Find information in This information is clarified in the README files and the comments section in the Excel book."


# Making README changes for each cohort
# From README_MUSC Cohort 2 
# Changed database 2/20
# Open field. Rats KAL041 and KAL042 were switched in locomotor boxes during second OFT so in raw data file rat KAL041 was run in box 2 labeled “KAL042” and KAL042 was run in box 1 labeled “KAL041,” KAL056 was run in box 7  labeled “NOANIMAL” in raw data file during the second OFT because box 8 wouldn’t start.  
openfieldtask_raw_df <- openfieldtask_raw_df %>% 
  mutate(subject_id = replace(subject_id, grepl("cohort02_group1_OF2", actfilename) & subject_id == "KAL041", "KAL0041"),
         subject_id = replace(subject_id, grepl("cohort02_group1_OF2", actfilename) & subject_id == "KAL042", "KAL041"),
         subject_id = replace(subject_id, grepl("cohort02_group1_OF2", actfilename)& subject_id == "KAL0041", "KAL041"),
         subject_id = replace(subject_id, grepl("cohort02_group1_OF2", actfilename)& subject_id == "NO ANIMAL", "KAL056")
         )
# Open field 1, raw data file titled “cohort02_group3_OF1_raw_data.ACT” mistakenly has KAL listed for subjects in all boxes; see excel file of raw data for correct subjects. 
u01.importxlsx_cT <- function(xlname){
  path_sheetnames <- excel_sheets(xlname)
  df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname, col_names = T) ## note the difference here, bc we don't want headers 
  names(df) <- path_sheetnames
  return(df)
}
cohort02_group3_OF1_raw_data_xl <-
  u01.importxlsx_cT("cohort02/cohort02_group3_OF1.xlsx")[[1]] %>%
  as.data.frame() %>%   
  clean_names() %>% 
  rename_all(funs(stringr::str_replace_all(., '_\\d+', ''))) %>%
  subset(!is.na(subject_id) & !is.na(filename) & !is.na(cage)) %>% # remove the by subject id aggregate summary stats 
  select(-(x38:rmovno.1) ) %>%  ## remove the by cage aggregate summary stats (same values as those above but diff formats
  rename("subject_id_xl" = "subject_id")

openfieldtask_raw_df <- openfieldtask_raw_df %>% 
  mutate(subject_id = replace(subject_id, grepl("cohort02_group3_OF1_raw_data.ACT", actfilename), NA)) %>% 
  left_join(., cohort02_group3_OF1_raw_data_xl[, c("cage", "hactv", "totdist", "filename", "subject_id_xl")], by = c("cage", "hactv", "totdist", "filename")) %>% 
  mutate(subject_id = coalesce(subject_id, subject_id_xl)) %>% 
  select(-subject_id_xl)

# Vertical beams on open field were placed too low, so any vertical activity (including rearing activity) is inaccurate.  
openfieldtask_raw_df <- openfieldtask_raw_df %>% 
  mutate_at(vars(vactv, vmovno, vtime, ractv), ~ replace(., cohort == "02", NA)) # changed these four based on the oft_column_descriptions that included "vert" ## the beams were raised for subsequent cohorts and this change is reflected in open_field_protocol_v2

## Mistaken subjects for the following subjects, must also use excel files to replace
cohort03_OFT_xl_tochange <- list.files(recursive = T, pattern = ".xlsx") %>% 
  grep("cohort03_subject_81_to_88|cohort03_subject_89_and_90|cohort03_subject_91_to_98_except96|99_100_96", ., value = T)
cohort03_raw_data_xl <-
  lapply(cohort03_OFT_xl_tochange, u01.importxlsx_cT) %>%
  unlist(recursive = F) %>% 
  rbindlist(fill = T) %>% 
  as.data.frame() %>%
  clean_names() %>%
  rename_all(funs(stringr::str_replace_all(., '_\\d+', ''))) %>%
  subset(!is.na(subject_id) & !is.na(filename) & !is.na(cage)) %>% # remove the by subject id aggregate summary stats 
  select(-(x38:rmovno.1) ) %>%    ## remove the by cage aggregate summary stats (same values as those above but diff format)
  rename("subject_id_xl" = "subject_id")
openfieldtask_raw_df <- openfieldtask_raw_df %>%
  mutate(subject_id = replace(subject_id, grepl("81_to_88_OF1_raw|89_and_90_OF1_raw|91_to_98_except96_OF1_raw|99_100_96_OF1_raw", actfilename), NA)) %>% 
  left_join(., cohort03_raw_data_xl[, c("cage", "hactv", "totdist", "filename", "subject_id_xl")], by = c("cage", "hactv", "totdist", "filename")) %>% 
  mutate(subject_id = coalesce(subject_id, subject_id_xl)) %>% 
  select(-subject_id_xl) %>% distinct() ## once we figure out why there is an extra entry, and then merge (distinct resolved it; original dimension is restore)

# In OF2, KAL106 was run in box 5 labeled “NOANIMAL” in raw data file because box 4 wouldn’t start.  
openfieldtask_raw_df <- openfieldtask_raw_df %>% 
  mutate(subject_id = replace(subject_id, grepl("cohort03_subject_101_102_105_106_OF2_raw_data.ACT", actfilename) & subject_id == "NOANIMAL" & cage == 5, "KAL106" ))
  

  
# ############################
# # Exp 3: TAIL FLICK
# ############################
# 
# 
