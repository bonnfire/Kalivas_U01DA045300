#############################
# Protocol 
#############################







setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/addiction_related_behaviors/MedPC_raw_data_files")
allcohorts_allexp_filenames <- list.files(full.names = T, recursive = T) #295 files

## in addiction tasks 

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
##  " | grep -v '20:' | grep -v 'B:'" could have also worked but it includes 5: from the A array above it


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

dim(lga_Barray)
dim(lga_subjects)
# the dim's don't match -- figure out why
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


lga_merge <- merge(lga_subjects, lga_Barray) %>% mutate(subjectid = ifelse(grepl("KAL", subjectid), subjectid, paste0("KAL", str_pad(subjectid, 3, "left", 0))))
lga_merge %>% naniar::vis_miss()

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
allcohorts <- system("grep -ir -b4 'subject: ' . | grep -iE '(start date|subject|box):' ", intern = TRUE)

startdate <- allcohorts %>% gsub("\r", "", .)%>% grep(".*Date:", ., value = T) %>% sub(".*Date:", "", .) %>% gsub(" ", "", .)
box <- allcohorts %>% gsub("\r", "", .)%>% grep(".*Box:", ., value = T) %>% sub(".*Box:", "", .) %>% gsub(" ", "", .)
subject <- allcohorts %>% gsub("\r", "", .) %>% grep(".*Subject:", ., value = T) %>% sub(".*Subject:", "", .)%>% gsub(" ", "", .)
cohort <- allcohorts %>% gsub("\r", "", .) %>% grep(".*Subject:", ., value = T) %>% str_match("Cohort \\d+") %>% unlist() %>% as.character()
experiment <- sapply(strsplit(allcohorts %>% gsub("\r", "", .)%>% grep(".*Date:", ., value = T), "[_]"), "[", 4) %>% 
  gsub("-.*", "",.) %>% 
  gsub("day ", "", .) %>% 
  gsub("ction3", "ction 3", .) %>% 
  gsub("Prime test", "Prime rein", .)
filename <- allcohorts %>% gsub("\r", "", .) %>% grep(".*Subject:", ., value = T) %>% str_match("MUSC_(.*?):") %>% as.data.frame() %>% 
  select(V2) %>% unlist() %>% as.character()

allcohorts_df <- data.frame(startdate = startdate, 
                            subject = subject,
                            cohort = cohort,
                            box = box, 
                            experiment = experiment,
                            filename = filename) %>% 
  arrange(subject, startdate, cohort)

allcohorts_df_nodupes <- allcohorts_df[!duplicated(allcohorts_df), ] 


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
#   Parray <- as.vector(t(data.matrix(indexremoved)))
  
  # pr_Parray_indices <- grep("^0:$", pr_Parray$V1)
# split_Parray <- split(pr_Parray, cumsum(1:nrow(pr_Parray) %in% pr_Parray_indices))
# processedPdata <- lapply(split_Parray, function(x){
#   indexremoved <- x %>% select(-V1)
#   Parray <- as.vector(t(data.matrix(indexremoved)))
#   Parray <- Parray[!is.na(Parray)]
#   Parray <- prepend(Parray, 1) #PR list with each break point listed, expect for the first breakpoint, which is 1. -MedPC column descriptions (cohort2-- could be an issue for other cohorts- checkXX )
#   Parray <- append(Parray, x$filename[1])
#   return(Parray)
# })

# make.unique = function(x, sep='_'){
#   ave(x, x, FUN=function(a){if(length(a) > 1){paste(a, 1:length(a), sep=sep)} else {a}})
# }
# 
# processedPdata_names <- sapply(processedPdata, function(x){
#   last <- tail(x, 1)
#   return(last)
# }) %>% make.unique() %>% unlist() %>% as.character()
# names(processedPdata) <- processedPdata_names
# 
# processedPdata <- lapply(processedPdata, function(x){
#   x <- x[-length(x)] # remove last value in vector 
#   x <- as.numeric(x) # turn vector back into numeric
#   return(x)
# })

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
  rename("subjectid" = "pr_subjects$subjectid")
  
pr_M_O <- lapply(allcohorts_pr_fnames, readM_O) %>% rbindlist(fill = T) %>% 
  tidyr::separate(V1, c("rownum", "var"), sep = ":") %>% 
  rename("value" = "V2") 

pr_O_vals <- pr_M_O %>% 
  dplyr::filter(var == "O") %>% 
  cbind(pr_subjects$subjectid) %>% 
  dplyr::select(-c("rownum", "var")) %>% 
  mutate(value = value + 1)  %>% # prevent the 0 digit issue
  rename("subjectid" = "pr_subjects$subjectid")
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
  left_join(kalivas_allcohorts[,c("cohort_number", "sex", "rfid", "dob", "internal_id")], ., by = c("internal_id"= "subjectid")) %>% 
  mutate(filename = gsub(".*MUSC_", "", filename)) %>% 
  left_join(., allcohorts_df[, c("startdate", "filename")]) %>% 
  mutate(startdate = unlist(startdate) %>% as.character %>% gsub('([0-9]+/[0-9]+/)', '\\120', .) %>% as.POSIXct(format="%m/%d/%Y"),
         experimentage = (startdate - dob) %>% as.numeric %>% round) %>% 
  distinct() %>% 
  arrange(cohort_number, internal_id) %>% 
  select(-c("dob")) %>%  
  select(cohort_number, sex, rfid, internal_id, startdate, everything())

# *****************
##  Extinction_prime_test (Is it primed reinstatement?)
allcohorts_expr_fnames <- grep("Primed reinstatement", allcohorts_allexp_filenames, ignore.case = T, value = T) #72 files


# Extract subject information
expr_subjects <- lapply(allcohorts_expr_fnames, readsubject) %>% 
  rbindlist(fill = T) %>% 
  rename("subjectid"= "V1")
# might not use this since the matrix is more complicated in finding subject id
# %>% 
#   group_by(filename) %>% 
#   mutate(numseq = row_number()) %>% 
#   ungroup() %>% 
#   arrange(filename, numseq)
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
  left_join(kalivas_allcohorts[,c("cohort_number", "sex", "rfid", "dob", "internal_id")], ., by = c("internal_id"= "subjectid")) %>% 
  mutate(filename = gsub(".*MUSC_", "", filename)) %>% 
  left_join(., allcohorts_df[, c("startdate", "filename")]) %>% 
  mutate(startdate = unlist(startdate) %>% as.character %>% gsub('([0-9]+/[0-9]+/)', '\\120', .) %>% as.POSIXct(format="%m/%d/%Y"),
         experimentage = (startdate - dob) %>% as.numeric %>% round) %>% 
  distinct() %>% 
  arrange(cohort_number, internal_id) %>% 
  select(-c("dob")) %>%  
  select(cohort_number, sex, rfid, internal_id, startdate, everything())


# *****************
##  Extinction
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
  mutate(subjectid = as.character(subjectid),
         subjectid = if_else(grepl("KAL", subjectid), subjectid, paste0("KAL", str_pad(subjectid, 3, "left", 0)))) %>%
  left_join(kalivas_allcohorts[,c("cohort_number", "sex", "rfid", "dob", "internal_id")], ., by = c("internal_id"= "subjectid")) %>% 
  mutate(filename = gsub(".*MUSC_", "", filename)) %>% 
  left_join(., allcohorts_df[, c("startdate", "filename")]) %>% 
  mutate(startdate = unlist(startdate) %>% as.character %>% gsub('([0-9]+/[0-9]+/)', '\\120', .) %>% as.POSIXct(format="%m/%d/%Y"),
         experimentage = (startdate - dob) %>% as.numeric %>% round,
         session = gsub(".*Extinction ", "", filename)) %>% 
  distinct() %>% 
  arrange(cohort_number, internal_id) %>% 
  select(-c("dob")) %>%  
  select(cohort_number, sex, rfid, internal_id, session, startdate, inactive_lever, active_lever, experimentage, filename) 
### most of the NA are dead animals BUT check because there might be exceptions


# *****************
##  Cued reinstatement
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

read_WX <- function(x){
  WX <- fread(paste0("grep -iEn '^(W|X):' ", "'", x, "'"), header = F, fill = T)
  WX$filename <- x 
  return(WX)
}

rein_WX <- lapply(allcohorts_rein_fnames, read_WX) %>% rbindlist(fill = T) %>%
  tidyr::separate(V1, c("rownum", "var"), sep = ":") %>% 
  rename("value" = "V2") %>%
  mutate(rownum = as.numeric(rownum)) %>% 
  arrange(filename, rownum)   
rein_W <- rein_WX %>% 
  dplyr::filter(var == "W") %>% 
  arrange(filename, rownum) %>% 
  cbind(rein_subjects$subjectid, .) %>% 
  rename("subjectid" = "rein_subjects$subjectid",
         "inactive_lever" = "value") 
rein_X <- rein_WX %>% 
  dplyr::filter(var == "X") %>% 
  cbind(rein_subjects$subjectid, .) %>% 
  rename("subjectid" = "rein_subjects$subjectid",
         "active_lever" = "value")
rein_allsubjects <- merge(rein_W[, c("subjectid", "inactive_lever")], rein_X[, c("subjectid", "active_lever", "filename")]) %>% 
  left_join(kalivas_allcohorts[,c("cohort_number", "sex", "rfid", "dob", "internal_id")], ., by = c("internal_id"= "subjectid")) %>% 
  mutate(filename = gsub(".*MUSC_", "", filename)) %>% 
  left_join(., allcohorts_df[, c("startdate", "filename")]) %>% 
  mutate(startdate = unlist(startdate) %>% as.character %>% gsub('([0-9]+/[0-9]+/)', '\\120', .) %>% as.POSIXct(format="%m/%d/%Y"),
         experimentage = (startdate - dob) %>% as.numeric %>% round) %>% 
  distinct() %>% 
  arrange(cohort_number, internal_id) %>% 
  select(-c("dob")) %>%  
  select(cohort_number, sex, rfid, internal_id, startdate, inactive_lever, active_lever, experimentage, filename) 
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
openfieldtask_files_raw <- list.files(path = ".", pattern = "*.ACT", full.names = T, recursive = T)

read_oft <- function(x){
  read_oft <- fread(x, fill = T)
  read_oft$actfilename <- x
  return(read_oft)
}

openfieldtask_raw_list <- lapply(openfieldtask_files_raw, read_oft)
names(openfieldtask_raw_list) = openfieldtask_files_raw

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


# # naniar::vis_miss(rbindlist(openfieldtask_raw_list, fill = T)) nothing abnormal; remove V38 bc all are empty; check why some columns are 100% empty and if the kalivas lab kept these # all na cycle lines contain the C:\\ extension
# openfieldtask_raw_df <- rbindlist(openfieldtask_raw_list, fill = T) # vis_miss only those two columns are empty now 
# 
# # to do: MUSC (Analyse) email (11/26/19) 'n some of the ACT files the subject ID was either mislabeled or unlabeled when the experimental session was originally set up, due to either operator error or the animal needing to switch cages at the last minute after it was too late to change the subject ID. So every animals data is there but the subject ID number does not match the cage it was run in--we know which cage each animal is ultimately run in because we take notes of during each session and write down any unexpected changes or errors. Find information in This information is clarified in the README files and the comments section in the Excel book."
# # note: the raw "total" summary stats are created in QC_PLOT_RAW_VS_EXCEL.R
# 

# Open field. Rats KAL041 and KAL042 were switched in locomotor boxes during second OFT so in raw data file rat KAL041 was run in box 2 labeled “KAL042” and KAL042 was run in box 1 labeled “KAL041,” KAL056 was run in box 7  labeled “NOANIMAL” in raw data file during the second OFT because box 8 wouldn’t start.  
# From README_MUSC Cohort 2 
# Changed database 2/20
openfieldtask_raw_df <- openfieldtask_raw_df %>% 
  mutate(subject_id = replace(subject_id, grepl("cohort02_group1_OF2", actfilename) & subject_id == "KAL041", "KAL0041"),
         subject_id = replace(subject_id, grepl("cohort02_group1_OF2", actfilename) & subject_id == "KAL042", "KAL041"),
         subject_id = replace(subject_id, grepl("cohort02_group1_OF2", actfilename)& subject_id == "KAL0041", "KAL041"),
         subject_id = replace(subject_id, grepl("cohort02_group1_OF2", actfilename)& subject_id == "NO ANIMAL", "KAL056")
         )
# Open field 1, raw data file titled “cohort02_group3_OF1_raw_data.ACT” mistakenly has KAL listed for subjects in all boxes; see excel file of raw data for correct subjects. 
cohort02_group3_OF1_raw_data_xl <-
  u01.importxlsx("cohort02/cohort02_group3_OF1.xlsx")[[1]] %>%
  as.data.frame() %>%
  clean_names() %>%
  rename_all(funs(stringr::str_replace_all(., '_\\d+', ''))) %>%
  subset(!is.na(subject_id) & !is.na(filename) & !is.na(cage)) %>% # remove the by subject id aggregate summary stats 
  select(-(x38:rmovno.1) ) %>%    ## remove the by cage aggregate summary stats (same values as those above but diff format)
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
  lapply(cohort03_OFT_xl_tochange, u01.importxlsx) %>%
  unlist(recursive = F) %>% 
  rbindlist(fill = T) %>% 
  as.data.frame() %>%
  clean_names() %>%
  rename_all(funs(stringr::str_replace_all(., '_\\d+', ''))) %>%
  subset(!is.na(subject_id) & !is.na(filename) & !is.na(cage)) %>% # remove the by subject id aggregate summary stats 
  select(-(x38:rmovno.1) ) %>%    ## remove the by cage aggregate summary stats (same values as those above but diff format)
  rename("subject_id_xl" = "subject_id")


# ############################
# # Exp 3: TAIL FLICK
# ############################
# 
# 
