## CREATE RAW TO IMITATE ITALY DATA


#### 
setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/addiction_related_behaviors/MedPC_raw_data_files")
allcohorts_allexp_filenames <- list.files(full.names = T, recursive = T) #1133 files

## in addiction tasks 



#####  LONG ACCESS

# all infusions are mg/kg/infusion
# behavioral testing occurred 4 days/wk (sun-thurs) from 19:00 to 7:00 EST
# syringe refill calculation based on the most recent weight from the animal 
# active lever = always right (20ug/kg/infusion; infusion volume is 100 ul; fixed ratio 1)


## cohort xx : after the session ended, rats received 0.05 ml of a mixture of Heparin and 1 mg/ml Gentamicin Sulfate (antibiotic) to maintain catheter patency and animal health
allcohorts_longaccess_fnames <- grep("long", allcohorts_allexp_filenames, ignore.case = T, value = T) #743 (cohort 2,3,4,5)

## TO GET : ESCALATION OF HEROIN INTAKE 12H 
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

lga_merge %>% naniar::vis_miss() #complete cases all 1786 observations

lga_merge <- lga_merge %>% 
  mutate(subjectid = replace(subjectid, grepl("MUSC_Cohort 4_L room_LgA 2", filename)&subjectid=="KAL169", "KAL159")) ## 4/16 "It is supposed to say KAL159 instead of KAL169. I compared it to the data sheet and the data matches KAL159." -Ayteria


selfadmin_escalation_12h <- lga_merge %>% 
  mutate(cohort = str_match(filename, "/(.*?)/")[,2], 
         cohort = str_extract(cohort, "\\d+"),
         filename = gsub(".*MUSC_", "", filename),
         intake = infusions * 20) %>% 
  # day_session = gsub(".*day", "", filename)) %>% 
  select(cohort, subjectid, filename, infusions, intake) %>% 
  arrange(cohort, subjectid, filename) %>% 
  subset(!subjectid %in% c("KALNA", "KAL000")) %>% 
  mutate(day_session = str_match(filename, "LgA (day )?\\d+")[,1] %>% parse_number()) %>% 
  subset(day_session %in% c(1:3, 10:12)) %>% 
  mutate(day_session_type = ifelse(day_session %in% c(1:3), "early", "late")) %>% 
  group_by(cohort, subjectid, day_session_type) %>% 
  summarize(avg_intake_12h = mean(intake)) %>% 
  ungroup() %>% 
  spread(day_session_type, avg_intake_12h) %>% 
  transmute(cohort = cohort, subjectid = subjectid, 
            escalation_12h = late - early)
selfadmin_escalation_12h %>% select(cohort) %>% table()

### deal with missing subjects later on XX 
lga_merge %>% subset(!subjectid %in% c("KALNA", "KAL000")) %>% dim # 2021
  


## TO GET : ESCALATION OF HEROIN INTAKE DURING THE FIRST HOUR OF SA
## lga -- first hour of the session
# Matrix S reports inter-infusion intervals in seconds, so your first hour will be any infusion equal to or under 3600 seconds (the counter is in respect to second 0).

readSarray <- function(x){
  Sarray <- fread(paste0("awk '/S:/{flag=1}/Start Date:|U:|^$/{flag=0}flag' ", "'", x, "'"), header = F, fill = T)
  Sarray$filename <- x
  Sarray <- Sarray %>%
    separate(V1, into = c("V2", "V3", "V4", "V5", "V6", "V7"), sep = "[[:space:]]+") %>%
    dplyr::filter(V2=="S:"&lead(V2)=="S:"|grepl("^\\d", V2)|V2=="S:"&row_number()==n())
  return(Sarray)
} 
lga_Sarray <- lapply(allcohorts_longaccess_fnames, readSarray) %>% rbindlist(fill = T)
# lga_Sarray %>% subset(is.na(V1)) FIND WHICH ONES RETURNED A NULL DATA.TABLE
lga_Sarray_indices <- grep("^[S0]:$", lga_Sarray$V2)

processedSdata_lga <- lapply(split(lga_Sarray, cumsum(1:nrow(lga_Sarray) %in% lga_Sarray_indices)), function(x){
  Sarray <- as.vector(t(data.matrix(x)))
  Sarray_df <- data.frame(Sarray = Sarray,
                          filename = x$filename[1]) %>% 
    group_by(filename) %>% 
    summarize(infusions = ifelse(sum(Sarray, na.rm = T) == 0, NA, length(Sarray[which(Sarray<3600)]))) %>% 
    ungroup()
  return(Sarray_df)
}) %>% rbindlist(fill = T) %>% 
  mutate(filename = as.character(filename))

if(nrow(processedSdata_lga) == nrow(lga_subjects)){
  processedSdata_lga <- cbind(processedSdata_lga, lga_subjects[,"subjectid"]) %>% 
    mutate(subjectid = str_extract(subjectid, "\\d+") %>% as.numeric,
           subjectid = paste0("KAL", str_pad(subjectid, 3, "left", "0")))
  print("LGA_S_merge object created")
} else {
  processedSdata_lga %>% 
    group_by(filename) %>% 
    add_count(filename) %>% 
    merge(., lga_subjects %>% 
            count(filename), by = "filename") %>%   
    rename("numberofBarrays" = "n.x", "numberofsubjects" = "n.y") %>% 
    select(numberofBarrays, numberofsubjects, filename) %>% 
    group_by(filename) %>% 
    slice(1) %>% 
    dplyr::filter(numberofBarrays != numberofsubjects) 
  print("LGA S DATA COULD NOT BE JOINED")}


processedSdata_lga %>% subset(!subjectid %in% c("KALNA", "KAL000")) %>% dim #2021
# processedSdata_lga %>% subset(is.na(intake))

selfadmin_escalation_1h <- processedSdata_lga %>% 
  mutate(cohort = str_match(filename, "/(.*?)/")[,2], 
         cohort = str_extract(cohort, "\\d+"),
         filename = gsub(".*MUSC_", "", filename),
         intake = infusions * 20) %>% 
  select(-infusions) %>% 
  arrange(cohort, subjectid, filename) %>% 
  subset(!subjectid %in% c("KALNA", "KAL000")) %>% 
  mutate(day_session = str_match(filename, "LgA (day )?\\d+")[,1] %>% parse_number()) %>% 
  subset(day_session %in% c(1:3, 10:12)) %>% 
  mutate(day_session_type = ifelse(day_session %in% c(1:3), "early", "late")) %>% 
  group_by(cohort, subjectid, day_session_type) %>% 
  summarize(avg_intake_1h = mean(intake)) %>% 
  ungroup() %>% 
  spread(day_session_type, avg_intake_1h) %>% 
  transmute(cohort = cohort, subjectid = subjectid, 
            escalation_1h = late - early)
selfadmin_escalation_1h %>% select(cohort) %>% table()

## TO GET: TOTAL HEROIN CONSUMPTION (MICROGRAM / KG)
#   refers to total consumption expressed in µg/kg during the first 12 days of self-administration
selfadmin_consumption_total <- lga_merge %>% 
  mutate(cohort = str_match(filename, "/(.*?)/")[,2], 
         cohort = str_extract(cohort, "\\d+"),
         filename = gsub(".*MUSC_", "", filename),
         intake = infusions * 20) %>% 
  # day_session = gsub(".*day", "", filename)) %>% 
  select(cohort, subjectid, filename, infusions, intake) %>% 
  arrange(cohort, subjectid, filename) %>% 
  subset(!subjectid %in% c("KALNA", "KAL000")) %>% 
  mutate(day_session = str_match(filename, "LgA (day )?\\d+")[,1] %>% parse_number()) %>% 
  subset(day_session %in% c(1:12)) %>%
  # mutate(day_session_type = ifelse(day_session %in% c(1:3), "early", "late")) %>% 
  group_by(cohort, subjectid) %>% 
  summarize(intake_total = sum(intake)) %>% 
  ungroup()


## TO GET: TOTAL HEROIN CONSUMPTION (MICROGRAM ; MULTIPLY BY KG)
## NOT NEEDED ANYMORE 
# "Nazza and I discussed that and we aren’t going to do that anymore, and are instead going to keep those two measure in ug/kg. The rationale is that we’re looking at individual variation and by converting to just ug we are essentially un-normalizing the data." -Brittany 04/27/2020


## PROGRESSIVE RATIO
selfadmin_pr <- pr_allsubjects %>% 
  select(cohort, subjectid, pr_step, infusions)



############################## PLOTS #########################################3


### PLOT SELF ADMIN
selfadmin_raw_and_italy <- merge(selfadmin_escalation_12h,
                                 selfadmin_escalation_1h,
                                 by = c("cohort", "subjectid")) %>%
  left_join(., selfadmin_consumption_total, by = c("cohort", "subjectid")) %>%
  mutate(u01 = "us") %>%
  left_join(., WFU_Kalivas_test_df[, c("labanimalid", "sex")], by = c("subjectid" =
                                                                        "labanimalid")) %>%  ## check if %>% subset(is.na(sex)) is empty
  left_join(., selfadmin_pr %>% mutate(cohort = parse_number(cohort) %>% as.character), by = c("cohort", "subjectid")) %>%
  left_join(., kalivas_cohort_xl[, c("internal_id", "comments", "resolution")], by = c("subjectid" =
                                                                                         "internal_id")) %>%
  mutate_if(is.numeric, ~ replace(
    .,
    grepl("die|dead", comments, ignore.case = T) |
      grepl("remove", resolution, ignore.case = T),
    NA
  )) %>%
  bind_rows(
    .,
    Italy_lgapr_C01_05_xl %>%
      select(
        cohort,
        transponder_number,
        escalation_of_heroin_intake_12h_in_µg_kg,
        escalation_of_heroin_intake_during_the_1st_hour_of_sa_in_µg_kg,
        total_heroin_consumprion_µg_kg,
        bp,
        pr_infusions_earned,
        sex
      ) %>% #, total_heroin_consumption_µg when the total is extracted
      rename(
        "subjectid" = "transponder_number",
        "escalation_12h" = "escalation_of_heroin_intake_12h_in_µg_kg",
        "escalation_1h" = "escalation_of_heroin_intake_during_the_1st_hour_of_sa_in_µg_kg",
        "intake_total" = "total_heroin_consumprion_µg_kg",
        "pr_step" = "bp", 
        "infusions" = "pr_infusions_earned"
      ) %>%
      mutate_at(vars(-one_of(
        "cohort", "subjectid", "sex"
      )), as.numeric) %>%
      mutate(u01 = "italy")
  ) %>% 
  mutate(cohort = str_pad(cohort, 2, side = "left", pad = "0"))
  

ggplot(selfadmin_raw_and_italy, aes(x = u01, y = escalation_12h)) + 
  geom_boxplot() + 
  facet_grid(~ sex) + 
  labs(title = "Kalivas Site Differences for Escalation 12h by Sex")

ggplot(selfadmin_raw_and_italy, aes(x = escalation_12h)) + 
  geom_histogram() + 
  facet_grid(~ u01 + sex) + 
  labs(title = "Kalivas Site Differences for Escalation 12h by Sex")

ggplot(selfadmin_raw_and_italy, aes(x = escalation_12h)) + 
  geom_histogram() + 
  facet_grid(~ u01) + 
  labs(title = "Kalivas Site Differences for Escalation 12h")

ggplot(selfadmin_raw_and_italy, aes(x = escalation_1h)) + 
  geom_histogram() + 
  facet_grid(~ u01 + sex) + 
  labs(title = "Kalivas Site Differences for Escalation 1h by Sex")
ggplot(selfadmin_raw_and_italy, aes(x = escalation_1h)) + 
  geom_histogram() + 
  facet_grid(~ u01) + 
  labs(title = "Kalivas Site Differences for Escalation 1h")

ggplot(selfadmin_raw_and_italy, aes(x = intake_total)) + 
  geom_histogram() + 
  facet_grid(~ u01 + sex) + 
  labs(title = "Kalivas Site Differences for Total Intake by Sex")
ggplot(selfadmin_raw_and_italy, aes(x = intake_total)) + 
  geom_histogram() + 
  facet_grid(~ u01) + 
  labs(title = "Kalivas Site Differences for Total Intake")






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
  # arrange(subject, enddate, cohort) %>% 
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












# *****************
##  Extinction_prime_test (Is it primed reinstatement?)


# the day after 3 days of heroin re-establishment (lga 13-15) from 1900 to 1est
# no syringes
# sessions terminate after 6 hours
# active lever (right lever) no longer results in heroin infusion, presentation of cue light, or the time out period
# first 4 hours is extinction training; 2 hours left = receive injection of 0.25 mg/kg of heroin subcutaneous (heroin prime) XX ASKING OKSANA IS THIS SEEMS STANDARD
# assess how the drug being "on board" affects drug-seeking behavior

allcohorts_expr_fnames <- grep("Primed reinstatement", allcohorts_allexp_filenames, ignore.case = T, value = T) #46 files


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
  left_join(kalivas_cohort_xl[,c("cohort", "sex", "rfid", "dob", "internal_id", "comments", "resolution")], ., by = c("internal_id"= "subjectid")) %>% 
  mutate(filename = gsub(".*MUSC_", "", filename)) %>% 
  left_join(., allcohorts_df_nodupes[, c("date", "filename")], by = "filename") %>% 
  mutate(date = unlist(date) %>% as.character %>% gsub('([0-9]+/[0-9]+/)', '\\120', .) %>% as.POSIXct(format="%m/%d/%Y"),
         experimentage = (date - dob) %>% as.numeric %>% round) %>% 
  distinct() %>% 
  arrange(cohort, internal_id) %>% 
  select(-c("dob")) %>%  
  select(cohort, sex, rfid, internal_id, date, everything()) %>% 
  mutate_if(is.numeric, ~ replace(
    .,
    grepl("die|dead", comments, ignore.case = T) |
      grepl("remove", resolution, ignore.case = T),
    NA
  )) %>% 
  mutate(lever = as.character(lever)) %>% 
  mutate(context = hour_1 + hour_2,
         extinction_before_priming = hour_3 + hour_4, 
         prime = hour_5 + hour_6) %>% 
  select(-matches("hour")) 

############################## PLOTS #########################################


## keep the session for inactive vs active grouping
withinsession_raw_and_italy <- expr_allsubjects %>% 
  arrange(lever) %>% ## to prep the columns be in the right order before merging/binding rows
  pivot_wider(names_from = lever, values_from = c(context, extinction_before_priming, prime)) %>% 
  select(-(matches("_NA$"))) %>% 
  select(-c("date", "comments", "resolution", "filename", "experimentage")) %>% 
  mutate(heroin_saline_yoked = NA,
         u01 = "us") %>% 
  bind_rows(., Italy_expr_C01_05_xl %>% 
              mutate(measurement = paste0(measurement, "_", session)) %>% 
              select(-c("session", "animal_id_given_by_behav_unit", "loco_index", "coat_color")) %>% # most of the animal id given by behav units are marked "same as breeder" 
              spread(measurement, value) %>% 
              rename("rfid" = "transponder_number", 
                     "internal_id" = "animal_id_given_by_breeder") %>% 
              select(-heroin_saline_yoked, everything()) %>%
              select(cohort, sex, rfid, internal_id, everything()) %>% 
              mutate(u01 = "italy"))
withinsession_raw_and_italy %>% naniar::vis_miss()
withinsession_raw_and_italy %>% 
  subset(is.na(context_active)&is.na(context_inactive))
# *****************
##  Extinction

# start the day after prime reinstatement test; occurs for 6 straight days from 1900 to 2100est (friday-wednesday)
# syringes were not needed 
# food, water bottle, and metal toy were not placed in cage
# session runs for 2 hours

allcohorts_ex_fnames <- grep("extinction", allcohorts_allexp_filenames, ignore.case = T, value = T) #256 files

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
  arrange(filename, rownum) # ignore the warning message: expected 2 pieces; this is expected
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


## fix kalna before merging onto excel 
ex_allsubjects <- cbind(ex_W[, c("subjectid", "inactive_lever")], ex_X[, c("subjectid", "active_lever", "filename")]) %>%
  clean_names() # since the two df's both have subjectid
if(nrow(ex_allsubjects %>% subset(subjectid != subjectid_2)) != 0) {
  print("Fix ex_W and ex_X cbind")
} else {
  ex_allsubjects <- ex_allsubjects %>%
    select(-subjectid_2) %>%
    mutate(
      subjectid = str_extract(subjectid, "\\d+") %>% as.numeric,
      subjectid = paste0("KAL", str_pad(subjectid, 3, "left", "0")),
      session = gsub(".*Extinction ", "", filename),
      self_administration_room = sub(".*MUSC_Cohort \\d+?_(.*?) room.*", "\\1", filename) %>% str_trim(),
      comp = ifelse(
        grepl("comp", filename, ignore.case = T),
        sub(".*room ( )?comp(uter)? (\\d+).*", "\\3", filename) %>% str_trim(),
        NA),
      cohort = str_pad(sub(".*Cohort (.*?)/.*", "\\1", filename),2,side = "left",pad = "0"),
      filename = gsub(".*MUSC_", "", filename)
    ) %>% 
    arrange(filename) %>% 
    cbind(., allcohorts_df %>% subset(grepl("extinction", filename, ignore.case = T)) %>% arrange(filename))
}


# check that this is 0
# ex_allsubjects %>% clean_names() %>% subset(filename != filename_2) %>% nrow

# if subjectid matches subject in all non-kal000 or non-kalna cases, then remove subjectid column, remove other duplicate columns
if(ex_allsubjects %>% clean_names() %>% subset(subjectid != subject & !grepl("KAL(NA|000)", subjectid)) %>% nrow == 0){
  ex_allsubjects <- ex_allsubjects %>% 
    clean_names() %>% 
    select(-c(one_of("subjectid"), matches("_2$"))) %>% 
    rename("subjectid" = "subject")
}

# look to github for broken file pipeline


ex_allsubjects %>% 
  group_by(filename) %>% 
  add_count(filename) %>% 
  merge(., allcohorts_df_nodupes %>%
          subset(grepl("extinction", filename, ignore.case = T)) %>%
          count(filename), by = "filename") %>%   
  rename("numberofsubjects_ex" = "n.x", "numberofsubjects_allcohorts" = "n.y") %>% 
  select(numberofsubjects_ex, numberofsubjects_allcohorts, filename) %>% 
  group_by(filename) %>% 
  slice(1) %>% 
  dplyr::filter(numberofsubjects_ex != numberofsubjects_allcohorts) 


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


allcohorts_rein_fnames <- grep("cued reinstatement", allcohorts_allexp_filenames, ignore.case = T, value = T) #41 files

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
  # left_join(kalivas_cohort_xl[,c("cohort_number", "sex", "rfid", "dob", "internal_id")], ., by = c("internal_id"= "subjectid")) %>% 
  left_join(WFU_Kalivas_test_df[,c("cohort", "sex", "rfid", "dob", "labanimalid")], ., by = c("labanimalid" = "subjectid")) %>% # get rat basic info
  left_join(., kalivas_cohort_xl[, c("internal_id", "rfid", "comments", "resolution")], by = c("labanimalid" = "internal_id", "rfid")) %>% # join comments to explain missing data
  mutate(filename = gsub(".*MUSC_", "", filename)) %>% # shorten the filename
  left_join(., allcohorts_df_nodupes[, c("date", "filename")], by = "filename") %>% # get file date 
  mutate(date = unlist(date) %>% as.character %>% gsub('([0-9]+/[0-9]+/)', '\\120', .) %>% as.POSIXct(format="%m/%d/%Y"),
         experimentage = (date - dob) %>% as.numeric %>% round) %>% 
  distinct() %>% 
  arrange(cohort, labanimalid) %>% 
  select(-c("dob")) %>%  
  select(cohort, sex, rfid, labanimalid, date, inactive_lever, active_lever, experimentage, filename, comments, resolution)

rein_allsubjects %>% naniar::vis_miss()
### most of the NA are dead animals BUT check because there might be exceptions # make excel vs raw graph esp for KAL043 because there seem to be wrong data

rein_allsubjects %>% gather("lever", "value", - ) %>% head(2)


########## PLOTS EXTINCTION AND CUED 
excu_raw_and_italy <- Italy_excu_C01_05_xl %>% 
  # mutate(measurement = paste0(measurement, "_", session)) %>% 
  # select(-c("session", "animal_id_given_by_behav_unit", "loco_index", "coat_color")) %>% # most of the animal id given by behav units are marked "same as breeder" 
  select(-c("animal_id_given_by_behav_unit", "loco_index", "coat_color")) %>% # most of the animal id given by behav units are marked "same as breeder" 
  # spread(measurement, value) %>% 
  rename("rfid" = "transponder_number", 
         "internal_id" = "animal_id_given_by_breeder") %>% 
  select(-heroin_saline_yoked, everything()) %>%
  select(cohort, sex, rfid, internal_id, everything()) %>% 
  mutate(u01 = "italy")









# ############################
# # Exp 1: ELEVATED PLUS MAZE
# ############################
# setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/behavioral_tasks/elevated_plus_maze")
# 
# # all wmv files
# 
# NEED TO FIGURE OUT HOW TO ANALYZE VIDEO FILES

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
           DATE = lubridate::ymd(strptime(DATE,format="%d-%B-%Y")),
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


# # to do: MUSC (Analyse) email (11/26/19) 'n some of the ACT files the subject ID was either mislabeled or unlabeled when the experimental session was originally set up, due to either operator error or the animal needing to switch cages at the last minute after it was too late to change the subject ID. So every animals data is there but the subject ID number does not match the cage it was run in--we know which cage each animal is ultimately run in because we take notes of during each session and write down any unexpected changes or errors. 
# Find information in This information is clarified in the README files and the comments section in the Excel book."
# Making README changes for each cohort
# From README_MUSC Cohort 2 
# Changed database 2/20
# Open field. Rats KAL041 and KAL042 were switched in locomotor boxes during second OFT so in raw data file rat KAL041 was run in box 2 labeled “KAL042” and KAL042 was run in box 1 labeled “KAL041,” KAL056 was run in box 7  labeled “NOANIMAL” in raw data file during the second OFT because box 8 wouldn’t start.  
openfieldtask_raw_df <- openfieldtask_raw_df %>% 
  mutate(subject_id = replace(subject_id, grepl("cohort02_group1_OF2", actfilename) & subject_id == "KAL041", "KAL0041"),
         subject_id = replace(subject_id, grepl("cohort02_group1_OF2", actfilename) & subject_id == "KAL042", "KAL041"),
         subject_id = replace(subject_id, grepl("cohort02_group1_OF2", actfilename) & subject_id == "KAL0041", "KAL042"),
         subject_id = replace(subject_id, grepl("cohort02_group1_OF2", actfilename) & subject_id == "NO ANIMAL", "KAL056")
  )

# KAL056 IS ASSIGNED BC THE BOX 8 FROM ACTFILENAME './cohort02/cohort02_group5_OF2_raw_data.ACT' WAS NOT WORKING

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


# KAL056 was run in box 7  labeled “NOANIMAL” in raw data file during the second OFT because box 8 wouldn’t start. 
openfieldtask_raw_df <- openfieldtask_raw_df %>% 
  mutate(subject_id = replace(subject_id, grepl("C2Group2OF2", filename) & subject_id == "NO ANIMAL" & cage == 7, "KAL056"))

# In the raw data files subjects KAL079 and KAL080 are listed as KAL080 and KAL073 respectively. Data can be found in Cohort02_Group5_OF1
openfieldtask_raw_df <- openfieldtask_raw_df %>% 
  mutate(subject_id = replace(subject_id, grepl("C2Group5OF1", filename) & subject_id == "KAL080", "KAL079"),
         subject_id = replace(subject_id, grepl("C2Group5OF1", filename) & subject_id == "KAL073", "KAL080"))


## Mistaken subjects for the following subjects, must also use excel files to replace
setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/behavioral_tasks/open_field_task")

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
  mutate(subject_id = replace(subject_id, grepl("cohort03_subject_101_102_105_106_OF2_raw_data.ACT", actfilename) & subject_id == "NOANIMAL" & cage == 5, "KAL106" ),
         subject_id = replace(subject_id, grepl("cohort03_subject_91_to_98_except96_OF1_raw_data.ACT", actfilename) & is.na(subject_id) & cage == 6, "KAL096" )) ## assigning despite NA (NOT CONFIRMED WITH KALIVAS TEAM)


# # note: the raw "total" summary stats are created in QC_PLOT_RAW_VS_EXCEL.R
openfieldtask_raw_df_total <- openfieldtask_raw_df %>% group_by(subject_id, cohort, filename, actfilename, cage, date, time) %>%
  summarise_at(c("ctrtime","rmovno", "strno", "totdist", "movtime"), sum) %>% 
  ungroup() %>% 
  rename("center_time_seconds" = "ctrtime",
         "number_of_rears" = "rmovno",
         "number_of_sterotypies" = "strno", 
         "total_cm_traveled" = "totdist", 
         "total_time_traveled_seconds" = "movtime") %>% 
  arrange(subject_id, date, time) %>% 
  mutate(subject_id = toupper(subject_id),
         date = as.character(date)) %>% 
  rename("labanimalid" = "subject_id") %>% 
  subset(rowSums(.[grep("_", names(.))], na.rm = T) != 0) %>%  ## 82 cases were sum == 0 in cohort2-4
  group_by(labanimalid) %>% 
  mutate(session = ifelse(dplyr::row_number(labanimalid) == 1, "before_SA", "after_SA")) %>% 
  ungroup() %>% select(-c("time", "cage", "filename"))

openfieldtask_raw_df_total %>% subset(!grepl("KAL\\d+", labanimalid))
# openfieldtask_raw_df_total %>% add_count(labanimalid) %>% subset(n == 2) %>% distinct(labanimalid, cohort) %>% select(cohort) %>% table()




# used to be relevant here # %>% mutate(actfilename = str_match(actfilename, "/.*/(.*?)_raw*.")[,2])
## note this: could be because of diff cohorts
openfieldtask_raw_df_total %>% select(labanimalid) %>% table() %>% 
  as.data.frame() %>% subset(Freq != 2) %>% 
  left_join(., kalivas_oft_allcohorts_excel_processed[c("internal_id", "comment", "resolution") ] , by = c("." = "internal_id"))


# ############################
# # Exp 3: TAIL FLICK
# ############################
# 
# WILL NOT GET RAW FILES
