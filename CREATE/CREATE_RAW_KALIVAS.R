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


# cohort 2 - KAL041-KAL079
# cohort 3 - KAL081-KAL118
# cohort 4 - XX 
# cohort 5 - XX 


#### 
setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/addiction_related_behaviors/MedPC_raw_data_files")
allcohorts_allexp_filenames_c01_09 <- list.files(full.names = T, recursive = T) %>% grep("Cohort [2-9]", ., value = T) #3969 files 

allcohorts_allexp_filenames_c01_09_df <- allcohorts_allexp_filenames_c01_09 %>% as.data.frame() %>% 
  mutate_all(as.character) %>% rename("filename" = ".") %>% 
  mutate(cohort = str_extract(filename, "Cohort \\d+"), 
         exp = gsub("[.]/\\D+ \\d+/(\\D+)/.*", "\\1", filename)) %>% 
  mutate(exp = case_when(
    grepl("long", exp, ignore.case = T) ~ "lga", 
    grepl("prime", exp, ignore.case = T) ~ "p_rein", 
    grepl("prog", exp, ignore.case = T) ~ "pr", 
    grepl("extinct", exp, ignore.case = T) ~ "extinction", 
    grepl("cue", exp, ignore.case = T) ~ "cued reinstatement", 
    TRUE ~ NA_character_
  ))
## in addiction tasks 

allcohorts_allexp_filenames_c01_09 <- list.files(full.names = T, recursive = T) %>% grep("Cohort [2-8]", ., value = T) 

allcohorts_allexp_filenames_c01_09_df <- allcohorts_allexp_filenames_c01_09 %>% as.data.frame() %>% 
  mutate_all(as.character) %>% rename("filename" = ".") %>% 
  mutate(cohort = str_extract(filename, "Cohort \\d+"), 
         exp = gsub("[.]/\\D+ \\d+/(\\D+)/.*", "\\1", filename)) %>% 
  mutate(exp = case_when(
    grepl("long", exp, ignore.case = T) ~ "lga", 
    grepl("prime", exp, ignore.case = T) ~ "p_rein", 
    grepl("prog", exp, ignore.case = T) ~ "pr", 
    grepl("extinct", exp, ignore.case = T) ~ "extinction", 
    grepl("cue", exp, ignore.case = T) ~ "cued reinstatement", 
    TRUE ~ NA_character_
  )) 


# *****************
# generate metadata from raw files, to be compared to the metadata recorded in excel
# create missingness table for kalivas team
setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/addiction_related_behaviors/MedPC_raw_data_files")
allcohorts <- system("grep -ir -b4 'subject: ' . | grep -iE '(end date|subject|box):' ", intern = TRUE)

allcohorts_unfixed <-
  data.frame(
    enddate = allcohorts %>% gsub("\r", "", .) %>% grep(".*Date:", ., value = T) %>% sub(".*Date:", "", .) %>% gsub(" ", "", .),
    subject = allcohorts %>% gsub("\r", "", .) %>% grep(".*Subject:", ., value = T) %>% sub(".*Subject:", "", .) %>% gsub(" ", "", .),
    cohort = allcohorts %>% gsub("\r", "", .) %>% grep(".*Subject:", ., value = T) %>% str_match("Cohort \\d+") %>% unlist() %>% as.character(),
    box = allcohorts %>% gsub("\r", "", .) %>% grep(".*Box:", ., value = T) %>% sub(".*Box:", "", .) %>% gsub(" ", "", .),
    experiment = sapply(strsplit(
      allcohorts %>% gsub("\r", "", .) %>% grep(".*Date:", ., value = T),
      "[_]"
    ), "[", 4) %>%
      gsub("-.*", "", .) %>%
      gsub("day ", "", .) %>%
      gsub("ction3", "ction 3", .) %>%
      gsub("Prime test", "Prime rein", .),
    filename = allcohorts %>% gsub("\r", "", .) %>% grep(".*Subject:", ., value = T) %>% str_match("MUSC_(.*?):") %>% as.data.frame() %>%
      select(V2) %>% unlist() %>% as.character()
  ) %>%
  mutate(
    subject = str_extract(subject, "\\d+") %>% as.numeric,
    subject = paste0("KAL", str_pad(subject, 3, "left", "0")),
    self_administration_room = ifelse(
      grepl("room", filename, ignore.case = T), 
      sub("Cohort \\d+?_(.*?) room.*", "\\1", filename, ignore.case = T) %>% str_trim(),
      NA
    ),
    comp = ifelse(
      grepl("comp", filename, ignore.case = T),
      sub(".*room ( )?comp(uter)? (\\d+).*", "\\3", filename) %>% str_trim(),
      NA
    ),
    roomcomp = paste0(self_administration_room, "_", comp)
  ) %>% 
  mutate_all(as.character) %>% 
  rename("date" = "enddate") %>% 
  mutate(experiment = ifelse(grepl("KAL", experiment), sub(".*\\d+_(.*)_KAL.*", "\\1", filename), experiment)) %>% 
  group_by(filename) %>% 
  mutate(row_order = row_number()) %>% 
  ungroup()

## check after running 
allcohorts_unfixed %>% mutate_all(as.factor) %>% summary

# fix KAL000 subject 0 ## XX for now, exclude from data 
# allcohorts_unfixed %>% subset(subject == "KAL000") %>% dim # identified 34
# KAL000 <- allcohorts_unfixed %>% subset(!subject %in% excel_compiled_phenotyping_metadata$internal_id | is.na(subject)) %>% # idenfied 42 (as of cohort 8)
#   left_join() %>% 
#   mutate(dbcomment = ifelse(grepl("KAL000", subject), "room and box info used to fill subject", NA)) %>% 
  

  
  
  # KAL000 <- allcohorts_unfixed %>% split(., .$cohort) %>% lapply(., function(x){
  #   x <- x %>% 
  #     arrange(roomcomp, as.numeric(box)) %>% 
  #     dplyr::filter(grepl("KAL000", subject)|lead(grepl("KAL000", subject))|lag(grepl("KAL000", subject))) %>% 
  #     mutate(dbcomment = ifelse(grepl("KAL000", subject), "room and box info used to fill subject", NA)) %>% 
  #     group_by(box) %>% mutate(subject = subject[!grepl("KAL000", subject)][1]) 
  #   # %>%  # spot checking for deaths
  #     # arrange(subject, start_date) 
#   return(x)
# }) %>% rbindlist(., idcol = "cohort") %>% 
#   subset(!is.na(dbcomment))

# goal is to reach 0 here; waiting for email confirmation about relationship bw room, box, and pc
# KAL000 %>% subset(is.na(subject)) 
# 
# allcohorts_df <- allcohorts_unfixed %>% 
#   dplyr::filter(!grepl("KAL000", subject)) %>% 
#   plyr::rbind.fill(., KAL000) %>%  # rbind with the added function of creating an NA column for nonmatching columns bw dfs A and B
#   arrange(filename, row_order)

# consider not using this bc it is hard to parse this out from extinction
# allcohorts_df_nodupes <- allcohorts_df[!duplicated(allcohorts_df), ] %>% mutate_all(as.character) # all from one file Cohort 2_L room_Extinction 6 because the sessions were run too short the first time and then regular times the second time 


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



#####  LONG ACCESS

# all infusions are mg/kg/infusion
# behavioral testing occurred 4 days/wk (sun-thurs) from 19:00 to 7:00 EST
# syringe refill calculation based on the most recent weight from the animal 
# active lever = always right (20ug/kg/infusion; infusion volume is 100 ul; fixed ratio 1)


## cohort xx : after the session ended, rats received 0.05 ml of a mixture of Heparin and 1 mg/ml Gentamicin Sulfate (antibiotic) to maintain catheter patency and animal health

allcohorts_lga_fnames_c01_09 <- allcohorts_allexp_filenames_c01_09_df %>% 
  subset(grepl("^lga$", exp, ignore.case = T)) %>% 
  select(filename) %>% unlist() %>% as.character #3070 files


readXY_lga <- function(x){
  setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/addiction_related_behaviors/MedPC_raw_data_files")
  
  subject <- fread(paste0("grep -i 'subject:' ", "'", x, "'", " | grep -Po ': \\K(w|KAL)?[0-9]*'"), header = F)
  subject <- subject %>% 
    rename("internal_id" = "V1")
  
  box <- fread(paste0("grep -i 'Box:' ", "'", x, "'"), header = F)
  box <- box %>% 
    rename("box" = "V2") %>% 
    select(-V1)
  
  startdate <- fread(paste0("grep -i 'Start Date:' ", "'", x, "'"), header = F)
  startdate <- startdate %>% 
    rename("startdate" = "V3") %>% 
    select(-V1, -V2)
  
  exp <- fread(paste0("grep -i 'Experiment:' ", "'", x, "'"), header = F)
  exp <- exp %>% unite(col = "exp", sep = "_", remove = TRUE, na.rm = FALSE)
  
  metadata <- cbind(subject, box) %>% 
    cbind(startdate) %>% 
    cbind(exp)
  
  
  ## extract data 
  # B array contains the inactive lever, active lever, and infusion information
  # S array contains the interval infusions
  Barray <- fread(paste0("grep -a1 --no-group-separator -E '(B):' ", "'", x, "'", " | grep -E '( 0):'"), header = F, fill = T)
  Barray <- Barray %>% 
    select(-c(V1, V5)) %>%
    dplyr::rename("inactive_lever" = "V2",
                  "active_lever" = "V3",
                  "infusions" = "V4")

  Sarray <- fread(paste0("awk '/S:/{flag=1}/Start Date:|U:|^$/{flag=0}flag' ", "'", x, "'"), header = F, fill = T)
  
  Sarray <- Sarray %>%
      separate(V1, into = c("V2", "V3", "V4", "V5", "V6", "V7"), sep = "[[:space:]]+") %>%
      dplyr::filter(V2=="S:"&lead(V2)=="S:"|grepl("^\\d", V2)|V2=="S:"&row_number()==n())

  lga_Sarray_indices <- grep("^[S0]:$", Sarray$V2)
  
  processedSdata_lga <- lapply(split(Sarray, cumsum(1:nrow(Sarray) %in% lga_Sarray_indices)), function(x){
    Sarray <- as.vector(t(data.matrix(x)))
    Sarray_df <- data.frame(Sarray = Sarray) %>% 
      summarize(intake_firsthour = ifelse(sum(Sarray, na.rm = T) == 0, NA, length(Sarray[which(Sarray<3600)]))) %>% 
      ungroup()
    return(Sarray_df)
  }) %>% rbindlist(fill = T) 
  
  
  data <- cbind(Barray, processedSdata_lga) 
  
  data <- cbind(metadata, data) %>% 
    mutate(filename = x)
  
  return(data)
  
  
} 


lga_raw_c01_09 <- lapply(allcohorts_lga_fnames_c01_09, readXY_lga)



# join into df and extract metadata
lga_raw_c01_09_df <- lga_raw_c01_09 %>% 
  rbindlist(fill = T) %>% 
  mutate(internal_id = str_extract(internal_id, "\\d+") %>% as.numeric,
         internal_id = paste0("KAL", str_pad(internal_id, 3, "left", "0")),
         internal_id = ifelse((grepl("NA", internal_id)|is.na(internal_id))&grepl("KAL", filename), str_extract(filename, "KAL\\d+"), internal_id), 
         cohort = paste0("C", str_pad(parse_number(str_extract(filename, "Cohort \\d+")), 2, "left", "0"))) %>% 
  mutate(filename = gsub(".*MUSC_", "", filename)) %>%  # shorten the filename %>% 
  mutate(day = str_extract(tolower(filename), "day \\d+"), 
         ext = str_extract(tolower(filename), "lga( )?\\d+")) %>% 
  mutate(day = coalesce(day, ext)) %>%
  select(-ext) %>% 
  mutate(day = parse_number(day) %>% str_pad(., 2, "left", "0") %>% as.character) %>% 
  mutate(startdate = unlist(startdate) %>% as.character() %>% gsub('([0-9]+/[0-9]+/)', '\\120', .) %>% as.POSIXct(format="%m/%d/%Y")) %>%
  mutate(room = ifelse(grepl("room", filename, ignore.case = T), str_match(filename, "[LOR]"), 
                       ifelse(grepl("room", exp, ignore.case = T), str_match(exp, "[LOR]"), NA)),
         comp = ifelse(grepl("comp", filename, ignore.case = T),
                       sub(".*comp(uter)? (\\d+).*", "\\2", filename, ignore.case = T) %>% str_trim(),
                       ifelse(grepl("comp", exp, ignore.case = T), parse_number(str_extract(tolower(exp), "comp(uter)?( |_)?\\d+")), NA))) %>% 
  mutate(compbox = ifelse(!is.na(comp), paste0(comp, ".", str_pad(box, 2, "left", "0")), box)) %>% 
  distinct()


lga_raw_c01_09_df %>% get_dupes(internal_id, day) %>% View()


## correct lab animal id's with excel
lga_raw_c01_09_df_id <- lga_raw_c01_09_df %>% 
  mutate_at(vars(matches("room|box")), as.character) %>% 
  full_join(kalivas_lga_allcohorts_excel_processed_c01_09_df %>% 
              mutate(self_administration_box = as.character(self_administration_box),
                     self_administration_box = ifelse(self_administration_room == "R"&grepl("\\d[.]1$", self_administration_box), gsub("([.])(1)$", "\\1\\20", self_administration_box), self_administration_box)) %>% 
              select(cohort, internal_id, session, self_administration_room, self_administration_box, matches("comments")), by = c("room" = "self_administration_room", "compbox" = "self_administration_box", "cohort", "day"="session")) %>% 
  mutate(internal_id.x = ifelse((is.na(internal_id.x)& !is.na(internal_id.y))|internal_id.x != internal_id.y, internal_id.y, internal_id.x)) %>% 
  distinct()

lga_raw_c01_09_df_id %>% get_dupes(internal_id.x, day) %>% View()


# clean up extra columns
# back to the df object
lga_raw_c01_09_df <- lga_raw_c01_09_df_id %>% 
  rename(internal_id = "internal_id.x") %>% 
  select(cohort, internal_id, inactive_lever, active_lever, room, compbox, startdate, filename, matches("comments"))





  
  
  
  
  
  
  
lga_allsubjects %>% naniar::vis_miss()




##  PR_test ############## xx PICK UP WHEN WE FIGURE OUT THE BOX

# behavioral testing occurred Sunday following 3 weeks of self admin training; 7 est
# mg/kg/infusion
# active lever = always right (20 ug/kg/infusion; infusion volume is 100 ul); inactive lever = always left
# terminate sessions after 12 hours, or after 1 hour of the next ratio not being achieveed  (seems like most can use start or end date bc the sessions are generally only a few hours long)



allcohorts_pr_fnames_c01_09 <- allcohorts_allexp_filenames_c01_09_df %>% 
  subset(grepl("^pr$", exp, ignore.case = T)) %>% 
  select(filename) %>% unlist() %>% as.character #202 files


readXY_pr <- function(x){
  setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/addiction_related_behaviors/MedPC_raw_data_files")
  
  subject <- fread(paste0("grep -i 'subject:' ", "'", x, "'", " | grep -Po ': \\K(w|KAL)?[0-9]*'"), header = F)
  subject <- subject %>% 
    rename("internal_id" = "V1")
  
  box <- fread(paste0("grep -i 'Box:' ", "'", x, "'"), header = F)
  box <- box %>% 
    rename("box" = "V2") %>% 
    select(-V1)
  
  startdate <- fread(paste0("grep -i 'Start Date:' ", "'", x, "'"), header = F)
  startdate <- startdate %>% 
    rename("startdate" = "V3") %>% 
    select(-V1, -V2)
  
  exp <- fread(paste0("grep -i 'Experiment:' ", "'", x, "'"), header = F)
  exp <- exp %>% unite(col = "exp", sep = "_", remove = TRUE, na.rm = FALSE)
  
  metadata <- cbind(subject, box) %>% 
    cbind(startdate) %>% 
    cbind(exp)
  
  
  ## extract data 
  # P array contains the break points and the O value contains the PR step at which the rat terminated at to give PR_step;
  # M contains the totaL_session_minutes; and B array contains  inactive lever, active lever, infusion, and current ratio information
    
  Barray <- fread(paste0("grep -a1 --no-group-separator -E '(B):' ", "'", x, "'", " | grep -E '( 0):'"), header = F, fill = T)
  Barray <- Barray %>% 
    select(-c(V1, V6)) %>%
    dplyr::rename("inactive_lever" = "V2",
                  "active_lever" = "V3",
                  "infusions" = "V4",
                  "current_ratio" = "V5")

    M <- fread(paste0("grep --no-group-separator -E 'M:' ", "'", x, "'", " | grep -E 'M:'"), header = F, fill = T) %>% 
      select(-c(V1)) %>% 
      dplyr::rename("total_session_minutes" = "V2")
    
    Parray <- c(0, 1, 2, 4, 6, 9, 12, 15, 20, 25, 32, 40, 50, 62, 77, 95, 118, 145, 178, 219, 268, 328, 402, 492, 603, 737, 901, 1102, 1347, 1646, 2012, 2459, 3004)
    O <- fread(paste0("grep --no-group-separator -E 'O:' ", "'", x, "'", " | grep -E 'O:'"), header = F, fill = T) %>% 
      select(-c(V1)) %>% 
      dplyr::rename("value" = "V2") %>% 
      mutate(value = value + 1) # prevent the 0 digit issue
  
    O$pr_step <- Parray[O$value]
  
  
  data <- cbind(Barray, M) %>% 
    cbind(O)
  
  data <- cbind(metadata, data) %>% 
    mutate(filename = x)
  
  return(data)
  
  
} 


pr_raw_c01_09 <- lapply(allcohorts_pr_fnames_c01_09, readXY_pr)

pr_raw_c01_09_df <- pr_raw_c01_09 %>% 
  rbindlist(fill = T) %>% 
  mutate(internal_id = str_extract(internal_id, "\\d+") %>% as.numeric,
         internal_id = paste0("KAL", str_pad(internal_id, 3, "left", "0")),
         cohort = paste0("C", str_pad(parse_number(str_extract(filename, "Cohort \\d+")), 2, "left", "0"))) %>% 
  mutate(filename = gsub(".*MUSC_", "", filename)) %>%  # shorten the filename
  mutate(startdate = unlist(startdate) %>% as.character %>% gsub('([0-9]+/[0-9]+/)', '\\120', .) %>% as.POSIXct(format="%m/%d/%Y")) %>% 
  mutate(room = ifelse(grepl("room", filename, ignore.case = T), str_match(filename, "[LOR]"), 
                       ifelse(grepl("room", exp, ignore.case = T), str_match(exp, "[LOR]"), NA)),
         comp = ifelse(grepl("comp", filename, ignore.case = T),
                       sub(".*comp(uter)? (\\d+).*", "\\2", filename, ignore.case = T) %>% str_trim(),
                       ifelse(grepl("comp", exp, ignore.case = T), parse_number(str_extract(tolower(exp), "comp(uter)?( |_)?\\d+")), NA))) %>% 
  mutate(compbox = ifelse(!is.na(comp), paste0(comp, ".", str_pad(box, 2, "left", "0")), box)) %>% 
  distinct()


# deal with dupes 
pr_raw_c01_09_df %>% get_dupes(internal_id)








# *****************
##  Extinction_prime_test (Is it primed reinstatement?)


# the day after 3 days of heroin re-establishment (lga 13-15) from 1900 to 1est
# no syringes
# sessions terminate after 6 hours
# active lever (right lever) no longer results in heroin infusion, presentation of cue light, or the time out period
# first 4 hours is extinction training; 2 hours left = receive injection of 0.25 mg/kg of heroin subcutaneous (heroin prime) XX ASKING OKSANA IS THIS SEEMS STANDARD
# assess how the drug being "on board" affects drug-seeking behavior



allcohorts_expr_fnames_c01_09 <- allcohorts_allexp_filenames_c01_09_df %>% 
  subset(grepl("p_rein", exp, ignore.case = T)) %>% 
  select(filename) %>% unlist() %>% as.character #212 files


readXY_expr <- function(x){
  setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/addiction_related_behaviors/MedPC_raw_data_files")
  
  subject <- fread(paste0("grep -i 'subject:' ", "'", x, "'", " | grep -Po ': \\K(w|KAL)?[0-9]*'"), header = F)
  subject <- subject %>% 
    rename("internal_id" = "V1")
  
  box <- fread(paste0("grep -i 'Box:' ", "'", x, "'"), header = F)
  box <- box %>% 
    rename("box" = "V2") %>% 
    select(-V1)
  
  startdate <- fread(paste0("grep -i 'Start Date:' ", "'", x, "'"), header = F)
  startdate <- startdate %>% 
    rename("startdate" = "V3") %>% 
    select(-V1, -V2)
  
  exp <- fread(paste0("grep -i 'Experiment:' ", "'", x, "'"), header = F)
  exp <- exp %>% unite(col = "exp", sep = "_", remove = TRUE, na.rm = FALSE)
  
  metadata <- cbind(subject, box) %>% 
    cbind(startdate) %>% 
    cbind(exp)
  
  # get A (inactive) lever presses ## XX Q = inactive, R = active , S = infusions
  Aarray <- fread(paste0("awk '/A:/{flag=1;next}/B:/{flag=0}flag' ", "'", x, "'"), header = F, fill = T)
      
  # get D (inactive) lever presses 
  Darray <- fread(paste0("awk '/D:/{flag=1;next}/E/{flag=0}flag' ", "'", x, "'"), header = F, fill = T)
  
  
  expr_Aarray <- function(x){
    indices <- grep("^0:$", x$V1)
    x_split <- split(x, cumsum(1:nrow(x) %in% indices))
    
    data_list <- lapply(x_split, function(x){
      indexremoved <- x %>% select(-V1)
      array <- as.vector(t(data.matrix(indexremoved)))
      array <- array[!is.na(array)]
      array <- colSums(matrix(array, nrow=2)) # take the sumes of every other row by forming a matrix and taking the column sums
        
      data <- data.frame(leverpresses = array[1:6], time_bin = paste0("hour_", paste0(1:6), "_inactive")) %>%
        spread(time_bin, leverpresses)
      
      return(data)
    })  %>% rbindlist()
    return(data_list)
  }
  
  Aarray <- expr_Aarray(Aarray)
  
  expr_Darray <- function(x){
    indices <- grep("^0:$", x$V1)
    x_split <- split(x, cumsum(1:nrow(x) %in% indices))
    
    data_list <- lapply(x_split, function(x){
      indexremoved <- x %>% select(-V1)
      array <- as.vector(t(data.matrix(indexremoved)))
      array <- array[!is.na(array)]
      array <- colSums(matrix(array, nrow=2)) # take the sumes of every other row by forming a matrix and taking the column sums
      
      data <- data.frame(leverpresses = array[1:6], time_bin = paste0("hour_", paste0(1:6), "_active")) %>%
        spread(time_bin, leverpresses)
      
      return(data)
    })  %>% rbindlist()
    return(data_list)
  }
  
  Darray <- expr_Darray(Darray)
  
  
  data <- cbind(Aarray, Darray)
  
  data <- cbind(metadata, data) %>% 
    mutate(filename = x)
  
  return(data)
  
  
} 


test_171 <- allcohorts_expr_fnames_c01_09 %>% grep("Cohort 5", ., value = T) %>% grep("KAL171", ., value = T)
readXY_expr(test_171)

expr_raw_c01_09 <- lapply(allcohorts_expr_fnames_c01_09, readXY_expr)

expr_raw_c01_09_df <- expr_raw_c01_09 %>% 
  rbindlist(fill = T) %>% 
  mutate(internal_id = str_extract(internal_id, "\\d+") %>% as.numeric,
         internal_id = paste0("KAL", str_pad(internal_id, 3, "left", "0")),
         cohort = paste0("C", str_pad(parse_number(str_extract(filename, "Cohort \\d+")), 2, "left", "0"))) %>% 
  mutate(filename = gsub(".*MUSC_", "", filename)) %>%  # shorten the filename
  mutate(startdate = unlist(startdate) %>% as.character %>% gsub('([0-9]+/[0-9]+/)', '\\120', .) %>% as.POSIXct(format="%m/%d/%Y")) %>% 
  mutate(room = ifelse(grepl("room", filename, ignore.case = T), str_match(filename, "[LOR]"), 
                       ifelse(grepl("room", exp, ignore.case = T), str_match(exp, "[LOR]"), NA)),
         comp = ifelse(grepl("comp", filename, ignore.case = T),
                       sub(".*comp(uter)? (\\d+).*", "\\2", filename, ignore.case = T) %>% str_trim(),
                       ifelse(grepl("comp", exp, ignore.case = T), parse_number(str_extract(tolower(exp), "comp(uter)?( |_)?\\d+")), NA))) %>% 
  mutate(compbox = ifelse(!is.na(comp), paste0(comp, ".", str_pad(box, 2, "left", "0")), box)) %>% 
  distinct()

# clean up dupe id's 
expr_raw_c01_09_df %>% get_dupes(internal_id)

expr_raw_c01_09_df <- expr_raw_c01_09_df %>% 
  subset(!grepl("[/]Prime Extinction", filename)) # remove dupes
  
# clean up columns
expr_raw_c01_09_df <- expr_raw_c01_09_df  %>% 
  select(cohort, internal_id, matches("hour"), room, compbox, startdate, filename)
  
  
  
  
  
  
  
  
  
# *****************
##  Extinction

# start the day after prime reinstatement test; occurs for 6 straight days from 1900 to 2100est (friday-wednesday)
# syringes were not needed 
# food, water bottle, and metal toy were not placed in cage
# session runs for 2 hours

allcohorts_ex_fnames_c01_09 <- allcohorts_allexp_filenames_c01_09_df %>% 
  subset(grepl("extinction", exp, ignore.case = T)) %>% 
  select(filename) %>% unlist() %>% as.character #1191 files


readXY_ext <- function(x){
  setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/addiction_related_behaviors/MedPC_raw_data_files")
  
  subject <- fread(paste0("grep -i 'subject:' ", "'", x, "'", " | grep -Po ': \\K(w|KAL)?[0-9]*'"), header = F)
  subject <- subject %>% 
    rename("internal_id" = "V1")
  
  box <- fread(paste0("grep -i 'Box:' ", "'", x, "'"), header = F)
  box <- box %>% 
    rename("box" = "V2") %>% 
    select(-V1)
  
  startdate <- fread(paste0("grep -i 'Start Date:' ", "'", x, "'"), header = F)
  startdate <- startdate %>% 
    rename("startdate" = "V3") %>% 
    select(-V1, -V2)
  
  exp <- fread(paste0("grep -i 'Experiment:' ", "'", x, "'"), header = F)
  exp <- exp %>% unite(col = "exp", sep = "_", remove = TRUE, na.rm = FALSE)
  
  metadata <- cbind(subject, box) %>% 
    cbind(startdate) %>% 
    cbind(exp)
  
  
  Warray <- fread(paste0("grep -iE '^(W):' ", "'", x, "'"), header = F, fill = T)
  Warray <- Warray %>% 
    rename("inactive_lever" = "V2") %>% 
    select(-V1)
  
  Xarray <- fread(paste0("grep -iE '^(X):' ", "'", x, "'"), header = F, fill = T)
  Xarray <- Xarray %>% 
    rename("active_lever" = "V2") %>% 
    select(-V1)
  
  data <- cbind(Warray, Xarray)
  
  data <- cbind(metadata, data) %>% 
    mutate(filename = x)
  
  return(data)
  
  
} 


ext_raw_c01_09 <- lapply(allcohorts_ex_fnames_c01_09, readXY_ext)

ext_raw_c01_09_df <- ext_raw_c01_09 %>% 
  rbindlist(fill = T) %>% 
  mutate(internal_id = str_extract(internal_id, "\\d+") %>% as.numeric,
         internal_id = paste0("KAL", str_pad(internal_id, 3, "left", "0")),
         cohort = paste0("C", str_pad(parse_number(str_extract(filename, "Cohort \\d+")), 2, "left", "0"))) %>% 
  mutate(day = str_extract(filename, "Day \\d+"), 
         ext = str_extract(filename, "Extinction( )?\\d+")) %>% 
  mutate(day = coalesce(day, ext)) %>%
  select(-ext) %>% 
  mutate(day = parse_number(day)) %>% 
  mutate(filename = gsub(".*MUSC_", "", filename)) %>%  # shorten the filename
  mutate(startdate = unlist(startdate) %>% as.character %>% gsub('([0-9]+/[0-9]+/)', '\\120', .) %>% as.POSIXct(format="%m/%d/%Y")) %>% 
  mutate(room = ifelse(grepl("room", filename, ignore.case = T), str_match(filename, "[LOR]"), 
    ifelse(grepl("room", exp, ignore.case = T), str_match(exp, "[LOR]"), NA)),
    room = ifelse(is.na(room)&cohort == "C09", str_match(toupper(exp), "_[LOR]_") %>% gsub("_", "", .), room), 
    comp = ifelse(grepl("comp", filename, ignore.case = T),
    sub(".*comp(uter)? (\\d+).*", "\\2", filename, ignore.case = T) %>% str_trim(),
    ifelse(grepl("comp", exp, ignore.case = T), parse_number(str_extract(tolower(exp), "comp(uter)?( |_)?\\d+")), NA))) %>% 
  mutate(compbox = ifelse(!is.na(comp), paste0(comp, ".", str_pad(box, 2, "left", "0")), box)) %>% 
  distinct()

ext_raw_c01_09_df %>% get_dupes(internal_id, day) %>% dim

## correct lab animal id's with excel
ext_raw_c01_09_df_id <- ext_raw_c01_09_df %>% 
  mutate_at(vars(matches("room|box")), as.character) %>% 
  full_join(kalivas_ex_allcohorts_excel_processed_c01_09_df_wide %>% 
              mutate(self_administration_box = as.character(self_administration_box),
                     self_administration_box = ifelse(self_administration_room == "R"&grepl("\\d[.]1$", self_administration_box), gsub("([.])(1)$", "\\1\\20", self_administration_box), self_administration_box)) %>% 
              select(cohort, internal_id, self_administration_room, self_administration_box, matches("comments")), by = c("room" = "self_administration_room", "compbox" = "self_administration_box", "cohort")) %>% 
  mutate(internal_id.x = ifelse((is.na(internal_id.x)& !is.na(internal_id.y))|internal_id.x != internal_id.y&!(grepl("box", comments_2)|grepl("box", comments_3)), internal_id.y, internal_id.x))

ext_raw_c01_09_df_id <- ext_raw_c01_09_df_id %>%
  add_count(internal_id.x, day) %>% 
  subset(!(inactive_lever == 0 & active_lever == 0 & n != 1 & !is.na(internal_id.x))) %>%  
  subset(!(room == "R"&grepl("^1[.]0[1-4]", compbox)))
# get_dupes(internal_id.x, day) %>% dim

# clean up extra columns
# back to the df object
ext_raw_c01_09_df <- ext_raw_c01_09_df_id %>% 
  rename(internal_id = "internal_id.x") %>% 
  select(cohort, internal_id, day, inactive_lever, active_lever, room, compbox, startdate, filename, matches("comments"))

ext_raw_c01_09_df_wide <- ext_raw_c01_09_df %>% 
  select(-filename, -compbox, -matches("comments")) %>% 
  subset(!is.na(day)) %>% 
  # mutate(compbox = ifelse(grepl("box", comments_2), parse_number(str_extract(comments_2, "box.*\\d")), 
  #                         ifelse(grepl("box", comments_3), parse_number(str_extract(comments_3, "box.*\\d")), compbox))) %>%
  pivot_wider(
    names_from = day,
    names_sep = "_",
    # names_glue = "{value}_{day}",
    values_from = c( inactive_lever, active_lever, startdate)
  ) %>% 
  left_join(kalivas_ex_allcohorts_excel_processed_c01_09_df_wide %>% 
              mutate(self_administration_box = ifelse(self_administration_room == "R"&grepl("\\d[.]1$", self_administration_box), gsub("([.])(1)$", "\\1\\20", self_administration_box), self_administration_box)) %>% 
              select(internal_id, cohort, self_administration_box) %>%
              rename("compbox" = "self_administration_box"), 
            by = c("internal_id", "cohort"))






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


allcohorts_rein_fnames_c01_09 <-allcohorts_allexp_filenames_c01_09_df %>% 
  subset(grepl("cued reinstatement", exp, ignore.case = T)) %>% 
           select(filename) %>% unlist() %>% as.character #161 files


readXY_cued <- function(x){
  setwd("~/Dropbox (Palmer Lab)/Peter_Kalivas_U01/addiction_related_behaviors/MedPC_raw_data_files")
  
  subject <- fread(paste0("grep -i 'subject:' ", "'", x, "'", " | grep -Po ': \\K(w|KAL)?[0-9]*'"), header = F)
  subject <- subject %>% 
    rename("internal_id" = "V1")
  
  box <- fread(paste0("grep -i 'Box:' ", "'", x, "'"), header = F)
  box <- box %>% 
    rename("box" = "V2") %>% 
    select(-V1)
  
  startdate <- fread(paste0("grep -i 'Start Date:' ", "'", x, "'"), header = F)
  startdate <- startdate %>% 
    rename("startdate" = "V3") %>% 
    select(-V1, -V2)
  
  
  exp <- fread(paste0("grep -i 'Experiment:' ", "'", x, "'"), header = F)
  exp <- exp %>% unite(col = "exp", sep = "_", remove = TRUE, na.rm = FALSE)
  
  metadata <- cbind(subject, box) %>% 
    cbind(startdate) %>% 
    cbind(exp)
  
  Warray <- fread(paste0("grep -iE '^(W):' ", "'", x, "'"), header = F, fill = T)
  Warray <- Warray %>% 
    rename("inactive_lever" = "V2") %>% 
    select(-V1)
  
  Xarray <- fread(paste0("grep -iE '^(X):' ", "'", x, "'"), header = F, fill = T)
  Xarray <- Xarray %>% 
    rename("active_lever" = "V2") %>% 
    select(-V1)
  
  data <- cbind(Warray, Xarray)
  
  data <- cbind(metadata, data) %>% 
    mutate(filename = x)
  
  return(data)
  
  
} 


cued_rein_raw_c01_09 <- lapply(allcohorts_rein_fnames_c01_09, readXY_cued)

# join into df and extract metadata
cued_rein_raw_c01_09_df <- cued_rein_raw_c01_09 %>% 
  rbindlist(fill = T) %>% 
  mutate(internal_id = str_extract(internal_id, "\\d+") %>% as.numeric,
         internal_id = paste0("KAL", str_pad(internal_id, 3, "left", "0")),
         internal_id = ifelse((grepl("NA", internal_id)|is.na(internal_id))&grepl("KAL", filename), str_extract(filename, "KAL\\d+"), internal_id), 
         cohort = paste0("C", str_pad(parse_number(str_extract(filename, "Cohort \\d+")), 2, "left", "0"))) %>% 
  mutate(filename = gsub(".*MUSC_", "", filename)) %>%  # shorten the filename
  mutate(startdate = unlist(startdate) %>% as.character() %>% gsub('([0-9]+/[0-9]+/)', '\\120', .) %>% as.POSIXct(format="%m/%d/%Y")) %>%
  mutate(room = ifelse(grepl("room", filename, ignore.case = T), str_match(filename, "[LOR]"), 
                       ifelse(grepl("room", exp, ignore.case = T), str_match(exp, "[LOR]"), NA)),
         room = ifelse(is.na(room)&cohort == "C09", str_match(toupper(exp), "_[LOR]_") %>% gsub("_", "", .), room), 
         comp = ifelse(grepl("comp", filename, ignore.case = T),
                       sub(".*comp(uter)? (\\d+).*", "\\2", filename, ignore.case = T) %>% str_trim(),
                       ifelse(grepl("comp", exp, ignore.case = T), parse_number(str_extract(tolower(exp), "comp(uter)?( |_)?\\d+")), NA))) %>% 
  mutate(compbox = ifelse(!is.na(comp), paste0(comp, ".", str_pad(box, 2, "left", "0")), box)) %>% 
  distinct()



cued_rein_raw_c01_09_df %>% get_dupes(internal_id) %>% dim

cued_rein_raw_c01_09_df <- cued_rein_raw_c01_09_df %>% 
  subset(filename != "./Cohort 9/cue reinstatement/2021-02-25_08h06m_Subject KAL349.txt") # README file, animal was sick and rerun

## correct lab animal id's with excel ( not needed )
# cued_rein_raw_c01_09_df_id <- cued_rein_raw_c01_09_df %>% 
#   mutate_at(vars(matches("room|box")), as.character) %>% 
#   full_join(kalivas_cued_allcohorts_excel_processed_c01_09_df %>% 
#               mutate(self_administration_box = as.character(self_administration_box),
#                      self_administration_box = ifelse(self_administration_room == "R"&grepl("\\d[.]1$", self_administration_box), gsub("([.])(1)$", "\\1\\20", self_administration_box), self_administration_box)) %>% 
#               select(cohort, internal_id, self_administration_room, self_administration_box, matches("comments")), by = c("room" = "self_administration_room", "compbox" = "self_administration_box", "cohort")) %>% 
#   mutate(internal_id.x = ifelse((is.na(internal_id.x)& !is.na(internal_id.y))|internal_id.x != internal_id.y, internal_id.y, internal_id.x))

# clean up extra columns
# back to the df object
cued_rein_raw_c01_09_df <- cued_rein_raw_c01_09_df %>% 
  select(cohort, internal_id, inactive_lever, active_lever, room, compbox, startdate, filename, matches("comments"))














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
