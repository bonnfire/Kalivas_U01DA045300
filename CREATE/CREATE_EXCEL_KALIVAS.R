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
openfieldtask_files_excel <- list.files(path = ".", pattern = "*.xlsx", full.names = T, recursive = T) 
openfieldtask_files_excel <- grep("cohort", openfieldtask_files_excel, ignore.case = T, value = T) # ignore the other xlsx file

# for(i in 1:length(openfieldtask_files)){
#     assign(gsub("\\.xlsx", "", sapply(strsplit(openfieldtask_files_excel, "/"), "[", 3))[i], u01.importxlsx(openfieldtask_files[i]))
# } # creates multiple objects for every excel file

ls()[sapply(ls(), function(i) class(get(i))) == "list"] # use this vector of these objects to make them into df's to be bound together in to a data frame cohort 2

openfieldtask_excel_df <- list()

# create df with all excel values
for(i in 1:length(openfieldtask_files_excel)){
  openfieldtask_excel_df <- lapply(sapply(strsplit(openfieldtask_files_excel, "/"), "[", 3), u01.importxlsx)

}

sapply(strsplit(openfieldtask_files_excel, "/"), "[", 3)
openfieldtask_excel_list <- lapply(openfieldtask_files_excel, read_excel)
# since sapply(openfieldtask_excel_df, length) is all 1's, we won't need to use the u01.importxlsx function; sapply(openfieldtask_excel_list, dim)[2,] %>% summary shows that all dataframes have 46 vars
names(openfieldtask_excel_list) <- gsub("\\.xlsx", "", openfieldtask_files_excel)

# test for one case, eventually use this to create the cohort objects above 
# change the variable names # extract the data and create the summary as a separate list item
names(cohort02_group1_OF1$C2Group1OF1_C2G1OF1) <- ifelse(grepl("^[[:alpha:]].*\\d$", names(cohort02_group1_OF1$C2Group1OF1_C2G1OF1)), gsub("[.].*", "", names(cohort02_group1_OF1$C2Group1OF1_C2G1OF1)), names(cohort02_group1_OF1$C2Group1OF1_C2G1OF1)) 

names(cohort02_group1_OF1$C2Group1OF1_C2G1OF1) <- mgsub::mgsub(names(cohort02_group1_OF1$C2Group1OF1_C2G1OF1),c("-| "), c("_")) %>% 
  tolower() %>% 
  make.unique(sep = ".")
  
cohort02_group1_OF1_test <- list()
cohort02_group1_OF1_test[[1]] <- cohort02_group1_OF1$C2Group1OF1_C2G1OF1 %>% 
                           select(ends_with(".1")) %>% # creates the "total" table
                            dplyr::filter(complete.cases(.)) 
cohort02_group1_OF1_test[[2]] <- cohort02_group1_OF1$C2Group1OF1_C2G1OF1 %>% 
  select(-matches("\\d$")) %>% 
  mutate(date = as.Date(date, format='%d-%b-%Y') ,
         time = chron::chron(times = time)) 
  
naniar::vis_miss(cohort02_group1_OF1_test[[2]]) # rather than visualizing the graph every time, # consider coding this ifelse in the future: return a comment if the percentages diff

cohort02_group1_OF1_test[[2]] <- cohort02_group1_OF1$C2Group1OF1_C2G1OF1 %>% 
  select(-matches("\\d$")) %>%  # matches is the only select helper that allows for regular expressions
  dplyr::filter(complete.cases(.)) %>% 
  select(subject_id, everything()) %>%  # reorder the columns 
  mutate(date = as.Date(date, format='%d-%b-%Y') ,
         time = chron::chron(times = time))

nrow(cohort02_group1_OF1_test[[2]]) == n_distinct(cohort02_group1_OF1_test[[2]]$subject_id)
# check the number of unique id matches expected values.



  
############################
# Exp 3: TAIL FLICK
############################


