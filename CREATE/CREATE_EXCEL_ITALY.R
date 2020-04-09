## CREATE EXCEL FOR KALIVAS ITALY GROUP



# Apurva TO DO LIST 
# Italy data
# go through the excel files
# check trait distribution check for experimental differences:
#   different session lengths
# different trait names
# how many traits collected?
  

# Received 4/3/2020 from Palmer from Nazzareno

# assign the category of varnames 
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300")
Italy_excel_C01_05 <- u01.importxlsx("U01_UNICAM_data summary.xlsx")[[2]]
names(Italy_excel_C01_05) <- Italy_excel_C01_05[1,] %>% unlist() %>% as.character() %>% 
  as.data.frame() %>% rename("names" = ".") %>% 
  fill(names) %>% unlist() %>% as.character() %>% make_clean_names()

Italy_excel_C01_05 <- Italy_excel_C01_05[-1, ] 

# ############################
# # Exp 1: ELEVATED PLUS MAZE
# ############################
Italy_epm_C01_05_xl <- Italy_excel_C01_05 %>% 
  select(matches("identity|elevated"))
names(Italy_epm_C01_05_xl) <- Italy_epm_C01_05_xl[1, ] %>% make_clean_names()
Italy_epm_C01_05_xl <- Italy_epm_C01_05_xl[-1,] %>% 
  gather(var, value, -transponder_number, -animal_id_given_by_breeder, -animal_id_given_by_behav_unit, -sex, -coat_color, -heroin_saline_yoked, -loco_index, -cohort) %>% 
  extract(var, c("measurement", "session"), "(.*)(\\d)") %>% 
  mutate(session = ifelse(session == 1, "before_SA", "after_SA"),
         value = format(round(as.numeric(value), 2), nsmall = 2),
         cohort = str_pad(parse_number(cohort), 2, "left", "0"))


# ############################
# # Exp 2: OPEN FIELD TASK
# ############################