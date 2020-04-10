## CREATE EXCEL FOR KALIVAS ITALY GROUP



# Apurva TO DO LIST 
# Italy data
# go through the excel files
# check trait distribution check for experimental differences:
#   different session lengths
# different trait names
# how many traits collected?



## check if the order is the same as the protocol for kalivas in us
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


# Received 4/3/2020 from Palmer from Nazzareno

# assign the category of varnames 
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Peter_Kalivas_U01DA045300")
Italy_excel_C01_05 <- u01.importxlsx("U01_UNICAM_data summary.xlsx")[[2]]
names(Italy_excel_C01_05) <- Italy_excel_C01_05[1,] %>% unlist() %>% as.character() %>% 
  as.data.frame() %>% rename("names" = ".") %>% 
  fill(names) %>% unlist() %>% as.character() %>% make_clean_names()

Italy_excel_C01_05 <- Italy_excel_C01_05[-1, ] 

# function to use for all exps below
xl_to_long_df <- function(x){
  names(x) <- x[1,] %>% make_clean_names()
  x <- x[-1,] %>% 
    gather(var, value, -transponder_number, -animal_id_given_by_breeder, -animal_id_given_by_behav_unit, -sex, -coat_color, -heroin_saline_yoked, -loco_index, -cohort) %>% 
    extract(var, c("measurement", "session"), "(.*)(\\d)") %>% 
    mutate(session = ifelse(session == 1, "before_SA", "after_SA"),
           value = format(round(as.numeric(value), 2), nsmall = 2),
           value = as.numeric(value),
           cohort = str_pad(parse_number(cohort), 2, "left", "0"))
  return(x)
}


# ############################
# # Exp 1: ELEVATED PLUS MAZE
# ############################
Italy_epm_C01_05_xl <- Italy_excel_C01_05 %>% 
  select(matches("identity|elevated"))
Italy_epm_C01_05_xl <- xl_to_long_df(Italy_epm_C01_05_xl)

# ############################
# # Exp 2: OPEN FIELD TASK
# ############################

Italy_oft_C01_05_xl <- Italy_excel_C01_05 %>% 
  select(matches("identity|open"))
Italy_oft_C01_05_xl <- xl_to_long_df(Italy_oft_C01_05_xl)

# ############################
# # Exp 3: TAIL FLICK
# ############################

Italy_tailflick_C01_05_xl <- Italy_excel_C01_05 %>% 
  select(matches("identity|tail_flick"))
Italy_tailflick_C01_05_xl <- xl_to_long_df(Italy_tailflick_C01_05_xl)

# ############################
# # Exp 4: Addiction related 
# ############################

#####  LONG ACCESS & PROGRESSIVE RATIO
Italy_lgapr_C01_05_xl <- Italy_excel_C01_05 %>% 
  select(matches("identity|self_admin"))
Italy_lgapr_C01_05_xl <- xl_to_long_df(Italy_lgapr_C01_05_xl)


#####  EXTINCTION PRIME XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

## figure how which hours maps to which? 
Italy_expr_C01_05_xl <- Italy_excel_C01_05 %>% 
  select(matches("identity|extinction"))
Italy_expr_C01_05_xl <- xl_to_long_df(Italy_expr_C01_05_xl)


#####  EXTINCTION XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

## figure how which hours maps to which? 
Italy_expr_C01_05_xl <- Italy_excel_C01_05 %>% 
  select(matches("identity|extinction"))
Italy_expr_C01_05_xl <- xl_to_long_df(Italy_expr_C01_05_xl)

#####  CUED REINSTATEMENT

# extinction de-escalation =(active lever Day 6) - (active lever Day 1)
## figure how which hours maps to which? 
Italy_cuedrein_C01_05_xl <- Italy_excel_C01_05 %>% 
  select(matches("identity|reinstatement"))
Italy_cuedrein_C01_05_xl <- xl_to_long_df(Italy_cuedrein_C01_05_xl)




