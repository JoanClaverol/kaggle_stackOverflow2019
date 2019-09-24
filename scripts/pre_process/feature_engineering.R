# -------------------------------------------------------------------------

# -------------------------------------------------------------------------


# libraries ---------------------------------------------------------------
if (require(pacman) == FALSE) {
  install.packages("pacman")
}
pacman::p_load(readr, dplyr, fastDummies, stringr, tidyr, magrittr)

# import data -------------------------------------------------------------
data <- read_csv(
  "data/raw_data/stack-overflow-2018-developer-survey/survey_results_data.csv"
  )

# pre process -------------------------------------------------------------

# find programming languages used by every responder
temp <- data %>% 
  select(
    "DevType", starts_with("Lang"), CommunicationTools, EducationTypes,
    SelfTaughtTypes, starts_with("Database"), starts_with("Platform"),
    starts_with("Framework"),IDE,Methodology,VersionControl, Gender
    ) 

csv2_variables <- names(temp)
for (i in 1:length(csv2_variables)) {
  # select relevant variables, split info. based on ";" and unnest extraction
  temp_df <- data %>%
    select(Respondent, temp_rename = csv2_variables[i]) %>% 
    mutate(
      temp_name_split = str_split(
        string = temp_rename,
        pattern = ";"
        )
      ) %>% 
    unnest(temp_name_split) %>%
    select(-temp_rename)
  # change the names of the dataframe
  names(temp_df) <- c("Respondent", csv2_variables[i])
  # dummify, extract repeated obs., and join the daa with data dataset
  data <- temp_df %>% 
    dummy_columns(csv2_variables[i], ignore_na = T,
                  remove_most_frequent_dummy = F, remove_first_dummy = F)  %>%
    distinct(Respondent, .keep_all = T) %>%
    right_join(y = data, by = 'Respondent')
  
}

# save data ---------------------------------------------------------------
write_rds(data, path = "data/clean_data/data_splited.rds")


