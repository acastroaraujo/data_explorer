
library(tidyverse)
library(haven)

setwd(here::here("01-explorer/"))

# helper functions --------------------------------------------------------

replace_dn <- function(x) {
  stopifnot(class(x) == "factor")
  levels(x) <- stringr::str_replace_all(levels(x), c("DonÂ´t know" = "Don't know"))
  return(x)
}

# data --------------------------------------------------------------------

raw <- readr::read_rds("data-scripts/raw-wvs.rds")

wt_vars <- c(
  "WEIGHT" = "W_WEIGHT"
)

con_vars <- c(
  "Q262", "Q274"
)

cat_vars <- c(
  "B_COUNTRY", 
  "Q_MODE", 
  "H_SETTLEMENT", 
  "H_URBRURAL",
  "Q260", 
  "X003R",
  "Q269", 
  "Q273", 
  "Q275",
  "Q276",
  "Q278",
  "Q279",
  "Q280",
  "Q287",
  "Q178",
  "Q180",
  "Q182",
  "Q183",
  "Q184",
  "Q185",
  "Q186",
  "Q189",
  "Q190",
  "Q193",
  "Q195"
)

rename_vars <- c(
  "age" = "Q262",
  "COUNTRY" = "B_COUNTRY", 
  "Q_MODE", 
  "H_SETTLEMENT", 
  "H_URBRURAL",
  "sex" = "Q260", 
  "age_recoded" = "X003R",
  "citizen" = "Q269", 
  "marital_status" = "Q273", 
  "n_children" = "Q274", 
  "educ" = "Q275",
  "educ_spouse" = "Q276",
  "educ_father" = "Q278",
  "employment" = "Q279",
  "employment_spouse" = "Q280",
  "social_class_subj" = "Q287",
  "justifiable_fare" = "Q178",
  "justifiable_taxes" = "Q180",
  "justifiable_homosexuality" = "Q182",
  "justifiable_prostitution" = "Q183",
  "justifiable_abortion" = "Q184",
  "justifiable_divorce" = "Q185",
  "justifiable_sex_marriage" = "Q186",
  "justifiable_man_beat_wife" = "Q189",
  "justifiable_parent_beat_child" = "Q190",
  "justifiable_casual_sex" = "Q193",
  "justifiable_death_penalty" = "Q195",
  "WEIGHT" = "W_WEIGHT"
)

wvs <- raw |> 
  mutate(across(everything(), haven::zap_missing)) |> 
  mutate(across(all_of(cat_vars), haven::as_factor)) |> ## this needs to change
  select(all_of(rename_vars)) |>
  mutate(across(where(is.factor), replace_dn)) |> 
  filter(!is.na(age))

write_rds(wvs, "datasets/wvs.rds", compress = "gz") 


# Dictionary --------------------------------------------------------------

wvs_dict <- c(
  "age_recoded" = "Age recoded (6 intervals)",
  "age" = "Age",
  "H_SETTLEMENT" = "Settlement type",
  "H_URBRURAL" = "Urban-Rural",
  "sex" = "Respondent’s sex",
  "citizen" = "Respondent citizen",
  "marital_status" = "Marital status",
  "n_children" = "Have you had children? If yes, how many?",
  "educ" = "What is the highest educational level that you have attained?",
  "educ_spouse" = "What is the highest educational level that your spouse has attained?",
  "educ_father" = "What is the highest educational level that your father has attained?",
  "employment" = "Employment status"                           ,
  "employment_spouse" = "Employment status - Respondent's Spouse",
  "social_class_subj" = "Social class (subjective)\nPeople sometimes describe themselves as belonging to the working class, the middle class, or the upper or lower class. Would you describe yourself as belonging to the...",
  "justifiable_fare" = "Justifiable: Avoiding a fare on public tran",
  "justifiable_taxes" = "Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card.\nCheating on taxes if you have a chance",
  "justifiable_homosexuality" = "Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card.\nHomosexuality",
  "justifiable_prostitution" = "Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card.\nProstitution",
  "justifiable_abortion" = "Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card.\nAbortion",
  "justifiable_divorce" = "Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card.\nDivorce",
  "justifiable_sex_marriage" = "Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card.\nSex before marriage",
  "justifiable_man_beat_wife" = "Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card.\nFor a man to beat his wife",
  "justifiable_parent_beat_child" = "Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card.\nParents beating children",
  "justifiable_casual_sex" = "Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card.\nHaving casual sex",
  "justifiable_death_penalty" = "Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card.\nDeath penalty",
  "Q_MODE" = "Mode of data collection",
  "COUNTRY" = "ISO 3166-1 numeric country code"
)

write_rds(wvs_dict, "datasets/wvs_dict.rds", compress = "gz")


