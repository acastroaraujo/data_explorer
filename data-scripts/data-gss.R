
# remotes::install_github("kjhealy/gssr")

library(tidyverse)
library(gssr)

# gss <- gss_get_yr(2021)
# data(gss_all)
# gss <- gss_all |> filter(year == 2021)

gss <- haven::read_dta("data-scripts/GSS2021.dta")

con_vars <- c(
  "age", "cohort", "coninc"
)

cat_vars <- c(
  "sex", "race", "region", "degree", "health",
  "satjob", "marital", 
  "libhomoy", "biblauth", "prochoic", "prolife",
  "partyid", "polviews", "fefam", "religinf", 
  "cappun", "gunlaw",
  "god"
  #"wrkstat"
)

wt_vars <- c(
  "WEIGHT" = "wtssps"
) 

gss <- gss |> 
  rename_all(tolower) |> 
  select(all_of(c(con_vars, cat_vars, wt_vars))) |> 
  mutate(across(everything(), haven::zap_missing)) |> 
  mutate(across(all_of(cat_vars), haven::as_factor)) |> 
  filter(!is.na(age)) 

write_rds(gss, "datasets/gss.rds", compress = "gz")

## get pairs
map(gss, \(x) attributes(x)$label) |> enframe() |> unnest(value)

gss_dict <- c(
  "cohort" = "Year of birth",
  "age" = "Age of respondent",
  "coninc" = "Family income in constant dollars (inflation-adjusted)",
  "sex" = "Respondent's sex",
  "race" = "Race of respondent",
  "region" = "Region of interview",
  "degree" = "Respondent's highest degree",
  "health" = "Would you say your own health, in general, is excellent, good, fair, or  poor?",
  #"wrkstat" = "Labor force status: Last week were you working full time, part time, going to school, keeping house, or what?",
  "satjob" = "On the whole, how satisfied are you with the work you do—would you say you are very satisfied, moderately satisfied, a little dissatisfied, or very dissatisfied?",
  "marital" = "Marital status",
  "libhomoy" = "If somebody in your community suggests that a book the gay person wrote in favor of homosexuality should be taken out of your public library, would you favor  removing it, or not?",
  "biblauth" = "The Bible is the highest authority for what I believe.",
  "prochoic" = "I consider myself pro-choice.",
  "prolife" = "I consider myself pro-life.",
  "partyid" = "Generally speaking, do you usually think of yourself as a Republican, Democrat, Independent, or what?",
  "polviews" = "We hear a lot of talk these days about liberals and conservatives. I'm going to show you a seven-point scale on which the political views that people might hold are arranged from extremely liberal—point 1—to extremely conservative—point 7. Where would you place yourself on this scale?",
  "fefam" = "It is much better for everyone involved if the man is the achiever outside the home and the woman takes care of the home and family.",
  "cappun" = "Do you favor or oppose the death penalty for persons convicted of murder?",
  "gunlaw" = "Would you favor or oppose a law which would require a person to obtain a police permit before he or she could buy a gun?",
  "god" = "Respondent's confidence in the existence of god."
)

write_rds(gss_dict, "datasets/gss_dict.rds", compress = "gz")


