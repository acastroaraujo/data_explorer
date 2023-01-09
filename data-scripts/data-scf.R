
library(tidyverse)

raw <- read_csv("data-scripts/sub-data.txt")

wt_vars <- c(
  "WEIGHT" = "WGT"
)

con_vars <- c(
  "AGE", "KIDS", "INCOME", "ASSET", "FIN", "DEBT", "FINLIT", 
  "KGINC", "KGTOTAL", "NETWORTH"
)

cat_vars <- c(
  "AGECL", "HHSEX", "EDCL", "HOUSECL", "OCCAT1", "OCCAT2", 
  "RACE", "RACECL", "FAMSTRUCT", "LIFECL", "ASSETCAT", "NWCAT", "NWPCTLECAT"
)


# set labels for selected variables ---------------------------------------

lab_AGECL <- c(
  "<35",
  "35-44",
  "45-54",
  "55-64",
  "65-74",
  ">=75"
)

lab_HHSEX <- c(
  "male",
  "female"
)

lab_EDCL <- c(
  "no high school diploma/GED",
  "high school diploma or GED",
  "some college",
  "college degree"
)

lab_FAMSTRUCT <- c(
  "not married/LWP + children",
  "not married/LWP + no children + reference person under 55",
  "not married/LWP + no children + reference person 55 or older",
  "married/LWP+ children",
  "married/LWP + no children"
)

lab_HOUSECL <- c(
  "owns ranch/farm/mobile home/house/condo/coop/etc.",
  "otherwise"
)

lab_OCCAT1 <- c(
  "work for someone else",
  "self-employed/partnership",
  "retired/disabled + (student/homemaker/misc. not working and age 65 or older)",
  "other groups not working (mainly those under 65 and out of the labor force)"
)

lab_OCCAT2 <- c(
  "managerial/professional",
  "technical/sales/services",
  "other (incl. production/craft/repair workers, operators, laborers, farmers, foresters, fishers)",
  "not working"
)

lab_RACE <- c(
  "white non-Hispanic",
  "black / African American",
  "Hispanic",
  "Other"
)

lab_RACECL <- c(
  "white non-Hispanic",
  "nonwhite or Hispanic"
)

lab_LIFECL <- c(
  "reference person under 55 + not married/LWP + no children",
  "reference person under 55 + married/LWP + no children",
  "reference person under 55 + married/LWP + children",
  "reference person under 55 + not married/LWP + children",
  "reference person 55 or older and working",
  "reference person 55 or older and not working"
)

lab_ASSETCAT <- c(
  "0-20",
  "20-39.9",
  "40-59.9",
  "60-79.9",
  "80-89.9",
  "90-100"
)

lab_NWCAT <- c(
  "0-24.9",
  "25-49.9",
  "50-74.9",
  "75-89.9",
  "90-100"
)

lab_NWPCTLECAT <- c(
  "0-9.9",
  "10-19.9",
  "20-29.9",
  "30-39.9",
  "40-49.9",
  "50-59.9",
  "60-69.9",
  "70-79.9",
  "80-89.9",
  "90-94.9",
  "95-98.9",
  "99-100"
)
  
scf <- raw |> 
  select(all_of(c(con_vars, cat_vars, wt_vars))) |> 
  mutate(AGECL = factor(AGECL, labels = lab_AGECL)) |>
  mutate(HHSEX = factor(HHSEX, labels = lab_HHSEX)) |> 
  mutate(EDCL = factor(EDCL, labels = lab_EDCL)) |> 
  mutate(HOUSECL = factor(HOUSECL, labels = lab_HOUSECL)) |> 
  mutate(OCCAT1 = factor(OCCAT1, labels = lab_OCCAT1)) |> 
  mutate(OCCAT2 = factor(OCCAT2, labels = lab_OCCAT2)) |> 
  mutate(RACE = factor(RACE, labels = lab_RACE)) |> 
  mutate(RACECL = factor(RACECL, labels = lab_RACECL)) |> 
  mutate(FAMSTRUCT = factor(FAMSTRUCT, labels = lab_FAMSTRUCT)) |> 
  mutate(LIFECL = factor(LIFECL, labels = lab_LIFECL)) |> 
  mutate(ASSETCAT = factor(ASSETCAT, labels = lab_ASSETCAT)) |> 
  mutate(NWCAT = factor(NWCAT, labels = lab_NWCAT)) |> 
  mutate(NWPCTLECAT = factor(NWPCTLECAT, labels = lab_NWPCTLECAT)) 

write_rds(scf, "datasets/scf.rds", compress = "gz")

scf_dict <- c(
  "AGECL" = "Age group of the reference person",
  "AGE" = "Age of reference person",
  "HHSEX" = "Gender of household reference person",
  "EDCL" = "Education category of reference person",
  "FAMSTRUCT" = "Family structure of household",
  "HOUSECL" = "Home-ownership category of household",
  "KIDS" = "Total number of children in household.\n\nIncludes natural children,step-children, and foster children of household reference person or spouse/partner.",
  "OCCAT1" = "Occupation categories for reference person",
  "OCCAT2" = "Occupation classification for reference person",
  "RACE" = "Race/ethnicity of respondent",
  "RACECL" = "Class of race of respondent",
  "LIFECL" = "Life cycle of reference person",
  "ASSET" = "Total value of assets held by household, 2019 dollars.\n\nThe sum of financial assets and nonfinancial assets.",
  "ASSETCAT" = "Asset percentile groups",
  "FIN" = "Total value of financial assets held by household, 2019 dollars.\n\nConsists of liquid assets, certificates of deposit, directly held pooled investment funds, stocks, bonds, quasi-liquid assets, savings bonds, whole life insurance, other managed assets, and other financial assets. See the definition of each asset for further details.",
  "DEBT" = "Total value of debt held by household, 2019 dollars.\n\nIncludes principal residence debt (mortgages and HELOCs), other lines of credit, debt for other residential property, credit card debt, installment loans,and other debt.",
  "FINLIT" = "Number of financial literacy questions answered correctly",
  "INCOME" = "Total amount of income of household, 2019 dollars.\n\nHousehold income for previous calendar year.  Inlcudes wages, self-employment and business income, taxable and tax-exempt interest, dividends, realized capital gains, food stamps and other support programs provided by the government, pension income and withdrawals from retirement accounts, Social Security income, alimony and other support payments, and miscellaneous sources of income.",
  "KGINC" = "Capital gain or loss income, 2019 dollars",
  "KGTOTAL" = "Total unrealized capital gains or losses for the household, 2019 dollars.\n\nIncludes the following capital gains: primary residence, other real estate, businesses, and stocks and pooled investment funds.\n\nCapital gains for the primary residence are calculated as the current price less the original purchase price and less improvements. Capital gains on real estate are calculated as the current vale less the purchase price adjusted for ownership share. Capital gains on businesses are calculated as current value less tax basis (active and nonactive businesses). Capital gains on stocks and pooled investment funds are calculated as the current value less gains or losses.",
  "NETWORTH" = "Total net worth of household, 2019 dollars.\n\nThe difference between assets and debt. See definitions of assets and debt for further clarification.",
  "NWCAT" = "Net worth percentile groups",
  "NWPCTLECAT" = "Alternate net worth percentile groups"
)


write_rds(scf_dict, "datasets/scf_dict.rds", compress = "gz")

