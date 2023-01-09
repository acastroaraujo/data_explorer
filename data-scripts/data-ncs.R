
library(tidyverse)
library(haven)

setwd(here::here("01-explorer/"))

# National Congregations Study --------------------------------------------

# The data was downloaded from the following link on 
# https://osf.io/s9hxb/download

file_path <- "data-scripts/National Congregations Study, Cumulative Dataset (1998, 2006-2007, 2012, and 2018-2019).DTA"
raw <- haven::read_dta(file_path)
raw <- dplyr::filter(raw, year == max(year))
empty_cols <- which(map_lgl(raw, \(x) all(is.na(x))))
raw <- raw[, -empty_cols]

con_vars <- c(
  "founded", "numtotal", "numreglr", "NUMSERV1", "musicmin", "income", "whitepct", "blackpct", "latinpct", "asianpct"
)

cat_vars <- c(
  "region", "dencode", "DENCODE3", "TRAD3", "budget", "clergone", "clergsex", "clerrace", "clergage",
  "extsing", "jump", "tongues", "prosperg", "politics", "voterreg", "racerel",
  "racepo", "lgbt", "mbrgay", "gaywed", "libcon", "LIBCON7"
)

wt_vars <- c(
  "WEIGHT" = "WT_ALL4_CONG_IGN"
)

ncs <- raw |> 
  select(all_of(c(con_vars, cat_vars, wt_vars))) |> 
  mutate(across(everything(), haven::zap_missing)) |> 
  mutate(across(all_of(cat_vars), haven::as_factor))

write_rds(ncs, "datasets/ncs.rds", compress = "gz")

ncs_dict <- c(
  "founded" = "In what year was your congregation officially founded?",
  "numtotal" = "How many persons would you say are associated in any way with the religious life of this congregation - counting both adults and children, counting both regular and irregular participants, counting both official or registered members and also participating nonmembers.  What is the total number of persons associated with this congregation to any degree at all? Remarks: In 1998 and 2006-07 a single number response was accepted. In 2012 and 2018-19 either a single number or a range was accepted. When ranges were accepted, the average was used as NUMTOTAL.",
  "numreglr" = "How many persons - counting both adults and children - would you say regularly participate in the religious life of your congregation - whether or not they are officially members of your congregation? Remarks: In 1998 and 2006-07 a single number response was accepted. In 2012 and 2018-19 either a single number or a range was accepted. When ranges were accepted, the average was used as NUMREGLR.",
  "NUMSERV1" = "In a typical week, how many worship services does your congregation hold?",
  "musicmin" = "[2006-07, 2012 and 2018-19 wording] Counting all the musical parts of this service, how many minutes would you say were taken up by music? [ARDA Note: Please see MUSICMIN in the survey instrument for full question wording.]",
  "income" = "What is the total amount of money your congregation received in income from all sources during your most recent fiscal year? Dollar amount.",
  "whitepct" = "What percent of the regular adult participants in your congregation are white and non-Hispanic? Percent of regular adult participants; Remarks: If an informant replied with an absolute number rather than a percent, we calculated a percent for WHITEPCT.",
  "blackpct" = "What percent are Black or African American? Percent of regular adult participants; Remarks: If an informant replied with an absolute number rather than a percent, we calculated a percent for BLACKPCT",
  "latinpct" = "Hispanic or Latino? Percent of regular adult participants; Remarks: If an informant replied with an absolute number rather than a percent, we calculated a percent for LATINPCT.",
  "asianpct" = "Asian or Pacific Islander? Percent of regular adult participants; Remarks: If an informant replied with an absolute number rather than a percent, we calculated a percent for ASIANPCT.",
  "region" = "Region in which congregation is located.  Remarks: See Statistical Abstracts (any edition), U.S. Bureau of the Census, for a list of states within regions.",
  "dencode" = "Denominational affiliation, collapsed; Remarks: DENCODE uses the information from DENOM and other questions asking about congregations' religious affiliations and traditions to create an aggregated religious family variable. [ARDA Note: Please see DENCODE in the survey instrument for full remarks.]",
  "DENCODE3" = "Denominational affiliation, collapsed further; Remarks: DENCODE3 collapses DENCODE into an even more broadly defined religious family variable. Note that DENCODE3 places congregations in a category if the religious tradition of the congregation was clear, even if HAVEDEN = 2.",
  "TRAD3" = "Religious Tradition - collapsed; Remarks: TRAD3 uses all available information to place congregations into very broad religious traditions [ARDA Note: Please see TRAD3 in the survey instrument for full remarks.]",
  "budget" = "Does your congregation operate with a formal, written annual budget?",
  "clergone" = "Is there one person who is the head or senior clergy person or religious leader in your congregation?",
  "clergsex" = "Is this person male or female? [ARDA Note: Please see DENOM in the survey instrument for full remarks.]",
  "clerrace" = "What race or ethnicity [are you/is this person?] Remarks: See remarks for CLERGSEX. [ARDA Note: Please see CLERRACE in the survey instrument for full remarks.]",
  "extsing" = "Was there a period of extended singing during the service where the congregation sang three or more songs in a row? Remarks: This item was asked only if there was singing by the congregation during the main service (SINGING=1).",
  "jump" = "Did any adults jump, shouts, or dance spontaneously during this service?",
  "tongues" = "Did people speak in tongues at any service within the past 12 months? Remarks: This item was asked only of Christian congregations.",
  "prosperg" = "Some religious groups teach that God gives financial wealth and good physical health to those with enough faith. Does your congregation teach this? Remarks: This item was asked only of Christian congregations.",
  "politics" = "Within the past 12 months, have there been any groups or meetings or classes or events specifically focused on the following purposes or activities? To discuss politics?",
  "voterreg" = "Within the past 12 months, have there been any groups or meetings or classes or events specifically focused on the following purposes or activities? An effort to get people registered to vote?",
  "racerel" = "Within the past 12 months, have there been any groups or meetings or classes or events specifically focused on the following purposes or activities? [2018-19 wording] To discuss issues related to race and race relations? [ARDA Note: Please see RACEREL in the survey instrument for full question wording.]",
  "racepo" = "Within the past 12 months, have there been any groups or meetings or classes or events specifically focused on the following purposes or activities? To discuss issues related to race and the police?",
  "lgbt" = "Within the past 12 months, have there been any groups or meetings or classes or events specifically focused on the following purposes or activities? To discuss issues related to sexual orientation or gender identity?",
  "mbrgay" = "[2012 wording] Would an openly gay or lesbian couple in a committed relationship be permitted to be full-fledged members of your congregation? [ARDA Note: Please see MBRGAY in the survey instrument for full remarks and question wording.]",
  "gaywed" = "Would your congregation allow a wedding of two people of the same sex to take place in your building? Remark: This item was asked only if the congregation owned their old building (OWNBLDG = 1). If the congregation reported not owning their own building (OWNBLDG=2), GAYWED was left as missing.",
  "libcon" = "Politically speaking, would your congregation be considered more on the conservative side, more on the liberal side, or right in the middle?",
  "LIBCON7" = "More elaborate scale of political leaning; Remarks: This variable was constructed from answers to LIBCON and follow-up questions 276a and 276b."
)

write_rds(ncs_dict, "datasets/ncs_dict.rds", compress = "gz")
