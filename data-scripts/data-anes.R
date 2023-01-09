

## remotes::install_github("jamesmartherus/anesr")

library(tidyverse)
library(anesr)

data(package = "anesr") #View a list of available datasets

data(timeseries_2020)

haven::zap_label(timeseries_2020$V202482) 

haven::zap_formats(timeseries_2020$V202482)

haven::zap_labels(timeseries_2020$V202482) |> zap_missing()
attributes(timeseries_2020$V202482)
