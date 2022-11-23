# Packages
library(tidyverse)
library(readxl)
library(ggplot2)
library(stringr)
library(lubridate)
library(rscopus)
# Set Elsevier key
options('elsevier_api_key' = 'f9378c32be1c80ee3b87b71ee34c18da')

# Load and format the reviewed papers
data <- read_xlsx("LiteratureExtracts/20220923_FilteredLiteratureFinal.xlsx",sheet = 1) |>
  filter(Suitable == 1)

# Get most recent citation
rscopus::plumx_metrics(type = "doi", value = data$DOI[2])
