# Purpose of this script is to format the search terms and align them
# https://elizagrames.github.io/litsearchr/litsearchr_vignette.html
library(litsearchr)
library(tidyverse)
library(assertthat)
library(readxl)

# Load the extracts
ll <- list.files("LiteratureExtracts/",full.names = TRUE)
ll <- ll[has_extension(ll, 'csv')]

# Read them in
df <- ll |> map(~read_csv(.x))
names(df) <- basename(ll)
# Length
sapply(df, nrow)

# --- #
# Copy them manually together in Excel!
# ----------- #
# Updated latest search queries
# Downloaded sheets have been copied together.

# Load them first
first <- read_xlsx("LiteratureExtracts/20220816_Combinedquery.xlsx",sheet = 1)
new <- read_xlsx("LiteratureExtracts/20220923_Results.xlsx",sheet = 1)

# Check for duplicates with first reviewed file
sum(first$Title %in% new$Title)
sum(first$DOI %in% new$DOI)

# Remove duplicates
new <- new |> filter(!(DOI %in% first$DOI))
new <- new |> filter(!(duplicated(DOI)))

# save output and manually copy to bottom of the excel sheet -> Review
write_excel_csv(new, "new.csv")
