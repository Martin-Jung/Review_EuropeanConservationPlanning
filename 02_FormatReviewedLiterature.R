# Packages
library(tidyverse)
library(readxl)
library(ggplot2)
library(stringr)
library(lubridate)
library(progress)
library(rscopus)
library(rcrossref)
key <- 'f9378c32be1c80ee3b87b71ee34c18da'
rscopus::set_api_key(key)
# Set Elsevier key
options('elsevier_api_key' = key)
options("Elsevier_API_Interactive" = key)

dir.create("resSaves",showWarnings = FALSE)

# Load and format the reviewed papers
data <- read_xlsx("LiteratureExtracts/20220923_FilteredLiteratureFinal.xlsx",sheet = 1) |>
  filter(Suitable == 1)
# Format DOI
data$newDOI <- stringr::str_replace_all(data$DOI, "http://dx.doi.org/","")
data$newDOI <- str_replace_all(data$newDOI, "https://dx.doi.org/","")
data$newDOI <- str_replace_all(data$newDOI, "Https://dx.doi.org/","")
data$newDOI <- str_replace_all(data$newDOI, "https://doi.org/","")
data$newDOI <- str_replace_all(data$newDOI, "https://","")

# ------------- #
#### Use Plum-X to gather citation metrics

out_cite <- data.frame()

pb <- progress::progress_bar$new(total = nrow(data))
# Loop through each entry
for(i in 1:nrow(data)){
  pb$tick()
  
  if(is.na(data$newDOI[i])) next()
  # Lookup citations
  lookup <- rscopus::plumx_metrics(type = "doi", value = data$newDOI[i], verbose = FALSE)
  
  o <- data.frame(date = lookup$get_statement$date,
                  doi = data$newDOI[i]
                  )
  
  # Finally also get citation count from crossref
  cite_cr <- rcrossref::cr_citation_count(doi = data$newDOI[i])
  o$crossref_citationcount <- cite_cr$count
  
  # Append if found
  if(has_name(lookup$content,"count_categories")) {
    # For each lookup entry check citations
    o <- bind_cols(o, bind_rows( lapply(lookup$content$count_categories, as.data.frame) ) )
  }   
  
  # # And from opencitations
  # opcit <- paste0("https://opencitations.net/index/coci/api/v1/citations/",data$newDOI[i])
  # result <- rjson::fromJSON(file = opcit)
  # citing <- lapply(result, function(x){
  #   x[['citing']]
  # })
  # o$opencit_citationcount <- length(citing)

  out_cite <- bind_rows(out_cite, o)
}

# Save the output to resSaves
saveRDS(out_cite, "resSaves/citation_extraction.rds")

# Format citation query

# Format country names

# Format other labels

# Save final output