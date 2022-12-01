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

# Also do the same for altmetric? #
# library(rAltmetric)
# library(purrr)
#
# # Get altmetric data
# alm <- function(x)  altmetrics(doi = x,apikey = "TBD") %>% altmetric_data()
# results <- pmap_df(list(data$newDOI), alm)

# --------------------- #
#### Generic data formatting ####
# Format citation query
library(dplyr)
library(tidyr)
library(assertthat)
library(stringr)
out_cite <- readRDS("resSaves/citation_extraction.rds")

# Formatted output
out_form <- out_cite |> select(date:crossref_citationcount) |> distinct()
out_form$cite_policy <- out_form$cite_scientific <- out_form$cite_socialmedia <- 0

# Very hacky distlling function
for(id in unique(out_form$doi)){
  print(id)
  sub <- out_cite |> filter(doi == id) |>
    # Remove capture and useage as we don't need this
    filter(name %in% c("citation", "socialMedia", "mention"))

  # Get citations in policy literature
  chk <- apply(sub, 2, function(z) grep("POLICY_CITED_BY_COUNT",z,ignore.case = TRUE)) |> unlist()
  # Get column name
  if(length(chk)>0){
    test <- sub |> filter(name == "citation")
    out_form$cite_policy[out_form$doi==id] <- test[,match(names(chk), names(test))+1]
  } else {
    out_form$cite_policy[out_form$doi==id] <- 0; chk <- NA
  }

  if("citation" %in% sub$name){
    # Get citations in scientific literature
    out_form$cite_scientific[out_form$doi==id] <- sub$total[sub$name == "citation"] - out_form$cite_policy[out_form$doi==id]
  }
  # Get social media and news mentions (don't care about origin, so use total
  out_form$cite_socialmedia[out_form$doi==id] <- sum(sub$total[sub$name=="socialMedia"],
                                                     sub$total[sub$name=="mention"])
}
assert_that(
  nrow(out_form)>0,
  all(out_form$cite_policy>=0),
  all(out_form$cite_scientific>=0)
)
saveRDS(out_form, "resSaves/citation_extraction_formatted.rds")

# Format country names


# Format other labels

# Save final output
