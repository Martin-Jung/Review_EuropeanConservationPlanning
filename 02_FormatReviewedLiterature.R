# Packages
library(tidyverse)
library(readxl)
library(sf)
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

# ------------- #
#### Format literature data ####
# Format DOI
data$newDOI <- stringr::str_replace_all(data$DOI, "http://dx.doi.org/","")
data$newDOI <- str_replace_all(data$newDOI, "https://dx.doi.org/","")
data$newDOI <- str_replace_all(data$newDOI, "Https://dx.doi.org/","")
data$newDOI <- str_replace_all(data$newDOI, "https://doi.org/","")
data$newDOI <- str_replace_all(data$newDOI, "https://","")

# Make names
names(data) <- make.names(names(data))

# Format classes
data$Extent <- factor(data$Extent, levels = c("Local", "National", "Regional", "Europe"))
data$Realm <- factor(data$Realm, levels = c("Terrestrial", "Freshwater", "Marine", "Cross-realm"))
data$Ecosystem.specificity <- fct_collapse(data$Ecosystem.specificity,
                                     None = c("all", "All", "none", "None"),
                                     ForestWoodland = c("Forest", "Woodland", "Forests"),
                                     Wetlands = c("Wetlands", "Wetland"),
                                     Urban = c("Urban"),
                                     Grassland = c("Grassland"),
                                     Cropland = c("Cropland", "Farmland"),
                                     Coastal = c("Coastal benthic", "Coastal", "Coast"),
                                     RiversLakes = c("Rivers and lakes", "Rivers", "River","Rivers, Ponds, Lakes", "Rivers, Lakes",
                                                     "Rivers, Ponds", "Riverine floodplains"),
                                     Multiple = c("Cropland, Grassland","Shrubland, Cropland", "Rivers, Coastal",
                                                  "Wetlands, Forest","Rivers, Estuary, Marine", "Rivers, Estuary, Marine",
                                                  "Forest, Sparse Vegetation, Cropland", "AgroSystems"),
                                     other_level = "Other"
                                     )
data$Ecosystem.specificity[is.na(data$Ecosystem.specificity)] <- "None"
assertthat::assert_that(!any(is.na(data$Ecosystem.specificity)))

data$Period <- factor(data$Period, levels = c("Contemporary only", "Future", "Both"))
data$Period[is.na(data$Period)] <- "Contemporary only"
data$Planning.purpose <- factor(data$Planning.purpose, c("Representation", "Priority places", "Management action", "Synergies/Tradeoffs", "Multiple"))
data$Method <- fct_collapse(data$Method,
                            "Marxan*" = "Marxan*", "Zonation" = "Zonation", "Integer programming" = "Integer programming",
                            "GIS Overlay" = c("GIS overlay/ Ranking / Clustering", "GIS overlay"),
                            "Other" = c("Other", "NA"))
data$Biodiversity.type <- factor(data$Biodiversity.type, levels = c("Species", "Ecosystems", "Ecosys. Services / NCP",
                                                                    "Multiple", "Other"))
data$Biodiversity.type[is.na(data$Biodiversity.type)] <- "Other"

# Constraints, Connectivity and other features
data$Multiple.objectives.or.constraints <- factor(data$Multiple.objectives.or.constraints,
                                                  levels = c("None", "Costs", "Constrains", "Multi-objective/criteria",
                                                             "Multiple", "Other"))
data$Multiple.objectives.or.constraints[is.na(data$Multiple.objectives.or.constraints)] <- "Other"

data$Connectivity <- factor(data$Connectivity, levels = c("None","Boundary", "Structural", "Functional", "Multiple"))
data$Costs <- factor(data$Costs, levels = c("no", "yes"))

data$Stakeholder.involvement <- factor(data$Stakeholder.involvement, levels = c("no", "yes"))

# Policy relevance formatting
po <- data$Policy.relevance
po <- ifelse(is.na(po) | po %in% c("none", "None"), "None", po)
po <- ifelse(str_detect(po, ","),"Multiple", ifelse(str_detect(tolower(po),"case study"),"Case study", ifelse(po == "None", "None", "Single") ) )
data$Policy.relevance <- po
data$Policy.relevance <- factor(data$Policy.relevance, levels = c("None", "Single", "Multiple", "Case study"))
rm(po)

# --- #
# Format country names list
co <- separate(data |> select(DOI,Region), col = Region,into = paste0("V",1:8), sep = ",") |>
  mutate_all(str_trim) |>
  # Reshape to longer using doi as ID
  reshape2::melt(id.vars = "DOI") |> dplyr::select(-variable) |> distinct() |> drop_na(value) |>
  rename(country = value)
# sort(unique(co$country))

# TODO:
# Get spatial files in here and match them against the names
# path_terresttrial <- "extdata/TerrestrialRegions.gpkg"
# regions <- sf::st_read(path_terresttrial)
path_terrestrial <- "extdata/TerrestrialRegions.gpkg"
regions <- sf::st_read(path_terrestrial)

# Some manual recoding
co$country[co$country=="German"] <- "Germany"
co$country[co$country=="Mediteranean"] <- "Mediterranean"
co$country[co$country=="Serbia"] <- "Republic of Serbia"
co$country[co$country=="Czech Republic"] <- "Czechia"
co$country[co$country=="Macedonia"] <- "North Macedonia"
co$country[co$country=="Macedonia"] <- "Cyprus"

co$country[which(!co$country %in% regions$SOVEREIGNT)]
assertthat::assert_that(!anyNA(co$country))

# Save output
assertthat::assert_that(!anyNA(co$country))
saveRDS(co, "resSaves/location_match.rds")

# Locality and finer level, to do (later)!
# data$Locality

# --- #

# Save final output
saveRDS(data, "resSaves/literature_formatted.rds")

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

# --------------- #
#### Format references for export to Zenodo ####
# Get latest file
data <- read_xlsx("LiteratureExtracts/20220923_FilteredLiteratureFinal.xlsx",sheet = 1) |>
  filter(Suitable == 1) |>
  dplyr::arrange(Year) # Sort by year
# Make names
names(data) <- make.names(names(data))

# Ignore unrelevant columns
data <- data |> dplyr::select(-Reviewer, -Suitable,-Cited.by, -Abstract) |>
  dplyr::mutate(ID = 1:n()) |>
  dplyr::relocate(ID, .before = "Extent") |>
  dplyr::rename(Journal = "Source.title")

# Add Manually added where missing
data$Source[is.na(data$Source)] <- "Manual"
data$Document.Type[is.na(data$Document.Type)] <- "Article"

# Format DOI
data$DOI <- stringr::str_replace_all(data$DOI, "http://dx.doi.org/","")
data$DOI <- str_replace_all(data$DOI, "https://dx.doi.org/","")
data$DOI <- str_replace_all(data$DOI, "Https://dx.doi.org/","")
data$DOI <- str_replace_all(data$DOI, "https://doi.org/","")
data$DOI <- str_replace_all(data$DOI, "https://","")
data$DOI <- str_replace_all(data$DOI, "dx.doi.org/","")
data$DOI <- stringr::str_trim(data$DOI,side = "both")

# Add citation information in
cit <- readRDS("resSaves/citation_extraction_formatted.rds") |>
  dplyr::select(doi, cite_scientific, cite_policy) |>
  dplyr::rename(cite_scientific_May2023 = "cite_scientific", cite_policy_May2023 = "cite_policy")

data <- data |> left_join(cit, by = c('DOI' = "doi"))
# Fill missing
data$cite_scientific_May2023[is.na(data$cite_scientific_May2023)] <-0
data$cite_policy_May2023[is.na(data$cite_policy_May2023)] <- 0

# Write output
write.csv(data, "resSaves/JungEtAl_EuropeanConservationPlanningStudies.csv",row.names = FALSE)
