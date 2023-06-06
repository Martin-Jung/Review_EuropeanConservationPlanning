library(tidyverse)
library(tidyr)
library(readxl)
library(ggplot2)
library(ggthemes)
library(scales)
library(scico)
library(ggsci)
library(lme4)
library(brms)
library(tmap)
# Default theme and design
mytheme <- theme_bw(base_size = 20) +
  theme(
    panel.background = element_rect(#fill = "transparent",
                                    colour = NA_character_), # necessary to avoid drawing panel outline
    plot.background = element_rect(fill = "white",
                                   colour = NA_character_), # necessary to avoid drawing plot outline
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    # Colour background stuff
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(colour ="black"),
    axis.title = element_text(colour ="black"),
    legend.key = element_rect(fill = "transparent")
  )
# --- #
# Load in formatted literature extract
df <- readRDS("resSaves/literature_formatted.rds")

# Load in formatted citation data
cit <- readRDS("resSaves/citation_extraction_formatted.rds") |> dplyr::select(-date)

# And location information
locations <- readRDS("resSaves/location_match.rds")

dir.create("figures", showWarnings = FALSE)

# ----------- #
#### Basic statistics ####

# Number of studies per extent
table(df$Extent);table(df$Extent)/nrow(df)

sort(table(locations$country),decreasing = TRUE) / nrow(locations)

# Number of studies per Realm
table(df$Realm); table(df$Realm)/nrow(df)

# Number of studies with different planning purpose
table(df$Planning.purpose); table(df$Planning.purpose) / nrow(df)

# Approach
table(df$Method); table(df$Method) / nrow(df)

# Ecosystem specificity
table(df$Ecosystem.specificity); table(df$Ecosystem.specificity) / nrow(df)

# Number of features per type
df |> count(Biodiversity.type) / nrow(df)
df |> group_by(Biodiversity.type) |> summarise(m = mean(Number.of.features, na.rm=T),
                                               min = min(Number.of.features, na.rm = TRUE),
                                               max = max(Number.of.features, na.rm = TRUE))

# --- #
# Connectivity
table(df$Connectivity); prop <- table(df$Connectivity) / nrow(df)
sum(prop[-1]) # How many assessed anything in this direction

# Current or future
table(df$Period); table(df$Period) / nrow(df)
sum((table(df$Period) / nrow(df))[-1])

# Stakeholders
table(df$Stakeholder.involvement); table(df$Stakeholder.involvement) / nrow(df)

# Land-use constraints
df2 <- df |> filter(Planning.purpose!='Representation')
table(df2$Multiple.objectives.or.constraints)
sum((table(df2$Multiple.objectives.or.constraints)/nrow(df2))[-1])

# Policy context
(prop <- table(df$Policy.relevance) / nrow(df))
sum(prop[-1]) # How many out of all?

colSums( table(df$Policy.relevance, df$Method)[-1,] )

# Is there any study that ticks all the boxes?
o <- df
o$score <- ifelse(o$Multiple.objectives.or.constraints=="None",0,1) +
  ifelse(o$Period=="Contemporary only",0,1) +
  ifelse(o$Connectivity == "None", 0, 1) +
   ifelse(o$Stakeholder.involvement=="no", 0, 1)
o |> filter(score == 4)

# --- #
o <- left_join(df, cit, by = c("newDOI" = "doi")) |> select(Extent, cite_socialmedia:cite_policy)
Hmisc::describe(o$cite_socialmedia)
Hmisc::describe(o$cite_scientific)
Hmisc::describe(o$cite_policy)


# ----------- #
#### Basic plots ####
# All kinds of plots that do not have any specific hypothesis or
# or are more exploratory and aim to explore the data as is

# Number of papers per year
o <- df |> group_by(Extent, Year) |> summarise(N = n()) |> arrange(Year)

ggplot(o, aes(x = Year, y = N, colour = Extent)) +
  mytheme +
  # geom_line(size = 1.5) +
  geom_smooth(linewidth = 1.5, se = FALSE,method = "loess") +
  scale_y_continuous(breaks = pretty_breaks(10)) +
  scale_colour_aaas() +
  theme(legend.position = c(.2,.75)) +
  theme(legend.title = element_text(size = 27), legend.text = element_text(size = 25),
        axis.title = element_text(size = 26) ,axis.text = element_text(size = 26)) +
  labs(y = "Number of studies", x = "")
ggsave(filename = "figures/exploratory_timeseries.png", width = 10, height = 9, dpi = 400)

# Realm
o <- df |> group_by(Realm) |> summarise(N = n())
o$Realm <- factor(o$Realm, levels = c("Terrestrial", "Marine", "Freshwater", "Cross-realm"))

ggplot(o, aes(x = Realm, y = N)) +
  mytheme +
  geom_bar(stat = "identity", fill = "grey20",show.legend = FALSE) +
  scale_y_continuous(expand = c(0,Inf)) +
  theme(legend.title = element_text(size = 27), legend.text = element_text(size = 25),
        axis.title = element_text(size = 26) ,axis.text = element_text(size = 26)) +
  labs(x = "", y = "Number of studies")
ggsave(filename = "figures/exploratory_realms.png", width = 10, height = 9, dpi = 400)

# --- #
# For each planning purpose, summarize the latter columns and
# visualize in a ballon plot
# Narrow facets over another, so that each shows how certain things were done
library(ggpubr)

# policy
o1 <- df |> drop_na(Planning.purpose) |> count(Planning.purpose, Policy.relevance) |>
  rename(var1 = Planning.purpose, var2 = Policy.relevance) |>
  group_by(var1) |> mutate(prop = prop.table(n)) |> mutate(type = "Policy aim")
  # pivot_wider(names_from = c("Planning.purpose"),values_from = "n",values_fill = 0)

# Multiple objectives
o2 <- df |> drop_na(Planning.purpose) |> count(Planning.purpose, Multiple.objectives.or.constraints) |>
  rename(var1 = Planning.purpose, var2 = Multiple.objectives.or.constraints) |>
  group_by(var1) |> mutate(prop = prop.table(n)) |> mutate(type = "Land use")

# Connectivity
o3 <- df |> drop_na(Planning.purpose) |> count(Planning.purpose, Connectivity) |>
  rename(var1 = Planning.purpose, var2 = Connectivity) |>
  group_by(var1) |> mutate(prop = prop.table(n)) |> mutate(type = "Connectivity")

# Stakeholder involvement
o4 <- df |> drop_na(Planning.purpose) |> count(Planning.purpose, Stakeholder.involvement) |>
  rename(var1 = Planning.purpose, var2 = Stakeholder.involvement) |>
  group_by(var1) |> mutate(prop = prop.table(n)) |> mutate(type = "Stakeholder\ninvolvement")

# Combine all
o <- bind_rows(o1,o2, o3,o4)

# Build plot by plot
ggplot(o, aes(x = var1, y = var2, size = n, colour = prop)) +
  mytheme +
  geom_point(stroke = 2,colour="black",show.legend = FALSE) +
  geom_point(stroke = 1) +
  scale_size_area(guide = guide_legend("Number of studies",override.aes = list(colour="black"))) +
  scale_colour_gradientn(colours = viridis::inferno(50),
                         guide = guide_colorbar("Proportion", barwidth = unit(3,'in'), title.vjust = .75)) +
  theme(legend.position = "bottom", legend.box = "horizontal",legend.box.background = element_rect(color = NA)) +
  geom_text(aes(label = paste0(n)), colour = "black",nudge_y = .4, size = 4) +
  labs(x = "", y = "") +
  facet_wrap(~type,scales = "free_y",nrow = 2) +
  theme(axis.text.x.bottom = element_text(hjust = 1, angle = 45)) +
  # Reduce distance between y-axis
  theme(plot.title = element_text(hjust = 0.5),
        legend.text = element_text(color = "black"), legend.title = element_text(color = "black") )
ggsave("figures/exploratory_bubble.png",width = 12,height = 10,dpi = 400)

#### Supplementary figures ####
# SI Figure 1 #
# Summarize the number of features per type

# Calculate averages
m <- df |> group_by(Biodiversity.type) |> summarise(m = mean(Number.of.features, na.rm=T),
                                                    sd = sd(Number.of.features, na.rm = TRUE),
                                               min = min(Number.of.features, na.rm = TRUE),
                                               max = max(Number.of.features, na.rm = TRUE))

o <- df |> group_by(Biodiversity.type, Year) |> summarise(N = n()) |>
  arrange(Year) |> tidyr::drop_na(N)


g1 <- ggplot(df, aes(x = Biodiversity.type, y = Number.of.features)) +
  mytheme +
  geom_violin( fill = "grey90") +
    geom_jitter(size = 1.5,width = .25) +
    # Add averages
    geom_point(data = m, aes(y = m), size = 5, colour = "red") +
  scale_y_log10() +
  theme(legend.title = element_text(size = 27), legend.text = element_text(size = 25),
        axis.title = element_text(size = 26) ,axis.text = element_text(size = 26)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = expression(log10(Features)), x = "")
ggsave(filename = "figures/SI_Figure1.png", plot = g1, width = 8, height = 8, dpi = 400)


# --- #
# SI Figure 2
# Does stakeholder involvement differ by spatial extent?
table(df$Stakeholder.involvement, df$Extent)

o <- df %>% count(Stakeholder.involvement, Extent, .drop = FALSE)

ggplot(o, aes(x = factor(Extent), y = n, fill = factor(Stakeholder.involvement))) +
  mytheme +
  geom_bar(stat = "identity",position = position_dodge()) +
  scale_fill_npg() +
    guides( fill = guide_legend(title = "Involvement\nstakeholders") ) +
    theme(legend.position = c(.7, .7)) +
  labs(x = "", y = "Number of studies")
ggsave("figures/SI_Figure2.png", width = 6,height = 7, dpi = 400)

#### Create spatial map of density of studies ####
# Take the location information from extData and rasterize with each study
# Then stack and plot
library(raster)
library(RStoolbox)
library(sf)
# Load BIOCLIMA raster
if(Sys.info()["sysname"] == "Windows"){
  template <- raster("../../BIOCLIMA/bioclima_code/data/referenceraster_10000.tif")
} else {
  template <- raster("../../BIOCLIMA/data/referenceraster_10000.tif")
}

terrestrial <- st_read("extdata/TerrestrialRegions.gpkg") |> sf::st_transform(crs = st_crs(template)) |>
  filter(!SOVEREIGNT %in% c("Iceland", "Moldova"))
marine <- st_read("extdata/MarineRegions.gpkg") |> st_transform(crs = st_crs(template)) |>
  filter(!(GEONAME %in% c("Jan Mayen Exclusive Economic Zone","Joint regime area Iceland / Norway (Jan Mayen)",
                           "Icelandic Exclusive Economic Zone",
                          "Greenlandic Exclusive Economic Zone")))

df$Region[df$Region=="Mediteranean"] <- "Mediterranean"
assertthat::assert_that(!anyNA(df$Region))

out <- raster::stack()
for(id in 1:nrow(df)){
  print(id)
  sub <- slice(df, id) |>
    left_join(locations, by = c("DOI" = "DOI"))
  # Rasterize depending on Realm
  if(all(sub$Realm == "Marine")){
    if(all(sub$Region == "Europe")){
      m  <- marine
    } else {
      if(all(sub$Region == "Mediterranean")){
        #FIXME: Dirty hack for now. Need to properly filter to region
        m <- marine |> filter(SOVEREIGN1 %in% c("Spain", "France", "Italy", "Croatia", "Greece","Cyprus",
                                                "Morocco",
                                                "Lebanon", "Egypt","Libya", "Malta", "Syria", "Turkey","Monaco",
                                                "Tunisia", "Montenegro", "Albania", "Palestine", "Israel",
                                                "Bosnia and Herzegovina", "Slovenia", "Algeria"))
      } else if(all(sub$Region == "Adriatic-Ionian Region")) {
        m <- marine |> filter(SOVEREIGN1 %in% c("Italy","Croatia", "Slovenia", "Greece","Montenegro","Albania","Bosnia and Herzegovina", "Slovenia"))
      } else { m <- marine |> filter(SOVEREIGN1 %in% sub$country) }
    }
    if(nrow(m)>0){
      o <- rasterize(m, template, 1)
      out <- raster::addLayer(out, o)
    } else { stop()}
  } else {
    if(all(sub$Region == "Europe")){
      m <- terrestrial
    } else {
      if(all(sub$Region == "Mediterranean")){
        #FIXME: Dirty hack for now. Need to properly filter to region
        m <- terrestrial |> filter(SOVEREIGNT %in% c("Spain", "France", "Italy", "Croatia", "Greece","Cyprus",
                                                "Morocco", "Vatican", "Northern Cyprus", "Cyprus No Mans Area",
                                                "San Marino", "Andorra", "Portugal","Vatican",
                                                "Lebanon", "Egypt","Libya", "Malta", "Syria", "Turkey","Monaco",
                                                "Tunisia", "Montenegro", "Albania", "Palestine", "Israel",
                                                "Bosnia and Herzegovina", "Slovenia", "Algeria"))
      } else if(all(sub$Region=="Alpes")){
        m <- terrestrial |> filter(SOVEREIGNT %in% c("Austria", "Switzerland"))
      } else if(all(sub$Region == "Danube catchment")){
        m <- terrestrial |> filter(SOVEREIGNT %in% c("Austria", "Bulgaria","Hungary","Romania","Germany", "Croatia"))
      } else { m <- terrestrial |> filter(SOVEREIGNT %in% sub$country) }
    }
    if(nrow(m)>0){
      o <- rasterize(m, template, 1)
      out <- raster::addLayer(out, o)
    } else {stop()}
  }
}
raster::nlayers(out)

ras <- sum(out, na.rm = TRUE)
ras[ras==0] <- NA
# plot(ras, col = ibis.iSDM:::ibis_colours$sdm_colour )

# m <- raster::mask(ras, template, inverse = TRUE)

# Clip terrestrial to European extent
ter <- terrestrial |> st_crop(extent(ras))

ggR(ras, geom_raster=TRUE) +
  mytheme + theme(panel.background = element_blank(),plot.background = element_blank()) +
  theme(panel.grid.major = element_line(colour = "grey60")) + # Increase visibility of grid lines
  scale_fill_gradientn(colors = scico(100, palette = "hawaii", direction = -1), na.value = NA,
                       guide = guide_colourbar(title = "Number of studies",barwidth = unit(3, 'in'),
                                               label.theme = element_text(color = "black",size = 20),
                                               title.theme = element_text(color = "black",size = 19)  ) ) +
  geom_sf(data = ter, fill = NA, colour = "grey90", linewidth = .75, show.legend = FALSE) +
  theme(legend.position = "bottom",axis.ticks = element_blank(), axis.text = element_blank()) + labs(x = "", y = "")
ggsave(plot = last_plot(),"figures/map_densitystudies.png", width = 10, height = 8, dpi = 400)

#### Specific plots and hypotheses ####
# Here we explore specific hypotheses surrounding the collated citation information

# ---- #
# Idea 1: Are studies of larger study extent more often cited in research and policy contexts
o <- left_join(df, cit, by = c("newDOI" = "doi")) |> dplyr::select(Extent,Year, cite_socialmedia:cite_policy)

# Test
cor.test(o$cite_socialmedia, o$cite_scientific) # Social media does not explain citation rate
cor.test(o$cite_socialmedia, o$cite_policy) # Also not in policy documents
cor.test(o$cite_scientific, o$cite_policy) # However more widely cited papers are more likely to be also cited by policy

# Fit zero-inflated bayesian Poisson regressions
fit1 <- brm(cite_scientific~Extent + (1|Year), data = o, family = zero_inflated_poisson(),backend = "cmdstanr") # Scientifically, no difference
fit2 <- brm(cite_policy~Extent + (1|Year), data = o, family = zero_inflated_poisson(),backend = "cmdstanr") # Scientifically, no difference

# Get conditional effects
me1 <- conditional_effects(fit1,
                           re_formula = NA, # Include group level effects
                           prob = 0.95
)
me2 <- conditional_effects(fit2,
                    re_formula = NA, # Include group level effects
                    prob = 0.95
                    )
o <- bind_rows(
  me1$Extent |> mutate(type = "Scientific literature"),
  me2$Extent |> mutate(type = "Policy documents")
)

# Save the partial results for later
saveRDS(o, "resSaves/samples_citationrates.rds")
o <- readRDS("resSaves/samples_citationrates.rds")

ggplot(o, aes(x = Extent, y = estimate__, ymin = lower__, ymax = upper__)) +
  mytheme +
  geom_pointrange(size = 1.5, linewidth = 1.2) +
  scale_y_continuous(breaks = pretty_breaks(4)) +
  facet_wrap(~type,scales = "free_y") +
  labs(x = "", y = "Relative citation rate")
ggsave("figures/test_citationrates.png",plot = last_plot(),width = 10.5,height = 5,dpi = 400)

# ---- #
# Idea 2: Are more complex or comprehensive studies more cited?
o <- left_join(df, cit, by = c("newDOI" = "doi"))
# More complex if accounting for all factors
o$bin_landuse <- ifelse(o$Multiple.objectives.or.constraints=="None",0,1) |> factor()
o$bin_connect <- ifelse(o$Connectivity == "None", 0, 1) |> factor()
o$bin_cost <- ifelse(o$Costs == "no", 0, 1) |> factor()
o$bin_stake <- ifelse(o$Stakeholder.involvement=="no", 0, 1) |> factor()

# fit <- glmer(cite_scientific ~bin_landuse + bin_connect + bin_cost + bin_stake + (1|Year), data = o, family = poisson())
# fit <- glmer(cite_policy ~bin_landuse + bin_connect + bin_cost + bin_stake + (1|Year), data = o, family = poisson())
# summary(fit)
# plot(effects::allEffects(fit))

# Fit zero-inflated Bayesian Poisson regressions
fit1 <- brm(cite_scientific ~bin_landuse + bin_connect + bin_cost + bin_stake + (1|Year), data = o, family = zero_inflated_poisson(),backend = "cmdstanr")
fit2 <- brm(cite_policy~bin_landuse + bin_connect + bin_cost + bin_stake + (1|Year), data = o, family = zero_inflated_poisson(),backend = "cmdstanr")

# Get conditional effects
me1 <- conditional_effects(fit1,
                           re_formula = NA, # Include group level effects
                           prob = 0.95
)
me2 <- conditional_effects(fit2,
                           re_formula = NA, # Include group level effects
                           prob = 0.95
)

o <- bind_rows(
  bind_rows(me1$bin_landuse,me1$bin_connect,me1$bin_cost, me1$bin_stake) |>
    mutate(type = "Scientific literature"),
  bind_rows(me2$bin_landuse,me2$bin_connect,me2$bin_cost, me2$bin_stake) |>
    mutate(type = "Policy documents")
) |> dplyr::select(type, bin_landuse, bin_connect, bin_cost, bin_stake, estimate__, lower__, upper__) |>
  pivot_longer(cols = bin_landuse:bin_stake)
o$name <- factor(o$name, levels = c("bin_landuse","bin_connect","bin_cost","bin_stake"),
                 labels = c("Land use", "Connectivity", "Costs", "Stakeholders"))

# Save the partial results for later
saveRDS(o, "resSaves/samples_citationrates_complexity.rds")
o <- readRDS("resSaves/samples_citationrates_complexity.rds")

o <- o |> filter(value=="1")

ggplot(o, aes(x = name, y = estimate__, ymin = lower__, ymax = upper__)) +
  mytheme +
  geom_pointrange(size = 1.5, linewidth = 1.2,position = position_dodge(.5)) +
  scale_y_continuous(breaks = pretty_breaks(4)) +
  facet_wrap(~type,scales = "free_y") +
  theme(axis.text.x.bottom = element_text(size = 16)) +
  labs(x = "", y = "Relative citation rate")
ggsave("figures/test_citationrates_complexity.png",plot = last_plot(),width = 10.5, height = 5,dpi = 400)

# ---- #
# Idea 2: Are Local and national studies more likely to incorporate stakeholder feedback
o <- left_join(df, cit, by = c("newDOI" = "doi")) |> dplyr::select(Policy.relevance,Stakeholder.involvement,Extent, Year, cite_socialmedia:cite_policy)

# Fit zero-inflated Bayesian Poisson regressions
fit1 <- brm(Stakeholder.involvement ~ Extent, data = o, family = bernoulli(),backend = "cmdstanr")
fit2 <- brm(Stakeholder.involvement ~ Policy.relevance, data = o, family = bernoulli(),backend = "cmdstanr")

# Get conditional effects
me1 <- conditional_effects(fit1,
                           re_formula = NA, # Include group level effects
                           prob = 0.95
)
me2 <- conditional_effects(fit2,
                           re_formula = NA, # Include group level effects
                           prob = 0.95
)

o <- bind_rows(
  bind_rows(me1$Extent) |> rename(variable = Extent) |>
    mutate(type = "Scientific literature"),
  bind_rows(me2$Policy.relevance) |> rename(variable = Policy.relevance) |>
    mutate(type = "Policy documents")
)

# Save the partial results for later
saveRDS(o, "resSaves/samples_stakeholder.rds")
o <- readRDS("resSaves/samples_stakeholder.rds")

g1 <- ggplot(o |> filter(type == "Scientific literature"),
       aes(x = variable, y = estimate__, ymin = lower__, ymax = upper__)) +
  mytheme +
  geom_pointrange(size = 1.5, linewidth = 1.2,position = position_dodge(.5)) +
  scale_y_continuous(breaks = pretty_breaks(4)) +
  facet_wrap(~type, scales = "free_y",drop = TRUE) +
  theme(axis.text.x.bottom = element_text(size = 16)) +
  labs(x = "", y = "Probability of\ninvolvement of stakeholders")
g2 <- ggplot(o |> filter(type == "Policy documents"),
             aes(x = variable, y = estimate__, ymin = lower__, ymax = upper__)) +
  mytheme +
  geom_pointrange(size = 1.5, linewidth = 1.2,position = position_dodge(.5)) +
  scale_y_continuous(breaks = pretty_breaks(4)) +
  facet_wrap(~type, scales = "free_y",drop = TRUE) +
  theme(axis.text.x.bottom = element_text(size = 16)) +
  labs(x = "", y = "Probability of\n involvement of stakeholders")

pg <- cowplot::plot_grid(g2, g1 + labs(y=""), align = "h")
cowplot::ggsave2("figures/test_stakeholderinvolvement.png",plot = pg, width = 10.5, height = 5,dpi = 400)

# ---- #
# Idea 3: Are studies that are referring to specific policies also more cited in policy agendas?
o <- left_join(df, cit, by = c("newDOI" = "doi")) |> dplyr::select(Policy.relevance,Year, cite_socialmedia:cite_policy)

#fit <- glmer(cite_policy~Policy.relevance + (1|Year), data=o, family = poisson() )
#plot(effects::allEffects(fit))
fit1 <- brm(cite_policy~Policy.relevance, data=o, family = zero_inflated_poisson(),backend = "cmdstanr")
fit2 <- brm(cite_scientific~Policy.relevance, data=o, family = zero_inflated_poisson(),backend = "cmdstanr")

# Get conditional effects
me1 <- conditional_effects(fit1,
                           re_formula = NA, # Include group level effects
                           prob = 0.95
)
me2 <- conditional_effects(fit2,
                           re_formula = NA, # Include group level effects
                           prob = 0.95
)
o <- bind_rows(
  me1$Policy.relevance |> mutate(type = "Policy documents"),
  me2$Policy.relevance |> mutate(type = "Scientific literature")
)

# Save the partial results for later
saveRDS(o, "resSaves/samples_citationrates_policy.rds")
o <- readRDS("resSaves/samples_citationrates_policy.rds")

ggplot(o, aes(x = Policy.relevance, y = estimate__, ymin = lower__, ymax = upper__)) +
  mytheme +
  geom_pointrange(size = 1.5, linewidth = 1.2) +
  scale_y_continuous(breaks = pretty_breaks(4)) +
  facet_wrap(~type,scales = "free_y") +
  labs(x = "", y = "Relative citation rate")
ggsave("figures/test_citationrates_policy.png",plot = last_plot(),width = 10.5,height =5,dpi = 400)


# ---- #
