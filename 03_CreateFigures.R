library(tidyverse)
library(tidyr)
library(readxl)
library(ggplot2)
library(ggthemes)
library(scales)
library(scico)
library(ggsci)
library(tmap)
# Default theme and design
mytheme <- theme_gray(base_size = 20) +
  theme(
    panel.background = element_rect(#fill = "transparent",
                                    colour = NA_character_), # necessary to avoid drawing panel outline
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_), # necessary to avoid drawing plot outline
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent")
  )
# --- #
# Load in formatted literature extract
df <- read_xlsx("LiteratureExtracts/20220923_FilteredLiteratureFinal.xlsx",sheet = 1) |>
  filter(Suitable == 1)

# Load in formatted citation data
cit <- readRDS("resSaves/citation_extraction_formatted.rds")

# Join

dir.create("figures", showWarnings = FALSE)
# ----------- #
#### Basic statistics ####


# ----------- #
#### Basic plots ####

# Number of papers per year
o <- df |> group_by(Extent, Year) |> summarise(N = n()) |> arrange(Year)

ggplot(o, aes(x = Year, y = N, colour = Extent)) +
  mytheme +
  # geom_line(size = 1.5) +
  geom_smooth(size = 1.5, se = FALSE,method = "loess") +
  scale_y_continuous(breaks = pretty_breaks(10)) +
  scale_colour_aaas() +
  theme(legend.position = c(.2,.75)) +
  labs(y = "Number of studies", x = "")

# Realm
o <- df |> group_by(Realm) |> summarise(N = n())
o$Realm <- factor(o$Realm, levels = c("Terrestrial", "Marine", "Freshwater", "Cross-realm"))

ggplot(o, aes(x = Realm, y = N, fill = Realm)) +
  mytheme +
  geom_bar(stat = "identity",show.legend = FALSE) +
  scale_fill_colorblind() +
  labs(x = "", y = "Number of studies")

# How often are stakeholder considered
o <- df |> group_by()
table(df$`Stakeholder involvement`)

# Planning purpose
o <- df |> drop_na("Planning purpose") |>
  group_by("Planning purpose") |> summarise(N = n())

table(df$`Planning purpose`)

# How is land-use considered
o <- df |> group_by(`Biodiversity type`)
ggplot(df, aes(x=`Biodiversity type`, y = `Number of features`)) +
  geom_point()

#### Species plots and hypotheses ####
# Idea 1: Are studies of larger study extent more often cited in research and policy contexts

# Idea 2: Are Local and national studies more likely to incorporate stakeholder feedback

# Idea 3: Are studies that are refeering to specific policies also more cited in policy agendas?

# Idea 4: Very few studies across scales account for connectivity, land-use and stakeholders
# - Simply assess statistics here and report
