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
mytheme <- theme_gray(base_size = 20) +
  theme(
    panel.background = element_rect(#fill = "transparent",
                                    colour = NA_character_), # necessary to avoid drawing panel outline
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_), # necessary to avoid drawing plot outline
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
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

# Number of studies per Realm
table(df$Realm); table(df$Realm)/nrow(df)

# Number of studies with different planning purpose
table(df$Planning.purpose); table(df$Planning.purpose) / nrow(df)

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
  geom_smooth(size = 1.5, se = FALSE,method = "loess") +
  scale_y_continuous(breaks = pretty_breaks(10)) +
  scale_colour_aaas() +
  theme(legend.position = c(.2,.75)) +
  labs(y = "Number of studies", x = "")
ggsave(filename = "figures/exploratory_timeseries.png", width = 8, height = 8, dpi = 400)

# Realm
o <- df |> group_by(Realm) |> summarise(N = n())
o$Realm <- factor(o$Realm, levels = c("Terrestrial", "Marine", "Freshwater", "Cross-realm"))

ggplot(o, aes(x = Realm, y = N)) +
  mytheme +
  geom_bar(stat = "identity", fill = "grey20",show.legend = FALSE) +
  scale_y_continuous(expand = c(0,Inf)) +
  labs(x = "", y = "Number of studies")
ggsave(filename = "figures/exploratory_realms.png", width = 7, height = 6, dpi = 400)

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
  geom_point() +
  scale_size_binned(guide = guide_legend("Number of studies")) +
  scale_colour_gradientn(colours = scico(50, palette = "roma",direction = -1),
                         guide = guide_colorbar("Proportion", barwidth = unit(3,'in'), title.vjust = .75)) +
  theme(legend.position = "bottom", legend.box = "horizontal",legend.box.background = element_rect(color = NA)) +
  labs(x = "", y = "") +
  facet_wrap(~type,scales = "free_y",nrow = 2) +
  theme(axis.text.x.bottom = element_text(hjust = 1, angle = 45)) +
  # Reduce distance between y-axis
  theme(plot.title = element_text(hjust = 0.5))
ggsave("figures/exploratory_bubble.png",width = 10,height = 10,dpi = 400)

#### Specific plots and hypotheses ####

# ---- #
# Idea 1: Are studies of larger study extent more often cited in research and policy contexts
o <- left_join(df, cit, by = c("newDOI" = "doi")) |> select(Extent, cite_socialmedia:cite_policy)

# Test
cor.test(o$cite_socialmedia, o$cite_scientific) # Social media does not explain citation rate
cor.test(o$cite_socialmedia, o$cite_policy) # Also not in policy documents
cor.test(o$cite_scientific, o$cite_policy) # However more widely cited papers are more likely to be also cited by policy
glm(o$cite_scientific~o$Extent) |> summary() # Scientifically, no difference
glm(o$cite_policy~o$Extent) |> summary() # European -wide study are slightly more likely to be cited
glm(o$cite_socialmedia~o$Extent) |> summary()
plot(effects::allEffects(glm(cite_policy~Extent,data=o,family = "poisson")))#
plot(pdp::partial(glm(cite_policy~Extent,data=o),"Extent"))

fit <- inla(cite_policy~Extent, data=o,family = "zeroinflatedpoisson1",
            quantiles =  c(.05,.5, .95),control.predictor = list(compute=TRUE))
fit <- inla(cite_scientific~Extent, data=o,family = "poisson",quantiles =  c(.05,.5, .95))
fit <- inla(cite_socialmedia~Extent, data=o,family = "poisson",quantiles =  c(.05,.5, .95))
summary(fit)

# ---- #
# Idea 2: Are Local and national studies more likely to incorporate stakeholder feedback


# ---- #
# Idea 3: Are studies that are refeering to specific policies also more cited in policy agendas?
o <- left_join(df, cit, by = c("newDOI" = "doi")) |> select(Policy.relevance, cite_socialmedia:cite_policy)

glm(o$cite_policy~o$Policy.relevance, family = "poisson") |> summary() # European -wide study are slightly more likely to be cited
fit <- brm(cite_policy~Policy.relevance, data=o, family = zero_inflated_poisson(),backend = "cmdstanr")
brms::marginal_effects(fit)
#plot(effects::allEffects(glm(cite_policy~Policy.relevance,data=o,family = "poisson")), lines = T)#

# ---- #
# Idea 4: Does an involvement of stakeholders increase the number of citations (scientific and policy-wise)
o <- left_join(df, cit, by = c("newDOI" = "doi")) #|> select(Extent,Year,Planning.purpose, Stakeholder.involvement, cite_scientific:cite_policy)

fit <- glmer(cite_scientific ~ Stakeholder.involvement + Planning.purpose + (1|Year), data =o, family = poisson())
summary(fit)
plot(effects::allEffects(fit))

# ---- #
# Idea 5: Management actions are usually ecosystem-specific and local scale


