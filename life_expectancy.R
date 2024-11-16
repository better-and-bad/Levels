

rm(list=ls())

library(dplyr)
library(readr)
library(tsibble)
library(ggplot2)
library(tidyverse)
library(gapminder)
library(gganimate)
library(gifski)
library(transformr)

theme_cx <- function(){
  theme_light() +
    theme(
      axis.text = element_text(size = 14),
      axis.title.x = element_text(size = 14, margin = margin(t = unit(12, "pt"))),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels by 45 degrees
      axis.title.y = element_text(size = 14, margin = margin(r = unit(12, "pt"))),
      plot.title = element_text(size = 18, face = "bold", margin = margin(b = unit(10, "pt")), hjust = 0.5),
      plot.subtitle = element_text(size = 14, face = "bold", margin = margin(b = unit(10, "pt")), hjust = 0.5),
      legend.text = element_text(size = 16),  # Increase the size of legend text
      legend.title = element_blank()
    )
}

### Set the theme
theme_set(theme_cx())

### animated gdp & life expectacy plot ###
gap <- ggplot(gapminder) +
  geom_point(aes(gdpPercap, lifeExp, color = country, size = pop), alpha = 0.7) +
  labs(title = "Global Development", subtitle='Year: {frame_time}', 
       x = 'GDP per capita', y = 'Life Expectancy') +
  transition_time(year) +
  scale_size(range = c(2, 25)) +
  ease_aes('linear') +
  coord_cartesian(xlim = c(0, 60000)) +
  scale_x_continuous(labels = scales::dollar_format()) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size=15),
        axis.title = element_text(size=15),
        plot.title = element_text(size = 20, face="bold", hjust=0.5),  # Adjust the title font size here
        plot.subtitle = element_text(size = 18, hjust=0.5))

# Animate the plot and save it as a GIF
gap_animation <- animate(gap, nframes = 100)
animate(gap, renderer = ffmpeg_renderer())

#### STATE GDP PER CAPITA ####
stat_gdp <- read_csv("/Users/jackconnors/Downloads/Table (1).csv")
head(stat_gdp)

stat_gdp <- stat_gdp %>% 
  pivot_longer(cols=-GeoName, names_to = "Year", values_to = "gdp/capita") 

state_gdp <- stat_gdp %>% 
  filter(GeoName != "United States" & GeoName %in% c("Mississippi",
                                                     "West Virginia", "Arkansas", "Kentucky",
                                                     "New Mexico", "South Carolina",
                                                     "Idaho", "Alabama"))
### RENAME STATE NAME IN GDP/CAP ###
state_abbreviations <- c(
  "Alabama" = "AL",
  "Arkansas" = "AR",
  "Idaho" = "ID",
  "Kentucky" = "KY",
  "Mississippi" = "MS",
  "New Mexico" = "NM",
  "South Carolina" = "SC",
  "West Virginia" = "WV")

### RENAME STATE NAMES ###
state_gdp <- state_gdp %>%
  mutate(GeoName = state_abbreviations[GeoName])

### ALABAMA LIFE EXP ###
al_life <- read_csv("/Users/jackconnors/Downloads/AL_bltper_1x1.csv")

al_life <- al_life %>% 
  filter(Age=="0") %>% 
  rename(lifeExp = ex, country=PopName) %>% 
  select(country, Year, lifeExp)

### Arkansas LIFE EXP ###
ar_life <- read_csv("/Users/jackconnors/Downloads/AR_bltper_1x1.csv")

ar_life <- ar_life %>% 
  filter(Age=="0") %>% 
  rename(lifeExp = ex, country=PopName) %>% 
  select(country, Year, lifeExp)

### idaho life exp ###
id_life <- read_csv("/Users/jackconnors/Downloads/ID_bltper_1x1.csv")

id_life <- id_life %>% 
  filter(Age=="0") %>% 
  rename(lifeExp = ex, country=PopName) %>% 
  select(country, Year, lifeExp)
### kentucky ###
ky_life <- read_csv("/Users/jackconnors/Downloads/KY_bltper_1x1.csv")

ky_life <- ky_life %>% 
  filter(Age=="0") %>% 
  rename(lifeExp = ex, country=PopName) %>% 
  select(country, Year, lifeExp)
### miss ###
ms_life <- read_csv("/Users/jackconnors/Downloads/MS_bltper_1x1.csv")

ms_life <- ms_life %>% 
  filter(Age=="0") %>% 
  rename(lifeExp = ex, country=PopName) %>% 
  select(country, Year, lifeExp)
### new mexico ###
nm_life <- read_csv("/Users/jackconnors/Downloads/NM_bltper_1x1.csv")

nm_life <- nm_life %>% 
  filter(Age=="0") %>% 
  rename(lifeExp = ex, country=PopName) %>% 
  select(country, Year, lifeExp)
### SOUTH CAROLINA ###
sc_life <- read_csv("/Users/jackconnors/Downloads/SC_bltper_1x1.csv")

sc_life <- sc_life %>% 
  filter(Age=="0") %>% 
  rename(lifeExp = ex, country=PopName) %>% 
  select(country, Year, lifeExp)
### WEST VIRGINIA ###
wv_life <- read_csv("/Users/jackconnors/Downloads/WV_bltper_1x1.csv")

wv_life <- wv_life %>% 
  filter(Age=="0") %>% 
  rename(lifeExp = ex, country=PopName) %>% 
  select(country, Year, lifeExp)
### MERGE LIFE EXPECTANCY DATAFRAMES ###
state_life <- bind_rows(sc_life, wv_life, nm_life, ms_life, ky_life, id_life, ar_life, al_life)

state_life <- state_life %>% 
  mutate(country = ifelse(country == "Mississippi", "MS", country))

### merge GDP w life exp data frame ###
state_gdp <- state_gdp %>% 
  rename(country=GeoName, gdpPercap=`gdp/capita`, year=Year)

state_life <- state_life %>% rename(year=Year)

states_data <- merge(state_life, state_gdp, by=c("year", "country")) %>% 
  arrange(country) 

write_csv(states_data, "states_data.csv")

### FINAL MERGE FOR STATES GDP & LIFE ###

### GAP STATES ###
gap_states <- read_csv("/Users/jackconnors/Downloads/Book1.csv")
head(gap_states)

# Create a subset of gap_states with the selected countries
countries_to_plot <- c("United States", "MS", "WV", "SC", "NM", "MS",
                       "KY", "ID", "AR", "AL")

# Create a subset of gap_states with the selected countries
subset_gap_states <- gap_states %>% filter(country %in% countries_to_plot)

# Create the animated plot
library(gganimate)

gap_states_year <- gap_states %>% 
  mutate(year=as.Date(as.character(year), format="%Y")) 

head(gap_states)

gap_state_animate <- ggplot(gap_states) +
  geom_point(aes(gdpPercap, lifeExp, color = country, size = pop), alpha = 0.7) +
  labs(title = "Differences are Within, not Between Countries", 
       x = 'GDP per capita', y = 'Life Expectancy', subtitle = "{floor(frame_time)}",
       caption="Source: gapminder, mortality.org, BEA.gov") +
  transition_time(year) +
  scale_size(range = c(2, 25)) +
  ease_aes('linear') +
  coord_cartesian(xlim = c(0, 60000)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 20),  # Adjust the title font size here
        plot.subtitle = element_text(size = 18))

gap_states_final <- gap_state_animate +
  geom_text(data = subset_gap_states, aes(x = gdpPercap, y = lifeExp, label = country), nudge_x = 1000, nudge_y = 3)

# Animate the plot and save it as a GIF
anim_save("gap_states_final.gif", gap_states_final)

gap_animation <- animate(gap_states_final, nframes = 100)
animate(gap_states_final, renderer = ffmpeg_renderer())
