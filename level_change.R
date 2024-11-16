
### distinctions between the binary world of 1960 and the convergence to 2007 
gapminder %>% 
  mutate(year = as.Date(paste0(year, "-01-01"), format="%Y-%m-%d")) %>% 
  filter(year == as.Date("2007-01-01")) %>% 
  filter(lifeExp < 50 & gdpPercap < 5000) %>% 
  count(n_distinct(country))

# Animate the plot and save it as a GIF
gap_animation <- animate(gap, nframes = 100)
animate(gap, renderer = ffmpeg_renderer())

#anim_save("gap.gif", gap)

### when did we have a binary world??
### in 1962 90 countries had a life expectancy below 60.
### 43 countries in 2007
### 
gapminder %>% 
  filter(year==2007) %>% 
  arrange(gdpPercap) %>% 
  head()
summary()
summarize(life = mean(lifeExp),
          gdp = mean(gdpPercap))
count(country) %>% 
  summarize(total = n_distinct(country))

####
grey_columns <- data.frame(xmin = c(2500, 8000, 25000), xmax = c(2500, 8000, 25000), ymin = -Inf, ymax = Inf)

income_mnts <- ggplot(gapminder) +
  geom_density(aes(x = gdpPercap, fill = continent), alpha = 0.5) +
  transition_time(year) +
  scale_size(range = c(2, 25)) +
  ease_aes('linear') +
  labs(title = "Density Plot of GDP per Capita by Continent",
       x = "GDP per Capita", y = "Density", subtitle = 'Year: {frame_time}') +
  geom_vline(xintercept = c(2500, 8000, 25000), color = "black", linetype = "dashed") +
  geom_text(aes(x=700, y=0.0004), label="Level 1", size=4) +
  geom_text(aes(x=5000, y=0.0004), label="Level 2", size=4) +
  geom_text(aes(x=18000, y=0.0004), label="Level 3", size=4) +
  geom_text(aes(x=30000, y=0.0004), label="Level 4", size=4) +
  coord_cartesian(ylim = c(0, 0.00045), xlim = c(0, 35000)) +
  labs(title = "GDP per Capita", x = "GDP per Capita", y = "Density", subtitle = 'Year: {frame_time}') +
  theme_minimal() +
  theme(axis.text = element_text(size=15),
        axis.title = element_text(size=15),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 18)) + 
  theme_minimal() +
  enter_fade()

##### 
double_log_breaks <- function(min_val, max_val, num_ticks) {
  log_min <- log10(min_val)
  log_max <- log10(max_val)
  log_breaks <- seq(log_min, log_max, length.out = num_ticks)
  breaks <- 10^log_breaks
  labels <- as.character(breaks)
  list(breaks = breaks, labels = labels)}

min_gdp <- min(gapminder$gdpPercap)
max_gdp <- max(gapminder$gdpPercap)

num_ticks <- 7

gapminder %>%
  ggplot() +
  geom_density(aes(x = gdpPercap, fill = continent), alpha = 0.5) +
  transition_time(year) +
  scale_size(range = c(2, 25)) +
  ease_aes('linear') +
  coord_cartesian(ylim = c(0, 2)) +
  labs(
    title = "Density Plot of GDP per Capita by Continent",
    x = "GDP per Capita",
    y = "Density",
    subtitle = 'Year: {frame_time}'
  ) +
  scale_x_continuous(
    breaks = double_log_breaks(min_gdp, max_gdp, num_ticks)$breaks,
    labels = double_log_breaks(min_gdp, max_gdp, num_ticks)$labels
  ) +
  theme_minimal()

####
levels_ts <- levels %>%
  dplyr::mutate(year= as.Date(paste0(year, "-01-01"), format="%Y-%m-%d")) %>% 
  as_tsibble(index=year, key=geo) 

levels_ts %>% 
  ggplot() +
  geom_col(aes(y=`Share of people on Level 1`, x=geo, fill=geo)) +
  geom_col(aes(y=`Share of people on Level 2`, x=geo, fill=geo)) +
  geom_col(aes(y=`Share of people on Level 3`, x=geo, fill=geo)) +
  geom_col(aes(y=`Share of people on Level 4`, x=geo, fill=geo)) +
  transition_time(year) +
  labs(title = "4 Levels", subtitle='year: {frame_time}', 
       x = 'Continent', y = '% of Pop') +
  scale_size(range = c(2, 25)) +
  coord_cartesian(ylim=c(0,1)) +
  ease_aes('linear') +
  theme_minimal()

filtered_data <- levels_ts %>% 
  filter(year == c("1960", "2007") & name %in% c("Africa", "Asia"))

levels_ts %>% 
  filter(year %in% c("1995-01-01", "2007-01-01") & name %in% c("Africa", "Asia")) %>%  
  select(year, name, `Share of people on Level 1`) %>% 
  ggplot() +
  geom_point(aes(year, `Share of people on Level 1`, color=name)) +
  geom_line(aes(year, `Share of people on Level 1`, color=name)) +
  scale_y_continuous(breaks = c(0.1, 0.2, 0.3, 0.4, 0.5)) +
  theme_minimal()
glimpse(country_mnts)

### REGIONAL LEVEL DATA ### 
world_mnts <- readxl::read_excel("/Users/jackconnors/citizenssalon/country_income_mnts.xlsx", 
                                 sheet="Data regions year", skip=1)

world_mnts <- world_mnts %>% 
  mutate(year = as.Date(paste0(year, "-01-01"),format="%Y-%m-%d"))

regional_num_per_level <- world_mnts %>%
  filter(year %in% c("1960-01-01", "2019-01-01")) %>%
  group_by(name, year, .groups = "drop") %>%
  summarize(
    num_ppl = `Share of people on Level 4` * population,
    `Share of people on Level 4` = sum(`Share of people on Level 4`),
    population = sum(population)
  ) %>%
  select(name, year, num_ppl, `Share of people on Level 4`, population)

regional_num_per_level %>% 
  group_by(name) %>% 
  mutate(upgrade = num_ppl - lag(num_ppl)) %>% 
  select(name, year, upgrade)

regional_num_per_level %>% 
  group_by(name) %>% 
  ggplot(aes(as.factor(year), `Share of people on Level 4`, fill=name)) +
  geom_col(position="stack")

### GLOBAL LEVEL DATA ON LEVELS ###
levels <- readxl::read_excel("/Users/jackconnors/citizenssalon/country_income_mnts.xlsx", 
                             sheet="Data regions year", skip=1)


global_mnts <- global_mnts %>% 
  mutate(year = as.Date(paste0(year, "-01-01"),format="%Y-%m-%d"))

### how many people lived in extreme poverty in 1960?
global_mnts %>% 
  filter(year == as.Date("1960-01-01")) %>% 
  select(year, `Number of people on Level 1`)
### COUNTRY LEVEL DATA ###
library(readr)
country_mnts <- readxl::read_excel("/Users/jackconnors/citizenssalon/country_income_mnts.xlsx", 
                                   sheet="Data countries year", skip=1)

country_mnts <- country_mnts %>% 
  mutate(year = as.Date(paste0(year, "-01-01"),format="%Y-%m-%d"))

### us distribution of levels over 70 years
us_data <- country_mnts %>%
  filter(year %in% c("1960-01-01", "2019-01-01"), name == "United States")

# Pivot the data
us_data_long <- us_data %>%
  pivot_longer(cols = starts_with("Share of People on Level"),
               names_to = "Level",
               values_to = "Share")

us_data_long <- us_data_long %>% 
  filter(Level != "Share of people on Level 4 or above") %>%
  select(Level, year, Share)

### plot distribution of levels
ggplot(us_data_long, aes(Level, Share)) +
  geom_col() +
  facet_wrap(~year)

### annual change in people on level 3
country_mnts %>% 
  filter(name == "United States", between(year, as.Date("1960-01-01"), as.Date("2019-01-01"))) %>% 
  ggplot() +
  geom_line(aes(year, `Number of people on Level 3`), color="green") +
  scale_y_continuous(labels = scales::label_number()) +
  labs(x = "Year", 
       y = "Number of People on Level 3 (in thousands)")

### good story or bad story ??
# Filter the data for the United States for the years 2008 and 2014
us_data <- us_data_long %>% 
  filter(name == "United States", year %in% as.Date(c("2008-01-01", "2014-01-01")))

# Calculate the changes in the number of people on Levels 3, 4, and 2
change_level_3 <- us_data %>%
  filter(Level == "Share of people on Level 3") %>%
  summarise(change_level_3 = diff(`Number of people on Level 3`))

change_level_4 <- us_data %>%
  filter(Level == "Share of people on Level 4") %>%
  summarise(change_level_4 = diff(`Number of people on Level 4`))

change_level_2 <- us_data %>%
  filter(Level == "Share of people on Level 2") %>%
  summarise(change_level_2 = diff(`Number of people on Level 2`))

# Compare the changes
if (change_level_3$change_level_3 > 0 && change_level_4$change_level_4 < 0) {
  cat("It's likely that people fell from Level 4 to Level 3.\n")
} else if (change_level_2$change_level_2 > 0) {
  cat("There might be people moving up from Level 2 to Level 3.\n")
} else {
  cat("Further analysis may be needed to determine the cause of the spike in Level 3.\n")
}


#### fertility and child mortality ####
fert <- readxl::read_excel("/Users/jackconnors/Downloads/children_per_woman_total_fertility.xlsx")  
mortality <- readxl::read_excel("/Users/jackconnors/Downloads/child_mortality_0_5_year_olds_dying_per_1000_born.xlsx")


mortality <- mortality %>% 
  pivot_longer(cols=-country,
               names_to= "year",
               values_to="mortality")
mortality <- mortality %>% 
  mutate(year = as.Date(paste0(year, "-01-01"), format="%Y-%m-%d"))

fert <- fert %>%
  pivot_longer(cols = -country, 
               names_to = "year",
               values_to = "fertility")
fert <- fert %>% 
  mutate(year = as.Date(paste0(year, "-01-01"), format="%Y-%m-%d"))


country_mnts$country <- country_mnts$name

ex_pov <- merge(mortality, fert, by=c("year", "country"))

country_mnts <- country_mnts %>% 
  mutate(country = case_when(country=="United States" ~ "USA",
                             T ~ country))

ex_pov <- merge(ex_pov, country_mnts, by=c("year", "country"))

ex_pov_select <- ex_pov %>% 
  mutate(per_10 = mortality / 100) %>% 
  select(year, country, per_10, fertility, population)

ex_pov_select %>% 
  filter(year %in% c("1960-01-01", "2019-01-01")) %>% 
  ggplot(aes(fertility, per_10, size=population*6*1.5, color=country)) +
  geom_point() +
  theme_bw() +
  labs(title = "Breaking the Binary", y="Mortality per 10 Births", 
       x="Babies per Woman", caption="Source: Gapminder") +
  theme(legend.position = "none",
        plot.title = element_text(size = 18, hjust=0.5),
        axis.title = element_text(size=16),
        axis.text = element_text(size=16),
        strip.text = element_text(size=16)) +
  facet_wrap(~year) +
  scale_y_reverse() +
  scale_x_reverse() +
  scale_size(range = c(2, 15.5))

### gganimate
library(gganimate)
ex_pov_select <- ex_pov_select %>% 
  mutate(per_10 = mortality/100)

ex_pov_arrange <- ex_pov_select %>%
  arrange(desc(fertility))

head(ex_pov_select)

ex_pov_select <- ex_pov_select %>% 
  mutate(year = as.Date(paste0(year, "-01-01"), format="%Y-%m-%d")) %>% 
  as_tsibble(index=year, key=country)

### BUBBLE GRAPH FERTILITY & CHILD MORTALITY ###
c <- ex_pov_select %>% 
  filter(year > as.Date("1960-01-01") & year < as.Date("2019-01-01")) %>% 
  ggplot(aes(-fertility, -per_10, size=population, color=country), lwd=1.5) +
  geom_point() +
  transition_time(year) +
  scale_size(range = c(2, 25)) +
  ease_aes('linear') +
  theme(legend.position = "none") +
  labs(title = "Breaking the Binary", subtitle="Year: {frame_time}", y="Child Mortality per 10 Births",
       x="Babies per Woman") +
  theme_minimal() +
  scale_x_continuous(labels = function(x) abs(x)) + 
  scale_y_continuous(labels = function(y) abs(y)) + 
  theme(legend.position = "none",
        axis.text = element_text(size=20, face="bold"),
        axis.title = element_text(size=20, face="bold"),
        plot.title = element_text(size = 25, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=22, hjust=0.5, face="bold"))

anim_save("breaking_binary.gif", breaking_binary)

breaking_binary_animation <- animate(breaking_binary, nframes = 100)
animate(breaking_binary_animation, renderer = ffmpeg_renderer())

### end fertility and mortality plotting ###
write_csv(country_2018, "country_2018.csv")

bricsu <- country_mnts %>% 
  filter(name %in% c("Brazil", "India", "China", "United States")) %>%
  filter(year=="2017-01-01")

### pivot longer ###

# Reshape the data into a longer format
bricsu_long <- bricsu %>%
  pivot_longer(cols = starts_with("Share of people on Level"), 
               names_to = "Level", values_to = "Share")

# Create a grouped bar plot with facet_wrap
bricsu_long %>%
  ggplot(aes(x = Level, y = Share, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Share of People on Different Levels by Country",
       subtitle="2017",
       x = "Level", y = "Share") +
  facet_wrap(~name, scales = "free_y", ncol = 2) +
  theme_minimal() +
  theme(axis.text.x = element_blank()) 

### countries fastest 1 --> 2 ###
growth <- country_mnts %>% 
  filter(year %in% c("1960-01-01", "2017-01-01"))

top_growth <- growth %>% 
  group_by(name) %>% 
  summarize(change = `Share of people on Level 2` - `Share of people on Level 1`) %>% 
  arrange(desc(change))

world <- readxl::read_excel("/Users/jackconnors/Downloads/country_income_mnts.xlsx", 
                            sheet="Data world year", skip=1)

world <- world %>% 
  mutate(year = as.Date(paste0(year, "-01-01"),format="%Y-%m-%d"))

world_change <- world %>% 
  filter(year %in% c("1960-01-01", "2017-01-01"))

world_change <- world_change %>% 
  pivot_longer(cols=starts_with("Share of people on Level"),
               names_to="Level", values_to = "Share")

world_change %>% 
  ggplot(aes(Level, Share, fill=year)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~year) +
  theme_minimal() +
  theme(axis.text.x = element_blank()) 


### asian growth
top_growth_asia <- growth %>% 
  filter(name %in% c("Japan", "Bangladesh", "Japan",
                     "Pakistan", "Indonesia"))

asia_growth <- top_growth_asia %>% 
  pivot_longer(cols=starts_with("Share of people on Level"),
               names_to="Level", values_to = "Share")

asia_growthh <- asia_growth %>% mutate(year = as.integer(format(year, "%Y")))  # Extract only the year part as integer

ggplot(asia_growthh, aes(x = Level, y = Share, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Levels", y = "", caption="Source:Gapminder",
       title="Asia's Glow Up") +
  facet_wrap(name~year, scales = "free", ncol = 2) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        strip.text = element_text(size = 18, face="bold"),
        axis.text.y = element_text(size=14),
        legend.position = "none",
        plot.title = element_text(size=20, hjust=0.5, face="bold"))