

###
####

regions <- readxl::read_excel("/Users/jackconnors/citizenssalon/country_income_mnts.xlsx", 
                              sheet="Data regions year", skip=1)

regions <- regions %>% 
  mutate(year = as.Date(paste0(year, "-01-01"),format="%Y-%m-%d"))

regions <- regions%>% 
  pivot_longer(cols=starts_with("Share of people on Level"),
               values_to="Share", names_to = "Level")

regions <- subset(regions, Level != "Share of people on Level 4 or above")

regions <- regions %>% 
  filter(year >= as.Date("1950-01-01") & 
           year <= as.Date("2019-01-01"))

regions %>% 
  dplyr::select(Level, Share, geo) %>% head()

regions_mnts <- ggplot(regions, aes(Level, Share, fill=geo)) +
  geom_bar(aes(y=Share), stat="identity", position="stack") +
  transition_time(year) +
  scale_x_discrete(breaks = 1:5, labels = 1:5) +
  ease_aes('linear') +
  labs(title = "Levels per Region",
       x = "Levels", y = "",
       subtitle = 'Year: {frame_time}') + 
  theme_minimal()  +
  scale_fill_brewer(palette = "Set2") +  # Use a color palette like Set2 for better distinction
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 20),
    axis.title = element_text(size = 20, face="bold"),
    legend.title = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 22, hjust = 0.5, face = "bold"),
    plot.caption = element_text(size = 12)
  )

anim_save("regions_mnts.gif", regions_mnts)

# Convert Level column to a factor with ordered levels
regions$Level <- factor(regions$Level, levels = unique(regions$Level), ordered = TRUE)

# Now, create the ggplot with the updated regions dataframe
library(gganimate)
library(ggplot2)
library(dplyr)
install.packages("tweenr")
head(regions)

regions <- regions %>% 
  mutate(geo = case_when(
    geo == "afr" ~ "Africa",
    geo == "ame" ~ "Americas",
    geo == "asi" ~ "Asia",
    geo == "eur" ~ "Europe",
    TRUE ~ geo
  ))

regions_mnts <- ggplot(regions, aes(Level, Share, fill = geo)) +
  geom_bar(aes(y = Share), stat = "identity", position = "stack") +
  gganimate::transition_states(year, transition_length = 2, state_length = 1) +
  ease_aes('linear') +
  labs(
    title = "Levels per Region",
    x = "Levels",
    y = "Share",
    subtitle = 'Year: {closest_state}'
  ) + 
  theme_bw() +
  theme(
    axis.title.y = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    plot.title = element_text(size = 20, hjust=0.5),
    plot.subtitle = element_text(size=16, hjust=0.5),
    axis.text.x=element_blank(),
    axis.text.y=element_text(size = 16),
    legend.text = element_text(size=20)
  ) 


gap_animation <- animate(regions_mnts, nframes = 100)
animate(regions_mnts, renderer = ffmpeg_renderer())
save_animation(gap_animation, "gap_animation.gif")