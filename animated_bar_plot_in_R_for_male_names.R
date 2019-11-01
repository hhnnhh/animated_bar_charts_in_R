library(tidyverse)
library(gganimate)

setwd("C:/Users/hanna/Dropbox/R_wissen/animated_bar_charts_in_R/data/")
list.files()
getwd()

setwd("./mostfrequentnames/")

#### GIRLS FIRST
myfiles<-list.files(pattern = '^girls')

library(dplyr)
library(readr)
# combine all CSV starting with "girls" to one dataframe named "gnames"
gnames <- list.files(pattern = '^girls') %>% 
  lapply(read_csv) %>% 
  bind_rows 

#rename the column x with freq for "frequency"
names(gnames)[names(gnames)=='x'] <- 'freq'

names_formatted <- gnames %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-freq),
         Value_rel = freq/freq[rank==1],
         Value_lbl = freq) %>%
  group_by(Vorname) %>% 
  filter(rank <=20) %>%
  ungroup()

names_formatted$rank<-round(names_formatted$rank,digits=0)

# Animation
is.numeric(names_formatted$rank)
is.numeric(gdp_tidy$rank)

anim <- ggplot(names_formatted, aes(rank, group = Vorname, 
                fill = as.factor(Vorname), color = as.factor(Vorname))) +
  geom_tile(aes(y = freq/2,
                height = freq,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Vorname, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=freq,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
         axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
       plot.margin = margin(2,2, 2, 4, "cm")) +
  transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Names frequency for year : {closest_state}',  
       subtitle  =  "Top 20 Names",
       caption  = "frequency of first names in Berlin | Data Source: https://daten.berlin.de/") 


# anim1 <- ggplot(gnames, aes(Vorname, freq, group = year)) +
#   geom_point() +
#   labs(title = "{closest_state}") +
#   transition_states(Vorname, transition_length = 3, state_length = 1) +
#   enter_fade() +
#   exit_fade()



# For GIF
install.packages("gifski")
library(gifski)
install.packages("png")
library(png)

animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim_name.gif")) 

# For MP4
devtools::install_github("leonawicz/mapmate")
library(mapmate)

animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = ffmpeg_renderer()) -> for_mp4

anim_save("animation.mp4", animation = for_mp4 )
