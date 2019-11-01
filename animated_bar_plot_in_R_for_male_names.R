library(tidyverse)
library(gganimate)

## --> needed for the nice design "theme_tufte" = optional
#library(extrafont)
#library(ggthemes)
## --> for rendering the GIF
#library(gifski)
#library(png)

setwd("C:/Users/hanna/Dropbox/R_wissen/animated_bar_charts_in_R/data/")
list.files()
getwd()

setwd("./mostfrequentnames/")

#### GIRLS FIRST
myfiles<-list.files(pattern = '^boys')

library(dplyr)
library(readr)
# combine all CSV starting with "girls" to one dataframe named "gnames"
bnames <- list.files(pattern = '^boys') %>% 
  lapply(read_csv) %>% 
  bind_rows 

#rename the column x with freq for "frequency"
names(bnames)[names(bnames)=='x'] <- 'freq'

test<-bnames %>% group_by(year) %>%
  filter(duplicated(freq))

# Ben & David 2012, freq=216; Ben & Oskar 2013, 209;
# Benjamin & Louis, 2013, 177; Jakob & Karl 201, 2014;
# Felix & Oskar, 2014, 255; Anton & Felix, 2015, 283
# Jonathan & Theodor, 2017, 196; Jakob & Luca, 2018, 204
bnames[11, "freq"]<-220
bnames[33, "freq"]<-210
bnames[39, "freq"]<-178
bnames[58, "freq"]<-200
bnames[71, "freq"]<-244
bnames[88, "freq"]<-282
bnames[119, "freq"]<-196
bnames[138, "freq"]<-205

names_formatted <- bnames %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-freq),
         Value_rel = freq/freq[rank==1],
         Value_lbl = freq) %>%
  group_by(Vorname) %>% 
  filter(rank <=20) %>%
  ungroup()

#I don't know if "rank" must be rounded, but I tried anyway
names_formatted$rank<-round(names_formatted$rank,digits=0)

# Animation
banim <- ggplot(names_formatted, aes(rank, group = Vorname, 
                fill = as.factor(Vorname), color = as.factor(Vorname))) +
  geom_tile(aes(y = freq/2,
                height = freq,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Vorname, " ")), vjust = 0.2, hjust = 1, fontface="bold",size=7) +
  geom_text(aes(y=freq,label = Value_lbl,hjust=0)) +
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
        plot.caption =element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
       plot.margin = margin(2,2, 2, 4, "cm")) +
  transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'BOY NAMES in Berlin, year : {closest_state}',  
       subtitle  =  "20 most frequent names",
       caption  = "frequency of first names in Berlin | Data Source: https://daten.berlin.de/ | animation plot: github.com/amrrs | names plot: github.com/hhnnhh") 

# For GIF


animate(banim, 400, fps = 20,  width = 1200, height = 1000, end_pause = 30,
        renderer = gifski_renderer("gganim_boyname.gif")) 

# For MP4
devtools::install_github("leonawicz/mapmate")
library(mapmate)

animate(anim, 200, fps = 20,  width = 1200, height = 1000, end_pause=50,
        renderer = ffmpeg_renderer()) -> for_mp4

anim_save("animation.mp4", animation = for_mp4 )
