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

#frequency of "MIRA" (row 100) in year 2016 is manually changed from 164 to 165, because there is also
# Marlene with 164 usages in 2016, and the barplot is therefore overlapping for those names
# same with "Anna" and "Emma", Emma is changed from 305 to 306
gnames[100, "freq"] <- 165
gnames[87, "freq"] <-306

#reformat gnames 
gnames_formatted <- gnames %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-freq),
         Value_rel = freq/freq[rank==1],
         Value_lbl = freq) %>%
  group_by(Vorname) %>% 
  filter(rank <=20) %>%
  ungroup()

# round rank - i don't know if the digits were a problem
gnames_formatted$rank<-round(gnames_formatted$rank,digits=0)

# Animation!!
ganim <- ggplot(gnames_formatted, aes(rank, group = Vorname, 
                fill = as.factor(Vorname), color = as.factor(Vorname))) +
  geom_tile(aes(y = freq/2,
                height = freq,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Vorname, " ")), vjust = 0.2, hjust = 1, fontface="bold", size=7) +
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
        plot.caption =element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
       plot.margin = margin(2,2, 2, 4, "cm")) +
  transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'GIRL NAMES in Berlin, for year : {closest_state}',  
       subtitle  =  "20 most frequent names",
       caption  = "frequency of first names in Berlin | Data Source: https://daten.berlin.de/ | animation plot: github.com/amrrs | names plot: github.com/hhnnhh") 

# For GIF
#install.packages("gifski")
#library(gifski)
#install.packages("png")
#library(png)

#I changed the number of frames from 200 to 400 to make the chart slower
animate(ganim, 400, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim_girlname.gif")) 

# For MP4 --> not working yet 11.12.2019

# devtools::install_github("leonawicz/mapmate")
# library(mapmate)
# 
# animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
#         renderer = ffmpeg_renderer()) -> for_mp4
# 
# anim_save("animation.mp4", animation = for_mp4 )

write.csv(bnames,"boys2012_2018.csv")
write.csv(gnames,"girls2012_2018.csv")
