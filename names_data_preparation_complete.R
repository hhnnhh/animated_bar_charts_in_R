## Hannah Bohle, 1.11.2019

## This script combines all csv from one year into one .Rdata file
## and calculates the 20 most frequent names for (a) girls and (b) boys in this year
## into to different data frames and saves them as .csv into a new folder ("newdata")
## --> results for all years can finally be found in the folder "mostfrequentnames"


### LoAD NAME DATA from https://daten.berlin.de/ into folder
## I have a folder for every year with csv for every area
## (can apparently also be done with API from github, but I didn't test it)
## check it out if you want to: 
## AND PREPARE IT


# 1. set working directory
# 2. go to folder where the csv-files are of one year
# 3. either load the existing dataset name.Rdata or create it from the csv: 
# a. create a list of all csv
# b.combine the csv while keeping the name as the header


setwd("C:/Users/hanna/Dropbox/R_wissen/animated_bar_charts_in_R/data/")
list.files()
getwd()
setwd("./namen_berlin_2018/")
#setwd("./namen_berlin_2017/")
#setwd("./namen_berlin_2016/")
#setwd("./namen_berlin_2015/")
#setwd("./namen_berlin_2014/")
#setwd("./namen_berlin_2012/")
getwd()
list.files()

# get all the csv-files in the working directory
# !! make sure its only the files you need!! otherwise you'll get  messed up
# sometimes the "seperator" must be changed from comma to semicolon or vice versa

if(file.exists("firstname.Rdata")) # doesn't load? not fixed yet.. :(
  load("firstname.Rdata") else
  {
    
myfiles<-list.files(pattern = '\\.csv$') # workingdirectory is default

mynames <- do.call("rbind", lapply(myfiles, function(x) {
  dat <- read.csv(x, header=TRUE,encoding = "UTF-8",sep=";")
  dat$fileName <- tools::file_path_sans_ext(basename(x))
  dat
}))
    save(mynames, file="firstname.Rdata")
  }

# now we rename the column named "fileName" with kiez
names(mynames)[names(mynames)=='fileName'] <- 'kiez'
# and check the data out. 
head(mynames)

# did we get all 12 kiez and not forget one?
is.factor(mynames$kiez)
mynames$kiez<-as.factor(mynames$kiez)
levels(mynames$kiez) # everything seems alright

# it would be great to use the _position_ of each first name, but unfortunately
# the position of the first name is not available for the older data sets, so: lets get rid of it for all of them 
# to keep it straight
mynames$position<-NULL

levels(mynames$kiez)[levels(mynames$kiez)=='charlottenburg-wilmersdorf'] <- 'Charlottenburg-Wilmersdorf'
levels(mynames$kiez)[levels(mynames$kiez)=='friedrichshain-kreuzberg'] <- 'Friedrichshain-Kreuzberg'
levels(mynames$kiez)[levels(mynames$kiez)=='lichtenberg'] <- 'Lichtenberg'
levels(mynames$kiez)[levels(mynames$kiez)=='marzahn-hellersdorf'] <- 'Marzahn-Hellersdorf'
levels(mynames$kiez)[levels(mynames$kiez)=='mitte'] <- 'Mitte'
levels(mynames$kiez)[levels(mynames$kiez)=='neukoelln'] <- 'Neukölln'
levels(mynames$kiez)[levels(mynames$kiez)=='pankow'] <- 'Pankow'
levels(mynames$kiez)[levels(mynames$kiez)=='reinickendorf'] <- 'Reinickendorf'
levels(mynames$kiez)[levels(mynames$kiez)=='spandau'] <- 'Spandau'
levels(mynames$kiez)[levels(mynames$kiez)=='steglitz-zehlendorf'] <- 'Steglitz-Zehlendorf'
levels(mynames$kiez)[levels(mynames$kiez)=='tempelhof-schoeneberg'] <- 'Tempelhof-Schöneberg'
levels(mynames$kiez)[levels(mynames)=='treptow-koepenick'] <- 'Treptow-Köpenick'

summe<-aggregate(mynames$anzahl, by=list(vorname=mynames$vorname), FUN=sum)
name<-merge(mynames,summe, by=vorname)
head(mynames)
head(summe)

#lets create subsets of males and females to reduce the data 
females<-subset(mynames,geschlecht=="w")
males<-subset(mynames,geschlecht=="m")

write.csv(males,"./newdata/boys2012_complete.csv")
write.csv(females,"./newdata/girls2012_complete.csv")



## WOMEN ONLY 
# get the frequency of all female names in Berlin by getting the sum of the frequency by name
rankf<-aggregate(females$anzahl, by=list(Vorname=females$vorname), FUN=sum)
# now we have a neat list of female names with frequency
rankf

#we could also get the sum by keeping the kiez, for further analyses
# so we don't have the same name in the kiez several times
#sumf<-aggregate(females$anzahl, by=list(Vorname=females$vorname,females$kiez), FUN=sum)
#sumf

## MEN ONLY 
# get the frequency of all female names in Berlin by getting the sum of the frequency by name
rankm<-aggregate(males$anzahl, by=list(Vorname=males$vorname), FUN=sum)
# now we have a neat list of female names with frequency
rankm

#get 10 highest names per kiez
library(dplyr)
girls<-rankf %>% 
  arrange(desc(x)) %>% 
  slice(1:20)
summary(girls)
boys<-rankm %>% 
  arrange(desc(x)) %>% 
  slice(1:20)
summary(boys)


# now add the year, we'll need it later: 
boys$year<-2012
girls$year<-2012

dir.create("newdata")

write.csv(boys,"./newdata/boys2012.csv")
write.csv(girls,"./newdata/girls2012.csv")

# to continue with making animated bar plots, use either
# "animated_bar_plot_in_R_for_female_names.R" or
# "animated_bar_plot_in_R_for_male_names.R"


