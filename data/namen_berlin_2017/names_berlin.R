
install.packages("plotly")
library(plotly)
set.seed(100)
data(diamonds)
d <- diamonds[sample(nrow(diamonds), 1000), ]
is.numeric(d$carat)
plot(d$carat)
plot(d$price)
plot(d$clarity)


qplot(data= diamonds, x= carat, y= price, colour= color) # works
qplot(data= diamonds, carat, price, colour= color) 


plot_ly(data=diamonds, x = d$carat, y = d$price , key = d$cut, type='scatter', mode="markers",
        name = "Clusters", text = d$cut, hoverinfo = "text")

plot_ly(d, x = d$carat, y = d$price, text = paste("Clarity: ", d$clarity),
        mode = "markers", color = d$carat, size = d$carat)


#Merge CSV files
library(purrr)
library(readr)
getwd()
#Laptop
wd<-setwd("C:/Users/Hannah Bohle/Dropbox/R_wissen/namen_berlin_2017")


csvfilenames <- list.files("C:/Users/Hannah Bohle/Dropbox/R_wissen/namen_berlin_2017",pattern="*.csv", all.files=TRUE, full.names=FALSE)

for(i in 1:length(csvfilenames))
{
  temp<-read.csv(file=paste("C:/Users/Hannah Bohle/Dropbox/R_wissen/namen_berlin_2017/",csvfilenames[i],sep=""), sep=";", header=T) 
  temp$filename_tag<-paste(csvfilenames[i])
  
  list[[i]]<-temp
  }

outfile<-as.data.frame(do.call(rbind,list))


temp$filename_tag
