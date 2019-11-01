# create "names17" first --> Berlin.R
# 16.6.2019, Hannah Bohle
library(ggplot2)

head(names17)

# https://www.data-to-viz.com/#treemap
# 1. Wordcloud to visualise total frequency of names in Kiez (or in Berlin?)
# 2. Dendrogram to visualise uniqueness of kiez naming?
# 3. Sankey to visualise first 10 names according to kiez
# 4. Treemap to visualise total frequency of first 10 names

Energy$names

#install.packages("plotly")
library(plotly)

levels(names17$kiez)
#girls<-subset(girls$anzahl!="1")
boys<-subset(names17,geschlecht=="m")
girls<-subset(names17,geschlecht=="w")
summary(girls)
girls<-subset(girls,position=="1")
boys<-subset(boys,position=="1")

#get 10 highest names per kiez
library(dplyr)
g<-girls %>% 
  arrange(desc(anzahl)) %>% 
  group_by(kiez) %>% slice(1:10)
summary(g)
b<-boys %>% 
  arrange(desc(anzahl)) %>% 
  group_by(kiez) %>% slice(1:10)
summary(b)
g$geschlecht<-NULL
g$position<-NULL
b$geschlecht<-NULL
b$position<-NULL
summary(g$anzahl)
summary(g$vorname)
#bartable = table(g$kiez, gg$anzahl)  ## get the cross tab
#barplot(bartable, beside = TRUE, legend = levels(unique(girls$vorname)))  ## plot 


g$nameid <- as.numeric(factor(g$vorname,levels=unique(g$vorname)))
g$kiezid <- as.numeric(factor(g$kiez,levels=unique(g$kiez)))
g$kiezidd<-g$kiezid+100
g$kiezidd
#g$geschlecht<-NULL
#g$position<-NULL
head(g)
g$anzahl
levels(g$vorname)
g$nameid

p <- plot_ly(
  type = "sankey",
  orientation = "v",
  
  node = list(
    label = g$vorname,
    color=g$vorname,
    #color = c("blue", "blue", "blue", "blue", "blue", "blue"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = g$nameid,
    target = g$kiezid,
    value =  g$anzahl
  )
) %>% 
  layout(
    title = "Basic Sankey Diagram",
    font = list(
      size = 10
    )
  )
p
g$kiezidd

# Load package
library(networkD3)

# Load energy projection data
URL <- "https://cdn.rawgit.com/christophergandrud/networkD3/master/JSONdata/energy.json"
Energy <- jsonlite::fromJSON(URL)

# Now we have 2 data frames: a 'links' data frame with 3 columns (from, to, value), and a 'nodes' data frame that gives the name of each node.
head( Energy$links )
head( Energy$nodes )
Energy$nodes

# Thus we can plot it
sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "TWh", fontSize = 12, nodeWidth = 30)



