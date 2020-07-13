#Georg Leistenschneider
#R Script for the DATA CLEANING of the German Everyday Risk Inventory (GERI)
#Cleaning of the Collection Survey V1.0 (07/2020)

#If you have any questions or want to receive the data for research 
#purposes feel free to contact me via stu204065@mail.uni-kiel.de

##I can not remember which packages are really necessary for the thingy so here you get a collection of
##all my packages. You surely won't need all of them, but you will not have any problems 
##doing stats on R ever again. You can now even use the neat wes anderson color palette
##which I ended up not using

## install packages 
#require('pacman')
#install.packages('RSQLite') 
#install.packages('jsonlite') 
#install.packages('stringr') 
#install.packages('tidyverse')
#install.packages('janitor')
#install.packages("data.table")
#install.packages("car")
#install.packages('dplyr')
#install.packages('yarrr')
#install.packages('psych')
#install.packages('irr')
#install.packages('synchrony')
#install.packages('ggplot2')
#install.packages('DataExplorer')
#install.packages('skimr')
#install.packages('wesanderson')


# load packages  
library("RSQLite")
library("jsonlite")
library("stringr")
library("tidyverse")
library("janitor")
library("data.table")
library("car")
library("plyr")
library("dplyr")
library("yarrr")
library("irr")
library("synchrony")
library("magrittr")
library("DataExplorer")
library("skimr")
library("wesanderson")


#use the data tranformator to turn your .sqlite into .csv. It's by the man himself
#and it's tremendous. Use it, it's the best. https://felixhenninger.shinyapps.io/labjs-export/

setwd('/Users/georgleistenschneider/Desktop/')

# 1. read in csv data
d.demo.R1 <- read.csv("CollectionFinal.csv", header = TRUE, sep = ';')
d.demo.R2 <- read.csv("CollectionFinal2.csv", header = TRUE, sep = ';')
d.demo <- rbind(d.demo.R1, d.demo.R2)

# 2. Cleansing
# 2.1 use every subject once
  participant <- unique(d.demo$observation)
  
# 2.2 find them quitters (who just did the demos)
nurDemo <- c()
  for (i in participant){
    tempo <- subset(d.demo, observation == i)
    if(("sit" %in% tempo$sender) == FALSE) {
      nurDemo <- c(nurDemo,i)
    }
  }

# 2.3 get cleaned dataset
d.shiny <- d.demo
for (i in nurDemo){
  
  d.shiny <- subset(d.shiny, observation != i)
}

summary(d.shiny, na.rm = TRUE)

write.csv(Datensatz,"/Users/georgleistenschneider/Desktop/demos.csv", row.names = FALSE)
#I have given up R at this point. The rest of the analysis was done in sweet sweet apple numbers... 
#(E.g. delete all the test runs 4 btw. etc.)
#Since it was just about the demographics it was not too painfull!