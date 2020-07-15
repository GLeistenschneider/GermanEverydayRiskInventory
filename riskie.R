#Georg Leistenschneider
#R Script for the ANALYSIS of the German Everyday Risk Inventory (GERI)
#Analysis Rating Survey V1.0 (07/2020)
#WARNING: Since I am not that confident with R, some things may seem confusing
#as long as you stick to the comments everything should work just fine...


#If you have any questions or want to receive the data for research 
#purposes feel free to contact me via stu204065@mail.uni-kiel.de


# install packages 
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

# Set working directory to the folder which contains the .csv file
setwd('/Users/georgleistenschneider/Desktop/')


#during the study i varied the situation sets (1=93, 2=92, 3=92), so that, uneven completions due to aborters, would be evened out
#and all situations would have a greater chance of being rated equally often. It appeared that many participants aborted the 
#study after completion of the first 10 ratings

#therefore round 1 (d.R1) had the configuration: 1,2,3
#round 2 (d.R2): 3,2,1
#round 3 (d.R3): 2,1,3

#read in csv data
d.R1 <- read.csv("RdataR1x.csv", header = TRUE, sep = ";") 
d.R2 <- read.csv("RdataR2x.csv", header = TRUE, sep = ";")
d.R3 <- read.csv("RdataR3x.csv", header = TRUE, sep = ";")

#combine datasets (154 Vpn)
d.xtrafull <- rbind(d.R1, d.R2, d.R3)

#delete unneccessary var's
d.full <- d.xtrafull[!(d.xtrafull$sender=="Seq" | 
                        d.xtrafull$sender=="Dem" | 
                        d.xtrafull$sender=="welcome" | 
                        d.xtrafull$sender=="SitIntro" | 
                        d.xtrafull$sender=="Progress" | 
                        d.xtrafull$sender=="vp" | 
                        d.xtrafull$sender=="Loop1" | 
                        d.xtrafull$sender=="Loop2" | 
                        d.xtrafull$sender=="Loop3"),]


rm(d.xtrafull)
####1. Cleansing ####
###1.1 identify exclusion data
#use every subject once
subject <- unique(d.full$observation)

## 1.1.1 find them quitters (who just did the demos)

nurDem <- c()
for (i in subject){
  temp <- subset(d.full, observation == i)
  if(("Sit" %in% temp$sender) == FALSE) {
    nurDem <- c(nurDem,i)
  }
}

##1.1.2 Smoking (Wie viele Zigaretten rauchen Sie üblicherweise am Tag?)
#exclude all >10
smokies <- c()
for (i in subject){
  temp <- subset(d.full, observation == i)
  if((temp$cig[1]) > 10) {
    smokies <- c(smokies,i)
  }
}

##1.1.3 Drinking (Wie viele Gläser Alkohol trinken Sie üblicherweise am Tag?)
#exclude all > 3
alkies <- c()
for (i in subject){
  temp <- subset(d.full, observation == i)
  if(temp$alc[1] > 3) {
    alkies <- c(alkies,i)
  }
}

##1.1.4 Drugs (Wie oft haben Sie in den letzten 6 Monaten illegale Drogen zu sich genommen?)
#exclude all dailies
junkies <- c()
for (i in subject){
  temp <- subset(d.full, observation == i)
  if((temp$drug[1]) == "daily") {
    junkies <- c(junkies,i)
  }
}

##1.1.5 Coffee (Wie viele Tassen (200ml) Kaffee trinken Sie üblicherweise am Tag?)
coffies <- c()
for (i in subject){
  temp <- subset(d.full, observation == i)
  if((temp$cof[1]) > 4) {
    coffies <- c(coffies,i)
  }
}



###1.2 Excluding data 
##1.2.1 make naughty list 
rausschmiss <- c(nurDem, junkies, smokies, coffies)


##1.2.2 get cleaned dataset
d.clean <- d.full
for (i in rausschmiss){
  
  d.clean <- subset(d.clean, observation != i)
}

#see what our data looks like and get descriptive stats, yo
summary(d.clean$drug, na.rm = TRUE)
skim(d.clean)

d.full <- d.clean


#### 2.  Analysis ####
###2.1 descriptive stats
##2.1.1 age mean
mean(d.full$age,na.rm=TRUE)
sd(d.full$age, na.rm = TRUE)

skim(d.full)

##2.1.2 gender 
table(d.full$gender)

#gender means
aggregate(formula = age ~ gender,
                       data = d.full,
                       FUN = mean)


##2.1.3 grad count
table(d.full$grad) #there is a chance of unaccuracy since some participants are from austria and have a different education system

##2.2 Situations
#times situations were rated
sitsrated <- table(d.full$story_number[d.full$sender=='Sit']) #least rated 2 times (1sit) | most rated mean 17 times (2sit)>>> (all sits rated)

#mean of riscity
sitsriskmean <- aggregate(formula = risk_oa ~ story_number,
                          data = d.full,
                          FUN = mean)
#variance of riscity
sitsriskvar <- aggregate(formula = risk_oa ~ story_number,
                         data = d.full,
                         FUN = var)


#create df for just situations
d.sit <- subset(x = d.full,
                subset = sender =="Sit", 
                select = c(1,31,32,33,34,35,36,37,38))

#create dataframe for relevant vars
df <- d.sit %>% 
  select(1 | matches('story') | matches('risk') | matches ('fame') | matches ('option_a') | matches ('option_b') | 
           matches ('observation'))

##2.3 Realibility Analysis
#THE ONE AND ONLY FUNCTION
#check for kripp alpha over risk_a, risk_b, risk_oa (49 Sits),
#make all high risks opt_a, calculate kripp.alpha, ...
kripp_per_story <- function(test){
  if(mean(test$risk_a)<mean(test$risk_b)){
    dummy <- test$risk_b
    dummy2 <- test$option_b
    test$risk_b <- test$risk_a
    test$option_b <- test$option_a
    test$risk_a <- dummy
    test$option_a <- dummy2
  }
  tibble(
    story_nr = test$story_number[1],
    story = test$story[1],
    opta = test$option_a[2],
    optb = test$option_b[2],
    observations = length(unique(test$observation)), #remove length(unique()) for both RATERANALYSIS
    risk_a = kripp.alpha(cbind(test$risk_a),method = 'ordinal')$value,
    risk_b = kripp.alpha(cbind(test$risk_b),method = 'ordinal')$value,
    risk_oa = kripp.alpha(cbind(test$risk_oa),method = 'ordinal')$value,
    fame_mean = mean(test$fame_known),
    fame_sd = sd(test$fame_known),
    risk_oa_4rlz = kripp.alpha(cbind(test$risk_a,test$risk_b,test$risk_oa),method = 'ordinal')$value,
    risk_mean_a = mean(test$risk_a),
    risk_sd_a = sd(test$risk_a),
    risk_mean_b = mean(test$risk_b),
    risk_sd_b = sd(test$risk_b),
    risk_mean_oa = mean(test$risk_oa),
    risk_sd_oa = sd(test$risk_oa)) #,
   # risk_OA4gender = test$risk_oa) #use this one for RATERANALYSIS 1
}


analyze <- map_dfr(unique(df$story_number),
                   ~ kripp_per_story(df[df$story_number == .,]))

##t-test for opt.a vs opt.b
analyze %>% 
  filter(risk_oa_4rlz > .667 & observations > 4) %$%
  t.test(risk_mean_a,
         risk_mean_b,
         paired = T,
         alternative = 'greater')

# get all situations with alpha >= .667
good_sits <- subset(analyze, risk_oa_4rlz >= .667 & observations > 4)


#get nice summary of data
skim(good_sits)
summary(good_sits)

#export inventory as .csv
fwrite(good_sits, file ="/Users/georgleistenschneider/Desktop/GERI.csv", sep = ";", dec = ",")





# ##2.4 Rateranalysis
# ##2.4.1 RATERANALYSIS 1
# ##IMPORTANT: BEFORE uncommenting and analysing raterdata remove length(unique()) infront of observations
# ##add risk_OA4gender = test$risk_oa) at the end of THE ONE AND ONLY FUNCTION 
# 
# 
# 
# 
# ##Make girly dataset
# sexy.girls <- subset(x = d.full,
#                      subset = gender == 'female',
#                      select = c(1,14))
# 
# #make c() thing for just girls names
# sexy.g <- data.frame(C=c(as.character(unique(sexy.girls$observation))))
# 
# #filter d.full for just girly results
# d.sexy.girls <- analyze %>% filter(
#   observations %in% sexy.g$C[sexy.g$C %in% analyze$observations]
# )
# d.sexy.girls$gender="female"
# 
# girly.sd <- sd(d.sexy.girls$risk_OA4gender)
# girly.mean <- mean(d.sexy.girls$risk_OA4gender)
# 
# 
# #make boys dataset
# sexy.boys <- subset(x = d.full,
#                     subset = gender == 'male',
#                     select = c(1,3))
# 
# #make c() thing for just boys names
# sexy.b <- data.frame(C=c(as.character(unique(sexy.boys$observation))))
# 
# #filter d.full for just boyy results
# d.sexy.boys <- analyze %>% filter(
#   observations %in% sexy.b$C[sexy.b$C %in% analyze$observations]
# )
# d.sexy.boys$gender="male"
# 
# 
# boyy.sd <- sd(d.sexy.boys$risk_OA4gender)
# boyy.mean <- mean(d.sexy.boys$risk_OA4gender)
# 
# d.sexy <- rbind(d.sexy.boys,d.sexy.girls)
# d.sexy <- subset(d.sexy, select = c(1, 5, 18, 19))
# 
# t.test(d.sexy.girls$risk_OA4gender,
#        d.sexy.boys$risk_OA4gender,
#        paired = F,
#        alternative = 'greater')
# 
# 
# 
# 
# 
# ##2.4.2 RATERANALYSIS 2
# #whom of the observationers made it into the GERI
# xxx <- analyze %>%
# filter(risk_oa_4rlz > .667 & length(observations) > 4)
# top.raters <- data.frame(C=c(as.character(unique(xxx$observations))))
# 
# top.raters.demos <- d.full %>% filter(
#    observation %in% top.raters
#  )
#  top.raters$C %in% d.full$observation
#  top.raters$C[top.raters$C %in% d.full$observation]
# 
# top.raters.demos4real <- d.full %>% filter(
#    observation %in% top.raters$C[top.raters$C %in% d.full$observation]
#  )
# 
# #get distribution of the only raters that count
# table(top.raters.demos4real$gender)
# 
# #get age of the o.r.t.c.
# mean(top.raters.demos4real$age,na.rm=TRUE)
# sd(top.raters.demos4real$age, na.rm = TRUE)
# 
# #get the education of o.r.t.c
# table(top.raters.demos4real$grad)
# 
# summary(top.raters.demos4real)
# skim(top.raters.demos4real)
# 
# length(unique(top.raters.demos4real$observation))
# 
# 

##2.4.3 Personality (BFI-10)

#reverse ratings for bfi10_e1, bfi10_v1, bfi10_g1, bfi10_n1, bfi10_o1 (according to BFI-10 scoring manual)
rev <- c("bfi10_e1", "bfi10_v1", "bfi10_g1", "bfi10_n1", "bfi10_o1")
top.raters.demos4real[ ,rev] = 6 - top.raters.demos4real[ ,rev]

#create df for BFI-10
BFI <- subset(x = top.raters.demos4real,
              subset = sender=="BFI-10", 
              select = c(1,21,22,23,24,25,26,27,28,29,30))
                
#get means for five dimensions by person
BFI$ex <- (BFI$bfi10_e1 + BFI$bfi10_e2)/2
BFI$ve <- (BFI$bfi10_v1 + BFI$bfi10_v2)/2
BFI$ge <- (BFI$bfi10_g1 + BFI$bfi10_g2)/2
BFI$ne <- (BFI$bfi10_n1 + BFI$bfi10_n2)/2
BFI$of <- (BFI$bfi10_o1 + BFI$bfi10_o2)/2

#reorder df
BFInew <- BFI[,c(1,2,7,12,3,8,13,4,9,14,5,10,15,6,11,16)]

#BFI w/ just means
BFI <- subset(x=BFInew,
              select = c(4,7,10,13,16))

#you could also just name them straight away like that
names(BFI)[1] <- "Extraversion"
names(BFI)[2] <- "Verträglichkeit"
names(BFI)[3] <- "Gewissenhaftigkeit"
names(BFI)[4] <- "Neurotizismus"
names(BFI)[5] <- "Offenheit"

#get not so mean mean and super.duper over all (sd)
mean(apply(BFI,2,mean))
sd(apply(BFI,2,mean))

#get a summary for the rest
summary(BFI)
skim(BFI)

#Make Boxplot for BFI-10
#choose the right color for our colorblind friends
BFIplot <- ggplot(stack(BFI), aes(x = ind, y = values, fill = ind)) +
  geom_boxplot(alpha=.8)+
  scale_fill_brewer(palette="YlGnBu")+
  labs(x = '', y = 'Ausprägung')+
  theme_minimal()+
  theme(legend.position = "none")


ggsave("BFInew",
       plot = BFIplot,
       device = png,
       path = "/Users/georgleistenschneider/Desktop/",
       scale = 1,
       width = 16,
       height = 8,
       units = c("cm"),
       dpi = 300,
       limitsize = TRUE
)


##3. Deal with the hockey data (Hockey et al., 2000 [Effects of mood and fatique on risk])

# Hockey: Nr. 119, 129, 51, 83, 7, 113, 268, 125, 234, 92, 
#             261, 180, 240, 233, 109, 22, 43, 223, 15, 82, 
#             142, 37, 169, 164, 275, 60, 108, 36, 101, 46, 
#             38, 57, 181, 266, 177, 172, 127, 17, 250

Hockey <- data.frame (C=c(119, 129, 51, 83, 7, 113, 268, 125, 234, 92, 
            261, 180, 240, 233, 109, 22, 43, 223, 15, 82,
            142, 37, 169, 164, 275, 60, 108, 36, 101, 46,
            38, 57, 181, 266, 177, 172, 127, 17, 250))

#Check which Hockeys made it into the final 49
Hockey$C %in% good_sits$story_nr
Hockey$C[Hockey$C %in% good_sits$story_nr]
Hockey.top <- analyze %>% filter(
  story_nr %in% c('119', '234', '233', '109', '43', '82', '38', '172')
)

Hockey.rated <- analyze %>% filter(
    story_nr %in% c('119', '129', '51', '83', '7', '113', '268', '125', '234', '92', 
                    '261', '180', '240', '233', '109', '22', '43', '223', '15', '82',
                    '142', '37', '169', '164', '275', '60', '108', '36', '101', '46',
                    '38', '57', '181', '266', '177', '172', '127', '17', '250')
)

