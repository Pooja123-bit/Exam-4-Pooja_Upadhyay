install.packages("tidyverse")
library(tidyverse)
library(stringr)
library(dplyr)
library(plyr)
library(ggplot2)
install.packages("osmdata")
library(ggmap)
library(osmdata)
library(datasets)
getwd()

##Section 1. Bar plot ____ / 8 points 

load("sec_stad.Rdata")

a<-sec_stad
a

#1. Generate a bar plot showing the capacity of each football stadium (4 points)
ggplot(data = a) +
  geom_bar(aes(x=Name, y=Capacity, fill=Capacity),
           position="dodge",
           stat="Identity") 

#a. Flip the axis so that the stadium names are arranged along the left side of 
#the plot and the values are on the bottom of the plot. (2 points) 
ggplot(data = a) +  
  geom_bar(aes(x=Capacity, y=Name, fill=Capacity),
           position="dodge",
           stat="Identity") 

#b. Set the x-axis text to be a 90-degree angle. (2 points) 
ggplot(data = a) +  
  geom_bar(aes(x=Capacity, y=Name, fill=Capacity),
           position="dodge",
           stat="Identity")+
           theme(axis.text.x = element_text(angle = 90))


##Section 2. Multiple Points plot ____ / 5 points 

load("team_statistics.Rdata") 

#2. Select teams belonging to the Sun Belt Conference from the data set. Generate a 
#point plot using ‘ggplot’ to illustrate if there is a relationship between the number
#of passing yards and the number of rushing yard for each team.

b<-ts[ts$Conference=="Sun Belt Conference",]
b

ggplot(data = b, aes(x =Pass.Yard, y=Rush.Yard, colour=Team)) +
  geom_point()


##Section 3: Box-and-whisker plot _____ / 10 points 
#3. For teams in the Big 10 Conference, generate a box-and-whisker plot for
#rushing yards. (6 points) 

c<-ts[ts$Conference=="Big Ten Conference",]
c

ggplot(data = c, aes(x= Team, y =Rush.Yard, fill=Team)) +
  geom_boxplot()

#a. Make the panel background dark blue and the fill of the box-and-whiskers 
#bright yellow (2 points)

ggplot(data = c, aes(x= Team, y= Rush.Yard))+
  geom_boxplot(fill='yellow', color='yellow')+
  theme_classic()+
theme(panel.background = element_rect(fill='darkblue'))

#b. Using this subset of data, tell me which team had the most rushing yards, 
#on average? (2 points)  

Team.max=max(c$Rush.Yard)
Team.max

# Minesota had the highest rushing yards
Team.max.o=c[order(-c$Rush.Yard),]
Team.R.Y=Team.max.o[1,]
Team.R.Y


##Section 4: Heat Map _____ / 8 points 

load("football_stats.Rdata")

#4. For teams in the Southeastern conference, generate a heat map with ‘ggplot’ 
#showing each team’s performance for each statistical variable. (6 points) 
f<-football.stats
f  

f.Sec<-f[f$Conference=="Southeastern Conference",]
f.Sec

ggplot(data = f.Sec, aes(x = variable, y = stat, color=Team)) +
  geom_tile()

#c. Plot the log-10 transformed values for the color scale. (2 points)
ggplot(data = f.Sec, aes(x = variable, y = stat, color=Team)) +
  geom_tile()+
  scale_y_log10()


#Section 5: Mapping _____ / 20 points
#5. Generate a ‘toner’ map figure showing the central and southeastern 
#United States. (5 points)
bb=c(left=min(a$lng),bottom=min(a$lat),
     right=max(a$lng),top=max(a$lat))
SEC.map=get_stamenmap(bbox=bb,zoom=6,
                      map='toner')

ggmap(SEC.map)

ggsave('Toner_Map_1.png')

#6. Generate a second ‘toner’ map figure showing the central and southeastern United
#States, plus the geographic location of all the SEC stadiums (5 points).
ggmap(SEC.map)+
  geom_point(data=a,aes(x=lng,y=lat))

ggsave('Toner_Map_2.png')

#7. Generate a third ‘toner’ map figure showing the geographic location of all the SEC
#stadiums, this time represent these locations using the following aesthetics:
#a.Size of the points scale with the ‘Capacity’ of each stadium (1 points) #b.The 
#colors of the points show degree of gradation in ‘Capacity’ among stadiums (3 points)
ggmap(SEC.map)+
  geom_point(data=a,aes(x=lng,y=lat,color=Capacity,size=Capacity))

ggsave('Toner_Map_3.png')

#8. Export and save these three maps as “.png” files. Please include them when you 
#upload to Moodle (or email) your exam. (6 points total; i.e., 2 points per map file)
getwd()

dir.create("C:/Users/pooja/OneDrive/Desktop/R/Exam-4-Pooja_Upadhyay")

ggsave(".png")


##Section 6: Analysis _____ / 4 points 
#9. In which state is the largest capacity stadium located? (2 points) 

Cap.S.L=max(a$Capacity)
Cap.S.L


Cap.S.L.o=a[order(-a$Capacity),]
Cap.S.L.o

State.Largest.Capacity=Cap.S.L.1[1,]
State.Largest.Capacity
#state with largest capacity stadium is TN


#10. What is the mean and standard deviation capacity of the stadiums in each state? 
#(Hint: you may need to use detach(package:plyr) ) (2 points) 

library(plyr)
detach(package:plyr)

mean.SD.capacity=a%>%group_by(State)%>%summarize(Mean.Cap=mean(Capacity),
                                                 SD.Cap=sd(Capacity))
mean.SD.capacity
