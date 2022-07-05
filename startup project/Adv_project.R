
#Setting up the directory

setwd("C:/Users/Ishan1/Desktop/Data Project")
startup.df <- read.csv(paste("startup_funding.csv", sep=""))
View(startup.df)

str(startup.df)

## Convert Date variable to Date format
startup.df$Date <- as.Date(startup.df$Date,format("%d/%m/%Y"))
## Confirm
str(startup.df)


## Make a new column for year
startup.df$year <- as.numeric(format(startup.df$Date,"%Y"))
## Year frequency table
yeartable <- table(startup.df$year )
yeartable

## Visualization using Bar chart
barplot(yeartable, xlab="Year", ylab = "No. of Startups which received funding", col="lightblue" )


# Which startups received funding highest number of times?
head(sort(table(startup.df$StartupName), decreasing=TRUE))

##Industry vertical frequency table
industrytable <- head(sort(table(startup.df$IndustryVertical), decreasing=TRUE))
industrytable

## Visualization using Bar chart
barplot(industrytable, xlab="Industry", ylab = "No. of Startups which received funding", col="lightblue",cex.names = 0.7 )

##Subvertical vertical frequency table
subverticaltable <- head(sort(table(startup.df$SubVertical), decreasing=TRUE))
subverticaltable

## Visualization using Bar chart
barplot(subverticaltable, xlab="Subvertical", ylab = "No. of Startups which received funding", col="lightblue",cex.names = 1 )

##Location frequency table
locationtable <- head(sort(table(startup.df$CityLocation), decreasing=TRUE))
locationtable

## Visualization using Bar chart
barplot(locationtable, xlab="Location", ylab = "No. of Startups which received funding", col="lightblue",cex.names = 1 )

##Investor frequency table
Investortable <- head(sort(table(startup.df$InvestorsName), decreasing=TRUE))
Investortable

## Visualization using Bar chart
barplot(Investortable, xlab="Investor Name", ylab = "No. of Startups which received funding", col="lightblue",cex.names = 0.5 )

##Investment Type frequency table
typetable <- head(sort(table(startup.df$InvestmentType), decreasing=TRUE))
typetable

## Convert factor from factor to numeric

startup.df$AmountInUSD<-as.numeric(as.character(startup.df$AmountInUSD))
startup.df$AmountInUSD[is.na(startup.df$AmountInUSD)] <- 0
disclosedfunding.df <- subset(startup.df,AmountInUSD >0)
library(psych)
describe(disclosedfunding.df$AmountInUSD)[,c(3:9)]

boxplot(disclosedfunding.df$AmountInUSD, horizontal = TRUE, ylim=c(0,20000000)  )

#Distribution of Funding Amount with City Location

Location <- c("Bangalore","Mumbai","New Delhi","Gurgaon","Pune")
Funding <- c(sum(disclosedfunding.df$AmountInUSD[disclosedfunding.df$CityLocation=="Bangalore"]),sum(disclosedfunding.df$AmountInUSD[disclosedfunding.df$CityLocation=="Mumbai"]),sum(disclosedfunding.df$AmountInUSD[disclosedfunding.df$CityLocation=="New Delhi"]),sum(disclosedfunding.df$AmountInUSD[disclosedfunding.df$CityLocation=="Gurgaon"]),sum(disclosedfunding.df$AmountInUSD[disclosedfunding.df$CityLocation=="Pune"]))
frame <- data.frame(Location,Funding)
pie(frame$Funding,labels = frame$Location,col=cm.colors(6),main = "Funding(USD) Received in top 5 cities" )

library("dplyr")

install.packages("plyr")           # Install & load plyr
library("plyr")

my = df %>%
  group_by(Headquarters) %>%
  summarise(ct = n()) %>%
  arrange(desc(ct)) 

df %>%
  group_by(Sector) %>%
  summarise(ct = n()) %>%
  arrange(desc(ct))

df %>%
  group_by(Stage) %>%
  summarise(ct = n()) %>%
  arrange(desc(ct))

df %>%
  group_by(Investors.Name) %>%
  summarise(ct = n()) %>%
  arrange(desc(ct))

df %>%
  group_by(ï..Founded) %>%
  summarise(ct = n()) %>%
  arrange(desc(ct))

top_hq <- df %>%
  group_by(Headquarters) %>%
  summarise(ct = n()) %>%
  filter(ct > 40)

top_hq %>%
  ggplot(aes(reorder(Headquarters, -ct, FUN = min), ct)) +
  geom_point(size = 4) +
  labs(x = "HQ", y = "Count") +
  coord_flip()

# Contigency table to compare mean fundings of different cities

p <- aggregate(AmountInUSD ~CityLocation , data=disclosedfunding.df ,mean)
p <- p[order(-p$AmountInUSD),]
## Only display for cities with maximum mean funding
head(p)

# Correlation Matrix

disclosedfunding.df$IndustryVertical <- as.numeric(disclosedfunding.df$IndustryVertical)
disclosedfunding.df$SubVertical <- as.numeric(disclosedfunding.df$SubVertical)
disclosedfunding.df$CityLocation <- as.numeric(disclosedfunding.df$CityLocation)
mat <- cor(disclosedfunding.df[,c(4,5,6,9)])
mat

library(corrplot)
corrplot(corr=cor(disclosedfunding.df[,c(4,5,6,9)]),method="ellipse")

#test1
tab <- table(disclosedfunding.df$CityLocation)
chisq.test(tab)

#test2
tab2 <- table(disclosedfunding.df$IndustryVertical )
chisq.test(tab2)

