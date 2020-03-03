#Import Library
library(rvest)
library(tidyverse)
library(abind)
library(gridExtra)

#Set System local to Thailand
Sys.setlocale(locale = "Thai")

#Scraping web multi pages by using function & lapply

url<-"https://www.mudleyworld.com/site/mudley?page="

#Create function to scraping table from MW to data frame
pg<-function(x){
    x%>%read_html()%>%html_nodes(xpath = '//*[@id="w1"]/table')%>%html_table()%>%as.data.frame()
}


#Scraping table by using lapply function
mwlist<-lapply(paste0(url,1:21),pg)


#rbind all data frame in the list by using abind package
mwtable<-abind(mwlist,along = 1)
mwdataframe<-as.data.frame(mwtable,stringsAsFactors = FALSE)


#Convert some data to proper class in data frame
mwdataframe$Challenge<-as.numeric(mwdataframe$Challenge)
mwdataframe$Challenge.Rank<-as.integer(mwdataframe$Challenge.Rank)

#manipulate data & clean by using tidyverse package
MWRanking<-mwdataframe%>%filter(Challenge>0)%>%
    select(Name,Challenge.Rank,Challenge)%>%arrange(desc(Challenge))

#Clean data by drop na
clean_MW<-drop_na(MWRanking)
clean_MW<-clean_MW%>%mutate(Challenge=Challenge*10^8)

#Select Top 20 Challenge Score
top20score<-head(clean_MW, n=20)

png(filename = "Top20score.png", width=480,height=500,bg = "white")
grid.table(top20score)
dev.off()

#MW Challenge 80 Ranking
clean_MW2_1<-clean_MW%>%filter(Challenge.Rank<=20)

clean_MW2_2<-clean_MW%>%filter(Challenge.Rank>=21 & Challenge.Rank<=40)

clean_MW2_3<-clean_MW%>%filter(Challenge.Rank>=41 & Challenge.Rank<=60)

clean_MW2_4<-clean_MW%>%filter(Challenge.Rank>=61 & Challenge.Rank<=80)

#Save data frame to image by using gridExtra Packages

png(filename = "Outlier.png", width=480,height=500,bg = "white")
grid.table(outlier)
dev.off()

#max challenge exp
max_challenge<-clean_MW%>%arrange(desc(Challenge))
outlier<-head(max_challenge,n=11)

#Boxplot to find outlier
boxplot(clean_MW$Challenge, horizontal=TRUE,xlab="BTC (Satoshi)", ylab="Challenge", main="Box Plot MW Challenge",outcol="red", col="grey")

