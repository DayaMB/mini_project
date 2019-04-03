BandData<-Project_data

BandData<-data.frame(Project_data)

#removing na values
BandData<-Project_data[complete.cases(Project_data),]

#add weekend variable
BandData$Weekend <- grepl("Sun.+",weekdays(BandData$Date))

#add total sleep Variable
BandData$Total_slp<-(BandData$Deep_sleep+BandData$Light_sleep)


#removing outliers
#steps
boxplot(BandData$Steps)
BandData$Steps[which(BandData$Steps>10000)]
boxplot(BandData$Steps,plot=FALSE)$out
outliers<-boxplot(BandData$Steps,plot=FALSE)$out
print(outliers)
BandData[which(BandData$Steps %in% outliers),]
BandData1<-BandData[-which(BandData$Steps %in% outliers),]
boxplot(BandData1$Steps)




#calories
boxplot(BandData1$Calories_Burned)
BandData1$Calories_Burned[which(BandData1$Calories_Burned>300)]
boxplot(BandData1$Calories_Burned,plot=FALSE)$out
outliers<-boxplot(BandData1$Calories_Burned,plot=FALSE)$out
print(outliers)
BandData1[which(BandData1$Calories_Burned %in% outliers),]
BandData2<-BandData1[-which(BandData1$Calories_Burned %in% outliers),]
boxplot(BandData2$Calories_Burned)

#light sleep 
boxplot(BandData1$Light_sleep)
BandData1$Light_sleep[which(BandData1$Light_sleep<100)]
boxplot(BandData1$Light_sleep,plot=FALSE)$out
outliers<-boxplot(BandData1$Light_sleep,plot=FALSE)$out
print(outliers)
BandData1[which(BandData1$Light_sleep %in% outliers),]
BandData2<-BandData1[-which(BandData1$Light_sleep %in% outliers),]
boxplot(BandData2$Light_sleep)

#deep sleep
boxplot(BandData2$Deep_sleep)
BandData2$Deep_sleep[which(BandData2$Deep_sleep>200)]
boxplot(BandData2$Deep_sleep,plot=FALSE)$out
outliers<-boxplot(BandData2$Deep_sleep,plot=FALSE)$out
print(outliers)
BandData2[which(BandData2$Deep_sleep %in% outliers),]
BandData3<-BandData2[-which(BandData2$Deep_sleep %in% outliers),]
boxplot(BandData3$Deep_sleep)


m_f<-table(BandData3$Sleep_type)
m_f
m_f_good=(105/157)*100
m_f_good

m_f_sleep<-data.frame(BandData3[which(BandData3$Sleep_type=='Good_sleep'),])
m_f_sleep_we<-data.frame(m_f_sleep[which(m_f_sleep$Weekend=='TRUE'),]) # sun
m_f_sleep_wd<-data.frame(m_f_sleep[which(m_f_sleep$Weekend=='FALSE'),]) # other

#overall weekend good sleep #female 62.85% male 37.14
m_f_sleep_we_good<-table(m_f_sleep$Gender)
m_f_sleep_we_good

m_f_sleep_wd_good<-table(m_f_sleep_wd$Sleep_type)
m_f_sleep_wd_good

male<-data.frame(BandData3[which(BandData3$Gender=='M'),])
m_we<-data.frame(male[which(male$Weekend=='TRUE'),]) # sun
m_wd<-data.frame(male[which(male$Weekend=='FALSE'),])# other days


male_we<-table(m_we$Sleep_type) 
male_we
male_good_we=(5/12)*100
male_good_we

male_wd<-table(m_wd$Sleep_type) 
male_wd
male_good_wd=(34/69)*100
male_good_wd

female_we<-table(f_we$Sleep_type) 
female_we
female_good_we=(8/10)*100
female_good_we

female_wd<-table(f_wd$Sleep_type) 
female_wd
female_good_wd=(58/66)*100
female_good_wd

male_good=(5/12)*100
male_good



barplot(gen,main = toupper("Gender"),
        ylab = "",
        col = "blue")


female<-data.frame(BandData3[which(BandData3$Gender=='F'),])
f_we<-data.frame(female[which(female$Weekend=='TRUE'),])#sun
f_wd<-data.frame(female[which(female$Weekend=='FALSE'),])#other days




#tot_sleep
boxplot(BandData$Total_slp)
BandData$Total_slp[which(BandData$Total_slp>550)]
boxplot(BandData$Total_slp,plot=FALSE)$out
outliers<-boxplot(BandData$Total_slp,plot=FALSE)$out
print(outliers)
BandData[which(BandData$Total_slp %in% outliers),]
BandData3<-BandData[-which(BandData$Total_slp %in% outliers),]
boxplot(BandData3$Total_slp)

#Before and after rmvng outliers
boxplot(BandData$Total_slp)
boxplot(BandData3$Total_slp)
#Bivariate analysis
cor(BandData3$Steps,BandData3$Total_slp)
cor(BandData3$Calories_Burned,BandData3$Total_slp)



gender<-table(BandData$Gender) 

barplot(gender,main = toupper("Gender"),
        ylab = "",
        col = "blue")


gender1<-table(BandData3$Gender) 

barplot(gender1,main = toupper("Gender"),
        ylab = "",
        col = "blue")

df$class=Sleep_type
head(df)
Sleep_type = c() 
for(i in 1:length(BandData3$Deep_sleep))
{
  if(BandData3$Deep_sleep[i]<=90)
  {
    Sleep_type = c(Sleep_type,"Not_enough_sleep")
  }
  else
  {
    Sleep_type = c(Sleep_type,"Good_sleep")
  }
}

#BandData3=cbind(BandData3,Sleep_type)
