BandData<-Project_data
BandData<-data.frame(Project_data)
BandData$Weekend <- grepl("Sun.+",weekdays(BandData$Date))

BandData$Deep_slp_per=((BandData$Deep_sleep)/(BandData$Light_sleep+BandData$Deep_sleep))*100

BandData$Sleep_type<-(BandData$Deep_slp_per > 10)

BandData$Total_slp<-(BandData$Deep_sleep+BandData$Light_sleep)

BandData<-Project_data[complete.cases(Project_data),]
#Univariate analysis


hist(BandData3$Steps,
     main = toupper("Steps"),
     ylab = "frequency",
     col = "blue")

hist(BandData3$Calories_Burned,
        main = toupper("Calories"),
        ylab = "frequency",
        col = "blue")


hist(BandData3$Light_sleep,
     main = toupper("Light sleep"),
     ylab = "",
     col = "blue")


hist(BandData3$Deep_sleep,
     main = toupper("deep sleep"),
     ylab = "",
     col = "blue")


boxplot(BandData$Deep_sleep,
     main = toupper("deep sleep"),
     ylab = "",
     col = "blue")

boxplot(BandData$Light_sleep,
        main = toupper("light sleep"),
        ylab = "minutes",
        col = "blue")

boxplot(BandData$Steps,
        main = toupper("Steps"),
        ylab = "",
        col = "blue")

boxplot(BandData$Calories_Burned,
        main = toupper(""),
        ylab = "",
        col = "blue")

gen<-table(BandData3$Gender) 

barplot(gen,main = toupper("Gender"),
        ylab = "",
        col = "blue")
BandData=
#-----------------
a=BandData$Steps
var(a)#7688117
sd(a)#2772.745
#------------------
b=BandData$Calories_Burned
var(b)#2446.128
sd(b)#49.45835 
#-----------------
c=BandData$Light_sleep
var(c)#5110.895
sd(c)#71.49052
#---------------
d=BandData$Deep_sleep
var(d)#4691.24
sd(d)#68.49263
#-----------
summary(BandData)
mean(BandData$Steps)
median(BandData$Steps)
#mode(BandData$Steps)

mean(BandData$Calories_Burned)#79.14634
median(BandData$Calories_Burned)#77

mean(BandData$Light_sleep)#240.8293
median(BandData$Light_sleep)#241

mean(BandData$Deep_sleep)#159.0976
median(BandData$Deep_sleep)#145

Inference= 'this is inference 12 lllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll'
m
print(m)
#--------------------------------------

a<-BandData[c(2:5)]
res<-cor(a)
#----------------------------------------
  #bivariate
  
plot(BandData$Steps,BandData$Deep_sleep)
abline(lm(BandData$Deep_sleep~BandData$Steps))

plot(BandData$Calories_Burned,BandData$Deep_sleep)
abline(lm(BandData$Deep_sleep~BandData$Calories_Burned))


plot(BandData$Calories_Burned,BandData$Light_sleep)