boxplot(male$Total_slp~male$Weekend)

#H0: mean total sleep on weekend = mean total sleep on weekdays

#two sided test

# Hypothesis testing is used to infer the result of a 
# hypothesis performed on sample data from a larger population. 
# The test tells the analyst whether or not his primary hypothesis is true. Statistical analysts 
# test a hypothesis by measuring and examining a random sample of the population being analyzed.


#When you perform a hypothesis test in 
#statistics, a p-value helps you determine the significance of your results. ... 

#H0: mean total sleep on weekend = mean total sleep on weekdays (if p value is greater than 0.05 accept the null hypothesis)
t.test(male$Total_slp~male$Weekend)

t.test(female$Total_slp~female$Weekend)

t.test(BandData3$Total_slp~BandData3$Weekend)

t.test()



fivenum(BandData3) #80.0  3283.0  5072.0  6156.5 10909.0
fivenum(BandData3)
datasummary <-data.frame(BandData3[c(2:5,10)])  
summary(BandData3)

#remove col
#BandData2<-data.frame(BandData2[c(-10)])   
  
TP=cm[1,1]
TN=cm[1,2]
FP=cm[2,1]
FN=cm[2,2]

accuracy=(TP+TN)/(TP+TN+FP+FN)
