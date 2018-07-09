# Install package
# ggplot2:for ggplot() functions.
install.packages("ggplot2")
#Use library command after installation to load the package
library(ggplot2)
library(lattice)
# Clean up :clear any previous data that can affect the result while running the code.
rm(list = ls())
# Declare two list variables
# 1.ages: to hold the female's final ages.
# 2.recruits :to hold total number of recruited offsprings.
ages<-c()
recruits<-c()
# loop to simulate 1000 female life histories.
for (i in 1:1000)
{
  # co :tracks condition of each individual female.
  # rnorm(1,100,10) : normal distribution a single draw with a mean of 100 and standard deviation of 10.
  co<-rnorm(1,100,10)
  # Initialise the variables with values as mensioned in the assignmnt.
  # Influence of environment represented by mean (enMu) and standard deviation (enSD)
  #initial value is set as 0 and 20 respectively and it changes for different scenarios.
  age<- 15
  alive<- 1
  enMu<- 20
  enSD<- 1
  b0<- -10
  b1<- 0.1
  inv<- 10
  s0<- -2
  s1<- 0.05
  calf<- 0
  calfage<- 0
  offspring<- 0
  recruit<- 0
  while(alive==1 && age<40)
  {
    co<-(co + rnorm(1,enMu,enSD))
    #Probability of Survival (sr) depend on linear predictor :s0+s1*co
    sr<-exp(s0+s1*co)/(1+exp(s0+s1*co))
    if(alive<-rbinom(1,1,sr)==1)
      age=age+1
    #Variable calf is 0 if female currently doesn't have a calf and 1 if she does.
    if(calf==0){
      #If female doesn't have a calf then:
      #Breeding Probability (b)depend on linear predictor :b0+b1*co
      #if a calf is born, then set calf to 1
      b<-exp(b0+b1*co)/(1+exp(b0+b1*co))
      if(rbinom(1,1,b)==1)
        calf<-1
    }
    else if(calf==1){
      #If female has a calf then:
      #decrement female's condition (co) by anual maternity investment (inv). 
      
      #set calf to zero if calf has died
      #set calfage to zero if calf has died, or increment it by one otherwise.
      co<-co-inv
      if(rbinom(1,1,0.80))
      {
        calfage=calfage+1
      }
      else
      {
        calf<-0
        calfage<-0
      }
    }
    if(calfage>4)
    {
      #If the calf has reached the age of independence then:
      #Increment the number of the mothers offspring by 1.
      #reset calf and calfage to zero.
      offspring= offspring+1
      calf<-0
      calfage<-0
    }
  }
  ages[i]<-age
  if(rbinom(1,1,0.98)==1)
    recruit=offspring
  recruits[i]<-recruit
}
# Creat data frames for ggplot(),ages.df for ages & recruits.df for recruits.
ages.df <- data.frame(ages)
recruits.df <- data.frame(recruits)

#Visualisation of result using histogram with commands  ggplot().
#Histograms for Ages at End of Reproduction

ggplot(data=ages.df, aes(x = ages)) +
  geom_histogram(binwidth = 2,
                 col="black",
                 aes(fill=..count..)) +
  scale_fill_gradient("Frequency",
                      low = "green",
                      high = "red") +
  labs(title="Histogram for Ages at End of Reproduction using ggplot()function in R",
       x="Ages",
       y="Frequency")
#Histograms for Inclusive Fitness 

ggplot(data=recruits.df,
       aes(x = recruits)) +
  geom_histogram(binwidth = .5,
                 col="black",
                 aes(fill=..count..)) +
  scale_fill_gradient("Frequency",
                      low = "green",
                      high = "red") +
  labs(title="Histogram for Inclusive Fitness using ggplot() function in R",
       x="Recruits",
       y="Frequency")
#Average number of offsprings produced (AvgOffProd)
AvgOffProd<-sum(recruits[1:1000])/1000 
