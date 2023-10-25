# loading data and ggplot2 library
rm(list=ls())
getwd()
setwd("C:/Users/arjun/OneDrive/Desktop/DataSci")
load("titanic_train.RDATA")
View(titanic.train)
library("ggplot2")


# --- function to save a graph to RGraphs folder given a title
save = function(title){
  ggsave(title,
         path="C:/Users/arjun/OneDrive/Desktop/DataSci/RGraphs",
         width=10,
         height=10,
         units = "cm") 
}


# --- Converting "Survived" variable to a yes/no rather than 1/0
replacement = c(rep("No",length(titanic.train$Survived)))
replacement[titanic.train$Survived==1] = "Yes"
titanic.train$Survived = replacement


# --- Percentage of each sex on board the ship
sexStatistics = function(){
  x = prop.table(table(titanic.train$Sex))
  print(x)
  
  # bar chart containing the same data
  ggplot(titanic.train) +
    aes(x=Sex) +
    geom_bar(fill="grey", color="black") +
    labs(x="Passenger Sex", y="Total Count")
}
sexStatistics()
save("Sex Distribution.png")


# --- percentage of each class on board the ship
classStatistics = function(){
  x = prop.table(table(titanic.train$Pclass)) 
  print(x)
  
  # bar chart containing the same data
  ggplot(titanic.train) +
    aes(x=Pclass) +
    geom_bar(fill="grey", color="black") +
    labs(x="Passenger Class", y="Total Count")
}
classStatistics()
save("Class Distribution.png")


# --- Distribution of passengers by age
ageStatistics = function(){
  x = summary(titanic.train$Age)
  print(x)
  x = prop.table(table(titanic.train$Age<18))
  print(x)
  
  ggplot(titanic.train) +
    aes(x=Age) +
    geom_histogram(bins=40) +
    labs(x="Age", y="Count")
}
ageStatistics()
save("Age Distribution.png")


# --- Distribution of fares
fareStatistics = function(){
  x = summary(titanic.train$Fare)
  print(x)
  
  ggplot(titanic.train) +
    aes(x=Fare) +
    geom_histogram(bins=60) +
    labs(y="Count")
}
fareStatistics()
save("Fare distribution.png")


# --- Distribution of survivors
survivalStatistics = function(){
  x = prop.table(table(titanic.train$Survived))
  print(x)
  
  ggplot(titanic.train) +
    aes(x=Survived) +
    geom_bar(fill="grey", color="black") +
    labs(x="Survived", y="Total Count")
}
survivalStatistics()
save("Survival Distribution.png")


# --- percentage of deaths split by sex
deathsBySex = function(){
  x = prop.table(table(titanic.train$Sex,as.factor(titanic.train$Survived)),1)
  print(x)
  
  ggplot(titanic.train) +
    aes(x=Sex, fill=Survived) +
    geom_bar(position = position_fill(), color="black") +
    labs(x="Passenger Sex", y="", fill="Survived?")
}
deathsBySex()
save("Survival by sex.png")


# --- Survival rates for minors vs adults
survivalMinors = function(){
  x = prop.table(table(titanic.train$Age<18,as.factor(titanic.train$Survived)),1)
  print(x)
  
  ggplot(titanic.train) +
    aes(x=Age<18, fill=Survived) +
    geom_bar(position = position_fill(), color="black")
}
survivalMinors()
save("Survival rates for minors and adults.png")


# --- Histogram of survival rates by age
byAge = function(){
  x = prop.table(table(as.factor(titanic.train$Age),titanic.train$Survived==1),1)
  print(x)

  ggplot(titanic.train) +
    aes(x=Age, fill=Survived) +
    geom_histogram(bins = 16, position = position_fill()) +
    labs(y="")
}
byAge()
print("Survival rates by age.png")


# --- Survival rates by embark location
byEmbark = function(){
  x = summary(titanic.train$Embarked)
  print(x)
  x = prop.table(table(titanic.train$Embarked,as.factor(titanic.train$Survived)),1)
  print(x)

  ggplot(titanic.train) +
    aes(x=Embarked, fill=Survived) +
    geom_bar(position = position_fill(), color="black") +
    labs("Embarked Location", y="", fill="Survived?")
}
byEmbark()  
save("Survival by embarkment location.png")


# --- Survival rate by ticket class
byClass = function(){
  x = prop.table(table(as.factor(titanic.train$Pclass),titanic.train$Survived==1),1)
  print(x)
  
  ggplot(titanic.train) +
    aes(x=Pclass, fill=Survived) +
    geom_bar(position = position_fill(), color="black") +
    labs(x="Ticket Class", y="", fill="Survived?")
}
byClass()
save("Survival by class.png")


# --- Relationship between embark location and class
embByClass = function(){
  x = prop.table(table(titanic.train$Embarked, titanic.train$Pclass),1)
  print(x)
  
  ggplot(titanic.train) +
    aes(x=Embarked, fill=Pclass) +
    geom_bar(position=position_fill(), color="black") +
    labs(x="Embark Location", y="", fill="Ticket Class")
}
embByClass()
save("Embark by class.png")


# --- Relationship between class and sex
classVsSex = function(){
  ggplot(titanic.train) +
    aes(x=Pclass, fill=Sex) +
    geom_bar(position=position_fill(), color="black") +
    labs(x="Ticket Class", y="")
}
classVsSex()
save("Class vs Sex.png")


ggplot(titanic.train) +
  aes(x=SibSp, fill=Survived) +
  geom_bar(position=position_fill())
table(titanic.train$SibSp)
ggplot(titanic.train) +
  aes(x=Parch, fill=Survived) +
  geom_bar(position=position_fill())



# --- Parsing by cabin data

# --- Function to separate the first digit of a string from the rest
splitFirst = function(x){
  # list every character in input as a vector "splitString"
  splitString = unlist(strsplit(as.character(x),split=""))
  
  # if there's cabin data, return the first digit in the vector
  if(length(splitString)>0){
    return(splitString[1]) 
  
  # otherwise, return a blank string
  }else{
    return("")
    
  }
}

# --- Function to separate the rest of the digits of a string from the first
# The opposite of splitFirst
splitRest = function(x){
  # list characters of a string as a vector "splitString"
  splitString = unlist(strsplit(as.character(x), split=""))
  
  # if there's more than one digit,
  if(length(splitString)>1){
    # find, as integer, the position of the first space in the string
    # if there are multiple cabins listed, there will be a space
    spacePositions = which(splitString == " ")
    firstSpacePosition = as.integer(spacePositions[1])
    
    # if there are no spaces (only one cabin listed), return every digit after the first
    # returns the cabin number
    if((length(spacePositions)==0)){
      pasted = paste(splitString[2:length(splitString)], collapse = "")
      return(pasted)

    # if there are multiple cabins, return the number of the first one listed
    }else{
      pasted = paste(splitString[2:firstSpacePosition], collapse = "")
      return(pasted) 
      
    }
  # if there is no cabin listed, return "0"
  }else{
    return("0")
    
  }
}

# creating new data columns for cabin letter and number
titanic.train$CabinLetter=""
titanic.train$CabinNumber=""

# --- Filling the cabin letter/number columns
for(i in 1:nrow(titanic.train)){
  titanic.train$CabinLetter[i]=splitFirst(titanic.train$Cabin[i])
}
for(i in 1:nrow(titanic.train)){
  titanic.train$CabinNumber[i]=splitRest(titanic.train$Cabin[i])
}


# --- Survival rate by cabin number
byCabinNumber = function(){
  ggplot(titanic.train) +
    aes(x=as.integer(CabinNumber), fill=Survived) +
    geom_histogram(bins = 30, position = position_fill()) +
    labs(x="Cabin Number", y="")
}
byCabinNumber()
save("Survival by cabin number.png")


# --- Survival rates by cabin letter
byCabinLetter = function(){
  x = table(titanic.train$CabinLetter)
  print(x)
  x = prop.table(table(titanic.train$CabinLetter,titanic.train$Survived),1)
  print(x)
  
  ggplot(titanic.train) +
    aes(x=CabinLetter, fill=Survived) +
    geom_bar(position = position_fill()) +
    labs(x="Cabin Letter", y="")
}
byCabinLetter()
save("Survival by cabin letter.png")

prop.table(table(titanic.train$CabinLetter,titanic.train$Pclass),1)


# --- Survival rates by whether cabin number is known
byCabinKnown = function(){
  x = table(titanic.train$Cabin!="",titanic.train$Survived)
  print(x)
  
  ggplot(titanic.train) +
    aes(x=(Cabin!=""), fill=Survived) +
    geom_bar(position = position_fill(), color="black") +
    labs(x="Is Cabin Number Known?", y="", fill="Survived?", title = "Survival Rates Depending on if the Cabin is Known:")
}
byCabinKnown()
save("Survival rates by whether or not a cabin is listed.png")


# --- Knowledge of cabin number split by sex
cabinKnownBySex = function(){
  x = table(titanic.train$Cabin!="", titanic.train$Sex)
  print(x)
  
  ggplot(titanic.train) +
    aes(x=(Cabin!=""), fill=Sex) +
    geom_bar(position = position_fill(), color="black") +
    labs(x="Is Cabin Number Known?", y="")
}
cabinKnownBySex()
save("If cabin number is known split by sex.png")


# --- Knowledge of cabin number split by ticket class
cabinKnownByClass = function(){
  x = prop.table(table(titanic.train$Cabin!="", titanic.train$Pclass),1)
  print(x)
  
  ggplot(titanic.train) +
    aes(x=(Pclass), fill=Cabin!="") +
    geom_bar(position = position_fill(), color="black") +
    labs(x="Ticket Class", y="", fill="Is Cabin Number Known?")
}
cabinKnownByClass()
save("Is cabin number known, split by class.png")


# --- Knowledge of cabin number split by embarkation location
cabinKnownByEmb = function(){
  x = prop.table(table(titanic.train$Embarked,titanic.train$Cabin!=""),1)
  print(x)
    
  ggplot(titanic.train) +
    aes(x=(Embarked), fill=Cabin!="") +
    geom_bar(position = position_fill(), color="black") +
    labs(x="Embarkation Location", y="", fill="Is Cabin Known?")
}
cabinKnownByEmb()
save("Is cabin known, split by embarkation.png")




# --- Survival rates by fare
byFare = function(){
  ggplot(titanic.train) +
    aes(x=Fare, fill=Survived) +
    geom_histogram(bins = 60, position = position_fill()) +
    labs(x="Ticket Fare", y="",fill="Survived?")
}
byFare()
save("Survival rates by fare.png")


# --- Survival rates by fare, with threshold fare 50
byFare50 = function(){
  x = prop.table(table(titanic.train$Fare>=50, titanic.train$Survived),1)
  print(x)
  
  ggplot(titanic.train) +
    aes(x=Fare>=50, fill=Survived) +
    geom_bar(position = position_fill(), color="black") +
    labs(x="Is Fare Greater Than 50?", y="", fill="Survived?")
}
byFare50()
save("Survival rates for fares greater than 50.png")


# --- Ticket class by fare
classByFare50 = function(){
  ggplot(titanic.train) +
    aes(x=Fare>=50, fill=Pclass) +
    geom_bar(position = position_fill(), color="black") +
    labs(x="Ticket Fare Greater Than 50?", y="", fill="Ticket Class")
}
classByFare50()
save("Ticket class by fare.png")

