#CLEANING

#Download df
dfpor <- read.csv("https://raw.githubusercontent.com/katieluong33/BUAN-381/refs/heads/main/student-mat.csv", header = TRUE, sep = ";")
dfmath <- read.csv("https://raw.githubusercontent.com/katieluong33/BUAN-381/refs/heads/main/student-por.csv", header = TRUE, sep = ";")

#add a column to each df called "class" to identify which course 
#the data is from, and convert it to a factor
dfpor$class <- "p"
dfpor$class <- as.factor(dfpor$class)
class(dfpor$class)

dfmath$class <- "m"
dfmath$class <- as.factor(dfmath$class)
class(dfmath$class)

#merge the two df together
mergeddf <- rbind(dfpor,dfmath)

