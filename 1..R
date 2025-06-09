# 1. Load and merge the datasets keeping all information available for the dates in which there is a measurement in “fx.csv”. [1 point]
# Couldn't import an edited version (removed columns) of speeches.csv 

speeches <- read.csv("speeches.csv", header = TRUE)
fx <- read.csv("fx.csv", header = TRUE)

# Rename the date column in df2 to match df1
colnames(speeches)[colnames(speeches) == "when_speech"] <- "Date"

# Only converned with EURO/USD FX rate, dates from both and speech content
fx_speech <- merge (speeches, fx, by.x= "speech_id", by.y="Date", all = TRUE)

colnames(speeches)  # Check column names in speeches
colnames(fx)        # Check column names in fx
colnames(fx_speech) # Check column names in fx_speech

#Removing and highlighting relevant columns - Date, Speech content (title), USD/EURO FX
library(dplyr)
fx_speech %>% select(Date, what_title, USD)

# Changing from long to wide
install.packages("reshape2")
library(reshape2)
fx_speech_widened <- dcast(fx_speech, Date ~ what_title, value.var = "USD", fun.aggregate = mean)
print(fx_speech_widened)
colnames(fx_speech_widened)
View(fx_speech_widened) 
