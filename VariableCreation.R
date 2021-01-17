# Resources
# Mini Course Workbook - http://naep-research.airprojects.org/AERA2020ProcDataNAEP/
# Mini Course Data Set - http://naep-research.airprojects.org/AERA2020ProcDataNAEP/TermsConditions.html


# 3.0 Prepare Working Environment ---------------------------------------------
# Install and Load packages
RequiredPackages <- c("data.table", "dplyr", "ggplot2", "stringr", 
                      "gridExtra", "tidyr")

for (pkg in RequiredPackages) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)}
  if (pkg %in% rownames(.packages()) == FALSE)
  {library(pkg, character.only = TRUE)}
}

# Load Mini Course Data into Your Working Environment
# eventData 
load("eventData.Rdata")
str(eventData)
head(eventData)
eventData[,length(unique(maskedStuID))]

# itemData 
load("itemData.Rdata")
str(itemData)
itemData

# StudentData 
load("studentData.Rdata")
str(studentData)
studentData


# 3.1 Clean and Manipulate the Data -------------------------------------------
# Subset data for Enter/Exit actions for RT Calculation 
EntExtData <- eventData[, .SD[c(interpretation=="Enter Item" | 
                                  interpretation=="Exit Item")],
                        by=.(maskedStuID,maskedItemID)]
head(EntExtData)

# Calculate the number of Enter/Exit each student has per item  
EntExtCount<-EntExtData[,list(Enter=sum(interpretation=="Enter Item"), 
                              Exit=sum(interpretation=="Exit Item")),
                        by=.(maskedStuID,maskedItemID)]
head(EntExtCount)

# Flag uneven cases 
EntExtCount[,UnequalEnterExit := (Enter!=Exit)] 
# NOT all entries have equal Enter/Exit numbers; we will exclude those

# How many student/item pairs have uneven Enter/Exit? ANSWER: 7 CASES
head(EntExtCount)
EntExtCount[,table(UnequalEnterExit)] 

# Find a case that has been flagged as TRUE
head(EntExtCount[UnequalEnterExit=="TRUE"])

# Take the flagged cases and remove them from the process data
UnequalEnterExitTrue<-EntExtCount[UnequalEnterExit=="TRUE",
                                  maskedStuID,maskedItemID]

eventDataNew<-data.table(anti_join(eventData,UnequalEnterExitTrue,
                                   by=c("maskedItemID", "maskedStuID")))

# Check the dimensions and number of students of the original data
dim(eventData)
eventData[,length(unique(maskedStuID))]

# Check the dimensions and number of students of the new data
dim(eventDataNew)
eventDataNew[,length(unique(maskedStuID))]


# 3.2 Item Response Time (RT) Calculation -------------------------------------
# Now that the Enter/Exit are equal we only need this to calculate item RT
eventDataEnterExit<-eventDataNew[interpretation=="Enter Item" | 
                                   interpretation=="Exit Item"]

# Individual Item RT 
StudentID<- as.character(
  eventDataEnterExit[seq(1,nrow(eventDataEnterExit), 2)]$maskedStuID)

ItemID<- as.character(
  eventDataEnterExit[seq(1, nrow(eventDataEnterExit), 2)]$maskedItemID)

ItemType<- as.character(
  eventDataEnterExit[seq(1, nrow(eventDataEnterExit), 2)]$itemType)

ItemRT<- diff(
  eventDataEnterExit$maskedTimeStamp)[seq(1, nrow(eventDataEnterExit), 2)]

# Populate the Response Time dataframe 
ResponseTime<- data.table(data.frame(StudentID, ItemID, ItemType, ItemRT))

# Take a look at the results
ResponseTime


# 3.3 Item Visit --------------------------------------------------------------
# NOTE: students may have multiple calculations for each item as they 
# may have enter/exit that particular item more than once

# Number of item visits per student 
ResponseTime[,ItemVisit:=seq_len(.N),by=.(StudentID, ItemID)]
ResponseTime

# Let's see an example
ResponseTime[StudentID=="1001" & ItemID=="M347101"]


# 3.4 Total Item Response Time (RT) -------------------------------------------
# Total Item RT 
ResponseTime[,TotalItemRT:=sum(ItemRT),by=.(StudentID, ItemID)]
ResponseTime

# Example
ResponseTime[StudentID=="1001" & ItemID=="M347101"]

# Dataset that only includes the total item RT
TIRT<-data.table(ResponseTime %>% group_by(StudentID) %>%
                   distinct(ItemID, .keep_all = T) %>%
                   select(-c(ItemRT, ItemVisit)))
TIRT


# 3.5 Total Student Response Time (RT) ----------------------------------------
# Total Student RT (sec) and RT (mins) 
ResponseTime[,TotalStuRT:=sum(ItemRT),by=.(StudentID)] 
ResponseTime[,TotalStuRTMins:=as.numeric(sum(ItemRT))/60,by=.(StudentID)]
ResponseTime

# Dataset that only includes the total student RT 
TSRT<-data.table(ResponseTime  %>% distinct(StudentID, .keep_all = T) %>%
                   select(StudentID,TotalStuRT,TotalStuRTMins))
TSRT

# Distribution of Student RT in Minutes
TSRT[,summary(TotalStuRTMins)]

# Histogram of distribution of Student RT in Minutes
hist(TSRT$TotalStuRTMins)
