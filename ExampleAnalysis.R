# Resources
# Mini Course Workbook - http://naep-research.airprojects.org/AERA2020ProcDataNAEP/
# Mini Course Data Set - http://naep-research.airprojects.org/AERA2020ProcDataNAEP/TermsConditions.html


# 4.0 Prepare Working Environment ---------------------------------------------
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

# StudentData 
load("studentData.Rdata")
str(studentData)
studentData


# 4.1 Data Preparation -------------------------------------------
# Calculator related events
calcEvents <- eventData[interpretation %in% c("Open Calculator", 
                                              "Close Calculator", 
                                              "Calculator Buffer")]

# Selecting the calculator buffer events
calcBuffer <- calcEvents[interpretation %in% "Calculator Buffer",]

# Selecting the events for the calculator item (M346101)
calcItem <- eventData %>% filter(maskedItemID=="M346101")


# 4.2.1 Percentage of Students Using the Calculator -----------------------------
calcUsed <- calcItem %>% 
  filter(interpretation %in% "Calculator Buffer" & 
           !(calcBuffer %in% "[]"))

calcItem$calcUse <- ifelse(calcItem$maskedStuID %in% 
                             calcUsed$maskedStuID, 1,0)

calcUseSummary <- calcItem %>% group_by(calcUse) %>% 
  summarize(nSts= n_distinct(maskedStuID),
            pSts = round(n_distinct(maskedStuID)/
                           n_distinct(calcItem$maskedStuID),
                         digits = 2))

calcUseSummary


# 4.2.2 Calculator Use and Performance ------------------------------------------
head(studentData)
nrow(studentData)

calcUseItem <- unique(calcItem[,c("maskedStuID", "calcUse", "maskedItemID")])
calUseItemScore <- merge(calcUseItem, studentData, by= "maskedStuID")

colnames(calUseItemScore)[4] <- "score"

calcPerf <-  calUseItemScore %>% group_by(calcUse, score) %>%
  summarize (nSts= n_distinct(maskedStuID))

calcPerf$score <- factor(calcPerf$score, levels = c("Incorrect", "Partial", 
                                                    "Correct"))

calcPerf$calcUse <- factor(calcPerf$calcUse, levels = c("0", "1"), 
                           labels = c("Not Used", "Used"))

ggplot(calcPerf, aes(x = score, y = nSts))+
  geom_bar(stat="identity", fill="blue") + 
  facet_wrap(~calcUse)+
  geom_text(aes(label=nSts), vjust= -0.10)+
  xlab ("Score") + 
  ylab ("Number of Students")+
  ylim (0,320)


# 4.3 Computations --------------------------------------------------------
# Selecting only the buffers for the students who used the calculator.
# Remember: If a student used the calculator, the buffer would not be empty!
calcItem <- data.table(calcItem)
buffers <- calcItem[calcUse %in% 1 & 
                      interpretation %in% "Calculator Buffer"]

n_distinct(buffers$maskedStuID)
nrow(buffers)


# 4.3.1 Computations Performed Using Calculator ---------------------------
# Cleaning the calculator buffer
# Selecting last recorded buffer in the item
lastBuffers <- buffers %>% 
  group_by(maskedStuID) %>%
  filter(maskedTimeStamp == max(maskedTimeStamp))

# Checking buffer consisting of only the last recorded buffers
unique(duplicated(lastBuffers$maskedStuID))
nrow(lastBuffers)
lastBuffers <- data.table(lastBuffers)

# There are some extra characters in the buffer other than the calculator keys
lastBuffers[,bufferClean:=str_replace_all(calcBuffer, '[\\[\\]\\"]', '')]

# Now, the buffer is ready to extract computations.
n_distinct(lastBuffers$bufferClean)


# 4.3.2 Number of Students Following Computation Strategies ---------------
# Part A solution method 1
method1 <- "9,3,SUBTRACT,7,2,EQUALS"
lastBuffers[,pAsol1:= str_detect(bufferClean, method1)*1]
table(lastBuffers$pAsol1)

# Part A solution method 2
method2 <- "7,2,SUBTRACT,9,3,EQUALS"
lastBuffers[,pAsol2:=str_detect(bufferClean, method2)*1]
table(lastBuffers$pAsol2)

# Part A common error
method3 <- c("7,2,ADD,9,3,EQUALS", "9,3,ADD,7,2,EQUALS")
computations3 <- paste(method3, collapse = "|")
lastBuffers[,pAerror:=str_detect(bufferClean,computations3)*1]
table(lastBuffers$pAerror)


# Part B solution method 1
method4 <- c("4,5,1,ADD,4,0,7,SUBTRACT,2,0,8,SUBTRACT,2,2,5,EQUALS",
             "4,0,7,ADD,4,5,2,SUBTRACT,2,0,8,SUBTRACT,2,2,5,EQUALS",
             "4,5,1,SUBTRACT,2,0,8,ADD,4,0,7,SUBTRACT,2,2,5,EQUALS",
             "4,5,1,SUBTRACT,2,2,5,ADD,4,0,7,SUBTRACT,2,0,8,EQUALS")

computations4 <- paste(method4, collapse = "|")
lastBuffers[,pBsol1:=str_detect(bufferClean,computations4)*1]
table(lastBuffers$pBsol1)

# Part B solution method 2
method5<- c("8,5,8,SUBTRACT,4,3,3,EQUALS") 
lastBuffers[,pBsol2:=str_detect(bufferClean,method5)*1]
table(lastBuffers$pBsol2)

# Part B error 1
method6 <- c("4,5,1,ADD,4,0,7,SUBTRACT,2,0,8,ADD,2,2,5,EQUALS",
             "4,0,7,ADD,4,5,1,SUBTRACT,2,0,8,ADD,2,2,5,EQUALS",
             "4,5,1,ADD,4,0,7,ADD,2,2,5,SUBTRACT,2,0,8,EQUALS")

computations6 <- paste0(method6, collapse = "|")
lastBuffers[,pBerror1:=str_detect(bufferClean,computations6)*1]
table(lastBuffers$pBerror1)

# Part B error 2
method7 <- "4,5,1,ADD,4,0,7,ADD,2,0,8,ADD,2,2,5,EQUALS"
lastBuffers[,pBerror2:=str_detect(bufferClean,method7)*1]
table(lastBuffers$pBerror2)


# 4.3.3 Computation Strategy and Performance ------------------------------
# Merging the data representing a followed strategy with scores
cols <- colnames(lastBuffers)[grepl("maskedStuID|sol|error", 
                                    colnames(lastBuffers))]

lastBuffersSelect <- lastBuffers[,cols, with=F]

# Disaggregating number of students following a method by their score
methodUseScore <- merge(lastBuffersSelect, studentData, by= "maskedStuID")

colnames(methodUseScore)[colnames(methodUseScore) == "M346101"] <- "score"

methodUsePerf <- methodUseScore[,-"maskedStuID"] %>% group_by(score) %>% 
  summarise_all(sum)

# Visualizing the summary statistics
methodUsePerf2 <- methodUsePerf  %>% 
  pivot_longer(-score, names_to = "method", values_to = "count")                                

methodUsePerf2$score <- factor(methodUsePerf2$score, 
                               levels = c("Incorrect", "Partial", 
                                          "Correct"))

ggplot(methodUsePerf2, aes(x = score, y = count))+
  geom_bar(stat="identity", fill="orange", colour="orange") + 
  facet_wrap(~method)+
  geom_text(aes(label=count), vjust= -0.10)+
  xlab ("Score") + 
  ylab ("Number of Students")+
  labs(title = "Number of Students in Each Score Category by Computation Method")+
  ylim(0,175)
