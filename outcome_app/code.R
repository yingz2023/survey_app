library("dplyr")
mydat_all <- `RHOAI_outcome...Sheet1.(1)`
mydat_all <- mydat_all[, -105]



data_scientist_data <- subset(mydat_all, V107 == "1", )
data_scientist_data <- data_scientist_data[, -c(105:107)]

ai_developer_data <- subset(mydat_all, V106 == "1")
ai_developer_data <- ai_developer_data[, -c(105:107)]

mlops_data <- subset(mydat_all, V108 == "1")
mlops_data <- mlops_data[, -c(105:107)]

combined_data <- mydat_all
combined_data <- combined_data[, -c(105:107)]

#mydat <- mydat[, -c(105,106,107,108)]
## odd Q column importance; even Q column satisfaction

mydat <- mlops_data
mydat[mydat=="0"]<-NA
library(dplyr)
library(data.table)
#freq
#odd_mydat <- mydat[, c(1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46, 49, 52, 55, 58, 61, 64, 67, 70, 73, 76, 79, 82, 85)]
#odd_mydat <- stack(odd_mydat)
#imp
odd_mydat <- mydat[, c(1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,77,78,79,83,84,85,86,87,88,89,90,91,92,93)]
odd_mydat <- stack(odd_mydat)
#sat
even_mydat <- mydat[, c(2,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,80,81,82,94,95,96,97,98,99,100,101,102,103,104)]
even_mydat <- stack(even_mydat)


#odd_mydat <- unname(odd_mydat)
#even_mydat <- unname(even_mydat)
#data.frame(importance=unlist(odd_mydat))
#odd_mydat <- data.frame(importance = c(t(odd_mydat)), stringsAsFactors=FALSE)
#odd_mydat <- as.numeric(odd_mydat$importance) - 5
#data.frame(satisfaction=unlist(even_mydat))
#even_mydat <- data.frame(satisfaction = c(t(even_mydat)), stringsAsFactors=FALSE)
#even_mydat <- as.numeric(even_mydat$satisfaction) - 5

mydat1 <- cbind(odd_mydat, even_mydat)
mydat1 <- mydat1[, c(1,3)]
colnames(mydat1) <- c("odd_mydat","even_mydat")

outcome <-rep(1:52,each=22)
mydat2 <- cbind(outcome, mydat1)
mydat2$odd_mydat <- as.numeric(mydat2$odd_mydat) 
mydat2$even_mydat <- as.numeric(mydat2$even_mydat) 
write.csv(mydat2,"~/Downloads/cleandata.csv", row.names = FALSE)


###first import saved data files in outcome_app folder and add outcome statements
mydat3 <- `RHOAI_outcome...Sheet2.(1)`
data_scientist_data <- merge(mydat3, data_scientist_data, by = "outcome")
ai_developer_data <- merge(mydat3, ai_developer_data, by = "outcome")
mlops_data <- merge(mydat3, mlops_data, by = "outcome")

write.csv(data_scientist_data,"~/Desktop/outcome_app/data_scientist_data.csv", row.names = FALSE)
write.csv(ai_developer_data,"~/Desktop/outcome_app/ai_developer_data.csv", row.names = FALSE)
write.csv(mlops_data,"~/Desktop/outcome_app/mlops_data.csv", row.names = FALSE)

#remember to load the file & change line 46
mydat2 <- cleandata
midpoint <- 3

# File and column names:

#mydat2 <- Dev.experience.outcome.data

filename <- mydat2
outcomeName <- "outcome"
impName <- "odd_mydat"
satName <- "even_mydat"
# Rows with empty values will be excluded from the calculations:
removeMissing <- TRUE

# functions:
op_score <- function(imp, sat) {
  return(imp + max(imp-sat,0) )
}

count_positives <- function(x, midpoint) {
  x = na.omit(x)
  return (10* (sum(x>midpoint)/length(x)) )
}

plotOppScore <- function(values)
{
  grey <- "#AAAAAA"
  par(xpd=F)
  lowerLimit <- 0
  plot(NULL, xlim=c(lowerLimit,10), ylim=c(lowerLimit,10), yaxt="n", xaxt="n", xaxs="i", yaxs="i", xlab = "Importance", ylab = "Satisfaction", main="Opportunity Landscape_all")
  axis(1, at = c(lowerLimit:10), labels=c(lowerLimit:10), cex=1)
  axis(2, at = c(lowerLimit:10), labels=c(lowerLimit:10), cex=1)
  segments(0, 0, 10, 10, lty = 1, col=grey)
  segments(5, 0, 10, 10, lty = 1, col=grey)
  abline(a = -11.5, b = (10-1)/(10-5.5), lty = 3, col=grey)
  abline(a = -14.5, b = (10-1)/(10-5.5), lty = 3, col=grey)
  text(3,5.8,"Overserved",col=grey)
  text(4,2,"Appropriately\nServed",col=grey)
  text(8,2,"Underserved",col=grey)
  text(8.5,6,"Opp>10",col=grey)
  text(9,4.5,"Opp>12",col=grey)
  text(9.5,3,"Opp>15",col=grey)
  par(xpd=T)
  points(values$importance, values$satisfaction, xlim=c(lowerLimit,10), ylim=c(lowerLimit,10), col=2, pch=19, cex=2)
  with(values, text(satisfaction~importance, labels = c(1:length(values$outcome)), pos = 2, col=1, font=2, srt=0, cex=0.8))
  legend(bg=rgb(0,0,0,0.08),lowerLimit+0.1,9.9, paste(c(1:length(values$outcome)),". ", values$outcome,
                                                      " (",round_df(values$oppscore,2),")",sep=""), cex=0.5, x.intersp=0.0)
}

# Borrowed from http://jeromyanglim.tumblr.com/post/50228877196/round-numbers-in-data-frame-that-contains-non
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

# Read the data and run the functions:

inputData <- mlops
N1 <- dim(inputData)[1]
# remove incomplete rows:
if(removeMissing) inputData <- inputData[rowSums(is.na(inputData)) == 0,]
missing <- N1 - dim(inputData)[1]
inputData <- inputData[c(outcomeName,impName, satName)]
colnames(inputData) <- c("outcome","importance","satisfaction")
imp <- aggregate(importance~outcome, inputData, count_positives, midpoint=midpoint)
sat <- aggregate(satisfaction~outcome, inputData, count_positives, midpoint=midpoint)
values <- merge(x = imp, y = sat, by = "outcome", all = TRUE)
values$oppscore = mapply(op_score, values$importance, values$satisfaction)
values <- values[order(-values$oppscore),]
rownames(values) <- NULL
if(missing > 0) writeLines(paste(missing,"missing values found. Rows excluded from calculations."))
dat <- print(round_df(values, 2))
plotOppScore(values)


