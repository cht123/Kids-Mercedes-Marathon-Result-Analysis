# do setup
setwd("~/Classes/DSS")
library(dplyr)
library(stringr) 

# get the data
#fileURL <- "http://www.besttimescct.com/results/Mercedes14_kids.txt"
fileURL <- "http://www.besttimescct.com/results/Mercedes15_kids.txt"


download.file(fileURL, destfile = "kidrace.txt", method = "curl")

kidrace <- read.csv("kidrace.txt", sep = "|", header = TRUE, skip = 4,blank.lines.skip = TRUE,na.strings = "?", 
                comment.char="", check.names = F, stringsAsFactors = F, quote="\"")

# identify rows to be removed
text_rows <- grepl("[A-Z]", kidrace[,1])
asterisk_rows <- grepl("==", kidrace[,1])

# remove bad rows
kidrace <- kidrace[!text_rows]
kidrace <- kidrace[!asterisk_rows,]
class(kidrace$v1) 
#clean up column names for now
colnames(kidrace) <- c("v1", "v2", "v3", "v4", "v5")

#stack columns
stacked_kidrace <- c(kidrace$v1, kidrace$v2,kidrace$v3,kidrace$v4,kidrace$v5)
stacked_kidrace <- str_trim(stacked_kidrace)

# find the first space
first_space <- str_locate(stacked_kidrace, " ")[,1]

# extract bib vector
bib <- str_sub(stacked_kidrace, 0, first_space-1)

# extract time
time <- str_trim(str_sub(stacked_kidrace, first_space, str_length(stacked_kidrace)))

# split time
split_time <- str_split(time, ":")

# functions to extract time elements
get_minutes<- function(time_string){
  min <- vector()  
  sec <- vector()
  for (i in 1:length(time_string)) {                                   
    min<-c(min, time_string[[i]][1])        
    sec<-c(sec, time_string[[i]][2])
  } 
  min
}

get_seconds<- function(time_string){  
  sec <- vector()
  for (i in 1:length(time_string)) {                                           
    sec<-c(sec, time_string[[i]][2])
  } 
  sec
}

# extract time elements
mins <- as.numeric(get_minutes(split_time))
sec <- as.numeric(get_seconds(split_time))

# calculate times
min_sec <- mins * 60 
tot_sec <- sec + min_sec
min_dec <- tot_sec / 60

# create data frame and remove incomplete rows
rep_data <- bind_cols(as.data.frame(bib), as.data.frame(tot_sec), as.data.frame(min_dec))
clean <- complete.cases(rep_data)
clean_rep_data <- rep_data[clean,]
clean_rep_data <- mutate(clean_rep_data, rank = min_rank(min_dec))
clean_rep_data <- mutate(clean_rep_data, pct_rank = percent_rank(min_dec)*100)

# Kid's positions
#Grace
filter(clean_rep_data, bib == "2478")

#Alex
filter(clean_rep_data, bib == "2409")

#Nate
filter(clean_rep_data, bib == "2410")

# calc 5 number
summary(clean_rep_data)

# create a histogram
hist(rep_data$min_dec, breaks = 80, border = "azure", lty="solid", col = "azure3",
     main = "2015 Kids Mercedes Marathon \n 1 Mile Run Times",
     xlab = "minutes")
rug(clean_rep_data$min_dec)
abline(v=10.35, lwd = 2, col = "darkgreen")
abline(v=6.95, lwd = 2, col = "blue") # lwd sets the weight of the line for the cutoff
abline(v=7.06, lwd = 2, col = "red") # lwd sets the weight of the line for the cutoff
text(25, 400, paste("Maximum = ", round(max(clean_rep_data$min_dec)), digits = 2)) 
text(25, 360, paste("Median = ", median(clean_rep_data$min_dec))) 
text(25, 320, paste("Average = ", round(mean(clean_rep_data$min_dec)), digits = 2)) 
text(25, 280, paste("Minimum = ", round(min(clean_rep_data$min_dec)), digits = 2)) 
text(25, 240, paste("Grace rank = ", select(filter(clean_rep_data, bib == "2478"), rank),
                    ",",
                    round(select(filter(clean_rep_data, bib == "2478"), pct_rank), digits = 2))) 
text(25, 200, paste("Alex rank = ", select(filter(clean_rep_data, bib == "2409"), rank),
                    ",",
                    round(select(filter(clean_rep_data, bib == "2409"), pct_rank), digits = 2))) 
text(25, 160, paste("Nate rank = ", select(filter(clean_rep_data, bib == "2410"), rank),
                    ",",
                    round(select(filter(clean_rep_data, bib == "2410"), pct_rank), digits = 2))) 

text(25, 100, paste("Total runners = ", length(clean_rep_data$min_dec)))

# create boxplot
boxplot(clean_rep_data$min_dec, col = "azure3" ,main = "Histogram of 1 Mile Run Times", ylab = "minutes" )
abline(h = 10.35, col = "darkgreen") 
abline(h = 6.95, col = "blue") 
abline(h = 7.06, col = "red") 