#Week 2 Assignment

####Assignment 1#####

#Using R Data Preparation
#1.a
url <- url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv"
ILPD <- read.csv(url,header=FALSE, stringsAsFactors=FALSE)

#1.b -Get the column headers
#Done

#1.c - Manually construct a vector of columns
header<-c("age", "gender", "total_bilirubin", "direct_bilirubin", "total_proteins", "albumin", "aG_ratio", "SGPT", "SGOT", "Alkphos", "selector")

#1.d - Associate names with the dataframe
names(ILPD) <- header

####Assignment 2#####
#2.a - Use head(ILPD) to view the first 6 rows.
head(ILPD)

#2.b - Determine the mean, median, and standard deviation (sd) of each column.
ILPD$Alkphos <- as.numeric(ILPD$Alkphos)
ILPD$gender <- NULL
mms.df <- data.frame(mean =sapply(ILPD,mean,na.rm=TRUE), 
                     median = sapply(ILPD,median,na.rm=TRUE),
                     sd = sapply(ILPD,sd,na.rm=TRUE))
mms.df

#2.c - What does na.rm = TRUE do in sd(x, na.rm = TRUE)
#It removes all of the NAs in the vector before calculating SD

#2.d - Create Histograms (hist) for each column where possible.
#print four at a time
#if you recive error figure margins too large, enlarge window or change set mfrow to c(1,1)
par(mfrow=c(2,2)) 
for (val in row.names(mms.df)){
  hist(ILPD[[val]], col="lavender")
}

#2.e - Use the plot(ILPD) function on this data frame to present a general overview of the data
par(mfrow=c(1,1))
#convert gender 0 into male 1 into female
ILPD$gender <- ifelse(ILPD$gender=="Male",0,1)
plot(ILPD)

#2.e.a What can you say about the data? 
#There are many things we can say about the data. For example, that some variables like age and SGOT
#are not correlated while s SGPT and SGOT are correlated. We can also see which variables are binary
#such as gender and selector.

#2.e.b - How can you tell if a vector contains continuous numbers or binary data?
#Binary data appear as dots that form straight perpendicular line segments. 
#Continuous data appear to be dots in a more cloud-like formation, which are not perpendicularly aligned.

#2.e.c - How can you tell if two vectors are correlated?
#Two continuous variables are correlated when the points on the scatter plot appear to coalesce along a line, 
#which tends to be around 45 positive or negative degree slope. For example, SGPT and SGOT appear to be correlated.
#With two binary or categorical variables it's more difficult to tell, however, if the line segments
#appear to be at uneven heights the variables may be correlated.

####Assignment 3####
#3.a -Remove Outliers: c(-1, 1, 5, 1, 1, 17, -3, 1, 1, 3)
data.vect <- c(-1, 1, 5, 1, 1, 17, -3, 1, 1, 3)
#create a function to remove outliers
remove.outliers <- function(x) {
  threshhold <- 2*sd(x)
  #keep only data in the IQR
  return(x[-threshhold < x & threshhold > x])
}
remove.outliers(data.vect)

#3.b - Relabel: c('BS', 'MS', 'PhD', 'HS', 'Bachelors', 'Masters', 'High School', 'BS', 'MS', 'MS')
data.abb <- c('BS', 'MS', 'PhD', 'HS', 'Bachelors', 'Masters', 'High School', 'BS', 'MS', 'MS')
data.abb[data.abb == 'Bachelors'] <- 'BS'
data.abb[data.abb == 'Masters'] <- 'MS'
data.abb[data.abb == 'High School'] <- 'HS'

#3.c - Normalize: c(-1, 1, 5, 1, 1, 17, -3, 1, 1, 3)
data.vect <- c(-1, 1, 5, 1, 1, 17, -3, 1, 1, 3)
#3.c.a - Min-Max Normalization
min.max.norm <-(data.vect-min(data.vect))/(max(data.vect)-min(data.vect))
min.max.norm
#3.c.b - Z-score normalization
zscore.norm <- scale(data.vect)[,1]
zscore.norm

#3.d - Binarize: c('Red', 'Green', 'Blue', 'Blue', 'Blue', 'Blue', 'Blue', 'Red', 'Green', 'Blue')
color.vect <-  c('Red', 'Green', 'Blue', 'Blue', 'Blue', 'Blue', 'Blue', 'Red', 'Green', 'Blue')
binary.matrix <- data.frame(red=as.numeric(color.vect == "Red"),
                            blue=as.numeric(color.vect == "Blue"),
                            green=as.numeric(color.vect == "Green"))


#3.e - Discretize: c(3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 12, 23, 23, 25, 81)
number.vect <-  c(3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 12, 23, 23, 25, 81)

#3.e.a - 3 Bins of equal range
cut(number.vect,breaks = 3)

#3.e.b - 3 Bins Equal of near equal amounts 
number.vect[1:9] <- "Lower"
number.vect[10:18] <- "Medium"
number.vect[19:28] <- "High"
number.vect
#TEST
