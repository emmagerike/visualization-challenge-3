
# Read in data, you may need to change the setwd() to a path where you are storing the UK Bike Industry.csv
setwd("/Users/andrewdavies_mbp/Andy's Documents/Teaching and students/URI/BIO 230G - Communicating Science Through Data Visualization/MakeoverMonday/02-25-19-UKBikes/")
data <- read.csv("UK Bike Industry.csv", header = TRUE, stringsAsFactors = FALSE) 

# To make life easier, I go through each column name and rename it to something that is easily worked with.
names(data) #This prints the column names
colnames(data)[1] <- "date"
colnames(data)[2] <- "manualbike"
colnames(data)[3] <- "ebike"
colnames(data)[4] <- "gva"
names(data) #This prints the column names

# Now we need to change the typeof(data$date) from character to a date field
data$date <- as.Date(data$date, "%d/%m/%Y") # We want the date column as a date which allows us to order correctly.
data <- data[order(as.Date(data$date)), ] # Order table by date column

# Investigative plots
par(mfrow=c(3,1)) # 3 plots on one page
plot(data$date, data$manualbike, type="l")
plot(data$date, data$ebike, type="l")
plot(data$date, data$gva, type="l")

par(mfrow=c(1,1))
plot(data$date, data$manualbike, type="l", ylim=c(0,1300000)) # Note scale disparity in y axis, hence I had to lim it
lines(data$date, data$ebike, col="green")

# Thinking now about what to show, lets look at proportion of ebikes to manual bikes
data$prop_e_m <- (data$ebike / data$manualbike) * 100
plot(data$date, data$prop_e_m, type="l") # Looks rubbish

# How many ebikes to manual bikes?
sum(data$manualbike, na.rm=TRUE) / sum(data$ebike, na.rm=TRUE) # approximately 54, lets run with that number..

# Now lets create a nice plot of the seasonal puchasing cycle of manual bikes, i.e. in Winter, Spring, Summer and Fall
par(mfrow=c(1,1))
data$month <- format(as.Date(data$date), "%m")
data2 <- aggregate(x=list(man=data$manualbike, e=data$ebike), by=list(q=data$month), FUN=mean, na.rm=TRUE)


# I used the ggplot2 library to draw the charts, and the pdf(....) function to export them as PDF's
library(ggplot2)
pdf("man.pdf")
ggplot(data=data2, aes(x=q, y=man)) +
  geom_bar(stat="identity")
dev.off()

pdf("e.pdf")
ggplot(data=data2, aes(x=q, y=e)) +
  geom_bar(stat="identity")
dev.off()


