############# <README> ###############
# This file is a project analyzing BYU's performance in NCAA football vs.
# the average NCAA football team based on the day of the week they are playing.
# I analyzed on several different variables, such as points, pass yards, time
# of possession, etc.
# I have attached the presentation file to go along with this code to display
# the actual analysis, since there is no way you can access this data by
# running my code.
############ </README> ##############

library(DBI)
library(RMySQL)

## open a connection to a MySQL database
con <- dbConnect(dbDriver("MySQL"), dbname = "********", user='*********',
                 password='*********', host='*******', port=*****)           #I have changed the values here for security purposes


#####Query for ALL TEAMS games
f <- dbGetQuery(con, paste('SELECT * FROM projectSMW'))
my.mean <- aggregate(cbind(RushYards,PassYards,TimeofPoss,Points) ~ Weekday, f, mean)
days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
x <- 1:length(days)
for (i in 1:length(days)){
  x[i] <- length(f$Weekday[f$Weekday == days[i]])
}
#number of games per day in the dataset
this <- data.frame(days,x)
this <- this[order(days),]
names(this)<-c("Weekday","Number.of.Games")
ALL.TEAMS <- merge(my.mean,this)
ALL.TEAMS


#####Query for only BYU games
BYU <- dbGetQuery(con, paste('SELECT * FROM projectSMW where TeamCode=77'))
my.mean <- aggregate(cbind(RushYards,PassYards,TimeofPoss,Points) ~ Weekday, BYU, mean)

days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
a <- 1:length(days)
for (i in 1:length(days)){
  a[i] <- length(BYU$Weekday[BYU$Weekday == days[i]])
}
BYU$Weekdaycode <- NA
BYU[BYU$Weekday=="Friday",]$Weekdaycode <- 2
BYU[BYU$Weekday=="Saturday",]$Weekdaycode <- 3
BYU[BYU$Weekday=="Thursday",]$Weekdaycode <- 1
#number of games per day in the dataset
this <- data.frame(days,a)
this <- this[order(days),]
names(this)<-c("Weekday","Number.of.Games")
BYU.TEAM <-merge(my.mean,this)
BYU.TEAM


#####Query for other team games
OTHER <- dbGetQuery(con, paste('SELECT * FROM projectSMW where TeamCode!=77'))
my.mean <- aggregate(cbind(RushYards,PassYards,TimeofPoss,Points) ~ Weekday, OTHER, mean)

days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
b <- 1:length(days)
for (i in 1:length(days)){
  b[i] <- length(OTHER$Weekday[OTHER$Weekday == days[i]])
}
OTHER$Weekdaycode <- NA
OTHER[OTHER$Weekday=="Friday",]$Weekdaycode <- 2
OTHER[OTHER$Weekday=="Saturday",]$Weekdaycode <- 3
OTHER[OTHER$Weekday=="Thursday",]$Weekdaycode <- 1
#number of games per day in the dataset
this <- data.frame(days,b)
this <- this[order(days),]
names(this)<-c("Weekday","Number.of.Games")
NOT.BYU <-merge(my.mean,this)
NOT.BYU

allTeams <- ALL.TEAMS[ALL.TEAMS$Weekday %in% c("Friday","Saturday","Thursday"),]
notByu <- NOT.BYU[NOT.BYU$Weekday %in% c("Friday","Saturday","Thursday"),]
byuTeam <- BYU.TEAM[BYU.TEAM$Weekday %in% c("Friday","Saturday","Thursday"),]

#add in weekdaycode to the aggregate summaries
byuTeam$Weekdaycode <- NA
byuTeam[byuTeam$Weekday=="Thursday",]$Weekdaycode <- 1 
byuTeam[byuTeam$Weekday=="Friday",]$Weekdaycode <- 2
byuTeam[byuTeam$Weekday=="Saturday",]$Weekdaycode <- 3

notByu$Weekdaycode <- NA
notByu[notByu$Weekday=="Thursday",]$Weekdaycode <- 1 
notByu[notByu$Weekday=="Friday",]$Weekdaycode <- 2
notByu[notByu$Weekday=="Saturday",]$Weekdaycode <- 3
byuTeam



par(mfrow=c(1,1))
plot(Points~Weekdaycode,data=byuTeam,ylim=c(25,35),col="royalblue")
points(Points~Weekdaycode,data=notByu,col="red")
abline(lm(Points~Weekdaycode,data=BYU),col="royalblue")
abline(lm(Points~Weekdaycode,data=OTHER),col="red")


##BYU vs. Other averages on Saturday
t.test(BYU$PassYards[BYU$Weekday=="Saturday"],
       OTHER$PassYards[OTHER$Weekday=="Saturday"])  # significant
t.test(BYU$RushYards[BYU$Weekday=="Saturday"],
       OTHER$RushYards[OTHER$Weekday=="Saturday"])  # non-significant
t.test(BYU$TimeofPoss[BYU$Weekday=="Saturday"],
       OTHER$TimeofPoss[OTHER$Weekday=="Saturday"])  # significant
t.test(BYU$Points[BYU$Weekday=="Saturday"],
       OTHER$Points[OTHER$Weekday=="Saturday"])  #significant
#BYU
par(mfrow=c(2,3))
hist(BYU$PassYards[BYU$Weekday=="Saturday"],freq=FALSE,
     xlim=c(0,600),xlab="Pass Yards",main="BYU Pass Yards on Saturday")
hist(BYU$TimeofPoss[BYU$Weekday=="Saturday"],freq=FALSE,
     xlim=c(1000,3000),xlab="Time of Possession",main="BYU Time of Posession on Saturday")
hist(BYU$Points[BYU$Weekday=="Saturday"],freq=FALSE,
     xlim=c(0,90),xlab="Points",main="BYU Points on Saturday")
# Other Teams
hist(OTHER$PassYards[OTHER$Weekday=="Saturday"],freq=FALSE,
     xlim=c(0,600),xlab="Pass Yards",main="Other Team Pass Yards on Saturday")
hist(OTHER$TimeofPoss[BYU$Weekday=="Saturday"],freq=FALSE,
     breaks=9,xlim=c(1000,3000),xlab="Time of Possession",
     main="Other Team Time of Possession on Saturday")
hist(OTHER$Points[OTHER$Weekday=="Saturday"],freq=FALSE,
     breaks=9,xlim=c(0,90),xlab="Points",main="Other Team Points on Saturday")


##BYU vs. Other averages on Friday -- not significant
t.test(BYU$PassYards[BYU$Weekday=="Friday"],OTHER$PassYards[OTHER$Weekday=="Friday"])
t.test(BYU$RushYards[BYU$Weekday=="Friday"],OTHER$RushYards[OTHER$Weekday=="Friday"])
t.test(BYU$TimeofPoss[BYU$Weekday=="Friday"],OTHER$TimeofPoss[OTHER$Weekday=="Friday"])
t.test(BYU$Points[BYU$Weekday=="Friday"],OTHER$Points[OTHER$Weekday=="Friday"])

##BYU vs. Other averages on Thursday  --  not significant
t.test(BYU$PassYards[BYU$Weekday=="Thursday"],OTHER$PassYards[OTHER$Weekday=="Thursday"])
#Rush Significantly worse on thursday
t.test(BYU$RushYards[BYU$Weekday=="Thursday"],OTHER$RushYards[OTHER$Weekday=="Thursday"])
t.test(BYU$TimeofPoss[BYU$Weekday=="Thursday"],OTHER$TimeofPoss[OTHER$Weekday=="Thursday"])
t.test(BYU$Points[BYU$Weekday=="Thursday"],OTHER$Points[OTHER$Weekday=="Thursday"])




##BYU vs. BYU averages for Pass on different days  --  not significant
t.test(BYU$PassYards[BYU$Weekday=="Saturday"],BYU$PassYards[BYU$Weekday=="Friday"])
t.test(BYU$PassYards[BYU$Weekday=="Saturday"],BYU$PassYards[BYU$Weekday=="Thursday"])
t.test(BYU$PassYards[BYU$Weekday=="Friday"],BYU$PassYards[BYU$Weekday=="Thursday"])
par(mfrow=c(3,1))
hist(BYU$PassYards[BYU$Weekday=="Thursday"],freq=FALSE,xlim=c(0,600),
     main="BYU Passing on Thursday",xlab= "Pass Yards")
hist(BYU$PassYards[BYU$Weekday=="Friday"],freq=FALSE,xlim=c(0,600),
     main="BYU Passing on Friday",xlab= "Pass Yards")
hist(BYU$PassYards[BYU$Weekday=="Saturday"],freq=FALSE,xlim=c(0,600),
     main="BYU Passing on Saturday",xlab= "Pass Yards")



##BYU vs. BYU averages for Rush on different days
t.test(BYU$RushYards[BYU$Weekday=="Saturday"],
       BYU$RushYards[BYU$Weekday=="Friday"]) # not significant
t.test(BYU$RushYards[BYU$Weekday=="Saturday"],
       BYU$RushYards[BYU$Weekday=="Thursday"]) # significant
t.test(BYU$RushYards[BYU$Weekday=="Friday"],
       BYU$RushYards[BYU$Weekday=="Thursday"]) # significant
par(mfrow=c(3,1))
hist(BYU$RushYards[BYU$Weekday=="Thursday"],freq=FALSE,xlim=c(0,600),
     main="BYU Rushing on Thursday",xlab= "Rushing Yards",col="red")
hist(BYU$RushYards[BYU$Weekday=="Friday"],freq=FALSE,xlim=c(0,600),
     main="BYU Rushing on Friday",xlab= "Rushing Yards",col="blue")
hist(BYU$RushYards[BYU$Weekday=="Saturday"],freq=FALSE,xlim=c(0,600),
     main="BYU Rushing on Saturday",xlab= "Rushing Yards",col="blue")



##BYU vs. BYU averages for Time of Possession on different days
t.test(BYU$TimeofPoss[BYU$Weekday=="Saturday"],
       BYU$TimeofPoss[BYU$Weekday=="Friday"]) # not significant
t.test(BYU$TimeofPoss[BYU$Weekday=="Saturday"],
       BYU$TimeofPoss[BYU$Weekday=="Thursday"]) # not significant
t.test(BYU$TimeofPoss[BYU$Weekday=="Friday"],
       BYU$TimeofPoss[BYU$Weekday=="Thursday"]) # not significant
par(mfrow=c(3,1))
hist(BYU$TimeofPoss[BYU$Weekday=="Thursday"],freq=FALSE,xlim=c(1000,4000),
     main="BYU Time of Possession on Thursday",breaks=20,xlab="Time of Possession")
hist(BYU$TimeofPoss[BYU$Weekday=="Friday"],freq=FALSE,xlim=c(1000,4000),
     main="BYU Time of Possession on Friday",xlab="Time of Possession")
hist(BYU$TimeofPoss[BYU$Weekday=="Saturday"],freq=FALSE,xlim=c(1000,4000),
     main="BYU Time of Possession on Saturday",breaks=20,xlab="Time of Possession")



##BYU vs. BYU averages for Points on different days
t.test(BYU$Points[BYU$Weekday=="Saturday"],
       BYU$Points[BYU$Weekday=="Friday"]) # not significant
t.test(BYU$Points[BYU$Weekday=="Saturday"],
       BYU$Points[BYU$Weekday=="Thursday"]) # not significant
t.test(BYU$Points[BYU$Weekday=="Friday"],
       BYU$Points[BYU$Weekday=="Thursday"]) # not significant
par(mfrow=c(3,1))
hist(BYU$Points[BYU$Weekday=="Thursday"],freq=FALSE,xlim=c(0,80),
     main="BYU Points on Thursday",xlab="Points")
hist(BYU$Points[BYU$Weekday=="Friday"],freq=FALSE,xlim=c(0,80),
     main="BYU Points on Friday",xlab="Points")
hist(BYU$Points[BYU$Weekday=="Saturday"],freq=FALSE,xlim=c(0,80),
     main="BYU Points on Saturday",xlab="Points")


#points BYU vs. Others
t.test(BYU$Points[BYU$Weekday=="Thursday"],
       OTHER$Points[BYU$Weekday=="Thursday"]) # not significant
t.test(BYU$Points[BYU$Weekday=="Friday"],
       OTHER$Points[BYU$Weekday=="Friday"]) # not significant
t.test(BYU$Points[BYU$Weekday=="Saturday"],
       OTHER$Points[BYU$Weekday=="Saturday"]) # significant
## plotting histograms of points by weekday
# BYU
par(mfrow=c(2,3))
hist(BYU$Points[BYU$Weekday=="Thursday"],freq=FALSE,
     xlim=c(0,90),xlab="Points",main="BYU Points on Thursday")
hist(BYU$Points[BYU$Weekday=="Friday"],freq=FALSE,
     xlim=c(0,90),xlab="Points",main="BYU Points on Friday")
hist(BYU$Points[BYU$Weekday=="Saturday"],freq=FALSE,
     xlim=c(0,90),xlab="Points",main="BYU Points on Saturday",col="blue")

# Other Teams
hist(OTHER$Points[OTHER$Weekday=="Thursday"],freq=FALSE,
     breaks=9,xlim=c(0,90),xlab="Points",main="Other Team Points on Thursday")
hist(OTHER$Points[OTHER$Weekday=="Friday"],freq=FALSE,
     breaks=9,xlim=c(0,90),xlab="Points",main="Other Team Points on Friday")
hist(OTHER$Points[OTHER$Weekday=="Saturday"],freq=FALSE,
     breaks=9,xlim=c(0,90),xlab="Points",main="Other Team Points on Saturday",col="red")



dbDisconnect(con) # Delete db Connection 
