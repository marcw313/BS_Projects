library(XML)
library(plyr)

Rating <- c("G","PG","PG-13","R")
year <- c(2010:2014)
iterate <- 1
movies <- list()  # create a list of 0.

for (y in 1:length(year))
{
  for(r in 1:length(Rating))
  {
    movies[[iterate]] <- readHTMLTable(paste0("http://www.the-numbers.com/movies/report/All/All/All/All/",Rating[r],"-(US)/All/All/All/All/None/None/",year[y],"/",year[y],"/None/None/None/None?view-order-by=domestic-box-office&show-release-year=On&view-order-direction=desc&show-production-budget=On&show-domestic-box-office=On"))[[1]]
    names(movies[[iterate]])<-c("Rating","Year","Title","Budget","domBoxOffice")
    movies[[iterate]]$Rating = Rating[r]
    iterate = iterate + 1
  }
}

movies.df = ldply(movies)
movies.df = subset(movies.df,Budget != "$0" & domBoxOffice != "$0")
movies.df$Budget <- gsub(",", "", movies.df$Budget, fixed=TRUE)
movies.df$Budget <- gsub("$", "", movies.df$Budget, fixed=TRUE)
movies.df$Budget <- as.numeric(movies.df$Budget)
movies.df$domBoxOffice <- gsub(",", "", movies.df$domBoxOffice, fixed=TRUE)
movies.df$domBoxOffice <- gsub("$", "", movies.df$domBoxOffice, fixed=TRUE)
movies.df$domBoxOffice <- as.numeric(movies.df$domBoxOffice)
#movies.df$Title <- gsub(" ","",movies.df$Title, fixed=TRUE)
# make into millions
movies.df$Budget = movies.df$Budget/1000000
movies.df$domBoxOffice = movies.df$domBoxOffice/1000000
boxoffice <- movies.df
# create factor levels for budget
boxoffice$budgetType = "big"
boxoffice$budgetType[boxoffice$Budget < mean(boxoffice$Budget)] = "low"
boxoffice$budgetType = factor(boxoffice$budgetType)


library(stringr)

#Create an empty vector into which we will store Rotten Tomato scores
RT <- vector()

for (i in 1:nrow(boxoffice))
{
  #URL's are fairly standard on Rotten Tomatoes, and these lines format the names properly
  movies_url <- str_replace_all(boxoffice$Title, fixed("The "),"")
  movies_url <- str_replace_all(movies_url, fixed("Les "),"")
  movies_url <- str_replace_all(movies_url, fixed(" "), "_")
  movies_url <- str_replace_all(movies_url, fixed("-"), "")
  movies_url <- str_replace_all(movies_url, fixed("."), "")
  movies_url <- str_replace_all(movies_url, fixed("("),"")
  movies_url <- str_replace_all(movies_url, fixed(")"),"")
  movies_url <- str_replace_all(movies_url, fixed(":"),"")
  movies_url <- str_replace_all(movies_url, fixed("'"),"")
  movies_url <- str_replace_all(movies_url, fixed("&"),"and")
  movies_url <- str_replace_all(movies_url, fixed(","),"")
  
  #We need to make URLs that have the year attached at the end first, otherwise a movie of 
  #the same name that was released previously would be queried, not the one we were looking
  #for.  However, if the movie has no "doppleganger", there is no year attached.
  
  url <- "http://www.rottentomatoes.com/m/"
  url_no_year <- paste(url,movies_url[i],"/", sep="")
  url_with_year <- paste(url,movies_url[i],"_",boxoffice$Year[i],"/",sep="")
  
  #For the cases in which the URL is not uniform, we enter a missing value for RT score
  
  thepage <- try(readLines(url_with_year), silent=TRUE)
  if ('try-error' %in% class(thepage))
  {   
    thepage <- try(readLines(url_no_year), silent=TRUE)
    if ('try-error' %in% class(thepage))
    {
      score <- ""
    }
    else
    {
      line <- grep('twitter:data1',thepage)[1]
      score <- sub('.*?name="twitter:data1" content="(.*?) of critics liked it">.*', "\\1", thepage[line])
      
      if (grepl("omniture",score)==TRUE)
      {
        score <- ""
      }
    }
  }
  else
  {
    line <- grep('twitter:data1',thepage)[1]
    score <- sub('.*?name="twitter:data1" content="(.*?) of critics liked it">.*', "\\1", thepage[line])
    
    if (grepl("omniture",score)==TRUE)
    {
      score <- ""
    }
  }
  
  RT <- c(RT, score)  
}

#Create a column for RT score in boxoffice
boxoffice$RT <- RT

#Delete all observations that didn't get a score, then set to numeric
boxoffice.clean <- subset(boxoffice,boxoffice$RT != "" & boxoffice$RT != "NA")
boxoffice.clean$RT <- gsub("%", "", boxoffice.clean$RT, fixed=TRUE)
boxoffice.clean$RT <- as.numeric(boxoffice.clean$RT)
boxoffice.clean$Rating <- factor(boxoffice.clean$Rating)

out <- lm(domBoxOffice~RT+Rating+budgetType, data=boxoffice.clean, x=TRUE)

qqnorm(out$residuals)
hist(out$residuals,breaks=17,main = "Histogram of Residuals")

