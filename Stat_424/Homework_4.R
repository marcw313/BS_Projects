library(stringr)

text <- "this is a String of text"
date <- "Sat Feb 09 22:09:57 2013"
date2 <- "Monday Jan 07 12:38 :43 2016  "
x<-c("abc123def","a.123","ab$23.45")

## parts 1 and 2 - I did the reading

## problem 3
# a) 
    word(date,1)
    word(date2,1)
# b)
    word(date,5)
    word(date2,5)
# c)
    strsplit(word(date,4),":")[[1]][2]
    strsplit(word(date2,4),":")[[1]][2]
    
##problem 4
# a)
    grep("[u|U]",state.name,value=TRUE)
# b)
    grep("(.)\\1",state.name,value=TRUE)
# c)
    z <- str_split(state.name," ")
    w <- str_split(state.name," ")
    for(i in 1:length(z)){
      if (length(z[[i]]) > 1){
        z[[i]] <- str_c(w[[i]][2],z[[i]][1],sep=" ")
      }
    }
  states <- unlist(z)
  states
    
    
## problem 5
    as.numeric(gsub("[a-z|$&+,:;=?@#|'<>^*()%!]", "", x,ignore.case = TRUE))
    
    
    