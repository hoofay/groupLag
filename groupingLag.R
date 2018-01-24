# author: daniel hough
# file: groupingLag.R
# date: 24th January 2017
# notes: function to apply grouping and ordering to a dataframe, with associated lag function.

library(dplyr)
library(lazyeval)

df <- data.frame(Names = c(rep('Dan',50),rep('Dave',100)),
                Dates = c(seq(1,100,by=2),seq(1,100,by=1)),
                Values = rnorm(150,0,1))

groupLag <- function(mydf,grouping,ranking,lag){
  df <- mydf
  groupL <- lapply(grouping,as.symbol)
  
  names <- c('Rank','RankDown')
  foos <- list(interp(~rank(var),var=as.name(ranking)),~Rank-lag)
  
  df <- df %>% group_by_(.dots=groupL) %>% mutate_(.dots=setNames(foos,names))
  
  selectedNames <- c('Rank','Values',grouping)
  df2 <- df %>% select_(.dots=selectedNames)
  colnames(df2) <- c('Rank','ValueDown',grouping)
  
  df <- df %>% left_join(df2,by=c('RankDown'='Rank',grouping)) %>% select(-Rank,-RankDown)
  
  return(df)
}

groupLag(df,c('Names'),c('Dates'),1)



