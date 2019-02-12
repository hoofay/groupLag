# dependencies

library(dplyr)
library(lazyeval)

#' group_lag.R
#' function to apply grouping and ordering to a dataframe, with associated lag function.
#'
#' @param mydf input dataframe
#' @param grouping colnames of vectors in mydf to use for dataframe grouping 
#' @param ranking colname of column in mydf to use for ranking 
#' @param lag number of entries to lag by
#' @param lagValue colname of column in mydf to use for lagged values
#'
#' @return mydf returned with additional column of lagged values
#' @export
#'
#' @examples
#'df <- data.frame(Names = c(rep('Dan',50),rep('Dave',100)),
#'                 Dates = c(seq(1,100,by=2),seq(1,100,by=1)),
#'                 Values = rnorm(150,0,1))
#'groupLag(df,c('Names'),c('Dates'),1,'Values')                  
groupLag <- function(mydf,grouping=NULL,ranking='Date',lag=1,lagValue='Values'){
  df <- mydf
  groupL <- lapply(grouping,as.symbol)
  
  names <- c('Rank','RankDown')
  foos <- list(interp(~rank(var),var=as.name(ranking)),~Rank-lag)
  
  df <- df %>% group_by_(.dots=groupL) %>% mutate_(.dots=setNames(foos,names))
  
  selectedNames <- c('Rank',lagValue,grouping)
  df2 <- df %>% select_(.dots=selectedNames)
  colnames(df2) <- c('Rank','lagValue',grouping)
  
  df <- df %>% left_join(df2,by=c('RankDown'='Rank',grouping)) %>% select(-Rank,-RankDown)
  
  return(df)
}


