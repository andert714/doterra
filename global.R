library(tidyverse)
library(fpp3)
library(dygraphs)
library(shiny)
library(shinydashboard)

dfs <- list(
  `Offered Calls` = read_csv('Offered Calls.csv', col_types = 'cDdddd'),
  AHT = read_csv('AHT.csv', col_types = 'cDdddd'),
  `Past Offered Calls` = read_csv('Past Offered Calls.csv'),
  `Past AHT` = read_csv('Past AHT.csv')
) %>%
  map(mutate_if, is.numeric, sqrt)

market_df <- read_csv('Markets.csv')

ts_graph <- function(ts, events = NULL){
  graph <- ts %>% 
    select('y', contains('yhat')) %>% 
    xts::xts(ts$ds) %>%
    dygraph() %>%
    dySeries('y', 'Actual', 'black') %>%
    dyRangeSelector()
  
  if('yhat' %in% names(ts)){
    graph <- graph %>% 
      dySeries(c('yhat_lower', 'yhat', 'yhat_upper'), 'Forecast', 'blue')
  }
  
  if(!is.null(events)){
    graph <- graph %>% 
      dyEvent(events)
  }
  graph
}
ts_accuracy <- function(ts){
  ts %>% 
    summarize(
      ME = mean(y - yhat, na.rm = TRUE),
      MAE = mean(abs(y - yhat), na.rm = TRUE),
      MASE = mean(abs(y - yhat), na.rm = TRUE)/mean(abs(y - lag(y, 7)), na.rm = TRUE),
      Coverage = mean(yhat_lower <= y & yhat_upper >= y, na.rm = TRUE),
      Width = mean(yhat_upper - yhat_lower, na.rm = TRUE)
    ) %>% 
    arrange(MASE, desc(Coverage))
}
ts_max <- function(ts){
  ts %>%
    filter(ds > today()) %>%
    group_by(Month = month(ds, label = TRUE)) %>%
    summarize(`Maximum Forecast` = max(yhat, na.rm = TRUE)) %>%
    mutate(Month = str_c(Month, ' 2020'))
}
