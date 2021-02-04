########## Libraries ##########
library(tidyverse)
library(fpp3)
library(prophet)

########## Folders ##########
markets <- read_csv('Markets.csv')
markets$start_aht <- as.Date(markets$start_aht, "%m/%d/%y")
markets$start_oc <- as.Date(markets$start_oc, "%m/%d/%y")
folders <- list(
  Data = 'O:/Call Center/WFM/Interns/Market Call Data', # directory of CMS data
  `Offered Calls` = 'O:/Call Center/WFM/Interns/Call Volume Forecasts/Current', # directory of call volume forecasts
  AHT = 'O:/Call Center/WFM/Interns/AHT Forecasts/Current', # directory of AHT forecasts
  `Past Offered Calls` = 'O:/Call Center/WFM/Interns/Accuracy/Offered Calls', # directory of past call volume forecasts
  `Past AHT` = 'O:/Call Center/WFM/Interns/Accuracy/AHT' # directory of past AHT forecasts
)

########## Import ##########

# Imports market data
# market: (string) market name
# y: (string) either 'Offered Calls' or 'AHT'
# ex. read_ts('Mexico', 'Offered Calls')
read_ts <- function(market, y){
  read_cms <- function(market, y){
    str_c(folders$Data, '/', market, '.csv') %>% 
      read_csv(skip = 3, col_names = FALSE, col_types = 'c____d__d_____') %>% 
      rename(ds = X1, `Offered Calls` = X6, AHT = X9) %>% 
      filter(ds != '_') %>% 
      mutate_at('ds', mdy) %>% 
      select(ds, y = all_of(y)) %>%  
      mutate(y = replace(y, y > 10000, NA))
  }
  if(market == 'Australia'){
    if(y == 'Offered Calls'){
      str_c(folders$Data, '/Australia.csv') %>% 
        read_csv %>% 
        transmute(ds = date, y = calls)
    } else if(y == 'AHT'){
      tibble()
    }
    # remove to split up CA into individual forecasts (add Chile)
  # } else if(market == 'Central America'){
  #   c('Central America', 'Guatemala', 'Ecuador', 'Costa Rica') %>% 
  #     map_df(read_cms, 'Offered Calls') %>% 
  #     group_by(ds) %>% 
  #     summarize(y = sum(y))
  } else{
    read_cms(market, y)
  }
}

# Imports market forecast
# market: (string) market name
# y: (string) either 'Offered Calls' or 'AHT'
# ex. read_fc('Mexico', 'Offered Calls')
read_fc <- function(market, y){
  csv <- market %in% filter(markets, start_aht == as_date('2020-05-10'))$market
  if(csv){
    df <- str_c(folders[[y]], '/', market, '.csv') %>%
      read_csv
  } else if(!csv){
    df <- str_c(folders[[y]], '/', market, '.xlsx') %>%
      readxl::read_excel() %>%
      rename_all(function(x) str_replace(x, 'Day', 'Date')) %>%
      mutate_at('Date', as_date)
  }
  df %>%
    rename(ds = Date, yhat_lower = `Lower 80%`, yhat = Forecast, yhat_upper = `Upper 80%`) %>%
    filter(ds >= today())
}

########## Analysis ##########

# Makes an interactive graph of a time series tibble
# ts: (tibble) a dataframe with columns ds (date), y (actuals), and optionally
# yhat_lower, yhat, and yhat_upper (predictions with intervals)
# ex. data <- read_ts('Mexico', 'Offered Calls')
# pred <- read_fc('Mexico', 'Offered Calls')
# df <- full_join(data, pred, 'ds')
# ts_graph(df)
ts_graph <- function(ts){
  graph <- ts %>% 
    select('y', contains('yhat')) %>% 
    xts::xts(ts$ds) %>%
    dygraphs::dygraph() %>%
    dygraphs::dySeries('y', 'Actual', 'black') %>%
    dygraphs::dyRangeSelector()
  
  if('yhat' %in% names(ts)){
    graph <- graph %>% 
      dygraphs::dySeries(c('yhat_lower', 'yhat', 'yhat_upper'), 'Forecast','blue')
  }
  graph
}

# Makes graph of seasonality boxplots of a time series tibble
# ts: (tibble) a dataframe with columns ds (date), y (actuals)
# ex. data <- read_ts('Mexico', 'Offered Calls')
# ts_boxplot(data)
ts_boxplot <- function(ts){
  ts_plot <- ts %>% 
    mutate(
      Weekday = wday(ds, label = TRUE),
      Month = month(ds, label = TRUE),
      Day = as.factor(day(ds))
    ) %>% 
    ggplot()
  gridExtra::grid.arrange(
    ts_plot + geom_boxplot(aes(Weekday, y)),
    ts_plot + geom_boxplot(aes(Month, y)),
    ts_plot + geom_boxplot(aes(Day, y)),
    layout_matrix = matrix(c(1, 3, 2, 3), ncol = 2)
  )
}

# Makes a tibble with exploratory data from a time series tibble
# ts: (tibble) a dataframe with columns ds (date), y (actuals)
# ex. data <- read_ts('Mexico', 'Offered Calls')
# ts_explore(data)
ts_explore <- function(ts){
  ts <- ts %>% 
    group_by(wday = wday(ds, label = TRUE)) %>% 
    mutate(wday_mean = mean(y))
  
  list(
    fix_hol = ts %>% 
      group_by(hol = str_c(month(ds, label = TRUE), day(ds))) %>% 
      summarize(diff = mean(y - wday_mean)),
    var_hol = ts %>% 
      group_by(hol = str_c(month(ds, label = TRUE), ceiling(day(ds)/7), wday)) %>% 
      summarize(diff = mean(y - wday_mean)),
    mday = ts %>% 
      group_by(hol = str_c(wday, day(ds))) %>% 
      summarize(diff = mean(y - wday_mean))
  ) %>% 
    bind_rows(.id = 'type') %>% 
    arrange(desc(abs(diff)))
}

# Makes an tibble with accuracy metrics from a time series tibble
# ts: (tibble) a dataframe with columns ds (date), y (actuals), and 
# yhat_lower, yhat, and yhat_upper (predictions with intervals)
# ex. data <- read_ts('Mexico', 'Offered Calls')
# pred <- read_fc('Mexico', 'Offered Calls')
# df <- full_join(data, pred, 'ds')
# ts_accuracy(df)
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

########## Modeling ##########

# Makes a prophet model (this is probably the most complicated one)
# Arguments:
# ts: tibble with ds (date) and y (actuals). It should only contain the dates in
# the past you want to use (I take out weekends and holidays when closed) along 
# with the dates in the future you want to predict (I take out weekends and 
# holidays when closed).
# prophet arguments: changepoints, n_changepoints, changepoint_range, prior_scale, seasonality_mode
# transform: (function) function for transforming time series
# back_transform: (function) function for back transforming time series (should be inverse of transform)
# seasonality: (string) a string containing all seasonalities to be used. Options are 'weekly', 'monthly', and 'yearly'
# fixed_hols: (string): Days of the year to be considered holidays (eg. c('May1), 'Dec24'))
# var_hols: (string): Days of the year to be considered holidays (eg. c('Feb1Mon', 'Mar3Mon'))
# month_days: (string) The combinations of day of the week and day of the month 
# to include as factors. Acceptable formats are 'All15' for all fifteenths to be
# a factor, or 'Mon15' for monday fifteenths to be a factor (eg. c('All15', 'Mon16'))
# last: (boolean) Whether to make the last day of the month a factor
# other: (list) a list of vectors containing dates which could affect the forecast (eg. BOGOs, easter, corona)
#
# Ex.
# tibble(ds = seq(as_date('2018-06-22'), as_date('2020-12-31'), 'day')) %>% 
#   filter(wday(ds) %in% 2:7,!(ds %in% as_date(c(
#     '2018-07-04', '2018-11-22', '2018-12-25', '2019-01-01',
#     '2019-07-04', '2019-11-28', '2019-12-25', '2020-01-01',
#     '2020-07-04', '2020-11-26', '2020-12-25'
#   )))) %>% 
#   left_join(read_ts('Mexico', 'Offered Calls'), 'ds') %>% 
#   make_model(
#     transform = function(x) sqrt(x),
#     back_transform = function(x) x*abs(x),
#     seasonality = c('weekly', 'monthly', 'yearly'),
#     fixed_hols = c('May1', 'May10', 'Sep16', 'Dec24'),
#     var_hols = c('Feb1Mon', 'Mar3Mon'),
#     month_days = c('All14', 'All15'),
#     last = TRUE, 
#     other = list(
#       bogo = as_date(c(
#         '2018-07-16', '2018-07-17', '2018-07-18', '2018-07-19', '2018-07-20',
#         '2018-11-19', '2018-11-20', '2018-11-21', '2018-11-23',
#         '2019-03-18', '2019-03-19', '2019-03-20', '2019-03-21', '2019-03-22',
#         '2019-07-16', '2019-07-17', '2019-07-18', '2019-07-19', '2019-07-22',
#         '2019-11-25', '2019-11-26', '2019-11-27', '2019-11-29',
#         '2020-04-13', '2020-04-14', '2020-04-15', '2020-04-16', '2020-04-17',
#         '2020-07-20', '2020-07-21', '2020-07-22', '2020-07-23', '2020-07-24',
#         '2020-11-23', '2020-11-24', '2020-11-25', '2020-11-27'
#       )), 
#       maundy_thursday = as_date(c('2018-03-29', '2019-04-18', '2020-04-10')),
#       good_friday = as_date(c('2018-03-30', '2019-04-19', '2020-04-09')),
#       corona = seq(as_date('2020-03-09'), as_date('2020-03-31'), 'day')
#     )
#   )

make_model <- function(ts, transform = NULL, back_transform = NULL, changepoints = NULL, n_changepoints = 25, changepoint_range = 0.8, prior_scale = c(seasonality = 10, holidays = 10, changepoint = 0.05), seasonality = NULL, seasonality_mode = 'additive', fixed_hols = NULL, var_hols = NULL, month_days = NULL, last = FALSE, other = list()){
  xreg_hol <- function(str, dates){
    month <- month(dates, label = TRUE) == substr(str, 1, 3)
    day <- day(dates) == substr(str, 4, 5)
    tibble(holiday = str, ds = dates[month & day])
  }
  xreg_var <- function(str, dates){
    month <- month(dates, label = TRUE) == substr(str, 1, 3)
    week <- ceiling(day(dates)/7) == substr(str, 4, 4)
    wday <- wday(dates, label = TRUE) == substr(str, 5, 7)
    tibble(holiday = str, ds = dates[month & week & wday])
  }
  xreg_mday <- function(str, dates){
    day <- day(dates) == substr(str, 4, 5)
    wday <- substr(str, 1, 3)
    if(wday == 'All'){
      tibble(holiday = str, ds = dates[day])
    }
    else if(wday == 'MtF'){
      wday <- wday(dates) %in% 2:6 
      tibble(holiday = str, ds = dates[day & wday])
    } else{
      wday <- wday(dates, label = TRUE) == wday
      tibble(holiday = str, ds = dates[day & wday])
    }
  }
  xreg_last <- function(dates){
    dates %>%
      enframe(NULL, 'ds') %>% 
      filter((day(ds + 1) == 1 & wday(ds) != 1) | (day(ds + 2) == 1 & wday(ds) == 7)) %>%
      transmute(holiday = 'last', ds)
  }
  
  if(is.null(transform)){
    transform <- function(x) x
  }
  
  ts <- ts %>% 
    mutate(y = transform(y))
  
  xreg <- list(
    fixed_hols = map_df(fixed_hols, xreg_hol, ts$ds),
    var_hols = map_df(var_hols, xreg_var, ts$ds),
    month_days = map_df(month_days, xreg_mday, ts$ds),
    other = map_df(other, enframe, NULL, 'ds', .id = 'holiday')
  )
  
  if(last){
    xreg$last <- xreg_last(ts$ds)
  }
  
  xreg <- bind_rows(xreg)
  if(nrow(xreg) == 0){
    xreg <- NULL
  }
  
  growth <- case_when(
    'cap' %in% names(ts) ~ 'logistic',
    TRUE ~ 'linear'
  )
  
  
  
  m <- prophet(
    growth = growth,
    changepoints = changepoints,
    n.changepoints = n_changepoints,
    changepoint.range = changepoint_range,
    yearly.seasonality = 'yearly' %in% seasonality,
    weekly.seasonality = 'weekly' %in% seasonality,
    daily.seasonality = FALSE,
    holidays = xreg,
    seasonality.mode = seasonality_mode,
    seasonality.prior.scale = prior_scale['seasonality'],
    holidays.prior.scale = prior_scale['holidays'],
    changepoint.prior.scale = prior_scale['changepoint']
  )
  
  if('monthly' %in% seasonality){
    m <- add_seasonality(m, 'monthly', 365.25/12, 5)
  }
  
  attr(m, 'ts') <- ts
  attr(m, 'back_transform') <- back_transform
  
  fit.prophet(m, ts)
}

# Returns forecast tibble from model made with make_model()
# ex. make_fc(model)
make_fc <- function(model){
  ts <- attr(model, 'ts')
  
  if(is.null(attr(model, 'back_transform'))){
    attr(model, 'back_transform') <- function(x) x
  }
  
  future <- ts %>% 
    select(ds, contains('cap'), contains('floor'))
  
  
  fc <- predict(model, future) %>% 
    select(ds, yhat_lower, yhat, yhat_upper) %>% 
    mutate(ds = as_date(ds))
  
  tibble(ds = seq(first(ts$ds), last(ts$ds), 'day')) %>% 
    full_join(ts, 'ds') %>% 
    full_join(fc, 'ds') %>% 
    mutate_if(is.numeric, attr(model, 'back_transform')) %>% 
    mutate_if(is.numeric, replace_na, 0) %>% 
    mutate_if(is.numeric, function(x) ifelse(x < 0, 0, x)) %>% 
    mutate(y = ifelse(ds >= today(), NA, y))
}



# Makes an ARIMA forecast with 80% prediction intervals
# Arguments:
# ts: tibble with ds (date) and y (actuals). It should only contain the dates in
# the past you want to use (I take out weekends and holidays when closed) along 
# with the dates in the future you want to predict (I take out weekends and 
# holidays when closed).
# transform: (function) function for transforming time series
# back_transform: (function) function for back transforming time series (should be inverse of transform)
# Ex.
# tibble(ds = seq(as_date('2018-06-22'), as_date('2020-12-31'), 'day')) %>% 
#   filter(wday(ds) %in% 2:7,!(ds %in% as_date(c(
#     '2018-07-04', '2018-11-22', '2018-12-25', '2019-01-01',
#     '2019-07-04', '2019-11-28', '2019-12-25', '2020-01-01',
#     '2020-07-04', '2020-11-26', '2020-12-25'
#   )))) %>% 
#   left_join(read_ts('Mexico', 'AHT'), 'ds') %>% 
#   make_arima
make_arima <- function(ts, transform = NULL, back_transform = NULL){
  if(is.null(transform)){
    transform <- function(x) x
  }
  if(is.null(back_transform)){
    back_transform <- function(x) x
  }
  
  ts <- ts %>% 
    mutate(y = transform(y)) %>% 
    as_tsibble(index = ds)
  
  future <- ts %>% 
    filter(ds >= today()) %>% 
    select(ds)
  
  fc <- ts %>% 
    filter(ds < today()) %>% 
    fill_gaps %>% 
    model(arima = ARIMA(y)) %>% 
    forecast(fill_gaps(future)) %>% 
    hilo(80) %>% 
    unpack_hilo("80%") %>% 
    as_tibble %>%
    transmute(
      Date = ds,
      `Lower 80%` = `80%_lower`,
      Forecast = .mean,
      `Upper 80%` = `80%_upper`
    ) %>% 
    mutate_if(is.numeric, back_transform) %>% 
    mutate_if(is.numeric, round)
  
  future %>% 
    rename(Date = ds) %>% 
    left_join(fc, 'Date') %>% 
    fill_gaps
}

########## Output ##########

# Writes Offered Calls.csv and AHT.csv
# y: (string) either 'Offered Calls' or 'AHT'
# Ex. write_current('Offered Calls')
write_current <- function(y){
  column <- case_when(
    y == 'Offered Calls' ~ 'start_oc',
    y == 'AHT' ~ 'start_aht'
  )
  full_join(
    drop_na(markets, all_of(column))$market %>% 
      set_names %>% 
      map_df(read_ts, y, .id = 'market'),
    drop_na(markets, all_of(column))$market %>% 
      set_names %>% 
      map_df(read_fc, y, .id = 'market'),
    c('market', 'ds')
  ) %>% 
    write_csv(str_c(y, '.csv'))
}

# Writes forecast in Accuracy folder
# Arguments:
# date: last date of data used to make forecasts
# y: (string) either 'Offered Calls' or 'AHT'
# fill_date: when a day was missed what was the last day to use to fill forecast
# Ex.
# (on Wednesday): 
# write_fc(today() - 1, 'Offered Calls')
# (on Monday):
# write_fc(today() - 3, 'Offered Calls', today() - 4)
# write_fc(today() - 2, 'Offered Calls', today() - 4)
# write_fc(today() - 1, 'Offered Calls')
write_fc <- function(date, y, fill_date = NULL){
  df <- str_c(y, '.csv') %>% 
    read_csv(col_types = 'cDdddd') %>% 
    select(-y) %>% 
    filter(between(ds, today(), today() + 89))
  
  folder <- folders[[str_c('Past ', y)]]
  
  if(!is.null(fill_date)){
    fill <- read_csv(str_c(folder, '/', fill_date, '.csv'))

    df <- bind_rows(fill, df, .id = 'cutoff') %>% 
      filter(between(ds, date + 1, date + 90)) %>% 
      group_by(market, ds) %>% 
      filter(cutoff == min(cutoff)) %>% 
      ungroup %>% 
      select(-cutoff)
  }
  write_csv(df, str_c(folder, '/', date, '.csv'))
}

# Writes Past Offered Calls.csv and Past AHT.csv
# y: (string) either 'Offered Calls' or 'AHT'
# Ex. write_past('Offered Calls')
write_past <- function(y){
  folder <- folders[[str_c('Past ', y)]]
  fcs <- dir(folder) %>%
    str_remove('.csv') %>% 
    as_date %>% 
    set_names %>% 
    map_df(function(x) read_csv(str_c(folder, '/', x, '.csv')), .id = 'cutoff') %>% 
    mutate_at('cutoff', as_date)
  data <- read_csv(str_c(y, '.csv'), col_types = 'cDd___') %>% 
    drop_na
  
  
  inner_join(data, fcs, c('market', 'ds')) %>% 
    select(market, cutoff, everything()) %>% 
    arrange(market, cutoff, ds) %>% 
    write_csv(str_c('Past ', y, '.csv'))
}

