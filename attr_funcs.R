attrition_fc <- function(data_filepath, fc_filepath){
  if(str_detect(data_filepath, ".xlsx")){
    df <- readxl::read_excel(data_filepath)
  } else{
    df <- read_csv(data_filepath)
  }
  df <- df %>% 
    mutate(FTE = replace_na(FTE, mean(FTE, na.rm = TRUE))) %>% 
    group_by(ds = yearmonth(`Termination Date`)) %>% 
    summarize(Heads = n(), FTE = sum(FTE, na.rm = TRUE)) %>%
    pivot_longer(-ds, names_to = "series", values_to = "y") %>% 
    filter(ds < yearmonth(today()))
  
  fc <- df %>%
    as_tsibble(key = series, index = ds) %>%
    stretch_tsibble(.step = 1, .init = 80) %>% 
    model(ARIMA(y)) %>%
    forecast(h = "1 year") %>%
    hilo(80) %>%
    unpack_hilo("80%") %>% 
    as_tibble %>%
    transmute(
      series,
      ds = as_date(ds),
      yhat_lower = `80%_lower`,
      yhat = .mean,
      yhat_upper = `80%_upper`,
      cutoff = as_date("2019-08-01") %m+% months(.id - 1)
    )
  
  unique(fc$cutoff) %>% 
    map_df(function(x) mutate(df, cutoff = x)) %>% 
    full_join(fc, c("ds", "series", "cutoff")) %>% 
    arrange(cutoff, series, ds) %>% 
    write_csv(fc_filepath)
}
update_josh <- function(file_in, file_out){
  read_csv(file_in) %>% 
    filter(ds == cutoff %m+% months(1) | (cutoff == max(cutoff) & ds > today())) %>% 
    select(`Termination Date` = ds, series, yhat) %>% 
    pivot_wider(names_from = series, values_from = yhat) %>% 
    write_csv(file_out)
  
}
update_hourly <- function(){
  hour_worked <- function(start, end, hour){
    if(is.na(start) | is.na(end)){
      NA
    } else if(start == end){
      NA
    } else if(start > end){
      as.numeric(start <= hour | end > hour)
    } else {
      as.numeric(start <= hour & end > hour)
    }
  }
  
  to_hour <- function(hour){
    case_when(
      hour == 0 ~ "12 AM",
      hour < 12 ~ str_c(hour, " AM"),
      hour == 12 ~ "12 PM",
      hour > 12 ~ str_c(hour - 12, " PM")
    )
  }
  
  df <- read_csv("Data.csv")
  
  0:23 %>% 
    map(function(x) transmute(df, hour_worked(hour(Shift), hour(`End Time`), x))) %>% 
    bind_cols %>% 
    `names<-`(to_hour(0:23)) %>% 
    mutate(
      Name = df$Name,
      Market = df$Market,
      Termination_Date = as_date(df$`Termination Date`)
    ) %>% 
    select(Name, Market, Termination_Date, everything()) %>% 
    write_csv("Hourly_Attrition.csv")
}


