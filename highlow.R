highlow <- function(x, period = "week", n = 60) {
  
  # 将日期取出换算成年月日
  df <- as_tibble(x) %>%
    mutate(date = as_date(index(x)), 
           year =  year(date), 
           month = month(date), 
           week = month(date), 
           day = yday(date)) # 一年中第几天
  
  get_col <- function(df, col = "Open"){
    names(df)[str_detect(names(df), col)]
  }
  
  # 重命名各列判断 adjusted 与 close 的关系
  df <- df %>%
    rename(
      # open = get_col(., "Open"),
      # high = get_col(., "High"),
      # low = get_col(., "Low"),
      # close = get_col(., "Close"),
      # volume = get_col(., "Volume"),
      adjusted = get_col(., "Adjusted")) %>%
    select(adjusted:day)
  
  # 按年月计算最后一个交易日的 adjusted
  period_df <- group_by(df, year, !!as.symbol(period)) %>% # 之前的方法 enquo() + !! 的方法可能不适用了
    summarise(adjusted = last(adjusted), 
              date = last(date)) 
  
  # 高点 lead(adjusted) > adjusted > lag(adjusted),低点反之
  period_df <- ungroup(period_df) %>%
    mutate(if_high = if_else(lead(adjusted) <= adjusted & adjusted >= lag(adjusted), TRUE, FALSE), 
           if_low = if_else(lead(adjusted) >= adjusted & adjusted <= lag(adjusted), TRUE, FALSE))
  
  # # 给高点排序
  # period_high <- filter(period_df, if_high) %>%
  #   arrange(desc(adjusted)) %>%
  #   mutate(rank = row_number())
  # 
  # # 给底点排序
  # period_low <- filter(period_df, if_low) %>%
  #   arrange(adjusted) %>%
  #   mutate(rank = row_number())
  # 
  # # 求高低点前 days 天的日期    # date_last_period = `year<-`(date, year(date) - 1), # 求1年前的日期
  # period_high <- mutate(period_high, date_last_period = date - days(n))
  # 
  # # 时段内的高点意味着本期高点比时段内其他高点高
  # period_high$high_last_period <- seq_len(nrow(period_high)) %>%
  #   map_dbl(~ max(filter(period_high, date <= period_high$date[.x], date > period_high$date_last_period[.x])$adjusted))
  # period_high <- filter(period_high, if_else(adjusted >= high_last_period, TRUE, FALSE)) %>% 
  #   arrange(desc(date))
  # 
  # # 同理求时段内低点
  # period_low <- mutate(period_low, date_last_period = date - days(n))
  # period_low$low_last_period <- seq_len(nrow(period_low)) %>%
  #   map_dbl(~ min(filter(period_low, date <= period_low$date[.x], date > period_low$date_last_period[.x])$adjusted))
  # period_low <- filter(period_low, if_else(adjusted <= low_last_period, TRUE, FALSE)) %>%
  #   arrange(desc(date))
  
  df <- mutate(df, date_last_period = date - days(n))
  df$high_last_period <- seq_len(nrow(df)) %>%
    map_dbl(~ max(filter(period_df, date <= df$date[.x], date > df$date_last_period[.x], if_high)$adjusted))
  
  df$low_last_period <- seq_len(nrow(df)) %>%
    map_dbl(~ min(filter(period_df, date <= df$date[.x], date > df$date_last_period[.x], if_low)$adjusted))
  
  select(df, adjusted, high_last_period, low_last_period) %>%
    mutate(high_last_period = if_else(is.infinite(high_last_period), adjusted, high_last_period), 
           low_last_period = if_else(is.infinite(low_last_period), adjusted, low_last_period), ) %>%
    zoo(., df$date) %>% # 不用.是因为需要 date 列保留
    xts()
}