library(tidyverse)
library(lubridate)
unzip("activity.zip")
activity <- read_csv("activity.csv")
activity$interval <- paste0("000",as.character(interval))
activity$interval <- with(activity, substr(interval, nchar(interval) - 3,
                                           nchar(interval)))

activity$date_time <- ymd_hm(paste(activity$date, activity$interval))

daily_steps <- activity %>% 
    group_by(date) %>% summarize("Total"=sum(steps, na.rm = TRUE))

summary(daily_steps$Total)[3]

hist(daily_steps$Total, breaks = 15,
     main = "Histogram of Daily Total Steps",
     xlab = "Daily Total Steps")

daily_pattern <- activity %>%
    group_by(interval) %>% summarize("Average"=mean(steps, na.rm = TRUE))

daily_pattern$interval <- as_datetime(daily_pattern$interval, tz = "UTC", format = "%H%M")
ungroup(daily_pattern)
ggplot(daily_pattern) +
    geom_line(mapping = aes(as.POSIXct.POSIXlt(interval), Average, group = 1)) +
    scale_x_datetime(date_label = "%H:%M") +
    labs(title = "Average daily steps",
         subtitle = "by 5 min increments",
         x = "Time",
         y = "Steps") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

daily_max <- filter(daily_pattern, Average == max(Average))

interval_lookup <- function(int) {
    lookup <- daily_pattern %>% filter(interval == int)
    lookup[[2]]
}

activ2 <- mutate(activity, steps_repl = steps)
activ2 <- mutate(activity, interval_repl = interval_lookup(interval))
