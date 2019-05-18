### thresh-analysis.r
### performance of thresh algorithm

source("thresh.r")

library(dplyr)        # sorting
library(ggplot2)      # plotting
library(lubridate)    # date

OUTPUT_DIR <- "paper/figs/"
PLOT.W <- 10
PLOT.H <- 4

# read in the data
data.yelp <- read.csv("data/yelp_data.csv")
data.yelp.past <- read.csv("data/yelp_data_past.csv")

SUB.SIZE <- 100000
data.yelp.sub <- data.yelp[sample(1:nrow(data.yelp), SUB.SIZE, replace=FALSE),]
data.yelp.past.sub <- data.yelp.past[sample(1:nrow(data.yelp.past), SUB.SIZE, replace=FALSE),]

INTERVAL.DAYS <- 10
INTERVAL.N <- floor(365 / INTERVAL.DAYS)
INTERVAL.SECS <- INTERVAL.DAYS * 24 * 60 * 60
# extract users
users <- unique(data.yelp.sub$user_id)
data.yelp.sub <- data.yelp.sub %>% arrange(date)
data.yelp.past.sub <- data.yelp.past.sub %>% arrange(date)
n <- length(users)

# calculate percentage of users submitting reviews over time
calc.percs <- function(data, start) {
  last.date <- as.POSIXct(start)
  period.users <- c()
  periods.df <- data.frame(date=character(), perc=numeric())
  # loop through all entries
  for (i in 1:nrow(data)) {
    if (i %% 10000 == 0) {
      print(i)
    }
    cur.day <- as.POSIXct(data$date[i])
    # continue in current period
    if (cur.day < last.date + INTERVAL.SECS) {
      # only include if new user
      cur.user <- as.character(data$user_id[i])
      if (!(cur.user %in% period.users)) {
        period.users <- c(period.users, cur.user)
      }
    } else {
      # move onto next period
      period.df <- data.frame(date=last.date, perc=(length(period.users) / n))
      periods.df <- rbind(periods.df, period.df)
      period.users <- c()
      last.date <- last.date + INTERVAL.SECS
    }
  }
  periods.df
  #period.df <- data.frame(date=last.date, perc=(length(period.users) / n))
  #periods.df <- rbind(periods.df, period.df)
}

# randomized response for local DP
randomized.response <- function (x, epsilon, values) {
  p <- exp(epsilon) / (exp(epsilon) + 1)
  if(runif(1) < p) {
    x
  } else {
    values[values != x]
  }
}

# calculate percentage of users submitting reviews over time using randomized response
calc.percs.random <- function(data, start, epsilon) {
  last.date <- as.POSIXct(start)
  period.users <- c()
  periods.df <- data.frame(date=character(), perc=numeric())
  # loop through all entries
  for (i in 1:nrow(data)) {
    if (i %% 10000 == 0) {
      print(i)
    }
    cur.day <- as.POSIXct(data$date[i])
    # continue in current period
    if (cur.day < last.date + INTERVAL.SECS) {
      # only include if new user
      cur.user <- as.character(data$user_id[i])
      if (!(cur.user %in% period.users)) {
        period.users <- c(period.users, cur.user)
      }
    } else {
      # move onto next period
      s <- sum(apply(matrix(rep(1, length(period.users))), MARGIN=1, FUN=randomized.response, epsilon / INTERVAL.N, c(0, 1)))
      s <- s + sum(apply(matrix(rep(0, length(period.users))), MARGIN=1, FUN=randomized.response, epsilon / INTERVAL.N, c(0, 1)))
      period.df <- data.frame(date=last.date, perc=(s / n))
      periods.df <- rbind(periods.df, period.df)
      period.users <- c()
      last.date <- last.date + INTERVAL.SECS
    }
  }
  periods.df
  #period.df <- data.frame(date=last.date, perc=(length(period.users) / n))
  #periods.df <- rbind(periods.df, period.df)
}

# get results
percs.2017 <- calc.percs(data.yelp.sub, "2017-01-01")
percs.2016 <- calc.percs(data.yelp.past.sub, "2016-01-01")
percs.2017.dp <- calc.percs.random(data.yelp.sub, "2017-01-01", 1)

# make the plots
plot.percs.2017 <- ggplot(percs.2017, aes(x=date, y=perc)) +
  geom_line() +
  theme_bw(); plot.percs.2017
plot.percs.2016 <- ggplot(percs.2016, aes(x=date, y=perc)) +
  geom_line() +
  theme_bw(); plot.percs.2016


percs.df <- rbind(percs.2017, percs.2016)
# make the plot
plot.percs <- ggplot(percs.df, aes(x=yday(date),
                                   y=perc,
                                   group=factor(year(date)),
                                   color=factor(year(date)))) +
  geom_line() +
  geom_line(data=percs.2017.dp, aes(x=yday(date), y=perc), linetype="dashed") +
  xlab("Day of Year") +
  scale_color_manual(values=c("green", "dark green"),
                    name="Year",
                    labels=c("2016", "2017")) +
  theme_bw() +
  theme(legend.position = "bottom"); plot.percs
ggsave(paste(OUTPUT_DIR, 'percs.jpg', sep=""), plot.percs, width=PLOT.W, height=PLOT.H)


