### analysis.r
### demonstration analysis of privacy loss on real data

source("private.r")       # load library of DP algos
source("apple.r")         # Apple DP Hadamard CMS implementation

library(ggplot2)          # plotting and graphics
library(dplyr)            # data manipulation
library(openssl)          # hashing
library(reshape2)         # for melting
library(scales)           # time scaling of plots

set.seed(100)

OUTPUT_DIR <- "paper/figs/"
PLOT.W <- 10
PLOT.H <- 4

# read in the data
data.yelp <- read.csv("data/yelp_data.csv")
data.past <- read.csv("data/yelp_data_past.csv")
data.bus <- read.csv("data/yelp_bus.csv")


##### EXPLORATORY
TOP.NUM <- 200
TOP.SHARED <- 100
TOP.GRAPH <- 10

# dict of businesses
bus.dict <- setNames(as.list(t(as.vector(data.bus$name))), as.vector(data.bus$business_id))

# table of past data
business.past.hist <- data.frame(table(data.past$business_id)) %>% arrange(desc(Freq))
business.past.hist.top <- business.past.hist[1:TOP.NUM,]
names(business.past.hist.top) <- c("Business", "PastFreq")

# table of current data
business.hist <- data.frame(table(data.yelp$business_id)) %>% arrange(desc(Freq))
business.hist.top <- business.hist[1:TOP.NUM,]
names(business.hist.top) <- c("Business", "CurFreq")

business.merged <- merge(business.hist.top, business.past.hist.top, by="Business")
business.merged <- business.merged %>% arrange(desc(PastFreq))
business.merged <- business.merged[1:TOP.SHARED,]
# add rank for ordering
business.merged$rank <- -rank(business.merged$PastFreq, ties.method = "random")
business.merged.bus <- as.vector(business.merged$Business)
business.merged$Business <- c()
business.merged.melt <- melt(business.merged, id.vars="rank")

# make the plot (summary histogram) [PLOT #1]
plot.histsum <- ggplot(business.merged.melt, aes(x=rank, y=value, fill=variable)) +
  theme_bw() +
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), legend.position = "bottom") +
  scale_x_discrete(expand = c(0, 0, 0, 0)) +
  scale_y_continuous(expand = c(0, 0, 0, 100)) +
  xlab("Business") +
  ylab("Reviews") +
  scale_fill_manual(values=c("dark green", "green"),
                    name="Year",
                    labels=c("2018", "2017")) +
  ggtitle("Top 100 Most Reviewed Yelp Businesses in 2017 and 2018") +
  geom_bar(stat="identity"); plot.histsum
ggsave(paste(OUTPUT_DIR, "histsum.jpg", sep=""), plot.histsum, width=PLOT.W, height=PLOT.H)

# plot the changes in heavy hitters
business.merged.bus <- business.merged.bus[1:TOP.GRAPH]
counts.dict.df <- business.past.hist.top[business.past.hist.top$Business %in% business.merged.bus,]
rownames(counts.dict.df) <- as.vector(counts.dict.df$Business)
counts.dict.df$Business <- c()
counts.dict <- setNames(as.list(t(counts.dict.df)), rownames(counts.dict.df))
# only keep relevant rows
data.yelp.topbus <- data.yelp[data.yelp$business_id %in% business.merged.bus,]
# loop through dates
data.yelp.topbus <- data.yelp.topbus %>% arrange(date)
data.yelp.topbus$review_id <- c()
data.yelp.topbus$user_id <- c()
data.yelp.topbus$count <- rep(0, nrow(data.yelp.topbus))
for (i in 1:nrow(data.yelp.topbus)) {
  business.cur <- as.character(data.yelp.topbus[i, "business_id"])
  data.yelp.topbus[i,"count"] <-
    as.numeric(counts.dict[[business.cur]])
  data.yelp.topbus[i, "Business"] <- bus.dict[[business.cur]]
  counts.dict[[business.cur]] <- data.yelp.topbus[i,"count"] + 1
  if (mod(i, 1000) == 0) {
    print(i)
  }
}
data.yelp.topbus$business_id <- c()
data.yelp.topbus$date <- as.POSIXct(as.vector(data.yelp.topbus$date))
# traceplot
plot.paths <- ggplot(data.yelp.topbus, aes(x=date, y=count, group=Business, color=Business)) +
  geom_line() +
  scale_x_datetime(expand = c(0.01, 0, 0.01, 0), breaks=date_breaks("2 month")) +
  scale_y_continuous(expand = c(0.05, 0, 0.05, 0)) +
  xlab("Date") +
  ylab("Reviews") +
  theme_bw() +
  theme(legend.position="bottom") +
  ggtitle("Reviews of Top 10 Businesses over 2017"); plot.paths
ggsave(paste(OUTPUT_DIR, "paths.jpg", sep=""), plot.paths, width=PLOT.W, height=PLOT.H)

##### Hadamard Count Mean Sketch algorithm
MAX_K <- 65536
k <- 65536
m <- 2^8 # hash functions of size 256 bits

# generate keys
keys <- as.character(sample(1:MAX_K, k, replace=FALSE))

# helper function to generate a hash function
generate.hash <- function (key) {
  function (d) {
    sha256(d, key=key)
  }
}

# generate k hashes using above keys
hashes <- apply(t(matrix(keys)), MARGIN=2, FUN=generate.hash)

hist.dp <- hcms.master(as.vector(data.yelp$business_id), 16, k, m, unique(data.yelp$business_id), hashes)


  