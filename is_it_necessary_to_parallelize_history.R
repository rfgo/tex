## this script is to compare performane of production/serial in
## ec2/rabbitmq + celery in ec2 for all intelligence web features

library(dplyr)
library(ggplot2)

features <- c("AH","PH");
threads <- c(1, 2, 4, 8, 16, 32, 48);
techs <- c(3, 4);

file_traits <- expand.grid(techs, features, threads);
names(file_traits) <- c('techs', 'features', 'threads');

# read and merge files

getDf <- function(thread, feature, tech) {
  print(paste(thread, feature, tech));
  path <- as.character(tech);
  tech <- ifelse(tech == 3, 'serial', 'parallelized');
  file <- paste(feature, "_", thread, ".csv", sep="");
  fn <- paste(path, file, sep="/");
  df <- read.csv(fn);
  df <- mutate(df, features=feature, threads=thread, id=1:nrow(df), techs=tech);
  return(select(df, id, features, threads, techs, time));
}

df <- data.frame();
for (n in 1:nrow(file_traits)) {
  trait <- file_traits[n,];
  df <- rbind(
    df, getDf(
      trait$threads,
      as.character(trait$features),
      as.character(trait$techs)
    ));
}
df <- mutate(
  df,
  features=as.factor(features),
  threads=as.factor(threads),
  techs=as.factor(techs)
)

## graphics
fbase = "/Users/robin/Works/logs/parallel/"
thresholds <- list(ACB=2, PCB=2, ATC=1.5, PTC=1.5, UATC=1.5, AH=0.3, PH=0.3)
for (feature in features) {
  p <- ggplot(filter(df, features==feature, time <= thresholds[feature]), aes(x=techs, y = time)) +
    geom_jitter(aes(colour=techs)) +
    facet_grid(. ~ threads) +
    ggtitle(feature);
  fn = paste(fbase, feature, '.png', sep="");
  ggsave(fn, p);
}

p + geom_jitter(aes(colour=techs)) + facet_grid(. ~ features + threads) + ggtitle('AH/PH - Parallele VS Serial') + theme(axis.ticks = element_blank(), axis.text.x = element_blank())

# log <- read.csv("/Users/robin/Works/logs/rabbitmq/tuning/full/1/log.csv")
# p <- ggplot(log, aes(X, avg)) + geom_line() + ggtitle("Avg time of all case as threads up")
# ggsave(paste(fbase, "overall_avg.png", sep=''))
#
# gs <- group_by(df, features, threads)
# ggplot(summarise(gs, avg=mean(time)), aes(threads, avg)) + geom_line(aes(colour=features)) + ggtitle("Avg time of featues")
# ggsave(paste(fbase, "avg_features.png", sep=''))
