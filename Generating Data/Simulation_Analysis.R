# load libraries

library(tidyverse)
library(ggplot2)
library(readxl)
library(psych)

# Loading data
data <- read_csv("02122020 data5.csv") %>%
  dplyr::mutate(dist = theta - b) 

clusters <- data %>%
  dplyr::group_by(j) %>%
  summarize(Dist_mean = mean(dist), RT_mean = mean(RT), RT_log_mean = mean(log(RT)))


ggplot(data = clusters, aes(x = Dist_mean, y = RT_mean)) +
  geom_line() +
  geom_point() +
  geom_smooth(method="loess", se=FALSE) +
  xlab("<U+03B8> - d") +
  ylab("Response Time (in ms)")

theta <- data %>%
  dplyr::select(i, theta) %>%
  distinct()

write_csv(theta,"03032020 thetas.csv")
