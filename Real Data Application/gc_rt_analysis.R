# load libraries

library(tidyverse)
library(ggplot2)
library(readxl)
library(psych)

## load data prepped in data_prep.R #----
data <- readRDS("Real Data Application/data.rds")
questions <- readRDS("Real Data Application/questions.rds")

# Loading estimates
items <- readxl::read_excel(path = "0 Graveyard/itemparameters.xlsx")
people <- readxl::read_excel(path = "0 Graveyard/personparameters.xlsx")

# one:
# Calculate average RT for all responses of 1, 2, 3... 6
# RT should be small for 1 and 6... ??? 2-5

data.long <- cbind(
  (data %>%
    dplyr::select(
      Subject,
      Gq1, Gq2, Gq3, Gq4, Gq5, Gq6, Gq7, Gq8, Gq9, Gq10,
      Gq11, Gq12, Gq13, Gq14, Gq15, Gq16, Gq17, Gq18, Gq19, Gq20,
      Gq21, Gq22, Gq23, Gq24, Gq25, Gq26, Gq27, Gq28, Gq29, Gq30,
      Gq31, Gq32, Gq33, Gq34, Gq35, Gq36, Gq37, Gq38, Gq39, Gq40,
      Gq41, Gq42, Gq43, Gq44, Gq45, Gq46, Gq47, Gq48, Gq49, Gq50,
      Gq51, Gq52, Gq53, Gq54, Gq55, Gq56, Gq57, Gq58, Gq59, Gq60,
      Gq61, Gq62, Gq63
    ) %>%
    tidyr::gather(Question, Response, Gq1:Gq63)),
  (data %>%
    dplyr::select(
      RTG1, RTG2, RTG3, RTG4, RTG5, RTG6, RTG7, RTG8, RTG9, RTG10,
      RTG11, RTG12, RTG13, RTG14, RTG15, RTG16, RTG17, RTG18, RTG19, RTG20,
      RTG21, RTG22, RTG23, RTG24, RTG25, RTG26, RTG27, RTG28, RTG29, RTG30,
      RTG31, RTG32, RTG33, RTG34, RTG35, RTG36, RTG37, RTG38, RTG39, RTG40,
      RTG41, RTG42, RTG43, RTG44, RTG45, RTG46, RTG47, RTG48, RTG49, RTG50,
      RTG51, RTG52, RTG53, RTG54, RTG55, RTG56, RTG57, RTG58, RTG59, RTG60,
      RTG61, RTG62, RTG63
    ) %>%
    tidyr::gather(QuestionRT, RT, RTG1:RTG63) %>%
    dplyr::select(-QuestionRT)) %>%
    dplyr::mutate(RT_log = log(RT),
                  RT2 = exp(RT_log))
)
test <- psych::describeBy(data.long$RT_log, group=data.long$Response, mat=TRUE) %>% dplyr::select(item, mean,sd)
test
psych::describe(data.long$RT_log)
RT.averages2<-psych::describeBy(data.long$RT, group = data.long$Response, mat = TRUE) %>% dplyr::select(item, mean,sd)
RT.averages2
RT.averages2b<-psych::describeBy(responses$rt, group = responses$rsp, mat = TRUE) %>% dplyr::select(item, mean,sd)
RT.averages2b

#summary(lm(test$item ~ poly(test$mean,2)))

ggplot(data = RT.averages2b, aes(x = item, y = mean, group = 1)) +
  geom_line() +
  geom_point()

ggplot(data = RT.averages2, aes(x = item, y = mean, group = 1)) +
  geom_line() +
  geom_point()

ggplot(data = RT.averages, aes(x = item, y = mean, group = 1)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1) +
  geom_line() +
  geom_point()

m1 <- aov(RT ~ as.factor(Response), data = data.long)
summary(m1)
TukeyHSD(m1)

# two:
# plot where x axis is theta - delta
# person item pair - 63 item pairs per person
# chunk every 75 or so and plot average theta - delta and average RT

# SAS: gplot - spline to allow shape to bend : i=join/// high-low
people$Q1_dist <- people$Theta - as.numeric(items[1, 2])
people$Q2_dist <- people$Theta - as.numeric(items[2, 2])
people$Q3_dist <- people$Theta - as.numeric(items[3, 2])
people$Q4_dist <- people$Theta - as.numeric(items[4, 2])
people$Q5_dist <- people$Theta - as.numeric(items[5, 2])
people$Q6_dist <- people$Theta - as.numeric(items[6, 2])
people$Q7_dist <- people$Theta - as.numeric(items[7, 2])
people$Q8_dist <- people$Theta - as.numeric(items[8, 2])
people$Q9_dist <- people$Theta - as.numeric(items[9, 2])
people$Q10_dist <- people$Theta - as.numeric(items[10, 2])
people$Q11_dist <- people$Theta - as.numeric(items[11, 2])
people$Q12_dist <- people$Theta - as.numeric(items[12, 2])
people$Q13_dist <- people$Theta - as.numeric(items[13, 2])
people$Q14_dist <- people$Theta - as.numeric(items[14, 2])
people$Q15_dist <- people$Theta - as.numeric(items[15, 2])
people$Q16_dist <- people$Theta - as.numeric(items[16, 2])
people$Q17_dist <- people$Theta - as.numeric(items[17, 2])
people$Q18_dist <- people$Theta - as.numeric(items[18, 2])
people$Q19_dist <- people$Theta - as.numeric(items[19, 2])
people$Q20_dist <- people$Theta - as.numeric(items[20, 2])
people$Q21_dist <- people$Theta - as.numeric(items[21, 2])
people$Q22_dist <- people$Theta - as.numeric(items[22, 2])
people$Q23_dist <- people$Theta - as.numeric(items[23, 2])
people$Q24_dist <- people$Theta - as.numeric(items[24, 2])
people$Q25_dist <- people$Theta - as.numeric(items[25, 2])
people$Q26_dist <- people$Theta - as.numeric(items[26, 2])
people$Q27_dist <- people$Theta - as.numeric(items[27, 2])
people$Q28_dist <- people$Theta - as.numeric(items[28, 2])
people$Q29_dist <- people$Theta - as.numeric(items[29, 2])
people$Q30_dist <- people$Theta - as.numeric(items[30, 2])
people$Q31_dist <- people$Theta - as.numeric(items[31, 2])
people$Q32_dist <- people$Theta - as.numeric(items[32, 2])
people$Q33_dist <- people$Theta - as.numeric(items[33, 2])
people$Q34_dist <- people$Theta - as.numeric(items[34, 2])
people$Q35_dist <- people$Theta - as.numeric(items[35, 2])
people$Q36_dist <- people$Theta - as.numeric(items[36, 2])
people$Q37_dist <- people$Theta - as.numeric(items[37, 2])
people$Q38_dist <- people$Theta - as.numeric(items[38, 2])
people$Q39_dist <- people$Theta - as.numeric(items[39, 2])
people$Q40_dist <- people$Theta - as.numeric(items[40, 2])
people$Q41_dist <- people$Theta - as.numeric(items[41, 2])
people$Q42_dist <- people$Theta - as.numeric(items[42, 2])
people$Q43_dist <- people$Theta - as.numeric(items[43, 2])
people$Q44_dist <- people$Theta - as.numeric(items[44, 2])
people$Q45_dist <- people$Theta - as.numeric(items[45, 2])
people$Q46_dist <- people$Theta - as.numeric(items[46, 2])
people$Q47_dist <- people$Theta - as.numeric(items[47, 2])
people$Q48_dist <- people$Theta - as.numeric(items[48, 2])
people$Q49_dist <- people$Theta - as.numeric(items[49, 2])
people$Q50_dist <- people$Theta - as.numeric(items[50, 2])
people$Q51_dist <- people$Theta - as.numeric(items[51, 2])
people$Q52_dist <- people$Theta - as.numeric(items[52, 2])
people$Q53_dist <- people$Theta - as.numeric(items[53, 2])
people$Q54_dist <- people$Theta - as.numeric(items[54, 2])
people$Q55_dist <- people$Theta - as.numeric(items[55, 2])
people$Q56_dist <- people$Theta - as.numeric(items[56, 2])
people$Q57_dist <- people$Theta - as.numeric(items[57, 2])
people$Q58_dist <- people$Theta - as.numeric(items[58, 2])
people$Q59_dist <- people$Theta - as.numeric(items[59, 2])
people$Q60_dist <- people$Theta - as.numeric(items[60, 2])
people$Q61_dist <- people$Theta - as.numeric(items[61, 2])
people$Q62_dist <- people$Theta - as.numeric(items[62, 2])
people$Q63_dist <- people$Theta - as.numeric(items[63, 2])

RT.long <- data %>%
  dplyr::select(
    Subject,
    RTG1, RTG2, RTG3, RTG4, RTG5, RTG6, RTG7, RTG8, RTG9, RTG10,
    RTG11, RTG12, RTG13, RTG14, RTG15, RTG16, RTG17, RTG18, RTG19, RTG20,
    RTG21, RTG22, RTG23, RTG24, RTG25, RTG26, RTG27, RTG28, RTG29, RTG30,
    RTG31, RTG32, RTG33, RTG34, RTG35, RTG36, RTG37, RTG38, RTG39, RTG40,
    RTG41, RTG42, RTG43, RTG44, RTG45, RTG46, RTG47, RTG48, RTG49, RTG50,
    RTG51, RTG52, RTG53, RTG54, RTG55, RTG56, RTG57, RTG58, RTG59, RTG60,
    RTG61, RTG62, RTG63
  ) %>%
  tidyr::gather(Question, RT, RTG1:RTG63) %>%
  dplyr::mutate(Question = str_replace(Question, "RTG", "Q"))

people.long <- people %>%
  dplyr::mutate(Cluster = c(
    rep(1, 75), rep(2, 75), rep(3, 75), rep(4, 75), rep(5, 75),
    rep(6, 75), rep(7, 75), rep(8, 75), rep(9, 75), rep(10, 75),
    rep(11, 75), rep(12, 27)
  )) %>%
  tidyr::gather(Question, Dist, Q1_dist:Q63_dist) %>%
  dplyr::mutate(Question = str_remove(Question, "_dist"))


distance.RT <- RT.long %>%
  dplyr::left_join(people.long, by = c("Question", "Subject")) %>%
  dplyr::mutate(RT_log = log(RT))

clusters <- distance.RT %>%
  dplyr::group_by(Question) %>%
  summarize(Dist_mean = mean(Dist), RT_mean = mean(RT), RT_log_mean = mean(RT_log))


ggplot(data = clusters, aes(x = Dist_mean, y = RT_mean)) +
  geom_line() +
  geom_point() +
  geom_smooth(method="loess", se=FALSE) +
  xlab("<U+03B8> - d") +
  ylab("Response Time (in ms)")

describe(clusters$RT_mean)

########## Transformation RT #############
# According to Fox et al. (2007), RT should be log transformed
# Redoing all above but with log transformed RTs

# One: Descriptives for RT

RT.averages.log <- psych::describeBy(data.long$RT, group = data.long$Response, mat = TRUE)

ggplot(data = RT.averages.log, aes(x = item, y = mean, group = 1)) +
  geom_line() +
  geom_point()

ggplot(data = RT.averages.log, aes(x = item, y = mean, group = 1)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1) +
  geom_line() +
  geom_point()

m2 <- aov(RT_log ~ as.factor(Response), data = data.long)
summary(m2)
TukeyHSD(m2)

# Two: Plot theta - delta against log10(RT)

clusters2 <- distance.RT
clusters2$Dist_mean_sqd <- (clusters2$Dist)^2
clusters3 <- clusters2 %>%
  dplyr::filter(RT < 18000)

ggplot(data = clusters3, aes(x = Dist_mean_sqd, y = RT)) +
  geom_line()+
  geom_point()+
  geom_smooth(method = "loess", se = FALSE) +
  xlab("<U+03B8> - d") +
  ylab("Response Time (in ms)")

model <- lm(RT_mean ~ poly(Dist_mean_sqd,3), data=clusters3)
summary(model)

prd <- data.frame(Dist_mean_sqd = seq(from = range(clusters3$Dist_mean_sqd)[1], to = range(clusters3$Dist_mean_sqd)[2], length.out = 100))
err <- predict(model, newdata = prd, se.fit = TRUE)

prd$lci <- err$fit - 1.96 * err$se.fit
prd$fit <- err$fit
prd$uci <- err$fit + 1.96 * err$se.fit

ggplot(prd, aes(x = Dist_mean_sqd, y = fit)) +
  theme_bw() +
  geom_line() +
  geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity") +
  geom_point(data = clusters3, aes(x = Dist_mean_sqd, y = RT_mean))

######### New clustering technique: ####
###### sort from largest to smallest, bin by theta - delta ###

###### Censoring RT data? Is there a threshold for 
###### removing data if someone is taking way too long?
#
## Fit GGUM information function, which is bimodal


boxplot(data.long$RT)
x <- data.long$RT
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- NA
x[x > (qnt[2] + H)] <- NA
tail(x)
(qnt[2] + H)
sum(is.na(x))
