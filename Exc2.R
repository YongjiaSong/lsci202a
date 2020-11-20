# q1
rm(list=ls()) # Clear the environment
library(tidyverse)
library(modelr)

d = read_tsv("http://socsci.uci.edu/~rfutrell/teaching/MWPND_by_picture.txt")
#q1.1.1
q1.plot = d %>%
  ggplot(aes(x = name_H, y = RT_M)) +
  geom_point() +
  facet_wrap(.~lang)

ggsave("H-stat.pdf",q1.plot)

#q1.1.2
RT_model = lm(RT_M~name_H, data = d)

#q1.1.3
q3.plot = d %>%
  add_predictions(RT_model) %>%
  ggplot(aes(x = name_H, y = RT_M)) +
  geom_point(alpha = 1/5) +
  facet_wrap(.~lang)+
  geom_line(aes(y = pred), color = "red")

ggsave("H_stat_regression.pdf",q3.plot)

#q1.1.4
q4.plot = d %>%
  add_residuals(RT_model) %>%
  ggplot(aes(x = name_H, y = resid)) +
  geom_point() +
  facet_wrap(.~lang) +
  labs(x = "H-statistic (bits)", y = "Residual RT (ms)")

ggsave("H_stat_residuals.pdf",q4.plot)

#q1.1.5
RT_model2 = lm(RT_M~lang+name_H, data = d)

q5_pred.plot = d %>%
  add_predictions(RT_model2) %>%
  ggplot(aes(x = name_H, y = RT_M)) +
  geom_point(alpha = 1/5) +
  facet_wrap(.~lang)+
  geom_line(aes(y = pred), color = "red") +
  ggsave("H_stat_regression_by_lang.pdf")

d %>%
  add_residuals(RT_model2) %>%
  ggplot(aes(x = name_H, y = resid)) +
  geom_point() +
  facet_wrap(.~lang) +
  labs(x = "H-statistic (bits)", y = "Residual RT (ms)")+
  ggsave("H_stat_residuals_by_lang.pdf")

#q1.1.6
RT_model3 = lm(RT_M~lang*name_H, data = d)

d %>%
  add_predictions(RT_model3) %>%
  ggplot(aes(x = name_H, y = RT_M)) +
  geom_point(alpha = 1/5) +
  facet_wrap(.~lang)+
  geom_line(aes(y = pred), color = "red") +
  ggsave("H_stat_regression_by_lang_slope.pdf")

d %>%
  add_residuals(RT_model3) %>%
  ggplot(aes(x = name_H, y = resid)) +
  geom_point() +
  facet_wrap(.~lang) +
  labs(x = "H-statistic (bits)", y = "Residual RT (ms)")+
  ggsave("H_stat_residuals_by_lang_slope.pdf")

#q1.1.7
full_predictions = d %>%
  add_predictions(RT_model3) %>%
  add_residuals(RT_model3)
  
write_csv(full_predictions,"full_predictions.csv")
#write.csv

#1.2
url = "https://tinyurl.com/y5fgh9mk"
d = read_csv(url)

#q1.2.1
d2 = d %>%
  mutate(Theme.animacy= if_else(Theme.animacy == "A", "animate", "inanimate"),
         Recipient.animacy= if_else(Recipient.animacy == "A", "animate", "inanimate"),
         Theme.definiteness = if_else(Theme.definiteness %in% c("Definite", "Definite-pn"), "D","I"),
         Recipient.definiteness =  if_else(Recipient.definiteness %in% c("Definite","Definite-pn"),"D", "I"))

#q1.2.2
d2 %>%
  gather(key = "Type_animacy", value = "animacy",Theme.animacy, Recipient.animacy) %>%
  ggplot(aes(x = animacy, fill = Response.variable)) +
  geom_bar(position = "fill") +
  facet_wrap(~Type_animacy)+
  ggsave("animacy_bars.pdf")

#q1.2.3
d2 %>%
  gather(key = "Type_definiteness", value = "definiteness", Recipient.definiteness, Theme.definiteness) %>%
  ggplot(aes(x = definiteness, fill = Response.variable)) +
  geom_bar(position = "fill") +
  facet_wrap(~Type_definiteness) +
  ggsave("definiteness_bars.pdf")

#q1.2.4
#The effect of animacy on dative alternation is similar to the effect of definiteness on it.
#The proportion of double-object dative with inanimate recipient is smaller than the proportion of it with animate recipient.
#The proportion of double-object dative with inanimate theme is larger than the proportion of it with animate theme.
#The results of definiteness have the similar pattern. The proportion with indefinite recipient is smaller and the proportion with indefinite theme is larger.

#q1.2.5
d3 = d2 %>%
  mutate(RV = if_else(Response.variable == "D", 1, 0))

m1 = glm(RV~Recipient.animacy + Recipient.definiteness + Theme.animacy + Theme.definiteness, data = d3, family = "binomial")

#q1.2.6
logistic = function(x){
  1/(1+exp(-x))
}

#logistic prediction of animacy
d3 %>%
  add_predictions(m1) %>%
  gather(key = "Type_animacy", value = "animacy",Theme.animacy, Recipient.animacy) %>%
  ggplot(aes(x= animacy, y = logistic(pred), fill = Response.variable)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~Type_animacy)
  
#logistic prediction of definiteness
d3 %>%
  add_predictions(m1) %>%
  gather(key = "Type_definiteness", value = "definiteness",Theme.definiteness, Recipient.definiteness) %>%
  ggplot(aes(x= definiteness, y = logistic(pred), fill = Response.variable)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~Type_definiteness)

#q1.2.7
#The prediction fits the pattern of the empirical data, but the predicted porportions of double-object dative for both animacy and definiteness
#are smaller than the empirical proportions. 





