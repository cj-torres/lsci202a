rm(list=ls())
library(tidyverse)
library(modelr)

d = read_tsv("http://socsci.uci.edu/~rfutrell/teaching/MWPND_by_picture.txt")

#1.1
#1
d %>% ggplot(aes(x=name_H,y=RT_M)) +
  geom_point() +
  facet_wrap(~lang) +
  ggsave("H_stat.pdf")

#2
RT_model = lm(RT_M ~ name_H, data = d)

#3
d %>% add_predictions(RT_model) %>%
  ggplot(aes(x=name_H)) +
  geom_point(aes(y = RT_M)) +
  geom_line(aes(y = pred,color="red")) +
  facet_wrap(~lang) +
  ggsave("H_stat_regression.pdf")
#German and Finnish are systematically underestimated, Greek and Bulgarian appear systematically overestimated


#4
d %>% add_residuals(RT_model) %>%
  ggplot(aes(x=name_H,y=resid)) +
  geom_point() +
  facet_wrap(~lang) +
  ggsave("H_stat_residuals.pdf")
#Greek and Bulgarian both have their residuals skewed downwards compared to the others, German is skewed upwards, 
#all languages appear to still have a linear pattern

#5
RT_model2 = lm(RT_M ~ name_H + lang, data = d)

d %>% add_predictions(RT_model2) %>%
  ggplot(aes(x=name_H)) +
  geom_point(aes(y=RT_M)) +
  geom_line(aes(y=pred,color="red")) +
  facet_wrap(~lang) +
  ggsave("H_stat_regression_by_lang.pdf")

d %>% add_residuals(RT_model2) %>%
  ggplot(aes(x=name_H,y=resid)) +
  geom_point() +
  facet_wrap(~lang) +
  ggsave("H_stat_residuals_by_lang.pdf")

#6
RT_model3 = lm(RT_M ~ name_H*lang + lang, data = d)

d %>% add_predictions(RT_model3) %>%
  ggplot(aes(x=name_H)) +
  geom_point(aes(y=RT_M)) +
  geom_line(aes(y=pred,color="red")) +
  facet_wrap(~lang) +
  ggsave("H_stat_regression_by_lang_slope.pdf")

d %>% add_residuals(RT_model3) %>%
  ggplot(aes(x=name_H,y=resid)) +
  geom_point() +
  facet_wrap(~lang) +
  ggsave("H_stat_residuals_by_lang_slope.pdf")

#7
d%>%add_residuals(RT_model3) %>%
  add_predictions(RT_model3) %>%
  write_csv("full_predictions.csv")

#1.2

#1
url = "https://tinyurl.com/y5fgh9mk"
d = read_csv(url)

d2 = d %>% mutate(Theme.animate = if_else(Theme.animacy=="A","animate","inanimate")) %>%
  mutate(Recipient.animate = if_else(Recipient.animacy=="A","animate","inanimate")) %>%
  mutate(Theme.definite = if_else(Theme.definiteness == "Definite" | Theme.definiteness == "Definite-pn","D","I")) %>%
  mutate(Recipient.definite = if_else(Recipient.definiteness == "Definite" | Recipient.definiteness == "Definite-pn",
                                      "D","I"))

#2
d2 %>% select(Token.ID,Recipient.animate,Theme.animate,Response.variable) %>% 
  gather(key="Attribute",value="Animacy",-Token.ID,-Response.variable) %>%
  group_by(Attribute,Animacy) %>% 
  ggplot(aes(x = Animacy)) +
  geom_bar(position = "fill", aes(fill = Response.variable)) +
  facet_wrap(~Attribute) +
  ggsave("animacy_bars.pdf")

#3
d2 %>% select(Token.ID,Recipient.definite,Theme.definite,Response.variable) %>% 
  gather(key="Attribute",value="Definiteness",-Token.ID,-Response.variable) %>%
  group_by(Attribute,Definiteness) %>% 
  ggplot(aes(x = Definiteness)) +
  geom_bar(position = "fill", aes(fill = Response.variable)) +
  facet_wrap(~Attribute) +
  ggsave("definiteness_bars.pdf")

#4 Dative is generally preferred in all cases, but recipient animacy encourages more uses of Dative and
# theme animacy encourages more preposition uses. A similar interaction occurs between recipient and theme
# definiteness where a definite recipient encourages more dative use and a definite theme encourages more prepositional
# use.

#5
d3 = d2 %>% mutate(prediction = as.numeric(Response.variable=="D")) #%>%
  #mutate(Recipient.definite = as.numeric(Recipient.definite=="D"))%>%
  #mutate(Theme.definite = as.numeric(Theme.definite=="D"))%>%
  #mutate(Recipient.animate = as.numeric(Recipient.animate=="animate"))%>%
  #mutate(Theme.animate = as.numeric(Theme.animate=="animate"))

LR_model = glm(prediction ~ Theme.definite*Recipient.definite*Theme.animate*Recipient.animate,
               data=d3,family="binomial")


logistic = function(x) {
  1 / (1 + exp(-x))
}

#6
d3 %>%
  add_predictions(LR_model) %>%
  mutate(prediction = if_else(round(logistic(pred))==1,"D","P")) %>%
  select(Token.ID,Recipient.animate,Theme.animate,prediction) %>%
  gather(key="Attribute",value="Animacy",-Token.ID,-prediction) %>%
  group_by(Attribute,Animacy) %>% 
  ggplot(aes(x = Animacy)) +
  geom_bar(position = "fill", aes(fill = prediction)) +
  facet_wrap(~Attribute)

d3 %>%
  add_predictions(LR_model) %>%
  mutate(prediction = if_else(round(logistic(pred))==1,"D","P")) %>%
  select(Token.ID,Recipient.definite,Theme.definite,prediction) %>%
  gather(key="Attribute",value="Definiteness",-Token.ID,-prediction) %>%
  group_by(Attribute,Definiteness) %>% 
  ggplot(aes(x = Definiteness)) +
  geom_bar(position = "fill", aes(fill = prediction)) +
  facet_wrap(~Attribute)

#7 The models get the general trends observed above correct, 
# but the differences in the amount of alternation is generally higher. 
# There may be something missing from the regression that could make the fit better.