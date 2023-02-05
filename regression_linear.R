data = read.csv('insurance.csv')
data
attach(data)

t.test(charges~sex)


t.test(bmi~sex)

anova(bmi~sex)

sum(is.na(data))

prop.test(charges~childr)

chisq.test(charges~bmi)

cor(sex~bmi)

############################################################
boxplot(charges)

barplot(age)


boxplot(age)

hist(charges)

hist(children)

barplot(age)



plot.(data = data)

data %>%
  ggplot(aes(x = sex, 
             fill = sex)) + 
  geom_boxplot() +
  coord_flip() +
  xlab("") +
  theme(legend.position = "none")















































































































