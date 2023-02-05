install.packages("dplyr")
library("dplyr")




# select colmuns
data %>% select(firstname,Sex)
data %>% select(firstname:Weight)
data %>% select(-Sex)
data[-c(3)]

#filter rows
data %>% filter(Age>60)
data %>% filter(Sex =='m')
data[data$Sex=='m',]



#mutate 
data %>% mutate(Height=Weight+100)
data %>% mutate(Height=Weight+100,BMI=Weight/(Height^2)*10000)
data$Height = DAta$
#summarise
  data %>% summarise(mean=mean(age))

  
#group by 
data = group by(Sex)%>%
  summarise(mean=mean(Age))

# arrange
data%>%arrange(age)

data%>%arrange(desc(age))

#






