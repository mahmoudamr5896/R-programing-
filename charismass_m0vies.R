#####################import data ##############################

df = read.csv(file.choose(),as.table())
df

head(df)
str(df)


count(is.na(df))
##############################
attach(df)
df %>%
  dplyr::mutate(gross= as.Date.numeric(gross),
                rating=as.integer(rating),
                votes=as.integer(votes)
         
         
         )

sum(is.na(df))



replace_na(df,list = "na",values = 0)

df$gross %>% replace_na(0)

































































