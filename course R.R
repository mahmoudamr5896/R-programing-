##################################pakages###########################
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(
  caret,         # Predictive analytics
  GGally,        # Scatterplot matrix
  magrittr,      # Pipes
  pacman,        # Load/unload packages
  parallel,      # Parallel processing
  randomForest,  # Random forests (obviously)
  rattle,        # Plot decision trees
  rio,           # Import/export data
  tictoc,        # Time operations
  tidyverse      # So many reasons
)



##################################loading data############################

df <- read.csv(file.choose())
View(df)
attach(df)

df <- mutate(
  open_t = ifelse(Open >= 4
                  ,"High"
                  ,"Low"),
  open_t = as_afactor(open_t)
)%>%
  print()
df %<>%                         # Overwrite data
  mutate(
    open_t = ifelse(            # open_t ("text")
      Open >= 4,                # Test
      "High",                   # Value if true
      "Low"                     # Value if false
    ),
    open_t = as_factor(open_t)  # Convert to factor
  ) %>%
  print()

###############################exploreing data #################################
df %>%
  pull(OPen)%>%
  boxblot()%>%
  median()


df %>%
  ggplot() + 
  geom_bar(
    aes(
      x    = open_t,  # Variable to chart
      fill = open_t   # Color bars by variable
    )
  ) + 
  theme(legend.position = "none")



tic("Scatterplot matrix")
df %>% 
  select(          # Reorder variables
    open_t,        # Put dichotomous "open" first
    Open,          # Then quantitative "open"
    Extrav:Consc,  # Then other Big 5
    age:engnat     # Then demographics
  ) %>% 
  ggpairs()
toc()  
####################################linear regression###############
fit_lm <- df %>%   # Use full data, save as "fit_lm"
  select(          # Put outcome first
    Open,          # Then quantitative "open"
    Extrav:Consc,  # Then other Big 5
    age:engnat     # Then demographics
  ) %>%
  lm()             # Default linear regression

# Show model summary
fit_lm %>% summary()




fit_lm%>%plot()


####################################SPLIT dATA ###################### 
train <-df %<% samble_freq(.70) # train_data 

test <- anti_join(df,train)    #test_data


























#####################################model K-NN ###############################
fit_knn <- train(
  open_t ~ age + gender + Extrav + Neurot + Agree + Consc,
  data = train,          # Use training data
  method = "knn", 
  trControl = statctrl,
  tuneLength = 20,       # 20 dif values for k
  na.action = "na.omit"
)


# Apply model to training data (takes a moment)
tic("k-NN")
fit_knn
toc() 

# Predict training set
open_p <- fit_knn %>%  # "predicted"
  predict(newdata = train)

# Accuracy of model on training data
table(
  actualclass = train$open_t, 
  predictedclass = open_p
) %>%
  confusionMatrix() %>%
  print()  

# Predict test set
open_p <- predict(  # Create new variable ("predicted")
  fit_knn,          # Apply saved model
  newdata = test    # Use test data
)

# Accuracy of model on test data
table(
  actualclass = test$open_t,  # True outcome
  predictedclass = open_p     # Predicted outcome
) %>%
  confusionMatrix() %>%         # Accuracy statistics
  print() 

# Train decision tree on training data (takes a moment)
tic("Decision tree")
fit_dt <- train(
  open_t ~ age + gender + Extrav + Neurot + Agree + Consc,
  data = train,      # Use training data
  method = "rpart",  # Recursive partitioning
  trControl = trainControl(method = "cv")  # Cross-validate
)
toc()
# Plot final training model
fit_dt$finalModel %>%
  fancyRpartPlot(
    main = "Predicting Open",
    sub = "Training Data"
  )

# Accuracy of model on training data
table(
  actualclass = train$open_t, 
  predictedclass = open_p
) %>%
  confusionMatrix() %>%
  print()  # .5802 accuracy on full training data

# DECISION TREE ON TEST DATA ###############################

# Predict test set
open_p <- fit_dt %>%
  predict(newdata = test)

# Accuracy of model on test data
table(
  actualclass = test$open_t, 
  predictedclass = open_p
) %>%
  confusionMatrix() %>%
  print() 

# RANDOM FOREST ON TRAINING DATA ###########################

# Define parameters for the random forest
control <- trainControl(
  method  = "repeatedcv",  # Repeated cross-validation
  number  = 10,            # Number of folds
  repeats = 3,             # Number of sets of folds
  search  = "random",      # Max number of tuning parameters
  allowParallel = TRUE     # Allow parallel processing
)

# Train random forest on training data (can take a while)
tic("Random forest")
fit_rf <- train(
  open_t ~ age + gender + Extrav + Neurot + Agree + Consc,
  data = train,         # Use training data
  method = "rf",        # Use random forests
  metric = "Accuracy",  # Use accuracy as criterion
  tuneLength = 15,      # Number of levels for parameters
  ntree = 300,          # Number of trees
  trControl = control   # Link to parameters
)
toc()  # 

# Plot accuracy by number of predictors
fit_rf %>% plot()

# Accuracy of model with training data
fit_rf$finalModel

# Predict training data
open_p <- fit_rf %>%        # "predicted"
  predict(newdata = train)  # Use train data

# Accuracy of model on test data
table(
  actualclass = train$open_t, 
  predictedclass = open_p
) %>%
  confusionMatrix() %>%
  print()  # 0.7145 accuracy on full training data

# RANDOM FOREST ON TEST DATA ###############################

# Predict test set
open_p <- fit_rf %>%       # "predicted"
  predict(newdata = test)  # Use test data

# Accuracy of model on test data
table(
  actualclass = test$open_t, 
  predictedclass = open_p
) %>%
  confusionMatrix() %>%
  print()  
















































