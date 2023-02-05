##############################################################
##          CLASSIFICATION                                   
#1] kNN 
# Take random subsample to save time
df %<>% sample_n(4000)

# SPLIT DATA ###############################################

# Split data into train and test sets
train <- df %>% sample_frac(.70)
test <- anti_join(df, train)

# EXPLORE TRAINING DATA ####################################

# Bar chart of "open_t"
train %>%
  ggplot() + 
  geom_bar(
    aes(
      x    = open_t,  # Variable to chart
      fill = open_t   # Color bars by variable
    )
  ) + 
  theme(legend.position = "none")

# COMPUTE KNN MODEL ON TRAINING DATA #######################

# Define parameters
statctrl <- trainControl(
  method = "repeatedcv",  # Repeated cross-validation
  number = 10,            # Number of folds
  repeats = 3             # Number of complete sets of folds
) 

# Define and save model
fit <- train(
  open_t ~ age + gender + Extrav + Neurot + Agree + Consc,
  data = train,          # Use training data
  method = "knn", 
  trControl = statctrl,
  tuneLength = 20,       # 20 dif values for k
  na.action = "na.omit"
)

# Apply model to training data (takes a moment)
fit

# APPLY MODEL TO TEST DATA #################################

# Predict test set
open_p <- predict(  # Create new variable ("predicted")
  fit,              # Apply saved model
  newdata = test    # Use test data
)

# Accuracy of model on test data
table(
  actualclass = test$open_t,  # True outcome
  predictedclass = open_p     # Predicted outcome
) %>%
  confusionMatrix() %>%         # Accuracy statistics
  print()

#2] decision trees

# Set random seed
set.seed(313)

# Split data into train and test sets
train <- df %>% sample_frac(.70)
test <- df %>% anti_join(train)

# EXPLORE TRAINING DATA ####################################

# Bar chart of "gender"
train %>%
  ggplot() + 
  geom_bar(aes(x = gender, fill = gender)) + 
  theme(legend.position = "none")

# Density plots of Big 5 variables 
train %>%
  gather(var, val, Extrav:Open) %>%
  ggplot(aes(val, group = gender, fill = gender)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~var) +
  theme(legend.position='bottom')

# MODEL TRAINING DATA ######################################

# Train decision tree on training data (takes a moment)
dt <- train(
  gender ~ .,        # Use all variables to predict gender
  data = train,      # Use training data
  method = "rpart",  # Recursive partitioning
  trControl = trainControl(method = "cv")  # Cross-validate
)

# Show processing summary
dt

# Description of final training model
dt$finalModel

# Plot final training model
dt$finalModel %>%
  fancyRpartPlot(
    main = "Predicting Gender from Big 5 Factors",
    sub = "Training Data"
  )

# Predict training set
gender_p <- dt %>%  # "predicted"
  predict(newdata = train)

# Accuracy of model on training data
table(
  actualclass = train$gender, 
  predictedclass = gender_p
) %>%
  confusionMatrix() %>%
  print()

# VALIDATE ON TEST DATA ####################################

# Predict test set
gender_p <- dt %>%
  predict(newdata = test)

# Accuracy of model on test data
table(
  actualclass = test$gender, 
  predictedclass = gender_p
) %>%
  confusionMatrix() %>%
  print()

#3] random forests 

# SPLIT DATA ###############################################

# Split data into train and test sets
train <- df %>% sample_frac(.70)
test <- df %>% anti_join(train)

# EXPLORE TRAINING DATA ####################################

# Bar chart of "gender"
train %>%
  ggplot() + 
  geom_bar(aes(x = gender, fill = gender)) + 
  theme(legend.position = "none")

# Density plots of Big 5 variables 
train %>%
  gather(var, val, Extrav:Open) %>%
  ggplot(aes(val, group = gender, fill = gender)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~var) +
  theme(legend.position = 'bottom')

# MODEL TRAINING DATA ######################################

# Define parameters for the train function
control <- trainControl(
  method  = "repeatedcv",  # Repeated cross-validation
  number  = 10,            # Number of folds
  repeats = 3,             # Number of sets of folds
  search  = "random",      # Max number of tuning parameters
  allowParallel = TRUE     # Allow parallel processing
)

# Train decision tree on training data (can take a while)
rf <- train(
  gender ~ . ,          # Predict gender from all other vars
  data = train,         # Use training data
  method = "rf",        # Use random forests
  metric = "Accuracy",  # Use accuracy as criterion
  tuneLength = 15,      # Number of levels for parameters
  ntree = 800,          # Number of trees
  trControl = control   # Link to parameters
)

# Show processing summary
rf

# Plot accuracy by number of predictors
rf %>% plot()

# Accuracy of model with training data
rf$finalModel

# Plot error by number of trees; Red is error for "Male,"
# green is error for "Female," and black is error or "OOB,"
# or "out of bag" (i.e., the probability that any given 
# prediction is not correct within the test data, or the 
# overall accuracy)
rf$finalModel %>% plot()

# APPLY MODEL TO TEST DATA #################################

# Predict test set
gender_p <- rf %>%         # "predicted"
  predict(newdata = test)  # Use test data

# Accuracy of model on test data
table(
  actualclass = test$gender, 
  predictedclass = gender_p
) %>%
  confusionMatrix() %>%
  print()



#4] svm













































































































































