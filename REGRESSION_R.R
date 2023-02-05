##############################################################
##                  linear_mpdel       ######
#1]  linear regression 

# Scatterplot of X  and Y
df %>% 
  ggplot(aes(X, Y)) +
  geom_point(size = 3) +
  geom_smooth(method = lm)
   
# Compute and save bivariate regression
fit1 <- df %>%                      # Save as "fit1"
  select(Y, X) %>%  # y, then x
  lm()   
# Summarize regression model
fit1 %>% summary()

# Confidence intervals for coefficients
fit1 %>% confint()

# Predict values of "volunteering"
fit1 %>% predict()

# Prediction intervals for values of "volunteering"
fit1 %>% predict(interval = "prediction")
# Regression diagnostics
fit1 %>% lm.influence()
fit1 %>% influence.measures()

# Diagnostic plots; run command then hit return in Console
# 1: Residuals vs. Fitted
# 2: Normal Q-Q
# 3: Scale-Location
# 4: Residuals vs. Leverage
fit1 %>% plot()


# MULTIPLE REGRESSION ######################################

# Moving the outcome to the front and removing all unneeded
# variables can make things easier
df %<>%            # Compound assignment pipe; overwrites df
  select( volunteering,  # Outcome variable selected first
          everything()   # Selects all other variables in df
  ) %>%
  print()


Three ways to specify model

# Most concise; uses first variable as outcome
df %>% lm()  # Or just lm(df)

# Identify outcome, infer rest; must specify dataset
lm(volunteering ~ ., data = df)

# Identify entire model
lm(volunteering ~ instagram + facebook + retweet +
     entrepreneur + gdpr + privacy + university + 
     mortgage + museum + scrapbook + modernDance, 
   data = df)

# Save model
fit2 <- df %>% lm()

# Show model
fit2

# Summarize regression model
fit2 %>% summary()

# Confidence intervals for coefficients
fit2 %>% confint()



#2]   lasso regression 
 
Variables need to be on same scale
df %<>%        # Compound assignment pipe
  scale() %>%  # Converts all variables to M = 0 & SD = 1
  as_tibble()  # Converts (again) to tibble

# Get descriptives again
df %>% summary()

# Data as matrix for lars package
y <- df %>% select( quality) %>% as.matrix()
X <- df %>% select(-quality) %>% as.matrix()

STEPWISE REGRESSION ######################################

# Compute model
fit_step <- lars(X, y, type = "stepwise")
# LASSO REGRESSION #########################################

# LASSO: Least Absolute Shrinkage and Selection Operator.
# Can also specify "forward.stagewise", which is like
# stepwise regression but with better generalizability, or
# "lar" for Least Angle Regression (LARS)

# Compute model
fit_lasso <- lars(X, y, type = "lasso")

# View more results
fit_step %>% view()      # View object with results
fit_step$R2              # Show R^2 at each step
fit_step$R2 %>% plot()   # Plot R^2 at each step
fit_step %>% coef()      # Get coefficients at each step
fit_step  
# LASSO REGRESSION #########################################

# LASSO: Least Absolute Shrinkage and Selection Operator.
# Can also specify "forward.stagewise", which is like
# stepwise regression but with better generalizability, or
# "lar" for Least Angle Regression (LARS)

# Compute model
fit_lasso <- lars(X, y, type = "lasso")

Plot results
fit_lasso %>% plot()     # Plot coefficients
legend(                  # Add legend
  "bottomleft",          # Position of legend
  lwd = 2,               # Line size
  col = (1:nrow(X)),     # Colors
  legend = colnames(X),  # Variable names
  cex = .7               # Font size
)

# View more results
fit_lasso$R2             # Show R^2 at each step
fit_lasso$R2 %>% plot()  # Plot R^2 at each step
fit_lasso %>% coef()     # Get coefficients at each step
fit_lasso   

#3] quantile regression 
# QUANTILE REGRESSION ######################################

# Compute model, display coefficients
fit <- df %>%
  rq(
    modernDance ~ scrapbook,  # y ~ x
    data = .,                 # Data source
    tau = 0.5                 # Quantile to use (median)
  ) %>%
  print()

# Confidence intervals for coefficients
fit %>% summary()

# t-tests for coefficients
fit %>% summary(se = "boot")

#4]  logstic regression 
# Logistic regression needs a 0/1 outcome variable, so
# we'll create a variable "female" where "Female" gets a 1
# (for "True") and "Male" gets a 0 (for "False")

df %<>%
  mutate(
    female = ifelse(       # Create new variable "female"
      gender == "Female",  # Test
      1,                   # Value if true
      0                    # Value if false
    )
  ) %>%
  select(gender, female, Extrav:Open) %>%
  print()

# Bar chart of "female"
df %>%
  ggplot() + 
  geom_bar(aes(x = female)) + 
  theme(legend.position = "none")

# EXPLORE DATA #############################################

# Explore data (wide output)
df %>% skim()

# BINOMIAL LOGISTIC REGRESSION #############################

# Compute model
fit <- glm(
  female ~ Extrav + Neurot + Agree + Consc + Open,
  data = df, 
  family = "binomial"
)

# Summarize regression model
fit %>% summary()  # Standard output
fit %>% tidy()     # Tidy output

# Confidence intervals for coefficients
fit %>% confint() 

# PREDICTED VALUES #########################################

# Predicted propobability are in "fitted values"
fit %>% view()

# See the first few predicted values
predict(fit, type = 'response') %>% head()

# Add predicted values to df
df %<>%
  mutate(
    predicted = predict(fit, type = 'response'),
    pred_gender = ifelse(  # Create variable "pred_gender"
      predicted > 0.5,     # Test if predicted values > 0.5
      "P_Female",          # If true, predict female
      "P_Male"             # If false, predict male
    )
  ) %>%
  select(
    gender,
    female,
    predicted,
    pred_gender,
    everything()
  ) %>%
  print()

# VISUALIZE PROBABILITIES ##################################

# Boxplots of probabilities by actual values
df %>%
  ggplot(aes(x = gender, 
             y = predicted, 
             fill = gender)) + 
  geom_boxplot() +
  coord_flip() +
  xlab("") +
  theme(legend.position = "none")

# Confusion matrix
df %$%                        # Exposition pipe
  table(gender, pred_gender)  # table(rows, columns)

# Side-by-side bar chart for confusion matrix
df %>%
  ggplot(aes(gender, fill = pred_gender)) + 
  geom_bar(position = position_dodge()) +
  theme(legend.position = "bottom")

# Row percentages for confusion matrix
df %$%
  table(gender, pred_gender) %>%
  prop.table(1) %>%  # 1 is for row percentages
  round(2) %>%       # Round to two decimal places
  `*`(100)           # Multiply by 100 to read as percent

# Plot logistic curve
df %>%
  ggplot(
    aes(
      x = predicted, 
      y = female
    )
  ) + 
  geom_point(alpha = .01) +
  stat_smooth(
    method = "glm", 
    method.args = list(family = binomial)
  ) + 
  xlab("Probability Female") +
  ylab("Respondent Is Female") 

#5]   poisson regression
# POISSON REGRESSION #######################################

# Poisson regression, also known as log-linear regression,
# models count/frequency data and contingency table. It is
# named after French mathematician Siméon Denis Poisson.

# Compute model
df %>%
  glm(                   # Generalized Linear Model
    count ~ spray,       # Count as a function of spray
    family = 'poisson',  # Use Poisson regression
    data = .             # Use df from pipe
  ) %>%
  summary()              # Summary ta




















































































































