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


































#####################################model K-NN ###############################