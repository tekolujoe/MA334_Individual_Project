if(!is.null(dev.list())) dev.off()  # clear out the past 
rm(list = ls())
cat("\014")

library(DescTools)
library(dplyr) # enables pipes %>% and more
library(tidyr) # for splitting on the period see below
library(moments)
library(reshape2)
par(mfrow=c(1, 1)) 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Proj_data_all_11 <-  read.csv("proportional_species_richness_NAs_removed.csv")

# SELECTING MY ALLOCATED SET OF BD5 TAXINOMIC GROUPS 
eco_selected_names <- c("Bird","Bryophytes","Butterflies","Hoverflies","Isopods")
str(eco_selected_names)

BD5_species <- (Proj_data_all_11[,eco_selected_names]) # ASSIGN A VARIABLE TO THE ALLOCATED BD5 GROUP

# CALCULATE THE BIODIVERSITY MEASURE AS A MEAN OF THE BD5 GROUPS
mean_selected <- rowMeans(Proj_data_all_11[,eco_selected_names])

# CREATE A NEW DATAFRAME WITH SELECTED COLUMMNS AND ADD A NEW COLUMNN WITH THE CALCULATED MEAN
Proj_data <- Proj_data_all_11%>%select("Location",eco_selected_names,
                                       "Easting","Northing","dominantLandClass",      
                                       "ecologicalStatus","period")%>%
  mutate(eco_status_5=mean_selected)

names(Proj_data)  # DISPLAY THE NAMES OF THE COLUMNS IN THE UPDATED DATAFRAME
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# CONVERT 'period' AND 'dominantLandClass' COLUMNS IN Proj_data TO CATEGORICAL VARIABLES
Proj_data$period <- as.factor(Proj_data$period)
Proj_data$dominantLandClass <- as.factor(Proj_data$dominantLandClass)

# you can select in some way, for example....
# Proj_data<- Proj_data%>%filter(grepl("w",dominantLandClass))
# Proj_data<- Proj_data%>%filter(grepl("TM",Location))
#Proj_data<- Proj_data%>%filter(grepl("TM",Location)|grepl("TG",Location))
#Proj_data <- Proj_data%>%filter(dominantLandClass=="3e")
View(Proj_data)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# THE DATA EXPLORATION

# UNIVARIATE ANALYSIS AND BASIC R PROGRAMMING

# TASK 1a. SUMMARY OF THE BD5 GROUP
sum_stat_BD5 <- summary(BD5_species)

# Task 1b: Getting winsorized mean for BD5 GROUP
# Creating a data frame from 'sum_stat_BD5'
sum_stat_BD5 <- data.frame(
  Variable = c("Bird", "Bryophytes", "Butterflies", "Hoverflies", "Isopods"),
  Min = c(0.2415, 0.3941, 0.3167, 0.1235, 0.0462),
  First_Quartile = c(0.8462, 0.6886, 0.7926, 0.5696, 0.3917),
  Median = c(0.9038, 0.7993, 0.8863, 0.6957, 0.5394),
  Mean = c(0.8872, 0.7866, 0.8746, 0.6795, 0.5500),
  Third_Quartile = c(0.9570, 0.8855, 0.9677, 0.8063, 0.7162),
  Max = c(1.1720, 1.1746, 1.3944, 1.1453, 1.2577)
)

# Calculate 20% winsorized mean
winsorized_mean <- function(x, trim = 0.2) {
  low_bound <- quantile(x, trim)
  upp_bound <- quantile(x, 1 - trim)
  wins_m <- pmax(pmin(x, upp_bound), low_bound)
  mean(wins_m)
}
wins_m_BD5_species <- function(BD5_species, trim = 0.2) {
  sapply(BD5_species, winsorized_mean, trim = trim)
}
BD5_wins_mean <- wins_m_BD5_species(BD5_species, trim = 0.2)
print(BD5_wins_mean)

# Store the BD5_species winsorized mean values in a vector
BD5_wins_m <- c(0.9007277, 0.7887870, 0.8787996, 0.6876278, 0.5489126)

# Add the new vector as a column to the sum_stat_BD5 table
final_sum_stat_BD5 <- cbind(sum_stat_BD5, BD5_wins_m)
print(final_sum_stat_BD5)

# Task 2: Estimating the correlations between all pairs of variables in BD5_species
# Calculate the correlation between all 'BD5_species' pairs excluding Easting and Northing
cor_mat <- cor(BD5_species)
print(cor_mat)

# box plot comparisons for the two periods for BD5_species  
eco_status_5 <- Proj_data%>%pull(eco_status_5)
eco_status_11 <- Proj_data%>%pull(ecologicalStatus)
eco_period <- Proj_data%>%pull(period)
plot(eco_status_5~eco_period)
plot(eco_status_11~eco_period)  # compare to BD11 changes 

# Task 3: Perform the boxplot for one variable (Butterflies) in BD5_species
# Create a Box plot for the "Butterflies" variable
plot(BD5_species$Butterflies~eco_period)

# comparing the two by CULMULATIVE DISTRIBUTIONS  
par(mfrow=c(1, 1))  # divide graph area in 1 columns
qqplot(Proj_data$eco_status_5,Proj_data$ecologicalStatus)
abline(0,1,col="red")

# both cdfs together  and do a kolmogorov test H0: distributions are the same
BD5_cdf <- ecdf(Proj_data$eco_status_5)
BD11_cdf <- ecdf(Proj_data$ecologicalStatus)
plot(BD11_cdf,col="red")
lines(BD5_cdf,col="green")
ks.test(Proj_data$eco_status_5,Proj_data$ecologicalStatus)

# Task 5a: HYPOTHESIS TESTING 1 (t-test)
# Performing t-test to compare the means of BD11_Species and BD5_Species.
# comparing the ecological status of BD11_Species(Proj_data_all_11) and BD5_Species(Proj_data)
eco_status_t_test <- t.test(Proj_data_all_11$ecologicalStatus, Proj_data$eco_status_5)
print(eco_status_t_test)

# Task 5b: HYPOTHESIS TESTING 2 (Kolmogorov-Smirnov test)
# Conducting a test to assess whether the samples from Y70 and Y00 in BD5 originate from the same distribution
BD5_period_ks_test <- ks.test(eco_status_5 ~ period, data = Proj_data)
print(BD5_period_ks_test)

# following code splits between the two periods to find the change in eco measures

# now investigate the change of BD5 measure between the two periods 
names(Proj_data)
Proj_data_split <- Proj_data%>%select(Location,period,eco_status_5)%>%
  pivot_wider(names_from =period,values_from=eco_status_5)%>%
  mutate(BD5_change=Y00-Y70)
View(Proj_data_split)
hist(Proj_data_split$BD5_change)  # the distribution of the BD5 change 
BD5_change <- Proj_data_split%>%pull(BD5_change)
t.test(BD5_change,mu=0)  # t test with H0: mu=0

# repeat this for the BD11 changes 
names(Proj_data)
Proj_data_all_11_split <- Proj_data%>%select(Location,period,ecologicalStatus)%>%
  pivot_wider(names_from =period,values_from=ecologicalStatus)%>%
  mutate(BD11_change=Y00-Y70)
hist(Proj_data_all_11_split$BD11_change)  # the distribution of the BD5 change 
BD11_change <- Proj_data_all_11_split%>%pull(BD11_change)
t.test(BD11_change,mu=0)  # t test with H0: mu=0

plot(BD11_change~BD5_change) # scatter plot
abline(lm(BD11_change~BD5_change),col="green") # fits a best fit line through data points
abline(0,1,col="red") # plots a 45 degree line
cor(BD11_change,BD5_change) # correlation coefficient

# here inner join  the dataframes for BD5 and BD11 
Eco_change_BD11 <- Proj_data_all_11_split%>%select(Location,BD11_change)
Eco_change_BD5 <- Proj_data_split%>%select(Location,BD5_change)
Both_eco_change <- inner_join(Eco_change_BD11,Eco_change_BD5,by="Location")
View(Both_eco_change)

# here add two columns for BD5up and BD11up (see assignment brief for definitions)
Both_eco_change <- Both_eco_change%>%
  mutate(BD11up=ifelse(Both_eco_change$BD11_change>0,1,0))%>%
  mutate(BD5up=ifelse(Both_eco_change$BD5_change>0,1,0))
View(Both_eco_change)
table(Both_eco_change$BD11up)  # distribution of BD11up
table(Both_eco_change$BD5up)   # distribution of BD5up

# creating joint distribution , a contingency table to interpret (chapter 7 of the book)
Table_up_down <- table(Both_eco_change$BD11up,Both_eco_change$BD5up) # contingency table for interpretation 
colnames(Table_up_down) <- c("down","up");rownames(Table_up_down) <- c("down","up")
Table_up_down
GTest(Table_up_down) # log likelihood ratio test 
summary(Table_up_down) # summary also gives the chi squared test (similar p value)

# Calculating Odds Ratio, Sensitivity, Specificity and Youden's Index
odds_ratio <- OddsRatio(Table_up_down)
Sensitivity <- Sens(Table_up_down)
Specificity <- Spec(Table_up_down)
Youdens_index <- Sensitivity + Specificity - 1

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Performing Simple Linear Regression 
names(Proj_data_all_11)
names(Proj_data)  # I choose Carabids as its not one of the five 
BD1 <- Proj_data_all_11$Carabids  # Extracting the values of BD1 from the Proj_data_all_11 data frame

# Function to perform simple linear regression and print summary
perform_regression <- function(predictor) {
  formula <- as.formula(paste("BD1 ~", predictor))   # Creating a formula for the regression
  regression_model <- lm(formula, data = Proj_data_all_11)   # Performing the regression
  cat("Regression for", predictor, ":\n")
  print(summary(regression_model)) # Printing the summary
}

predictors <- c("Bird", "Bryophytes", "Butterflies", "Hoverflies", "Isopods")
lapply(predictors, perform_regression)   # Perform regression for each predictor variable
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Performing Multiple Linear Regression
# Estimating AIC for initial MLR Model
Initial_MLR <- lm(BD1 ~ Bird + Bryophytes + Butterflies + Hoverflies + Isopods, data = Proj_data)
summary(Initial_MLR)
(aic_init_model <- AIC(Initial_MLR))
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Performing Feature Selection
MLR1 <- lm(BD1 ~ Bird + Bryophytes + Butterflies + Hoverflies, data = Proj_data)
summary(MLR1)
(aic_MLR1 <- AIC(MLR1))
MLR2 <- lm(BD1 ~ Bird + Bryophytes + Butterflies + Isopods, data = Proj_data)
summary(MLR2)
(aic_MLR2 <- AIC(MLR2))
MLR3 <- lm(BD1 ~ Bird + Bryophytes + Hoverflies + Isopods, data = Proj_data)
summary(MLR3)
(aic_MLR3 <- AIC(MLR3))
MLR4 <- lm(BD1 ~ Bird + Butterflies + Hoverflies + Isopods, data = Proj_data)
summary(MLR4)
(aic_MLR4 <- AIC(MLR4))
MLR5 <- lm(BD1 ~ Bryophytes + Butterflies + Hoverflies + Isopods, data = Proj_data)
summary(MLR5)
(aic_MLR5 <- AIC(MLR5))
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Finding all possible combinations of two predictor variables
poss_combs <- combn(c("Bird", "Bryophytes", "Butterflies", "Hoverflies", "Isopods"), 2, simplify = TRUE)
best_model <- NULL
lowest_aic <- Inf
best_int <- NULL
# Iterate through each interaction combination, create an interaction term, fit a model and check for AIC
for (i in 1:ncol(poss_combs)) {
  int_term <- poss_combs[, i]
  model_formula <- as.formula(sprintf("BD1 ~ Bird + Bryophytes + Butterflies + Hoverflies + Isopods + %s*%s", int_term[1], int_term[2]))
  model <- lm(model_formula, data = Proj_data_all_11) 
  aic_value <- AIC(model)
  if (aic_value < lowest_aic) {
    best_model <- model
    lowest_aic <- aic_value
    best_int <- int_term
  }
}
summary(best_model)
cat("Best AIC:", lowest_aic, "\n")
cat("Best Interaction Term:", best_int[1], "*", best_int[2], "\n")

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Working with Training and Validation Sets
training_set <- subset(Proj_data_all_11, period == "Y70")
BD1_training_set <- training_set$Ladybirds
validation_set <- subset(Proj_data_all_11, period == "Y00")
BD1_validation <- validation_set$Ladybirds
model <- lm(BD1_training_set ~ Bird + Bryophytes + Butterflies + Hoverflies + Isopods, data = training_set)
training_preds <- predict(model, newdata = training_set)
validation_preds <- predict(model, newdata = validation_set)

# Computing Mean Squared Error (MSE) for the training and validation sets
(MSE_training <- mean((BD1_training_set - training_preds)^2))
(MSE_validation <- mean((BD1_validation - validation_preds)^2))
summary(model)

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Finally, the open section (to be written by you alone)
# Open Analysis 1
#  showing distribution of mean values for the BD5 Species
# Create an empty data frame 'table' to store summary statistics
BD5_table <- data.frame()
for(i in c(2:6)){
  BD5_table <- rbind(BD5_table,
                     c(names(Proj_data)[i],
                       round(mean(Proj_data[,i],na.rm = TRUE),digits = 2),
                       round(sd(Proj_data[,i],na.rm = TRUE),digits = 2),
                       round(skewness(Proj_data[,i],na.rm = TRUE),digits = 2)
                     ))} # Assign column names to the 'table' data frame

colnames(BD5_table) <- c("BD5_species","mean","sd","skewness") # Assign column names to the 'table' data frame
BD5_table%>%arrange(sd,skewness) # Display the 'table' data frame, arranged by standard deviation and skewness

# Create a histogram using ggplot2 to show distribution of mean values for the BD5 Species
ggplot(BD5_table, aes(x = BD5_species, y = mean, fill = BD5_species)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Mean Values by BD5 Species", x = "BD5 Species", y = "Mean") +
  theme_minimal()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Open Analysis 2
# Estimating Correlation all pairs of BD5 with Easting and Northing 
new_cor <- cor(Proj_data[, c("Bird", "Bryophytes", "Butterflies", "Hoverflies", "Isopods", "Easting", "Northing")])
print(new_cor)
new_cor_mat <- round(x = cor(new_cor,use="pairwise.complete.obs"), digits = 2)
print(new_cor_mat)

# plot correlation heatmap
cor_heatmap <- color_palette <- colorRampPalette(c("blue", "white", "red"))(20) # Define a color palette
heatmap(new_cor_mat, col = color_palette, symm = TRUE, margins = c(5, 5), main = "Correlation Heatmap")




