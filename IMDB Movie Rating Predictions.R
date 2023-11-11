require(visreg)
require(glue)
require(car)
require(lmtest)
require(splines)
require(psych)
require(stargazer)
require(plm)
require(ggplot2)
require(ggpubr)
require(caTools)
require(methods)
require(boot)
require(tidyr)
require(dplyr)
require(caret)
library(ggplot2)
library(gridExtra)
library(lmtest)
library(sandwich)
library(boot)

path <- "C:/Users/vinay/OneDrive/Documents/R-Files/"

#Attaching dataset
imdb_data <- read.csv(paste0(path, "imdb_training_data.csv"))
attach(imdb_data)

###############################################################################################
#################################### Phase:1 Data Preprocessing ###############################
###############################################################################################

#Drop label columns
data <- imdb_data %>%
  select(-movie_title, -movie_id, -imdb_link)

#Drop different genre columns
columns_to_remove <- c(
  "action", "adventure", "scifi", "thriller",
  "musical", "romance", "western", "sport",
  "horror", "drama", "war", "animation", "crime"
)
data <- data %>%
  select(-one_of(columns_to_remove))

#Dummify the genres column
data <- data %>%
  separate_rows(genres, sep = "\\|") %>% # Split genres into separate rows
  mutate(genre = 1) %>% # Add a column of 1s as a placeholder
  pivot_wider(names_from = genres, values_from = genre, names_prefix = "genres", values_fill = 0)

#Dummify the plot_keywords column
data <- data %>%
  separate_rows(plot_keywords, sep = "\\|") %>% # Split genres into separate rows
  mutate(plot_keyword = 1) %>% # Add a column of 1s as a placeholder
  pivot_wider(names_from = plot_keywords, values_from = plot_keyword, names_prefix = "plot_keywords", values_fill = 0)

#Dummify the categorical columns
columns_to_dummify <- c(
  "release_month", "language", "country", "maturity_rating",
  "distributor", "director", "actor1", "actor2", "actor3",
  "colour_film", "cinematographer", "production_company"
)
for (colName in columns_to_dummify) {
  formula <- reformulate(colName)
  dv <- dummyVars(formula, data = data, fullRank = FALSE)
  dv <- predict(dv, newdata = data)
  data <- data %>% select(-all_of(colName))
  # Append the dummy columns to the main data
  data <- cbind(data, dv)
}

#Boxplot for the numerical columns
#Plugin what columns you would want the boxplot for in the vector !!!!!
data_long <- data %>%
  pivot_longer(cols = c(imdb_score, movie_budget, duration, aspect_ratio, nb_news_articles, nb_faces),
               names_to = "variable", 
               values_to = "value")
#Plot boxplots
ggplot(data_long, aes(x = variable, y = value)) + 
  geom_boxplot(aes(fill = variable)) + 
  facet_wrap(~ variable, scales = "free", ncol = 6) + 
  labs(y = "Value", x = "", title = "Boxplots of Various Variables against IMDb Score") + 
  theme_minimal() + 
  theme(legend.position = "none")

#Histograms plotting
#Plugin what columns you would want the histogram for in the vector !!!!!
data_long2 <- data %>%
  pivot_longer(cols = c(movie_budget, duration, aspect_ratio, nb_news_articles, nb_faces),
               names_to = "variable", 
               values_to = "value")
#Plot histograms
ggplot(data_long2, aes(x = value)) + 
  geom_histogram(aes(fill = variable), bins = 30, position = "identity", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free_x", ncol = 5) + 
  labs(x = "Value", y = "Count", title = "Histograms of Various Variables") + 
  theme_minimal() + 
  theme(legend.position = "none")

#Removal of outliers
#Function to compute non-outlier range
compute_range <- function(column) {
  Q1 <- quantile(column, 0.25)
  Q3 <- quantile(column, 0.75)
  IQR <- Q3 - Q1
  list(lower = Q1 - 1.5 * IQR, upper = Q3 + 1.5 * IQR)
}

#Remove outliers for each column
#Currently this removes outliers from movie_budget, duration, aspect_ratio, nb_news_articles, nb_faces
data_filtered <- data %>%
  filter(between(movie_budget, 
                 compute_range(movie_budget)$lower, 
                 compute_range(movie_budget)$upper)) %>%
  filter(between(duration, 
                 compute_range(duration)$lower, 
                 compute_range(duration)$upper)) %>%
  filter(between(aspect_ratio, 
                 compute_range(aspect_ratio)$lower, 
                 compute_range(aspect_ratio)$upper)) %>%
  filter(between(nb_news_articles, 
                 compute_range(nb_news_articles)$lower, 
                 compute_range(nb_news_articles)$upper)) %>%
  filter(between(nb_faces, 
                 compute_range(nb_faces)$lower, 
                 compute_range(nb_faces)$upper))

data_filtered <- data_filtered[, sapply(data_filtered, function(col) sum(col) >= 2)]

###############################################################################################
##################################### Phase:1 Ends ############################################
###############################################################################################

###############################################################################################
################################## Phase:2 Model Selection ####################################
###############################################################################################
names(data_filtered) <- gsub("[ &-]", "_", names(data_filtered))

# Identify the index of the 'movie_meter_IMDBpro' column
index <- which(names(data_filtered) == "movie_meter_IMDBpro")

# Store all subsequent column names in the categorical_columns vector
categorical_columns <- names(data_filtered)[(index+1):ncol(data_filtered)]
categorical_columns

# Identify numeric columns 'imdb_score'
numeric_cols <- sapply(data_filtered, is.numeric)
numeric_col_names <- names(data_filtered)[numeric_cols]

quantvars <- data_filtered[, numeric_col_names]
corr_matrix=cor(quantvars)
# Extract the correlation values for 'imdb_score' only
imdb_corr = corr_matrix['imdb_score',]
# Round the values
rounded_imdb_corr = round(imdb_corr, 3)
View(rounded_imdb_corr)

#FINDING THE STRONGEST POSITIVE AND NEGATIVE CORRELATIONS TO IMDB_SCORE
# Sort the values to get top positive correlations, and exclude the correlation of imdb_score with itself (which is 1)
top_positive = sort(imdb_corr[imdb_corr < 1], decreasing = TRUE)[1:10]
# Sort the values to get top negative correlations
top_negative = sort(imdb_corr)[1:10]

top_positive
top_negative

#SCATTERPLOTS FOR ALL NUMERIC VARIABLES (NOT DUMMY COLUMNS)
#Create scatter plots using a for loop
plot_list <- list()
for (col in numeric_col_names) {
  if (!col %in% categorical_columns) {
    plot <- ggplot(data_filtered, aes_string(x = col, y = "imdb_score")) +
      geom_point(alpha = 0.5) +
      labs(title = paste("imdb_score vs", col), x = col, y = "imdb_score")
    plot_list[[col]] <- plot
  }}

# Arrange the plots in a grid
grid.arrange(grobs = plot_list, ncol = 3)

# SINGLE VARIABLE REGRESSIONS AGAINST IMDB_SCORE
# Get all the column names except for 'imdb_score'
predictor_vars <- setdiff(names(data_filtered), "imdb_score")
# Rsquared, P value and Coeff estimate DF

# Initialize an empty dataframe to store the results
results_df <- data.frame()  # Ensure that this dataframe is initially empty

for (var in predictor_vars) {
  formula_string <- as.formula(paste("imdb_score ~", "`", var, "`", sep = ""))
  model <- lm(formula_string, data = data_filtered)
  
  model_summary <- summary(model)
  
  coeff_estimate <- coef(model)[2]
  p_value <- model_summary$coefficients[2, 4]
  r_squared <- model_summary$r.squared

  # Append results to the results dataframe
  results_df <- rbind(results_df, data.frame(
    Variable = var,
    Coefficient = coeff_estimate,
    P_Value = p_value,
    R_Squared = r_squared
  ))
}

write.csv(results_df, paste0(path, "result_df.csv"), row.names=TRUE)

#NCV test - For heteroskedacitiy
# Initialize an empty list to store variable names
significant_vars <- c()

for (var in predictor_vars) {
  if (var %in% categorical_columns) {
    next  # Skip this iteration and move to the next variable
  }
  
  formula_string <- as.formula(paste("imdb_score ~", "`", var, "`", sep = ""))
  model <- lm(formula_string, data = data_filtered)
  
  test_result <- ncvTest(model)
  p_value <- test_result$p
  
  if (is.null(p_value)) {
    cat("Variable:", var, "Model summary:\n")
    print(summary(model))
  } else {
    cat("Variable:", var, "P-value:", p_value, "\n")
    if (p_value < 0.05) {
      # Append variable to the list if p-value < 0.05
      significant_vars <- c(significant_vars, var)
    }
  }
}

# Print the significant variables after the loop
print(significant_vars)

#################################################################################################
# REDO the Results df to account for heteroskedasticity
#################################################################################################

results_df_het <- data.frame()

for (var in predictor_vars) {
  formula_string <- as.formula(paste("imdb_score ~", "`", var, "`", sep = ""))
  model <- lm(formula_string, data = data_filtered)
  
  # Check if the variable is in significant_vars and apply coeftest
  if (var %in% significant_vars) {
    coeftest_result <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
    coeff_estimate <- coeftest_result[2, 1]
    p_value <- coeftest_result[2, 4]
  } else {
    model_summary <- summary(model)
    coeff_estimate <- coef(model)[2]
    p_value <- model_summary$coefficients[2, 4]
  }
  
  r_squared <- summary(model)$r.squared
  
  # Append results to the results dataframe
  results_df_het <- rbind(results_df, data.frame(
    Variable = var,
    Coefficient = coeff_estimate,
    P_Value = p_value,
    R_Squared = r_squared
  ))
}

# Filter the dataframe
filtered_df <- results_df_het %>% 
  filter(P_Value < 0.05) %>%
  arrange(desc(R_Squared)) %>%  # Sort by R_Squared descending
  mutate(Rank = row_number())  # Add a new column 'Rank' representing the number of variables

# Ensure that the R_Squared column is numeric for plotting
filtered_df$R_Squared <- as.numeric(as.character(filtered_df$R_Squared))

# Create a plot with enhanced aesthetics
ggplot(filtered_df, aes(x = Rank, y = R_Squared)) +
  geom_line(color = "#3498db") +
  geom_point(color = "#3498db", size = 1.5) +
  geom_hline(yintercept = 0.01, linetype="dashed", color="black", size=0.8) + # Made the line dashed and less thick
  labs(title = "R_Squared",
       x = "Number of Variables",
       y = "R_Squared") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis text for better visibility
        plot.title = element_text(hjust = 0.5),  # Center the title
        panel.grid.major = element_line(colour = "#d3d3d3"),  # Light grey grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),  # Remove panel background
        plot.background = element_rect(fill = "#f7f7f7"),  # Light grey plot background
        axis.title = element_text(face="bold")) +  # Bold axis titles
  scale_y_continuous(breaks = c(0.01), labels = c(0.01))  # Add a label for the 0.01 point on the y-axis



write.csv(results_df_het, paste0(path, "results_df_het"), row.names=TRUE)

# Set up a grid for the plots
par(mfrow=c(4,2))

# Loop over each variable in significant_vars
for (var in significant_vars) {
  formula_string <- as.formula(paste("imdb_score ~", var))
  model <- lm(formula_string, data = data_filtered)
  
  # Add variable name as the title of the residual plot
  residualPlot(model, quadratic=FALSE, main=paste("Residual Plot for:", var))
}

# Reset the graphical parameters to default
par(mfrow=c(1,1))

#Get Tukey test values for all variables

# Initialize an empty list to store variable names
significant_nonlinear <- c()

for (var in predictor_vars) {
  if (var %in% categorical_columns) {
    next  # Skip this iteration and move to the next variable
  }
  
  formula_string <- as.formula(paste("imdb_score ~", "`", var, "`", sep = ""))
  model <- lm(formula_string, data = data_filtered)
  
  # Generate residual plots for the model
  res_plots <- residualPlots(model)
  
  # Extract the p-value for the variable in question
  p_value <- res_plots[3]
  
  cat("Variable:", var, "P-value:", p_value, "\n")
  if (p_value < 0.1) {
    # Append variable to the list if p-value < 0.1
    significant_nonlinear <- c(significant_nonlinear, var)
  }
}

significant_nonlinear

################################################################################
#FILTERING OUT THE VARIABLES THAT HAVE PVALUE GREATER THAN 0.05 (NON SIGNIFICANT PREDICTORS)
model_predictor <- subset(results_df_het, P_Value < 0.05 )$Variable
model_predictor <- setdiff(model_predictor, categorical_columns)

#MULTICOLLINEARITY TEST
# Use backticks around each variable name
formatted_vars <- paste0("`", model_predictor, "`", collapse = " + ")

# Create the formula string
formula_string <- as.formula(paste("imdb_score ~", formatted_vars))
model <- lm(formula_string, data = data_filtered)
summary(model)

# Check the VIF for the model
vif_scores <- vif(model)

# Print the VIF scores
print(vif_scores)

# Create the formula string with all your predictors
model_predictor1 <- subset(results_df_het, P_Value < 0.05 & R_Squared > 0.01)$Variable
formatted_vars <- paste0("`", model_predictor1, "`", collapse = " + ")

formula_string <- as.formula(paste("imdb_score ~", formatted_vars))

# Fit the model
fit <- glm(formula_string, data = data_filtered)

# Perform k-fold cross-validation
set.seed(123) # for reproducibility
mse <- cv.glm(data_filtered, fit, K=10)$delta[1]

print(paste("Mean Squared Error from 10-fold CV:", mse))

test_data <- read.csv(paste0(path, "imdb_test_data.csv"))

####################################################################################
non_linear_vars <- c("movie_budget", "release_year", "nb_news_articles", "actor3_star_meter", "nb_faces", "movie_meter_IMDBpro")

normal_string <- ""
non_linear_significant <- c()

for (var in model_predictor1) {
  if (var %in% non_linear_vars) {
    non_linear_significant <- c(non_linear_significant, var)
    next
  }
  normal_string <- paste0(normal_string, " ", var, " +")
}

model_strings_list <- list()

for (i in 1:5) {
  for (j in 1:5) {
    if (i == 1) {
      release_year_term <- "release_year"
    } else {
      release_year_term <- paste0("poly(release_year,", i,")")
    }
    if (j == 1) {
      nb_news_articles_term <- "nb_news_articles"
    } else {
      nb_news_articles_term <- paste0("poly(nb_news_articles,", j,")")
    }
    newStr <- paste(normal_string, release_year_term, "+", nb_news_articles_term)
    formula_string <- as.formula(paste("imdb_score ~", newStr))
    model_strings_list <- c(model_strings_list, formula_string)
  }
}

model_list <- list()

for (idx in 1:length(model_strings_list)) {
  formula_string <- model_strings_list[[idx]]
  print(formula_string)
  model <- lm(formula_string, data=data_filtered)
  model_name <- paste("model", idx)
  model_list[[model_name]] <- model
}

names(model_list) <- NULL
anova_results <- do.call(anova, model_list)
print(anova_results)

####################### Anova Done #########################

fit <- glm(model_strings_list[[12]], data = data_filtered)

set.seed(123) # For reproducibility
mse <- cv.glm(data_filtered, fit, K=10)$delta[1]

print(paste("Mean Squared Error from 10-fold CV:", mse))

summary(model)

###############################################################################################
################################### Phase:2 Ends ##############################################
###############################################################################################

###############################################################################################
################################### Phase:3 Making Predictions ################################
###############################################################################################

# Attaching Dataset
imdb_test_data <- read.csv(paste0(path, "imdb_test_data.csv"))
attach(imdb_test_data)

#Drop label columns
test_data <- imdb_test_data %>%
  select(-movie_title, -movie_id, -imdb_link)

#Drop different genre columns (same as Phase:1 step)
test_data <- test_data %>%
  select(-one_of(columns_to_remove))

#Dummify the genres column
test_data <- test_data %>%
  separate_rows(genres, sep = "\\|") %>% # Split genres into separate rows
  mutate(genre = 1) %>% # Add a column of 1s as a placeholder
  pivot_wider(names_from = genres, values_from = genre, names_prefix = "genres", values_fill = 0)

#Dummify the plot_keywords column
test_data <- test_data %>%
  separate_rows(plot_keywords, sep = "\\|") %>% # Split genres into separate rows
  mutate(plot_keyword = 1) %>% # Add a column of 1s as a placeholder
  pivot_wider(names_from = plot_keywords, values_from = plot_keyword, names_prefix = "plot_keywords", values_fill = 0)

#Dummify the categorical columns (Removed language, film_colour since they had only 1 value)
columns_to_dummify_test <- c(
  "country", "maturity_rating",
  "distributor", "director", "actor1", "actor2", "actor3",
  "cinematographer", "production_company"
)
for (colName in columns_to_dummify_test) {
    formula <- reformulate(colName)
    dv <- dummyVars(formula, data = test_data, fullRank = FALSE)
    dv <- predict(dv, newdata = test_data)
    test_data <- test_data %>% select(-all_of(colName))
    # Append the dummy columns to the main data
    test_data <- cbind(test_data, dv)
}

# Handling missing columns in the test_data
for(column in colnames(data_filtered)){
  if(!(column %in% colnames(test_data))){
    test_data[[column]] <- 0
  }
}

# Handling extra columns in the test_data
for(column in colnames(test_data)){
  if(!(column %in% colnames(data_filtered))){
    test_data[[column]] <- NULL
  }
}

# Predict imdb_score values for test_data based on the model (the 'model' value can be changed to check different predictions)
predicted_values_test <- predict(model, newdata = test_data)

predicted_values_test

###############################################################################################
##################################### Phase:3 Ends ############################################
###############################################################################################
