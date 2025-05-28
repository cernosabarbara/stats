library(dplyr)
library(car)
library(ggplot2)
library(corrplot)
library(psych)
library(imputeTS)
library(mice)

# Set base path for data files
base_path <- "C:/Users/Bibi Sophia/Downloads/Masters/stats_final/"

# Load each CSV file
file1 <- read.csv(paste0(base_path, "crime.csv"))
file2 <- read.csv(paste0(base_path, "unemployment_foreign.csv"))
file3 <- read.csv(paste0(base_path, "unemployment_domestic.csv"))
file4 <- read.csv(paste0(base_path, "edu_domestic.csv"))
file5 <- read.csv(paste0(base_path, "socialprotection.csv"))
file6 <- read.csv(paste0(base_path, "immigration.csv"))
file7 <- read.csv(paste0(base_path, "socialexclusion.csv"))
file8 <- read.csv(paste0(base_path, "population.csv"), stringsAsFactors = FALSE)
file9 <- read.csv(paste0(base_path, "edu_non-EU.csv"))
file10 <- read.csv(paste0(base_path, "suspects_citizenship_domestic.csv"))
file11 <- read.csv(paste0(base_path, "suspects_citizenship_foreign.csv"))

# Rename 'Value' column to respective variable and convert to numeric
file1$crime <- as.numeric(gsub(",", ".", file1$Value))
file2$unemployment_foreign <- as.numeric(gsub(",", ".", file2$Value))
file3$unemployment_domestic <- as.numeric(gsub(",", ".", file3$Value))
file4$edu_domestic <- as.numeric(gsub(",", ".", file4$Value))
file5$socialprotection <- as.numeric(gsub(",", ".", file5$Value))
file6$immigration <- as.numeric(gsub(",", ".", file6$Value))
file7$socialexclusion <- as.numeric(gsub(",", ".", file7$Value))
file8$population <- as.numeric(gsub(",", ".", file8$Value))
file9$edu_non_EU <- as.numeric(gsub(",", ".", file9$Value))
file10$suspects_domestic <- as.numeric(gsub(",", ".", file10$Value))
file11$suspects_foreign <- as.numeric(gsub(",", ".", file11$Value))

# Keep only relevant columns
file1 <- file1[, c("geo", "TIME_PERIOD", "crime")]
file2 <- file2[, c("geo", "TIME_PERIOD", "unemployment_foreign")]
file3 <- file3[, c("geo", "TIME_PERIOD", "unemployment_domestic")]
file4 <- file4[, c("geo", "TIME_PERIOD", "edu_domestic")]
file5 <- file5[, c("geo", "TIME_PERIOD", "socialprotection")]
file6 <- file6[, c("geo", "TIME_PERIOD", "immigration")]
file7 <- file7[, c("geo", "TIME_PERIOD", "socialexclusion")]
file8 <- file8[, c("geo", "TIME_PERIOD", "population")]
file9 <- file9[, c("geo", "TIME_PERIOD", "edu_non_EU")]
file10 <- file10[, c("geo", "TIME_PERIOD", "suspects_domestic")]
file11 <- file11[, c("geo", "TIME_PERIOD", "suspects_foreign")]

# Merge all datasets
combined <- Reduce(function(x, y) merge(x, y, by = c("geo", "TIME_PERIOD"), all = TRUE),
                   list(file1, file2, file3, file4, file5, file6, file7, file8, file9, file10, file11))

# Calculate immigration-related variables
combined <- combined %>%
  group_by(geo) %>%
  mutate(
    immigration_pct = (immigration / population) * 100,  # Yearly % of population
    cumulative_immigration = cumsum(ifelse(is.na(immigration), 0, immigration)),  
    cumulative_immigration_pct = (cumulative_immigration / population) * 100  # Cumulative % of population
  ) %>%
  ungroup()
# I calculated immigration as a percentage of population to normalize immigration flows across countries. 
# Cumulative immigration tracks total immigration over time, with NA values set to 0 to avoid gaps, 
# and its percentage shows long-term demographic changes, which may relate to crime trends.

# Filter for EU countries plus Norway, Switzerland, and UK, 2008–2023
eu_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland",
  "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
  "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden",
  "United Kingdom", "Norway", "Switzerland"
)
filtered_data <- combined %>%
  filter(geo %in% eu_countries, TIME_PERIOD >= 2008, TIME_PERIOD <= 2023)

# Impute missing values using Kalman filter before calculating ratios
filtered_data <- filtered_data %>%
  mutate(across(c("crime", "unemployment_domestic", "edu_domestic", "edu_non_EU",
                  "socialprotection", "immigration_pct", "cumulative_immigration_pct",
                  "socialexclusion", "suspects_domestic", "suspects_foreign", "unemployment_foreign"),
                na_kalman))

# Calculate ratio variables 
filtered_data <- filtered_data %>%
  mutate(
    unemployment_ratio = unemployment_foreign / unemployment_domestic,
    edu_ratio = edu_non_EU / edu_domestic,
    suspects_ratio = suspects_foreign / suspects_domestic
  )
# I created ratios of foreign to domestic unemployment, education, and suspects to compare integration challenges between groups. These ratios highlight what may drive crime, like economic or educational gaps, as suggested by social disorganization theory.

str(filtered_data)
numeric_vars <- c("crime", "socialprotection", "immigration_pct",
                  "cumulative_immigration_pct", "socialexclusion",
                  "unemployment_ratio", "edu_ratio", "suspects_ratio")

# Explore missing data patterns
dim(filtered_data)
md.pattern(filtered_data) #missing data patterns (pretty blue red)

#miceImp<- mice(filtered_data,m=5,maxit=10) 

# Compute correlation matrix for all data
cor_matrix <- cor(filtered_data[, numeric_vars], use = "complete.obs")
print(cor_matrix)


# PART I: Identifying change in crime rates over time

# Summary statistics: mean and standard error for crime rate by country
# I calculate mean crime rates and confidence intervals to compare countries
tmp <- filtered_data %>%
  group_by(geo) %>%
  summarise(mean_crime = mean(crime, na.rm = TRUE),
            se_crime = sd(crime, na.rm = TRUE) / sqrt(n())) %>%
  arrange(desc(mean_crime))

# Error bars plot for all countries
# I plot mean crime rates with 95% CI to show variation across countries
ggplot(tmp, aes(x = reorder(geo, mean_crime), y = mean_crime, 
                ymin = mean_crime - 1.96 * se_crime, ymax = mean_crime + 1.96 * se_crime)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  labs(x = "Country", y = "Mean Crime Rate (per 100,000)") +
  ggtitle("Mean Crime Rate by Country with 95% CI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Filter for 2020–2022
data_2020_2022 <- filtered_data %>%
  filter(TIME_PERIOD >= 2020 & TIME_PERIOD <= 2022)

# Calculate average crime rates for 2020–2022, top 10
top_10_crime_rates <- data_2020_2022 %>%
  group_by(geo) %>%
  summarise(avg_crime_rate_2020_2022 = mean(crime, na.rm = TRUE)) %>%
  arrange(desc(avg_crime_rate_2020_2022)) %>%
  head(10)

# Filter for 2008–2009
data_2008_2009 <- filtered_data %>%
  filter(TIME_PERIOD >= 2008 & TIME_PERIOD <= 2009)

# Average crime rates for 2008–2009
crime_rates_2008_2009 <- data_2008_2009 %>%
  group_by(geo) %>%
  summarise(avg_crime_rate_2008_2009 = mean(crime, na.rm = TRUE))

# Compare crime rates
comparison <- top_10_crime_rates %>%
  left_join(crime_rates_2008_2009, by = "geo") %>%
  mutate(
    change = avg_crime_rate_2020_2022 - avg_crime_rate_2008_2009,
    percent_change = (change / avg_crime_rate_2008_2009) * 100
  )

# Filter for top 6 high-crime countries
top_6 <- filtered_data %>% filter(geo %in% c("Denmark", "Finland", "France", "Ireland", "Sweden", "Norway"))

# Calculate slopes for crime trends in top-6 countries
# I compute slopes to identify countries with the strongest crime increases
top_6_slopes <- top_6 %>%
  group_by(geo) %>%
  summarise(slope = coef(lm(crime ~ TIME_PERIOD))[2]) %>%
  filter(slope > 0) %>%
  arrange(desc(slope)) %>%
  head(6)

# Line chart for top-6 countries with positive slopes
# I visualize trends for countries with the largest crime increases
top_6_slope_data <- top_6 %>%
  filter(geo %in% top_6_slopes$geo)
ggplot(top_6_slope_data, aes(x = TIME_PERIOD, y = crime, color = geo)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Crime Rate Trends for Top-6 Countries with Positive Slopes", 
       x = "Time Period", 
       y = "Crime Rate (per 100,000)") +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set3")

knitr::kable(comparison, digits = 2, caption = "Crime Rate Comparison (per 100,000)")
# I will examine the top 6 countries with the highest positive change in crime rates in my paper


ggplot(top_6, aes(x = TIME_PERIOD, y = crime, color = geo, group = geo)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Crime Rates Over Time (Significant Trends)",
    x = "Time Period",
    y = "Crime Rate"
  ) +
  theme(legend.position = "none")
# Crime Trends: Crime rates surged in the top-six countries, with Denmark (+224%) and France (+194%) showing the largest increases from 2008–2009 to 2020–2022, reflecting significant social changes.


# Part 2: Correlation and linear regression for top-6 countries
# Correlation matrix for top 6 countries
cor_matrix_top6 <- cor(top_6[, numeric_vars], use = "complete.obs")
print(cor_matrix_top6)

# Regression model for top 6 countries
crime_model_top_6 <- lm(crime ~ unemployment_ratio + edu_ratio + suspects_ratio +
                          cumulative_immigration_pct + socialprotection, data = top_6)
summary(crime_model_top_6)

# These are the countries with the highest increses in crime rates which were the central point of my research paper. The LRM captures 67.8% of crime variance with all the variables being statistically significant.  

# Edu_ratio shows lower immigrant education vs. natives increases crime.
# Unemployment_ratio suggests foreign unemployment fuels crime.
# Cumulative_immigration_pct indicates long-term immigration insreases crime, possibly due to lower integration.
# Suspects_ratio confirms foreign-born suspects drive crime spikes—for example Sweden’s immigrant enclaves or Denmark’s high cumulative immigration.

# Scatter plots for top 6 countries
ggplot(top_6, aes(x = suspects_ratio, y = crime, color = geo)) +
  geom_point() + labs(title = "Crime vs. Suspects Ratio (Non-EU/Domestic)")

ggplot(top_6, aes(x = unemployment_ratio, y = crime, color = geo)) +
  geom_point() + labs(title = "Crime vs. unemployment_ratio")

ggplot(top_6, aes(x = cumulative_immigration_pct, y = crime, color = geo)) +
  geom_point() + labs(title = "Crime vs. cumulative_immigration_pct")

ggplot(top_6, aes(x = edu_ratio, y = crime, color = unemployment_ratio)) +
  geom_point() + labs(title = "Crime vs. Edu Ratio, Colored by Unemployment Ratio")

ggplot(top_6, aes(x = suspects_ratio, y = crime, color = cumulative_immigration_pct)) +
  geom_point() + labs(title = "Crime vs. Suspects Ratio (Non-EU/Domestic)")


# Expanded regression model for top 6 countries
crime_model_top_6 <- lm(crime ~ unemployment_ratio + edu_ratio + suspects_ratio +
                          cumulative_immigration_pct + socialprotection + socialexclusion, data = top_6)
summary(crime_model_top_6)

# The multiple R squared for this model is even 0.772, with all but edu_ratio being statistically significant, and the unemployment_ratio , cumulative_immigration_pct and socialexclusion being the most significant

ggplot(top_6, aes(x = socialexclusion, y = crime, color = geo)) +
  geom_point() + labs(title = "Crime vs. socialexclusion")

ggplot(top_6, aes(x = cumulative_immigration_pct, y = crime, color = unemployment_ratio)) +
  geom_point() + labs(title = "Crime vs. cumulative_immigration_pct (by unemployment_ratio)")



# ANOVA for top-6 countries
# I test if crime rates differ significantly across top-6 countries and key variables
aov_crime_geo <- aov(crime ~ geo, data = top_6)
aov_crime_suspects <- aov(crime ~ suspects_ratio, data = top_6)
aov_crime_socialexclusion <- aov(crime ~ socialexclusion, data = top_6)
print("ANOVA: Crime by Country")
summary(aov_crime_geo)
print("ANOVA: Crime by Suspects Ratio")
summary(aov_crime_suspects)
print("ANOVA: Crime by Social Exclusion")
summary(aov_crime_socialexclusion)
# Crime differs significantly across countries (p<2e-16), and suspects_ratio (p=1.03e-09) and socialexclusion (p=7.7e-09) strongly affect crime, confirming regression findings.

# PCA for top-6 countries
# I use PCA to reduce dimensions and identify key variable patterns
pca_data <- top_6[, numeric_vars] %>% na.omit() %>% scale()
pca_result <- prcomp(pca_data, scale. = TRUE)
# PC1 and PC2 explain 61.41% of variance (38.05% and 23.36%), reducing dimensions and showing key patterns in crime, socialexclusion, and immigration variables.

summary(pca_result)
# Biplot to visualize countries and variables
biplot(pca_result, main = "PCA Biplot: Top-6 Countries")

# Factor Analysis for top-6 countries
# I use FA to group variables into latent factors (e.g., integration)
fa_data <- top_6[, numeric_vars] %>% na.omit() %>% scale()
# Suitability checks
KMO(fa_data)
cortest.bartlett(cor(fa_data), n = nrow(fa_data))
# KMO=0.52 and Bartlett’s test (p=4.4e-67) confirm FA suitability. Two factors explain 52% of variance: ML1 (crime, suspects_ratio) and ML2 (edu_ratio, socialexclusion). Fit is moderate (RMSEA=0.298), suggesting integration-related factors drive crime.

fa_result <- fa(fa_data, nfactors = 2, rotate = "varimax", fm = "ml")
print(fa_result, digits = 2)
# Visualize factor loadings
fa.diagram(fa_result)


# Part 3
# Correlation and linear regression for non-top-6 countries
non_top_6 <- filtered_data %>% filter(!geo %in% c("Denmark", "Finland", "France", "Ireland", "Sweden", "Norway"))
# Correlation matrix
cor_matrix_non_top6 <- cor(non_top_6[, numeric_vars], use = "complete.obs")
print(cor_matrix_non_top6)
# The correlation matrix shows weak relationships, with socialprotection (0.36) and immigration_pct (0.32) having the strongest positive correlations with crime

# Regression model
crime_model_non_top_6 <- lm(crime ~ unemployment_ratio + edu_ratio + suspects_ratio +
                              cumulative_immigration_pct + socialexclusion, data = non_top_6)
summary(crime_model_non_top_6)
# The regression model explains only 10% of crime variance (R-squared: 0.1026), far less than the top-6 model (77%), indicating other factors drive crime in these countries. Significant predictors include edu_ratio, suspects_ratio, and cumulative_immigration_pct

# Regression model for all data
crime_model <- lm(crime ~ unemployment_ratio + edu_ratio + suspects_ratio +
                    cumulative_immigration_pct + socialprotection, data = filtered_data)
summary(crime_model)
# The model for all the countries has a R-squared:  0.46 (meaning there are other variables potentially contributing to it) with socialprotection being statistically significant

ggplot(filtered_data, aes(x = edu_ratio, y = crime, color = geo)) +
  geom_point() + labs(title = "Crime vs. Education Ratio (Non-EU/Domestic)")

# Conclusion
# In my paper, I examined how crime rates, specifically sexual violence, relate to social factors in European countries from 2008 to 2023.
# My key findings show that crime rates increased significantly in the top-6 countries, with Denmark (+224%) and France (+194%) showing the largest rises from 2008–2009 to 2020–2022. 
# In these countries, higher ratios of foreign suspects (r = 0.57) and social exclusion (r = 0.55) strongly correlated with crime, and my regression model explained 77% of crime variance, with unemployment ratio, suspect ratio, cumulative immigration, and social exclusion as key drivers. 
# For non-top-6 countries, crime had weaker links to immigration (r = 0.32) and education ratios (r = 0.22), and my model explained only 10% of crime variance, suggesting other factors matter more. 
# This shows that immigration-related issues drive crime more in high-crime countries, while non-top-6 countries are more stable.

