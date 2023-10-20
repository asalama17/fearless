
###
  # title: "DS6306 - Analyzing US Craft Beers and Breweries - Case Study"
  # output: R Script
  # Author(s): Ahmad Salama & Almuhannad Qneis
  # date: "2023-10-08"
###

# load libraries:
library(tidyverse)
library(dplyr)
library(GGally)
library(ggplot2)
library(ggthemes)
library(maps)
library(usdata)
library(class)
library(caret)
###

# set working directory:
setwd("./data")

# preparing and loading data related to US states and their geographical information
us_state_map_data <- map_data("state")
us_states <- data.frame(
  StateAbb = state.abb,
  StateName = tolower(state.name)
)
us_state_center_geo <- data.frame(region=tolower(state.name), CenterLong=state.center$x, CenterLatitude=state.center$y)


# load breweries data:
breweries_data_raw <- read.csv("Breweries.csv", header = TRUE, stringsAsFactors = FALSE)
# trim character values in the data
breweries_data_cleaned <- breweries_data_raw %>% 
  sapply(function(x) if(is.character(x)) str_squish(x) else x)
breweries_data_cleaned <- as.data.frame(breweries_data_cleaned)
breweries_data_cleaned$Brew_ID <- as.integer(breweries_data_cleaned$Brew_ID)

summary(breweries_data_cleaned)
head(breweries_data_cleaned)

# load beers data:
beers_data_raw <-  as.data.frame(read.csv("Beers.csv", header = TRUE))
beers_data_cleaned <- beers_data_raw 
# change ABV from character to numeric values
beers_data_cleaned$ABV = as.numeric(beers_data_cleaned$ABV)
breweries_data_cleaned$State <- as.factor(str_squish(breweries_data_cleaned$State))



# 1. How many breweries are present in each state?

# verify that there are no missing values in the breweries data set:
breweries_missing_data_df <- breweries_data_cleaned %>% sapply(function(x) sum(is.na(x))) #no missing data.

# creating a bar plot to visualize the count of breweries in different US states.
breweries_state_count <- breweries_data_cleaned %>% 
  group_by(State) %>%
  summarise(Count = n())

breweries_state_count %>%
  ggplot(mapping = aes(x = reorder(State, +Count))) +
  geom_bar(mapping = aes(y = Count), stat = "identity") + 
  ggtitle("Brewery Count by US State") +
  xlab("State") +
  ylab("Count") +
  geom_text(mapping = aes(x = reorder(State, +Count), y = Count, label = Count),
            stat = 'identity', position = position_dodge(.9), vjust = -0.5, size = 4, color = "red") +
  theme_economist()

# addition plot: create a heat map displaying the count of breweries in US states.
breweries_state_count <- merge(breweries_state_count,us_states, by.x = "State", by.y = "StateAbb", all.x = TRUE)
breweries_state_count <- left_join(us_state_map_data, breweries_state_count, by = c("region" = "StateName"))
breweries_state_count <- left_join(breweries_state_count, us_state_center_geo, by = c("region" = "region"))

breweries_state_count %>%
  ggplot(mapping = aes(x = long, y = lat, group = group, fill = Count)) +
  geom_polygon() +
  geom_path(color = "white", size = 1.5) +  # Add state borders
  geom_text(mapping = aes(CenterLong, CenterLatitude, label = Count), color = "white") +
  coord_fixed(ratio=1.5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", 
                      name = "Brewery Count") +
  ggtitle("Brewery Count by US State") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_economist()

# 2.	Merge beer data with the breweries data. Print the first 6 observations
# merge breweries & beers data based on Brewery_id column.
beer_breweries <- left_join(beers_data_cleaned, breweries_data_cleaned, by = c("Brewery_id" = "Brew_ID"))
# change column names for clarity:
colnames(beer_breweries)[colnames(beer_breweries) == "Name.x"] <- "Beer_Name"
colnames(beer_breweries)[colnames(beer_breweries) == "Name.y"] <- "Brewery_Name"
head(beer_breweries)

# 3.	Address the missing values in each column:

# calc mean for IBU and ABV before the handling of missing values
beer_breweries %>% 
  summarize(IBU = mean(.data[["IBU"]], na.rm = TRUE), ABV  = mean(.data[["ABV"]], na.rm = TRUE))

beer_breweries %>% sapply(function(x) sum(is.na(x))) 

missing_values_count <- abv_ibu_mean_grouped_by_style_city %>%
  sapply(function(x) sum(is.na(x)))

missing_values_count <- abv_ibu_mean_grouped_by_style %>%
  sapply(function(x) sum(is.na(x)))

missing_values_by_state <- beer_breweries %>% 
  group_by(State) %>%
  summarise(ibu_na = sum(is.na(IBU)), abv_na = sum(is.na(ABV)), count = n())

missing_values_by_state %>% ggplot(mapping = aes(x = reorder(State, +count))) +
  geom_bar(aes(y = count, fill = "Total Observations"), stat = "identity") +
  geom_bar(aes(y = ibu_na, fill = "IBU NA"), stat = "identity") +
  geom_bar(aes(y = abv_na, fill = "ABV NA"), stat = "identity") +
  scale_y_continuous(name = "Count", breaks = seq(0, 300, by = 20)) +
  ggtitle("Count of Missing Values in Total Observations by State") +
  xlab("State") +
  ylab("Count") +
  #scale_fill_identity(name = '', guide = 'legend',labels = c('IBV NA', 'IBU NA', 'Total Observations')) +
  scale_fill_manual(values = c("Total Observations" = "grey27", "IBU NA" = "forestgreen", "ABV NA" = "firebrick1")) +
  theme_economist()

missing <- beer_breweries %>% 
  group_by(City) %>%
  summarise(sum_na = sum(is.na(IBU)), total = n())

plot_missing_values(beer_breweries)

# calc mean of ABV & IBU by city, state, and style:
abv_ibu_mean_grouped_by_style_city <- beer_breweries %>% 
  group_by(Style, City) %>%
  summarize(IBU = mean(.data[["IBU"]], na.rm = TRUE), ABV  = mean(.data[["ABV"]], na.rm = TRUE))

for(i in 1:nrow(beer_breweries)) {
  if(is.na(beer_breweries[i,"ABV"])) {
    sub_abv = abv_ibu_mean_grouped_by_style_city %>%
      filter(City == beer_breweries[i,"City"], Style == beer_breweries[i,"Style"])
    beer_breweries[i,"ABV"] <- sub_abv[1,"ABV"][1,1]
  }
  
  if(is.na(beer_breweries[i,"IBU"])) {
    sub_ibu = abv_ibu_mean_grouped_by_style_city %>% 
      filter(City == beer_breweries[i,"City"])
    beer_breweries[i,"IBU"] <- sub_ibu[1,"IBU"][1,1]
  }
}
plot_missing_values(beer_breweries)

beer_breweries %>% 
  summarize(IBU = mean(.data[["IBU"]], na.rm = TRUE), ABV  = mean(.data[["ABV"]], na.rm = TRUE))

abv_ibu_mean_grouped_by_city <- beer_breweries %>% 
  group_by(City) %>%
  select(City,ABV, IBU) %>%
  summarize_at(vars(ABV, IBU), mean, na.rm = TRUE)

for(i in 1:nrow(beer_breweries)) {
  if(is.na(beer_breweries[i,"ABV"])) {
    sub_abv = abv_ibu_mean_grouped_by_city %>%
      filter(City == beer_breweries[i,"City"])
    beer_breweries[i,"ABV"] <- sub_abv[1,"ABV"][1,1]
  }
  
  if(is.na(beer_breweries[i,"IBU"])) {
    sub_ibu = abv_ibu_mean_grouped_by_city %>% 
      filter(City == beer_breweries[i,"City"])
    beer_breweries[i,"IBU"] <- sub_ibu[1,"IBU"][1,1]
  }
}
plot_missing_values(beer_breweries)

beer_breweries %>% 
  summarize(IBU = mean(.data[["IBU"]], na.rm = TRUE), ABV  = mean(.data[["ABV"]], na.rm = TRUE))

abv_ibu_mean_grouped_by_state <- beer_breweries %>% 
  group_by(State) %>%
  select(State,ABV, IBU) %>%
  summarize_at(vars(ABV, IBU), mean, na.rm = TRUE)

for(i in 1:nrow(beer_breweries)) {
  if(is.na(beer_breweries[i,"ABV"])) {
    sub_abv = abv_ibu_mean_grouped_by_state %>%
      filter(State == beer_breweries[i,"State"])
    beer_breweries[i,"ABV"] <- sub_abv[1,"ABV"][1,1]
  }
  
  if(is.na(beer_breweries[i,"IBU"])) {
    sub_ibu = abv_ibu_mean_grouped_by_state %>% 
      filter(State == beer_breweries[i,"State"])
    beer_breweries[i,"IBU"] <- sub_ibu[1,"IBU"][1,1]
  }
}
beer_breweries %>% sapply(function(x) sum(is.na(x)))


plot_missing_values(beer_breweries)

# calc mean for IBU and ABV after the handling of missing values
beer_breweries %>% 
  summarize(IBU = mean(.data[["IBU"]], na.rm = TRUE), ABV  = mean(.data[["ABV"]], na.rm = TRUE))

# 4. Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare:

# calc median IBU & ABV by state
state_median_abv_ibu <- beer_breweries %>% 
  group_by(State) %>%
  select(State, ABV, IBU) %>%
  summarize_at(vars(ABV, IBU), median, na.rm = TRUE)

# create a bar plot for ABV median by state:
state_median_abv_ibu %>% 
  ggplot(mapping = aes(x = reorder(State, +ABV), y = ABV)) +
  geom_bar(stat = "identity") + 
  ggtitle("ABV Median by State") +
  xlab("State") +
  geom_text(mapping = aes(reorder(State, +ABV), y = ABV, 
                          label = paste0("             ", ABV), angle = 90),
            stat = 'identity', position = position_dodge(.9), size = 4, color = "red") +
  geom_hline(yintercept = median(state_median_abv_ibu$ABV, na.rm = TRUE), linetype = "dashed",
             color = "blue") +
  theme_economist()

# create a bar plot for IBU median by state:
state_median_abv_ibu %>% 
  ggplot(mapping = aes(x = reorder(State, +IBU), y = IBU)) +
  geom_bar(stat = "identity") + 
  ggtitle("IBU Median by State") +
  xlab("State") +
  geom_text(mapping = aes(reorder(State, +IBU), y = IBU, 
                          label = paste0("             ", round(IBU, 1)), angle = 90),
            stat = 'identity', position = position_dodge(.9), size = 4, color = "red") +
  geom_hline(yintercept = median(state_median_abv_ibu$IBU, na.rm = TRUE), linetype = "solid",
             color = "blue") +
  theme_economist()


#us heat map:
state_median_abv_ibu <- merge(state_median_abv_ibu,us_states, by.x = "State", by.y = "StateAbb", all.x = TRUE)
state_median_abv_ibu <- left_join(us_state_map_data, state_median_abv_ibu, by = c("region" = "StateName"))
merged_data <- left_join(state_median_abv_ibu, us_state_center_geo, by = c("region" = "region"))

merged_data %>%
  ggplot(mapping = aes(x = long, y = lat, group = group, fill = ABV)) +
  geom_polygon() +
  geom_path(color = "white", size = 1.5) +  # Add state borders
  geom_text(mapping = aes(CenterLong, CenterLatitude, label = round(ABV, 2)), color = "firebrick", size = 5) +
  coord_fixed(ratio=1.5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "ABV", 
                      guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  ggtitle("ABV Median by State") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_economist()

# 5. Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?

# calc max ABV by state
max_abv_by_state <- beer_breweries %>% 
  group_by(State) %>%
  summarise(max_ABV = max(ABV, na.rm = TRUE)) %>%
  select(State, max_ABV)

# create a a bar plot for max ABV value in each state
max_abv_by_state %>% 
  ggplot(mapping = aes(x = reorder(State, +max_ABV))) +
  geom_bar(mapping = aes(y = max_ABV), stat = "identity") + 
  ggtitle("Max ABV by State") +
  xlab("State") +
  ylab("MAX ABV") +
  geom_text(mapping = aes(reorder(State, +max_ABV), y = max_ABV, 
                          label = paste0("             ", round(max_ABV, 4)), angle = 90),
            stat = 'identity', size = 4, color = "red") +
  theme_economist()

# calc max ibu by state
max_ibu_by_state <- beer_breweries %>% 
  group_by(State) %>%
  summarise(max_IBU = max(IBU, na.rm = TRUE)) %>%
  select(State, max_IBU)

# create a a bar plot for max IBU value in each state
max_ibu_by_state %>% 
  ggplot(mapping = aes(x = reorder(State, +max_IBU))) +
  geom_bar(mapping = aes(y = max_IBU), stat = "identity") + 
  ggtitle("Max IBU by State") +
  xlab("State") +
  ylab("MAX IBU") +
  geom_text(mapping = aes(reorder(State, +max_IBU), y = max_IBU, 
                          label = paste0("             ", round(max_IBU, 4)), angle = 90),
            stat = 'identity', size = 4, color = "red") +
  theme_economist()

# 6. Comment on the summary statistics and distribution of the ABV variable.

# calc summary of ABV values
summary(beer_breweries$ABV)

# create a box blot to show the distribution of ABV with average value
beer_breweries %>%
  ggplot(mapping = aes(y = ABV)) +
  geom_boxplot() +
  geom_point(aes(x = 0, y = mean(beer_breweries$ABV)), color = "red", shape = 18, size = 4) +
  geom_text(aes(x = 0, y = mean(beer_breweries$ABV), label = round(mean(beer_breweries$ABV), 3)), vjust = -0.75, color = "red") +
  ggtitle("ABV Distribution") +
  xlab("ABV") +
  ylab("Count") +
  theme_economist()

# 7. Is there an apparent relationship between the bitterness of the beer and its alcoholic content? 
  # A. Draw a scatter plot.  
  # B. Make your best judgment of a relationship
  # C. EXPLAIN your answer.

# create visualizations for the relationship between ABV & IBU
beer_breweries %>%
  select(ABV, IBU) %>%
  ggplot(mapping = aes(x = ABV, y = IBU)) +
  geom_point(na.rm = TRUE) +
  geom_smooth() +
  ggtitle("Alcohol Content (ABV) vs Bitterness (IBU)") +
  theme_economist()

beer_breweries %>% 
  select(ABV, IBU) %>%
  ggpairs(columns = c("ABV", "IBU"))



# 8.	Budweiser would also like to investigate the difference with respect to IBU and ABV between IPAs (India Pale Ales)
  # and other types of Ale (any beer with “Ale” in its name other than IPA).  
  # use KNN classification to investigate this relationship.  
  # Provide statistical evidence one way or the other. You can of course assume your audience is comfortable with percentages
  # feel free to supplement your response to this question with any other methods or techniques you have learned.   

beer_breweries$Is_IPA = ifelse(str_detect(beer_breweries$Style, "IPA"), TRUE, FALSE)
beer_breweries$Is_ALE = ifelse(!str_detect(beer_breweries$Style, regex("IPA", ignore_case = TRUE)) & (str_detect(beer_breweries$Beer_Name, regex("Ale", ignore_case = TRUE)) |
                                                                                                        str_detect(beer_breweries$Style, regex("Ale", ignore_case = TRUE))) , TRUE, FALSE)

beer_breweries_with_beer_type <- beer_breweries %>%
    filter( !(Is_ALE == FALSE & Is_IPA == FALSE), !(Is_ALE == TRUE & Is_IPA == TRUE)) %>%
    filter(!is.na(IBU)) %>%
  mutate(Beer_TYPE = as.factor(ifelse(Is_ALE == TRUE, "ALE", "IPA")))

# calc mean & median:
beer_type_abv_ibu_summary <- beer_breweries_with_beer_type %>% 
  #filter(Beer_TYPE == "ALE") %>%
  group_by(Beer_TYPE) %>%
  summarize(ibu_mean = mean(IBU), ibu_median = median(IBU), abv_mean = mean(ABV), abv_median = median(ABV))
  
  
# box plot to compare IBU distribution in ALE & IPA
beer_breweries_with_beer_type %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = Beer_TYPE, y = IBU)) +
  geom_point(data = beer_type_abv_ibu_summary[beer_type_abv_ibu_summary$Beer_TYPE == "IPA",],
             mapping = aes(x = "IPA", y = round(ibu_mean, 2), color = "Mean"), shape = 16, size = 4) +
  geom_text(data = beer_type_abv_ibu_summary[beer_type_abv_ibu_summary$Beer_TYPE == "IPA",],
            mapping = aes(x = "IPA", y = ibu_mean, label = round(ibu_mean, 2), color = "Mean"), vjust = -1.0) +
  geom_point(data = beer_type_abv_ibu_summary[beer_type_abv_ibu_summary$Beer_TYPE == "ALE",],
             mapping = aes(x = "ALE", y = round(ibu_mean, 2), color = "Mean"), shape = 16, size = 4) +
  geom_text(data = beer_type_abv_ibu_summary[beer_type_abv_ibu_summary$Beer_TYPE == "ALE",],
            mapping = aes(x = "ALE", y = ibu_mean, label = round(ibu_mean, 2), color = "Mean"), vjust = -1.0) +
  scale_y_continuous(breaks = seq(0, 150, by = 20)) +
  scale_fill_manual(values = c("Mean" = "firebrick1")) +
  ggtitle("IBU Distribution By Beer Type") +
  xlab("Beer Type") +
  theme_economist()

beer_breweries_with_beer_type %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = Beer_TYPE, y = ABV)) +
  geom_point(data = beer_type_abv_ibu_summary[beer_type_abv_ibu_summary$Beer_TYPE == "IPA",],
             mapping = aes(x = "IPA", y = round(abv_mean, 2), color = "Mean"), shape = 16, size = 4) +
  geom_text(data = beer_type_abv_ibu_summary[beer_type_abv_ibu_summary$Beer_TYPE == "IPA",],
            mapping = aes(x = "IPA", y = abv_mean, label = round(abv_mean, 2), color = "Mean"), vjust = -2) +
  geom_point(data = beer_type_abv_ibu_summary[beer_type_abv_ibu_summary$Beer_TYPE == "ALE",],
             mapping = aes(x = "ALE", y = round(abv_mean, 2), color = "Mean"), shape = 16, size = 4) +
  geom_text(data = beer_type_abv_ibu_summary[beer_type_abv_ibu_summary$Beer_TYPE == "ALE",],
            mapping = aes(x = "ALE", y = abv_mean, label = round(abv_mean, 2), color = "Mean"), vjust = -0.1) +
  scale_y_continuous(breaks = seq(0, 0.2, by = 0.01)) +
  scale_fill_manual(values = c("Mean" = "firebrick1")) +
  ggtitle("ABV Distribution By Beer Type") +
  xlab("Beer Type") +
  theme_economist()


beer_breweries_with_beer_type %>%
  ggplot(mapping = aes(x = IBU, y = ABV, color = Beer_TYPE)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 0.2, by = 0.01)) +
  scale_x_continuous(breaks = seq(0, 180, by = 20)) +
  ggtitle("ABV & IBU Distribution By Beer Type") +
  xlab("IBU") +
  theme_economist()

beer_breweries_with_beer_type %>% 
  select(ABV, IBU, Beer_TYPE) %>%
  ggpairs(columns = c("ABV", "Beer_TYPE"))

#KNN:
set.seed(4)
split_perc <- 0.70 
n <- dim(beer_breweries_with_beer_type)[1]
k_iterations <- 100
iterations_for_each_k <- 20

indices <- sample(1:n, size = round(split_perc * n))

beer_training_set <- beer_breweries_with_beer_type[indices,]
beer_testing_set <- beer_breweries_with_beer_type[-indices,]

# place holder for k & accuracy
accs <- data.frame(K = numeric(k_iterations), 
                   Accuracy = numeric(k_iterations),
                   Sensitiviy = numeric(k_iterations),
                   Specificity = numeric(k_iterations))

# iterate over different k values
for(i in 1:k_iterations) {
  accuracy <- 0
  sensitiviy <- 0
  specificity <- 0
  
  # loop through iterations with different and random training and testing sets.
  for(j in 1:iterations_for_each_k) {
    # randomly generate indices, by not setting the seed each iteration 
    # should be unique; hence, real cross validation
    indices <- sample(1:n, size = round(split_perc * n))
    beer_training_set <- beer_breweries_with_beer_type[indices,]
    beer_testing_set <- beer_breweries_with_beer_type[-indices,]
    # use knn, formula: Beer Type ~ ABV + IBU
    model <- knn(train = beer_training_set[, c("ABV", "IBU")], test = beer_testing_set[, c("ABV", "IBU")],
                 cl = beer_training_set$Beer_TYPE, k = i, prob = FALSE)
    # create the confusion matrix
    cm <- confusionMatrix(data = model, reference = beer_testing_set$Beer_TYPE)
    
    accuracy <- accuracy + cm$overall["Accuracy"]
    sensitiviy <- sensitiviy + cm$byClass["Sensitivity"]
    specificity <- specificity + cm$byClass["Specificity"]
  }
  
  # calculate average accuracy for the k valueL
  accs$K[i] <- i
  accs$Accuracy[i] <- round(accuracy / 20, 3)
  accs$Sensitiviy[i] <- round(sensitiviy / 20, 2)
  accs$Specificity[i] <- round(specificity / 20, 2)
}

accs %>% 
  ggplot(mapping = aes(x = accs$K)) +
  geom_line(y = accs$Accuracy, color = "firebrick") +
  geom_line(y = accs$Sensitiviy, color = "forestgreen") +
  geom_line(y = accs$Specificity, color = "blue")

# get the best accuracy row
best_accuracy <- filter(accs, accs$Accuracy == max(accs$Accuracy))

# plot the results and mark the best accuracy:
accs %>%
  ggplot(mapping = aes(x = K, y = Accuracy, type = "l", color = "Accuracy")) +
  geom_line() +
  geom_point(mapping = aes(x = best_accuracy$K, y = best_accuracy$Accuracy), color = "firebrick", show.legend = FALSE, size = 4, shape = 16) +
  scale_fill_manual(values = c("Accuracy" = "forestgreen")) +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) +
  ggtitle("Beer Type KNN Model - Accuracy vs K Value") +
  theme_economist()

# formula: Beer Type ~ ABV + IBU with K = 85
indices <- sample(1:n, size = round(split_perc * n))
beer_training_set <- beer_breweries_with_beer_type[indices,]
beer_testing_set <- beer_breweries_with_beer_type[-indices,]
model <- knn(train = beer_training_set[, c("ABV", "IBU")], test = beer_testing_set[, c("ABV", "IBU")],
             cl = beer_training_set$Beer_TYPE, k = 85)
# create a confusion matrix:
cm <- confusionMatrix(data = model, reference = beer_testing_set$Beer_TYPE)


beers_data_cleaned %>%
  filter("Type" == "ALE")
factor(beers_data_cleaned$Type)
view(beers_data_cleaned)

# 9. Knock their socks off!  Find one other useful inference from the data that you feel Budweiser may be able to find value in.  
  # You must convince them why it is important and back up your conviction with appropriate statistical evidence. 


plot_missing_values <- function(df) {
  # get the names of columns with missing values
  columns_with_missing_values <- names(which(sapply(df, function(x) sum(is.na(x)) > 0)))
  
  # calc missing values count for each column
  missing_values_count <- df %>%
    sapply(function(x) sum(is.na(x)))
  
  # create dataframe for columns with total number of mising values
  missing_values_df <- data.frame(
    column_name = columns_with_missing_values,
    missing_values_total = as.vector(sapply(columns_with_missing_values, 
                                            function(x) {
                                              if(is.na(missing_values_count[x])) 0 else missing_values_count[x]
                                                 })))
  
  missing_values_df %>%
    ggplot(mapping = aes(x = column_name, y = missing_values_total)) +
    geom_bar(stat = "identity") + 
    ggtitle("Missing values Count by Column") +
    xlab("Missing Stat Column Name") +
    ylab("Count") +
    geom_text(mapping = aes(x = column_name, y = missing_values_total, 
                            label = missing_values_total),
              stat = 'identity', position = position_dodge(.9), vjust = -0.5, hjust = 2, size = 5) +
    geom_text(mapping = aes(x = column_name, y = missing_values_total, 
                            label = paste0(" (",scales::percent(missing_values_total/dim(beer_breweries)[1]), ")")),
              stat = 'identity', position = position_dodge(.9), vjust = -0.5, size = 5) +
    theme_economist()
}