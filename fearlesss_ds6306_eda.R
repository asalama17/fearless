
library(tidyverse)
library(dplyr)
library(GGally)

breweries_data_raw <- read.csv("Breweries.csv", header = TRUE)
breweries_data_cleaned <- breweries_data_raw

beers_data_raw <- read.csv("Beers.csv", header = TRUE)
beers_data_cleaned <- beers_data_raw

# 1. How many breweries are present in each state?
breweries_data_cleaned %>% 
  group_by(State) %>%
  mutate(Count = n()) %>%
  select(State, Count) %>%
  ggplot(mapping = aes(x = State, y = Count)) +
  geom_bar(stat = "identity") + ggtitle("Number of breweries in each state")

# 2.	Merge beer data with the breweries data. Print the first 6 observations
breweries_beer <- breweries_data_cleaned %>% 
  inner_join(beers_data_cleaned, by = join_by(Brew_ID == Brewery_id))
head(breweries_beer)

# 3.	Address the missing values in each column:
# maybe assign the average value per city\state for missing values?

breweries_beer %>% sapply(function(x) sum(is.na(x))) 
# %>%
#   select_if(is.na(x) == TRUE)

names(which(colSums(is.na(breweries_beer)) > 0))

columns_with_missing_values <- names(which(sapply(breweries_beer, function(x) sum(is.na(x)) > 0)))
missing_values_count <- breweries_beer %>%
  sapply(function(x) sum(is.na(x)))

missing_values_df <- data.frame(
  column_name = columns_name_with_missing_values,
  missing_values_count = as.vector(sapply(columns_name_with_missing_values, function(x) missing_values_count[x]))
)

missing_values_df %>%
  ggplot(mapping = aes(x = column_name, y = missing_values_count)) +
  geom_bar(stat = "identity")

# TODO: fix missing values by mean of city & style:
breweries_beer %>% group_by(City,State,Style) %>%
  select(ABV, IBU) %>%
  summarize_at(vars(ABV, IBU), mean, na.rm = TRUE)

breweries_beer %>% group_by(State,City,Style) %>%
  select(ABV, IBU) %>%
  summarize_at(vars(ABV, IBU), mean, na.rm = TRUE)

# 4. Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare:

state_mean_abv_ibu <- breweries_beer %>% group_by(State) %>%
  select(ABV, IBU) %>%
  summarize_at(vars(ABV, IBU), mean, na.rm = TRUE)

state_mean_abv <- breweries_beer %>% group_by(State) %>%
  select(ABV) %>%
  summarize_at(vars(ABV), mean, na.rm = TRUE) %>%
  mutate(Type = "ABV")
state_mean_abv <- state_mean_abv %>% rename_at("ABV", ~'Mean')

state_mean_ibu <- breweries_beer %>% group_by(State) %>%
  select(IBU) %>%
  summarize_at(vars(IBU), mean, na.rm = TRUE) %>%
  mutate(Type = "IBU")
state_mean_ibu <- state_mean_ibu %>% rename_at("IBU", ~'Mean')

state_mean_abv_ibu <- rbind(state_mean_abv, state_mean_ibu)


# 5. Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?
state_mean_abv_ibu %>% 
  ggplot(mapping = aes(x = State, y = Mean, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", order(decreasing = TRUE))

state_mean_ibu[order(state_mean_ibu$Mean, decreasing = TRUE),] %>%
  ggplot(mapping = aes(x = reorder(State, +Mean), y = Mean, fill = Type), ) +
  geom_bar(stat = "identity", position = "dodge", na.rm = TRUE)

state_mean_abv[order(state_mean_abv$Mean, decreasing = TRUE),] %>%
  ggplot(mapping = aes(x = reorder(State, +Mean), y = Mean, fill = Type), ) +
  geom_bar(stat = "identity", position = "dodge", na.rm = TRUE)

# 6. Comment on the summary statistics and distribution of the ABV variable.

# 7. Is there an apparent relationship between the bitterness of the beer and its alcoholic content? 
  # A. Draw a scatter plot.  
  # B. Make your best judgment of a relationship
  # C. EXPLAIN your answer.
breweries_beer %>%
  select(ABV, IBU) %>%
  ggplot(mapping = aes(x = ABV, y = IBU)) +
  geom_point(na.rm = TRUE) +
  geom_smooth()

breweries_beer %>% 
  select(ABV, IBU) %>%
  ggpairs(columns = c("ABV", "IBU"))



# 8.	Budweiser would also like to investigate the difference with respect to IBU and ABV between IPAs (India Pale Ales)
  # and other types of Ale (any beer with “Ale” in its name other than IPA).  
  # use KNN classification to investigate this relationship.  
  # Provide statistical evidence one way or the other. You can of course assume your audience is comfortable with percentages
  # feel free to supplement your response to this question with any other methods or techniques you have learned.   

beers_data_cleaned %>%
  mutate(Type = )
beers_data_cleaned$Is_ALE = as.factor(ifelse(!str_detect(beers_data_cleaned$Style, "IPA") & str_detect(beers_data_cleaned$Name, "Ale"), TRUE, FALSE))
beers_data_cleaned$Is_IPA = as.factor(ifelse(str_detect(beers_data_cleaned$Style, "IPA"), TRUE, FALSE))
beers_data_cleaned %>%
  filter("Type" == "ALE")
factor(beers_data_cleaned$Type)
view(beers_data_cleaned)

# 9. Knock their socks off!  Find one other useful inference from the data that you feel Budweiser may be able to find value in.  
  # You must convince them why it is important and back up your conviction with appropriate statistical evidence. 
