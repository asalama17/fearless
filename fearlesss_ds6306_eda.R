
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
###

# set working directory:
setwd("./data")

# load breweries data:
breweries_data_raw <- read.csv("Breweries.csv", header = TRUE, stringsAsFactors = FALSE)
# trim character values in the data
breweries_data_cleaned <- breweries_data_raw %>% 
  sapply(function(x) if(is.character(x)) str_squish(x) else x)
breweries_data_cleaned <- as.data.frame(breweries_data_cleaned)
breweries_data_cleaned$Brew_ID <- as.integer(breweries_data_cleaned$Brew_ID)

summary(breweries_data_cleaned)
head(breweries_data_cleaned)

beers_data_raw <-  as.data.frame(read.csv("Beers.csv", header = TRUE))
beers_data_cleaned <- beers_data_raw 
# change ABV from character to numeric values
beers_data_cleaned$ABV = as.numeric(beers_data_cleaned$ABV)

breweries_data_cleaned$State <- as.factor(str_squish(breweries_data_cleaned$State))

us_states <- data.frame(
  StateAbb = state.abb,
  StateName = state.name
)



# 1. How many breweries are present in each state?

# check if there is any missing data:
breweries_missing_data_df <- breweries_data_cleaned %>% sapply(function(x) sum(is.na(x))) #no missing data.

breweries_state_count <- breweries_data_cleaned %>% 
  group_by(State) %>%
  mutate(Count = n()) %>%
  select(State, Count)

breweries_data_cleaned %>%
  ggplot(mapping = aes(x = reorder(State, breweries_state_count$Count))) +
  geom_bar(stat = "count") + 
  ggtitle("Brewery Count by US State") +
  xlab("State") +
  ylab("Count") +
  stat_count(geom = "text", colour = "white", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.75)) +
  theme_economist()

us_states <- map_data("state")
snames <- data.frame(region=tolower(state.name), CenterLong=state.center$x, CenterLatitude=state.center$y)
merged_data <- merge(breweries_state_count,snames, by.x = "StateFullName", by.y = "region", all.x = TRUE)

merged_data %>%
ggplot(mapping = aes(x = long, y = lat, group = group, fill = Count)) +
  geom_polygon() +
  geom_path(color = "white", size = 1.5) +  # Add state borders
  geom_text(aes(CenterLong, CenterLatitude, label = Count), color = "white") +
  coord_fixed(ratio=1.5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", 
                      name = "Brewery Count") +
  ggtitle("Brewery Count by US State") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_economist()





map.df <- map.df[order(map.df$order),]

breweries_data_cleaned %>% 
  group_by(State) %>%
  mutate(Count = n()) %>%
  select(State, Count) %>%
  ggplot(mapping = aes(x = State, y = Count)) +
  geom_polygon(aes(fill=Count))+
  geom_path()+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()
# 2.	Merge beer data with the breweries data. Print the first 6 observations
breweries_beer <- breweries_data_cleaned %>% 
  inner_join(beers_data_cleaned, by = join_by(Brew_ID == Brewery_id))
head(breweries_beer)

# 3.	Address the missing values in each column:
# maybe assign the average value per city\state for missing values?

breweries_beer %>% sapply(function(x) sum(is.na(x))) 
summary(breweries_beer)

missing <- breweries_beer %>% 
  group_by(City) %>%
  summarise(sum_na = sum(is.na(IBU)), total = n())

# get the names of columns with missing values
columns_with_missing_values <- names(which(sapply(breweries_beer, function(x) sum(is.na(x)) > 0)))

# calc missing values count for each column
missing_values_count <- breweries_beer %>%
  sapply(function(x) sum(is.na(x)))

# create dataframe for columns with total number of mising values
missing_values_df <- data.frame(
  column_name = columns_with_missing_values,
  missing_values_total = as.vector(sapply(columns_with_missing_values, function(x) missing_values_count[x]))
)

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
                          label = paste0(" (",scales::percent(missing_values_total/dim(breweries_beer)[1]), ")")),
    stat = 'identity', position = position_dodge(.9), vjust = -0.5, size = 5) +
  theme_economist()

# calc mean of ABV & IBU by city, state, and style:
abv_ibu_mean_grouped_by_style_city <- breweries_beer %>% 
  group_by(Style, City) %>%
  summarize(IBU = mean(.data[["IBU"]], na.rm = TRUE), ABV  = mean(.data[["ABV"]], na.rm = TRUE))

abv_ibu_mean_grouped_by_city <- breweries_beer %>% 
  group_by(City) %>%
  select(City,ABV, IBU) %>%
  summarize_at(vars(ABV, IBU), mean, na.rm = TRUE)

abv_ibu_mean_grouped_by_style <- breweries_beer %>% 
  group_by(Style) %>%
  select(Style,ABV, IBU) %>%
  summarize_at(vars(ABV, IBU), mean, na.rm = TRUE)

missing_values_count <- abv_ibu_mean_grouped_by_style_city %>%
  sapply(function(x) sum(is.na(x)))

missing_values_count <- abv_ibu_mean_grouped_by_style %>%
  sapply(function(x) sum(is.na(x)))

# is there any more missing values:
abv_ibu_mean_grouped_by_city %>% 
  sapply(function(x) sum(is.na(x)))


for(i in 1:nrow(breweries_beer)) {
  print(i)
  row = breweries_beer[i,]
  if(is.na(breweries_beer[i,"ABV"])) {
    sub_abv = abv_ibu_mean_grouped_by_style_city %>%
      #filter(Style == row["Style"], City == row["City"], State == row["State"])
      filter(City == row["City"])
    breweries_beer[i,"ABV"] <- sub_abv[1,"ABV"]
  }
  
  if(is.na(breweries_beer[i,"IBU"])) {
    sub_ibu = abv_ibu_mean_grouped_by_style_city %>% 
      #filter(Style == row["Style"], City == row["City"], State == row["State"]) 
      filter(City == row["City"])
    breweries_beer[i,"IBU"] <- sub_ibu[1,"IBU"]
    if(nrow(sub_ibu) > 0 & !is.na(sub_ibu[1,"IBU"])) {
     
    }
  }
}

for(i in 1:nrow(breweries_beer)) {
  print(i)
  row = breweries_beer[i,]
  if(is.na(breweries_beer[i,"ABV"])) {
    sub_abv = abv_ibu_mean_grouped_by_style %>%
      #filter(Style == row["Style"], City == row["City"], State == row["State"])
      filter(Style == row["Style"])
    breweries_beer[i,"ABV"] <- sub_abv[1,"ABV"]
    if(nrow(sub_abv) > 0 & !is.na(sub_abv[1,"ABV"])) {
      
    }
  }
  
  if(is.na(breweries_beer[i,"IBU"])) {
    sub_ibu = abv_ibu_mean_grouped_by_style %>% 
      #filter(Style == row["Style"], City == row["City"], State == row["State"]) 
      filter(Style == row["Style"])
    breweries_beer[i,"IBU"] <- sub_ibu[1,"IBU"]
    if(nrow(sub_ibu) > 0 & !is.na(sub_ibu[1,"IBU"])) {
      
    }
  }
}

for(i in 1:nrow(breweries_beer)) {
  print(i)
  row = breweries_beer[i,]
  if(is.na(breweries_beer[i,"ABV"])) {
    sub_abv = abv_ibu_mean_grouped_by_city %>%
      #filter(Style == row["Style"], City == row["City"], State == row["State"])
      filter(City == row["City"])
    breweries_beer[i,"ABV"] <- sub_abv[1,"ABV"]
    if(nrow(sub_abv) > 0 & !is.na(sub_abv[1,"ABV"])) {
      
    }
  }
  
  if(is.na(breweries_beer[i,"IBU"])) {
    sub_ibu = abv_ibu_mean_grouped_by_city %>% 
      #filter(Style == row["Style"], City == row["City"], State == row["State"]) 
      filter(City == row["City"])
    breweries_beer[i,"IBU"] <- sub_ibu[1,"IBU"]
    if(nrow(sub_ibu) > 0 & !is.na(sub_ibu[1,"IBU"])) {
      
    }
  }
}

# get the names of columns with missing values
columns_with_missing_values <- names(which(sapply(breweries_beer, function(x) sum(is.na(x)) > 0)))

# calc missing values count for each column
missing_values_count <- breweries_beer %>%
  sapply(function(x) sum(is.na(x)))

# create dataframe for columns with total number of mising values
missing_values_df <- data.frame(
  column_name = columns_with_missing_values,
  missing_values_total = as.vector(sapply(columns_with_missing_values, function(x) missing_values_count[x]))
)

missing_values_df %>%
  ggplot(mapping = aes(x = column_name, y = missing_values_total)) +
  geom_bar(stat = "identity") + 
  ggtitle("Missing values Count by Column") +
  xlab("Missing Stat Column Name") +
  ylab("Count") +
  theme_economist()


# 4. Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare:

# state_mean_abv_ibu <- breweries_beer %>% group_by(State) %>%
#   select(ABV, IBU) %>%
#   summarize_at(vars(ABV, IBU), mean, na.rm = TRUE)
# 
# state_mean_abv <- breweries_beer %>% group_by(State) %>%
#   select(ABV) %>%
#   summarize_at(vars(ABV), mean, na.rm = TRUE) %>%
#   mutate(Type = "ABV")
# state_mean_abv <- state_mean_abv %>% rename_at("ABV", ~'Mean')
# 
# state_mean_ibu <- breweries_beer %>% group_by(State) %>%
#   select(IBU) %>%
#   summarize_at(vars(IBU), mean, na.rm = TRUE) %>%
#   mutate(Type = "IBU")
# state_mean_ibu <- state_mean_ibu %>% rename_at("IBU", ~'Mean')
# 
# state_mean_abv_ibu <- rbind(state_mean_abv, state_mean_ibu)

###MEDIAN ALCOHOL
state_median_abv_ibu <- breweries_beer %>% group_by(State) %>%
  select(State, ABV, IBU) %>%
  summarize_at(vars(ABV, IBU), median, na.rm = TRUE)

# state_median_abv <- breweries_beer %>% group_by(State) %>%
#   select(ABV) %>%
#   summarize_at(vars(ABV), median, na.rm = TRUE) %>%
#   mutate(Type = "ABV")
# state_median_abv <- state_median_abv %>% rename_at("ABV", ~'Median')
# 
# state_median_ibu <- breweries_beer %>% group_by(State) %>%
#   select(IBU) %>%
#   summarize_at(vars(IBU), median, na.rm = TRUE) %>%
#   mutate(Type = "IBU")
# state_median_ibu <- state_median_ibu %>% rename_at("IBU", ~'Median')
# 
# state_median_abv_ibu <- rbind(state_median_abv, state_median_ibu)

# bar plot
state_median_abv_ibu %>% 
  ggplot(mapping = aes(x = reorder(State, +ABV), y = ABV)) +
  geom_bar(stat = "identity") + 
  ggtitle("ABV Median by State") +
  xlab("State") +
  theme_economist()

state_median_abv_ibu %>% 
  ggplot(mapping = aes(x = reorder(State, +IBU), y = IBU)) +
  geom_bar(stat = "identity") + 
  ggtitle("IBU Median by State") +
  xlab("State") +
  theme_economist()


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

max_abv_state <- max(state_median_abv_ibu$ABV,na.rm = TRUE)
state_median_abv_ibu %>% filter(ABV == max_abv_state)

max_ibu_state <- max(state_median_abv_ibu$IBU,na.rm = TRUE)
state_median_abv_ibu %>% filter(IBU == max_ibu_state)

# 6. Comment on the summary statistics and distribution of the ABV variable.

summary(breweries_beer$ABV)

breweries_beer %>%
  ggplot(mapping = aes(x = ABV)) +
  geom_line(stat = "count") +
  xlab("ABV") +
  ylab("Count") +
  ggtitle("ABV Distribution") +
  theme_economist()

breweries_beer %>%
  ggplot(mapping = aes(y = ABV)) +
  geom_boxplot() +
  ggtitle("ABV Distribution") +
  theme_economist()

# 7. Is there an apparent relationship between the bitterness of the beer and its alcoholic content? 
  # A. Draw a scatter plot.  
  # B. Make your best judgment of a relationship
  # C. EXPLAIN your answer.
breweries_beer %>%
  select(ABV, IBU) %>%
  ggplot(mapping = aes(x = ABV, y = IBU)) +
  geom_point(na.rm = TRUE) +
  geom_smooth() +
  ggtitle("Alcohol Content (ABV) vs Bitterness (IBU)") +
  theme_economist()

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
