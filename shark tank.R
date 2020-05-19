library(tidyverse)
library(ggplot2)
library(expss)
library(formattable)

#rename the variables so they're more comprehensible
names(shark) <- c("Row", "Season", "Number", "Company", "Deal", "Industry", "Gender", "Amount", "Equity", "Valuation", "Corcoran", 
                   "Cuban", "Greiner", "Herjavec", "John", "OLeary", "Harrington", "Guest", "numSharks", "perShark", "Details")

#Section 0: Cleaning -- 
#Turn deals, and deals with specific sharks, to logicals
one <- shark %>% mutate(DealBool = ifelse(Deal == "Yes", TRUE, FALSE)) %>% mutate(CorcoranDeal = ifelse(is.na(Corcoran), FALSE, TRUE)) %>% 
  mutate(CubannDeal = ifelse(is.na(Cuban), FALSE, TRUE)) %>% mutate(GrenierDeal = ifelse(is.na(Greiner), FALSE, TRUE)) %>% 
  mutate(HerjavecDeal = ifelse(is.na(Herjavec), FALSE, TRUE)) %>% mutate(JohnDeal = ifelse(is.na(John), FALSE, TRUE)) %>% 
  mutate(OLearyDeal = ifelse(is.na(OLeary), FALSE, TRUE)) %>% mutate(HarringtonDeal = ifelse(is.na(Harrington), FALSE, TRUE)) %>%
  mutate(GuestDeal = ifelse(is.na(Guest), FALSE, TRUE))
#view(one)

#Turn seasons, gender, and Industry into factors
two <- one %>% mutate(sex = as.factor(Gender)) %>% mutate(seasons = as.factor(Season)) %>% mutate(Industries = as.factor(Industry))
#view(two)

#Turn valuation, amount, perShark, and equity into numericals
three <- two %>% mutate(amount = parse_number(Amount)) %>% mutate(valuation = parse_number(Valuation)) %>% 
  mutate(percent = parse_number(Equity)) %>% mutate(eachShark = parse_number(perShark)) %>% mutate(numShark = numSharks)
#view(three)

#pull out and make our new dataset pretty
data <- three[, c(1, 32, 3, 4, 22, 33, 31, 34, 36, 35, 23, 24, 25, 26, 27, 28, 29, 30, 38, 37, 21)]
names(data) <- c("Row", "Season", "Number", "Company", "Deal", "Industry", "Gender", "Amount", "Equity", "Valuation", "Corcoran", 
                 "Cuban", "Greiner", "Herjavec", "John", "OLeary", "Harrington", "Guest", "numSharks", "perShark", "Details")
data <- data %>% filter(!is.na(Season)) %>% mutate(Row = Row-1)
#view(data)

#some feature specific datasets: Deals by shark
barbaraDeals <- data %>% filter(Corcoran)
markDeals <- data %>% filter(Cuban)
loriDeals <- data %>% filter(Greiner)
robertDeals <- data %>% filter(Herjavec)
daymondDeals <- data %>% filter(John)
kevinDeals <- data %>% filter(OLeary)
multiSharkDeals <- data %>% filter(numSharks > 1)

deal <- data %>% filter(!is.na(Amount))
noDeal <- data %>% filter(is.na(Amount))

#Section 1: General Trends -- 

#Find the gender distributions by deals
genderByDeal <- data %>% filter(!is.na(Gender)) %>% mutate(DealFact = ifelse(Deal, "Yes", "No")) %>% 
  mutate(DealFact = as.factor(DealFact)) %>% ggplot(aes(x = DealFact, fill = Gender)) + 
  geom_bar(position = "dodge", stat = 'count', aes(label = ..count..)) + 
  labs(x = "Got a Deal?", y = "Number", title = "Gender Distribution: No Deals Vs. Deals")
#genderByDeal
#observations:
      #men enter the tank at twice the rate of women, 3 times the rate of mixed teams
      #deal closings are more or less proporitional to people who enter the tank, slight female bias

#the most common industry is:
frequencyIndustry <- data %>% filter(!is.na(Industry)) %>% ggplot(aes(x = Industry)) + geom_bar() + coord_flip()
#frequencyIndustry - the most frequent are Food and beverage, least frequent are business services, media, green
#deals by industry
dealByIndustry <- data %>% filter(!is.na(Industry) & !is.na(Deal)) %>% mutate(DealFact = ifelse(Deal, "Yes", "No")) %>% 
  mutate(DealFact = as.factor(DealFact)) %>% ggplot(aes(x = Industry, fill = DealFact)) + 
  geom_bar(position = "fill") + coord_flip() +
  labs(x = "Industry", y = "Proportion", title = "Deal Distribution by Industry")
#dealByIndustry
#observations:
      #business services have the worst deal rate, at around 25% closure
      #Fitness and Media/entertainment have the best deal rates, at just above 50%
      #most industries hover between a 40 and 50% deal closure rate

#boxplot of deal amounts - median at a little over 125,000
amountBox <- deal %>% filter(Amount <= 1000000) %>% ggplot(aes(x = "", y = Amount)) + geom_boxplot() + 
  labs(x = "", y = "Deal Amount", title = "Distribution of Deal Amount")
#amountBox
#boxplot of deal percentages - median is around 33.3%
equityBox <- deal %>% ggplot(aes(x = "", y = Equity)) + geom_boxplot() + 
  labs(x = "", y = "Equity Percent", title = "Distribution of Deal Equity")
#equityBox
#boxplot of valuations (zoom in to 2 million cap) - Median is around 375,000 valuations
valBox <- deal %>% filter(Valuation <= 2000000) %>% ggplot(aes(x = "", y = Valuation)) + geom_boxplot() + 
  labs(x = "", y = "Deal Valuation", title = "Distribution of Deal Valuations")
#valBox

#are there a change in trends among the multi-shark-deals? Yes, Food and beverage tie with Fashion for multi shark
multiIndustry <- multiSharkDeals %>% filter(!is.na(Industry)) %>% ggplot(aes(x = Industry)) + geom_bar() + coord_flip()
#multiIndustry
#gender by multi shark shows no change
#amount by multi shark shows no change
#equity by multi shark shows no change
#Are there a change in trends among multi shark deals? Yes, valuation have a 500,000 median
multiVal <- multiSharkDeals %>% filter(Valuation <= 2000000) %>% ggplot(aes(x = "", y = Valuation)) + geom_boxplot() + 
  labs(x = "", y = "Deal Valuation", title = "Distribution of Deal Valuations")
#multiVal

#table of medians and means - by industry, gender
modDeal <- deal %>% filter(!is.na(Equity) & !is.na(Amount) & !is.na(Valuation) & !is.na(Gender) & !is.na(Industry)) %>% 
            select(Industry, Amount, Equity, Valuation)
indusTable <- modDeal%>% select(Amount, Equity, Valuation) %>% aggregate(by = list(modDeal$Industry), FUN = mean) %>% 
  mutate(Amount = accounting(Amount)) %>% mutate(Equity = percent(Equity*.01)) %>% mutate(Valuation = accounting(Valuation)) 
indusTableNice <- formattable(indusTable, align = c("l", "r", "r", "r")) %>% setNames(c("Industry", "Mean Amount Taken", "Mean Equity Given", "Mean Ending Valuation"))
#indusTableNice

#Section 2: Trends by Shark -- 
#barbara deals industry
barbIndustries<- barbaraDeals %>% filter(!is.na(Industry)) %>% ggplot(aes(x = "", fill = Industry)) +
  geom_bar(position = "fill") + labs(x = "Barbara's Industries", y = "Proportion", title = "Barbara Corcoran: Investment by Industry")
#barbIndustries #Barbara invests mostly in food, with the least money in consumer products, software, and pet.
#find Barbara's average stats...
barbPlots <- barbaraDeals %>% filter(!is.na(Equity) & !is.na(Amount) & !is.na(Valuation)) %>% select(Amount, Equity, Valuation) %>% 
      mutate(Amount = accounting(Amount)) %>% mutate(Equity = percent(Equity*.01)) %>% mutate(Valuation = accounting(Valuation)) %>% 
      summarize(Companies = n(), MeanAmount = mean(Amount), MeanEquity = mean(Equity), meanValuation = mean(Valuation)) %>%
      formattable(., align = c("l", "r", "r", "r")) %>% setNames(c("Barbara's Companies", "Mean Amount Taken", "Mean Equity Given", "Mean Ending Valuation"))
#barbPlots

#lori deals by industry
loriIndustries<- loriDeals %>% filter(!is.na(Industry)) %>% ggplot(aes(x = "", fill = Industry)) +
  geom_bar(position = "fill") + labs(x = "Lori's Industries", y = "Proportion", title = "Lori Greiner: Investment by Industry")
#loriIndustries #Lori mainly invests in Lifestyle/home goods, with an aversion to media, green, and healthcare.
#find Lori's average stats...
loriPlots <- loriDeals %>% filter(!is.na(Equity) & !is.na(Amount) & !is.na(Valuation)) %>% select(Amount, Equity, Valuation) %>% 
  mutate(Amount = accounting(Amount)) %>% mutate(Equity = percent(Equity*.01)) %>% mutate(Valuation = accounting(Valuation)) %>% 
  summarize(Companies = n(), MeanAmount = mean(Amount), MeanEquity = mean(Equity), meanValuation = mean(Valuation)) %>%
  formattable(., align = c("l", "r", "r", "r")) %>% setNames(c("Lori's Companies", "Mean Amount Taken", "Mean Equity Given", "Mean Ending Valuation"))
#loriPlots

#Mark industry spread
markIndustries<- markDeals %>% filter(!is.na(Industry)) %>%ggplot(aes(x = "", fill = Industry)) +
  geom_bar(position = "fill") + labs(x = "Mark's Industries", y = "Proportion", title = "Mark Cuban: Investment by Industry")
#markIndustries #mostly invests in food, tech, and fashion, least likely to invest in children and pet products
#find Mark's average stats...
markPlots <- markDeals %>% filter(!is.na(Equity) & !is.na(Amount) & !is.na(Valuation)) %>% select(Amount, Equity, Valuation) %>% 
  mutate(Amount = accounting(Amount)) %>% mutate(Equity = percent(Equity*.01)) %>% mutate(Valuation = accounting(Valuation)) %>% 
  summarize(Companies = n(), MeanAmount = mean(Amount), MeanEquity = mean(Equity), meanValuation = mean(Valuation)) %>%
  formattable(., align = c("l", "r", "r", "r")) %>% setNames(c("Mark's Companies", "Mean Amount Taken", "Mean Equity Given", "Mean Ending Valuation"))
#markPlots

#daymond's industry spread 
daymondIndustries<- daymondDeals %>% filter(!is.na(Industry)) %>%ggplot(aes(x = "", fill = Industry)) +
  geom_bar(position = "fill") + labs(x = "Daymond's Industries", y = "Proportion", title = "Daymond John: Investment by Industry")
#daymondIndustries #least likely to invest in media, home products, tech. Most likely to invest in fashion
#find daymond's average stats...
daymondPlots <- daymondDeals %>% filter(!is.na(Equity) & !is.na(Amount) & !is.na(Valuation)) %>% select(Amount, Equity, Valuation) %>% 
  mutate(Amount = accounting(Amount)) %>% mutate(Equity = percent(Equity*.01)) %>% mutate(Valuation = accounting(Valuation)) %>% 
  summarize(Companies = n(), MeanAmount = mean(Amount), MeanEquity = mean(Equity), meanValuation = mean(Valuation)) %>%
  formattable(., align = c("l", "r", "r", "r")) %>% setNames(c("Daymond's Companies", "Mean Amount Taken", "Mean Equity Given", "Mean Ending Valuation"))
#daymondPlots

#robert's industry spread
robertIndustries<- robertDeals %>% filter(!is.na(Industry)) %>%ggplot(aes(x = "", fill = Industry)) +
  geom_bar(position = "fill") + labs(x = "Robert's Industries", y = "Proportion", title = "Robert Herjavec: Investment by Industry")
#robertIndustries #most likely to invest in lifestyle/home, fitness, fashion; least in media, consumer products
#find robert's average stats...
robertPlots <- robertDeals %>% filter(!is.na(Equity) & !is.na(Amount) & !is.na(Valuation)) %>% select(Amount, Equity, Valuation) %>% 
  mutate(Amount = accounting(Amount)) %>% mutate(Equity = percent(Equity*.01)) %>% mutate(Valuation = accounting(Valuation)) %>% 
  summarize(Companies = n(), MeanAmount = mean(Amount), MeanEquity = mean(Equity), meanValuation = mean(Valuation)) %>%
  formattable(., align = c("l", "r", "r", "r")) %>% setNames(c("Robert's Companies", "Mean Amount Taken", "Mean Equity Given", "Mean Ending Valuation"))
#robertPlots

#kevin's industry spread
kevinIndustries<- kevinDeals %>% filter(!is.na(Industry)) %>%ggplot(aes(x = "", fill = Industry)) +
  geom_bar(position = "fill") + labs(x = "Kevin's Industries", y = "Proportion", title = "Kevin O'Leary: Investment by Industry")
#kevinIndustries #most likely to invest in tech, food and beverage, least in green tech, fitness
#find robert's average stats...
kevinPlots <- kevinDeals %>% filter(!is.na(Equity) & !is.na(Amount) & !is.na(Valuation)) %>% select(Amount, Equity, Valuation) %>% 
  mutate(Amount = accounting(Amount)) %>% mutate(Equity = percent(Equity*.01)) %>% mutate(Valuation = accounting(Valuation)) %>% 
  summarize(Companies = n(), MeanAmount = mean(Amount), MeanEquity = mean(Equity), meanValuation = mean(Valuation)) %>%
  formattable(., align = c("l", "r", "r", "r")) %>% setNames(c("Kevin's Companies", "Mean Amount Taken", "Mean Equity Given", "Mean Ending Valuation"))
#kevinPlots