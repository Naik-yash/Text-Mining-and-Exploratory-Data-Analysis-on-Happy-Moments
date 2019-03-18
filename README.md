# Text-Mining-and-Exploratory-Data-Analysis-on-People-s-Happy-Moments
Exploratory Data Analysis on Happy Moments
Does happiness mean different things to different people? If so, what might be the factors that affect people’s definitions of happiness? And how?

If you ask the people around you to describe the most unforgettable happy moments, you are very likely to be overwhelmed by the variety of their experiences. I’m highly inspired to advance the state of the art of understanding the causes of happiness. I will utilize exploratory data analysis to compare the patterns of happy causes across different subgroups of people to identify the factors that might influence people’s happiness sources.

I will join the cleaned version of happy moments text file and demographic dataset to perform text mining and create data visualization to more intuitively compare the happiness causes among different subgroups of people based on their demographic features.

Do sources of happiness vary across people in different age groups?

I divide the whole population into five groups based on their age: iGeneration(Teens & younger), Millennials(18 - 34 years old), Generation X(35 - 49 years old), Baby Boomers(50 - 69 years old) and Senior Citizen(70+ years older). Then I make a contingency table of age group and happy moments categories:

Data_age <- merge(hm_data[,c("wid","predicted_category")],demographic[,c("wid","age")])
# join the processed happy moments dataset with age in demographic data
Data_age <- Data_age[order(Data_age$age),]
Data_age <- Data_age[grepl("[0-9]{1,2}", Data_age$age),] # remove rows with invalid values
Data_age$age <- as.numeric(Data_age$age)
Data_age$age_group[Data_age$age < 18] <-  "iGeneration"
Data_age$age_group[Data_age$age < 35 & Data_age$age >= 18] <- "Millennial"
Data_age$age_group[Data_age$age < 50 & Data_age$age >= 35] <- "Generation X"
Data_age$age_group[Data_age$age < 70 & Data_age$age >= 50] <- "Baby Boomers"
Data_age$age_group[Data_age$age >= 70] <- "Senior Citizen"
Data_age_split <- split(Data_age, Data_age$age_group)
categorycount <- function(df){
  affection        <- sum(df$predicted_category == "affection")
  achievement      <- sum(df$predicted_category == "achievement")
  bonding          <- sum(df$predicted_category == "bonding")
  enjoy_the_moment <- sum(df$predicted_category == "enjoy_the_moment")
  leisure          <- sum(df$predicted_category == "leisure")
  exercise         <- sum(df$predicted_category == "exercise")
  nature           <- sum(df$predicted_category == "nature")
  return(c(affection,achievement,bonding,enjoy_the_moment,leisure,exercise,nature))
}
age.summatrix <- sapply(Data_age_split, categorycount)
rownames(age.summatrix) <- c("affection","achievement","bonding",
                          "enjoy_the_moment","leisure","exercise","nature") 
age.summatrix <- age.summatrix[,c("iGeneration","Millennial",
                                  "Generation X","Baby Boomers","Senior Citizen")]
age.summatrix
##                  iGeneration Millennial Generation X Baby Boomers
## affection               4017      12622         9437         4446
## achievement             4941      13628         8606         3803
## bonding                 1825       4668         2527          945
## enjoy_the_moment        1606       4575         2830         1166
## leisure                  905       3474         1878          715
## exercise                 177        562          275          112
## nature                   163        580          487          282
##                  Senior Citizen
## affection                  3571
## achievement                2884
## bonding                     753
## enjoy_the_moment            916
## leisure                     480
## exercise                     69
## nature                      329
Next, we conduct a chi-square test to check the correlation between age and what make people happy.

chisq.test(age.summatrix)
## 
##  Pearson's Chi-squared test
## 
## data:  age.summatrix
## X-squared = 1141.6, df = 24, p-value < 2.2e-16
We can see that the Chi-squared statistic is significant, which provides evidence that age does have an effect on the reasons why people feel happy. Then I explore the patterns of happiness categories among those age groups. I display the proportions of happiness sources in the table below:

categoryproportion <- function(df){
  affection        <- mean(df$predicted_category == "affection")
  achievement      <- mean(df$predicted_category == "achievement")
  bonding          <- mean(df$predicted_category == "bonding")
  enjoy_the_moment <- mean(df$predicted_category == "enjoy_the_moment")
  leisure          <- mean(df$predicted_category == "leisure")
  exercise         <- mean(df$predicted_category == "exercise")
  nature           <- mean(df$predicted_category == "nature")
  return(c(affection,achievement,bonding,enjoy_the_moment,leisure,exercise,nature))
}
age.propmatrix <- sapply(Data_age_split, categoryproportion)
rownames(age.propmatrix) <- c("affection","achievement","bonding",
                          "enjoy_the_moment","leisure","exercise","nature") 
age.propmatrix <- 
  age.propmatrix[,c("iGeneration","Millennial","Generation X",
                    "Baby Boomers","Senior Citizen")]
age.propmatrix
##                  iGeneration Millennial Generation X Baby Boomers
## affection         0.29463107 0.31469246   0.36240399  0.387653675
## achievement       0.36240282 0.33977412   0.33049155  0.331589502
## bonding           0.13385654 0.11638286   0.09704301  0.082396024
## enjoy_the_moment  0.11779375 0.11406418   0.10867896  0.101665359
## leisure           0.06637817 0.08661398   0.07211982  0.062341965
## exercise          0.01298225 0.01401182   0.01056068  0.009765455
## nature            0.01195541 0.01446059   0.01870200  0.024588020
##                  Senior Citizen
## affection           0.396689625
## achievement         0.320373250
## bonding             0.083648078
## enjoy_the_moment    0.101755166
## leisure             0.053321484
## exercise            0.007664963
## nature              0.036547434
We can notice that the proportions of “affection” and “nature” slightly increase as people age while the proportions of “achievement” and “bonding” slightly decrease as people grow older. The reason might be that people tend to become more emotional when they age and their pace of life gets slower. So, they might gradually care less about their career and achievement and more about their beloved people. They also want to get closer to nature and experience a better environment. I further visualize the proportion pattern in the following heatmap:

age.heatmap <- heatmap(t(age.propmatrix),col = cm.colors(256), margins=c(10,5))


These changes of color depth in the above heatmap more intuitively support my finding that people tend to switch from career/achievement to emotional needs as they age.

Are the things that make men and women happy different?

First I display a contingency table of gender and happiness categories and perform the chi-square test to check the correlation between them:

Data_gender <- merge(hm_data[,c("wid", "text", "predicted_category")],
                     demographic[,c("wid", "gender")])
Data_gender <- Data_gender[Data_gender$gender%in%c("f","m"),]
# join the processed happy moments dataset with gender in demographic data
Data_gender_split <- split(Data_gender, Data_gender$gender)
gender.summatrix <- sapply(Data_gender_split, categorycount)
gender.summatrix <- gender.summatrix[1:7,2:3]
rownames(gender.summatrix) <- c("affection","achievement","bonding",
                          "enjoy_the_moment","leisure","exercise","nature") 
gender.summatrix
##                      f     m
## affection        17193 16744
## achievement      12516 21110
## bonding           4174  6454
## enjoy_the_moment  4304  6703
## leisure           2578  4824
## exercise           365   820
## nature             889   942
chisq.test(gender.summatrix)
## 
##  Pearson's Chi-squared test
## 
## data:  gender.summatrix
## X-squared = 1677, df = 6, p-value < 2.2e-16
The chi-squared statistic is 1677, which significantly demonstrates that the patterns of happiness sources for men and women are different. Next, I draw the word clouds to show the most high-frequency words in the cleaned version of happiness texts for both genders respectively.

Data_gender_m <- Data_gender[Data_gender$gender == "m", ]
Data_gender_f <- Data_gender[Data_gender$gender == "f", ]
The word cloud for females is as follows:

ff.all <- Corpus(VectorSource(Data_gender_f$text))
tdm.all<-TermDocumentMatrix(ff.all)
tdm.tidy=tidy(tdm.all)
tdm.overall=summarise(group_by(tdm.tidy, term), sum(count))
wordcloud(tdm.overall$term, tdm.overall$`sum(count)`,
          scale=c(5,0.5),
          max.words=100,
          min.freq=1,
          random.order=FALSE,
          rot.per=0.3,
          use.r.layout=T,
          random.color=FALSE,
          colors=brewer.pal(9,"Blues"))


We can notice that many of the top high-frequency words have to do with family members, household chores or daily activities. The things that women care about most are usually associated with “affection”. This reflects the fact that women are more emotional and they need to express their feelings and relate to one another. Women more often choose family over work. One reality persists that women most often are the ones who adjust their schedules and make compromises when the needs of children and other family members collide with work.

The word cloud for males is as follows:

ff.all <- Corpus(VectorSource(Data_gender_m$text))
tdm.all<-TermDocumentMatrix(ff.all)
tdm.tidy=tidy(tdm.all)
tdm.overall=summarise(group_by(tdm.tidy, term), sum(count))
wordcloud(tdm.overall$term, tdm.overall$`sum(count)`,
          scale=c(5,0.5),
          max.words=100,
          min.freq=1,
          random.order=FALSE,
          rot.per=0.3,
          use.r.layout=T,
          random.color=FALSE,
          colors=brewer.pal(9,"Blues"))


One thing we can notice from the word cloud is that men are more career-oriented than women. The happy moments key words regarding achievement in both job and school appear much more frequently than women. Another thing worth noting is that the happiness sources of men are obviously more diverse than women. Besides affection and achievement, the things that can make men happy also include exercise, sports, game, video, party, etc. in their leisure time. Men seem to pursue a rich, full and colorful life.

Comparative Sentiment Analysis - Do people feel happy due to different reasons before and after they become parents?

Next, I will perform comparative sentiment analysis on people who have already become parents and people who haven’t. I want to explore whether having children affect their sources of happiness by their most common positive and negative words.

Data_parenthood <- merge(demographic[,c("wid","parenthood")], hm_data[,c("wid","text")])
Data_parenthood <- Data_parenthood[,c("text","parenthood")] 
The result of sentiment analysis for those parents are as follows:

tidy_parenthood <- Data_parenthood %>%
  filter(parenthood == "y") %>%
  unnest_tokens(word, text)
bing_word_counts <- tidy_parenthood %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


The result of sentiment analysis for those childless people is shown as below:

tidy_parenthood <- Data_parenthood %>%
  filter(parenthood == "n") %>%
  unnest_tokens(word, text)
bing_word_counts <- tidy_parenthood %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


When we analyze the positive sentiments, we can find that childless people enjoy more personal pleasure and freedom because we can notice the words such as “free”, “delicious”, etc. contribute more to their positive sentiment while those people who become parents seem to enjoy more joint celebration or have some fun together. Some useful insights can also be obtained by analyzing the negative sentiments. Those childless people have more “unexpected” or “stress” moments while people suffer more “worried” or “struggling” moments after they become parents because they are concerned about their kids and they need to undertake more responsibilities of being a good parent. It is the process of overcoming the difficulties and fulfilling their responsibilities that gives those parents more happiness. In short, parenthood changes the life style and mental status of people and thus highly affect people’s sources of happiness.

Are people’s happy moments also associated with their marital status?

In this part, I want to introduce another method to perform comparative analysis for people in two marital status:married and single. Here I create bigrams to explore the correlations between words and visualize the network of bigrams. Then I can find the different patterns of the frequency or sentiments.

Data_marital <- merge(hm_data[,c("wid","text")],demographic[,c("wid","marital")])
# join the processed happy moments dataset with country in demographic data
Data_marital <- Data_marital[Data_marital$marital%in%c("married","single"),
                             c("marital","text")]
# I only consider two marital status: married and single for simplicity 
The bigram for married people’s happy moment words is as below:

marital_bigrams <- Data_marital %>%
  filter(marital=="married") %>% # picking out records where marital status is married
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_separated <- marital_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>% # remove uninteresting stop words
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
head(bigram_counts,15)
The network graph for married people’s happy moment words is as below:

bigram_graph <- bigram_counts %>%
  filter(n > 100) %>%
  graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


The bigram for single people’s happy moment words is as below:

marital_bigrams <- Data_marital %>%
  filter(marital=="single") %>% # picking out records where marital status is single
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_separated <- marital_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>% # remove uninteresting stop words
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
head(bigram_counts,15)
The network graph for single people’s happy moment words is as below:

bigram_graph <- bigram_counts %>%
  filter(n > 100) %>%
  graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


When comparing the two sets of output, I can find that people get more pleasure from their family activities after marriage. Married people tend to be more family-oriented and the themes of their happiness are more likely to be correlated to their family like “mother”, “daughter”, “birthday”, etc. But when people are still single, they have more personal time and thus their life is much more colorful. They can develop their personal hobbies like “walk dog”, “eat ice cream”, “read book”, “eat delicious food”, “buy car”, “play video game”, etc. They have fewer things to worry about and can enjoy more leisure when they are still single.

How do people around the world understand happiness in different ways?

Here I plot global heatmaps for each happiness category to display the geographic patterns of happiness causes. This way I can explore whether country of residence impacts people’s happiness notion.

Data_country <- merge(hm_data[,c("wid","predicted_category")],demographic[,c("wid","country")])
# join the processed happy moments dataset with country in demographic data
Data_country <- Data_country[order(Data_country$country),]
Data_country <- Data_country[grepl("[A-Z]{3}", Data_country$country),] 
# remove rows with invalid values
Data_country_split <- split(Data_country, Data_country$country)
country.prop.affection <- function(df){
  return(mean(df$predicted_category == "affection"))
}
prop.affection     <- sapply(Data_country_split, country.prop.affection)
prop.affection.df  <- data.frame(names(prop.affection), prop.affection)
rownames(prop.affection.df) <- NULL
prop.affection.df  <- prop.affection.df[-1,]
colnames(prop.affection.df) <- c("ISO3", "prop.affection")
sPDF <- joinCountryData2Map(prop.affection.df, 
                            joinCode = "ISO3",
                            nameJoinColumn = "ISO3")
## 99 codes from your data successfully matched countries in the map
## 1 codes from your data failed to match with a country code in the map
## 144 codes from the map weren't represented in your data
mapDevice() #create world map shaped window
mapCountryData(sPDF,nameColumnToPlot='prop.affection')
## You asked for 7 quantiles, only 6 could be created in quantiles classification


country.prop.achievement <- function(df){
  return(mean(df$predicted_category == "achievement"))
}
prop.achievement     <- sapply(Data_country_split, country.prop.achievement)
prop.achievement.df  <- data.frame(names(prop.achievement), prop.achievement)
rownames(prop.achievement.df) <- NULL
prop.achievement.df  <- prop.achievement.df[-1,]
colnames(prop.achievement.df) <- c("ISO3", "prop.achievement")
sPDF <- joinCountryData2Map(prop.achievement.df, 
                            joinCode = "ISO3",
                            nameJoinColumn = "ISO3")
## 99 codes from your data successfully matched countries in the map
## 1 codes from your data failed to match with a country code in the map
## 144 codes from the map weren't represented in your data
mapDevice() #create world map shaped window
mapCountryData(sPDF,nameColumnToPlot='prop.achievement')


country.prop.bonding <- function(df){
  return(mean(df$predicted_category == "bonding"))
}
prop.bonding     <- sapply(Data_country_split, country.prop.bonding)
prop.bonding.df  <- data.frame(names(prop.bonding), prop.bonding)
rownames(prop.bonding.df) <- NULL
prop.bonding.df  <- prop.bonding.df[-1,]
colnames(prop.bonding.df) <- c("ISO3", "prop.bonding")
sPDF <- joinCountryData2Map(prop.bonding.df, 
                            joinCode = "ISO3",
                            nameJoinColumn = "ISO3")
## 99 codes from your data successfully matched countries in the map
## 1 codes from your data failed to match with a country code in the map
## 144 codes from the map weren't represented in your data
mapDevice() #create world map shaped window
mapCountryData(sPDF,nameColumnToPlot='prop.bonding')
## You asked for 7 quantiles, only 2 could be created in quantiles classification


country.prop.enjoy_the_moment <- function(df){
  return(mean(df$predicted_category == "enjoy_the_moment"))
}
prop.enjoy_the_moment     <- sapply(Data_country_split, country.prop.enjoy_the_moment)
prop.enjoy_the_moment.df  <- data.frame(names(prop.enjoy_the_moment),
                                        prop.enjoy_the_moment)
rownames(prop.enjoy_the_moment.df) <- NULL
prop.enjoy_the_moment.df  <- prop.enjoy_the_moment.df[-1,]
colnames(prop.enjoy_the_moment.df) <- c("ISO3", "prop.enjoy_the_moment")
sPDF <- joinCountryData2Map(prop.enjoy_the_moment.df, 
                            joinCode = "ISO3",
                            nameJoinColumn = "ISO3")
## 99 codes from your data successfully matched countries in the map
## 1 codes from your data failed to match with a country code in the map
## 144 codes from the map weren't represented in your data
mapDevice() #create world map shaped window
mapCountryData(sPDF,nameColumnToPlot='prop.enjoy_the_moment')
## You asked for 7 quantiles, only 4 could be created in quantiles classification


country.prop.exercise <- function(df){
  return(mean(df$predicted_category == "exercise"))
}
prop.exercise     <- sapply(Data_country_split, country.prop.exercise)
prop.exercise.df  <- data.frame(names(prop.exercise), prop.exercise)
rownames(prop.exercise.df) <- NULL
prop.exercise.df  <- prop.exercise.df[-1,]
colnames(prop.exercise.df) <- c("ISO3", "prop.exercise")
sPDF <- joinCountryData2Map(prop.exercise.df, 
                            joinCode = "ISO3",
                            nameJoinColumn = "ISO3")
## 99 codes from your data successfully matched countries in the map
## 1 codes from your data failed to match with a country code in the map
## 144 codes from the map weren't represented in your data
mapDevice() #create world map shaped window
mapCountryData(sPDF,nameColumnToPlot='prop.exercise')
## You asked for 7 quantiles, only 1 could be created in quantiles classification


country.prop.leisure <- function(df){
  return(mean(df$predicted_category == "leisure"))
}
prop.leisure     <- sapply(Data_country_split, country.prop.leisure)
prop.leisure.df  <- data.frame(names(prop.leisure), prop.leisure)
rownames(prop.leisure.df) <- NULL
prop.leisure.df  <- prop.leisure.df[-1,]
colnames(prop.leisure.df) <- c("ISO3", "prop.leisure")
sPDF <- joinCountryData2Map(prop.leisure.df, 
                            joinCode = "ISO3",
                            nameJoinColumn = "ISO3")
## 99 codes from your data successfully matched countries in the map
## 1 codes from your data failed to match with a country code in the map
## 144 codes from the map weren't represented in your data
mapDevice() #create world map shaped window
mapCountryData(sPDF,nameColumnToPlot='prop.leisure')
## You asked for 7 quantiles, only 2 could be created in quantiles classification


country.prop.nature <- function(df){
  return(mean(df$predicted_category == "nature"))
}
prop.nature     <- sapply(Data_country_split, country.prop.nature)
prop.nature.df  <- data.frame(names(prop.nature), prop.nature)
rownames(prop.nature.df) <- NULL
prop.nature.df  <- prop.nature.df[-1,]
colnames(prop.nature.df) <- c("ISO3", "prop.nature")
sPDF <- joinCountryData2Map(prop.nature.df, 
                            joinCode = "ISO3",
                            nameJoinColumn = "ISO3")
## 99 codes from your data successfully matched countries in the map
## 1 codes from your data failed to match with a country code in the map
## 144 codes from the map weren't represented in your data
mapDevice() #create world map shaped window
mapCountryData(sPDF,nameColumnToPlot='prop.nature')
## You asked for 7 quantiles, only 1 could be created in quantiles classification


After closely observing and comparing the geographic pattern of happiness causes in the global heatmap, we can find that the country where people live might be an important factor affecting their happiness sources. One limitation of this analysis is that the sample sizes of some countries are limited and thus the results for some countries might be less reliable. But if we analyze the overall pattern, we can still discover some useful things. We can notice that affectional reasons give more pleasure to people in some less developed countries such as Indo-Pakistan areas or part of Eastern Europe. Besides, the happiness moments of people in the Western world(North America, Latin America, Oceania and Western Europe) come from bonding more than people in other areas. The formation of close relationships matters more to them. They care less about personal achievement than people in some developing countries such as some Southeastern countries. People in “Third World” are less likely to choose the things regarding enjoy-the-moment or leisure as their major happiness sources.

Conclusion
People become more emotional as they age and their pace of life gets slower. People tend to switch from achievement in career to emotional needs as they get older. They also want to get closer to nature and live in a better environment.
Women are more emotional. The things that women care about most are usually associated with “affection”. They need to express their feelings and relate to one another. In contrast, men are more career-oriented and their academic or professional achievement give them more happiness. Their happiness sources are also more diverse and they seem to pursue a rich, full and colorful life.
Married people tend to be more family-oriented especially after they become parents and the themes of their happiness are more likely to be correlated to their family. It is the process of overcoming the difficulties and fulfilling their responsibilities that gives them more happiness. But when people are still single or childless, they have more personal time to develop their personal hobbies. They have fewer things to worry about and can enjoy more leisure. These people suffer more unexpected or stress moments on their own.
There exists geographic pattern of happiness causes around the world. For people living in some less developed countries, affectional reasons give more pleasure to them. These people are also less likely to choose the things regarding enjoy-the-moment or leisure as their major happiness sources. For Western people, the formation of close relationships matters more to them and they care less about personal achievement.
References
Natural Language Processing: from Tutorial
Text Mining: from Topic Modelling
