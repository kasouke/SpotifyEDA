# SpotifyEDA
Analysis of Spotify EDA with R
#Source data: https://www.kaggle.com/yamaerenay/spotify-dataset-19212020-160k-tracks (file too large to upload)
#1 Load libraries
 library(data.table)
 library(DT)
 library(kableExtra)
 library(knitr)
 library(stringr)
 library(formattable)
 library(plotly)
 library(wordcloud)
 library(RColorBrewer)
 library(tidyverse)
 library(tidyselect)
 library(tm)
 library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(ggthemr)
library(caret)
library(randomForest)
library(e1071)
ggthemr('flat dark')

#2 Load csv file
 setwd("~/Documents")
 df <- read_csv("tracks.csv")

#3 based on analysis, year is not present. Hence, extract year from release date
df$year <- substr(df$release_date, 1, 4)

#4 based on analysis, decade is not present. Hence, group year by decade. 
df$decade<- ifelse((df$year<1930),"The20s",
                   ifelse( (df$year>1929)&(df$year<1940),"The30s",
                   ifelse( (df$year>1939)&(df$year<1950),"The40s",
                   ifelse( (df$year>1949)&(df$year<1960),"The50s",
                   ifelse( (df$year>1959)&(df$year<1970),"The60s",
                   ifelse( (df$year>1969)&(df$year<1980),"The70s",
                   ifelse( (df$year>1979)&(df$year<1990),"The80s",
                   ifelse( (df$year>1989)&(df$year<2000),"The90s",
                   ifelse( (df$year>1999)&(df$year<2010),"The2000s",
                   ifelse( (df$year>2009)&(df$year<2020),"The2010s",
                   ifelse( (df$year>2019),"The2020s","NA")))))))))))



#5 insert a new column to describe mode
df$isMode <- ifelse(df$mode>0,"Major","Minor")

#6 insert a new column to describe explicit
df$isExplicit <- ifelse(df$explicit>0,"Explicit","Clean")

#7 insert a new column to describe popularity
df$isPopular <- ifelse((df$popularity>74.999),"Very Popular",
                  ifelse((df$popularity>49.999)&(df$popularity<75),"Popular",
ifelse((df$popularity>24.999)&(df$popularity<50),"Average",
ifelse((df$popularity<25),"Not Popular","NA"))))

#8 insert a new column to describe key
df$isKey <- ifelse((df$key<1),"C",
                   ifelse( (df$key>0)&(df$key<2),"C#",
                   ifelse( (df$key>1)&(df$key<3),"D",
                   ifelse( (df$key>2)&(df$key<4),"D#",
                   ifelse( (df$key>3)&(df$key<5),"E",
                   ifelse( (df$key>4)&(df$key<6),"F",
                   ifelse( (df$key>5)&(df$key<7),"F#",
                   ifelse( (df$key>6)&(df$key<8),"G",
                   ifelse( (df$key>7)&(df$key<9),"G#",
                   ifelse( (df$key>8)&(df$key<10),"A",
                   ifelse( (df$key>9)&(df$key<11),"A#",
                   ifelse( (df$key>10)&(df$key<12),"B","NA"))))))))))))



#9 insert a new column to describe Scale by concatenating  isKey and isMode
df$isScale <- str_c(df$isKey,' ',df$isMode)


# 10 Top100 songs


df1 <- select(df,name,artists,year, decade, popularity, isExplicit, isScale)
Top100 <- df1 %>% slice_max(popularity, n = 100,with_ties = FALSE)


#11 Top 20 songs by decades
df2 <- select(df,name,artists,decade,popularity, isExplicit, isScale)
 
Top20decade<- df2 %>% 
    arrange(desc(popularity)) %>% 
    group_by(decade) %>% slice(1:20)

for (variable in unique(Top20decade$decade)) {
         assign(variable, Top20decade %>% filter (decade == variable,), envir = .GlobalEnv)  }

#12 inserting theme https://github.com/walkerkq/kp_themes/blob/master/theme_kp.R  & Plotting audio density feature by decade
dfnew <- select(df,isScale,isPopular,decade,popularity,danceability,energy,loudness,speechiness,acousticness,instrumentalness,liveness,valence,tempo)

feature_names <- names(dfnew)[5:13]

dfnew%>%
  select(c('decade', feature_names)) %>%
  pivot_longer(cols = feature_names) %>%
  ggplot(aes(x = value)) +
  geom_density(aes(color = decade), alpha = 0.5) +
  facet_wrap(~name, ncol = 3, scales = 'free') +
  labs(title = 'Spotify Audio Feature Density - by Decade',
       x = '', y = 'density') +
  theme(axis.text.y = element_blank()) + 
  scale_color_kp(palette = 'mixed')

dfnew %>%
  select(c('isScale', feature_names)) %>%
  pivot_longer(cols = feature_names) %>%
  ggplot(aes(x = value)) +
  geom_density(aes(color = isScale), alpha = 0.5) +
  facet_wrap(~name, ncol = 3, scales = 'free') +
  labs(title = 'Spotify Audio Feature Density - by Scale',
       x = '', y = 'density') +
  theme(axis.text.y = element_blank()) + 
  scale_color_kp(palette = 'mixed')


dfnew %>%
  select(c('isPopular', feature_names)) %>%
  pivot_longer(cols = feature_names) %>%
  ggplot(aes(x = value)) +
  geom_density(aes(color = isPopular), alpha = 0.5) +
  facet_wrap(~name, ncol = 3, scales = 'free') +
  labs(title = 'Spotify Audio Feature Density - by Popularity',
       x = '', y = 'density') +
  theme(axis.text.y = element_blank()) + 
  scale_color_kp(palette = 'mixed')

#13 plotting audio feature correlation

feature_names1 <- names(dfnew)[4:13]
dfnew %>%
  select(feature_names1) %>%
  scale() %>%
  cor() %>%
  corrplot::corrplot(method = 'color', 
                     order = 'hclust', 
                     type = 'upper', 
                     diag = FALSE, 
                     tl.col = 'black',
                     addCoef.col = "grey30",
                     number.cex = 0.6,
                     col = colorRampPalette(colors = c(
                       kp_cols('red'), 
                       'white', 
                       kp_cols('dark_blue')))(200),
                     main = 'Audio Feature Correlation',
                     mar = c(2,2,2,2),
                     family = 'Avenir')

#14 Random Forest

dfnew1 <- select(df,isScale,isPopular,decade,popularity,danceability,energy,loudness,acousticness,instrumentalness)

dfnew1$isPopular <- ifelse((df$popularity>49.99),"Popular","Not Popular")

dfnew2 <- dfnew1 %>%
  mutate_if(is.character, as.factor)

dfnew3 <- select(dfnew2,-isScale,-popularity,-decade)

#check number of levels as randomForest cannot handle more than 53 lvls
str(dfnew3)

# set the random seed number
set.seed(196)

trainRowNumbers <- createDataPartition(dfnew3$isPopular, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
trainData <- dfnew3[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- dfnew3[-trainRowNumbers,]

# Store Xtr, ytr, Xte, yte for later use.
Xtrain = trainData %>% select (-isPopular)
ytrain = trainData$isPopular

Xtest = testData %>% select (-isPopular)
ytest = testData$isPopular

head(Xtrain)
head(ytrain)
head(Xtest)
head(ytest)

RF1 <- randomForest::randomForest(Xtrain,ytrain ,Xtest,ytest,
                                                                   mtry=4, ntree = 300, importance =TRUE)

RF1$importance
arrange(as.data.frame(RF1$importance),desc(MeanDecreaseGini))


confusionMatrix(RF1$test$predicted, ytest, positive = "Popular")

varImpPlot(RF1)
