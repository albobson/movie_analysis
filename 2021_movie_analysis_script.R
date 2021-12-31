## Alexander Robertson
## 2021/12/31
## Playing around with the data I generated over the last two years 
## watching movies.
## For more info, visit www.aj-robertson.com

## Ideas to expand on in the future:
## Time of year vs rating given
## Scatterplot of data by time of year
library(dplyr)

movies_master = read.csv("20211231_movies.csv")
m=as_tibble(movies_master)

head(m)
dim(m)

## Changing the variables to the correct class type
nums = c("personal_rating","year","budget","runtime","r.direction",
         "r.cinematography","r.score","r.acting","r.screenplay","avg_rating",
         "cost")
nonnums = c("title","genre1","genre2","rewatch","mpa","nudity","twist")
m[nums] = sapply(m[nums],as.numeric)
m[nonnums] = sapply(m[nonnums],as.character)

## Changing budget to millions
m$budget = m$budget/1000000

## Converting dates to class() = date
m$watchdate = as.Date(as.character(m$watchdate), format = "%Y%m%d")
class(m$watchdate)

## Selecting just movies watched in 2021
## Note: This needs to be modified per year
m <- m %>%
  filter(watchdate >= as.Date(as.character(20210101), format = "%Y%m%d"))

## Adding a week column
m$watchweek = lubridate::week(m$watchdate)
  

####### Looking at my overall rating distribution  ##########
## Creating a column with rounded rating
library(ggplot2)
m$roundrating = round(m$personal_rating/.5)*.5
rat_dist = as.data.frame(table(m$roundrating))
names(rat_dist)[1] = "rating"
names(rat_dist)[2] = "freq"
grat_dist = ggplot(data = rat_dist, aes(x =rating, y = freq, fill=rating))
grat_dist + geom_col() + 
  theme_minimal() +
  labs(title="Distribution of ratings across all movies watched in 2021",
       x = "Personal Rating", 
       y = "Number of movies") + 
  theme(text = element_text(size=13), legend.position = "none")


############## Analysis of by movie release date #########################
## Plotting personal rating by year movie was released 
g = ggplot(data = m, aes(x=year, y=personal_rating, color=genre1))
g + geom_point() + 
  theme_minimal() +
  ylim(c(2,10)) +
  labs(title="Personal Rating by Year of Movie Release", 
       y = "Personal Rating (1-10)", x="Year of movie release", 
       color = "Primary genre") + theme(text = element_text(size=13))

## Making a decades column
m$decade = m$year - m$year %% 10

## The number of movies watched by decade
dec = as.data.frame(table(m$decade))
names(dec)[1] = "dec"
names(dec)[2] = "freq"
gdec = ggplot(data = dec, aes(x = dec, y = freq, fill=dec))
gdec + geom_col() + labs(title="Movies watched in 2021 by decade", 
                         x = "Decade of movie release", y = "Total number") + 
  theme_minimal() +
  theme(text = element_text(size=13), legend.position = "none")

## Finding the average rating by decade. Then seeing if there's a statistically 
## significant difference
# install.packages("doBy")
library(doBy)
avgdec = summaryBy(personal_rating~decade, data = m, FUN = mean, na.rm = TRUE)
names(avgdec)[1] = "decade"
names(avgdec)[2] = "avg_rating"

gavgdec = ggplot(data = avgdec, aes(x=decade, y=avg_rating, fill = decade))
gavgdec + geom_col() + labs(title="Average rating by decade", 
                            y = "Average Personal Rating (1-10)", 
                            x="Decade of movie release") + 
  theme_minimal() +
  theme(text = element_text(size=15), legend.position = "none")

## ANOVA to see differences
summary(aov(personal_rating~decade, data = m))





################## Looking at genre ####################
## Finding the number of movies by genre1 
gen = as.data.frame(table(m$genre1))
names(gen)[1] = "genre"
names(gen)[2] = "freq"
## Reordering it from largest to smallest 
gen = transform(gen, genre = reorder(genre, -freq))
ggen = ggplot(data = gen, aes(x=genre, y=freq, fill = genre))
ggen + geom_col() + labs(title="Movies watched in 2021 by primary genre", 
                         x = "", y = "Total number") + 
  theme_minimal() +
  theme(text = element_text(size=15), 
        axis.text.x = element_text(angle = 90, hjust=0.95), 
        legend.position = "none")

## Finding the average ratings by genre and then finding if there is a 
## statistical significance between them
## Removing any genres with less than 3 entries
small_gen = m %>%
  group_by(genre1) %>%
  filter(n() > 3)
## Creating a table of average scores and transforming it from largest to 
## smallest
avgg = summaryBy(personal_rating~genre1, 
                 data = small_gen, FUN = mean, na.rm = TRUE)
avgg = transform(avgg, genre1 = reorder(genre1, -personal_rating.mean))
## Graphing table
gavgg = ggplot(data = avgg, aes(x=genre1, y = personal_rating.mean, 
                                fill = genre1))
gavgg + geom_col() + 
  labs(title="Average ratings by primary genre", 
       x = "", y = "Average Rating (1-10)") + 
  theme_minimal() +
  theme(text = element_text(size=15), 
        axis.text.x = element_text(angle = 0), 
        legend.position = "none")

## Significant difference between scores?
summary(aov(personal_rating~genre1, data=m))





############ Looking at MPA ratings ################
## Playing around with MPA ratings
rat = as.data.frame(table(m$mpa))
names(rat)[1] = "mpa"
names(rat)[2] = "freq"
rat
## Graphing rat
rat = transform(rat, mpa = reorder(mpa, -freq))
grat = ggplot(data = rat, aes(x=mpa, y=freq, fill = mpa))
grat + geom_col() + 
  labs(title="Movies watched in 2021 by MPA Rating", 
       x = "Rating", y = "Total number") +
  theme_minimal() +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 0), 
        legend.position = "none")




############# Linear model ################## 
## Varrables of interest
library(MASS)
lin_col = c("personal_rating","year","genre1","rewatch","mpa",
            "budget","runtime","nudity","twist")
lin_frame = m[,lin_col]

## Chaning to numeric/character
nums = c("personal_rating","year","budget","runtime")
nonnums = c("genre1","rewatch","mpa","nudity","twist")
lin_frame[nums] = sapply(lin_frame[nums],as.numeric)
lin_frame[nonnums] = sapply(lin_frame[nonnums],as.character)
lin_frame = na.omit(lin_frame)

## Creating linear model
linmod = lm(personal_rating~., data=lin_frame)
summary(linmod)

## Finding the optimal variables
opt_mod = stepAIC(linmod)
summary(opt_mod)

## Finding the percent error
paste("Percent error:", 
      round(sigma(opt_mod)*100/mean(lin_frame$personal_rating), digits = 2))

## Plotting by time of year watched the movies
## Plotting the movies watched by week of the year
m$watchweek = as.numeric(m$watchweek)
gwek = ggplot(data = m, aes(x=watchweek, fill=genre1))
gwek + geom_bar() + 
  labs(title="Movies watched per week of 2021", 
       x = "Week", y = "Number per week",
       fill = "Primary Genre") + 
  theme_minimal() +
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 0))


## Plotting the individual movie ratings by date
## Need more work on this - want to fill by genre or something
gdat = ggplot(data = m, aes(x = watchdate, y = personal_rating, color=genre1))
gdat + geom_point() +
  labs(title="Movie Ratings by Watch Date", 
       x = "", y = "Rating",
       color = "Genre") +
  ylim(c(2,10)) +
  theme_minimal() +
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 0))
