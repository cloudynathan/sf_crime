# ------ San Francisco Crime Dataset ------ #
# https://www.kaggle.com/roshansharma/sanfranciso-crime-dataset

library(tidyverse)
library(ggmap)

df <- read.csv("C:/workspaceR/sf_crime/data.csv", stringsAsFactors = FALSE)
df %>% head()
str(df)

options(scipen = 999) #disable scientific notation

#list of character variables. counted and sorted. 
df %>% select_if(is.character) %>% map(~count(data.frame(x=.x), x)) %>% modify(. %>% arrange(-n))

#plot number of crime in each PdDistrict
df %>% select(PdDistrict) %>% group_by(PdDistrict) %>% count() %>% arrange(-n) %>% filter(n > 10) %>%
  ggplot(aes(x=reorder(PdDistrict,n), y=n)) + 
  geom_bar(stat = "identity", aes(fill=PdDistrict)) + 
  coord_flip() +
  guides(fill="none") + #remove legend
  xlab("PdDistrict") +
  scale_y_continuous(limits = c(0,40000)) +
  ggtitle("Number of crimes in each PdDistrct") +
  theme(plot.title = element_text(hjust = 0.3))

#plot Top 5 crime categories in each PdDistrct
df %>% select(Category, PdDistrict) %>% filter(PdDistrict != "") %>%
  group_by(Category, PdDistrict) %>% count() %>% arrange(PdDistrict,-n) %>% 
  group_by(PdDistrict) %>% slice(1:5) %>% 
  ggplot(aes(x=reorder(Category, desc(Category)), y=n)) +
  geom_bar(stat = "identity", aes(fill=Category)) + 
  coord_flip() + 
  guides(fill="none") +
  facet_wrap(~PdDistrict) +
  xlab("Categories") +
  ggtitle("Top 5 crime categories in each PdDistrct") +
  theme(plot.title = element_text(hjust = 0.4))

#map of San Francisco
sf_map <- get_map(location = 'San Francisco, California', zoom = 12)
ggmap(sf_map)

#map bottom 10 categories of crime (minumum 100 crimes committed)
bottom10_crime <- df %>% select(Category, X, Y) %>% 
  filter(Category %in% (df %>% select(Category) %>% 
                               group_by(Category) %>% count() %>% arrange(-n) %>% 
                               filter(n >= 100) %>%
                               tail(10) %>% pull(Category)))

ggmap(sf_map) + 
  geom_point(data=bottom10_crime, aes(x=X, y=Y, color=Category), size=2, alpha=.7) + 
  facet_wrap(~Category) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Bottom 10 categories of crime (minimum 100 crimes committed)")

#map of Montgomery BART station
mont_map <- get_map(location = '598 Market St, San Francisco, CA 94104', zoom = 16, color = "bw", legend = "topleft")
ggmap(mont_map)

#bottom 4 crimes (minimum 500 crimes committed)
bottom4_crime <- df %>% select(Category, X, Y) %>% 
  filter(Category %in% (df %>% select(Category) %>% 
                          group_by(Category) %>% count() %>% arrange(-n) %>% 
                          filter(n >= 500) %>%
                          tail(4) %>% pull(Category)))

#restrict top 4 crimes to Montgomery BART station
mont_bottom4_crime <- bottom4_crime %>% filter(between(X, -122.410, -122.395) & between(Y, 37.784, 37.794))

#bottom 4 crimes near Montgomery BART station (min. 500 crimes committed)
ggmap(mont_map) + 
  geom_point(data=mont_bottom4_crime, aes(x=X, y=Y, color=Category), size=2, alpha=1) + 
  ggtitle("Montgomery BART Station: Least occuring crimes (min. 500 crimes)") + 
  theme(plot.title = element_text(hjust = 0.5))

#bottom 4 crimes near Montgomery BART station seperated by category (min. 500 crimes committed)
##Why do I get a warning message (Removed 24 rows containing missing values)? Is it because my table is a little too big for my map?
##Is it possible to make my facet_wrap plots clearer? The non-facet_wrap version is clear.
ggmap(mont_map) + 
  geom_point(data=mont_bottom4_crime, aes(x=X, y=Y, color=Category), size=2, alpha=1) + 
  facet_wrap(~ Category) +
  ggtitle("Montgomery BART Station: Least occuring crimes (min. 500 crimes)") + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
        