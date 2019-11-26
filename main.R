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

