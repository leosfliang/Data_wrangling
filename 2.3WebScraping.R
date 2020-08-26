# import a webpage into R
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h

tab <- h %>% html_nodes("table")
tab <- tab[[2]]

tab <- tab %>% html_table
class(tab)

tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)

#the guacamole recipe page, we already have done this and determined that we need the following selectors
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
direction <- h %>% html_node(".o-Method__m-Step") %>% html_text()

guacamole <- list(recipe,prep_time,ingredients,direction)
guacamole

#function for extracting infor in FOODnetwork
get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
  direction <- h %>% html_node(".o-Method__m-Step") %>% html_text()
  return(list(recipe,prep_time,ingredients,direction))
}

get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")


### Assessment
library(tidyverse)
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_text(nodes[[8]])
html_table(nodes[[4]])
html_table(nodes[[5]])
html_table(nodes[[6]])

html_table(nodes[[length(nodes)-2]])
html_table(nodes[[length(nodes)-1]])
html_table(nodes[[length(nodes)]])

tab_1 <- html_table(nodes[[10]])
tab_2 <- html_table(nodes[[19]])
tab_2 <- tab_2 %>% slice(-1)
tab_1 <- tab_1 %>% select(-X1)
tab_1 <- tab_1 %>% slice(-1)
colnames(tab_1) = c("Team", "Payroll", "Average")
colnames(tab_2) = c("Team", "Payroll", "Average")
tab_fljn<- full_join(tab_1,tab_2, by = "Team")
tab_fljn %>% filter(!is.na(Payroll.x) | !is.na(Payroll.y)) %>% nrow

#q5 brexit
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
nodes <- html_nodes(h, "table")
length(nodes)
head(html_table(nodes[[1]], fill = TRUE))
head(html_table(nodes[[2]], fill = TRUE))
head(html_table(nodes[[3]], fill = TRUE))
head(html_table(nodes[[4]], fill = TRUE))
head(html_table(nodes[[5]], fill = TRUE))
head(html_table(nodes[[6]], fill = TRUE))
head(html_table(nodes[[7]], fill = TRUE))


