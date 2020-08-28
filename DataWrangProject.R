library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system2("open", args = fn)
txt <- pdf_text(fn)
#2
x <- txt[9] %>% str_split("\n")
class(x)
length(x)

s<-x[[1]]
class(s)
length(s)

s<-str_trim(s)
s[1]

header_index <- str_which(s,"2015")[1]
header_index

month <- str_split(s[header_index], "\\s+", simplify = T)[1]
header <- str_split(s[header_index], "\\s+", simplify = T)[-1]


tail_index <- str_which(s, "Total")

n <- str_count(s,"\\d+")
sum(n ==1)
index_oneNum<- str_which(s, "^\\d+$")
remove_index <- c(1:header_index, index_oneNum, tail_index:length(s))
new_s <- s[-remove_index] 
length(new_s)

new_s <- str_split_fixed(new_s, "\\s+", n = 6)[,1:5]
tab <- new_s %>% as.data.frame %>% setNames(c("day",header)) %>% 
  mutate_at(c("day",header), as.numeric) %>% mutate(month = month)

tab %>% pull("2015") %>% mean
tab %>% summarize_at("2015", mean)
tab %>% summarize_at("2016", mean)
tab %>% filter(day %in% c(1:19)) %>% summarize_at("2017", mean)
tab %>% filter(day %in% c(20:30)) %>% summarize_at("2017", mean)

tab1 <- tab %>% gather(year,deaths, c(-day,-month)) %>%
  mutate(deaths = as.numeric(deaths))
tab1

tab1 %>%
  filter(!year ==2018) %>% ggplot(aes(day,deaths, color = year)) +
  geom_line() +
  geom_vline(xintercept = 20)
  