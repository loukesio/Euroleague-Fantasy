
library(here)
library(ltc)
library(tidyverse)
library(xlsx)

setwd(here("data"))
coach <- read.xlsx(file ="161021_stats.xlsx", sheetIndex = 1, header = TRUE)
head(coach)

players <- read.xlsx(file ="161021_stats.xlsx", sheetIndex = 2, header = TRUE)

# check if there are players with the same and surname
# since they do not have give in each row a unique ID
players %>% group_by(Name, Surname) %>% filter(n() > 1)


players %>%
  select(-starts_with("NA")) %>%
  mutate(id = row_number()) %>%
  mutate_at(c("Total.score"), ~replace(.,.==0,000000.1)) %>%
  mutate(index=Total.score/Quotation) %>%
  arrange(desc(index)) %>%
  View()

set.seed(123)
players <- paste("player",rep(1:20))
score <- runif(20, min=4, max=16.7)
index <- runif(20, min=-1, max=9)
role <- rep(c("C","F","F","G","G"),4)

tfd <- as.data.frame(t(df))
combn(tfd, 10, simplify=FALSE)


df
df %>%
  complete(score, fill = list(weights=0))

combn(X.df, 2, simplify=FALSE)

library("RcppRoll")

# check the rolling sum
df %>%
  mutate(roll_sum = roll_sum(score, 10, align = "right", fill = NA))


df1 <- df[!apply(df, 1, function(x) any(duplicated(x))), ]

lapply(as.data.frame(combn(ncol(df) - 1, 3)), function(idx)
  df[, c(1, idx + 1)])



i<- combn(df$score, 10, sum) <= 100
head(combn(df$score, 10)[,i])


n <- 14
l <- rep(list(0:1), n)
expand.grid(l)
crossing(df)
expand(df)

df1 <- dplyr::tibble(
  group = c(1:2, 1),
  item_id = c(1:2, 2),
  item_name = c("a", "b", "b"),
  value1 = 1:3,
  value2 = 4:6
)
df1
df1 %>% complete(group, nesting(item_id, item_name))

