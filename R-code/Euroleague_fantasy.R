
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
  filter_at(c("Surname"), ~grepl("LAZ",.))


distinct(players,Team)
anadolou=c("#213557") # https://whatthelogo.com/logo/anadolu-efes/232612
real.madrid =c("#FEBE10") # gold https://whatthelogo.com/logo/real-madrid-club-crest-new/227629
cska.moscow =c("#D71920") # red https://whatthelogo.com/logo/pbc-cska-moscow/237542
barcelona = c("#8E224B") #purple https://whatthelogo.com/logo/barcelona-futbol/647
fener = c("#FFED00") #yellow https://whatthelogo.com/logo/fenerbahce-beko-basketbol/231475
olympiakos = c("#D4192D") #cherry https://whatthelogo.com/logo/olympiacos-basketball/239918
armani  =  c("#E2231A") #red https://whatthelogo.com/logo/olimpia-milano/237598
macabi = c("0076BC") #blue https://whatthelogo.com/logo/maccabi-electra-tel-aviv/239699
alba = c("#f7ca00") #yellow alba
zenit=c("#40A8DE") #skyblue https://whatthelogo.com/logo/fc-zenit-saint-petersburg/228568
unix=c("#03ad4f") #green
baskonia=c("#ac1b32") # deep red
bayern=c("#EE263C") #strawberry red
zalgiris = c("#2B7743") # dark green
pao = c("#00965D") # panathinaikos
red.star =c("#ED1C24") # cherry red
monaco = c("#CEA22D") # dull orange

install.packages("ggsci")
# learn this and pass through in R https://themockup.blog/static/slides/nfl-tidymodels.html#92

# I think this is cool and is exactly what you need, focucs
#https://nanx.me/ggsci/reference/pal_uchicago.html







install.packages("systemfonts", repos="https://mac.R-project.org", type="binary")


library(gt)
library(gtExtras)
library(dplyr)
library(htmltools)

# original source: https://www.bloomberg.com/graphics/2021-german-election-results/

party_df <- tibble(
  Party = c("SPD", "CDU/CSU", "Greens", "FDP", "AfD", "Left", "Other"),
  Seats = c(206, 196, 118, 92, 83, 39, 1),
  `% of 2nd Votes` = c(25.7, 24.1, 14.8, 11.5, 10.3, 4.9, 8.7)
)

minimal_table <- gt(party_df) %>%
  gt_plt_dot(column = Seats, category_column = Party,  max_value = 379,
             palette = c("#ec323f", "black", "#63d64a", "#fff24e", "#4fabf7", "#e956ad", "grey")) %>%
  gtExtras::gt_theme_nytimes() %>%
  tab_header(title = "Results by Party in the Bundestag Election",
             subtitle = "Seats and votes are based on provisional official results.") %>%
  cols_width(Party ~ px(368), 3 ~ px(30))

party_table <- gt(party_df) %>%
  gt_plt_dot(column = Seats, category_column = Party,  max_value = 368,
             palette = c("#ec323f", "black", "#63d64a", "#fff24e", "#4fabf7", "#e956ad", "grey")) %>%
  gtExtras::gt_theme_nytimes() %>%
  tab_header(title = "Results by Party in the Bundestag Election",
             subtitle = "Seats and votes are based on provisional official results.") %>%
  cols_width(Party ~ px(300), 3 ~ px(30)) %>%
  tab_style(style = list(cell_text(color = "grey"),cell_borders(color = "white")),
            locations = cells_body(3)) %>%
  tab_source_note(
    html(
      paste0(
        "With a total of 735 seats<br>",
        "<span style='color:#bfbfbf;'>Data as of: Sept 26, 2021, 11:09PM CDT</span>"
      )
    )
  ) %>%
  tab_style(style = cell_borders("right", "lightgrey", "dashed"),
            cells_body(Party)) %>%
  tab_style(style = cell_borders("top", "white"), cells_body(rows = 1)) %>%
  tab_options(table.border.bottom.color = "white")

combo_table <- htmltools::div(
  party_table,
  htmltools::div(
    "368 seats for majority",
    style = paste0(
      htmltools::css(
        background= "white", font.size = px(11), width = px(60),
        font.family = "arial", display = "flex", text.align = "center",
        color = "#999", position = "fixed", top = "230px", left = "290px"
      )
    )
  )
)


party_table
# to save as an img
gtExtras::gtsave_extra(combo_table, "combo-table.png", vwidth = 450, vheight = 430)


p2data <- "https://raw.githubusercontent.com/datavizpyr/data/master/Emmy_awards_summary_tidytuesday.tsv"
stream_data <- read_tsv(p2data)
stream_data %>%
  gt() %>%
  gt_plt_bullet(column = Nominee, target = Winner) %>%
  gt_fa_column(column = type)


team_df <- readRDS(url("https://github.com/nflverse/nflfastR-data/raw/master/teams_colors_logos.rds"))

team_df %>%
  dplyr::select(team_nick, team_abbr, team_conf, team_division, team_wordmark) %>%
  head(8)

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

