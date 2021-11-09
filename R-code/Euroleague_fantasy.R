################
# library ltc
################

library(here)
library(ltc)
library(tidyverse)
library(xlsx)
library(ggsci) # it has nice palettes
library(gtExtras)
library(gt)

#######################
# read euroleague data
########################

coach <- read.xlsx(here("data","071121_stats.xlsx"), sheetIndex = 1, header = TRUE)
head(coach)

players <- read.xlsx(here("data","071121_stats.xlsx"), sheetIndex = 2, header = TRUE)

# check if there are players with the same and surname
# since they do not have give in each row a unique ID
players %>% group_by(Name, Surname) %>% filter(n() > 1)

# here is the list of all the players
players <- players %>%
  select(-starts_with("NA")) %>%
  mutate(id = row_number()) %>%
  mutate_at(c("Total.score"), ~replace(.,.==0,000000.1)) %>%
  mutate(index=Total.score/Quotation) %>%
  arrange(desc(index))
  #filter_at(c("Surname"), ~grepl("LAZ",.))

### isolate each team
euro.teams <- distinct(players,Team) %>%
  arrange(Team)

# add the metadata to your data
# to find the colors I have used the whatthelogo.com website
metadata <- euro.teams %>%
  mutate(colors=c("#213557","#FEBE10","#D71920","#8E224B","#FFED00","#D4192D","#E2231A",
                  "#0076BC","#f7ca00","#40A8DE","#03ad4f","#ac1b32","#EE263C","#2B7743",
                  "#00965D","#ED1C24","#CEA22D","#333333")) %>%
  mutate(team_logo=c("https://raw.githubusercontent.com/loukesio/Euroleague-Fantasy/main/data/logos/Alba.png",
                     "https://raw.githubusercontent.com/loukesio/Euroleague-Fantasy/main/data/logos/Efes.png",
                     "https://raw.githubusercontent.com/loukesio/Euroleague-Fantasy/main/data/logos/Monaco.png",
                     "https://raw.githubusercontent.com/loukesio/Euroleague-Fantasy/main/data/logos/Armani.png",
                     "https://raw.githubusercontent.com/loukesio/Euroleague-Fantasy/main/data/logos/Baskonia.png",
                     "https://raw.githubusercontent.com/loukesio/Euroleague-Fantasy/main/data/logos/Crvena_zvezda.png",
                     "https://raw.githubusercontent.com/loukesio/Euroleague-Fantasy/main/data/logos/Cska.png",
                     "https://raw.githubusercontent.com/loukesio/Euroleague-Fantasy/main/data/logos/Barcelona.png",
                     "https://raw.githubusercontent.com/loukesio/Euroleague-Fantasy/main/data/logos/Bayern.png",
                     "https://raw.githubusercontent.com/loukesio/Euroleague-Fantasy/main/data/logos/Fener.png",
                     "https://raw.githubusercontent.com/loukesio/Euroleague-Fantasy/main/data/logos/Asvel.png",
                     "https://raw.githubusercontent.com/loukesio/Euroleague-Fantasy/main/data/logos/Macabi.png",
                     "https://raw.githubusercontent.com/loukesio/Euroleague-Fantasy/main/data/logos/Olympiakos.png",
                     "https://raw.githubusercontent.com/loukesio/Euroleague-Fantasy/main/data/logos/Panathinaikos.png",
                     "https://raw.githubusercontent.com/loukesio/Euroleague-Fantasy/main/data/logos/Real.png",
                     "https://raw.githubusercontent.com/loukesio/Euroleague-Fantasy/main/data/logos/Unics.png",
                     "https://raw.githubusercontent.com/loukesio/Euroleague-Fantasy/main/data/logos/Zalgiris.png",
                     "https://raw.githubusercontent.com/loukesio/Euroleague-Fantasy/main/data/logos/Zenit.png" ))

###############################
# connect metadata and player
###############################
all.data <- left_join(players,metadata,by=c("Team"))
metadata

top5 <- all.data %>%
  arrange(desc(index)) %>%
  rename(`Total score`=Total.score, Teams=team_logo) %>%
  select(Surname,Team,`Total score`,Role,Quotation,index,Teams) %>%
  group_by(Role) %>% slice_max(order_by = index, n = 5)


top5 %>%
  gt(groupname_col = "Role") %>%
  gt_merge_stack(col1 = Surname, col2 = Team) %>%
  gt_img_rows(Teams) %>%
  gtsave(
    here("data","Round9.png"),
    vwidth = 815,
    vheight = 931
  )

###############################
# best 10 players
###############################

best10 <-top5 %>%
  select(Surname,`Total score`,Quotation,Role)

repeat {
  idx <- unlist(
    Map(
      sample,
      split(1:nrow(best10), best10$Role),
      c(2, 4, 4)
    )
  )
  s <- sum(best10$score[idx])
  if (s >= 95.5 & s <= 100.4) break
}

head(best10)
best10 %>% split(.$Role) -> best10_split
C <- best10_split$Center$Surname
F <- best10_split$Forward$Surname
G <- best10_split$Guard$Surname

best10$Role
head(best10)

C_comb <- combn(C, 2) %>% t %>% as_tibble()
F_comb <- combn(F, 4) %>% t %>% as_tibble()
G_comb <- combn(G, 4) %>% t %>% as_tibble()

crossing(C_comb, F_comb, G_comb, .name_repair = "unique") %>%
  mutate(sim_num = row_number()) %>%
  pivot_longer(-sim_num) %>%
  left_join(best10, by = c("value" = "Surname")) %>%
  group_by(sim_num) %>%
  mutate(Total.score = sum(score)) %>%
  ungroup() %>%
  filter(Total.score >= 95.5, score <= 100.4)


# anadolou=c("#213557") # https://whatthelogo.com/logo/anadolu-efes/232612
# real.madrid =c("#FEBE10") # gold https://whatthelogo.com/logo/real-madrid-club-crest-new/227629
# cska.moscow =c("#D71920") # red https://whatthelogo.com/logo/pbc-cska-moscow/237542
# barcelona = c("#8E224B") #purple https://whatthelogo.com/logo/barcelona-futbol/647
# fener = c("#FFED00") #yellow https://whatthelogo.com/logo/fenerbahce-beko-basketbol/231475
# olympiakos = c("#D4192D") #cherry https://whatthelogo.com/logo/olympiacos-basketball/239918
# armani  =  c("#E2231A") #red https://whatthelogo.com/logo/olimpia-milano/237598
# macabi = c("#0076BC") #blue https://whatthelogo.com/logo/maccabi-electra-tel-aviv/239699
# alba = c("#f7ca00") #yellow alba
# zenit=c("#40A8DE") #skyblue https://whatthelogo.com/logo/fc-zenit-saint-petersburg/228568
# unix=c("#03ad4f") #green
# baskonia=c("#ac1b32") # deep red
# bayern=c("#EE263C") #strawberry red
# zalgiris = c("#2B7743") # dark green
# pao = c("#00965D") # panathinaikos
# red.star =c("#ED1C24") # cherry red
# monaco = c("#CEA22D") # dull orange
# asvel =c("#333333")

#_______________________________________________________________________________
#                                           _           _    _
#  __ _  ___  _ _  _ __   __ _  _ _    ___ | | ___  __ | |_ (_) ___  _ _   ___
# / _` |/ -_)| '_|| '  \ / _` || ' \  / -_)| |/ -_)/ _||  _|| |/ _ \| ' \ (_-<
# \__, |\___||_|  |_|_|_|\__,_||_||_| \___||_|\___|\__| \__||_|\___/|_||_|/__/
# |___/
#_______________________________________________________________________________





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

