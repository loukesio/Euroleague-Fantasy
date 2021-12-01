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

coach <- read.xlsx(here("data","041121_stats.xlsx"), sheetIndex = 1, header = TRUE)
head(coach)

players <- read.xlsx(here("data","041121_stats.xlsx"), sheetIndex = 2, header = TRUE)

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
    here("data","Round5.png")
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
