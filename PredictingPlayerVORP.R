# The basic goal is find which variables do the best in predicting Box Plus Minus and VORP.
# If we can reasonably predict a player's VORP, we can make a reliable prediction of SRS and W-L.
# We need to start by scraping a few sets of data from 1976-1977 to 2016.
# 1. Player's vitals (height, draft position, etc.)
# 2. Box stats
# 3. Advanced stats
# Box and Advanced stats should be easy--bbref includes season summaries for both.
# Vitals likely need to be pulled the draft index,
# Those without draft statuses can be labeled undrafted or int'l free agent.
# Let's begin with the draft index, since it will also include some key career stats for each player.
# We won't have height, but we can always scrape individual pages later on.
library(rvest)
library(ggplot2)
library(stringr)
library(randomForest)
library(dplyr)
library(caret)
library(doSNOW)
library(e1071)
library(parallel)
library(rpart)
library(rpart.plot)
library(DAAG)
library(reshape2)
library(heavy)

# KEY DFS
# years_pro_df for all info (unfiltered)
# years_pro_df for Totals (excluding individual team breakdowns for traded/released players)
# yearly_VORP_df for year by year VORP




scrapeDraftStats <- function(year_first, year_last) {
  # Setting the year range and creating URLs to be scraped
  year_range <- year_first:year_last
  current_year <- year_range[1]
  bbr_URLs <- NULL
  for (i in year_range) {
    bbr_link <- paste("http://www.basketball-reference.com/draft/NBA_", current_year, ".html", sep = "")
    bbr_URLs <- c(bbr_URLs, bbr_link)
    current_year <- current_year + 1
  }
  draft_player_stats_df <- NULL
  year_range <- year_first:year_last
  current_year <- year_range[1]
  # Goes through each URL and scrapes each table for regular season
  for (i in bbr_URLs) {
    link <- read_html(i)
    current_link <- link %>%
      html_nodes("table") %>%
      html_table(fill = TRUE)
    current_table <- current_link[[1]]
    current_table["Season"] <- as.integer(current_year)
    current_year <- current_year + 1
    #print(current_year)
    draft_player_stats_df <- rbind(draft_player_stats_df, current_table)
  }
  # exporting data-frame
  draft_player_stats_df <<- data.frame(draft_player_stats_df)
}
scrapeDraftStats(1976, 2016)
# Awesome! Now to clean it up.
colnames(draft_player_stats_df) <- draft_player_stats_df[1, ]
draft_player_stats_df <- draft_player_stats_df[-1, ]
draft_player_stats_df <- draft_player_stats_df[!(draft_player_stats_df$Rk==""),]
draft_player_stats_df <- draft_player_stats_df[!(draft_player_stats_df$Rk=="Rk"),]
colnames(draft_player_stats_df)[23] <- "DraftYear"
draft_player_stats_df$ID <- seq.int(nrow(draft_player_stats_df))
# Changing appropriate columns to numeric
cols <- c(1,2,6:22)
for (i in cols) {
  draft_player_stats_df[[i]] <- as.numeric(draft_player_stats_df[[i]])
}
# We also need to take care of duplicate names, players being drafted multiple times, etc.
dup_draft <- draft_player_stats_df[duplicated(draft_player_stats_df$Player),]
#IDs to delete <- 117,150,264, 155, 37, 121, 325,448, 102, 467, 201,655,486,660,692,696,741,861, 881,882,891,643,944,956, 652, 144,660,141,
# to delete <- 1506, 1940, 1905, 659, 1970, 1573
remove_ids <- c(117,150,264, 155, 37, 121, 325,448, 102, 467, 201,655,486,660,692,696,741,861, 881,882,891,643,944,956, 652, 144,660,141, 1506, 1940, 1905, 659, 1970, 1573)
test_draft <- draft_player_stats_df
test_draft <- test_draft[!(test_draft$ID %in% remove_ids),]
name_ids <- c(647,856,222,990,1030,1040,1392,1404,1451,1493,1502,1513,1605,1671,1550,1739,1830,1914,2070,2076,2210,2268,2276,2292,2306,2420,
              2421,2475,2486,2525,197,2583,2728,2795,2848,2889,2907,3071,3142,99,3154,3195,3431,2971,3478,3548,407,3577,3766,2437,3829,2427,3840,3970)
replacement_names <- c("Larry Williams Two","Ken Jones Two","Eddie Lee Johnson","Eddie Arnet Johnson","John Johnson Two","Ron Davis Two",
                       "Michael Edwards Two",
                       "Mike Phillips Two",
                       "Mike Davis Two",
                       "Mike Jackson Two",
                       "Craig Robinson Two",
                       "Charles Jones Two",
                       "Charles Jones Three",
                       "Charles Jones Four",
                       "Tony Brown Two",
                       "Terry Martin Two",
                       "Fred Brown Two",
                       "Charles Bradley Two",
                       "Keith Smith Two",
                       "Mike Williams Two",
                       "Greg Anderson Two",
                       "Scott Thompson Two",
                       "David Johnson Two",
                       "Terry Williams Two",
                       "Ricky Brown Two",
                       "Lee Johnson Two",
                       "Michael Anderson Two",
                       "Greg Grant Two",
                       "Willie Burton Two",
                       "Tony Smith Two",
                       "Larry Johnson One",
                       "Anthony Jones Two",
                       "Michael Smith Two",
                       "Mark Davis Two",
                       "Ben Davis Two",
                       "Charles Smith Two",
                       "Cedric Henderson Two",
                       "Mike Smith Two",
                       "Ken Johnson Two",
                       "Mike Dunleavy Sr",
                       "Mike Dunleavy Jr",
                       "Sam Clancy Two",
                       "Dee Brown Two",
                       "Corey Brewer One",
                       "Marcus Williams Two",
                       "Patrick Ewing Jr",
                       "Gerald Henderson Sr",
                       "Gerald Henderson Jr",
                       "Jeffrey Taylor",
                       "Tim Hardaway Sr",
                       "Tim Hardaway Jr",
                       "Glen Rice Sr",
                       "Glen Rice Jr",
                       "Marcus Thornton Two")
# 647 Larry Williams Two
# 856 Ken Jones Two
# 222 Eddie Lee Johnson
# 990 Eddie Arnet Johnson
# 1030 John Johnson Two
# 1040 Ron Davis Two
# 1392 Michael Edwards Two
# 1404 Mike Phillips Two
# 1451 Mike Davis Two
# 1493 Mike Jackson Two
# 1502 Craig Robinson Two
# 1513 Charles Jones Two
# 1605 Charles Jones Three
# 1671 Charles Jones Four
# 1550 Tony Brown Two
# 1739 Terry Martin Two
# 1830 Fred Brown Two
# 1914 Charles Bradley Two
# 2070 Keith Smith Two
# 2076 Mike Williams Two
# 2210 Greg Anderson Two
# 2268 Scott Thompson Two
# 2276 David Johnson Two
# 2292 Terry Williams Two
# 2306 Ricky Brown Two
# 2420 Lee Johnson Two
# 2421 Michael Anderson Two
# 2475 Greg Grant Two
# 2486 Willie Burton Two
# 2528 Tony Smith Two
# 197 Larry Johnson One
# 2583 Anthony Jones Two
# 2728 Michael Smith Two
# 2795 Mark Davis Two
# 2848 Ben Davis Two
# 2889 Charles Smith Two
# 2907 Cedric Henderson Two
# 3071 Mike Smith Two
# 3142 Ken Johnson Two
# 99 Mike Dunleavy Sr
# 3154 Mike Dunleavy Jr
# 3195 Sam Clancy Two
# 3431 Dee Brown Two
# 2971 Corey Brewer One
# 3478 Marcus Williams Two
# 3548 Patrick Ewing Jr
# 407 Gerald Henderson Sr
# 3577 Gerald Henderson Jr
# 3776 Jeffery Taylor
# 2437 Tim Hardaway Sr
# 3829 Tim Hardaway Jr
# 2427 Glen Rice Sr
# 3840 Glen Rice Jr
# 3970 Marcus Thornton Two
# NEED TO CHANGE Jeffrey Taylor to Jeffery Taylor!
write.csv(test_draft, "draft_test_df.csv", row.names = TRUE)
test_draft <- read.csv("draft_test_df.csv", header = TRUE)
dup_check <- test_draft[duplicated(test_draft$Player),]              
# No duplicates--good to remove and overwrite old df
rm(dup_check)
test_draft <- test_draft[, -1]
draft_player_stats_df <- test_draft

# Now box stats
scrapeBoxStats <- function(year_first, year_last) {
  # Setting the year range and creating URLs to be scraped
  year_range <- year_first:year_last
  current_year <- year_range[1]
  bbr_URLs <- NULL
  for (i in year_range) {
    bbr_link <- paste("http://www.basketball-reference.com/leagues/NBA_", current_year, "_totals.html", sep = "")
    bbr_URLs <- c(bbr_URLs, bbr_link)
    current_year <- current_year + 1
  }
  box_player_stats_df <- NULL
  year_range <- year_first:year_last
  current_year <- year_range[1]
  # Goes through each URL and scrapes each table for regular season
  for (i in bbr_URLs) {
    link <- read_html(i)
    current_link <- link %>%
      html_nodes("table") %>%
      html_table(fill = TRUE)
    current_table <- current_link[[1]]
    current_table["Season"] <- as.integer(current_year)
    current_year <- current_year + 1
    #print(current_year)
    box_player_stats_df <- rbind(box_player_stats_df, current_table)
  }
  # exporting data-frame
  box_player_stats_df <<- data.frame(box_player_stats_df)
}
scrapeBoxStats(1977, 2016)
# Excellent, now cleaning.
box_player_stats_df <- box_player_stats_df[!(box_player_stats_df$Rk=="Rk"),]
cols <- c(1,4,6:30)
for (i in cols) {
  box_player_stats_df[[i]] <- as.numeric(box_player_stats_df[[i]])
}
write.csv(box_player_stats_df, "box_player_stats_df.csv")

# Now for advanced stats
scrapeAdvancedStats <- function(year_first, year_last) {
  # Setting the year range and creating URLs to be scraped
  year_range <- year_first:year_last
  current_year <- year_range[1]
  bbr_URLs <- NULL
  for (i in year_range) {
    bbr_link <- paste("http://www.basketball-reference.com/leagues/NBA_", current_year, "_advanced.html", sep = "")
    bbr_URLs <- c(bbr_URLs, bbr_link)
    current_year <- current_year + 1
  }
  advanced_player_stats_df <- NULL
  year_range <- year_first:year_last
  current_year <- year_range[1]
  # Goes through each URL and scrapes each table for regular season
  for (i in bbr_URLs) {
    link <- read_html(i)
    current_link <- link %>%
      html_nodes("table") %>%
      html_table(fill = TRUE)
    current_table <- current_link[[1]]
    current_table["Season"] <- as.integer(current_year)
    current_year <- current_year + 1
    #print(current_year)
    advanced_player_stats_df <- rbind(advanced_player_stats_df, current_table)
  }
  # exporting data-frame
  advanced_player_stats_df <<- data.frame(advanced_player_stats_df)
}
scrapeAdvancedStats(1977, 2016)

advanced_player_stats_df <- advanced_player_stats_df[!(advanced_player_stats_df$Rk=="Rk"),]
cols <- c(1,4,6:29)
for (i in cols) {
  advanced_player_stats_df[[i]] <- as.numeric(advanced_player_stats_df[[i]])
}
write.csv(advanced_player_stats_df, "advanced_player_stats_df.csv")
# Excellent--we've scraped all the needed stats for now.
# Let's do some quick visualization of some the vectors here.
# VORP vs. draft position
qplot(draft_player_stats_df$Pk, draft_player_stats_df$VORP)
# Fairly clear exponential relationship between draft position and career VORP
# How about BPM vs. draft position
qplot(draft_player_stats_df$Pk, draft_player_stats_df$BPM)
# Slight relationship here.
# I think we can conclude that draft position will be a factor in projecting career VORP.
# Playing around with ggplot
m_j_stats <- advanced_player_stats_df[(advanced_player_stats_df$Player=="LeBron James"),]
ggplot(m_j_stats, aes(x=Season, y=VORP)) +
  geom_area()
rm(m_j_stats)
# NOTE: Hall of Famers seem to have an asterisk after their name. Might need to remove.
# Let's pick a few draft classes. Analyze the first four years of those careers and then ask what was predictive of career success.
# We can create a few factors like first three VORP, VORP improvement, BPM improvement in defense etc.
# Let's start by examining the 2003 draft class. (Just to play around before doing a real examination)
advanced_2004to2016 <- advanced_player_stats_df[(advanced_player_stats_df$Season>=2004),]
# We should probably remove the multiple entries from players who switch teams during the season. We only need the TOT.
removeMultipleTeamEntries <- function() {
  advanced_2004to2016$Season <- as.factor(advanced_2004to2016$Season)
  player_list <- NULL
  for (j in levels(advanced_2004to2016$Season)) {
    current_season <- advanced_2004to2016[(advanced_2004to2016$Season==j),]
    for (i in 1:nrow(current_season)) {
      current_player <- current_season[i,2]
      current_player_df <- current_season[(current_season$Player==current_player),]
      player_list <- rbind(player_list, current_player_df[1,])
    }
  }
  player_list <<- data.frame(unique(player_list))
}
removeMultipleTeamEntries()
advanced_2004to2016 <- player_list
rm(player_list)
# Getting all names the same
test_adv <- read.csv("advanced_player_stats_df.csv", header = TRUE)
test_adv <- test_adv[, -1]
#advanced_player_stats_df <- test_adv
#box_player_stats_df$Player <- advanced_player_stats_df$Player
# Names are all the same now.
# Asterisks for HoF people is an issue. Let's add in a column that specifies in the player is in the Hof.
# We can then delete the HoF asterisk.
test_adv$HoF <- ifelse(str_detect(test_adv$Player, "[*]"), 1, 0)
test_adv$Player <- gsub("[*]", "", test_adv$Player)
advanced_player_stats_df$Player <- test_adv$Player
box_player_stats_df$Player <- test_adv$Player
advanced_player_stats_df$HoF <- test_adv$HoF
box_player_stats_df$HoF <- test_adv$HoF

# Grabbing older drafts more complete information
scrapeDraftStatsTwo <- function(year_first, year_last) {
  # Setting the year range and creating URLs to be scraped
  year_range <- year_first:year_last
  current_year <- year_range[1]
  bbr_URLs <- NULL
  for (i in year_range) {
    bbr_link <- paste("http://www.basketball-reference.com/draft/NBA_", current_year, ".html", sep = "")
    bbr_URLs <- c(bbr_URLs, bbr_link)
    current_year <- current_year + 1
  }
  draft1967to1975_player_stats_df <- NULL
  year_range <- year_first:year_last
  current_year <- year_range[1]
  # Goes through each URL and scrapes each table for regular season
  for (i in bbr_URLs) {
    link <- read_html(i)
    current_link <- link %>%
      html_nodes("table") %>%
      html_table(fill = TRUE)
    current_table <- current_link[[1]]
    current_table["Season"] <- as.integer(current_year)
    current_year <- current_year + 1
    print(current_year)
    draft1967to1975_player_stats_df <- rbind(draft1967to1975_player_stats_df, current_table)
  }
  # exporting data-frame
  draft1967to1975_player_stats_df <<- data.frame(draft1967to1975_player_stats_df)
}
scrapeDraftStatsTwo(1967, 1975)
colnames(draft1967to1975_player_stats_df) <- draft1967to1975_player_stats_df[1, ]
draft1967to1975_player_stats_df <- draft1967to1975_player_stats_df[-1, ]
draft1967to1975_player_stats_df <- draft1967to1975_player_stats_df[!(draft1967to1975_player_stats_df$Rk==""),]
draft1967to1975_player_stats_df <- draft1967to1975_player_stats_df[!(draft1967to1975_player_stats_df$Rk=="Rk"),]
colnames(draft1967to1975_player_stats_df)[23] <- "DraftYear"
draft1967to1975_player_stats_df$ID <- seq.int(nrow(draft1967to1975_player_stats_df))
# Changing appropriate columns to numeric
cols <- c(1,2,6:22)
for (i in cols) {
  draft1967to1975_player_stats_df[[i]] <- as.numeric(draft1967to1975_player_stats_df[[i]])
}
# We should add in a column that specifies when the player was drafted (or brought into the league)
addDraftYear <- function() {
  current_year <- 1976
  draft_year_list <- NULL
  for (i in 1:41) {
    draft_selection <- draft_player_stats_df[(draft_player_stats_df$DraftYear==current_year),]
    player_selection <- test_adv[(test_adv$Season==(current_year+1)),]
    draft_entry <- ifelse(player_selection$Player %in% draft_selection$Player, current_year , NA)
    draft_year_list <- c(draft_year_list, draft_entry)
    current_year <- current_year + 1
    #print(current_year)
  }
  test_adv$DraftYear <<- draft_year_list
}
addDraftYear()
# Problems with above function--missing when players came over late.
addDraftYearTwo <- function() {
  player_list <- unique(test_adv$Player)
  new_draft_df <- NULL
  player_Player <- NULL
  player_DraftYear <- NULL
  player_DraftPk <- NULL
  draft_player_stats_df$Player <- as.character(draft_player_stats_df$Player)
  for (p in player_list) {
    player_insert <- NULL
    if (p %in% draft1967to1975_player_stats_df$Player) {
      player <- draft1967to1975_player_stats_df[(draft1967to1975_player_stats_df$Player==p),]
      player_Player <- c(player_Player, player$Player)
      player_DraftYear <- c(player_DraftYear, player$DraftYear)
      player_DraftPk <- c(player_DraftPk, player$Pk)
    }
    else if (p %in% draft_player_stats_df$Player) {
      player <- draft_player_stats_df[(draft_player_stats_df$Player==p),]
      player_Player <- c(player_Player, player$Player)
      player_DraftYear <- c(player_DraftYear, player$DraftYear)
      player_DraftPk <- c(player_DraftPk, player$Pk)
    }
    else {
      player <- test_adv[(test_adv$Player==p),]
      player_Player <- c(player_Player, player[1,2])
      player_DraftYear <- c(player_DraftYear, 0)
      player_DraftPk <- c(player_DraftPk, 0)
    }
  }
  new_draft_df <<- data.frame(Player=player_Player, Pk=player_DraftPk, DraftYear=player_DraftYear)
}
addDraftYearTwo()
# Much better!
# But we are still missing draft info for players from 1977. We can fix that by scraping earlier draft info.
# We need to go through and manually enter some information for players from drafts before 1967
# (Note: We didn't scrape those earlier drafts because of complex formatting issues that would have taken more time than neccesary to fix)
write.csv(new_draft_df, "new_draft_df.csv", row.names = TRUE)
new_draft_df <- read.csv("new_draft_df.csv", header = TRUE)
# GREAT! Do not edit new_draft_df
# Looks good. We only really need a "years pro" designation now.
# Updating with new draft info for a rough estimate of yearspro for players in 1977
#holder_yp_df <- years_pro_df
#holder_yp_df <- holder_yp_df[,-(30:32)]
holder_yp_df <- merge(holder_yp_df,new_draft_df)
holder_yp_df <- holder_yp_df[with(holder_yp_df, order(Season, Rk)), ]
holder_yp_df <- holder_yp_df[,-30]

addYearsPro <- function() {
  years_pro_df <- NULL
  player_list <- unique(holder_yp_df$Player)
  #holder_yp_df$YearsPro <- NA
  for (p in player_list) {
    player_selection <- holder_yp_df[(holder_yp_df$Player==p),]
    counter <- 1
    print(p)
    for (s in 1:nrow(player_selection)) {
      if (counter > 1) {
        current_year <- player_selection[counter,28]
        previous_year <- player_selection[(counter-1),28]
        previous_yp <- player_selection[(counter-1),32]
        if (current_year==previous_year) {
          player_selection[counter,32] <- previous_yp
          years_pro_df <- rbind(years_pro_df,player_selection[counter,])
          counter <- counter + 1
        }
        else {
          player_selection[counter,32] <- previous_yp + 1
          years_pro_df <- rbind(years_pro_df,player_selection[counter,])
          counter <- counter + 1
        }
      }
      else {
        if (player_selection[1,28]==1977) {
          if (player_selection[1,31]>0) {
            player_selection[1,32] <- (1977 - player_selection[1,31])
            years_pro_df <- rbind(years_pro_df,player_selection[counter,])
            counter <- counter + 1
          }
          else {
            player_selection[1,32] <- (player_selection[1,4]-22)
            years_pro_df <- rbind(years_pro_df,player_selection[counter,])
            counter <- counter + 1
          }
        }
        else {
          player_selection[1,32] <- 1
          years_pro_df <- rbind(years_pro_df,player_selection[counter,])
          counter <- counter + 1
        }
      }
    }
  }
  years_pro_df_holder <<- data.frame(years_pro_df)
}
addYearsPro()
years_pro_df <- years_pro_df_holder
colnames(years_pro_df)[32] <- "YearsPro"
years_pro_df <- years_pro_df[with(years_pro_df, order(Season, Rk)), ]
# Awesome, we just need to fix some players with odd numbers (undrafted 70s players who came in early.)
write.csv(years_pro_df, "years_pro_df.csv")
years_pro_df <- read.csv("years_pro_df.csv", header = TRUE)
years_pro_df <- years_pro_df[,-1]
not_drafted <- years_pro_df[(years_pro_df$DraftYear==0),]
# Excellent!!! We now have years_pro, draft year, and pk taken care of. 70s era stuff is fixed.

# Now let's get a df that only includes Total stats for each player. years_pro_df will have the full information.
extractingPlayerTotals <- function() {
  tot_list <- NULL
  for (p in unique(years_pro_df$Player)) {
    #print(p)
    player <- years_pro_df[(years_pro_df$Player==p),]
    for (s in unique(player$Season)) {
      #print(s)
      season_selection <- player[(player$Season==s),]
      tot_list <- rbind(tot_list, season_selection[1,])
    }
  }
  years_pro_df_TOT <<- data.frame(tot_list)
}
extractingPlayerTotals()
# Looks right.

# UPDATE: We are prorating VORP, G, MP, and the three win shares totals (not ws.48) for the 1999 and 2012 lockout shortened seasons. We won't be touching any other stats for now.
years_pro_df_TOT_NOT_PRORATED <- years_pro_df_TOT
prorateLockoutSeasons <- function() {
  season_holder <- NULL
  for (i in unique(years_pro_df_TOT$Season)) {
    season <- years_pro_df_TOT[(years_pro_df_TOT$Season==i),]
    if (i==1999) {
      season$VORP <- round((season$VORP*1.6), digits = 1)
      season$OWS <- round((season$OWS*1.6), digits = 1)
      season$DWS <- round((season$DWS*1.6), digits = 1)
      season$WS <- round((season$WS*1.6), digits = 1)
      season$MP <- round((season$MP*1.6), digits = 0)
      season$G <- round((season$G*1.6), digits = 0)
      season_holder <- rbind(season_holder, season)
    }
    else if (i==2012) {
      season$VORP <- round((season$VORP*1.212121), digits = 1)
      season$OWS <- round((season$OWS*1.212121), digits = 1)
      season$DWS <- round((season$DWS*1.212121), digits = 1)
      season$WS <- round((season$WS*1.212121), digits = 1)
      season$MP <- round((season$MP*1.212121), digits = 0)
      season$G <- round((season$G*1.212121), digits = 0)
      season_holder <- rbind(season_holder, season)
    }
    else {
      season_holder <- rbind(season_holder, season)
    }
  }
  years_pro_df_TOT <<- season_holder
}
prorateLockoutSeasons()
# Looks good, now to update the yearly_VORP_df and associated rfs

# Let's do visualization for now.
advanced_1985to2016 <- years_pro_df[(years_pro_df$Season>1984),]
advanced_1985to2016$Pk <- as.numeric(advanced_1985to2016$Pk)
qplot(advanced_1985to2016$Age, advanced_1985to2016$VORP)
# Initial guesses on key signals: Pk, first four years of VORP, BPM, TS.

rf.train.1 <- advanced_1985to2016[c("Pk","MP","BPM")]
rf.label <- advanced_1985to2016$VORP
set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

# Let's use Pk, MP, BPM, and first four years of VORP to try to predict the next few years of VORP

# Start by selecting players with 8 or more years experience
# This is just an intial entry--Selecting players with 8 or more years of experience is obviously an issue since players who stay in the league longer are likely to be good.
players_with1year <- years_pro_df[(years_pro_df$YearsPro==1),]
players_with1year$Player <- as.character(players_with1year$Player)
players_with8years <- years_pro_df[(years_pro_df$YearsPro==8),]
players_with8years$Player <- as.character(players_with8years$Player)
players_1to8years <- players_with1year[(players_with1year[,1] %in% players_with8years[,1]),]
players_1to8years <- unique(players_1to8years$Player)
#print(players_1to8years)
addVORPEightYearTotals <- function() {
  four_year_list <- NULL
  second_four_year_list <- NULL
  for (p in players_1to8years) {
    player_selection <- years_pro_df_TOT[(years_pro_df_TOT$Player==p),]
    four_years <- player_selection[(player_selection$YearsPro<=4),]
    print(four_years)
    second_four_years <- player_selection[(player_selection$YearsPro>=5 & player_selection$YearsPro<9),]
    four_year_list <- c(four_year_list, round(sum(four_years$VORP), digits = 1))
    second_four_year_list <- c(second_four_year_list, round(sum(second_four_years$VORP), digits = 1))
  }
  eight_year_VORP_df <<- data.frame(Player=players_1to8years, four_year_VORP=four_year_list, second_four_years_VORP=second_four_year_list)
}
addVORPEightYearTotals()
# Looks good after a few checks.
years_pro_df_EightYears <- merge(years_pro_df_TOT,eight_year_VORP_df)

# Ok, we can now use the features of age, draft pick, MP, BPM, and first four years of VORP against the second four years of VORP
# Hold on: We should really be averaging the first four years of stats, as well as the next four years of stats
rf.train.2 <- years_pro_df_EightYears[c("Age","Pk","X3PAr","TRB.","TS.","AST.","STL.","BLK.","TOV.","USG.","four_year_VORP", "second_four_years_VORP")]
rf.train.2 <- rf.train.2[complete.cases(rf.train.2),]
rf.label.2 <- rf.train.2$second_four_years_VORP
rf.train.2 <- rf.train.2[,-12]
set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label.2, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)
# OOB REPORTS!:
# Age, Pk, BPM, 4 year VORP = 80.86%
# Age, Pk, 4 year VORP = 83.59%
# 4 year VORP = 74.59%
# Age, Pk, TRB., TS., AST., STL., BLK., TOV., USG., four year VORP = 85.93%
# Age, Pk, X3PAr, TRB., TS., AST., STL., BLK., TOV., USG., four year VORP
# We should also add in a column that totals the VORP for each year the player has played. This will make building a predictive mdoel that much easier.
# YearOneVORP, YearTwoVORP...
AddYearlyVORP <- function() {
  yearly_VORP_df <- data.frame(unique(years_pro_df_TOT$Player))
  colnames(yearly_VORP_df)[1] <- "Player"
  yearly_VORP_df$Player <- as.character(yearly_VORP_df$Player)
  counter <- 1
  for (p in yearly_VORP_df$Player) {
    player <- years_pro_df_TOT[(years_pro_df_TOT$Player==p),]
    yp_min <- min(player$YearsPro)
    yp_max <- max(player$YearsPro)
    for (i in 1:21) {
      if (i < yp_min) {
        yearly_VORP_df[counter,(i+1)] <- NA
      }
      else if (i > yp_max) {
        yearly_VORP_df[counter,(i+1)] <- NA
      }
      else {
        player_refine <- player[(player$YearsPro==i),]
        years_pro_current <- player_refine$YearsPro
        yearly_VORP_df[counter, (years_pro_current+1)] <- player_refine$VORP
      }
    }
    counter <- counter + 1
  }
  yearly_VORP_df <<- yearly_VORP_df
}
AddYearlyVORP()
# All fixed! Good, we just need to adjust colnames
colnames(yearly_VORP_df)[2:22] <- c("Year1VORP","Year2VORP","Year3VORP","Year4VORP","Year5VORP","Year6VORP","Year7VORP","Year8VORP","Year9VORP","Year10VORP",
                                    "Year11VORP","Year12VORP","Year13VORP","Year14VORP","Year15VORP","Year16VORP","Year17VORP","Year18VORP","Year19VORP","Year20VORP",
                                    "Year21VORP")
# We'll likely need to subset the data from each season to match it to the VORP
vorp_onetothree_df <- yearly_VORP_df[,1:5]
vorp_onetothree_df <- vorp_onetothree_df[complete.cases(vorp_onetothree_df),]
year_onetothree_df <- merge(years_pro_df_TOT,vorp_onetothree_df)
year_onetothree_df <- year_onetothree_df[(year_onetothree_df$YearsPro<=4),]
year_onetothree_df <- year_onetothree_df[with(year_onetothree_df, order(Player,YearsPro)), ]
year_onetothree_df[is.na(year_onetothree_df)] <- 0
#year_onetothree_df <- year_onetothree_df[complete.cases(year_onetothree_df),]
# Looks good!

rf.train.3 <- year_onetothree_df
rf.train.3 <- rf.train.3[c("Year1VORP","Year2VORP","Year3VORP")]
rf.label.3 <- year_onetothree_df$Year4VORP
rf.3 <- randomForest(x=rf.train.3, y= rf.label.3,importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)
# OOB = 84.2% with Age, Pk, X3PAr, TRB. TS. AST. STL. BLK. TOV. USG. Year1-3VORP
# Most predictive variables in descending order: Year3VORP, Year2VORP, Pk, TRB., X3PAr, Year1VORP, STL., BLK., AST., TOV., TS., USG., Age
# Try with VORPyears, Pk, TRB., X3PAr (results below)
# OOB = 94.67%. In all likelihood this is overfitting, but let's play with it first. TRB. X3PAr, Pk, Year3, Year2, Years One (order of most predictive descending)
# Let's try VORP only.
# OOB - 92.51% for VORP only. Interesting: This rf sees Year2VORP as the most predictive by far. Then Year3, then year1.

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!BIG TO DO: PRORATING VORP AND OTHER STATS FOR LOCKOUT SHORTENED YEARS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 1999 season shortened to 50 games. Multiply VORP by 1.6
# 2012 season shortened to 66 games. Multiply VORP by 1.212121
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Ok, thoughts on how the whole thing should be structured.
# The VORP projection should be projected from fewer variables, mostly centered around previous VORP
# We will also need to project Minutes Played and we can do a regression on this for each player
# We can likely reserve BLK, AST, TRB, rates for similiarity to other players.
# Finally we can think about producing a regression on college players, predicting their first year VORP
# So---four projections: VORP, MP, Similiarity to other players, First year players.
# Initial plan:
# Develop random forests for a prediction of each year as well as career VORP
# UPDATE: We were previously including every entry for every season in the development of the rf
# This was a big mistake that led to overfitting
# We need to include one row per player (featuring Career VORP, Yearly VORP, Pk, % of games played or possibly Total MP or Total G)
# To consider: Should we include a bayesian prior guess of VORP that is adjusted by the rf projection?
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Career VORP
TEST_VORP_df <- yearly_VORP_df
TEST_VORP_df[is.na(TEST_VORP_df)] <- 0
for (i in 1:nrow(TEST_VORP_df)) {
  TEST_VORP_df[i,23] <- round(sum(TEST_VORP_df[i,2:22]),digits = 2)
}
yearly_VORP_df$CareerVORP <- TEST_VORP_df$CareerVORP
TEST_VORP_df <- merge(TEST_VORP_df, new_draft_df)
# Examing various features against career VORP
# Career VORP vs. Pk
qplot(TEST_VORP_df$Pk, TEST_VORP_df$CareerVORP)
# Career VORP vs. Year1Vorp
qplot(TEST_VORP_df$Year1VORP, TEST_VORP_df$CareerVORP)
# Career VORP vs. Year 4Vorp
qplot(yearly_VORP_df$Year4VORP, yearly_VORP_df$CareerVORP)
mj_y_h <- yearly_VORP_df[(yearly_VORP_df$Player=="Michael Jordan"),]
mj_y_h <- c(mj_y_h[,2:16])
mj_df <- data.frame(1:15)
mj_df$VORP <- as.numeric(mj_y_h)
colnames(mj_df)[1] <- "Year"
ggplot(data = mj_df, aes(x=Year,y=VORP)) +
  geom_point()

# 2nd Year Train
vorp_onetotwo_df <- yearly_VORP_df[,1:3]
vorp_onetotwo_df <- vorp_onetotwo_df[complete.cases(vorp_onetotwo_df),]
# No need for VORPSum here since we only have one recorded year of VORP
year_onetotwo_df <- merge(years_pro_df_TOT,vorp_onetotwo_df)
year_onetotwo_df <- year_onetotwo_df[(year_onetotwo_df$YearsPro==1),]
addTotalMPandGpercent(year_onetotwo_df, 1)
year_onetotwo_df <- df_updated
year_onetotwo_df[is.na(year_onetotwo_df)] <- 0
# No need for AverageVORP or AverageVORPIncrease or VORPIncrease

rf.label.years1to2 <- year_onetotwo_df$Year2VORP
set.seed(1234)
rf.train.years1to2 <- year_onetotwo_df
rf.train.years1to2 <- rf.train.years1to2[c("Year1VORP", "BPM", "WS", "Pk", "Age")]
rf.years1to2 <- randomForest(x=rf.train.years1to2, y=rf.label.years1to2,importance = TRUE, ntree = 1000)
rf.years1to2
varImpPlot(rf.years1to2)
year_onetotwo_df$PredVORP <- rf.years1to2$predicted
str(rf.years1to2)
rf.years1to2$predicted
qplot(rf.years1to2$predicted,rf.label.years1to2)
# 49.27% for Year1VORP, Gpercent, Pk, Age
# 48.71% for Year1VORP, Pk
# BEST RUN: 51.58% for Year1VORP, Pk, BPM, WS, Age

# Linear and Poly Regression for year 2
onetotwo_lr <- lm(Year2VORP ~ Year1VORP + TotalMP + Gpercent + Pk + WS + Age, data = year_onetotwo_df)
summary(onetotwo_lr)
# BEST RUN: .5359 for Year1VORP, TotalMP, Gpercent, Pk, WS, Age (Tried BPM)

# Poly Regresson
onetotwo_poly <- lm(Year2VORP ~ poly(Year1VORP+ TotalMP + Pk, 2, raw = TRUE), data = year_onetotwo_df)
summary(onetotwo_poly)
# .3256 rsquared for Year1VORP, TotalMP, Gpercent, Pk, WS, type=2 (.283 with type=2)
# .3247 rsquared ofr Year1VORP, TotalMP, Pk

# Now let's predict the second year for players with only one year played
vorp_onetotwo_df <- yearly_VORP_df[,1:2]
vorp_onetotwo_df_complete <- vorp_onetotwo_df[complete.cases(vorp_onetotwo_df),]
year_onetotwo_df_preds <- merge(years_pro_df_TOT,vorp_onetotwo_df_complete)
year_onetotwo_df_preds <- year_onetotwo_df_preds[(year_onetotwo_df_preds$YearsPro==1 & year_onetotwo_df_preds$Season==2016),]
year_onetotwo_df_preds <- year_onetotwo_df_preds[with(year_onetotwo_df_preds, order(Player,YearsPro)), ]
year_onetotwo_df_preds[is.na(year_onetotwo_df_preds)] <- 0

# Subset our test records and features
onetotwo_preds <- year_onetotwo_df_preds[c("Year1VORP","BPM", "MP", "G", "X3PAr", "FTr")]
# Make predictions
set.seed(1234)
rf.years1to2.preds <- predict(rf.years1to2, onetotwo_preds)
table(rf.years1to2.preds)
rf.years1to2.preds
year_onetotwo_df_preds$VORPpreds <- round(rf.years1to2.preds, digits = 1)

# Linear regression for Year 2 Stars
year_onetotwo_df_stars <- filter(year_onetotwo_df, Year1VORP >= 1.5, Year2VORP > 1)
onetotwo_lr_stars <- lm(Year2VORP ~ Pk + Year1VORP, data = year_onetotwo_df_stars)
summary(onetotwo_lr_stars)
year_onetotwo_df_stars$fitted.values <- round(onetotwo_lr_stars$fitted.values, digits = 2)
plot(onetotwo_lr_stars)
# .3410 for Pk + Year1VORP
qplot(year_onetotwo_df_stars$Year1VORP, year_onetotwo_df_stars$Year2VORP)
# LR for Year 2 other
year_onetotwo_df_other <- filter(year_onetotwo_df, Year1VORP <= 1.4)
onetotwo_lr_other <- lm(Year2VORP ~ Year1VORP + Pk + Age + WS + TotalMP, data = year_onetotwo_df_other)
summary(onetotwo_lr_other)
year_onetotwo_df_other$fitted.values <- round(onetotwo_lr_other$fitted.values, digits = 2)
plot(onetotwo_lr_other)
# .3597 for Year1VORP + Pk + Age + WS + TotalMP

# Let's see how training the 3rd year VORP goes
vorp_onetothree_df <- yearly_VORP_df[,1:4]
vorp_onetothree_df <- vorp_onetothree_df[complete.cases(vorp_onetothree_df),]
vorp_onetothree_df$VORPSum <- sapply(1:nrow(vorp_onetothree_df), function(x) sum(vorp_onetothree_df[x,(2:3)]))
year_onetothree_df <- merge(years_pro_df_TOT,vorp_onetothree_df)
year_onetothree_df <- year_onetothree_df[(year_onetothree_df$YearsPro<=2),]
addTotalMPandGpercent <- function(df, yp) {
  df_updated <- NULL
  for (p in unique(df$Player)) {
    player_selection <- df[(df$Player==p),]
    player_selection$TotalMP <- sum(player_selection$MP)
    player_selection$Gpercent <- ((sum(player_selection$G))/(yp*82))
    df_updated <- rbind(df_updated,player_selection)
  }
  df_updated <<- df_updated
}
addTotalMPandGpercent(year_onetothree_df, 2)
year_onetothree_df <- df_updated[(df_updated$YearsPro==2),]
year_onetothree_df[is.na(year_onetothree_df)] <- 0
year_onetothree_df$VORPIncrease <- year_onetothree_df$Year2VORP - year_onetothree_df$Year1VORP
year_onetothree_df <- mutate(year_onetothree_df, AverageVORP = round((VORPSum/YearsPro),digits=2))
# No need for AverageVORPIncrease

rf.label.years1to3 <- year_onetothree_df$Year3VORP
set.seed(1234)
rf.train.years1to3 <- year_onetothree_df
rf.train.years1to3 <- rf.train.years1to3[c("Year1VORP", "Year2VORP", "Pk", "Age", "VORPIncrease", "Gpercent", "VORPSum")]
rf.years1to3 <- randomForest(x=rf.train.years1to3, y=rf.label.years1to3,importance = TRUE, ntree = 1000)
rf.years1to3
varImpPlot(rf.years1to3)
# 58.21% for Years1-2, Pk, TotalMP, Gpercent
# 57.47% for Years1-2, Pk
# 58.25% for Years1-2, Pk, TotalMP
# 58.04% for Years1-2, Pk, Gpercent
# 58.46% for Years1-2, Pk, TotalMP, Gpercent, VORPIncrease (Removing Pk does not improve OOB)
# 58.18% for Years1-2, VORPIncrease, "Pk"
# 58.66% for Years1-2, Pk, TotalMP, Gpercent, VORPIncrease, VORPSum
# 58.25% for Years1-2, Pk, VORPIncrease, VORPSum (Removing Pk does not help)
# BEST RUN: 58.38% for Years1-2, Pk, VORPSum, Age, VORPIncrease, Gpercent
year_onetothree_df$PredVORP <- round(rf.years1to3$predicted, digits = 1)
str(rf.years1to3)
qplot(rf.years1to3$predicted,rf.label.years1to3)

# Trying out some linear regression to see if we get a better projection compared to the rf.
onetothree_lr <- lm(Year3VORP ~ Year1VORP + Year2VORP + Age + Gpercent, data = year_onetothree_df)
onetothree_lr
summary(onetothree_lr)
str(summary(onetothree_lr))
plot(onetothree_lr)
year_onetothree_df$fitted.values <- round(onetothree_lr$fitted.values, digits = 2)
# BEST RUN: .6108 for Years1-2, Age, Gpercent (Tried Pk, AverageVORP, VORPSum)

# Polynomial regression for above year1-3
onetothree_poly <- lm(Year3VORP ~ poly(Year1VORP + Year2VORP, 4, raw = TRUE), data = year_onetothree_df)
onetothree_poly
summary(onetothree_poly)
str(summary(onetothree_poly))
plot(onetothree_poly)

# Best run: .585 r squared for Year1-2, .591 for Year2 

# Linear regression for Year 3 Stars
year_onetothree_df_stars <- filter(year_onetothree_df, Year2VORP >= 4, Year3VORP >= 3)
onetothree_lr_stars <- lm(Year3VORP ~ Year2VORP + Year1VORP, data = year_onetothree_df_stars)
summary(onetothree_lr_stars)
year_onetothree_df_stars$fitted.values <- round(onetothree_lr_stars$fitted.values, digits = 2)
plot(onetothree_lr_stars)
# .439 for Year1VORP + Year2VORP

# LR for Year 3 others
year_onetothree_df_other <- filter(year_onetothree_df, Year2VORP <= 3.9)
onetothree_lr_other <- lm(Year3VORP ~ Year1VORP + Year2VORP + Age + Gpercent, data = year_onetothree_df_other)
summary(onetothree_lr_other)
year_onetothree_df_other$fitted.values <- round(onetothree_lr_other$fitted.values, digits = 2)
plot(onetothree_lr_other)
# .5309 for Year1VORP + Year2VORP + Age + Gpercent

# 4th year train
vorp_onetofour_df <- yearly_VORP_df[,1:5]
vorp_onetofour_df <- vorp_onetofour_df[complete.cases(vorp_onetofour_df),]
vorp_onetofour_df$VORPSum <- sapply(1:nrow(vorp_onetofour_df), function(x) sum(vorp_onetofour_df[x,(2:4)]))
year_onetofour_df <- merge(years_pro_df_TOT,vorp_onetofour_df)
year_onetofour_df <- year_onetofour_df[(year_onetofour_df$YearsPro<=3),]
addTotalMPandGpercent(year_onetofour_df, 3)
year_onetofour_df <- df_updated[(df_updated$YearsPro==3),]
year_onetofour_df[is.na(year_onetofour_df)] <- 0
year_onetofour_df$VORPIncrease <- year_onetofour_df$Year3VORP - year_onetofour_df$Year2VORP
year_onetofour_df <- mutate(year_onetofour_df, AverageVORP = round((VORPSum/YearsPro),digits=2))
year_onetofour_df <- mutate(year_onetofour_df, AverageVORPIncrease = round(((Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/2, digits = 2))


rf.label.years1to4 <- year_onetofour_df$Year4VORP
set.seed(1234)
rf.train.years1to4 <- year_onetofour_df
rf.train.years1to4 <- rf.train.years1to4[c("Year1VORP", "Year2VORP", "Year3VORP", "VORPSum", "VORPIncrease", "AverageVORP", "AverageVORPIncrease", "Pk", "TotalMP", "Gpercent")]
rf.years1to4 <- randomForest(x=rf.train.years1to4, y=rf.label.years1to4,importance = TRUE, ntree = 1000)
rf.years1to4
varImpPlot(rf.years1to4)
str(rf.years1to4)
# 61.28% for Years1-3, Pk, VORPIncrease, VORPSum, TotalMP, Gpercent, AverageVORP, AverageVORPIncrease
qplot(rf.years1to4$predicted,year_onetofour_df$Year4VORP)
year_onetofour_df$PredVORP <- round(rf.years1to4$predicted, digits = 1)

# Linear and Polynomial Regression for Year 4
onetofour_lr <- lm(Year4VORP ~ Year1VORP + Year2VORP + Year3VORP + Age, data = year_onetofour_df)
summary(onetofour_lr)
plot(onetofour_lr)
# BEST RUN: .6392 for Years1-3, Age

onetofour_poly <- lm(Year4VORP ~ poly(Year1VORP + Year2VORP + Year3VORP + Age, 2, raw = TRUE), data = year_onetofour_df)
onetofour_poly
summary(onetofour_poly)
# R values far below LR

# Adding VORP for 70s era players for a more complete picture? Lots of missing values before 1977 and 1974.
no_first_year_df <- yearly_VORP_df[!(complete.cases(yearly_VORP_df$Year1VORP)), ]
# Might be impossible to bring in this data for now.

# Linear regression for Year 4 Stars
year_onetofour_df_stars <- filter(year_onetofour_df, Year3VORP >= 4, Year4VORP >= 3)
onetofour_lr_stars <- lm(Year4VORP ~ Year3VORP + Year2VORP + Year1VORP + TotalMP + Pk + Age, data = year_onetofour_df_stars)
summary(onetofour_lr_stars)
year_onetofour_df_stars$fitted.values <- round(onetofour_lr_stars$fitted.values, digits = 2)
plot(onetofour_lr_stars)
# .5906 for Year3VORP + Year2VORP + Year1VORP + TotalMP + Pk + Age

# LR for Year 4 others
year_onetofour_df_other <- filter(year_onetofour_df, Year3VORP <= 3.9)
onetofour_lr_other <- lm(Year4VORP ~ Year1VORP + Year2VORP + Year3VORP + Age, data = year_onetofour_df_other)
summary(onetofour_lr_other)
year_onetofour_df_other$fitted.values <- round(onetofour_lr_other$fitted.values, digits = 2)
plot(onetofour_lr_other)
# .5244 for Year1VORP + Year2VORP + Year3VORP + Age

# 5th year train
vorp_onetofive_df <- yearly_VORP_df[,1:6]
vorp_onetofive_df <- vorp_onetofive_df[complete.cases(vorp_onetofive_df),]
vorp_onetofive_df$VORPSum <- sapply(1:nrow(vorp_onetofive_df), function(x) round(sum(vorp_onetofive_df[x,(2:5)]),digits = 1))
year_onetofive_df <- merge(years_pro_df_TOT,vorp_onetofive_df)
year_onetofive_df <- year_onetofive_df[(year_onetofive_df$YearsPro<=4),]
addTotalMPandGpercent(year_onetofive_df, 4)
year_onetofive_df <- df_updated[(df_updated$YearsPro==4),]
year_onetofive_df[is.na(year_onetofive_df)] <- 0
year_onetofive_df$VORPIncrease <- year_onetofive_df$Year4VORP - year_onetofive_df$Year3VORP
year_onetofive_df <- mutate(year_onetofive_df, AverageVORP = round((VORPSum/YearsPro),digits=2))
year_onetofive_df <- mutate(year_onetofive_df, AverageVORPIncrease = round(((Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/3, digits = 2))


rf.label.years1to5 <- year_onetofive_df$Year5VORP
set.seed(1234)
rf.train.years1to5 <- year_onetofive_df
rf.train.years1to5 <- rf.train.years1to5[c("Year1VORP", "Year2VORP", "Year3VORP", "Year4VORP","VORPSum", "VORPIncrease", "Gpercent", "TotalMP")]
rf.years1to5 <- randomForest(x=rf.train.years1to5, y=rf.label.years1to5,importance = TRUE, ntree = 1000)
rf.years1to5
varImpPlot(rf.years1to5)
str(rf.years1to5)
# 60.6% for Years1-4, Pk, VORPIncrease, VORPSum
# 60.92% for Years1-4, Pk, VORPIncrease, VORPSum, TotalMP, Gpercent
# 61.19% for Years1-4, VORPIncrease, VORPSum, TotalMP, Gpercent
# 59.39% for Years3-4, VORPIncrease, VORPSum
# 60.85% for Years3-4, VORPIncrease, VORPSum, Gpercent, TotalMP
# 61.4% for Years3-4, VORPIncrease, VORPSum, Gpercent, TotalMP, Pk
# 60.99% for Years1-4, Pk, VORPIncrease, VORPSum, Gpercent, TotalMP, Pk
# BEST RUN: 61.19% for Years1-4, VORPIncrease, VORPSum, Gpercent, TotalMP
# lower numbers for just Year3/4VORP, VORPSum, VORPIncrease combinations
qplot(rf.years1to5$predicted,year_onetofive_df$Year5VORP)
year_onetofive_df$PredVORP <- round(rf.years1to5$predicted, digits = 1)
year_onetofive_df$Residuals <- year_onetofive_df$Year5VORP - year_onetofive_df$PredVORP
qplot(year_onetofive_df$Residuals)

# LR and Poly for 5th Year

onetofive_lr <- lm(Year5VORP ~ Year1VORP + Year2VORP + Year3VORP + Year4VORP + AverageVORP + Age + Pk, data = year_onetofive_df)
onetofive_lr
summary(onetofive_lr)
plot(onetofive_lr)
# .653 for Years1-4, AverageVORP, Age, Pk

onetofive_poly <- lm(Year5VORP ~ poly(Year1VORP + Year2VORP + Year3VORP + Year4VORP + AverageVORP + Age + Pk, 2, raw = TRUE), data = year_onetofive_df)
onetofive_poly
summary(onetofive_poly)
plot(onetofive_poly)
# Not valuable

# Linear regression for Year 5 Stars
year_onetofive_df_stars <- filter(year_onetofive_df, Year4VORP >= 4, Year5VORP >= 4)
onetofive_lr_stars <- lm(Year5VORP ~ Year4VORP + VORPSum, data = year_onetofive_df_stars)
summary(onetofive_lr_stars)
year_onetofive_df_stars$fitted.values <- round(onetofive_lr_stars$fitted.values, digits = 2)
plot(onetofive_lr_stars)
# .6139 for Year4VORP + VORPSum

# LR for Year 5 others
year_onetofive_df_other <- filter(year_onetofive_df, Year4VORP <= 3.9)
onetofive_lr_other <- lm(Year5VORP ~ Year2VORP + Year3VORP + Year4VORP + Age, data = year_onetofive_df_other)
summary(onetofive_lr_other)
year_onetofive_df_other$fitted.values <- round(onetofive_lr_other$fitted.values, digits = 2)
plot(onetofive_lr_other)
# .5412 for Year1VORP + Year2VORP + Year3VORP + Year4VORP + Age

# 6th year train
vorp_onetosix_df <- yearly_VORP_df[,1:7]
vorp_onetosix_df <- vorp_onetosix_df[complete.cases(vorp_onetosix_df),]
vorp_onetosix_df$VORPSum <- sapply(1:nrow(vorp_onetosix_df), function(x) round(sum(vorp_onetosix_df[x,(2:6)]),digits = 1))
year_onetosix_df <- merge(years_pro_df_TOT,vorp_onetosix_df)
year_onetosix_df <- year_onetosix_df[(year_onetosix_df$YearsPro<=5),]
addTotalMPandGpercent(year_onetosix_df, 5)
year_onetosix_df <- df_updated[(df_updated$YearsPro==5),]
year_onetosix_df[is.na(year_onetosix_df)] <- 0
year_onetosix_df$VORPIncrease <- year_onetosix_df$Year5VORP - year_onetosix_df$Year4VORP
year_onetosix_df <- mutate(year_onetosix_df, AverageVORP = round((VORPSum/YearsPro),digits=2))
year_onetosix_df <- mutate(year_onetosix_df, AverageVORPIncrease = round(((Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/4, digits = 2))

rf.label.years1to6 <- year_onetosix_df$Year6VORP
set.seed(1234)
rf.train.years1to6 <- year_onetosix_df
rf.train.years1to6 <- rf.train.years1to6[c("Year1VORP","Year2VORP","Year3VORP","Year4VORP","Year5VORP","VORPIncrease", "VORPSum","TotalMP", "Age", "AverageVORP", "AverageVORPIncrease")]
rf.years1to6 <- randomForest(x=rf.train.years1to6, y=rf.label.years1to6,importance = TRUE, ntree = 1000)
rf.years1to6
varImpPlot(rf.years1to6)
str(rf.years1to6)
# REDO for rfs with VORPSum included as we fixed this value
# 61.68% for Year1-5VORP, Pk
# 62.36% for Year1-5VORP, Pk, VORPSum, VORPIncrease (tried removing one of last two to no effect)
# 62.31% for Year1-5VORP, Pk, VORPSum, VORPIncrease, TotalMP, Gpercent
# 62.47% for Year1-5VORP, VORPSum, VORPIncrease, TotalMP, Gpercent
# BEST RUN: 63.55% for Year1-5VORP, VORPSum, VORPIncrease, TotalMP, Age, AverageVORP, AverageVORPIncrease,
# 62.42% for Year4-5VORP, VORPSum, VORPIncrease, TotalMP
# 61.94% for Year4-5VORP, VORPSum, VORPIncrease
# 61.25% for Year4-5VORP, VORPSum
# 61.13% for Year4-5VORP
# 57.21% FOR Year5VORP
qplot(rf.years1to6$predicted,year_onetosix_df$Year6VORP)
year_onetosix_df$PredVORP <- round(rf.years1to6$predicted, digits = 1)
year_onetosix_df$Residuals <- year_onetosix_df$Year6VORP - year_onetosix_df$PredVORP
qplot(year_onetosix_df$Residuals)

# Linear and Polynomial Regression for Year 6
onetosix_lr <- lm(Year6VORP ~ Year3VORP + Year4VORP + Year5VORP + Age + Gpercent, data = year_onetosix_df)
onetosix_lr
summary(onetosix_lr)
plot(onetosix_lr)
str(summary(onetosix_lr))
# BEST RUN: .6618 rsquared for Years3-5, Age, Gpercent

qplot(onetosix_lr$fitted.values, year_onetosix_df$Year6VORP)
year_onetosix_df$PredLRVORP <- round(onetosix_lr$fitted.values, digits = 1)
year_onetosix_df$LRResiduals <- round(onetosix_lr$residuals, digits = 1)

# Polynomial regression
onetosix_poly <- lm(Year6VORP ~ poly(Year3VORP + Year4VORP + Year5VORP, 3, raw = TRUE), data = year_onetosix_df)
onetosix_poly
str(summary(onetosix_poly))
summary(onetosix_poly)
plot(onetosix_poly)
# .6429 for Years3-5 with type 2
# .6395 for Years3-5 with type 1
# .6442 for Years3-5 with type 3
# .6442 for Years3-5 with type 4

# Linear regression for Year 6 Stars
year_onetosix_df_stars <- filter(year_onetosix_df, VORPSum >= 18, Year5VORP >= 4, Year6VORP >= 3)
onetosix_lr_stars <- lm(Year6VORP ~ Year4VORP + Year5VORP + AverageVORPIncrease, data = year_onetosix_df_stars)
summary(onetosix_lr_stars)
year_onetosix_df_stars$fitted.values <- round(onetosix_lr_stars$fitted.values, digits = 2)
plot(onetosix_lr_stars)
# .5828 for  Year4VORP + Year5VORP + AverageVORPIncrease

# LR for Year 6 others
year_onetosix_df_other <- filter(year_onetosix_df, Year5VORP <= 3.9)
onetosix_lr_other <- lm(Year6VORP ~ Year3VORP + Year4VORP + Year5VORP + Age, data = year_onetosix_df_other)
summary(onetosix_lr_other)
year_onetosix_df_other$fitted.values <- round(onetosix_lr_other$fitted.values, digits = 2)
plot(onetosix_lr_other)
# .4836 for Year3VORP + Year4VORP + Year5VORP + Age

# 7th year train
vorp_onetoseven_df <- yearly_VORP_df[,1:8]
vorp_onetoseven_df <- vorp_onetoseven_df[complete.cases(vorp_onetoseven_df),]
vorp_onetoseven_df$VORPSum <- sapply(1:nrow(vorp_onetoseven_df), function(x) round(sum(vorp_onetoseven_df[x,(2:7)]), digits = 1))
year_onetoseven_df <- merge(years_pro_df_TOT,vorp_onetoseven_df)
year_onetoseven_df <- year_onetoseven_df[(year_onetoseven_df$YearsPro<=6),]
addTotalMPandGpercent(year_onetoseven_df, 6)
year_onetoseven_df <- df_updated[(df_updated$YearsPro==6),]
year_onetoseven_df[is.na(year_onetoseven_df)] <- 0
year_onetoseven_df$VORPIncrease <- year_onetoseven_df$Year6VORP - year_onetoseven_df$Year5VORP
year_onetoseven_df <- mutate(year_onetoseven_df, AverageVORP = round((VORPSum/YearsPro),digits=2))
year_onetoseven_df <- mutate(year_onetoseven_df, AverageVORPIncrease = round(((Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/5, digits = 2))


rf.label.years1to7 <- year_onetoseven_df$Year7VORP
set.seed(1234)
rf.train.years1to7 <- year_onetoseven_df
rf.train.years1to7 <- rf.train.years1to7[c("Year1VORP", "Year2VORP", "Year3VORP", "Year4VORP", "Year5VORP","Year6VORP", "VORPSum", "VORPIncrease", "Pk", "Gpercent", "AverageVORP", "AverageVORPIncrease")]
rf.years1to7 <- randomForest(x=rf.train.years1to7, y=rf.label.years1to7,importance = TRUE, ntree = 1000)
rf.years1to7
varImpPlot(rf.years1to7)
str(rf.years1to7)
# 66.57% for Years1-6, Pk, VORPSum, VORPIncrease, TotalMP, Gpercent (down to 66.32 with Drafted added)
# 62.18% for Years5-6 (worse with just year 6)
# 66.29% for Years3-6, Pk, VORPSum, VORPIncrease, TotalMP, Gpercent
# 64.54% for Years5-6, VORPSum, VORPIncrease
# 64.11% for Years3-6, VORPSum, VORPIncrease
# 65.06% for Years1-6, VORPSum, VORPIncrease
# 66.05% for Years1-6, VORPSum, VORPIncrease, Pk
# 66.39% for Years1-6, VORPSum, VORPIncrease, Pk, TotalMP, Gpercent, Drafted, AverageVORP, AverageVORPIncrease
# 66.61% for Years1-6, VORPSum, VORPIncrease, Pk, Gpercent, AverageVORP, AverageVORPIncrease, Age
qplot(rf.years1to7$predicted,year_onetoseven_df$Year7VORP)
year_onetoseven_df$PredVORP <- round(rf.years1to7$predicted, digits = 1)
year_onetoseven_df$Residuals <- year_onetoseven_df$Year7VORP - year_onetoseven_df$PredVORP

# Linear and Poly regression for Year 7
onetoseven_lr <- lm(Year7VORP ~ Year3VORP + Year4VORP + Year5VORP + Year6VORP + Age, data = year_onetoseven_df)
onetoseven_lr
summary(onetoseven_lr)
plot(onetoseven_lr)
str(summary(onetoseven_lr))
# BEST RUN: .6966 for Years3-6, Age
qplot(onetoseven_lr$fitted.values, year_onetoseven_df$Year7VORP)
year_onetoseven_df$PredLRVORP <- round(onetoseven_lr$fitted.values, digits = 1)
year_onetoseven_df$LRResiduals <- round(onetoseven_lr$residuals, digits = 1)

qplot(onetosix_lr$fitted.values, year_onetosix_df$Year6VORP)
year_onetosix_df$PredLRVORP <- round(onetosix_lr$fitted.values, digits = 1)
year_onetosix_df$LRResiduals <- round(onetosix_lr$residuals, digits = 1)

# Polynomial regression
onetoseven_poly <- lm(Year7VORP ~ poly(Year5VORP + Year6VORP, 2, raw = TRUE), data = year_onetoseven_df)
onetoseven_poly
str(summary(onetoseven_poly))
summary(onetoseven_poly)
plot(onetoseven_poly)
# .3345 rsquared for Years1-6, VORPSum, VORPIncrease, Pk, TotalMP, Gpercent, type = 2
# .6368 rsquared for Years1-6, VORPSum, VORPIncrease, type = 2
# .6296 rsquared for Years1-6, type = 2 (.6303 for type = 3)
# .6906 rsquared for Years5-6, type = 3 (.6889 for type = 1, .6905 for type = 2) (Adding in extra years hurts regression)

# LR regression for players with more than 41 games (switched to Gpercent higher than .9)
year_onetoseven_41_df <- year_onetoseven_df[(year_onetoseven_df$Gpercent>.9),]
onetoseven_41_lr <- lm(Year7VORP ~ Year1VORP + Year2VORP + Year3VORP + Year4VORP +Year5VORP + Year6VORP + VORPSum + VORPIncrease + Pk, data = year_onetoseven_41_df)
summary(onetoseven_41_lr)
# .7245 rsquared for Year4-6, Pk
# .7336 rsquared for Year1-6, VORPSum, VORPIncrease, Pk
# Some improvement, but only a few percent compared to LR of full data


# Predicting 7th year for players going into 7th year in 2017
vorp_onetoseven_df_preds <- yearly_VORP_df[,1:7]
vorp_onetoseven_df_preds <- vorp_onetoseven_df_preds[complete.cases(vorp_onetoseven_df_preds),]
year_onetoseven_df_preds <- merge(years_pro_df_TOT,vorp_onetoseven_df_preds)
year_onetoseven_df_preds <- year_onetoseven_df_preds[(year_onetoseven_df_preds$DraftYear==2010 & year_onetoseven_df_preds$YearsPro<=6),]
year_onetoseven_df_preds <- year_onetoseven_df_preds[with(year_onetoseven_df_preds, order(Player,YearsPro)), ]
year_onetoseven_df_preds[is.na(year_onetoseven_df_preds)] <- 0

onetoseven_preds <- year_onetoseven_df_preds[c("Year1VORP", "Year2VORP", "Year3VORP", "Year4VORP", "Year5VORP","Year6VORP", "Pk")]
set.seed(1234)
rf.years1to7.preds <- predict(rf.years1to7, onetoseven_preds)
rf.years1to7.preds
year_onetoseven_df_preds$VORPpreds <- round(rf.years1to7.preds, digits = 1)
year_onetoseven_df_preds <- year_onetoseven_df_preds[(year_onetoseven_df_preds$Season==2016),]

# BIG QUESTION: DOES TEAMMATE VORP HAVE AN EFFECT ON THE PLAYER?
# Also, how can we program in the typical player arc? Create player categories and run rfs from there? Or will subsetting the data weaken the prediction?
# Remove injury years?
# Subset players into various groups (All-Stars, Starters, Bench, End-of-bench)?
# AVERAGE YEARLY VORP INCREASE?

# Prediction Intervals for year_onetoseven
onetoseven_pint <- data.frame(predict(onetoseven_lr, year_onetoseven_df, interval = "predict"))
onetoseven_pint$Player <- year_onetoseven_df$Player
ggplot() +
  geom_pointrange(data=onetoseven_pint, mapping=aes(x=Player, y=fit, ymin=lwr, ymax=upr))
lebron_interval <- onetoseven_pint[(onetoseven_pint$Player=="LeBron James"),]
ggplot() +
  geom_pointrange(data=lebron_interval, mapping=aes(x=Player, y=fit, ymin=lwr, ymax=upr))
year_onetoseven_df$Year7VORP %in% range(onetoseven_pint$lwr,onetoseven_pint$upr)
for (i in 1:nrow(year_onetoseven_df)) {
  if (year_onetoseven_df[i,39] > onetoseven_pint$lwr & onetoseven_pint$upr) {
    print("in range")
  }
  else {
    print(year_onetoseven_df[i,1])
  }
}

# Does adjusting value for Pk affect projection?
# Test produced a higher r-squared but a lower adjusted r-squared likely indicating a lack of real efficacy.
test_df <- years_pro_df_TOT
#test_df$Pk <- gsub("//<0//>", "Undrafted", test_df$Pk)
test_df$Pk <- gsub("Undrafted", "0", test_df$Pk)
years_pro_df_TOT$Pk <- test_df$Pk
years_pro_df_TOT$Pk <- as.numeric(years_pro_df_TOT$Pk)
# How about adding in a 1-0 drafted? column
test_df$Drafted <- ifelse(test_df$Pk>0, 1, 0)
years_pro_df_TOT$Drafted <- test_df$Drafted


# Linear regression for Year 7 Stars
year_onetoseven_df_stars <- filter(year_onetoseven_df, VORPSum >= 18, Year6VORP >= 4, Year7VORP >= 3)
onetoseven_lr_stars <- lm(Year7VORP ~ Year5VORP + Year6VORP, data = year_onetoseven_df_stars)
summary(onetoseven_lr_stars)
year_onetoseven_df_stars$fitted.values <- round(onetoseven_lr_stars$fitted.values, digits = 2)
plot(onetoseven_lr_stars)
# .6097 for Year5VORP + Year6VORP
qplot(year_onetoseven_df$VORP)
# LR for Year 7 others
year_onetoseven_df_other <- filter(year_onetoseven_df, Year6VORP <= 3.9)
onetoseven_lr_other <- lm(Year7VORP ~ Year5VORP + Year6VORP + VORPSum + Gpercent, data = year_onetoseven_df_other)
summary(onetoseven_lr_other)
year_onetoseven_df_other$fitted.values <- round(onetoseven_lr_other$fitted.values, digits = 2)
plot(onetoseven_lr_other)
# .5556 for Year5VORP + Year6VORP + VORPSum + Gpercent

#8th Year Train

vorp_onetoeight_df <- yearly_VORP_df[,1:9]
vorp_onetoeight_df <- vorp_onetoeight_df[complete.cases(vorp_onetoeight_df),]
vorp_onetoeight_df$VORPSum <- sapply(1:nrow(vorp_onetoeight_df), function(x) round(sum(vorp_onetoeight_df[x,(2:8)]), digits = 1))
year_onetoeight_df <- merge(years_pro_df_TOT,vorp_onetoeight_df)
year_onetoeight_df <- year_onetoeight_df[(year_onetoeight_df$YearsPro<=7),]
addTotalMPandGpercent(year_onetoeight_df, 7)
year_onetoeight_df <- df_updated[(df_updated$YearsPro==7),]
year_onetoeight_df[is.na(year_onetoeight_df)] <- 0
year_onetoeight_df$VORPIncrease <- year_onetoeight_df$Year7VORP - year_onetoeight_df$Year6VORP
year_onetoeight_df <- mutate(year_onetoeight_df, AverageVORP = round((VORPSum/YearsPro),digits=2))
year_onetoeight_df <- mutate(year_onetoeight_df, AverageVORPIncrease = round(((Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/6, digits = 2))


rf.label.years1to8 <- year_onetoeight_df$Year8VORP
set.seed(1234)
rf.train.years1to8 <- year_onetoeight_df
rf.train.years1to8 <- rf.train.years1to8[c("Year5VORP","Year6VORP","Year7VORP","AverageVORPIncrease", "AverageVORP", "Gpercent", "Age")]
rf.years1to8 <- randomForest(x=rf.train.years1to8, y=rf.label.years1to8,importance = TRUE, ntree = 1000)
rf.years1to8
varImpPlot(rf.years1to8)
str(rf.years1to8)
# 65.41 for Years1-7, VOPRSum, VORPIncrease, Pk, TotalMP, Gpercent, Drafted
# 65.62 for Years1-7, VORPSum, VORPIncrease, Pk, TotalMP, Gpercent, Drafted, Age
# 66.51 for Years4-7, VORPSum, VORPIncrease, Age, Pk
# 65.87 for Years4-7, VORPSum, VORPIncrease
# 64.33 for Years6-7
# 62.51 for Year7
# 66.96 for Year5-7, AverageVORP, AverageVORPIncrease. Gpercent, Age (best combination of these various features)
qplot(rf.years1to8$predicted,year_onetoeight_df$Year8VORP)
year_onetoeight_df$PredVORP <- round(rf.years1to8$predicted, digits = 1)
year_onetoeight_df$Residuals <- year_onetoeight_df$Year8VORP - year_onetoeight_df$PredVORP

# Linear and Poly regression for Year 8
onetoeight_lr <- lm(Year8VORP ~ Year5VORP + Year6VORP + Year7VORP + Age + Gpercent, data = year_onetoeight_df)
onetoeight_lr
summary(onetoeight_lr)
plot(onetoeight_lr)
str(summary(onetoeight_lr))
# BEST RUN: .6874 for Years5-7, Age, Gpercent
qplot(onetoeight_lr$fitted.values, year_onetoeight_df$Year8VORP)
year_onetoeight_df$PredLRVORP <- round(onetoeight_lr$fitted.values, digits = 1)
year_onetoeight_df$LRResiduals <- round(onetoeight_lr$residuals, digits = 1)

# Polynomial regression
onetoeight_poly <- lm(Year8VORP ~ poly(Year5VORP + Year6VORP + Year7VORP, 1, raw = TRUE), data = year_onetoeight_df)
onetoeight_poly
# .5867 for Years1-7, type=1
# .6615 for Years5-7, type=1 (no improvement with type = 2)
str(summary(onetoeight_poly))
summary(onetoeight_poly)
plot(onetoeight_poly)

# Create two new features: "AverageVORP(vorp per year)" and "AverageVORPIncrease
year_onetoeight_df <- mutate(year_onetoeight_df, AverageVORP = round((VORPSum/YearsPro),digits=2))
# Damn, I love dplyr
year_onetoeight_df <- mutate(year_onetoeight_df, AverageVORPIncrease = round(((Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/6, digits = 2))

# Linear regression for Year 8 Stars
year_onetoeight_df_stars <- filter(year_onetoeight_df, VORPSum >= 20, Year7VORP >= 4, Year8VORP >= 3)
onetoeight_lr_stars <- lm(Year8VORP ~ Year7VORP, data = year_onetoeight_df_stars)
summary(onetoeight_lr_stars)
year_onetoeight_df_stars$fitted.values <- round(onetoeight_lr_stars$fitted.values, digits = 2)
plot(onetoeight_lr_stars)
# .4044 for Year7VORP
qplot(year_onetoeight_df$VORP, year_onetoeight_df$VORPSum)
# LR for Year 8 others
year_onetoeight_df_other <- filter(year_onetoeight_df, Year7VORP <= 3.9)
onetoeight_lr_other <- lm(Year8VORP ~ Year5VORP + Year6VORP + Year7VORP + Age, data = year_onetoeight_df_other)
summary(onetoeight_lr_other)
year_onetoeight_df_other$fitted.values <- round(onetoeight_lr_other$fitted.values, digits = 2)
plot(onetoeight_lr_other)
# .5688 for Year5VORP + Year6VORP + VORPSum + Gpercent

# 9th year train
vorp_onetonine_df <- yearly_VORP_df[,1:10]
vorp_onetonine_df <- vorp_onetonine_df[complete.cases(vorp_onetonine_df),]
vorp_onetonine_df$VORPSum <- sapply(1:nrow(vorp_onetonine_df), function(x) round(sum(vorp_onetonine_df[x,(2:9)]), digits = 1))
year_onetonine_df <- merge(years_pro_df_TOT,vorp_onetonine_df)
year_onetonine_df <- year_onetonine_df[(year_onetonine_df$YearsPro<=8),]
addTotalMPandGpercent(year_onetonine_df, 8)
year_onetonine_df <- df_updated[(df_updated$YearsPro==8),]
year_onetonine_df[is.na(year_onetonine_df)] <- 0
year_onetonine_df$VORPIncrease <- year_onetonine_df$Year8VORP - year_onetonine_df$Year7VORP
year_onetonine_df <- mutate(year_onetonine_df, AverageVORP = round((VORPSum/YearsPro),digits=2))
year_onetonine_df <- mutate(year_onetonine_df, AverageVORPIncrease = round(((Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/7, digits = 2))


rf.label.years1to9 <- year_onetonine_df$Year9VORP
set.seed(1234)
rf.train.years1to9 <- year_onetonine_df
rf.train.years1to9 <- rf.train.years1to9[c("Year1VORP", "Year2VORP", "Year3VORP", "Year4VORP", "Year5VORP","Year6VORP","Year7VORP", "Year8VORP", "AverageVORPIncrease", "AverageVORP", "Gpercent", "Age")]
rf.years1to9 <- randomForest(x=rf.train.years1to9, y=rf.label.years1to9,importance = TRUE, ntree = 1000)
rf.years1to9
varImpPlot(rf.years1to9)
str(rf.years1to9)
# 65.95% for Years1-8, AverageVORPIncrease, AverageVORP, Gpercent, Age
qplot(rf.years1to9$predicted,year_onetonine_df$Year9VORP)
year_onetonine_df$PredVORP <- round(rf.years1to9$predicted, digits = 1)
year_onetonine_df$Residuals <- year_onetonine_df$Year9VORP - year_onetonine_df$PredVORP

# Linear and Poly regression for Year 9
onetonine_lr <- lm(Year9VORP ~ Year2VORP + Year3VORP + Year4VORP + Year5VORP + Year6VORP +Year7VORP + Year8VORP + Age + Pk, data = year_onetonine_df)
onetonine_lr
summary(onetonine_lr)
str(summary(onetonine_lr))
plot(onetonine_lr)
# BEST RUN: .7004 for Years2-8, Age, Pk
qplot(onetonine_lr$fitted.values, year_onetonine_df$Year9VORP)
year_onetonine_df$PredLRVORP <- round(onetonine_lr$fitted.values, digits = 1)
year_onetonine_df$LRResiduals <- round(onetonine_lr$residuals, digits = 1)

# HeavyLM regression
onetonine_heavy_lr <- heavyLm(Year9VORP ~ Year2VORP + Year3VORP + Year4VORP + Year5VORP + Year6VORP +Year7VORP + Year8VORP + Age + Pk, data = year_onetonine_df, model = TRUE)
summary(onetonine_heavy_lr)
onetonine_heavy_lr$fitted.values
year_onetonine_df$HeavyPreds <- round(onetonine_heavy_lr$fitted.values, digits = 1)
# Still undervaluing highest VORPs

# VORPS greater than five
year_onetonine_df_fiveormore <- filter(year_onetonine_df, Year9VORP >= 5)
onetonine_lr_fiveormore <- lm(Year9VORP ~ Year2VORP + Year3VORP + Year4VORP + Year5VORP + Year6VORP +Year7VORP + Year8VORP + Age + Pk, data = year_onetonine_df_fiveormore)
summary(onetonine_lr_fiveormore)
year_onetonine_df_fiveormore$fitted.values <- onetonine_lr_fiveormore$fitted.values
# Hell of a lot better when it comes to assessing superstars and not undervaluing their VORP (bad p values and adjusted r squared, but this is likely due to sample size)

# What about VORP greater than four
year_onetonine_df_fourormore <- filter(year_onetonine_df, Year9VORP >= 4)
onetonine_lr_fourormore <- lm(Year9VORP ~ Year2VORP + Year3VORP + Year4VORP + Year5VORP + Year6VORP +Year7VORP + Year8VORP + Age + Pk, data = year_onetonine_df_fourormore)
summary(onetonine_lr_fourormore)
year_onetonine_df_fourormore$fitted.values <- onetonine_lr_fourormore$fitted.values
# Probably useful

# VORP greater than three
# 3 creates problems, how about 3.5?
year_onetonine_df_threeormore <- filter(year_onetonine_df, Year9VORP >= 3.5)
onetonine_lr_threeormore <- lm(Year9VORP ~ Year2VORP + Year3VORP + Year4VORP + Year5VORP + Year6VORP +Year7VORP + Year8VORP + Age + Pk, data = year_onetonine_df_threeormore)
summary(onetonine_lr_threeormore)
year_onetonine_df_threeormore$fitted.values <- onetonine_lr_threeormore$fitted.values
plot(onetonine_lr_threeormore)

# Less than 3.5?
year_onetonine_df_lessthan3.5 <- filter(year_onetonine_df, Year9VORP <= 3.4)
onetonine_lr_3.5less <- lm(Year9VORP ~ Year2VORP + Year3VORP + Year4VORP + Year5VORP + Year6VORP +Year7VORP + Year8VORP + Age + Pk, data = year_onetonine_df_lessthan3.5)
summary(onetonine_lr_3.5less)
year_onetonine_df_lessthan3.5$fitted.values <- onetonine_lr_3.5less$fitted.values

# VORP greater than three (for previous year)
year_onetonine_df_threeormore_second <- filter(year_onetonine_df, Year8VORP >= 2, Year9VORP > 1)
onetonine_lr_threeormore_second <- lm(Year9VORP ~ Year8VORP + AverageVORP, data = year_onetonine_df_threeormore_second)
summary(onetonine_lr_threeormore_second)
# BEST RUN: .6383 for Year 8, AverageVORP
year_onetonine_df_stars$fitted.values <- round(onetonine_lr_stars$fitted.values, digits = 2)
plot(onetonine_lr_stars)

# VORP--3 tiers
year_onetonine_df_stars <- filter(year_onetonine_df, Year8VORP >= 5, Year9VORP > 1)
onetonine_lr_stars <- lm(Year9VORP ~ Year7VORP + Year8VORP, data = year_onetonine_df_stars)
summary(onetonine_lr_stars)
year_onetonine_df_stars$fitted.values <- round(onetonine_lr_stars$fitted.values, digits = 2)
plot(onetonine_lr_stars)

year_onetonine_df_starters <- filter(year_onetonine_df, Year8VORP >= 1, Year8VORP <= 4.9)
onetonine_lr_starters <- lm(Year9VORP ~ Year7VORP + Year8VORP + AverageVORP + VORPSum, data = year_onetonine_df_starters)
summary(onetonine_lr_starters)
year_onetonine_df_starters$fitted.values <- round(onetonine_lr_starters$fitted.values, digits = 2)
plot(onetonine_lr_starters)

year_onetonine_df_bench <- filter(year_onetonine_df, Year9VORP < 1)
onetonine_lr_bench <- lm(Year9VORP ~ Year8VORP + AverageVORP + VORPSum, data = year_onetonine_df_bench)
summary(onetonine_lr_bench)
plot(onetonine_lr_bench)

# Polynomial regression
onetonine_poly <- lm(Year9VORP ~ poly(Year5VORP + Year6VORP + Year7VORP + Year8VORP + AverageVORP + AverageVORPIncrease + Gpercent, 1, raw = TRUE), data = year_onetonine_df)
onetonine_poly
summary(onetonine_poly)
# .673 for Years5-8, AverageVORP, AverageVORPIncrease
str(summary(onetonine_poly))
plot(onetonine_poly)

# Seperating into tiers of VORP?
star_onetonine <- data.frame(filter(year_onetonine_df, AverageVORP>=3))
class(star_onetonine)
star_onetonine_lr <- lm(Year9VORP ~ Year5VORP + Year6VORP +Year7VORP + Year8VORP + Age + AverageVORP + AverageVORPIncrease + TotalMP, data = star_onetonine)
summary(star_onetonine_lr)
# Various rsquared in the .65 region

# Examining players with more than 60 games played in season
morethan65games_onetonine <- filter(year_onetonine_df, G>=65)
morethan65games_onetonine_lr <- lm(Year9VORP ~ Year1VORP + Year2VORP + Year3VORP + Year4VORP + Year5VORP + Year6VORP +Year7VORP + Year8VORP + Age + AverageVORP + AverageVORPIncrease + Pk, data = morethan65games_onetonine)
summary(morethan65games_onetonine_lr)
# .706 for Years1-8, Age, AverageVORP, AverageVORPIncrease, Gpercent, Pk, VORPSum, VORPIncrease, TotalMP

# So no real improvement when seperating into tiers or excluding injured players

# Let's try to build a decision tree and do cross-validation to see if that produces more accurate projections.

set.seed(1234)
cv.onetonine <- createMultiFolds(rf.label.years1to9, k = 10, times = 10)
# Check stratification
table(rf.label.years1to9)
# Set up caret's trainControl object per above
ctrl.onetonine <- trainControl(method = 'repeatedcv', number = 10, repeats = 10,
                       index = cv.onetonine)
# Setup doSNOW package for multi-core training. This is
# helpful to be training a lot of trees.
# NOTE - This works on Windows and Mac, unlike doMC
#library(parallel)
detectCores()
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)
# Set seed for reproducibility and train
#library(e1071)
set.seed(34324)
onetonine.cv.1 <- train(x = rf.train.years1to9, y = rf.label.years1to9, method = "rf",
                   tuneLength = 3, ntree = 1000, trControl = ctrl.onetonine)
# Shutdown cluster
stopCluster(cl)
onetonine.cv.1
# Improvement from .6625 to .668

# Cart tree
# Create utility function for a cart tree
rpart.cv <- function(seed, training, labels, ctrl) {
  cl <- makeCluster(4, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  # Leverage forumula interface for training
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30,
                    trControl = ctrl)
  
  # Shutdown cluster
  stopCluster(cl)
  
  return(rpart.cv)
}
# Grab features
features = c("Year6VORP","Year7VORP", "Year8VORP", "AverageVORPIncrease", "AverageVORP", "Gpercent", "Age")
rpart.train.onetonine <- year_onetonine_df[features]

# Run CV and check out results
rpart.cv.onetonine.1 <- rpart.cv(94622, rpart.train.onetonine, rf.label.years1to9, ctrl.onetonine)
rpart.cv.onetonine.1

# Plot
prp(rpart.cv.onetonine.1$finalModel, type = 0, extra = 1, under = TRUE)
# Best run for Years1-8, AverageVORPIncrease, AverageVORP, Gpercent, Age is .5922
# Best run for Years6-8, AverageVORPIncrease, AverageVORP, Gpercent, Age is .605
# In all likelihood, using decision trees and randomforests will just not produce superior results to multivariate lr

# Cross-validation of the Linear Regression?
onetonine_lr.cv.1 <- cv.lm(data = year_onetonine_df, form.lm = formula(onetonine_lr),m=5,dots = FALSE, seed=1234,plotit = TRUE, printit = TRUE)
summary(onetonine_lr.cv.1)

# Let's try on an earlier year that might avoid the problem of the model not adapting to players declining.
onetoseven_lr.cv.1 <- cv.lm(data = year_onetoseven_df, form.lm = formula(onetoseven_lr),m=5,dots = FALSE, seed=1234,plotit = TRUE, printit = TRUE)
summary(onetoseven_lr.cv.1)
summary(onetoseven_lr)
qplot(onetoseven_lr.cv.1$cvpred,onetoseven_lr.cv.1$Year7VORP)
qplot(onetoseven_lr$fitted.values,onetoseven_lr.cv.1$Year7VORP)
onetoseven_lr.cv.1$residuals <- round(onetoseven_lr.cv.1$cvpred-onetoseven_lr.cv.1$Year7VORP, digits = 2)
sum(onetoseven_lr.cv.1$residuals)
year_onetoseven_df$residuals <- onetoseven_lr$residuals
round(sum(year_onetoseven_df$residuals),digits = 2)
str(summary(onetoseven_lr))
qplot(onetoseven_lr$fitted.values,onetoseven_lr.cv.1$cvpred)
onetoseven_lr.cv.1$Predicted

cor(onetoseven_lr.cv.1$cvpred,onetoseven_lr.cv.1$Year7VORP)
cor(onetoseven_lr.cv.1$Predicted, onetoseven_lr.cv.1$Year7VORP)
cor(onetoseven_lr$fitted.values, onetoseven_lr.cv.1$Year7VORP)

# Linear regression for Year 9 Stars
year_onetonine_df_stars <- filter(year_onetonine_df, VORPSum >= 26, Year8VORP >= 4, Year9VORP >= 3)
onetonine_lr_stars <- lm(Year9VORP ~ Year7VORP + Year8VORP, data = year_onetonine_df_stars)
summary(onetonine_lr_stars)
year_onetonine_df_stars$fitted.values <- round(onetonine_lr_stars$fitted.values, digits = 2)
plot(onetonine_lr_stars)
# .5628 for Year8VORP + VORPSum + Gpercent
qplot(year_onetonine_df$VORPSum)
# LR for Year9 others
year_onetonine_df_other <- filter(year_onetonine_df, Year8VORP <= 3.9)
onetonine_lr_other <- lm(Year9VORP ~ Year4VORP + Year5VORP + Year6VORP + Year7VORP + Year8VORP + Age + AverageVORP, data = year_onetonine_df_other)
summary(onetonine_lr_other)
year_onetonine_df_other$fitted.values <- round(onetonine_lr_other$fitted.values, digits = 2)
plot(onetonine_lr_other)
# .5398 for Year4VORP + Year5VORP + Year6VORP + Year7VORP + Year8VORP + Age + AverageVORP

#10th year train
vorp_onetoten_df <- yearly_VORP_df[,1:11]
vorp_onetoten_df <- vorp_onetoten_df[complete.cases(vorp_onetoten_df),]
vorp_onetoten_df$VORPSum <- sapply(1:nrow(vorp_onetoten_df), function(x) round(sum(vorp_onetoten_df[x,(2:10)]), digits = 1))
year_onetoten_df <- merge(years_pro_df_TOT,vorp_onetoten_df)
year_onetoten_df <- year_onetoten_df[(year_onetoten_df$YearsPro<=9),]
addTotalMPandGpercent(year_onetoten_df, 9)
year_onetoten_df <- df_updated[(df_updated$YearsPro==9),]
year_onetoten_df[is.na(year_onetoten_df)] <- 0
year_onetoten_df$VORPIncrease <- year_onetoten_df$Year9VORP - year_onetoten_df$Year8VORP
year_onetoten_df <- mutate(year_onetoten_df, AverageVORP = round((VORPSum/YearsPro),digits=2))
year_onetoten_df <- mutate(year_onetoten_df, AverageVORPIncrease = round(((Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/8, digits = 2))


rf.label.years1to10 <- year_onetoten_df$Year10VORP
set.seed(1234)
rf.train.years1to10 <- year_onetoten_df
rf.train.years1to10 <- rf.train.years1to10[c("Year7VORP", "Year8VORP","Year9VORP", "AverageVORPIncrease", "AverageVORP")]
rf.years1to10 <- randomForest(x=rf.train.years1to10, y=rf.label.years1to10,importance = TRUE, ntree = 1000)
rf.years1to10
varImpPlot(rf.years1to10)
str(rf.years1to10)
# 62.27% for Years1-9, AverageVORPIncrease, AverageVORP, Gpercent, Age
# 63.07% for Years7-9, AverageVORPIncrease, AverageVORP
# 62.62% for Years7-9, AverageVORPIncrease, AverageVORP, Pk
qplot(rf.years1to10$predicted,year_onetoten_df$Year10VORP)
year_onetoten_df$PredVORP <- round(rf.years1to10$predicted, digits = 1)
year_onetoten_df$Residuals <- year_onetoten_df$Year10VORP - year_onetoten_df$PredVORP

# Linear and Poly regression for Year 10
onetoten_lr <- lm(Year10VORP ~ Year7VORP + Year8VORP + Year9VORP + Age + Year6VORP, data = year_onetoten_df)
onetoten_lr
summary(onetoten_lr)
str(summary(onetoten_lr))
plot(onetoten_lr)
# BEST RUN: .6376 adjusted for Years6-9, Age
qplot(onetoten_lr$fitted.values, year_onetoten_df$Year10VORP)
year_onetoten_df$PredLRVORP <- round(onetoten_lr$fitted.values, digits = 1)
year_onetoten_df$LRResiduals <- round(onetoten_lr$residuals, digits = 1)

# Polynomial regression
onetoten_poly <- lm(Year10VORP ~ poly(Year1VORP + Year2VORP + Year3VORP + Year4VORP + Year5VORP + Year6VORP + Year7VORP + Year8VORP + Year9VORP + AverageVORP + AverageVORPIncrease + Gpercent, 1, raw = TRUE), data = year_onetoten_df)
onetoten_poly
summary(onetoten_poly)
# .542 FOR Years1-9, AverageVORP, AverageVORPIncrease, Gpercent (worse with type=2)
str(summary(onetoten_poly))
plot(onetoten_poly)
qplot(year_onetoten_df$Year10VORP)

# Linear regression for Year 10 Stars
year_onetoten_df_stars <- filter(year_onetoten_df, VORPSum >= 30, Year9VORP >= 4, Year10VORP >= 3)
onetoten_lr_stars <- lm(Year10VORP ~ Year9VORP + AverageVORP, data = year_onetoten_df_stars)
summary(onetoten_lr_stars)
year_onetoten_df_stars$fitted.values <- round(onetoten_lr_stars$fitted.values, digits = 2)
plot(onetoten_lr_stars)
# .5002 for Year9VORP + AverageVORPIncrease + Age + Gpercent
qplot(year_onetoten_df$VORPSum)
# LR for Year10 others
year_onetoten_df_other <- filter(year_onetoten_df, Year9VORP <= 3.9)
onetoten_lr_other <- lm(Year10VORP ~ Year5VORP + Year6VORP + Year7VORP + Year8VORP + Year9VORP + Age + Gpercent, data = year_onetoten_df_other)
summary(onetoten_lr_other)
year_onetoten_df_other$fitted.values <- round(onetoten_lr_other$fitted.values, digits = 2)
plot(onetoten_lr_other)
# .561 for Year5VORP + Year6VORP + Year7VORP + Year8VORP + Year9VORP + Age + Gpercent

# 11th Year Train
vorp_onetoeleven_df <- yearly_VORP_df[,1:12]
vorp_onetoeleven_df <- vorp_onetoeleven_df[complete.cases(vorp_onetoeleven_df),]
vorp_onetoeleven_df$VORPSum <- sapply(1:nrow(vorp_onetoeleven_df), function(x) round(sum(vorp_onetoeleven_df[x,(2:11)]), digits = 1))
year_onetoeleven_df <- merge(years_pro_df_TOT,vorp_onetoeleven_df)
year_onetoeleven_df <- year_onetoeleven_df[(year_onetoeleven_df$YearsPro<=10),]
addTotalMPandGpercent(year_onetoeleven_df, 10)
year_onetoeleven_df <- df_updated[(df_updated$YearsPro==10),]
year_onetoeleven_df[is.na(year_onetoeleven_df)] <- 0
year_onetoeleven_df$VORPIncrease <- year_onetoeleven_df$Year10VORP - year_onetoeleven_df$Year9VORP
year_onetoeleven_df <- mutate(year_onetoeleven_df, AverageVORP = round((VORPSum/YearsPro),digits=2))
year_onetoeleven_df <- mutate(year_onetoeleven_df, AverageVORPIncrease = round(((Year10VORP-Year9VORP)+(Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/9, digits = 2))


rf.label.years1to11 <- year_onetoeleven_df$Year11VORP
set.seed(1234)
rf.train.years1to11 <- year_onetoeleven_df
rf.train.years1to11 <- rf.train.years1to11[c("Year8VORP","Year9VORP","Year10VORP", "AverageVORP", "VORPSum")]
rf.years1to11 <- randomForest(x=rf.train.years1to11, y=rf.label.years1to11,importance = TRUE, ntree = 1000)
rf.years1to11
varImpPlot(rf.years1to11)
str(rf.years1to11)
# 71.19% for Years8-10, AverageVORP, VORPSum (Not improved by removing Year8) (Adding Age produces no effect) (Tried AverageVORPIncrease, Year7VORP, VORPIncrease)
qplot(rf.years1to11$predicted,year_onetoeleven_df$Year11VORP)
year_onetoeleven_df$PredVORP <- round(rf.years1to11$predicted, digits = 1)
year_onetoeleven_df$Residuals <- year_onetoeleven_df$Year11VORP - year_onetoeleven_df$PredVORP

# Linear and Poly regression for Year 11
onetoeleven_lr <- lm(Year11VORP ~ Year8VORP + Year9VORP + Year10VORP + AverageVORPIncrease + TotalMP, data = year_onetoeleven_df)
onetoeleven_lr
summary(onetoeleven_lr)
str(summary(onetoeleven_lr))
plot(onetoeleven_lr)
# .7203 adjusted for Years1-10, AverageVORP, Pk, Age, Gpercent, AverageVORPIncrease
# .7168 adjusted for Years5-10, AverageVORP, Pk, Age, Gpercent, AverageVORPIncrease
# .7179 adjusted for Years7-10, AverageVORP, AverageVORPIncrease (No difference with VORPSum subbed for AverageVORP)
# BEST RUN: .7202 adjusted for Years8-10, AverageVORPIncrease, TotalMP (Tried Pk, Age, Gpercent, VORPSum, VORPIncrease)
qplot(onetoeleven_lr$fitted.values, year_onetoeleven_df$Year11VORP)
year_onetoeleven_df$PredLRVORP <- round(onetoeleven_lr$fitted.values, digits = 1)
year_onetoeleven_df$LRResiduals <- round(onetoeleven_lr$residuals, digits = 1)

# Polynomial regression
onetoeleven_poly <- lm(Year11VORP ~ poly(Year8VORP + Year9VORP + Year10VORP + AverageVORPIncrease + TotalMP, 2, raw = TRUE), data = year_onetoeleven_df)
onetoeleven_poly
summary(onetoeleven_poly)
# .3317 for Years8-10, AverageVORPIncrease, TotalMP (type=2)
str(summary(onetoeleven_poly))
plot(onetoeleven_poly)
qplot(year_onetoeleven_df$Year11VORP)

# Linear regression for Year 11 Stars
year_onetoeleven_df_stars <- filter(year_onetoeleven_df, VORPSum >= 32, Year10VORP >= 3.3, Year11VORP >= 3)
onetoeleven_lr_stars <- lm(Year11VORP ~ Year10VORP, data = year_onetoeleven_df_stars)
summary(onetoeleven_lr_stars)
year_onetoeleven_df_stars$fitted.values <- round(onetoeleven_lr_stars$fitted.values, digits = 2)
plot(onetoeleven_lr_stars)
# .5717 for Year10VORP
# .5002 for Year9VORP + AverageVORPIncrease + Age + Gpercent
qplot(year_onetoeleven_df$VORPSum)
# LR for Year11 others
year_onetoeleven_df_other <- filter(year_onetoeleven_df, Year10VORP <= 3.2)
onetoeleven_lr_other <- lm(Year11VORP ~ Year8VORP + Year9VORP + Year10VORP + VORPSum, data = year_onetoeleven_df_other)
summary(onetoeleven_lr_other)
year_onetoeleven_df_other$fitted.values <- round(onetoeleven_lr_other$fitted.values, digits = 2)
plot(onetoeleven_lr_other)
# .5420 for Year8VORP + Year9VORP + Year10VORP + VORPSum


# 12th Year Train
vorp_onetotwelve_df <- yearly_VORP_df[,1:13]
vorp_onetotwelve_df <- vorp_onetotwelve_df[complete.cases(vorp_onetotwelve_df),]
vorp_onetotwelve_df$VORPSum <- sapply(1:nrow(vorp_onetotwelve_df), function(x) round(sum(vorp_onetotwelve_df[x,(2:12)]), digits = 1))
year_onetotwelve_df <- merge(years_pro_df_TOT,vorp_onetotwelve_df)
year_onetotwelve_df <- year_onetotwelve_df[(year_onetotwelve_df$YearsPro<=11),]
addTotalMPandGpercent(year_onetotwelve_df, 11)
year_onetotwelve_df <- df_updated[(df_updated$YearsPro==11),]
year_onetotwelve_df[is.na(year_onetotwelve_df)] <- 0
year_onetotwelve_df$VORPIncrease <- year_onetotwelve_df$Year11VORP - year_onetotwelve_df$Year10VORP
year_onetotwelve_df <- mutate(year_onetotwelve_df, AverageVORP = round((VORPSum/YearsPro),digits=2))
year_onetotwelve_df <- mutate(year_onetotwelve_df, AverageVORPIncrease = round(((Year11VORP-Year10VORP)+(Year10VORP-Year9VORP)+(Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/10, digits = 2))


rf.label.years1to12 <- year_onetotwelve_df$Year12VORP
set.seed(1234)
rf.train.years1to12 <- year_onetotwelve_df
rf.train.years1to12 <- rf.train.years1to12[c("Year8VORP","Year9VORP","Year10VORP", "Year11VORP", "AverageVORP", "VORPIncrease", "AverageVORPIncrease")]
rf.years1to12 <- randomForest(x=rf.train.years1to12, y=rf.label.years1to12,importance = TRUE, ntree = 1000)
rf.years1to12
varImpPlot(rf.years1to12)
str(rf.years1to12)
# 71.95% for Years 8-11, AverageVORP, VORPIncrease
# BEST RUN: 71.68% for Years 8-11, AverageVORP, VORPIncrease, AverageVORPIncrease (Tried Pk, Gpercent, Age, VORPSum, Age, TotalMP, Year7VORP)
qplot(rf.years1to12$predicted,year_onetotwelve_df$Year12VORP)
year_onetotwelve_df$PredVORP <- round(rf.years1to12$predicted, digits = 1)
year_onetotwelve_df$Residuals <- year_onetotwelve_df$Year12VORP - year_onetotwelve_df$PredVORP

# Linear and Poly regression for Year 12
onetotwelve_lr <- lm(Year12VORP ~ Year9VORP + Year10VORP + Year11VORP + VORPSum + AverageVORP, data = year_onetotwelve_df)
onetotwelve_lr
summary(onetotwelve_lr)
str(summary(onetotwelve_lr))
plot(onetotwelve_lr)
# BEST RUN: .7436 for Years9-11, VORPSum, AverageVORP
qplot(onetotwelve_lr$fitted.values, year_onetotwelve_df$Year12VORP)
year_onetotwelve_df$PredLRVORP <- round(onetotwelve_lr$fitted.values, digits = 1)
year_onetotwelve_df$LRResiduals <- round(onetotwelve_lr$residuals, digits = 1)

# Exponential Regression does not make sense given negative values
# I also examined positive values and an exponential line did not produce a more accurate regression

# Polynomial regression
onetotwelve_poly <- lm(Year12VORP ~ poly(Year11VORP, 2, raw = TRUE), data = year_onetotwelve_df)
onetotwelve_poly
log(year_onetotwelve_df$Year12VORP)
summary(onetotwelve_poly)
# .7217 for Year11VORP type=2
str(summary(onetotwelve_poly))
plot(onetotwelve_poly)
qplot(year_onetotwelve_df$Year12VORP)

# Linear regression for Year 12 Stars
year_onetotwelve_df_stars <- filter(year_onetotwelve_df, Year11VORP >= 2.5, Year12VORP >= 2.5)
onetotwelve_lr_stars <- lm(Year12VORP ~ Year11VORP + AverageVORP, data = year_onetotwelve_df_stars)
summary(onetotwelve_lr_stars)
year_onetotwelve_df_stars$fitted.values <- round(onetotwelve_lr_stars$fitted.values, digits = 2)
plot(onetotwelve_lr_stars)
# .6143 for Year11VORP + AverageVORP
qplot(year_onetotwelve_df$VORPSum)
# LR for Year12 others
year_onetotwelve_df_other <- filter(year_onetotwelve_df, Year11VORP <= 2.4)
onetotwelve_lr_other <- lm(Year12VORP ~ Year10VORP + Year11VORP + VORPSum + Gpercent, data = year_onetotwelve_df_other)
summary(onetotwelve_lr_other)
year_onetotwelve_df_other$fitted.values <- round(onetotwelve_lr_other$fitted.values, digits = 2)
plot(onetotwelve_lr_other)
# .4420 for Year10VORP + Year11VORP + VORPSum + Gpercent

# 13th Year Train
vorp_onetothirteen_df <- yearly_VORP_df[,1:14]
vorp_onetothirteen_df <- vorp_onetothirteen_df[complete.cases(vorp_onetothirteen_df),]
vorp_onetothirteen_df$VORPSum <- sapply(1:nrow(vorp_onetothirteen_df), function(x) round(sum(vorp_onetothirteen_df[x,(2:13)]), digits = 1))
year_onetothirteen_df <- merge(years_pro_df_TOT,vorp_onetothirteen_df)
year_onetothirteen_df <- year_onetothirteen_df[(year_onetothirteen_df$YearsPro<=12),]
addTotalMPandGpercent(year_onetothirteen_df, 12)
year_onetothirteen_df <- df_updated[(df_updated$YearsPro==12),]
year_onetothirteen_df[is.na(year_onetothirteen_df)] <- 0
year_onetothirteen_df$VORPIncrease <- year_onetothirteen_df$Year12VORP - year_onetothirteen_df$Year11VORP
year_onetothirteen_df <- mutate(year_onetothirteen_df, AverageVORP = round((VORPSum/YearsPro),digits=2))
year_onetothirteen_df <- mutate(year_onetothirteen_df, AverageVORPIncrease = round(((Year12VORP-Year11VORP)+(Year11VORP-Year10VORP)+(Year10VORP-Year9VORP)+(Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/11, digits = 2))


rf.label.years1to13 <- year_onetothirteen_df$Year13VORP
set.seed(1234)
rf.train.years1to13 <- year_onetothirteen_df
rf.train.years1to13 <- rf.train.years1to13[c("Year9VORP", "Year10VORP", "Year11VORP", "Year12VORP", "Gpercent", "AverageVORP", "VORPIncrease")]
rf.years1to13 <- randomForest(x=rf.train.years1to13, y=rf.label.years1to13,importance = TRUE, ntree = 1000)
rf.years1to13
varImpPlot(rf.years1to13)
str(rf.years1to13)
# 61.66% for Years1-12, AverageVORP, Gpercent
# 64.31% for Years9-12, AverageVORP, Gpercent, VORPIncrease (Removing Gpercent hurts) (Tried AverageVORPIncrease, VORPSum, Pk, Age, TotalMP)
qplot(rf.years1to13$predicted,year_onetothirteen_df$Year13VORP)
year_onetothirteen_df$PredVORP <- round(rf.years1to13$predicted, digits = 1)
year_onetothirteen_df$Residuals <- year_onetothirteen_df$Year13VORP - year_onetothirteen_df$PredVORP

# Linear and Poly regression for Year 13
onetothirteen_lr <- lm(Year13VORP ~ Year11VORP + Year12VORP + AverageVORP, data = year_onetothirteen_df)
onetothirteen_lr
summary(onetothirteen_lr)
str(summary(onetothirteen_lr))
plot(onetothirteen_lr)
# .6626 adjusted for Years11-12, AverageVORPIncrese, AverageVORP
# .6531 adjusted for Years10-12
# BEST RUN: .6636 adjusted for Years11-12, AverageVORP (Tried Year10VORP, Gpercent, VORPSum, Pk, TotalMP, VORPIncrease, AverageVORPIncrease)
qplot(onetothirteen_lr$fitted.values, year_onetothirteen_df$Year13VORP)
year_onetothirteen_df$PredLRVORP <- round(onetothirteen_lr$fitted.values, digits = 1)
year_onetothirteen_df$LRResiduals <- round(onetothirteen_lr$residuals, digits = 1)

# Polynomial regression
onetothirteen_poly <- lm(Year13VORP ~ poly(Year11VORP + Year12VORP + AverageVORP, 2, raw = TRUE), data = year_onetothirteen_df)
onetothirteen_poly
summary(onetothirteen_poly)
# .654 for Years11-12, AverageVORP, type=2
str(summary(onetothirteen_poly))
plot(onetothirteen_poly)
qplot(year_onetothirteen_df$Year13VORP)

# Linear regression for Year 13 Stars
year_onetothirteen_df_stars <- filter(year_onetothirteen_df, Year12VORP >= 2.5, Year13VORP >= 2.5)
onetothirteen_lr_stars <- lm(Year13VORP ~ Year8VORP + Year9VORP + Year10VORP + Year11VORP + Year12VORP, data = year_onetothirteen_df_stars)
summary(onetothirteen_lr_stars)
year_onetothirteen_df_stars$fitted.values <- round(onetothirteen_lr_stars$fitted.values, digits = 2)
plot(onetothirteen_lr_stars)
# .7050 for Year8VORP + Year9VORP + Year10VORP + Year11VORP + Year12VORP
qplot(year_onetothirteen_df$VORP)
# LR for Year13 others
year_onetothirteen_df_other <- filter(year_onetothirteen_df, Year12VORP <= 2.4)
onetothirteen_lr_other <- lm(Year13VORP ~ Year11VORP + Year12VORP, data = year_onetothirteen_df_other)
summary(onetothirteen_lr_other)
year_onetothirteen_df_other$fitted.values <- round(onetothirteen_lr_other$fitted.values, digits = 2)
plot(onetothirteen_lr_other)
# .4653 for Year12VORP + VORPSum

# 14th Year Train

vorp_onetofourteen_df <- yearly_VORP_df[,1:15]
vorp_onetofourteen_df <- vorp_onetofourteen_df[complete.cases(vorp_onetofourteen_df),]
vorp_onetofourteen_df$VORPSum <- sapply(1:nrow(vorp_onetofourteen_df), function(x) round(sum(vorp_onetofourteen_df[x,(2:14)]), digits = 1))
year_onetofourteen_df <- merge(years_pro_df_TOT,vorp_onetofourteen_df)
year_onetofourteen_df <- year_onetofourteen_df[(year_onetofourteen_df$YearsPro<=13),]
addTotalMPandGpercent(year_onetofourteen_df, 13)
year_onetofourteen_df <- df_updated[(df_updated$YearsPro==13),]
year_onetofourteen_df[is.na(year_onetofourteen_df)] <- 0
year_onetofourteen_df$VORPIncrease <- year_onetofourteen_df$Year13VORP - year_onetofourteen_df$Year12VORP
year_onetofourteen_df <- mutate(year_onetofourteen_df, AverageVORP = round((VORPSum/YearsPro),digits=2))
year_onetofourteen_df <- mutate(year_onetofourteen_df, AverageVORPIncrease = round(((Year13VORP-Year12VORP)+(Year12VORP-Year11VORP)+(Year11VORP-Year10VORP)+(Year10VORP-Year9VORP)+(Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/12, digits = 2))


rf.label.years1to14 <- year_onetofourteen_df$Year14VORP
set.seed(1234)
rf.train.years1to14 <- year_onetofourteen_df
rf.train.years1to14 <- rf.train.years1to14[c("Year11VORP", "Year12VORP", "Year13VORP",  "AverageVORP", "AverageVORPIncrease", "Pk")]
rf.years1to14 <- randomForest(x=rf.train.years1to14, y=rf.label.years1to14,importance = TRUE, ntree = 1000)
rf.years1to14
varImpPlot(rf.years1to14)
str(rf.years1to14)
# 58.75% for years 1-13, AverageVORP, Gpercent
# 59.57% for years 11-13, AverageVORP, Gpercent
# 60.12% for years 11-13, AverageVORP
# 63.04% for years 11-13, AverageVORP, Age, Pk
# 62.48% for years 11-13, AverageVORP, Age, Pk, AverageVORPIncrease
# BEST RUN: 64.28% for years 11-13, AverageVORP, AverageVORPIncrease, Pk
# 63.41% for years 11-13, AverageVORP, Pk, VORPSum
# 63.22% for years 11-13, AverageVORP, Pk, VORPIncrease
# 63.1% for years 11-13, AverageVORP, Pk, Gpercent
qplot(rf.years1to14$predicted,year_onetofourteen_df$Year14VORP)
year_onetofourteen_df$PredVORP <- round(rf.years1to14$predicted, digits = 1)
year_onetofourteen_df$Residuals <- year_onetofourteen_df$Year14VORP - year_onetofourteen_df$PredVORP

# Linear and Poly regression for Year 14
onetofourteen_lr <- lm(Year14VORP ~ Year11VORP + Year12VORP + Year13VORP + AverageVORPIncrease, data = year_onetofourteen_df)
onetofourteen_lr
summary(onetofourteen_lr)
str(summary(onetofourteen_lr))
plot(onetofourteen_lr)
# BEST RUN: .6819 rsquared adjusted for Years11-13, AverageVORPIncrease

qplot(onetofourteen_lr$fitted.values, year_onetofourteen_df$Year14VORP)
year_onetofourteen_df$PredLRVORP <- round(onetofourteen_lr$fitted.values, digits = 1)
year_onetofourteen_df$LRResiduals <- round(onetofourteen_lr$residuals, digits = 1)

# Polynomial regression
onetofourteen_poly <- lm(Year14VORP ~ poly(Year11VORP + Year12VORP + Year13VORP + AverageVORPIncrease, 1, raw = TRUE), data = year_onetofourteen_df)
onetofourteen_poly
summary(onetofourteen_poly)
# .6612 for years11-13, AverageVORPIncrease type=1 (type=2 produces no change)
# Other attempts do not produce a superior regression.
str(summary(onetofourteen_poly))
plot(onetofourteen_poly)
qplot(year_onetofourteen_df$Year14VORP)

# Linear regression for Year 14 Stars
year_onetofourteen_df_stars <- filter(year_onetofourteen_df, Year13VORP >= 2, Year14VORP >= 2)
onetofourteen_lr_stars <- lm(Year14VORP ~ Year9VORP + Year10VORP + Year11VORP + Year12VORP + Year13VORP, data = year_onetofourteen_df_stars)
summary(onetofourteen_lr_stars)
year_onetofourteen_df_stars$fitted.values <- round(onetofourteen_lr_stars$fitted.values, digits = 2)
plot(onetofourteen_lr_stars)
# .6090 for Year9VORP + Year10VORP + Year11VORP + Year12VORP + Year13VORP
qplot(year_onetofourteen_df$VORP)
# LR for Year14 others
year_onetofourteen_df_other <- filter(year_onetofourteen_df, Year13VORP <= 1.9)
onetofourteen_lr_other <- lm(Year14VORP ~ Year11VORP + Year12VORP + Year13VORP, data = year_onetofourteen_df_other)
summary(onetofourteen_lr_other)
year_onetofourteen_df_other$fitted.values <- round(onetofourteen_lr_other$fitted.values, digits = 2)
plot(onetofourteen_lr_other)
# .4606 for Year11VORP + Year12VORP + Year13VORP, data = year_onetofourteen_df_other)

# 15th Year Train

vorp_onetofifteen_df <- yearly_VORP_df[,1:16]
vorp_onetofifteen_df <- vorp_onetofifteen_df[complete.cases(vorp_onetofifteen_df),]
vorp_onetofifteen_df$VORPSum <- sapply(1:nrow(vorp_onetofifteen_df), function(x) round(sum(vorp_onetofifteen_df[x,(2:15)]), digits = 1))
year_onetofifteen_df <- merge(years_pro_df_TOT,vorp_onetofifteen_df)
year_onetofifteen_df <- year_onetofifteen_df[(year_onetofifteen_df$YearsPro<=14),]
addTotalMPandGpercent(year_onetofifteen_df, 14)
year_onetofifteen_df <- df_updated[(df_updated$YearsPro==14),]
year_onetofifteen_df[is.na(year_onetofifteen_df)] <- 0
year_onetofifteen_df$VORPIncrease <- year_onetofifteen_df$Year14VORP - year_onetofifteen_df$Year13VORP
year_onetofifteen_df <- mutate(year_onetofifteen_df, AverageVORP = round((VORPSum/YearsPro),digits=2))
year_onetofifteen_df <- mutate(year_onetofifteen_df, AverageVORPIncrease = round(((Year14VORP-Year13VORP)+(Year13VORP-Year12VORP)+(Year12VORP-Year11VORP)+(Year11VORP-Year10VORP)+(Year10VORP-Year9VORP)+(Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/13, digits = 2))


rf.label.years1to15 <- year_onetofifteen_df$Year15VORP
set.seed(1234)
rf.train.years1to15 <- year_onetofifteen_df
rf.train.years1to15 <- rf.train.years1to15[c("Year12VORP", "Year13VORP", "Year14VORP", "AverageVORP", "AverageVORPIncrease", "Gpercent")]
rf.years1to15 <- randomForest(x=rf.train.years1to15, y=rf.label.years1to15,importance = TRUE, ntree = 1000)
rf.years1to15
varImpPlot(rf.years1to15)
str(rf.years1to15)
# 64.32 for Years11-14, AverageVORP, Pk
# BEST RUN: 66.25 for Years12-14, AverageVORP, AverageVORPIncrease, Gpercent (Tried VORPSum, VORPIncrease, Age, Year11VORP)
qplot(rf.years1to15$predicted,year_onetofifteen_df$Year15VORP)
year_onetofifteen_df$PredVORP <- round(rf.years1to15$predicted, digits = 1)
year_onetofifteen_df$Residuals <- year_onetofifteen_df$Year15VORP - year_onetofifteen_df$PredVORP

# Linear and Poly regression for Year 15
onetofifteen_lr <- lm(Year15VORP ~ Year13VORP + Year14VORP, data = year_onetofifteen_df)
onetofifteen_lr
summary(onetofifteen_lr)
str(summary(onetofifteen_lr))
plot(onetofifteen_lr)
# .7224 adjusted for Years12-14, AverageVORP, AverageVORPIncrease, Gpercent
# BEST RUN: .7259 adjusted for Years13-14

qplot(onetofifteen_lr$fitted.values, year_onetofifteen_df$Year15VORP)
year_onetofifteen_df$PredLRVORP <- round(onetofifteen_lr$fitted.values, digits = 1)
year_onetofifteen_df$LRResiduals <- round(onetofifteen_lr$residuals, digits = 1)

# Polynomial regression
onetofifteen_poly <- lm(Year15VORP ~ poly(Year13VORP + Year14VORP + AverageVORP, 1, raw = TRUE), data = year_onetofifteen_df)
onetofifteen_poly
summary(onetofifteen_poly)
# .7182 for Year13-14, AverageVORP
str(summary(onetofifteen_poly))
plot(onetofifteen_poly)
qplot(year_onetofifteen_df$Year15VORP)

# Linear regression for Year 15 Stars
year_onetofifteen_df_stars <- filter(year_onetofifteen_df, Year14VORP >= 2, Year15VORP >= 2)
onetofifteen_lr_stars <- lm(Year15VORP ~ Year14VORP + AverageVORPIncrease, data = year_onetofifteen_df_stars)
summary(onetofifteen_lr_stars)
year_onetofifteen_df_stars$fitted.values <- round(onetofifteen_lr_stars$fitted.values, digits = 2)
plot(onetofifteen_lr_stars)
# .5936 for Year14VORP + AverageVORPIncrease
qplot(year_onetofifteen_df$VORP)
# LR for Year15 others
year_onetofifteen_df_other <- filter(year_onetofifteen_df, Year14VORP <= 1.9)
onetofifteen_lr_other <- lm(Year15VORP ~ Year13VORP + Year14VORP, data = year_onetofifteen_df_other)
summary(onetofifteen_lr_other)
year_onetofifteen_df_other$fitted.values <- round(onetofifteen_lr_other$fitted.values, digits = 2)
plot(onetofifteen_lr_other)
# .4775 for Year13VORP + Year14VORP

# 16th Year Train

vorp_onetosixteen_df <- yearly_VORP_df[,1:17]
vorp_onetosixteen_df <- vorp_onetosixteen_df[complete.cases(vorp_onetosixteen_df),]
vorp_onetosixteen_df$VORPSum <- sapply(1:nrow(vorp_onetosixteen_df), function(x) round(sum(vorp_onetosixteen_df[x,(2:16)]), digits = 1))
year_onetosixteen_df <- merge(years_pro_df_TOT,vorp_onetosixteen_df)
year_onetosixteen_df <- year_onetosixteen_df[(year_onetosixteen_df$YearsPro<=15),]
addTotalMPandGpercent(year_onetosixteen_df, 15)
year_onetosixteen_df <- df_updated[(df_updated$YearsPro==15),]
year_onetosixteen_df[is.na(year_onetosixteen_df)] <- 0
year_onetosixteen_df$VORPIncrease <- year_onetosixteen_df$Year15VORP - year_onetosixteen_df$Year14VORP
year_onetosixteen_df <- mutate(year_onetosixteen_df, AverageVORP = round((VORPSum/YearsPro),digits=2))
year_onetosixteen_df <- mutate(year_onetosixteen_df, AverageVORPIncrease = round(((Year15VORP-Year14VORP)+(Year14VORP-Year13VORP)+(Year13VORP-Year12VORP)+(Year12VORP-Year11VORP)+(Year11VORP-Year10VORP)+(Year10VORP-Year9VORP)+(Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/14, digits = 2))


rf.label.years1to16 <- year_onetosixteen_df$Year16VORP
set.seed(1234)
rf.train.years1to16 <- year_onetosixteen_df
rf.train.years1to16 <- rf.train.years1to16[c("Year11VORP", "Year12VORP", "Year13VORP", "Year14VORP", "Year15VORP", "AverageVORP", "AverageVORPIncrease", "Gpercent")]
rf.years1to16 <- randomForest(x=rf.train.years1to16, y=rf.label.years1to16,importance = TRUE, ntree = 1000)
rf.years1to16
varImpPlot(rf.years1to16)
str(rf.years1to16)
# 53.26% for Years12-15, AverageVORP, AverageVORPIncrease, Gpercent
# 55.2% for Years11-15, AverageVORP, AverageVORPIncrease, Gpercent (Tried VORPSum, VORPIncrease, TotalMP, Pk, Age, Year10VORP)
qplot(rf.years1to16$predicted,year_onetosixteen_df$Year16VORP)
year_onetosixteen_df$PredVORP <- round(rf.years1to16$predicted, digits = 1)
year_onetosixteen_df$Residuals <- year_onetosixteen_df$Year16VORP - year_onetosixteen_df$PredVORP

# Linear and Poly regression for Year 16
onetosixteen_lr <- lm(Year16VORP ~ Year13VORP + Year15VORP, data = year_onetosixteen_df)
onetosixteen_lr
summary(onetosixteen_lr)
str(summary(onetosixteen_lr))
plot(onetosixteen_lr)
# .6218 adjusted Years13-15, AverageVORP
# .6262 adjusted Years13-15, AverageVORPIncrease (Tried Year12)
# .6062 adjusted Year15
# .6116 adjusted Years14-15
# .6263 adjusted Years13-15
# BEST RUN: .63 adjusted Years13 + Year15 (Tried Year11-12VORP, Pk, Age, VORPSum, AverageVORP, AverageVORPIncrease, Gpercent, TotalMP)
# POLYNOMIAL SUPERIOR!

qplot(onetosixteen_lr$fitted.values, year_onetosixteen_df$Year16VORP)
year_onetosixteen_df$PredLRVORP <- round(onetosixteen_lr$fitted.values, digits = 1)
year_onetosixteen_df$LRResiduals <- round(onetosixteen_lr$residuals, digits = 1)

# Polynomial regression
onetosixteen_poly <- lm(Year16VORP ~ poly(Year13VORP + AverageVORPIncrease + Year15VORP + Gpercent, 2, raw = TRUE), data = year_onetosixteen_df)
onetosixteen_poly
summary(onetosixteen_poly)
# .6098 for Years13-15, AverageVORP type1
# .6262 for Years13 + Years15 type1
# .6694 for Years13, Years15, AverageVORPIncrease type2 (not improved with type=3) (Tried Year11-14VORP, AverageVORP, Pk, Age, VORPSum, VORPIncrease, TotalMP)
# BEST RUN, BETTER THAN LR: .6703 for Years13,15, AverageVORPIncrease, Gpercent type2
str(summary(onetosixteen_poly))
plot(onetosixteen_poly)
qplot(year_onetosixteen_df$Year16VORP)

# Linear regression for Year 16 Stars
year_onetosixteen_df_stars <- filter(year_onetosixteen_df, Year15VORP >= 2, Year16VORP >= 1.5)
onetosixteen_lr_stars <- lm(Year16VORP ~ Year13VORP + Year14VORP + Year15VORP + VORPSum, data = year_onetosixteen_df_stars)
summary(onetosixteen_lr_stars)
year_onetosixteen_df_stars$fitted.values <- round(onetosixteen_lr_stars$fitted.values, digits = 2)
plot(onetosixteen_lr_stars)
# .7866 for Year13VORP + Year14VORP + Year15VORP + VORPSum
qplot(year_onetosixteen_df$VORP)
# LR for Year16 others
year_onetosixteen_df_other <- filter(year_onetosixteen_df, Year15VORP <= 1.9)
onetosixteen_lr_other <- lm(Year16VORP ~ Year15VORP + VORPSum, data = year_onetosixteen_df_other)
summary(onetosixteen_lr_other)
year_onetosixteen_df_other$fitted.values <- round(onetosixteen_lr_other$fitted.values, digits = 2)
plot(onetosixteen_lr_other)
# .3126 for Year15VORP + VORPSum

# 17th Year Train

vorp_onetoseventeen_df <- yearly_VORP_df[,1:18]
vorp_onetoseventeen_df <- vorp_onetoseventeen_df[complete.cases(vorp_onetoseventeen_df),]
vorp_onetoseventeen_df$VORPSum <- sapply(1:nrow(vorp_onetoseventeen_df), function(x) round(sum(vorp_onetoseventeen_df[x,(2:17)]), digits = 1))
year_onetoseventeen_df <- merge(years_pro_df_TOT,vorp_onetoseventeen_df)
year_onetoseventeen_df <- year_onetoseventeen_df[(year_onetoseventeen_df$YearsPro<=16),]
addTotalMPandGpercent(year_onetoseventeen_df, 16)
year_onetoseventeen_df <- df_updated[(df_updated$YearsPro==16),]
year_onetoseventeen_df[is.na(year_onetoseventeen_df)] <- 0
year_onetoseventeen_df$VORPIncrease <- year_onetoseventeen_df$Year16VORP - year_onetoseventeen_df$Year15VORP
year_onetoseventeen_df <- mutate(year_onetoseventeen_df, AverageVORP = round((VORPSum/YearsPro),digits=2))
year_onetoseventeen_df <- mutate(year_onetoseventeen_df, AverageVORPIncrease = round(((Year16VORP-Year15VORP)+(Year15VORP-Year14VORP)+(Year14VORP-Year13VORP)+(Year13VORP-Year12VORP)+(Year12VORP-Year11VORP)+(Year11VORP-Year10VORP)+(Year10VORP-Year9VORP)+(Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/15, digits = 2))


rf.label.years1to17 <- year_onetoseventeen_df$Year17VORP
set.seed(1234)
rf.train.years1to17 <- year_onetoseventeen_df
rf.train.years1to17 <- rf.train.years1to17[c("Year11VORP", "Year13VORP", "Year15VORP", "Year16VORP", "AverageVORP", "AverageVORPIncrease", "Pk")]
rf.years1to17 <- randomForest(x=rf.train.years1to17, y=rf.label.years1to17,importance = TRUE, ntree = 1000)
rf.years1to17
varImpPlot(rf.years1to17)
str(rf.years1to17)
# 73.52% for Year11-16, AverageVORP, AverageVORPIncrease (Tried Gpercent, VORPSum, VORPIncrease, TotalMP, Age, Pk)
# BEST RUN: 75.7% for Years11,13,15,16, AverageVORP, AverageVORPIncrease, Pk
qplot(rf.years1to17$predicted,year_onetoseventeen_df$Year17VORP)
year_onetoseventeen_df$PredVORP <- round(rf.years1to17$predicted, digits = 1)
year_onetoseventeen_df$Residuals <- year_onetoseventeen_df$Year17VORP - year_onetoseventeen_df$PredVORP

# Linear and Poly regression for Year 17
onetoseventeen_lr <- lm(Year17VORP ~ Year13VORP + Year15VORP + Year16VORP + Age, data = year_onetoseventeen_df)
onetoseventeen_lr
summary(onetoseventeen_lr)
str(summary(onetoseventeen_lr))
plot(onetoseventeen_lr)
# .6977 for Year11,13,15,16, AverageVORP, AverageVORPIncrease, Pk
# .7072 for Year11,13,15,16
# .7115 for Years11-16
# .7199 for Year13,15,16, Age (Tried AverageVORPIncrease, AverageVORP, Pk, VORPSum)
# RANDOM FOREST SUPERIOR!

qplot(onetoseventeen_lr$fitted.values, year_onetoseventeen_df$Year17VORP)
year_onetoseventeen_df$PredLRVORP <- round(onetoseventeen_lr$fitted.values, digits = 1)
year_onetoseventeen_df$LRResiduals <- round(onetoseventeen_lr$residuals, digits = 1)

# Polynomial regression
onetoseventeen_poly <- lm(Year17VORP ~ poly(Year13VORP + Year15VORP + Year16VORP + Age, 2, raw = TRUE), data = year_onetoseventeen_df)
onetoseventeen_poly
summary(onetoseventeen_poly)
# .6039 for Year13,15,16, Age type=2
str(summary(onetoseventeen_poly))
plot(onetoseventeen_poly)
qplot(year_onetoseventeen_df$Year17VORP)

# Linear regression for Year 17 Stars
year_onetoseventeen_df_stars <- filter(year_onetoseventeen_df, Year16VORP >= 1.5, Year17VORP >= 1.5)
onetoseventeen_lr_stars <- lm(Year17VORP ~ Year13VORP + Year14VORP + Year15VORP + VORPSum, data = year_onetoseventeen_df_stars)
summary(onetoseventeen_lr_stars)
year_onetoseventeen_df_stars$fitted.values <- round(onetoseventeen_lr_stars$fitted.values, digits = 2)
plot(onetoseventeen_lr_stars)
# .7866 for Year13VORP + Year14VORP + Year15VORP + VORPSum
qplot(year_onetoseventeen_df$VORP)
# LR for Year17 others
year_onetoseventeen_df_other <- filter(year_onetoseventeen_df, Year16VORP <= 1.4)
onetoseventeen_lr_other <- lm(Year17VORP ~ Year15VORP + Year16VORP + VORPSum, data = year_onetoseventeen_df_other)
summary(onetoseventeen_lr_other)
year_onetoseventeen_df_other$fitted.values <- round(onetoseventeen_lr_other$fitted.values, digits = 2)
plot(onetoseventeen_lr_other)
# .3126 for Year15VORP + VORPSum

# 18th Year Train

vorp_onetoeighteen_df <- yearly_VORP_df[,1:19]
vorp_onetoeighteen_df <- vorp_onetoeighteen_df[complete.cases(vorp_onetoeighteen_df),]
vorp_onetoeighteen_df$VORPSum <- sapply(1:nrow(vorp_onetoeighteen_df), function(x) round(sum(vorp_onetoeighteen_df[x,(2:18)]), digits = 1))
year_onetoeighteen_df <- merge(years_pro_df_TOT,vorp_onetoeighteen_df)
year_onetoeighteen_df <- year_onetoeighteen_df[(year_onetoeighteen_df$YearsPro<=17),]
addTotalMPandGpercent(year_onetoeighteen_df, 17)
year_onetoeighteen_df <- df_updated[(df_updated$YearsPro==17),]
year_onetoeighteen_df[is.na(year_onetoeighteen_df)] <- 0
year_onetoeighteen_df$VORPIncrease <- year_onetoeighteen_df$Year17VORP - year_onetoeighteen_df$Year16VORP
year_onetoeighteen_df <- mutate(year_onetoeighteen_df, AverageVORP = round((VORPSum/YearsPro),digits=2))
year_onetoeighteen_df <- mutate(year_onetoeighteen_df, AverageVORPIncrease = round(((Year17VORP-Year16VORP)+(Year16VORP-Year15VORP)+(Year15VORP-Year14VORP)+(Year14VORP-Year13VORP)+(Year13VORP-Year12VORP)+(Year12VORP-Year11VORP)+(Year11VORP-Year10VORP)+(Year10VORP-Year9VORP)+(Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/16, digits = 2))


rf.label.years1to18 <- year_onetoeighteen_df$Year18VORP
set.seed(1234)
rf.train.years1to18 <- year_onetoeighteen_df
rf.train.years1to18 <- rf.train.years1to18[c("Year15VORP", "Year16VORP", "Year17VORP", "AverageVORP", "Gpercent", "VORPSum", "VORPIncrease", "Age")]
rf.years1to18 <- randomForest(x=rf.train.years1to18, y=rf.label.years1to18,importance = TRUE, ntree = 1000)
rf.years1to18
varImpPlot(rf.years1to18)
str(rf.years1to18)
# BEST RUN: 50.01% for Years15-17, AverageVORP, Gpercent, VORPSum, VORPIncrease, Age
qplot(rf.years1to18$predicted,year_onetoeighteen_df$Year18VORP)
year_onetoeighteen_df$PredVORP <- round(rf.years1to18$predicted, digits = 1)
year_onetoeighteen_df$Residuals <- year_onetoeighteen_df$Year18VORP - year_onetoeighteen_df$PredVORP

# Linear and Poly regression for Year 18
onetoeighteen_lr <- lm(Year18VORP ~ Year16VORP, data = year_onetoeighteen_df)
onetoeighteen_lr
summary(onetoeighteen_lr)
str(summary(onetoeighteen_lr))
plot(onetoeighteen_lr)
# .5998 for Years16-17
# .611 for Year16 (Tried Age, Pk, AverageVORP, AverageVORPIncrease, VORPSum, Gpercent, TotalMP, Several different Years)
# Polynomial superior

qplot(onetoeighteen_lr$fitted.values, year_onetoeighteen_df$Year18VORP)
year_onetoeighteen_df$PredLRVORP <- round(onetoeighteen_lr$fitted.values, digits = 1)
year_onetoeighteen_df$LRResiduals <- round(onetoeighteen_lr$residuals, digits = 1)

# Polynomial regression
onetoeighteen_poly <- lm(Year18VORP ~ poly(Year16VORP + Gpercent, 2, raw = TRUE), data = year_onetoeighteen_df)
onetoeighteen_poly
summary(onetoeighteen_poly)
# .6026 for Year15-17, Age type=2
# .6633 for Year16, type=2
# .6715 for Year16, Gpercent
str(summary(onetoeighteen_poly))
plot(onetoeighteen_poly)
qplot(year_onetoeighteen_df$Year18VORP)

# 19th Year Train

vorp_onetonineteen_df <- yearly_VORP_df[,1:20]
vorp_onetonineteen_df <- vorp_onetonineteen_df[complete.cases(vorp_onetonineteen_df),]
vorp_onetonineteen_df$VORPSum <- sapply(1:nrow(vorp_onetonineteen_df), function(x) round(sum(vorp_onetonineteen_df[x,(2:19)]), digits = 1))
year_onetonineteen_df <- merge(years_pro_df_TOT,vorp_onetonineteen_df)
year_onetonineteen_df <- year_onetonineteen_df[(year_onetonineteen_df$YearsPro<=18),]
addTotalMPandGpercent(year_onetonineteen_df, 18)
year_onetonineteen_df <- df_updated[(df_updated$YearsPro==18),]
year_onetonineteen_df[is.na(year_onetonineteen_df)] <- 0
year_onetonineteen_df$VORPIncrease <- year_onetonineteen_df$Year18VORP - year_onetonineteen_df$Year17VORP
year_onetonineteen_df <- mutate(year_onetonineteen_df, AverageVORP = round((VORPSum/YearsPro),digits=2))
year_onetonineteen_df <- mutate(year_onetonineteen_df, AverageVORPIncrease = round(((Year18VORP-Year17VORP)+(Year17VORP-Year16VORP)+(Year16VORP-Year15VORP)+(Year15VORP-Year14VORP)+(Year14VORP-Year13VORP)+(Year13VORP-Year12VORP)+(Year12VORP-Year11VORP)+(Year11VORP-Year10VORP)+(Year10VORP-Year9VORP)+(Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/17, digits = 2))


rf.label.years1to19 <- year_onetonineteen_df$Year19VORP
set.seed(1234)
rf.train.years1to19 <- year_onetonineteen_df
rf.train.years1to19 <- rf.train.years1to19[("Year18VORP")]
rf.years1to19 <- randomForest(x=rf.train.years1to19, y=rf.label.years1to19,importance = TRUE, ntree = 1000)
rf.years1to19
varImpPlot(rf.years1to19)
str(rf.years1to19)
# Fairly horrible results, moving on
qplot(rf.years1to19$predicted,year_onetonineteen_df$Year19VORP)
year_onetonineteen_df$PredVORP <- round(rf.years1to19$predicted, digits = 1)
year_onetonineteen_df$Residuals <- year_onetonineteen_df$Year19VORP - year_onetonineteen_df$PredVORP

# Linear and Poly regression for Year 19
onetonineteen_lr <- lm(Year19VORP ~ Year18VORP, data = year_onetonineteen_df)
onetonineteen_lr
summary(onetonineteen_lr)
str(summary(onetonineteen_lr))
plot(onetonineteen_lr)
# .4475 (Tried a bunch of other ones)

qplot(onetonineteen_lr$fitted.values, year_onetonineteen_df$Year19VORP)
year_onetonineteen_df$PredLRVORP <- round(onetonineteen_lr$fitted.values, digits = 1)
year_onetonineteen_df$LRResiduals <- round(onetonineteen_lr$residuals, digits = 1)

# Polynomial regression
onetonineteen_poly <- lm(Year19VORP ~ poly(Year18VORP + Year17VORP + Year16VORP, 1, raw = TRUE), data = year_onetonineteen_df)
onetonineteen_poly
summary(onetonineteen_poly)
# BEST RUN: .4801 for Years16-18
str(summary(onetonineteen_poly))
plot(onetonineteen_poly)
qplot(year_onetonineteen_df$Year19VORP)

# GUIDE TO EACH YEAR
# Year 2 = LR .5359 
# Year 3 = LR .6108
# Year 4 = LR .6392
# Year 5 = LR .6530
# Year 6 = LR .6618
# Year 7 = LR .6966
# Year 8 = LR .6874
# Year 9 = LR .7004
# Year 10 = LR .6376
# Year 11 = LR .7202
# Year 12 = LR .7457
# Year 13 = LR .6634
# Year 14 = LR .6819
# Year 15 = LR .7259
# Year 16 = POLY .6703
# Year 17 = RF! .7570
# Year 18 = POLY .6715
# Year 19 = POLY .4801

# Rookie Predictions will be performed later.

# Year 2 Predictions
vorp_selection <- yearly_VORP_df[,1:2]
vorp_selection <- vorp_selection[complete.cases(vorp_selection),]
# No need for VORPSum here since we only have one recorded year of VORP
# Also, no VORPIncrease, AverageVORP, AverageVORPIncrease
year2_preds <- merge(years_pro_df_TOT,vorp_selection)
year2_preds <- filter(year2_preds, YearsPro==1, Season==2016)
addTotalMPandGpercent(year2_preds, 1)
year2_preds <- df_updated
year2_preds[is.na(year2_preds)] <- 0
year2_preds_stars <- filter(year2_preds, Year1VORP >= 1.5)
year2_preds_other <- filter(year2_preds, Year1VORP <= 1.4)
preds_stars <- data.frame(predict(onetotwo_lr_stars, year2_preds_stars, interval = "predict"))
year2_preds_stars <- select(year2_preds_stars, Player, Rk, Tm)
year2_preds_stars$Fit <- round(preds_stars$fit, digits = 2)
year2_preds_stars$Lwr <- round(preds_stars$lwr, digits = 2)
year2_preds_stars$Upr <- round(preds_stars$upr, digits = 2)
preds_other <- data.frame(predict(onetotwo_lr_other, year2_preds_other, interval = "predict"))
year2_preds_other <- select(year2_preds_other, Player, Rk, Tm)
year2_preds_other$Fit <- round(preds_other$fit, digits = 2)
year2_preds_other$Lwr <- round(preds_other$lwr, digits = 2)
year2_preds_other$Upr <- round(preds_other$upr, digits = 2)
preds_2017 <- year2_preds_stars
preds_2017 <- rbind(preds_2017, year2_preds_other)

# Year 3 Predictions
vorp_selection <- yearly_VORP_df[,1:3]
vorp_selection <- vorp_selection[complete.cases(vorp_selection),]
vorp_selection$VORPSum <- sapply(1:nrow(vorp_selection), function(x) round(sum(vorp_selection[x,(2:3)]), digits = 1))
year3_preds <- merge(years_pro_df_TOT,vorp_selection)
year3_preds <- filter(year3_preds, YearsPro<=2)
addTotalMPandGpercent(year3_preds, 2)
year3_preds <- df_updated
year3_preds <- filter(year3_preds, Season==2016)
year3_preds[is.na(year3_preds)] <- 0
year3_preds$VORPIncrease <- year3_preds$Year2VORP - year3_preds$Year1VORP
year3_preds <- mutate(year3_preds, AverageVORP = round((VORPSum/YearsPro),digits = 2))
# No AverageVORPIncrease yet
year3_preds_stars <- filter(year3_preds, Year2VORP >= 4)
year3_preds_other <- filter(year3_preds, Year2VORP <= 3.9)
# No Stars
preds_stars <- data.frame(predict(onetothree_lr_stars, year3_preds_stars, interval = "predict"))
year3_preds_stars <- select(year3_preds_stars, Player, Rk, Tm)
year3_preds_stars$Fit <- round(preds_stars$fit, digits = 2)
year3_preds_stars$Lwr <- round(preds_stars$lwr, digits = 2)
year3_preds_stars$Upr <- round(preds_stars$upr, digits = 2)
preds_other <- data.frame(predict(onetothree_lr_other, year3_preds_other, interval = "predict"))
year3_preds_other <- select(year3_preds_other, Player, Rk, Tm)
year3_preds_other$Fit <- round(preds_other$fit, digits = 2)
year3_preds_other$Lwr <- round(preds_other$lwr, digits = 2)
year3_preds_other$Upr <- round(preds_other$upr, digits = 2)
preds_2017 <- rbind(preds_2017,year3_preds_other)

# Year 4 Predictions
vorp_selection <- yearly_VORP_df[,1:4]
vorp_selection <- vorp_selection[complete.cases(vorp_selection),]
vorp_selection$VORPSum <- sapply(1:nrow(vorp_selection), function(x) round(sum(vorp_selection[x,(2:4)]), digits = 1))
year4_preds <- merge(years_pro_df_TOT,vorp_selection)
year4_preds <- filter(year4_preds, YearsPro<=3)
addTotalMPandGpercent(year4_preds, 3)
year4_preds <- df_updated
year4_preds <- filter(year4_preds, Season==2016)
year4_preds[is.na(year4_preds)] <- 0
year4_preds$VORPIncrease <- year4_preds$Year3VORP - year4_preds$Year2VORP
year4_preds <- mutate(year4_preds, AverageVORP = round((VORPSum/YearsPro),digits = 2))
year4_preds <- mutate(year4_preds, AverageVORPIncrease = round(((Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/2, digits = 2))
year4_preds_stars <- filter(year4_preds, Year3VORP >= 3)
year4_preds_other <- filter(year4_preds, Year3VORP <= 2.9)
preds_stars <- data.frame(predict(onetofour_lr_stars, year4_preds_stars, interval = "predict"))
year4_preds_stars <- select(year4_preds_stars, Player, Rk, Tm)
year4_preds_stars$Fit <- round(preds_stars$fit, digits = 2)
year4_preds_stars$Lwr <- round(preds_stars$lwr, digits = 2)
year4_preds_stars$Upr <- round(preds_stars$upr, digits = 2)
preds_other <- data.frame(predict(onetofour_lr_other, year4_preds_other, interval = "predict"))
year4_preds_other <- select(year4_preds_other, Player, Rk, Tm)
year4_preds_other$Fit <- round(preds_other$fit, digits = 2)
year4_preds_other$Lwr <- round(preds_other$lwr, digits = 2)
year4_preds_other$Upr <- round(preds_other$upr, digits = 2)
preds_2017 <- rbind(preds_2017, year4_preds_stars, year4_preds_other)

# Year 5 Predictions
vorp_selection <- yearly_VORP_df[,1:5]
vorp_selection <- vorp_selection[complete.cases(vorp_selection),]
vorp_selection$VORPSum <- sapply(1:nrow(vorp_selection), function(x) round(sum(vorp_selection[x,(2:5)]), digits = 1))
year5_preds <- merge(years_pro_df_TOT,vorp_selection)
year5_preds <- filter(year5_preds, YearsPro<=4)
addTotalMPandGpercent(year5_preds, 4)
year5_preds <- df_updated
year5_preds <- filter(year5_preds, Season==2016)
year5_preds[is.na(year5_preds)] <- 0
year5_preds$VORPIncrease <- year5_preds$Year4VORP - year5_preds$Year3VORP
year5_preds <- mutate(year5_preds, AverageVORP = round((VORPSum/YearsPro),digits = 2))
year5_preds <- mutate(year5_preds, AverageVORPIncrease = round(((Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/3, digits = 2))
year5_preds_stars <- filter(year5_preds, Year4VORP >= 2.2)
year5_preds_other <- filter(year5_preds, Year4VORP <= 2.1)
preds_stars <- data.frame(predict(onetofive_lr_stars, year5_preds_stars, interval = "predict"))
year5_preds_stars <- select(year5_preds_stars, Player, Rk, Tm)
year5_preds_stars$Fit <- round(preds_stars$fit, digits = 2)
year5_preds_stars$Lwr <- round(preds_stars$lwr, digits = 2)
year5_preds_stars$Upr <- round(preds_stars$upr, digits = 2)
preds_other <- data.frame(predict(onetofive_lr_other, year5_preds_other, interval = "predict"))
year5_preds_other <- select(year5_preds_other, Player, Rk, Tm)
year5_preds_other$Fit <- round(preds_other$fit, digits = 2)
year5_preds_other$Lwr <- round(preds_other$lwr, digits = 2)
year5_preds_other$Upr <- round(preds_other$upr, digits = 2)
preds_2017 <- rbind(preds_2017, year5_preds_stars, year5_preds_other)

# Year 6 Predictions
vorp_selection <- yearly_VORP_df[,1:6]
vorp_selection <- vorp_selection[complete.cases(vorp_selection),]
vorp_selection$VORPSum <- sapply(1:nrow(vorp_selection), function(x) round(sum(vorp_selection[x,(2:6)]), digits = 1))
year6_preds <- merge(years_pro_df_TOT,vorp_selection)
year6_preds <- filter(year6_preds, YearsPro<=5)
addTotalMPandGpercent(year6_preds, 5)
year6_preds <- df_updated
year6_preds <- filter(year6_preds, Season==2016)
year6_preds[is.na(year6_preds)] <- 0
year6_preds$VORPIncrease <- year6_preds$Year5VORP - year6_preds$Year4VORP
year6_preds <- mutate(year6_preds, AverageVORP = round((VORPSum/YearsPro),digits = 2))
year6_preds <- mutate(year6_preds, AverageVORPIncrease = round(((Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/4, digits = 2))
year6_preds_stars <- filter(year6_preds, Year5VORP >= 2.3 | Player == "Kyrie Irving" | Player == "Klay Thompson")
year6_preds_other <- filter(year6_preds, Year5VORP <= 2.2, Player != "Kyrie Irving", Player != "Klay Thompson")
preds_stars <- data.frame(predict(onetosix_lr_stars, year6_preds_stars, interval = "predict"))
year6_preds_stars <- select(year6_preds_stars, Player, Rk, Tm)
year6_preds_stars$Fit <- round(preds_stars$fit, digits = 2)
year6_preds_stars$Lwr <- round(preds_stars$lwr, digits = 2)
year6_preds_stars$Upr <- round(preds_stars$upr, digits = 2)
preds_other <- data.frame(predict(onetosix_lr_other, year6_preds_other, interval = "predict"))
year6_preds_other <- select(year6_preds_other, Player, Rk, Tm)
year6_preds_other$Fit <- round(preds_other$fit, digits = 2)
year6_preds_other$Lwr <- round(preds_other$lwr, digits = 2)
year6_preds_other$Upr <- round(preds_other$upr, digits = 2)
preds_2017 <- rbind(preds_2017, year6_preds_stars, year6_preds_other)

# Year 7 Predictions
vorp_selection <- yearly_VORP_df[,1:7]
vorp_selection <- vorp_selection[complete.cases(vorp_selection),]
vorp_selection$VORPSum <- sapply(1:nrow(vorp_selection), function(x) round(sum(vorp_selection[x,(2:7)]), digits = 1))
year7_preds <- merge(years_pro_df_TOT,vorp_selection)
year7_preds <- filter(year7_preds, YearsPro<=6)
addTotalMPandGpercent(year7_preds, 6)
year7_preds <- df_updated
year7_preds <- filter(year7_preds, Season==2016)
year7_preds[is.na(year7_preds)] <- 0
year7_preds$VORPIncrease <- year7_preds$Year6VORP - year7_preds$Year5VORP
year7_preds <- mutate(year7_preds, AverageVORP = round((VORPSum/YearsPro),digits = 2))
year7_preds <- mutate(year7_preds, AverageVORPIncrease = round(((Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/5, digits = 2))
year7_preds_stars <- filter(year7_preds, Year6VORP >= 2.6 | Player == "Blake Griffin" | Player == "Derrick Favors")
year7_preds_other <- filter(year7_preds, Year6VORP <= 2.3, Player != "Blake Griffin", Player != "Derrick Favors")
preds_stars <- data.frame(predict(onetoseven_lr_stars, year7_preds_stars, interval = "predict"))
year7_preds_stars <- select(year7_preds_stars, Player, Rk, Tm)
year7_preds_stars$Fit <- round(preds_stars$fit, digits = 2)
year7_preds_stars$Lwr <- round(preds_stars$lwr, digits = 2)
year7_preds_stars$Upr <- round(preds_stars$upr, digits = 2)
preds_other <- data.frame(predict(onetoseven_lr_other, year7_preds_other, interval = "predict"))
year7_preds_other <- select(year7_preds_other, Player, Rk, Tm)
year7_preds_other$Fit <- round(preds_other$fit, digits = 2)
year7_preds_other$Lwr <- round(preds_other$lwr, digits = 2)
year7_preds_other$Upr <- round(preds_other$upr, digits = 2)
preds_2017 <- rbind(preds_2017, year7_preds_stars, year7_preds_other)

# Year 8 Predictions
vorp_selection <- yearly_VORP_df[,1:8]
vorp_selection <- vorp_selection[complete.cases(vorp_selection),]
vorp_selection$VORPSum <- sapply(1:nrow(vorp_selection), function(x) round(sum(vorp_selection[x,(2:8)]), digits = 1))
year8_preds <- merge(years_pro_df_TOT,vorp_selection)
year8_preds <- filter(year8_preds, YearsPro<=7)
addTotalMPandGpercent(year8_preds, 7)
year8_preds <- df_updated
year8_preds <- filter(year8_preds, Season==2016)
year8_preds[is.na(year8_preds)] <- 0
year8_preds$VORPIncrease <- year8_preds$Year7VORP - year8_preds$Year6VORP
year8_preds <- mutate(year8_preds, AverageVORP = round((VORPSum/YearsPro),digits = 2))
year8_preds <- mutate(year8_preds, AverageVORPIncrease = round(((Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/6, digits = 2))
year8_preds_stars <- filter(year8_preds, Year7VORP >= 2.6)
year8_preds_other <- filter(year8_preds, Year7VORP <= 2.5)
preds_stars <- data.frame(predict(onetoeight_lr_stars, year8_preds_stars, interval = "predict"))
year8_preds_stars <- select(year8_preds_stars, Player, Rk, Tm)
year8_preds_stars$Fit <- round(preds_stars$fit, digits = 2)
year8_preds_stars$Lwr <- round(preds_stars$lwr, digits = 2)
year8_preds_stars$Upr <- round(preds_stars$upr, digits = 2)
preds_other <- data.frame(predict(onetoeight_lr_other, year8_preds_other, interval = "predict"))
year8_preds_other <- select(year8_preds_other, Player, Rk, Tm)
year8_preds_other$Fit <- round(preds_other$fit, digits = 2)
year8_preds_other$Lwr <- round(preds_other$lwr, digits = 2)
year8_preds_other$Upr <- round(preds_other$upr, digits = 2)
preds_2017 <- rbind(preds_2017, year8_preds_stars, year8_preds_other)

# Year 9 Predictions
vorp_selection <- yearly_VORP_df[,1:9]
vorp_selection <- vorp_selection[complete.cases(vorp_selection),]
vorp_selection$VORPSum <- sapply(1:nrow(vorp_selection), function(x) round(sum(vorp_selection[x,(2:9)]), digits = 1))
year9_preds <- merge(years_pro_df_TOT,vorp_selection)
year9_preds <- filter(year9_preds, YearsPro<=8)
addTotalMPandGpercent(year9_preds, 8)
year9_preds <- df_updated
year9_preds <- filter(year9_preds, Season==2016)
year9_preds[is.na(year9_preds)] <- 0
year9_preds$VORPIncrease <- year9_preds$Year8VORP - year9_preds$Year7VORP
year9_preds <- mutate(year9_preds, AverageVORP = round((VORPSum/YearsPro),digits = 2))
year9_preds <- mutate(year9_preds, AverageVORPIncrease = round(((Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/7, digits = 2))
year9_preds_stars <- filter(year9_preds, Year8VORP >= 2.5)
year9_preds_other <- filter(year9_preds, Year8VORP <= 2.4)
preds_stars <- data.frame(predict(onetonine_lr_stars, year9_preds_stars, interval = "predict"))
year9_preds_stars <- select(year9_preds_stars, Player, Rk, Tm)
year9_preds_stars$Fit <- round(preds_stars$fit, digits = 2)
year9_preds_stars$Lwr <- round(preds_stars$lwr, digits = 2)
year9_preds_stars$Upr <- round(preds_stars$upr, digits = 2)
preds_other <- data.frame(predict(onetonine_lr_other, year9_preds_other, interval = "predict"))
year9_preds_other <- select(year9_preds_other, Player, Rk, Tm)
year9_preds_other$Fit <- round(preds_other$fit, digits = 2)
year9_preds_other$Lwr <- round(preds_other$lwr, digits = 2)
year9_preds_other$Upr <- round(preds_other$upr, digits = 2)
preds_2017 <- rbind(preds_2017, year9_preds_stars, year9_preds_other)

# Year 10 Predictions
vorp_selection <- yearly_VORP_df[,1:10]
vorp_selection <- vorp_selection[complete.cases(vorp_selection),]
vorp_selection$VORPSum <- sapply(1:nrow(vorp_selection), function(x) round(sum(vorp_selection[x,(2:10)]), digits = 1))
year10_preds <- merge(years_pro_df_TOT,vorp_selection)
year10_preds <- filter(year10_preds, YearsPro<=9)
addTotalMPandGpercent(year10_preds, 9)
year10_preds <- df_updated
year10_preds <- filter(year10_preds, Season==2016)
year10_preds[is.na(year10_preds)] <- 0
year10_preds$VORPIncrease <- year10_preds$Year9VORP - year10_preds$Year8VORP
year10_preds <- mutate(year10_preds, AverageVORP = round((VORPSum/YearsPro),digits = 2))
year10_preds <- mutate(year10_preds, AverageVORPIncrease = round(((Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/8, digits = 2))
year10_preds_stars <- filter(year10_preds, Year9VORP >= 4)
year10_preds_other <- filter(year10_preds, Year9VORP <= 3.9)
preds_stars <- data.frame(predict(onetoten_lr_stars, year10_preds_stars, interval = "predict"))
year10_preds_stars <- select(year10_preds_stars, Player, Rk, Tm)
year10_preds_stars$Fit <- round(preds_stars$fit, digits = 2)
year10_preds_stars$Lwr <- round(preds_stars$lwr, digits = 2)
year10_preds_stars$Upr <- round(preds_stars$upr, digits = 2)
preds_other <- data.frame(predict(onetoten_lr_other, year10_preds_other, interval = "predict"))
year10_preds_other <- select(year10_preds_other, Player, Rk, Tm)
year10_preds_other$Fit <- round(preds_other$fit, digits = 2)
year10_preds_other$Lwr <- round(preds_other$lwr, digits = 2)
year10_preds_other$Upr <- round(preds_other$upr, digits = 2)
preds_2017 <- rbind(preds_2017, year10_preds_stars, year10_preds_other)

# Year 11 Predictions
vorp_selection <- yearly_VORP_df[,1:11]
vorp_selection <- vorp_selection[complete.cases(vorp_selection),]
vorp_selection$VORPSum <- sapply(1:nrow(vorp_selection), function(x) round(sum(vorp_selection[x,(2:11)]), digits = 1))
year11_preds <- merge(years_pro_df_TOT,vorp_selection)
year11_preds <- filter(year11_preds, YearsPro<=10)
addTotalMPandGpercent(year11_preds, 10)
year11_preds <- df_updated
year11_preds <- filter(year11_preds, Season==2016)
year11_preds[is.na(year11_preds)] <- 0
year11_preds$VORPIncrease <- year11_preds$Year10VORP - year11_preds$Year9VORP
year11_preds <- mutate(year11_preds, AverageVORP = round((VORPSum/YearsPro),digits = 2))
year11_preds <- mutate(year11_preds, AverageVORPIncrease = round(((Year10VORP-Year9VORP)+(Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/9, digits = 2))
year11_preds_stars <- filter(year11_preds, Year10VORP >= 2.2)
year11_preds_other <- filter(year11_preds, Year10VORP <= 2.1)
preds_stars <- data.frame(predict(onetoeleven_lr_stars, year11_preds_stars, interval = "predict"))
year11_preds_stars <- select(year11_preds_stars, Player, Rk, Tm)
year11_preds_stars$Fit <- round(preds_stars$fit, digits = 2)
year11_preds_stars$Lwr <- round(preds_stars$lwr, digits = 2)
year11_preds_stars$Upr <- round(preds_stars$upr, digits = 2)
preds_other <- data.frame(predict(onetoeleven_lr_other, year11_preds_other, interval = "predict"))
year11_preds_other <- select(year11_preds_other, Player, Rk, Tm)
year11_preds_other$Fit <- round(preds_other$fit, digits = 2)
year11_preds_other$Lwr <- round(preds_other$lwr, digits = 2)
year11_preds_other$Upr <- round(preds_other$upr, digits = 2)
preds_2017 <- rbind(preds_2017, year11_preds_stars, year11_preds_other)

# Year 12 Predictions
vorp_selection <- yearly_VORP_df[,1:12]
vorp_selection <- vorp_selection[complete.cases(vorp_selection),]
vorp_selection$VORPSum <- sapply(1:nrow(vorp_selection), function(x) round(sum(vorp_selection[x,(2:12)]), digits = 1))
year12_preds <- merge(years_pro_df_TOT,vorp_selection)
year12_preds <- filter(year12_preds, YearsPro<=11)
addTotalMPandGpercent(year12_preds, 11)
year12_preds <- df_updated
year12_preds <- filter(year12_preds, Season==2016)
year12_preds[is.na(year12_preds)] <- 0
year12_preds$VORPIncrease <- year12_preds$Year11VORP - year12_preds$Year10VORP
year12_preds <- mutate(year12_preds, AverageVORP = round((VORPSum/YearsPro),digits = 2))
year12_preds <- mutate(year12_preds, AverageVORPIncrease = round(((Year11VORP-Year10VORP)+(Year10VORP-Year9VORP)+(Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/10, digits = 2))
year12_preds_stars <- filter(year12_preds, Year11VORP >= 4)
year12_preds_other <- filter(year12_preds, Year11VORP <= 3.9)
preds_stars <- data.frame(predict(onetotwelve_lr_stars, year12_preds_stars, interval = "predict"))
year12_preds_stars <- select(year12_preds_stars, Player, Rk, Tm)
year12_preds_stars$Fit <- round(preds_stars$fit, digits = 2)
year12_preds_stars$Lwr <- round(preds_stars$lwr, digits = 2)
year12_preds_stars$Upr <- round(preds_stars$upr, digits = 2)
preds_other <- data.frame(predict(onetotwelve_lr_other, year12_preds_other, interval = "predict"))
year12_preds_other <- select(year12_preds_other, Player, Rk, Tm)
year12_preds_other$Fit <- round(preds_other$fit, digits = 2)
year12_preds_other$Lwr <- round(preds_other$lwr, digits = 2)
year12_preds_other$Upr <- round(preds_other$upr, digits = 2)
preds_2017 <- rbind(preds_2017, year12_preds_stars, year12_preds_other)

# Year 13 Predictions
vorp_selection <- yearly_VORP_df[,1:13]
vorp_selection <- vorp_selection[complete.cases(vorp_selection),]
vorp_selection$VORPSum <- sapply(1:nrow(vorp_selection), function(x) round(sum(vorp_selection[x,(2:13)]), digits = 1))
year13_preds <- merge(years_pro_df_TOT,vorp_selection)
year13_preds <- filter(year13_preds, YearsPro<=12)
addTotalMPandGpercent(year13_preds, 12)
year13_preds <- df_updated
year13_preds <- filter(year13_preds, Season==2016)
year13_preds[is.na(year13_preds)] <- 0
year13_preds$VORPIncrease <- year13_preds$Year12VORP - year13_preds$Year11VORP
year13_preds <- mutate(year13_preds, AverageVORP = round((VORPSum/YearsPro),digits = 2))
year13_preds <- mutate(year13_preds, AverageVORPIncrease = round(((Year12VORP-Year11VORP)+(Year11VORP-Year10VORP)+(Year10VORP-Year9VORP)+(Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/11, digits = 2))
year13_preds_stars <- filter(year13_preds, Year12VORP >= 4)
year13_preds_other <- filter(year13_preds, Year12VORP <= 3.9)
preds_stars <- data.frame(predict(onetothirteen_lr_stars, year13_preds_stars, interval = "predict"))
year13_preds_stars <- select(year13_preds_stars, Player, Rk, Tm)
year13_preds_stars$Fit <- round(preds_stars$fit, digits = 2)
year13_preds_stars$Lwr <- round(preds_stars$lwr, digits = 2)
year13_preds_stars$Upr <- round(preds_stars$upr, digits = 2)
preds_other <- data.frame(predict(onetothirteen_lr_other, year13_preds_other, interval = "predict"))
year13_preds_other <- select(year13_preds_other, Player, Rk, Tm)
year13_preds_other$Fit <- round(preds_other$fit, digits = 2)
year13_preds_other$Lwr <- round(preds_other$lwr, digits = 2)
year13_preds_other$Upr <- round(preds_other$upr, digits = 2)
preds_2017 <- rbind(preds_2017, year13_preds_stars, year13_preds_other)

# Year 14 Predictions
vorp_selection <- yearly_VORP_df[,1:14]
vorp_selection <- vorp_selection[complete.cases(vorp_selection),]
vorp_selection$VORPSum <- sapply(1:nrow(vorp_selection), function(x) round(sum(vorp_selection[x,(2:14)]), digits = 1))
year14_preds <- merge(years_pro_df_TOT,vorp_selection)
year14_preds <- filter(year14_preds, YearsPro<=13)
addTotalMPandGpercent(year14_preds, 13)
year14_preds <- df_updated
year14_preds <- filter(year14_preds, Season==2016)
year14_preds[is.na(year14_preds)] <- 0
year14_preds$VORPIncrease <- year14_preds$Year13VORP - year14_preds$Year12VORP
year14_preds <- mutate(year14_preds, AverageVORP = round((VORPSum/YearsPro),digits = 2))
year14_preds <- mutate(year14_preds, AverageVORPIncrease = round(((Year13VORP-Year12VORP)+(Year12VORP-Year11VORP)+(Year11VORP-Year10VORP)+(Year10VORP-Year9VORP)+(Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/12, digits = 2))
year14_preds_stars <- filter(year14_preds, Year13VORP >= 2.9)
year14_preds_other <- filter(year14_preds, Year13VORP <= 2.8)
# Here it makes more sense to use the other regression (LeBron does not conform to standard curve)
preds_stars <- data.frame(predict(onetofourteen_lr_other, year14_preds_stars, interval = "predict"))
year14_preds_stars <- select(year14_preds_stars, Player, Rk, Tm)
year14_preds_stars$Fit <- round(preds_stars$fit, digits = 2)
year14_preds_stars$Lwr <- round(preds_stars$lwr, digits = 2)
year14_preds_stars$Upr <- round(preds_stars$upr, digits = 2)
preds_other <- data.frame(predict(onetofourteen_lr_other, year14_preds_other, interval = "predict"))
year14_preds_other <- select(year14_preds_other, Player, Rk, Tm)
year14_preds_other$Fit <- round(preds_other$fit, digits = 2)
year14_preds_other$Lwr <- round(preds_other$lwr, digits = 2)
year14_preds_other$Upr <- round(preds_other$upr, digits = 2)
preds_2017 <- rbind(preds_2017, year14_preds_stars, year14_preds_other)

# Year 15 Predictions
vorp_selection <- yearly_VORP_df[,1:15]
vorp_selection <- vorp_selection[complete.cases(vorp_selection),]
vorp_selection$VORPSum <- sapply(1:nrow(vorp_selection), function(x) round(sum(vorp_selection[x,(2:15)]), digits = 1))
year15_preds <- merge(years_pro_df_TOT,vorp_selection)
year15_preds <- filter(year15_preds, YearsPro<=14)
addTotalMPandGpercent(year15_preds, 14)
year15_preds <- df_updated
year15_preds <- filter(year15_preds, Season==2016)
year15_preds[is.na(year15_preds)] <- 0
year15_preds$VORPIncrease <- year15_preds$Year14VORP - year15_preds$Year13VORP
year15_preds <- mutate(year15_preds, AverageVORP = round((VORPSum/YearsPro),digits = 2))
year15_preds <- mutate(year15_preds, AverageVORPIncrease = round(((Year14VORP-Year13VORP)+(Year13VORP-Year12VORP)+(Year12VORP-Year11VORP)+(Year11VORP-Year10VORP)+(Year10VORP-Year9VORP)+(Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/13, digits = 2))
year15_preds_stars <- filter(year15_preds, Year14VORP >= 2.9)
year15_preds_other <- filter(year15_preds, Year14VORP <= 2.8)
preds_stars <- data.frame(predict(onetofifteen_lr_other, year15_preds_stars, interval = "predict"))
year15_preds_stars <- select(year15_preds_stars, Player, Rk, Tm)
year15_preds_stars$Fit <- round(preds_stars$fit, digits = 2)
year15_preds_stars$Lwr <- round(preds_stars$lwr, digits = 2)
year15_preds_stars$Upr <- round(preds_stars$upr, digits = 2)
preds_other <- data.frame(predict(onetofifteen_lr_other, year15_preds_other, interval = "predict"))
year15_preds_other <- select(year15_preds_other, Player, Rk, Tm)
year15_preds_other$Fit <- round(preds_other$fit, digits = 2)
year15_preds_other$Lwr <- round(preds_other$lwr, digits = 2)
year15_preds_other$Upr <- round(preds_other$upr, digits = 2)
preds_2017 <- rbind(preds_2017, year15_preds_stars, year15_preds_other)

# Year 16 Predictions
vorp_selection <- yearly_VORP_df[,1:16]
vorp_selection <- vorp_selection[complete.cases(vorp_selection),]
vorp_selection$VORPSum <- sapply(1:nrow(vorp_selection), function(x) round(sum(vorp_selection[x,(2:16)]), digits = 1))
year16_preds <- merge(years_pro_df_TOT,vorp_selection)
year16_preds <- filter(year16_preds, YearsPro<=15)
addTotalMPandGpercent(year16_preds, 15)
year16_preds <- df_updated
year16_preds <- filter(year16_preds, Season==2016)
year16_preds[is.na(year16_preds)] <- 0
year16_preds$VORPIncrease <- year16_preds$Year15VORP - year16_preds$Year14VORP
year16_preds <- mutate(year16_preds, AverageVORP = round((VORPSum/YearsPro),digits = 2))
year16_preds <- mutate(year16_preds, AverageVORPIncrease = round(((Year15VORP-Year14VORP)+(Year14VORP-Year13VORP)+(Year13VORP-Year12VORP)+(Year12VORP-Year11VORP)+(Year11VORP-Year10VORP)+(Year10VORP-Year9VORP)+(Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/14, digits = 2))
year16_preds_stars <- filter(year16_preds, Year15VORP >= 2.9)
year16_preds_other <- filter(year16_preds, Year15VORP <= 2.8)
preds_stars <- data.frame(predict(onetosixteen_lr_other, year16_preds_stars, interval = "predict"))
year16_preds_stars <- select(year16_preds_stars, Player, Rk, Tm)
year16_preds_stars$Fit <- round(preds_stars$fit, digits = 2)
year16_preds_stars$Lwr <- round(preds_stars$lwr, digits = 2)
year16_preds_stars$Upr <- round(preds_stars$upr, digits = 2)
preds_other <- data.frame(predict(onetosixteen_lr_other, year16_preds_other, interval = "predict"))
year16_preds_other <- select(year16_preds_other, Player, Rk, Tm)
year16_preds_other$Fit <- round(preds_other$fit, digits = 2)
year16_preds_other$Lwr <- round(preds_other$lwr, digits = 2)
year16_preds_other$Upr <- round(preds_other$upr, digits = 2)
preds_2017 <- rbind(preds_2017, year16_preds_stars, year16_preds_other)

# Year 17 Predictions
vorp_selection <- yearly_VORP_df[,1:17]
vorp_selection <- vorp_selection[complete.cases(vorp_selection),]
vorp_selection$VORPSum <- sapply(1:nrow(vorp_selection), function(x) round(sum(vorp_selection[x,(2:17)]), digits = 1))
year17_preds <- merge(years_pro_df_TOT,vorp_selection)
year17_preds <- filter(year17_preds, YearsPro<=16)
addTotalMPandGpercent(year17_preds, 16)
year17_preds <- df_updated
year17_preds <- filter(year17_preds, Season==2016)
year17_preds[is.na(year17_preds)] <- 0
year17_preds$VORPIncrease <- year17_preds$Year16VORP - year17_preds$Year15VORP
year17_preds <- mutate(year17_preds, AverageVORP = round((VORPSum/YearsPro),digits = 2))
year17_preds <- mutate(year17_preds, AverageVORPIncrease = round(((Year16VORP-Year15VORP)+(Year15VORP-Year14VORP)+(Year14VORP-Year13VORP)+(Year13VORP-Year12VORP)+(Year12VORP-Year11VORP)+(Year11VORP-Year10VORP)+(Year10VORP-Year9VORP)+(Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/15, digits = 2))
preds <- data.frame(predict(onetoseventeen_lr, year17_preds, interval = "predict"))
year17_preds <- select(year17_preds, Player, Rk, Tm)
year17_preds$Fit <- round(preds$fit, digits = 2)
year17_preds$Lwr <- round(preds$lwr, digits = 2)
year17_preds$Upr <- round(preds$upr, digits = 2)
preds_2017 <- rbind(preds_2017,year17_preds)

# Year 18 Predictions
vorp_selection <- yearly_VORP_df[,1:18]
vorp_selection <- vorp_selection[complete.cases(vorp_selection),]
vorp_selection$VORPSum <- sapply(1:nrow(vorp_selection), function(x) round(sum(vorp_selection[x,(2:18)]), digits = 1))
year18_preds <- merge(years_pro_df_TOT,vorp_selection)
year18_preds <- filter(year18_preds, YearsPro<=17)
addTotalMPandGpercent(year18_preds, 17)
year18_preds <- df_updated
year18_preds <- filter(year18_preds, Season==2016)
year18_preds[is.na(year18_preds)] <- 0
year18_preds$VORPIncrease <- year18_preds$Year17VORP - year18_preds$Year16VORP
year18_preds <- mutate(year18_preds, AverageVORP = round((VORPSum/YearsPro),digits = 2))
year18_preds <- mutate(year18_preds, AverageVORPIncrease = round(((Year17VORP-Year16VORP)+(Year16VORP-Year15VORP)+(Year15VORP-Year14VORP)+(Year14VORP-Year13VORP)+(Year13VORP-Year12VORP)+(Year12VORP-Year11VORP)+(Year11VORP-Year10VORP)+(Year10VORP-Year9VORP)+(Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/16, digits = 2))
preds <- data.frame(predict(onetoeighteen_poly, year18_preds, interval = "predict"))
year18_preds <- select(year18_preds, Player, Rk, Tm)
year18_preds$Fit <- round(preds$fit, digits = 2)
year18_preds$Lwr <- round(preds$lwr, digits = 2)
year18_preds$Upr <- round(preds$upr, digits = 2)
preds_2017 <- rbind(preds_2017,year18_preds)

# Year 19 Predictions
vorp_selection <- yearly_VORP_df[,1:19]
vorp_selection <- vorp_selection[complete.cases(vorp_selection),]
vorp_selection$VORPSum <- sapply(1:nrow(vorp_selection), function(x) round(sum(vorp_selection[x,(2:19)]), digits = 1))
year19_preds <- merge(years_pro_df_TOT,vorp_selection)
year19_preds <- filter(year19_preds, YearsPro<=18)
addTotalMPandGpercent(year19_preds, 18)
year19_preds <- df_updated
year19_preds <- filter(year19_preds, Season==2016)
year19_preds[is.na(year19_preds)] <- 0
year19_preds$VORPIncrease <- year19_preds$Year18VORP - year19_preds$Year17VORP
year19_preds <- mutate(year19_preds, AverageVORP = round((VORPSum/YearsPro),digits = 2))
year19_preds <- mutate(year19_preds, AverageVORPIncrease = round(((Year18VORP-Year17VORP)+(Year17VORP-Year16VORP)+(Year16VORP-Year15VORP)+(Year15VORP-Year14VORP)+(Year14VORP-Year13VORP)+(Year13VORP-Year12VORP)+(Year12VORP-Year11VORP)+(Year11VORP-Year10VORP)+(Year10VORP-Year9VORP)+(Year9VORP-Year8VORP)+(Year8VORP-Year7VORP)+(Year7VORP-Year6VORP)+(Year6VORP-Year5VORP)+(Year5VORP-Year4VORP)+(Year4VORP-Year3VORP)+(Year3VORP-Year2VORP)+(Year2VORP-Year1VORP))/17, digits = 2))
preds <- data.frame(predict(onetonineteen_poly, year19_preds, interval = "predict"))
year19_preds <- select(year19_preds, Player, Rk, Tm)
year19_preds$Fit <- round(preds$fit, digits = 2)
year19_preds$Lwr <- round(preds$lwr, digits = 2)
year19_preds$Upr <- round(preds$upr, digits = 2)
preds_2017 <- rbind(preds_2017,year19_preds)
preds_holder <- preds_2017
write.csv(preds_2017, "preds_2017.csv", col.names = TRUE)
# Year 2 = LR .5359 
# Year 3 = LR .6108
# Year 4 = LR .6392
# Year 5 = LR .6530
# Year 6 = LR .6618
# Year 7 = LR .6966
# Year 8 = LR .6874
# Year 9 = LR .7004
# Year 10 = LR .6376
# Year 11 = LR .7202
# Year 12 = LR .7457
# Year 13 = LR .6634
# Year 14 = LR .6819
# Year 15 = LR .7259
# Year 16 = POLY .6703
# Year 17 = RF! .7570 (Switch to LR)
# Year 18 = POLY .6715
# Year 19 = POLY .4801

# Creating Projections for Newly Drafted players using BPM as key variable (limited to 2010 onwards)
# BBR player names differ between college and NBA...
draft_2011to2015 <- filter(draft_player_stats_df, DraftYear >= 2011, DraftYear <2016)
draft_2011to2015_college <- filter(draft_2011to2015, College != "")
# Formatting names for the bbr links
first_names <- NULL
for (i in 1:236) {
  first_names <- c(first_names,test_names[[i]][1])
}
first_names <- gsub("[.]", "", first_names)
first_names <- gsub("[']", "", first_names)
first_names <- tolower(first_names)
last_names <- NULL
for (i in 1:236) {
  last_names <- c(last_names,test_names[[i]][2])
}
last_names <- gsub("[.]", "", last_names)
last_names <- gsub("[']", "", last_names)
last_names <- tolower(last_names)
third_names <- NULL
for (i in 1:236) {
  third_names <- c(third_names,test_names[[i]][3])
}
third_names <- gsub("[.]", "", third_names)
third_names <- gsub("[']", "", third_names)
third_names <- tolower(third_names)
college_names <- data.frame(FirstNames = as.character(first_names), LastNames = as.character(last_names), ThirdNames = as.character(third_names))
college_names$FirstNames <- as.character(college_names$FirstNames)
college_names$LastNames <- as.character(college_names$LastNames)
college_names$ThirdNames <- as.character(college_names$ThirdNames)
college_names[187,2] <- "devyn-marble"
write.csv(college_names, "college_names.csv")
college_names <- read.csv("college_names.csv")
college_names <- college_names[,-1]
college_names[51,3] <- "2"
college_names[125,3] <- "2"
college_names <- college_names[-132,]
college_names[199,3] <- "2"
college_names[218,3] <- "5"
college_names[222,3] <- "2"
college_names[126,3] <- "4"
college_names[118,3] <- "2"
college_names[134,3] <- "3"

scrapeCollegeStats <- function() {
  bbr_URLs <- NULL
  for (i in 1:235) {
    bbr_link <- paste("http://www.sports-reference.com/cbb/players/", college_names[i,1], "-", college_names[i,2], "-", college_names[i,3], ".html", sep = "")
    bbr_URLs <- c(bbr_URLs, bbr_link)
  }
  college_train <- NULL
  draft_2011to2015_college <- draft_2011to2015_college[-132,]
  player_names <- as.character(draft_2011to2015_college$Player)
  player_pk <- as.numeric(draft_2011to2015_college$Pk)
  player_dy <- as.numeric(draft_2011to2015_college$DraftYear)
  player_id <- as.numeric(draft_2011to2015_college$ID)
  counter <- 1
  for (i in bbr_URLs) {
    print(i)
    link <- read_html(i)
    current_link <- link %>%
      html_nodes("table") %>%
      html_table(fill = TRUE)
    current_table <- current_link[[5]]
    current_table["Player"] <- player_names[counter]
    current_table["Pk"] <- player_pk[counter]
    current_table["DraftYear"] <- player_dy[counter]
    current_table["ID"] <- player_id[counter]
    college_train <- rbind(college_train, current_table)
    counter <- counter + 1
  }
  college_train <<- college_train
}
scrapeCollegeStats()
# Ok, all errors fixed. Grab undrafted players and intl's.
undrafted_2011to2015 <- filter(years_pro_df_TOT, Drafted == 0, Season >= 2012, YearsPro == 1)
internationals_2011to2015 <- filter(draft_2011to2015, DraftYear >= 2011, College == "")
internationals_2011to2015$College <- "International"
# Since this is going to remain a fairly simple regression, we'll only predict for drafted college players
# Intl's will be assigned a mean average of the Pk in their position
write.csv(college_train, "college_train.csv")
college_train <- read.csv("college_train.csv")
first_year_2011to2015 <- filter(years_pro_df_TOT, DraftYear >= 2011, Season >= 2011, YearsPro == 1)
first_year_2011to2015 <- first_year_2011to2015[c("Player", "Age", "VORP", "Season", "Pk", "DraftYear")]
first_year_2011to2015$Player <- as.character(first_year_2011to2015$Player)

collegestats_2011 <- filter(college_train, DraftYear == 2011, Season == "2010-11")
collegestats_2012 <- filter(college_train, DraftYear == 2012, Season == "2011-12")
collegestats_2013 <- filter(college_train, DraftYear == 2013, Season == "2012-13")
collegestats_2014 <- filter(college_train, DraftYear == 2014, Season == "2013-14")
collegestats_2015 <- filter(college_train, DraftYear == 2015, Season == "2014-15")
collected_collegestats <- rbind(collegestats_2011, collegestats_2012, collegestats_2013, collegestats_2014, collegestats_2015)
collected_collegestats$Player <- as.character(collected_collegestats$Player)
collected_collegestats_subset <- collected_collegestats[(collected_collegestats$Player %in% first_year_2011to2015$Player),]
first_year_2011to2015_subset <- first_year_2011to2015[(first_year_2011to2015$Player %in% collected_collegestats_subset$Player),]
first_year_2011to2015_subset <- arrange(first_year_2011to2015_subset, DraftYear, Pk)
write.csv(first_year_2011to2015_subset, "first_year_2011to2015_subset.csv")
write.csv(collected_collegestats_subset, "collected_collegestats_subset.csv")
college_train_cleaned <- read.csv("college_train_cleaned.csv")
college_train_cleaned <- college_train_cleaned[,-1]

# Train for College Players
rf.label.college <- college_train_cleaned$VORP
set.seed(1234)
rf.train.college <- college_train_cleaned
rf.train.college <- rf.train.college[c("BPM", "Pk", "DBPM", "WS")]
rf.college <- randomForest(x=rf.train.college, y=rf.label.college,importance = TRUE, ntree = 1000)
rf.college
varImpPlot(rf.college)
str(rf.college)
# 19.48 for BPM, Pk, DBPM, WS
qplot(rf.college$predicted,college_train_cleaned$VORP)
college_train_cleaned$PredVORP <- round(rf.college$predicted, digits = 2)

# Linear and Polynomial Regression for Year 4
college_lr <- lm(VORP ~ BPM + WS + PER + DBPM, data = college_train_cleaned)
summary(college_lr)
plot(college_lr)
# .1312 for BPM, Pk, Age (Pk, Age not significant)
# .1605 for BPM, WS
# .1677 for BPM, WS, PER
# .1970 for BPM, WS, PER, DBPM (Tried WS.40)
qplot(college_train_cleaned$VORP,college_train_cleaned$Pk)

college_poly <- lm(VORP ~ poly(BPM + WS.40 + DBPM + PER + FTr, 3, raw = TRUE), data = college_train_cleaned)
college_poly
summary(college_poly)
# .1781 for BPM, WS, PER, DBPM, type = 3
# .2035 for BPM type=3
# .2038 for BPM, WS.40 type =3
# .2211 for BPM, DBPM, WS.40 type =2
# .2344 for BPM, DBPM, WS.40 type =3
# .2280 for BPM, DBPM, WS.40, PER type =3 (with solid p values)
# BEST RUN: .2294 for BPM, DBPM, WS.40, PER, FTr type=3

# Grabbing 2016 Draft Table
current_link <- read_html("http://www.basketball-reference.com/draft/NBA_2016.html") %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)
draft_2016 <- data.frame(current_link[[1]])
draft_2016 <- sapply(draft_2016, as.character)
colnames(draft_2016) <- draft_2016[1, ]
draft_2016 <- data.frame(draft_2016[-1,])
draft_2016$College <- as.character(draft_2016$College)
draft_2016 <- draft_2016[-31,]
draft_2016_collegeplayers <- filter(draft_2016, College != "")
draft_2016_internationals <- filter(draft_2016, College == "")

# Formatting 2016 names
draft_2016_collegeplayers$Player <- as.character(draft_2016_collegeplayers$Player)
test_names <- strsplit(draft_2016_collegeplayers$Player, " ")
first_names <- NULL
for (i in 1:44) {
  first_names <- c(first_names,test_names[[i]][1])
}
first_names <- gsub("[.]", "", first_names)
first_names <- gsub("[']", "", first_names)
first_names <- tolower(first_names)
last_names <- NULL
for (i in 1:44) {
  last_names <- c(last_names,test_names[[i]][2])
}
last_names <- gsub("[.]", "", last_names)
last_names <- gsub("[']", "", last_names)
last_names <- tolower(last_names)
third_names <- NULL
for (i in 1:44) {
  third_names <- c(third_names,test_names[[i]][3])
}
third_names <- gsub("[.]", "", third_names)
third_names <- gsub("[']", "", third_names)
third_names <- tolower(third_names)
college_names_2016 <- data.frame(FirstNames = as.character(first_names), LastNames = as.character(last_names), ThirdNames = as.character(third_names))
college_names_2016$FirstNames <- as.character(college_names_2016$FirstNames)
college_names_2016$LastNames <- as.character(college_names_2016$LastNames)
college_names_2016$ThirdNames <- as.character(college_names_2016$ThirdNames)
write.csv(college_names_2016, "college_names_2016.csv")
college_names_2016 <- read.csv("college_names_2016.csv")
college_names_2016$FirstNames <- as.character(college_names_2016$FirstNames)
college_names_2016[7,1] <- "marquese"
college_names_2016[39,1] <- "kahlil"

scrape2016CollegeStats <- function() {
  bbr_URLs <- NULL
  for (i in 1:44) {
    bbr_link <- paste("http://www.sports-reference.com/cbb/players/", college_names_2016[i,1], "-", college_names_2016[i,2], "-", college_names_2016[i,3], ".html", sep = "")
    bbr_URLs <- c(bbr_URLs, bbr_link)
  }
  college_2016_predict <- NULL
  player_names <- as.character(draft_2016_collegeplayers$Player)
  player_pk <- as.numeric(draft_2016_collegeplayers$Pk)
  counter <- 1
  for (i in bbr_URLs) {
    print(i)
    link <- read_html(i)
    current_link <- link %>%
      html_nodes("table") %>%
      html_table(fill = TRUE)
    current_table <- current_link[[5]]
    current_table["Player"] <- player_names[counter]
    current_table["Pk"] <- player_pk[counter]
    current_table["DraftYear"] <- 2016
    college_2016_predict <- rbind(college_2016_predict, current_table)
    counter <- counter + 1
  }
  college_2016_predict <<- college_2016_predict
}
scrape2016CollegeStats()    
write.csv(college_2016_predict, "college_2016_predict.csv")
college_2016_predict_cleaned <- read.csv("college_2016_predict_cleaned.csv")

# Good, now predicting first year VORP for college players
# Using the Random Forest here and calcualting 95% prediction intervals!
preds <- predict(rf.college, college_2016_predict_cleaned, predict.all = TRUE)
college_preds <- select(college_2016_predict_cleaned, Player)
college_preds$Rk <- NA
teams_add <- filter(draft_2016, College != "")
college_preds$Tm <- teams_add$Tm
college_preds$Fit <- round(preds$aggregate, digits = 2)
preds.int <- apply(preds$individual, 1, function(x) {
  c( mean(x)) + c(-1,1)*sd(x) + quantile(x, c(0.025,0.975))
})
t(preds.int)
college_preds$Lwr <- round(preds.int[1,], digits = 2)
college_preds$Upr <- round(preds.int[2,], digits = 2)
college_preds
preds_holder <- preds_2017
preds_2017 <- rbind(preds_2017,college_preds)

# Since we don't have time to go into scraping international stats, we will just average the first year
# VORP production of each Pk and apply that to the intl's

year_one_df <- filter(years_pro_df_TOT, YearsPro == 1)
filter(year_one_df, Pk==1)
average_rookie_VORP <- data.frame(matrix(0, ncol = 60, nrow = 1))
for (i in 1:60) {
  pk_holder <- filter(year_one_df, Pk == i)
  pk_holder <- round((sum(pk_holder$VORP)/length(pk_holder)), digits = 2)
  average_rookie_VORP[,i] <- pk_holder
}
international_preds <- select(draft_2016_internationals, Player)
international_preds$Rk <- NA
international_preds$Tm <- draft_2016_internationals$Tm
international_preds$Fit <- c(.56, .33, -.12, -.12, -.13, .05, .28, -.02, -.2, -.08, -.1, -.03, -.06, -.09, -.11, .04)
international_preds$Lwr <- international_preds$Fit - 1.5
international_preds$Upr <- international_preds$Fit + 1.5
preds_2017 <- rbind(preds_2017, international_preds)
write.csv(preds_2017, "preds_2017_final_version2.csv")

preds_2017_final <- read.csv("preds_2017_final.csv")
preds_2017_final <- filter(preds_2017_final, Notes != "Stash")
preds_2017_final$Player <- as.character(preds_2017_final$Player)
preds_2017_final$Tm <- as.character(preds_2017_final$Tm)

preds_2017_final_version2_noinjured <- read.csv("preds_2017_final_version2.csv")
preds_2017_final_version2_noinjured <- filter(preds_2017_final_version2, Notes != "Stash")
preds_2017_final_version2_noinjured$Player <- as.character(preds_2017_final_version2$Player)
preds_2017_final_version2_noinjured$Tm <- as.character(preds_2017_final_version2$Tm)
preds_2017_final_version2_noinjured <- filter(preds_2017_final_version2_noinjured, Notes != "Injured")

TeamVORP_2017 <- read.csv("TeamVORP_2017.csv")
TeamVORP_2017$Team <- as.character(TeamVORP_2017$Team)
colnames(TeamVORP_2017)[1] <- "Tm"

# Calculating Team VORP
calculateTeamVORP <- function() {
  for (i in 1:30) {
    tm <- TeamVORP_2017[i,1]
    selection <- filter(preds_2017_noinjured_FINAL, Tm == tm)
    selection <- sum(selection$Fit)
    TeamVORP_2017[i,2] <- selection
  }
  Team_Projections_2017 <<- TeamVORP_2017
}
calculateTeamVORP()
colnames(Team_Projections_2017)[2] <- "Team_VORP"
summary(WandTeamVORP_lr_2012removed)

# Compensating for LeBron's outlier status (+1.5) and Kyrie's outlier fit (+2)
preds_2017_final_version2[9,4] <- 6.84
preds_2017_final_version2[9,6] <- 8.47
preds_2017_final_version2[65,4] <- 3.57
preds_2017_final_version2[65,5] <- .24
preds_2017_final_version2[65,6] <- 6.89
# Adjusting cavs
Team_Projections_2017[6,2] <- 16.66
# Towns is likely undervalued, adding one VORP
preds_2017_final_version2_noinjured[19,4:6] <- c(4.79,2.36,7.22)
write.csv(preds_2017_final_version2_noinjured, "preds_2017_noinjured.csv")
preds_2017_noinjured_FINAL <- read.csv("preds_2017_noinjured.csv")

team_v_2017 <- Team_Projections_2017$Team_VORP
colnames(Team_Projections_2017)[2] <- "Team_VORP"
win_projection2017_FINAL <- data.frame(predict(WandTeamVORP_lr_2012removed, Team_Projections_2017, interval = "predict"))
Team_Projections_2017$Fit <- round(win_projection2017_version2$fit, digits = 1)
Team_Projections_2017$Lwr <- round(win_projection2017_version2$lwr, digits = 1)
Team_Projections_2017$Upr <- round(win_projection2017_version2$upr, digits = 1)

Team_Projections_2017_noinjured <- Team_Projections_2017

preds <- data.frame(predict(onetonineteen_poly, year19_preds, interval = "predict"))
year19_preds <- select(year19_preds, Player, Rk, Tm)
year19_preds$Fit <- round(preds$fit, digits = 2)
year19_preds$Lwr <- round(preds$lwr, digits = 2)
year19_preds$Upr <- round(preds$upr, digits = 2)
# I fear that the projection undervalues the production of stars. Why not create a new projection only for those stars?
Team_Projections_2017_withinjured <- Team_Projections_2017
write.csv(Team_Projections_2017_withinjured, "Team_Projections_2017_withinjured.csv")
write.csv(Team_Projections_2017_noinjured, "Team_Projections_2017_noinjured.csv")
elo_relationship_df <- data.frame("Elo"=c(1200,1300,1400,1500,1600,1700,1800), "Wins"=c(15,22,31,41,51,60,67))
elo_lr <- lm(Elo ~ Wins, data = elo_relationship_df)
summary(elo_lr)
colnames(Team_Projections_2017_noinjured)[3] <- "Wins"
elo_fit <- predict(elo_lr, Team_Projections_2017_noinjured)
Team_Projections_2017_noinjured$EstimatedElo <- round(elo_fit)

# Ok, last refinement should be to run an elo simulation of the season (helps take schedule/quality of opponent into account)
# Structure--take in NBA schedule
# Assign Home-Away designators
# Predict outcome/mov
# Use RNG to simulate outcome
# Detemine winner
# Adjust elo rating
# Expected example using NYK vs. CLE (Home)
schedule_2017_df <- read.csv("schedule_2017.csv", header = TRUE)
elo_2017_df <- Team_Projections_2017_noinjured
elo_2017_df$CurrentElo <- elo_2017_df$EstimatedElo
elo_2017_df$CurrentWins <- numeric(30)
elo_2017_df$CurrentLosses <- numeric(30)
expected_away <- 1/(1 + (10^(((1656+100)-1421)/400)))
.1269261
expected_home <- 1/(1 + (10^((1421-(1656+100))/400)))
.8730739
# RNG
runif(1,0,1)
#Sample results .1137034 (cleveland loss)
result_away <- 1421+(20*(.8862966-.1269261))
result_away
# 1436.187 (NYK goes up because RNG result was .1137034 (less than expected result), making actual result (1-.1137034))
result_home <- 1656+(20*(.1137034-.8730739))
result_home
# 1640.813 (CLE goes down)
# .5728534(win by less than expected)
result_away <- round(1421+(20*(0.4271466-.1269261)), digits = 1)
result_away
# 1427.004 (NYK still goes up because it lost by less than expected)
result_home <- round(1656+(20*(.5728534-.8730739)), digits = 1)
result_home
# 1649.996 (CLE still goes down because it won by less than expected)
simulate2017season <- function() {
  for (i in 1:1230) {
    k = 0
    matchup <- filter(schedule_2017_df, GmRk == i)
    away_team <- filter(elo_2017_df, Tm == matchup$Away)
    home_team <- filter(elo_2017_df, Tm == matchup$Home)
    away_elo <- away_team$CurrentElo
    home_elo <- home_team$CurrentElo
    expected_away <- 1/(1 + (10^(((home_elo+100)-away_elo)/400)))
    expected_home <- 1/(1 + (10^((away_elo-(100+home_elo))/400)))
    if (expected_home > expected_away) {
      expected_away_range <- c(0,expected_away)
      expected_home_range <- c((expected_away+0.00000001),1)
    }
    else if (expected_away > expected_home) {
      expected_away_range <- c((expected_home+0.00000001),1)
      expected_home_range <- c(0,expected_home)
    }
    result <- runif(1,0,1)
    # Home team favored
    if (expected_home >= expected_away) {
      # Home win
      if (result >= expected_home_range[1] & result <= expected_home_range[2]) {
        away_team$CurrentLosses <- away_team$CurrentLosses + 1
        home_team$CurrentWins <- home_team$CurrentWins + 1
        away_team$CurrentElo <- round(away_elo+(k*((1-result)-expected_away)), digits = 1)
        home_team$CurrentElo <- round(home_elo+(k*(result-expected_home)), digits = 1)
      }
      # Away win
      else if (result >= expected_away_range[1] & result <= expected_away_range[2]) {
        # No need for result_min calc since home is favored
        away_team$CurrentWins <- away_team$CurrentWins + 1
        home_team$CurrentLosses <- home_team$CurrentLosses + 1
        away_team$CurrentElo <- round(away_elo+(k*((1-result)-expected_away)), digits = 1)
        home_team$CurrentElo <- round(home_elo+(k*(result-expected_home)), digits = 1)
      }
    }
    # Away team favored
    else if (expected_home < expected_away) {
      # Home win
      if (result >= expected_home_range[1] & result <= expected_home_range[2]) {
        away_team$CurrentLosses <- away_team$CurrentLosses + 1
        home_team$CurrentWins <- home_team$CurrentWins + 1
        away_team$CurrentElo <- round(away_elo+(k*(result-expected_away)), digits = 1)
        home_team$CurrentElo <- round(home_elo+(k*((1-result)-expected_home)), digits = 1)
      }
      # Away win
      else if (result >= expected_away_range[1] & result <= expected_away_range[2]) {
        away_team$CurrentWins <- away_team$CurrentWins + 1
        home_team$CurrentLosses <- home_team$CurrentLosses + 1
        away_team$CurrentElo <- round(away_elo+(k*(result-expected_away)), digits = 1)
        home_team$CurrentElo <- round(home_elo+(k*((1-result)-expected_home)), digits = 1)
      }
    }
    elo_2017_df[(elo_2017_df$Tm==away_team$Tm),] <- away_team
    elo_2017_df[(elo_2017_df$Tm==home_team$Tm),] <- home_team
  }
  holder_elo_df <<- elo_2017_df
}
simulate2017season()
season_simulations_df <- data.frame("Tm"=holder_elo_df$Tm, "Elo"=holder_elo_df$CurrentElo, "Wins"=holder_elo_df$CurrentWins, "Losses"=holder_elo_df$CurrentLosses)
simulateMultipleSeasons <- function(simulation_num) {
  for (x in 1:simulation_num) {
    simulate2017season()
    print(x)
    season_simulations_df <<- mutate(season_simulations_df, Wins = (Wins+holder_elo_df$CurrentWins), Losses = (Losses+holder_elo_df$CurrentLosses))
  }
  season_simulations_df <<- mutate(season_simulations_df, Wins = Wins/(simulation_num+1), Losses = Losses/(simulation_num+1))
}
simulateMultipleSeasons(1900)
# 2000 simulations
write.csv(season_simulations_df, "season_predictions_FINAL.csv")
season_predictions_FINAL <- read.csv("season_predictions_FINAL.csv")
season_predictions_FINAL <- season_predictions_FINAL[,-1]


# MOV calc to adjust range of outcomes
((20+3)^.8)/(7.5+(.006*650))
# Calculating relationship between elo diff and point spread
1757-1445 #312 diff, spread +11 for CLE
1664-1598 #66 diff, spead +2.5 for POR
1929-1617 #312
312/11 #1 PT per 28.4 elo diff
66/2.5 #1 PT per 26.4 elo diff
(1558-1472)/3 # 1 PT per 28.6
(1565-1416)/5 # 1 PT per 29.8
(1662-1310)/12 # 29.3
(1703-1478)/8 #28.1
(1507-1534)/1 #27
(1552-1486)/2.5 #26.4
(1550-1564)/.5 #28
(1479-1599)/4 #30
(1510-1449)/2.5 #24.4
(1450-1553)/3.5 #29.4
(28.4+26.4+28.6+29.8+29.3+28.1+27+26.4+28+30+24.4+29.4)/12
# Approximately 28 elo diff per point in point spread

# Examining typical point spreads
schedule_2016_df <- read.csv("schedule_2016.csv", header = TRUE)
team_elo_2016_df <- read.csv("team_elo_2016.csv", header = TRUE)
schedule_2016_df$Away <- as.character(schedule_2016_df$Away)
team_elo_2016_df$Away <- as.character(team_elo_2016_df$Away)
for (i in 1:30) {
  subset <- filter(schedule_2016_df, Away == team_elo_2016_df[i,1])
  subset$AwayElo <- team_elo_2016_df[i,2]
  schedule_2016_df[(schedule_2016_df$Away==team_elo_2016_df[i,1]),] <- subset
  subset <- filter(schedule_2016_df, Home == team_elo_2016_df[i,1])
  subset$HomeElo <- team_elo_2016_df[i,2]+100
  schedule_2016_df[(schedule_2016_df$Home==team_elo_2016_df[i,1]),] <- subset
}
schedule_2016_df <- mutate(schedule_2016_df, AwayEloDiff = AwayElo-HomeElo, HomeEloDiff = HomeElo-AwayElo)
qplot(schedule_2016_df$AwayDiff)

# Result min version of simulateseason function
# result <- runif(1,0,1)
# # Home team favored
# if (expected_home >= expected_away) {
#   # Home win
#   if (result >= expected_home_range[1] & result <= expected_home_range[2]) {
#     #result_min <- 3.031433/(7.5+(.006*((home_elo+100)-away_elo)))
#     if (result >= result_min) {
#       away_team$CurrentLosses <- away_team$CurrentLosses + 1
#       home_team$CurrentWins <- home_team$CurrentWins + 1
#       away_team$CurrentElo <- round(away_elo+(k*((1-result)-expected_away)), digits = 1)
#       home_team$CurrentElo <- round(home_elo+(k*(result-expected_home)), digits = 1)
#     }
#     else if (result < result_min) {
#       away_team$CurrentLosses <- away_team$CurrentLosses + 1
#       home_team$CurrentWins <- home_team$CurrentWins + 1
#       away_team$CurrentElo <- round(away_elo+(k*((1-result_min)-expected_away)), digits = 1)
#       home_team$CurrentElo <- round(home_elo+(k*(result_min-expected_home)), digits = 1)
#     }
#   }
#   # Away win
#   else if (result >= expected_away_range[1] & result <= expected_away_range[2]) {
#     # No need for result_min calc since home is favored
#     away_team$CurrentWins <- away_team$CurrentWins + 1
#     home_team$CurrentLosses <- home_team$CurrentLosses + 1
#     away_team$CurrentElo <- round(away_elo+(k*((1-result)-expected_away)), digits = 1)
#     home_team$CurrentElo <- round(home_elo+(k*(result-expected_home)), digits = 1)
#   }
# }
# # Away team favored
# else if (expected_home < expected_away) {
#   # Home win
#   if (result >= expected_home_range[1] & result <= expected_home_range[2]) {
#     away_team$CurrentLosses <- away_team$CurrentLosses + 1
#     home_team$CurrentWins <- home_team$CurrentWins + 1
#     away_team$CurrentElo <- round(away_elo+(k*(result-expected_away)), digits = 1)
#     home_team$CurrentElo <- round(home_elo+(k*((1-result)-expected_home)), digits = 1)
#   }
#   # Away win
#   else if (result >= expected_away_range[1] & result <= expected_away_range[2]) {
#     #result_min <- 3.031433/(7.5+(.006*(away_elo-(home_elo+100))))
#     if (result >= result_min) {
#       away_team$CurrentWins <- away_team$CurrentWins + 1
#       home_team$CurrentLosses <- home_team$CurrentLosses + 1
#       away_team$CurrentElo <- round(away_elo+(k*(result-expected_away)), digits = 1)
#       home_team$CurrentElo <- round(home_elo+(k*((1-result)-expected_home)), digits = 1)
#     }
#     else if (result < result_min) {
#       away_team$CurrentWins <- away_team$CurrentWins + 1
#       home_team$CurrentLosses <- home_team$CurrentLosses + 1
#       away_team$CurrentElo <- round(away_elo+(k*(result_min-expected_away)), digits = 1)
#       home_team$CurrentElo <- round(home_elo+(k*((1-result_min)-expected_home)), digits = 1)
#     }
