library(rvest)
library(stringr)
library(stringi)
library(plyr)
library(dplyr)
library(ggplot2)

library(lubridate)
library(networkD3)
library(tidyr)
require(devtools)
install_github('rCharts', 'ramnathv')
library(rCharts)

url <- "https://en.wikipedia.org/wiki/List_of_heavyweight_boxing_champions"


# grab the list of urls to boxers' pages, note that the CSS tag was picked up via the magical SelectorGadget
master.page_parsed <- html(url)
master.links <- master.page_parsed %>% html_nodes("td+ td > a") %>% html_attr("href") %>% XML::getRelativeURL(url)

#now if we're looking at a specific boxer, once the data are obtained

# 1. store the url of the boxer's page, then pass it to rvest for parsing
table_someboxer <- html(link) %>% html_node("td") %>% html_table()

# 2. couldn't figure out to specify the table I need, as there are multiple ones for several boxers; to mediate, grabbed all tables, then picked the longest one
n.rows <- unlist(lapply(table_someboxer, function(t) dim(t)[1]))
table_someboxer <- table_someboxer[which.max(n.rows)] %>% data.frame()

#3. then turn that list member into a dataframe, as done above
#4. finally, remove the top two rows
table_someboxer <- table_someboxer[-2,]

#5. add a column that would identify the boxer
boxer.name <- str_sub(names(links), 7)
boxer.name <- gsub("_", " ", boxer.name)

#Following this logic,

#to get the master dataset

table.list <- html(master.links[1]) %>% html_table(header=TRUE, fill=TRUE)
n.rows <- unlist(lapply(table.list, function(t) dim(t)[1]))
table_interm <- table.list[which.max(n.rows)] %>% data.frame(row.names = NULL)
master.df <- table_interm[-1,]
boxer.nameunder <- str_sub(names(master.links[1]), 7)
boxer.name <- gsub("_", " ", boxer.nameunder)
colnames(master.df) <- c("Res.", "Record", "Opponent", "Type", "Round", "Date", "Location", "Notes")
master.df$Fighter <- boxer.name

#then loop through the links and grab the tables that can be grabbed

for (link in master.links[2:length(master.links)]) {
  table.list <- html(link) %>% html_table(header=TRUE, fill=TRUE)
  n.rows <- unlist(lapply(table.list, function(t) dim(t)[1]))
  table_interm <- table.list[which.max(n.rows)] %>% data.frame(row.names = NULL)
  table_boxer <- table_interm[-1,]
  if (ncol(table_boxer) == 8) {
    round <- as.integer(trimws(str_sub(table_boxer[1,5], start=1, end=2)))
    if (is.na(round)==FALSE) {
      boxer.nameunder <- str_sub(toString(link), 31)
      boxer.name <- gsub("_", " ", boxer.nameunder)
      colnames(table_boxer) <- c("Res.", "Record", "Opponent", "Type", "Round", "Date", "Location", "Notes")
      table_boxer$Fighter <- boxer.name
      table_boxer <- data.frame(table_boxer)
      master.df <- rbind.fill(master.df, table_boxer)
    }
    
    if (is.na(round)==TRUE) {
      trouble.list <- c(trouble.list, link)
    }}
  
  else {
    trouble.list <- c(trouble.list, link)
  }
}

#Trouble.list should capture all the tables for which manual cleanup is required :(
#Given the trouble list, some manual work is required. Given that there are only seven boxers for which data was not fetched, it's not that complicated of a task

#Now to the fun part - assigning IDs to boxers. First, 
#remove the NAs
master.df %>% group_by(Res.) %>% filter(!(is.na(Res.))) %>% ungroup()

#Next step would be to create some sort of a reference or an index list - individuals on this list will be 1) unique 2) assigned a random integer ID number
index.list <- unique(c(master.df$Opponent, master.df$Fighter))

#Now assign an ID to each fighter
index.ids <- as.numeric(as.factor(index.list))

#Combine the IDs with the fighter names - that will give us an index.list
index.list <- as.data.frame(cbind(index.list, index.ids))

#Finally, match the IDs with fighter names using left_join from dplyr

colnames(index.list) <- c("Fighter", "Fighter.id")
master.copy <- left_join(master.df, index.list, "Fighter")

#Repeat for the other column of fighters

colnames(index.list) <- c("Opponent", "Opponent.id")
master.copy <- left_join(master.copy, index.list, "Opponent")

#Extract the year (out of curiousity tried the "stringi" package) and construct decade
master.copy$Year <- stri_extract_last_regex(master.copy$Date, '\\d{4}')
master.copy$Decade <- paste(str_sub(master.copy$Year, start = 1, end = 3), '0', sep = '')

#Clean up the number of rounds column

master.copy$Round <- trimws(str_sub(master.copy$Round, start = 1, end = 2))

#Now try to mark the fights that involved a title contention, regardless of whether the boxer in question won or lost

for (i in 1:nrow(master.copy)) {
  #res.check <- str_detect(master.copy$Res.[i], pattern = 'Win')
  title.check <- str_detect(master.copy$Notes[i], pattern = '[a-zA-Z]+itle')
  title.eliminator <- str_detect(master.copy$Notes[i], pattern = '[a-zA-Z]+liminator')
  title.heavyweight <- str_detect(master.copy$Notes[i], pattern = '[a-zA-Z]+eavyweight')
  
  if (is.na(title.check)==FALSE) {                            
    if ((title.check == TRUE) && (title.eliminator == FALSE) && (title.heavyweight == TRUE)) {
      master.copy$Title[i] = 1
    }
  }
}


#Some of those title contentions, naturally, had to be matches where one boxer in the pair obtained his recognized heavyweight belt, thus becoming a Heavyweight Boxing Champion of the World.
#Picking these out is a bit tricky: for one, where money is also lives politics: high profile bouts attract sponsors, TV cameras, and audiences, and many "organizations" would like to award such high profile titles but carry no competitive weight behind them whatsoever. To simply things, we'll mark only fights sanctioned by the IBF, WBA, WBC, and WBO as world heavyweight contentions.
#Before 1960s, however, these federations or organizations didn't exist as such. Heavyweight title was awarded by a number of other organizations of varying obscurity and was simply called "World Heavyweight" or "European Heavyweight" title. We'll include these filters also.
#In other words, this loop will indicate every instance when any given fighter gained or regained (retained) a heavyweight title.

for (i in 1:nrow(master.copy)) {
  world.title.check <- str_detect(master.copy$Notes[i], pattern = c("IBF", "WHO", "WBA", "WBC", "[a-zA-Z]+orld [a-zA-Z]+eavyweight", "EBU"))

  if (is.na(world.title.check) == FALSE) {
    if ((master.copy$Res.[i] == 'Win') && any(world.title.check == TRUE))  {
    master.copy$Title.hwc[i] = 1
    }
  }
}

#This will only mark the rows that represent retention fights (a situation in which a boxer possesses a title and successfully defends it)
  
for (i in 1:nrow(master.copy)) {
    world.title.check <- str_detect(master.copy$Notes[i], pattern = c("IBF", "WHO", "WBA", "WBC", "[a-zA-Z]+orld [a-zA-Z]+eavyweight", "EBU"))
    title.retained <- str_detect(master.copy$Notes[i], pattern = "[a-zA-Z]+etained")
  if (is.na(world.title.check) == FALSE && is.na(title.retained) == FALSE) {
    if (master.copy$Res.[i] == 'Win' && title.retained == TRUE && any(world.title.check == TRUE))  {
      master.copy$Title.defense[i] = 1
    }
  }
}
  
  
if (master.copy$Title[i] == 1 & master.copy$Title.hwc[i] == 0) {
master.copy$Title.defense = 1
}


#Now fix the dates, as they vary in format (those darn European editors of Wikipedia :))
date.formats <- c('%d/%m/%Y', '%Y-%m-%d', '%B %d, %Y')
master.copy$Date.formatted <- parse_date_time(master.copy$Date, orders = date.formats)


#Count the number of bouts and rounds each champion has fought; we'll use dplyr for this
rounds.bouts <- master.copy %>% group_by(Fighter) %>% summarize(Rounds = sum(as.integer(Round), na.rm = TRUE), Bouts = n())

#Figure out title stats
title.fights <- master.copy %>% group_by(Fighter) %>% filter(Title == 1) %>% summarize('Title Fights' = n())
title.fights.won <- master.copy %>% group_by(Fighter) %>% filter(Title == 1 & Res. == 'Win') %>% summarize('Title fights won' = n())
title.ratio <- round(title.fights.won[,2]/title.fights[,2], 2)

#Compile the wins/losses stats, along with wins by knock outs
wins <- master.copy %>% group_by(Fighter) %>% filter(Res. == 'Win') %>% summarize(Wins = n())
kos <- master.copy %>% group_by(Fighter) %>% filter(Type == 'KO') %>% summarize(KOs = n())
wins.bouts <- (wins$Wins)/(rounds.bouts$Bouts)

champion.stats <- cbind(rounds.bouts, wins$Wins, round(wins.bouts, 2), title.fights$`Title Fights`, kos$KOs, round(ARB, 2))
colnames(champion.stats) <- c('Fighter', 'Rounds', 'Bouts', 'Wins', 'Wins to Bouts ratio','Title fights', 'KOs', 'ARB')


#Create a matrix of rounds/KOs for all fighters, effectively a crosstab
#First sort, according to the first title fight a champion parttook in
boxer.index <- master.copy[master.copy$Title == 1,]
boxer.index <- boxer.index[order(boxer.index$Fighter),]
boxer.index <- boxer.index[order(boxer.index$Date.formatted, decreasing = FALSE),]
boxer.index <- boxer.index[!duplicated(boxer.index$Fighter),]

#Create the rounds matrix needed to compute the heatmap
rounds.matrix <- as.data.frame.matrix(table(master.copy$Fighter, master.copy$Round), stringsAsFactors = TRUE)

#Now reorder it based on the order created in boxer.index
rounds.matrix <- rounds.matrix[rownames(boxer.index),, drop = FALSE]

rounds.matrix <- rounds.matrix[,1:15]

#Create the heatmap
rounds.label <- c("Round 1", "Round 2", "Round 3", "Round 4", "Round 5", "Round 6", "Round 7", "Round 8", "Round 9", "Round 10", "Round 11", "Round 12", "Round 13", "Round 14", "Round 15")
heatmap <- d3heatmap(rounds.matrix, scale = "row", colors = "Blues", dendrogram = "none", Rowv = FALSE, Colv = FALSE, xaxis_height = 100, yaxis_width = 300)

#Compute the average length of a title fight, over time
master.copy$Year <- format(master.copy$Date.formatted, '%Y')
rounds.mean.hw <- master.copy %>% filter(Title.hwc == 1) %>% group_by(Year) %>% summarize(Average = mean(Round), Max = max(Round), Min = min(Round))

count.pretitle <- ''
fighter.index <- unique(master.copy$Fighter)
for (fighter in fighter.index) {
  temp <- master.copy %>% filter(Fighter == fighter)
  diffs <- temp$Title[-1L] != temp$Title[-length(temp$Title)]
  idx <- c(which(diffs), length(temp$Title))
  count.pretitle <- rbind(count.pretitle, data.frame(fighter, idx[1]))
  }

#we'll need to get rid of the NA in the first row
count.pretitle <- count.pretitle[-1,]

#and now we'll order the results of this loop according to the same logic we used earlier, by the date of the first belt challenge. I realize this isn't a very conventional method for achieving the sorting I'm looking for, but I couldn't get the rows to take on the order I wanted. Instead, I simply joined the earlier fighter list with correct ordering to my data from the loop above.

colnames(count.pretitle) <- c('Fighter', 'Bouts')
count.pretitle <- left_join(boxer.index, count.pretitle, by = 'Fighter')

#first, transform the Fighter column into factors; this will preserve my desired order
count.pretitle <- transform(count.pretitle, Fighter = factor(Fighter, levels=unique(Fighter)))
count.pretitle$Bouts <- as.integer(count.pretitle$Bouts)

#now apply a bit of ggploting to create the graph
bouts.graph <- ggplot(count.pretitle) + geom_bar(aes(x=Fighter, y=Bouts), fill='gray', stat = 'identity') + coord_flip() + theme(axis.text.y = element_text(size=rel(.8))) + theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(linetype=3, color='darkgray')) + stat_smooth(aes(x=Fighter, y=Bouts, group = 1), se = FALSE) + ylab("Bouts before first belt challenge")

#MISSSING FRANK BRUNO
# troublesome boxers - Ali (5), Terrell (6 - time issue), Frasier (7, same time problem), Foreman (9), Spinks (10 - one column is missing, rows 484-530), Coetzee (16), Spinks (21), Tyson (24), 
# Douglas (28), Holyfield (29), Bowe (32), Lewis (33), Bentt (35), Hide (36)