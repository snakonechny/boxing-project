library(rvest)
library(stringr)
library(plyr)
library(data.table)
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

#trouble.list should capture all the tables for which manual cleanup is required :(


#remove the NAs
master.df %>% group_by(Res.) %>% filter(!(is.na(Res.))) %>% ungroup()


#MISSSING FRANK BRUNO