#Commmittee Roll Call Scraping
library(tidyverse)
library(rvest)
library(stringr)
library(stringi)
library(tictoc)
library(gt)


setwd("C:/Users/edice/OneDrive - PA Legislative Data Processing Center/R")

tic(msg = "Time used to scrape all committee votes: ", quiet = FALSE)


allvotes.day = data.frame()
allvotes.committee = data.frame()
allvotes = data.frame()

launchpage <- read_html("https://ldpc6.legis.state.pa.us/cfdocs/legis/RCC/PUBLIC/index.cfm?chamber=H")
#year2 test
#
launchpage <- read_html("https://ldpc6.legis.state.pa.us/cfdocs/legis/RCC/PUBLIC/index.cfm?Chamber=H&sYear=2019&sInd=0")

#Create a list of links to the committee roll calls (where they then have a list of dates)
committee.links <- as.list(paste0("https://ldpc6.legis.state.pa.us/cfdocs/legis/RCC/PUBLIC/", launchpage %>% html_nodes("[class = 'col-md-12 col-sm-12 col-xs-12 faketable-cell']") %>% html_node("a") %>% html_attr("href")))


print(paste("Scraping..."))
# too long to perform this task in one go, so cutting in half.
for (k in 14:length(committee.links)) {
  tic(msg = "Time Spent Processing This Committee: ")
  committee.dates <- as.list(paste0("https://ldpc6.legis.state.pa.us/cfdocs/legis/RCC/PUBLIC/",read_html(committee.links[[k]]) %>% html_nodes('.CalendarDisplay-List-Days-Day') %>% html_node("a") %>% html_attr("href") ))
  
  
  for (j in 1:length(committee.dates)) { 
    tic(msg = "Time to process this date: ")
    
    print(read_html(committee.dates[[j]]) %>% html_node("h2") %>% html_text())
    print(paste("Scraping Committee Meeting #", j, "of", length(committee.dates), "for your selected Session. This committee is #", k ,"of", length(committee.links), "in the queue."))
    
    committee.rcc.list <- as.list(paste0("https://ldpc6.legis.state.pa.us/cfdocs/legis/RCC/PUBLIC/", read_html(committee.dates[[j]]) %>% html_nodes('.faketable-cell a') %>% html_attr("href")))
    
    
    for (i in 1:length(committee.rcc.list)) {
      tic(msg = "time to process this vote:")
      rcc.page <- read_html(committee.rcc.list[[i]])
      committee.rcc.list[[i]]
      #RCS Data Elements
      #Committee
      #date
      #bill
      #print number
      #Committee Roll call number - extract from link
      #Type of Motion
      #Brief Description
      #Amendment Sponsor - extract from amendment text
      
      committee <- rcc.page %>% html_nodes(".col-md-12 .col-sm-12 a") %>% html_text
      date <- rcc.page %>% html_nodes(".col-xs-10 a") %>% html_text()
      bill <- rcc.page %>% html_nodes(".col-xs-4 a:nth-child(1)") %>% html_text()
      
      
      #need error checking loop here
      amendment.number <- rcc.page %>% html_nodes(".col-xs-4 a+ a") %>% html_text()
      amendment.sponsor <- read_html(paste0("https://ldpc6.legis.state.pa.us/",rcc.page %>% html_nodes(".col-xs-4 a+ a") %>% html_attr("href"))) %>% html_node(".y2") %>%html_text()
      rcc.number <- sapply(strsplit(committee.rcc.list[[i]], split = "\\="),tail,1L)
      motion.type <- rcc.page %>% html_nodes(".Rcc-Detail:nth-child(3) .col-xs-4~ .col-xs-4") %>% html_text()
      brief.description <- rcc.page %>% html_nodes(".Rcc-Detail+ .Rcc-Detail .col-xs-10") %>% html_text()
      result <- rcc.page %>% html_nodes(".col-xs-2+ .col-xs-2") %>% html_text()
      yea.votes <- rcc.page %>% html_nodes(".Rcc-Vote-Summary:nth-child(8) .col-xs-10") %>% html_text()
      nay.votes <- rcc.page %>% html_nodes(".Rcc-Vote-Summary:nth-child(9) .col-xs-10") %>% html_text()
      vote.data <- rcc.page %>% html_nodes(".col-sm-6") %>% html_text()
      vote.data <- gsub("[\r\n\t]", "", vote.data)
      vote.data <- trimws(vote.data)
      vote.data <-as.data.frame(vote.data)
      colnames(vote.data) <- "data" #rename the column in this dataframe so it can properly be referenced in the next commands to cut out the unwanted header from the table
      vote.data <- vote.data %>% filter(!grepl("MAJORITY MEMBERS", vote.data$data)) # search for and save only those entries which do not match either MAJORITY or MINORITY (the headers)
      vote.data <- vote.data %>% filter(!grepl("MINORITY MEMBERS", vote.data$data))
      
      vote.data <-do.call("cbind", split(vote.data, rep(c(1, 2), length.out = nrow(vote.data)))) #pull the even rows with the votes and match them to the odd row with the member
      vote.data <-as.data.frame(vote.data)
      colnames(vote.data) <- c("member", "vote")
      
      #Start to assemble all of the committee roll call vote information for this vote into a single data.frame
      #initialize a dataframe
      rcc.vote.data <- setNames(data.frame(matrix(ncol =13 , nrow = 0)), c("committee","date", "bill", "amendment.number", "amendment.sponsor", "rcc.number", "motion.type", "brief.description", "yea.votes","nay.votes", "result", "member", "vote"))
      
      #insert the data
      rcc.vote.data <- rbind(rcc.vote.data,vote.data)
      rcc.vote.data <- cbind(rcc.vote.data, bill)
      if (is_empty(amendment.number)) {amendment.number <- NA}
      rcc.vote.data <- cbind(rcc.vote.data, amendment.number)
      rcc.vote.data <- cbind(rcc.vote.data, amendment.sponsor)
      rcc.vote.data <- cbind(rcc.vote.data, rcc.number)
      rcc.vote.data <- cbind(rcc.vote.data, motion.type)
      rcc.vote.data <- cbind(rcc.vote.data, brief.description)
      rcc.vote.data <- cbind(rcc.vote.data,yea.votes)
      rcc.vote.data <- cbind(rcc.vote.data,nay.votes)
      rcc.vote.data <- cbind(rcc.vote.data, result)
      rcc.vote.data <- cbind(rcc.vote.data, date)
      rcc.vote.data <- cbind(rcc.vote.data,committee)
      

      
      
      #should now have 12 variables and probably 25 members per committee
      allvotes.day <- rbind(allvotes.day, rcc.vote.data)
      gc()
      toc()
    }
    allvotes.committee <- rbind(allvotes.committee, allvotes.day)
    gc()
    toc()
  }
  allvotes <- rbind(allvotes, allvotes.committee)
  print("**************************************")
  print("*        Committee complete          *")
  print("**************************************")
  toc()
  gc()
}
toc()
save(allvotes, file = "allvotes2019 - 2657.rda")









allvotes %>%
  group_by(motion.type) %>% 
  summarize(vote.count = n_distinct(rcc.number))

allvotes %>%
  summarize(vote.count = n_distinct(rcc.number))



amendments <- allvotes %>%
  group_by(committee) %>%  
    filter(motion.type == "Adopt Amendment") %>%
  summarize(vote.count = n_distinct(rcc.number)) 

#Table with amendments considered by committee
gt(amendments, rowname_col = 'committee' )%>%
  summary_rows(columns ='vote.count', fns = list('Total Amendments'='sum'), decimals = 0 ) %>%
  tab_header(
    title = "Roll Call Votes to Adopt Amendment in House Committees",
    subtitle = "2019/20 Legislative Session"
  ) %>%
  tab_stubhead(label = 'Committee') %>%
  cols_label(vote.count = "Number of Amendments Considered")

amendments2 <- allvotes %>%
  group_by(committee,result) %>%  
  filter(motion.type == "Adopt Amendment") %>%
  summarize(vote.count = n_distinct(rcc.number))


amendments3 <- amendments2 %>% spread(result,vote.count, fill=0)


  tbl_summary(amendments3) %>% add_n()





# #Cleaning before pushing to aggregation dataframe.
# allvotes$member <- gsub("[\r\n\t]", "", allvotes$member)
# allvotes$member <- trimws(allvotes$member)
# 
# allvotes$vote <- gsub("[\r\n\t]", "", allvotes$vote)
# allvotes$vote <- trimws(allvotes$vote)
# 
# allvotes$bill <- gsub("[\r\n\t]", "", allvotes$bill)
# allvotes$bill <- trimws(allvotes$bill)
# 
# allvotes$amendment.number <- gsub("[\r\n\t]", "", allvotes$amendment.number)
# allvotes$amendment.number <- trimws(allvotes$amendment.number)
# 
# allvotes$amendment.sponsor <- gsub("[\r\n\t]", "", allvotes$amendment.sponsor)
# allvotes$amendment.sponsor <- trimws(allvotes$amendment.sponsor)
# 
# allvotes$motion.type <- gsub("[\r\n\t]", "", allvotes$motion.type)
# allvotes$motion.type <- trimws(allvotes$motion.type)
# 
# allvotes$brief.description <- gsub("[\r\n\t]", "", allvotes$brief.description)
# allvotes$brief.description <- trimws(allvotes$brief.description)
# 
# allvotes$date <- gsub("[\r\n\t]", "", allvotes$date)
# allvotes$date <- trimws(allvotes$date)
# 
# allvotes$result <- gsub("[\r\n\t]", "", allvotes$result)
# allvotes$result <- trimws(allvotes$result)
# 
# allvotes$vote <- gsub("[\r\n\t]", "", allvotes$vote)
# allvotes$vote <- trimws(allvotes$vote)
# 
# allvotes$committee <- gsub("[\r\n\t]", "", allvotes$committee)
# allvotes$committee <- trimws(allvotes$committee)

# d <- subset(allvotes, select = -c(member, vote))      
# d <- d[!duplicated(d), ]
# d %>% group_by(motion.type) %>% summarize(vote.count = n_distinct((rcc.number)))
