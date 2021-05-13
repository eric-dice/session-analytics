#House Floor Roll Call Vote
#Notes As of 4-26

library(tidyverse)
library(rvest)
library(stringr)
library(stringi)

launchpage<- read_html("https://ldpc6.legis.state.pa.us/cfdocs/legis/RC/Public/rc_view_date.cfm?rc_body=H&SPick=20190&CalendarDisplay=List") # 2019


session.day.links <- as.list(paste0("https://ldpc6.legis.state.pa.us",launchpage %>% html_nodes(".CalendarDisplay-List-Days-Day") %>% html_nodes("a") %>% html_attr("href")))

all.votes <- data.frame()


for (a in 1:length(session.day.links)){
  print(paste0("Processing Session Day ",a, " of ",length(session.day.links) ))
  rcs.links <- as.list(paste0("https://ldpc6.legis.state.pa.us/cfdocs/legis/RC/Public/",read_html(session.day.links[[a]]) %>% html_nodes(".faketable-cell") %>% html_nodes("a") %>% html_attr("href")))
  days.votes <- data.frame()
  
   for (b in 1:length(rcs.links)){
     
    print(paste0("Scraping Vote # ",b, " of ", length(rcs.links)))
    rcs.page <- read_html(rcs.links[[b]])
    
    # Data to Gather
    rcs.number <- rcs.page %>% html_nodes(".caption-subject") %>% html_text()
    motion <- rcs.page %>% html_nodes(xpath= '/html/body/div[5]/div/div/div/div/div[2]/div/div[1]/div[2]') %>% html_text()
    motion <- gsub("[\r\n\t]", "", motion)
    motion <-trimws(motion)
    yea.votes <- rcs.page %>% html_nodes("#RCVotesSum div:nth-child(2)") %>% html_text()
    nay.votes <- rcs.page %>% html_nodes("#RCVotesSum div:nth-child(5)") %>% html_text()
    lve.votes <- rcs.page %>% html_nodes("#RCVotesSum div:nth-child(8)") %>% html_text()
    nv.votes  <- rcs.page %>% html_nodes("#RCVotesSum div:nth-child(11)") %>% html_text()
    bill.number <- rcs.page %>% html_nodes(xpath= '/html/body/div[5]/div/div/div/div/div[2]/div/div[1]/div[1]/div[3]/a') %>% html_text()
    #uncontested calendar error checking
    #if(length(bill.number)>1) {bill.number <- paste("Uncontested Calendar -", vote.date)}
    
    
    vote.date <- rcs.page %>% html_nodes(".portlet-body div:nth-child(1) > a") %>% html_text()
    short.title <- rcs.page %>% html_nodes(xpath= '/html/body/div[5]/div/div/div/div/div[2]/div/div[1]/div[5]/div[2]/div[2]') %>% html_text()
    
    vote = data.frame()
    vote.data = data.frame()
    
    #loop through 4 columns of Votes
        for (j in seq(from = 1, to = 4, by = 1) ) {
          for (k in seq(from = 1, to = 51, by = 1)) {
            
            vote = as.vector(rcs.page %>% html_node( xpath = paste0("/html/body/div[5]/div/div/div/div/div[2]/div/div[2]/div/div[",j,"]/div[",k,"]")) %>% html_text() ) 
            
            vote.data <-rbind(vote.data,vote)
          }
        }
    colnames(vote.data) <- c("vote.data")
    
    
    #Error Checks
    if(length(bill.number)>1) {print("Uncontested")
      bill.number <- paste("Uncontested Calendar -", vote.date)}
    if (is_empty(bill.number)) {bill.number <- rcs.page %>% html_nodes( xpath= '/html/body/div[5]/div/div/div/div/div[2]/div/div[1]/div[1]/div[3]') %>% html_text()}
    if (is_empty(short.title)) {short.title <- NA}
    #push data to frame and bind to master list
    vote.data$bill <- bill.number
    vote.data$motion <- motion
    vote.data$rcs.number <- rcs.number
    vote.data$vote.date <- vote.date
    vote.data$short.title <- short.title
    
    days.votes <- rbind(days.votes,vote.data)
    
    }
all.votes <- rbind(all.votes, days.votes)

}




#Clean-up
all.votes$vote.data <- gsub("[\r\n\t]", "", all.votes$vote.data)
all.votes$motion <- gsub("[\r\n\t]", "", all.votes$motion)
all.votes$motion <- str_trim(all.votes$motion, side = c("both"))
all.votes$motion <- str_squish(all.votes$motion)

all.votes$bill <- gsub("[\r\n\t]", "", all.votes$bill)
all.votes$bill <- str_trim(all.votes$bill, side = c("both"))
all.votes$bill <- str_squish(all.votes$bill)


all.votes$vote.data <- gsub("\u00A0", " ", all.votes$vote.data, fixed = TRUE) #find and replace non-breaking spaces with spaces
all.votes$vote.data <- trimws(all.votes$vote.data) #trim leading whitespace
all.votes$vote.data[all.votes$vote.data==""] <- NA #convert any remaining pesky values that are just spaces to NA
all.votes <- all.votes %>% filter(!is.na(vote.data)) # remove rows where the vote.data is NA (the blanks in the vote layout)

#Split the vote into member and the vote
all.votes$vote <- substr(all.votes$vote.data,1,1)
all.votes$member <- str_sub(all.votes$vote.data,2,stri_length(all.votes$vote.data))

table(all.votes$vote)
all.votes %>% distinct(bill)



#all.votes %>% filter(., grepl("MASTER",bill)) 


