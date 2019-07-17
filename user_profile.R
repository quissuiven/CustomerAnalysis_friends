


library(readxl)
data <- read_excel("D:/users_number_of_friends_tableau.xlsx")
View(data)


#workflow: 1. extract unique ids  2. lookup unique ids and sum all friends   3. for each number of friends, find #occurrence, plot histo


#finding unique ids + total number of friends for each
unique_id <- c(unique(data$user_id))                        
unique_id


TotalFriends <- c()
TotalFriendsCount <- 0
loopcount <- 0

for (id in unique_id){
  for (j in data$user_id){
      loopcount = loopcount + 1
      if(id==j){
           TotalFriendsCount = TotalFriendsCount + data$friends_made[loopcount]
      }
  }
  TotalFriends <- append(TotalFriends,TotalFriendsCount)
  TotalFriendsCount <- 0
  loopcount <- 0
}

TotalFriends_df <- data.frame(unique_id,TotalFriends)

write.csv(TotalFriends_df, "D:/totalfriends.csv")


#For each total friend count, find number of users who have that number of friends

unique_TotalFriends <- c(unique(TotalFriends))

Count_unique_TotalFriends <- c()
a <- 0
for (i in unique_TotalFriends){
  a <- sum(TotalFriends_df$TotalFriends == i, na.rm=TRUE)
  Count_unique_TotalFriends <- append(Count_unique_TotalFriends,a)
  a <- 0
}

UniqueTotalFriends_df <- data.frame(unique_TotalFriends,Count_unique_TotalFriends)

write.csv(UniqueTotalFriends_df, "D:/uniquetotalfriends.csv")

#Extract unique user ids with only 2 total friends

userid_twofriends <- subset(TotalFriends_df, TotalFriends_df$TotalFriends==2) 

write.csv(userid_twofriends, "D:/twofriends.csv")

data2 <- read_excel("D:/user_profile_all_R.xlsx")
#useridvector2friends <- subset(userid_twofriends,select=c(unique_id))
friends2vector <- userid_twofriends$unique_id
datawithout2friends <- subset(data2,!user_id%in%friends2vector)

write.csv(datawithout2friends, "D:/Datawithout2friends.csv")

#define age, using birthdays
datawithout2friends$age <- 2018 - as.numeric(substr(datawithout2friends$birthday,1,4))
 
#Find number of users belonging to certain age                              #table in R automatically calculates frequency
agecounting <- data.frame(matrix(,nrow=345521),ncol=0)
agecounting$ncol <- datawithout2friends$age
#agecounting$freq <- as.data.frame(table(unlist(agecounting$ncol)))
#final1 <- as.data.frame(table(unlist(agecounting$ncol)))

#Find number of users belonging to certain gender
trial <- as.data.frame(datawithout2friends$gender)
trialgender <- as.data.frame(table(unlist(trial)))
trialgender

colnames(trial) = c("name")
library(plyr)
table(trial)                    
typeof(trial$name)                  
nrow(trial)

table(trial$name == 0)
table(trial$name == 1)
table(trial$name == 2)

#Find number of users belonging to certain registration month
datawithout2friends$registerdate <- format(as.Date(datawithout2friends$created_at), "%b %Y")
table(trial$name == 3)

date1 <- as.data.frame(table(unlist(datawithout2friends$registerdate)))             #unlist returns list in column to row

