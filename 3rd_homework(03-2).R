setwd("C:/education/R/workspace")
customer <- read.csv("./data/loarn/customer.csv",
                     sep="|",header = TRUE,
                     stringsAsFactors=FALSE)
View(customer)                     

library(lubridate)

end <- parse_date_time(2015, "%Y")
end

start <- parse_date_time(customer$AGE, "%Y")
start

diff_in_yr=(end-start)/365
diff_in_yr

customer$AGE <- floor(diff_in_yr)
View(customer)

findInterval(25, c(10,29,39,49,59,90))
findInterval(45,c(10,29,39,49,59,90))
customer$AGE <- findInterval(customer$AGE,c(10,29,39,49,59,90))
View(customer)

customer$AGE <- factor(customer$AGE, labels=c("20s&below",
                                              "30s","40s","50s",
                                              "60s&up"))
View(customer)

loarn <- read.csv("./data/loarn/loarn.csv",
                  sep="|",header = TRUE, stringsAsFactors = FALSE)
View(loarn)

start <- parse_date_time("199312","%Y%m")
start_ln <- parse_date_time(loarn$LN_FROM,"%Y%m")
start_ln

end_ln <- parse_date_time(loarn$YM,"%Y%m")
end_ln

diff_in_month=as.double(difftime(end_ln,start_ln,
                                 units="days")*12/365)
loarn$DUR_LN <- floor(diff_in_month)
View(loarn)

library(dplyr)

loarn_1512 <- loarn %>% dplyr::filter(YM == 201512)
View(loarn_1512)

overdue <- read.csv("./data/loarn/overdue.csv",
                    sep="|", header = TRUE)
View(overdue)
start_dlq <- parse_date_time(overdue$DLQ_FROM,"%Y%m")
end_dlq <- parse_date_time(as.character(overdue$YM),"%Y%m")
diff_in_month=as.double(difftime(end_dlq,start_dlq,units = "days")*12/365)
overdue$DUR_DLQ <- floor(diff_in_month)
View(overdue)

overdue_1512 <- overdue %>% dplyr::filter(YM==201512)
View(overdue_1512)

left_join(loarn_1512,customer,by=c("CustomerId"))
df<-left_join(loarn_1512,customer,by=c("CustomerId"))
View(df)

JOIN_KEY <- df %>%  group_by(CustomerId) %>% 
  summarise(NUM_LN=n())

x <- right_join(customer,JOIN_KEY,by="CustomerId")
View(x)

LN_AMT <- df %>% 
  group_by(CustomerId) %>% 
  summarise(AMT_LN=sum(LN_AMT))
x <- right_join(x, LN_AMT, by="CustomerId")
View(x)

NUM_NBLN <-
  df %>% dplyr::filter(SCTR_CD!=1) %>% 
  group_by(CustomerId) %>% 
  dplyr::summarise(NUM_NBLN=n())

x<-right_join(x, NUM_NBLN, by="CustomerId")
View(x)

NBLN_AMT <- df %>% 
  dplyr::filter(SCTR_CD != 1) %>% 
  group_by(CustomerId) %>% 
  dplyr::summarise(NBLN_AMT=sum(LN_AMT))

x<-right_join(x,NBLN_AMT,by="CustomerId")
View(x)

library(plyr)
df1 <- join(loarn_1512, overdue_1512,by=c("CustomerId", "COM_KEY"))
View(df1)

df1$DLQ_1512 <- ifelse(is.na(df1$DLQ_CD_1),0,1)
View(df1)

NUM_DLQ_1512 <- overdue_1512 %>% 
  group_by(CustomerId) %>% 
  dplyr::summarise(NUM_DLQ_1512=n())

y <- left_join(x, NUM_DLQ_1512, by="CustomerId")

y$NUM_DLQ_1512[is.na(y$NUM_DLQ_1512)]<-0
View(y)

overdue_2016 <- 
  overdue %>% dplyr::filter(YM>201512& YM<=201612)

NUM_DLQ_2016 <- overdue_2016 %>% 
  group_by(CustomerId) %>% 
  dplyr::summarise(NUM_DLQ_2016=n())

y <- left_join(y, NUM_DLQ_2016, by="CustomerId")
y$NUM_DLQ_2016[is.na(y$NUM_DLQ_2016)]<-0

DLQ_AMT_1512 <- overdue_1512 %>% 
  group_by(CustomerId) %>% 
  dplyr::summarise(DLQ_AMT_1512=sum(DLQ_AMT))

y <- join(y, DLQ_AMT_1512, by="CustomerId")
y$DLQ_AMT_1512[is.na(y$DLQ_AMT_1512)]<-0
View(y)

y$DLQ_AMT_1512 <- ifelse(y$NUM_DLQ_1512>0,1,0)
View(y)

library(data.table)
NUM_COM <- data.frame(setDT(df)[, .(NUM_COM=uniqueN(COM_KEY)),
                                 .(CustomerId)])
View(NUM_COM)
z <- left_join(y, NUM_COM, by="CustomerId")
View(z)

z$NUM_DLQ_2016 <- ifelse(z$NUM_DLQ_2016>0,1,0)

input <- z

str(input)

input$NUM_DLQ_2016 <- factor(input$NUM_DLQ_2016)
input$NUM_DLQ_1512 <- factor(input$NUM_DLQ_1512)
input$GENDER <- factor(input$GENDER)
str(input)

input <- input %>% 
  select(-CustomerId)
View(input)
