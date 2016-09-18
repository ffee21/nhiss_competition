rm(list=ls())

library(RMySQL)
library(dplyr)
library(ggplot2)

con = dbConnect(MySQL(),user="root", password="qwer1234", 
                dbname="nhiss", host="localhost")
myQuery <- "SELECT 
              THE_ID,
              CONCAT(MDCARE_STRT_YEAR, '-', LPAD(MDCARE_STRT_MONTH, 2, '0')) AS YEARMONTH, 
              MDCARE_STRT_YEAR, 
              MDCARE_STRT_MONTH, 
              MDCARE_STRT_DAY, 
              MDCARE_STRT_DT, 
              PT_R_ID,
              ED_RC_TOT_AMT,
              EDC_SBA,
              EDC_INSUR_BRDN_AMT
            FROM MCARE 
            ORDER BY MDCARE_STRT_DT ASC"
R1 <- dbGetQuery(con, myQuery)
R2 <- tbl_df(R1)
R3 <- R2 %>%
  group_by(YEARMONTH, MDCARE_STRT_YEAR, MDCARE_STRT_MONTH) %>%
  summarize(count = n_distinct(THE_ID)) %>%
  arrange(MDCARE_STRT_YEAR, MDCARE_STRT_MONTH)
  

R3$YEARMONTH = as.factor(R3$YEARMONTH)
R3$MDCARE_STRT_YEAR = as.factor(R3$MDCARE_STRT_YEAR)
R3$MDCARE_STRT_MONTH = as.factor(R3$MDCARE_STRT_MONTH)
R3$count = as.numeric(R3$count)

ggplot(R3, aes(x=YEARMONTH, y=count)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))

ggplot(R3, aes(MDCARE_STRT_MONTH, count)) +
  geom_bar(aes(fill=MDCARE_STRT_YEAR), stat="identity", position="dodge") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))

c201408 <- subset(R3, MDCARE_STRT_YEAR==2014 & MDCARE_STRT_MONTH==8)$count
c201407 <- subset(R3, MDCARE_STRT_YEAR==2014 & MDCARE_STRT_MONTH==7)$count
c201308 <- subset(R3, MDCARE_STRT_YEAR==2013 & MDCARE_STRT_MONTH==8)$count

c201408
(c201408 / c201407 - 1) * 100
(c201408 / c201308 - 1) * 100



R4 <- R3 %>%
  group_by(MDCARE_STRT_MONTH) %>%
  summarize(avg = mean(count), stdv = sd(count)) %>%
  select(MDCARE_STRT_MONTH, avg, stdv) %>%
  arrange(MDCARE_STRT_MONTH)

ggplot(R4, aes(x=MDCARE_STRT_MONTH, y=avg)) +
  geom_bar(stat="identity") +
  coord_cartesian(xlim=c(1, 12)) +
  geom_errorbar(data=R4, mapping=aes(x=MDCARE_STRT_MONTH, ymin=avg-stdv, ymax=avg+stdv))


# patients count
R5 <- R2 %>%
  group_by(MDCARE_STRT_YEAR, MDCARE_STRT_MONTH) %>%
  summarize(count = n_distinct(PT_R_ID)) %>%
  arrange(MDCARE_STRT_YEAR, MDCARE_STRT_MONTH)

R5$MDCARE_STRT_YEAR = as.factor(R5$MDCARE_STRT_YEAR)
R5$MDCARE_STRT_MONTH = as.factor(R5$MDCARE_STRT_MONTH)
R5$count = as.numeric(R5$count)

ggplot(R5, aes(x=MDCARE_STRT_MONTH, y=count)) +
  geom_bar(aes(fill=MDCARE_STRT_YEAR), stat="identity", position="dodge") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))

p201408 <- subset(R5, MDCARE_STRT_YEAR==2014 & MDCARE_STRT_MONTH==8)$count
p201407 <- subset(R5, MDCARE_STRT_YEAR==2014 & MDCARE_STRT_MONTH==7)$count
p201308 <- subset(R5, MDCARE_STRT_YEAR==2013 & MDCARE_STRT_MONTH==8)$count

p201408
(p201408 / p201407 - 1) * 100
(p201408 / p201308 - 1) * 100

a201408 <- c201408/p201408
a201407 <- c201407/p201407
a201308 <- c201308/p201308

a201408
(a201408 / a201407 - 1) * 100
(a201408 / a201308 - 1) * 100


# medical bill summary

R6 <- R2 %>%
  group_by(MDCARE_STRT_YEAR, MDCARE_STRT_MONTH) %>%
  summarize(bill = sum(as.double(ED_RC_TOT_AMT))) %>%
  arrange(MDCARE_STRT_YEAR, MDCARE_STRT_MONTH)

R6$MDCARE_STRT_YEAR = as.factor(R6$MDCARE_STRT_YEAR)
R6$MDCARE_STRT_MONTH = as.factor(R6$MDCARE_STRT_MONTH)

ggplot(R6, aes(x=MDCARE_STRT_MONTH, y=bill)) +
  geom_bar(aes(fill=MDCARE_STRT_YEAR), stat="identity", position="dodge") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))

bt201408 <- subset(R6, MDCARE_STRT_YEAR==2014 & MDCARE_STRT_MONTH==8)$bill
bt201407 <- subset(R6, MDCARE_STRT_YEAR==2014 & MDCARE_STRT_MONTH==7)$bill
bt201308 <- subset(R6, MDCARE_STRT_YEAR==2013 & MDCARE_STRT_MONTH==8)$bill

bt201408
(bt201408 / bt201407 - 1) * 100
(bt201408 / bt201308 - 1) * 100



# company's medical bill summary

R7 <- R2 %>%
  group_by(MDCARE_STRT_YEAR, MDCARE_STRT_MONTH) %>%
  summarize(bill = sum(as.double(EDC_INSUR_BRDN_AMT))) %>%
  arrange(MDCARE_STRT_YEAR, MDCARE_STRT_MONTH)

R7$MDCARE_STRT_YEAR = as.factor(R7$MDCARE_STRT_YEAR)
R7$MDCARE_STRT_MONTH = as.factor(R7$MDCARE_STRT_MONTH)

ggplot(R7, aes(x=MDCARE_STRT_MONTH, y=bill)) +
  geom_bar(aes(fill=MDCARE_STRT_YEAR), stat="identity", position="dodge") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))

bc201408 <- subset(R7, MDCARE_STRT_YEAR==2014 & MDCARE_STRT_MONTH==8)$bill
bc201407 <- subset(R7, MDCARE_STRT_YEAR==2014 & MDCARE_STRT_MONTH==7)$bill
bc201308 <- subset(R7, MDCARE_STRT_YEAR==2013 & MDCARE_STRT_MONTH==8)$bill

bc201408
(bc201408 / bc201407 - 1) * 100
(bc201408 / bc201308 - 1) * 100


avr_bc201408 <- bc201408/c201408
avr_bc201407 <- bc201407/c201407
avr_bc201308 <- bc201308/c201408

avr_bc201408
(avr_bc201408 / avr_bc201407 - 1) * 100
(avr_bc201408 / avr_bc201308 - 1) * 100

# inexpensive medical practice percentage

R8 <- R2 %>% 
  group_by(MDCARE_STRT_YEAR, MDCARE_STRT_MONTH) %>% 
  mutate(med=median(as.double(ED_RC_TOT_AMT)), 
         inexpen=(ED_RC_TOT_AMT<med*0.5), 
         expen=(ED_RC_TOT_AMT>med*1.5)) %>% 
  summarise(inexpen_ratio=as.numeric(table(inexpen)[2])/n(), 
            expen_ratio=as.numeric(table(expen)[2])/n(),
            bill=sum(as.double(EDC_INSUR_BRDN_AMT)),
            inexpen_bill=sum(as.double(EDC_INSUR_BRDN_AMT)*as.integer(inexpen)),
            inexpen_bill_ratio=inexpen_bill/bill,
            expen_bill=sum(as.double(EDC_INSUR_BRDN_AMT)*as.integer(expen)),
            expen_bill_ratio=expen_bill/bill)

R8$MDCARE_STRT_YEAR = as.factor(R8$MDCARE_STRT_YEAR)
R8$MDCARE_STRT_MONTH = as.factor(R8$MDCARE_STRT_MONTH)

ggplot(R8, aes(x=MDCARE_STRT_MONTH, y=inexpen_ratio)) +
  geom_bar(aes(fill=MDCARE_STRT_YEAR), stat="identity", position="dodge") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))

ier201408 <- subset(R8, MDCARE_STRT_YEAR==2014 & MDCARE_STRT_MONTH==8)$inexpen_ratio
ier201407 <- subset(R8, MDCARE_STRT_YEAR==2014 & MDCARE_STRT_MONTH==7)$inexpen_ratio
ier201308 <- subset(R8, MDCARE_STRT_YEAR==2013 & MDCARE_STRT_MONTH==8)$inexpen_ratio

ier201408 * 100
(ier201408 / ier201407 - 1) * 100
(ier201408 / ier201308 - 1) * 100

ggplot(R8, aes(x=MDCARE_STRT_MONTH, y=expen_ratio)) +
  geom_bar(aes(fill=MDCARE_STRT_YEAR), stat="identity", position="dodge") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))

er201408 <- subset(R8, MDCARE_STRT_YEAR==2014 & MDCARE_STRT_MONTH==8)$expen_ratio
er201407 <- subset(R8, MDCARE_STRT_YEAR==2014 & MDCARE_STRT_MONTH==7)$expen_ratio
er201308 <- subset(R8, MDCARE_STRT_YEAR==2013 & MDCARE_STRT_MONTH==8)$expen_ratio

er201408 * 100
(er201408 / er201407 - 1) * 100
(er201408 / er201308 - 1) * 100


ggplot(R8, aes(x=MDCARE_STRT_MONTH, y=inexpen_bill_ratio)) +
  geom_bar(aes(fill=MDCARE_STRT_YEAR), stat="identity", position="dodge") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))

iebr201408 <- subset(R8, MDCARE_STRT_YEAR==2014 & MDCARE_STRT_MONTH==8)$inexpen_bill_ratio
iebr201407 <- subset(R8, MDCARE_STRT_YEAR==2014 & MDCARE_STRT_MONTH==7)$inexpen_bill_ratio
iebr201308 <- subset(R8, MDCARE_STRT_YEAR==2013 & MDCARE_STRT_MONTH==8)$inexpen_bill_ratio

iebr201408 * 100
(iebr201408 / iebr201407 - 1) * 100
(iebr201408 / iebr201308 - 1) * 100

ebr201408 <- subset(R8, MDCARE_STRT_YEAR==2014 & MDCARE_STRT_MONTH==8)$expen_bill_ratio
ebr201407 <- subset(R8, MDCARE_STRT_YEAR==2014 & MDCARE_STRT_MONTH==7)$expen_bill_ratio
ebr201308 <- subset(R8, MDCARE_STRT_YEAR==2013 & MDCARE_STRT_MONTH==8)$expen_bill_ratio

ebr201408 * 100
(ebr201408 / ebr201407 - 1) * 100
(ebr201408 / ebr201308 - 1) * 100


ggplot(R8, aes(x=MDCARE_STRT_MONTH, y=expen_bill_ratio)) +
  geom_bar(aes(fill=MDCARE_STRT_YEAR), stat="identity", position="dodge") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))



dbDisconnect(con)