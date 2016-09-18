library(RMySQL)


con <- dbConnect(MySQL(), user="root", password="kaist5753", dbname="nhiss", host="localhost")

# tables <- c("PT_R", "PT_BFC", "INST_R", "INST", "MCARE")
tables <- c("PT_R", "PT_BFC")

#tbl <- lapply(tables, dbReadTable, conn=con)

myquery <- "SELECT * FROM PT_BFC"
tbl2 <- dbGetQuery(con, myquery)

summary(tbl2)

dbDisconnect(con)