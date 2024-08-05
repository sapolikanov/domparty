
# “Rules vs discretion in pandemic times: evidence from Russian health procurement” #

# Install necessary packages

install.packages("rjson")
install.packages("RJSONIO")
install.packages("dplyr")
install.packages('rio')
install.packages('sparklyr')

# Activate packages

library(dplyr)
library(jsonlite)
library(jqr)


contracts1 <- jsonlite::stream_in(con = "contracts_44fz_201901-latest.jsonl", flatten = TRUE)
Sys.setlocale(locale = "Russian")

memory.limit(10000000)

contracts <- "C:/contracts/contracts_44fz_201901-20210601.jsonl"
contracts1 <- stream_in(textConnection(readLines(contracts,n = 1000)), flatten = TRUE)
all.equal(contracts1, as.data.frame(contracts1))
head(contracts1)7

colnames(contracts1)





