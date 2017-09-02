library(httr)
library(stringr)
library(tidyquant)
library(quantmod)

URLS <- c("https://www.nseindia.com/marketinfo/companyTracker/compInfo.jsp?symbol=%s&series=EQ",
          "http://www.moneycontrol.com/mccode/common/autosuggesion.php?query=%s&type=1&format=json",
          "http://www.moneycontrol.com/tech_charts/nse/his/%s.csv")

getData <- function(symbol) {
  ISIN <- GET(sprintf(URLS[1], symbol)) %>% content("text") %>% str_match("ISIN : </b>(.*)</td>") %>% .[2]
  MC <- GET(sprintf(URLS[2], ISIN)) %>% content("parsed") %>% .[[1]] %>% .$sc_id %>% tolower()
  CSV <- GET(sprintf(URLS[3], MC)) %>% content("text", encoding = "ISO-8859-1") %>% 
    read_csv(col_names = c("Date", "Open", "High", "Low", "Close", "Volume", "Bonus", "Dividends", "Rights", "Splits")) %>%
    mutate(Date = as.POSIXct(Date, format = "%d %b %Y"))
  return(CSV)
}

symbol <- "BAJAJ-AUTO"
df <- getData(symbol)

barChart(df %>% as_xts, name=symbol, subset='2016::', theme='white.mono', bar.type='hlc')
