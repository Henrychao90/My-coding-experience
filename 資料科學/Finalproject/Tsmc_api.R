library(httr)
library(data.table)
library(dplyr)
url = 'https://api.finmindtrade.com/api/v4/data'
response = httr::GET(
  url = url,
  query = list(
    dataset="TaiwanStockInstitutionalInvestorsBuySell",
    data_id= "2330",
    start_date= "2014-12-24",
    token = "" # 參考登入，獲取金鑰
  )
)
data = content(response)
df = data$data %>% 
  do.call('rbind',.) %>% 
  data.table
tail(df)

