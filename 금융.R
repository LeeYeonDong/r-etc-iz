### PBR = 주가 / 주당순자산율 ### PER = 주가 / 당기순이익 ### ROE = 당기순이익 / 자본총액 = PBR/PER


###### 주식코드(ticker)_컴퍼니가이드

컴퍼니가이드_url <- "http://comp.fnguide.com/XML/Market/CompanyList.txt"

컴퍼니가이드 <- fromJSON(컴퍼니가이드_url)$Co
head(컴퍼니가이드)
str(주식코드)
summary(주식코드)

tail(컴퍼니가이드)



###### 주식코드(ticker)_KRXmarketdata -- opt권한

krxmarket_url <- "http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx"

krxmarket_query <- list(
  name = "fileDown",
  filetype = "csv",
  url = "MKD/04/0404/04040200/mkd04040200_01",
  market_gubun = "ALL",
  sect_tp_cd = "ALL",
  schdate = "20191209",
  pagePath = "contents/MKD/04/0404/04040200/MKD04040200.jsp"
)

krxmarket_otp <- GET(url = krxmarket_url,
                     query = krxmarket_query)  %>% 
  read_html() %>% 
  html_text()



###### 주식코드_KRXmarketdata -- csv불러오기

krxmarket_url_down <- "http://file.krx.co.kr/download.jspx"

주식코드_krxmarketurl_down <- POST(url = krxmarket_url_down,
                               query = list(code = krxmarket_otp),
                               add_headers(referer = krxmarket_url)) %>% 
  read_html() %>% 
  html_text()

주식코드_krxmarketurl_down <- 주식코드_krxmarketurl_down %>% read_csv()

주식코드_krxmarketurl_down <- 주식코드_krxmarketurl_down %>% 
  select("종목코드","종목명","년","x2017년")


###### 주가(price)