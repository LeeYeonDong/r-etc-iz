
res_jg <- GET(url = "https://cafe.naver.com/joonggonara",
              query = list(search.clubid = 10050146,
                           search.media = 0,
                           search.searchdate = "all",
                           search.defaultValue = 1,
                           search.exact = "" ,
                           search.include = "", 
                           userDisplay = 15,
                           search.exclude = "", 
                           search.option = 0,
                           search.sortBy = "date",
                           search.searchBy = 0,
                           search.searchBlockYn = 0,
                           search.includeAll = "" ,
                           search.query = "%BF%A1%BE%EE%C7%C1%B6%F3%C0%CC%B1%E2%26",
                           search.viewtype = "title",
                           search.viewtype = "title",
                           search.page = 1))





##cmd
#cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445







### RSelnium

remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()


### 중고나라(최신순)

main <- "https://cafe.naver.com/joonggonara"
mid1 <- "?iframe_url=/ArticleSearchList.nhn%3Fsearch.clubid=10050146%26search.media=0%26search.searchdate=all%26search.defaultValue=1%26search.exact=%26search.include=%26userDisplay=15%26search.exclude=%26search.option=0%26search.sortBy=date%26search.searchBy=0%26search.searchBlockYn=0%26search.includeAll=%26search.query="
검색어 <- "%BF%A1%BE%EE%C7%C1%B6%F3%C0%CC%B1%E2%26search.viewtype=title%26"
mid2 <- "search.viewtype=title%26search.page=1"


## 최초의 검색 주소(반복문 전까지) 이어붙이기

target <- paste(main, mid1, sep = "")

target <- paste(target, 검색어, sep = "")

target <- paste(target, mid2, sep = "")


remDr$navigate(target)


##링크1

for(i in 0:n){
  
  i <- i+1
  
  frontpage <- remDr$getPageSource()[[1]]
  링크.tmp <- read_html(frontpage) 
  링크.tmp <- 링크.tmp %>%  
    html_nodes("tr") %>% 
    html_nodes("div.inner_list") %>% 
    html_nodes("a.article") %>%
    html_attr("href") %>%
    unique()
  
  