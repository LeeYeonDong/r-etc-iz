# 1. java와 rJava 설치
# Amazon Corretto Java를 설치합니다.
install.packages("multilinguer")
library(multilinguer)
install_jdk()

# 2. KoNLP 의존성 패키지 설치
# KoNLP 의존성 패키지를 설치합니다.
install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"), type = "binary")

# 3. 깃헙 버전 KoNLP 설치
# 깃헙 버전 KoNLP 패키지를 설치합니다.
install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP", 
                        upgrade = "never",
                        INSTALL_opts=c("--no-multiarch"))

buildDictionary(ext_dic = "woorimalsam")  # "woorimalsam" dic을 불러옵니다
useNIADic() 

# 4. KoNLP 실행 확인
# KoNLP 패키지가 작동하는지 확인합니다.

library(KoNLP)
## Checking user defined dictionary!
extractNoun("대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다.")
## [1] "대한민국" "주권"     "국민"     "권력"     "국민"

# 5. Fail to locate 'scala-library-2.11.8.jar' 에러 해결하기
# KoNLP 설치 중 Fail to locate 'scala-library-2.11.8.jar' 에러가 발생하면 다음 코드를 실행합니다. 그런 다음 RStudio를 재실행한 후 KoNLP 패키지가 작동하는지 확인합니다.

download.file(url = "https://repo1.maven.org/maven2/org/scala-lang/scala-library/2.11.8/scala-library-2.11.8.jar",
              destfile = paste0(.libPaths()[1], "/KoNLP/Java/scala-library-2.11.8.jar"))


# 1. https://repo1.maven.org/maven2/org/scala-lang/scala-library/2.11.8/scala-library-2.11.8.jar 여기에 접속하셔서 <scala-library-2.11.8> 파일을 다운
# 2. C:\Users\선생님 노트북 이름\AppData\Local\R\win-library\4.2\KoNLP\java에 가셔서 다운받은 scala-library-2.11.8 파일을 붙여넣기한다
# 3. C:\Program Files\R\R-4.2.2\library 로 간다
# 4. 아래 KoNLP(D드라이브에 받아놓은) 다운받아 C:\Program Files\R\R-4.2.2\library에 붙여넣은 다음 KoNLP폴더를 압축을 푼다
# 5. R studio를 모두 끄고 다시 실행한다
