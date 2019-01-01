library(stringr)

get_dic_naver <- function(word){
  out <- NULL
  v <- c("1","11","21","31","41")
  q <- iconv(word,from="cp949",to="UTF-8")
  q <- enc2utf8(q)
  q <- URLencode(q)
  for(i in v){
    url <- paste0("https://search.naver.com/search.naver?where=kdic&sm=tab_pge&query=",q,"&ie=utf8&start=",i)
    temp <- readLines(url, encoding = "UTF-8")
    temp <- paste(temp, collapse=" ")
    temp <- unlist(str_split(temp, "<"))
    get_url <- temp[which(str_detect(temp,"title_area"))+1]
    start <- str_locate(get_url, "http")[,1]
    end <- str_locate(get_url, "target")[,1]
    get_url <- str_sub(get_url, start, end-3)
    out <- c(out, get_url)
  }
  return(out)
}

# 신간회
one <- get_dic_naver("신간회")
# 한글날
two <- get_dic_naver("한글날")
# 임시정부
three <- get_dic_naver("임시정부")
three <- c(three, get_dic_naver("광복군"))
# 카이로 회담
four <- get_dic_naver("카이로 회담")
# 조선건국준비위원
five <- get_dic_naver("조선건국준비위원")
# 815광복
six <- get_dic_naver("광복절")


word <- "안중근"
get_wiki <- function(word){
  q <- iconv(word,from="cp949",to="UTF-8")
  q <- enc2utf8(q)
  q <- URLencode(q)
  url <- paste0("https://ko.wikipedia.org/wiki/",q)
  temp <- readLines(url, encoding = "UTF-8")
  start <- which(str_detect(temp,"firstHeading"))
  last <- which(str_detect(temp,"visualClear"))
  contents <- temp[start:last]
  contents <- gsub("<.*?>","", contents)
  contents <- gsub("\t","",contents)
  contents <- gsub("[0-9]","",contents)
  contents <- gsub("[con-zcon-z]","",contents)
  contents <- contents[-which(nchar(contents)==0)]
  contents <- paste(contents, collapse = " ")
  contents <- gsub("<.*?>","", contents)
  contents <- gsub("[con-zcon-z]","",contents)
  contents <- gsub("[[:punct:]]"," ", contents)
  contents <- gsub("우리 모두의 백과사전","", contents)
  contents <- gsub("위키백과","", contents)
  contents <- gsub("문서","", contents)
  contents <- gsub("출처","", contents)
  contents <- gsub("위키백과","", contents)
  return(contents)
}
get_wiki("안중근")

#http://100.daum.net/search/entry?q=3.1%EC%9A%B4%EB%8F%99&page=1
get_dic_daum <- function(word){
  out <- NULL
  q <- iconv(word,from="cp949",to="UTF-8")
  q <- enc2utf8(q)
  q <- URLencode(q)
  for(i in 1:5){
    url <- paste0("http://100.daum.net/search/entry?q=",q,"&page=",i)
    temp <- readLines(url, encoding = "UTF-8")

  }
}

