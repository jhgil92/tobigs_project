#################################################################
# LIBRARY
library(igraph)
library(KoNLP)
library(stringr)
library(tm)
library(readr)
library(ggplot2)
library(visNetwork)

#for(i in 1:length(names)){
#  mergeUserDic(data.frame(names[i], 'ncn'))
#}


##################################################################
# DATA LOAD
event <- read.csv("C:\\Users\\hp1\\Desktop\\사건166.csv")
name <- read.csv("C:\\Users\\hp1\\Desktop\\name.csv")
name <- name[-135,] # 중복인물 제외
names <- as.character(unique(name[,2]))

##################################################################
# FUNCTION DEFINITION
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
find_names <- function(contents){
  count <- rep(0, length(names))
  for(i in 1:length(names)){
    if(i %in% c(6,77,80,85,86,129,164,177,178,198,201)){
      tmp <- unlist(str_split(names[i],"_"))
      for(j in 1:10){
        if(str_detect(contents[[j]], tmp[1]) | str_detect(contents[[j]], tmp[2])){
          count[i] <- count[i]+1
        }
      }
    }else{
      for(j in 1:10){
        if(str_detect(contents[[j]], names[i])){
          count[i] <- count[i]+1
        }
      }
    }
  }
  names(count) <- names
  sort(count[count!=0], decreasing = T)
}
normalize <- function(x){return((x - min(x)) / (max(x) - min(x)))}

##################################################################
for(n in 153:166){
  # 크롤링할 URL주소
  event1 <- get_dic_naver(event$hlod.titleKor[n])
  event1 <- event1[str_detect(event1, "terms.naver")]
  # 관련 데이터 크롤링
  contents <- list()
  length(contents) <- 10
  for(i in 1:10){
    tmp <- read_lines(event1[i])
    s <- which(str_detect(tmp, "<div id=\"container\">"))
    e <- which(str_detect(tmp, "tmp_source"))
    tmp <- tmp[s:e]
    tmp <- gsub("<.*?>","", tmp)
    tmp <- gsub("\t","", tmp)
    tmp <- gsub("&gt;","",tmp)
    tmp <- gsub("&nbsp;","",tmp)
    tmp <- gsub("&lt;","",tmp)
    tmp <- paste(tmp, collapse = ".")
    tmp <- str_sub(tmp, start=str_locate(tmp, "window.onload")[1,2]+39)
    contents[[i]] <- tmp
    contents_split <- lapply(contents, function(x){str_split(x, "\\.")})
    contents_split <- lapply(contents_split, function(x){return(unlist(x)[nchar(unlist(x))>2])})
    cat("\n",i)
  }
  # 등장인물
  event1_names <- find_names(contents)
  if(length(event1_names)>20){event1_names <- event1_names[1:20]}
  
  # 네트워크 계산
  net <- NULL
  tmp <- as.data.frame(matrix(NA, ncol=2))
  names(event1_names) <- gsub("_","|",names(event1_names))
  for(i in 1:length(names(event1_names))){
    if(i!=length(names(event1_names))){
      for(j in (i+1):length(names(event1_names))){
        for(h in 1:10){
          if(str_detect(unlist(contents)[h], names(event1_names)[i]) & str_detect(unlist(contents)[h], names(event1_names)[j])){
            tmp[1,1] <- names(event1_names)[i]
            tmp[1,2] <- names(event1_names)[j]
            net <- rbind(net, tmp)
          }
        }
      }
    }
    cat("\n",i)
  }
  for(i in 1:length(names(event1_names))){
    if(i!=length(names(event1_names))){
      for(j in (i+1):length(names(event1_names))){
        for(h in 1:length(unlist(contents_split))){
          if(str_detect(unlist(contents_split)[h], names(event1_names)[i]) & str_detect(unlist(contents_split)[h], names(event1_names)[j])){
            tmp[1,1] <- names(event1_names)[i]
            tmp[1,2] <- names(event1_names)[j]
            net <- rbind(net, tmp)
          }
        }
      }
    }
    cat("\n",i)
  }
  if(nrow(net)==1){
    net <- as.data.frame(matrix(gsub("\\|","_",net), ncol=2))
  }else{
    net <- as.data.frame(apply(net,2, function(x)gsub("\\|","_",x)))
  }
  names(event1_names) <- gsub("\\|","_",names(event1_names))
  net <- net[order(net[,1], net[,2]),]
  rownames(net) <- 1:nrow(net)
  nett <- unique(net)
  tmp <- c(rownames(nett),nrow(net)+1)
  tmp <- as.numeric(tmp)
  count <- tmp[2:length(tmp)] - tmp[1:(length(tmp)-1)]
  
  links <- cbind(nett, count)
  rownames(links) <- 1:nrow(links)
  colnames(links) <- c("from", "to", "weight")
  nodes <- as.data.frame(matrix(NA, ncol=3, nrow=length(event1_names)))
  colnames(nodes) <- c("id","power","type")
  nodes[,1] <- names(event1_names)
  nodes[,2] <- as.vector(event1_names)
  for(i in 1:nrow(nodes)){
    nodes[i,3] <- as.character(name[which(name$x == nodes[i,1]),][,3])
  }
  for(i in 1:nrow(nodes)){
    nodes$title[i] <- paste0("<p>이름 : ", as.character(nodes[i,1]), "<br>",
                             "설명 : ", as.character(name[which(name$x == nodes[i,1]),][,4],"</p>"))
  }
  
  
  # nodes 옵션
  nodes$size <- (normalize(nodes$power)+0.1)*20+5
  nodes$borderWidth <- 1
  ver.col <- NULL
  for(i in 1:length(event1_names)){
    if(nodes$type[i]=="한"){ver.col[i] <- "blue"}#adjustcolor( "royalblue3", alpha.f = tmp[i])}
    if(nodes$type[i]=="일"){ver.col[i] <- "red"}#adjustcolor( "red", alpha.f = tmp[i])}
    if(nodes$type[i]=="친일"){ver.col[i] <- "violetred"}#adjustcolor( "violetred1", alpha.f = tmp[i])}
    if(nodes$type[i]=="기타"){ ver.col[i] <- "green"}#adjustcolor( "palegreen", alpha.f = tmp[i])}
  }
  #nodes$color <- ver.col
  nodes$color.background <- ver.col
  nodes$color.highlight.background <- ver.col
  nodes$color.border <- "black"
  nodes$color.highlight.border <- "darkgray"
  nodes$label <- nodes$id
  nodes$shadow <- T
  nodes$font.color <- 'gray35'
  nodes$font.size <- (normalize(nodes$power)+0.1)*10+5
  nodes$font.famliy <- 'malgun'
  
  # 링크 옵션
  if(max(links$weight)>6){
    links$width <- (((normalize(links$weight)+0.1)*10)^2)/20
  }else{
    links$width <- (links$weight/20)^2
  }
  links$color <- "gray"  
  links$smooth <- F
  links$shadow <- T
  
  network <- visNetwork(nodes, links, main = "CHARACTER NETWORK",width="100%", height="600px") %>%
    visOptions(highlightNearest = TRUE,
               nodesIdSelection = TRUE,
               selectedBy = "type")# %>%
    #visNodes(color=list(background = ver.col,
    #                    border = 'brown'))
    #visInteraction(navigationButtons = TRUE)
  visSave(network, paste0("event",n,".html"))
  cat("\n",paste0("################## ",n,"번째 사건까지 저장되었습니다."," ##################"))
}