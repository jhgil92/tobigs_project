# LIBRARY
#source("https://bioconductor.org/biocLite.R")
#biocLite("graph")
library(igraph)
library(KoNLP)
library(stringr)
library(tm)
library(readr)
library(ggplot2)
library(visNetwork)
library(KoNLP)
library(NLP)
library(tm)
library(openNLP)
library(graph)
library(DT)

##################################################################
# DATA LOAD
event <- read.csv("C:\\Users\\hp1\\Desktop\\사건166.csv")
name <- read.csv("C:\\Users\\hp1\\Desktop\\name.csv")
name <- name[-135,] # 중복인물 제외
names <- as.character(unique(name[,2]))

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
SplitText <- function(Phrase) { 
  unlist(strsplit(Phrase," "))
}
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
IsPunctuated <- function(Phrase) {
  length(grep("\\.|,|!|\\?|;|:|\\)|]|}\\Z",Phrase,perl=TRUE))>0 # punctuation: . , ! ? ; : ) ] }
}
SelectTaggedWords <- function(Words,tagID) {
  Words[ grep(tagID,Words) ]
}
RemoveTags <- function(Words) {
  sub("/[A-Z]{2,3}","",Words)
}
IsSelectedWord <- function(Word) {
  ifelse(length(which(selected_words == Word))>0, TRUE, FALSE)
}
GetWordLinks <- function(position,scope) {
  scope <- ifelse(position+scope>length(words),length(words),position+scope)
  links <- ""
  for (i in (position+1):scope) {
    if ( IsSelectedWord(words[i]) ) links <- c(links,words[i])
  }
  
  if (length(links)>1) {
    links[2:length(links)]
  }
  else {
    links <- ""
  }
}
ConstructTextGraph <- function(n) { 
  word_graph <- new("graphNEL")
  i <- 1
  while (i < length(words) ) {
    if ( IsSelectedWord(words[i]) ) {                                   
      links <- GetWordLinks(i,n)                                
      if (links[1] != "") {                                     
        #cat(i," ",words[i]," - ",paste(c(links),collapse=" "),"\n")
        if ( length(which(nodes(word_graph)==words[i]))==0  ) {     
          word_graph <- addNode(words[i],word_graph)
        }                                               
        
        for (j in 1:length(links)) {
          if ( length(which(nodes(word_graph)==links[j]))==0 ) {
            word_graph <- addNode(links[j],word_graph)
            word_graph <- addEdge(words[i],links[j],word_graph,1)
          } 
          else {
            if ( length(which(edges(word_graph,links[j])[[1]]==words[i]))>0 ) { 
              prev_edge_weight <- as.numeric(edgeData(word_graph,words[i],links[j],"weight"))
              edgeData(word_graph,words[i],links[j],"weight") <- prev_edge_weight+1
            }
            else {
              word_graph <- addEdge(words[i],links[j],word_graph,1)
            }
          } 
        }
      }
    }
    i <- i+1
  }
  word_graph
}
ko.words <- function(x){
  x <- as.character(x)
  pos <- paste(SimplePos09(x))
  extracted <- str_match(pos, '([가-힣]+)/[NP]')
  keyword <- extracted[,2]
  keyword[!is.na(keyword)]
}
pre_words <- function(x){
  words <- gsub("[+[가-힣]+\\s+[가-힣]+]" ,"", as.character(x))
  words <- gsub("\\(+[가-힣]+\\s+[가-힣]+\\)"," ", words)
  words <- gsub("\\(+[가-힣]+\\)"," ", words)
  words <- gsub("[+[가-힣]+]" ,"", words)
  words <- SimplePos09(words)
  words <- paste(words)
  words <- gsub("/[A-Z]"," ", words)
  words <- gsub("\\+"," ",words)
  words <- paste(words)
  words <- unlist(str_split(words, " "))
  words <- words[nchar(words)!=0]
  return(words)
}

#####################################
for(n in 159:166){
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
  #########################################
  cat("\n","###################",n,"######## 요약시작 ##########")
  doc <- contents[[which.max(unlist(lapply(contents_split, length))[1:3])]]
  
  corp <- Corpus(VectorSource(doc))
  corp <- tm_map(corp, stripWhitespace)
  corp <- tm_map(corp, removePunctuation)
  words <- pre_words(corp[[1]])
  selected_words <- unique(ko.words(doc))
  text_graph <- ConstructTextGraph(2)
  cat("\n","###################",n,"######## 그래프 생성 ##########")
  words_with_punctuation <- SplitText(as.character(doc))
  d <- 0.85
  threshold <- 1e-4
  text_nodes <- nodes(text_graph)
  nodes_num <- length(text_nodes)
  nodes_rank <- matrix(1,nodes_num,2)
  k <- 0
  convergence_reached <- FALSE
  repeat {
    for (i in 1:nodes_num) {
      incoming_link <- adj(text_graph,text_nodes[i])[[1]]
      incoming_num <- length(incoming_link)
      
      tmp <- 0
      for (j in 1:incoming_num) {
        link_num <- which(text_nodes==incoming_link[j])
        outgoing_num <- length(adj(text_graph,text_nodes[link_num])[[1]])
        tmp <- tmp + nodes_rank[link_num,1] / outgoing_num
      }
      nodes_rank[i,1] <- (1-d)+d*tmp
    }
    k <- k+1
    for (i in 1:nodes_num) {
      if (abs(nodes_rank[i,1]-nodes_rank[i,2])<threshold) convergence_reached <- TRUE
    }
    if (convergence_reached) break
    nodes_rank[,2] <- nodes_rank[,1]
  }
  cat("\n","###################",n,"######## 계산 끝 ##########")
  keywords_num <- round(nodes_num/3)
  ranked_words <- data.frame(text_nodes,nodes_rank[,1])
  names(ranked_words) <- c("word","rank")
  strong_words <- ranked_words[order(ranked_words$rank,decreasing=TRUE),]
  strong_words <- as.character(strong_words$word[1:keywords_num])
  keywords <- ""
  keywords_scores <- 0
  for (i in 1:keywords_num) {
    keyword_positions <- which(words==strong_words[i])
    for (j in 1:length(keyword_positions)) {
      keyword <- ""
      keyword_score <- 0
      k <- keyword_positions[j]                                       
      repeat {
        if (IsSelectedWord(words[k])) { 
          keyword <- trim(paste(c(keyword,words[k]),collapse=" "))
          keyword_score <- keyword_score + ranked_words[which(ranked_words$word==words[k]),2]
        }
        else break                                                    
        
        if (IsPunctuated(words_with_punctuation[k])) break
        if (k==length(words)) break                               
        k <- k+1
      }
      k <- keyword_positions[j]-1                                 
      repeat {
        if (k<1) break
        
        if (IsSelectedWord(words[k])) { 
          keyword <- paste(c(words[k],trim(keyword)),collapse=" ")
          keyword_score <- keyword_score + ranked_words[which(ranked_words$word==words[k]),2]
        }
        else break
        
        if (k>1) {            
          if (IsPunctuated(words_with_punctuation[k-1])) break
        } 
        k <- k-1
      }
      if (keyword!=strong_words[i]) { 
        keywords <- c(keywords,keyword)
        keywords_scores <- c(keywords_scores,keyword_score)
      }   
    }
  }
  keywords_df <- data.frame(keywords,keywords_scores)
  keywords_list <- keywords_df[order(keywords_df$keywords_scores,decreasing=TRUE),]
  top_keywords <- unique(keywords_list$keywords)
  origin <- str_split(doc, "\\.")
  sentence <- unlist(origin)
  sentence <- str_split(sentence, "")
  sentence <- lapply(sentence, function(x)gsub(" ", "", x))
  sentence <- lapply(sentence, function(x){return(x[nchar(x)!=0])})
  top <- gsub(" ", "", top_keywords)
  top <- str_split(top, "")
  ranking <- NULL
  i <- 1
  while(length(ranking)!=10){
    rank <- NULL
    for(j in 1:length(sentence)){
      rank[j]  <- (sum(unique(top[[i]]) %in% unique(sentence[[j]])) / length(unique(top[[i]])) )
    }
    ranking <- c(ranking,which.max(rank))
    ranking <- unique(sort(ranking))
    i <- i+1
  }
  output <- NULL
  for(i in 1:10){
    output[[i]] <- origin[[1]][ranking[i]]
  }
  output <- gsub("  ","",output)
  output <- gsub("\n","",output)
  output <- paste0(output, ".")
  out <-datatable(data.frame(output), colnames=c("내용요약"),
                  #caption = htmltools::tags$caption(
                  #  style = 'caption-side: bottom; text-align: center;',
                  #  htmltools::em('TEXT Rank 알고리즘을 사용한 사건요약')),
                  options = list(pageLength = 10,
                                 deferRender = TRUE,
                                 scrollY = 300,
                                 scroller = TRUE,
                                 searchHighlight = TRUE), rownames=F)
  
  dir <- paste0("C:\\Users\\hp1\\Documents\\tobigs_project_file\\",n)
  dir.create(dir)
  setwd(dir)
  write(as.character(event$hlod.explanation[n]), "short.txt")
  saveWidget(out, 'summary.html')
  visSave(network, "network.html")
  
  cat("\n",paste0("################## ",n,"번째 사건까지 저장되었습니다."," ##################"))
}

##################################################################################################