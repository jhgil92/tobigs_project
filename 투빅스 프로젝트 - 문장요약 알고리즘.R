# LIBRARY
#source("https://bioconductor.org/biocLite.R")
#biocLite("graph")
library(KoNLP)
library(NLP)
library(tm)
library(openNLP)
library(graph)
library(DT)

# FUNCTION DEFINITION
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


#####################################

n <- 10
corp <- Corpus(VectorSource(doc))
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, removePunctuation)
words <- pre_words(corp[[1]])
selected_words <- unique(ko.words(doc))
text_graph <- ConstructTextGraph(2)
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
while(length(ranking)!=n){
  rank <- NULL
  for(j in 1:length(sentence)){
    rank[j]  <- (sum(unique(top[[i]]) %in% unique(sentence[[j]])) / length(unique(top[[i]])) )
  }
  ranking <- c(ranking,which.max(rank))
  ranking <- unique(sort(ranking))
  i <- i+1
}
output <- NULL
for(i in 1:n){
  output[[i]] <- origin[[1]][ranking[i]]
}
output <- gsub("  ","",output)
output <- gsub("\n","",output)
output <- paste0(output, ".")
output
out <-datatable(data.frame(output), colnames=c("내용요약"),
                #caption = htmltools::tags$caption(
                #  style = 'caption-side: bottom; text-align: center;',
                #  htmltools::em('TEXT Rank 알고리즘을 사용한 사건요약')),
                options = list(pageLength = 10,
                               deferRender = TRUE,
                               scrollY = 300,
                               scroller = TRUE,
                               searchHighlight = TRUE), rownames=F)
out
saveWidget(out, 'out.html')
