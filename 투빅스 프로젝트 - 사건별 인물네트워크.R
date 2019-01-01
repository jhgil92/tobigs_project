library(KoNLP)
library(stringr)
library(tm)
library(readr)

name <- read.csv("C:\\Users\\hp1\\Desktop\\name.csv")
names <- as.character(unique(name[,2]))

#for(i in 1:length(names)){
#  mergeUserDic(data.frame(names[i], 'ncn'))
#}

event <- read.csv("C:\\Users\\hp1\\Desktop\\사건166.csv")

get_dic_naver <- function(word){
  out <- NULL
  v <- c("1","11")#c("1","11","21","31","41")
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

event$hlod.titleKor

n = 142

event1 <- get_dic_naver(event$hlod.titleKor[n])
event1 <- event1[str_detect(event1, "terms.naver")]
event1

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
  tmp <- paste(tmp, collapse = ".")
  tmp <- str_sub(tmp, start=str_locate(tmp, "window.onload")[1,2]+39)
  contents[[i]] <- tmp
  contents_split <- lapply(contents, function(x){str_split(x, "\\.")})
  contents_split <- lapply(contents_split, function(x){return(unlist(x)[nchar(unlist(x))>2])})
  cat("\n",i)
}

#contents[[1]]
#contents_split[[1]]
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

event1_names <- find_names(contents)
names(event1_names)
if(length(event1_names)>20){event1_names <- event1_names[1:20]}

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
net <- as.data.frame(apply(net,2, function(x)gsub("\\|","_",x)))
names(event1_names) <- gsub("\\|","_",names(event1_names))

net <- net[order(net[,1], net[,2]),]
rownames(net) <- 1:nrow(net)

nett <- unique(net)
tmp <- c(rownames(nett),nrow(net)+1)
tmp <- as.numeric(tmp)
count <- tmp[2:length(tmp)] - tmp[1:(length(tmp)-1)]
nett <- cbind(nett, count)
rownames(nett) <- 1:nrow(nett)

###################
library(igraph)
#library(ggplot2)


myedges <- character(2*nrow(nett))
myedges[(1:length(myedges) %% 2) == 1] <- as.character(nett$V1)
myedges[(1:length(myedges) %% 2) != 1] <- as.character(nett$V2)

g <- graph(edges = myedges)

normalize <- function(x){return((x - min(x)) / (max(x) - min(x)))}
eg.col <- NULL
for(i in 1:length(nett$count)){
  tmp <- normalize(nett$count)+0.3
  eg.col[i] <- adjustcolor( "darkgray", alpha.f = tmp[i])
}

check <- NULL
for(i in 1:length(names(event1_names[V(g)$name]))){
  check[i] <- which(names(event1_names[V(g)$name])[i]==name$x)[1]
}
check <- name[check,3]
tmp <- normalize(as.vector(event1_names[V(g)$name]))+0.4

ver.col <- NULL
for(i in 1:length(event1_names)){
  if(check[i]=="한"){ver.col[i] <- adjustcolor( "royalblue3", alpha.f = tmp[i])}
  if(check[i]=="일"){ver.col[i] <- adjustcolor( "red", alpha.f = tmp[i])}
  if(check[i]=="친일"){ver.col[i] <- adjustcolor( "violetred1", alpha.f = tmp[i])}
  if(check[i]=="기타"){ ver.col[i] <- adjustcolor( "palegreen", alpha.f = tmp[i])}
}

plot(g, edge.arrow.size = 0.001,
     main = "CHARACTER NETWORK",
     edge.color =  eg.col,
     edge.width =  (normalize(nett$count)+1)^2.5,
     edge.curved = F,
     edge.label.font = 3,
     vertex.color = ver.col,
     vertex.frame.color = ver.col,
     vertex.label.color = "black",
     vertex.label.family = 'sans',
     vertex.label.cex = 1.2,
     vertex.size = (normalize(as.vector(event1_names[V(g)$name]))+0.1)*20+5,
     vertex.shape = "circle",
     layout = layout.fruchterman.reingold,
     asp=1)
