clust <- as.data.frame(matrix(0,nrow=398, ncol=166))
rownames(clust) <- names
colnames(clust) <- as.character(event$hlod.titleKor)

for(n in 153:166){
  # 크롤링할 URL주소
  event1 <- get_dic_naver(event$hlod.titleKor[n])
  event1 <- event1[str_detect(event1, "terms.naver")]
  # 관련 데이터 크롤링
  contents <- list()
  length(contents) <- 10
  for(j in 1:10){
    tmp <- read_lines(event1[j])
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
    contents[[j]] <- tmp
    #contents_split <- lapply(contents, function(x){str_split(x, "\\.")})
    #contents_split <- lapply(contents_split, function(x){return(unlist(x)[nchar(unlist(x))>2])})
    cat("\n",j)
  }
  event1_names <- find_names(contents)
  for(i in 1:length(event1_names)){
    clust[which(names(event1_names)[i] == names)[1],n] <- as.numeric(event1_names[i])
  }
  cat("\n","########################",n)
}

clust1 <- clust[rowSums(clust)>5,colSums(clust)!=0]
write.csv(clust1, "인물클러스트.csv", row.names=F)

clust1 <- read.csv("인물클러스트.csv")

km <- kmeans(clust1, 10)
table(km$cluster)
rownames(clust1)[km$cluster==10]