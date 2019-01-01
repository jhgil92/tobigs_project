library(KoNLP)
library(NLP)
library(tm)
library(openNLP)
library(graph)
# --- FUNCTIONS
tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}
###### illustrate usage of tagPOS
str <- "this is a the first sentence."
tagged_str <-  tagPOS(str)
tagged_str

## $POStagged
## [1] "this/DT is/VBZ a/DT the/DT first/JJ sentence/NN ./."
## 
## $POStags
## [1] "DT"  "VBZ" "DT"  "DT"  "JJ"  "NN"  "."
###### Other utility functions
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
        cat(i," ",words[i]," - ",paste(c(links),collapse=" "),"\n")
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

# --- MAIN CODE
doc <- c("Compatibility of systems of linear constraints over the set of natural numbers. 
         Criteria of compatibility of a system of linear Diophantine equations, strict inequations, 
         and nonstrict inequations are considered. 
         Upper bounds for components of a minimal set of solutions and algorithms of construction of 
         minimal generating sets of solutions for all types of systems are given. 
         These criteria and the corresponding algorithms for constructing a minimal supporting set of solutions 
         can be used in solving all the considered  types systems and systems of mixed types.")

corp <- Corpus(VectorSource(doc))
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, tolower)
words_with_punctuation <- SplitText(as.character(corp[[1]]))
corp <- tm_map(corp, removePunctuation)

#--- GRAPH CONSTRUCTION
words <- SplitText(as.character(corp[[1]]))
tagged_text <- tagPOS(corp[[1]])
tagged_words <- SplitText(as.character(tagged_text))
tagged_words <- c(SelectTaggedWords(tagged_words,"/NN"),SelectTaggedWords(tagged_words,"/JJ"))  # keep only NN & JJ tagged words 
tagged_words <- RemoveTags(tagged_words)
selected_words <- unique(tagged_words)

test <- c("일본의 식민지 지배에 저항해 1919년 3월 1일을 기점으로 전국에서 일어난 항일독립운동. 3월 1일 서울을 시작으로 도시에서 농촌 등지로 전파되며 전국적인 규모로 확산되었고, 갈수록 참여하는 인원과 계층이 늘어나면서 운동의 양상도 비폭력 시위에서 폭력투쟁으로 변모했다. 이 운동은 국외로도 확산되어 중국의 만주와 일본의 도쿄·오사카, 미국의 필라델피아 등에서도 독립시위가 벌어졌다. 독립선언문을 작성·서명하고 운동을 계획한 이들이 각 종교와 단체를 연합한 민족대표 33인이라면, 실제 전국적인 운동을 주도한 이들은 무명의 학생과 청년들이었다. 일제는 이를 무력으로 강경 진압해 통계상으로만 3개월 동안 7,509명이 사망했으며, 15,961명이 상해를 입었다.")
test <- c("‘정운호 게이트’의 파장이 커지고 있다. 정운호 네이처리퍼블릭 대표의 비리 사건이 전관 변호사와 현직 판·검사의 유착 여부, 정·관계 로비 의혹으로 비화되는 가운데 부장급 현직 검사가 정 대표에게 1억원을 받은 정황이 포착됐다. 검찰은 정 대표의 브로커로부터 1억원을 이 검사에게 전달했다는 진술을 확보하고 해당 검사를 소환키로 했다. 정운호 게이트 이후 현직 검사의 비리가 파악된 것은 처음이다. 검찰은 정 대표가 2010년 네이처리퍼블릭이 인수한 회사의 서울 지하철 상가 운영업체 선정 과정에 대한 감사원 감사를 무마하려는 의도로 감사원 관계자와 동문인 이 검사에게 돈을 준 것으로 보고 있다. 2012년 11월 구속된 김광준 검사 이후 3년7개월 만에 뇌물검사 사건이 재발한 것이다. 

          한마디로 충격이다. 정의의 마지막 보루임을 자임하는 검찰의 뇌물 추문이 사실이라면 용서받을 수 없는 중범죄다. 김광준 사건이 잊혀질 만하자 또 이런 일이 생긴 데 대해 검찰도 곤혹스러워하고 있다. 그나마 검찰이 혐의를 신속히 확인해 후속 조치를 취하는 등 기민하게 대처해 다행이다. 앞으로도 만에 하나 제 식구를 감싸겠다는 조직보호 논리로 접근하다가는 여론의 뭇매를 맞을 수밖에 없다는 것을 명심해야 한다. 철저하게 수사해 엄단하는 한편 재발하지 않도록 근본적인 방안을 내놔야 한다. 
          
          부장급 현직 검사의 뇌물의혹 사건으로 정운호 게이트 일단이 드러나고 있다. 정 대표가 사업 성공을 위해서는 현직 법조인에게도 금품 살포를 서슴지 않았다는 게 사실상 확인된 것이다. 그러나 이 사안은 정운호 게이트의 본질은 아니다. 핵심에서 불거진 곁가지일 뿐이다. 검찰의 칼끝이 겨눠야 할 곳은 전관예우를 받은 부장판사 및 검사장 출신 변호사와 현관과의 고질적인 유착 여부, 정·관계를 상대로 한 광범위한 로비 의혹이다. 정운호 사건을 보면 납득할 수 없는 일이 하나 둘이 아니다. 전관과 현관 법조인의 결탁 없이는 도저히 이해되지 않는 정황이 적지 않다. 
          
          검찰은 뇌물검사 사건을 정운호 게이트의 몸통을 파헤치는 계기로 삼아야 한다. 의혹을 제대로 밝혀내지 못하면 특별검사가 나설수밖에 없다는 사실을 명심하고 비상한 각오로 수사에 임해야 겠다.
          ")

test <- c("설마설마하던 영국의 유럽연합(EU) 탈퇴, 브렉시트가 현실화하면서 전 세계가 충격에 빠졌다. 24일 발표된 국민투표 결과 영국은 51.9%의 찬성으로 EU 탈퇴를 택했다. EU 주요 4개국 중 하나이자, 세계 5대 기축통화국인 영국의 선택을 두고 ‘역사적 이혼’이란 평가가 나온다. EU의 장래를 두고 불안감이 엄습하고 있으며 세계 경제는 휘청거리고 있다. 책임있는 국제사회의 주요국들에선 브렉시트를 계기로 고립주의가 확산될 가능성이 높다. 세계인들은 영국인이 브렉시트를 거부함으로써 성장 둔화와 이민자 문제로 시험대에 오른 EU가 다시 전열을 가다듬고 새 출발하길 기대했지만 물거품이 됐다.

          브렉시트 국민투표 과정에서 나타난 혼란상은 가장 오랜 민주주의의 역사를 지닌 영국에 걸맞지 않은 것이었다. 브렉시트가 가져올 경제적 실익에 대한 이성적 논의보다 이민자 문제 같은 감성적 이슈가 분위기를 지배했다. 브렉시트로 영국 국내총생산이 5% 감소하고, 파운드화 가치가 20% 하락할 수 있다는 경고는 무력했다. 대신 민족주의, 외국인 혐오증, 고립주의가 횡행했다. 타국 이민자들은 영국인들의 일자리와 복지를 갉아먹는 증오의 대상으로 변했고 브렉시트 찬성파들은 나치식 선전선동술도 불사했다. 보리스 존슨 전 런던시장이 EU를 히틀러와 나폴레옹의 유럽 정복 시도에 비유하며 유럽을 하나의 공동체로 묶으려는 시도는 시대착오적이라고 비판한 게 대표적이다. 극우단체들은 ‘영국이 먼저다’라며 민족주의를 부추기는 데 열중했다.
          
          이슬람 근본주의자들의 테러와 난민 문제로 유럽의 다문화주의가 위기를 맞고 있지만 이는 영국만의 문제가 아니라는 사실을 알아야 한다. 영국이 EU로부터 받는 수혜금액에서 기여금을 뺀 순기여금액이 마이너스지만 독일이나 프랑스보다는 그래도 사정이 낫다. 지난 10개 분기 성장률도 평균 2.5%로 유로존(유로화 사용 19개국) 평균 0.2%보다 높았다. 그럼에도 브렉시트 찬성파들은 내부 자체의 갈등요인을 외면하고 이민자에게 모든 책임을 돌렸다. EU로부터 받는 수혜는 무시한 채 EU에 내는 분담금을 국내로 돌리자는 무책임한 주장을 내세웠다.
          
          영국은 앞으로 EU에서 떨어져나와 각종 자유무역협정을 따로 체결해야 한다. 수출은 어려워지고 파운드화는 불안한 흐름을 보일 것이며 국제금융 중심지로서의 위상은 타격이 불가피하다. 친EU 성향의 스코틀랜드 독립 요구는 더욱 거세질지 모른다. 격랑 속으로 빠져들 영국도 우려스럽지만 하나의 유럽 구상도 타격을 받을 수밖에 없다는 점에 브렉시트의 심각성이 있다. 영국은 브렉시트를 통해 1973년 EU 전신인 유럽공동체(EC)에 가입한 후 43년 만에 유럽 통합의 흐름에서 스스로 벗어났다. 28개 EU 회원국 중 첫번째 탈퇴국이다. 2차 세계대전 후 유럽인들은 함께 잘사는 공동체를 향한 비전을 품어왔다. 1952년 유럽석탄철강공동체(ECSC)를 시작으로 유럽경제공동체(EEC), 유럽공동체를 거쳐 1993년 마스트리히트 조약이 발효하면서 EU가 출범했다. 
          
          인구 5억명, 전 세계 국내총생산의 23%를 차지하는 EU의 구심력이 이제는 브렉시트로 흔들리면서 시험대에 놓였다. 이미 프랑스, 네덜란드 등에선 탈퇴를 요구하는 극우파들의 목소리가 커지고 있으며 정치·군사적 불안정성도 두드러질 것으로 보인다. 영국이 국제사회의 책임있는 구성원으로서 의무를 내팽개쳤다는 비난을 면키 어려운 이유다. 세계화를 가장 먼저 경험했고 자유무역을 통해 가장 많은 이득을 얻은 영국이 EU 탈퇴를 택한 것은 아이러니다.
          
          브렉시트를 두고 국제사회가 한탄과 아쉬움, 비판의 목소리를 제기할 수 있어도 영국인들의 선택에 깔린 원인을 직시하지 않으면 안된다. 바로 양극화를 해소할 수 있는 정치사회 시스템이 작동하지 않을 경우 시민들은 기성 정치체제에 등을 돌린다는 사실이다. 이민자에게 일자리를 뺏긴다고 여기는 저소득층이 브렉시트 찬성에 표를 던진 것으로 알려지고 있다. 양극화는 지역 간, 계층 간, 세대 간 간극을 넓히고 공동체를 위협하는 요인임을 브렉시트 투표 결과가 보여줬다. 영국은 자국의 다음 세대와 유럽, 세계에 미칠 파급력을 고려할 때 브렉시트를 선택하지 말았어야 했다. 그러나 이미 물은 엎질러졌다. 파장을 줄이기 위한 대책 마련에 영국은 책임감 있는 자세로 주변국들과 머리를 맞대야 할 것이다.")

test <- c("정부의 ‘맞춤형 보육’ 시행에 항의하는 일부 어린이집이 어제 집단행동을 벌였다. 자율 등원 방식의 집단행동이어서 어린이집에 오는 아이를 막는 사례는 없었다. 우려했던 큰 혼란을 피한 것은 다행이다. 그러나 당장 아이 맡길 곳이 마땅치 않았던 부모는 혹시라도 어린이집이 문을 닫지 않을까 마음을 졸일 수밖에 없었다. 아이를 마음 놓고 키우기 어려운 한국 보육의 현실을 적나라하게 보여줬다. 세계 최저 출산율 국가임에도 저출산을 막기는커녕 아이 낳기 힘든 환경을 방치하는 듯하다. 출산 장려 정책의 하나인 무상보육은 정부가 오히려 지원을 축소하고 있다. 
          
          통계청이 어제 발표한 인구동향을 보면 올해 들어 4월까지 출생아 수는 14만7900명으로 지난해 같은 기간보다 5.2% 감소했다. 이런 추세라면 올해 출생아 수는 42만명에도 못 미쳐 역대 최저였던 2005년 기록(43만5031명)을 갈아치울 게 확실시된다. 2014년 1.205에서 지난해 1.240으로 소폭 상승세로 돌아섰던 출산율도 다시 하락할 수밖에 없다. 한국의 초저출산 추세는 내년부터 15~64세 생산가능인구 감소를 초래한다. 경제활동이 가능한 인구가 줄어들면 노동력이 부족해 공장 가동이 줄어든다. 소비와 투자도 위축돼 저성장에 빠지게 된다. 
          
          저출산이 국가적 재앙을 초래할 것으로 우려되지만 정부 대책은 거꾸로 가고 있다. 저출산의 주요 원인은 집값과 보육비 부담이 크기 때문이다. 시민 부담을 덜기 위해 정부는 2012년 무상보육 정책을 꺼내 들었다. 그러나 지금은 무상보육 구조조정이 필요하다며 맞춤형 보육을 밀어붙이고 있다. 맞춤반 보육료는 종일반의 80%이다. 어린이집은 보육료 삭감에 따라 운영난이 심해지고 서비스 질도 떨어진다고 반대한다. 아이를 맞춤반에 보내야 하는 전업주부의 가사노동을 인정하지 않고, 여성의 사회진출을 가로막는다는 지적도 나왔다. 
          
          어린이집과 부모 양측 모두가 반대하고, 탁상행정이라는 비판을 받고 있는 획일적인 현행 맞춤형 보육은 시행을 미루고 정밀하게 보완책을 마련해야 한다. 보육료 삭감 영향을 받는 어린이집, 아이를 맡기는 부모와의 소통과 협의를 통해 개선방안을 찾아야 할 것이다. 누구에게도 혜택이 돌아가지 않는다면 정책의 가치는 없다. 정부가 보육예산 절감에만 매달리느라 저출산을 부추기는, 소탐대실하는 우를 범하지 않기를 바란다.")

test <- c("3ㆍ1운동은 수개월 동안 지속되었으며 도시 등 교통이 발달한 곳을 중심으로 시작되어 농촌 등지로 전파되며 전국적인 규모로 확산되었다. 그리고 갈수록 참여하는 인원과 계층이 늘어나면서 운동의 양상도 비폭력 시위에서 폭력투쟁으로 발전하였다. 국외로도 확산되어 만주, 연해주, 도쿄, 오사카, 필라델피아 등에서도 독립시위가 벌어졌다.
          
          3ㆍ1운동의 전개 과정은 크게 3단계로 구분된다. 1단계(점화기)에서는 서울을 비롯해 평양ㆍ진남포ㆍ안주ㆍ의주ㆍ선천ㆍ원산 등의 주요 도시에서 독립선언서가 배포되어 운동이 시작되었다. 이 시기에는 비폭력 투쟁을 특징으로 했으며, 학생들이 주도적인 역할을 하였다. 3월 10일을 전후로 한 2단계(도시확산기)에 운동은 전국의 주요 도시들로 확산되었으며, 상인과 노동자들도 철시와 파업으로 적극적으로 참여했다. 3월 중순 이후의 3단계(농촌확산기)에는 도시뿐 아니라 농촌에서도 시위가 일상화하였다. 농민 등이 적극적으로 참여하면서 시위의 규모도 커졌으며, 시위의 양상도 몽둥이와 죽창 등으로 무장하여 면사무소와 헌병 주재소 등을 습격하는 폭력투쟁으로 발전하였다. 특히 3월 하순에서 4월 상순까지의 시기에 전체 시위의 60% 이상이 일어날 정도로 운동은 최고조에 이르렀는데, 그 가운데 절반 정도가 폭력투쟁으로 나타났다.
          
          3월 1일에 미리 계획했던 대로 서울과 평양ㆍ의주ㆍ선천ㆍ안주ㆍ원산ㆍ진남포 등 6개 도시에서 동시에 독립만세운동이 시작되었다. 하지만 민족대표들은 독립선언식의 거행장소를 군중들이 모여 있던 탑골공원에서 서울 인사동의 태화관(泰和館)으로 일방적으로 변경하였다. 민족대표 33명 가운데 29명은 3월 1일 오후 2시 태화관에 모여 세브란스 의학전문학교 학생인 서영환(徐永煥)을 통해 독립통고서를 조선총독부에 전달했다. 그리고 오후 3시 한용운(韓龍雲)이 독립선언서를 낭독한 뒤에 일본 경찰에 통고하여 스스로 체포되었다. 탑골공원에 모여 있던 학생들은 장소 변경에 당황하여 강기덕 등을 민족대표들에게 보내 항의하기도 했으나, 2시 30분 무렵에 따로 독립선언서를 낭독하고 두 갈래로 나뉘어 종로ㆍ서울역ㆍ정동ㆍ이화학당ㆍ서대문 등을 행진하며 시위를 벌였다.
          
          3월 2일에는 함흥ㆍ수안ㆍ황주ㆍ중화ㆍ강서ㆍ대동ㆍ해주ㆍ개성 등 천도교와 기독교의 조직력이 강한 평안도ㆍ함경도ㆍ황해도의 주요도시들로 시위가 확산되었다. 3월 3일에는 고종의 장례식을 보기 위해 많은 사람들이 서울로 모였고, 이들 가운데 많은 수가 시위운동에 참가했다. 서울의 학생들은 원래의 계획대로 3월 5일 남대문역 광장에서 만세시위를 벌였고, 평양과 광주 등의 학생들도 결사대를 조직해 이에 참여했다. 이 날의 시위는 고종의 장례식을 참관하고 지방으로 돌아가는 참배객들에게 운동의 지속성을 전파하여 3월 중순 이후 각 지방으로 시위운동이 확산되는 데 큰 구실을 하였다.
          
          3월 10일 이후에는 시위가 경상도ㆍ전라도ㆍ강원도ㆍ충청도 등 중남부 지방으로 확대되어 전국적 규모로 확산되었는데, 이 과정에는 교사와 학생 등 지방 사회의 지식인들이 중요한 구실을 했다. 이들은 선언서 등의 각종 유인물과 시위 경험을 각 지역에 전파했으며, 비밀결사와 결사대를 조직해 시위를 조직하고 주도하였다. 청년과 학생들이 주도한 비밀결사는 전단과 격문 등을 제작ㆍ배포하여 투쟁열기를 높였는데, 일부 지역에서는 지방신문을 만들어 민족의 총궐기와 결사항쟁을 촉구하기도 했다. 3ㆍ1운동 당시에 발간되었던 신문은 <조선독립신문>, <노동회보>, <반도의 목탁>, <충북자유보>, <혁신공보>, <각성호외보>, <광주신문>, <강화독립회보> 등 30여 종에 이르며, 이 가운데 <조선독립신문>은 27호까지 만들어지기도 했다.
          
          3ㆍ1운동의 전국적 확산에 큰 역할을 했던 청년과 학생들은 독립청원이라는 대외의존적인 태도를 지녔던 민족대표들과는 달리 민족의 주체역량으로 독립을 쟁취할 것을 주장했다. 이들의 노력으로 운동은 3월 중순 이후 농촌 지역으로 확산되었으며, 노동자ㆍ농민ㆍ중소상공인 등 각계각층의 민중들이 적극적으로 참여하는 민중운동으로 발전되었다.
          
          서울에서는 3월 5일에 앞장섰던 학생들이 대거 검거되면서 일시적으로 소강상태에 빠졌으나, 3월 4일에 시작된 평양과 선천의 철시투쟁에 이어 3월 9일부터 서울 시내의 주요 상점도 철시를 하면서 상인들도 항일투쟁에 나섰다. 3월 20일 무렵부터는 노동자들의 궐기를 호소하는 <노동회보>가 배포되었고, 3월 22일에는 남대문 앞에서 노동자대회가 개최되었다. 그 날부터 시내 곳곳에서 야간시위가 계속되었으며, 3월 26일에는 경성철도와 전차 노동자들도 파업을 벌였다. 이러한 노동자의 항일시위는 고양ㆍ부천ㆍ시흥ㆍ김포 등 주변 농촌 지역의 시위를 촉발시켰다.
          
          농민 시위는 주로 장날에 일어났는데, 시위 주동자들은 각 마을로 통문을 돌리거나 전단을 살포하여 미리 시위 계획을 알렸다. 장을 돌아다니는 행상들은 각지에서 벌어지는 시위의 경험을 전하는 구실을 하기도 했다. 과거 의병투쟁이 활발했던 지역에서는 산상봉화시위나 횃불시위 등을 벌이기도 했으며, 떼를 지어 며칠씩 마을들을 돌아다니며 시위에 참가하는 ‘만세꾼’이 등장하기도 했다.
          
          운동이 발전될수록 투쟁 목표가 구체화되고 조직화되었으며, 비폭력적인 만세시위운동에서 계획적이고 공세적인 폭력투쟁으로 진전되는 경향이 두드러졌다. 폭력투쟁은 일제의 탄압에 대한 방어적인 대응으로 나타난 것도 있었지만, 일제의 권력기관에 대해 계획적이고 공세적으로 나타나는 경우도 있었다. 일본 헌병의 총격 등으로 시위가 강제로 해산되면 군중들은 몽둥이와 죽창 등으로 무장하여 헌병 주재소와 면사무소, 우편소, 금융조합, 일본인과 친일인사의 집 등을 파괴하고 각종 수탈용 장부와 무기를 빼앗아 소각하는 등 무력을 행사했다. 그러나 처음부터 일제의 권력기관을 접수하려 나서는 경우도 있었는데, 강원도 통천에서는 총검으로 무장하고 시위를 벌이기도 했다. 특히 국외독립운동의 영향을 많이 받았던 평안도와 함경도 등 북부지방에서 이러한 경향이 강했는데, 간도ㆍ연해주 지역의 독립운동세력은 3ㆍ1운동 당시 국내진공계획을 세우기도 했다.
          
          일제는 3ㆍ1운동을 무력으로 무자비하게 진압했는데, 화성 제암리ㆍ천안 아우내ㆍ정주 곽산ㆍ남원 광한루ㆍ익산 이리 등 전국 각지에서 시위대에 총격을 가하는 등 학살을 저질렀다. 그리고 시위자들을 체포하여 가혹한 고문을 서슴지 않았다. 당시 일제의 통계에 따르면 3ㆍ1운동 이후 3개월 동안 시위진압과정에서 7,509명이 사망했으며, 15,961명이 상해를 입었다. 46,948명이 구금되었고, 교회 47개소, 학교 2개교, 민가 715채가 소각되었다.
          [네이버 지식백과] 3·1운동 [三一運動] (두산백과)
          
          ")

words <- gsub("[+[가-힣]+\\s+[가-힣]+]" ,"", test)
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


selected_words <- unique(ko.words(test))

text_graph <- ConstructTextGraph(2)  # co-occurrence of window size 2

words_with_punctuation <- SplitText(as.character(test))

d <- 0.85                               # damping factor
threshold <- 1e-4               # convergence threshold 
text_nodes <- nodes(text_graph)
nodes_num <- length(text_nodes)
nodes_rank <- matrix(1,nodes_num,2)

k <- 0                                  # iterations
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
# --- POST-PROCESSING
keywords_num <- round(nodes_num/3) # a third of the number of vertices in the graph.
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
keywords_list <- unique(keywords_list)

