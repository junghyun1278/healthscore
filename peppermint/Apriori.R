library(data.table)
library(dplyr)
setwd("/state/partition1/home/")

####################### 데이터 불러오기 #########
for(i in 2010:2013){
  assign(paste0("jk", i), fread(paste0("./nhis2/01.jk/nhid_jk_", i,".txt")))
  assign(paste0("gj", i), fread(paste0("/state/partition1/home/yd0011/DataCampus/dat/nhid_gj_", i,".txt")))
  assign(paste0("gy", i), fread(paste0("./nhis2/02.T120/nhid_gy20_t1_", i,".txt")))
}

################################################

## 건강검진 데이터, 진료 데이터, 자격 데이터 병합
data2010 <- left_join(gj2010, gy2010, by="PERSON_ID")
data2010 <- left_join(data2010, jk2010, by="PERSON_ID")

data2011 <- left_join(gj2011, gy2011, by="PERSON_ID")
data2011 <- left_join(data2011, jk2011, by="PERSON_ID")

data2012 <- left_join(gj2012, gy2012, by="PERSON_ID")
data2012 <- left_join(data2012, jk2011, by="PERSON_ID")

data2013 <- left_join(gj2013, gy2013, by="PERSON_ID")
data2013 <- left_join(data2013, jk2013, by="PERSON_ID")


### 질병코드 저장
hepaSeq <- "K70"

diaSeq <- paste0("E", seq(10, 14, 1))

kidneySeq1 <- paste0("N", seq(17, 19, 1))
kidneySeq2 <- c("N990", "O084", "O904", "P960", "I120", "I129", "I131", "I132")


seq1 <- c(paste0("A", seq(0, 9, 1)), paste0("B", seq(0, 9, 1)))
seq2 <- c(paste0("C", seq(0, 9, 1)), paste0("D", seq(0, 4, 1)))
seq3 <- paste0("D", seq(5, 9, 1))
seq4 <- paste0("E", seq(0, 9, 1))
seq5 <- paste0("F", seq(0, 9, 1))
seq6 <- paste0("G", seq(0, 9, 1))
seq7 <- paste0("H", seq(0, 5, 1))
seq8 <- paste0("H", seq(6, 9, 1))
seq9 <- paste0("I", seq(0, 9, 1))
seq10 <- paste0("J", seq(0, 9, 1))
seq11 <- paste0("K", seq(0, 9, 1))
seq12 <- paste0("L", seq(0, 9, 1))
seq13 <- paste0("M", seq(0, 9, 1))
seq14 <- paste0("N", seq(0, 9, 1))
seq15 <- paste0("O", seq(0, 9, 1))
seq16 <- paste0("P", seq(0, 9, 1))
seq17 <- paste0("Q", seq(0, 9, 1))
seq18 <- paste0("R", seq(0, 9, 1))
seq19 <- paste0("U", seq(0, 9, 1))




### 질병 유무
for (i in 2010:2013) {
  assign("data", get(paste0("data", i)))
  
  data$BMI <- (data$WEIGHT/(data$HEIGHT*data$HEIGHT)*10000)
  
  data$IS_HEPA <- 0
  data$IS_HEPA <- ifelse(substr(data$MAIN_SICK, 1, 3) %in% hepaSeq |
                           substr(data$SUB_SICK, 1, 3) %in% hepaSeq , 1, 0)
  
  data$IS_OBES <- 0
  data$IS_OBES <- ifelse(data$BMI>= 25,1,0)
  
  data$IS_BP <- 0
  data$IS_BP <- ifelse(data$BP_HIGH >=140| data$BP_LWST >=90, 1,0)
  
  data$IS_LIP <- 0
  data$IS_LIP <-  ifelse(data$TOT_CHOLE>=230 | data$TRIGLYCERIDE>=200 | data$LDL_CHOLE>=150, 1, 0)
  
  data$IS_DIA <- 0
  data$IS_DIA <- ifelse(substr(data$MAIN_SICK, 1, 3) %in% diaSeq |
                          substr(data$SUB_SICK, 1, 3) %in% diaSeq, 1, 0)
  
  data$IS_KID <- 0
  data$IS_KID <- ifelse(substr(data$MAIN_SICK,1,3) %in% kidneySeq1 |
                          substr(data$SUB_SICK,1,3) %in% kidneySeq1 |
                          data$MAIN_SICK %in% kidneySeq2 |
                          data$SUB_SICK %in% kidneySeq2 , 1, 0) 
  
  assign("D", data.frame(PERSON_ID = data$PERSON_ID,
                         MAIN_SICK = data$MAIN_SICK,
                         SUB_SICK = data$SUB_SICK,
                         IS_BP = data$IS_BP,
                         IS_DIA = data$IS_DIA,
                         IS_OBES = data$IS_OBES,
                         IS_HEPA = data$IS_HEPA,
                         IS_LIP = data$IS_LIP,
                         IS_KID = data$IS_KID))
  
  for (j in 1:19) {
    res <- ifelse(substr(data$MAIN_SICK, 1, 2) %in% get(paste0("seq", j)) |
                    substr(data$SUB_SICK, 1, 2) %in% get(paste0("seq", j)), 1, 0)
    D <- cbind(D, res)
  }
  assign(paste0("D",i), D)
}

colnames(D2010)
names(D2010)[10:28] <- seq(1, 19, 1)
names(D2011)[10:28] <- seq(1, 19, 1)
names(D2012)[10:28] <- seq(1, 19, 1)
names(D2013)[10:28] <- seq(1, 19, 1)

D2010 %>%
  select(-c(MAIN_SICK, SUB_SICK)) %>%
  group_by(PERSON_ID) %>%
  mutate_each(funs(mysum = sum(.))) -> D1

D2011 %>%
  select(-c(MAIN_SICK, SUB_SICK)) %>%
  group_by(PERSON_ID) %>%
  mutate_each(funs(mysum = sum(.))) -> D2

D2012 %>%
  select(-c(MAIN_SICK, SUB_SICK)) %>%
  group_by(PERSON_ID) %>%
  mutate_each(funs(mysum = sum(.))) -> D3

D2013 %>%
  select(-c(MAIN_SICK, SUB_SICK)) %>%
  group_by(PERSON_ID) %>%
  mutate_each(funs(mysum = sum(.))) -> D4

D1 <- D1[,-c(2:26)]
D2 <- D2[,-c(2:26)]
D3 <- D3[,-c(2:26)]
D4 <- D4[,-c(2:26)]
DA <- rbind(D4, D3, D2, D1)

names(DA)[-1] <- c("IS_BP", "IS_DIA", "IS_OBES", "IS_HEPA", "IS_LIP", "IS_KID", seq(1,19,1)) 
DA <- DA[!duplicated(DA$PERSON_ID),]
DA <- DA[complete.cases(DA),]

DA[,2:26] <- sapply(DA[,2:26], function(x) ifelse(x>=1, 1, NA))
names(DA)[8:26] <- c("infectious", "neoplasms", "immune", "metabolic",
                     "mental", "nervous", "eye", "ears", "circulatory",
                     "respiratory", "digestive", "skin", "tissue", "genitourinary",
                     "pregnancy", "originating", "congenital", "abnormal",
                     "special")

library(arules)
library(arulesViz)
library(visNetwork)
library(igraph)
df <- as.data.frame(sapply(DA[,-1],as.factor))
df_trans <- as(df[,-c(1:6)],'transactions')
df_rule <- apriori(data = df_trans, parameter =
                     list(support = 0.05, confidence = 0.6, maxtime=30))

prune.dup.rules <- function(rules){
  rule.subset.matrix <- is.subset(rules, rules)
  rule.subset.matrix[lower.tri(rule.subset.matrix, diag=T)] <- NA
  dup.rules <- colSums(rule.subset.matrix, na.rm=T) >= 1
  pruned.rules <- rules[dup.rules]
  return(pruned.rules)
}
df_rule <- prune.dup.rules(df_rule)

subrules <- subset(df_rule,
                       items %in%
                         c("metabolic=1"))

ig <- plot(sort(subrules, by = "lift")[1:50], method = "graph", control = list(type="items"))
tf <- tempfile( )
saveAsGraph(subrules, file = tf, format = "dot" )
ig_df <- get.data.frame( ig, what = "both" )
vis_group <- c(4, ig_df$edges[ig_df$edges$from ==4,2])
visNetwork(
  nodes = data.frame(
    id = ig_df$vertices$name
    ,value = ig_df$vertices$lift # could change to lift or confidence
    ,title = ifelse(ig_df$vertices$label == "",ig_df$vertices$name, ig_df$vertices$label)
    ,group = ifelse(ig_df$vertices$name %in% vis_group, 1, 0)
    ,ig_df$vertices
    ,font.size = ifelse(ig_df$vertices$label == "metabolic=1", 35, 20)
    ,font.color = "black"
  )
  , edges = ig_df$edges
) %>%
  visEdges(  ig_df$edges, arrows = "to" ) %>%
  visOptions( highlightNearest = T )



