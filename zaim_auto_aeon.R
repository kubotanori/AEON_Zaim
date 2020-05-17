install.packages("tidyverse")

library(tidyverse)
rm(list=ls())
df0.0 <- read.delim(file.choose(), encoding="UTF-8", header=FALSE, row.names=NULL)
df0<-df0.0
df0$cat<-NA
df0$cat<-df0$V1 %>%
  substr(2,nchar(str_extract(df0$V1,"\\[.+\\]"))-1)
df0$value<-NA
df0$value<-df0$V1 %>%
  substr(nchar(str_extract(df0$V1,"\\[.+\\]"))+1,nchar(df0$V1))
find_meisai<-max(which(df0$cat=="ご注文内容"))
firstNA <- min(which(is.na(df0$cat)))
df1<-df0[(which(df0$cat=="ご注文内容")+1):(min(which(is.na(df0$cat)))-1),]
df1$id<-ceiling((1:length(df1$cat))/5)
df1.5<-df0[(min(which(is.na(df0$cat)))):length(df0$V1),]
for(i in 1:length(df1.5$V1)){
if(is.na(df1.5$cat[i])){
  df1.5$cat[i]<-substr(str_extract(df1.5$V1[i],"小計\\（税抜\\）\\: \\d+\\,*\\d*円"),1,6)
  df1.5$value[i]<-as.numeric(sub(",","",substr(str_extract(df1.5$V1[i],"小計\\（税抜\\）\\: \\d+\\,*\\d*円"),9,nchar(str_extract(df1.5$V1[i],"小計\\（税抜\\）\\: \\d+\\,*\\d*円"))-1)))
  }else {df1.5$cat[i]<-df1.5$cat[i]}
}
for(i in 1:length(df1.5$V1)){
  if(is.na(df1.5$cat[i])){
    df1.5$cat[i]<-substr(str_extract(df1.5$V1[i],"送料 \\（税抜\\）\\: \\d+\\,*\\d*円"),1,7)
    df1.5$value[i]<-as.numeric(sub(",","",substr(str_extract(df1.5$V1[i],"送料 \\（税抜\\）\\: \\d+\\,*\\d*円"),10,nchar(str_extract(df1.5$V1[i],"送料 \\（税抜\\）\\: \\d+\\,*\\d*円"))-1)))
  }else {df1.5$cat[i]<-df1.5$cat[i]}
}
for(i in 1:length(df1.5$V1)){
  if(is.na(df1.5$cat[i])){
    df1.5$cat[i]<-substr(str_extract(df1.5$V1[i],"手数料\\（税抜\\）\\: \\d+\\,*\\d*円"),1,7)
    df1.5$value[i]<-as.numeric(sub(",","",substr(str_extract(df1.5$V1[i],"手数料\\（税抜\\）\\: \\d+\\,*\\d*円"),10,nchar(str_extract(df1.5$V1[i],"手数料\\（税抜\\）\\: \\d+\\,*\\d*円"))-1)))
  }else {df1.5$cat[i]<-df1.5$cat[i]}
}
for(i in 1:length(df1.5$V1)){
  if(is.na(df1.5$cat[i])){
    df1.5$cat[i]<-substr(str_extract(df1.5$V1[i],"手数料\\（税抜\\）\\: \\d+\\,*\\d*円"),1,7)
    df1.5$value[i]<-as.numeric(sub(",","",substr(str_extract(df1.5$V1[i],"手数料\\（税抜\\）\\: \\d+\\,*\\d*円"),10,nchar(str_extract(df1.5$V1[i],"手数料\\（税抜\\）\\: \\d+\\,*\\d*円"))-1)))
  }else {df1.5$cat[i]<-df1.5$cat[i]}
}
for(i in 1:length(df1.5$V1)){
  if(is.na(df1.5$cat[i])){
    df1.5$cat[i]<-substr(str_extract(df1.5$V1[i],"外税8\\.0%：\\d+\\,*\\d*円"),1,6)
    df1.5$value[i]<-as.numeric(sub(",","",substr(str_extract(df1.5$V1[i],"外税8\\.0%：\\d+\\,*\\d*円"),8,nchar(str_extract(df1.5$V1[i],"外税8\\.0%：\\d+\\,*\\d*円"))-1)))
  }else {df1.5$cat[i]<-df1.5$cat[i]}
}
for(i in 1:length(df1.5$V1)){
  if(is.na(df1.5$cat[i])){
    df1.5$cat[i]<-substr(str_extract(df1.5$V1[i],"外税10\\.0% ：\\d+\\,*\\d*円"),1,7)
    df1.5$value[i]<-as.numeric(sub(",","",substr(str_extract(df1.5$V1[i],"外税10\\.0% ：\\d+\\,*\\d*円"),10,nchar(str_extract(df1.5$V1[i],"外税10\\.0% ：\\d+\\,*\\d*円"))-1)))
  }else {df1.5$cat[i]<-df1.5$cat[i]}
}
for(i in 1:length(df1.5$V1)){
  if(is.na(df1.5$cat[i])){
    df1.5$cat[i]<-substr(str_extract(df1.5$V1[i],"お支払い総計：\\d+\\,*\\d*円"),1,7)
    df1.5$value[i]<-as.numeric(sub(",","",substr(str_extract(df1.5$V1[i],"お支払い総計：\\d+\\,*\\d*円"),8,nchar(str_extract(df1.5$V1[i],"お支払い総計：\\d+\\,*\\d*円"))-1)))
  }else {df1.5$cat[i]<-df1.5$cat[i]}
}
df1.6<-df1.5 %>%
  filter(!is.na(df1.5$cat))%>%
  select(c(cat,value)) %>%
  filter(cat=="送料 （税抜）"|cat=="手数料（税抜）"|cat=="外税8.0%"|cat=="外税10.0%")%>%
  mutate(id=NA)%>%
  mutate(memo=NA)
df1.7 <- df1.6[,c(3,1,2,4)]
colnames(df1.7)<-c("id","商品名","price","memo")
df1.7$price<-as.numeric(df1.7$price)

df2<-df1 %>%
  select(-V1) %>%
  group_by(id) %>%
  pivot_wider(names_from = cat, values_from = value)
df2$price<-NA
df2$price<-df2$`価格/数量` %>%
  substr(nchar(df2$`価格/数量`)-nchar(str_extract(df2$`価格/数量`,"小計：\\d+円"))+4,nchar(df2$`価格/数量`)-1) %>%
  as.numeric()
df2$memo<-NA
df2$memo<-df2$`価格/数量` %>%
  substr(1,nchar(df2$`価格/数量`)-nchar(str_extract(df2$`価格/数量`,"小計：\\d+円"))-2)
df3<-df2 %>%
  select(c(`商品名`,price,memo))%>%
  rbind(df1.7)
date<- str_extract(df0$value[which(df0$cat=="お届け日時")],".+年.+月.+日") %>%
  gsub(" ", "", ., fixed = TRUE) %>%
  gsub("年","/",., fixed = TRUE) %>%
  gsub("月","/",., fixed = TRUE) %>%
  gsub("日","",., fixed = TRUE)  %>%
  as.Date(format="%Y/%m/%d")
df3$`日付`<-NA
df3$`日付`<-date
df3$shop<-NA
df3$shop<-"イオン 金沢八景店"
df3$`方法`<-NA
df3$`方法`<-"payment"
df3.1<-ungroup(df3)
df4<-NA
df4<-df3.1 %>%
  select(c(`日付`,`方法`)) %>%
  mutate(`カテゴリ`=NA) %>%
  mutate(`カテゴリの内訳`=NA) %>%
  mutate(`支払元`=NA) %>%
  mutate(`入金先`=NA) %>%
  mutate(`支払元`=NA)%>% 
  mutate(`品目`=NA) %>%
  mutate(`メモ`=NA) %>%
  mutate(`お店`=NA) %>%
  mutate(`通貨`=NA) %>%
  mutate(`収入`=NA) %>%
  mutate(`支出`=NA) %>%
  mutate(`振替`=NA) %>%
  mutate(`残高調整`=NA) %>%
  mutate(`通貨変換前の金額`=NA) %>%
  mutate(`集計の設定`=NA)

df4$`品目`<-df3$`商品名`
df4$`メモ`<-df3$memo
df4$`お店`<-df3$shop
df4$`支出`=df3$price

write.csv(df4, "upload_for_zaim.csv", row.names = FALSE)

  
  
  
