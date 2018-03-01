setwd("//wfs1/users$/shuyi/Desktop/628module2/data")
train1=readRDS("re_train1.rds")
# train2=readRDS("re_train2.rds")
# train3=readRDS("re_train3.rds")
neg=c("no","not","isn't","isnot","wasn't","wasnot","aren't","arenot","weren't","werenot","can't","cannot","don't","doesn't","didn't","donot","doesnot","didnot")
pos=c("good","nice","right","fresh","delicious","like","love","want")
library("tm")
stoplist=stopwords("english")
stoplist=stoplist[-which(stoplist %in% tolower(c(neg,pos)))]
stoplist=c(stoplist,"us","just","order","ordered","go","went","get","got","going","even","people","can","restaurant","will","would","also","too","one",as.character(1:100),"one","two","three")
words_freq=function(s){
  t1=subset(train1,train1[['stars']]==s)
  t1words=t1[['words']]
  words1=c()
  for(i in 1:length(t1words)){
    tr1=t1words[[i]]
    tr1=tr1[-which(tolower(tr1) %in% tolower(stoplist))]
    if(length(tr1)>=2){
      for(k in 2:length(tr1)){
        if(tolower(tr1[k]) %in% tolower(pos)){
          if(any(tolower(tr1[1:(k-1)]) %in% tolower(neg))){
            tr1[k]=paste0("NOT",tr1[k])
          }
        }
      }
    }
    words1=c(words1,tr1)
  }
  twords1=table(words1)
  fwords1=data.frame(name=names(twords1),freq=as.integer(twords1))
  attach(fwords1)
  swords1=fwords1[order(-freq),]
  swords1=head(swords1,500)
  write.csv(swords1,paste0('freq',as.character(s)),row.names=F,col.names =F)
  detach(fwords1)
}


for(s in 1:5){
  words_freq(s)
}


