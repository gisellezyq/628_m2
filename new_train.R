setwd("//wfs1/users$/shuyi/Desktop/628module2/data")
library("syuzhet")
# to make word cloud
library(wordcloud) 
library(scales)
library(text2vec)
train1=readRDS("re_train1.rds")
# train2=readRDS("re_train2.rds")
# train3=readRDS("re_train3.rds")
neg=c("no","not","isn't","isnot","wasn't","wasnot","aren't","arenot","weren't","werenot","can't",
      "cannot","don't","doesn't","didn't","donot","doesnot","didnot","aviod","didin't","didnot",
      "haven't","hasn't","havenot","hasnot","lack","none","nothing","without","miss","missing","minus",
      "lost","couldn't","wouldn't","won't","willnot","only","couldnot","lose","if","should","would","could",
      "need","far","rather","used","neither","never","except","lacking")

#so far from; rather than; would be; should be
like=c("good","nice","right","fresh","delicious","like","love","want","best","great",
      "finishing","finish","recommended","recommend","worth","worthy","worthwhile",
      "proper","properly","cleaning","clean","wash","washing","taste","tasty","spectacular",
      "special","creative","success","successful","deal","enjoy","enjoying","enjoyed",
      "impressed","impressive","impress","impressing","ok","okay","cute","loyal","cool","replacement",
      "back","come","coming","go","going","try","repeat","repeat","here","return","again","better",
      "favorite","cheap","appreciate","appreciated","appreciating","acknowledging","fabulous")
hate=c("bad","worst","expensive","disgusting","crappy","flavorless","salty","elsewhere","overpriced",
       "low","poor","overcharged","bland","greasy","nasty","underwhelmed","disppoint","disappointed",
       "disappointing","blame","hate","tired","forget")

#back=c("back","come","coming","go","going","try","repeat","repeat","here")
stoplist=read.table("stoplist.txt",header = T)
stoplist=data.frame(stoplist)$x

words_freq=function(t1){
  result=list()
  t1_s=t1[['sentences']]
  t1_w=list()
  for(i in 1:length(t1_s)){
    stemp=t1_s[[i]]
    ls=length(stemp)
    wtemp=c()
    for(j in 1:ls){
      swords=word_tokenizer(stemp[j])[[1]]
      lenw=length(swords)
      if(lenw>=2){
        for(k in 1:(lenw-1)){
          if(tolower(swords[k]) %in% tolower(neg)){
            for(a_k in (k+1):lenw){
              if(tolower(swords[a_k]) %in% tolower(c(like,hate))){
                swords[a_k]=paste0("NOT",swords[a_k])
                swords=swords[-k]
              }
            }
          }
        }
      }
      wtemp=c(wtemp,swords)
      if(any(wtemp %in% stoplist)){
        wtemp=wtemp[-which(wtemp %in% stoplist)]
      }
      t1_w[[i]]=wtemp
    }
  }
  result$words=t1_w
  result$stars=t1$stars
  result$category=t1$categories
  result$text=t1$text
  return(result)
}


for(s in 1:50){
  t1=readRDS(paste0("re_train",s,".rds"))
  result=words_freq(t1)
  saveRDS(result,file=paste0('new_train',s,'.rds'))
}


