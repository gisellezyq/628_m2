train1=data.frame(read.csv("train1.csv",header = T))

# [1] ['Burgers', 'Fast Food', 'Restaurants']                                                   
# [2] ['Steakhouses', 'Restaurants', 'Cheesesteaks', 'Food', 'American (New)', 'Desserts']      
# [3] ['Seafood', 'Restaurants', 'Chinese', 'Live/Raw Food']                                    
# [4] ['Breakfast & Brunch', 'French', 'Restaurants']                                           
# [5] ['Caterers', 'Sandwiches', 'Event Planning & Services', 'Delis', 'Burgers', 'Restaurants']
# [6] ['Seafood', 'American (Traditional)', 'Cajun/Creole', 'Restaurants']                      
# 8458 Levels: ['Active Life', 'American (New)', 'Restaurants', 'Golf'] ...



get_cate=function(rate){
  "%ni%" <- Negate("%in%")
  cate=c()
  temp=subset(train1,train1$stars==rate)
  for(i in 1:dim(train1)[1]){
    csplit=strsplit(as.character(temp$categories[i]),',')[[1]]
    for(c in 1:length(csplit)){
      ssplit=strsplit(csplit[c],"'")[[1]]
      ssplit=ssplit[which(ssplit %ni% c("[","]"," "))]
      cate=c(cate,ssplit)
    }
  }
  return(cate)
}

for(r in 1:5){
  t1=get_cate(r)
  t=t1[-which(t1 == "Restaurants")]
  st=sort(table(t),decreasing = T)
  catefreq=data.frame(cbind(name=names(st),freq=as.integer(st)))
  write.csv(catefreq,paste0("catefreq",as.character(r),".csv"),row.names = FALSE)
}

