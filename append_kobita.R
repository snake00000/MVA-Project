setwd("C:/Users/lenovo/Downloads/Kobita_Data")
names <- list.files(path="C:/Users/lenovo/Downloads/Kobita_Data")
genre_append=function(x){
  data=read.csv(file=x)
  data$genre=substr(x,1,nchar(x)-4)
  write.csv(data,file=x)
}
lapply(names,genre_append)
df=do.call(rbind.data.frame,lapply(names,function(x) read.csv(file=x)))
df=df[,3:4]
colnames(df)=c("kobita","porjaay")
