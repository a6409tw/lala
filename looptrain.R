
rm(list=ls())
working.path<-"~/Dropbox/李晨/lala"
load("~/Dropbox/Data/transcript.Rdata")
eco101<-readRDS("eco101.rds")
colnames(transcript.data)[1]<-'ID'
pecon.track<-c()
for(i in 1:dim(eco101)[1]){
  #print(i)
  #i<-10
  id.i<-eco101[i,c('ID')]
  # 抽出學號為id.i的學生成績單
  transcript.i <- filter(transcript.data,ID==as.integer(id.i))
  # 判斷是否經原被當
  # 只看100學年上學期且課程名稱為 經濟學原理
  transcript.i %>%  filter(學年==100,學期==1,科目名稱=='經濟學原理') -> econ101.1.i
  pecon.1.flunk.i<- ifelse( (dim(econ101.1.i)[1]!=0), # 若此條件成立
                            (econ101.1.i$成績<60)*1, #成立時執行
                            NA) #不成立時執行
  # 只看100學年下學期且課程名稱為 經濟學原理
  transcript.i %>%  filter(學年==100,學期==2,科目名稱=='經濟學原理') -> econ101.2.i
  pecon.2.flunk.i<- ifelse( (dim(econ101.2.i)[1]!=0), # 若此條件成立
                            (econ101.2.i$成績<60)*1, #成立時執行
                            NA) #不成立時執行
  pecon.track<-rbind(pecon.track,c(id.i, pecon.1.flunk.i,pecon.2.flunk.i))
}

pecon.track2<-pecon.track
unlist(pecon.track2)->pecon.track2 #資料變成一串數字，非144x3的陣列
pecon.track2<-matrix(pecon.track2,144,3) #一串數字轉成144x3的矩陣
pecon.track2<-as.data.frame(pecon.track2) #矩陣改成data frame
colnames(pecon.track2)<-c('ID','經原上當','經原下當') 
table(pecon.track2[,2:3]) #查看狀態

pecon.track<-as.matrix(pecon.track)

colnames(pecon.track)<-c('ID','經原上當','經原下當') 
pecon.track<-as.data.frame(pecon.track)
table(pecon.track$經原上當)
