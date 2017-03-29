rm(list=ls()) 
working.path<-"~/Dropbox/lala/"
save.data<-paste(working.path,'eco100.rds')
#load student ability data
load("~/Dropbox/Data/ student ability.Rdata")
#load registration data
load("~/Dropbox/Data/registration.Rdata")
#load lala data
lala100<- read_excel("~/Dropbox/lala/lala101.xlsx")
#留下學號及中文姓名
regisdata100<-registration.data[,c('學號','10001中文姓名')]
#留下100學年入學的同學姓名與學號
regisdata100<-dplyr::filter(regisdata100,substring(regisdata100$學號,2,4)=="100")
#rename
colnames(regisdata100)<-c("ID","name")
merge100<-full_join(regisdata100,lala100,by="name")
merge100<-filter(merge100,merge100$ID!="NA")
lala.na.location<-which(is.na(merge100$lala))

merge100[lala.na.location,c('lala')]<-0
table(merge100$lala)
#和能力及成績資料合併
eco100<-left_join(merge100,irt.out2,by='ID')

load("~/Dropbox/Data/transcript.Rdata")
econ.subject<-c("金融市場（一）",
                "金融市場（二）",
                "金融實務（一）",
                "金融實務（二）",
                "財務經濟學",
                "財務管理",
                "經濟結合之經濟學",
                "經濟發展",
                "經濟成長",
                "投資學",
                "保險學",
                "國際金融理論",
                "貨幣理論與政策",
                "期貨交易理論與實務",
                "企業管理",
                "合作經濟學",
                "公用事業經濟學",
                "法律經濟學",
                "租稅各論",
                "高科技產業分析",
                "產業經濟學",
                "創新經濟學",
                "管理經濟學",
                "網際網路經濟學",
                "醫療經濟學",
                "反托拉斯經濟學",
                "反托拉斯經濟學專題研究",
                "中小企業研究",
                "公營事業研究",
                "多國籍公司經濟學",
                "通訊傳播經濟分析",
                "競爭政策分析",
                "產業結構與政策",
                "國際貿易理論與政策",
                "國際貿易實務",
                "福利經濟",
                "管制經濟學",
                "土地經濟分析",
                "都市經濟學",
                "勞動經濟學",
                "資源經濟學",
                "環境經濟學",
                "人口經濟學",
                "家庭經濟學",
                "運動經濟學",
                "迴歸分析",
                "線性代數",
                "經濟資料視覺化處理",
                "作業研究",
                "計量經濟學",
                "高等統計學",
                "高等微積分",
                "經濟套裝軟體",
                "經濟數學",
                "賽局理論（一）",
                "賽局理論（二）",
                "一般均衡理論與應用",
                "成本效益分析",
                "行為經濟學",
                "資訊經濟學",
                "實驗經濟學",
                "應用數量方法",
                "統計軟體資料處理與應用",
                "個體經濟理論分析",
                "經濟理論分析",
                "動態經濟方法",
                "數理經濟學",
                "經濟閱讀",
                "經濟論壇（一）",
                "經濟論壇（二）",
                "經濟問題與政策（一）",
                "經濟問題與政策（二）",
                "經濟學原理",
                "個體經濟學",
                "公共經濟學",
                "統計學",
                "貨幣銀行學",
                "微積分",
                "總體經濟學",
                "國際經濟學",
                "經濟思想史")
library(dplyr)
transcript.data<-tbl_df(transcript.data)
transcript.data %>% filter(科目名稱 %in% econ.subject)->dataeconomy
x1<-dataeconomy$學分數*dataeconomy$成績
mutate(dataeconomy,加權=x1)->dataeconomy
only100<-dplyr::filter(dataeconomy,dataeconomy$學年=="100")


gpa.track100<-c()
for(i in 1:144){
  #print(i)
  #i<-1
  id.i<-eco100[i,c('ID')]
  # 抽出學號為id.i的學生成績單
  data.i <- filter(only100,學號==as.integer(id.i))
  
  gpa.i<- ( sum(data.i$加權)/sum(data.i$學分數))
  
  gpa.track100<-rbind(gpa.track100,c(id.i,gpa.i))
}

unlist(gpa.track100)->gpa.track100 #資料變成一串數字，非144x3的陣列
gpa.track100<-matrix(gpa.track100,144,2) #一串數字轉成144x3的矩陣
gpa.track100<-as.data.frame(gpa.track100) #矩陣改成data frame
colnames(gpa.track100)<-c('ID','gpa.1') 
gpa.100<-left_join(eco100,gpa.track100,by='ID')
gpa.100$lala<-as.logical(gpa.100$lala)
####2nd
only101<-dplyr::filter(dataeconomy,dataeconomy$學年=="101")


gpa.track101<-c()
for(i in 1:144){
  #print(i)
  #i<-1
  id.i<-eco100[i,c('ID')]
  # 抽出學號為id.i的學生成績單
  data.i <- filter(only101,學號==as.integer(id.i))
  
  gpa.i<- ( sum(data.i$加權)/sum(data.i$學分數))
  
  gpa.track101<-rbind(gpa.track101,c(id.i,gpa.i))
}

unlist(gpa.track101)->gpa.track101 #資料變成一串數字，非144x3的陣列
gpa.track101<-matrix(gpa.track101,144,2) #一串數字轉成144x3的矩陣
gpa.track101<-as.data.frame(gpa.track101) #矩陣改成data frame
colnames(gpa.track101)<-c('ID','gpa.2') 
gpa.100<-left_join(gpa.100,gpa.track101,by='ID')

####3rd
only102<-dplyr::filter(dataeconomy,dataeconomy$學年=="102")


gpa.track102<-c()
for(i in 1:144){
  #print(i)
  #i<-1
  id.i<-eco100[i,c('ID')]
  # 抽出學號為id.i的學生成績單
  data.i <- filter(only102,學號==as.integer(id.i))
  
  gpa.i<- ( sum(data.i$加權)/sum(data.i$學分數))
  
  gpa.track102<-rbind(gpa.track102,c(id.i,gpa.i))
}

unlist(gpa.track102)->gpa.track102 #資料變成一串數字，非144x3的陣列
gpa.track102<-matrix(gpa.track102,144,2) #一串數字轉成144x3的矩陣
gpa.track102<-as.data.frame(gpa.track102) #矩陣改成data frame
colnames(gpa.track102)<-c('ID','gpa.3') 
gpa.100<-left_join(gpa.100,gpa.track102,by='ID')

####4th
only103<-dplyr::filter(dataeconomy,dataeconomy$學年=="103")


gpa.track103<-c()
for(i in 1:144){
  #print(i)
  #i<-1
  id.i<-eco100[i,c('ID')]
  # 抽出學號為id.i的學生成績單
  data.i <- filter(only103,學號==as.integer(id.i))
  
  gpa.i<- ( sum(data.i$加權)/sum(data.i$學分數))
  
  gpa.track103<-rbind(gpa.track103,c(id.i,gpa.i))
}

unlist(gpa.track103)->gpa.track103 #資料變成一串數字，非144x3的陣列
gpa.track103<-matrix(gpa.track103,144,2) #一串數字轉成144x3的矩陣
gpa.track103<-as.data.frame(gpa.track103) #矩陣改成data frame
colnames(gpa.track103)<-c('ID','gpa.4') 
gpa.100<-left_join(gpa.100,gpa.track103,by='ID')

####12
only1001<-dplyr::filter(dataeconomy,dataeconomy$學年!="103")

only1001<-dplyr::filter(only1001,only1001$學年!="102")
gpa.track1001<-c()
for(i in 1:144){
  #print(i)
  #i<-1
  id.i<-eco100[i,c('ID')]
  # 抽出學號為id.i的學生成績單
  data.i <- filter(only1001,學號==as.integer(id.i))
  
  gpa.i<- ( sum(data.i$加權)/sum(data.i$學分數))
  
  gpa.track1001<-rbind(gpa.track1001,c(id.i,gpa.i))
}

unlist(gpa.track1001)->gpa.track1001 #資料變成一串數字，非144x3的陣列
gpa.track1001<-matrix(gpa.track1001,144,2) #一串數字轉成144x3的矩陣
gpa.track1001<-as.data.frame(gpa.track1001) #矩陣改成data frame
colnames(gpa.track1001)<-c('ID','gpa.12') 
gpa.100<-left_join(gpa.100,gpa.track1001,by='ID')

####123
only10012<-dplyr::filter(dataeconomy,dataeconomy$學年!="103")


gpa.track10012<-c()
for(i in 1:144){
  #print(i)
  #i<-1
  id.i<-eco100[i,c('ID')]
  # 抽出學號為id.i的學生成績單
  data.i <- filter(only10012,學號==as.integer(id.i))
  
  gpa.i<- ( sum(data.i$加權)/sum(data.i$學分數))
  
  gpa.track10012<-rbind(gpa.track10012,c(id.i,gpa.i))
}

unlist(gpa.track10012)->gpa.track10012 #資料變成一串數字，非144x3的陣列
gpa.track10012<-matrix(gpa.track10012,144,2) #一串數字轉成144x3的矩陣
gpa.track10012<-as.data.frame(gpa.track10012) #矩陣改成data frame
colnames(gpa.track10012)<-c('ID','gpa.123') 
gpa.100<-left_join(gpa.100,gpa.track10012,by='ID')
library(readxl)
指100 <- read_excel("~/Dropbox/Data/100學年度後學測、指考成績/指考/100指考錄取生資料檔.xls")

指100<-指100[,c('考生姓名','學測總分')]
colnames(指100)<-c("name","aaa")
gpa.100<-left_join(gpa.100,指100,by='name')

申100<- read_excel("~/Dropbox/Data/100學年度後學測、指考成績/繁星、個人申請/100個人申請錄取生資料檔.xls")
申100<-申100[,c('姓名','學測總分')]
colnames(申100)<-c("name","bbb")
gpa.100<-left_join(gpa.100,申100,by='name')

繁100 <- read_excel("~/Dropbox/Data/100學年度後學測、指考成績/繁星、個人申請/100繁星推薦錄取生資料檔.xls")
繁100<-繁100[,c('姓名','學測總分')]
colnames(繁100)<-c("name","ccc")
gpa.100<-left_join(gpa.100,繁100,by='name')
gpa.100$aaa[is.na(gpa.100$aaa)]=0
gpa.100$bbb[is.na(gpa.100$bbb)]=0
gpa.100$ccc[is.na(gpa.100$ccc)]=0
gpa.100$aaa[(gpa.100$aaa=="--")]=0
gpa.100$aaa<-as.numeric(gpa.100$aaa)
xent<-(gpa.100$aaa+gpa.100$bbb+gpa.100$ccc)
mutate(gpa.100,ent=xent)->gpa.100
load("~/Dropbox/Data/address.Rdata")
address100<-address.data[,c('學號','10001通訊地址')]
colnames(address100)<-c("ID","address")
gpa.100<-left_join(gpa.100,address100,by='ID')
economy100<-gpa.100[,-c(3,4,14,15,16)]
economy100$lala<-as.numeric(economy100$lala)
earn<- read_excel("~/Dropbox/lala/台灣各區所得.xlsx")
economy100<-left_join(economy100,earn,by='address')
save(economy100,file=save.data)


