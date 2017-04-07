rm(list=ls()) 
working.path<-"~/Dropbox/lala/"
save.data<-paste(working.path,'eco102.rds')
load("~/Dropbox/Data/registration.Rdata")
library(readxl)
lala102 <- read_excel("~/Dropbox/lala/102lalaecon.xlsx")
regisdata102<-registration.data[,c('學號','10201中文姓名')]
regisdata102<-dplyr::filter(regisdata102,substring(regisdata102$學號,2,4)=="102")
colnames(regisdata102)<-c("ID","name")
merge102<-full_join(regisdata102,lala102,by="name")
merge102<-filter(merge102,merge102$ID!="NA")
lala.na.location<-which(is.na(merge102$lala))

merge102[lala.na.location,c('lala')]<-0
table(merge102$lala)
#和能力及成績資料合併

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
only102<-dplyr::filter(dataeconomy,dataeconomy$學年=="102")


gpa.track102<-c()
for(i in 1:143){
  #print(i)
  #i<-1
  id.i<-merge102[i,c('ID')]
  # 抽出學號為id.i的學生成績單
  data.i <- filter(only102,學號==as.integer(id.i))
  
  gpa.i<- ( sum(data.i$加權)/sum(data.i$學分數))
  
  gpa.track102<-rbind(gpa.track102,c(id.i,gpa.i))
}

unlist(gpa.track102)->gpa.track102 #資料變成一串數字，非144x3的陣列
gpa.track102<-matrix(gpa.track102,143,2) #一串數字轉成144x3的矩陣
gpa.track102<-as.data.frame(gpa.track102) #矩陣改成data frame
colnames(gpa.track102)<-c('ID','gpa.1') 
gpa.102<-left_join(merge102,gpa.track102,by='ID')
gpa.102$lala<-as.logical(gpa.102$lala)
##########
only103<-dplyr::filter(dataeconomy,dataeconomy$學年=="103")


gpa.track103<-c()
for(i in 1:143){
  #print(i)
  #i<-1
  id.i<-merge102[i,c('ID')]
  # 抽出學號為id.i的學生成績單
  data.i <- filter(only103,學號==as.integer(id.i))
  
  gpa.i<- ( sum(data.i$加權)/sum(data.i$學分數))
  
  gpa.track103<-rbind(gpa.track103,c(id.i,gpa.i))
}

unlist(gpa.track103)->gpa.track103 #資料變成一串數字，非144x3的陣列
gpa.track103<-matrix(gpa.track103,143,2) #一串數字轉成144x3的矩陣
gpa.track103<-as.data.frame(gpa.track103) #矩陣改成data frame
colnames(gpa.track103)<-c('ID','gpa.2') 
gpa.102<-left_join(gpa.102,gpa.track103,by='ID')
gpa.102$lala<-as.logical(gpa.102$lala)


#####
only104<-dplyr::filter(dataeconomy,dataeconomy$學年=="104")


gpa.track104<-c()
for(i in 1:143){
  #print(i)
  #i<-1
  id.i<-merge102[i,c('ID')]
  # 抽出學號為id.i的學生成績單
  data.i <- filter(only104,學號==as.integer(id.i))
  
  gpa.i<- ( sum(data.i$加權)/sum(data.i$學分數))
  
  gpa.track104<-rbind(gpa.track104,c(id.i,gpa.i))
}

unlist(gpa.track104)->gpa.track104 #資料變成一串數字，非144x3的陣列
gpa.track104<-matrix(gpa.track104,143,2) #一串數字轉成144x3的矩陣
gpa.track104<-as.data.frame(gpa.track104) #矩陣改成data frame
colnames(gpa.track104)<-c('ID','gpa.3') 
gpa.102<-left_join(gpa.102,gpa.track104,by='ID')
gpa.102$lala<-as.logical(gpa.102$lala)
指102 <- read_excel("~/Dropbox/Data/100學年度後學測、指考成績/指考/102指考錄取生資料檔.xls")

指102<-指102[,c('考生姓名','學測總分')]
colnames(指102)<-c("name","aaa")
gpa.102<-left_join(gpa.102,指102,by='name')

申102<- read_excel("~/Dropbox/Data/100學年度後學測、指考成績/繁星、個人申請/102個人申請錄取生資料檔.xls")
申102<-申102[,c('姓名','學測總分')]
colnames(申102)<-c("name","bbb")
gpa.102<-left_join(gpa.102,申102,by='name')

繁102 <- read_excel("~/Dropbox/Data/100學年度後學測、指考成績/繁星、個人申請/102繁星推薦錄取生資料檔.xls")
繁102<-繁102[c(5,17)]
colnames(繁102)<-c("name","ccc")

gpa.102<-left_join(gpa.102,繁102,by='name')
gpa.102$aaa[is.na(gpa.102$aaa)]=0
gpa.102$bbb[is.na(gpa.102$bbb)]=0
gpa.102$ccc[is.na(gpa.102$ccc)]=0
gpa.102$aaa[(gpa.102$aaa=="--")]=0
gpa.102$aaa<-as.numeric(gpa.102$aaa)
gpa.102$ccc<-as.numeric(gpa.102$ccc)
xent<-(gpa.102$aaa+gpa.102$bbb+gpa.102$ccc)
mutate(gpa.102,ent=xent)->gpa.102
load("~/Dropbox/Data/address.Rdata")
address102<-address.data[,c('學號','10201通訊地址')]
colnames(address102)<-c("ID","address")
gpa.102<-left_join(gpa.102,address102,by='ID')
economy102<-gpa.102[,-c(2,3,8,9,10)]
economy102$lala<-as.numeric(economy102$lala)
earn<- read_excel("~/Dropbox/lala/台灣各區所得.xlsx")
economy102<-left_join(economy102,earn,by='address')
save(economy102,file=save.data)
