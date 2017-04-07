rm(list=ls()) 
working.path<-"~/Dropbox/lala/"
save.data<-paste(working.path,'eco103.rds')
load("~/Dropbox/Data/registration.Rdata")
library(readxl)
lala103 <- read_excel("~/Dropbox/lala/103lalaecon.xlsx")
regisdata103<-registration.data[,c('學號','10301中文姓名')]
regisdata103<-dplyr::filter(regisdata103,substring(regisdata103$學號,2,4)=="103")
colnames(regisdata103)<-c("ID","name")
merge103<-full_join(regisdata103,lala103,by="name")
merge103<-filter(merge103,merge103$ID!="NA")
lala.na.location<-which(is.na(merge103$lala))

merge103[lala.na.location,c('lala')]<-0
table(merge103$lala)
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
only103<-dplyr::filter(dataeconomy,dataeconomy$學年=="103")


gpa.track103<-c()
for(i in 1:135){
  #print(i)
  #i<-1
  id.i<-merge103[i,c('ID')]
  # 抽出學號為id.i的學生成績單
  data.i <- filter(only103,學號==as.integer(id.i))
  
  gpa.i<- ( sum(data.i$加權)/sum(data.i$學分數))
  
  gpa.track103<-rbind(gpa.track103,c(id.i,gpa.i))
}

unlist(gpa.track103)->gpa.track103 #資料變成一串數字，非144x3的陣列
gpa.track103<-matrix(gpa.track103,135,2) #一串數字轉成144x3的矩陣
gpa.track103<-as.data.frame(gpa.track103) #矩陣改成data frame
colnames(gpa.track103)<-c('ID','gpa.1') 
gpa.103<-left_join(merge103,gpa.track103,by='ID')
gpa.103$lala<-as.logical(gpa.103$lala)
##########
only104<-dplyr::filter(dataeconomy,dataeconomy$學年=="104")


gpa.track104<-c()
for(i in 1:135){
  #print(i)
  #i<-1
  id.i<-merge103[i,c('ID')]
  # 抽出學號為id.i的學生成績單
  data.i <- filter(only104,學號==as.integer(id.i))
  
  gpa.i<- ( sum(data.i$加權)/sum(data.i$學分數))
  
  gpa.track104<-rbind(gpa.track104,c(id.i,gpa.i))
}

unlist(gpa.track104)->gpa.track104 #資料變成一串數字，非144x3的陣列
gpa.track104<-matrix(gpa.track104,135,2) #一串數字轉成144x3的矩陣
gpa.track104<-as.data.frame(gpa.track104) #矩陣改成data frame
colnames(gpa.track104)<-c('ID','gpa.2') 
gpa.103<-left_join(gpa.103,gpa.track104,by='ID')
gpa.103$lala<-as.logical(gpa.103$lala)


指103 <- read_excel("~/Dropbox/Data/100學年度後學測、指考成績/指考/103指考錄取生資料檔.xls")

指103<-指103[c(3,12)]
colnames(指103)<-c("name","aaa")
gpa.103<-left_join(gpa.103,指103,by='name')

申103<- read_excel("~/Dropbox/Data/100學年度後學測、指考成績/繁星、個人申請/103個人申請錄取生資料檔.xls")
申103<-申103[,c('姓名','學測總分')]
colnames(申103)<-c("name","bbb")
gpa.103<-left_join(gpa.103,申103,by='name')

繁103 <- read_excel("~/Dropbox/Data/100學年度後學測、指考成績/繁星、個人申請/103繁星推薦錄取生資料檔.xls")
繁103<-繁103[c(6,19)]
colnames(繁103)<-c("name","ccc")

gpa.103<-left_join(gpa.103,繁103,by='name')
gpa.103$aaa[is.na(gpa.103$aaa)]=0
gpa.103$bbb[is.na(gpa.103$bbb)]=0
gpa.103$ccc[is.na(gpa.103$ccc)]=0
gpa.103$aaa[(gpa.103$aaa=="--")]=0
gpa.103$aaa<-as.numeric(gpa.103$aaa)
gpa.103$ccc<-as.numeric(gpa.103$ccc)
gpa.103$bbb<-as.numeric(gpa.103$bbb)
xent<-(gpa.103$aaa+gpa.103$bbb+gpa.103$ccc)
mutate(gpa.103,ent=xent)->gpa.103
load("~/Dropbox/Data/address.Rdata")
address103<-address.data[,c('學號','10301通訊地址')]
colnames(address103)<-c("ID","address")
gpa.103<-left_join(gpa.103,address103,by='ID')
economy103<-gpa.103[,-c(2,3,8,9,7)]
economy103$lala<-as.numeric(economy103$lala)
earn<- read_excel("~/Dropbox/lala/台灣各區所得.xlsx")
economy103<-left_join(economy103,earn,by='address')
save(economy103,file=save.data)
