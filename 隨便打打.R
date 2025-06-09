mtcars

id <-c(1,2,3)
age <-c(25,30,35)
sex<-c("male","female","male")
pay<-c(1000,1500,2000)
dataframe<-data.frame(id,age,sex,pay)
dataframe

#建立股標名稱向量
name<-c("0050","0051","0056")
#建立分配股利或股息的向量
type<-c("股息","股息","股息")
#建立去年和今年所分配額度的向量
prelnterest<-c(100,200,300)
nowlnterest<-c(50,150,200)

InDFrame<-data.frame(name,type,prelnterest,nowlnterest)
InDFrame

InDFrame[1,3]

InDFrame[2:3,2:4]

InDFrame[1:3,"prelnterest"]

InDFrame$ nowlnterest

InDFrame$prelnterest


subset(InDFrame,subset=prelnterest>100)

smallToBig<-c(100,1000,10)
order(smallToBig)

smallToBig<-c(100,1000,10)
smallToBig[order(smallToBig)]
order(prelnterest)

edit(InDFrame)
InDFrame<-edit(InDFrame)


#U7
StudentId<-c(1234,5678,4567)
StudentSex<-c("Male","Female","Male")
StudentName<- c("John","Mary","Jack")
StudentList <-list(StudentId,StudentSex,StudentName)
names(StudentList) <- c("學生證號碼","學生性別","學生姓名")
StudentList

StudentList[[1]]
StudentList[["學生證號碼"]]
StudentList$學生證號碼

#StudentList <- list(學生證號碼=StudentId,學生性別=StudentSex,學生姓名=StudentName)
#StudentList

StudentClass<-"資工系"
StudentList <- c(StudentList,系所=StudentClass)
StudentList

MovName <- "一級玩家"
actName <- c("Tye Sheridan","Olivia Cooke","Ben Mendelsohn","T.J. Miller","Simon Pegg","Mark Rylance")

scores <- c(4.5, 4.0, 5.0)
sources <- c("IMDb1", "IMDb2", "IMDb3")
comments <- c("Best Film", "Very Funny", "Good Idea")

# 建立資料框
REV <- data.frame(scores, sources, comments)

# 建立列表
List <- list(
  電影名稱 = MovName,
  主要演員 = actName,
  相關評論 = REV
)

# 加入新的項目
ReleaseDate <- 20180329
List <- c(List, 電影上映日期 = ReleaseDate)

# 正確查詢第三位演員
print(List$主要演員[3])

# 正確顯示相關評論
print(List$相關評論)


poker<-function(n){
  color<-c("黑桃","紅心","方塊","梅花")
  number<-c("A","J","Q","K","2","3","4","5","6","7","8","9","10")
  pokerResult<-rep(NA,n)
  colors<-rep(color,13) #13組("黑桃","紅心","方塊","梅花")
  numbers<-rep(number,4) #4組("A","J","Q","K","2","3","4","5","6","7","8","9","10")
  cards<-paste(numbers,colors) #paste()將字串連起來預設以一個空格連接
  pokerResult <- sample(cards,size=n) #pokerResult 一組向量
  return(pokerResult)
}
poker(52)

grade <- c("A", "B", "C", "D", "E")
百分數 <- c("80-100分", "70-79分", "60-69分", "50-59分", "49分以下")
GPA <- c(4:0)
GPA轉換表 <- data.frame(grade, 百分數, GPA)
GPA轉換表

五科成績 <- sample(0:100,5,replace=TRUE) #分數可重複
五科成績

成績轉GPA < function(score){
  GPAID <- ifelse(score>=80,1,
                ifelse(score>=70,2,
                       ifelse(score>=60,3,
                              ifelse(score>=50,4,5))))
  data.frame(成績=score,GPA轉換表[GPAID,],row.names=NULL) #以資料框格式輸出
}
成績轉GPA(五科成績)

成績轉GPA(五科成績)$GPA


學生姓名<-c("A","B","C","D","E")
調整前成績<-sample(0:59,5,replace=TRUE)

調整學生成績<-function(score){
  
調整後成績<-rep(NA,5)
  for(i in 1:5){
    調整後成績[i]=sqrt(score[i])*10
  }
  data.frame(學生姓名,調整前成績,調整後成績)

}
調整學生成績(調整前成績)



demo(graphics)

plotNumber<-1:10
plot(plotNumber,type="l",main="Linear",xlab="X",ylab="Y")

plotNumber<-1:10
plot(plotNumber,type="l",col="blue",lty=2,main="Linear",xlab="X",ylab="Y")

plotNumber<-1:10
plot(plotNumber,type="p",col="blue",lty=2,main="Linear",xlab="X",ylab="Y")

dotNumber <- 1:10
dotchart(dotNumber,label=dotNumber,pch=2,color="red",main="Dot",xlab="X",ylab="Y")

barNumber<-matrix(sample(1:10),nrow=2,ncol=5)
barNumber
barplot(barNumber,col=c("1","2"),beside=TRUE,names.arg=c("L1","L2","L3","L4","L5"),main="Barplot")

barplot(barNumber,col=c("1","2"),names.arg=c("L1","L2","L3","L4","L5"),main="Barplot")


pieNumber<-1:10
pie(pieNumber,main="圓餅圖")

pieNumber<-1:5
pie(pieNumber,labels=c("P1","P2","P3","P4","P5"),col=rainbow(length(pieNumber)),main="圓餅圖")

x<-1:4
y<-5:8
boxplot(x,y,col="red",border="blue")