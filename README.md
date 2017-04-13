# weather-visualization-of-beijing-
============================================


```r
library(RCurl)
library(XML)
library(dplyr)
library(ggplot2)
library(stringr)
library(rvest)
library(lubridate)
library("DT")
library(openair)
library(ggplot2)
library(scales)
library(showtext)
library(grid)
library(Cairo)
```


```r
all.pcg<-c("RCurl","XML","dplyr","ggplot2","stringr","rvest","lubridate","DT","scales","showtext","grid","Cairo") 
req.pcg <- function(pcg){ 
new <- pcg[!(pcg %in% installed.packages()[, "Package"])] 
if (length(new)) install.packages(new, dependencies = T) 
sapply(pcg, require, ch = T) 
} 
req.pcg(all.pcg)
```

```r
year<-2014:2016
month<-sprintf("%02d",1:12)
url<- paste("https://www.aqistudy.cn/historydata/daydata.php?city=北京&month=",expand.grid(year,month)$Var1
,expand.grid(year,month)$Var2,sep="")
```


> 以上过程构造了北京市2014~2016年三整年的历史空气质量数据：<br>



```r
#先写完一个看下具体情况:<br>
tbls<-read_html(url[1],encoding="utf-8")%>%html_table(.,header=TRUE,trim=TRUE);tbls<-tbls[[1]]
mytable<-data.frame()
for (i in url){
Sys.sleep(sample(1:5,1))
fun<-function(m){
table<-read_html(m,encoding="utf-8")%>%html_table(.,header=TRUE,trim=TRUE)
table<-table[[1]]
}
mytable<-rbind(mytable,fun(i))
}
```

-------------------------------------------------


这里预览一下以上数据结构：<br>

```r
dim(mytable)
[1] 1096   11

attributes(mytable)$names

 [1] "日期"     "AQI"      "范围"     "质量等级" "PM2.5"    "PM10"     "SO2"     
 [8] "CO"       "NO2"      "O3"       "排名" 
```


```r
datatable(mytable)

使用DT表格预览数据集：

mytable<-read.csv("beijingtianqi.csv",stringsAsFactors=FALSE,check.names=FALSE) 
```

```r
#查看数据结构和变量属性是否符合分析需要：<br>
str(mytable)
'data.frame':	1096 obs. of  11 variables:
 $ 日期    : chr  "2014-01-01" "2014-01-02" "2014-01-03" "2014-01-04" ...
 $ AQI     : int  87 119 81 151 121 181 144 29 44 86 ...
 $ 范围    : chr  "76~99" "80~218" "38~202" "87~229" ...
 $ 质量等级: chr  "良" "轻度污染" "良" "中度污染" ...
 $ PM2.5   : num  45 111.4 46.5 114.4 90.5 ...
 $ PM10    : num  111.3 168.5 97.7 147 117.5 ...
 $ SO2     : num  27.7 69.3 29 40 35.9 46.3 34.4 13 19.3 53.2 ...
 $ CO      : num  1.5 3.43 1.31 2.82 2.31 ...
 $ NO2     : num  61.9 93.1 52 75.4 67 68.2 59.8 21.1 34.5 70.5 ...
 $ O3      : int  64 17 65 10 57 11 55 59 65 28 ...
 $ 排名    : int  32 66 41 112 67 104 85 9 25 58 ...
```

-------------------------


####定义日期变量格式：<br>


```r
mytable$日期<-as.Date(mytable$日期)

names(mytable)[c(1,3,4,11)]<-c("date","Range","Level","Order")
mytable$Year<-year(mytable$date)

breaks<-c(0,50,100,150,200,300,500)
label<-c("excellent","good","Mild pollution","moderate pollution","heavy pollution ","serious pollution")

filter(mytable,Year==2014)%>%calendarPlot(.,pollutant="AQI",breaks=breaks,labels=label,year=2014)
filter(mytable,Year==2015)%>%calendarPlot(.,pollutant="AQI",breaks=breaks,labels=label,year=2015)
filter(mytable,Year==2016)%>%calendarPlot(.,pollutant="AQI",breaks=breaks,labels=label,year=2016)
```


####接下来让我们疯狂一把，将北京三年的空气质量指标AQI用一幅图形尽数呈现。<br>

#####首先要生成一个副本数据：<br>

```r
mydata1<-mytable
write.table (mytable,"beijingtianqi.csv",sep=",",row.names=FALSE)
mydata11<-mydata1[c("date","AQI","Year")]
myasst<-mydata11[mydata11$date %in% as.Date(c("2014-01-01","2015-01-01","2016-01-01")),]
mydata11<-rbind(mydata11,myasst)
```

####因为作图需要，2016年是闰年，2月有29天，14、15年均为28天，会导致最终数据不等长，影响之后的图表制作过程，这里暂且将其去除。<br>
```r
mydata11<-arrange(mydata11,Year,date)
mydata11<-mydata11[mydata11$date!="2016-02-29",]
mydata11$Month<-month(mydata11$date)
mydata11$Monthdata<--5
mydata11$Monthjo<-ifelse(mydata11$Month%%2==0,"A","B")
circlemonth<-seq(0,180,length=17)
circlebj<-rep(c(-circlemonth[1:3],rev(circlemonth[1:3])),2)
```

```r
mydata11$ID<-rep(seq(from=0,to=365),3)
mydata11$Year<-factor(mydata11$Year,order=T)
mydata11$Asst<-5
mydata11$Asst[mydata11$Year==2015]<-10
mydata11$Asst[mydata11$Year==2016]<-15
mydata11A<-mydata11[mydata11$Year==2014&mydata11$Monthjo=="A",]
mydata11B<-mydata11[mydata11$Year==2014&mydata11$Monthjo=="B",]

mydata11$Quarter<-quarter(mydata11$date)
mydata11$Quarterdata<-20
mydata11C<-mydata11%>%filter(mydata11$Year==2014)%>%filter(Quarter %in% c(1,3)) 
mydata11D<-mydata11%>%filter(mydata11$Year==2014)%>%filter(Quarter %in% c(2,4)) 
circlequarter<-seq(45,315,length=4)
circleqd<-rep(c(-circlequarter[1],circlequarter[1]),2)

mydata11$FADD<-cut(mydata11$AQI,breaks=c(0,50,100,150,200,300,500),labels=c("0~50","51~100","101~150","151~200","201~300","301~500"),order=T)
```

```r
#作图方法1：（简便方法，但效果不太好调整）<br>
CairoPNG(file="ECOCirclejj.png",width=1488,height=996)
showtext.begin()
ggplot(data=mydata11)+
geom_tile(aes(ID,Year,fill=FADD))+
coord_polar(theta="x")+
expand_limits(ylim=c(-4,4))+
scale_fill_brewer(palette="YlOrRd",type="seq",direction=1,guide=guide_legend(reverse=TRUE))+
labs(title="2014~2016年度北京市空气质量水平可视化",subtitle="数据根据AQI指标水平进行分段分割",caption="Source：https://www.aqistudy.cn/",x="",y="",fill="")+
theme(
axis.text=element_blank(),
axis.title=element_blank(),
axis.ticks=element_blank(),
panel.background=element_blank(),
panel.grid=element_blank(),
panel.border=element_blank(),
legend.key.size=unit(1.2,'cm'),
legend.key.height=unit(1,'cm'),
legend.text.align=1, 
legend.position=c(1,0.95),legend.justification=c(1,1),
legend.text=element_text(size=20,hjust=3,vjust=3,face="bold"),
plot.background=element_blank(),
plot.title=element_text(size=50,lineheight=1.5),
plot.subtitle=element_text(size=35,lineheight=1.5),
plot.caption=element_text(size=25,hjust=0,lineheight=1.2),
plot.margin=unit(c(.5,.5,.5,.5),"lines")
)
showtext.end()
dev.off()
```

```r
#这里使用geom_raster()图层进行映射（不支持极坐标转换）<br>
breaks<-aggregate(ID~Month,data=mydata11[mydata11$Year==2014,],FUN=median)
CairoPNG(file="ECOCirclejjj.png",width=1200,height=600)
showtext.begin()
ggplot(data=mydata11)+
geom_raster(aes(ID,Year,fill=FADD))+
scale_fill_brewer(palette="YlOrRd",type="seq",direction=1,guide=guide_legend(reverse=TRUE))+
scale_x_continuous(breaks=breaks[,2],labels=paste0(1:12,"月"))+
labs(title="2014~2016年度北京市空气质量水平可视化",subtitle="数据根据AQI指标水平进行分段分割",caption="Source：https://www.aqistudy.cn/",fill="")+
theme(
text=element_text(family="myfont"),
axis.text=element_text(size=20),
axis.title=element_blank(),
axis.ticks=element_blank(),
panel.background=element_blank(),
panel.grid=element_blank(),
panel.border=element_blank(),
legend.key.size=unit(1.2,'cm'),
legend.key.height=unit(1,'cm'),
legend.text.align=1,
legend.text=element_text(size=20,hjust=3,vjust=3,face="bold"),
plot.background=element_blank(),
plot.title=element_text(size=50,lineheight=1.5),
plot.subtitle=element_text(size=35,lineheight=1.5),
plot.caption=element_text(size=25,hjust=0,lineheight=1.2),
plot.margin=unit(c(.5,.5,.5,.5),"lines")
)
showtext.end()
dev.off()
```

```r
#作图方法2：（虽然代码多但是调整相对自由）<br>
setwd("F:/微信公众号/公众号——数据小魔方/2017年4月/20170404")
font.add("myfont","msyhl.ttc")
CairoPNG(file="ECOCircle.png",width=1488,height=996)
showtext.begin()
ggplot()+
geom_bar(data=mydata11A,aes(x=ID,y=Monthdata),stat="identity",width=1,fill="#ECEDD1",col="#ECEDD1")+
geom_bar(data=mydata11B,aes(x=ID,y=Monthdata),stat="identity",width=1,fill="#DFE0B1",col="#DFE0B1")+
geom_bar(data=mydata11C,aes(x=ID,y=Quarterdata),stat="identity",width=1,fill="#BDBDBD",col="#BDBDBD")+
geom_bar(data=mydata11D,aes(x=ID,y=Quarterdata),stat="identity",width=1,fill="#D4D2D3",col="#D4D2D3")+
geom_bar(data=mydata11[mydata11$Year==2016,],aes(x=ID,y=Asst,fill=FADD),stat="identity",width=1)+
geom_bar(data=mydata11[mydata11$Year==2015,],aes(x=ID,y=Asst,fill=FADD),stat="identity",width=1)+
geom_bar(data=mydata11[mydata11$Year==2014,],aes(x=ID,y=Asst,fill=FADD),stat="identity",width=1)+
scale_fill_brewer(palette="YlOrRd",type="seq",direction=1,guide=guide_legend(reverse=TRUE))+
coord_polar(theta="x")+
ylim(-20,20)+
guides(colour=guide_legend(reverse=TRUE))+
geom_text(data=NULL,aes(x=circlemonth,y=-2.5,label=paste0(1:12,"月"),angle=circlebj),family="myfont",size=7,hjust=0.5,vjust=.5)+
geom_text(data=NULL,aes(x=circlequarter,y=17.5,label=paste0(c("一","二","三","四"),"季度"),angle=circleqd),family="myfont",size=7,hjust=0.5,vjust=.5)+
annotate("text",x=0,y=-15,label="北京",size=25,hjust=.5,vjust=1,family="myfont") +    
labs(title="2014~2016年度北京市空气质量水平可视化",subtitle="数据根据AQI指标水平进行分段分割",caption="Source：https://www.aqistudy.cn/",x="",y="",fill="")+
theme(
text=element_text(family="myfont"),
axis.text=element_blank(),
axis.title=element_blank(),
axis.ticks=element_blank(),
panel.background=element_blank(),
panel.grid=element_blank(),
panel.border=element_blank(),
legend.key.size=unit(1.2,'cm'),
legend.key.height=unit(1,'cm'),
legend.text.align=1, 
legend.position=c(1,0),legend.justification=c(1,0),
legend.text=element_text(size=20,hjust=3,vjust=3,face="bold"),
plot.background=element_blank(),
plot.title=element_text(size=50,lineheight=1.5),
plot.subtitle=element_text(size=35,lineheight=1.5),
plot.caption=element_text(size=25,hjust=0,lineheight=1.2),
plot.margin=unit(c(.5,.5,.5,.5),"lines"),
)
showtext.end()
dev.off()
```

