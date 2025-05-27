#Upload and clean age data
FPO_a<-read.csv("/home/aaron/Documents/Disc golf analytics/Aging curves/Ages.csv",header=T,sep=",")
FPO_a$Age<-numeric(nrow(FPO_a))
for (i in 1:nrow(FPO_a)){
  FPO_a$Age[i]<-substr(as.character(FPO_a$Date[i]),(nchar(as.character(FPO_a$Date[i]))-3),nchar(as.character(FPO_a$Date[i])))
}
FPO_a$Age<-as.numeric(FPO_a$Age)

#read in outFd for PDGA numbers
outFdF<-read.csv("/home/aaron/Documents/Disc golf analytics/DiscGolfEloFPO.csv", sep=",", header=T)
outFdF$Last<-sub('.* ','',outFdF$Name)

FPO_a$PDGA<-outFdF$PDGA[match(FPO_a$Last,outFdF$Last)]
FPO_a<-FPO_a[complete.cases(FPO_a[,c(1,5,6)]),c(1,5,6)]
FPO_a<-FPO_a[-c(1:8),]

#scrape ratings data
require(rvest)
FPO_a$avail<-numeric(nrow(FPO_a));PDGA<-numeric()
event <- read_html(paste("https://www.pdga.com/player/",FPO_a$PDGA[1],"/history",sep=""))
ratd<-event %>%
  html_nodes(".dates") %>%
  html_text()
ratr<-event %>%
  html_nodes(".player-rating") %>%
  html_text()
ratrd<-event %>%
  html_nodes(".round") %>%
  html_text()
if (length(ratd)!=0) {FPO_a$avail[1]<-"y"} else {FPO_a$avail[1]<-"n"}
if (length(ratd)!=0) {PDGA<-rep(FPO_a$PDGA[1],length(ratd))}
for (i in 2:nrow(FPO_a)) {
  event <- read_html(paste("https://www.pdga.com/player/",FPO_a$PDGA[i],"/history",sep=""))
  ratd1<-event %>%
    html_nodes(".dates") %>%
    html_text()
  ratd<-c(ratd,ratd1)
  ratr1<-event %>%
    html_nodes(".player-rating") %>%
    html_text()
  ratr<-c(ratr,ratr1)
  ratrd1<-event %>%
    html_nodes(".round") %>%
    html_text()
  ratrd<-c(ratrd,ratrd1)
  if (length(ratd1)!=0) {FPO_a$avail[i]<-"y"} else {FPO_a$avail[i]<-"n"}
  if (length(ratd1)!=0) {PDGA<-c(PDGA,rep(FPO_a$PDGA[i],length(ratd1)))}
  print(i)
}

FPO_a<-FPO_a[FPO_a$avail=="y",]

FPO_ar<-data.frame(ratd,ratr,ratrd)

#formatting data
FPO_a<-FPO_a[order(unique(PDGA)),]
FPO_ar<-FPO_ar[order(PDGA),]
FPO_ar$Name<-rep(FPO_a$Last,table(PDGA))
FPO_ar$Byear<-rep(FPO_a$Age,table(PDGA))
FPO_ar<-FPO_ar[FPO_ar$ratd!="Effective Date",]

#reduce FPO_ar$ratd to just year
FPO_ar$year<-numeric(nrow(FPO_ar))
for (i in 1:nrow(FPO_ar)){
  FPO_ar$year[i]<-substr(as.character(FPO_ar$ratd[i]),(nchar(as.character(FPO_ar$ratd[i]))-3),nchar(as.character(FPO_ar$ratd[i])))
}
FPO_ar$year<-as.numeric(FPO_ar$year)

#Calculate age
FPO_ar$Age<-FPO_ar$year-FPO_ar$Byear

FPO_ar$ratr<-as.numeric(as.character(FPO_ar$ratr))
FPO_ar$ratrd<-as.numeric(as.character(FPO_ar$ratrd))

#controlling for rating inflation
summary(lm(FPO_ar$ratr~FPO_ar$year+FPO_ar$Age+I(FPO_ar$Age^2)))
color<-adjustcolor(col="gray",alpha.f=0.5)
plot(FPO_ar$ratr~FPO_ar$year,col=color,ylab="Average Rating",
     xlab="Year",cex=1.5,ylim=c(850,1040),pch=20)
abline(lm(FPO_ar$ratr~FPO_ar$year),lwd=3,col="firebrick3")

FPO_ar$infy<-FPO_ar$year-1997
FPO_ar$ratr<-FPO_ar$ratr-(FPO_ar$infy*.9754)

#Calculate age curve
require(matrixStats)
ry<-tapply(FPO_ar$ratr,FPO_ar[c("Name","Age")],mean,na.rm=T)[,16:49]
ryp<-apply(ry,1,diff)
w<-t(tapply(FPO_ar$ratrd,FPO_ar[c("Name","Age")],sum,na.rm=T))[16:49,]
aging1<-numeric(nrow(ryp))
for (i in 1:nrow(ryp)){
  aging1[i]<-weighted.mean(ryp[i,],w[i,],na.rm=T)}
aging<-cumsum(c(0,aging1))
year<-c(16,as.numeric(names(cumsum(c(0,rowMeans(ryp,na.rm=T))))[-1]))

m<-loess(aging ~ year, span=1)
color<-adjustcolor(col="gray",alpha.f=0.5)
plot(aging~year,ylab="Change in Rating",xlab="Age",pch=20,col=color,cex=1.5)
lines(year,predict(m),lwd=2)

m2<-lm(aging~year+I(year^2)+I(year^3))
color<-adjustcolor(col="gray",alpha.f=0.5)
plot(aging~year,ylab="Change in Rating",xlab="Age",pch=20,col=color,cex=1.5)
lines(year,predict(m2),lwd=2)
