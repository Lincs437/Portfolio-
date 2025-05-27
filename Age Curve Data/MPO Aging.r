#Upload and clean age data
MPO_a<-read.csv("C:/Users/ahoward1/Documents/Disc golf analytics/Aging curves/Disc-Golf-Aging-Curves/Ages.csv",header=T,sep=",")
MPO_a$Age<-numeric(nrow(MPO_a))
for (i in 1:nrow(MPO_a)){
  MPO_a$Age[i]<-substr(as.character(MPO_a$Date[i]),(nchar(as.character(MPO_a$Date[i]))-3),nchar(as.character(MPO_a$Date[i])))
}
MPO_a$Age<-as.numeric(MPO_a$Age)

#read in outFd for PDGA numbers
outFd<-read.csv("C:/Users/ahoward1/Documents/Disc golf analytics/outFd.csv", sep=",", header=T)
outFd$Last <- sub('.* ','',outFd$Name)
outFd$First <- sub(' .*','',outFd$Name)

MPO_a$First[MPO_a$Last=="McMahon"] <- "Eagle"
outFd$First[outFd$Last=="Wysocki"] <- "Ricky"

#MPO_a$PDGA<-outFd$PDGA[match(MPO_a$Last,outFd$Last)]
MPO_a$PDGA<-outFd$PDGA[match(paste(MPO_a$First,MPO_a$Last,sep=" "),paste(outFd$First,outFd$Last,sep=" "))]


MPO_a<-MPO_a[complete.cases(MPO_a[,c(1,2,5,6)]),c(1,2,5,6)]
MPO_a<-MPO_a[1:which(MPO_a$Last=="Wysocki"),]

which(table(MPO_a$Last)>=2)

#scrape ratings data
require(rvest)
MPO_a$avail<-numeric(nrow(MPO_a));PDGA<-numeric()
event <- read_html(paste("https://www.pdga.com/player/",MPO_a$PDGA[1],"/history",sep=""))
ratd<-event %>%
  html_nodes(".date") %>%
  html_text()
ratr<-event %>%
  html_nodes(".player-rating") %>%
  html_text()
ratrd<-event %>%
  html_nodes(".round") %>%
  html_text()
if (length(ratd)!=0) {MPO_a$avail[1]<-"y"} else {MPO_a$avail[1]<-"n"}
if (length(ratd)!=0) {PDGA<-rep(MPO_a$PDGA[1],length(ratd))}
for (i in 2:nrow(MPO_a)) {
event <- read_html(paste("https://www.pdga.com/player/",MPO_a$PDGA[i],"/history",sep=""))
Sys.sleep(1)
ratd1<-event %>%
  html_nodes(".date") %>%
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
if (length(ratd1)!=0) {MPO_a$avail[i]<-"y"} else {MPO_a$avail[i]<-"n"}
if (length(ratd1)!=0) {PDGA<-c(PDGA,rep(MPO_a$PDGA[i],length(ratd1)))}
print(i)
Sys.sleep(1)
}

MPO_a<-MPO_a[MPO_a$avail=="y",]

ratd<-ratd[ratd!="Effective Date"]
ratr<-ratr[ratr!="Rating"]
ratrd<-ratrd[ratrd!="Rounds Used"]

MPO_ar<-data.frame(ratd,ratr,ratrd)

#formatting data
MPO_a<-MPO_a[order(unique(PDGA)),]
MPO_ar<-MPO_ar[order(PDGA),]
MPO_ar$Name<-rep(MPO_a$Last,table(PDGA))
MPO_ar$Name_f<-rep(MPO_a$First,table(PDGA))
MPO_ar$Byear<-rep(MPO_a$Age,table(PDGA))
MPO_ar$PDGA<-rep(MPO_a$PDGA,table(PDGA))
MPO_ar<-MPO_ar[MPO_ar$ratd!="Effective Date",]

#reduce MPO_ar$ratd to just year
MPO_ar$year<-numeric(nrow(MPO_ar))
for (i in 1:nrow(MPO_ar)){
  MPO_ar$year[i]<-substr(as.character(MPO_ar$ratd[i]),(nchar(as.character(MPO_ar$ratd[i]))-3),nchar(as.character(MPO_ar$ratd[i])))
}
MPO_ar$year<-as.numeric(MPO_ar$year)

#Calculate age
MPO_ar$Age<-MPO_ar$year-MPO_ar$Byear

MPO_ar$ratr<-as.numeric(as.character(MPO_ar$ratr))
MPO_ar$ratrd<-as.numeric(as.character(MPO_ar$ratrd))

#check for age errors
range(MPO_ar$Age)

write.csv(MPO_ar,"MPO_ar.csv")

#########################################################
########Run through here to create MPO_ar dataset########
#########################################################


