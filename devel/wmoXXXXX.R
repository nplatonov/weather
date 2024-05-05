require(plutil)
tz <- "Asia/Krasnoyarsk"
'f1_prepare' <- function(wmo=25563) {
   rds <- past("weather",wmo,".qs")
   if (T & file.exists(rds))
      return(0L)
   srcname <- dir(pattern=paste0(".*",wmo,".*.csv(\\.gz)*$"))
   a <- read.table(srcname,header=TRUE,sep=";",comment="#"
                  ,encoding="UTF-8",check.names=FALSE)
   colnames(a)[1] <- "time"
   a$time <- as.POSIXct(a$time,format="%d.%m.%Y %H:%M",tz=tz)
  # print(subset(a,as.Date(time)==as.Date("2014-12-07")))
  # a$time <- a$time-as.difftime(12,units="hours")
  # Sys.setenv(TZ="Zulu")
   w <- apply(with(a,cbind(WW,W1,W2)),1,paste,collapse=" ")
   w <- mygsub("буря","ST*",w)
   w <- mygsub("метель","BLSN*",w)
   w <- mygsub("туман","BR*",w)
   w <- mygsub("ливневой снег","SHSN*",w)
   w <- mygsub("ливневой дождь","SHRA*",w)
   w <- mygsub("ливень","SH*",w)
   w <- mygsub("дождь","RA*",w)
   w <- mygsub("снег","SN*",w)
   w <- mygsub("поземок","DR*",w)
   w <- mygsub("([А-Яа-я]|\\(|\\)|\\.|\\/|\\s|\\,)","",w)
   w <- mygsub("\\*$","",w)
   w <- mygsub("\\*","/",w)
   w <- unname(sapply(w,function(x){
      paste(unique(mystrsplit(x,split="/")),collapse="/")
   }))
   a$WW <- w
   rm(w)
   a$DD <- mygsub("Ветер, дующий с севера","N",a$DD)
   a$DD <- mygsub("Ветер, дующий с северо-северо-востока","NNE",a$DD)
   a$DD <- mygsub("Ветер, дующий с юго-юго-запада","SSW",a$DD)
   a$DD <- mygsub("Ветер, дующий с юго-запада","SW",a$DD)
   a$DD <- mygsub("Ветер, дующий с востоко-юго-востока","ESE",a$DD)
   a$DD <- mygsub("Ветер, дующий с юго-востока","SE",a$DD)
   a$DD <- mygsub("Ветер, дующий с юга","S",a$DD)
   a$DD <- mygsub("Ветер, дующий с запада","W",a$DD)
   a$DD <- mygsub("Штиль, безветрие","C",a$DD) ## calm
   a$DD <- mygsub("Ветер, дующий с северо-востока","NE",a$DD)
   a$DD <- mygsub("Ветер, дующий с востока","E",a$DD)
   a$DD <- mygsub("Ветер, дующий с северо-запада","NW",a$DD)
   a$DD <- mygsub("Ветер, дующий с западо-северо-запада","WNW",a$DD)
   a$DD <- mygsub("Ветер, дующий с северо-северо-запада","NNW",a$DD)
   a$DD <- mygsub("Ветер, дующий с юго-юго-востока","SSE",a$DD)
   a$DD <- mygsub("Ветер, дующий с западо-юго-запада","WSW",a$DD)
   a$DD <- mygsub("Ветер, дующий с востоко-северо-востока","ENE",a$DD)
   a$DD <- mygsub("Переменное направление","VRB",a$DD)
   a$DD[nchar(a$DD)==0] <- "---"
   a$N <- mygsub("100%.","100",a$N)
   a$N <- mygsub("90  или более, но не 100%","95",a$N)
   a$N <- mygsub("70 – 80%.","75",a$N)
   a$N <- mygsub("20–30%.","25",a$N)
   a$N <- mygsub("Облаков нет.","0",a$N)
   a$N <- mygsub("50%.","50",a$N)
   a$N <- mygsub("40%.","40",a$N)
   a$N <- mygsub("60%.","60",a$N)
   a$N <- mygsub("10%  или менее, но не 0","5",a$N)
   a$N <- mygsub("Небо не видно из-за тумана и/или других метеорологических явлений.","-1",a$N)
   a$N <- as.integer(a$N)
   a$sss[a$sss==""] <- NA_character_
   a$sss[a$sss=="Снежный покров не постоянный."] <- "-10"
   a$sss[a$sss=="Измерение невозможно или неточно."] <- "-20"
   a$sss[a$sss=="Менее 0.5"] <- "0.5"
   a$sss <- as.numeric(a$sss)*10
   a$RRR[a$RRR=="Осадков нет"] <- "0"
   a$RRR[a$RRR=="Следы осадков"] <- "9.8"
   a$RRR[a$RRR=="948.0"] <- -0.9
   a$RRR <- as.numeric(a$RRR)*10
   a$VV[a$VV=="менее 0.1"] <- 0.1
   a$VV[a$VV=="менее 0.05"] <- 0.0
   a$VV <- as.numeric(a$VV)
   a$VV[a$VV>99.9] <- 99.9
   if (F) {
      a$VV <- round(a$VV*10)
   }
   else {
      print(table(is.na(a$VV)))
      a$VV <- cut(a$VV,breaks=c(0,0.2,0.5,1,2,3,5,12,90),right=FALSE)
      print(table(a$VV))
   }
   a$Nh <- mygsub("100%.","100",a$Nh)
   a$Nh <- mygsub("90  или более, но не 100%","95",a$Nh)
   a$Nh <- mygsub("70 – 80%.","75",a$Nh)
   a$Nh <- mygsub("20–30%.","25",a$Nh)
   a$Nh <- mygsub("Облаков нет.","0",a$Nh)
   a$Nh <- mygsub("50%.","50",a$Nh)
   a$Nh <- mygsub("40%.","40",a$Nh)
   a$Nh <- mygsub("60%.","60",a$Nh)
   a$Nh <- mygsub("10%  или менее, но не 0","5",a$Nh)
   a$Nh <- mygsub("Небо не видно из-за тумана и/или других метеорологических явлений.","-1",a$Nh)
   a$Nh <- as.integer(a$Nh)
   if (F) {
      a$H <- mygsub("2500 или более, или облаков нет.","2500",a$H)
      a$H <- mygsub("600-1000","600",a$H)
      a$H <- mygsub("600-1000","600",a$H)
      a$H <- mygsub("300-600","300",a$H)
      a$H <- mygsub("200-300","200",a$H)
      a$H <- mygsub("1000-1500","1000",a$H)
      a$H <- mygsub("2000-2500","2000",a$H)
      a$H <- mygsub("100-200","100",a$H)
      a$H <- mygsub("50-100","50",a$H)
      a$H <- mygsub("1500-2000","1500",a$H)
      a$H <- mygsub("Менее 50","25",a$H)
      a$H[a$H==""] <- "9999"
      a$H <- as.integer(a$H)
   }
   a$mon <- as.integer(format(a$time,"%m"))
   if (F) {
      ind <- which(format(a$time,"%m%d")=="0229")
      print(a[ind,])
   }
   julian <- as.integer(format(a$time,"%j"))
   a$year <- as.integer(format(a$time,"%Y"))
   if (F) {
      ind <- a$mon>=3 & a$year %in% seq(1960,2040,by=4)
      julian[ind] <- julian[ind]-1L
   }
   a$julian <- julian
  # print(unique(a$H))
  # a <- a[which(as.Date(a$time)<=as.Date("2014-10-01")),]
   if (grepl("\\.qs$",rds))
      qs::qsave(a,rds)
   else
      saveRDS(a,rds)
  # print(str(a))
   rm(a)
   0L
}
'f2_figures' <- function(id=21982) {
   wmo <- qs::qread(past("weather",id,".qs"))
   wmo <- wmo[with(wmo,order(time)),]
   colnames(wmo)[mygrep("^E.?$",colnames(wmo))] <- c("soil","snow")
  # print(table(wmo$snow))
   wmo <- subset(wmo,select=c(time,T,Ff,ff3,DD,WW,RRR,tR,sss))#,soil,snow))
   wmo$period <- c(NA,diff(wmo$time)/3600)
   wmo$period[1] <- min(head(wmo$period,6))
   mon <- as.integer(format(wmo$time,"%m"))
  # wmo <- wmo[mon>=10 | mon<=6,]
   wmo <- wmo[mon>=7 & mon<=11,]
   wmo$mon <- as.integer(format(wmo$time,"%m"))
   julian <- as.integer(format(wmo$time,"%j"))
   wmo$year <- as.integer(format(wmo$time,"%Y"))
   ind <- which(wmo$mon<3) ## '<8' for Oct-Jun, '<3' for Jul-Dec
   print(table(wmo$year))
   if (length(ind)) {
      wmo$year[ind] <- wmo$year[ind]-1L
      wmo$season <- factor(paste0(wmo$year,"/",wmo$year+1))
   }
   else
      wmo$season <- factor(wmo$year)
  # wmo$year <- NULL
   wmo$T <- as.numeric(wmo$T)
   year <- rep(2017,nrow(wmo))
   year[ind] <- year[ind]+1L
  # wmo$julian <- julian
  # wmo$julian[ind] <- wmo$julian[ind]+365
   ind2 <- which(wmo$year %in% seq(1960,2040,by=4))
   julian[ind2] <- julian[ind2]-1L
  # wmo$julian[ind2] <- wmo$julian[ind2]+1L
   wmo$xlab <- as.POSIXct(paste(year,julian,format(wmo$time,"%H:%M"))
                         ,"%Y %j %H:%M",tz=tz)
  # print(ursa::series(head(wmo,650)))
   figname <- paste0("wmo",id,"_%d.png")
   if (length(ind <- file.exists(figlist <- sprintf(figname,seq(15)))))
   file.remove(figlist[ind])
   device <- gsub(".*\\.(.+)$","\\1",figname)
   switch(device
         ,png=png(figname,width=840,height=500,type="cairo",bg="transparent")
         ,svg=svg(figname,width=10,height=6,pointsize=8)
         ,pdf=pdf(figname,width=10,height=6,pointsize=8)
         ,stop("unknown graphic device"))
  # wmo <- subset(wmo,year>2007)
   wmo$T[wmo$T<(-45)] <- NA
   wmo$sss[wmo$sss>1100] <- NA
   wmo$RRR[wmo$RRR>2000] <- NA
   wmo$prec <- 0
   wmo$blsn <- 0
   for (s in levels(wmo$season)) {
      ind <- which(wmo$season==s)
      if (!length(ind))
         next
     # message(s)
      RRR <- wmo$RRR[ind]
      RRR[is.na(RRR)] <- 0
      tR <- wmo$tR[ind]
      tR[is.na(tR)] <- Inf
      wmo$prec[ind] <- cumsum(RRR/tR)
      wmo$blsn[ind][mygrep("BLSN",wmo$WW[ind])] <- 1
      wmo$blsn[ind] <- cumsum(wmo$blsn[ind]*wmo$period[ind]/24)
     # wmo$blsn[ind] <- cumsum(wmo$blsn[ind])
     # print(ursa::series(wmo[ind,]))
     # print(wmo$WW[ind][blsn])
   }
  # str(wmo)
  # daily <- aggregate(list(blsn=wmo$blsn),by=list(date=as.Date(wmo$time)),length)
  # print(table(daily$blsn))
  # q()
  # wmo <- subset(wmo,!is.na(T) & T>-45 & sss<1150)
   listYear <- 
   if (F)
      listYear <- sort(sample(listYear,6))
   wmo <- subset(wmo,year %in% listYear)
   require(ggplot2)
   p0 <- theme_bw(base_size=switch(device,png=20,svg=16,pdf=16
                 ,stop("unknown graphic device")))
   dl <- "        %b"
   if (TRUE) {
      p1 <- ggplot(wmo,aes(xlab,T,colour=season))+
           # geom_point(size=1)+
            geom_smooth(span=1/36,se=F,alpha=0.1,size=1)+
            labs(y="Air temperature, °C",x=NULL)+
            scale_x_datetime(date_label=dl
                            ,date_breaks="1 month",date_minor_breaks="1 month")+
            p0
      print(p1)
      saveRDS(p1,"plotly_p1.rds")
     # require(widgetframe)
     # require(plotly)
     # frameWidget(ggplotly(p1))#,height=590,width=790)
   }
   if (TRUE) {
      p2 <- ggplot(wmo,aes(xlab,sss,colour=season))+
            geom_point(size=2)+
            labs(y="Snow depth, mm",x=NULL)+
            scale_x_datetime(date_label=dl
                            ,date_breaks="1 month",date_minor_breaks="1 month")+
            p0
      saveRDS(p2,"plotly_p2.rds")
      print(p2)
   }
   if (FALSE) {
      p3 <- ggplot(wmo,aes(xlab,prec,colour=season))+
            geom_line(size=2)+
            labs(y="Cumulative precipitation, mm",x=NULL)+
            scale_x_datetime(date_label=dl
                            ,date_breaks="1 month",date_minor_breaks="1 month")+
            p0
      print(p3)
   }
   if (TRUE) {
      p4 <- ggplot(wmo,aes(xlab,blsn,colour=season))+
            geom_line(size=2)+
            labs(y="Cumulative blowing snow, days",x=NULL)+
            scale_x_datetime(date_label=dl
                            ,date_breaks="1 month",date_minor_breaks="1 month")+
            p0
      saveRDS(p4,"plotly_p4.rds")
      print(p4)
   }
   if (TRUE) {
      p8 <- plot.windrose(data=subset(wmo,mon %in% c(7,8,9))
                         ,spd=Ff,dir=DD
                        # ,duration=period
                         )+
            facet_wrap(~season,nrow=2)+
            labs(caption="Wind rose for July, August, September")+
            p0
      saveRDS(p8,"plotly_p8.rds")
      print(p8)
   }
   if (TRUE) {
      p9 <- plot.windrose(data=subset(wmo,mon %in% c(8,9,10))
                         ,spd=Ff,dir=DD
                        # ,duration=period
                         )+
            facet_wrap(~season,nrow=2)+
            labs(caption="Wind rose for August, September, October")+
            p0
      saveRDS(p9,"plotly_p9.rds")
      print(p9)
   }
   if (TRUE) {
      p10 <- plot.windrose(data=subset(wmo,mon %in% c(9,10,11))
                         ,spd=Ff,dir=DD
                        # ,duration=period
                         )+
            facet_wrap(~season,nrow=2)+
            labs(caption="Wind rose for September, October, November")+
            p0
      saveRDS(p10,"plotly_p10.rds")
      print(p10)
   }
   dev.off()
   ursa:::.open(figname)
   0L
}
'f3' <- function() {
   data.in <- read.csv(file = "./20130101.csv.gz",
                    col.names = c("date","hr","ws.80","wd.80"))
   str(data.in)
   source("windrose.R")
   svg("windrose.svg")
   require(ggplot2)
   p1 <- plot.windrose(spd = data.in$ws.80,
                   dir = data.in$wd.80)
   print(p1)
   dev.off()
   ursa:::.open("windrose.svg")
   0L
}
invisible({
   stationList <- as.integer(gsub(".*(2\\d{4}).*","\\1"
                                 ,dir(pattern=".*2\\d{4}.*\\.csv(\\.gz$)$")))
   if (T) {
      lapply(stationList,f1_prepare)
   }
   else {
      station <- stationList[3]
     # f1_prepare(station)
     # f2_figures(station)
     # f3()
   }
})
