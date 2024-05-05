#' ---
#' zoutput: flexdashboard::flex_dashboard
#' sevin: true
#' ---
#'
lutWMO <- rbind(data.frame(wmo="23022",name="",lon=61+43/60,lat=69+45/60)
               ,data.frame(wmo="20946",name="",lon=59+04/60,lat=70+27/60)
               ,data.frame(wmo="23103",name="",lon=53+46/60,lat=68+56/60)
               ,data.frame(wmo="23112",name="",lon=58+00/60,lat=68+48/60)
               ,data.frame(wmo="23114",name="",lon=56+38/60,lat=68+35/60)
               ,data.frame(wmo="23205",name="",lon=53+02/60,lat=67+38/60)
               )
if (F) {
   plutil::ursula()
   a <- spatialize(lutWMO,style="web")
   d3 <- expand.grid(date=c(1,3,4),time=seq(6,18,by=3))
   d3 <- apply(d3,1,\(x) sprintf("2023-05-%02d %02d:00",x[1],x[2]))
   d3 <- as.POSIXct(d3,tz="Europe/Moscow")
   loc <- spatialize("../gpx/track_points.geojson")
   ind <- sapply(d3,\(x) which.min(abs(x-loc$time)))
   d <- ursa:::.dist2(loc[ind,],a,summarize=FALSE)
   d$time <- d3
   d$wmo <- a$wmo[d$ind]
   d <- d[order(d$time),]
   print(d)
   print(d3)
   str(loc)
  # loc <- spatialize()
   if (T) {
      xy <- spatial_coordinates(a)
      session_grid(expand=1.1)
      compose_open(retina=2,fileout="assets/wmo-location.png")
      panel_new()
      panel_raster("mapnik")
      panel_plot(a["wmo"])
      panel_text(xy[,1],xy[,2],a$wmo,pos=1)
      panel_decor(margin=c(T,T,F,F),language="ru",scalebar.pos="topright",coast=F)
     # panel_legend(x="topleft")
      compose_close(bpp=8)
      q()
   }
}
lutWW <- c(ST="буря",BLSN="метель",BR="туман",SHSN="ливневый снег"
          ,SHRA="ливневый дождь",RA="дождь",SN="снег",DR="позёмок",SH="ливень")
knit <- ursa:::.isKnitr()
if (knit)
   options(knitr.kable.NA = '')
patt <- "^(2\\d{4})\\s*-\\s*(.+)\\.csv(\\.gz)*$"
list1 <- dir(pattern=patt)
lutWMO <- gsub(patt,"\\2",basename(list1))
names(lutWMO) <- gsub(patt,"\\1",basename(list1))
lutWMO <- gsub("^м\\s","м. ",lutWMO)
wmo <- lapply(list1,\(fname) {
   message(fname)
   a <- read.csv2(fname,skip=6,check.names=FALSE)
   a[,!nchar(colnames(a))] <- NULL
   a$wmo <- gsub(".*(^\\d{5}).*","\\1",basename(fname))
   colnames(a)[1] <- "time"
   a$time <- as.POSIXct(a$time,format="%d.%m.%Y %H:%M")
   a <- a[order(a$time),]
   a$hh <- format(a$time,"%H")
   a$dd <- format(a$time,"%d")
   a$ddhh <- format(a$time,"%d%h")
   a <- a[a$dd %in% c("01","03","04") & a$hh %in% c("06","09","12","15","18"),]
   # a <- a[!(a$ddhh %in% c("0106"))]
   rownames(a) <- NULL
   a$T <- as.numeric(a$T)
   a$Po <- as.numeric(a$Po)
   a$P <- as.numeric(a$P)
   a$Pa <- as.numeric(a$Pa)
   a$Td <- as.numeric(a$Td)
   a$VV <- as.numeric(a$VV)
   a$Tn <- as.numeric(a$Tn)
   a$Tx <- as.numeric(a$Tx)
   if (F) {
      if (all(is.na(a$ff10)))
         a$ff10 <- NULL
      if (all(is.na(a$ff3)))
         a$ff3 <- NULL
      if (all(is.na(a$E)))
         a$E <- NULL
      if (all(is.na(a$Tg)))
         a$Tg <- NULL
   }
   w <- apply(with(a,cbind(WW,W1,W2)),1,\(x) {
      paste(unique(x),collapse=" ")
   })
   w <- gsub("буря","ST*",w,ignore.case=TRUE)
   w <- gsub("метель","BLSN*",w,ignore.case=TRUE)
   w <- gsub("туман","BR*",w,ignore.case=TRUE)
   w <- gsub("ливневой снег","SHSN*",w,ignore.case=TRUE)
   w <- gsub("ливневой дождь","SHRA*",w,ignore.case=TRUE)
   w <- gsub("ливень","SH*",w,ignore.case=TRUE)
   w <- gsub("дождь","RA*",w,ignore.case=TRUE)
   w <- gsub("снег","SN*",w,ignore.case=TRUE)
   w <- gsub("поземок","DR*",w,ignore.case=TRUE)
   w <- gsub("([А-Яа-я]|\\(|\\)|\\.|\\/|\\s|\\,)","",w)
   w <- gsub("\\*$","",w)
   w <- gsub("\\*"," ",w)
   w <- unname(sapply(w,function(x) {
      paste(unique(strsplit(x,split="/")[[1]]),collapse="/")
   }))
   a$WW <- w
   rm(w)
   a$W1 <- a$W2 <- NULL
   a$DD <- gsub("Ветер, дующий с севера","N",a$DD,ignore.case=TRUE)
   a$DD <- gsub("Ветер, дующий с северо-северо-востока","NNE",a$DD,ignore.case=TRUE)
   a$DD <- gsub("Ветер, дующий с юго-юго-запада","SSW",a$DD,ignore.case=TRUE)
   a$DD <- gsub("Ветер, дующий с юго-запада","SW",a$DD,ignore.case=TRUE)
   a$DD <- gsub("Ветер, дующий с востоко-юго-востока","ESE",a$DD,ignore.case=TRUE)
   a$DD <- gsub("Ветер, дующий с юго-востока","SE",a$DD,ignore.case=TRUE)
   a$DD <- gsub("Ветер, дующий с юга","S",a$DD,ignore.case=TRUE)
   a$DD <- gsub("Ветер, дующий с запада","W",a$DD,ignore.case=TRUE)
   a$DD <- gsub("Штиль, безветрие","C",a$DD,ignore.case=TRUE) ## calm
   a$DD <- gsub("Ветер, дующий с северо-востока","NE",a$DD,ignore.case=TRUE)
   a$DD <- gsub("Ветер, дующий с востока","E",a$DD,ignore.case=TRUE)
   a$DD <- gsub("Ветер, дующий с северо-запада","NW",a$DD,ignore.case=TRUE)
   a$DD <- gsub("Ветер, дующий с западо-северо-запада","WNW",a$DD,ignore.case=TRUE)
   a$DD <- gsub("Ветер, дующий с северо-северо-запада","NNW",a$DD,ignore.case=TRUE)
   a$DD <- gsub("Ветер, дующий с юго-юго-востока","SSE",a$DD,ignore.case=TRUE)
   a$DD <- gsub("Ветер, дующий с западо-юго-запада","WSW",a$DD,ignore.case=TRUE)
   a$DD <- gsub("Ветер, дующий с востоко-северо-востока","ENE",a$DD,ignore.case=TRUE)
   a$DD <- gsub("Переменное направление","VRB",a$DD,ignore.case=TRUE)
   a$DD[nchar(a$DD)==0] <- "---"
   a$N <- gsub("100%.","100",a$N,ignore.case=TRUE)
   a$N <- gsub("90  или более, но не 100%","95",a$N,ignore.case=TRUE)
   a$N <- gsub("70 – 80%.","75",a$N,ignore.case=TRUE)
   a$N <- gsub("20–30%.","25",a$N,ignore.case=TRUE)
   a$N <- gsub("Облаков нет.","0",a$N,ignore.case=TRUE)
   a$N <- gsub("50%.","50",a$N,ignore.case=TRUE)
   a$N <- gsub("40%.","40",a$N,ignore.case=TRUE)
   a$N <- gsub("60%.","60",a$N,ignore.case=TRUE)
   a$N <- gsub("10%  или менее, но не 0","5",a$N,ignore.case=TRUE)
   a$N <- gsub("Небо не видно из-за тумана и/или других метеорологических явлений.","-1",a$N,ignore.case=TRUE)
   a$N <- as.integer(a$N)
   a$Nh <- gsub("100%.","100",a$Nh,ignore.case=TRUE)
   a$Nh <- gsub("90  или более, но не 100%","95",a$Nh,ignore.case=TRUE)
   a$Nh <- gsub("70 – 80%.","75",a$Nh,ignore.case=TRUE)
   a$Nh <- gsub("20–30%.","25",a$Nh,ignore.case=TRUE)
   a$Nh <- gsub("Облаков нет.","0",a$Nh,ignore.case=TRUE)
   a$Nh <- gsub("50%.","50",a$Nh,ignore.case=TRUE)
   a$Nh <- gsub("40%.","40",a$Nh,ignore.case=TRUE)
   a$Nh <- gsub("60%.","60",a$Nh,ignore.case=TRUE)
   a$Nh <- gsub("10%  или менее, но не 0","5",a$Nh,ignore.case=TRUE)
   a$Nh <- gsub("Небо не видно из-за тумана и/или других метеорологических явлений.","-1",a$Nh,ignore.case=TRUE)
   a$Nh <- as.integer(a$Nh)
   a$H <- gsub("2500 или более, или облаков нет.","2500",a$H,ignore.case=TRUE)
   a$H <- gsub("600-1000","600",a$H,ignore.case=TRUE)
   a$H <- gsub("300-600","300",a$H,ignore.case=TRUE)
   a$H <- gsub("200-300","200",a$H,ignore.case=TRUE)
   a$H <- gsub("1000-1500","1000",a$H,ignore.case=TRUE)
   a$H <- gsub("2000-2500","2000",a$H,ignore.case=TRUE)
   a$H <- gsub("100-200","100",a$H,ignore.case=TRUE)
   a$H <- gsub("50-100","50",a$H,ignore.case=TRUE)
   a$H <- gsub("1500-2000","1500",a$H,ignore.case=TRUE)
   a$H <- gsub("Менее 50","25",a$H,ignore.case=TRUE)
   a$H[a$H==""] <- "9999"
   a$H <- as.integer(a$H)
   a$RRR[a$RRR=="Осадков нет"] <- "0"
   a$RRR[a$RRR=="Следы осадков"] <- "9.8"
   a$RRR[a$RRR=="948.0"] <- -0.9
   a$Tn <- a$Tx <- a$'E\'' <- a$sss <- a$Td <- a$tR <- a$Pa <- a$P <- a$ff10 <- NULL
   a$dd <- a$hh <- a$ddhh <- NULL
   a
})
q()
if (F) {
   names(wmo) <- gsub(".*(^\\d{5}).*","\\1",basename(list1))
   str(wmo)
   print(lapply(wmo,colnames) |> do.call(rbind,args=_))
} else {
   wmo <- do.call(rbind,wmo)
   wmo$WW[wmo$WW=="NA"] <- ""
   if (T) {
      if (all(is.na(wmo$ff10)))
         wmo$ff10 <- NULL
      if (all(is.na(wmo$ff3)))
         wmo$ff3 <- NULL
      if (all(is.na(wmo$E)))
         wmo$E <- NULL
      if (all(is.na(wmo$Tg)))
         wmo$Tg <- NULL
   }
}
lutL <- unique(wmo$Cl)
nameL <- rep("---",length(lutL))
nameL[which(is.na(lutL))] <- "неизв."
nameL[grep("облаков нет",lutL)] <- "На ярусе без облаков"
nameL[grep("не из кучевых",lutL)] <- "Слоис­то­куче­вые не из кучевых"
nameL[grep("волокнистые",lutL)] <- "Волок­нис­тые"
nameL[grep("лысые",lutL)] <- "Кучево­дожде­вые лысые"
nameL[grep("на разных уровнях",lutL)] <- "На разных уровнях"
nameL[grep("разорванные",lutL)] <- "Слоис­тые, но не облака плохой погоды"
nameL[grep("из кучевых",lutL)] <- "Слоис­то­куче­вые из кучевых"
names(lutL) <- nameL
wmo$Cl <- nameL[match(wmo$Cl,lutL)]
lutM <- unique(wmo$Cm)
nameM <- rep("---",length(lutM))
nameM[which(is.na(lutM))] <- "неизв."
nameM[grep("облаков нет",lutM)] <- "На ярусе без облаков"
nameM[grep("непросвечивающие",lutM)] <- "Высоко­сло­истые плотные"
nameM[grep("уплотняются",lutM)] <- "Высоко­ку­чевые уплот­ня­ются"
nameM[grep("башенкообразные",lutM)] <- "Башенко­об­разные"
nameM[grep("на одном уровне",lutM)] <- "Одно­уров­невые"
nameM[grep("или.+либо",lutM)] <- "Всё сложно"
names(lutM) <- nameM
wmo$Cm <- nameM[match(wmo$Cm,lutM)]
lutH <- unique(wmo$Ch)
nameH <- rep("---",length(lutH))
nameM[which(is.na(lutM))] <- "неизв."
nameH[grep("перист.+нет",lutH)] <- "Перистых нет"
nameH[grep("покрывающие",lutH)] <- "Покры­ва­ющие всё небо"
nameH[grep("нитевидные",lutH)] <- "Преоб­ла­дают ните­видные"
nameH[grep("преобладают",lutH)] <- "Преоб­ла­дают перисто­кучевые"
nameH[grep("плотные",lutH)] <- "Плотные"
nameH[grep("уплотняющиеся",lutH)] <- "Уплот­ня­ю­щиеся"
names(lutH) <- nameH
wmo$Ch <- nameH[match(wmo$Ch,lutH)]
wmo$time <- plutil::format_time(wmo$time,"%d %B %Hч")
cname <- colnames(wmo)
cname[cname=="time"] <- ""
cname[cname=="Cm"] <- "Обла­ка ср. яруса"
cname[cname=="Ch"] <- "Обла­ка выс. яруса"
cname[cname=="Cl"] <- "Обла­ка ниж. яруса"
cname[cname=="RRR"] <- "Осад­ков за 12ч, мм"
cname[cname=="WW"] <- "Явле­ния по­го­ды"
cname[cname=="VV"] <- "Гориз. види­мость, км"
cname[cname=="DD"] <- "Нап­равл. ветра"
cname[cname=="T"] <- "°C"
cname[cname=="N"] <- "Общая облач­ность, %"
cname[cname=="H"] <- "Ниж­ний край облач­ности"
cname[cname=="Nh"] <- "Обла­ков нижай­шего яруса, %"
cname[cname=="U"] <- "Влаж­ность, %"
cname[cname=="Po"] <- "Давле­ние на ур. стан­ции, мм рт ст"
cname[cname=="Ff"] <- "Ско­рость вет­ра, м/с"
cname[cname=="ff10"] <- "Макс. ско­рость вет­ра за 10', м/с"
cname[cname=="ff3"] <- "Порывы вет­ра, м/с"
colnames(wmo) <- cname
#str(wmo$WW)
if (T)
   wmo <- by(wmo,wmo$wmo,\(x) x)
if (is.data.frame(wmo))
   knitr::kable(wmo)
if (!is.data.frame(wmo)) {
   level2 <- lapply(names(wmo),function(station) knitr::knit_expand(file="rp5-lev2.Rmd"))
   if (F)
      cat(unlist(level2),sep="\n")
}
#' `r if (!is.data.frame(wmo)) knitr::knit(text=level2)`
#'
#+lutWW
knitr::kable(lutWW,caption="Обозначения явлений погоды",col.names=NULL)
#'
#+lutL
knitr::kable(data.frame(abbr=names(lutL),value=lutL)
            ,caption="Сокращения облаков нижнего яруса",col.names=NULL,row.names=FALSE)
#'
#+lutM
knitr::kable(data.frame(abbr=names(lutM),value=lutM)
            ,caption="Сокращения облаков среднего яруса",col.names=NULL,row.names=FALSE)
#'
#+lutH
knitr::kable(data.frame(abbr=names(lutH),value=lutH)
            ,caption="Сокращения облаков высокого яруса",col.names=NULL,row.names=FALSE)
#'
