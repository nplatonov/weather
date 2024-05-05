staffOnly <- T & nchar(Sys.getenv("MSOFFICE"))>0
lib.loc <- "D:/ongoing/.arcnet/Rlibs"
require(ursa,lib.loc=if (F & dir.exists(lib.loc)) lib.loc else NULL)
suppressMessages(require(ggplot2))
isShiny <- isNamespaceLoaded("shiny")
tz <- "Asia/Krasnoyarsk"
device <- "png"
if (isNamespaceLoaded("ggplot2"))
   p0 <- theme_minimal(base_size=switch(device,png=16,svg=16,pdf=16
                 ,stop("unknown graphic device")))
dl <- "       %b"
count <- 0L
station0 <- spatial_read("dixon")
source("windrose.R")
'stationLUT' <- function() {
   if (F) {
      .stationLUT <- c('о. Тройной'=20471
                     ,'м. Стерлегова'=20476
                     ,'о. Диксон'=20674
                     ,'Соп. Карга'=20871)
   }
   ret <- station0$wmo
   names(ret) <- if (ursa:::.isKnitr()) station0$name else station0$alt
   ret
}
'seasonLUT' <- function() {
   if (ursa:::.isKnitr())
      return(c('Январь'="01",'Февраль'="02",'Март'="03",'Апрель'="04"
           ,'Май'="05",'Июнь'="06",'Июль'="07",'Август'="08",'Сентябрь'="09"
           ,'Октябрь'="10",'Ноябрь'="11",'Декабрь'="12"))
   m <- sprintf("%02d",seq(12))
   b <- format(as.Date(paste("2021",m,"15",sep="-")),"%B")
   names(m) <- b
   m
}
'get_wmo' <- function() {
   if (isShiny) {
      showModal(modalDialog(title="Получение WMO"
                           ,"Нужно немного подождать"
                           ,size="s",easyClose=T,footer=NULL))
      on.exit(removeModal())
   }
   list1 <- dir(pattern="^.*2\\d{4}.*\\.qs$")
   wmoID <- as.integer(gsub("\\D*(2\\d{4})\\D*","\\1",basename(list1)))
   wmo <- lapply(wmoID,\(x) {
      fname <- list1[grep(x,basename(list1))]
      a <- qs::qread(fname)
      a <- cbind(station=x,a)
      a
   }) |> do.call(rbind,args=_)
   return(wmo)
   if (FALSE) {
      str(wmo)
     # ursa:::.elapsedTime("A0")
     # str(as.character(wmo$time))
      q()
      if (T) {
         ind <- which(wmo$mon<3) ## '<8' for Oct-Jun, '<3' for Jul-Dec
         # print(table(wmo$year))
         if (length(ind)) {
            wmo$year[ind] <- wmo$year[ind]-1L
            wmo$season <- factor(paste0(wmo$year,"/",wmo$year+1))
         } else {
            wmo$season <- factor(wmo$year)
         }
      }
      # wmo$year <- NULL
      wmo$T <- as.numeric(wmo$T)
      year <- rep(2017,nrow(wmo))
      year[ind] <- year[ind]+1L
      print(year)
      q()
      # wmo$julian <- julian
      # wmo$julian[ind] <- wmo$julian[ind]+365
      ind2 <- which(wmo$year %in% seq(1960,2040,by=4))
      julian[ind2] <- julian[ind2]-1L
      # wmo$julian[ind2] <- wmo$julian[ind2]+1L
      wmo$xlab <- as.POSIXct(paste(year,julian,format(wmo$time,"%H:%M"))
                            ,"%Y %j %H:%M",tz=tz)
   }
   wmo
}
'by_month' <- function(ind) {
   if (isShiny)
      cat("By months:\n")
   if (is.character(ind)) {
      if ("none" %in% ind)
         return(integer())
      else if ("all" %in% ind)
         return(seq(12))
      op <- options(warn=-1)
      ind2 <- as.integer(ind)
      if (!anyNA(ind2))
         ind <- ind2
      rm(ind2)
      options(op)
   }
   if (length(ind)==1)
      return(ind)
   if (length(ind)==2) {
      if (ind[2]>ind[1])
         return(seq(ind[1],ind[2]))
      s2 <- seq(ind[1],ind[2]+12L)
      s2[s2>12] <- s2[s2>12]-12L
      return(s2)
   }
   d <- diff(ind)
   if (any(d>1))
      return(integer())
   return(ind)
}
'subset_wmo' <- function(wmo,prm,station,year,season) {
   if (isShiny)
      cat("Subset WMO:\n")
  # wmo <- wmo[wmo$station %in% c(20674),] ## devel
   da <- wmo
   if ((!missing(season))&&(length(season))) {
      m <- by_month(season)
      da <- da[da$mon %in% m,]
   }
   else
      m <- seq(12)
   aux <- data.frame(julian=da$julian,ylab=da$year,yyyy=2021)
   ind <- da$mon>=3 & da$year %in% seq(1960,2040,by=4)
   aux$julian[ind] <- aux$julian[ind]-1L
   if (length(ind <- which(diff(m)!=1))>0) {
      m2 <- m[seq(ind+1,length(m))]
      ind2 <- da$mon %in% m2
      aux$yyyy[!ind2] <- aux$yyyy[!ind2]-1L
      aux$ylab[!ind2] <- aux$ylab[!ind2]+1L
   }
   if ((!missing(year))&&(length(year))) {
      if (length(ind <- which(aux$ylab %in% year))>0) {
         da <- da[ind,]
         aux <- aux[ind,]
      }
   }
   if ((!missing(station))&&(length(station))) {
      if (length(ind <- which(da$station %in% station))>0) {
         da <- da[ind,]
         aux <- aux[ind,]
      }
      st <- table(da$station)
      if (length(st)>1) {
         sy <- as.integer(names(tail(table(aux$ylab),1)))
         ind <- aux$ylab %in% sy
         da <- da[ind,]
         aux <- aux[ind,]
      }
   }
   st <- table(da$station)
   if (length(st)==1) {
      if ((length(m)==1)||(head(m,1)<tail(m,1))) {
         da$slab <- as.character(aux$ylab)
      }
      else {
         da$slab <- sapply(aux$ylab,\(x) paste(c(x-1,x),collapse="/"))
      }
   }
   else
      da$slab <- da$station
   da$xlab <- as.POSIXct(paste(aux$yyyy,aux$julian,format(da$time,"%H:%M"))
                         ,"%Y %j %H:%M",tz=tz)
   if ((!FALSE)&&(!missing(prm))&&(length(prm))) {
      da <- da[,c("station","time","mon","year","julian","slab","xlab",prm)]
      if ((FALSE)&&(length(prm)==1)) {
         da$'prm' <- da[[prm]]
         da[[prm]] <- NULL
      }
   }
  # da2prn <- da[,c("station","time","T","year","mon","slab","xlab")]
  # rownames(da2prn) <- NULL
  # print(ursa::series(da2prn,12))
   da
}
'lut_WMO' <- function(prm) {
   lut <- c('T'="Температура воздуха, °C"
           ,'Ff'="Скорость ветра, м/с"
           ,'VV'="Горизонтальная видимость"
           ,'WW'="Явления погоды"
           ,'Cl'="Облака нижнего яруса"
           ,'Nh'="Количество облаков Cl"
           ,'H'="Нижний край облачности"
           ,'RRR'="Осадков за 12ч, мм"
           ,'U'="Влажность"
           ,'N'="Общая облачность"
           ,'sss'="Снежный покров"
           )
   ind <- match(prm,names(lut))
   ind2 <- which(!is.na(ind))
   names(prm) <- prm
   if (!length(ind2))
      return(prm)
   names(prm)[ind2] <- lut[ind[ind2]]
   prm
}
'plot_timeseries' <- function(wmo,prm="T",station,year,season,span=NA,tuning=NULL) {
   if (isShiny)
      cat("Plot timeseries",dQuote(prm),":\n")
   if (isShiny)
      str(list(season=season,station=station,year=year,span=span,tuning=tuning))
   da <- subset_wmo(wmo,prm=prm,station=station,year=year,season=season)
   if (missing(season))
      season <- unique(da$mon)
   da$prm <- da[[prm]]
   if (F & isShiny)
      str(list(count=count,season=unique(da$mon),year=unique(da$year)
              ,station=unique(da$station),span=span,tuning=tuning))
  # print(ursa::series(da[,c("xlab","prm")]))
  # q()
  if (F & !is.numeric(da$prm)) {
      p <- table(da$prm)
      y <- sapply(strsplit(names(p),split="\\W"),\(x) {
         paste(unique(sort(gsub("\\d","",x))),collapse="/")
      })
      names(y) <- names(p)
      da$prm <- y[match(da$prm,names(p))]
   }
   else if (F) {
      if (prm=="H") {
         if (T)
            da$prm[da$prm>3000] <- 3000
         else
            da <- da[da$prm<3000,]
      }
   }
   if (prm %in% c("sss")) {
      da$prm[da$prm<0] <- 5
      da <- da[!is.na(da$prm),]
     # da$prm[is.na(da$prm)] <- 0
   }
   if (prm %in% c("Cl")) {
      keepPrm <- da$prm
      lutPrm <- unique(da$prm)
      nameL <- rep("---",length(lutPrm))
      nameL[which(is.na(lutPrm))] <- "неизв."
      nameL[grep("облаков нет",lutPrm)] <- "На ярусе без облаков"
      nameL[grep("не из кучевых",lutPrm)] <- "Слоис­то­куче­вые не из кучевых"
      nameL[grep("волокнистые",lutPrm)] <- "Волок­нис­тые"
      nameL[grep("лысые",lutPrm)] <- "Кучево­дожде­вые лысые"
      nameL[grep("на разных уровнях",lutPrm)] <- "На разных уровнях"
      nameL[grep("разорванные",lutPrm)] <- "Слоис­тые, но не облака плохой погоды"
      nameL[grep("из кучевых",lutPrm)] <- "Слоис­то­куче­вые из кучевых"
      nameL[grep("мощные",lutPrm)] <- "Средние или мощные на одном уровне"
      names(lutPrm) <- nameL
      da$prm <- nameL[match(da$prm,lutPrm)]
      da$prm <- gsub("­","",da$prm)
      if (any(nchar(keepPrm[da$prm=="---"])>0))
         stop("Inclomplete LUT for ",dQuote(prm))
      da$prm[grep("---",da$prm)] <- "Не указано"
   }
   if (prm %in% c("VV")) {
      da <- da[!is.na(da$prm),]
   }
   if (prm=="H") {
      kwd <- c('9999'="Неизвестно")
      da$prm[grep("2500.+",da$prm)] <- ">2500 или лучше"
      da$prm[!nchar(da$prm)] <- names(kwd)
      v <- gsub("^(>)*(\\d+)(\\D.*|$)","\\2",da$prm)
      v[grep("Менее 50",v)] <- "25"
      v <- as.integer(v)
      tv <- table(as.integer(v))
      lev <- match(as.character(v),names(tv))
      da1 <- data.frame(prm=da$prm,value=v,lev=lev)
      da2 <- da1[!duplicated(da1[,c("prm","lev")]),]
      da2 <- da2[order(da2$lev),]
      da2$ok <- da2$value>500
      da2$ok[da2$ok=="FALSE"] <- "Низкая облачность"
      da2$ok[da2$ok=="TRUE"] <- "Высокая облачность"
      da$prm[da$prm==names(kwd)] <- kwd
      da2$prm[da2$prm==names(kwd)] <- kwd
      da3 <- da
      ind <- match(da3$prm,da2$prm)
      print(summary(ind))
      da3$prm <- as.character(da2$ok[ind])
      lev <- c(da2$prm,unique(da2$ok))
      print(da2)
     # da3$prm <- NULL
     # da$prm <- NULL
      if (isTRUE(tuning=="category"))
         da <- da3
      else if (isTRUE(tuning=="value"))
         da3 <- NULL
      else
         da <- rbind(da,da3)
      da$prm <- factor(da$prm,levels=lev,ordered=TRUE)
      print(summary(da$prm))
   }
   if (T & ursa:::.isKnitr())
      label <- names(lut_WMO(prm))
   else
      label <- prm
   if (is.na(span)) {
      if (F) {
         d <- as.numeric(diff(range(da$xlab)),units="days")
         n <- length(unique(da$xlab))
         span <- 20/d
         print(c(n=n))
      }
      else
         span <- 0
   }
   if (length(ind <- na.omit(match(da$slab,stationLUT()))))
      da$slab <- names(stationLUT())[ind]
   slab <- unique(da$slab)
   ##~ print(table(da[[prm]]))
   ##~ print(table(da[["prm"]]))
   if (prm %in% c("WW")) {
      da <- da[,c("station","time","prm","mon","slab","xlab")]
      da$prm[!nchar(da$prm)] <- "---"
      da$prm <- gsub("(\\d|\\:)","",da$prm)
      da <- by(da,da$prm,\(x) {
         p2 <- unique(strsplit(x$prm[1],split="/")[[1]])
         x2 <- lapply(p2,\(p) {
            x$prm <- p
            x
         })
         x2 <- do.call(rbind,x2)
         x2
      })
      da <- do.call(rbind,da)
      if (isTRUE(tuning=="value"))
         da <- da[da$prm!="---",]
      else if (isTRUE(tuning=="category")) {
         da$prm[da$prm!="---"] <- "+++"
      }
      if (T | ursa:::.isKnitr()) {
         lut <- c('SN'="Снег"
                 ,'RA'="Дождь"
                 ,'ST'="Буря"
                 ,'BLSN'="Метель"
                 ,'SHSN'="Ливневой снег"
                 ,'SHRA'="Ливневой дождь"
                 ,'SH'="Ливень"
                 ,'BR'="Туман"
                 ,'DR'="Позёмок"
                 ,'---'="Без явлений"
                 ,'+++'="Осадки и др."
                 )
         ind <- match(da$prm,names(lut))
         ind2 <- which(!is.na(ind))
         if (length(ind2)>0) {
            da$prm[ind2] <- lut[ind[ind2]]
            if (isTRUE(tuning=="category"))
               da$prm <- factor(da$prm,levels=rev(tail(lut,2)),ordered=TRUE)
         }
      }
   }
   if (isTRUE(tuning=="category")) {
      if (prm %in% c("Ff","VV")) {
         th <- switch(prm,Ff=c('м/с'=5),VV=c('км'=4L),stop("need to define"))
         if (is.factor(da$prm)) {
            lev <- strsplit(levels(da$prm)[th],split="(\\[|\\(|,|\\)|\\])")[[1]]
            lev <- lev[nchar(lev)>0][1]
            names(lev) <- names(th)
         }
         else
            lev <- th
         da3 <- da
         kwd <- paste0(c("Менее ",""),lev," ",names(lev),c(""," и более"))
         ind <- if (prm %in% c("Ff","WW")) seq(2,1) else seq(1,2)
         da3$prm <- kwd[1]
         da3$prm[as.numeric(da$prm)>th] <- kwd[2]
         da3$prm <- factor(da3$prm,levels=kwd[ind],ordered=TRUE)
         da3 <- da3[order(da3$time),]
         da <- da3
      }
   }
   ta <- table(da$prm)
   ta <- ta[ta>0]
   if (span>=1.0) {
      s <- by_month(season)
      sm <- table(da$mon)
      if (length(sm)<4) {
         da$mon <- names(seasonLUT())[da$mon]
         m3 <- factor(da$mon,levels=names(seasonLUT())[s],ordered=TRUE)
      }
      else {
         m3 <- factor(unname(seasonLUT)[da$mon],levels=unname(seasonLUT())[s],ordered=TRUE)
      }
      da$m3 <- m3
     # print(ursa::series(da[,c("station","year","mon","time","xlab","prm")]))
      da$slab <- factor(da$slab)
     # str(da[,c("m3","prm","slab")])
     # q()
      p1 <- ggplot(da,aes(m3,prm,color=slab))
      if (is.numeric(da$prm)) {
        # dodge <- position_dodge(width=0.9)
         p1 <- p1+geom_violin(position=position_dodge(preserve="total")
                            # ,adjust=0.3
                             )
         p1 <- p1+geom_jitter(size=1
                             ,alpha=0.15
                             ,position=position_jitterdodge(jitter.height=0.5)
                             )
         p1 <- p1+stat_summary(fun.y="mean"
                              ,geom="crossbar"
                              ,mapping=aes(ymin=..y.., ymax=..y..)
                              ,width=0.4
                              ,position=position_dodge()
                              ,show.legend=FALSE
                              )
      }
      else {
         if (F) {
            p1 <- p1+geom_jitter(NULL
                               # ,height=0.3
                               # ,width=0.3
                               # ,position=position_jitterdodge(jitter.height=0.2
                               #                               ,jitter.width=0.2)
                               # ,position=position_dodge(width=1.2,preserve="total")
                                ,position=position_dodge2()
                               # ,position=position_jitter()
                               # ,position=ggforce::position_auto(scale = FALSE)
                                ,alpha=0.5
                                ,size=3
                                )
           # p1 <- p1+geom_point(position=position_jitterdodge(dodge.width=0.9))
           # p1 <- p1+geom_point()
         }
         else {
            if (length(ta)==2) {
               span <- 0.9999
               p1 <- ggplot(da,aes(xlab,after_stat(count),fill=as.character(prm)))
               p1 <- p1+geom_density(position="fill")
               p1 <- p1+facet_grid(slab~.,scales="free",as.table=FALSE,switch="both")
              # p1 <- p1+scale_x_discrete(labels=element_blank())#,position="top")
              # p1 <- p1+scale_y_discrete(labels=element_blank())
            }
            else {
               p1 <- ggplot(da,aes(slab,prm,color=slab))
               p1 <- p1+scale_x_discrete(labels=element_blank())#,position="top")
               p1 <- p1+scale_y_discrete(labels=element_blank())
               p1 <- p1+geom_jitter(alpha=0.25,size=3
                  ,position=position_jitterdodge(jitter.height=0.5 #+1/length(ta)
                                                ,jitter.width=2+1/length(ta)
                                                ,dodge.width=0))
              # p1 <- p1+facet_grid(prm~m3,scales="free",space="free",as.table=FALSE,switch="both")
               p1 <- p1+facet_grid(prm~m3,scales="free",as.table=FALSE,switch="both")
               ##~ p1 <- p1+theme(strip.text.x=element_blank())
               ##~ p1 <- p1+theme(strip.text.y=element_blank())
            }
         }
      }
   }
   else {
     # da <- da[,c()]
      p1 <- ggplot(da,aes(xlab,prm,colour=slab))
      size <- length(slab)^(-0.75)
      if ((is.numeric(da$prm))&&(span>0)) {
         p1 <- p1+geom_smooth(method="loess",span=span,se=length(slab)<3,alpha=0.4,size=size)
         if (length(slab)==1)
            p1 <- p1+geom_point(size=1,alpha=0.5)
      }
      else if (T) {
         if ((is.character(da$prm))||(is.factor(da$prm))) {
            if ((span>0)&&(length(ta)==2)) {
               p1 <- ggplot(da,aes(xlab,after_stat(count),fill=prm))
               p1 <- p1+geom_density(position="fill",adjust=span)
               p1 <- p1+facet_grid(slab~.,scales="free",as.table=FALSE,switch="both")
            }
            else {
               p1 <- ggplot(da,aes(xlab,slab,colour=slab))
               p1 <- p1+scale_y_discrete(labels=element_blank())#,position="top")
               p1 <- p1+geom_jitter(size=2,height=0.25,alpha=0.4)
               p1 <- p1+facet_grid(prm~.,scales="free",as.table=FALSE,switch="both")
            }
         }
         else
            p1 <- p1+geom_point(size=size*2)
      }
      else
         p1 <- p1+geom_line(size=size)
   }
   if (staffOnly)
      count <<- count+1L
   if (staffOnly)
      p1 <- p1+labs(y=paste0(count,": ",label),x=NULL)
   else
      p1 <- p1+labs(y=label,x=NULL)
   if (span<1)
      p1 <- p1+scale_x_datetime(date_label=dl
                               ,date_breaks="1 month",date_minor_breaks="1 month")
   p1 <- p1+p0
   if ((span<1)&&(length(ta)==2))
      p1 <- p1+theme(axis.text.y.left=element_blank())
   p1 <- p1+theme(legend.position="bottom",legend.title=element_blank())
   p1 <- p1+theme(strip.text.y.left=element_text(angle=0))
   p1 <- p1+theme(axis.ticks.y.left=element_blank())
   p1 <- p1+theme(panel.grid.major.y=element_blank())
  # p1 <- p1+theme(strip.text.x=element_blank())
  # p1 <- p1+theme(strip.text.y=element_blank())
  # p1 <- p1+labs(caption=paste("count:",count))
   if (isShiny)
      cat("            plotting finished!\n")
   p1
}
'plot_windrose' <- function(wmo,station,year,season) {
   if (isShiny)
      cat("Plot windrose:\n")
   da <- subset_wmo(wmo,station=station,year=year,season=season)
   tm <- table(da$mon)
   ts <- table(da$slab)
   if (length(indS <- na.omit(match(names(ts),stationLUT())))>0) {
      da$slab <- names(stationLUT())[match(da$slab,names(ts))]
      ts <- table(da$slab)
   }
   da$mon <- names(seasonLUT())[da$mon]
   p8 <- plot.windrose(data=da,spd=Ff,dir=DD)
   if (length(tm)<length(ts)) {
      p8 <- p8+facet_grid(vars(mon),vars(slab))
   }
   else {
      p8 <- p8+facet_grid(vars(slab),vars(mon))
   }
   p8 <- p8+p0
   p8
}
'station_map' <- function(station,ref0=c(320,480)) {
   if (missing(station))
      station <- station0
   if (!is_spatial(station)) {
      station1 <- station0[station0$wmo %in% station,]
      if (!spatial_count(station1))
         station1 <- station0[grep(station1[1],station0$name,ignore.case=TRUE),]
      if (!spatial_count(station1))
         station1 <- station0[grep(station1[1],station0$alt,ignore.case=TRUE),]
      if (!spatial_count(station1))
         station <- station0
      else
         station <- station1
   }
   if (isShiny)
      ref0 <- ref0*2
   isWeb <- FALSE
   station <- ursa:::spatialize(station,style=ifelse(isWeb,"web","laea"),resetGrid=TRUE)
   g1 <- session_grid(expand=1.2)
   v1 <- ursa(g1,"cellsize")
   if (isWeb) {
      v2 <- 2*6378137*pi/(2^(1:21+8))
     # print(format(v1,sci=FALSE),quote=FALSE)
     # print(format(v2,sci=FALSE),quote=FALSE)
      zoom <- which.min(abs(v1-v2))
      zoom0 <- 8
      if (zoom>zoom0)
         g1 <- regrid(g1,zoom=2^(zoom-zoom0))
   }
   else {
      cs <- 300
      if (v1<cs)
         g1 <- regrid(g1,expand=cs/v1,mul=v1/cs)
   }
   session_grid(consistent_grid(g1,ref=ref0))
  # session_grid(consistent_grid(g1,ref=c(400,400)),expand=c(1.8,1.1))
  # compose_open(scale=1,pointsize=5)
   compose_open(scale=1)#,retina=0.5)
   panel_new("white")
  # ursa:::.ursaOptions()
   if (isWeb)
      panel_raster("opentopomap")
   panel_decor(coast=!isWeb,language="ru",coast.fill="#00000010",margin=c(T,T,F,F)
              ,scalebar.pos="bottomright")
   panel_plot(station,col="black")
   xy <- spatial_coordinates(station)
   for (i in seq(nrow(xy)))
      panel_annotation(x=xy[i,1],y=xy[i,2],label=station$name[i],adj=c(2,-0.5))
   compose_close()
}
