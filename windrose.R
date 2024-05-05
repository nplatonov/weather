# https://stackoverflow.com/questions/17266780/wind-rose-with-ggplot-r
'plot.windrose' <- function(data,
                      spd,
                      dir,
                      spdres = 2,
                      dirres = 45/2,
                      spdmin = 0,
                      spdmax = 20,
                      spdseq = NULL,
                     # palette = "YlGnBu",
                      countmax = NA,
                      caption = "Wind speed (m/s)",
                      duration = NA,
                      debug = 0) {
# https://stackoverflow.com/questions/17266780/wind-rose-with-ggplot-r
# Look to see what data was passed in to the function
 # if ((exists("data"))&&(is.character(data[,dir])))
  if (!missing(data)) {
     mc <- as.list(match.call())
     if (is.symbol(mc$spd))
        spd <- as.character(mc$spd)
     if (is.symbol(mc$dir))
        dir <- as.character(mc$dir)
     if (is.symbol(mc$duration))
        duration <- as.character(mc$duration)
  }
  cond1 <- (!missing(data))&&(is.character(data[,dir]))
  cond2 <- !cond1&&is.character(dir)
  if ((cond1)||(cond2)) {
     if (cond1)
        rhumb <- data[,dir]
     if (cond2)
        rhumb <- dir
     rdir <- rep(NA,length(rhumb))
     rdir[rhumb=="N"] <- 0*45/2
     rdir[rhumb=="NNE"] <- 1*45/2
     rdir[rhumb=="NE"] <- 2*45/2
     rdir[rhumb=="ENE"] <- 3*45/2
     rdir[rhumb=="E"] <- 4*45/2
     rdir[rhumb=="ESE"] <- 5*45/2
     rdir[rhumb=="SE"] <- 6*45/2
     rdir[rhumb=="SSE"] <- 7*45/2
     rdir[rhumb=="S"] <- 8*45/2
     rdir[rhumb=="SSW"] <- 9*45/2
     rdir[rhumb=="SW"] <- 10*45/2
     rdir[rhumb=="WSW"] <- 11*45/2
     rdir[rhumb=="W"] <- 12*45/2
     rdir[rhumb=="WNW"] <- 13*45/2
     rdir[rhumb=="NW"] <- 14*45/2
     rdir[rhumb=="NNW"] <- 15*45/2
     if (cond1) 
        data[,dir] <- rdir
     else
        dir <- rdir
     rm(rdir)
  }
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (!missing(data)) {
    # data <- data[,c(spd,dir)]
    # colnames(data) <- c("spd","dir")
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }
  data <- data[!is.na(data[,spd]) & !is.na(data[,dir]),]
  if (!is.na(duration)) {
     if (is.character(duration))
         dur <- data[,duration]
      else
         dur <- duration
     # print(table(dur))
      if (TRUE) {
         j1 <- cumsum(dur)
         j2 <- c(1,head(j1,-1)+1)
         res <- data[rep(1,max(j1)),]
         rownames(res) <- NULL
         for (i in seq_along(j1)) {
            res[j2[i]:j1[i],] <- data[rep(i,dur[i]),]
         }
      }
      else {
         res <- NULL
         for (i in seq(nrow(data))) {
            res <- rbind(res,data[rep(i,dur[i]),])
         }
      }
      data <- res
  }
  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA

  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  if (TRUE) {
     a <- pretty(data[,spd],n=9)
    # a <- ursa:::.deintervale(ursa::colorize(data[,spd]
    #                             ,stretch="linear",nbreak=9,tail=0)$colortable)
     spdmin <- min(a)
     spdmax <- max(a)
     spdseq <- unique(c(0,a))
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1

  # create the color map
  ##~ spd.colors <- colorRampPalette(brewer.pal(min(max(3,
                                                    ##~ n.colors.in.range),
                                                ##~ min(9,
                                                    ##~ n.colors.in.range)),                                               
                                            ##~ palette))(n.colors.in.range)

  spd.colors <- rev(viridis::plasma(n.colors.in.range))
 # spd.colors <- ursa::cubehelix(n.colors.in.range)
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax,
                          "-",
                          max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)
  # clean up the data
  data. <- na.omit(data)

  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  if (dirres==45/2) {
     dir.labels <- c("N","NNE","NE","ENE","E","ESE","SE","SSE","S"
                        ,"SSW","SW","WSW","W","WNW","NW","NNW","N")
  }
  else if (dirres==45) {
     dir.labels <- c("N","NE","E","SE","S","SW","W","NW","N")
  }
  else if (dirres==90) {
     dir.labels <- c("N","E","S","W","N")
  }
  else {
     dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                     paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                           "-",
                           seq(3*dirres/2, 360-dirres/2, by = dirres)),
                     paste(360-dirres/2,"-",dirres/2))
  }
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
 # data$dir.binned[nchar(data$dir.binned)==3] <- ""

  # Run debug if required ----
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")       
  }  

  # deal with change in ordering introduced somewhere around version 2.2
  if(packageVersion("ggplot2") > "2.2"){    
   # cat("Hadley broke my code\n")
    data$spd.binned = with(data, factor(spd.binned, levels = rev(levels(spd.binned))))
    spd.colors = rev(spd.colors)
  }

  # create the plot ----
  mylabel <- levels(dir.binned)
  if (dirres==45/2)
     mylabel[nchar(mylabel)>2] <- ""
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned)) +
    geom_bar() + 
    scale_x_discrete(drop = FALSE,labels = mylabel) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = caption, 
                      values = spd.colors,
                      drop = FALSE) +
    labs(x=NULL,y=NULL)+
    theme(axis.title.x = element_blank())#,axis.text.x=element_blank())

  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }

  # print the plot
  # print(p.windrose)  

  # return the handle to the wind rose
  return(p.windrose)
}
