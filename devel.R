#'
#+
source("resources.R")
# station_map()
options(device=function() png(width=960,height=720))
ursa:::.elapsedTime("wmo -- start")
wmo <- get_wmo()
ursa:::.elapsedTime("wmo -- finish")
station <- c('Dikson'="20674",'Sterlegova'="20476",'SopKarga'="20871"
            ,'TSYK'="20471",'Belyi'="20667") |> head(1)
year <- seq(2023,2005) |> head(5) |> tail(5)
season <- seasonLUT()[seq(8,11)]
prm <- "T"
str(list(prm=prm,station=unname(station),year=unname(year),season=unname(season)))
da <- subset_wmo(wmo,prm=prm,station=station,year=year,season=season)
str(da)
q()
#+ out.width=480
if (F) {
   station_map(station)
}
#+
if (T) {
   p1 <- plot_timeseries(da,prm=prm,span=0,tuning=c("all","value","category")[1])
   print(p1)
  # ursa:::widgetize(plotly::ggplotly(p1))
}
#+
if (F) {
  # p1 <- plot_timeseries(wmo,prm="T",station=station,year=year,season=season,span=0)
   p2 <- plot_windrose(da)
   print(p2)
}
