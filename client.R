'wmo_select_and_plot' <- function(prm,var=NULL) {
  # return(invisible(NULL))
  # cat(paste0("wmo_select_and_plot: prm=",dQuote(prm),"\n"))
   opt <- gsub("prm","opt",prm)
   adj <- gsub("prm","adj",prm)
   fillCol(flex=c(NA,1)
      ,fillRow(flex=NA
         ,selectInput(prm,"" #,"Параметр по выбору"
              ,choices=if (is.character(var)) c('Какой параметр выбрать?'=var)
                                         else c('Параметр по выбору'="")
              ,selected="",multiple=FALSE,width="230px")
         ,HTML("&nbsp;")
         ,selectInput(opt,"",choices=c('Уточнение'="")
              ,selected="",multiple=FALSE,width="150px")
         ,HTML("&nbsp;")
         ,sliderInput(adj,label=c("Сглаживание","")[1]
                     ,min=0,max=1,step=0.01,value=0,width="135px")
      )
      ,renderPlot({
         req(input[["draw"]])
         req(nchar(input[[prm]])>0)
         req(nchar(input[[opt]])>0)
         req(rvStationSelected() -> station)
         req(rvYearSelected() -> year)
         req(rvSeasonSelected() -> season)
         ##~ req(input[[prm]] %in% colnames(rvWMO()))
         req((length(year)==1)||(length(station)==1))
         ret <- plot_timeseries(rvWMO()
                        ,input[[prm]]
                        ,station=station
                        ,year=year
                        ,season=season
                       # ,span=rvSpan()
                        ,span=input[[adj]]
                        ,tuning=input[[opt]]
                        )
         ret
      })
   )
}
'uiStationMap' <- function(id=NULL) {
  # renderImage({
      req(input[["draw"]])
      req(id)
      w <- session$clientData[[paste0('output_',id,'_width')]]
      h <- session$clientData[[paste0('output_',id,'_height')]]
      req(is.numeric(h))
      req(is.numeric(w))
      ref0 <- round(c(h,w))-c(30,50) # ref0 <- c(400,400)
      station <- station0[station0$wmo %in% as.integer(input[["station"]]),]
      req(spatial_count(station)>0)
      ret <- station_map(station,ref=ref0)
     # return(list(src=ret))
      list(src=ret
          ,'width'="auto"
          ,'height'="100%"
         # ,'max-height'="100%"
          ,'object-fit'=c("scale-down","contain")[2]
         # ,class="propersize"
          ) #,display="inline-block")
  # },deleteFile=TRUE)
}
