exchange <- reactiveValues(ind=NA,prm=character())
'rvStationSelected' <- reactive(input[["station"]])
'rvYearSelected' <- reactive({
   y <- grep("\\D",input[["year"]],value=TRUE,invert=TRUE)
   y[order(y,decreasing=TRUE)]
})
'rvSeasonSelected' <- reactive({
   grep("\\D",input[["month"]],value=TRUE,invert=TRUE)
})
'rvSpan' <- reactive(input[["span"]])
'rvYearList' <- reactive({
   if (F) {
      wmo <- rvWMO()
      a <- subset(wmo[wmo$station %in% input[["station"]] &
                      wmo$mon %in% match(rvSeasonSelected(),unname(seasonLUT())),])
      yearList <- sort(unique(format(a$time,"%Y")),decreasing=TRUE)
      if (F & Sys.Date()<as.Date("2024-07-01"))
         yearList <- yearList[yearList!=2024]
   }
   else
      yearList <- as.character(seq(2023,2005))
   yearList
})
'rvWMO' <- reactive({
   wmo <- get_wmo()
   if (T)
      return(wmo)
   wmos <- subset_wmo(wmo
                   # ,station=rvStationSelected()
                   # ,year=rvYearSelected()
                   # ,season=rvSeasonSelected()
                    )
   if (!is.null(wmos))
      return(wmos)
   wmo
})
'rvStation' <- reactive({
   wmo <- rvWMO()
   unique(wmo$station)
})
'rvSubsetWMO' <- function() {
   wmo <- subset_wmo(rvWMO()
                    ,station=rvStationSelected()
                    ,year=rvYearSelected()
                    ,season=rvSeasonSelected()
                    )
   wmo
}
if (F) observeEvent(list(input[["year"]],input[["season"]],input[["station"]]),{
   updateCheckboxInput(session,"draw",value=FALSE)
})
if (T) observeEvent(list(input[["station"]],input[["month"]]),{
   yearList <- rvYearList()
   ss <- length(input[["station"]])
   ns <- ifelse(ss>1,1,1)
   if (!length(input[["year"]]))
      sy <- head(yearList,ns)
   else if (length(input[["year"]])==1)
      sy <- input[["year"]]
   else if (ss>1)
      sy <- head(input[["year"]],1)
   else
      sy <- input[["year"]]
   if (("2023" %in% yearList)&&(length(input[["station"]])==1)) {
      names(yearList) <- yearList
      if (T)
         yearList <- c(head(yearList,2)
                      ,'Свежий'="head1"
                      ,'Три года'="head3"
                      ,'Пять лет'="head5"
                      ,'Десять лет'="head10"
                      ,'Весь срок'=paste0("tail",length(yearList))
                      ,tail(yearList,-2))
   }
   updateSelectInput(session,"year",choices=yearList,selected=sy)
})
if (T) observeEvent(input[["month"]],{
   if ("none" %in% input[["month"]])
      updateSelectInput(session,"month",selected=c('Не выбрано'="aaa"))
   else if (length(input[["month"]])>92) {
      choices <- names(seasonLUT())[match(input[["month"]],seasonLUT())]
      choices <- input[["month"]]
      names(choices) <- names(seasonLUT())[match(choices,seasonLUT())]
      if (!anyNA(choices)) {
         choices <- c(choices,'Сбросить'="reset")
         updateSelectInput(session,"month",selected=input[["month"]],choices=choices)
      }
   }
   else if (T) {
      ind <- match(input[["month"]],seasonLUT())
      ind <- by_month(ind)
      if (length(ind)<93)
         choices <- seasonLUT()
      else
         choices <- "ccc"
      choices <- c('Не выбрано'="bbb",choices)
      updateSelectInput(session,"month"
                       ,selected=if (length(ind)) seasonLUT()[ind] else "ddd"
                      # ,choices=choices
                       )
   }
})
if (T) observeEvent(input[["year"]],{
   sy <- input[["year"]]
   if (length(ind <- grep("\\D",sy))>0) {
      sy <- as.integer(gsub("\\D","",sy[ind]))
      yearList <- rvYearList()
      sy <- head(yearList,sy)
      updateSelectInput(session,"year",selected=sy)
   }
   req(length(input[["station"]])>1)
   if (!length(sy))
      sy <- 2010
   if (length(sy)>1)
      sy <- tail(sy,1)
   updateSelectInput(session,"year",selected=sy
                   # ,choices=seq(2020,2030)
                    )
   
})
observe({
   cat("observe 'prmX':\n")
   prm <- colnames(rvWMO()) |> lut_WMO()
   default <- c('Параметр по выбору'="missed")
   for (p in c("prm1","prm2","prm3","prm4")) {
      if (isTRUE(input[[p]] %in% prm)) {
         updateSelectInput(session,p
                          ,selected=c(input[[p]],default)[1]
                          ,choices=c(default,prm)[-1]
                          )
      }
   }
})
'updateOptByPrm' <- function(p) {
   observeEvent(input[[p]],{
      choices <- c('Отобразить всё'="all",'По значениям'="value",'По категориям'="category")
      if (isTRUE(input[[p]] %in% c("H","Ff","WW","VV"))) {
        # choices[1] <- "all"
         updateSelectInput(session,gsub("prm","opt",p),selected=choices[1],choices=choices)
      }
      else {
         updateSelectInput(session,gsub("prm","opt",p),selected=choices[1],choices=choices[1])
      }
   })
}
updateOptByPrm("prm1")
updateOptByPrm("prm2")
updateOptByPrm("prm3")
updateOptByPrm("prm4")
