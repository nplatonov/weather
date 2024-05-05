invisible({
   ursa:::.elapsedTime("start")
   opW <- options(warn=10)
   appnameList <- "taimyr"
   appname <- commandArgs(TRUE)
   if (length(appname)!=1)
      appname <- tail(appnameList,1)
   else
      appname <- match.arg(appname,appnameList)
   wd <- setwd(".")
   require(rsconnect)
   options(rsconnect=list(name="nplatonov"
                         ,token="8984D8B8C8AA4B6DF21A00D53EDA3C89"
                         ,secret="V+DzJpl+pgLnOEiZ72ePRvCfkDjb2CtTG09riqEy"))
   opShiny <- getOption("rsconnect")
   if (is.null(opShiny))
      stop("Authentification data are not receieved")
   res1 <- try(with(opShiny,setAccountInfo(name=name,token=token,secret=secret)))
   if (inherits(res1,"try-error"))
      str(res1)
   chunk1 <- dir(path=".",pattern=paste0("^(app|dixon|server|client|resource|windrose|.+\\d{5})"
                                        ,".*\\.(R|Rmd|geojson|qs)$"),full.names=TRUE)
   chunk2 <- dir(path=c("assets","site_libs"),pattern=".+",full.names=TRUE)
   appfiles <- c(chunk1,chunk2)
   print(appfiles)
   res2 <- try(deployApp(appName=appname
                        ,appFiles=appfiles
                        ,appTitle="Taymir za mir"
                        ,account=opShiny$name))
   if (inherits(res2,"try-error"))
      str(res2)
   options(opW)
   setwd(wd)
   ursa:::.elapsedTime("finish")
})
warnings()
