require(ggplot2)
set.seed(12345)
hillest<-c(rep(1.1,100*4*3)+0*rnorm(100*4*3,sd=0.2),
       rep(1.9,100*4*3)+0*rnorm(100*4*3,sd=0.2))
rep<-rep(1:100,4*3*2)
process<-rep(rep(c("Process 1","Process 2","Process 3","Process 4"),each=100),3*2)
memorypar<-rep(rep(c("0.1","0.2","0.3"),each=4*100),2)
tailindex<-rep(c("1.1","1.9"),each=3*4*100)
ex5<-data.frame(hillest=hillest,rep=rep,process=process,memorypar=memorypar, tailindex=tailindex)
ex5 <- ex5[ex5$process=="Process 1",]
ex5$process <- NULL
str(ex5)
#stat_sum_df <- function(fun, geom="crossbar", ...) {stat_summary(fun.data=fun, geom=geom, ...) }

dodge <- position_dodge(width=0.9) 
p <- ggplot(ex5,aes(x=tailindex ,y=hillest,color=memorypar)) 
p <- p + geom_point(position=position_jitterdodge(dodge.width=0.9,dodge.height=0.9))
# p <- p + geom_jitter(position=dodge)
p
