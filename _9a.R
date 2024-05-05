'bymonth' <- function() {
   n <- sample(seq(1,12),1)
   ind <- sort(sample(seq(1,12),n))
   ind <- c(4,5,6,9)
   print(ind)
   if (length(ind)==2) {
      s1 <- seq(ind[1],ind[2])
      print(s1)
      s2 <- seq(ind[2],ind[1]+12)
      if (length(s1)<length(s2))
         return(s1)
      s2[s2>12] <- s2[s2>12]-12
      return(s2)
   }
   integer()
}
bymonth()
