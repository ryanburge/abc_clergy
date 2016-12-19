library(haven)
library(psych)
library(dplyr)

abc <- read_dta("abc_final.dta")

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
abc[is.nan(abc)] <- 0


abc$rav1 <- abc$q18_2
abc$rav2 <- abc$q18_3
abc$rav3 <- abc$q18_5
abc$rav4 <- abc$q18_7
abc$rav5 <- abc$q18_9
abc$rav <- abc$rav1 + abc$rav2 + abc$rav3 + abc$rav4 + abc$rav5
abc$rav <- abc$rav/5

rav <- select(abc, rav1, rav2, rav3, rav4, rav5)

cronbach(rav[,1:5]) #.843

abc$relcon1 <- 6- abc$q44_1
abc$relcon2 <- 6- abc$q44_2
abc$relcon3 <- 6- abc$q44_3
abc$relcon4 <- 6- abc$q44_4
abc$relcon5 <- 6- abc$q44_5
abc$relcon6 <- 6- abc$q44_6
abc$relcon <- abc$relcon1 + abc$relcon2 + abc$relcon3 + abc$relcon4 + abc$relcon5 + abc$relcon6
abc$relcon <- abc$relcon/6

relcon <- select(abc, relcon1, relcon2, relcon3, relcon4, relcon5, relcon6)

cronbach(relcon[,1:6]) #.906

ggplot(abc, aes(x=relcon, y=rav))  + geom_point(shape =1, position=position_jitter(width=.1,height=.1)) + geom_smooth(method=lm) + xlab("Religious Conservatism") + ylab("Religious Authority")

