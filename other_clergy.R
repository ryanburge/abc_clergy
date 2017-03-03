library(haven)
library(dplyr)
library(car)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(extrafont)
library(gridExtra)
library(grid)

clergy <- read_dta("C:/Users/Ryan Burge/Dropbox/Clergy 2014/clergy 3-11-14a.dta")

clergy <- filter(clergy, denom <= 6)


clergy$denom<-Recode(clergy$denom,"1='SBC';
                    2='UMC';
                    3='Brethren';
                    4='Greek Orthodox';
                    5='PCUSA';
                    6='RCA'",
                    as.factor=TRUE)

## Going to Compare the Theology of Each Clergy 
clergy$relcon1 <- 6- clergy$q68_1
clergy$relcon2 <- 6- clergy$q68_2
clergy$relcon3 <- 6- clergy$q68_3
clergy$relcon4 <- 6- clergy$q68_4
clergy$relcon5 <- 6- clergy$q68_5
clergy$relcon6 <- 6- clergy$q68_6
clergy$relcon <- clergy$relcon1 + clergy$relcon2 + clergy$relcon3 + clergy$relcon4 + clergy$relcon5 + clergy$relcon6
clergy$relcon <- clergy$relcon/6

theodata <- clergy %>% group_by(denom) %>% 
summarise(devil = mean(relcon1, na.rm =TRUE), literal = mean(relcon2, na.rm =TRUE), return = mean(relcon3, na.rm =TRUE), virgin = mean(relcon4, na.rm =TRUE), absolute = mean(relcon5, na.rm =TRUE), male = mean(relcon6, na.rm =TRUE))

## There was no good way to do this so I wrote the theodata df to a csv and then just added the ABCUSA results. It's still called theodata.csv

## This is the dataset with ABCUSA as 1 group
theodata <- read.csv("theodata.csv")

theodata$denom <- factor(theodata$denom, levels=unique(theodata$denom))


theo1 <- ggplot(theodata, aes(x = value, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(denom))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +xlab("Higher Values = More Conservative") + ylab("Theology Question") + xlim(1,5.5)  +
  scale_x_continuous(breaks = c(1,2,3,4,5), labels = c("SD", "D", "Neither", "A", "SA")) +  
  scale_fill_manual(values = c("#4DAF4A" , "#984EA3", "#FF7F00" , "#FFFF33" , "#A65628", "#F781BF")) + 
  theme(text=element_text(size=18, family="KerkisSans"))

## This is the dataset with ABCUSA as 2 groups
theodata2 <- read.csv("theodata2.csv")

theo2 <- ggplot(theodata2, aes(x = value, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(denom))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +xlab("Higher Values = More Conservative") + ylab("Theology Question") + xlim(1,5.5)  +
  scale_x_continuous(breaks = c(1,2,3,4,5), labels = c("SD", "D", "Neither", "A", "SA")) +  
  scale_fill_brewer(palette = "Set1") + 
  theme(text=element_text(size=18, family="KerkisSans"))

grid.arrange(theo1, theo2, ncol =2)


## Party ID Measure

clergy$repubid <- Recode(clergy$q63, "8=0")
clergy$repubid[clergy$repubid==0] <- NA

pid <- clergy %>% group_by(denom) %>% summarise(pid = mean(repubid, na.rm =TRUE))
df1 <- data.frame("ABCUSA", 4.04)
names(df1)<-c("denom", "pid")
pid <- rbind(pid, df1)
pid <- pid[-1,]
pid$label <- c("PID")

ggplot(pid, aes(x=pid, y =label)) + geom_point(color = "black", shape=21, size =4, aes(fill = factor(denom))) + 
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(1,7), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Dem.", "Dem.", "Lean Dem.", "Independent", "Lean Rep.", "Rep.", "Strong. Rep")) +  
  scale_fill_manual(values = c("#4DAF4A" , "#984EA3", "#FF7F00" , "#FFFF33" , "#A65628", "#F781BF")) + 
  theme(text=element_text(size=18, family="KerkisSans")) + xlab("") + ylab("")


## Plotting SBC Party IDs

sbcplot <- data.frame(time = factor(c("Strong Democrat", "Moderate Democrat", "Lean Democrat", "Independent","Lean Republican", "Moderate Republican", "Strong Republican"), levels=c("Strong Democrat", "Moderate Democrat", "Lean Democrat", "Independent","Lean Republican", "Moderate Republican", "Strong Republican")),
                 total_bill = c(1,1,1,0,11,13,10))

ggplot(data=sbcplot, aes(x=time, y=total_bill, fill=time)) + geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90)) + scale_fill_brewer(palette="RdBu", direction=-1) +
  theme(legend.position="none") + xlab("Party Identification") + ylab("Number of Respondents") + 
  theme(text=element_text(size=18, family="KerkisSans")) + ggtitle("                        Southern Baptist Clergy Party Identification")



## Scatter Plotting

clergy$repubid[clergy$repubid==0] <- NA
abc.s <- select(abc, repubid, relcon, denom)
clergy.s <- select(clergy, repubid, relcon, denom)
df <- rbind(abc.s, clergy.s)
df <- filter(df, denom != "Brethren")

ggplot(df, aes(repubid, relcon)) + 
  geom_point(size =2 ,position=position_jitter(width=.5,height=.5), aes(colour = factor(denom))) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") + xlab("") +
  scale_x_continuous(limits = c(1,7), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Dem.", "Dem.", "Lean Dem.", "Independent", "Lean Rep.", "Rep.", "Strong. Rep")) +  
  ylab("Religious Conservatism") + scale_fill_brewer(palette = "Set1")  + geom_smooth() + 
  theme(text=element_text(size=18, family="KerkisSans"))


## Involvement

clergy$involve1 <- 8 - clergy$q2_1
clergy$involve2 <- 8 - clergy$q2_2
clergy$involve3 <- 8 - clergy$q2_3
clergy$involve1 <- Recode(clergy$involve1, "8=0")
clergy$involve2 <- Recode(clergy$involve2, "8=0")
clergy$involve3 <- Recode(clergy$involve3, "8=0")

clergy$involve <- clergy$involve1 + clergy$involve2 + clergy$involve3
clergy$involve <- clergy$involve/21

involve <- clergy %>% group_by(denom) %>% summarise(value = mean(involve, na.rm =TRUE))


df2 <- data.frame("ABCUSA", .5176933)
names(df2)<-c("denom", "value")
involve <- rbind(involve, df2)
involve <- involve[-1,]
involve <- involve[-5,]
involve$label <- c("Involvement")

ggplot(involve, aes(x=value, y =label)) + geom_point(color = "black", shape=21, size =4, aes(fill = factor(denom))) + 
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom")  +  
  xlim(.25,.75) +
  scale_fill_manual(values = c("#4DAF4A" , "#984EA3", "#FF7F00" , "#FFFF33" , "#F781BF")) + 
  theme(text=element_text(size=18, family="KerkisSans")) + xlab("How Involved Should Your Denomination Be?") + ylab("")