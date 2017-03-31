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
  scale_fill_manual(values = c("#4DAF4A" , "#984EA3", "#FF7F00" , "#FFFF33" , "#A65628", "seagreen1")) + 
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
  scale_fill_manual(values = c("#4DAF4A" , "#984EA3", "#FF7F00" , "#FFFF33" , "#A65628", "seagreen1")) + 
  theme(text=element_text(size=18, family="KerkisSans")) + xlab("") + ylab("")


## Plotting SBC Party IDs

sbcplot <- data.frame(time = factor(c("Strong Dem.", "Dem.", "Lean Dem.", "Independent", "Lean Rep.", "Rep.", "Strong. Rep")),
                 total_bill = c(1,1,1,0,11,13,10))

sbcplot$time <- factor(sbcplot$time, levels=unique(sbcplot$time))

ggplot(data=sbcplot, aes(x=time, y=total_bill, fill=time)) + geom_bar(stat="identity") + 
  scale_fill_brewer(palette="RdBu", direction=-1) +
  theme(legend.position="none") + xlab("Party Identification") + ylab("Number of Respondents") + 
  theme(text=element_text(size=18, family="KerkisSans")) + ggtitle("Southern Baptist Clergy Party Identification")  +
  theme(plot.title = element_text(hjust = 0.5)) 

## Plotting ABC Party IDs

abc$partyid <- factor(abc$q39, levels=unique(abc$q39))
abc$partyid <- factor(abc$q39, levels = c(1,2,3,4,5,6,7),labels = c("Strong Dem.", "Dem.", "Lean Dem.", "Independent", "Lean Rep.", "Rep.", "Strong. Rep"))
ggplot(na.omit(abc), aes(x= partyid, fill=factor(partyid)), color= factor(partyid)) + geom_bar() + 
  theme(axis.text.x = element_text(angle = 90)) + scale_fill_brewer(palette="RdBu", direction=-1) + 
  theme(legend.position="none") + xlab("Party Identification") + ylab("Number of Respondents") + 
  theme(text=element_text(size=18, family="KerkisSans")) + ggtitle("American Baptist Clergy Party Identification") +
  theme(plot.title = element_text(hjust = 0.5)) 


## Scatter Plotting

clergy$repubid[clergy$repubid==0] <- NA
abc.s <- select(abc, repubid, relcon, denom)
clergy.s <- select(clergy, repubid, relcon, denom)
df <- rbind(clergy.s, abc.s)
df <- filter(df, denom != "Brethren")

ggplot(df, aes(repubid, relcon)) + 
  geom_point(size =2 ,position=position_jitter(width=.5,height=.5), aes(colour = factor(denom))) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") + xlab("Political Ideology") +
  scale_x_continuous(limits = c(1,7), breaks = c(1,2,3,4,5,6,7), labels = c("Strong Dem.", "Dem.", "Lean Dem.", "Independent", "Lean Rep.", "Rep.", "Strong. Rep")) +  
  ylab("Religious Conservatism") + 
  scale_fill_manual(values = c("#4DAF4A" , "#984EA3", "#FF7F00" , "#FFFF33" , "#A65628", "seagreen1" ))  + 
  geom_smooth() + 
  theme(text=element_text(size=18, family="KerkisSans")) + ggtitle("How are Ideology and Theology Related?") +
  theme(plot.title = element_text(hjust = 0.5)) 

## Political Activity By Clergy 
pact <- clergy %>% group_by(denom) %>% summarise(sum_vote = sum(q43_1, na.rm = TRUE), sum_contact = sum(q43_2, na.rm = TRUE), sum_donate = sum(q43_3, na.rm = TRUE), sum_vol1 = sum(q43_4, na.rm = TRUE), sum_volig = sum(q43_5, na.rm = TRUE), sum_urge = sum(q43_6, na.rm = TRUE), sum_protest = sum(q43_7, na.rm = TRUE), sum_rally= sum(q43_8, na.rm = TRUE), sum_study= sum(q43_9, na.rm = TRUE), sum_register= sum(q43_10, na.rm = TRUE))
pact <- filter(pact, denom != "Brethren")
pact$total <- c(27, 218, 120, 55, 35)
pact$pct_vote <- pact$sum_vote/pact$total
pact$pct_contact <- pact$sum_contact/pact$total
pact$pct_donate <- pact$sum_donate/pact$total
pact$pct_vol1 <- pact$sum_vol1/pact$total
pact$pct_volig <- pact$sum_volig/pact$total
pact$pct_urge <- pact$sum_urge/pact$total
pact$pct_protest <- pact$sum_protest/pact$total
pact$pct_rally <- pact$sum_rally/pact$total
pact$pct_study <- pact$sum_study/pact$total
pact$pct_register <- pact$sum_register/pact$total

ppct <- select(pact, denom, pct_vote, pct_contact, pct_donate, pct_vol1, pct_volig, pct_urge, pct_protest, pct_rally, pct_study, pct_register)

abcpct <- data.frame("ABCUSA", .669, .394, .160, .041, .077, .596, .119, .137, .022, .073)
names(abcpct)<-c("denom", "pct_vote", "pct_contact", "pct_donate", "pct_vol1", "pct_volig", "pct_urge", "pct_protest", "pct_rally", "pct_study", "pct_register")
ppct <- rbind(ppct, abcpct)

names(ppct) <- c("denom", "Vote", "Contact Official", "Donate", "Campaign Vol.", "I.G. Vol.", "Encourage Vote", "Protested", "Attend Rally", "Pol. Study Group", "Regist. Drive")

melt_pct <- melt(ppct, id="denom")

ggplot(melt_pct, aes(x=reorder(variable, value), y=value*100, group = denom)) + geom_bar(aes(fill=denom),stat="identity", position= "dodge")  + 
  ggtitle("Political Activities by Denomination") +
  scale_fill_manual(values = c("#4DAF4A" , "#984EA3", "#FF7F00" , "#FFFF33" , "#A65628", "seagreen1" )) +
  xlab("Political Act") + ylab("Percentage of Clergy") + 
  theme(legend.title=element_blank()) + 
  theme(text=element_text(size=16, family="KerkisSans")) + coord_flip() +
  theme(legend.position = "bottom")  +
  theme(plot.title = element_text(hjust = 0.5)) 

## Denominational Involvement

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

dot1 <- ggplot(involve, aes(x=value, y =label)) + geom_point(color = "black", shape=21, size =4, aes(fill = factor(denom))) + 
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom")  +  
  xlim(.25,.75) +
  scale_fill_manual(values = c("#4DAF4A" , "#984EA3", "#FF7F00" , "#FFFF33" , "seagreen1")) + 
  theme(text=element_text(size=18, family="KerkisSans")) + xlab("How Involved Should Your Denomination Be?") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5)) 

df3 <- data.frame("ABC - DJT", .51)
df4 <- data.frame("ABC - HRC", .74)
names(df3)<-c("denom", "value")
names(df4)<-c("denom", "value")

involve <- clergy %>% group_by(denom) %>% summarise(value = mean(involve, na.rm =TRUE))

involve2 <- rbind(df3, df4, involve)
involve2 <- filter(involve2, denom !="Brethren")
involve2 <- filter(involve2, denom != "UMC")

involve2$label <- c("Involvement")

dot2 <- ggplot(involve2, aes(x=value, y =label)) + geom_point(color = "black", shape=21, size =4, aes(fill = factor(denom))) + 
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom")  +  
  xlim(.25,.75) +
  scale_fill_brewer(palette = "Set1") + 
  theme(text=element_text(size=18, family="KerkisSans")) + xlab("How Involved Should Your Denomination Be?") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5)) 

grid.arrange(dot1, dot2, ncol =1)

## RAV Coding

abc$rav1 <- abc$q18_2
abc$rav2 <- abc$q18_3
abc$rav3 <- abc$q18_5
abc$rav4 <- abc$q18_7
abc$rav5 <- abc$q18_9
abc$rav <- abc$rav1 + abc$rav2 + abc$rav3 + abc$rav4 + abc$rav5
abc$rav <- abc$rav/5

clergy$rav1 <- clergy$q26_2
clergy$rav2 <- clergy$q26_3
clergy$rav3 <- clergy$q26_5
clergy$rav4 <- clergy$q26_7
clergy$rav5 <- clergy$q26_9
clergy$rav <- clergy$rav1 + clergy$rav2 + clergy$rav3 + clergy$rav4 + clergy$rav5
clergy$rav <- clergy$rav/5


## Plotting Single Scale of RAV
rav <- clergy %>% group_by(denom) %>% summarise(rav = mean(rav, na.rm = TRUE))
df5 <- data.frame("ABCUSA", 3.366146)
names(df5)<-c("denom", "rav")
rav <- rbind(rav, df5)
rav$label <- c("Religious Authority")

ggplot(rav %>% filter(denom != "Brethren"), aes(x=rav, y =label)) + geom_point(color = "black", shape=21, size =4, aes(fill = factor(denom))) + 
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom")  +  
  xlim(2,5) +
  scale_fill_manual(values = c("#4DAF4A" , "#984EA3", "#FF7F00" , "#FFFF33" , "#A65628", "seagreen1")) + 
  theme(text=element_text(size=18, family="KerkisSans")) + xlab("Religious Authority Scale") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5)) 

## Plotting Each RAV Question

rav1 <- clergy %>% group_by(denom) %>% summarise(stepout = mean(rav1, na.rm = TRUE), construct = mean(rav2, na.rm = TRUE), congmake = mean(rav3, na.rm = TRUE), postmodern = mean(rav4, na.rm = TRUE), manybible = mean(rav5, na.rm = TRUE))
rav2 <- abc %>% group_by(denom) %>%   summarise(stepout = mean(rav1, na.rm = TRUE), construct = mean(rav2, na.rm = TRUE), congmake = mean(rav3, na.rm = TRUE), postmodern = mean(rav4, na.rm = TRUE), manybible = mean(rav5, na.rm = TRUE))
rav3 <- rbind(rav1, rav2)

rav3 <- filter(rav3, denom != "Brethren")

names(rav3) <- c("denom", "Clergy Step Out of Way", "Cong. Construct Salvation", "Cong. Makes Gospel", "Adapt to Postmodern Culture", "Many Interpt. of Bible")


melt_rav <- melt(rav3, id="denom")
melt_rav$newval <- 5- melt_rav$value

ggplot(melt_rav, aes(x = newval, y = variable))  +
  geom_point(color = "black", shape=21, size =4, aes(fill = factor(denom))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +xlab("<- More Religious Authority : Less Religious Authority ->") + ylab("") + xlim(0,5.5)  +
  scale_x_continuous(breaks = c(0,1,2,3,4), labels = c("SD", "D", "Neither", "A", "SA")) +  
  scale_fill_manual(values = c("#4DAF4A" , "#984EA3", "#FF7F00" , "#FFFF33" , "#A65628", "seagreen1")) + 
  theme(text=element_text(size=18, family="KerkisSans"))



