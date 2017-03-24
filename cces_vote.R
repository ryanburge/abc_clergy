library(haven)
library(dplyr)
library(weights)
library(forcats)
library(extrafont)
library(car)

cces16 <- read_dta("C:/Users/Ryan Burge/Desktop/cces.dta")

cces12 <- read_dta("C:/Users/Ryan Burge/Desktop/cces12.dta")

cces08 <- read_dta("C:/Users/Ryan Burge/Desktop/cces2008.dta")

cces08$candidate <- Recode(cces08$CC327, "1= 'Republican';
                                          2= 'Democrat';
                                          3= 'Libertarian';
                                          4= 'Green';
                                          5= 'Libertarian';
                                          6= 'Green';
                                          7= 'Other';
                                          8= 'Not Vote';
                                          9= 'Not Sure'; else = NA")



cces12$candidate<-Recode(cces12$CC410a,"1='Democrat';
                    2='Republican';
                    3= 'Other';
                    4= 'Not Vote';
                    7= 'Not Sure';
                    8= 'Skipped'; else = NA")



cces16$candidate<-Recode(cces16$CC16_410a,"1='Republican';
                    2='Democrat';
                    3='Libertarian';
                    4='Green';
                    5= 'Other';
                    6= 'Not Vote';
                    7= 'Not Sure';
                    8= 'Evan McMullin'; else = NA")

cces08$abc <- Recode(cces08$V222, "2=1; else=0")
cces08$sbc <- Recode(cces08$V222, "1=1; else=0")


vote08 <- cces08 %>%  filter(abc ==1 & complete.cases(candidate)) %>% 
  count(candidate, wt = V201) %>% 
  mutate(weight = prop.table(n), year = c("2008")) %>% mutate(weight = weight*100) %>%  arrange(desc(weight))


cces12$abc <- Recode(cces12$religpew_baptist, "2=1; else=0")
cces12$sbc <- Recode(cces12$religpew_baptist, "1=1; else=0")

vote12 <- cces12 %>%  filter(abc ==1 & complete.cases(candidate)) %>% 
  count(candidate, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n), year = c("2012")) %>% mutate(weight = weight*100) %>%  arrange(desc(weight))


cces16$abc <- Recode(cces16$religpew_baptist, "2=1; else=0")
cces16$sbc <- Recode(cces16$religpew_baptist, "1=1; else=0")

vote16 <- cces16 %>%  filter(abc ==1 & complete.cases(candidate)) %>% 
  count(candidate, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), year = c("2016")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

plot <- rbind(vote08, vote12, vote16)


plot$candidate <- factor(plot$candidate, levels=unique(plot$candidate))




ggplot(plot, aes(1, weight, fill= fct_rev(candidate))) + geom_col()  + coord_flip() +  
theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + 
  theme(legend.position="bottom") +
  ggtitle("American Baptist Voters in Presidential Elections") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("black","goldenrod1" , "forestgreen", "darkorchid4", "azure4",  "firebrick1", "dodgerblue3")) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="")  + facet_grid(year ~ .) 


cces16$bagain <- Recode(cces16$pew_bornagain, "1=1; else=2")
cces12$bagain <- Recode(cces12$pew_bornagain, "1=1; else=2")

cces16$bagain<-Recode(cces16$bagain,"1='Born Again or Evangelical';
                    2='Not Born Again or Evangelical';
                         else = NA")

cces12$bagain<-Recode(cces12$bagain,"1='Born Again or Evangelical';
                    2='Not Born Again or Evangelical';
                         else = NA")

cces08$bagain<-Recode(cces08$V215, "1='Born Again or Evangelical';
                    2='Not Born Again or Evangelical';
                         else = NA")


ba16 <- cces16 %>%  filter(abc ==1) %>% 
  count(bagain, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), year = c("2016")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

ba12 <- cces12 %>%  filter(abc ==1) %>% 
  count(bagain, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n), year = c("2012")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

ba08 <- cces08 %>%  filter(abc ==1) %>% 
  count(bagain, wt = V201) %>% 
  mutate(weight = prop.table(n), year = c("2008")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

baplot <- rbind(ba08, ba12, ba16)
baplot <- filter(baplot, bagain == "Born Again or Evangelical")
baplot$denom <- c("American Baptist")

ba16 <- cces16 %>%  filter(sbc ==1) %>% 
  count(bagain, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), year = c("2016")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

ba12 <- cces12 %>%  filter(sbc ==1) %>% 
  count(bagain, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n), year = c("2012")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

ba08 <- cces08 %>%  filter(sbc ==1) %>% 
  count(bagain, wt = V201) %>% 
  mutate(weight = prop.table(n), year = c("2008")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))


sbcplot <- rbind(ba08, ba12, ba16)
sbcplot <- filter(sbcplot, bagain == "Born Again or Evangelical")
sbcplot$denom <- c("So. Baptist")


cces16$evannd <- Recode(cces16$religpew_nondenom, "1:90=1; else=0")
cces12$evannd <- Recode(cces12$religpew_nondenom, "1:90=1; else=0")
cces08$evannd <- Recode(cces08$V224, "1:90=1; else=0")

ndba16 <- cces16 %>%  filter(evannd ==1) %>% 
  count(bagain, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), year = c("2016")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

ndba12 <- cces12 %>%  filter(evannd ==1) %>% 
  count(bagain, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n), year = c("2012")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

ndba08 <- cces08 %>%  filter(evannd ==1) %>% 
  count(bagain, wt = V201) %>% 
  mutate(weight = prop.table(n), year = c("2008")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

ndplot <- rbind(ndba08, ndba12, ndba16)
ndplot <- filter(ndplot, bagain == "Born Again or Evangelical")
ndplot$denom <- c("Non-Denom.")


######### UMC

cces16$umc <- Recode(cces16$religpew_methodist, "1=1; else=0")
cces12$umc <- Recode(cces12$religpew_methodist, "1=1; else=0")
cces08$umc <- Recode(cces08$V223, "1=1; else=0")

umcba16 <- cces16 %>%  filter(umc ==1) %>% 
  count(bagain, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), year = c("2016")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

umcba12 <- cces12 %>%  filter(umc ==1) %>% 
  count(bagain, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n), year = c("2012")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

umcba08 <- cces08 %>%  filter(umc ==1) %>% 
  count(bagain, wt = V201) %>% 
  mutate(weight = prop.table(n), year = c("2008")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

umcplot <- rbind(umcba08, umcba12, umcba16)
umcplot <- filter(umcplot, bagain == "Born Again or Evangelical")
umcplot$denom <- c("United Meth.")


cces16$pcusa <- Recode(cces16$religpew_presby, "1=1; else=0")
cces12$pcusa <- Recode(cces12$religpew_presby, "1=1; else=0")
cces08$pcusa <- Recode(cces08$V226, "1=1; else=0")

pcusaba16 <- cces16 %>%  filter(pcusa ==1) %>% 
  count(bagain, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), year = c("2016")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

pcusaba12 <- cces12 %>%  filter(pcusa ==1) %>% 
  count(bagain, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n), year = c("2012")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

pcusaba08 <- cces08 %>%  filter(pcusa ==1) %>% 
  count(bagain, wt = V201) %>% 
  mutate(weight = prop.table(n), year = c("2008")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

pcusaplot <- rbind(pcusaba08, pcusaba12, pcusaba16)
pcusaplot <- filter(pcusaplot, bagain == "Born Again or Evangelical")
pcusaplot$denom <- c("PCUSA")

mplot <- rbind(baplot, sbcplot, ndplot, umcplot, pcusaplot)

ggplot(mplot, aes(x=denom, y=weight, fill = year)) + geom_col(position = "dodge") +  
  theme(axis.ticks = element_blank()) + ylab("Percent Born Again or Evangelical") + 
  theme(legend.position="bottom") +
  ggtitle("What Denominations are Evangelical?") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("chartreuse4","darkorange1", "dodgerblue3" )) +  
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("Denomination") 


####

cces16$pente <- Recode(cces16$religpew_pentecost, "1:90=1; else=0")
cces12$pente <- Recode(cces12$religpew_pentecost, "1:90=1; else=0")
cces08$pente <- Recode(cces08$V227, "1:90=1; else=0")

penteba16 <- cces16 %>%  filter(pente ==1) %>% 
  count(bagain, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), year = c("2016")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

penteba12 <- cces12 %>%  filter(pente ==1) %>% 
  count(bagain, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n), year = c("2012")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

penteba08 <- cces08 %>%  filter(pente ==1) %>% 
  count(bagain, wt = V201) %>% 
  mutate(weight = prop.table(n), year = c("2008")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

penteplot <- rbind(penteba08, penteba12, penteba16)
penteplot <- filter(penteplot, bagain == "Born Again or Evangelical")
penteplot$denom <- c("Pentecostal")


####

cces16$epis <- Recode(cces16$religpew_episcop, "1:90=1; else=0")
cces12$epis <- Recode(cces12$religpew_episcop, "1:90=1; else=0")
cces08$epis <- Recode(cces08$V228, "1:90=1; else=0")

episba16 <- cces16 %>%  filter(epis ==1) %>% 
  count(bagain, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), year = c("2016")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

episba12 <- cces12 %>%  filter(epis ==1) %>% 
  count(bagain, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n), year = c("2012")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

episba08 <- cces08 %>%  filter(epis ==1) %>% 
  count(bagain, wt = V201) %>% 
  mutate(weight = prop.table(n), year = c("2008")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

episplot <- rbind(episba08, episba12, episba16)
episplot <- filter(episplot, bagain == "Born Again or Evangelical")
episplot$denom <- c("Episcopal")

mplot <- rbind(baplot, sbcplot, ndplot, umcplot, pcusaplot, penteplot, episplot)

ggplot(mplot, aes(x=denom, y=weight, fill = year)) + geom_col(position = "dodge") +  
  theme(axis.ticks = element_blank()) + ylab("Percent Born Again or Evangelical") + 
  theme(legend.position="bottom") +
  ggtitle("What Denominations are Evangelical?") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("chartreuse4","darkorange1", "dodgerblue3" )) +  
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("Denomination") 


