library(haven)
library(dplyr)
library(weights)
library(forcats)
library(extrafont)
library(car)

cces16 <- read_dta("D:/cces/data/cces.dta")

cces12 <- read_dta("D:/cces/data/cces12.dta")

cces08 <- read_dta("D:/cces/data/cces2008.dta")

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

###

cces16$gaym <- Recode(cces16$CC16_335, "1=1; else=0")

cces16$gaym<-Recode(cces16$gaym,"1='Favor Gay Marriage';
                    0='Do Not Favor Gay Marriage';
                      else = NA")

cces12$gaym <- Recode(cces12$CC326, "1=1; else=0")

cces12$gaym<-Recode(cces12$gaym,"1='Favor Gay Marriage';
                    0='Do Not Favor Gay Marriage';
                      else = NA")

cces08$gaym <- Recode(cces08$CC316f, "2=1; else=0")

cces08$gaym<-Recode(cces08$gaym,"1='Favor Gay Marriage';
                    0='Do Not Favor Gay Marriage';
                      else = NA")


abcgay16 <- cces16 %>%  filter(abc ==1) %>% 
  count(gaym, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), year = c("2016")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

abcgay12 <- cces12 %>%  filter(abc ==1) %>% 
  count(gaym, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n), year = c("2012")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

abcgay08 <- cces08 %>%  filter(abc ==1) %>% 
  count(gaym, wt = V201) %>% 
  mutate(weight = prop.table(n), year = c("2008")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

abcgayplot <- rbind(abcgay08, abcgay12, abcgay16)
abcgayplot <- filter(abcgayplot, gaym == "Favor Gay Marriage")
abcgayplot$denom <- c("Am. Baptist")


sbcgay16 <- cces16 %>%  filter(sbc ==1) %>% 
  count(gaym, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), year = c("2016")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

sbcgay12 <- cces12 %>%  filter(sbc ==1) %>% 
  count(gaym, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n), year = c("2012")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

sbcgay08 <- cces08 %>%  filter(sbc ==1) %>% 
  count(gaym, wt = V201) %>% 
  mutate(weight = prop.table(n), year = c("2008")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

sbcgayplot <- rbind(sbcgay08, sbcgay12, sbcgay16)
sbcgayplot <- filter(sbcgayplot, gaym == "Favor Gay Marriage")
sbcgayplot$denom <- c("So. Baptist")

### 
umcgay16 <- cces16 %>%  filter(umc ==1) %>% 
  count(gaym, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), year = c("2016")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

umcgay12 <- cces12 %>%  filter(umc ==1) %>% 
  count(gaym, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n), year = c("2012")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

umcgay08 <- cces08 %>%  filter(umc ==1) %>% 
  count(gaym, wt = V201) %>% 
  mutate(weight = prop.table(n), year = c("2008")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

umcgayplot <- rbind(umcgay08, umcgay12, umcgay16)
umcgayplot <- filter(umcgayplot, gaym == "Favor Gay Marriage")
umcgayplot$denom <- c("United Meth.")

##

evanndgay16 <- cces16 %>%  filter(evannd ==1) %>% 
  count(gaym, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), year = c("2016")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

evanndgay12 <- cces12 %>%  filter(evannd ==1) %>% 
  count(gaym, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n), year = c("2012")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

evanndgay08 <- cces08 %>%  filter(evannd ==1) %>% 
  count(gaym, wt = V201) %>% 
  mutate(weight = prop.table(n), year = c("2008")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

evanndgayplot <- rbind(evanndgay08, evanndgay12, evanndgay16)
evanndgayplot <- filter(evanndgayplot, gaym == "Favor Gay Marriage")
evanndgayplot$denom <- c("Non-Denom.")

###

pcusagay16 <- cces16 %>%  filter(pcusa ==1) %>% 
  count(gaym, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), year = c("2016")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

pcusagay12 <- cces12 %>%  filter(pcusa ==1) %>% 
  count(gaym, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n), year = c("2012")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

pcusagay08 <- cces08 %>%  filter(pcusa ==1) %>% 
  count(gaym, wt = V201) %>% 
  mutate(weight = prop.table(n), year = c("2008")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

pcusagayplot <- rbind(pcusagay08, pcusagay12, pcusagay16)
pcusagayplot <- filter(pcusagayplot, gaym == "Favor Gay Marriage")
pcusagayplot$denom <- c("PCUSA")


episgay16 <- cces16 %>%  filter(epis ==1) %>% 
  count(gaym, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), year = c("2016")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

episgay12 <- cces12 %>%  filter(epis ==1) %>% 
  count(gaym, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n), year = c("2012")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

episgay08 <- cces08 %>%  filter(epis ==1) %>% 
  count(gaym, wt = V201) %>% 
  mutate(weight = prop.table(n), year = c("2008")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

episgayplot <- rbind(episgay08, episgay12, episgay16)
episgayplot <- filter(episgayplot, gaym == "Favor Gay Marriage")
episgayplot$denom <- c("Episcopal")


pentegay16 <- cces16 %>%  filter(pente ==1) %>% 
  count(gaym, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), year = c("2016")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

pentegay12 <- cces12 %>%  filter(pente ==1) %>% 
  count(gaym, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n), year = c("2012")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

pentegay08 <- cces08 %>%  filter(pente ==1) %>% 
  count(gaym, wt = V201) %>% 
  mutate(weight = prop.table(n), year = c("2008")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

pentegayplot <- rbind(pentegay08, pentegay12, pentegay16)
pentegayplot <- filter(pentegayplot, gaym == "Favor Gay Marriage")
pentegayplot$denom <- c("Pentecostal")

gayplot <- rbind(abcgayplot, sbcgayplot, umcgayplot, evanndgayplot, pcusagayplot, episgayplot, pentegayplot)

ggplot(data = gayplot , aes(x = year, y = weight, color = denom, label = denom)) +
  geom_line(aes(group = denom), size=2) + ylab("Support for Gay Marriage") + xlab("Year") + ggtitle("How has Support for Gay Marriage Changed?") +
  scale_color_manual(values = c("seagreen1", "#4DAF4A" , "#984EA3", "#FF7F00" , "#FFFF33" , "#A65628", "pink"))  +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + theme(legend.title = element_blank())


#### Abortion Plots 

cces16$abany <- Recode(cces16$CC16_332a, "1=1; else=0")
cces16$abrape <- Recode(cces16$CC16_332b, "1=1; else=0")
cces16$ablate <- Recode(cces16$CC16_332c, "1=1; else=0")
cces16$abins <- Recode(cces16$CC16_332d, "1=1; else=0")
cces16$abfund <- Recode(cces16$CC16_332e, "1=1; else=0")
cces16$ablife <- Recode(cces16$CC16_332f, "1=1; else=0")


abcabany <- cces16 %>%  filter(abc ==1) %>% 
  count(var = abany, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Any Reason"), denom = c("Am. Baptist")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

abcabrape <- cces16 %>%  filter(abc ==1) %>% 
  count(var = abrape, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Rape or Incest"), denom = c("Am. Baptist")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

abcablate <- cces16 %>%  filter(abc ==1) %>% 
  count(var = ablate, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Ban Late Term"), denom = c("Am. Baptist")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

abcabins <- cces16 %>%  filter(abc ==1) %>% 
  count(var = abins, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Insurance Cover"), denom = c("Am. Baptist")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

abcabfund <- cces16 %>%  filter(abc ==1) %>% 
  count(var = abfund, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Prohibit Govt. Funds"), denom = c("Am. Baptist")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

abcablife <- cces16 %>%  filter(abc ==1) %>% 
  count(var = ablife, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Completely Illegal"), denom = c("Am. Baptist")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

abcab <- rbind(abcabany, abcabrape, abcablate, abcabins, abcabfund, abcablife)
abcab <- filter(abcab, var ==1)


sbcabany <- cces16 %>%  filter(sbc ==1) %>% 
  count(var = abany, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Any Reason"), denom = c("So. Baptist")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

sbcabrape <- cces16 %>%  filter(sbc ==1) %>% 
  count(var = abrape, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Rape or Incest"), denom = c("So. Baptist")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

sbcablate <- cces16 %>%  filter(sbc ==1) %>% 
  count(var = ablate, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Ban Late Term"), denom = c("So. Baptist")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

sbcabins <- cces16 %>%  filter(sbc ==1) %>% 
  count(var = abins, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Insurance Cover"), denom = c("So. Baptist")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

sbcabfund <- cces16 %>%  filter(sbc ==1) %>% 
  count(var = abfund, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Prohibit Govt. Funds"), denom = c("So. Baptist")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

sbcablife <- cces16 %>%  filter(sbc ==1) %>% 
  count(var = ablife, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Completely Illegal"), denom = c("So. Baptist")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

sbcab <- rbind(sbcabany, sbcabrape, sbcablate, sbcabins, sbcabfund, sbcablife)
sbcab <- filter(sbcab, var ==1)


umcabany <- cces16 %>%  filter(umc ==1) %>% 
  count(var = abany, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Any Reason"), denom = c("United Meth. ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

umcabrape <- cces16 %>%  filter(umc ==1) %>% 
  count(var = abrape, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Rape or Incest"), denom = c("United Meth. ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

umcablate <- cces16 %>%  filter(umc ==1) %>% 
  count(var = ablate, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Ban Late Term"), denom = c("United Meth. ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

umcabins <- cces16 %>%  filter(umc ==1) %>% 
  count(var = abins, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Insurance Cover"), denom = c("United Meth. ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

umcabfund <- cces16 %>%  filter(umc ==1) %>% 
  count(var = abfund, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Prohibit Govt. Funds"), denom = c("United Meth. ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

umcablife <- cces16 %>%  filter(umc ==1) %>% 
  count(var = ablife, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Completely Illegal"), denom = c("United Meth. ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

umcab <- rbind(umcabany, umcabrape, umcablate, umcabins, umcabfund, umcablife)
umcab <- filter(umcab, var ==1)


penteabany <- cces16 %>%  filter(pente ==1) %>% 
  count(var = abany, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Any Reason"), denom = c("Pentecostal ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

penteabrape <- cces16 %>%  filter(pente ==1) %>% 
  count(var = abrape, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Rape or Incest"), denom = c("Pentecostal ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

penteablate <- cces16 %>%  filter(pente ==1) %>% 
  count(var = ablate, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Ban Late Term"), denom = c("Pentecostal ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

penteabins <- cces16 %>%  filter(pente ==1) %>% 
  count(var = abins, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Insurance Cover"), denom = c("Pentecostal ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

penteabfund <- cces16 %>%  filter(pente ==1) %>% 
  count(var = abfund, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Prohibit Govt. Funds"), denom = c("Pentecostal ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

penteablife <- cces16 %>%  filter(pente ==1) %>% 
  count(var = ablife, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Completely Illegal"), denom = c("Pentecostal ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

penteab <- rbind(penteabany, penteabrape, penteablate, penteabins, penteabfund, penteablife)
penteab <- filter(penteab, var ==1)


evanndabany <- cces16 %>%  filter(evannd ==1) %>% 
  count(var = abany, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Any Reason"), denom = c("Non-Denom ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

evanndabrape <- cces16 %>%  filter(evannd ==1) %>% 
  count(var = abrape, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Rape or Incest"), denom = c("Non-Denom ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

evanndablate <- cces16 %>%  filter(evannd ==1) %>% 
  count(var = ablate, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Ban Late Term"), denom = c("Non-Denom ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

evanndabins <- cces16 %>%  filter(evannd ==1) %>% 
  count(var = abins, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Insurance Cover"), denom = c("Non-Denom ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

evanndabfund <- cces16 %>%  filter(evannd ==1) %>% 
  count(var = abfund, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Prohibit Govt. Funds"), denom = c("Non-Denom ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

evanndablife <- cces16 %>%  filter(evannd ==1) %>% 
  count(var = ablife, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Completely Illegal"), denom = c("Non-Denom ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

evanndab <- rbind(evanndabany, evanndabrape, evanndablate, evanndabins, evanndabfund, evanndablife)
evanndab <- filter(evanndab, var ==1)



episabany <- cces16 %>%  filter(epis ==1) %>% 
  count(var = abany, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Any Reason"), denom = c("Episcopal ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

episabrape <- cces16 %>%  filter(epis ==1) %>% 
  count(var = abrape, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Rape or Incest"), denom = c("Episcopal ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

episablate <- cces16 %>%  filter(epis ==1) %>% 
  count(var = ablate, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Ban Late Term"), denom = c("Episcopal ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

episabins <- cces16 %>%  filter(epis ==1) %>% 
  count(var = abins, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Insurance Cover"), denom = c("Episcopal ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

episabfund <- cces16 %>%  filter(epis ==1) %>% 
  count(var = abfund, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Prohibit Govt. Funds"), denom = c("Episcopal ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

episablife <- cces16 %>%  filter(epis ==1) %>% 
  count(var = ablife, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Completely Illegal"), denom = c("Episcopal")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

episab <- rbind(episabany, episabrape, episablate, episabins, episabfund, episablife)
episab <- filter(episab, var ==1)


pcusaabany <- cces16 %>%  filter(pcusa ==1) %>% 
  count(var = abany, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Any Reason"), denom = c("PCUSA ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

pcusaabrape <- cces16 %>%  filter(pcusa ==1) %>% 
  count(var = abrape, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Rape or Incest"), denom = c("PCUSA ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

pcusaablate <- cces16 %>%  filter(pcusa ==1) %>% 
  count(var = ablate, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Ban Late Term"), denom = c("PCUSA ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

pcusaabins <- cces16 %>%  filter(pcusa ==1) %>% 
  count(var = abins, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Insurance Cover"), denom = c("PCUSA ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

pcusaabfund <- cces16 %>%  filter(pcusa ==1) %>% 
  count(var = abfund, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Prohibit Govt. Funds"), denom = c("PCUSA ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

pcusaablife <- cces16 %>%  filter(pcusa ==1) %>% 
  count(var = ablife, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), sit = c("Completely Illegal"), denom = c("PCUSA ")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

pcusaab <- rbind(pcusaabany, pcusaabrape, pcusaablate, pcusaabins, pcusaabfund, pcusaablife)
pcusaab <- filter(pcusaab, var ==1)
 
abortplot <- rbind(abcab, sbcab, pcusaab, episab, evanndab, penteab, umcab)


ggplot(abortplot, aes(x = weight, y = sit))  +
  geom_point(shape=21, size =4, aes(fill = factor(denom))) +  theme(legend.title=element_blank())+
  scale_fill_manual(values = c("seagreen1", "#4DAF4A" , "#984EA3", "#FF7F00" , "#FFFF33" , "#A65628", "pink")) + 
  ylab("Abortion Situation") + xlab("Percent in Favor of Abortion") +
  ggtitle("Abortion Scenarios in 2016")+ 
  theme(text=element_text(size=16, family="KerkisSans")) + xlim(0,100)  +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")

### Church Attendance

cces16$attend <- 7 - cces16$pew_churatd
cces16$attend <- Recode(cces16$attend, "0= 'Do not Know';
                                        1= 'Never';
                                        2= 'Seldom';
                                        3= 'Yearly';
                                        4= 'Monthly';
                                        5= 'Weekly';
                                        6= 'More than Weekly'")

abcatt <- cces16 %>%  filter(abc ==1 & complete.cases(attend)) %>% 
  count(attend, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("Am. Baptist")) %>% mutate(weight = weight*100) 

sbcatt <- cces16 %>%  filter(sbc ==1 & complete.cases(attend)) %>% 
  count(attend, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("So. Baptist")) %>% mutate(weight = weight*100) 

ndatt <- cces16 %>%  filter(evannd ==1 & complete.cases(attend)) %>% 
  count(attend, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("Non Denom")) %>% mutate(weight = weight*100) 

episatt <- cces16 %>%  filter(epis ==1 & complete.cases(attend)) %>% 
  count(attend, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("Episcopal")) %>% mutate(weight = weight*100) 

umcatt <- cces16 %>%  filter(umc ==1 & complete.cases(attend)) %>% 
  count(attend, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("United Meth.")) %>% mutate(weight = weight*100) 

penteatt <- cces16 %>%  filter(pente ==1 & complete.cases(attend)) %>% 
  count(attend, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("Pentecostal")) %>% mutate(weight = weight*100) 

pcusaatt <- cces16 %>%  filter(pcusa ==1 & complete.cases(attend)) %>% 
  count(attend, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("PCUSA")) %>% mutate(weight = weight*100) 

attplot <- rbind(abcatt, sbcatt, ndatt, episatt, umcatt, penteatt, pcusaatt)
attplot <- filter(attplot, attend != "NaN")
attplot <- filter(attplot, attend >0)
#attplot$weight <- as.numeric(attplot$weight)


ggplot(abcatt, aes(x=attend, y=weight)) + geom_col(fill = "seagreen1", color = "black" ) +
  scale_x_continuous(limits = c(.5,6.5), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  ggtitle("How Often Do ABC Members Attend?") + xlab("") + ylab("Percent of Respondents") + 
  theme(text=element_text(size=16, family="KerkisSans")) + 
  theme(plot.title = element_text(hjust = 0.5)) 
  
   
ggplot(attplot, aes(x=attend, y=weight, fill = denom)) + geom_col(position = "dodge") +  
  theme(axis.ticks = element_blank()) + ylab("Percent Attending") + 
  theme(legend.position="bottom") +
  ggtitle("How Does Attendance Vary Across Denominations?") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans"))  +  
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("") +
  scale_fill_manual(values = c("seagreen1", "#4DAF4A" , "#984EA3", "#FF7F00" , "#FFFF33" , "#A65628", "pink"))  +
  scale_x_continuous(limits = c(.5,6.5), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+"))

####

abceduc <- cces16 %>%  filter(abc ==1 & complete.cases(educ)) %>% 
  count(educ, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("Am. Baptist")) %>% mutate(weight = weight*100) 

sbceduc <- cces16 %>%  filter(sbc ==1 & complete.cases(educ)) %>% 
  count(educ, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("So. Baptist")) %>% mutate(weight = weight*100) 

ndeduc <- cces16 %>%  filter(evannd ==1 & complete.cases(educ)) %>% 
  count(educ, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("Non Denom")) %>% mutate(weight = weight*100) 

episeduc <- cces16 %>%  filter(epis ==1 & complete.cases(educ)) %>% 
  count(educ, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("Episcopal")) %>% mutate(weight = weight*100) 

umceduc <- cces16 %>%  filter(umc ==1 & complete.cases(educ)) %>% 
  count(educ, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("United Meth.")) %>% mutate(weight = weight*100) 

penteeduc <- cces16 %>%  filter(pente ==1 & complete.cases(educ)) %>% 
  count(educ, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("Pentecostal")) %>% mutate(weight = weight*100) 

pcusaeduc <- cces16 %>%  filter(pcusa ==1 & complete.cases(educ)) %>% 
  count(educ, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("PCUSA")) %>% mutate(weight = weight*100) 

educplot <- rbind(abceduc, sbceduc, ndeduc, episeduc, umceduc, penteeduc, pcusaeduc)
educplot <- filter(educplot, educ != "NaN")
educplot <- filter(educplot, educ >0)

ggplot(abceduc, aes(x=educ, y=weight)) + geom_col(fill = "seagreen1", color = "black" ) +
  scale_x_continuous(limits = c(.5,6.5), breaks = c(1,2,3,4,5,6), labels = c("Less than HS", "HS Grad", "Some College", "Associates", "Bachelors", "Graduate")) +
  ggtitle("How Much Education in the ABC?") + xlab("Highest Level of Education Completed") + ylab("Percent of Respondents") + 
  theme(text=element_text(size=16, family="KerkisSans")) + 
  theme(plot.title = element_text(hjust = 0.5)) 


ggplot(educplot, aes(x=educ, y=weight, fill = denom)) + geom_col(position = "dodge") +  
  theme(axis.ticks = element_blank()) + ylab("Percent of Respodents") + 
  theme(legend.position="bottom") +
  ggtitle("How Does Attendance Vary Across Denominations?") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans"))  +  
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("") +
  scale_fill_manual(values = c("seagreen1", "#4DAF4A" , "#984EA3", "#FF7F00" , "#FFFF33" , "#A65628", "pink"))  +
  scale_x_continuous(limits = c(.5,6.5), breaks = c(1,2,3,4,5,6), labels = c("Less than HS", "HS Grad", "Some College", "Associates", "Bachelors", "Graduate")) 

####

abcrace <- cces16 %>%  filter(abc ==1 & complete.cases(race)) %>% 
  count(race, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("Am. Baptist")) %>% mutate(weight = weight*100) 

sbcrace <- cces16 %>%  filter(sbc ==1 & complete.cases(race)) %>% 
  count(race, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("So. Baptist")) %>% mutate(weight = weight*100) 

ndrace <- cces16 %>%  filter(evannd ==1 & complete.cases(race)) %>% 
  count(race, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("Non Denom")) %>% mutate(weight = weight*100) 

episrace <- cces16 %>%  filter(epis ==1 & complete.cases(race)) %>% 
  count(race, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("Episcopal")) %>% mutate(weight = weight*100) 

umcrace <- cces16 %>%  filter(umc ==1 & complete.cases(race)) %>% 
  count(race, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("United Meth.")) %>% mutate(weight = weight*100) 

penterace <- cces16 %>%  filter(pente ==1 & complete.cases(race)) %>% 
  count(race, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("Pentecostal")) %>% mutate(weight = weight*100) 

pcusarace <- cces16 %>%  filter(pcusa ==1 & complete.cases(race)) %>% 
  count(race, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), denom = c("PCUSA")) %>% mutate(weight = weight*100) 

raceplot <- rbind(abcrace, sbcrace, ndrace, episrace, umcrace, penterace, pcusarace)
raceplot <- filter(raceplot, race != "NaN")
raceplot <- filter(raceplot, race >0)


ggplot(abcrace, aes(x=race, y=weight)) + geom_col(fill = "seagreen1", color = "black" ) +
  scale_x_continuous(limits = c(.5,8.5), breaks = c(1,2,3,4,5,6,7,8), labels = c("White", "Black", "Hispanic", "Asian", "Native Am.", "Middle East.", "Mixed Race", "Other")) +
  ggtitle("Racial Composition of the ABC?") + xlab("What Racial or Ethnic Group Best Describes You?") + ylab("Percent of Respondents") + 
  theme(text=element_text(size=16, family="KerkisSans")) + 
  theme(plot.title = element_text(hjust = 0.5)) 


ggplot(raceplot, aes(x=race, y=weight, fill = denom)) + geom_col(position = "dodge") +  
  theme(axis.ticks = element_blank()) + ylab("Percent of Respodents") + 
  theme(legend.position="bottom") +
  ggtitle("Racial Composition of the ABC?") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans"))  +  
  guides(fill = guide_legend(reverse = FALSE)) + labs(fill="")  + xlab("") +
  scale_fill_manual(values = c("seagreen1", "#4DAF4A" , "#984EA3", "#FF7F00" , "#FFFF33" , "#A65628", "pink"))  +
  scale_x_continuous(limits = c(.5,8.5), breaks = c(1,2,3,4,5,6,7,8), labels = c("White", "Black", "Hispanic", "Asian", "Native Am.", "Middle East.", "Mixed Race", "Other"))

