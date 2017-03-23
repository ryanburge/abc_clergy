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

vote08 <- cces08 %>%  filter(abc ==1 & complete.cases(candidate)) %>% 
  count(candidate, wt = V201) %>% 
  mutate(weight = prop.table(n), year = c("2008")) %>% mutate(weight = weight*100) %>%  arrange(desc(weight))


cces12$abc <- Recode(cces12$religpew_baptist, "2=1; else=0")

vote12 <- cces12 %>%  filter(abc ==1 & complete.cases(candidate)) %>% 
  count(candidate, wt = weight_vv_post) %>% 
  mutate(weight = prop.table(n), year = c("2012")) %>% mutate(weight = weight*100) %>%  arrange(desc(weight))


cces16$abc <- Recode(cces16$religpew_baptist, "2=1; else=0")

vote16 <- cces16 %>%  filter(abc ==1 & complete.cases(candidate)) %>% 
  count(candidate, wt = commonweight_post) %>% 
  mutate(weight = prop.table(n), year = c("2016")) %>% mutate(weight = weight*100) %>% arrange(desc(weight))

plot <- rbind(vote08, vote12, vote16)


plot$candidate <- factor(plot$candidate, levels=unique(plot$candidate))


plot.cor <- c(283, 547, 608)


ggplot(plot, aes(1, weight, fill= fct_rev(candidate))) + geom_col()  + coord_flip() +  
theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + 
  theme(legend.position="bottom") +
  ggtitle("American Baptist Voters in Presidential Elections") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("black","goldenrod1" , "forestgreen", "darkorchid4", "azure4",  "firebrick1", "dodgerblue3")) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="")  + facet_grid(year ~ .) 






