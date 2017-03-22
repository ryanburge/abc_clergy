library(haven)
library(dplyr)
library(weights)
library(forcats)
cces16 <- read_dta("C:/cces16.dta")

abc <- filter(cces16, religpew_baptist ==2)

vote16 <- data.frame("candidate" = c("Donald Trump", "Hillary Clinton", "Gary Johnson", "Jill Stein"),
                     pct =c(38.32, 57.9, 1.8, .5))

vote16$candidate <- factor(vote16$candidate, levels = vote16$candidate)


ggplot(vote16, aes(x=reorder(candidate, -pct), y=pct)) + geom_col()


ggplot(vote16, aes(1, pct, fill= fct_rev(candidate))) + geom_col()  + coord_flip()  + 
  scale_fill_manual(values=c("black", "darkgrey", "forestgreen", "goldenrod1", "dodgerblue3", "firebrick1", "goldenro", "red"))


ggplot(vote16, aes(1, pct, fill= fct_rev(candidate))) + geom_col()  + coord_flip() +  
theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + 
  theme(legend.position="bottom") +
  ggtitle("2016 Presidential Election") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("forestgreen", "goldenrod1", "dodgerblue3", "firebrick1")) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="")  


cces12 <- read_dta("C:/cces12.dta")

abc12 <- filter(cces12, religpew_baptist ==2)

vote12 <- data.frame("candidate" = c("Mitt Romney", "Barack Obama", "Other"), pct =c(37.6, 61.2, 1.2))

ggplot(vote12, aes(1, pct, fill= fct_rev(candidate))) + geom_col()  + coord_flip() +  
  theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + ylab("Percent of Votes Cast") + 
  theme(legend.position="bottom") +
  ggtitle("2012 Presidential Election") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + 
  scale_fill_manual(values=c("darkgrey", "firebrick1", "dodgerblue3")) +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="") 





