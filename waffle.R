
library(waffle)
library(extrafont)

parts <- c(`Sampled`=2, `Contacted`=17, `Not Contacted`=36)
waffle(parts, rows=5, size =1, xlab = "1 square = 100 churches", colors = c("#4FC2D7","#A0BE68", "#E6A667"))  + 
  theme(text=element_text(size=18, family="KerkisSans"))