
library(waffle)
library(extrafont)

parts <- c(`Total Sampled`=2, `Total Contacted`=17, `Churches Not Contacted`=36)
waffle(parts, rows=5, size =3, xlab = "1 square = 100 churches", colors = c("#FF5A5F","#FFB400", "#007A87"))  + 
  theme(text=element_text(size=18, family="KerkisSans"))