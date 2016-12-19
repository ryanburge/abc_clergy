abc <- read_dta("abc_final.dta")


## Church Growth
abc$grow <- abc$q2 + abc$q7 + abc$q50
abc$grow[abc$grow==1] <- "Declined in Total Membership"
abc$grow[abc$grow==3] <- "Increased in Total Membership"
abc$grow[abc$grow==4] <- "Remained the Same"

## Church Competition

abc$compete <- abc$q3 + abc$q52
abc$compete[abc$compete==1] <- "Yes"
abc$compete[abc$compete==2] <- "No"
abc$compete[abc$compete==3] <- "Haven't thought about it"

##Political Activity 
abc$pol1 <- abc$q5_1 + abc$q5_2 + abc$q5_3 + abc$q5_4 + abc$q5_5 + abc$q5_6 + abc$q5_7 + abc$q5_8 + abc$q5_9 + abc$q5_10
abc$pol2 <- abc$q10_1 + abc$q10_2 + abc$q10_3 + abc$q10_4 + abc$q10_5 + abc$q10_6 + abc$q10_7 + abc$q10_8 + abc$q10_9 + abc$q10_10
abc$pol3 <- abc$q13_1 + abc$q13_2 + abc$q13_3 + abc$q13_4 + abc$q13_5 + abc$q13_6 + abc$q13_7 + abc$q13_8 + abc$q13_9 + abc$q13_10
abc$pol <- abc$pol1 + abc$pol2 + abc$pol3

abc$vote <- abc$q5_1 + abc$q10_1 + abc$q13_1
abc$contact <- abc$q5_2 + abc$q10_2 + abc$q13_2
abc$donate <- abc$q5_3 + abc$q10_3 + abc$q13_3
abc$vol1 <- abc$q5_4 + abc$q10_4 + abc$q13_4
abc$volig <- abc$q5_5 + abc$q10_5 + abc$q13_5
abc$urge <- abc$q5_6 + abc$q10_6 + abc$q13_6
abc$protest <- abc$q5_7 + abc$q10_7 + abc$q13_7
abc$rally <- abc$q5_8 + abc$q10_8 + abc$q13_8
abc$study <- abc$q5_9 + abc$q10_9 + abc$q13_9
abc$register <- abc$q5_10 + abc$q10_10 + abc$q13_10

##Outreach activities
abc$out1 <- recode(abc$q6_1, "1=1; else=0")
abc$out2 <- recode(abc$q6_2, "1=1; else=0")
abc$out3 <- recode(abc$q6_3, "1=1; else=0")
abc$out4 <- recode(abc$q6_4, "1=1; else=0")

abc$out5 <- recode(abc$q11_1, "1=1; else=0")
abc$out6 <- recode(abc$q11_2, "1=1; else=0")
abc$out7 <- recode(abc$q11_3, "1=1; else=0")
abc$out8 <- recode(abc$q11_4, "1=1; else=0")

abc$out9 <- recode(abc$q14_1, "1=1; else=0")
abc$out10 <- recode(abc$q14_2, "1=1; else=0")
abc$out11 <- recode(abc$q14_3, "1=1; else=0")
abc$out12 <- recode(abc$q14_4, "1=1; else=0")

abc$outevent <- abc$out1 + abc$out5 + abc$out9 
abc$outworship <- abc$out2 + abc$out6 + abc$out10
abc$outcomm <- abc$out3 + abc$out7 + abc$out11
abc$outmail <- abc$out4 + abc$out8 + abc$out12

abc$out <- abc$out1 + abc$out2 + abc$out3 + abc$out4 + abc$out5 + abc$out6 + abc$out7 + abc$out8 + abc$out9 + abc$out10 + abc$out11 + abc$out12 

## Make unpopular political statement

abc$state1 <- recode(abc$q78_1, "1=1; else=0")
abc$state2 <- recode(abc$q78_2, "1=1; else=0")
abc$state3 <- recode(abc$q78_3, "1=1; else=0")
abc$state4 <- recode(abc$q78_4, "1=1; else=0")
abc$state5 <- recode(abc$q78_5, "1=1; else=0")
abc$state6 <- recode(abc$q78_6, "1=1; else=0")

abc$state7 <- recode(abc$q77_1, "1=1; else=0")
abc$state8 <- recode(abc$q77_2, "1=1; else=0")
abc$state9 <- recode(abc$q77_3, "1=1; else=0")
abc$state10 <- recode(abc$q77_4, "1=1; else=0")
abc$state11 <- recode(abc$q77_5, "1=1; else=0")
abc$state12 <- recode(abc$q77_6, "1=1; else=0")

abc$state13 <- recode(abc$q25_1, "1=1; else=0")
abc$state14 <- recode(abc$q25_2, "1=1; else=0")
abc$state15 <- recode(abc$q25_3, "1=1; else=0")
abc$state16 <- recode(abc$q25_4, "1=1; else=0")
abc$state17 <- recode(abc$q25_5, "1=1; else=0")
abc$state18 <- recode(abc$q25_6, "1=1; else=0")

abc$sign <- abc$state1 + abc$state7 + abc$state13
abc$bumper <- abc$state2 + abc$state8 + abc$state14
abc$demonstrate <- abc$state3 + abc$state9 + abc$state15
abc$button <- abc$state4 + abc$state10 + abc$state16
abc$petition <- abc$state5 + abc$state11 + abc$state17
abc$letter <- abc$state6 + abc$state12 + abc$state18

abc$state <- abc$state1 + abc$state2 + abc$state3 + abc$state4 + abc$state5 + abc$state6 + abc$state7 + abc$state8 + abc$state9 + abc$state10 + abc$state11 + abc$state12 + abc$state13 + abc$state14 + abc$state15 + abc$state16 + abc$state17 + abc$state18

## ABC Involvement

abc$involve1 <- 8 - abc$q4_1
abc$involve2 <- 8 - abc$q4_2
abc$involve3 <- 8 - abc$q4_3
abc$involve1 <- recode(abc$involve1, "8=0")
abc$involve2 <- recode(abc$involve2, "8=0")
abc$involve3 <- recode(abc$involve3, "8=0")

abc$involve4 <- 8 - abc$q9_1
abc$involve5 <- 8 - abc$q9_2
abc$involve6 <- 8 - abc$q9_3
abc$involve4 <- recode(abc$involve4, "8=0")
abc$involve5 <- recode(abc$involve5, "8=0")
abc$involve6 <- recode(abc$involve6, "8=0")

abc$involve7 <- 8 - abc$q12_1
abc$involve8 <- 8 - abc$q12_2
abc$involve9 <- 8 - abc$q12_3
abc$involve7 <- recode(abc$involve7, "8=0")
abc$involve8 <- recode(abc$involve8, "8=0")
abc$involve9 <- recode(abc$involve9, "8=0")

abc$involve <- abc$involve1 + abc$involve2 + abc$involve3 + abc$involve4 + abc$involve5 + abc$involve6 + abc$involve7 + abc$involve8 + abc$involve9
abc$involve <- abc$involve/21

## Right Direction
abc$rightdirect <- recode(abc$q73, "1=7; 4=6; 5=5; 6=4; 7=3; 8=2; 9=1")

## Trust in Govt
abc$trust <- 8- abc$q74

## Clergy Political Representation

abc$rep1 <- recode(abc$q15_1, "1=1; else=0")
abc$rep2 <- recode(abc$q15_2, "1=1; else=0")
abc$rep3 <- recode(abc$q15_3, "1=1; else=0")
abc$rep4 <- recode(abc$q15_4, "1=1; else=0")
abc$rep5 <- recode(abc$q15_5, "1=1; else=0")

abc$rep <- abc$rep1 + abc$rep2 + abc$rep3 + abc$rep4 + abc$rep5

## Religious Authority

abc$rav1 <- 6- abc$q18_2
abc$rav2 <- 6- abc$q18_3
abc$rav3 <- 6- abc$q18_5
abc$rav4 <- 6- abc$q18_7
abc$rav5 <- 6- abc$q18_9
abc$rav1 <- recode(abc$rav1, "6=0")
abc$rav2 <- recode(abc$rav2, "6=0")
abc$rav3 <- recode(abc$rav3, "6=0")
abc$rav4 <- recode(abc$rav4, "6=0")
abc$rav5 <- recode(abc$rav5, "6=0")
abc$rav <- abc$rav1 + abc$rav2 + abc$rav3 + abc$rav4 + abc$rav5
abc$rav <- abc$rav/5

## Democratic Norms

abc$dem1 <- 6 - abc$q20_1
abc$dem2 <- 6 - abc$q20_2
abc$dem3 <- 6 - abc$q20_3
abc$dem4 <- 6 - abc$q20_4
abc$dem5 <- 6 - abc$q20_5
abc$dem1 <- recode(abc$dem1, "6=0")
abc$dem2 <- recode(abc$dem2, "6=0")
abc$dem3 <- recode(abc$dem3, "6=0")
abc$dem4 <- recode(abc$dem4, "6=0")
abc$dem5 <- recode(abc$dem5, "6=0")

abc$dem <- abc$dem1 + abc$dem2 + abc$dem3 + abc$dem4 + abc$dem5

## Vote Choice

abc$votechoice <- abc$q21
abc$votechoice[abc$votechoice==1] <- "Clinton"
abc$votechoice[abc$votechoice==2] <- "Trump"
abc$votechoice[abc$votechoice==5] <- "Johnson"
abc$votechoice[abc$votechoice==3] <- "Stein"
abc$votechoice[abc$votechoice==4] <- "Other"
abc$votechoice[abc$votechoice==6] <- "Not Vote"
abc$votechoice[abc$votechoice==0] <- "No Answer"

## Congregation Vote

abc$hrcshare <- abc$q22_1
abc$djtshare <- abc$q22_2
abc$hrcshare[abc$hrcshare == 0] <- NA
abc$djtshare[abc$djtshare == 0] <- NA

## Reason for Vote
abc$votereason <- abc$q67
abc$votereason[abc$votereason==1] <- "Abortion"
abc$votereason[abc$votereason==2] <- "Immigration"
abc$votereason[abc$votereason==3] <- "Judicial Appointments"
abc$votereason[abc$votereason==4] <- "Religious Freedom"
abc$votereason[abc$votereason==5] <- "Hillary Clinton"
abc$votereason[abc$votereason==6] <- "Donald Trump"
abc$votereason[abc$votereason==7] <- "Treatment of Women"
abc$votereason[abc$votereason==8] <- "Temperament"
abc$votereason[abc$votereason==9] <- "Something Else"

## Frequency of Election Discussion

abc$discuss <- abc$q63
abc$peoplenumber <- abc$q64_1

## Election Questions
abc$witness <- 8 - abc$q66_1
abc$witness <- recode(abc$witness, "8=0")
abc$agenda <- 8 - abc$q66_2
abc$agenda <- recode(abc$agenda, "8=0")
abc$doubts <- 8 - abc$q66_7
abc$doubts <- recode(abc$doubts, "8=0")
abc$disagree <- 8 - abc$q66_3
abc$disagree <- recode(abc$disagree, "8=0")
abc$rightdead <- 8 - abc$q66_4
abc$rightdead <- recode(abc$rightdead, "8=0")
abc$bond <- 8 - abc$q66_5
abc$bond <- recode(abc$bond, "8=0")
abc$immoral <- 8 - abc$q66_6
abc$immoral <- recode(abc$immoral, "8=0")

## Attendance Numbers
abc$members <- abc$q28_1_text
abc$attendance <- abc$q28_2_text
abc$active <- abc$q28_3_text

## Demographics of Congregation

abc$size <- abc$q29
abc$class <- abc$q30

## Compare Yourself to Congreation
abc$compare <- (abc$q32_1 + abc$q32_2 + abc$q32_3 + abc$q32_4 + abc$q32_5)/5
ggplot(abc %>% filter(compare >0), aes(compare)) + geom_bar()

## Compare Yourself to Community
abc$comparecomm <- abc$q26

## Compare Congregation to the Community 
abc$comparecong <- (abc$q60_1 + abc$q60_2 + abc$q60_3 + abc$q60_4 + abc$q60_5)/5

## Gender 
abc$male <- recode(abc$q38, "1=1; else=0")

## Political Affiliation

abc$repubid <- recode(abc$q39, "8=0")
abc$conideo <- abc$q68

# Race
abc$white <- abc$q41_1
abc$black <- recode(abc$q41_2, "1=2")
abc$hispanic <- recode(abc$q41_3, "1=3")
abc$native <- recode(abc$q41_6, "1=4")
abc$race <- abc$white + abc$black + abc$hispanic + abc$native
abc$race[abc$race==1] <- "White"
abc$race[abc$race==2] <- "Black"
abc$race[abc$race==3] <- "Hispanic"
abc$race[abc$race==4] <- "Native American"
abc$race[abc$race==5] <- NA

## Religious Labels 

count <- read.csv("count.csv")
ggplot(count, aes(x=label, y = count)) + geom_col()

## Inclusive Sermon Messages

abc$include1 <- 6- abc$q24_1
abc$include1 <- recode(abc$include1, "6=0")
abc$include2 <- 6- abc$q24_2
abc$include2 <- recode(abc$include2, "6=0")
abc$include3 <- abc$q24_3
abc$include4 <- 6- abc$q24_4
abc$include4 <- recode(abc$include4, "6=0")
abc$include <- (abc$include1 + abc$include2 + abc$include3 + abc$include4)/20

## Religious Conservative

abc$relcon1 <- 6- abc$q44_1
abc$relcon2 <- 6- abc$q44_2
abc$relcon3 <- 6- abc$q44_3
abc$relcon4 <- 6- abc$q44_4
abc$relcon5 <- 6- abc$q44_5
abc$relcon6 <- 6- abc$q44_6
abc$relcon1 <- recode(abc$relcon1, "6=0")
abc$relcon2 <- recode(abc$relcon2, "6=0")
abc$relcon3 <- recode(abc$relcon3, "6=0")
abc$relcon4 <- recode(abc$relcon4, "6=0")
abc$relcon5 <- recode(abc$relcon5, "6=0")
abc$relcon6 <- recode(abc$relcon6, "6=0")

abc$relcon <- abc$relcon1 + abc$relcon2 + abc$relcon3 + abc$relcon4 + abc$relcon5 + abc$relcon6
abc$relcon <- abc$relcon/30

## Years of Service
abc <- read.csv("abc.csv")
abc$yrsmin <- abc$q45_1_text
abc$yrscong <- abc$q45_2_text

## Age
abc$age<- 2016 - abc$q46_1
abc$age[abc$age ==2016] <- NA

## Education

abc$educ <- factor(abc$q48)
abc$educ <- ordered(abc$educ, levels = c(3,4,5,6), labels = c("Some College", "College Degree", "Graduate Work", "Doctoral Degree"))


write.csv(abc, "abc_final.csv")
