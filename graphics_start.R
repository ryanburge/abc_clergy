ggplot(abc, aes(q21)) + geom_bar(aes(y = ((..count..)/sum(..count..))*100),  fill="red", colour="black")+ scale_x_continuous(breaks = c(1,2,5,3,4,6), labels = c("Clinton", "Trump", "Johnson", "Stein", "Other", "None")) + ggtitle("If the election were to be held today, for which presidential candidate would you vote?") + ylab("Percent of Sample") +

ggplot(abc, aes(q66_1)) + geom_bar(aes(y = ((..count..)/sum(..count..))*100),  fill="red", colour="black")+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), labels = c("Strongly Agree", "Agree", "Somewhat Agree", "Neither Agree or Disagree", "Somewhat Disagree", "Disagree", "Strongly Disagree")) + 
  ggtitle("                        Public support for Donald Trump will hurt Christian witness ") + 
  ylab("Percent of Sample") + theme(axis.title.x=element_blank())  + theme(axis.text.x = element_text(angle = 90))

ggplot(abc, aes(q66_5)) + geom_bar(aes(y = ((..count..)/sum(..count..))*100),  fill="red", colour="black")+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), labels = c("Strongly Agree", "Agree", "Somewhat Agree", "Neither Agree or Disagree", "Somewhat Disagree", "Disagree", "Strongly Disagree")) +
  ggtitle("            The bond between evangelicals and the Republican Party is a healthy lasting one.  ") +
  ylab("Percent of Sample") +     theme(axis.title.x=element_blank())  + theme(axis.text.x = element_text(angle = 90))


## Histogram of Falwell Treatment 
forgive <- filter(abc, q70 == 1)
not.forgive <- filter(abc, q71 == 1)
not.forgive$label <- c("No Treatment")
forgive$label <- c("Forgiveness Treatment")
df.plot<- rbind(not.forgive, forgive)

ggplot(data=subset(df.plot, !is.na(q66_1)), aes(x=q66_1, fill=label)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position="dodge") + theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels=percent) + ylab("Percent of Sample") + xlab("Public support for Donald Trump will hurt Christian witness ") +
  theme(text=element_text(size=16, family="Roboto")) +    theme(legend.position="bottom") +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), labels = c("Strongly Agree", "Agree", "Somewhat Agree", "Neither Agree or Disagree", "Somewhat Disagree", "Disagree", "Strongly Disagree"))

## RAV and CR support 
ggplot(abc, aes(x=q66_5, y=rav))  + geom_point(shape =1, position=position_jitter(width=.1,height=.1)) + geom_smooth(method=lm) + xlab("The bond between evangelicals and the Republican Party is a healthy lasting one") + ylab("Religious Authority") + scale_x_continuous(breaks = c(1,2,3,4,5,6,7), labels = c("Strongly Agree", "Agree", "Somewhat Agree", "Neither Agree or Disagree", "Somewhat Disagree", "Disagree", "Strongly Disagree"))