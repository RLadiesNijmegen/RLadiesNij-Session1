## R ladies intro survey analysis! ##
## Note: this is not 100% plug and play. Some labels were edited off-line using illustrator
## Laurel Brehm / Amie Fairs 19 June 2018 ##

library(plyr)
library(ggplot2)
library(ggforce)
library(wordcloud)


## Import data files ##
## Amie did some revaluing offline, & I renamed columns
 d <- read.csv("RLadiesNijmegenintroductionsurvey-Deidentified.csv", header=T)
 ## some of the questions (with more than one answer per line are expanded and saved separately)
 q2 <- read.csv("WhatSee.csv", header=T)
 q5 <- read.csv("WhatR.csv", header=T)
 q7 <- read.csv("Packages.csv", header=T)
 q9 <- read.csv("OtherSoftware.csv", header=T)
 q12 <- read.csv("HelpR.csv", header=T)

 ##set color palette
 # http://www.color-hex.com/color-palette/61807
 
 ## 3 colors
 col3 <- c('#fde500','#13c7ee','#b5eebc')
 
 ## 4 colors
 col4 <- c('#fde500','#13c7ee','#b5eebc','#f000ff')
 
 ## gradients of pink
 grad <- colorRampPalette(c("white",'#f000ff'))
 
 
 ## Question 1: what's your level in R?
d$LevelR <- factor(d$LevelR, levels=c("Never used it but I would like to begin","Beginner","Intermediate","Advanced"))

dR <- as.data.frame(table(d$LevelR))
colnames(dR)[1]<- "LevelR"
dR$LabelPos <- c(2,9,26,40) ## these are hand-adjusted to look pretty on my screen
dR$Freq <- as.numeric(as.character(dR$Freq))
dR$Perc <- paste(round(eval(dR$Freq/sum(dR$Freq)),4)*100,"%")  ## make a label of the proportion of whole

## horizontal stacked bar plot with labels underneath
ggplot(dR,aes(x=1,y=Freq,fill=LevelR)) + geom_bar(color='black',stat="identity",position = position_stack(reverse = TRUE)) +
  geom_text(aes(x=0.5,label=Perc,y=LabelPos))+
  geom_text(aes(label=LevelR,y=LabelPos,angle=90))+
  coord_flip() + scale_x_continuous("",breaks=NULL) + scale_y_discrete("",breaks=NULL) + theme_void() +
  scale_fill_manual(values=col4, guide=F) + ggtitle("What is your level in R?" ) 


## Question 2: What would you like to see at RLadies Nijmegen? (please check all that apply)
dWhat <- as.data.frame(table(q2))
dWhat <- dWhat[dWhat$Freq != 0 ,]
dWhat$YRank <- 9-as.numeric(as.factor(dWhat$Freq))
dWhat[5,4]=9  ## revalue 2 of them to eliminate overlap
dWhat[7,4]=10

## bubble plot. x-axis: category of topic, y-axis sorted by ordinal rank of topic
## bubble size reflects number of votes
ggplot(dWhat,aes(x0=as.numeric(WhatIs)*50,y0=YRank*50,r=Freq,fill=WhatIs)) +
  geom_circle() + scale_fill_manual(values=col3, guide=F) +
  geom_text(aes(x=as.numeric(WhatIs)*50,y=YRank*50,label=WhatSee)) + theme_void() +
  ggtitle("What would you like to see at RLadies Nijmegen?")
  

## Question 3: Are you in academia or industry?
dB <- as.data.frame(table(d$Background))
colnames(dB)[1]<- "Background"
dB$LabelPos <- c(21,42.5,43.5)  ## more hand revaluing
dB$LabelPos2 <- c(21,41,45)
dB$Freq <- as.numeric(as.character(dB$Freq))
dB$Perc <- paste(round(eval(dB$Freq/sum(dB$Freq)),4)*100,"%")

##horizontal stacked bar
ggplot(dB,aes(x=1,y=Freq,fill=Background)) + geom_bar(color='black',stat="identity",position = position_stack(reverse = TRUE)) +
  geom_text(aes(x=0.5,label=Perc,y=LabelPos2))+
  geom_text(aes(label=Background,y=LabelPos,angle=90))+
  coord_flip() + scale_x_continuous("",breaks=NULL) + scale_y_discrete("",breaks=NULL) + theme_void() +
  scale_fill_manual(values=col3, guide=F) + ggtitle("Are you in academia or industry?" ) 


## Question 4: If academia, which level?
d$AcademicLevel <- factor(d$AcademicLevel, levels=c("Masters student","PhD","Post-doc","Assistant Professor/ Prof"))

dA <- as.data.frame(table(d$AcademicLevel))
colnames(dA)[1]<- "AcademicLevel"

dA$LabelPos <- c(5,20,37,42)

dA$Freq <- as.numeric(as.character(dA$Freq))
dA$Perc <- paste(round(eval(dA$Freq/sum(dA$Freq)),4)*100,"%")

## horizontal stacked bar
ggplot(dA,aes(x=1,y=Freq,fill=AcademicLevel)) + geom_bar(color='black',stat="identity",position = position_stack(reverse = TRUE)) +
  geom_text(aes(x=0.5,label=Perc,y=LabelPos))+
  geom_text(aes(label=AcademicLevel,y=LabelPos,angle=90))+
  coord_flip() + scale_x_continuous("",breaks=NULL) + scale_y_discrete("",breaks=NULL) + theme_void() +
  scale_fill_manual(values=col4, guide=F) + ggtitle("If academia, which level?" ) 



## Question 5: What do you use R for?
dWhatR <- as.data.frame(table(q5))
dWhatR <- dWhatR[dWhatR$Freq != 0 ,]
dWhatR$YRank <- 8-as.numeric(as.factor(dWhatR$Freq))
dWhatR <- dWhatR[dWhatR$WhatR!="Statistics",]    ## because this has complete overlap with data analysis

## bubble plot. x-axis: category of topic, y-axis sorted by ordinal rank of topic
## bubble size reflects number of votes
ggplot(dWhatR,aes(x0=as.numeric(WhatIs)*50,y0=YRank*50,r=Freq,fill=WhatIs)) +
  geom_circle() + scale_fill_manual(values=col3, guide=F) +
  geom_text(aes(x=as.numeric(WhatIs)*50,y=YRank*50,label=WhatR)) + theme_void() +
  ggtitle("What would you like to see at RLadies Nijmegen?")


## Question 6: How long have you used R?
## histogram with labels for filled points
ggplot(d,aes(x=LongRN))+ geom_histogram(binwidth=1,fill='#f000ff',color="black") + scale_y_continuous("Number of R Ladies") +
  scale_x_continuous("Years of R usership",breaks=c(0,1,2,3,4,5,7,10)) + theme_bw() + ggtitle("How long have you used R?")


## Question 7: What packages do you use?
## a wordcloud, just to do something diferent
wordcloud(as.matrix(q7),min.freq=1)


## Question 8: Are you in other groups?  
## pie chart, just to do something different
dOther <- as.data.frame(table(d$OtherGroups))
pie(dOther$Freq,dOther$Var1,col=col3,main="Are you in other R groups?")


## Question 9: What other software do you use?
## wordcloud
wordcloud(as.matrix(q9),min.freq=1)

## Question 10: Would you be willing to be actively involved in mentorship?
## being a little sloppy in my tabulation labeling because it goes nearly immediately to the plot
dMent <- as.data.frame(table(d$ActiveMentor))
dMent$Var1 <- as.character(dMent$Var1)

## pie chart
pie(dMent$Freq,dMent$Var1,col=col3,main=" Would you be willing to be actively involved in mentorship?")

## Question 11: Would you be wiling to be actively involved in organizing?
## being a little sloppy in my tabulation labeling because it goes nearly immediately to the plot
dOrg <- as.data.frame(table(d$ActiveOrganize))
dOrg$Var1 <- as.character(dOrg$Var1)

pie(dOrg$Freq,dOrg$Var1,col=col3,main=" Would you be willing to be actively involved in organizing?")


## Question 12: What would you be willing to mentor or organize?
dHelp <- as.data.frame(table(q12))
dHelp$YRank <- 6-as.numeric(as.factor(dHelp$Freq))

## bubble plot. x-axis: category of topic, y-axis sorted by ordinal rank of topic
## bubble size reflects number of votes
ggplot(dHelp,aes(x0=50,y0=YRank*30,r=Freq)) +
  geom_circle(fill='#f000ff') +
  geom_text(aes(x=50,y=YRank*30,label=q12)) + theme_void() +
  ggtitle("What would you be willing to mentor or organize?")



## Question 13: Do you commute?
dCom <- as.data.frame(table(d$Commute))

pie(dCom$Freq,dCom$Var1,col=col3,main="Will you commute to these meetings?")


## Question 14: How long is your commute?
## frequency polygon for this one
ggplot(d,aes(x=CommuteNumeric))+ geom_freqpoly(binwidth=15,color='#f000ff',size=2) + scale_y_continuous("Number of R Ladies") +
 scale_x_continuous("Commute Time (Minutes)") + theme_bw() + ggtitle("How long is your commute?")


## Question 15: Scheduling preferences?
## build table
Calendar <- rbind(c(35,5),c(11,10),c(11,10))
colnames(Calendar)=c("Weekday","Weekend")
rownames(Calendar)=c("Evening","Afternoon","Morning")
Calendar <-(as.matrix(Calendar))

### build heat map with annotations-- will only work in certain size plot windows
heatmap(Calendar,Rowv=NA, Colv=NA, col = grad(20),
      labRow="",labCol="",
      scale="none",main=" \n  \n")
mtext("Weekdays",3,4,at=-1)
mtext("Weekends",3,4,at=0.25)
mtext("Morning",2,3,at=1)
mtext("Afternoon",2,3,at=0)
mtext("Evenings",2,3,at=-1)

      