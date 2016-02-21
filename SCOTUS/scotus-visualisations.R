all <- read.csv("justices-nominations.csv")
presidents <- read.csv("presidents.csv")

number <- c(6, 5, 7, 9, 10, 7, 9)
changes <- c(1789, 1801, 1807, 1837, 1863, 1866, 1869)


presEnd <- 14

ggplot(data=subset(all, Reason.for!="Rejection")) + 
  geom_rect(aes(xmin=start, xmax=end-0.1), alpha=0.6,  ymin=0, ymax=presEnd,  fill="grey80", data=subset(presidents, Party=="None")) +
  geom_rect(aes(xmin=start, xmax=end-0.1), alpha=0.6,  ymin=0, ymax=presEnd,  fill="#ea9978", data=subset(presidents, Party=="Federalist")) +
  geom_rect(aes(xmin=start, xmax=end-0.1), alpha=0.6,  ymin=0, ymax=presEnd,  fill="#008000", data=subset(presidents, Party %in% c("National-Republican", "Democratic-Republican"))) +
  geom_rect(aes(xmin=start, xmax=end-0.1), alpha=0.6,  ymin=0, ymax=presEnd,  fill="#F0DC82", data=subset(presidents, Party=="Whig")) +
  geom_rect(aes(xmin=start, xmax=end-0.1), alpha=0.6,  ymin=0, ymax=presEnd,  fill="#E91D0E", data=subset(presidents, Party=="Republican")) +
  geom_rect(aes(xmin=start, xmax=end-0.1), alpha=0.6,  ymin=0, ymax=presEnd,  fill="#232066", data=subset(presidents, Party=="Democrat")) +
  geom_text(aes(x=end, label=Name, y=presEnd-0.1), size=7, data=subset(presidents, !(Party %in% c("Federalist","None", "Whig"))), 
            vjust=0, hjust=1, angle=90,  nudge_x=-.5,  colour="grey90") +
  geom_text(aes(x=end, label=Name, y=presEnd-0.1), size=7, data=subset(presidents, (Party %in% c("Federalist", "None", "Whig"))), 
            vjust=0, hjust=1, angle=90,  nudge_x=-.5,  colour="grey60") +
#  geom_rect(aes(xmin=elections-1, xmax=elections), ymin=0, ymax=12, fill="grey85", data=data.frame(elections=elections)) +
#  geom_rect(aes(xmin=changes-0.5, xmax=changes), ymin=0, ymax=12, fill="grey85", data=data.frame(changes=changes)) +
  geom_rect(aes(xmin=appointed, xmax=terminated, ymin=LineNum-.4, ymax=LineNum+.4,
                fill=Died, colour= Died), data=subset(all, Result=="confirmed" & Reason.for != "Rejection")) +
  geom_point(aes(x=year(submit), y=LineNum)) + #, colour=electionYearSubmit)) +
  geom_label(aes(x=(appointed+terminated)/2, y=LineNum, label=Last, fill=Died, colour=Died), size=3, data=subset(all, Result=="confirmed")) +
  theme_bw() +
  #  theme(legend.position="none") + 
  scale_y_discrete(breaks = 1:11, label=levels(all$Line), limits=1:presEnd) +
  scale_x_continuous(breaks=c(changes, seq(1788, 2016, by=8)), 
                     labels=c(paste("\n",c("SCOTUS size: 6",number[-1])), seq(1788, 2016, by=8))) +
  theme(legend.position="bottom") +
  scale_fill_manual("", values=c( "black", "grey97")) +
  scale_colour_manual("", values=c("grey97", "black")) +
  xlab("Year") +
  ylab("Justice line") 
