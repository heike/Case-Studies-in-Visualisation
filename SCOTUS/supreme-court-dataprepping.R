library("httr")
library("XML")

getTables <- function(url) {
  # Define certicificate file
  cafile <- system.file("CurlSSL", "cacert.pem", package = "RCurl")
  
  # Read page
  page <- GET(
    url, 
    config(cainfo = cafile)
  )
  # Use regex to extract the desired table
  readHTMLTable(content(page, as='text'))
}

fixStuff <- function(x) {
  gsub("\\[.*\\]", "", x)
}


fixName <- function(name) {
  name <- gsub(" +", " ", name, fixed=TRUE)
  name <- gsub("\n", " ", name, fixed=TRUE)
  name <- gsub("*", "", name, fixed=TRUE)
  name <- gsub("†", "", name, fixed=TRUE)
  name <- gsub("([A-Z][A-Z a-z()0-9']+)(.*)\\1", "\\1\\2", name)
  name <- gsub("([A-Z][A-Za-z .']*)\\1", "\\1", name)
  splits <- strsplit(name, split="/") 
  sapply(splits, function(x) paste(unique(x),sep="/", collapse="/"))
}


# brute force approach:
getDate <- function(label) {
  # first eight digits are yyyymmdd for born, voted into office and resigned/retired/died
  # pull out first eight digits and transform to a date
  require(lubridate)
  dates <- substr(label, 1, 8)
  ymd(dates)
}

##################
# Data

nominations <- "https://en.wikipedia.org/wiki/List_of_nominations_to_the_Supreme_Court_of_the_United_States"
# Parse the table
tabs <- getTables(nominations)
nominations <- tabs[[2]]
names(nominations) <- gsub("\n"," ", names(nominations))

presURL <- "http://australianpolitics.com/united-states-of-america/president/list-of-presidents-of-the-united-states"
# Parse the table
tabs <- getTables(presURL)
presidents <- tabs[[1]]
names <- as.character(unlist(presidents[2,]))
names(presidents) <- names
names(presidents) <- gsub("\n"," ", names(presidents))
presidents <- presidents[-(1:2),]
presidents$start <- as.numeric(substr(as.character(presidents$Term), 1,4))
presidents$end <- as.numeric(substr(as.character(presidents$Term), 6,9))
pnames <- strsplit(as.character(presidents$Name), split=" ")
presidents$Last <- unlist(plyr::ldply(pnames, function(x) x[length(x)]))
presidents$Party <- gsub("\n","", presidents$Party)

presidents$end[presidents$Last=="Obama"] <- 2016
presidents$end[presidents$Name=="William H. Harrison"] <- 1841
presidents$end[presidents$Last=="Garfield"] <- 1881

#justices <- "https://en.wikipedia.org/wiki/List_of_Justices_of_the_Supreme_Court_of_the_United_States"
justices <- read.csv("justices.csv")
justices$Judge <- gsub("\xca","", justices$Judge)
justices$Judge <- gsub("*","", justices$Judge, fixed=TRUE)


justices$born <- as.numeric(substr(justices$Born.Died, 1, 4))
justices$died <- as.numeric(substr(justices$Born.Died, 6, 9))
justices$appointed <- as.numeric(substr(justices$Active.service, 1, 4))
justices$retired <- as.numeric(substr(justices$Active.service, 6, 9))

justices$Reason.for <- fixStuff(justices$Reason.for)
justices$terminated <- with(justices, pmin(died, retired, na.rm=T))

elections <- seq(1788, 2016, by=4)

justices$electionYearAppointment <- justices$appointed %in% elections

library(lubridate)
library(ggplot2)
nominations$submit <- mdy(nominations$`Date of Submission to Senate`)
nominations$confirm <- mdy(nominations$`Date of Result`)
nominations$electionYearSubmit <- year(nominations$submit) %in% elections
nominations$electionYearConfirm <- year(nominations$confirm) %in% elections

qplot(appointed, electionYearAppointment, data=justices) + 
  geom_text(aes(label=Judge), angle=45, hjust=0, vjust=0,
            data=subset(justices, electionYearAppointment))

qplot(x=year(nominations$submit), y=year(nominations$submit) %% 4,  colour=Result, #geom="jitter", 
      data=nominations)
qplot(x=year(nominations$confirm), y=year(nominations$confirm) %% 4,  colour=Result, #geom="jitter", 
      data=nominations)

qplot(appointed, electionYearAppointment, data=justices) + 
  geom_text(aes(label=Judge), angle=45, hjust=0, vjust=0,
            data=subset(justices, electionYearAppointment))

qplot(year(submit), electionYearSubmit, data=nominations) + 
  geom_text(aes(label=Nominee), angle=45, hjust=0, vjust=0,
            data=subset(nominations, electionYearSubmit))

# use nominations/justices data to come up with a timeline of justices ... and presidents?
justices$terminated[justices$Reason.for=="Currently serving"] <- 2016

nominations$Nominee <- gsub("\\[[0-9]\\]","",nominations$Nominee)
nominations$Nominee[nominations$Nominee=="John Roberts"] <- "John G. Roberts"
justices[grep("Lucius", justices$Judge),"Judge"] <- "Lucius Quintus Cincinnatus Lamar"

all <- merge(justices, nominations, by.x="Judge", by.y="Nominee", all=T)
all <- all[order(all$submit),]


all$Judge <- reorder(all$Judge, all$appointed, min)
judges <- gsub(", [J|S]r.", "", as.character(all$Judge))
jnames <- strsplit(judges, split=" ")
all$Last <- unlist(plyr::llply(jnames, function(x) {
  if (length(x) ==2) return(x[2])
  n = length(x)
  if (nchar(x[n]) <=  2) res <- x[n-1]
  else res <- x[n]
  gsub(",","", res)
}))
all$Last[all$Last == "Devanter"] <- "Van Devanter"
all$terminated[all$Judge == "Edward Douglass White" & all$`Seat Rank` == "AJ"] <- 1910
all$appointed[all$Judge == "Edward Douglass White" & all$`Seat Rank` == "CJ"] <- 1910
all$terminated[all$Judge == "William Rehnquist" & all$`Seat Rank` == "AJ"] <- 1986
all$Reason.for[all$Judge == "William Rehnquist" & all$`Seat Rank` == "AJ"] <- "Resignation"
all$appointed[all$Judge == "William Rehnquist" & all$`Seat Rank` == "CJ"] <- 1986
all$appointed[all$Judge == "Harlan F. Stone" & all$`Seat Rank` == "CJ"] <- 1941
all$terminated[all$Judge == "Harlan F. Stone" & all$`Seat Rank` == "AJ"] <- 1941
all$Reason.for[all$Judge == "Harlan F. Stone" & all$`Seat Rank` == "AJ"] <- "Resignation"
all[all$Judge == "Charles Evans Hughes",] 
# Charles Evans Hughes is duplicated:
all <- all[-99,]
all <- all[-112,]

write.csv(all, file="justices-nominations.csv", row.names=FALSE)

ggplot(data=all, aes(x=appointed, xend=terminated, y=Judge, yend=Judge)) + geom_segment()

subset(all, Replacing=="Inaugural")
all$Line <- NA
#all$Line[all$Replacing=="Inaugural"] <- as.character(all$Last)[all$Replacing=="Inaugural"]
CJ <- grep("CJ", all$`Seat Rank`)
all$Line[CJ] <- "Chief Justice"


ggplot(data=all) + 
  geom_segment(aes(x=appointed, xend=terminated, y=Line, yend=Line, colour=Judge)) +
  theme(legend.position="none") +
  geom_point(aes(x=year(submit), y=Line))



findNextID <- function(id) {
  grep(all$Last[id], all$Replacing)
}

succ <- function(line, currentID) {
  all$Line[currentID] <<- line
  ids <- findNextID(currentID)
#  browser()
  if( length(ids) == 0) return()
  if (length(ids) > 1) {
    # no Chief justices
    ids <- ids[which(all$`Seat Rank`[ids] != "CJ")]
  }
  if (length(ids) > 1) {
    # get rid of all replacements that are submitted at the wrong time
    ended <- all[currentID, "terminated"]
    submit <- year(all[ids,]$submit)
    ids <- ids[which((submit-ended) <= 3 & (submit-ended) >= -1)]
  }
  all$Line[ids] <<- line
  
  id <- ids[which(all$Result[ids]=="confirmed")]
  if( length(id) == 0) return()
  
  if (length(id) > 1) {
    ended <- all[currentID, "terminated"]
    id <- id[which.min(abs(all$appointed[id]-ended))]
  }
  if (length(id) != 1) browser()
  
  succ(line, id)
}

succ("AJ (seat 1)", grep("Wilson", all$Last))
succ("AJ (seat 2)", 7)
succ("AJ (seat 3)", grep("Blair", all$Last))
succ("AJ (seat 4)", 4)
succ("AJ (seat 5)", grep("Iredell", all$Last))
succ("AJ (seat 6)", grep("Todd", all$Last))
succ("AJ (seat 7)", grep("Catron", all$Last))
succ("AJ (seat 8)", grep("McKinley", all$Last))
succ("AJ (seat 9)", grep("Field", all$Last))
succ("AJ (seat 10)", grep("Bradley", all$Last))
#succ("Harrison", grep("Harrison", all$Last))
#succ("Hoar", grep("Hoar", all$Last))
all$Line[grep("Hoar", all$Last)] <- "AJ (seat 10)"  # rejected by the senate
all$Line[grep("Harrison", all$Last)] <- "AJ (seat 5)"  # 

#succ("Smith", grep("Smith", all$Last))
all$Line[grep("Smith", all$Last)] <- "AJ (seat 8)"  # after he declined, McKinley was submitted

# all$Line <- reorder(all$Line, all$appointed, function(x) min(x, na.rm=TRUE))
ggplot(data=all) + 
  geom_rect(aes(xmin=changes-0.5, xmax=changes), ymin=0, ymax=12, fill="grey70", data=data.frame(changes=changes)) +
  geom_segment(aes(x=appointed, xend=terminated, y=Line, yend=Line, colour=Judge)) +
  theme(legend.position="none") +
  geom_point(aes(x=year(submit), y=Line, colour=Result)) +
  geom_label(aes(x=(appointed+terminated)/2, y=Line, label=Last), size=3,  nudge_y=0.3, data=subset(all, Result=="confirmed"))

all$Line <- factor(all$Line, levels = c("Chief Justice", paste("AJ (seat ",1:10,")",sep="")))
all$LineNum <- as.numeric(all$Line)


library(dplyr)
all <- all %>% group_by(Judge) %>% mutate(n=n())

# John Rutledge is in there 4 times right now - that's an error in the merging
which(all$Judge =="John Rutledge")
all <- all[-5,]
which(all$Judge =="John Rutledge")
all <- all[-11,]

# Reuben Walworth is in 3 times, but that was all for different nominations

# quite a few Associate Justices became Chief Justice later on. This means, they are in the dataset at least twice, but 
# the reason for going out of the office is not necessarily right, because the wiki page counts AJ and CJ service as one

unique(subset(all, n==2)$Judge)
data.frame(subset(all, Judge=="John Jay"))
data.frame(subset(all, Judge=="William Cushing")) # obviously had to resign from AJ
all$Reason.for[which(all$Judge=="William Cushing" & all$`Seat Rank`=="AJ")] <- "Resignation"
data.frame(subset(all, Judge=="William Paterson"))
data.frame(subset(all, Judge=="Roger B. Taney"))
all$Reason.for[which(all$Judge=="Roger B. Taney" & all$`Seat Rank`=="AJ")] <- "Resignation"
data.frame(subset(all, Judge=="John Canfield Spencer"))
data.frame(subset(all, Judge=="Edward King"))
data.frame(subset(all, Judge=="Stanley Matthews"))
data.frame(subset(all, Judge=="William Hornblower"))
data.frame(subset(all, Judge=="Edward Douglass White"))
all$Reason.for[which(all$Judge=="Edward Douglass White" & all$`Seat Rank`=="AJ")] <- "Resignation"
data.frame(subset(all, Judge=="Charles Evans Hughes"))
data.frame(subset(all, Judge=="Pierce Butler"))
data.frame(subset(all, Judge=="Harlan F. Stone"))
data.frame(subset(all, Judge=="John Marshall Harlan II"))
data.frame(subset(all, Judge=="Abe Fortas"))
data.frame(subset(all, Judge=="William Rehnquist"))





all$Died <- all$Reason.for == "Death"
all$Died <- factor(all$Died)
levels(all$Died) <- c("Resigned/Retired/Currently Serving", "Died in Office")




write.csv(presidents, "presidents.csv", row.names=FALSE)
write.csv(all, "justices-nominations.csv", row.names=FALSE)

############
