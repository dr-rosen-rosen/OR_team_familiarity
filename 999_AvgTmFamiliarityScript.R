library(readxl)
#load file with all team rosters
#make sure that the teams are listed in chronological order and that the the team names over time are in alphanumeric order
#(that might not be necessary but ensures that the assessment of potential past experiences is pulling on the right set of past teams)
roster <- read_excel("C:/Users/Anna/Dropbox/Children's Hospital/quant data/CHPRosters.xlsx")
roster<-as.data.frame(roster)
#make sure the individual identifier (I called this var MaskedIDa) is a factor variable
roster$MaskedIDa<-as.factor(roster$MaskedIDa)
#make sure the team identifier (I called this var WeekColor) is a factor variable
roster$WeekColor<-as.factor(roster$WeekColor)

#create function to determine familiarity at dyad level for each team

#load function that will extract each dyad and, per team, look for past instances of that dyad to calculate familiarity for the dyad 
Familiarity<-function (dat) {
  #data frame that holds all of the generated pairs
  finalDf<-data.frame()
  #get all the unique teams to loop over
  Teams<- unique(dat$Team)
  for(Team in Teams) {
    subset<-dat[dat$Team==Team,]
    #vector of people
      allIDs<-as.vector(dat$MaskedIDa)
    #make vector of pairs for each subset
      personIDs<- as.vector(subset$MaskedIDa)
      #get pair matrix of personIDs (2 columns)
      pairsDf<-as.data.frame(t(combn(personIDs,2)))
      #make readable column names
      names(pairsDf)<-c("Person1","Person2")
      pairsDf$Person1<-as.factor(pairsDf$Person1)
      pairsDf$Person2<-as.factor(pairsDf$Person2)
      #in pairsDf, create a column of zeros in preparation for results
      pairsDf$familiarity<-0
      #create count variable to sum
      pairsDf$dyad<-1
      #add in identifying information
      pairsDf$Team<-Team
      #stack this iterations pairsDf with all the other ones
      #loop through all rows for the focal team and fill in total familiarity
      holding<-rbind(finalDf,pairsDf)
      teamsubset<-data.frame()
      teamoutput<-data.frame()
      for (row in 1:nrow(pairsDf)) {
        #levels(pairsDf$Person1)<-levels(holding$Person1)
        #levels(pairsDf$Person2)<-levels(holding$Person2)
        P1<-(pairsDf$Person1[row])
        P1<-as.vector(P1)
        P2<-(pairsDf$Person2[row])
        P2<-as.vector(P2)
        likepairs<-subset(holding, holding$Person1 == P1 & holding$Person2 == P2)
        #teamsubset$Person1 <- pairsDf$Person1[row]
        #teamsubset$Person2 <- pairsDf$Person2[row]
        #teamsubset$Team <- pairsDf$Team
        pairsDf$familiarity[row] <- sum(likepairs$dyad)
        dyad<-subset(pairsDf, pairsDf$Person1 == pairsDf$Person1[row] & pairsDf$Person2 == pairsDf$Person2[row])
        teamoutput<-rbind(teamoutput,dyad)
      }
      finalDf<-rbind(finalDf,teamoutput)
      print(Team)
  }
  #return finalDf
  finalDf
}


#run the function on your data set
teamfamiliarity<-Familiarity(roster)

#I had two team identifiers and created a column for one of them in the above function, so, this is an additional step that
#brings in the second team identifier
#from the roster, extract just the team identifiers (I had two variables hence taking two columns)
TeamNumandName<-roster[1:2]
TeamNumandName<-unique(TeamNumandName)

#merge output with the initial roster, based on the team identifier used in the function
#this creates the final dyadic output where each dyad has both team identifiers and a familiarity number
TeamFamiliarityDataDyads<-merge(teamfamiliarity,TeamNumandName,by="Team")

#create a "Team Familiarity" score. I did this by taking the average of a team's dyads' familiarity:

#generic function that takes the means, ignoring any NAs (you should not have any NAs, I just had this function from other work)
library(dplyr)
my_mean <- function(x) {
  mean(x, na.rm = TRUE)
}
#take the average by team:
AvgTeamFamiliarity<-TeamFamiliarityDataDyads %>% group_by(WeekColor) %>% summarise_at(c("familiarity"),funs(my_mean))

#write output to file:
write.csv(AvgTeamFamiliarity,file="C:/Users/Anna/Dropbox/Children's Hospital/quant data/AvgTmFamiliarity.csv")