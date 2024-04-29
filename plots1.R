## This code reads expert-elicitation data from the file "data1.csv".
## The code uses that data to make two plots.
## Set working directory to the "inputData" folder.
## Plots will appear in the "outputs" folder.
## No special packages are required. 

##################################################
########## IMPORT AND ORGANIZE SCORING RESULTS FROM EVALUATORS

## import the csv file created by Google Forms
import<-read.csv("data1.csv")

## define schema for translating SCORES in "data1.csv" to numbers
mapping.scores<-c("unpromising"=1, "middling"=2, "promising"=3, "unable to assess"=NA)

## define schema for numbering CRITERIA in "data1.csv"
## (this schema re-numbers some criteria compared to numbering used in the Google Form)
mapping.criteria<-c(
"1. Near-term planning needs for problems with multi-decadal time horizons that permit the adoption of flexible approaches (H1, H3)"=1,
"2. Sensitivity to coastal flooding and other climate hazards, including the confluence of multiple hazards (H1)"=2,
"3. Sensitivity to changing landforms (H1)"=3,
"4. Feedbacks among household decision making about adaptation, municipal finance, housing markets, and insurance markets (H2)"=4,
"5. Potential for analysis to address JEDI"=8,
"8. Absence of a significant, well-funded, current resilience planning effort"=9,
"9. Presence of a population base that includes a high degree of social vulnerability"=14,
"10. Enthusiasm and capacity for participating in sustained engagement over the MACH timeframe"=10,
"11. Availability of partners and plausible funding sources to support implementation"=11,
"12. Complementarity and absence of overlap with existing climate decision support projects"=12
)
                   
## make a new matrix to house data translated from "data1.csv"
num.cand<-ncol(import)-4  #four of the columns contain other stuff
scores<-matrix(nrow=nrow(import), ncol=num.cand+2)  #we want two extra columns

## each row will contain one evaluator's scores on one criterion
## let the first column indicate the criterion
scores[,1]<-mapping.criteria[import[,3]]

## let subsequent columns indicate the scores
for (i in 1:num.cand){
  scores[,i+1]<-mapping.scores[import[,i+3]]
}

## let the final column of "scores" indicate whether the evaluator added a comment
## 1 means yes and 0 means no
scores[,num.cand+2]<-import[,17]!=""

##################################################
########## MAKE OBJECTS TO ASSIST IN LABELING

## anonymized names for the candidate sites
candidates<-c("R1","R2","R3","R4","R5","R6","R7","R8","m1","m2","m3","m4","m5")

## criteria in the order used for figures
## (different from the order used in the data file)
criteria<-c(
  "DAPP-like & tractable",
  "sensitivity to hazards",
  "landform feedbacks",
  "market feedbacks",
#  "tractable quantities",
#  "tractable system model",
#  "access to households",
  "JEDI in analysis",
  "absence of planning",
  "sustained engagement",
  "pathways for benefit",
  "free from conflict",
#  "region-scale links",
  "socially vulnerable"
)

## the criteria with numbers and by-hand line breaks
criteria.breaks<-c(
  "DAPP-like & \ntractable (1,5,6)",
  "sensitivity to \nhazards (2)",
  "landform \nfeedbacks (3)",
  "market \nfeedbacks (4)",
#  "tractable \nquantities (5)",
#  "tractable sys-\ntem model (6)",
#  "access to \nhouseholds (7)",
  "JEDI in \nanalysis (8)",
  "absence of \nplanning (9)",
  "sustained en-\ngagement (10)",
  "pathways for \nbenefit (11)",
  "free from \nconflict (12)",
  "socially \nvulnerable (14)"
#  "region-scale \nlinks (13)"
)

##################################################
########## DEFINE FUNCTIONS TO SIMPLIFY PLOTTING BY CRITERION 

## call this function to plot raw scores from one evaluator on one criterion
## input x is the row number of the evaluator
plot.person<-function(x){
  plot(scores[x,2:14], col=individual, pch=3, ylim=c(.7,3.3), axes=FALSE, cex=1.1, lwd=1.5, bty="n", ylab="")
  if (any(is.na(scores[x,]))){
    abline(v=which(is.na(scores[x,2:14])), col=individual, lwd=1)
  }
  axis(side=2, at=c(1,2,3), labels=c("u", "m", "p"), las=1)
  axis(side=1, at=1:13, labels=1:13, las=1, tick=FALSE, line=-1.3, cex.axis=.5)
}

## call this function to add (to existing plot) scores from additional evaluator on same criterion
## input x is the row number of the evaluator
points.person<-function(x){
  points(scores[x,2:14], col=individual, pch= 3, cex=1.1, lwd=1.5)
  if (any(is.na(scores[x,])))
    abline(v=which(is.na(scores[x,2:14])), col=individual, lwd=1)
}

## call this function to add (to existing plot) the average of several evaluators on one criterion
## input x is the criterion number
points.mean<-function(x){ 
  if (sum(scores[,1]==x)==1){
    points(scores[scores[,1]==x,2:14], pch=1, lwd=.9, cex=1.1)
  }
  else
    points(colMeans(scores[scores[,1]==x,2:14], na.rm=TRUE), pch=1, lwd=.9, cex=1.1)
}

## call this function to add a single criterion label to the left of a plot 
## input x is the criterion number
crit.label<-function(x){
  mtext(criteria.breaks[x], side=2, cex=.6, line=2.6)
}

## call this function to add all candidate names above or below a plot
## input x is the side to plot on (3=above, 1=below)
cand.label<-function(x){ 
  axis(side=x, at=1:13, labels=candidates, las=2, tick=FALSE, line=.1, cex.axis=.9)
}

## call this function to add the number of evaluators to the right of the plot
## input x is the criterion number
annotations.num.eval<-function(x){
  if (sum(scores[,1]==x)==1){ #if one evaluator scored that criterion ...
    points(14,3, xpd=TRUE, pch="1", cex=1,col=individual_b) #write a "1"
  }
  if (sum(scores[,1]==x)==2){ #if two evaluators scored that criterion ...
    points(14,3, xpd=TRUE, pch="2", cex=1,col=individual_b) #write a "2"
  }
  if (sum(scores[,1]==x)==3){ #if three evaluators scored that criterion ...
    points(14,3, xpd=TRUE, pch="3", cex=1,col=individual_b) #write a "3"
  }
  if (sum(scores[,1]==x)==4){ #if three evaluators scored that criterion ...
    points(14,3, xpd=TRUE, pch="4", cex=1,col=individual_b) #write a "3"
  }
}

## call this function to add, to the right of the plot, asterisks representing evaluator comments
## input x is the criterion number
annotations.comments<-function(x){
  if (sum(scores[scores[,1]==x,15])!=0){  #if the number of comments on that criterion is non-zero ...
    points(14,2.2, xpd=TRUE, pch=8, cex=.8, lwd=.5) #add an asterisk
  }
  if (sum(scores[scores[,1]==x,15])>=2){  #if the number of comments on that criterion is two or more...
    points(14,1.7, xpd=TRUE, pch=8, cex=.8, lwd=.5) #add another asterisk below the first one
  }
  if (sum(scores[scores[,1]==x,15])==3){  #if the number of comments on that criterion is three ...
    points(14,1.2, xpd=TRUE, pch=8, cex=.8, lwd=.5) #add another asterisk below the second one
  }
}

## call this function to print the legend for criterion-by-criterion scores
## the function has no input
## there are multiple components to this legend, each placed with a separate "legend" command for more control
legend.by.crit<-function(){
  legend("topleft",inset=c(-.2, -2),legend=c("individual","average"),pch=c(3,1),col=c(individual,"black"),pt.cex=1.1,cex=.8,bty="n",xpd=NA)
  legend("topleft",inset=c(.1, -2),legend=c("unable to assess"),pch = c("|"),col=c(individual),pt.cex= 1.1,cex=.8,bty="n",xpd=NA)
  legend("topleft",inset=c(.52, -2),legend=c("number of evaluators"),pch = c("1"),col = c(individual_b),pt.cex= 1,cex=.8,bty="n",xpd=NA)
  legend("topleft",inset=c(.52, -1.72),legend=c("evaluator comment"),pch = c(8),pt.cex=.8,cex=.8,bty="n",xpd=NA)
}

##################################################
########## PLOT SCORES BY CRITERION

individual<-rgb(0,235,235, max = 255, alpha = 150) #color for individual scores and "unable to assess" lines
individual_b<-rgb(10,235,235, max = 255, alpha = 230)  #slightly darker color for number of evaluators annotation

crit.index<-c(1,2,3,4,8,9,10,11,12,14) #use this to walk through the criteria within for loops

pdf("./../outputs/FigureS1.pdf", height=6, width=4.2)
par(mfrow=c(10,1), mar = c(.3, 9, .3, 1.5) + 0.1, oma=c(2,0,7,1.5), las=1, xpd=FALSE)

for (i in 1:10){  #go criterion by criterion ...
  plot.person(match(crit.index[i],scores[,1]))  #plot scores from first evaluator on that criterion
  if (length(which(scores[,1]==crit.index[i]))>=2){ #if more than one evaluator scored that same criterion ...
    for (j in 2:length(which(scores[,1]==crit.index[i]))){
      points.person(which(scores[,1]==crit.index[i])[j])  #add scores from the other evaluators on same criterion
    }
  }  
  points.mean(crit.index[i])  #plot averages among evaluators for that criterion
  crit.label(i) #add criterion name to the left of the panel
  annotations.num.eval(crit.index[i]) #add info to the right of the plot: number of evaluators
  annotations.comments(crit.index[i]) #add info to the right of the plot: evaluator comment(s)
  if (i==1){  #if it's the first panel in the multiplot ...
    axis(side=3, at=1:26, labels=1:26, las=1, tick=FALSE, line=-.9, cex.axis=.5) #add axis above ...
    cand.label(3) #add candidate labels to that axis
    legend.by.crit() #and make the legend in the upper left corner
  }
} 
#end plot
dev.off() #print plot to file

##################################################
########## DEFINE FUNCTIONS TO SIMPLIFY PLOTTING BY CANDIDATE

## call this function to plot raw scores from all evaluators on a single candidate
## the function plots vertical bars indicating "unable to assess"
## input x is the candidate number
plot.score.cand<-function(x){
  plot(match(scores[,1],crit.index),scores[,x],col=individual,pch=3,ylim=c(.7,5.3),xlim=c(1,10),axes=FALSE,cex=1.1,lwd=1.5,bty="n",ylab="")
  abline(v=c(match(scores[is.na(scores[,x]),1],crit.index)), col=individual, lwd=1)
  axis(side=2, at=c(1,3,5), labels=c("u", "m", "p"), las=1, line=.25)
  axis(side=1, at=1:10, labels=c("1,5,6",crit.index[2:10]), las=1, tick=FALSE, line=-1.3, cex.axis=.5)
}

## call this function to add averages across evaluators for all criteria on one candidate
## input x is the candidate number
points.mean.cand<-function(x){
  means<-vector(length=14)
  for (i in crit.index){
    if (sum(scores[,1]==i)==1) 
      means[i]<-scores[scores[,1]==i,x]
    else
      means[i]<-mean(scores[scores[,1]==i,x], na.rm=TRUE)
  }
  points(1:10,means[crit.index], pch=1, lwd=.9, cex=1.1)
}

## call this function to add the candidate label to the left of the panel 
## input x is the candidate number
side.label<-function(x){
  mtext(candidates[x], side=2, cex=.65, line=2.6)
}

## call this function to add criteria names above or below a panel
## input x is the side to put the names on (top=3, bottom=1)
axis.explainer<-function(x){ 
  axis(side=x, at=1:10, labels=criteria, las=2, tick=FALSE, line=.1, cex.axis=.9)
}

## call this function to print the legend for candidate-by-candidate scores INCLUDING THE TIER-BASED RANKING
## the function has no input
## there are multiple components to this legend, each placed with a separate "legend" command for more control
legend.by.cand<-function(){
  legend("topleft",inset=c(0, -5.3),legend=c("individual","average"),pch=c(3,1),col=c(individual,"black"),pt.cex=1.1,cex=.8,bty="n",xpd=NA)
  legend("topleft",inset=c(.35, -5.3),legend=c("unable to assess"),pch=c("|"),col=c(individual),pt.cex=1.1,cex=.8,bty="n",xpd=NA)
}

##################################################
########## PLOT SCORES BY CANDIDATE

pdf("./../outputs/FigureS2.pdf", height=7.3, width=2.7)
par(mfcol=c(13,1), mar = c(.4, 5, .3, .5) + 0.1, oma=c(1,0,13.5,1), las=1, xpd=FALSE)

for (i in 1:13){
  plot.score.cand(i+1)
  #unable(i+1)
  points.mean.cand(i+1)
  side.label(i)
  if (i==1 || i==14) {  #if it's a top-row panel in the multiplot ...
    axis(side=3, at=1:10, labels=c("1,5,6",crit.index[2:10]), las=1, tick=FALSE, line=-.85, cex.axis=.5) #add an extra axis above ...
    axis.explainer(3) #and add criteria names above the panel
  }
  if (i==1){  #if it's the very first panel in the multiplot ...
    legend.by.cand()  #add the legend
  }
}
#end plot
dev.off() #print plot to file
