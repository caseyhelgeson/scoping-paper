## This code reads expert-elicitation data from the file "data2.csv".
## The code uses that data to make several plots.
## Set working directory to the "inputData" folder.
## Plots will appear in the "outputs" folder. 

## Requires RColorBrewer
library(RColorBrewer)

##################################################
########## IMPORT AND ORGANIZE SCORING RESULTS FROM EVALUATORS

## import the csv file created by Google Forms
import<-read.csv("data2.csv")

## define schema for translating scores in "scoring_sheet_2.csv" to numbers
mapping.scores<-c("1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "unable to assess"=NA)

## define schema for translating criteria in "scoring_sheet_2.csv" to numbers
## (this schema re-numbers some criteria compared to numbering used in the Google Form)
mapping.criteria<-c(
"1. Near-term planning needs for problems with multi-decadal time horizons that permit the adoption of flexible approaches"=1,
"2. Sensitivity to coastal flooding and other climate hazards, including the confluence of multiple hazards"=2,  
"3. Sensitivity of coastal hazards to changing landforms"=3,    
"4. Feedbacks among household decision making about adaptation, municipal finance, housing markets, and insurance markets"=4,
"4* Availability of community partners with which to study household-level dynamics"=7,
"5. Potential for analysis to address JEDI"=8,
"6. Analytical tractability – Quantifiability"=5,
"7. Analytical tractability – System Model"=6,
"8. Absence of a significant, well-funded, current resilience planning effort"=9,
#"9. Presence of a population base that includes a high degree of social vulnerability", #not used for the shortlist
"10. Enthusiasm and capacity for participating in sustained engagement over the MACH timeframe"=10,
"11. Potential for research to benefit the community"=11,  
"12. Free from conflict with other climate decision support projects"=12,
"13. Problem connects appropriately with anticipated regional-scale research"=13
)

## make a new matrix to house data translated from "scoring_sheet_2.csv"
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
scores[,num.cand+2]<-import[,30]!=""

##################################################
########## MAKE OBJECTS TO ASSIST IN LABELING

## anonymized names for the candidate decision problems 
candidates<-c(
  "R1 harden infrastructure",
  "R1 port terminal design",
  "R1 combined sewer",
  "R1 flood hazard",
  "R1 master planning",
  "m1 harden housing",
  "m1 resilience planning",
  "R3 landfill closure",
  "R3 freshwater marsh",
  "R3 wetland restoration",
  "R3 restore tidal marsh",
  "R3 remediation site",
  "R3 road improvement",
  "R3 superfund site",
  "R3 flood protection",
  "R3 state park design",
  "R3 sewer redesign",
  "R3 flood ordinance",
  "m4 redevelop downtown",
  "m4 facilities relocation",
  "m4 road elevation",
  "m4 damaged housing",
  "m4 wastewater treatment",
  "m4 flood measures",
  "m4 beach nourishment",
  "m4 protect coastal park" 
)

## candidate names again but no location code
candidates.no<-c(
  "harden infrastructure",
  "port terminal design",
  "combined sewer",
  "flood hazard",
  "master planning",
  "harden housing",
  "resilience planning",
  "landfill closure",
  "freshwater marsh",
  "wetland restoration",
  "restore tidal marsh",
  "remediation site",
  "road improvement",
  "superfund site",
  "flood protection",
  "state park design",
  "sewer redesign",
  "flood ordinance",
  "redevelop downtown",
  "facilities relocation",
  "road elevation",
  "damaged housing",
  "wastewater treatment",
  "flood measures",
  "beach nourishment",
  "protect coastal park" 
)

## candidate names again but shortened
candidates.short<-c(
  "infrastructure",
  "port terminal",
  "comb. sewer",
  "flood hazard",
  "master plan",
  "housing",
  "resilience",
  "landfill closure",
  "fresh marsh",
  "wetland rest.",
  "tidal marsh",
  "remediation",
  "road improve",
  "superfund site",
  "flood protect.",
  "park design",
  "sewer redesign",
  "flood ordinance",
  "downtown",
  "facilities",
  "road elevation",
  "dmgd. housing",
  "wastewater",
  "flood measures",
  "beach nourish.",
  "coastal park" 
)

candidates.breaks<-c(
  "harden infra-\nstructure (1)",
  "port terminal \ndesign (2)",
  "combined \nsewer (3)",
  "flood \nhazard (4)",
  "master \nplanning (5)",
  "harden \nhousing (6)",
  "resilience \nplanning (7)",
  "landfill \nclosure (8)",
  "freshwater \nmarsh (9)",
  "wetland rest-\noration (10)",
  "restore tidal \nmarsh (11)",
  "remedia-\ntion site (12)",
  "road improve-\nment (13)",
  "superfund \nsite (14)",
  "flood protec-\ntion (15)",
  "state park \ndesign (16)",
  "sewer \nredesign (17)",
  "flood \nordinance (18)",
  "redevelop \ndowntown (19)",
  "facilities \nrelocation (20)",
  "road \nelevation (21)",
  "damaged \nhousing (22)",
  "wastewater \ntreatment (23)",
  "flood \nmeasures (24)",
  "beach nourish-\nment (25)",
  "protect coastal \npark (26)" 
)

## criteria in the order used for figures
## DIFFERENT FROM THE ORDER in the data file 
criteria<-c(
  "complex planning needs",
  "sensitivity to hazards",
  "landform feedbacks",
  "market feedbacks",
  "tractable quantities",
  "tractable system model",
  "access to households",
  "JEDI in analysis",
  "absence of planning",
#  "socially vulnerable", #not used this round
  "sustained engagement",
  "pathways for benefit",
  "free from conflict",
  "region-scale links"
)

## the criteria with numbers and by-hand line breaks
## commented numbers show order in data table
criteria.breaks<-c(
  "complex plan-\nning needs (1)",#1
  "sensitivity to \nhazards (2)",#2
  "landform \nfeedbacks (3)",#3
  "market \nfeedbacks (4)",#4
  "tractable \nquantities (5)",#7
  "tractable sys-\ntem model (6)",#8
  "access to \nhouseholds (7)",#5
  "JEDI in \nanalysis (8)",#6
  "absence of \nplanning (9)",#9
#  "socially vulnerable (10)", #not used this round
  "sustained en-\ngagement (10)",#10
  "pathways for \nbenefit (11)",#11
  "free from \nconflict (12)",#12
  "region-scale \nlinks (13)"#13
)


##################################################
########## DEFINE FUNCTIONS TO SIMPLIFY PLOTTING 
########## FOR PLOTTING SCORES BY CRITERION 

## call this function to plot raw scores from one evaluator on one criterion
## input x is the row number of the evaluator
plot.person<-function(x){
  plot(scores[x,2:27], col=individual, pch=3, ylim=c(.7,5.3), axes=FALSE, cex=1.1, lwd=1.5, bty="n", ylab="")
  if (any(is.na(scores[x,]))){
    abline(v=which(is.na(scores[x,2:27])), col=individual, lwd=1)
  }
  axis(side=2, at=c(1,3,5), labels=c("u", "m", "p"), las=1)
  axis(side=1, at=1:26, labels=1:26, las=1, tick=FALSE, line=-1.3, cex.axis=.5)
}

## call this function to add (to existing plot) scores from additional evaluator on same criterion
## input x is the row number of the evaluator
points.person<-function(x){
  points(scores[x,2:27], col=individual, pch= 3, cex=1.1, lwd=1.5)
  if (any(is.na(scores[x,])))
    abline(v=which(is.na(scores[x,2:27])), col=individual, lwd=1)
}

## call this function to add (to existing plot) the average of several evaluators on one criterion
## input x is the criterion number
points.mean<-function(x){ 
  if (sum(scores[,1]==x)==1){
    points(scores[scores[,1]==x,2:27], pch=1, lwd=.9, cex=1.1)
  }
  else
    points(colMeans(scores[scores[,1]==x,2:27], na.rm=TRUE), pch=1, lwd=.9, cex=1.1)
}

## call this function to add a single criterion label to the left of a plot 
## input x is the criterion number
crit.label<-function(x){
  mtext(criteria.breaks[x], side=2, cex=.6, line=2.6)
}

## call this function to add all candidate names above or below a plot
## input x is the side to plot on (3=above, 1=below)
cand.label<-function(x){ 
  axis(side=x, at=1:26, labels=candidates, las=2, tick=FALSE, line=.1, cex.axis=.9)
}

## call this function to add the number of evaluators to the right of the plot
## input x is the criterion number
annotations.num.eval<-function(x){
  if (sum(scores[,1]==x)==1){ #if one evaluator scored that criterion ...
    points(27,4.5, xpd=TRUE, pch="1", cex=1,col=individual_b) #write a "1"
    }
  if (sum(scores[,1]==x)==2){ #if two evaluators scored that criterion ...
    points(27,4.5, xpd=TRUE, pch="2", cex=1,col=individual_b) #write a "2"
  }
  if (sum(scores[,1]==x)==3){ #if three evaluators scored that criterion ...
    points(27,4.5, xpd=TRUE, pch="3", cex=1,col=individual_b) #write a "3"
  }
 }

## call this function to add, to the right of the plot, asterisks representing evaluator comments
## input x is the criterion number
annotations.comments<-function(x){
  if (sum(scores[scores[,1]==x,28])!=0){  #if the number of comments on that criterion is non-zero ...
    points(27,3, xpd=TRUE, pch=8, cex=.8, lwd=.5) #add an asterisk
  }
  if (sum(scores[scores[,1]==x,28])>=2){  #if the number of comments on that criterion is two or more ...
    points(27,2, xpd=TRUE, pch=8, cex=.8, lwd=.5) #add another asterisk below the first one
  }
  if (sum(scores[scores[,1]==x,28])==3){  #if the number of comments on that criterion is three ...
    points(27,1, xpd=TRUE, pch=8, cex=.8, lwd=.5) #add another asterisk below the second one
  }
}

## call this function to print the legend for criterion-by-criterion scores
## the function has no input
## there are multiple components to this legend, each placed with a separate "legend" command for more control
legend.by.crit<-function(){
  legend("topleft", inset = c(-.26, -3.5),legend = c("individual","average"),pch = c(3,1),col = c(individual, "black"), pt.cex= 1.1,cex=.8,bty="n",xpd=NA)
  legend("topleft", inset = c(-.26, -2.8),legend = c("unable to assess"),pch = c("|"),col = c(individual), pt.cex= 1.1,cex=.8,bty="n",xpd=NA)
  legend("topleft", inset = c(-.26, -2),legend = c("number of evaluators"),pch = c("1"),col = c(individual_b), pt.cex= 1,cex=.8,bty="n",xpd=NA)
  legend("topleft", inset = c(-.26, -1.6),legend = c("evaluator comment"),pch = c(8),pt.cex= 1,cex=.8,bty="n",xpd=NA)
  }

##################################################
########## PLOT SCORES BY CRITERION

individual<-rgb(255, 100, 100, max = 255, alpha = 90) #color for individual scores and "unable to assess" lines
individual_b<-rgb(255, 100, 100, max = 255, alpha = 110)  #slightly darker color for number of evaluators annotation

pdf("./../outputs/FigureS3.pdf", height=8.5, width=5.5)
par(mfrow=c(13,1), mar = c(.3, 9, .3, 1) + 0.1, oma=c(12,0,12,1.5), las=1, xpd=FALSE)

for (i in 1:13){  #go criterion by criterion ...
  plot.person(match(i,scores[,1]))  #plot scores from first evaluator on that criterion
  if (length(which(scores[,1]==i))>=2){ #if more than one evaluator scored that same criterion ...
    for (j in 2:length(which(scores[,1]==i))){
      points.person(which(scores[,1]==i)[j])  #add scores from the other evaluators on same criterion
    }
  }  
  points.mean(i)  #plot averages among evaluators for that criterion
  crit.label(i) #add criterion name to the left of the panel
  annotations.num.eval(i) #add info to the right of the plot: number of evaluators
  annotations.comments(i) #add info to the right of the plot: evaluator comment(s)
  if (i==1){  #if it's the first panel in the multiplot ...
    axis(side=3, at=1:26, labels=1:26, las=1, tick=FALSE, line=-.9, cex.axis=.5) #add axis above ...
    cand.label(3) #add candidate labels to that axis
    legend.by.crit() #and make the legend in the upper left corner
  }
} 
#end plot
dev.off() #print plot to file

##################################################
########## DEFINE MORE FUNCTIONS TO SIMPLIFY PLOTTING
########## FOR PLOTTING BY CANDIDATE

## call this function to plot raw scores from all evaluators on a single candidate
## the function plots vertical bars indicating "unable to assess"
## input x is the candidate number
plot.score.cand<-function(x){
  plot(scores[,1], scores[,x], col=individual, pch=3, ylim=c(.7,5.3), xlim=c(1,13), axes=FALSE, cex=1.1, lwd=1.5, bty="n", ylab="")
  abline(v=c(scores[is.na(scores[,x]),1]), col=individual, lwd=1)
  axis(side=2, at=c(1,3,5), labels=c("u", "m", "p"), las=1, line=.25)
  axis(side=1, at=1:13, labels=c(1:13), las=1, tick=FALSE, line=-1.3, cex.axis=.5)
}

## call this function to add averages across evaluators for all criteria on one candidate
## input x is the candidate number
points.mean.cand<-function(x){
means<-vector(length=13)
  for (i in c(1:13)){
    if (sum(scores[,1]==i)==1) 
      means[i]<-scores[scores[,1]==i,x]
    else
      means[i]<-mean(scores[scores[,1]==i,x], na.rm=TRUE)
  }
  points(1:13,means[1:13], pch=1, lwd=.9, cex=1.1)
}

## call this function to add the candidate label to the left of the panel 
## input x is the candidate number
side.label<-function(x){
  mtext(candidates.breaks[x], side=2, cex=.65, line=2.6)
}

## call this function to add candidate names above or below a panel
## input x is the side to put the names on (top=3, bottom=1)
axis.explainer<-function(x){ 
  axis(side=x, at=1:13, labels=criteria, las=2, tick=FALSE, line=.1, cex.axis=.9)
}

## call this function to print the legend for candidate-by-candidate scores INCLUDING THE TIER-BASED RANKING
## the function has no input
## there are multiple components to this legend, each placed with a separate "legend" command for more control
legend.by.cand<-function(){
  legend("topleft", inset = c(-.47, -2),legend = c("individual","average"),pch = c(3,1),col = c(individual,"black"),pt.cex= 1.1,cex=.8,bty="n",xpd=NA)
  legend("topleft", inset = c(-0.47, -1.3),legend = c("unable to assess"),pch = c("|"),col = c(individual),pt.cex= 1.1,cex=.8,bty="n",xpd=NA)
}

##################################################
########## PLOT SCORES BY CANDIDATE

pdf("./../outputs/FigureS4.pdf", height=8, width=7)
par(mfcol=c(13,2), mar = c(.4, 9.5, .3, .5) + 0.1, oma=c(10,0,10.5,1), las=1, xpd=FALSE)

for (i in 1:26){
  plot.score.cand(i+1)
  #unable(i+1)
  points.mean.cand(i+1)
  side.label(i)
  if (i==1 || i==14) {  #if it's a top-row panel in the multiplot ...
    axis(side=3, at=1:13, labels=1:13, las=1, tick=FALSE, line=-.85, cex.axis=.5) #add an extra axis above ...
    axis.explainer(3) #and add criteria names above the panel
  }
#  if (i==13 || i==26) { #if it's a bottom-row panel in the multiplot ...
#    axis.explainer(1) #add criteria names below the panel
#  }
  if (i==1){  #if it's the very first panel in the multiplot ...
  legend.by.cand()  #add the legend
  }
}
#end plot
dev.off() #print plot to file

##################################################
########## AGGREGATE SCORES ACROSS CRITERIA 

## aggregate via equal-weight mean
## first average across evaluators
all.means<-matrix(nrow=13, ncol=26) #13 criteria x 26 candidates
for (j in 1:26){  # for each candidate
  for (i in 1:13){   # for each criterion
    all.means[i,j]<-mean(scores[scores[,1]==i,j+1], na.rm=TRUE)
  }
}
## now average across criteria
equal.weight<-colMeans(all.means,na.rm=TRUE)

## aggregate via satisficing (specifically, count the number of criteria below middling)
## put the aggregate scores in a vector
satisficing<-vector(length=26)
for (i in 1:26){
  satisficing[i]<-sum(all.means[,i]>=3,na.rm=TRUE)
}

##################################################
########## PLOT AGGREGATE MEASURES 

breaks.ew<-quantile(equal.weight, probs=seq(0,1,.25))
breaks.sat<-quantile(satisficing, probs=seq(0,1,.25))
pal<-rev(brewer.pal(7,name="Greys")[1:4])
pals<-brewer.pal(5,name="Blues")

## make indexing object to order candidates from best to worst scores
rank<-rev(order(satisficing,equal.weight))

## make indexing object for color-coding of points by region/municipality
locations<-c(rep(1,times=5),rep(3,times=2),rep(2,times=11),rep(4,times=8))
#brewer.pal(4,name="Dark2")

pdf("./../outputs/Figure3.pdf", height=3.9, width=7)
par(mfcol=c(2,1), mar = c(.4, 3.3, .2, .2), oma=c(6,0,2,0), las=1, xpd=FALSE)

plot(equal.weight, #just to get the plot size before adding backgound quartile lines
     pch=16, lwd=1.1, cex=1.6,xlab="", ylab="", yaxt="n", xaxt="n", cex.axis=.75, col="white", ylim=c(2.2,4.8))
abline(h=c(breaks.ew[c(2,3,4)]), col="grey", lwd=.5)
points(colMeans(all.means,na.rm=TRUE)[rank],pch=16,lwd=1,cex=1.6,col=brewer.pal(4,name="Dark2")[locations[rank]])

axis(side=2, at=c(2.5,3,3.5,4,4.5), labels=FALSE, las=1, tick=TRUE, line=0, cex.axis=.7)
axis(side=2, at=c(2.5,3,3.5,4,4.5), labels=c(2.5,"3.0",3.5,"4.0",4.5), las=1, tick=FALSE, line=-.3, cex.axis=.7)
#axis(side=3, at=1:26, labels=rep("", times=26), las=1, tick=TRUE, line=0, cex.axis=.6)
axis(side=3, at=1:26, labels=c(1:26)[rank], las=1, tick=FALSE, line=-.85, cex.axis=.7)#cex.axis=.58
title(xlab="decision problems", line=-7.2, cex.lab=.75, xpd=NA)
#axis(side=3, at=1:26, labels=candidates.no[rank], las=2, tick=FALSE, line=0, cex.axis=.75)#line=.5

text((x=1:26)+.02, y=-1.6, labels=candidates.no[rank], srt=48, adj=1, xpd=NA, cex=.75)#-.91

title(ylab="average score \nacross criteria", line=1.8, cex.lab=.75)

legend("topright", inset=c(0,-.1), legend=c("R1","R3", "m1", "m4"), pch=19, col=brewer.pal(4,name="Dark2"), xpd=NA, box.lwd=0, bty="n", cex=.75, pt.cex=1.5, title="", ncol=2) #"topleft", inset = c(-.1, -.95), title="anonymized \nlocation codes"

text(23.33,4.08,labels="anonymized \nlocation codes", cex=.75, adj=1, pos=2)

plot(satisficing, pch=16, cex=1.6, xlab="", ylab="", xaxt="n", yaxt="n", col="white", ylim=c(3.5,13.55))#just to get the plot size right before drawing over it

abline(h=c(breaks.sat[c(2,3,4)]), col="grey", lwd=.5)
points(satisficing[rank], pch=16, cex=1.6, xlab="", ylab="", xaxt="n", yaxt="n", col=brewer.pal(4,name="Dark2")[locations[rank]])

axis(side=1, at=1:26, labels=c(1:26)[rank], las=1, tick=FALSE, line=-1.2, cex.axis=.7)

axis(2, at=c(1,3,5,7,9,11,13), labels=c(12,10,8,6,4,2,0), cex.axis=.75)
title(ylab="number of criteria \nbelow 'middling'", line=1.8, cex.lab=.75)

#end plot
dev.off() #print plot to file


##################################################
########## CALCULATE CORRELATIONS AMONG CRITERIA
#correlations<-matrix(nrow=13,ncol=13)
#for(i in 1:12){
#  for(j in (i+1):13){
#    correlations[i,j]<-round(cor(pairs.all[,i],pairs.all[,j],use="pairwise.complete.obs"),digits=2)
#  }
#}

##################################################
########## PAIRS PLOT OF ALL CRITERIA

## abbreviate criteria names further for the pairs plots
criteria.mini<-c(
  "complex \nplanning \nneeds \n(1)",
  "sensitivity \nto hazards\n(2)",
  "landform \nfeedbacks \n(3)",
  "market \nfeedbacks \n(4)",
  "tractable \nquantities \n(5)",
  "tractable \nsystem \nmodel \n(6)",
  "household \naccess \n(7)",
  "JEDI in \n analysis\n(8)",
  "planning \nabsence \n(9)",
  "sustained \nengage-\nment \n(10)",
  "pathways \nto benefits \n(11)",
  "free from \nconflict \n(12)",
  "regional-\nscale \nlinks \n(13)"
)

## sort rows to manipulate plotting order: put worse candidates behind better
ordered.means<-cbind(all.means[,order(equal.weight)])

## make indexing object for shape-coding by aggregate score
## see which candidates were in the top quartile in on either aggregate metric
best<-(satisficing>=breaks.sat[4]) >=1
best.pch<-rep(15,times=26) #make indexing object
best.pch[best==TRUE]<-c(3,1,2,1,1,2,3) #top quartile in satisficing get their own shape (partly by-hand hack)
best.pch<-best.pch[order(equal.weight)] #sequence pch-s in the order we will plot points 

## make indexing object for color-coding of points by region/municipality
sites<-c(brewer.pal(4,name="Dark2")[c(rep(1,times=5),rep(3,times=2),rep(2,times=11),rep(4,times=8))])
sites<-sites[order(equal.weight)]

## make pairs plot
pdf("./../outputs/FigureS5.pdf", height=8, width=8)
par(oma=c(0,0,0,0), mar=c(0,0,0,0))
pairs(
  t( #transpose data into correct orientation for R's "pairs" function
    cbind(rep(5, times=(13)),ordered.means[1:13,]) # but first add ideal points
  ), 
  lower.panel=NULL,
  xlim=c(.95,5.05), ylim=c(.95,5.05), 
  yaxt="na", xaxt="na",
  labels=criteria.mini,
  cex.labels=.83,
  col=c("black",sites), #add the color for the ideal points
  pch=c(15,best.pch), #add a shape for the ideal points
  cex=c(1.3,rep(1,times=26)), #ideal point may need a different size
  lwd=1.2,
  gap=0.4,
  xpd=FALSE)

legend("bottomleft", inset = c(.1, .1),                  
       legend = c(
         "ideal point",
         "flood hazard (R1)", 
         "master planning (R1)", 
         "combined sewer (R1)", 
         "harden infrastructure (R1)", 
         "port terminals (R1)", 
         "road elevation (m4)", 
         "wastewater treatment (m4)", 
         "beach nourishment (m4)",
         "other (m4)",
         "harden housing (m1)", 
         "other (m1)", 
         "other (R3)"
         ),
       pch=c(15,1,2,3,4,5,1,2,3,15,1,15,15),
       col=c("black",brewer.pal(4,name="Dark2")[c(rep(1,times=5),rep(4,times=4),rep(3,times=2),rep(2,times=1))]),
       pt.cex= 1.3,
       pt.lwd=1.5,
       bty="n",
       xpd=NA
       )
#end figure
dev.off() #print plot to file  


##################################################
########## PAIRS PLOT USING ALL 13 CRITERIA (SIMPLIFIED, DOES NOT APPEAR IN PAPER OR SI)

## prepare data for the "pairs" function
pairs.all<-t(all.means)  #transpose the matrix of average scores

## make pairs plot for all 13 criteria
pdf("./../outputs/FigureS5*.pdf", height=9, width=9)

pairs(rbind(rep(5, times=13),pairs.all),# add a perfect score to each column for the ideal points 
      lower.panel=NULL,
      xlim=c(.9,5.1), ylim=c(.9,5.1), 
      yaxt="na", xaxt="na",
      gap=.65,
      labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13),
      col=c("pink",rep("gray25",times=26)),
      pch=c(15,rep(16, times=26)),
      cex=c(1.2,rep(1.1, times=26)),
      cex.labels=1.3,
      lwd=.8,
      xpd=FALSE)

legend("bottomleft", inset = c(.086,.45),legend =c("ideal point", "candidate decision problem"),pch=c(15,16),col=c("pink","gray25"),pt.cex= 1.3,cex=.9,box.lwd=0,xpd=NA)

legend("bottomleft", inset = c(.06, .1),
       legend =c(1,2,3,4,5,6,7,8,9,10,11,12,13),
       #col=c("yellow2","gray25"),
       pt.cex= 1,cex=.9,bty="n",xpd=NA)

legend("bottomleft", inset = c(.09, .1),
       legend =criteria,
       #col=c("yellow2","gray25"),
       pt.cex= 1,cex=.9,bty="n",xpd=NA)

#end figure
dev.off() #print plot to file  


##################################################
########## SMALL SUBSET OF THE PAIRS PLOT

pdf("./../outputs/Figure4.pdf", height=2.1, width=7)
par(mfcol=c(1,3), mar = c(4.15,1.7,0.15,0.1), oma=c(.5,13.6,0,0))

## add ideal points
mwi<-cbind(ordered.means, rep(5,times=13))#mwi:means-with-ideal

plot(mwi[5,],mwi[6,], ylim=c(1,5), xlim=c(1,5), xlab="",col=c(sites,"black"), pch=c(best.pch,15),cex=2, lwd=1.6, las=1)
abline(lm(mwi[6,]~mwi[5,]), col='#80808060', lwd=1.8)
#points(mwi[5,],mwi[6,], ylim=c(1,5), xlim=c(1,5), xlab="",col=c(sites,"black"), pch=c(best.pch,15),cex=1.7, lwd=1.6, las=1)
  title(ylab="tractable system model", line=2.2, cex.lab=1, xpd=NA)
  title(xlab="tractable quantities", line=2.2, cex.lab=1)

  legend("bottomleft", inset = c(-1.36, -.3),                  
         legend = c(
           "ideal point",
           "flood hazard (R1)", 
           "master planning (R1)", 
           "combined sewer (R1)", 
           "other (R1)", 
           "road elevation (m4)", 
           "wastewater treatment (m4)", 
           "beach nourishment (m4)",
           "other (m4)",
           "harden housing (m1)", 
           "other (m1)", 
           "other (R3)"
         ),
         y.intersp=1.18,
         x.intersp=1.18,
         pch=c(15,1,2,3,15,1,2,3,15,1,15,15),
         col=c("black",brewer.pal(4,name="Dark2")[c(rep(1,times=4),rep(4,times=4),rep(3,times=2),rep(2,times=1))]),
         pt.cex= 2,
         pt.lwd=1.6,
         #cex=1,
         bty="n",
         xpd=NA
  )
  
plot(mwi[4,],mwi[6,], ylim=c(1,5), xlim=c(1,5), xlab="", col=c(sites,"black"), pch=c(best.pch,15),cex=2,lwd=1.6,yaxt="n", ylab="")
abline(lm(mwi[6,]~mwi[4,]), col='#80808060', lwd=1.8)
#points(mwi[10,],mwi[6,], ylim=c(1,5), xlim=c(1,5), xlab="", col=c(sites,"black"), pch=c(best.pch,15),cex=1.7,lwd=1.6,yaxt="n", ylab="")
  axis(side=2, at=1:5, labels=rep("",times=5), las=1, tick=TRUE)
  title(xlab="market feedbacks", line=2.2, cex.lab=1)
  
plot(mwi[9,],mwi[6,], ylim=c(1,5), xlim=c(1,5), xlab="", col=c(sites,"black"),pch=c(best.pch,15),cex=2,lwd=1.6,yaxt="n", ylab="")
abline(lm(mwi[6,]~mwi[9,]), col='#80808060', lwd=1.8)
#points(mwi[9,],mwi[6,], ylim=c(1,5), xlim=c(1,5), xlab="", col=c(sites,"black"),pch=c(best.pch,15),cex=1.7,lwd=1.6,yaxt="n", ylab="")
  axis(side=2, at=1:5, labels=rep("",times=5), las=1, tick=TRUE)
  title(xlab="absence of planning", line=2.2, cex.lab=1)

#end figure
dev.off() #print plot to file  
  
  
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################

