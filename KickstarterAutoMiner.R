install.packages("tidyverse")
install.packages("RColorBrewer") 
install.packages("tm")
install.packages("wordcloud")
install.packages("car")
install.packages("files")
install.packages("patchwork")
install.packages("hrbrthemes")
install.packages("vcd")
install.packages("gplots")
install.packages("corrplot")
install.packages("randomForest") 
devtools::install_github('skinner927/reprtree')
install.packages("reader") 
install.packages("effects")
install.packages("pscl")

library(rpart)        # Classification Tree
library(rpart.plot)   # Classification Tree plot
require(caTools)
library(pscl)
library(effects)
library(reader)
library(reprtree)
library(randomForest) 
library(corrplot)
library(gplots)
library(vcd)
library(ggplot2)
library(RColorBrewer) 
library(tidyverse)
library(tm)
library(wordcloud)
library(car)
library(files)
library(utils)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(ggpubr)
library(dplyr)
library(tidyr)

# LOAD VARIABLES
options(scipen=999)
# the "target" directory specified in the YAML file is actually the source for the analysis portion
# the real target for this portion should be a charts directory off the TargetDirectory pulled from the YAML file.
TargetDirectory <- "c:/1/output/"
SourceDirectory <- TargetDirectory
#TargetDirectory <- "d:/1/output/charts/"
ChartsDirectory <- paste(TargetDirectory,"charts/",sep="")
ResultsDirectory <- paste(TargetDirectory,"TestResults/",sep="")
SuccessGridsDirectory <- paste(TargetDirectory,"SuccessGrids/", sep = "")
TargetFileName <- "kickstarter_filtered.csv"
TargetAllFileName <- "kickstarter_all.csv"
TargetFileNamestub <- "kickstarter_filtered"
TargetAllFileNamestub <- "kickstarter_all"
PledgedColor = "green"
PercentColor = "blue"
nb.cols <- 999
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)


# LOAD SOUCE FILES
AllProjects <- read.csv(paste(SourceDirectory, TargetAllFileName, sep="" ,collapse=NULL), stringsAsFactors = T)
#FilteredProjects <- read.csv(paste(SourceDirectory,"kickstarter_clean.csv", sep="" ,collapse=NULL))
FilteredProjects <- read.csv(paste(SourceDirectory,TargetFileName, sep="" ,collapse=NULL), stringsAsFactors = T)
ks.proj <- FilteredProjects
FilteredProjects$ProjectDuration <- as.integer( sub('.*-([0-9]+).*','\\1',difftime(FilteredProjects$deadline,FilteredProjects$launched_at, units = "days")))
CategoryGroupsFiltered <- read.csv(paste(SuccessGridsDirectory, paste(TargetFileNamestub,"_by_category_success.csv",sep="",collapse=NULL), sep="" ,collapse=NULL))
SuccessAllProjects <- CategoryGroupsAll <- read.csv(paste(SourceDirectory,paste(TargetAllFileNamestub,"_projectsuccessb_Overall.csv", sep="" ,collapse=NULL), sep="" ,collapse=NULL), stringsAsFactor = T)


# CREATE DESTINATION DIRECTORIES
dir.create(file.path(ChartsDirectory), showWarnings = FALSE)
dir.create(file.path(ResultsDirectory), showWarnings = FALSE)

# create day, month, year variables

# give each project a unique ID and format dates as dates
ks.proj$ID <- as.character(rownames(ks.proj))
ks.proj$name <- as.character(ks.proj$name)
ks.proj$deadline <- as.Date(ks.proj$deadline)
ks.proj$launched <- as.Date(ks.proj$launched_at)

# reformat project success
ks.proj$projectsuccess <- as.character(ks.proj$projectsuccess )
ks.proj$projectsuccess <- as.factor(ks.proj$projectsuccess )

ks.proj <- ks.proj %>% 
  separate(col = "deadline", into = c("deadline_year", "deadline_month", "deadline_day"), sep = "-") %>%
  separate(col = "launched", into = c("launched_year", "launched_month", "launched_day"), sep = "-")

ks.proj$launched_day <- as.numeric(ks.proj$launched_day)
ks.proj$deadline_day <- as.numeric(ks.proj$deadline_day)


summary(ks.proj$projectsuccess )

FilteredProjects <- FilteredProjects %>%
  separate(col = "deadline", into = c("deadline_year", "deadline_month", "deadline_day"), sep = "-") %>%
  separate(col = "launched_at", into = c("launched_year", "launched_month", "launched_day"), sep = "-")

FilteredProjects$launched_day <- as.numeric(FilteredProjects$launched_day)
FilteredProjects$deadline_day <- as.numeric(FilteredProjects$deadline_day)





# ANOVA ON PLEDGED TO DETERMINE NORMALITY and VARIANCE ASSUPMTIONS
categorypledgedanova <- aov(pledged ~ category, data=FilteredProjects)
summary(categorypledgedanova)
png(paste(ChartsDirectory,"CategoryPledgedAvonaResiduals.png", sep="" ,collapse=NULL), width = 1500, height = 700)
plot(categorypledgedanova, 1)
dev.off()
cpacapt<- capture.output(categorypledgedanova)
writeLines(cpacapt, con=file(paste(ResultsDirectory,"CategoryPledgedAnovaResults.txt", sep="" ,collapse=NULL)))
closeAllConnections()

 
 # BARTLETT TEST

bt <- bartlett.test(pledged ~ category, data=FilteredProjects)
btcap <- capture.output(bt)
writeLines(btcap, con=file(paste(ResultsDirectory,"CategoryPledgedFilteredbartlett.txt", sep="" ,collapse=NULL)))
closeAllConnections()

# TUKEY TEST
tukeycap <- capture.output(TukeyHSD(categorypledgedanova))
writeLines(tukeycap, con=file(paste(ResultsDirectory,"CategoryPledgedTukeyResults.txt", sep="" ,collapse=NULL)))
closeAllConnections()

 
# LEVENE TEST  
lt <- leveneTest(pledged ~ category, data = FilteredProjects)
ltcap <- capture.output(lt)
writeLines(ltcap, con=file(paste(ResultsDirectory,"CategoryPledgedFilteredlevene.txt", sep="" ,collapse=NULL)))
closeAllConnections()

 
 # ONEWAY TEST
onewaytestcap <- capture.output(oneway.test(pledged ~ category, data = FilteredProjects))
writeLines(onewaytestcap, con=file(paste(ResultsDirectory,"CategoryPledgedFilteredoneway.txt", sep="" ,collapse=NULL)))
closeAllConnections()

 
 # PAIRWISE T TestResults/
writeLines(capture.output(pairwise.t.test(FilteredProjects$pledged, FilteredProjects$category, p.adjust.method = "BH", pool.sd = FALSE)),   con=file(paste(ResultsDirectory,"CategoryPledgedFilteredpairwiseT.txt", sep="" ,collapse=NULL))  )
closeAllConnections()

 # SHAPIRO TEST FOR NORMALITY
aov_residuals <- residuals(object = categorypledgedanova )
stcap <- capture.output(shapiro.test(x = aov_residuals[0:5000] ))
writeLines(stcap, con=file(paste(ResultsDirectory,"CategoryPledgedFilteredshapiro.txt", sep="" ,collapse=NULL)))
closeAllConnections()
FilteredProjects5k <- FilteredProjects[0:4999,]
FilteredProjects5k <- subset(FilteredProjects5k, backers_count > 1)
FilteredProjects5k$logbackers <- log(FilteredProjects5k$backers_count)
shapiro.test(FilteredProjects5k$goal)
shapiro.test(FilteredProjects5k$pledged)
shapiro.test(FilteredProjects5k$backers_count)
FilteredProjects5k$logbackers <- log1p(FilteredProjects$backers_count)
shapiro.test(FilteredProjects5k$logbackers)



# CHARTS FOR CATEGORY
CategoryGroupsAll <- read.csv(paste(SuccessGridsDirectory,paste(TargetAllFileNamestub,"_by_category_success.csv", sep="" ,collapse=NULL), sep="" ,collapse=NULL), stringsAsFactor = T)
CategoryGroupsFiltered <- read.csv(paste(SuccessGridsDirectory,paste(TargetFileNamestub,"_by_category_success.csv", sep="" ,collapse=NULL), sep="" ,collapse=NULL), stringsAsFactor = T)


levels <- levels(as.factor(FilteredProjects$category))
FilteredProjects$category <- factor(FilteredProjects$category, levels=sort(levels))
png(paste(ChartsDirectory,"BarChartStackedFilteredSuccessByCategory.png", sep="" ,collapse=NULL), width = 700, height = 500)
ggplot(FilteredProjects, aes(x = category, fill = projectsuccess)) +
  geom_bar() +
  coord_flip() +
  theme(legend.position = "bottom") +
  ylab("Number of projects") + xlab("") +
  ggtitle("Project Success by Category - Filtered")  + 
 ylab("Success %") + theme(plot.title = element_text(hjust = 0.5)) 
 dev.off()

png(paste(ChartsDirectory,"BarChartStackedAllSuccessByCategory.png", sep="" ,collapse=NULL), width = 700, height = 500)
ggplot(AllProjects, aes(x = category, fill = projectsuccess)) +
  geom_bar() +
  coord_flip() +
  theme(legend.position = "bottom") +
  ylab("Number of projects") + xlab("") +
  ggtitle("Project Success by Category - All")  + 
 ylab("Success %") + theme(plot.title = element_text(hjust = 0.5)) 
 dev.off()

png(paste(ChartsDirectory,"BarChartFilteredSuccessByCategory.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(data=CategoryGroupsFiltered, aes(x=category, y=PercentSuccess, fill=category,colour=category)) +   geom_bar(stat="identity") + scale_color_manual(values = mycolors) + ggtitle("Success% by Category - Filtered")+ xlab("Category") + ylab("Success %") + theme(plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept = mean(CategoryGroupsFiltered$PercentSuccess), color="blue")
dev.off()

png(paste(ChartsDirectory,"BarChartFilteredActualPledgedByCategory.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(data=CategoryGroupsFiltered, aes(x=category, y=MeanActualPledged, fill=category,colour=category)) +   geom_bar(stat="identity") + scale_color_manual(values = mycolors) + ggtitle("Actual Pledged Amount (Mean) by Category - Filtered") + xlab("Category") + ylab("Mean Pledged Amount") + theme(plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept = mean(CategoryGroupsFiltered$MeanActualPledged), color="blue")
dev.off()

png(paste(ChartsDirectory,"BarChartFilteredMeanPledgedByCategory.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(data=CategoryGroupsFiltered, aes(x=category, y=MeanPledged, fill=category,colour=category)) +  
 geom_bar(stat="identity") + scale_color_manual(values = mycolors) + 
 ggtitle("Pledged Amount (Mean) by Category - Filtered") + xlab("Category") + ylab("Mean Pledged Amount") + 
 theme(plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept = mean(FilteredProjects$pledged), color="blue")+
  geom_text(aes(0,mean(FilteredProjects$pledged), label = paste('Mean Pledge: $',round(mean(FilteredProjects$pledged),digits=0),sep = "", collapse = NULL), hjust = -0.5, vjust = -1),color = "blue")
  dev.off()


png(paste(ChartsDirectory,"BarChartFilteredMedianPledgedByCategory.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(data=CategoryGroupsFiltered, aes(x=category, y=MedianPledged, fill=category,colour=category)) +  
geom_bar(stat="identity") + scale_color_manual(values = mycolors) + 
ggtitle("Pledged Amount (Median) by Category - Filtered") + xlab("Category") + ylab("Median Pledged Amount") +
 theme(plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept = mean(CategoryGroupsFiltered$MedianPledged), color="blue") + 
 geom_text(aes(0,median(FilteredProjects$pledged), label = paste('Median Pledge: $',round(median(FilteredProjects$pledged),digits=0),sep = "", collapse = NULL), hjust = -0.5, vjust = 0.5),color = "blue")
 dev.off()


png(paste(ChartsDirectory,"BarChartAllSuccessByCategory.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(data=CategoryGroupsAll, aes(x=category, y=PercentSuccess, fill=category,colour=category)) +   geom_bar(stat="identity") + scale_color_manual(values = mycolors) + ggtitle("Success% by Category - All")+ xlab("Category") + ylab("Success %") + theme(plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept = mean(CategoryGroupsAll$PercentSuccess), color="blue")
dev.off()

png(paste(ChartsDirectory,"BarChartAllActualPledgedByCategory.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(data=CategoryGroupsAll, aes(x=category, y=MeanActualPledged, fill=category,colour=category)) +   geom_bar(stat="identity") + scale_color_manual(values = mycolors) + ggtitle("Actual Pledged Amount (Mean) by Category - All")+ xlab("Category") + ylab("Mean Pledged Amount") + theme(plot.title = element_text(hjust = 0.5))  + geom_hline(yintercept = mean(CategoryGroupsAll$MeanActualPledged), color="blue")
dev.off()


dt <- as.table(as.matrix(CategoryGroupsAll))
dtchi <- dt[ , c("SuccessCount","FailCount")]
colnames(dtchi)[colnames(dtchi) == "SuccessCount"] = "Success"
colnames(dtchi)[colnames(dtchi) == "FailCount"] = "Failed"
png(paste(ChartsDirectory,"BalloonChartAllSuccessByCategory.png", sep="" ,collapse=NULL), width = 800, height = 900)
balloonplot(t(dtchi), main ="Category Success - All",  xlab ="", ylab="",label = FALSE, show.margins = FALSE,cum.margins=FALSE)
dev.off()

dt <- as.table(as.matrix(CategoryGroupsFiltered))
dtchi <- dt[ , c("SuccessCount","FailCount")]
colnames(dtchi)[colnames(dtchi) == "SuccessCount"] = "Success"
colnames(dtchi)[colnames(dtchi) == "FailCount"] = "Failed"
png(paste(ChartsDirectory,"BalloonChartFilteredSuccessByCategory.png", sep="" ,collapse=NULL), width = 800, height = 900)
balloonplot(t(dtchi), main ="Category Success - Filtered",  xlab ="", ylab="",label = FALSE, show.margins = FALSE,cum.margins=FALSE)
dev.off()

# Charts for project state (SUCCESS / FAILED)

png(paste(ChartsDirectory,"BarChartFilteredByState.png", sep="" ,collapse=NULL), width = 700, height = 500)
ggplot(FilteredProjects, aes(state, fill = state)) +
  geom_bar() +
  ylab("# of Projects") + xlab("State") +
  theme(legend.position = "bottom") +
  ggtitle("Project State - Filtered")+
  theme(plot.title = element_text(hjust = 0.5))
  dev.off()
  
  png(paste(ChartsDirectory,"BarChartAllByState.png", sep="" ,collapse=NULL), width = 700, height = 500)
ggplot(AllProjects, aes(state, fill = state)) +
  geom_bar() +
  ylab("# of Projects") + xlab("State") +
  theme(legend.position = "bottom") +
  ggtitle("Project State - Complete")+
  theme(plot.title = element_text(hjust = 0.5))
  dev.off()
  

png(paste(ChartsDirectory,"BarChartAllByState.png", sep="" ,collapse=NULL), width = 700, height = 500)
ggplot(AllProjects, aes(state, fill = state)) +
  geom_bar() +
  ylab("# of Projects") + xlab("State") +
  theme(legend.position = "bottom") +
  ggtitle("Project State - All")+
  theme(plot.title = element_text(hjust = 0.5))
  dev.off()




# CHI SQUARE TESTS FOR CATEGORY
chires <- chisq.test(table(FilteredProjects$category, FilteredProjects$projectsuccess))
chisqcap <- capture.output(chires)
writeLines(chisqcap, con=file(paste(ResultsDirectory,"CategorySuccessFilteredChiSquare.txt", sep="" ,collapse=NULL)))
closeAllConnections()

chires <- chisq.test(table(AllProjects$category, AllProjects$projectsuccess))
chisqcap <- capture.output(chires)
writeLines(chisqcap, con=file(paste(ResultsDirectory,"CategorySuccessAllChiSquare.txt", sep="" ,collapse=NULL)))
closeAllConnections()

dta <- read.csv(paste(SourceDirectory, TargetFileName, sep="" ,collapse=NULL),stringsAsFactors=T)
dta$projectsuccess <- relevel(dta$projectsuccess, ref="Success")
# logistic regression 
by.category <- glm(projectsuccess ~ category, dta, family="binomial")
write.csv(summary(by.category)['coefficients'],file=paste(ResultsDirectory,"CategorySuccessFilteredLogisticRegression.csv", sep="" ,collapse=NULL))
closeAllConnections()

#SUBCATEGORY
chires <- chisq.test(table(FilteredProjects$subcategory, FilteredProjects$projectsuccess))
chisqcap <- capture.output(chires)
writeLines(chisqcap, con=file(paste(ResultsDirectory,"SubCategorySuccessFilteredChiSquare.txt", sep="" ,collapse=NULL)))
closeAllConnections()

FilteredProjectsSub <- FilteredProjects
FilteredProjectsSub$SubMatch <-  ifelse(as.character(FilteredProjectsSub$category)==as.character(FilteredProjectsSub$subcategory), "Matched", "NoMatch") 
submatchchitest <- capture.output(chisq.test(table(FilteredProjectsSub$SubMatch, FilteredProjectsSub$projectsuccess)))
writeLines(submatchchitest, con=file(paste(ResultsDirectory,"SubMatchFilteredChiSquare.txt", sep="" ,collapse=NULL)))
ConTableSubMatch <- capture.output(table(FilteredProjectsSub$SubMatch, FilteredProjectsSub$projectsuccess))
writeLines(ConTableSubMatch, con=file(paste(ResultsDirectory,"SubMatchContingencyTable.txt", sep="" ,collapse=NULL)))




# CHARTS FOR STATE (state_loc)

StateGroupsFiltered <- read.csv(paste(SuccessGridsDirectory,paste(TargetFileNamestub,"_country_equals_US_by_state_loc_success.csv", sep="" ,collapse=NULL), sep="" ,collapse=NULL))
png(paste(ChartsDirectory,"BarChartFilteredMeanPledgedByState.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(data=StateGroupsFiltered, aes(x=state_loc, y=MeanPledged, fill=state_loc,colour=state_loc)) +   
geom_bar(stat="identity") + scale_color_manual(values = mycolors) + 
ggtitle("Pledged Amount (Mean) by State - Filtered") + 
xlab("State") + ylab("Mean Pledged Amount") + 
theme(plot.title = element_text(hjust = 0.5))+ 
geom_hline(yintercept = mean(StateGroupsFiltered$MeanPledged), color="blue")+
geom_text(aes(0,mean(StateGroupsFiltered$MeanPledged), label = paste('$',round(mean(StateGroupsFiltered$MeanPledged),digits=0),sep = "", collapse = NULL), hjust = -0.5, vjust = -1),color = "blue")
dev.off()



png(paste(ChartsDirectory,"BarChartFilteredUSOnlySuccessByState.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(data=StateGroupsFiltered, aes(x=state_loc, y=PercentSuccess, fill=state_loc,colour=state_loc)) +   
geom_bar(stat="identity") + scale_color_manual(values = mycolors) +
geom_text(aes(label=TotalCount),vjust=-1,show.legend = FALSE) + 
ggtitle("Success By State (US Only) - Filtered")+ 
xlab("State")+ ylab("Success %") + theme(plot.title = element_text(hjust = 0.5))  + 
geom_hline(yintercept = mean(StateGroupsFiltered$PercentSuccess),color="blue") + 
geom_text(aes(0,mean(StateGroupsFiltered$PercentSuccess), label = paste(round(mean(StateGroupsFiltered$PercentSuccess),digits=0),"%",sep = "", collapse = NULL), hjust = -0.5, vjust = -1),color = "blue") 
dev.off()

StateGroupsFilteredSorted <- StateGroupsFiltered[order(StateGroupsFiltered$PercentSuccess),]
attach(StateGroupsFilteredSorted)
png(paste(ChartsDirectory,"BarChartFilteredUSOnlySuccessByStateSorted.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(data=StateGroupsFilteredSorted, aes(x = reorder(state_loc ,PercentSuccess ), y=PercentSuccess, fill=state_loc,colour=state_loc)) +   geom_bar(stat="identity") + scale_color_manual(values = mycolors) + geom_text(aes(label=TotalCount),vjust=-1,show.legend = FALSE) + ggtitle("Success By State (US Only) - Filtered, Sorted")+ xlab("State")+ ylab("Success %") + theme(plot.title = element_text(hjust = 0.5))  + 
geom_hline(yintercept = mean(PercentSuccess),color="blue") + geom_text(aes(0,mean(PercentSuccess), label = paste("Mean:",round(mean(PercentSuccess),digits=0),"%",sep = "", collapse = NULL), hjust = -0.5, vjust = -1),color = "red") 
dev.off()
detach(StateGroupsFilteredSorted)

StateGroupsAll <- read.csv(paste(SuccessGridsDirectory,paste(TargetFileNamestub,"_country_equals_US_by_state_loc_success.csv", sep="" ,collapse=NULL), sep="" ,collapse=NULL))
png(paste(ChartsDirectory,"BarChartAllUSOnlySuccessByState.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(data=StateGroupsAll, aes(x=state_loc, y=PercentSuccess, fill=state_loc,colour=state_loc)) +   geom_bar(stat="identity") + scale_color_manual(values = mycolors) + geom_text(aes(label=TotalCount),vjust=-1,show.legend = FALSE) + ggtitle("Success By State (US Only) - All")+ xlab("State")+ ylab("Success %") + theme(plot.title = element_text(hjust = 0.5))  + geom_hline(yintercept = mean(StateGroupsAll$PercentSuccess),color="blue") + geom_text(aes(0,mean(StateGroupsAll$PercentSuccess), label = paste(round(mean(StateGroupsAll$PercentSuccess),digits=0),"%",sep = "", collapse = NULL), hjust = -0.5, vjust = -1),color = "red") 
dev.off()
attach(StateGroupsAll)
png(paste(ChartsDirectory,"BarChartAllUSOnlySuccessByStateSorted.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(data=StateGroupsAll, aes(x = reorder(state_loc ,PercentSuccess ), y=PercentSuccess, fill=state_loc,colour=state_loc)) +   geom_bar(stat="identity") + scale_color_manual(values = mycolors) + geom_text(aes(label=TotalCount),vjust=-1,show.legend = FALSE) + ggtitle("Success By State (US Only) - All, Sorted")+ xlab("State")+ ylab("Success %") + theme(plot.title = element_text(hjust = 0.5))  + geom_hline(yintercept = mean(PercentSuccess),color="blue") + geom_text(aes(0,mean(PercentSuccess), label = paste("Mean:",round(mean(PercentSuccess),digits=0),"%",sep = "", collapse = NULL), hjust = -0.5, vjust = -1),color = "red") 
dev.off()
detach(StateGroupsAll)

USOnlyAll <- read.csv(paste(SourceDirectory,paste('Subsets/', TargetAllFileNamestub, "_country_equals_US.csv", sep="" ,collapse=NULL), sep="" ,collapse=NULL),stringsAsFactors=T)
# set the reference level for logistic regression
USOnlyAll$projectsuccess <- relevel(USOnlyAll$projectsuccess, ref="Success")
ccap <- capture.output(chisq.test(table(USOnlyAll$state_loc, USOnlyAll$projectsuccess)))
writeLines(ccap, con=file(paste(ResultsDirectory,"StateSuccessAllUSChiSquare.txt", sep="" ,collapse=NULL)))
closeAllConnections()

by.state <- glm(projectsuccess ~ state_loc, USOnlyAll, family="binomial")
write.csv(summary(by.state)['coefficients'],file=paste(ResultsDirectory,"StateSuccessAllUSLogisticRegression.csv", sep="" ,collapse=NULL))
closeAllConnections()

USOnlyFiltered <- read.csv(paste(SourceDirectory,paste('Subsets/', TargetFileNamestub, "_country_equals_US.csv", sep="" ,collapse=NULL), sep="" ,collapse=NULL),stringsAsFactors=T)
# set the reference level for logistic regression
USOnlyFiltered$projectsuccess <- relevel(USOnlyFiltered$projectsuccess, ref="Success")
ccap <- chisq.test(table(USOnlyFiltered$state_loc, USOnlyFiltered$projectsuccess))
writeLines(ccap, con=file(paste(ResultsDirectory,"StateSuccessFilteredUSChiSquare.txt", sep="" ,collapse=NULL)))
closeAllConnections()

by.state.filt <- glm(projectsuccess ~ state_loc, USOnlyFiltered, family="binomial")
write.csv(summary(by.state.filt)['coefficients'],file=paste(ResultsDirectory,"StateSuccessFilteredUSLogisticRegression.csv", sep="" ,collapse=NULL))
closeAllConnections()





# CHARTS FOR LANGUAGES

LanguageGroupsFiltered <- read.csv(paste(SuccessGridsDirectory,  paste(TargetFileNamestub,"_by_language_success.csv", sep="" ,collapse=NULL), sep="" ,collapse=NULL))
png(paste(ChartsDirectory,"BarChartFilteredSuccessByLanguage.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(data=LanguageGroupsFiltered, aes(x=language, y=PercentSuccess, fill=language,colour=language)) +   geom_bar(stat="identity") + scale_color_manual(values = mycolors) + geom_text(aes(label=TotalCount),vjust=-1, show.legend = FALSE) + ggtitle("Success% by Language - Filtered")+ xlab("Language")+ ylab("Success %") + theme(plot.title = element_text(hjust = 0.5))  + geom_hline(yintercept = mean(LanguageGroupsFiltered$PercentSuccess), color="blue")+ geom_text(aes(0,mean(LanguageGroupsFiltered$PercentSuccess), label = paste(round(mean(LanguageGroupsFiltered$PercentSuccess),digits=0),"%",sep = "", collapse = NULL), hjust = -0.5, vjust = -1),color = "red") 
dev.off()

LanguageGroupsAll <- read.csv(paste(SuccessGridsDirectory,paste(TargetAllFileNamestub,"_by_language_success.csv", sep="" ,collapse=NULL), sep="" ,collapse=NULL))
LanguageGroupsAll <- subset(LanguageGroupsAll, TotalCount > 10)
LanguageGroupsAll <- subset(LanguageGroupsAll, !(language == 0))
AllSuccess <-  NROW(subset(AllProjects, state == "SUCCESSFUL"))
AllFailed <- NROW(subset(AllProjects, state == "FAILED"))
AllPercent <- round((AllSuccess/(AllSuccess + AllFailed))*100,1)

png(paste(ChartsDirectory,"BarChartAllSuccessByLanguage.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(data=LanguageGroupsAll, aes(x=language, y=PercentSuccess, fill=language,colour=language)) +   
geom_bar(stat="identity") + scale_color_manual(values = mycolors) + geom_text(aes(label=TotalCount),vjust=-1,show.legend = FALSE) + ggtitle("Success% by Language - All")+ 
xlab("Language")+ ylab("Success %") + theme(plot.title = element_text(hjust = 0.5))  + 
geom_hline(yintercept = AllPercent,color="blue") + 
geom_text(aes(0,AllPercent, label = paste(AllPercent,"%",sep = "", collapse = NULL), hjust = -0.5, vjust = -1),color = "red") 
dev.off()

anova_pledged_language_filtered <- aov(pledged ~ language, data = FilteredProjects)
acap <- capture.output(Anova(anova_pledged_language_filtered,type = "III"))
writeLines(acap, paste(ResultsDirectory,"PledgeLanguageFilteredANOVA.txt", sep="" ,collapse=NULL))



# TECHNOLOGY CATEGORY

TechnologySubcatFiltered <- read.csv(paste(SuccessGridsDirectory,paste(TargetFileNamestub,"_TECHNOLOGY_by_subcategory_success.csv", sep="" ,collapse=NULL), sep="" ,collapse=NULL))
TechnologySubcatFiltered$goalgroup.cat <- paste0('$',TechnologySubcatFiltered$MinGoal,"-$",TechnologySubcatFiltered$MaxGoal)

png(paste(ChartsDirectory,"BarChartFilteredSuccessTechnologyBySubCategory.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(data=TechnologySubcatFiltered, aes(x=subcategory, y=PercentSuccess, fill=subcategory,colour=subcategory)) +   
geom_bar(stat="identity") + scale_color_manual(values = mycolors) + 
ggtitle("Technology Category Success% by SubCategory  - Filtered")+ 
xlab("SubCategory") + ylab("Success %") + 
theme(plot.title = element_text(hjust = 0.5))+ 
geom_hline(yintercept = mean(TechnologySubcatFiltered$PercentSuccess), color="blue") + 
geom_text(aes(0,mean(TechnologySubcatFiltered$PercentSuccess),label = paste(" ",round(mean(TechnologySubcatFiltered$PercentSuccess),digits = 0),"%"),hjust =0, vjust = 0))
dev.off()

TechnologygoalgroupFiltered <- read.csv(paste(SuccessGridsDirectory,paste(TargetFileNamestub,"_TECHNOLOGY_by_goalgroup_success.csv", sep="" ,collapse=NULL), sep="" ,collapse=NULL))
TechnologygoalgroupFiltered$goalgroup.cat <- paste0('$',TechnologygoalgroupFiltered$MinGoal,"-$",TechnologygoalgroupFiltered$MaxGoal)
png(paste(ChartsDirectory,"LineGraphTechFilteredSuccessVsGoalAmount.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(TechnologygoalgroupFiltered, aes(x=goalgroup, y=PercentSuccess)) +
geom_point() +
geom_smooth(se=F) +
scale_x_discrete(name="Goal Amount", limits=TechnologygoalgroupFiltered$goalgroup.cat) +
scale_y_continuous(name="Success Ratio") +
ggtitle("Success By Goal Amount - Filtered - Technology Category")+ theme(plot.title = element_text(hjust = 0.5))
dev.off()


# CHARTS FOR GOALGROUP
#lgnd <- c('$1-$330','$333-$500','$508-$1000','$1008-$2000','$2020-$3000','$3080-$5000','$5100-$8000','$8100-$10000','$10007-$15000','$15150-$25000','$25001-$50000','$51000-$495400','$500000-$1000000')
GoalGroupsFiltered <- read.csv(paste(SuccessGridsDirectory,paste(TargetFileNamestub,"_by_goalgroup_success.csv", sep="" ,collapse=NULL), sep="" ,collapse=NULL)) 
GoalGroupsFiltered$goalgroup.cat <- paste0('$',GoalGroupsFiltered$MinGoal,"-$",GoalGroupsFiltered$MaxGoal)
GoalGroupsAll <- read.csv(paste(SuccessGridsDirectory,TargetAllFileNamestub,"_by_goalgroup_success.csv", sep="" ,collapse=NULL)) 
GoalGroupsAll$goalgroup.cat <- paste0('$',GoalGroupsAll$MinGoal,"-$",GoalGroupsAll$MaxGoal)
GoalGroupsFiltered$goalgroup.cat <- paste0('$',GoalGroupsFiltered$MinGoal,"-$",TechCategoryGroupsFiltered$MaxGoal)

GoalSuccessFiltered <- read.csv(paste(SuccessGridsDirectory,paste(TargetFileNamestub,"_by_goal_success.csv", sep="" ,collapse=NULL), sep="" ,collapse=NULL)) 
png(paste(ChartsDirectory,"LineGraphFilteredSuccessVsGoal.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(GoalSuccessFiltered, aes(x=goal, y=PercentSuccess)) +
geom_point() +
geom_smooth(se=F) +
scale_x_discrete(name="Goal") +
scale_y_continuous(name="Percent Success") +
ggtitle("Success By Goal - Filtered")+ theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(paste(ChartsDirectory,"LineGraphFilteredSuccessVsGoalGroup.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(GoalGroupsFiltered, aes(x=goalgroup, y=PercentSuccess)) +
geom_point() +
geom_smooth(se=F) +
scale_x_discrete(name="Goal Amount by Goal Group", limits=GoalGroupsFiltered$goalgroup.cat) +
scale_y_continuous(name="Percent Success") +
ggtitle("Success By Goal Group - Filtered")+ theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(paste(ChartsDirectory,"LineGraphFilteredPledgedVsGoalGroup.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(GoalGroupsFiltered, aes(x=goalgroup, y=MeanPledged)) + 
geom_point() +
geom_smooth(color=PledgedColor, size=2, se=F) + 
scale_x_discrete(name="Goal Amount by Goal Group", limits=GoalGroupsFiltered$goalgroup.cat) +
scale_y_continuous(name="Mean Pledged Amount") + 
ggtitle("Mean Pledge By Goal Group - Filtered")+ theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(paste(ChartsDirectory,"LineGraphFilteredMedianPledgedVsGoalGroup.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(GoalGroupsFiltered, aes(x=goalgroup, y=MedianPledged)) + 
geom_point() +
geom_smooth(color=PledgedColor, size=2,se=F ) + 
scale_x_discrete(name="Goal Amount by Goal Group", limits=GoalGroupsFiltered$goalgroup.cat) +
scale_y_continuous(name="Median Pledged Amount") + 
ggtitle("Median Pledged By Goal Group - Filtered")+ theme(plot.title = element_text(hjust = 0.5))
dev.off()


png(paste(ChartsDirectory,"LineGraphAllSuccessVsGoalAmount.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(GoalGroupsAll, aes(x=goalgroup, y=PercentSuccess)) +
geom_point() +
geom_smooth(color=PercentColor, size=2, se=F) + 
scale_x_discrete(name="Goal Amount", limits=GoalGroupsAll$goalgroup.cat) +
scale_y_continuous(name="Percent Success") +
ggtitle("Success By Goal Amount - All") + theme(plot.title = element_text(hjust = 0.5))
#ggplot(GoalGroupsAll, aes(x=goalgroup, y=PercentSuccess)) + geom_point() +geom_smooth(se=F) + scale_x_discrete(name="Goal Amount", limits=lgnd) + scale_y_continuous(name="Percent Success") + ggtitle("Success By Goal Amount - All")
dev.off()

# CHARTS & TESTS FOR GOAL
png(paste(ChartsDirectory,"LineGraphFilteredSuccessVsGoal.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(GoalGroupsFiltered, aes(x=goalgroup, y=PercentSuccess)) +
geom_point() +
geom_smooth(se=F) +
scale_x_discrete(name="Goal Amount by Goal Group", limits=GoalGroupsFiltered$goalgroup.cat) +
scale_y_continuous(name="Percent Success") +
ggtitle("Success By Goal Group - Filtered")+ theme(plot.title = element_text(hjust = 0.5))
dev.off()

chires <- chisq.test(table(FilteredProjects$goal, FilteredProjects$projectsuccess))
chisqcap <- capture.output(chires)
writeLines(chisqcap, con=file(paste(ResultsDirectory,"GoalGroupSuccessAllChiSquare.txt", sep="" ,collapse=NULL)))
closeAllConnections()

my_anova_actualpledged <- aov(pledged ~ goal, data = FilteredProjects)
acap <- capture.output(Anova(my_anova_actualpledged,type = "III"))
writeLines(acap, paste(ResultsDirectory,"PledgeGoalFilteredANOVA.txt", sep="" ,collapse=NULL))
closeAllConnections()

my_anova_actualpledged <- aov(actualpledged ~ category * goalgroup, data = FilteredProjects)
acap <- capture.output(Anova(my_anova_actualpledged,type = "III"))
writeLines(acap, paste(ResultsDirectory,"ActualPledgeCategoryGoalgroupFilteredANOVA.txt", sep="" ,collapse=NULL))
closeAllConnections()


my_anova_pledged <- aov(pledged ~ category * goal, data = FilteredProjects)
acap <- capture.output(Anova(my_anova_pledged,type = "III"))
writeLines(acap, paste(ResultsDirectory,"PledgedCategoryGoalFilteredANOVA.txt", sep="" ,collapse=NULL))
closeAllConnections()

my_anova_actualpledgedall <- aov(actualpledged ~ category * goal, data = FilteredProjects)
acap <- capture.output(Anova(my_anova_actualpledgedall,type = "III"))
writeLines(acap, paste(ResultsDirectory,"ActualPledgeCategoryGoalFilteredANOVA.txt", sep="" ,collapse=NULL))
closeAllConnections()

my_anova_pledgedall <- aov(actualpledged ~ category * goal, data = AllProjects)
acap <- capture.output(Anova(my_anova_pledgedall,type = "III"))
writeLines(acap, paste(ResultsDirectory,"ActualPledgeCategoryGoalAllANOVA.txt", sep="" ,collapse=NULL))
closeAllConnections()


p1 <- ggplot(ks.proj, aes(log(goal),  fill = projectsuccess)) +
  geom_density() +
  theme(legend.position = "bottom") +
  xlab("USD pledged (log-transformed)") + ylab("") +
  ggtitle("KS projects' Goal")

# Log-transformed usd_goal_real
p2 <- ggplot(ks.proj, aes(x = projectsuccess, y = log(goal), fill = projectsuccess)) +
  geom_boxplot() +
  theme(legend.position = "bottom") +
  ylab("Goal in USD (log-transformed)") + xlab("") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() + 
  ggtitle(" Goal of the KS projects (Log)")
png(paste(ChartsDirectory,"GoalVsPledgedLog.png", sep="" ,collapse=NULL), width = 1500, height = 700)
gridExtra::grid.arrange(p1, p2, ncol = 2)
dev.off()




# TWO-LINE CHARTS 
TechCategoryGroupsFiltered <- read.csv(paste(SuccessGridsDirectory,paste(TargetFileNamestub,"_TECHNOLOGY_by_goalgroup_success.csv", sep="" ,collapse=NULL), sep="" ,collapse=NULL))

GoalGroupsFiltered <- read.csv(paste(SuccessGridsDirectory,paste(TargetFileNamestub,"_by_goalgroup_success.csv", sep="" ,collapse=NULL), sep="" ,collapse=NULL)) 
GoalGroupsFiltered$goalgroup.cat <- paste0('$',GoalGroupsFiltered$MinGoal,"-$",GoalGroupsFiltered$MaxGoal)


png(paste(ChartsDirectory,"LineGraphFilteredPledgedAndSuccessVsGoalAmount.png", sep="" ,collapse=NULL), width = 1000, height = 700)
require(scales)
coeff <- .0003

ggplot(GoalGroupsFiltered, aes(x=goalgroup, y=MeanPledged))   +
geom_smooth(color=PledgedColor, size=2, se=F) + 
scale_x_discrete(name="Goal Amount", limits=GoalGroupsFiltered$goalgroup.cat) +
scale_y_continuous(name="Mean Pledged Amount", sec.axis = sec_axis( trans=~.*coeff, name="Chance of Success")) +
geom_smooth(aes(y=PercentSuccess / coeff), size=2)+  
theme_light() +
theme(axis.title.y = element_text(color = PledgedColor, size=13),axis.title.y.right = element_text(color = PercentColor, size=13))+ 
ggtitle("Mean Pledged Amount and Chance of Success By Goal Amount - Filtered") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(paste(ChartsDirectory,"LineGraphFilteredTechnologyPledgedAndSuccessVsGoalAmount.png", sep="" ,collapse=NULL), width = 1000, height = 700)
require(scales)
coeff <- .0003

ggplot(TechCategoryGroupsFiltered, aes(x=goalgroup, y=MeanPledged))   +
geom_smooth(color=PledgedColor, size=2, se=F) + 
scale_x_discrete(name="Goal Amount", limits=TechCategoryGroupsFiltered$goalgroup.cat) +
scale_y_continuous(name="Mean Pledged Amount", sec.axis = sec_axis( trans=~.*coeff, name="Chance of Success")) +
geom_smooth(aes(y=PercentSuccess / coeff), size=2)+  
theme_light() +
theme(axis.title.y = element_text(color = PledgedColor, size=13),axis.title.y.right = element_text(color = PercentColor, size=13))+ 
ggtitle("Mean Pledged Amount and Chance of Success By Goal Amount - Filtered - Technology Category") + theme(plot.title = element_text(hjust = 0.5))
dev.off()


# CHARTS BY YEAR
YearGroupsAll <- read.csv(paste(SuccessGridsDirectory,paste(TargetAllFileNamestub,"_by_year_success.csv", sep="" ,collapse=NULL), sep="" ,collapse=NULL)) 
YEARS = min(YearGroupsAll['year']):max(YearGroupsAll['year'])

png(paste(ChartsDirectory,"LineGraphAllSuccessbyyear.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(YearGroupsAll, aes(x=year, y=PercentSuccess)) +
geom_point() +
geom_smooth(color=PercentColor, size=2, se=F) + 
scale_y_continuous(name="Percent Success") +
scale_x_continuous("Year" , labels = as.character(YEARS), breaks = YEARS ) +
ggtitle("Success By Year - All") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(paste(ChartsDirectory,"LineGraphAllPledgedByYear.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(YearGroupsAll, aes(x=year, y=MeanPledged)) +
geom_point() +
geom_smooth(color=PledgedColor, size=2, se=F) + 
scale_y_continuous(name="Mean Pledged $") +
scale_x_continuous("Year" , labels = as.character(YEARS), breaks = YEARS ) +
ggtitle("Mean Pledge By Year - All") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(paste(ChartsDirectory,"LineGraphAllMedianPledgedByYear.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(YearGroupsAll, aes(x=year, y=MedianPledged)) +
geom_point() +
geom_smooth(color=PledgedColor, size=2, se=F) + 
scale_y_continuous(name="Median Pledged $") +
scale_x_continuous("Year" , labels = as.character(YEARS), breaks = YEARS ) +
ggtitle("Median Pledge By Year - All") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(paste(ChartsDirectory,"LineGraphAllGoalByYear.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(YearGroupsAll, aes(x=year, y=MeanGoal)) +
geom_point() +
geom_smooth(color=PledgedColor, size=2, se=F) + 
scale_y_continuous(name="Mean Goal $") +
scale_x_continuous("Year" , labels = as.character(YEARS), breaks = YEARS ) +
ggtitle("Mean Goal By Year - All") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(paste(ChartsDirectory,"BarChartStackedAllSuccessByYear.png", sep="" ,collapse=NULL), width = 700, height = 500)
ggplot(ks.projall, aes(x = launched_year, fill = projectsuccess)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ylab("Number of projects") + xlab("Year launched") +
  ggtitle("Project Success by Year")+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()  

# DESIGN CATEGORY

DesignCategoryGroupsFiltered <- read.csv(paste(SuccessGridsDirectory,paste(TargetFileNamestub,"_DESIGN_by_goalgroup_success.csv", sep="" ,collapse=NULL), sep="" ,collapse=NULL))
DesignCategoryGroupsFiltered$goalgroup.cat <- paste0('$',DesignCategoryGroupsFiltered$MinGoal,"-$",DesignCategoryGroupsFiltered$MaxGoal)

png(paste(ChartsDirectory,"LineGraphDesignFilteredPledgeVsGoal.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(DesignCategoryGroupsFiltered, aes(x=goalgroup, y=MeanPledged)) +
geom_point() +
geom_smooth(color=PledgedColor, size=2, se=F) +
scale_x_discrete(name="Goal Amount", limits=DesignCategoryGroupsFiltered$goalgroup.cat) +
scale_y_continuous(name="Mean Pledge") +
ggtitle("Mean Pledge by Goal Amount - Filtered - Design Category")+ theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(paste(ChartsDirectory,"LineGraphDesignFilteredSuccessVsGoal.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(DesignCategoryGroupsFiltered, aes(x=goalgroup, y=PercentSuccess)) +
geom_point() +
geom_smooth(color=PercentColor, size=2, se=F) +
scale_x_discrete(name="Goal Amount", limits=DesignCategoryGroupsFiltered$goalgroup.cat) +
scale_y_continuous(name="Success") +
ggtitle("Success by Goal Amount - Filtered - Design Category")+ theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(paste(ChartsDirectory,"LineGraphDesignFilteredPledgeVsGoal.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(TechCategoryGroupsFiltered, aes(x=goalgroup, y=MeanPledged)) +
geom_point() +
geom_smooth(color=PledgedColor, size=2, se=F) +
scale_x_discrete(name="Goal Amount", limits=GoalGroupsFiltered$goalgroup.cat) +
scale_y_continuous(name="Mean Pledge") +
ggtitle("Mean Pledge by Goal Amount - Filtered - Technology Category")+ theme(plot.title = element_text(hjust = 0.5))
dev.off()


#scale_y_continuous(name="Mean Pledged Amount", labels=format(aty, scientific=FALSE), sec.axis = sec_axis( trans=~.*coeff, name="Chance of Success")) + ylim(0,100)
#lgnd <- c('$1-$400','$404-$750','$753-$1200','$1208-$2000','$2001-$3000','$3001-$4970','$4975-$5000','$5000.01-$7500','$7501-$10000','$10001-$16000','$16060-$25000','$25001-$50000','$50042-$1000000')


# WORDCLOUDS


SuccessBlurbs = read.delim(paste(SourceDirectory,paste(TargetFileNamestub,"_Success_Blurbs.txt", sep="" ,collapse=NULL), sep="" ,collapse=NULL),stringsAsFactor = FALSE)
FailedBlurbs = read.delim(paste(SourceDirectory,paste(TargetFileNamestub,"_Failed_Blurbs.txt", sep="" ,collapse=NULL), sep="" ,collapse=NULL),stringsAsFactor = FALSE)

all = c(SuccessBlurbs,FailedBlurbs)
corpus = Corpus(VectorSource(all))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, c("the", "and", stopwords("english")))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = c("Success","Failed")
pdf(paste(ChartsDirectory,"Comparison_Blurbs_WordCloud.pdf", sep="" ,collapse=NULL),width=5,height=5)
comparison.cloud(tdm,random.order=FALSE, colors = c("#55C264", "red", "#FF0099", "#6600CC"),title.size=1.5, max.words=100)
dev.off()

SuccessNames = read.delim(paste(SourceDirectory,paste(TargetFileNamestub,"_Success_Names.txt",sep="", collapse = NULL),sep="", collapse = NULL),stringsAsFactor = FALSE, encoding = "UTF-8")
FailedNames = read.delim(paste(SourceDirectory,paste(TargetFileNamestub,"_Failed_Names.txt",sep="", collapse = NULL),sep="", collapse = NULL),stringsAsFactor = FALSE, encoding = "UTF-8")
allname = c(SuccessNames,FailedNames)
corpus = Corpus(VectorSource(allname))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, c("the", "and", stopwords("english")))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = c("Success","Failed")
pdf(paste(ChartsDirectory,"Comparison_Names_WordCloud.pdf", sep="" ,collapse=NULL),width=5,height=5)
comparison.cloud(tdm,random.order=FALSE, colors = c("#55C264", "red", "#FF0099", "#6600CC"),title.size=1.5, max.words=100)
dev.off()




# TESTS FOR COUNTRY
anova_pledged_country_all <- aov(pledged ~ country, data = AllProjects)
acap <- capture.output(Anova(anova_pledged_country_all,type = "III"))
writeLines(acap, paste(ResultsDirectory,"PledgeCountryAllANOVA.txt", sep="" ,collapse=NULL))
anova_backers_country_all <- aov(backers_count ~ country, data = AllProjects)
acap <- capture.output(Anova(anova_backers_country_all,type = "III"))
writeLines(acap, paste(ResultsDirectory,"BackersCountryAllANOVA.txt", sep="" ,collapse=NULL))

dta <- read.csv(paste(SourceDirectory, TargetAllFileName, sep="" ,collapse=NULL),stringsAsFactors=T)
dta$country[dta$country =="XX"] <- "US"
dta$country <- sub("^$","NM",dta$country ) # it did not detect namibia properly
dta$language <- sub("0","en",dta$language) # manually scanned these, all english 


# set the reference level for logistic regression
dta$projectsuccess <- relevel(dta$projectsuccess, ref="Success")

# chi square and fisher's exact test (chi square isn't a good fit because of the small counts)
ChiTest <- chisq.test(table(dta$country, dta$projectsuccess))

#FisherTest <- fisher.test(table(dta$country, dta$projectsuccess), simulate.p.value=TRUE)
#pdf(paste(ResultsDirectory,"CountrySuccessCleanChiSquare.pdf", sep="" ,collapse=NULL),width=5,height=5)
#dev.off()
fcap <- capture.output(fisher.test(table(dta$country, dta$projectsuccess), simulate.p.value=TRUE))
writeLines(fcap, paste(ResultsDirectory,"SuccessCountryAllfisher.txt", sep="" ,collapse=NULL))


ks.proj$tmp.var <- 1
num.proj <- aggregate(tmp.var ~ country, ks.proj, sum)
num.proj$country.reduced <- ifelse(num.proj$tmp.var < 100, "<100", as.character(num.proj$country))
colnames(num.proj) <- c("country", "num.projects", "country.reduced")
ks.proj <- ks.proj[,1:35]
ks.proj <- merge(ks.proj, num.proj, by="country")
ks.proj$country.reduced <- as.factor(ks.proj$country.reduced)
by.country <- glm(projectsuccess ~ country.reduced, ks.proj, family="binomial")
test <- capture.output(summary(by.country))
write.csv(test,file=paste(ResultsDirectory,"CountrySuccessAllLogisticRegression.csv", sep="" ,collapse=NULL))
# logistic regression using untrimmed data
#by.country <- glm(projectsuccess ~ country, dta, family="binomial")
#summary(by.country)
#write.csv(summary(by.country)['coefficients'],file=paste(ResultsDirectory,"CountrySuccessAllLogisticRegression.csv", sep="" ,collapse=NULL))

# Just to see how many are still pretty small in the full dataset
# table(dta$country, dta$projectsuccess)

  
#png(paste(ChartsDirectory,"BarChartAllTop30CountriesSuccess.png", sep="" ,collapse=NULL), width = 1500, height = 700)
#successmean <- round(length(which(dta$projectsuccess == "Success"))/ NROW(dta),3)
#ggplot(tbl2, aes(x = new.country, y = perc, fill=projectsuccess)) +
 # geom_bar(stat="identity") + 
 # scale_y_continuous(labels=scales::percent) +
 # geom_text(aes(label = round(perc*100,1)), size = 3, position = position_stack(vjust=.5)) +
 # ylab("relative frequencies")  +
 # geom_hline(yintercept = successmean, color="black")+
#grid.text(paste0(round(successmean*100,1),"%"), gp=gpar(col="blue", fontsize=10), x = unit(.98, "npc"), y = unit(successmean+.08, "npc")) +
#  theme(legend.position="bottom") 
  
dev.off()
#ggtitle("Success % Countries with more than 100 projects")+ theme(plot.title = element_text(hjust = 0.5)) +



countrygrid <- read.csv(paste(SuccessGridsDirectory,paste(TargetAllFileNamestub,"_by_country_success.csv", sep="" ,collapse=NULL), sep="" ,collapse=NULL)) 
countrygrid100 <- subset(countrygrid, TotalCount > 100)
png(paste(ChartsDirectory,"BarChartAllMeanPledgedByCountry.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(data=countrygrid100, aes(x=country, y=MeanPledged, fill=country,colour=country)) +   
geom_bar(stat="identity") + scale_color_manual(values = mycolors) + 
ggtitle("Pledged Amount (Mean) by Country - All")+ xlab("Country") + ylab("Mean Pledged Amount") + 
theme(plot.title = element_text(hjust = 0.5))  + 
geom_hline(yintercept = mean(countrygrid100$MeanPledged), color=PledgedColor) +
geom_text(aes(0,mean(MeanPledged), label = paste("Mean: $",round(mean(MeanPledged),digits=0),sep = "", collapse = NULL), hjust = -0.5, vjust = -1),color = "black") 
dev.off()

png(paste(ChartsDirectory,"BarChartAllMedianPledgedByCountry.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(data=countrygrid100, aes(x=country, y=MedianPledged, fill=country,colour=country)) +   
geom_bar(stat="identity") + scale_color_manual(values = mycolors) + 
ggtitle("Pledged Amount (Median) by Country - All")+ xlab("Country") + ylab("Median Pledged Amount") + 
theme(plot.title = element_text(hjust = 0.5))  + 
geom_hline(yintercept = mean(countrygrid100$MedianPledged), color=PledgedColor) +
geom_text(aes(0,mean(MedianPledged), label = paste("Mean Median: $",round(mean(MedianPledged),digits=0),sep = "", collapse = NULL), hjust = -0.5, vjust = -1),color = "black") 
dev.off()

png(paste(ChartsDirectory,"BarChartAllMeanBackersByCountry.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(data=countrygrid100, aes(x=country, y=MeanBackers, fill=country,colour=country)) +   
geom_bar(stat="identity") + scale_color_manual(values = mycolors) + 
ggtitle("Backer Count (Mean) by Country - All")+ xlab("Country") + ylab("Mean Baker Count") + 
theme(plot.title = element_text(hjust = 0.5))  + 
geom_hline(yintercept = mean(countrygrid100$MeanBackers), color="blue") +
geom_text(aes(0,mean(MeanBackers), label = paste("Mean Backer Count:",round(mean(MeanBackers),digits=0),sep = "", collapse = NULL), hjust = -0.5, vjust = -1),color = "black") 
dev.off()

png(paste(ChartsDirectory,"BarChartAllMedianBackersByCountry.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(data=countrygrid100, aes(x=country, y=MedianBackers, fill=country,colour=country)) +   
geom_bar(stat="identity") + scale_color_manual(values = mycolors) + 
ggtitle("Backer Count (Median) by Country - All")+ xlab("Country") + ylab("Median Baker Count") + 
theme(plot.title = element_text(hjust = 0.5))  + 
geom_hline(yintercept = median(AllProjects$backers_count), color="blue") +
geom_text(aes(0,mean(MedianBackers), label = paste("Median Backer Count:",round(median(AllProjects$backers_count),digits=0),sep = "", collapse = NULL), hjust = -0.2, vjust = -1),color = "black") 
dev.off()

png(paste(ChartsDirectory,"BarChartTop30SuccessByCountry.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(data=countrygrid100, aes(x=country, y=PercentSuccess, fill=country,colour=country)) +   
geom_bar(stat="identity") + scale_color_manual(values = mycolors) + 
ggtitle("Success by Country - Countries > 100 Projects")+ xlab("Country") + ylab("% Success") + 
theme(plot.title = element_text(hjust = 0.5))  + 
geom_hline(yintercept = SuccessAllProjects$PercentSuccess, color=PercentColor) +
geom_text(aes(0,mean(PercentSuccess), label = paste("Mean Success: ",round(SuccessAllProjects$PercentSuccess,digits=0), "%",sep = "", collapse = NULL), hjust = -0.2, vjust = -0.2),color = "black") 

dev.off()
# DURATION DAYS

png(paste(ChartsDirectory,"LineGraphFilteredSuccessVsDurationDays.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(DurationdaysFiltered, aes(x=durationdays, y=PercentSuccess)) +
geom_point() +
geom_smooth(se=F) +
scale_x_discrete(name="Duration (days)", limits=DurationdaysFiltered$durationdays) +
scale_y_continuous(name="Percent Success") +
ggtitle("Success By Duration (days) - Filtered")+ theme(plot.title = element_text(hjust = 0.5))
dev.off()

ddres <- chisq.test(table(FilteredProjects$durationdays, FilteredProjects$projectsuccess))
chisqcap <- capture.output(chires)
writeLines(chisqcap, con=file(paste(ResultsDirectory,"DurationDaysSuccessFilteredChiSquare.txt", sep="" ,collapse=NULL)))
closeAllConnections()

FilteredProjects5000 <-  subset(FilteredProjects, goal == 5000)
png(paste(ChartsDirectory,"BarGraphFilteredSuccessVsDurationDays5000goal.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(FilteredProjects5000, aes(x = durationdays, fill = projectsuccess)) +
  geom_bar() +
  coord_flip() +
  theme(legend.position = "bottom") +
  ylab("Number of projects") + xlab("PRoject Duration (days)") +
  ggtitle("Duration and Success of $5,000.00 Projects")+ theme(plot.title = element_text(hjust = 0.5))
  dev.off()

nrow(FilteredProjects5000)
FilteredProjects5000.Success <- subset(FilteredProjects5000, projectsuccess == "Success")
nrow(FilteredProjects5000.Success)/nrow(FilteredProjects5000)

FilteredProjects.30 <- subset(FilteredProjects5000, durationdays < 31)
FilteredProjects.30.Success <- subset(FilteredProjects.30, projectsuccess == "Success")
nrow(FilteredProjects.30)
nrow(FilteredProjects.30.Success)/nrow(FilteredProjects.30)

FilteredProjects5000.60 <- subset(FilteredProjects5000, durationdays >31 )
FilteredProjects5000.60.Success <- subset(FilteredProjects5000.60, projectsuccess == "Success")
nrow(FilteredProjects5000.60)
nrow(FilteredProjects5000.60.Success)/nrow(FilteredProjects5000.60)


FilteredProjects.30 <- subset(FilteredProjects, durationdays == 30)
FilteredProjects.30.Success <- subset(FilteredProjects.30, projectsuccess == "Success")
nrow(FilteredProjects.30)
nrow(FilteredProjects.30.Success)/nrow(FilteredProjects.30)

FilteredProjects.30 <- subset(FilteredProjects, durationdays < 31)
FilteredProjects.30.Success <- subset(FilteredProjects.30, projectsuccess == "Success")
nrow(FilteredProjects.30)
nrow(FilteredProjects.30.Success)/nrow(FilteredProjects.30)

FilteredProjects5000.60 <- subset(FilteredProjects, durationdays >31 )
FilteredProjects5000.60.Success <- subset(FilteredProjects5000.60, projectsuccess == "Success")
nrow(FilteredProjects5000.60.Success)/nrow(FilteredProjects5000.60)


FilteredProjects.30 <- subset(FilteredProjects, durationdays < 31)
FilteredProjects.30.Success <- subset(FilteredProjects.30, projectsuccess == "Success")
nrow(FilteredProjects.30)
nrow(FilteredProjects.30.Success)/nrow(FilteredProjects.30)

FilteredProjects.60 <- subset(FilteredProjects, durationdays > 31)
FilteredProjects.60.Success <- subset(FilteredProjects.60, projectsuccess == "Success")
nrow(FilteredProjects.60)
nrow(FilteredProjects.60.Success)/nrow(FilteredProjects.60)



FilteredProjects10000 <-  subset(FilteredProjects, goal == 10000)
png(paste(ChartsDirectory,"BarGraphFilteredSuccessVsDurationDays10000goal.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(FilteredProjects10000, aes(x = durationdays, fill = projectsuccess)) +
  geom_bar() +
  coord_flip() +
  theme(legend.position = "bottom") +
  ylab("Number of projects") + xlab("Project Duration (days)") +
  ggtitle("Duration and Success of $10,000.00 Projects")+ theme(plot.title = element_text(hjust = 0.5))
  dev.off()

FilteredProjects10000.Success <- subset(FilteredProjects10000, projectsuccess == "Success")
row(FilteredProjects10000)
nrow(FilteredProjects10000.Success)/nrow(FilteredProjects10000)

FilteredProjects10000.30 <- subset(FilteredProjects, durationdays < 31)
FilteredProjects10000.30.Success <- subset(FilteredProjects10000.30, projectsuccess == "Success")
nrow(FilteredProjects10000.30.Success)/nrow(FilteredProjects10000.30)

FilteredProjects10000.60 <- subset(FilteredProjects, durationdays >31 )
FilteredProjects10000.60.Success <- subset(FilteredProjects10000.60, projectsuccess == "Success")
nrow(FilteredProjects10000.60.Success)/nrow(FilteredProjects10000.60)

FilteredProjects5000.30 <- subset(FilteredProjects, durationdays < 31)
FilteredProjects5000.30.Success <- subset(FilteredProjects5000.30, projectsuccess == "Success")
nrow(FilteredProjects5000.30.Success)/nrow(FilteredProjects5000.30)

FilteredProjects5000.60 <- subset(FilteredProjects, durationdays >31 )
FilteredProjects5000.60.Success <- subset(FilteredProjects5000.60, projectsuccess == "Success")
nrow(FilteredProjects5000.60.Success)/nrow(FilteredProjects5000.60)



FilteredProjects10000 <-  subset(FilteredProjects, goal == 10000)
png(paste(ChartsDirectory,"BarGraphFilteredSuccessVsDurationDays10000goal.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(FilteredProjects10000, aes(x = durationdays, fill = projectsuccess)) +
  geom_bar() +
  coord_flip() +
  theme(legend.position = "bottom") +
  ylab("Number of projects") + xlab("Project Duration (days)") +
  ggtitle("Duration and Success of $10,000.00 Projects")+ theme(plot.title = element_text(hjust = 0.5))
  dev.off()

FilteredProjects20000 <- subset(FilteredProjects, goal == 20000)
FilteredProjects20000.Success <- subset(FilteredProjects20000, projectsuccess == "Success")
row(FilteredProjects20000)
nrow(FilteredProjects20000.Success)/nrow(FilteredProjects20000)

FilteredProjects20000.30 <- subset(FilteredProjects20000, durationdays < 31)
FilteredProjects20000.30.Success <- subset(FilteredProjects20000.30, projectsuccess == "Success")
nrow(FilteredProjects20000.30.Success)/nrow(FilteredProjects20000.30)

FilteredProjects20000.60 <- subset(FilteredProjects20000, durationdays >31 )
FilteredProjects20000.60.Success <- subset(FilteredProjects20000.60, projectsuccess == "Success")
nrow(FilteredProjects20000.60.Success)/nrow(FilteredProjects20000.60)

FilteredProjects5000.30 <- subset(FilteredProjects, durationdays < 31)
FilteredProjects5000.30.Success <- subset(FilteredProjects5000.30, projectsuccess == "Success")
nrow(FilteredProjects5000.30.Success)/nrow(FilteredProjects5000.30)

FilteredProjects5000.60 <- subset(FilteredProjects, durationdays >31 )
FilteredProjects5000.60.Success <- subset(FilteredProjects5000.60, projectsuccess == "Success")
nrow(FilteredProjects5000.60.Success)/nrow(FilteredProjects5000.60)




# STAFF PICK

FilteredProjectsSP <- FilteredProjects
scap <- capture.output(chisq.test(table(FilteredProjectsSP$staff_pick, FilteredProjectsSP$projectsuccess)))
writeLines(scap, paste(ResultsDirectory,"SuccessStaffPIckFilteredChiSquare.txt", sep="" ,collapse=NULL))
closeAllConnections()


# RANDOM FOREST


	

fmla = projectsuccess ~ category + staff_pick + deadline_year + goal +
    deadline_month + deadline_day + durationdays + country +
    goalgroup + launched_year + launched_month + launched_day + SubMatch
ks.proj  <- FilteredProjects
ks.proj$tmp.var <- 1
num.proj <- aggregate(tmp.var ~ country, ks.proj, sum)
num.proj$country.reduced <- ifelse(num.proj$tmp.var < 100, "<100", as.character(num.proj$country))
colnames(num.proj) <- c("country", "num.projects", "country.reduced")
ks.proj <- ks.proj[,1:35]
ks.proj <- merge(ks.proj, num.proj, by="country")
ks.proj$country.reduced <- as.factor(ks.proj$country.reduced)
ks.proj$SubMatch <-  ifelse(as.character(ks.proj$category)==as.character(ks.proj$subcategory), "Matched", "NoMatch") 	
ks.proj$country.reduced <- relevel(ks.proj$country.reduced, ref="US")
ks.proj$language <- relevel(ks.proj$language, ref="en")
ks.proj$category <- relevel(ks.proj$category, ref="ART")	
# create Training - Test and Validation set (60 - 20 - 20%)
set.seed(12420360)
spec = c(train = .6, test = .2, validate = .2)
g = sample(cut(seq(nrow(ks.proj)), nrow(ks.proj)*cumsum(c(0,spec)), labels = names(spec)))

res = split(FilteredProjects, g)
train.data <- res$train
test.data <- res$test
validation.data <- res$validate
rf.model2 <- randomForest(fmla, data = train.data, importance = TRUE, ntree=500)	
	
	
	
	
FilteredProjectsSuccessRF <-	subset(FilteredProjects, select = c("projectsuccess","category", "goal", "staff_pick", "language"))
sample = sample.split(FilteredProjectsSuccessRF$projectsuccess, SplitRatio = .75)
train = subset(FilteredProjectsSuccessRF, sample == TRUE)
test  = subset(FilteredProjectsSuccessRF, sample == FALSE)
successrf <- capture.output(randomForest(projectsuccess ~ .,data=train))
writeLines(successrf, paste(ResultsDirectory,"Success.category.goal.staff_pick.language.FilteredRandomForest.txt", sep="" ,collapse=NULL))


FilteredProjectsPledgedRF <-	subset(FilteredProjects, select = c("pledged", "category", "goal", "staff_pick", "language"))
sample = sample.split(FilteredProjectsPledgedRF, SplitRatio = .75)
train = subset(FilteredProjectsPledgedRF, sample == TRUE)
test  = subset(FilteredProjectsPledgedRF, sample == FALSE)
pledgedrf <- capture.output(randomForest(pledged ~ .,data=train))
writeLines(pledgedrf, paste(ResultsDirectory,"Pledged.category.goal.staff_pick.language.FilteredRandomForest.txt", sep="" ,collapse=NULL))

AllProjectsSuccessRF <-	subset(AllProjects, select = c("projectsuccess","category", "goal", "staff_pick", "language"))
sample = sample.split(AllProjectsSuccessRF$projectsuccess, SplitRatio = .75)
train = subset(AllProjectsSuccessRF, sample == TRUE)
test  = subset(AllProjectsSuccessRF, sample == FALSE)
successAllrf <- capture.output(randomForest(projectsuccess ~ .,data=train))
writeLines(successAllrf, paste(ResultsDirectory,"Success.category.goal.staff_pick.language.AllRandomForest.txt", sep="" ,collapse=NULL))

AllProjectsPledgedRF <-	subset(AllProjects, select = c("pledged", "category", "goal",  "staff_pick","language"))
sample = sample.split(AllProjectsPledgedRF, SplitRatio = .75)
train = subset(AllProjectsPledgedRF, sample == TRUE)
test  = subset(AllProjectsPledgedRF, sample == FALSE)
pledgedAllrf <- capture.output(randomForest(pledged ~ .,data=train))
writeLines(pledgedAllrf, paste(ResultsDirectory,"Pledged.category.goal.staff_pick.language.AllRandomForest.txt", sep="" ,collapse=NULL))
closeAllConnections()


pledged.rf <- capture.output(randomForest(pledged ~ ., data=AllProjectsRF, importance=TRUE, ntree=500, mtry = 2, do.trace=100))
writeLines(pledged.rf, paste(ResultsDirectory,"Success.category.goal.staff_pick.language.AllRandomForest.txt", sep="" ,collapse=NULL))

AllProjectsRF <-	subset(AllProjects, select = c("pledged", "category", "goal", "staff_pick"))

pledged.rf <- capture.output(randomForest(pledged ~ ., data=AllProjectsRF, importance=TRUE, ntree=500, mtry = 2, do.trace=100))
writeLines(pledged.rf, paste(ResultsDirectory,"PledgedAllOOBRandomForest.txt", sep="" ,collapse=NULL))

FilteredProjectsRF <-	subset(FilteredProjects, select = c("pledged", "category", "goal", "staff_pick"))

pledged.rf <- randomForest(pledged ~ ., data=FilteredProjectsRF, importance=TRUE, ntree=500, mtry = 2, do.trace=100)
cap <- capture.output(pledged.rf)
writeLines(cap, paste(ResultsDirectory,"PledgedFilteredOOBRandomForest.txt", sep="" ,collapse=NULL))

closeAllConnections()




# DENSITY PLOTS

png(paste(ChartsDirectory,"DensityPlotPledgedValue.png", sep="" ,collapse=NULL), width = 1500, height = 700)
plot(density(AllProjects$pledged))
dev.off()

png(paste(ChartsDirectory,"DensityPlotLogPledgedValue.png", sep="" ,collapse=NULL), width = 1500, height = 700)
plot(density(log(AllProjects$pledged)))
dev.off()

AllProjectsfilt <-  subset(AllProjects, state == "SUCCESSFUL" | state == "FAILED")
AllProjectsfilt <-  subset(AllProjectsfilt, pledged > 1)
AllProjectsfilt <-  subset(AllProjectsfilt, goal > 1)

png(paste(ChartsDirectory,"DensityPlotLogNonZeroPledgedValueAllProjects.png", sep="" ,collapse=NULL), width = 1500, height = 700)
plot(density(log(AllProjectsfilt$pledged)))
dev.off()

png(paste(ChartsDirectory,"DensityPlotLogNonZeroGoalValueAllProjects.png", sep="" ,collapse=NULL), width = 1500, height = 700)
plot(density(log(AllProjectsfilt$goal)))
dev.off()

FilteredProjectsfilt <-  subset(FilteredProjects)
FilteredProjectsfilt <-  subset(FilteredProjectsfilt, pledged > 1)
png(paste(ChartsDirectory,"DensityPlotLogNonZeroPledgedValueFilteredProjects.png", sep="" ,collapse=NULL), width = 1500, height = 700)
plot(density(log(FilteredProjectsfilt$pledged)))
dev.off()

png(paste(ChartsDirectory,"DensityPlotLogNonZeroGoalValueFilteredProjects.png", sep="" ,collapse=NULL), width = 1500, height = 700)
plot(density(log(FilteredProjectsfilt$goal)))
dev.off()

# BACKERS 
ks.proj <- read.csv(paste(SourceDirectory, TargetFileName, sep="" ,collapse=NULL), stringsAsFactors = T)
# Log transforming the backers field shows the distribution better
p1 <- ggplot(ks.proj, aes(log(backers_count),  fill = projectsuccess)) +
  geom_density() +
  theme(legend.position = "bottom") +
  ylab("Number of Backers") + xlab("") +
  ggtitle("# of Backers (Log)")

p2 <- ggplot(ks.proj, aes(x = projectsuccess, y = log(backers_count), fill = projectsuccess)) +
  geom_boxplot() +
  coord_flip() + 
  theme(legend.position = "bottom") +
  ylab("# of Backers (log-transformed)") + xlab("") +
  ggtitle("# of Backers (Log)")
png(paste(ChartsDirectory,"NumberOfBackersTransformation.png", sep="" ,collapse=NULL), width = 1500, height = 700)
gridExtra::grid.arrange(p1, p2, ncol = 2)
dev.off()

p1 <- ggplot(ks.proj, aes(x = log(backers_count), y = log(actualpledged))) +
  geom_jitter(aes(color = projectsuccess)) +
  theme(legend.position = "bottom") +
  ylab("Amount pledged (log)") + xlab("Backers (log)") +
  ggtitle("KS projects USD Pledged vs Backers")

p2 <- ggplot(ks.proj, aes(x = (backers_count), y = log(actualpledged))) +
  geom_jitter(aes(color = projectsuccess)) +
  theme(legend.position = "bottom") +
  ylab("Amount pledged (log)") + xlab("Backers") +
  ggtitle("KS projects USD Pledged vs Backers")

p3 <- ggplot(ks.proj, aes(x = log(backers_count), y = log(goal))) +
  geom_jitter(aes(color = projectsuccess)) +
  theme(legend.position = "bottom") +
  ylab("Goal (log)") + xlab("Backers (log)") +
  ggtitle("KS projects' Goal vs Backers")

p4 <- ggplot(ks.proj, aes(x = (backers_count), y = log(goal))) +
  geom_jitter(aes(color = projectsuccess)) +
  theme(legend.position = "bottom") +
  ylab("Goal (log)") + xlab("Backers") +
  ggtitle("KS projects' Goal vs Backers")

png(paste(ChartsDirectory,"NumberOfBackersSkewed.png", sep="" ,collapse=NULL), width = 1500, height = 700)
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
dev.off()


# Correlation between all numerical variables
convert <-  c("backers_count",  "goal", "pledged", "deadline_year", "deadline_month", "deadline_day",   "durationdays",  
 "actualpledged", "goalgroup", "pledgedgroup", "launched_year", "launched_month", "launched_day")

ks2 <-sapply(ks.proj[,convert],as.numeric)       

corMat <- cor(ks2)

corrplot.mixed(corMat,tl.pos = "lt")

#### Predictive Analysis

# Create a binary outcome for success
ks.proj$outcome <- ifelse(ks.proj$projectsuccess=="Success", 1, 0)

# Set reference level for each categorical variable
ks.proj$country.reduced <- relevel(ks.proj$country.reduced, ref="US")
ks.proj$language <- relevel(ks.proj$language, ref="en")
ks.proj$category <- relevel(ks.proj$category, ref="ART")
ks.proj$SubMatch <-  ifelse(as.character(ks.proj$category)==as.character(ks.proj$subcategory), "Matched", "NoMatch") 

# create Training - Test and Validation set (60 - 20 - 20%)
set.seed(12420360)
spec = c(train = .6, test = .2, validate = .2)
g = sample(cut(seq(nrow(ks.proj)), nrow(ks.proj)*cumsum(c(0,spec)), labels = names(spec)))

res = split(ks.proj, g)
train.data <- res$train
test.data <- res$test
validation.data <- res$validate

# Logistic regression
memory.limit(860000000)

nullmodel <- glm(projectsuccess~1, data = train.data, family = "binomial")

fullmodel <- glm(projectsuccess~category+	goal  + num.projects +
staff_pick+	deadline_year +	deadline_month +	deadline_day + durationdays +	
country.reduced +	language+	 launched_year +	launched_month + launched_day + SubMatch
, data = train.data, family = binomial())

summary(fullmodel)

write.csv(summary(fullmodel)['coefficients'],file=paste(ResultsDirectory,"FullLogisticRegressionFiltered.csv", sep="" ,collapse=NULL))
summary(fullmodel)

# Model selection

step.model1 <- MASS::stepAIC(fullmodel, direction = "both", trace = FALSE)


# Reduced model, using projectsuccess

ProspectProjects <- read.csv(paste(SourceDirectory,"kickstarter_prospect.csv", sep="" ,collapse=NULL), stringsAsFactors = T)

ProspectProjects$ID <- as.character(rownames(ProspectProjects))
ProspectProjects$name <- as.character(ProspectProjects$name)
ProspectProjects$deadline <- as.Date(ProspectProjects$deadline)
ProspectProjects$launched <- as.Date(ProspectProjects$launched_at)

# reformat project success
ProspectProjects$projectsuccess <- as.character(ProspectProjects$projectsuccess )
ProspectProjects$projectsuccess <- as.factor(ProspectProjects$projectsuccess )

ProspectProjects <- ProspectProjects %>% 
  separate(col = "deadline", into = c("deadline_year", "deadline_month", "deadline_day"), sep = "-") %>%
  separate(col = "launched", into = c("launched_year", "launched_month", "launched_day"), sep = "-")

ProspectProjects$launched_day <- as.numeric(ProspectProjects$launched_day)
ProspectProjects$deadline_day <- as.numeric(ProspectProjects$deadline_day)


ProspectProjects$tmp.var <- 1
num.proj <- aggregate(tmp.var ~ country, ProspectProjects, sum)
num.proj$country.reduced <- ifelse(num.proj$tmp.var < 100, "<100", as.character(num.proj$country))
colnames(num.proj) <- c("country", "num.projects", "country.reduced")
ProspectProjects <- ProspectProjects[,1:35]
ProspectProjects <- merge(ProspectProjects, num.proj, by="country")
ProspectProjects$country.reduced <- as.factor(ProspectProjects$country.reduced)

ProspectProjects$SubMatch <-  ifelse(as.character(ProspectProjects$category)==as.character(ProspectProjects$subcategory), "Matched", "NoMatch") 


rf.pred.val <- predict(rf.model2, ProspectProjects, type = "class")

# LOG SCALE GRAPHS 

goal.log.mean = aggregate(log(goal) ~ projectsuccess, ks.proj, mean)
colnames(goal.log.mean) <- c("projectsuccess", "log.mean")
goal.log.mean$exp.mean <- exp(goal.log.mean$log.mean)
goal.log.mean$label <- paste0("$", round(goal.log.mean$exp.mean,0))
png(paste(ChartsDirectory,"skewedbarlogTransformation.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(ks.proj, aes(x = projectsuccess, y = log(goal), fill = projectsuccess)) +
  stat_boxplot(outlier.shape = 1) +
  geom_text(data=goal.log.mean, aes(label=label, x=projectsuccess, y = log.mean),
            size=6, nudge_x=.45) +
  theme(legend.position = "bottom") +
  ylab("Goal in USD (log-transformed)") + xlab("") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  ggtitle("Goal of the KS projects (Log)")+
  labs(caption="The mean of the log transformed value is represented in the anti-log scale, which is the geometric mean.")
dev.off()

ks.proj$pledged <-  ifelse(as.character(ks.proj$pledged)==as.character("0"), ".000001", ks.proj$pledged) 
pledged.log.mean = aggregate(log(pledged) ~ projectsuccess, ks.proj, mean)
colnames(pledged.log.mean) <- c("projectsuccess", "log.mean")
pledged.log.mean$exp.mean <- exp(pledged.log.mean$log.mean)
pledged.log.mean$label <- paste0("$", round(pledged.log.mean$exp.mean,0))
png(paste(ChartsDirectory,"skewedbarlogpledgedTransformation.png", sep="" ,collapse=NULL), width = 1500, height = 700)
ggplot(ks.proj, aes(x = projectsuccess, y = log(pledged), fill = projectsuccess)) +
  stat_boxplot(outlier.shape = 1) +
  geom_text(data=pledged.log.mean, aes(label=label, x=projectsuccess, y = log.mean),
            size=6, nudge_x=.45) +
  theme(legend.position = "bottom") +
  ylab("Pledged in USD (log-transformed)") + xlab("") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  ggtitle("Goal of the KS projects (Log)")+
  labs(caption="The mean of the log transformed value is represented in the anti-log scale, which is the geometric mean.")
dev.off()



