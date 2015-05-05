setwd("/Users/Mengfan/Softnews")

################################################
# take random samples
nyt100k <- read.csv("nyt100k-rand.csv")

################################################
# create soft news vector
# credits to Gaurav Sood
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##
##      NYT Soft Train   	 
##		Last Edited: 2.26.15  	         
##   	Gaurav Sood								 
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
# News Desk
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# inconsistencies
nyt100k$News.Desk[nyt100k$News.Desk %in% c('Metropolitan desk', 'Metropolitian desk')]  <- 'Metropolitan Desk'	

##recode one factor due to change in mid 1990s
nyt100k$News.Desk[nyt100k$News.Desk == 'Business/Financial Desk'] <- 'Financial Desk'

## News Desk Based Classification
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nyt100k$categories <- NA

# Arts
arts 	<- c("Art and Leisure Desk", "Museums", "Arts Almanac Supplement", "Cultural Desk - SummerTimes Supplement", "Cultural",
           "Arts", "The Arts", "Arts/Cultural Desk", "Cultural Desk", "Arts and Leisure Desk", "Arts & Leisure", "Cultural Desk;", 
           "Arts & Liesure Desk", "The Art/Cultural Desk", "Cultural/Arts Desk", "The Arts/Cultural Desk", "Tha Arts/Cultural Desk",
           "<br>The Arts/Cultural Desk", "The Arts\\Cultural Desk", "Summer Arts Supplement", "Arts & Ideas/Cultural Desk",
           "Art & Ideas/Cultural Desk", "The Arts/Cultrual Desk", "Cultural desk")

style 	<- c("Style", "Style Desk", "Stlye Desk", "Styles of the Times Desk", "Men's Fashions of The Times Magazine", 
            "Style Desk;", "Style of The Times", "Styles of The Times Desk", "Styles of The TimesStyles of The Times", "Styles of the Times",
            "Men's Fashions of The Times", "Men's Fashions of The TimesMagazine", "Men's Fashion of The Times Magazine", 
            "Men's Fashion of the Times Magazine", "T: Women's Fashion Magazine", "T: Men's Fashion Magazine", "Thursday Styles Desk",
            "Style and Entertaining Magazine","Men's Fashions of the Times Magazine", "Fashions of the Times Magazine", "Fashions of The Times Magazine",
            "Living DeskStyle Desk", "House & Home/Style Desk", "Styles of The Times", "Thursday Styles", "Style and Entertaining MagazineStyle and Enter") 

books	<- c("Book Review Desk", "Book Review Dest", "Book Reviw Desk")

travel  <- c("TravelDesk", "Travel Desk", "Sophisticated Traveler Magazine", "T: Travel Magazine", "Travel DeskTravel Desk", "Escapes")

local 	<- c("Metropitan Desk", "Metroploitan Desk", "Metropoliton Desk", "Metropolitan", "Metropolitan Desk;", "Metrpolitan Desk", "Metropolian Desk", "Metropoltan Desk", 
            "Metropolitan Desak", "Metropolitan Deski", "Metroplitan Desk", "qMetropolitan Desk", "Metrolpolitian Desk", "Metropolitain Desk", "Metroploitian Desk", "Metropolitan Desk", 
            "Connecticut Weekly Desk", "New Jersey Desk", "New Jersey Weekly Desk", "Westchester Weekly Desk", "Westchester Weekly Deask", 
            "Long Island Weekly Desk", "Long Island Waekly Desk", "Long Island Weekly", "Long Island Desk", "New Jersey Weely Desk",
            "The City Weekly Desk", "The City Weekly", "The City" , "The City Weekly Section", "The City Desk", "City Weekly Desk", 
            "The City Weekly/Queens", "The City Weelky Desk", "TheCity Weekly Desk", "The City/Weekly Desk", "New Jersey/Weekly Desk",
            "The City Weeky Desk", "Connecticut Desk", "Metropoliatn Desk", "Metropoltian Desk", "Metro Desk","The City Weekly Deslk",
            "The City Weekly Desk\n The City Weekly Desk", "The City Weekl Desk", "Metropolitan DeskMetropolitan Desk", "Connecticut Weekly desk", 
            "New Jerey Weekly Desk", "Metropolitan Desk Section D", "Metropolitian Desk", "Metropolitan Dsk", "Metropolitan Deskreign Desk")

sports 	<- c("Sport Desk", "Sports", "Sports Desk", "Sports Deks", "Adventure Sports", "Sports DEsk", "Sports DeskSports Desk", 
             "Sports Deskk")

gensoft <- c("Holiday Times Supplement", "Spring Times Supplement" , "Summer Times Supplement", "Summer Times Supplementa", "Autumn Times Supplement",
             "Winter Times Supplement", "Home Entertaining Magazine",  "Television Desk", "Social Desk", "House & Home/Style", "Style and Entertaining Magaziner",
             "House & Home\\Style Desk","Wireless Living","T: Design Magazine", "T: Living Magazine", "Televison Desk", "T: Beauty",
             "THoliday","Play","Play Magazine", "Home Design Desk", "Home Design Magazine", "Entertaining Magazine", "Televison", 
             "Society Dek", "Springs Times Supplement",  "Television", "Living Desk;", "Society DeskMetropolitan Desk", "Technology", 
             "Business Travel", "The New Season Magazine", "House and Home Style", "The Marathon", "Society Desk" )

realest <- c("Real Estate", "Real Estate Desk", "Commercial Real Estate Report", "Residential Real Estate Report", "Real Estate Desk", "Real Estate desk",
             "Real Estate desk", "Real Estate", "Commercial Real Estate Report", "Residential Real Estate Report", "Real Estate desk",
             "Commercial Real Estate Report", "Residential Real Estate Report", "Real Estate Desk")

persfin	<- c("Careers Supplement Desk", "Careers Supplement" , "Financial Planning Guide: Personal Investing", "Financial Planning Guide: Personal", 
             "Workplace", "Retirement", "Financial Planning Guide: Your Taxes", "Job Market Desk", "Job Market", "Your Taxes Supplement", 
             "Personal Investing Supplement Desk")


bizfin		<- c("Financial Desk", "Financial", "Business Desk", "Business/Finance Desk", "Business/Financial Desk;", "Financial/Business Desk", "Business/Foreign Desk", "Money and Business/Financial Desk", "Money & Business/Financial Desk",
             "Money & Business/Financial Desk","Monet and Business/Financial Desk", "Money and Busines/Financial Desk", "Money andBusiness/Financial Desk",
             "Money and Business/FinancialDesk", "Business/FinancialDesk", "Business/Finacial Desk", "Business/Financial Desk Section D", 
             "Money and Business/Financial", "Money & Businees Desk", "Business Day/Financial", "Money and Business/Financial DeskMoneyand Bus",
             "Business\\Financial Desk", "Business/Finanical Desk", "Financial Desk;", "Money and Business/Financial DeskMoney and Bus",
             "Business/Financial desk", "DealBook", "Moneyand Business/Financial Desk", "SundayBusines", "SundayBusinessSundayBusiness", "SundayBusiness",
             "Business/Financial", "Business/Financial DeskBusiness/Financial Desk", "Money and Business/Fiancial Desk", "Small Business", 
             "Business World Magazine", "Sunday Business", "BIZ", "The Business of Green", "The Business of Health", "Retail")

cars 		<- c("Autmobiles", "Automobile Show Desk", "Automobies", "Automoiles", "AuTomobiles", "Automobile Desk", "Cars", 
            "Automobliles", "Automoblies", "Autombiles", "Automobile", "Automobiles Desk", "Automobles", "Automobiles")

leisure 	<- c( "Arts CultureStyle Leisure", "Weekend Desk", "Weekend Desk", "Leisure/Weekend Desk", "Weekend DeskWeekend Desk", "Weekend Desk;", "Vacation", 
               "Arts and Leisure Desk Desk", "Movies, Performing Arts/Weekend Desk", "Movies,Performing Arts/Weekend Desk", "Summer Movies",
               "Movies, Performing Arts/Weekend DeskMovies, Pe","Business Financial Desk", "Arts and Leisure")

health 		<- c("Good Health Magazine", "The Good Health Magazine", "Health and Fitness", "Health&Fitness", "Women's HealthWomen's Health", "Women's Health", "Health & Fitness", 
              "Health & Fitness Desk" , "Men's Health", "Men & Health", "PersonalHealth", "Health")

fnews		<- c("Foreign desk", "Foriegn Desk", "Foreign DEsk", "Foreignl Desk", "1;             Foreign Desk", "Foreign Desk")

natdesk 	<- c("National Desk", "National Edition - Final", "National Deskl", "Natioanl Desk", "National Dsek", "National News", "National", 
              "National desk", "National DeskNational Desk", "National Desk;"  )

living 		<- c("Living Desk  Section C","Living Desk", "Living DeskLiving Desk")

classifieds <- c("Classified", "Classified Desk", "Classifed", "Classifieds", "Classsified", "Classfied","classified" )

dining		<- c("Dining In, Dining Out", "Dining in, Dining out/Style Desk" , "Dining In/Dining Out" , "Dining In/Dining Out/Living Desk", 
             "Dining In, Dining Out/Style Desk", "Dining In, Dining Out/Cultural Desk", "Dining, Dining Out/Cultural Desk", 
             "Dining In, Dining Out/Style DeskDining In, Din"  )

misc 		<- c("Survey of Education Desk", "Summer Survey of Education", "Op-Ed at 20 Supplement", "World of New York Magazine", 
            "Education Life SupplementMetropolitan Desk" ,  "Education Life", "Education Life Supplement", "Education Life Supple", 
            "Magazine DeskMetropolitan Desk", "Citcuits", "Circuits", "Circuits Desk","CircuitsCircuits", 
            "Education Life SupplementEducation Life Supple", "E-Commerce", "Entrepreneurs", "The Millennium", "The Millenium", 
            "Generations", "Flight", "Week" , "Metro", "Working", "Giving",  "The Year in Pictures", "The Year In Pictures" , 
            "2005: The Year In Pictures", "ContinuousNews", "Voter Guide 2004" )  

home 	<- c("Home Desk", "Home DeskHome Desk", "Home Desk;", "Home Desk;", "Home Desk", "Home DeskHome Desk")                                   
wkinrev <- c("Week in Review desk", "Week in Review Desk", "Week In Review", "Week in Review", "Week in Review Desk" , "Week in Review desk", "Weekin Review Desk", 
             "Week In Review Desk", "Week in review desk", "Week in Review Deskn", "Week In Review DeskWeek In Review Desk")
edit 	<- c("Editorial desk", "Editoral Desk", "Editorial Desk", "Editorial Desk")
science <- c("Science Desk", "Science","Science Desk;", "The Natural World", "Science Desk")
obits 	<- c("Obituary",  "Obits", "Obituary")
mgz 	<- c("Magazine Desk", "New York, New York Magazine", "Magazine", "Magazine Desk", "New York, New York Magazine")                                                                     


nyt100k$categories[nyt100k$News.Desk %in% arts]		<- "Arts"
nyt100k$categories[nyt100k$News.Desk %in% style]	<- "Style"
nyt100k$categories[nyt100k$News.Desk %in% books]	<- "Books"
nyt100k$categories[nyt100k$News.Desk %in% travel]	<- "Travel"
nyt100k$categories[nyt100k$News.Desk %in% local]	<- "Local"
nyt100k$categories[nyt100k$News.Desk %in% sports]	<- "Sports"
nyt100k$categories[nyt100k$News.Desk %in% gensoft]	<- "Gen Soft"
nyt100k$categories[nyt100k$News.Desk %in% realest]	<- "Real Estate"
nyt100k$categories[nyt100k$News.Desk %in% persfin]	<- "Personal Finance"
nyt100k$categories[nyt100k$News.Desk %in% bizfin]	<- "Business Finance"
nyt100k$categories[nyt100k$News.Desk %in% cars]		<- "Cars"

nyt100k$categories[nyt100k$News.Desk %in% leisure]	<- "Leisure"
nyt100k$categories[nyt100k$News.Desk %in% health]	<- "Health"
nyt100k$categories[nyt100k$News.Desk %in% fnews]	<- "Foreign News"
nyt100k$categories[nyt100k$News.Desk %in% natdesk]	<- "National"
nyt100k$categories[nyt100k$News.Desk %in% living]	<- "Living"
nyt100k$categories[nyt100k$News.Desk %in% classifieds]	<- "Classifieds"
nyt100k$categories[nyt100k$News.Desk %in% dining]	<- "Dining"
nyt100k$categories[nyt100k$News.Desk %in% misc]		<- "Misc"

nyt100k$categories[nyt100k$News.Desk %in% home]		<- "Home Desk"
nyt100k$categories[nyt100k$News.Desk %in% wkinrev]	<- "Week in Review"
nyt100k$categories[nyt100k$News.Desk %in% edit]		<- "Editorial"
nyt100k$categories[nyt100k$News.Desk %in% science]	<- "Science"
nyt100k$categories[nyt100k$News.Desk %in% obits]	<- "Obits"
nyt100k$categories[nyt100k$News.Desk %in% mgz]		<- "Magazine"

# Numeric categories
nyt100k$catn <- as.numeric(as.factor(nyt100k$categories))

# Online Section
# ~~~~~~~~~~~~~~~~~~~~~
nyt100k$Opinion 	<- grepl("Opinion", 	as.character(nyt100k$Online.Section))
nyt100k$Obituaries 	<- grepl("Obituaries", 	as.character(nyt100k$Online.Section))
nyt100k$Corrections <- grepl("Corrections", as.character(nyt100k$Online.Section))
nyt100k$Classifieds	<- grepl("Classified", 	as.character(nyt100k$Online.Section))

# Get No News
# ~~~~~~~~~~~~~~~~~~~~~~
# Based on Online Sections
# On the fence: "Real Estate", "Magazine", "Technology"

# NA to Sections with more than 1 label
nyt100k$OnlineSectionr <- ifelse(grepl(";", nyt100k$Online.Section), NA, nyt100k$Online.Section)

# Match only where there is one label
nyt100k$OnlineSSoft <-  nyt100k$OnlineSectionr %in% c("Automobiles", "Arts", "Theater", "Dining and Wine", "Movies", "Style","Books", "Home and Garden", "Sports", "Travel")

# Match any of the labels; If total soft matches half or more	
matches 			<- lapply(c("Automobiles", "Arts", "Theater", "Dining and Wine", "Movies", "Style","Books", "Home and Garden", "Sports", "Travel"), grepl, nyt100k$Online.Section, fixed=TRUE)
nyt100k$OnlineMSoft 	<- rowSums(as.data.frame(matches))

# Based on News Desk
# No News/Soft News
nyt100k$NewsDeskSoft <-  nyt100k$categories %in% c("Arts", "Books", "Cars", "Dining", "Gen Soft", "Leisure", "Living", "Sports", "Style", "Travel", "Personal Finance",  "Health", "Real Estate")

##The next lines create a time variable 
##for quarter/year and day/month/year observations. 
nyt100k$Publication.Month 	<- as.numeric(nyt100k$Publication.Month)
nyt100k$quarter 				<- with(nyt100k, ifelse( Publication.Month <= 3, 1,  ifelse(Publication.Month <=6, 2, ifelse(Publication.Month <= 9, 3, 4))))

# Create dummy variable for soft news
nyt100k[,"SoftNews"] <- 0
for (i in 1:nrow(nyt100k)){
  if (nyt100k$NewsDeskSoft[i] == TRUE ){
    nyt100k$SoftNews[i] <- 1
  }
}

# save the dataset
write.csv(nyt100k,"nyt100k.csv")

##################################################
# take 500 hard news and 500 soft news
hardnews <- subset(nyt100k,NewsDeskSoft == FALSE)
softnews <- subset(nyt100k,NewsDeskSoft == TRUE)
hard_500 <- hardnews[sample(nrow(hardnews),500),]
soft_500 <- softnews[sample(nrow(softnews),500),]
nyt1k <- rbind(hard_500,soft_500)

# save the sample data set #
write.csv(nyt1k,"nyt1k.csv")

##################################################
# preprocess data (take out stop words etc. etc.) with preprocessData.py by Gaurav Sood
# Body and Lead Paragraph is preprocessed

nyt1k <- read.csv("nyt1k_LP_Body_cleaned.csv")

##################################################
# use chi-square to find top 500 bigrams and trigrams

Sys.setenv(JAVA_HOME = '/Library/Java//Home')
Sys.setenv(LD_LIBRARY_PATH = '$LD_LIBRARY_PATH:$JAVA_HOME/lib')
## install.packages('rJava', type='source')
library(rJava)
options(java.parameters = "-Xmx8000m")
library(RWeka)

soft <- subset(nyt1k,NewsDeskSoft==TRUE)
hard <- subset(nyt1k,NewsDeskSoft==FALSE)

soft_body <- soft$Body
hard_body <- hard$Body
soft_lead <- soft$Lead.Paragraph

body <- nyt1k$Body
lead <- nyt1k$Lead.Paragraph

bi <- NGramTokenizer(body, Weka_control(min = 2, max = 2))
tri <- NGramTokenizer(body, Weka_control(min = 3, max = 3))

bi_soft <- NGramTokenizer(soft_body, Weka_control(min = 2, max = 2))
tri_soft <- NGramTokenizer(soft_body, Weka_control(min = 3, max = 3))

bi_hard <- NGramTokenizer(hard_body, Weka_control(min = 2, max = 2))
tri_hard <- NGramTokenizer(hard_body, Weka_control(min = 3, max = 3))

# bigrams and trigrams with the highest frequency #

bi_soft_sorted <- sort(table(bi_soft),decreasing=T)
write.csv(bi_soft_sorted,"bi_soft_sorted.csv")
bi_soft_sorted <- read.csv("bi_soft_sorted.csv")
colnames(bi_soft_sorted) <- c("bigrams","frequency_S")

tri_soft_sorted <- sort(table(tri_soft),decreasing=T)
write.csv(tri_soft_sorted,"tri_soft_sorted.csv")
tri_soft_sorted <- read.csv("tri_soft_sorted.csv")
colnames(tri_soft_sorted) <- c("trigrams","frequency_S")

bi_hard_sorted <- sort(table(bi_hard),decreasing=T)
write.csv(bi_hard_sorted,"bi_hard_sorted.csv")
bi_hard_sorted <- read.csv("bi_hard_sorted.csv")
colnames(bi_hard_sorted) <- c("bigrams","frequency_H")

tri_hard_sorted <- sort(table(tri_hard),decreasing=T)
write.csv(tri_hard_sorted,"tri_hard_sorted.csv")
tri_hard_sorted <- read.csv("tri_hard_sorted.csv")
colnames(tri_hard_sorted) <- c("trigrams","frequency_H")

bisorted <- sort(table(bi),decreasing=T)
write.csv(bisorted,"bisorted.csv")
bisorted <- read.csv("bisorted.csv")
colnames(bisorted) <- c("bigrams","frequency")

trisorted <- sort(table(tri),decreasing=T)
write.csv(trisorted,"trisorted.csv")
trisorted <- read.csv("trisorted.csv")
colnames(trisorted) <- c("trigrams","frequency")

# calculating chi2 for each bigram and trigram #

totalfreqbi <- sum(bisorted$frequency)
totalfreqtri <- sum(trisorted$frequency)
totalfreqbi
totalfreqtri
# totalfreqbi = 338959 & totalfreqtri = 337974

# calculate chi2 for bigrams #
allbigrams <- merge(bi_soft_sorted,bi_hard_sorted,by="bigrams")

chi2bi <- matrix(0,ncol=1,nrow=nrow(allbigrams))
  
allbigrams <- cbind(allbigrams,chi2bi)

colnames(allbigrams) <- c("bigrams","frequency_S","frequency_H","chi2")

for (i in 1:nrow(allbigrams)) {
  bifs <- as.double(allbigrams[i,2])
  bifh <- as.double(allbigrams[i,3])
  bifsNOT <- as.double(totalfreqbi-bifs)
  bifhNOT <- as.double(totalfreqbi-bifh)
  numerator = (bifs*bifhNOT-bifh*bifsNOT)^2
  denominator = (bifs+bifh)*(bifs+bifsNOT)*(bifh+bifhNOT)*(bifsNOT+bifhNOT)
  allbigrams[i,4] <- numerator/denominator
}

allbigrams <- allbigrams[order(allbigrams$chi2,decreasing=TRUE),]

write.csv(allbigrams,"allbigrams.csv")

# calculate chi2 for trigrams #

alltrigrams <- merge(tri_soft_sorted,tri_hard_sorted,by="trigrams")

chi2tri <- matrix(0,ncol=1,nrow=nrow(alltrigrams))

alltrigrams <- cbind(alltrigrams,chi2tri)

colnames(alltrigrams) <- c("trigrams","frequency_S","frequency_H","chi2")

for (i in 1:nrow(alltrigrams)) {
  trifs <- as.double(alltrigrams[i,2])
  trifh <- as.double(alltrigrams[i,3])
  trifsNOT <- as.double(totalfreqtri-trifs)
  trifhNOT <- as.double(totalfreqtri-trifh)
  numerator = (trifs*trifhNOT-trifh*trifsNOT)^2
  denominator = (trifs+trifh)*(trifs+trifsNOT)*(trifh+trifhNOT)*(trifsNOT+trifhNOT)
  alltrigrams[i,4] <- numerator/denominator
}

alltrigrams <- alltrigrams[order(alltrigrams$chi2,decreasing=TRUE),]

write.csv(alltrigrams,"alltrigrams.csv")

# take out the bigrams and trigrams that do not make sense #
bigram_drops <- c('also said','two year','mr mr','first time','year mr','said yesterday','next sunday','staten island'
           ,'year earlier','york time','would use','like mr','said statement','four year',
           'said dr','last week','year ago','last month','told report',
           'percent percent','three year','would like','said interview','year old','even though','said one',
           'next week','ca nt','said ms','ask mr','said ms','told mr','son mr','state mr','tuesday pm',
           'six month','day mr','said mani','new hampshire',
           'said could','ask mr','lower manhattan','million american','recent months','said expect',
           'show mr','york state','m sure','mr thompson','said believ','said also','next month','said peopl','five year','said told',
           'time squar','north carolina','like mani','said plan','said time','said think','two day','sever year','believ mr','know mr',
           'let alon','said two','today would','time mr','could go','said recent','said said','said still','recent year','one anoth',
           'also found','four month','jersey citi','one time','say would','week ago','new york','fifth avenu',
           'week last')
cleaned_bigrams <- subset(allbigrams,(allbigrams[,1] %in% drops)==FALSE)

trigram_drops <- c('new york time','new york ny','said ca nt','two year ago','said nt want','last four year',
                   'said last week','four year ago','new york mr','last three year','last two month','said d like',
                   'said nt know','sever year ago')
cleaned_trigrams <- subset(alltrigrams,(alltrigrams[,1] %in% drops)==FALSE)

# keep the top 500 with the highest chi-square #

nyttopbigrams <- head(cleaned_bigrams,500)
nyttoptrigrams <- head(cleaned_trigrams,500)

write.csv(nyttopbigrams,"nyttopbigrams.csv")
write.csv(nyttoptrigrams,"nyttoptrigrams.csv")
#################################################
# build the model #

softdummy <- nyt1k$SoftNews

# body and lead have been defined

predictsoft <- softdummy

# create new columns for bigrams and name the columns #
zeromatrix <- matrix(0,nrow=length(predictsoft),ncol=500)
predictsoft <- cbind(predictsoft,zeromatrix)
bigramdummy <- t(matrix(nyttopbigrams[,1]))


for (i in 1:500) {
  if (i==1) { names <- c(toString(bigramdummy[i])) }
  else { names <- c(names,toString(bigramdummy[i])) }
}

colnames(predictsoft) <- c("softdummy",names)

# create new columns for trigrams and name the columns #

predictsoft <- cbind(predictsoft,zeromatrix)
trigramdummy <- t(matrix(nyttoptrigrams[,1]))


for (i in 1:500) {
  if (i==1) { trinames <- c(toString(trigramdummy[i])) }
  else { trinames <- c(trinames,toString(trigramdummy[i])) }
}

colnames(predictsoft) <- c("softdummy",names,trinames)

# check whether the speaker has said those bigrams #

for (i in 1:nrow(predictsoft)) {
  for (j in 2:501) { 
    if ((grepl(names[j-1],toString(body[i]),ignore.case = TRUE, perl = FALSE,fixed = FALSE, useBytes = FALSE) == TRUE) 
        || (grepl(names[j-1],toString(lead[i]),ignore.case = TRUE, perl = FALSE,fixed = FALSE, useBytes = FALSE) == TRUE))
      
    { predictsoft[i,j] <- 1}
    else 
    { predictsoft[i,j] <- 0 }
  }
  
}

# check whether the speaker has said those trigrams #

for (i in 1:nrow(predictsoft)) {
  for (j in 502:1001) { 
    if ((grepl(trinames[j-501],toString(body[i]),ignore.case = TRUE, perl = FALSE,fixed = FALSE, useBytes = FALSE) == TRUE) 
        || (grepl(names[j-501],toString(lead[i]),ignore.case = TRUE, perl = FALSE,fixed = FALSE, useBytes = FALSE) == TRUE))
      
    { predictsoft[i,j] <- 1}
    else 
    { predictsoft[i,j] <- 0 }
  }
  
}

# ignore the missing values #

predictsoft <- na.omit(predictsoft)


write.csv(predictsoft,"predictsoft.csv")

#######################################################################################################
# SVM #
# install.packages("e1071")

set.seed(1)

# take a training data set #
train <- sample(1000,500)
data_train <- data.frame(predictsoft[train,])
x=model.matrix(softdummy~.,data=data_train)[,-1]
y=data_train$softdummy

dat = data.frame(x=x, y=as.factor(y))
library(e1071)
svmfit <- svm(y~.,data=dat,method = "C-classification", kernel = "linear", cost = 10, scale=FALSE)

summary(svmfit)

set.seed(1)
tune.out=tune(svm,y~.,data=dat,kernel = "linear", ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10)),scale=FALSE)

summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)


# test on test data set #
data_test <- data.frame(predictsoft[-train,])

x_test=model.matrix(data_test$softdummy~.,data=data_test)[,-1]

y_test=data_test$softdummy

dat_test = data.frame(x=x_test, y=as.factor(y_test))

ypred=predict(bestmod, dat_test)

table(predict=ypred, truth=y_test)


############################################################ 
# svm with radial kernel
set.seed(1)
svmfitrad = svm(y~., data=dat,kernel = "radial", gamma = 1, cost = 1, scale=FALSE)

summary(svmfitrad)

# cross validation with kernel radial
set.seed(1)
tune.out_rad=tune(svm, y~., data=dat, kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)),scale=FALSE)
summary(tune.out_rad)
bestmod_rad=tune.out_rad$best.model
summary(bestmod_rad)

# prediction with radial
ypred_rad=predict(bestmod_rad, dat_test)

table(predict=ypred_rad, truth=y_test)


