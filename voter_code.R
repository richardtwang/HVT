#### package to read excel file
install.packages("readxl")
library(readxl)

#### source data: http://dl.ncsbe.gov/index.html?prefix=data/

filecnt <- list.files(pattern="*.xlsx") ## count all the excel files in the folder so make sure all the files are closed and you don't have other excel files in the same folder
for (i in 1:length(filecnt)) {

	#if the merged dataset exists, append to it
	if(exists("myds")){
		tempds<-read_excel(filecnt[i])
		tempds$cnt<-i
		tempds$fnm<-filecnt[i]
		myds<-rbind(myds, tempds)  # append data
		rm(tempds)}

	#if the merged dataset doesn't exist, create it
    	else {myds<-read_excel(filecnt[i])
	      myds$cnt<-i
	      myds$fnm<-filecnt[i]} # this case is only true for loading the first file

}

#### save your dataset permanently
save(myds, file="C:/path/myds.Rdata")

#### cleaning data
myds$'2016 General'[is.na(myds$'2016 General')] <- 0 ### replace missing target variable with 0
D<-na.omit(myds) ### drop missing value
D$'2016 General'[D$'2016 General'==2]<-1 ### replace target variable having value 2 with 1
names(D)[2]<-paste("tarvar") ### rename target variable
names(D) <- gsub(" ", "_", names(D))  ### replace the spaces with underscore in the fields names
D<-D[which(D$Total_Vote!=0),] ### drop total_vote of 0
D$mtpct<-D$Midterm_Vote/D$Total_Vote*100  ### midterm as a percentage of total vote

#### zip and 2015 census data
zipds <- read.table("NCVoter_zipcode.txt", header=TRUE, sep="\t")
censusds<- read.table("C:/path/Voter/Census2015.txt", header=TRUE, sep="\t")

install.packages("sqldf")
library(sqldf)
D<-D[c(1:7,9,12)]
DD<-sqldf("SELECT D.*, zipds.ethnic_code, censusds.* FROM D, join zipds on D.Voter=zipds.ncid join censusds on zipds.zip_code=censusds.zip")

fullds<-sqldf(
	"
	select tarvar, 
		Race, 
		Party, 
		Gender, 
		Age_Group, 
		ethnic_code as Ethnicity, 
		case when total_vote in (1,2,3) then total_vote
		     when total_vote in (4,5) then 4 else 5 end as Total_Vote,
		Preceding_Presidential,
		case when mtpct>25 then 1 else 0 end as Midterm_Pct_over25,
		avg_under40*100 as Pct_Under40,
		avg_under60*100 as Pct_Under60,
		avg_above60*100 as Pct_Above60,
		avg_hisp*100 as Pct_Hisp,
		avg_white*100 as Pct_White,
		avg_black*100 as Pct_Black,
		avg_asian*100 as Pct_Asian,
		avg_oop*100 as Pct_Owner,
		case when avg_hinc < 55000 then 1 else 0 end as House_Income_Below55K,
		avg_hsize as Avg_House_Size,
		avg_white_collar*100 as Pct_White_Collar
        from DD
	")

str(fullds)

#### to run the model in caret, fields have to be either factor or numeric (can't have char or int); 
D<-data.frame(lapply(fullds[,c(7:9, 18)], as.numeric), fullds[,c(1:6, 10:17, 19:20)]) # turn int to numeric
D$tarvar<-as.factor(D$tarvar)
levels(D$tarvar) <- c("N", "Y")
levels(D$Age_Group) <- make.names(levels(D$Age_Group), unique = TRUE) # fix some of the names

set.seed(2969)
train <- createDataPartition(y=D$tarvar, p=.4, list=FALSE) ## sample 40% (set.seed(2969) before sampling or save trainds to reproduce the same model results)
trnds <- D[ train,]
tstds <- D[-train,]
### change the reference level for factor variables
trnds<-within(trnds, Race<-relevel(Race, ref="W"))
trnds<-within(trnds, Party<-relevel(Party, ref="DEM"))
trnds<-within(trnds, Gender<-relevel(Gender, ref="M"))
trnds<-within(trnds, Ethnicity<-relevel(Ethnicity, ref="NL"))
save(trnds, file="trainds_mod.Rdata")
save(tstds, file="testds_mod.Rdata")

#### caret
ctrl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary )
m <- train(tarvar ~ ., data=trnds, method="glm", family="binomial", trControl=ctrl, metric="ROC")
### output variance and covariance
outvcv<-vcov(m$finalModel)
head(outvcv[1:4, 1:4]) ### copy the variance/covariance to the excel

#### glm (a lot faster than caret)
mm<-glm(tarvar ~ ., data=trnds, family="binomial")
### output variance and covariance
outcv<-vcov(mm)
head(outvcv[1:4, 1:4]) 

#### stargazer output (stargazer works with glm but not caret)
library(stargazer)
stargazer(mm, ci = T, single.row = T, type = "text",style="all")
stargazer(mm, single.row = T, type = "text", style="default")

stargazer(mm, type = "text", 
          title            = "Logistic Regression Output",
          dep.var.labels   = "2016 Presidential Election",
	  style="default")



p <- predict(m, newdata = tstds)

### confusion matrix
outcm<-confusionMatrix(p, testset$tarvar)
capture.output( print(outcm, print.gap=3), file="confusion_matrix.txt")

### coefficients
outcoef<-summary(m)$coefficients
capture.output(print(outcoef, print.gap=4), file="model_coef.txt")

### variable ranking
outv <- varImp(m, scale = FALSE)
capture.output( print(outv, print.gap=3), file="var_rank.txt")

### model results 
summary(m) ### copy and paste to excel


### check correlation between census variables
cor(censusds[, c(3:9, 11:15, 17:19)])
