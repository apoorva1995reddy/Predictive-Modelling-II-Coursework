Exercise2
================
18 August 2018

Problem1
--------

Loading and cleaning data

``` r
ABIA=read.csv("../data/ABIA.csv")
ABIA$ArrDelay[is.na(ABIA$ArrDelay)] = 0
codes=read.csv("../data/airport-codes.csv")
codes1=codes[c('name','coordinates','iata_code')]
codes1=na.omit(codes1)
codes1=codes1[!duplicated(codes1[c('iata_code')]),]
ABIA$Week<- ABIA$DayOfWeek
ABIA$Week[ABIA$DayOfWeek == 1] <- 'Monday'
ABIA$Week[ABIA$DayOfWeek == 2] <- 'Tuesday'
ABIA$Week[ABIA$DayOfWeek == 3] <- 'Wednesday'
ABIA$Week[ABIA$DayOfWeek == 4] <- 'Thursday'
ABIA$Week[ABIA$DayOfWeek == 5] <- 'Friday'
ABIA$Week[ABIA$DayOfWeek == 6] <- 'Saturday'
ABIA$Week[ABIA$DayOfWeek == 7] <- 'Sunday'
ABIA$MonthQual<-ABIA$Month
ABIA$MonthQual[ABIA$MonthQual == 1] <- 'January'
ABIA$MonthQual[ABIA$MonthQual == 2] <- 'February'
ABIA$MonthQual[ABIA$MonthQual == 3] <- 'March'
ABIA$MonthQual[ABIA$MonthQual == 4] <- 'April'
ABIA$MonthQual[ABIA$MonthQual == 5] <- 'May'
ABIA$MonthQual[ABIA$MonthQual == 6] <- 'June'
ABIA$MonthQual[ABIA$MonthQual == 7] <-'July'
ABIA$MonthQual[ABIA$MonthQual == 8] <- 'August'
ABIA$MonthQual[ABIA$MonthQual == 9] <- 'September'
ABIA$MonthQual[ABIA$MonthQual == 10] <- 'October'
ABIA$MonthQual[ABIA$MonthQual == 11] <- 'November'
ABIA$MonthQual[ABIA$MonthQual == 12] <- 'December'

ABIA$CancellationCode1="No cancellation"
ABIA$CancellationCode1[ABIA$CancellationCode =='A']="Carrier"
ABIA$CancellationCode1[ABIA$CancellationCode == 'B'] = 'Weather'
ABIA$CancellationCode1[ABIA$CancellationCode == 'C'] = 'NAS'
ABIA$CancellationCode1[ABIA$CancellationCode == 'D'] = 'Security'

ABIA=merge(ABIA, codes1, by.x = "Dest", by.y = "iata_code")

ABIA$Dephr=substr(ABIA$DepTime,1,nchar(ABIA$DepTime)-2)
ABIA$Dephr[ABIA$Dephr == ''] = 0
ABIA$Dephr=as.numeric(ABIA$Dephr)

ABIA$Direction[ABIA$Dest=="AUS"]<-"To Aus"
ABIA$Direction[ABIA$Origin=="AUS"]<-"From Aus"
ABIA_notcancelled=ABIA[ABIA$Cancelled==0,]
```

``` r
library(ggplot2)
ggplot(ABIA_notcancelled, aes(Dephr)) + geom_bar(width=0.8,position="dodge",color="black")+
  labs(x = "Dephr", y = "Count",title = "#Flights to and from Austin")+
  theme(legend.text = element_text(colour="blue", size=10, 
                                   face="bold"))
```

![](Solutions_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
FlighttofromAUS=table(ABIA_notcancelled$Direction,ABIA_notcancelled$Dephr)
barplot(FlighttofromAUS, main="Successful flights",
        xlab="Dep hr", col=c("orange","blue"),beside = TRUE,legend = rownames(FlighttofromAUS))
```

![](Solutions_files/figure-markdown_github/unnamed-chunk-3-1.png)

More traffic from 6AM-7PM

``` r
DF_Q1=cbind(aggregate(ArrDelay ~ Dephr+DayOfWeek, data = ABIA_notcancelled, FUN = function(x) c(mn = mean(x))),aggregate(ArrDelay ~ Dephr+Week, data = ABIA_notcancelled, FUN = function(x) c(mn = length(x))))
DF_Q1=DF_Q1[,c(1,2,3,6)]
colnames(DF_Q1)[3] <- "Avg_Arr_Delay"
colnames(DF_Q1)[4] <- "flights"
DF_Q1=na.omit(DF_Q1)
```

``` r
library(ggplot2)
require(viridis)
```

    ## Loading required package: viridis

    ## Loading required package: viridisLite

``` r
library(ggthemes)

ggplot(data = DF_Q1, aes(x=Dephr,y=as.factor(DayOfWeek))) + geom_tile(aes(fill=flights)) + scale_fill_viridis(option="magma")+xlab('Dephr')+theme_tufte(base_family="Helvetica")+ggtitle("No of Flights")+xlab("Time")
```

![](Solutions_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
ggplot(data = DF_Q1, aes(x = Dephr ,y = as.factor(DayOfWeek))) + 
  geom_tile(aes(fill = Avg_Arr_Delay)) + 
  coord_equal(ratio = 1)+ scale_fill_viridis(option="magma") + theme_tufte(base_family="Helvetica")+ggtitle("Avg delay in minutes")+xlab("Departure time")
```

![](Solutions_files/figure-markdown_github/unnamed-chunk-6-1.png)

Though there are very few flights during early mornings, avg delays tend to be longer for these flights

``` r
ABIA$CSRDephr_Correct = substr(ABIA$CRSDepTime,1,nchar(ABIA$CRSDepTime)-2)
ABIA$CSRDephr_Correct=as.numeric(ABIA$CSRDephr_Correct)
ABIA_cancelled=ABIA[ABIA$Cancelled==1,]
DF_Q1_1=aggregate(Year ~ CSRDephr_Correct+DayOfWeek, data = ABIA_cancelled, FUN = function(x) c(mn = length(x)))
colnames(DF_Q1_1)[3] <- "Cancellations"
ggplot(data = DF_Q1_1, aes(x=CSRDephr_Correct,y=as.factor(DayOfWeek))) + geom_tile(aes(fill=Cancellations))+ scale_fill_viridis(option="magma")+xlab('CSRDephr_Correct')+theme_tufte(base_family="Helvetica")+ggtitle("Cancelled flights")+xlab("Time")
```

![](Solutions_files/figure-markdown_github/unnamed-chunk-7-1.png)

More cancellations on weekdays, specially tuesdays

``` r
DF_Q2=cbind(aggregate(ArrDelay ~ as.factor(Month), data = ABIA_notcancelled, FUN = function(x) c(mn = mean(x))),aggregate(ArrDelay ~ MonthQual, data = ABIA_notcancelled, FUN = function(x) c(mn = length(x))))
DF_Q2=DF_Q2[,c(1,2,4)]
colnames(DF_Q2)[2] <- "Avg_Arr_Delay"
colnames(DF_Q2)[3] <- "flights"
require(ggplot2)
ggplot(data = ABIA_notcancelled, aes(x=as.factor(Month), y=ArrDelay)) + geom_boxplot()+scale_y_continuous(limits=c(0,200), breaks=seq(0,200,20), expand = c(0, 0))
```

![](Solutions_files/figure-markdown_github/unnamed-chunk-8-1.png) There are many outliers, more spread and the median arrival delays are almost same across all the months.

``` r
MoM_Cancellations= table(ABIA_cancelled$CancellationCode1,ABIA_cancelled$Month)
barplot(MoM_Cancellations, main="Cancelled flights",
        xlab="Month", col=c("orange","red","blue"),
        beside = TRUE,las=2,legend = rownames(MoM_Cancellations),args.legend = list(x = "topright",bty="n"))
```

![](Solutions_files/figure-markdown_github/unnamed-chunk-9-1.png)

Majority of the flights cancelled during September month were because of bad weather.

``` r
Fromaustin=ABIA[ABIA$Origin=='AUS',]
Fromaustin$Type='Successful'
Fromaustin$Type[Fromaustin$Diverted == 1] = 'Diverted'
Fromaustin$Type[Fromaustin$Cancelled == 1] = 'Cancelled'

From_austin_successful=Fromaustin[Fromaustin$Type=='Successful',]
From_austin_successful=table(From_austin_successful$Type,From_austin_successful$Dest)
barplot(From_austin_successful, main="Successful flights",
        xlab="Destination", col=c("darkblue"),legend = rownames(From_austin_successful),beside = TRUE,srt=90,las=2)
```

![](Solutions_files/figure-markdown_github/unnamed-chunk-10-1.png)

More traffic towards DAL,DFW,IAH,ORD, PHX,ATL

``` r
From_austin_failed=Fromaustin[Fromaustin$Type!='Successful',]
From_austin_failed=table(From_austin_failed$Type,From_austin_failed$Dest)
barplot(From_austin_failed, main="Cancelled/Diverted flights",
        xlab="Destination", col=c("orange","red"),
        legend = rownames(From_austin_failed),beside = TRUE,srt=90,las=2)
```

![](Solutions_files/figure-markdown_github/unnamed-chunk-11-1.png)

DAL,DFW,ORD have higher cancellations More cancelled flights than diverted flights

``` r
Fromaustin_cancelledsplit=Fromaustin[Fromaustin$Cancelled==1,]
Fromaustin_cancelledsplit= table(Fromaustin_cancelledsplit$CancellationCode1,Fromaustin_cancelledsplit$Dest)
barplot(Fromaustin_cancelledsplit, main="Cancelled flights",
        xlab="Destination", col=c("orange","red","blue"),
        beside = TRUE,las=2,legend = rownames(Fromaustin_cancelledsplit),args.legend = list(x = "topright"))
```

![](Solutions_files/figure-markdown_github/unnamed-chunk-12-1.png)

Most of the cancellations are due to Carrier or Weather. ORD and DFW have very high cancelled flights because of NAS, delay that is within the control of the National Airspace System (NAS) may include: non-extreme weather conditions, airport operations, heavy traffic volume, air traffic control, etc

``` r
DF_Q4_1=aggregate(Year ~ name+Month, data = ABIA_cancelled[ABIA_cancelled$Origin=='AUS',], FUN = function(x) c(mn = length(x)))
colnames(DF_Q4_1)[3] <- "Monthly_Cancellations"
ggplot(data = DF_Q4_1, aes(x=as.factor(Month),y=name)) + geom_tile(aes(fill=Monthly_Cancellations))+ scale_fill_viridis(option="magma")+xlab('CSRDephr_Correct')+theme_tufte(base_family="Helvetica")+ggtitle("Cancelled flights")+xlab("Month")
```

![](Solutions_files/figure-markdown_github/unnamed-chunk-13-1.png)

DL and DFW airorts had many cancellations in the first half year, the numbers improved great deal in the later part of the year

You can see that William P Hobby aiport and George Bush International airpoprts had more cancellations in September that increased the overall cancellations due to bad weather for the month. This is because of Hurrican Ike, tropical cyclone , sixth costliest Atlantic hurraine that severely affected Texas

Problem2
--------

Author Attribution
------------------

In the C50train directory, you have ~50 articles from each of 50 different authors (one author per directory). Use this training data (and this data alone) to build the two models. Then apply your model to the articles by the same authors in the C50test directory, which is about the same size as the training set. How well do your models do at predicting the author identities in this out-of-sample setting? Are there any sets of authors whose articles seem difficult to distinguish from one another? Which model do you prefer?

#### Loading necessary libraries

``` r
library(tm) 
```

    ## Loading required package: NLP

    ## 
    ## Attaching package: 'NLP'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     annotate

``` r
library(magrittr)
library(slam)
library(proxy)
```

    ## 
    ## Attaching package: 'proxy'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     as.dist, dist

    ## The following object is masked from 'package:base':
    ## 
    ##     as.matrix

#### Reading Train and Test data from the files

``` r
readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') 
}

train_list = Sys.glob('../data/ReutersC50/C50train/*/*.txt')
train = lapply(train_list, readerPlain) 



test_list = Sys.glob('../data/ReutersC50/C50test/*/*.txt')
test = lapply(test_list, readerPlain) 

mynames = train_list %>%
{ strsplit(., '/', fixed=TRUE) } %>%
{ lapply(., tail, n=2) } %>%
{ lapply(., paste0, collapse = '') } %>%
  unlist


mynamestest = test_list %>%
{ strsplit(., '/', fixed=TRUE) } %>%
{ lapply(., tail, n=2) } %>%
{ lapply(., paste0, collapse = '') } %>%
  unlist

names(train) = mynames
names(test) = mynamestest
```

#### Extracting the Author names for Train and Test data

``` r
authors = train_list %>%
{ strsplit(., '/', fixed=TRUE) } %>%
{ lapply(., function(x) x[5]) } %>%
{ lapply(., paste0, collapse = '') } %>%
  unlist

authors_test = test_list %>%
{ strsplit(., '/', fixed=TRUE) } %>%
{ lapply(., function(x) x[5]) } %>%
{ lapply(., paste0, collapse = '') } %>%
  unlist
```

#### Generating the Corpus for train and test data

``` r
documents_raw = Corpus(VectorSource(train))
documents_raw_test = Corpus(VectorSource(test))
```

#### Tokenization

``` r
my_documents = documents_raw
my_documents = tm_map(my_documents, content_transformer(tolower)) # make everything lowercase
my_documents = tm_map(my_documents, content_transformer(removeNumbers)) # remove numbers
my_documents = tm_map(my_documents, content_transformer(removePunctuation)) # remove punctuation
my_documents = tm_map(my_documents, content_transformer(stripWhitespace)) ## remove excess white-space

my_documents_test = documents_raw_test
my_documents_test = tm_map(my_documents_test, content_transformer(tolower)) # make everything lowercase
my_documents_test = tm_map(my_documents_test, content_transformer(removeNumbers)) # remove numbers
my_documents_test = tm_map(my_documents_test, content_transformer(removePunctuation)) # remove punctuation
my_documents_test = tm_map(my_documents_test, content_transformer(stripWhitespace)) ## remove excess white-space
```

#### Removing Stopwords

``` r
my_documents = tm_map(my_documents, content_transformer(removeWords), stopwords("en"))
my_documents_test = tm_map(my_documents_test, content_transformer(removeWords), stopwords("en"))
```

#### Creating Doc-term-matrix

Here we have created 2 set of train and test datasets The first set will involve dropping the uncommon words between the train and test dataset The second will involve including to test matrix the terms not present in test but in train as a dummy column with 0 assigned as the value

``` r
DTM_train = DocumentTermMatrix(my_documents)
DTM_test = DocumentTermMatrix(my_documents_test)

#Removing Sparse terms 
DTM_train = removeSparseTerms(DTM_train, 0.95)
DTM_test = removeSparseTerms(DTM_test, 0.95)

#Not removing Sparse terms in case of the second train-test set
DTM_train2 = DTM_train
DTM_test2 <- DocumentTermMatrix(my_documents_test, control = list(dictionary=Terms(DTM_train2)))
```

#### Constructing TF-IDF weights for both the above sets discussed

``` r
tfidf_train = weightTfIdf(DTM_train)
tfidf_test = weightTfIdf(DTM_test)

tfidf_test2 = weightTfIdf(DTM_test2)
tfidf_train2 = weightTfIdf(DTM_train2)
```

#### Dimensionlity Reduction: Principal Component Analysis

``` r
X = as.matrix(tfidf_train)
X_test = as.matrix(tfidf_test)

X2 = as.matrix(tfidf_train2)
X_test2 = as.matrix(tfidf_test2)

#Removing columns with entries with 0 values
scrub_cols = which(colSums(X) == 0)
scrub_cols_test = which(colSums(X_test) == 0)

scrub_cols2 = which(colSums(X2) == 0)
scrub_cols_test2 = which(colSums(X_test2) == 0)

X = X[,-scrub_cols]
X_test = X_test[,-scrub_cols_test]

X2 = X2[,-scrub_cols]
X_test2 = X_test2[,-scrub_cols_test2]
```

##### For the first set dropping the uncommon words

``` r
X_test <- X_test[,intersect(colnames(X_test),colnames(X))]
X <- X[,intersect(colnames(X_test),colnames(X))]
```

##### Running PCA for the train sets and predicting the PCs for the test set

``` r
pca_train = prcomp(X, scale=TRUE)
pca_test=predict(pca_train,newdata = X_test )

pca_train2 = prcomp(X2,scale=TRUE)
pca_test2=predict(pca_train2,newdata = X_test2 )
```

Choosing number of PCs to be selected

``` r
plot(pca_train,type='line')
```

![](Solutions_files/figure-markdown_github/unnamed-chunk-25-1.png)

``` r
#summary(pca_train)

vars <- apply(pca_train$x, 2, var)  
props <- vars / sum(vars)
cumsum(props)
```

    ##        PC1        PC2        PC3        PC4        PC5        PC6 
    ## 0.01880642 0.03135189 0.04114958 0.05027389 0.05886699 0.06704405 
    ##        PC7        PC8        PC9       PC10       PC11       PC12 
    ## 0.07408726 0.08090368 0.08752631 0.09379907 0.09973996 0.10549257 
    ##       PC13       PC14       PC15       PC16       PC17       PC18 
    ## 0.11095862 0.11612637 0.12119208 0.12585170 0.13042295 0.13490748 
    ##       PC19       PC20       PC21       PC22       PC23       PC24 
    ## 0.13933905 0.14370433 0.14801157 0.15224337 0.15640178 0.16049597 
    ##       PC25       PC26       PC27       PC28       PC29       PC30 
    ## 0.16449932 0.16846328 0.17238432 0.17618302 0.17992554 0.18365726 
    ##       PC31       PC32       PC33       PC34       PC35       PC36 
    ## 0.18734920 0.19098462 0.19458017 0.19816650 0.20165306 0.20509510 
    ##       PC37       PC38       PC39       PC40       PC41       PC42 
    ## 0.20850221 0.21189164 0.21526229 0.21862234 0.22195549 0.22526646 
    ##       PC43       PC44       PC45       PC46       PC47       PC48 
    ## 0.22854225 0.23179473 0.23501223 0.23822716 0.24140687 0.24455059 
    ##       PC49       PC50       PC51       PC52       PC53       PC54 
    ## 0.24768091 0.25078930 0.25389026 0.25697917 0.26005401 0.26310835 
    ##       PC55       PC56       PC57       PC58       PC59       PC60 
    ## 0.26612012 0.26912051 0.27210730 0.27507792 0.27801787 0.28095189 
    ##       PC61       PC62       PC63       PC64       PC65       PC66 
    ## 0.28387033 0.28676200 0.28964149 0.29250765 0.29535524 0.29819690 
    ##       PC67       PC68       PC69       PC70       PC71       PC72 
    ## 0.30100606 0.30380390 0.30658953 0.30935764 0.31212285 0.31487592 
    ##       PC73       PC74       PC75       PC76       PC77       PC78 
    ## 0.31761566 0.32034788 0.32306759 0.32575889 0.32843514 0.33109389 
    ##       PC79       PC80       PC81       PC82       PC83       PC84 
    ## 0.33375086 0.33640274 0.33903478 0.34165393 0.34426295 0.34686717 
    ##       PC85       PC86       PC87       PC88       PC89       PC90 
    ## 0.34946609 0.35204247 0.35459458 0.35714475 0.35969126 0.36222721 
    ##       PC91       PC92       PC93       PC94       PC95       PC96 
    ## 0.36475167 0.36725549 0.36975113 0.37223756 0.37470071 0.37715650 
    ##       PC97       PC98       PC99      PC100      PC101      PC102 
    ## 0.37960359 0.38203911 0.38447328 0.38690231 0.38931021 0.39170489 
    ##      PC103      PC104      PC105      PC106      PC107      PC108 
    ## 0.39409290 0.39646782 0.39883760 0.40119989 0.40354300 0.40587564 
    ##      PC109      PC110      PC111      PC112      PC113      PC114 
    ## 0.40820656 0.41053024 0.41284397 0.41514603 0.41744074 0.41972913 
    ##      PC115      PC116      PC117      PC118      PC119      PC120 
    ## 0.42200916 0.42427570 0.42653898 0.42879301 0.43103298 0.43326640 
    ##      PC121      PC122      PC123      PC124      PC125      PC126 
    ## 0.43549184 0.43771491 0.43993573 0.44214717 0.44433767 0.44652317 
    ##      PC127      PC128      PC129      PC130      PC131      PC132 
    ## 0.44869902 0.45087186 0.45303718 0.45519685 0.45734618 0.45948178 
    ##      PC133      PC134      PC135      PC136      PC137      PC138 
    ## 0.46161531 0.46374267 0.46585937 0.46797314 0.47007303 0.47215527 
    ##      PC139      PC140      PC141      PC142      PC143      PC144 
    ## 0.47422645 0.47629357 0.47835500 0.48041040 0.48245443 0.48449698 
    ##      PC145      PC146      PC147      PC148      PC149      PC150 
    ## 0.48653237 0.48856243 0.49058743 0.49260210 0.49461222 0.49661454 
    ##      PC151      PC152      PC153      PC154      PC155      PC156 
    ## 0.49860835 0.50059820 0.50258171 0.50455530 0.50652041 0.50848140 
    ##      PC157      PC158      PC159      PC160      PC161      PC162 
    ## 0.51043008 0.51237482 0.51431370 0.51624839 0.51817832 0.52010040 
    ##      PC163      PC164      PC165      PC166      PC167      PC168 
    ## 0.52201561 0.52392843 0.52582794 0.52772252 0.52961138 0.53149364 
    ##      PC169      PC170      PC171      PC172      PC173      PC174 
    ## 0.53337132 0.53524455 0.53710920 0.53896709 0.54082015 0.54266778 
    ##      PC175      PC176      PC177      PC178      PC179      PC180 
    ## 0.54450650 0.54633546 0.54816075 0.54997941 0.55179027 0.55359582 
    ##      PC181      PC182      PC183      PC184      PC185      PC186 
    ## 0.55539578 0.55718973 0.55898349 0.56077345 0.56254854 0.56432253 
    ##      PC187      PC188      PC189      PC190      PC191      PC192 
    ## 0.56609070 0.56785450 0.56961151 0.57136002 0.57310251 0.57483905 
    ##      PC193      PC194      PC195      PC196      PC197      PC198 
    ## 0.57657295 0.57830330 0.58002270 0.58173932 0.58344771 0.58515175 
    ##      PC199      PC200      PC201      PC202      PC203      PC204 
    ## 0.58685096 0.58854534 0.59023703 0.59191627 0.59358851 0.59525810 
    ##      PC205      PC206      PC207      PC208      PC209      PC210 
    ## 0.59692498 0.59859050 0.60024393 0.60188994 0.60353495 0.60517315 
    ##      PC211      PC212      PC213      PC214      PC215      PC216 
    ## 0.60680555 0.60843439 0.61005918 0.61167311 0.61328478 0.61489162 
    ##      PC217      PC218      PC219      PC220      PC221      PC222 
    ## 0.61649264 0.61809104 0.61968411 0.62127480 0.62285980 0.62443612 
    ##      PC223      PC224      PC225      PC226      PC227      PC228 
    ## 0.62601101 0.62758174 0.62914765 0.63071019 0.63226674 0.63381409 
    ##      PC229      PC230      PC231      PC232      PC233      PC234 
    ## 0.63535874 0.63689889 0.63843279 0.63995941 0.64147955 0.64299607 
    ##      PC235      PC236      PC237      PC238      PC239      PC240 
    ## 0.64450998 0.64601716 0.64752317 0.64902269 0.65051386 0.65200270 
    ##      PC241      PC242      PC243      PC244      PC245      PC246 
    ## 0.65348812 0.65496966 0.65644620 0.65791527 0.65938105 0.66084519 
    ##      PC247      PC248      PC249      PC250      PC251      PC252 
    ## 0.66229905 0.66375201 0.66519761 0.66663725 0.66807640 0.66950849 
    ##      PC253      PC254      PC255      PC256      PC257      PC258 
    ## 0.67093353 0.67235141 0.67376759 0.67517705 0.67657934 0.67797990 
    ##      PC259      PC260      PC261      PC262      PC263      PC264 
    ## 0.67938036 0.68077821 0.68217573 0.68356396 0.68495037 0.68633282 
    ##      PC265      PC266      PC267      PC268      PC269      PC270 
    ## 0.68771353 0.68908533 0.69045237 0.69181446 0.69317186 0.69452404 
    ##      PC271      PC272      PC273      PC274      PC275      PC276 
    ## 0.69587533 0.69722199 0.69856590 0.69990719 0.70123990 0.70257032 
    ##      PC277      PC278      PC279      PC280      PC281      PC282 
    ## 0.70389529 0.70520978 0.70652234 0.70782776 0.70912954 0.71042604 
    ##      PC283      PC284      PC285      PC286      PC287      PC288 
    ## 0.71172019 0.71301215 0.71430308 0.71558989 0.71686960 0.71814444 
    ##      PC289      PC290      PC291      PC292      PC293      PC294 
    ## 0.71941773 0.72068637 0.72195012 0.72321278 0.72447444 0.72572839 
    ##      PC295      PC296      PC297      PC298      PC299      PC300 
    ## 0.72697989 0.72822152 0.72946124 0.73069656 0.73192997 0.73315770 
    ##      PC301      PC302      PC303      PC304      PC305      PC306 
    ## 0.73438270 0.73560647 0.73682501 0.73803764 0.73924781 0.74045344 
    ##      PC307      PC308      PC309      PC310      PC311      PC312 
    ## 0.74165751 0.74285941 0.74405279 0.74524491 0.74643391 0.74761400 
    ##      PC313      PC314      PC315      PC316      PC317      PC318 
    ## 0.74879212 0.74996440 0.75113433 0.75230143 0.75346659 0.75462776 
    ##      PC319      PC320      PC321      PC322      PC323      PC324 
    ## 0.75578517 0.75693591 0.75808538 0.75923319 0.76037469 0.76151062 
    ##      PC325      PC326      PC327      PC328      PC329      PC330 
    ## 0.76264242 0.76377060 0.76489806 0.76602176 0.76714333 0.76826017 
    ##      PC331      PC332      PC333      PC334      PC335      PC336 
    ## 0.76937470 0.77048311 0.77158848 0.77268854 0.77378702 0.77488211 
    ##      PC337      PC338      PC339      PC340      PC341      PC342 
    ## 0.77597382 0.77706179 0.77814696 0.77922601 0.78030461 0.78137906 
    ##      PC343      PC344      PC345      PC346      PC347      PC348 
    ## 0.78245021 0.78352078 0.78458564 0.78564794 0.78670828 0.78776355 
    ##      PC349      PC350      PC351      PC352      PC353      PC354 
    ## 0.78881433 0.78986302 0.79090680 0.79194717 0.79298654 0.79402044 
    ##      PC355      PC356      PC357      PC358      PC359      PC360 
    ## 0.79505118 0.79607954 0.79710582 0.79812686 0.79914623 0.80016041 
    ##      PC361      PC362      PC363      PC364      PC365      PC366 
    ## 0.80117053 0.80217768 0.80318183 0.80418311 0.80517930 0.80617435 
    ##      PC367      PC368      PC369      PC370      PC371      PC372 
    ## 0.80716477 0.80815448 0.80913909 0.81012299 0.81110072 0.81207389 
    ##      PC373      PC374      PC375      PC376      PC377      PC378 
    ## 0.81304337 0.81401121 0.81497762 0.81593964 0.81690083 0.81785697 
    ##      PC379      PC380      PC381      PC382      PC383      PC384 
    ## 0.81880885 0.81975862 0.82070544 0.82164854 0.82258807 0.82352111 
    ##      PC385      PC386      PC387      PC388      PC389      PC390 
    ## 0.82445289 0.82538117 0.82630631 0.82723099 0.82815159 0.82906922 
    ##      PC391      PC392      PC393      PC394      PC395      PC396 
    ## 0.82998419 0.83089534 0.83180409 0.83271055 0.83361370 0.83451465 
    ##      PC397      PC398      PC399      PC400      PC401      PC402 
    ## 0.83541262 0.83630997 0.83720295 0.83809170 0.83897674 0.83985693 
    ##      PC403      PC404      PC405      PC406      PC407      PC408 
    ## 0.84073590 0.84161195 0.84248666 0.84335905 0.84422875 0.84509272 
    ##      PC409      PC410      PC411      PC412      PC413      PC414 
    ## 0.84595502 0.84681586 0.84767430 0.84852719 0.84937792 0.85022707 
    ##      PC415      PC416      PC417      PC418      PC419      PC420 
    ## 0.85107419 0.85191666 0.85275881 0.85359862 0.85443606 0.85526812 
    ##      PC421      PC422      PC423      PC424      PC425      PC426 
    ## 0.85609766 0.85692401 0.85774480 0.85856492 0.85937874 0.86019161 
    ##      PC427      PC428      PC429      PC430      PC431      PC432 
    ## 0.86100190 0.86180840 0.86261257 0.86341530 0.86421077 0.86500410 
    ##      PC433      PC434      PC435      PC436      PC437      PC438 
    ## 0.86579483 0.86658478 0.86737108 0.86815690 0.86893880 0.86971772 
    ##      PC439      PC440      PC441      PC442      PC443      PC444 
    ## 0.87049586 0.87127143 0.87204472 0.87281619 0.87358440 0.87435049 
    ##      PC445      PC446      PC447      PC448      PC449      PC450 
    ## 0.87511288 0.87587504 0.87663450 0.87738926 0.87813961 0.87888669 
    ##      PC451      PC452      PC453      PC454      PC455      PC456 
    ## 0.87963326 0.88037771 0.88111862 0.88185678 0.88259199 0.88332492 
    ##      PC457      PC458      PC459      PC460      PC461      PC462 
    ## 0.88405713 0.88478596 0.88551465 0.88623983 0.88696192 0.88767990 
    ##      PC463      PC464      PC465      PC466      PC467      PC468 
    ## 0.88839522 0.88910899 0.88981965 0.89052880 0.89123472 0.89193733 
    ##      PC469      PC470      PC471      PC472      PC473      PC474 
    ## 0.89263797 0.89333573 0.89403223 0.89472594 0.89541822 0.89610456 
    ##      PC475      PC476      PC477      PC478      PC479      PC480 
    ## 0.89678879 0.89746983 0.89814769 0.89882190 0.89949566 0.90016785 
    ##      PC481      PC482      PC483      PC484      PC485      PC486 
    ## 0.90083680 0.90150175 0.90216549 0.90282673 0.90348550 0.90414259 
    ##      PC487      PC488      PC489      PC490      PC491      PC492 
    ## 0.90479780 0.90545002 0.90610129 0.90674980 0.90739500 0.90803791 
    ##      PC493      PC494      PC495      PC496      PC497      PC498 
    ## 0.90867951 0.90931991 0.90995700 0.91059021 0.91122144 0.91185095 
    ##      PC499      PC500      PC501      PC502      PC503      PC504 
    ## 0.91247886 0.91310284 0.91372586 0.91434504 0.91496268 0.91557673 
    ##      PC505      PC506      PC507      PC508      PC509      PC510 
    ## 0.91618970 0.91680215 0.91740947 0.91801302 0.91861444 0.91921481 
    ##      PC511      PC512      PC513      PC514      PC515      PC516 
    ## 0.91981163 0.92040785 0.92100299 0.92159418 0.92218187 0.92276818 
    ##      PC517      PC518      PC519      PC520      PC521      PC522 
    ## 0.92335234 0.92393511 0.92451558 0.92509361 0.92566960 0.92624433 
    ##      PC523      PC524      PC525      PC526      PC527      PC528 
    ## 0.92681784 0.92738767 0.92795551 0.92852060 0.92908379 0.92964485 
    ##      PC529      PC530      PC531      PC532      PC533      PC534 
    ## 0.93020154 0.93075561 0.93130772 0.93185797 0.93240663 0.93295150 
    ##      PC535      PC536      PC537      PC538      PC539      PC540 
    ## 0.93349579 0.93403697 0.93457679 0.93511314 0.93564910 0.93618138 
    ##      PC541      PC542      PC543      PC544      PC545      PC546 
    ## 0.93671226 0.93724146 0.93776817 0.93829014 0.93881074 0.93932928 
    ##      PC547      PC548      PC549      PC550      PC551      PC552 
    ## 0.93984611 0.94036051 0.94087448 0.94138741 0.94189745 0.94240499 
    ##      PC553      PC554      PC555      PC556      PC557      PC558 
    ## 0.94290983 0.94341225 0.94391231 0.94441175 0.94490970 0.94540508 
    ##      PC559      PC560      PC561      PC562      PC563      PC564 
    ## 0.94589698 0.94638697 0.94687597 0.94736364 0.94784769 0.94833077 
    ##      PC565      PC566      PC567      PC568      PC569      PC570 
    ## 0.94881168 0.94929011 0.94976594 0.95024108 0.95071219 0.95118165 
    ##      PC571      PC572      PC573      PC574      PC575      PC576 
    ## 0.95164911 0.95211514 0.95258029 0.95304249 0.95350362 0.95396185 
    ##      PC577      PC578      PC579      PC580      PC581      PC582 
    ## 0.95441581 0.95486812 0.95531796 0.95576654 0.95621389 0.95666007 
    ##      PC583      PC584      PC585      PC586      PC587      PC588 
    ## 0.95710249 0.95754463 0.95798279 0.95841907 0.95885336 0.95928555 
    ##      PC589      PC590      PC591      PC592      PC593      PC594 
    ## 0.95971751 0.96014643 0.96057383 0.96099921 0.96142299 0.96184516 
    ##      PC595      PC596      PC597      PC598      PC599      PC600 
    ## 0.96226581 0.96268354 0.96310045 0.96351471 0.96392745 0.96433933 
    ##      PC601      PC602      PC603      PC604      PC605      PC606 
    ## 0.96474755 0.96515490 0.96556088 0.96596621 0.96636719 0.96676569 
    ##      PC607      PC608      PC609      PC610      PC611      PC612 
    ## 0.96716095 0.96755526 0.96794647 0.96833394 0.96872087 0.96910599 
    ##      PC613      PC614      PC615      PC616      PC617      PC618 
    ## 0.96948725 0.96986800 0.97024569 0.97062262 0.97099618 0.97136916 
    ##      PC619      PC620      PC621      PC622      PC623      PC624 
    ## 0.97173982 0.97211025 0.97247712 0.97284247 0.97320581 0.97356671 
    ##      PC625      PC626      PC627      PC628      PC629      PC630 
    ## 0.97392662 0.97428498 0.97463898 0.97499167 0.97534413 0.97569376 
    ##      PC631      PC632      PC633      PC634      PC635      PC636 
    ## 0.97604321 0.97638862 0.97673274 0.97707354 0.97741352 0.97775089 
    ##      PC637      PC638      PC639      PC640      PC641      PC642 
    ## 0.97808721 0.97842189 0.97875610 0.97908882 0.97941884 0.97974787 
    ##      PC643      PC644      PC645      PC646      PC647      PC648 
    ## 0.98007412 0.98039999 0.98072174 0.98104331 0.98136256 0.98167917 
    ##      PC649      PC650      PC651      PC652      PC653      PC654 
    ## 0.98199347 0.98230519 0.98261524 0.98292481 0.98323229 0.98353675 
    ##      PC655      PC656      PC657      PC658      PC659      PC660 
    ## 0.98383933 0.98414021 0.98443997 0.98473834 0.98503512 0.98532956 
    ##      PC661      PC662      PC663      PC664      PC665      PC666 
    ## 0.98562225 0.98591367 0.98620298 0.98649094 0.98677697 0.98706086 
    ##      PC667      PC668      PC669      PC670      PC671      PC672 
    ## 0.98734248 0.98762335 0.98790273 0.98818017 0.98845483 0.98872833 
    ##      PC673      PC674      PC675      PC676      PC677      PC678 
    ## 0.98899984 0.98926839 0.98953508 0.98979950 0.99006297 0.99032410 
    ##      PC679      PC680      PC681      PC682      PC683      PC684 
    ## 0.99058502 0.99084452 0.99109917 0.99135347 0.99160487 0.99185367 
    ##      PC685      PC686      PC687      PC688      PC689      PC690 
    ## 0.99210166 0.99234886 0.99259379 0.99283792 0.99307928 0.99331933 
    ##      PC691      PC692      PC693      PC694      PC695      PC696 
    ## 0.99355477 0.99378823 0.99401988 0.99424941 0.99447633 0.99470186 
    ##      PC697      PC698      PC699      PC700      PC701      PC702 
    ## 0.99492428 0.99514496 0.99536455 0.99558131 0.99579482 0.99600612 
    ##      PC703      PC704      PC705      PC706      PC707      PC708 
    ## 0.99621421 0.99642205 0.99662553 0.99682792 0.99702882 0.99722787 
    ##      PC709      PC710      PC711      PC712      PC713      PC714 
    ## 0.99742434 0.99761836 0.99781100 0.99799965 0.99818687 0.99836921 
    ##      PC715      PC716      PC717      PC718      PC719      PC720 
    ## 0.99855054 0.99873004 0.99890406 0.99907449 0.99924127 0.99940769 
    ##      PC721      PC722      PC723      PC724      PC725      PC726 
    ## 0.99956793 0.99972111 0.99985470 0.99996565 0.99999521 1.00000000

``` r
plot(pca_train2,type='line')
```

![](Solutions_files/figure-markdown_github/unnamed-chunk-26-1.png)

``` r
#summary(pca_train2)

vars2 <- apply(pca_train2$x, 2, var)  
props2 <- vars2 / sum(vars2)
cumsum(props2)
```

    ##        PC1        PC2        PC3        PC4        PC5        PC6 
    ## 0.01840046 0.03057497 0.04005754 0.04935801 0.05772552 0.06576188 
    ##        PC7        PC8        PC9       PC10       PC11       PC12 
    ## 0.07275091 0.07957039 0.08598598 0.09216449 0.09803120 0.10376274 
    ##       PC13       PC14       PC15       PC16       PC17       PC18 
    ## 0.10910643 0.11421973 0.11913229 0.12365935 0.12816083 0.13257009 
    ##       PC19       PC20       PC21       PC22       PC23       PC24 
    ## 0.13689310 0.14118666 0.14536308 0.14944751 0.15351896 0.15756467 
    ##       PC25       PC26       PC27       PC28       PC29       PC30 
    ## 0.16153255 0.16539840 0.16917579 0.17294146 0.17660785 0.18024797 
    ##       PC31       PC32       PC33       PC34       PC35       PC36 
    ## 0.18384934 0.18737561 0.19086475 0.19429694 0.19771298 0.20107431 
    ##       PC37       PC38       PC39       PC40       PC41       PC42 
    ## 0.20441439 0.20773311 0.21102235 0.21427830 0.21750827 0.22072318 
    ##       PC43       PC44       PC45       PC46       PC47       PC48 
    ## 0.22393348 0.22708701 0.23023703 0.23335000 0.23642473 0.23947909 
    ##       PC49       PC50       PC51       PC52       PC53       PC54 
    ## 0.24250108 0.24549704 0.24848380 0.25144858 0.25440848 0.25736228 
    ##       PC55       PC56       PC57       PC58       PC59       PC60 
    ## 0.26029263 0.26320253 0.26608682 0.26895449 0.27180381 0.27463858 
    ##       PC61       PC62       PC63       PC64       PC65       PC66 
    ## 0.27745091 0.28025191 0.28304137 0.28581235 0.28858178 0.29133598 
    ##       PC67       PC68       PC69       PC70       PC71       PC72 
    ## 0.29407663 0.29680565 0.29950389 0.30218436 0.30485610 0.30751206 
    ##       PC73       PC74       PC75       PC76       PC77       PC78 
    ## 0.31016000 0.31280067 0.31542115 0.31803532 0.32063378 0.32321416 
    ##       PC79       PC80       PC81       PC82       PC83       PC84 
    ## 0.32577951 0.32832198 0.33086147 0.33339447 0.33591913 0.33842991 
    ##       PC85       PC86       PC87       PC88       PC89       PC90 
    ## 0.34093254 0.34342172 0.34589837 0.34837128 0.35083475 0.35328804 
    ##       PC91       PC92       PC93       PC94       PC95       PC96 
    ## 0.35572132 0.35814208 0.36053945 0.36293393 0.36532460 0.36769880 
    ##       PC97       PC98       PC99      PC100      PC101      PC102 
    ## 0.37005695 0.37240787 0.37475324 0.37709001 0.37941912 0.38174080 
    ##      PC103      PC104      PC105      PC106      PC107      PC108 
    ## 0.38404960 0.38635792 0.38865392 0.39094026 0.39321811 0.39547311 
    ##      PC109      PC110      PC111      PC112      PC113      PC114 
    ## 0.39772316 0.39996292 0.40219960 0.40443247 0.40665738 0.40887849 
    ##      PC115      PC116      PC117      PC118      PC119      PC120 
    ## 0.41108906 0.41329277 0.41549098 0.41767064 0.41984172 0.42201099 
    ##      PC121      PC122      PC123      PC124      PC125      PC126 
    ## 0.42416837 0.42631892 0.42846309 0.43059563 0.43272425 0.43484556 
    ##      PC127      PC128      PC129      PC130      PC131      PC132 
    ## 0.43695935 0.43907029 0.44116912 0.44325547 0.44534012 0.44740764 
    ##      PC133      PC134      PC135      PC136      PC137      PC138 
    ## 0.44947179 0.45152744 0.45357753 0.45562575 0.45766621 0.45969168 
    ##      PC139      PC140      PC141      PC142      PC143      PC144 
    ## 0.46171506 0.46373315 0.46574368 0.46775110 0.46975471 0.47174658 
    ##      PC145      PC146      PC147      PC148      PC149      PC150 
    ## 0.47372599 0.47569900 0.47766640 0.47962012 0.48157201 0.48351735 
    ##      PC151      PC152      PC153      PC154      PC155      PC156 
    ## 0.48546143 0.48739992 0.48932017 0.49123577 0.49314885 0.49505769 
    ##      PC157      PC158      PC159      PC160      PC161      PC162 
    ## 0.49696118 0.49885614 0.50074001 0.50261440 0.50448714 0.50635549 
    ##      PC163      PC164      PC165      PC166      PC167      PC168 
    ## 0.50821878 0.51007605 0.51192520 0.51377006 0.51561034 0.51744631 
    ##      PC169      PC170      PC171      PC172      PC173      PC174 
    ## 0.51927489 0.52109872 0.52291506 0.52472286 0.52652385 0.52831641 
    ##      PC175      PC176      PC177      PC178      PC179      PC180 
    ## 0.53010198 0.53188157 0.53365916 0.53543223 0.53719887 0.53895944 
    ##      PC181      PC182      PC183      PC184      PC185      PC186 
    ## 0.54071749 0.54246689 0.54420461 0.54594110 0.54767067 0.54939299 
    ##      PC187      PC188      PC189      PC190      PC191      PC192 
    ## 0.55111199 0.55282646 0.55453517 0.55624207 0.55793905 0.55963136 
    ##      PC193      PC194      PC195      PC196      PC197      PC198 
    ## 0.56131638 0.56299764 0.56467598 0.56635275 0.56802278 0.56968232 
    ##      PC199      PC200      PC201      PC202      PC203      PC204 
    ## 0.57133708 0.57298797 0.57463089 0.57626852 0.57790525 0.57953916 
    ##      PC205      PC206      PC207      PC208      PC209      PC210 
    ## 0.58116460 0.58278476 0.58439847 0.58600783 0.58761547 0.58921903 
    ##      PC211      PC212      PC213      PC214      PC215      PC216 
    ## 0.59081649 0.59241106 0.59399565 0.59557629 0.59715104 0.59872266 
    ##      PC217      PC218      PC219      PC220      PC221      PC222 
    ## 0.60028880 0.60184778 0.60340422 0.60495537 0.60650137 0.60804560 
    ##      PC223      PC224      PC225      PC226      PC227      PC228 
    ## 0.60958009 0.61111313 0.61263986 0.61416508 0.61568393 0.61719458 
    ##      PC229      PC230      PC231      PC232      PC233      PC234 
    ## 0.61870154 0.62020515 0.62170291 0.62319281 0.62467423 0.62615312 
    ##      PC235      PC236      PC237      PC238      PC239      PC240 
    ## 0.62762803 0.62909930 0.63056650 0.63202781 0.63348615 0.63493855 
    ##      PC241      PC242      PC243      PC244      PC245      PC246 
    ## 0.63638551 0.63782651 0.63926275 0.64069635 0.64212425 0.64354987 
    ##      PC247      PC248      PC249      PC250      PC251      PC252 
    ## 0.64497217 0.64638961 0.64780456 0.64921837 0.65062365 0.65202644 
    ##      PC253      PC254      PC255      PC256      PC257      PC258 
    ## 0.65342105 0.65481417 0.65620016 0.65758404 0.65896581 0.66034225 
    ##      PC259      PC260      PC261      PC262      PC263      PC264 
    ## 0.66171574 0.66308657 0.66445440 0.66581894 0.66717780 0.66853055 
    ##      PC265      PC266      PC267      PC268      PC269      PC270 
    ## 0.66987966 0.67122505 0.67256220 0.67389630 0.67522992 0.67656134 
    ##      PC271      PC272      PC273      PC274      PC275      PC276 
    ## 0.67788594 0.67920536 0.68052236 0.68183726 0.68314659 0.68444963 
    ##      PC277      PC278      PC279      PC280      PC281      PC282 
    ## 0.68575044 0.68704923 0.68834488 0.68963552 0.69092079 0.69220182 
    ##      PC283      PC284      PC285      PC286      PC287      PC288 
    ## 0.69348086 0.69475524 0.69602615 0.69729234 0.69855597 0.69981409 
    ##      PC289      PC290      PC291      PC292      PC293      PC294 
    ## 0.70106753 0.70231543 0.70356154 0.70480483 0.70604070 0.70727464 
    ##      PC295      PC296      PC297      PC298      PC299      PC300 
    ## 0.70849804 0.70971857 0.71093798 0.71215529 0.71337028 0.71458309 
    ##      PC301      PC302      PC303      PC304      PC305      PC306 
    ## 0.71579105 0.71699357 0.71819317 0.71938691 0.72057760 0.72176512 
    ##      PC307      PC308      PC309      PC310      PC311      PC312 
    ## 0.72294753 0.72412847 0.72530559 0.72647998 0.72765113 0.72881817 
    ##      PC313      PC314      PC315      PC316      PC317      PC318 
    ## 0.72998284 0.73114468 0.73229931 0.73345037 0.73459951 0.73574652 
    ##      PC319      PC320      PC321      PC322      PC323      PC324 
    ## 0.73688969 0.73802758 0.73916272 0.74029312 0.74141902 0.74254205 
    ##      PC325      PC326      PC327      PC328      PC329      PC330 
    ## 0.74366325 0.74478208 0.74589689 0.74700715 0.74811441 0.74921922 
    ##      PC331      PC332      PC333      PC334      PC335      PC336 
    ## 0.75031985 0.75141704 0.75251059 0.75360017 0.75468817 0.75577378 
    ##      PC337      PC338      PC339      PC340      PC341      PC342 
    ## 0.75685594 0.75793355 0.75901029 0.76008069 0.76114663 0.76221133 
    ##      PC343      PC344      PC345      PC346      PC347      PC348 
    ## 0.76327192 0.76432865 0.76538428 0.76643399 0.76748053 0.76852602 
    ##      PC349      PC350      PC351      PC352      PC353      PC354 
    ## 0.76956857 0.77060736 0.77164342 0.77267871 0.77371050 0.77474123 
    ##      PC355      PC356      PC357      PC358      PC359      PC360 
    ## 0.77577031 0.77679627 0.77781937 0.77883995 0.77985468 0.78086518 
    ##      PC361      PC362      PC363      PC364      PC365      PC366 
    ## 0.78187219 0.78287428 0.78387271 0.78486879 0.78585893 0.78684598 
    ##      PC367      PC368      PC369      PC370      PC371      PC372 
    ## 0.78782988 0.78881307 0.78979050 0.79076734 0.79174005 0.79271139 
    ##      PC373      PC374      PC375      PC376      PC377      PC378 
    ## 0.79367647 0.79463881 0.79559976 0.79655720 0.79751163 0.79846421 
    ##      PC379      PC380      PC381      PC382      PC383      PC384 
    ## 0.79941185 0.80035744 0.80129745 0.80223620 0.80317183 0.80410495 
    ##      PC385      PC386      PC387      PC388      PC389      PC390 
    ## 0.80503473 0.80596071 0.80688486 0.80780663 0.80872630 0.80964328 
    ##      PC391      PC392      PC393      PC394      PC395      PC396 
    ## 0.81055938 0.81146718 0.81237391 0.81327893 0.81417960 0.81507542 
    ##      PC397      PC398      PC399      PC400      PC401      PC402 
    ## 0.81596898 0.81686145 0.81775166 0.81863765 0.81952223 0.82040442 
    ##      PC403      PC404      PC405      PC406      PC407      PC408 
    ## 0.82128333 0.82216041 0.82303555 0.82390762 0.82477721 0.82564410 
    ##      PC409      PC410      PC411      PC412      PC413      PC414 
    ## 0.82650965 0.82737338 0.82823286 0.82909018 0.82994522 0.83079638 
    ##      PC415      PC416      PC417      PC418      PC419      PC420 
    ## 0.83164200 0.83248665 0.83332789 0.83416894 0.83500765 0.83584078 
    ##      PC421      PC422      PC423      PC424      PC425      PC426 
    ## 0.83667283 0.83750233 0.83832897 0.83915308 0.83997357 0.84078940 
    ##      PC427      PC428      PC429      PC430      PC431      PC432 
    ## 0.84160169 0.84241192 0.84322092 0.84402893 0.84483470 0.84563726 
    ##      PC433      PC434      PC435      PC436      PC437      PC438 
    ## 0.84643758 0.84723669 0.84803359 0.84882697 0.84961873 0.85040554 
    ##      PC439      PC440      PC441      PC442      PC443      PC444 
    ## 0.85119069 0.85197446 0.85275303 0.85352894 0.85430269 0.85507217 
    ##      PC445      PC446      PC447      PC448      PC449      PC450 
    ## 0.85583763 0.85660212 0.85736509 0.85812607 0.85888427 0.85964138 
    ##      PC451      PC452      PC453      PC454      PC455      PC456 
    ## 0.86039309 0.86114177 0.86188866 0.86263499 0.86337877 0.86412030 
    ##      PC457      PC458      PC459      PC460      PC461      PC462 
    ## 0.86485929 0.86559446 0.86632644 0.86705632 0.86778487 0.86851002 
    ##      PC463      PC464      PC465      PC466      PC467      PC468 
    ## 0.86923356 0.86995315 0.87067195 0.87138974 0.87210742 0.87282027 
    ##      PC469      PC470      PC471      PC472      PC473      PC474 
    ## 0.87353035 0.87423872 0.87494383 0.87564713 0.87634990 0.87704920 
    ##      PC475      PC476      PC477      PC478      PC479      PC480 
    ## 0.87774600 0.87843847 0.87913036 0.87982063 0.88051010 0.88119630 
    ##      PC481      PC482      PC483      PC484      PC485      PC486 
    ## 0.88188097 0.88256372 0.88324443 0.88392480 0.88460172 0.88527295 
    ##      PC487      PC488      PC489      PC490      PC491      PC492 
    ## 0.88594222 0.88661039 0.88727585 0.88793896 0.88859961 0.88925885 
    ##      PC493      PC494      PC495      PC496      PC497      PC498 
    ## 0.88991569 0.89057209 0.89122360 0.89187448 0.89252128 0.89316593 
    ##      PC499      PC500      PC501      PC502      PC503      PC504 
    ## 0.89380910 0.89444976 0.89508890 0.89572678 0.89636235 0.89699426 
    ##      PC505      PC506      PC507      PC508      PC509      PC510 
    ## 0.89762398 0.89825060 0.89887524 0.89949895 0.90012035 0.90074000 
    ##      PC511      PC512      PC513      PC514      PC515      PC516 
    ## 0.90135805 0.90197200 0.90258231 0.90319084 0.90379780 0.90440275 
    ##      PC517      PC518      PC519      PC520      PC521      PC522 
    ## 0.90500668 0.90560862 0.90620917 0.90680656 0.90740323 0.90799761 
    ##      PC523      PC524      PC525      PC526      PC527      PC528 
    ## 0.90859120 0.90918170 0.90976964 0.91035578 0.91093893 0.91151925 
    ##      PC529      PC530      PC531      PC532      PC533      PC534 
    ## 0.91209743 0.91267284 0.91324691 0.91382007 0.91439043 0.91495820 
    ##      PC535      PC536      PC537      PC538      PC539      PC540 
    ## 0.91552372 0.91608717 0.91664793 0.91720640 0.91776402 0.91831960 
    ##      PC541      PC542      PC543      PC544      PC545      PC546 
    ## 0.91887154 0.91942285 0.91997123 0.92051552 0.92105940 0.92160143 
    ##      PC547      PC548      PC549      PC550      PC551      PC552 
    ## 0.92214047 0.92267826 0.92321532 0.92375106 0.92428541 0.92481457 
    ##      PC553      PC554      PC555      PC556      PC557      PC558 
    ## 0.92534288 0.92587005 0.92639508 0.92691833 0.92743893 0.92795635 
    ##      PC559      PC560      PC561      PC562      PC563      PC564 
    ## 0.92847127 0.92898474 0.92949697 0.93000712 0.93051631 0.93102414 
    ##      PC565      PC566      PC567      PC568      PC569      PC570 
    ## 0.93152919 0.93203153 0.93253131 0.93303043 0.93352653 0.93402035 
    ##      PC571      PC572      PC573      PC574      PC575      PC576 
    ## 0.93451260 0.93500395 0.93549139 0.93597775 0.93646253 0.93694594 
    ##      PC577      PC578      PC579      PC580      PC581      PC582 
    ## 0.93742808 0.93790573 0.93838187 0.93885702 0.93933103 0.93980049 
    ##      PC583      PC584      PC585      PC586      PC587      PC588 
    ## 0.94026895 0.94073722 0.94120413 0.94166928 0.94213141 0.94259158 
    ##      PC589      PC590      PC591      PC592      PC593      PC594 
    ## 0.94305029 0.94350854 0.94396583 0.94441819 0.94486991 0.94532112 
    ##      PC595      PC596      PC597      PC598      PC599      PC600 
    ## 0.94576986 0.94621621 0.94665937 0.94710081 0.94753998 0.94797823 
    ##      PC601      PC602      PC603      PC604      PC605      PC606 
    ## 0.94841305 0.94884720 0.94928036 0.94971104 0.95014003 0.95056518 
    ##      PC607      PC608      PC609      PC610      PC611      PC612 
    ## 0.95098850 0.95141041 0.95183089 0.95224908 0.95266686 0.95308246 
    ##      PC613      PC614      PC615      PC616      PC617      PC618 
    ## 0.95349689 0.95390894 0.95431917 0.95472885 0.95513780 0.95554358 
    ##      PC619      PC620      PC621      PC622      PC623      PC624 
    ## 0.95594732 0.95634984 0.95675156 0.95715265 0.95755250 0.95795076 
    ##      PC625      PC626      PC627      PC628      PC629      PC630 
    ## 0.95834539 0.95873841 0.95912929 0.95951741 0.95990373 0.96028979 
    ##      PC631      PC632      PC633      PC634      PC635      PC636 
    ## 0.96067324 0.96105525 0.96143658 0.96181510 0.96219316 0.96256963 
    ##      PC637      PC638      PC639      PC640      PC641      PC642 
    ## 0.96294378 0.96331570 0.96368726 0.96405730 0.96442469 0.96479164 
    ##      PC643      PC644      PC645      PC646      PC647      PC648 
    ## 0.96515623 0.96551970 0.96588171 0.96624268 0.96660031 0.96695663 
    ##      PC649      PC650      PC651      PC652      PC653      PC654 
    ## 0.96730895 0.96765951 0.96800732 0.96835407 0.96869975 0.96904539 
    ##      PC655      PC656      PC657      PC658      PC659      PC660 
    ## 0.96938974 0.96973256 0.97007219 0.97041070 0.97074770 0.97108233 
    ##      PC661      PC662      PC663      PC664      PC665      PC666 
    ## 0.97141559 0.97174637 0.97207520 0.97240335 0.97273092 0.97305678 
    ##      PC667      PC668      PC669      PC670      PC671      PC672 
    ## 0.97338109 0.97370332 0.97402359 0.97434297 0.97466047 0.97497699 
    ##      PC673      PC674      PC675      PC676      PC677      PC678 
    ## 0.97529254 0.97560662 0.97591767 0.97622796 0.97653708 0.97684239 
    ##      PC679      PC680      PC681      PC682      PC683      PC684 
    ## 0.97714676 0.97744916 0.97775048 0.97804774 0.97834401 0.97863943 
    ##      PC685      PC686      PC687      PC688      PC689      PC690 
    ## 0.97893378 0.97922619 0.97951712 0.97980773 0.98009599 0.98038164 
    ##      PC691      PC692      PC693      PC694      PC695      PC696 
    ## 0.98066608 0.98094936 0.98123117 0.98151113 0.98178998 0.98206820 
    ##      PC697      PC698      PC699      PC700      PC701      PC702 
    ## 0.98234450 0.98261921 0.98289237 0.98316422 0.98343302 0.98370058 
    ##      PC703      PC704      PC705      PC706      PC707      PC708 
    ## 0.98396744 0.98423189 0.98449466 0.98475646 0.98501794 0.98527693 
    ##      PC709      PC710      PC711      PC712      PC713      PC714 
    ## 0.98553421 0.98579057 0.98604459 0.98629772 0.98655028 0.98680002 
    ##      PC715      PC716      PC717      PC718      PC719      PC720 
    ## 0.98704845 0.98729529 0.98754095 0.98778227 0.98802317 0.98826245 
    ##      PC721      PC722      PC723      PC724      PC725      PC726 
    ## 0.98850149 0.98873988 0.98897750 0.98921142 0.98944398 0.98967580 
    ##      PC727      PC728      PC729      PC730      PC731      PC732 
    ## 0.98990641 0.99013514 0.99036329 0.99058886 0.99081306 0.99103475 
    ##      PC733      PC734      PC735      PC736      PC737      PC738 
    ## 0.99125545 0.99147450 0.99169161 0.99190613 0.99211985 0.99233219 
    ##      PC739      PC740      PC741      PC742      PC743      PC744 
    ## 0.99254307 0.99275223 0.99295874 0.99316423 0.99336892 0.99357105 
    ##      PC745      PC746      PC747      PC748      PC749      PC750 
    ## 0.99377297 0.99397360 0.99417239 0.99437015 0.99456451 0.99475859 
    ##      PC751      PC752      PC753      PC754      PC755      PC756 
    ## 0.99495021 0.99513898 0.99532609 0.99551270 0.99569722 0.99588070 
    ##      PC757      PC758      PC759      PC760      PC761      PC762 
    ## 0.99606018 0.99623907 0.99641615 0.99659225 0.99676517 0.99693712 
    ##      PC763      PC764      PC765      PC766      PC767      PC768 
    ## 0.99710803 0.99727746 0.99744476 0.99761006 0.99777352 0.99793647 
    ##      PC769      PC770      PC771      PC772      PC773      PC774 
    ## 0.99809610 0.99825361 0.99840645 0.99855832 0.99870684 0.99885369 
    ##      PC775      PC776      PC777      PC778      PC779      PC780 
    ## 0.99899886 0.99914144 0.99927977 0.99941488 0.99954831 0.99967725 
    ##      PC781      PC782      PC783      PC784      PC785 
    ## 0.99979192 0.99989363 0.99998485 0.99999573 1.00000000

Choosing 60% varability hence taking 207 PCs for first set and 217 PCs for second set

Attaching target variable 'author' to the train and test datsets generated after generating PCs

``` r
df_train = data.frame(pca_train$x[,1:207])
df_train['author']=authors
df_test = data.frame(pca_test[,1:207])
df_test2 = df_test
df_test2['author']=authors_test


df_train_new = data.frame(pca_train2$x[,1:217])
df_train_new['author']=authors
df_test_new = data.frame(pca_test2[,1:217])
df_test_new2 = df_test_new
df_test_new2['author']=authors_test
```

#### Naive Bayes for intersected train-test

``` r
library('e1071')
nb_model = naiveBayes(as.factor(author) ~., data=df_train)
nb_predictions_test = predict(nb_model,df_test)
#table(nb_predictions_test,df_test$author)
cm <- caret::confusionMatrix(nb_predictions_test,as.factor(df_test2$author))
accuracy <- cm$overall['Accuracy']
accuracy
```

    ## Accuracy 
    ##   0.4516

``` r
#nb_predictions_test
#45%
```

#### Random Forest for intersected train-test

``` r
library('randomForest')
```

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
rf_model = randomForest(as.factor(author) ~ ., data=df_train, ntree=100, mtry=15, importance=TRUE)
rf_predictions_test = predict(rf_model,df_test)
#table(rf_predictions_test,df_test2$author)
cm <- caret::confusionMatrix(rf_predictions_test,as.factor(df_test2$author))
accuracy <- cm$overall['Accuracy']
accuracy
#45%
```

#### Naive Bayes for psuedo word handled train-test

``` r
nb_model_new = naiveBayes(as.factor(author) ~., data=df_train_new)
nb_predictions_test_new = predict(nb_model_new,df_test_new)
#table(nb_predictions_test,df_test$author)
cm <- caret::confusionMatrix(nb_predictions_test_new,as.factor(df_test_new2$author))
accuracy <- cm$overall['Accuracy']
accuracy
```

    ## Accuracy 
    ##   0.4548

``` r
#nb_predictions_test
#46%
```

#### Random Forest for psuedo word handled train-test

``` r
rf_model_new = randomForest(as.factor(author) ~ ., data=df_train_new, ntree=100, mtry=15, importance=TRUE)
rf_predictions_test_new = predict(rf_model_new,df_test_new)
cm <- caret::confusionMatrix(rf_predictions_test_new,as.factor(df_test_new2$author))
accuracy <- cm$overall['Accuracy']
accuracy
```

    ## Accuracy 
    ##   0.4524

``` r
#45% accuracy
```

Hence, we can say that pseudo word handled data with Naive Bayes gives us the best results with 46% accuracy.

Problem3
--------

We have multiple baskets of grocery items and will find some interesting association rules using the items in these baskets. This is how the data looks like after reading from the text file:

``` r
groceries = scan("../data/groceries.txt", what="", sep="\n")
print(paste("number of baskets = ", length(groceries)))
```

    ## [1] "number of baskets =  9835"

``` r
head(groceries)
```

    ## [1] "citrus fruit,semi-finished bread,margarine,ready soups"             
    ## [2] "tropical fruit,yogurt,coffee"                                       
    ## [3] "whole milk"                                                         
    ## [4] "pip fruit,yogurt,cream cheese ,meat spreads"                        
    ## [5] "other vegetables,whole milk,condensed milk,long life bakery product"
    ## [6] "whole milk,butter,yogurt,rice,abrasive cleaner"

Transforming the data to make it readable in a transaction format accepted by arules package:

``` r
y = strsplit(groceries, ",")
gro_trans = as(y, "transactions")
summary(gro_trans)
```

    ## transactions as itemMatrix in sparse format with
    ##  9835 rows (elements/itemsets/transactions) and
    ##  169 columns (items) and a density of 0.02609146 
    ## 
    ## most frequent items:
    ##       whole milk other vegetables       rolls/buns             soda 
    ##             2513             1903             1809             1715 
    ##           yogurt          (Other) 
    ##             1372            34055 
    ## 
    ## element (itemset/transaction) length distribution:
    ## sizes
    ##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
    ## 2159 1643 1299 1005  855  645  545  438  350  246  182  117   78   77   55 
    ##   16   17   18   19   20   21   22   23   24   26   27   28   29   32 
    ##   46   29   14   14    9   11    4    6    1    1    1    1    3    1 
    ## 
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.000   3.000   4.409   6.000  32.000 
    ## 
    ## includes extended item information - examples:
    ##             labels
    ## 1 abrasive cleaner
    ## 2 artif. sweetener
    ## 3   baby cosmetics

Few points to note from the summary:
1. Presenece of 169 different items in all the transactions combined.
2. Whole milk, other vegetables, rolls/buns are the most frequently bought items.
3. 2159 out of 9835 transactions have only one item in the cart.
4. 75% transactions have less than 6 items in the cart.

Now we will run the apriori algorithm to get the rules from these transactions. In order to select threshold for support, let us consider itemsets which appear atleast in 50 baskets out of 9835. That converts to a support threshold of (50/9835) = 0.005. Threshold for confidence will be fixed at 0.2 to ensure that atleast 1 of 5 baskets that contain X also have Y in them(for the rule X -&gt; Y). Also we put in the paramenter minlen = 2 to avoid rules with NULL value of X.

``` r
groceryrules = apriori(gro_trans, 
                     parameter=list(support=0.005, confidence=.2, minlen=2))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.2    0.1    1 none FALSE            TRUE       5   0.005      2
    ##  maxlen target   ext
    ##      10  rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 49 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [120 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 done [0.00s].
    ## writing ... [872 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

This created 872 rules. Let us plot these rules. ![](Solutions_files/figure-markdown_github/unnamed-chunk-36-1.png)

It is observed that rules with high lift have low support.

Looking at the two-key plot for studying the variations in rules with size of itemset:
![](Solutions_files/figure-markdown_github/unnamed-chunk-37-1.png)

All the rules of higher order have low support and itemsets of lower order have low confidence. Lets examine these rules.

#### Rules with high lift

``` r
inspect(subset(groceryrules, lift > 3.5))
```

    ##      lhs                     rhs                      support confidence     lift count
    ## [1]  {herbs}              => {root vegetables}    0.007015760  0.4312500 3.956477    69
    ## [2]  {berries}            => {whipped/sour cream} 0.009049314  0.2721713 3.796886    89
    ## [3]  {onions,                                                                          
    ##       other vegetables}   => {root vegetables}    0.005693950  0.4000000 3.669776    56
    ## [4]  {beef,                                                                            
    ##       other vegetables}   => {root vegetables}    0.007930859  0.4020619 3.688692    78
    ## [5]  {curd,                                                                            
    ##       tropical fruit}     => {yogurt}             0.005287239  0.5148515 3.690645    52
    ## [6]  {domestic eggs,                                                                   
    ##       whole milk}         => {butter}             0.005998983  0.2000000 3.609174    59
    ## [7]  {butter,                                                                          
    ##       other vegetables}   => {whipped/sour cream} 0.005795628  0.2893401 4.036397    57
    ## [8]  {other vegetables,                                                                
    ##       whipped/sour cream} => {butter}             0.005795628  0.2007042 3.621883    57
    ## [9]  {whipped/sour cream,                                                              
    ##       whole milk}         => {butter}             0.006710727  0.2082019 3.757185    66
    ## [10] {citrus fruit,                                                                    
    ##       pip fruit}          => {tropical fruit}     0.005592272  0.4044118 3.854060    55
    ## [11] {citrus fruit,                                                                    
    ##       tropical fruit}     => {pip fruit}          0.005592272  0.2806122 3.709437    55
    ## [12] {other vegetables,                                                                
    ##       whole milk,                                                                      
    ##       yogurt}             => {whipped/sour cream} 0.005592272  0.2511416 3.503514    55
    ## [13] {other vegetables,                                                                
    ##       pip fruit,                                                                       
    ##       whole milk}         => {root vegetables}    0.005490595  0.4060150 3.724961    54
    ## [14] {citrus fruit,                                                                    
    ##       other vegetables,                                                                
    ##       whole milk}         => {root vegetables}    0.005795628  0.4453125 4.085493    57
    ## [15] {root vegetables,                                                                 
    ##       whole milk,                                                                      
    ##       yogurt}             => {tropical fruit}     0.005693950  0.3916084 3.732043    56
    ## [16] {other vegetables,                                                                
    ##       tropical fruit,                                                                  
    ##       whole milk}         => {root vegetables}    0.007015760  0.4107143 3.768074    69

#### Rules with low confidence and high support rules.

``` r
inspect(subset(groceryrules, support > 0.025 & confidence < 0.35))
```

    ##      lhs                   rhs                support    confidence
    ## [1]  {newspapers}       => {whole milk}       0.02735130 0.3426752 
    ## [2]  {pip fruit}        => {other vegetables} 0.02613116 0.3454301 
    ## [3]  {citrus fruit}     => {other vegetables} 0.02887646 0.3488943 
    ## [4]  {sausage}          => {rolls/buns}       0.03060498 0.3257576 
    ## [5]  {sausage}          => {other vegetables} 0.02694459 0.2867965 
    ## [6]  {sausage}          => {whole milk}       0.02989324 0.3181818 
    ## [7]  {bottled water}    => {soda}             0.02897814 0.2621895 
    ## [8]  {bottled water}    => {whole milk}       0.03436706 0.3109476 
    ## [9]  {tropical fruit}   => {yogurt}           0.02928317 0.2790698 
    ## [10] {yogurt}           => {tropical fruit}   0.02928317 0.2099125 
    ## [11] {tropical fruit}   => {other vegetables} 0.03589222 0.3420543 
    ## [12] {root vegetables}  => {yogurt}           0.02582613 0.2369403 
    ## [13] {other vegetables} => {root vegetables}  0.04738180 0.2448765 
    ## [14] {soda}             => {rolls/buns}       0.03833249 0.2198251 
    ## [15] {rolls/buns}       => {soda}             0.03833249 0.2084024 
    ## [16] {soda}             => {whole milk}       0.04006101 0.2297376 
    ## [17] {yogurt}           => {rolls/buns}       0.03436706 0.2463557 
    ## [18] {yogurt}           => {other vegetables} 0.04341637 0.3112245 
    ## [19] {other vegetables} => {yogurt}           0.04341637 0.2243826 
    ## [20] {whole milk}       => {yogurt}           0.05602440 0.2192598 
    ## [21] {rolls/buns}       => {other vegetables} 0.04260295 0.2316197 
    ## [22] {other vegetables} => {rolls/buns}       0.04260295 0.2201787 
    ## [23] {rolls/buns}       => {whole milk}       0.05663447 0.3079049 
    ## [24] {whole milk}       => {rolls/buns}       0.05663447 0.2216474 
    ## [25] {whole milk}       => {other vegetables} 0.07483477 0.2928770 
    ##      lift      count
    ## [1]  1.3411103 269  
    ## [2]  1.7852365 257  
    ## [3]  1.8031403 284  
    ## [4]  1.7710480 301  
    ## [5]  1.4822091 265  
    ## [6]  1.2452520 294  
    ## [7]  1.5035766 285  
    ## [8]  1.2169396 338  
    ## [9]  2.0004746 288  
    ## [10] 2.0004746 288  
    ## [11] 1.7677896 353  
    ## [12] 1.6984751 254  
    ## [13] 2.2466049 466  
    ## [14] 1.1951242 377  
    ## [15] 1.1951242 377  
    ## [16] 0.8991124 394  
    ## [17] 1.3393633 338  
    ## [18] 1.6084566 427  
    ## [19] 1.6084566 427  
    ## [20] 1.5717351 551  
    ## [21] 1.1970465 419  
    ## [22] 1.1970465 419  
    ## [23] 1.2050318 557  
    ## [24] 1.2050318 557  
    ## [25] 1.5136341 736

Items in these rules are the most frequently bought items as we saw from the summary of transactions. This explains the high support of these rules and low confidence.

#### Rules with high confidence and high support:

``` r
inspect(subset(groceryrules, support > 0.025 & confidence > 0.35))
```

    ##      lhs                        rhs                support    confidence
    ## [1]  {curd}                  => {whole milk}       0.02613116 0.4904580 
    ## [2]  {brown bread}           => {whole milk}       0.02521607 0.3887147 
    ## [3]  {butter}                => {whole milk}       0.02755465 0.4972477 
    ## [4]  {domestic eggs}         => {whole milk}       0.02999492 0.4727564 
    ## [5]  {fruit/vegetable juice} => {whole milk}       0.02663955 0.3684951 
    ## [6]  {whipped/sour cream}    => {other vegetables} 0.02887646 0.4028369 
    ## [7]  {whipped/sour cream}    => {whole milk}       0.03223183 0.4496454 
    ## [8]  {pip fruit}             => {whole milk}       0.03009659 0.3978495 
    ## [9]  {pastry}                => {whole milk}       0.03324860 0.3737143 
    ## [10] {citrus fruit}          => {whole milk}       0.03050330 0.3685504 
    ## [11] {tropical fruit}        => {whole milk}       0.04229792 0.4031008 
    ## [12] {root vegetables}       => {other vegetables} 0.04738180 0.4347015 
    ## [13] {root vegetables}       => {whole milk}       0.04890696 0.4486940 
    ## [14] {yogurt}                => {whole milk}       0.05602440 0.4016035 
    ## [15] {other vegetables}      => {whole milk}       0.07483477 0.3867578 
    ##      lift     count
    ## [1]  1.919481 257  
    ## [2]  1.521293 248  
    ## [3]  1.946053 271  
    ## [4]  1.850203 295  
    ## [5]  1.442160 262  
    ## [6]  2.081924 284  
    ## [7]  1.759754 317  
    ## [8]  1.557043 296  
    ## [9]  1.462587 327  
    ## [10] 1.442377 300  
    ## [11] 1.577595 416  
    ## [12] 2.246605 466  
    ## [13] 1.756031 481  
    ## [14] 1.571735 551  
    ## [15] 1.513634 736

#### Rules on network graph

``` r
sub1 = subset(groceryrules, subset=support > 0.025 & confidence > 0.35)
plot(sub1, method='graph')
```

![](Solutions_files/figure-markdown_github/unnamed-chunk-41-1.png)

``` r
plot(head(sub1, 5, by='lift'), method='graph')
```

![](Solutions_files/figure-markdown_github/unnamed-chunk-41-2.png)
