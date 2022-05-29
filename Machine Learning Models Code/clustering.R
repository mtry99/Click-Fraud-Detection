##### LIBRARIES ##### 
library(plyr)
library(factoextra)
library(cluster)

data <- read.csv("train_sample.csv")
data$date <- as.Date(data$click_time)


##### MODEL 1: FREQUENCY OF EACH DATE #####

consolidatedData <- count(data, c("ip", "device", "app", "channel", "os", 
                                  "date", "is_attributed"))

### Create target variables for each date
consolidatedNov6 <- consolidatedData[consolidatedData$date == "2017-11-06",]
consolidatedNov6$Nov6 <- consolidatedNov6$freq
consolidatedNov6$Nov7 <- 0
consolidatedNov6$Nov8 <- 0
consolidatedNov6$Nov9 <- 0
str(consolidatedNov6)

consolidatedNov7 <- consolidatedData[consolidatedData$date == "2017-11-07",]
consolidatedNov7$Nov6 <- 0
consolidatedNov7$Nov7 <- consolidatedNov7$freq
consolidatedNov7$Nov8 <- 0
consolidatedNov7$Nov9 <- 0

consolidatedNov8 <- consolidatedData[consolidatedData$date == "2017-11-08",]
consolidatedNov8$Nov6 <- 0
consolidatedNov8$Nov7 <- 0
consolidatedNov8$Nov8 <- consolidatedNov8$freq
consolidatedNov8$Nov9 <- 0

consolidatedNov9 <- consolidatedData[consolidatedData$date == "2017-11-09",]
consolidatedNov9$Nov6 <- 0
consolidatedNov9$Nov7 <- 0
consolidatedNov9$Nov8 <- 0
consolidatedNov9$Nov9 <- consolidatedNov9$freq

# combine the segmented dataset
newConsolidated <- rbind(consolidatedNov6, consolidatedNov7, consolidatedNov8, consolidatedNov9)

# ensure values are unique
newConsolidatedUnique <- aggregate(cbind(Nov6,Nov7,Nov8,Nov9) ~ ip + device + app + 	
                                     channel + os + is_attributed,
                                   newConsolidated, sum)

# remove rows where is_attributed==1
toCluster1 <- newConsolidatedUnique[newConsolidatedUnique$is_attributed==0,]

# keep only frequency variables
toCluster1$ip <- NULL
toCluster1$device <- NULL
toCluster1$app <- NULL
toCluster1$channel <- NULL
toCluster1$os <- NULL
toCluster1$is_attributed <- NULL

# run kmeans for k = 1,...,20
cls1 <- kmeans(toCluster1, 1, iter.max = 2000, nstart = 10)
cls2 <- kmeans(toCluster1, 2, iter.max = 2000, nstart = 10)
cls3 <- kmeans(toCluster1, 3, iter.max = 2000, nstart = 10)
cls4 <- kmeans(toCluster1, 4, iter.max = 2000, nstart = 10)
cls5 <- kmeans(toCluster1, 5, iter.max = 2000, nstart = 10)
cls6 <- kmeans(toCluster1, 6, iter.max = 2000, nstart = 10)
cls7 <- kmeans(toCluster1, 7, iter.max = 2000, nstart = 10)
cls8 <- kmeans(toCluster1, 8, iter.max = 2000, nstart = 10)
cls9 <- kmeans(toCluster1, 9, iter.max = 2000, nstart = 10)
cls10 <- kmeans(toCluster1, 10, iter.max = 2000, nstart = 10)
cls11 <- kmeans(toCluster1, 11, iter.max = 2000, nstart = 10)
cls12 <- kmeans(toCluster1, 12, iter.max = 2000, nstart = 10)
cls13 <- kmeans(toCluster1, 13, iter.max = 2000, nstart = 10)
cls14 <- kmeans(toCluster1, 14, iter.max = 2000, nstart = 10)
cls15 <- kmeans(toCluster1, 15, iter.max = 2000, nstart = 10)
cls16 <- kmeans(toCluster1, 16, iter.max = 2000, nstart = 10)
cls17 <- kmeans(toCluster1, 17, iter.max = 2000, nstart = 10)
cls18 <- kmeans(toCluster1, 18, iter.max = 2000, nstart = 10)
cls19 <- kmeans(toCluster1, 19, iter.max = 2000, nstart = 10)
cls20 <- kmeans(toCluster1, 20, iter.max = 2000, nstart = 10)

# get R^2 for all clusters
kvec <- c(1:20)
kvec[1] <- 1-sum(cls1$withinss) / cls1$totss
kvec[2] <- 1-sum(cls2$withinss) / cls2$totss
kvec[3] <- 1-sum(cls3$withinss) / cls3$totss
kvec[4] <- 1-sum(cls4$withinss) / cls4$totss
kvec[5] <- 1-sum(cls5$withinss) / cls5$totss
kvec[6] <- 1-sum(cls6$withinss) / cls6$totss
kvec[7] <- 1-sum(cls7$withinss) / cls7$totss
kvec[8] <- 1-sum(cls8$withinss) / cls8$totss
kvec[9] <- 1-sum(cls9$withinss) / cls9$totss
kvec[10] <- 1-sum(cls10$withinss) / cls10$totss
kvec[11] <- 1-sum(cls11$withinss) / cls11$totss
kvec[12] <- 1-sum(cls12$withinss) / cls12$totss
kvec[13] <- 1-sum(cls13$withinss) / cls13$totss
kvec[14] <- 1-sum(cls14$withinss) / cls14$totss
kvec[15] <- 1-sum(cls15$withinss) / cls15$totss
kvec[16] <- 1-sum(cls16$withinss) / cls16$totss
kvec[17] <- 1-sum(cls17$withinss) / cls17$totss
kvec[18] <- 1-sum(cls18$withinss) / cls18$totss
kvec[19] <- 1-sum(cls19$withinss) / cls19$totss
kvec[20] <- 1-sum(cls20$withinss) / cls20$totss

plot(kvec)

# 6 is optimal number of clusters
clusplot(toCluster1, cls6$cluster, color = TRUE)
print(cls6)

# fviz_cluster(cls6, toCluster1)

# cluster 2 rows
newConsolidatedUnique[c(138,21751,21752,23135,23172,23185,23193,23214,25527,27780,27885,
                        27977,28957,29692,49443,49776,51489,52609,57940,59531,59563,59565,
                        59566,61782,64579,64943,66731,67681),]

# cluster 6 rows
newConsolidatedUnique[c(5925,5926,5935,5973,9672,9882,12293,16787,16992,17177,17609,17883,17941,18113,
                        23125,23143,23145,23171,23188,23189,23197,23202,23215,23262,24513,24769,25328,25336,
                        25525,25794,25877,26376,26377,27050,27235,27518,27781,27978,28201,28641,28758,28801,
                        28836,28876,28976,29099,30486,34211,34589,34817,43721,47514,49777,51784,51862,52765,
                        52798,52815,55170,57941,58205,58745,59460,59463,59464,59465,59471,59472,59476,59483,
                        59508,59509,59516,59517,59528,59529,59530,59534,59538,59547,59548,59549,59559,60443,
                        61005,61353,61838,61839,61968,62231,62235,62415,62606,63174,63175,63211,63231,63486,
                        63861,63924,64578,64692,64761,64783,65056,65099,65327,65328,65490,65640,65771,65916,
                        66730,67679,72820,72939,79064,80698,86126),]


##### MODEL 2: OVERALL FREQUENCY #####

consolidatedData <- count(data, c("ip", "device", "app", "channel", "os", "is_attributed"))

# only keep rows where is_attributed=0
toCluster2 <- consolidatedData[consolidatedData$is_attributed==0,]

# only keep frequency variable
toCluster2$ip <- NULL
toCluster2$device <- NULL
toCluster2$app <- NULL
toCluster2$channel <- NULL
toCluster2$os <- NULL
toCluster2$is_attributed <- NULL

# run kmeans for k = 1,...,14
clu1 <- kmeans(toCluster2, 1, iter.max = 2000, nstart = 10)
clu2 <- kmeans(toCluster2, 2, iter.max = 2000, nstart = 10)
clu3 <- kmeans(toCluster2, 3, iter.max = 2000, nstart = 10)
clu4 <- kmeans(toCluster2, 4, iter.max = 2000, nstart = 10)
clu5 <- kmeans(toCluster2, 5, iter.max = 2000, nstart = 10)
clu6 <- kmeans(toCluster2, 6, iter.max = 2000, nstart = 10)
clu7 <- kmeans(toCluster2, 7, iter.max = 2000, nstart = 10)
clu8 <- kmeans(toCluster2, 8, iter.max = 2000, nstart = 10)
clu9 <- kmeans(toCluster2, 9, iter.max = 2000, nstart = 10)
clu10 <- kmeans(toCluster2, 10, iter.max = 2000, nstart = 10)
clu11 <- kmeans(toCluster2, 11, iter.max = 2000, nstart = 10)
clu12 <- kmeans(toCluster2, 12, iter.max = 2000, nstart = 10)
clu13 <- kmeans(toCluster2, 13, iter.max = 2000, nstart = 10)
clu14 <- kmeans(toCluster2, 14, iter.max = 2000, nstart = 10)

# get R^2 for all clusters
kvec <- c(1:14)
kvec[1] <- 1-sum(clu1$withinss) / clu1$totss
kvec[2] <- 1-sum(clu2$withinss) / clu2$totss
kvec[3] <- 1-sum(clu3$withinss) / clu3$totss
kvec[4] <- 1-sum(clu4$withinss) / clu4$totss
kvec[5] <- 1-sum(clu5$withinss) / clu5$totss
kvec[6] <- 1-sum(clu6$withinss) / clu6$totss
kvec[7] <- 1-sum(clu7$withinss) / clu7$totss
kvec[8] <- 1-sum(clu8$withinss) / clu8$totss
kvec[9] <- 1-sum(clu9$withinss) / clu9$totss
kvec[10] <- 1-sum(clu10$withinss) / clu10$totss
kvec[11] <- 1-sum(clu11$withinss) / clu11$totss
kvec[12] <- 1-sum(clu12$withinss) / clu12$totss
kvec[13] <- 1-sum(clu13$withinss) / clu13$totss
kvec[14] <- 1-sum(clu14$withinss) / clu14$totss

plot(kvec)


clusplot(toCluster2, clu4$cluster, color = TRUE)
print(clu4)

# cluster 2
consolidatedData[c(249,371,1293,3039,3582,4887,5507,5699,6054,7308,7543,8644,11703,
                        12136,19497,22605,36942,37435),]


# cluster 4
consolidatedData[c(15,195,282,724,808,1378,1395,1899,1910,1927,2330,2373,2411,2614,
                        2635,2695,2837,2873,3007,3750,3906,3971,4032,4211,4403,4932,5624,5719,
                        5787,5823,5918,5984,6669,6911,7054,7151,7155,7213,7433,7879,7883,8116,
                        8697,8845,8948,9038,9195,9239,9442,10113,10345,10555,10814,10881,11146,11624,
                        11948,11969,12221,12429,12947,13021,13567,13935,14295,14887,15228,15503,15663,16471,
                        17226,17291,17678,17906,18311,18467,19264,19914,20834,21206,22552,23679,24033,24150,
                        25021,25147,25859,26236,28611,29036,29077,29662,30824,31017,31359,31830,33763,33946,
                        39608,40685,41847,42909,45908,46927,48019,55505,58390,59856,60595,66998),]
