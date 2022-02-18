#import dataset ke dataIPK
dataIPK=read.csv(file = "C:/Users/AERLANGGA OFFICE/Documents/KNN/dataakademikKNN.csv", sep=",")

#cekjumlah data
nrow(dataIPK)

#lihat data 
View(dataIPK)

summary(dataIPK[1:5])

#fungsi normalisasi
normalize <- function(x) { (x -min(x))/(max(x)-min(x))}

dataIPK_norm <- as.data.frame(lapply(dataIPK[,c(3:4)], normalize))
head(dataIPK_norm)


###data splicing###
#generate random number 80% dari row dataset
data_split <- sample(1:nrow(dataIPK_norm), 0.8 * nrow(dataIPK_norm))

#ekstraksi data training
train <- dataIPK_norm[data_split,]
head(train)
dim(train)

#ekstraksi data testing
test <- dataIPK_norm[-data_split,]
head(test)
dim(test)


###uji kredibilitas

#ekstraksi kolom ke 5 untuk cek akurasi
target_category <- dataIPK[data_split,5]
test_category <- dataIPK[-data_split,5]

target_category
test_category

###membuat model KNN
#load paket
library(class)

#running knn (K=3)
test_pred <- knn(train, test, cl=target_category,k=3)
test_pred

#compare aktual vs prediksi
df_pred=data.frame(test_category, test_pred)
df_pred

###akurasi
#confusion matrix
table <- table(test_category, test_pred)
table

#evaluasi model
#install gmodels
install.packages("gmodels")
library("gmodels")
CrossTable(x=test_category, y=test_pred, prop.chisq=FALSE)

#check akurasi
#presentase prediksi yang melambangkan sebarapa akurat model knn yang dibuat

accuracy <- 
  function(x) {sum(diag(x)) / (sum(rowSums(x))) * 100}
accuracy(table)

