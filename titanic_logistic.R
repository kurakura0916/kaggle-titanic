library(ggplot2)
library(makedummies)
library(dplyr)
library(car)

# test / train データの読み込み
train <- read.csv("train.csv",header = T,stringsAsFactors = F,na.strings=(c("NA", "")))
test <- read.csv("test.csv",header = T,stringsAsFactors = F,na.strings=(c("NA", "")))

# 欠損値の確認
sapply(train,function(x) sum(is.na(x)))

# 性別毎に年齢のヒストグラムを作成
# ggplot(data=train, aes(x=Age, fill=Sex)) + geom_histogram(alpha=0.4,binwidth=5)

# 性別毎にデータフレームを用意
train_male <- subset(train,Sex == 'male')
train_female <- subset(train,Sex == 'female')

# 中央値の確認
median(train_male$Age,na.rm = TRUE)
median(train_female$Age,na.rm = TRUE)

# 年齢でのヒストグラムを作成
hist(train_male$Age,breaks=seq(0,80,5))
hist(train_female$Age,breaks=seq(0,80,5))

# 中央値で補完する
train_male$Age[is.na(train_male$Age)] <- median(train_male$Age,na.rm = TRUE)
train_female$Age[is.na(train_female$Age)] <- median(train_female$Age,na.rm = TRUE)

# 性別毎で分けていたデータフレームをマージ
train <- rbind(train_male,train_female)

# EmbarkedがNAの行を確認
train[which(is.na(train$Embarked)),]
table(train$Pclass,train$Embarked)

# Pclassが1であることのみ共通であるので、Pclassが1であること人のデータフレームを作成
train_pclass_1 <- subset(train,Pclass == 1)
# Embarked毎にFareの中央値を確認
train_pclass_1 %>% group_by(Embarked) %>% summarise(median = median(Fare))

# EmbarkedがNAの乗客のFare（中央値）がCと近いので、Cで補完
train$Embarked[is.na(train$Embarked)] <- "C"

# 性別毎にデータフレームを用意
test_male <- subset(test,test$Sex == 'male')
test_female <- subset(test,test$Sex == 'female')

ggplot(data=test, aes(x=Age, fill=Sex)) + geom_histogram(alpha=0.4,binwidth=5)

#中央値で補完する
test_male$Age[is.na(test_male$Age)] <- median(test_male$Age,na.rm = TRUE)
test_female$Age[is.na(test_female$Age)] <- median(test_female$Age,na.rm = TRUE)

# 性別毎で分けていたデータフレームをマージ
test <- rbind(test_male,test_female)

# FareがNAの行を確認
test[which(is.na(test$Fare)),]

# Pclassが3かつEmbarkedがSかつSibSpが0かつParchが0の乗客のFare（中央値）を確認
test_Fare_is_null_condition <- subset(test,test$Pclass == 1 & test$Embarked == 'S' & test$SibSp == 0 & test$Parch == 0)
median(test_Fare_is_null_condition$Fare,na.rm = TRUE)

# Fareを上記条件の中央値で補完する
test$Fare[is.na(test$Fare)] <- 30.5

# 因子型へ変更
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)

test$Pclass <- as.factor(test$Pclass)
test$Sex <- as.factor(test$Sex)
test$Embarked <- as.factor(test$Embarked)

# ダミー変数用の変数を選択
train_dummy_cols <- train %>% dplyr::select(Pclass,Sex,Embarked)
test_dummy_cols <- test %>% dplyr::select(Pclass,Sex,Embarked)

# ダミー変数以外の変数を選択
train_not_dummy_cols <- train %>% dplyr::select(PassengerId,Survived,Name,Age,SibSp,Parch,Ticket,Fare,Cabin)
test_not_dummy_cols <- test %>% dplyr::select(PassengerId,Name,Age,SibSp,Parch,Ticket,Fare,Cabin)

# ダミー変数の作成
train_dummies <- makedummies(train_dummy_cols,basal_level = TRUE) 
test_dummies <- makedummies(test_dummy_cols,basal_level = TRUE) 

# データフレームをマージ
train <- cbind(train_dummies,train_not_dummy_cols)
test <- cbind(test_dummies,test_not_dummy_cols)

# SibSpとParchで家族の人数を説明変数に加える
train$Family_size <- train$SibSp + train$Parch +1
test$Family_size <- test$SibSp + test$Parch +1
train <- train %>% mutate(isAlone = ifelse(Family_size == 1,1,0))
test <- test %>% mutate(isAlone = ifelse(Family_size == 1,1,0))
train$isAlone <- as.factor(train$isAlone)
test$isAlone <- as.factor(test$isAlone)

# ロジスティック回帰モデルによる予測

model2 <- glm(Survived~Pclass_1+Pclass_2+Sex_male+Embarked_C+Embarked_Q+Age+Fare+SibSp+Parch+isAlone,
              data = train,
              family = binomial)

# ロジスティック回帰の結果を表示
summary(model2)

# VIFを確認
vif(model2)

# Pclass_1 / Pclass_2 / Sex_male / Age / SibSp / isAlone の変数が他の変数と比較し、生死に影響を与えているとわかる。
# 上記の変数から読み取れることとしては以下のことが考えられる。
# Pclass_1 / Pclass_2の回帰係数より、チケットのクラスが上がるほど生存確率が上がる。
# Sex_male の回帰係数より、女性の方が生存確率が上がる。女性や子供が優先的に救助ボートに乗れた？（映画通りなのかと思われる）
# Ageの回帰係数より、年齢が低いほど生存確率が上がる。上記同様に子供は優先して救助ボートに乗れた可能性が高い。
# SibSpの回帰係数より、同乗している兄弟 / 配偶者の数が少ないほど生存確率が上がる。行動をする人数が少ないほど動きやすかった？
# isAloneの回帰係数より一人で行動するよりかは複数人いた方が生存確率が上がる。
# 上記と矛盾する点があるが、複数人の中でも行動しやすい / 生き残りやすい人数の閾値があるのではないかと思われる。

ypred <- predict(model2,newdata = test , type="response")

ypred_flag<-ifelse(ypred > 0.5, 1, 0)
solution <- data.frame(PassengerId = test$PassengerId,Survived = ypred_flag)
write.csv(solution,file = "submit_01.csv",row.names = F)

# スコア：0.75598

