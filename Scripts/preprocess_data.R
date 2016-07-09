library(readr)
require("data.table")
require(dplyr)
# train <- fread("Data/train.csv",header=TRUE,colClasses = c(rep("integer",7),rep(c("numeric","integer"),2)))


train <- fread("Data/train.csv", header = TRUE,
               select = c("Cliente_ID", "Producto_ID", "Agencia_ID",
                          "Semana", "Ruta_SAK", "Demanda_uni_equil"))
test <- fread('Data//test.csv',header = TRUE,
              select = c("Cliente_ID", "Producto_ID", "Agencia_ID",
                         "Semana", "Ruta_SAK"))
head(train)


# table(train$Producto_ID)
# product_return <- filter(train$Producto_ID, train$Dev_uni_proxima > 0 )
train$log_demand = log1p(train$Demanda_uni_equil) 

#set a table key to enable fast aggregations
setkey(train, Producto_ID, Cliente_ID, Ruta_SAK)
setkey(test, Producto_ID, Cliente_ID, Ruta_SAK)

print("Computing means")
mean_total <- mean(train$log_demand) #overall mean
#mean by product
mean_P <-  train[, .(MP = mean(log_demand)), by = .(Producto_ID)]
#mean by product and ruta
mean_PR <- train[, .(MPR = mean(log_demand)), by = .(Producto_ID, Ruta_SAK)] 
#mean by product, client, agencia
mean_PCA <- train[, .(MPCA = mean(log_demand)), by = .(Producto_ID, Cliente_ID, Agencia_ID)]

print("Merging means with test set")
submit <- merge(test, mean_PCA, all.x = TRUE, by = c("Producto_ID", "Cliente_ID", "Agencia_ID"))
submit <- merge(submit, mean_PR, all.x = TRUE, by = c("Producto_ID", "Ruta_SAK"))
submit <- merge(submit, mean_P, all.x = TRUE, by = "Producto_ID")

# Now create Predictions column;
submit$Pred <- expm1(submit$MPCA)*0.9+0.1
submit[is.na(Pred)]$Pred <- expm1(submit[is.na(Pred)]$MPR)*0.9+0.1
submit[is.na(Pred)]$Pred <- expm1(submit[is.na(Pred)]$MP)*0.9+0.1
submit[is.na(Pred)]$Pred <- expm1(mean_total)

print("Write out submission file")
# now relabel columns ready for creating submission
setnames(submit,"Pred","Demanda_uni_equil")
# Any results you write to the current directory are saved as output.
write.csv(submit[,.(id,Demanda_uni_equil)],"submit_mean_by_Agency_Ruta_Client.csv", row.names = FALSE)
print("Done!")
