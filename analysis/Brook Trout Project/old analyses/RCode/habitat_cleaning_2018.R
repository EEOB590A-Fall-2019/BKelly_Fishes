### Habitat Analysis ###
# 2018 Field Data: 88 Streams #

# Load Data
habitat <- as.data.frame(read.csv(file.choose()))
head(habitat)

hab <- habitat[,7:86]
head(hab)

#calculate correlation numbers for covariates
correlation <- as.data.frame(round(cor(hab),2))

#export correlation dataframe
?write.table
write.table(correlation,"HabCor18.txt",sep="\t",row.names=T, col.names = T)

names(hab)
hist(hab$temp)
hist(hab$do)
plot(hab)

?matplot
matplot(y=hab[,1:3], type = "l", lty = 1)

loghab <- log(1+hab)
names(loghab)
head(loghab)

write.table(loghab,"LogTransformedHabVars18.txt",sep="\t",row.names=T, col.names = T)
