## Landcover Variables for 100m riparian buffer area on all 88 streams - 2018 data
## Landcover data: NLCD 2011

cover <- as.data.frame(read.csv(file.choose()))
head(cover)
hist(cover$PctDeveloped)
hist(cover$PctForest)
hist(cover$PctGrassland)
hist(cover$PctPasture)
hist(cover$PctCrops)

LogCover <- log(1+cover[,3:7])
hist(LogCover$PctDeveloped)
hist(LogCover$PctForest)
hist(LogCover$PctGrassland)
hist(LogCover$PctPasture)
hist(LogCover$PctCrops)

cor(LogCover)

write.table(LogCover,"LogLandcover18.txt",sep="\t",row.names=T, col.names = T)
