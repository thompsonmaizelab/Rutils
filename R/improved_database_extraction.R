#put db files in "database" folder within working directory
setwd("C:/Users/PSM.AThompsonLab/Desktop/data_to_merge")

#database files need to be in your working directory in a folder titled 'databases' for this code to work right
#merged file and conflicts file will be put in the working directory
#to check working directory is set right
getwd()

#may need to install some additional packages before RSQLite will load
install.packages("RSQLite")
library(RSQLite)

#connect to a database driver
sqlite.driver <- dbDriver("SQLite")

#list of .db files
#should make a list of database files in the working directory
filenames<-list.files("./databases",pattern="*.db")

#initiate merged db
all.db.traits<-data.frame(rid=integer(),parent=factor(),userValue=numeric())

#read in all databases and append to merged
setwd("./databases")

for (filex in filenames){
    temp<-dbConnect(sqlite.driver, dbname=filex)
    temptraits<-dbReadTable(temp,"user_traits")
    dbDisconnect(temp)
    temp<-data.frame(temptraits$rid,temptraits$parent,temptraits$userValue)
    colnames(temp)<-c("rid","parent","userValue")
    all.db.traits<-rbind(all.db.traits,temp)
}
rm(temp)
rm(temptraits)
setwd("../")

# Get unique plot_ids
dataUnique<-data.frame(as.character(unique(all.db.traits[,"rid"])))
names(dataUnique)<-"rid"
dataUnique<-data.frame(dataUnique[order(dataUnique$rid),])
names(dataUnique)<-"rid"

# Set up data.frame for conflicts
conflictsout<-data.frame(rid=integer(),parent=factor(),userValue=numeric())

# Split based on trait (splits to list)
dataSplit<-split(all.db.traits,all.db.traits$parent)

# Add traits/data as columns
for (i in 1:length(dataSplit)) {
    temp<-dataSplit[[i]]
    temp<-data.frame(temp$rid,temp$parent,temp$userValue)
    colnames(temp)<-c("rid","parent","userValue")
    if(dim(temp)[1]==0) {
        next
    }
    traitName<-temp$parent[1]
    temp<-temp[order(temp[,"rid"],temp[,"userValue"]),]
    temp<-temp[!(temp$userValue==""),]
    conflicts<-temp[(duplicated(temp$rid) | duplicated(temp$rid,fromLast=T)),]
    conflictsout<-rbind(conflictsout,conflicts)
    cat(paste(traitName),"has",nrow(conflicts),"conflicts!","\n")
    temp<-temp[!duplicated(temp$rid),]
    temp<-temp[,-2]
    colnames(temp)<-c("rid", as.character(traitName))                  
    dataUnique<-merge(dataUnique,temp,by="rid",all=TRUE)
}

colnames(dataUnique)[1]<-"plot"
colnames(conflictsout)[1]<-"plot"

# Write data
write.csv(dataUnique,file=paste("databaseTableFormat.csv",sep=""),row.names=F)

# Write conflicts
write.csv(conflictsout,file=paste("databaseConflicts.csv",sep=""),row.names=F)

#to make sure all databases were included
print(filenames)
