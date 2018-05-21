setwd("C:/Users/marti")
directorio="C:/Users/marti/specdata"
completos <- function(directorio, id = 1:332){
    nobs<-vector("numeric")
    for (i in id ){
        nombre <- formatC(i,width=3,flag="0")
        archivo <- paste("specdata/",nombre,".csv",sep="")
        datos<- read.csv(file=archivo, sep = ",", header = T)
        complet<-directorio[complete.cases(datos)]
        nobs <- c(nobs,length(complet))
    }
    resultado<-data.frame(id=c(1:length(nobs)),nobs)
    resultado
}

completos(directorio,1)
completos(directorio,c(2,4,8,10,12))
completos(directorio,30:25)
completos(directorio,3)
