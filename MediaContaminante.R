setwd("C:/Users/marti")
mediacontaminante <- function(directorio, contaminante, id = 1:332) {
    final <- vector("numeric")
    for (i in id ){
        nombre <- formatC(i,width=3,flag="0")
        archivo <- paste("specdata/",nombre,".csv",sep="")
        datos<- read.csv(file=archivo, sep = ",", header = T)
        if (contaminante=="sulfate"){
            datfin<-datos$sulfate   
        } else if (contaminante=="nitrate"){
            datfin<-datos$nitrate
        }
        final <- c(final, datfin)
        promedio <- mean(final,na.rm=TRUE)
    }
    promedio
}

mediacontaminante(directorio,"nitrate",70:72)