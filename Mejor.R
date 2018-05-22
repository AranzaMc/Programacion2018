setwd("C:/Users/marti/Calidad de Hospitales")
mejor <- function(estado, resultado) {
datos<-read.table("C:/Users/marti/Calidad de Hospitales/outcome-of-care-measures.csv",sep = ",",header=T)
    datos.nombre.filtrados<-datos$Hospital.Name[datos$State==estado]
    if (length(datos.nombre.filtrados)<1) {
        stop("Estado inválido")
    } 
    else {
        if (resultado=="ataque"){
            datos.ataque.filtrados<-datos$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[datos$State==estado]
            datoslimpios<-data.frame(nombre=datos.nombre.filtrados,ataque=datos.ataque.filtrados)
            wow<-datoslimpios$nombre[!datoslimpios$ataque=="Not Available"]
            wow1<-datoslimpios$ataque[!datoslimpios$ataque=="Not Available"]
            vec<-as.numeric(as.character(wow1))
            r<-min(vec)
            datosmaslimpios<-data.frame(Nombre=wow,Ataques=wow1)
            resultado<-datosmaslimpios$Nombre[datosmaslimpios$Ataques==r]
            resultado
        } else if (resultado=="falla"){
            datos.falla.filtrados<-datos$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[datos$State==estado]
            datoslimpios<-data.frame(nombre=datos.nombre.filtrados,falla=datos.falla.filtrados)
            wow2<-datoslimpios$nombre[!datoslimpios$falla=="Not Available"]
            wow21<-datoslimpios$falla[!datoslimpios$falla=="Not Available"]
            vec<-as.numeric(as.character(wow21))
            r<-min(vec)
            datosmaslimpios<-data.frame(Nombre=wow2,Fallas=wow21)
            resultado<-datosmaslimpios$Nombre[datosmaslimpios$Fallas==r]
            resultado
        } else if (resultado=="neumonia"){
            datos.neumo.filtrados<-datos$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[datos$State==estado]
            datoslimpios<-data.frame(nombre=datos.nombre.filtrados,neumo=datos.neumo.filtrados)
            wow2<-datoslimpios$nombre[!datoslimpios$neumo=="Not Available"]
            wow21<-datoslimpios$neumo[!datoslimpios$neumo=="Not Available"]
            vec<-as.numeric(as.character(wow21))
            r<-min(vec)
            datosmaslimpios<-data.frame(Nombre=wow2,Neumonia=wow21)
            resultado<-datosmaslimpios$Nombre[datosmaslimpios$Neumonia==r]
            resultado
        } else {
        stop("Resultado inválido")
        }
    }
}

mejor("TX","falla")
mejor("MD","ataque")
mejor("MD","neumonia")
mejor("BB","ataque")
mejor("NY","atakue")