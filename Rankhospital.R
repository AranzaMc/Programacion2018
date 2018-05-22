setwd("C:/Users/marti/Calidad de Hospitales")
rankhospital <- function(estado, resultado,num="mejor") {
    datos<-read.table("C:/Users/marti/Calidad de Hospitales/outcome-of-care-measures.csv",sep = ",",header=T)
    datos.nombre.filtrados<-datos$Hospital.Name[datos$State==estado]
    if (length(datos.nombre.filtrados)<1) {
        stop("Estado inválido")
    } else {
        if (resultado=="ataque"){
            datos.ataque.filtrados<-datos$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[datos$State==estado]
            datoslimpios<-data.frame(nombre=datos.nombre.filtrados,ataque=datos.ataque.filtrados)
            wow<-datoslimpios$nombre[!datoslimpios$ataque=="Not Available"]
            wow1<-datoslimpios$ataque[!datoslimpios$ataque=="Not Available"]
                vec<-as.numeric(as.character(wow1))
                r<-min(vec)
                s<-max(vec)
                datosmaslimpios<-data.frame(Nombre=wow,Ataques=vec)
                orden<-datosmaslimpios[order(datosmaslimpios$Ataques,datosmaslimpios$Nombre),]
                orden
                datofin<-data.frame(orden,Numero=(c(1:length(wow))))
                datofin
                resultadomin<-datosmaslimpios$Nombre[datosmaslimpios$Ataques==r]
                resultadomax<-datosmaslimpios$Nombre[datosmaslimpios$Ataques==s]
                if (num=="mejor"){
                    resultadomin
                } else if (num=="peor"){
                    resultadomax
                } else{
                    resultado<-datofin$Nombre[datofin$Numero==num] 
                    resultado
                }   
        } else if (resultado=="falla"){
            datos.falla.filtrados<-datos$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[datos$State==estado]
            datoslimpios<-data.frame(nombre=datos.nombre.filtrados,falla=datos.falla.filtrados)
            wow2<-datoslimpios$nombre[!datoslimpios$falla=="Not Available"]
            wow21<-datoslimpios$falla[!datoslimpios$falla=="Not Available"]
            vec<-as.numeric(as.character(wow21))
            r<-min(vec)
            s<-max(vec)
            datosmaslimpios<-data.frame(Nombre=wow2,Fallas=vec)
            orden<-datosmaslimpios[order(datosmaslimpios$Fallas,datosmaslimpios$Nombre),]
            orden
            datofin<-data.frame(orden,Numero=(c(1:length(wow2))))
            datofin
            resultadomin<-datosmaslimpios$Nombre[datosmaslimpios$Fallas==r]
            resultadomax<-datosmaslimpios$Nombre[datosmaslimpios$Fallas==s]
            if (num=="mejor"){
                resultadomin
            } else if (num=="peor"){
                resultadomax
            } else{
                resultado<-datofin$Nombre[datofin$Numero==num] 
                resultado
            }
        } else if (resultado=="neumonia"){
            datos.neumo.filtrados<-datos$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[datos$State==estado]
            datoslimpios<-data.frame(nombre=datos.nombre.filtrados,neumo=datos.neumo.filtrados)
            wow2<-datoslimpios$nombre[!datoslimpios$neumo=="Not Available"]
            wow21<-datoslimpios$neumo[!datoslimpios$neumo=="Not Available"]
            vec<-as.numeric(as.character(wow21))
            as.numeric(as.character(wow21))
            r<-min(vec)
            s<-max(vec)
            datosmaslimpios<-data.frame(Nombre=wow2,Neumonia=vec)
            #ColClasses
            orden<-datosmaslimpios[order(datosmaslimpios$Neumonia,datosmaslimpios$Nombre),]
            orden
            datofin<-data.frame(orden,Numero=(c(1:length(wow2))))
            datofin
            resultadomin<-datosmaslimpios$Nombre[datosmaslimpios$Neumonia==r]
            resultadomax<-datosmaslimpios$Nombre[datosmaslimpios$Neumonia==s]
            if (num=="mejor"){
                resultadomin
            } else if (num=="peor"){
                resultadomax
            } else{
                resultado<-datofin$Nombre[datofin$Numero==num] 
                resultado
            }
            
        } else {
            stop("Resultado inválido")
        }
    }
}
rankhospital("TX","falla", 4)
rankhospital("MD","ataque", "peor")
rankhospital("MN","ataque", 5000)