setwd("C:/Users/marti/Calidad de Hospitales")
rankingcompleto <- function(resultado,num="mejor") {
    todosest<-c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UT","VT","VI","VA","WA","WV","WI","WY","GU")
    estadofinal<-vector("character")
    nombrefinal<-vector("character")
    finalisimo<-data.frame()
    datos<-read.table("C:/Users/marti/Calidad de Hospitales/outcome-of-care-measures.csv",sep = ",",header=T)
    datos.estado<-datos$State
    datos.nombre<-datos$Hospital.Name
    if (resultado=="ataque"){
        datos.ataque<-datos$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
        datoslimpios<-data.frame(estado=datos.estado, nombre=datos.nombre,ataque=datos.ataque)
        wow<-datoslimpios$nombre[!datoslimpios$ataque=="Not Available"]
        wow1<-datoslimpios$ataque[!datoslimpios$ataque=="Not Available"]
        wow2<-datoslimpios$estado[!datoslimpios$ataque=="Not Available"]
        datosmaslimpios<-data.frame(Estado=wow2,Nombre=wow,Ataques=wow1)
        for (i in todosest){
            estado<-i
            po<-datosmaslimpios$Estado[datosmaslimpios$Estado==estado]
            po1<-datosmaslimpios$Nombre[datosmaslimpios$Estado==estado]
            po2<-datosmaslimpios$Ataques[datosmaslimpios$Estado==estado]
            porestado<-data.frame(Estado=po,Nombre=po1,Ataques=po2)
            orden<-porestado[order(porestado$Ataques,porestado$Nombre),]
            porestadofin<-data.frame(orden,Numero=(c(1:length(po))))
            if (num=="mejor"){
                resulta<-porestadofin$Nombre[porestadofin$Numero=="1"]
                estadofinal<-as.factor(resulta)
                resulta2<-porestadofin$Estado[porestadofin$Numero=="1"]
                nombrefinal<-as.factor(resulta2)
                nuevasfilas<-data.frame(Nombre=estadofinal,Estado=nombrefinal)
                finalisimo=rbind(finalisimo,nuevasfilas)
            }else if (num=="peor"){
                resulta<-porestadofin$Nombre[porestadofin$Numero==length(po)]
                estadofinal<-as.factor(resulta)
                resulta2<-porestadofin$Estado[porestadofin$Numero==length(po)]
                nombrefinal<-as.factor(resulta2)
                nuevasfilas<-data.frame(Nombre=estadofinal,Estado=nombrefinal)
                finalisimo=rbind(finalisimo,nuevasfilas)
            } else{
                resulta<-porestadofin$Nombre[porestadofin$Numero==num]
                estadofinal<-as.factor(resulta)
                resulta2<-porestadofin$Estado[porestadofin$Numero==num]
                nombrefinal<-as.factor(resulta2)
                nuevasfilas<-data.frame(Nombre=estadofinal,Estado=nombrefinal)
                finalisimo=rbind(finalisimo,nuevasfilas)
            }
        }
        finalisimo
    } else if (resultado=="falla"){
        datos.falla<-datos$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
        datoslimpios<-data.frame(estado=datos.estado, nombre=datos.nombre,falla=datos.falla)
        wow<-datoslimpios$nombre[!datoslimpios$falla=="Not Available"]
        wow1<-datoslimpios$falla[!datoslimpios$falla=="Not Available"]
        wow2<-datoslimpios$estado[!datoslimpios$falla=="Not Available"]
        datosmaslimpios<-data.frame(Estado=wow2,Nombre=wow,Fallas=wow1)
        for (i in todosest){
            estado<-i
            po<-datosmaslimpios$Estado[datosmaslimpios$Estado==estado]
            po1<-datosmaslimpios$Nombre[datosmaslimpios$Estado==estado]
            po2<-datosmaslimpios$Fallas[datosmaslimpios$Estado==estado]
            porestado<-data.frame(Estado=po,Nombre=po1,Fallas=po2)
            orden<-porestado[order(porestado$Fallas,porestado$Nombre),]
            porestadofin<-data.frame(orden,Numero=(c(1:length(po))))
            if (num=="mejor"){
                resulta<-porestadofin$Nombre[porestadofin$Numero=="1"]
                estadofinal<-as.factor(resulta)
                resulta2<-porestadofin$Estado[porestadofin$Numero=="1"]
                nombrefinal<-as.factor(resulta2)
                nuevasfilas<-data.frame(Nombre=estadofinal,Estado=nombrefinal)
                finalisimo=rbind(finalisimo,nuevasfilas)
            }else if (num=="peor"){
                resulta<-porestadofin$Nombre[porestadofin$Numero==length(po)]
                estadofinal<-as.factor(resulta)
                resulta2<-porestadofin$Estado[porestadofin$Numero==length(po)]
                nombrefinal<-as.factor(resulta2)
                nuevasfilas<-data.frame(Nombre=estadofinal,Estado=nombrefinal)
                finalisimo=rbind(finalisimo,nuevasfilas)
            } else{
                resulta<-porestadofin$Nombre[porestadofin$Numero==num]
                estadofinal<-as.factor(resulta)
                resulta2<-porestadofin$Estado[porestadofin$Numero==num]
                nombrefinal<-as.factor(resulta2)
                nuevasfilas<-data.frame(Nombre=estadofinal,Estado=nombrefinal)
                finalisimo=rbind(finalisimo,nuevasfilas)
            }
        }
        finalisimo
    }else if (resultado=="neumonia"){
        datos.neumonia<-datos$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
        datoslimpios<-data.frame(estado=datos.estado, nombre=datos.nombre,neumonia=datos.neumonia)
        wow<-datoslimpios$nombre[!datoslimpios$neumonia=="Not Available"]
        wow1<-datoslimpios$neumonia[!datoslimpios$neumonia=="Not Available"]
        wow2<-datoslimpios$estado[!datoslimpios$neumonia=="Not Available"]
        datosmaslimpios<-data.frame(Estado=wow2,Nombre=wow,Neumonia=wow1)
        for (i in todosest){
            estado<-i
            po<-datosmaslimpios$Estado[datosmaslimpios$Estado==estado]
            po1<-datosmaslimpios$Nombre[datosmaslimpios$Estado==estado]
            po2<-datosmaslimpios$Neumonia[datosmaslimpios$Estado==estado]
            porestado<-data.frame(Estado=po,Nombre=po1,Neumonia=po2)
            orden<-porestado[order(porestado$Neumonia,porestado$Nombre),]
            porestadofin<-data.frame(orden,Numero=(c(1:length(po))))
            if (num=="mejor"){
                resulta<-porestadofin$Nombre[porestadofin$Numero=="1"]
                estadofinal<-as.factor(resulta)
                resulta2<-porestadofin$Estado[porestadofin$Numero=="1"]
                nombrefinal<-as.factor(resulta2)
                nuevasfilas<-data.frame(Nombre=estadofinal,Estado=nombrefinal)
                finalisimo=rbind(finalisimo,nuevasfilas)
            }else if (num=="peor"){
                resulta<-porestadofin$Nombre[porestadofin$Numero==length(po)]
                estadofinal<-as.factor(resulta)
                resulta2<-porestadofin$Estado[porestadofin$Numero==length(po)]
                nombrefinal<-as.factor(resulta2)
                nuevasfilas<-data.frame(Nombre=estadofinal,Estado=nombrefinal)
                finalisimo=rbind(finalisimo,nuevasfilas)
            } else{
                resulta<-porestadofin$Nombre[porestadofin$Numero==num]
                estadofinal<-as.factor(resulta)
                resulta2<-porestadofin$Estado[porestadofin$Numero==num]
                nombrefinal<-as.factor(resulta2)
                nuevasfilas<-data.frame(Nombre=estadofinal,Estado=nombrefinal)
                finalisimo=rbind(finalisimo,nuevasfilas)
            }
        }
finalisimo
    }
}

head(rankingcompleto("ataque",20),10)
tail(rankingcompleto("neumonia","peor"),3)
tail(rankingcompleto("falla","mejor"),10)