# OPCION 1 DEL MENU
registrarCandidato<-function(candidatos,votos){
    limite<-10;
    if(length(candidatos)>=limite){
        print(paste("Solo pueden registrar ", limite, " candidatos"));
        flush.console();
        return(candidatos);
    }else{
        can<-readline("Nombre del candidato: ");
        candidatos<-c(candidatos,can);
        votos<-c(votos,0);
        assign("candidatos", candidatos, envir = .GlobalEnv)
        assign("votos", votos, envir = .GlobalEnv)
        return (0);
    }
}
#Opcion 2 DEL MENU
reiniciarVotos<-function(votos){
    for(i in 1:length(votos)){
        
        votos[i]<-0
    }
    assign("votos", votos, envir = .GlobalEnv)
}

# OPCION 3 DEL MENU
listarCandidatos<-function(candidatos){
    print("LISTA DE CANDIDATOS")
    for(can in candidatos){
        print(can);
    }
    flush.console();
    return(0);
}

# OPCION 4 DEL MENU
votar<-function(candidatos,votos){
    print("LISTA DE CANDIDATOS")
    for(i in 1:length(candidatos)){
        print(paste(i," - ",candidatos[i]));
    }
    flush.console();
    opt<-readline("Elija su candidato: ");
    opt<-as.numeric(opt);
    
    if (opt > length(candidatos) || opt < 0) {
        print("Opcion Incorrecta")
        return(0)
    }
    
    votos[opt]<-votos[opt]+1;
    assign("votos", votos, envir = .GlobalEnv)
    votoAzar(votos);
    return(1);
}

votoAzar<-function(votos){
    num<-length(votos)
    v<-sample(1:num,1);
    votos[v]<-votos[v]+1;
    assign("votos", votos, envir = .GlobalEnv)
    return(0);
}

mostrarVotos<-function(candidatos,votos){
    for(i in 1:length(candidatos)){
        print(paste(candidatos[i],":", votos[i]));            
        flush.console();
    }
}

ordenarCandidatos<-function(candidatos,votos){
    for(i in 1:length(votos)){
        for(j in 1:length(votos)){
            if(votos[i]>votos[j]){
                temp<-votos[i]
                votos[i] <- votos[j]
                votos[j]<-temp
                
                temp2<-candidatos[i]
                candidatos[i]<- candidatos[j]
                candidatos[j]<- temp2
            }
        }
    }
    assign("votos", votos, envir = .GlobalEnv)
    assign("candidatos", candidatos, envir = .GlobalEnv)
}

graficar <- function(votos,candidatos)
{ 
    
    names(votos)=candidatos
    #barplot(votos,col=c("orange","blue"),
    #       legend.text=candidatos,ylim=c(0,0.8),main="Votos",
    #       ylab ="Numero de Votos",las=1);
    barplot(votos, col=c("blue"),main="Elecciones",legend.text=c("Azul"),ylab ="Cantidad Votos",xlab ="Candidatos");
    return;
} 

candidatos<-c("Voto en Blanco")
votos<-c(0);

print("MENU DE OPCIONES");
print("1 Registrar candidato");
print("2 Reiniciar votos");
print("3 Listar candidatos");
print("4 Usuario ingresa un voto");

print("-1 Salir");

while(1){
    opt<-readline("Elija una opcion: ");
    if(opt==1){
        registrarCandidato(candidatos,votos);
    }
    if(opt==2){
        reiniciarVotos(votos);
        mostrarVotos(candidatos,votos);
    }
    if(opt==3){
        listarCandidatos(candidatos);
    }
    if(opt==4){
        r <- 0
        while(r == 0)
            r <- votar(candidatos,votos)
        mostrarVotos(candidatos,votos)
        graficar(votos, candidatos)
    }
    
    if(opt==-1){
        ordenarCandidatos(candidatos,votos)
        mostrarVotos(candidatos,votos)
        if(votos == 0){
            print("No hubo un Ganador")
        }else{
            print(paste("El Ganador es",candidatos[1],"Con",votos[1],"Votos"))
            
        }
        
        colores <- c()
        for (i in 1:length(votos)) {
            if (i == 1) {
                colores <- c(colores,"red")
            } else{
                colores <- c(colores,"blue")
            }
          
                names(votos)=candidatos
                barplot(votos, col=colores,main="Elecciones",legend.text=c("Rojo","Azul"),ylab ="Cantidad de Votos",xlab ="Candidatos")
                return;
            }
            
        }
        return (0)      
    }
    





