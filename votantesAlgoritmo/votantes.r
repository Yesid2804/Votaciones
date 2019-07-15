3### OPCION 1 DEL MENU
registrarCandidato<-function(candidatos,votos){
    limite<-3;
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

#OPCION 3 DEL MENU
listarCandidatos<-function(candidatos){
    print("LISTA DE CANDIDATOS")
    for(can in candidatos){
        print(can);
    }
    flush.console();
    return(0);
}


#OPCION 4 DEL MENU
votar<-function(candidatos,votos){
    print("LISTA DE CANDIDATOS")
    for(i in 1:length(candidatos)){
        print(paste(i," - ",candidatos[i]));
    }
    flush.console();
    opt<-readline("Elija su candidato: ");
    opt<-as.numeric(opt);
    
    votos[opt]<-votos[opt]+1;
    assign("votos", votos, envir = .GlobalEnv)
    votoAzar(votos);
    return(0);
}

votoAzar<-function(votos){
    num<-length(votos)
    v<-sample(1:num,1);
    votos[v]<-votos[v]+1;
    assign("votos", votos, envir = .GlobalEnv)
    return(0);
}


candidatos<-c("blanco","Yesid")
votos<-c(0,0);

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
    if(opt==3){
        listarCandidatos(candidatos);
    }
    if(opt==4){
        votar(candidatos,votos);
    }
    
    if(opt==-1){
        print(votos);
        return (0)
    }
    
    plot(votos,col="darkolivegreen1");
    Sys.sleep(0)
}
