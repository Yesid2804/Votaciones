
candidatos<-data.frame("blanco"=0)
can<-"Yan";
#candidatos<-c(votos,"yesid"=10)
candidatos<-c(candidatos,textConnection(can));
print(candidatos)


registrarCandidato<-function(candidatos){
    limite<-3;
    if(length(candidatos)>=limite){
        print(paste("Solo pueden registrar ", limite, " candidatos"));
        flush.console();
        return(candidatos);
    }else{
        can<-readline("Nombre del candidato: ");
        candidatos<-c(candidatos,cant(can)=0);
        #votos<-c(votos,print(can)=0);        
        return (candidatos);
    }
}



listarCandidatos<-function(candidatos){
    print("LISTA DE CANDIDATOS")
    for(can in candidatos){
        print(can);
    }
    opt<-readline("Elija un candidato: ");
    
    
    flush.console();
}

votar<-function(candidatos){
    for(i in 1:length(candidatos)){
        print(paste(i," - ",candidatos[i]));
    }
}

candidatos<-data.frame("blanco"=0)
votos<-c(blanco=0);
print("MENU DE OPCIONES");
print("1 Registrar candidato");
print("2 Reiniciar votos");
print("3 Listar candidatos");
print("4 Usuario ingresa un voto");

print("-1 Salir");

while(1){
    opt<-readline("Elija una opcion: ");
    if(opt==1){
        candidatos<-registrarCandidato(candidatos);
        #candidatos<-resul[1];
        print(candidatos);
        flush.console();
        #votos<-resul[2];
    }
    if(opt==3){
        listarCandidatos(candidatos);
    }
    
    if(opt==-1){
        return (0)
    }
}
