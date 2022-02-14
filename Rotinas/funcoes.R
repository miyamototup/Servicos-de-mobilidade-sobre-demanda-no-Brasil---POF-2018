
despesas<-function(produtos,despesa_individual){
  
  
  #Função para calcular moda
  statmod <- function(x) {
    z <- table(as.vector(x))
    names(z)[z == max(z)]
  }
  
  ldespind=list()
  ldespind2=list()
  
  ldespind=split(despesa_individual,paste(despesa_individual$ESTRATO_POF,despesa_individual$COD_UPA,
                                          despesa_individual$NUM_DOM,despesa_individual$COD_INFORMANTE))
  
  
  for (i in 1:length(ldespind)){
    
    for (j in 1:(dim(produtos)[1]))  {
      #forma de criar coluna que eu não conhecia aé então, usando df[] ao invés de df$  
      #Aí já puxo o nome diretamente do cadastro de produtos  
      #Parte 1 = identificar produtos de interesse, formas de pagamento e valor gasto  
      ldespind[[i]][produtos[j,2]]=0
      ldespind[[i]][[produtos[[j,2]]]][ldespind[[i]]$V9001==produtos[j,1]]=1
      ldespind[[i]][paste(produtos[j,2],'fpgto',sep='_')]=NA
      ldespind[[i]][[paste(produtos[j,2],'fpgto',sep='_')]][ldespind[[i]]$V9001==produtos[j,1]]=ldespind[[i]]$V9002[ldespind[[i]]$V9001==produtos[j,1]]
      ldespind[[i]][paste(produtos[j,2],'valor',sep='_')]=0
      ldespind[[i]][[paste(produtos[j,2],'valor',sep='_')]][ldespind[[i]]$V9001==produtos[j,1]]=ldespind[[i]]$V8000[ldespind[[i]]$V9001==produtos[j,1]]
      
      
      #Parte 2 = Identificar quem utilizou ou não o serviço de interesse, quantas vezes utilizou,
      # forma de pagamento mais frequente e o valor total que foi gasto
      #As variáveis foram criadas da colujna 25 em diante
      
      #Utilizou o produto/serviço?
      V=max(ldespind[[i]][[produtos[[j,2]]]],na.rm=T)
      #Quantas vezes utilizou?
      V_qtde=sum(ldespind[[i]][[produtos[[j,2]]]],na.rm=T)
      #Forma de pagamento mais frequente?
      V_fpgto=statmod(ldespind[[i]][[paste(produtos[j,2],'fpgto',sep='_')]])
      if (is.null(V_fpgto)){V_fpgto=NA}
      #Valor total gasto no período com o produto/serviço?
      V_valor=sum(ldespind[[i]][[paste(produtos[j,2],'valor',sep='_')]],na.rm=T)
      
      
      
      desp=as.data.frame(cbind(V,V_qtde,V_fpgto,V_valor),stringsAsFactors = T)
      colnames(desp)=c(produtos[j,2],paste(produtos[j,2],'qtde',sep='_'),
                       paste(produtos[j,2],'fpgto',sep='_'),
                       paste(produtos[j,2],'valor',sep='_'))
      
      #Legal! Com essa solução consegui acumular os resultados sem precisar criar
      #outro contador
      if(j==1){
        desp2=desp
      } else{
        desp2=cbind(desp2,desp)
      }
      
    }
    
    desp=ldespind[[i]][1,c(1:7,24)]
    desp=cbind(desp,desp2)
    ldespind2[[i]]=desp
  }
  
  despesa_individual=do.call(rbind.data.frame,ldespind2)
  return(despesa_individual)
  
}

#Função para obter rendimentos, tempo de deslocamento e horas de trabalho

rendtrab=function(rendimento_trabalho){
  
  lrendind=split(rendimento_trabalho,paste(rendimento_trabalho$ESTRATO_POF,rendimento_trabalho$COD_UPA,
                                           rendimento_trabalho$NUM_DOM,rendimento_trabalho$COD_INFORMANTE))
  for (i in 1:length(lrendind)){
    lrendind[[i]]$nempreg=dim(lrendind[[i]])[1]
    colnames(lrendind[[i]])[5:7]=c('salario','horastrab','tempdesloc')
    if (dim(lrendind[[i]])[1]>1){
      lrendind[[i]]=lrendind[[i]][order(-lrendind[[i]]$tempdesloc),]
      salario=sum(lrendind[[i]]$salario,na.rm=T)
      horastrab=sum(lrendind[[i]]$horastrab,na.rm=T)
      emp=lrendind[[i]][1,c(1:4,7,8)]
      emp=cbind(emp,salario,horastrab)
      emp=emp[,c(1:4,7,8,5,6)]
      lrendind[[i]]=emp
      
    }
  }
  rendimento_trabalho=do.call(rbind.data.frame,lrendind)
  return(rendimento_trabalho)
}

#Captar se o indivíduo possui outros rendimentos e o valor deles
orend=function(outros_rendimentos){
  
  lout_rend=split(outros_rendimentos,paste(outros_rendimentos$ESTRATO_POF,outros_rendimentos$COD_UPA,
                                           outros_rendimentos$NUM_DOM,outros_rendimentos$COD_INFORMANTE))
for (i in 1:length(lout_rend)){
  
  out_rend=lout_rend[[i]][1,c(1:4)]
  outros_rend=1
  voutros_rend=sum(lout_rend[[i]]$V8500,na.rm = T)
  out_rend=cbind(out_rend,outros_rend,voutros_rend)
  lout_rend[[i]]=out_rend
}
  outros_rendimentos=do.call(rbind.data.frame,lout_rend)
  return(outros_rendimentos)
  }
