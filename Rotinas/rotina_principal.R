library(xlsx)
setwd('/Arquivos/Documentos/Pesquisa/MBA - Maira/Rotinas')
source('funcoes.R')

#Definindo o diretório onde as bases MORADOR, DESPESA_INDIVIDUAL E RENDIMENTO_TRABALHO
# estão localizadas
setwd('/Arquivos/Documentos/Pesquisa/MBA - Maira/POF-2017-2018/Dados/Arquivos de dados')


#Despesas individuais
despesa_individual = read.fwf("DESPESA_INDIVIDUAL.txt", widths = c(2,4,1,9,2,1,2,2,2,7,2,10,2
                        ,2,1,1,1,12,10,1,2,14,14,10), na.strings=c(" ")
                        , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA"
                        , "NUM_DOM", "NUM_UC", "COD_INFORMANTE", "QUADRO", "SEQ", "V9001"
                        , "V9002", "V8000", "V9010", "V9011", "V9012"
                        , "V4104", "V4105", "DEFLATOR", "V8000_DEFLA"
                        , "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO"
                        , "PESO", "PESO_FINAL", "RENDA_TOTAL"), dec=".")   


#Produtos que preciso destacar
setwd('/Arquivos/Documentos/Pesquisa/MBA - Maira/POF-2017-2018/Documentacao/Documentos')
produtos=read.xlsx('Cadastro de Produtos.xls',sheetIndex = 1)
#Sei que preciso destacar apenas os itens do quadro 23 e alguns do 41
#porque é onde estão as variáveis de transporte. Vou selecionar todo o quadro 23
#para posteriormente excluir só o que são gastos referentes a transporte não terreste.
#Depois isolo os transportes de interesse (substitutos próximos a uber) mas para uma segunda
#rodada posso obter os gastos totais associados a transporte terrestre


setwd('/Arquivos/Documentos/Pesquisa/MBA - Maira/POF-2017-2018/Dados/Arquivos de dados')
#Quadro 23
produtos23=produtos[produtos$Quadro==23,]
produtos23=produtos23[,c(2,3)]
produtos23$V9001..Código.do.Produto.=as.numeric(levels(produtos23$V9001..Código.do.Produto.)[produtos23$V9001..Código.do.Produto.])
produtos23$V9000..Descrição.do.Produto.=as.character(produtos23$V9000..Descrição.do.Produto.)

#Função despesas foi criada para isolar os gastos relacionados aos produtos de interesse e a forma
#de pagamento mais frequente.
despesa_individual23=despesas(produtos23,despesa_individual)
#Salvar a tabela de despesas individuais na pasta do banco de dados
saveRDS(despesa_individual23,file="desptranspQ23.RDS")


#Quadro 41
#produtos41=produtos[produtos$Quadro==41,]
#produtos41=produtos41[,c(2,3)]
#produtos41$V9001..Código.do.Produto.=as.numeric(levels(produtos41$V9001..Código.do.Produto.)[produtos41$V9001..Código.do.Produto.])
#produtos41$V9000..Descrição.do.Produto.=as.character(produtos41$V9000..Descrição.do.Produto.)

#Função despesas foi criada para isolar os gastos relacionados aos produtos de interesse e a forma
#de pagamento mais frequente.
#despesa_individual41=despesas(produtos41,despesa_individual)
#Salvar a tabela de despesas individuais na pasta do banco de dados
#saveRDS(despesa_individual41,file="desptranspQ41.RDS")




####COnferir depois se os dados são os mesmos. Se for. deletar os antigos
#Carregar dados de despesas de transporte
setwd('/Arquivos/Documentos/Pesquisa/MBA - Maira/POF-2017-2018/Dados/Arquivos de dados')
despesa_individual=readRDS('desptranspQ23.RDS')


####################################
#########Dados dos moradores
morador = read.fwf("MORADOR.txt", widths = c(2,4,1,9,2,1,2,2,1,2,2,4,3,1,1,14,14,10)
                   , na.strings=c(" "), col.names = c("UF", "ESTRATO_POF"
                                                      , "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC"
                                                      , "COD_INFORMANTE", "V0306", "V0401", "V04021", "V04022"
                                                      , "V04023", "V0403", "V0404", "V0405", "PESO", "PESO_FINAL"
                                                      , "RENDA_TOTAL") , dec=".")   

morador=morador[,c(2,4:5,7,8,13:15)]

#Juntar dados de moradores com despesas de transporte
despesa_individual=merge(morador,despesa_individual,by=c("ESTRATO_POF","COD_UPA","NUM_DOM","COD_INFORMANTE"),
                         all.x = F,all.y = T)
colnames(despesa_individual)[5:8]=c("posfamilia","idade","sexo","cor")


#Salvar arquivo de despesas de transporte+individuos. Salvei em csv para corrigir 
#o tipo de variável ao carregar (de factor para numeric)
#write.csv(despesa_individual,file="desptranspindv.csv",row.names = F)

#Carregar arquivos de despesas individuais+ dados do indivíduo
setwd('/Arquivos/Documentos/Pesquisa/MBA - Maira/POF-2017-2018/Dados/Arquivos de dados')
despesa_individual=read.csv('desptranspindv.csv',header=T)


##############################
#Carregar dados de Rendimentos do trabalho
rendimento_trabalho =  read.fwf("RENDIMENTO_TRABALHO.txt", widths = c(2,4,1,9,2,1,2,2,1,1,7
                        ,1,1,1,1,1,1,7,7,7,7,2,2,3,1,12,10,10,10,10,1,1,14,14,10)
                        , na.strings=c(" "), col.names = c("UF", "ESTRATO_POF"
                        , "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE"
                        , "QUADRO", "SUB_QUADRO", "SEQ", "V9001", "V5302", "V53021", "V5303"
                        , "V5304", "V5305", "V5307", "V8500", "V531112", "V531122", "V531132"
                        , "V9010", "V9011", "V5314", "V5315", "DEFLATOR", "V8500_DEFLA"
                        , "V531112_DEFLA", "V531122_DEFLA", "V531132_DEFLA", "COD_IMPUT_VALOR"
                        , "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL", "RENDA_TOTAL"), dec=".")


rendimento_trabalho=rendimento_trabalho[,c(2,4,5,7,18,24,25)]
#Aplicação da função rend_trab
rendimento_trabalho=rendtrab(rendimento_trabalho)



#unir despesa_individual com rendimento_trabalho
despesa_individual=merge(rendimento_trabalho,despesa_individual,
                         by=c("ESTRATO_POF","COD_UPA","NUM_DOM","COD_INFORMANTE"),
                         all.x=F,all.y=T)

#setwd('/Arquivos/Documentos/Pesquisa/MBA - Maira/POF-2017-2018/Dados/Arquivos de dados')
#saveRDS(despesa_individual,file="desp_rend.RDS")


#Carregar dados de despesa individual + rendimentos do trabalho
setwd('/Arquivos/Documentos/Pesquisa/MBA - Maira/POF-2017-2018/Dados/Arquivos de dados')
despesa_individual=readRDS("desp_rend.RDS")


#Outros rendimentos
outros_rendimentos = read.fwf("OUTROS_RENDIMENTOS.txt", widths = c(2,4,1,9,2,1,2,2,2,7,10,10,2
                        ,2,12,10,10,1,1,14,14,10), na.strings=c(" "), col.names = c("UF"
                        , "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC"
                        , "COD_INFORMANTE", "QUADRO", "SEQ", "V9001", "V8500", "V8501"
                        , "V9010", "V9011", "DEFLATOR", "V8500_DEFLA", "V8501_DEFLA"
                        , "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL"
                        , "RENDA_TOTAL"), dec=".")   

outros_rendimentos=outros_rendimentos[,c(2,4,5,7,11)]

#Função para captar se o indivíduo possui outras fontes de rendimento e qual é o valor desses
#rendimentos somados
outros_rendimentos=orend(outros_rendimentos)

#Unir, despesas_individual, drendimento do trabalho e outras fontes de rendimento
despesa_individual=merge(outros_rendimentos,despesa_individual,
                         by=c("ESTRATO_POF","COD_UPA","NUM_DOM","COD_INFORMANTE"),
                         all.x=F,all.y=T)


#setwd('/Arquivos/Documentos/Pesquisa/MBA - Maira/POF-2017-2018/Dados/Arquivos de dados')
#saveRDS(despesa_individual,file="desp_rend_outrend.RDS")


#Regressões quantílicas para avaliar o impacto dos fatores individuais no gastos dos
#indivíduos

library(quantreg)
library(dummies)
#Qunatílica para gastos não deu certo :/

setwd('/Arquivos/Documentos/Pesquisa/MBA - Maira/POF-2017-2018/Dados/Arquivos de dados')
despesa_individual=readRDS('desp_rend_outrend.RDS')

#Tratamento de algumas variáveis para ajuste do modelo
despesa_individual$outros_rend[is.na(despesa_individual[["outros_rend"]])]=0
#mulher 0 e homem 1
despesa_individual[["sexo"]][despesa_individual[["sexo"]]==2]=0
#urbano 1 e rural 0
despesa_individual$TIPO_SITUACAO_REG[despesa_individual[["TIPO_SITUACAO_REG"]]==2]=0
#Salário igual a logaritmo do salário
despesa_individual$salario=log(despesa_individual$salario)
despesa_individual$salario[is.infinite(despesa_individual$salario)]=NA

# Quantidade de empregos por indivíduos
# 1     2     3     4     5     6     7     8    10 
#67495 11421  1288   190    36     9     1     2     1 
#Agrupar de 4 a 10 empregos antes de criar binárias
despesa_individual$nempreg[despesa_individual$nempreg==5|despesa_individual$nempreg==6|
                             despesa_individual$nempreg==7|despesa_individual$nempreg==8|
                             despesa_individual$nempreg==9|despesa_individual$nempreg==10]=4
#Binárias
despesa_individual=cbind(despesa_individual,dummy(despesa_individual$nempreg,sep = '_nempreg_'))


#Cor
# 1     2     3     4     5     9 
#48817 13664   597 63954   586   238 
#Vou agrupar amaelos e  indígenas em 3
despesa_individual$cor[despesa_individual$cor==5]=3
#Sem declaração será NA
despesa_individual$cor[despesa_individual$cor==9]=NA
#Binárias
despesa_individual=cbind(despesa_individual,dummy(despesa_individual$cor, sep='_cor_'))



#Tempo de deslocamento até o trabalho
#    1     2     3     4     5     9 
#25038 36608 11558  5047  1317   875 
#Agrupar 4 e 5 
despesa_individual$tempdesloc[despesa_individual$tempdesloc==5]=4
#Sem declaração será NA
despesa_individual$tempdesloc[despesa_individual$tempdesloc==9]=NA
#Binárias
despesa_individual=cbind(despesa_individual,dummy(despesa_individual$tempdesloc,sep="_tempdesloc_"))



#Regiões
#Agrupar estados por região e depois criar binárias
despesa_individual$regiao[despesa_individual$UF==11|despesa_individual$UF==12|despesa_individual$UF==13|
                            despesa_individual$UF==14|despesa_individual$UF==15|despesa_individual$UF==16|
                            despesa_individual$UF==17]='norte'

despesa_individual$regiao[despesa_individual$UF==21|despesa_individual$UF==22|despesa_individual$UF==23|
                            despesa_individual$UF==24|despesa_individual$UF==25|despesa_individual$UF==26|
                            despesa_individual$UF==27|despesa_individual$UF==28|despesa_individual$UF==29]='nordeste'

despesa_individual$regiao[despesa_individual$UF==31|despesa_individual$UF==31|despesa_individual$UF==33|
                            despesa_individual$UF==35]='sudeste'

despesa_individual$regiao[despesa_individual$UF==41|despesa_individual$UF==42|despesa_individual$UF==43]="sul"

despesa_individual$regiao[despesa_individual$UF==50|despesa_individual$UF==51|despesa_individual$UF==52|
                            despesa_individual$UF==53]='centro-oeste'

#centro-oeste      nodeste        norte      sudeste          sul 
#16317        44078        17620        26455        17285 
despesa_individual=cbind(despesa_individual,dummy(despesa_individual$regiao,sep="_regiao_"))



#Colunas da tabela de despesa_individual para taxi, onibus urbano e uber
vetor=c(239,19,67,155)

for (i in 1:length(vetor)){

bd=despesa_individual[despesa_individual[[vetor[i]]]==1,]

#teste de exclusão de salários e horas trabalhadas, na. o que sobra? como os coficientes mudam?
#bd=bd[!is.na(bd$salario)&!is.na(bd$horastrab),]
#Aparentemente não afeta os coeficientes

bd=bd[bd[[vetor[i]+3]]<=2000,]
y=log(bd[[vetor[i]+3]]/bd[[vetor[i]+1]])

#Todas as variáveis
#x=as.matrix(bd[,c(12,13,7,8,16,256:258,260:262,266:268,272:275)])

x=as.matrix(bd[,c(12,13,7,266:268,272:275)])


#se='boot' serve para forçar o summary a mostrar o t valeu ao invés de intervalos de confiânça
#setwd('/Arquivos/Documentos/Pesquisa/MBA - Maira/Resultados/Regressoes_quantilicas')
q25bd=rq(y~x, data=bd,tau=0.25)
pq=summary(q25bd,se='boot')
print(names(bd[vetor[[i]]]))
print(pq)
#pq=pq$coefficients
#write.csv(pq,file=paste((names(bd[vetor[i]])),'25.csv',sep='_'),row.names=T)

q50bd=rq(y~x, data=bd,tau=0.50)
pq=summary(q50bd,se='boot')
print(names(bd[vetor[[i]]]))
print(pq)
#pq=pq$coefficients
#write.csv(pq,file=paste((names(bd[vetor[i]])),'50.csv',sep='_'),row.names=T)

q75bd=rq(y~x, data=bd,tau=0.75)
pq=summary(q75bd,se='boot')
print(names(bd[vetor[[i]]]))
print(pq)
#pq=pq$coefficients
#write.csv(pq,file=paste((names(bd[vetor[i]])),'75.csv',sep='_'),row.names=T)
}


#####################################################
####################################################

#Seguir a estratégia do paper publicado na revista de administração da USP e tentar aplicar
#heckman, considerando todos os indivíduos. Não sei se tem que usar indivíduos ou domicílios
#Tentar Heckman
library(sampleSelection)
library(dummies)

setwd('/Arquivos/Documentos/Pesquisa/MBA - Maira/POF-2017-2018/Dados/Arquivos de dados')
despesa_individual=readRDS('desp_rend_outrend.RDS')

#Tratamento de algumas variáveis para ajuste do modelo
despesa_individual$outros_rend[is.na(despesa_individual[["outros_rend"]])]=0
#mulher 0 e homem 1
despesa_individual[["sexo"]][despesa_individual[["sexo"]]==2]=0
#urbano 1 e rural 0
despesa_individual$TIPO_SITUACAO_REG[despesa_individual[["TIPO_SITUACAO_REG"]]==2]=0
#Salário igual a logaritmo do salário
despesa_individual$salario[despesa_individual$salario==9999999|
                             despesa_individual$salario==19999998]=NA
#despesa_individual$salario=log(despesa_individual$salario)
#despesa_individual$salario[is.infinite(despesa_individual$salario)]=NA

#Faixas salariais
#salário mínomo da época era de 937,00
despesa_individual$salmin=despesa_individual$salario/937
despesa_individual$faixasal[despesa_individual$salmin<=1]=1
despesa_individual$faixasal[despesa_individual$salmin>1&despesa_individual$salmin<=1.5]=2
despesa_individual$faixasal[despesa_individual$salmin>1.5&despesa_individual$salmin<=2.5]=3
despesa_individual$faixasal[despesa_individual$salmin>2.5]=4
despesa_individual=cbind(despesa_individual,dummy(despesa_individual$faixasal,sep = "_faixasal_"))


# Quantidade de empregos por indivíduos
# 1     2     3     4     5     6     7     8    10 
#67495 11421  1288   190    36     9     1     2     1 
#Agrupar de 4 a 10 empregos antes de criar binárias
despesa_individual$nempreg[despesa_individual$nempreg==5|despesa_individual$nempreg==6|
                             despesa_individual$nempreg==7|despesa_individual$nempreg==8|
                             despesa_individual$nempreg==9|despesa_individual$nempreg==10]=4
#Binárias
despesa_individual=cbind(despesa_individual,dummy(despesa_individual$nempreg,sep = '_nempreg_'))



#Cor
# 1     2     3     4     5     9 
#48817 13664   597 63954   586   238 
#Vou agrupar amaelos e  indígenas em 3
despesa_individual$cor[despesa_individual$cor==5]=3
#Sem declaração será NA
despesa_individual$cor[despesa_individual$cor==9]=NA
#Binárias
despesa_individual=cbind(despesa_individual,dummy(despesa_individual$cor, sep='_cor_'))



#Tempo de deslocamento até o trabalho
#    1     2     3     4     5     9 
#25038 36608 11558  5047  1317   875 
#Agrupar 4 e 5 
despesa_individual$tempdesloc[despesa_individual$tempdesloc==5]=4
#Sem declaração será NA
despesa_individual$tempdesloc[despesa_individual$tempdesloc==9]=NA
#Binárias
despesa_individual=cbind(despesa_individual,dummy(despesa_individual$tempdesloc,sep="_tempdesloc_"))



#Regiões
#Agrupar estados por região e depois criar binárias
despesa_individual$regiao[despesa_individual$UF==11|despesa_individual$UF==12|despesa_individual$UF==13|
                            despesa_individual$UF==14|despesa_individual$UF==15|despesa_individual$UF==16|
                            despesa_individual$UF==17]='norte'

despesa_individual$regiao[despesa_individual$UF==21|despesa_individual$UF==22|despesa_individual$UF==23|
                            despesa_individual$UF==24|despesa_individual$UF==25|despesa_individual$UF==26|
                            despesa_individual$UF==27|despesa_individual$UF==28|despesa_individual$UF==29]='nordeste'

despesa_individual$regiao[despesa_individual$UF==31|despesa_individual$UF==31|despesa_individual$UF==33|
                            despesa_individual$UF==35]='sudeste'

despesa_individual$regiao[despesa_individual$UF==41|despesa_individual$UF==42|despesa_individual$UF==43]="sul"

despesa_individual$regiao[despesa_individual$UF==50|despesa_individual$UF==51|despesa_individual$UF==52|
                            despesa_individual$UF==53]='centro-oeste'

#centro-oeste      nodeste        norte      sudeste          sul 
#16317        44078        17620        26455        17285 
despesa_individual=cbind(despesa_individual,dummy(despesa_individual$regiao,sep="_regiao_"))



#despesa_individual=despesa_individual[,c(239:242,5,12,13,7,8,16,256:258,260:262,266:268,272:275)]

#Transformar idade em faixa etária
despesa_individual$faixa_etaria[despesa_individual$idade<=25]=1
despesa_individual$faixa_etaria[despesa_individual$idade>25&despesa_individual$idade<=50]=2
despesa_individual$faixa_etaria[despesa_individual$idade>50]=3
#Variável binária para faixa etária que substituirá idade no modelo
despesa_individual=cbind(despesa_individual,dummy(despesa_individual$faixa_etaria,sep="_faixaetaria_"))

#Binária para horas trabalhadas na semana
#Categorizada pelos quantis
despesa_individual$horastrabalhadas[despesa_individual$horastrab<=30]=1
despesa_individual$horastrabalhadas[despesa_individual$horastrab>30&despesa_individual$horastrab<=40]=2
despesa_individual$horastrabalhadas[despesa_individual$horastrab>40&despesa_individual$horastrab<=48]=3
despesa_individual$horastrabalhadas[despesa_individual$horastrab>48]=4
despesa_individual=cbind(despesa_individual,dummy(despesa_individual$horastrabalhadas,sep = "_horastrab_"))


##Variável para regiões metropolitanas e não metropolitanas para substituir as regionais geográficas.
#ouuu binárias para as cincor principais regiões metropolitaas do BR e cate de referência como o restante do país. 

#y=despesa_individual[,c(1)]
#x=as.matrix(despesa_individual[,c(3:20)])


##Logit (só para testar.. saída talvez seja essa. umlogit para os principais substitutos)

#Uber
logituber=glm(UBER ~ despesa_individual_faixaetaria_2 + despesa_individual_faixaetaria_3 +
                sexo + despesa_individual_faixasal_2 + despesa_individual_faixasal_3 + despesa_individual_faixasal_4 +
                despesa_individual_horastrab_2 + despesa_individual_horastrab_3 +
                despesa_individual_horastrab_4 + 
                despesa_individual_cor_1 + despesa_individual_cor_2 + despesa_individual_cor_3 +
                despesa_individual_nempreg_2 + despesa_individual_nempreg_3 + despesa_individual_nempreg_4 + despesa_individual_tempdesloc_2 + 
                despesa_individual_tempdesloc_3 + despesa_individual_tempdesloc_4 + 
                despesa_individual_regiao_norte + despesa_individual_regiao_nordeste + 
                despesa_individual_regiao_norte + despesa_individual_regiao_sudeste +
                despesa_individual_regiao_sul,data=despesa_individual,family=binomial)
summary(logituber)

## odds ratios only
#exp(coef(logituber))

## odds ratios and 95% CI
exp(cbind(OR = coef(logituber), confint(logituber)))


#Taxi
logittaxi=glm(TAXI ~ despesa_individual_faixaetaria_2 + despesa_individual_faixaetaria_3 +
                sexo + despesa_individual_faixasal_2 + despesa_individual_faixasal_3 + despesa_individual_faixasal_4 +
                despesa_individual_horastrab_2 + despesa_individual_horastrab_3 +
                despesa_individual_horastrab_4 + 
                despesa_individual_cor_1 + despesa_individual_cor_2 + despesa_individual_cor_3 +
                despesa_individual_nempreg_2 + despesa_individual_nempreg_3 + despesa_individual_nempreg_4 + despesa_individual_tempdesloc_2 + 
                despesa_individual_tempdesloc_3 + despesa_individual_tempdesloc_4 + 
                despesa_individual_regiao_norte + despesa_individual_regiao_nordeste + 
                despesa_individual_regiao_norte + despesa_individual_regiao_sudeste +
                despesa_individual_regiao_sul,data=despesa_individual,family=binomial)
summary(logittaxi)

## odds ratios only
#exp(coef(logituber))
## odds ratios and 95% CI
exp(cbind(OR = coef(logittaxi), confint(logittaxi)))

#ônibus urbano
logitonibus=glm(ONIBUS.URBANO ~ despesa_individual_faixaetaria_2 + despesa_individual_faixaetaria_3 +
                  sexo + despesa_individual_faixasal_2 + despesa_individual_faixasal_3 + despesa_individual_faixasal_4 +
                  despesa_individual_horastrab_2 + despesa_individual_horastrab_3 +
                  despesa_individual_horastrab_4 + 
                  despesa_individual_cor_1 + despesa_individual_cor_2 + despesa_individual_cor_3 +
                  despesa_individual_nempreg_2 + despesa_individual_nempreg_3 + despesa_individual_nempreg_4 + despesa_individual_tempdesloc_2 + 
                  despesa_individual_tempdesloc_3 + despesa_individual_tempdesloc_4 + 
                  despesa_individual_regiao_norte + despesa_individual_regiao_nordeste + 
                  despesa_individual_regiao_norte + despesa_individual_regiao_sudeste +
                  despesa_individual_regiao_sul,data=despesa_individual,family=binomial)
summary(logitonibus)

## odds ratios only
#exp(coef(logituber))
## odds ratios and 95% CI
exp(cbind(OR = coef(logitonibus), confint(logitonibus)))


#gasolina comum
logitgasolina=glm(GASOLINA.COMUM..COMBUSTIVEL.DE.VEICULO. ~ despesa_individual_faixaetaria_2 + despesa_individual_faixaetaria_3 +
                    sexo + despesa_individual_faixasal_2 + despesa_individual_faixasal_3 + despesa_individual_faixasal_4 +
                    despesa_individual_horastrab_2 + despesa_individual_horastrab_3 +
                    despesa_individual_horastrab_4 + 
                    despesa_individual_cor_1 + despesa_individual_cor_2 + despesa_individual_cor_3 +
                    despesa_individual_nempreg_2 + despesa_individual_nempreg_3 + despesa_individual_nempreg_4 + despesa_individual_tempdesloc_2 + 
                    despesa_individual_tempdesloc_3 + despesa_individual_tempdesloc_4 + 
                    despesa_individual_regiao_norte + despesa_individual_regiao_nordeste + 
                    despesa_individual_regiao_norte + despesa_individual_regiao_sudeste +
                    despesa_individual_regiao_sul,data=despesa_individual,family=binomial)
summary(logitgasolina)

## odds ratios only
#exp(coef(logituber))
## odds ratios and 95% CI
exp(cbind(OR = coef(logitgasolina), confint(logitgasolina)))


#ônibus intermunicipal
logitonibusinter=glm(ONIBUS.INTERMUNICIPAL ~ despesa_individual_faixaetaria_2 + despesa_individual_faixaetaria_3 +
                       sexo + despesa_individual_faixasal_2 + despesa_individual_faixasal_3 + despesa_individual_faixasal_4 +
                       + despesa_individual_horastrab_2 + despesa_individual_horastrab_3 +
                       despesa_individual_horastrab_4 + 
                       despesa_individual_cor_1 + despesa_individual_cor_2 + despesa_individual_cor_3 +
                       despesa_individual_nempreg_2 + despesa_individual_nempreg_3 + despesa_individual_nempreg_4 + despesa_individual_tempdesloc_2 + 
                       despesa_individual_tempdesloc_3 + despesa_individual_tempdesloc_4 + 
                       despesa_individual_regiao_norte + despesa_individual_regiao_nordeste + 
                       despesa_individual_regiao_norte + despesa_individual_regiao_sudeste +
                       despesa_individual_regiao_sul,data=despesa_individual,family=binomial)
summary(logitonibusinter)

## odds ratios only
#exp(coef(logituber))
## odds ratios and 95% CI
exp(cbind(OR = coef(logitonibusinter), confint(logitonibusinter)))


#Estatísticas descritivas
#Cria DF
ed2=as.data.frame(NA)
#Seleciona as variáveis de interesse
edt=despesa_individual[,c(19,67,155,239,13,257:276,278:283,285:287,289:293)]

for (i in 1:dim(edt)[2]){
ed=as.data.frame(table(edt[[colnames(edt)[i]]]))
ed2[i,1]=colnames(edt)[i]
ed2[i,2]=ed[1,2]
ed2[i,3]=ed[2,2]
}
colnames(ed2)=c('Variavel','Nao','Sim')

#Acrescentar idade média na última linha do DF só para faciliar a exportação
ed2[i+1,1]="idade média"
ed2[i+1,3]=mean(despesa_individual$idade,na.rm=T)
#Exportar para manipular a versão final em planilha
setwd('/Arquivos/Documentos/Pesquisa/MBA - Maira/POF-2017-2018/Dados/Arquivos de dados/')
write.csv(ed2,file="Estatísticas_descritivas.csv",row.names = F)




#OLS (Só para teste)
#reg=lm(formula = y~x,data=despesa_individual)

#Acho que R já faz o subset automaticamente porque os coeficientes tem o mesmo valor que antes
#reg=lm(formula = y~x,data=subset(despesa_individual,despesa_individual$UBER==1))

#Heckman
heckuber=heckit(UBER ~ idade + sexo + salario + TIPO_SITUACAO_REG + despesa_individual_cor_1 + 
                despesa_individual_cor_2 + despesa_individual_cor_3 + despesa_individual_nempreg_2 + 
                despesa_individual_nempreg_3  + 
                  despesa_individual_tempdesloc_3 + despesa_individual_tempdesloc_4 + 
                  despesa_individual_regiao_norte + despesa_individual_regiao_nordeste + 
                  despesa_individual_regiao_norte + despesa_individual_regiao_sudeste +
                  despesa_individual_regiao_sul,
                log(UBER_valor/UBER_qtde) ~ idade + sexo + salario + despesa_individual_cor_1 + 
                  despesa_individual_cor_2 + despesa_individual_cor_3 +  despesa_individual_tempdesloc_2 + 
                  despesa_individual_tempdesloc_3 + despesa_individual_tempdesloc_4 + 
                despesa_individual_regiao_norte + despesa_individual_regiao_nordeste + 
                  despesa_individual_regiao_norte + despesa_individual_regiao_sudeste +
                  despesa_individual_regiao_sul,
                data=despesa_individual)

summary(heckuber)


