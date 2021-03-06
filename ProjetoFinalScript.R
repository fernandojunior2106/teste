library(tidyr)
library(dbplyr)
library(magrittr)
library(readxl)
library(plyr)
library(dplyr)
library(tidyverse)
library(formattable)
library(gridExtra)

#Tratando os dados para os funcion�rios

FuncRaw <- read_xlsx("Dados - Projeto INOVAR.xlsx",
                     sheet = "Func Raw")
str(FuncRaw)

FuncRaw <- separate(FuncRaw, 2, c("CNPJ", "ID do usuario"), remove = T, sep = "#")

#Tratando os dados para os gestores

GestorRaw <- read_xlsx("Dados - Projeto INOVAR.xlsx",
                       sheet = "Gestor Raw")
str(GestorRaw)

GestorRaw <- separate(GestorRaw, 2, c("CNPJ", "ID do usuario"), remove = T, sep = "#")
GestorRaw <- GestorRaw[ -c(1, 3, 4, 95)]

#Tratando os dados para os gestores de ti

GestorTIRaw <- read_xlsx("Dados - Projeto INOVAR.xlsx",
                         sheet = "Gestor TI Raw")
str(GestorTIRaw)

GestorTIRaw <- separate(GestorTIRaw, 2, c("CNPJ", "ID do usuario"), remove = T, sep = "#")
GestorTIRaw <- GestorTIRaw[ -c(1, 3:5, 63)]

#----------------------------------------------------------------------------------------------------

#M�dias cultura e aprendizado

F_Cult_Aprend <- FuncRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(lrncult1:lrncult10), all_vars(!is.na(.))) %>% 
  summarise_at(vars(lrncult1:lrncult10), mean)

#M�dia das m�dias culrura e aprendizado

F_M�dia_Cult_Aprend <- gather(F_Cult_Aprend, key = media, value = lrncultmedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(lrncultmedia), mean)

#M�dias capacidade de coordena��o interna

F_Coord_Int <- FuncRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(coord1:coord5), all_vars(!is.na(.))) %>%
  summarise_at(vars(coord1:coord5), mean)

#M�dia das m�dias capacidade de coordena��o interna

F_M�dia_Coord_Int <- gather(F_Coord_Int, key = media, value = Fcoordmedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(Fcoordmedia), mean)

#M�dias n�vel de integra��o entre �reas

F_Intgr_Areas <- FuncRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(integr1:integr5), all_vars(!is.na(.))) %>%
  summarise_at(vars(integr1:integr5), mean)

#M�dia das m�dias n�vel de integra��o entre �reas

F_M�dia_Intgr_Areas <- gather(F_Intgr_Areas, key = media, value = Fintegrmedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(Fintegrmedia), mean)

#M�dias capacidade de comunica��o do principal gestor

F_Com_Gest <- FuncRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(com1:com3), all_vars(!is.na(.))) %>%
  summarise_at(vars(com1:com3), mean)

#M�dia das m�dias capacidade de comunica��o do principal gestor

F_M�dia_Com_Gest <- gather(F_Com_Gest, key = media, value = commedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(commedia), mean)

#M�dias capacidade de lideran�a transformacional do principal gestor

F_Lid_Trans <- FuncRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(lid1:lid14), all_vars(!is.na(.))) %>%
  summarise_at(vars(lid1:lid14), mean)

#M�dia das m�dias capacidade de lideran�a transformacional do principal gestor

F_M�dia_Lid_Trans <- gather(F_Lid_Trans, key = media, value = lidmedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(lidmedia), mean)

#Criar novo dataframe com as m�dias parciais

join <- full_join(F_Cult_Aprend, F_M�dia_Cult_Aprend, by = "CNPJ")
join2 <- full_join(join, F_Coord_Int, by = "CNPJ")
join3 <- full_join(join2, F_M�dia_Coord_Int, by = "CNPJ")
join4 <- full_join(join3, F_Intgr_Areas, by = "CNPJ")
join5 <- full_join(join4, F_M�dia_Intgr_Areas, by = "CNPJ")
join6 <- full_join(join5, F_Com_Gest, by = "CNPJ")
join7 <- full_join(join6, F_M�dia_Com_Gest, by = "CNPJ")
join8 <- full_join(join7, F_Lid_Trans, by = "CNPJ")
join9 <- full_join(join8, F_M�dia_Lid_Trans, by = "CNPJ")

Dados_func <- join9

rm(list=ls(pattern="join"))
rm(list=ls(pattern="F_"))

Dados_func %<>% plyr::rename(c("coord1"="Fcoord1", "coord2"="Fcoord2", "coord3"="Fcoord3", "coord4"="Fcoord4", "coord5"="Fcoord5", "integr1"="Fintegr1", "integr2"="Fintegr2", "integr3"="Fintegr3", "integr4"="Fintegr4", "integr5"="Fintegr5" ))

#----------------------------------------------------------------------------------------------------------------------------------------------------------------

#Dados gestor

#M�dias performance

G_Perf <- GestorRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(perf1:perf5), all_vars(!is.na(.))) %>%
  summarise_at(vars(perf1:perf5), mean)

#------------------------------------------------------------------------------------

#M�dias inova��o geral

G_Inovgeral <- GestorRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(produtos, mercados, admin), all_vars(!is.na(.))) %>%
  summarise_at(vars(produtos, mercados, admin), mean)

#Total de inova��o geral 

G_Soma_Inovgeral <- gather(G_Inovgeral, key = media, value = Ginovgeral, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(Ginovgeral), sum)

#M�dias inova��o radical 

G_Inovradical <- GestorRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(produtos_rad, mercados_rad, admin_rad), all_vars(!is.na(.))) %>%
  summarise_at(vars(produtos_rad, mercados_rad, admin_rad), mean)

#Total de inova��o radical 

G_Soma_Inovradical <- gather(G_Inovradical, key = media, value = Ginovradical, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(Ginovradical), sum)
#-------------------------------------------------------------------------------------

#M�dias rede de relacionamentos externos

G_Rel_Ext <- GestorRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(associacoes:outras_orgs), all_vars(!is.na(.))) %>%
  summarise_at(vars(associacoes:outras_orgs), mean)

#M�dia das m�dias rede de relacionamentos externos

G_M�dias_Rel_Ext <- gather(G_Rel_Ext, key = media, value = relmedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(relmedia), mean)

#M�dias de compet�ncias gerenciais

G_Comp_Geren <- GestorRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(compger1:compger3), all_vars(!is.na(.))) %>%
  summarise_at(vars(compger1:compger3), mean)

#M�dia das m�dias de compet�ncias gerenciais

G_M�dias_Comp_Geren <- gather(G_Comp_Geren, key = media, value = compgermedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(compgermedia), mean)

#---------------------------------------------------------------------------------

#M�dias de Monitoramento do ambiente externo

G_Mon_AE <- GestorRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(recon1:recon5), all_vars(!is.na(.))) %>%
  summarise_at(vars(recon1:recon5), mean)

#M�dia das m�dias de Monitoramento do ambiente externo

G_M�dias_Mon_AE <- gather(G_Mon_AE, key = media, value = reconmedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(reconmedia), mean)

#M�dias de assimila��o de conhecimentos externos

G_Assm_CE <- GestorRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(assim1:assim4), all_vars(!is.na(.))) %>%
  summarise_at(vars(assim1:assim4), mean)

#M�dia das m�dias de assimila��o de conhecimentos externos

G_M�dias_Assm_CE <- gather(G_Assm_CE, key = media, value = assimmedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(assimmedia), mean)

#M�dias de Reten��o de conhecimentos

G_Ret_Conh <- GestorRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(mant1:mant4), all_vars(!is.na(.))) %>%
  summarise_at(vars(mant1:mant4), mean)

#M�dia das m�dias de Reten��o de conhecimentos

G_M�dias_Ret_Conh <- gather(G_Ret_Conh, key = media, value = mantmedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(mantmedia), mean)

#M�dias de Ativa��o de conhecimentos

G_Atv_Conh <- GestorRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(reat1:reat4), all_vars(!is.na(.))) %>%
  summarise_at(vars(reat1:reat4), mean)

#M�dia das m�dias de Ativa��o de conhecimentos

G_M�dias_Atv_Conh <- gather(G_Atv_Conh, key = media, value = reatmedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(reatmedia), mean)

#M�dias de Tranforma��o de conhecimentos

G_Trans_Conh <- GestorRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(trans1:trans4), all_vars(!is.na(.))) %>%
  summarise_at(vars(trans1:trans4), mean)

#M�dia das m�dias de Tranforma��o de conhecimentos

G_M�dias_Trans_Conh <- gather(G_Trans_Conh, key = media, value = transmedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(transmedia), mean)

#M�dias de Aplica��o de conhecimentos

G_Apl_Conh <- GestorRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(aplic1:aplic4), all_vars(!is.na(.))) %>%
  summarise_at(vars(aplic1:aplic4), mean)

#M�dia das m�dias de Aplica��o de conhecimentos

G_M�dias_Apl_Conh <- gather(G_Apl_Conh, key = media, value = aplicmedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(aplicmedia), mean)
#----------------------------------------------------------------------------------------

#M�dias de Integra��o

G_Integ <- GestorRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(integr1:integr5), all_vars(!is.na(.))) %>%
  summarise_at(vars(integr1:integr5), mean)

#M�dia das m�dias de Integra��o

G_M�dias_Integ <- gather(G_Integ, key = media, value = Gintegrmedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(Gintegrmedia), mean)

#M�dias de Coordena��o

G_Coord <- GestorRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(coord1:coord5), all_vars(!is.na(.))) %>%
  summarise_at(vars(coord1:coord5), mean)

#M�dia das m�dias de Integra��o

G_M�dias_Coord <- gather(G_Coord, key = media, value = Gcoordmedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(Gcoordmedia), mean)

#M�dias de Propens�o a assumir riscos

G_Assm_Risc <- GestorRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(riscos1:riscos7), all_vars(!is.na(.))) %>%
  summarise_at(vars(riscos1:riscos7), mean)

#M�dia das m�dias de Propens�o a assumir riscos

G_M�dias_Assm_Risc <- gather(G_Assm_Risc, key = media, value = riscosmedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(riscosmedia), mean)

#Arrumando dados 

GestorRaw <- GestorRaw[ c(1:16, 35:37, 76:84)]

#Criar novo dataframe com as m�dias parciais

join <- full_join(GestorRaw, G_Perf, by = "CNPJ")
join2 <- full_join(join, G_Inovgeral, by = "CNPJ")
join3 <- full_join(join2, G_Soma_Inovgeral, by = "CNPJ") 
join4 <- full_join(join3, G_Inovradical, by = "CNPJ")
join5 <- full_join(join4, G_Soma_Inovradical, by = "CNPJ")
join6 <- full_join(join5, G_Rel_Ext, by = "CNPJ")
join7 <- full_join(join6, G_M�dias_Rel_Ext, by = "CNPJ")
join8 <- full_join(join7, G_Comp_Geren, by = "CNPJ")
join9 <- full_join(join8, G_M�dias_Comp_Geren, by = "CNPJ")
join10 <- full_join(join9, G_Mon_AE, by = "CNPJ")
join11 <- full_join(join10, G_M�dias_Mon_AE, by = "CNPJ")
join12 <- full_join(join11, G_Assm_CE, by = "CNPJ")
join13 <- full_join(join12, G_M�dias_Assm_CE, by = "CNPJ")
join14 <- full_join(join13, G_Ret_Conh, by = "CNPJ")
join15 <- full_join(join14, G_M�dias_Ret_Conh, by = "CNPJ")
join16 <- full_join(join15, G_Atv_Conh, by = "CNPJ")
join17 <- full_join(join16, G_M�dias_Atv_Conh, by = "CNPJ")
join18 <- full_join(join17, G_Trans_Conh, by = "CNPJ")
join19 <- full_join(join18, G_M�dias_Trans_Conh, by = "CNPJ")
join20 <- full_join(join19, G_Apl_Conh, by = "CNPJ")
join21 <- full_join(join20, G_M�dias_Apl_Conh, by = "CNPJ")
join22 <- full_join(join21, G_Integ, by = "CNPJ")
join23 <- full_join(join22, G_M�dias_Integ, by = "CNPJ")
join24 <- full_join(join23, G_Coord, by = "CNPJ")
join25 <- full_join(join24, G_M�dias_Coord, by = "CNPJ")
join26 <- full_join(join25, G_Assm_Risc, by = "CNPJ")
join27 <- full_join(join26, G_M�dias_Assm_Risc, by = "CNPJ")

Dados_gestor <- join27

Dados_gestor %<>% plyr::rename(c("coord1"="Gcoord1", "coord2"="Gcoord2", "coord3"="Gcoord3", "coord4"="Gcoord4", "coord5"="Gcoord5",
                                 "integr1"="Gintegr1", "integr2"="Gintegr2", "integr3"="Gintegr3", "integr4"="Gintegr4", "integr5"="Gintegr5",
                                 "atividades___1" = "Gatividades___1", "atividades___2" = "Gatividades___2", "atividades___3" = "Gatividades___3",
                                 "atividades___4" = "Gatividades___4", "atividades___5" = "Gatividades___5", "exp_gestao" = "Gexp_gestao",
                                 "exp_prof" = "Gexp_prof", "funcao" = "Gfuncao", "funcao_outro", "Gfuncao_outro", "idade" = "Gidade",
                                 "nivel_educ" = "Gnivel_educ", "produtos" = "Gprodutos", "produtos_rad" = "Gprodutos_rad", "tempo_funcao" = "Gtempo_funcao",
                                 "sexo" = "Gsexo"))

rm(list=ls(pattern="join"))

#---------------------------------------------------------------------------------------------------------------------------------------------------------

#Dados gestor de TI

#M�dias inova��o geral em TI

TI_Inovgeral <- GestorTIRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(ferramentas, metodologias, funcionalidades, produtos), all_vars(!is.na(.))) %>%
  summarise_at(vars(ferramentas, metodologias, funcionalidades, produtos), mean)

#Total de inova��o geral em TI

TI_Soma_Inovgeral <- gather(TI_Inovgeral, key = media, value = TIinovgeral, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(TIinovgeral), sum)

#M�dias inova��o radical em TI

TI_Inovradical <- GestorTIRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(ferramentas_rad, metodologias_rad, funcionalidades_rad, produtos_rad), all_vars(!is.na(.))) %>%
  summarise_at(vars(ferramentas_rad, metodologias_rad, funcionalidades_rad, produtos_rad), mean)

#Total de inova��o radical em TI

TI_Soma_Inovradical <- gather(TI_Inovradical, key = media, value = TIinovradical, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(TIinovradical), sum)
#----------------------------------------------------------------------------------------------------------------

#M�dia de flexibilidades dos sistemaas de TI

TI_sist <- GestorTIRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(sist1:sist5), all_vars(!is.na(.))) %>% 
  summarise_at(vars(sist1:sist5), mean)

#M�dia das m�dias de flexibilidades dos sistemaas de TI

TI_M�dias_sist <- gather(TI_sist, key = media, value = sistmedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(sistmedia), mean)

#Diversidade e profundidade de conhecimentos

#Retirando NA's

TI_diverprof <- GestorTIRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(plat1:metod4), all_vars(!is.na(.))) %>%
  summarise_at(vars(plat1:metod4), mean)

#Diversidade (contagem "regular" ou maior)

TI_diversidade <- GestorTIRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(plat1:metod4), all_vars(!is.na(.))) %>%
  summarise_at(vars(plat1:metod4), mean) %>%
  gather( key = vari�veis, value = valor, -CNPJ) %>%
  group_by(CNPJ) %>%
  filter_at(vars(valor), all_vars(. >= 2)) %>% 
  dplyr::summarize(diversidade = n())

#Profundidade (soma "bom" ou maior)

TI_profundidade <- GestorTIRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(plat1:metod4), all_vars(!is.na(.))) %>%
  summarise_at(vars(plat1:metod4), mean) %>%
  gather( key = vari�veis, value = profundidade, -CNPJ) %>%
  group_by(CNPJ) %>%
  filter_at(vars(profundidade), all_vars(. >= 3)) %>% 
  summarise_at(vars(profundidade), sum)

#M�dia de Qualidade do software desenvolvido

TI_qual <- GestorTIRaw %>% tbl_df %>% 
  group_by(CNPJ) %>%
  filter_at(vars(desproj1:desproj6), all_vars(!is.na(.))) %>% 
  summarise_at(vars(desproj1:desproj6), mean)

#M�dia das m�dias de Qualidade do software desenvolvido

TI_M�dias_qual <- gather(TI_qual, key = media, value = desprojmedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(desprojmedia), mean)

#Arrumando dados 

GestorTIRaw <- GestorTIRaw[ c(1:8, 17:18, 51:58)]

#Criar novo dataframe com as m�dias parciais

join <- full_join(GestorTIRaw, TI_Inovgeral, by = "CNPJ")
join2 <- full_join(join, TI_Soma_Inovgeral, by = "CNPJ")
join3 <- full_join(join2, TI_Inovradical, by = "CNPJ")
join4 <- full_join(join3, TI_Soma_Inovradical, by = "CNPJ")
join5 <- full_join(join4, TI_sist, by = "CNPJ")
join6 <- full_join(join5, TI_M�dias_sist, by = "CNPJ")
join7 <- full_join(join6, TI_diverprof, by = "CNPJ")
join8 <-  full_join(join7, TI_diversidade, by = "CNPJ")
join9 <- full_join(join8, TI_profundidade, by = "CNPJ")
join10 <- full_join(join9, TI_qual, by = "CNPJ")
join11 <- full_join(join10, TI_M�dias_qual, by = "CNPJ")

Dados_gestor_TI <- join11

rm(list=ls(pattern="join"))

Dados_gestor_TI %<>% plyr::rename(c("atividades___1" = "TIatividades___1", "atividades___2" = "TIatividades___2", "atividades___3" = "TIatividades___3",
                                    "atividades___4" = "TIatividades___4", "atividades___5" = "TIatividades___5", "exp_gestao" = "TIexp_gestao",
                                    "exp_prof" = "TIexp_prof", "funcao" = "TIfuncao", "funcao_outro" = "TIfuncao_outro", "idade" = "TIidade",
                                    "nivel_educ" = "TInivel_educ", "produtos" = "TIprodutos", "produtos_rad" = "TIprodutos_rad",
                                    "sexo" = "TIsexo", "tempo_funcao" = "TItempo_funcao"))

#---------------------------------------------------------------------------------
#Juntando dados de inova��o geral

join <- full_join(GestorRaw[1], G_Soma_Inovgeral)
Inovgeral <- full_join(join, TI_Soma_Inovgeral)

#Total de inova��es gerais das empresas

Inovgeral <- gather(Inovgeral, key = soma, value = inovgeralmedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(inovgeralmedia), sum)

rm(list=ls(pattern="join"))

#Juntando dados de inova��o radical

join <- full_join(GestorRaw[1], G_Soma_Inovradical)
Inovradical <- full_join(join, TI_Soma_Inovradical)

#Total de inova��es gerais das empresas

Inovradical <- gather(Inovradical, key = soma, value = inovradicalmedia, -CNPJ) %>%
  group_by(CNPJ) %>%
  summarise_at(vars(inovradicalmedia), sum)

rm(list=ls(pattern="join"))

#---------------------------------------------------------------------------------
#Dados finais agrupados

join <- full_join(Dados_gestor, Inovgeral, by = "CNPJ")
join2 <- full_join(join, Inovradical, by = "CNPJ")
join3 <- full_join(join2, Dados_gestor_TI, by = "CNPJ")
Dados_finais <- full_join(join3, Dados_func, by = "CNPJ")

rm(list=ls(pattern="join"))
rm(list=ls(pattern="TI_"))
rm(list=ls(pattern="G_"))

#Arrumando os fatores

Dados_finais$estado %<>% factor(levels = 1:27,
                                labels = c("Acre (AC)", "Alagoas (AL)", "Amap� (AP)", "Amazonas (AM)", "Bahia (BA)", "Cear� (CE)", "Distrito Federal (DF)",
                                           "Esp�rito Santo (ES)", "Goi�s (GO)", "Maranh�o (MA)", "Mato Grosso (MT)", "Mato Grosso do Sul (MS)",
                                           "Minas Gerais (MG)", "Par� (PA)", "Para�ba (PB)", "Paran� (PR)", "Pernambuco (PE)", "Piau� (PI)",
                                           "Rio de Janeiro (RJ)", "Rio Grande do Norte (RN)", "Rio Grande do Sul (RS)", "Rond�nia (RO)", "Roraima (RR)",
                                           "Santa Catarina (SC)", "S�o Paulo (SP)", "Sergipe (SE)", "Tocantins (TO)"),
                                ordered = F)

Dados_finais$abrangencia %<>% factor(levels = 1:5,
                                     labels = c("Global", "Nacional", "Regional", "Estadual", "Municipal"),
                                     ordered = F) 

Dados_finais$Gnivel_educ %<>% factor(levels = 1:6,
                                     labels = c("Ensino Fundamental", "Ensino M�dio", "Gradua��o", "P�s-Gradua��o", "Mestrado", "Doutorado"),
                                     ordered = T) 

Dados_finais$TInivel_educ %<>% factor(levels = 1:6,
                                      labels = c("Ensino Fundamental", "Ensino M�dio", "Gradua��o", "P�s-Gradua��o", "Mestrado", "Doutorado"),
                                      ordered = T) 

Dados_finais$Gfuncao %<>% factor(levels = 1:5,
                                 labels = c("Propriet�rio/S�cio", "Presidente", "Diretor", "Gerente", "Outro"),
                                 ordered = F)

Dados_finais$TIfuncao %<>% factor(levels = 1:5,
                                  labels = c("Propriet�rio/S�cio", "Presidente", "Diretor", "Gerente", "Outro"),
                                  ordered = F)

Dados_finais$TIsexo %<>% factor(levels = c("M", "H"),
                                labels = c("Mulher", "Homem"),
                                ordered = F)

Dados_finais$Gsexo %<>% factor(levels = c("M", "H"),
                               labels = c("Mulher", "Homem"),
                               ordered = F)

Dados_finais$abriu_empr %<>% factor(levels = c("S", "N"),
                                    labels = c("Sim", "N�o"),
                                    ordered = F)


#Remover empresas que tenham menos de 35% da receita vinda de softwares

Dados_finais %<>% filter_at(vars(pct_receita_ativ), all_vars(. > 35))

#Manter s� as empresas que responderam tudo

Dados_finais %>% filter_at(vars(1:23, 25:112, 114:119, 121:211), all_vars(!is.na(.))) -> Dados_Finais_Completos