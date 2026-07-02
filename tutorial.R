# =====================================================================
# Explorando dados da PNS com o pacote survey
# Script R (versao executavel do tutorial)
#
# Caio Cesar Soares Goncalves - Departamento de Demografia / CEDEPLAR-UFMG
# Workshop DEMOPOP 2026 - Licenca CC BY 4.0
# Site: https://demopop.github.io/pns-survey/
#
# Requer: survey, dplyr, ggplot2 (instalados automaticamente pelo bloco
# abaixo). Mantenha a pasta data/ (com populacao_sintetica.rds, pns2019.rds
# e pns2013.rds) no diretorio de trabalho e rode de cima para baixo.
# ---------------------------------------------------------------------
# ANTES DE RODAR: aponte o diretorio de trabalho para a pasta que contem
# a subpasta data/.
#   No RStudio: menu Session > Set Working Directory > To Source File Location
# Ou descomente e ajuste:
# setwd("C:/caminho/para/workshop-pns")
# =====================================================================

pacotes  <- c("survey", "dplyr", "ggplot2")
faltando <- setdiff(pacotes, rownames(installed.packages()))
if (length(faltando) > 0) install.packages(faltando)
invisible(lapply(pacotes, library, character.only = TRUE))
options(scipen = 999, survey.lonely.psu = "adjust")


######################################################################
## A população (e a verdade)
######################################################################

library(survey)   # análise de amostras complexas (Lumley)
library(dplyr)    # manipulação de dados

pop <- readRDS("data/populacao_sintetica.rds")  # carrega a população sintética inteira
N <- nrow(pop)                                  # N = tamanho da população (nº de linhas)
glimpse(pop)                                    # espia as colunas: região, município, sexo, saúde...

# Como temos a POPULAÇÃO inteira, sabemos a VERDADE que queremos estimar:
P <- mean(pop$saude_ruim)                            # proporção verdadeira de "saúde ruim/regular"
P
round(tapply(pop$saude_ruim, pop$regiao, mean), 3)   # tapply: a mesma média, calculada por região


######################################################################
## Decida antes: sorteando 300 das `r N` pessoas, quão perto da verdade (`r round(P,3)`) a estimativa deve cair?
######################################################################

set.seed(1)               # fixa o gerador aleatório (resultados reproduzíveis)
n  <- 300                 # tamanho da amostra
am <- pop[sample(N, n), ] # sorteia n linhas (pessoas) ao acaso da população 'pop'
am$peso <- N / n          # peso de cada pessoa = N/n: ela "representa" N/n habitantes
am$fpcN <- N              # guarda o tamanho da população (para a correção de população finita)

# svydesign() descreve o PLANO AMOSTRAL ao R — é a base de toda estimativa:
#   ids     = ~1     -> NÃO há conglomerados (sorteio direto de pessoas)
#   weights = ~peso  -> a coluna com os pesos amostrais
#   fpc     = ~fpcN  -> tamanho da população (correção de população finita)
#   data    = am     -> a tabela com a amostra
des_aas <- svydesign(ids = ~1, weights = ~peso, fpc = ~fpcN, data = am)

# svymean(): estima a MÉDIA/proporção respeitando o desenho (~ indica a variável):
svymean(~saude_ruim, des_aas)
# confint(): intervalo de confiança de 95% da estimativa acima
confint(svymean(~saude_ruim, des_aas))


######################################################################
## Decida antes: estratificar por região vai melhorar a precisão? E vai melhorar **igual** para a saúde e para a renda?
######################################################################

set.seed(2)
taxa <- 300 / N                       # a MESMA fração sorteada em cada estrato (alocação proporcional)

# sorteia essa fração de cada região:
am_e <- pop |>
  group_by(regiao) |>
  slice_sample(prop = taxa) |>
  ungroup()

# para o peso, juntamos Nh (tamanho do estrato na população) e usamos nh (sorteados):
Nh_df <- count(pop, regiao, name = "Nh")     # nº de pessoas em cada região (estrato)
am_e  <- am_e |>
  left_join(Nh_df, by = "regiao") |>         # cola o Nh em cada pessoa
  group_by(regiao) |>
  mutate(peso = Nh / n()) |>                 # peso = Nh / nh   (n() = nº sorteado no estrato)
  ungroup()

# o argumento NOVO aqui é 'strata':
#   strata = ~regiao -> os estratos; fpc = ~Nh -> tamanho do estrato na população
des_est <- svydesign(ids = ~1, strata = ~regiao, weights = ~peso, fpc = ~Nh, data = am_e)
svymean(~saude_ruim, des_est)   # estima a proporção de saúde ruim/regular
svymean(~rdpc,       des_est)   # e a média da renda domiciliar per capita


######################################################################
## Alocação desproporcional → pesos desiguais
######################################################################

# alocação DESPROPORCIONAL: sobre-amostra o Nordeste (120) frente às outras regiões
set.seed(4)
am_d <- bind_rows(
  filter(pop, regiao == "Norte")        |> slice_sample(n = 40),
  filter(pop, regiao == "Nordeste")     |> slice_sample(n = 120),
  filter(pop, regiao == "Centro-Oeste") |> slice_sample(n = 40),
  filter(pop, regiao == "Sudeste")      |> slice_sample(n = 50),
  filter(pop, regiao == "Sul")          |> slice_sample(n = 50)
) |>
  left_join(Nh_df, by = "regiao") |>
  group_by(regiao) |> mutate(peso = Nh / n()) |> ungroup()   # peso = Nh/nh (varia entre estratos)

mean(am_d$saude_ruim)                                          # média SEM peso (ingênua)
svymean(~saude_ruim, svydesign(ids = ~1, strata = ~regiao,    # média COM peso (correta)
                               weights = ~peso, data = am_d))


######################################################################
## Decida antes: sorteando 15 municípios e 25 pessoas em cada (n = 375), o erro-padrão vai subir ou descer em relação à AAS?
######################################################################

muns <- unique(pop$municipio); M <- length(muns)   # M = total de municípios na população
mm <- 15; ni <- 25                                 # mm = municípios sorteados; ni = pessoas por município

set.seed(7)
sel  <- sample(muns, mm)                            # 1º estágio: sorteia mm municípios (as "UPAs")
am_c <- pop |>
  filter(municipio %in% sel) |>                     # mantém só os municípios sorteados
  group_by(municipio) |> slice_sample(n = ni) |> ungroup()   # 2º estágio: ni pessoas em cada

# peso dos dois estágios = (M/mm) · (Ni/ni):
Ni_df <- count(pop, municipio, name = "Ni")         # nº de pessoas em cada município
am_c  <- am_c |>
  left_join(Ni_df, by = "municipio") |>             # cola o Ni
  mutate(peso = (M/mm) * (Ni/ni))

# o argumento NOVO aqui é 'ids' (os conglomerados):
#   ids = ~municipio -> diz ao survey que o sorteio foi por conglomerados (municípios)
des_cl <- svydesign(ids = ~municipio, weights = ~peso, data = am_c)
svymean(~saude_ruim, des_cl, deff = TRUE)          # deff = TRUE -> também reporta o efeito de plano amostral


######################################################################
## Quanto o desenho importa? (Monte Carlo)
######################################################################

# uma função que sorteia UMA amostra conglomerada (15 municípios, 25 pessoas em cada):
sortear_cluster <- function() {
  sm <- sample(muns, mm)
  pop |> filter(municipio %in% sm) |>
    group_by(municipio) |> slice_sample(n = ni) |> ungroup() |>
    left_join(Ni_df, by = "municipio") |> mutate(peso = (M/mm) * (Ni/ni))
}

set.seed(5); B <- 500
cob <- replicate(B, {                              # B amostras conglomeradas; em cada uma...
  a       <- sortear_cluster()
  des     <- svydesign(ids = ~municipio, weights = ~peso, data = a)   # desenho CERTO (com cluster)
  des_ing <- svydesign(ids = ~1,         weights = ~peso, data = a)   # desenho INGÊNUO (sem cluster)
  est     <- coef(svymean(~saude_ruim, des))                          # a estimativa
  c(desenho = abs(est - P)/SE(svymean(~saude_ruim, des))     < 1.96,  # o IC do desenho cobre a verdade P?
    ingenuo = abs(est - P)/SE(svymean(~saude_ruim, des_ing)) < 1.96)  # e o IC ingênuo?
})
round(rowMeans(cob) * 100, 1)   # % das vezes em que cada IC cobriu a verdade (desenho vs ingênuo)


######################################################################
## Declarando o desenho amostral
######################################################################

# (bloco ILUSTRATIVO - nao rode: requer o microdado bruto do IBGE)
# library(readr)
# # posições do input_PNS_2019.txt (exemplo abreviado — só três colunas):
# pns19 <- read_fwf("PNS_2019.txt",
#   fwf_positions(start = c(3, 10, 1426), end = c(9, 18, 1439),
#                 col_names = c("V0024", "UPA_PNS", "V00291")))

pns19 <- readRDS("data/pns2019.rds")   # subconjunto da PNS 2019 já preparado

# desenho da PNS — combina tudo o que vimos na Parte 1, agora com dados reais:
#   ids     = ~UPA_PNS      -> conglomerados (as UPAs)
#   strata  = ~V0024        -> estratos geográficos
#   weights = ~peso_sel_cal -> peso do morador selecionado, calibrado (V00291)
#   nest    = TRUE          -> UPAs numeradas DENTRO de cada estrato
des <- svydesign(ids = ~UPA_PNS, strata = ~V0024, weights = ~peso_sel_cal,
                 data = pns19, nest = TRUE)
des                                    # imprime um resumo do plano amostral


######################################################################
## Calibração — por que 65% vira 66%
######################################################################

des_sc <- svydesign(ids = ~UPA_PNS, strata = ~V0024, weights = ~peso_sel,
                    data = pns19, nest = TRUE)                 # mesmo desenho, peso SEM calibração
round(100 * c(
  `sem calibração` = coef(svymean(~saude_boa, des_sc))[2],    # coef()[2] = a proporção "saúde boa" (TRUE)
  `com calibração` = coef(svymean(~saude_boa, des))[2]), 1)


######################################################################
## Pós-estratificação — a calibração mais simples
######################################################################

# totais populacionais por domínio = a projeção do IBGE (V00292, por V00293)
totais <- unique(data.frame(V00293 = pns19$V00293, Freq = pns19$proj_dom))

# postStratify(): ajusta os pesos do 'design' até baterem com os totais conhecidos:
#   1º arg     -> o desenho (aqui com o peso SEM calibração)
#   strata     = ~V00293 -> a variável que define os pós-estratos (domínios)
#   population = totais   -> data.frame com os domínios e a coluna 'Freq' (os totais)
des_ps <- postStratify(
  svydesign(ids = ~UPA_PNS, strata = ~V0024, weights = ~peso_sel, data = pns19, nest = TRUE),
  strata = ~V00293, population = totais)

svymean(~saude_boa, des)      # atalho: peso calibrado (V00291) direto
svymean(~saude_boa, des_ps)   # via oficial: pós-estratificada — reproduz o SE do IBGE

des <- des_ps                 # adotamos a via oficial daqui em diante


######################################################################
## Parte 3 · Estimativas descritivas
######################################################################

# update(): cria/modifica uma variável DENTRO do objeto de desenho.
# Aqui convertemos o lógico saude_ruim em 0/1, para as saídas ficarem limpas:
des <- update(des, saude_ruim = as.numeric(saude_ruim))


######################################################################
## Prevalência — e a validação contra o publicado
######################################################################

svymean(~autoaval, des, na.rm = TRUE)   # média de variável CATEGÓRICA = proporção de cada categoria
svyciprop(~saude_ruim, des)             # svyciprop(): proporção de um indicador 0/1, já com IC 95%


######################################################################
## Estimativas por domínio — e a pegadinha do `subset`
######################################################################

# svyby(): repete uma estimativa em subgrupos
#   ~saude_ruim            -> o que estimar
#   ~sexo                  -> variável que define os subgrupos
#   des                    -> o desenho
#   svymean                -> função aplicada em cada grupo
#   vartype = c("se","cv") -> além da estimativa, reporta erro-padrão e CV
svyby(~saude_ruim, ~sexo, des, svymean, vartype = c("se", "cv"))

# subset(des, ...): cria um SUBDESENHO (a forma CERTA de estimar num subgrupo) —
# preserva estratos e UPAs de toda a amostra para o cálculo correto da variância
svymean(~saude_ruim, subset(des, sexo == "Mulher"))   # só mulheres


######################################################################
## Cruzamentos que não estão nas tabelas (a vantagem do microdado)
######################################################################

svyby(~saude_ruim, ~raca_cor, des, svymean, vartype = "cv")  # proporção por cor/raça, com o CV de cada uma


######################################################################
## O efeito do desenho na PNS real
######################################################################

svymean(~saude_ruim, des, deff = "replace")  # deff = "replace": variância do desenho ÷ a de uma AAS


######################################################################
## Ideação suicida (PHQ-9, item N018)
######################################################################

svyciprop(~ideacao_suicida, des)                          # prevalência nacional (proporção + IC)
svyby(~ideacao_suicida, ~sexo, des, svyciprop, vartype = "ci")  # por sexo; vartype="ci" traz o IC


######################################################################
## Depressão: a escala (PHQ-9) × o diagnóstico
######################################################################

# prevalência de cada medida de depressão (coef() extrai só a estimativa):
round(100 * c(rastreio_PHQ9 = coef(svyciprop(~depr_phq9, des)),
              diagnostico   = coef(svyciprop(~depr_diag, des))), 1)

# cria no desenho uma variável de 4 grupos (cruzamento rastreio × diagnóstico):
des <- update(des, grupo = factor(dplyr::case_when(   # case_when: if/else encadeado
  depr_phq9 & depr_diag ~ "ambos",
  depr_phq9             ~ "só rastreio (PHQ-9)",
  depr_diag             ~ "só diagnóstico",
  TRUE                  ~ "nenhum")))
svymean(~grupo, des, na.rm = TRUE)                    # proporção da população em cada grupo


######################################################################
## Violência por parceiro (módulo V)
######################################################################

# cria os indicadores de violência a partir dos itens do módulo V.
# (X=="1") %in% TRUE -> TRUE se "Sim"; trata ausência (NA) como "não":
des <- update(des,
  viol_fisica = (V01401=="1")%in%TRUE | (V01402=="1")%in%TRUE | (V01403=="1")%in%TRUE |
                (V01404=="1")%in%TRUE | (V01405=="1")%in%TRUE,                   # alguma agressão física
  viol_qq     = (V00201=="1")%in%TRUE | (V00202=="1")%in%TRUE | (V00203=="1")%in%TRUE |
                (V00204=="1")%in%TRUE | (V00205=="1")%in%TRUE | (V01401=="1")%in%TRUE |
                (V01402=="1")%in%TRUE | (V01403=="1")%in%TRUE | (V01404=="1")%in%TRUE |
                (V01405=="1")%in%TRUE | (V02701=="1")%in%TRUE | (V02702=="1")%in%TRUE,  # qualquer violência
  agr_companheiro = (V006 == "01") %in% TRUE)                                     # agressor = cônjuge/companheiro

# prevalências entre as MULHERES (subconjunto do desenho):
round(100 * c(qualquer = coef(svyciprop(~viol_qq,     subset(des, sexo == "Mulher"))),
              fisica   = coef(svyciprop(~viol_fisica, subset(des, sexo == "Mulher")))), 1)
# entre as agredidas, a % cujo principal agressor foi o companheiro:
svyciprop(~agr_companheiro, subset(des, sexo == "Mulher" & viol_qq))


######################################################################
## Orientação sexual (Y008, novo em 2019)
######################################################################

svymean(~orient_sexual, des, na.rm = TRUE)   # proporção em cada categoria de orientação sexual


######################################################################
## Densidade ponderada
######################################################################

# a renda é MUITO assimétrica -> usamos a escala LOG, que "abre" a distribuição.
# log exige valores positivos, então tiramos antes quem tem renda zero:
des_renda <- subset(des, rdpc > 0)

# svyhist(): histograma PONDERADO (usa os pesos do desenho) de log10(renda).
# axes = FALSE para, em seguida, rotular o eixo x em REAIS (e não em "3, 4, 5"):
svyhist(~log10(rdpc), des_renda, main = "", col = "#d6e4f0", breaks = 30,
        xlab = "Renda domiciliar per capita (R$ — escala log)", axes = FALSE)
axis(2)                                    # eixo y (densidade)
reais <- c(100, 300, 1000, 3000, 10000)    # marcas que QUEREMOS mostrar, em reais...
axis(1, at = log10(reais), labels = reais) # ...posicionadas no lugar certo (log10) do eixo


######################################################################
## Estimativas com intervalo de confiança
######################################################################

library(ggplot2)
# cria a faixa etária dentro do desenho:
des <- update(des, faixa = cut(idade, c(17, 29, 44, 59, Inf),
                               labels = c("18-29", "30-44", "45-59", "60+")))
# svyby com vartype="ci" devolve um data.frame com a estimativa e as colunas ci_l / ci_u:
est <- svyby(~saude_ruim, ~faixa, des, svymean, vartype = "ci")

ggplot(est, aes(faixa, saude_ruim)) +
  geom_col(fill = "#2980b9", width = .65) +                   # barras = estimativa
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u), width = .2) +  # barras de erro = IC 95%
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Faixa etária", y = "Saúde ruim/regular") +
  theme_minimal()


######################################################################
## Regressão logística (svyglm)
######################################################################

# svyglm(): regressão respeitando o desenho.
#   saude_ruim ~ ...       -> desfecho (0/1) e preditores
#   design = des           -> o plano amostral
#   family = quasibinomial -> regressão LOGÍSTICA
mod <- svyglm(saude_ruim ~ sexo + faixa + regiao + raca_cor,
              design = des, family = quasibinomial())
round(cbind(OR = exp(coef(mod)), exp(confint(mod))), 2)   # exp(coef)=razões de chance (OR); confint=IC


######################################################################
## Por que não `glm` comum?
######################################################################

dat   <- des$variables                                       # os dados de dentro do desenho
# glm() COMUM: ignora estrato/conglomerado (só usa os pesos, normalizados):
naive <- glm(saude_ruim ~ sexo + faixa + regiao + raca_cor, data = dat,
             family = quasibinomial(), weights = peso_sel_cal / mean(peso_sel_cal))
# compara o erro-padrão de um coeficiente: desenho (svyglm) × ingênuo (glm):
round(c(desenho = summary(mod)$coef["sexoMulher", "Std. Error"],
        ingenuo = summary(naive)$coef["sexoMulher", "Std. Error"]), 4)


######################################################################
## Modelo ordinal da autoavaliação (svyolr)
######################################################################

# transforma a autoavaliação num fator ORDENADO (Muito boa < ... < Muito ruim):
des <- update(des, autoaval_ord = ordered(autoaval,
        levels = c("Muito boa","Boa","Regular","Ruim","Muito ruim")))
# svyolr(): regressão ORDINAL (odds proporcionais) respeitando o desenho:
mod_ord <- svyolr(autoaval_ord ~ sexo + faixa + regiao, design = des)
round(exp(coef(mod_ord)), 2)   # exp(coef) = razões de chance de estar numa categoria pior


######################################################################
## Os dois desenhos e a comparabilidade
######################################################################

pns13 <- readRDS("data/pns2013.rds")   # subconjunto da PNS 2013 (mesma estrutura de variáveis)
# mesmo desenho de 2019, agora para 2013:
des13 <- svydesign(ids = ~UPA_PNS, strata = ~V0024, weights = ~peso_sel_cal,
                   data = pns13, nest = TRUE)


######################################################################
## Testando a diferença — do jeito certo
######################################################################

compara <- function(v) {                        # compara um indicador 'v' entre 2013 e 2019
  e13 <- svyciprop(reformulate(v), des13)        # estimativa em 2013 (reformulate monta ~v)
  e19 <- svyciprop(reformulate(v), des)          # estimativa em 2019
  dif <- coef(e19) - coef(e13)                   # diferença 2019 − 2013
  se  <- sqrt(SE(e13)^2 + SE(e19)^2)             # SE da diferença: amostras INDEPENDENTES -> soma das variâncias
  data.frame(`2013 (%)` = round(coef(e13)*100, 1), `2019 (%)` = round(coef(e19)*100, 1),
             `diferença` = round(dif*100, 1),
             `IC95% da diferença` = sprintf("[%.1f, %.1f]", (dif-1.96*se)*100, (dif+1.96*se)*100),
             check.names = FALSE)
}
do.call(rbind, lapply(
  c(`autoavaliação ruim` = "saude_ruim", `depressão (diagnóstico)` = "depr_diag",
    `ideação suicida` = "ideacao_suicida", `depressão (PHQ-9)` = "depr_phq9"), compara))

