# =====================================================================
# Curadoria do subset da PNS 2019 (morador selecionado, 18+)
# Lê o microdado de largura fixa do IBGE PELO CAMINHO MANUAL (sem PNSIBGE):
# readr::read_fwf + posições do input_PNS_2019.txt. Deriva a âncora
# (autoavaliação) + as 4 vitrines + covariáveis e salva data/pns2019.rds.
# =====================================================================
suppressMessages({library(readr); library(dplyr); library(survey)})

# AJUSTE o caminho do .txt do IBGE (descompactado de PNS_2019.zip):
txt <- "C:/Users/caios/AppData/Local/Temp/pns2019/PNS_2019.txt"

# --- posições (start, end) extraídas do input_PNS_2019.txt ---
spec <- tibble::tribble(
  ~name,      ~s,   ~e,
  "V0001",      1,    2,   # UF
  "V0024",      3,    9,   # estrato
  "UPA_PNS",   10,   18,   # UPA (conglomerado / PSU)
  "V0006_PNS", 19,   22,
  "V0015",     23,   24,
  "C006",     108,  108,   # sexo
  "C008",     117,  119,   # idade
  "C009",     120,  120,   # cor/raça
  "M001",     527,  527,   # entrevista do adulto selecionado (1 = realizada)
  "N001",     557,  557,   # autoavaliação de saúde  (ÂNCORA)
  "N010",     564,  564, "N011",565,565, "N012",566,566, "N013",567,567,
  "N014",     568,  568, "N015",569,569, "N016",570,570, "N017",571,571,
  "N018",     572,  572,   # PHQ-9 item 9: ideação suicida/autolesão
  "Q00201",   807,  807, "Q00202",808,808, "Q003",809,810,  # hipertensão (validação/exemplo)
  "Q092",     986,  986,   # diagnóstico de depressão
  "V001",    1246, 1246, "V00201",1248,1248, "V00202",1249,1249, "V00203",1250,1250,
  "V00204",  1251, 1251, "V00205",1252,1252, "V006",1254,1255,     # violência: ameaças + agressor
  "V01401",  1257, 1257, "V01402",1258,1258, "V01403",1259,1259,
  "V01404",  1260, 1260, "V01405",1261,1261,                       # violência física
  "V02701",  1266, 1266, "V02702",1267,1267,                       # violência sexual
  "Y008",    1304, 1304,   # orientação sexual (novo em 2019)
  "V0028",   1367, 1380,   # peso domicílio (s/ calibração)
  "V0029",   1381, 1394,   # peso morador selecionado (s/ calibração)
  "V00291",  1426, 1439,   # peso morador selecionado COM calibração (oficial)
  "V00283",  1500, 1502, "V00293",1503,1507,  # domínios de projeção (pós-estratificação)
  "V00292",  1466, 1482,   # projeção da população do domínio do morador selecionado
  "VDD004A", 1514, 1514,   # nível de instrução
  "VDF002",  1519, 1526, "VDF003",1527,1534    # renda domiciliar / per capita
)

d <- read_fwf(txt,
              fwf_positions(spec$s, spec$e, spec$name),
              col_types = strrep("c", nrow(spec)), na = "")

phq <- c("N010","N011","N012","N013","N014","N015","N016","N017","N018")

d <- d |>
  mutate(
    ano        = 2019L,
    idade      = as.integer(C008),
    selecionado= M001 == "1",
    peso_sel    = as.numeric(V0029)  / 1e8,      # SEM calibração (14.8 => /1e8)
    peso_sel_cal= as.numeric(V00291) / 1e8,      # COM calibração (oficial)
    proj_dom    = as.numeric(V00292) / 1e8,       # projeção da população do domínio (V00293)
    peso_dom    = as.numeric(V0028)  / 1e8,
    regiao     = factor(substr(V0001,1,1), levels = as.character(1:5),
                        labels = c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste")),
    sexo       = factor(C006, levels = c("1","2"), labels = c("Homem","Mulher")),
    raca_cor   = factor(C009, levels = as.character(1:5),
                        labels = c("Branca","Preta","Amarela","Parda","Indígena")),
    autoaval   = factor(N001, levels = as.character(1:5),
                        labels = c("Muito boa","Boa","Regular","Ruim","Muito ruim")),
    saude_boa  = N001 %in% c("1","2"),                 # boa/muito boa  (validação ~66%)
    saude_ruim = N001 %in% c("3","4","5"),             # regular/ruim/muito ruim
    ideacao_suicida = N018 %in% c("2","3","4"),        # PHQ item 9 > "nenhum dia"
    depr_diag  = Q092 == "1",                           # diagnóstico autorreferido
    phq9       = rowSums(sapply(d[phq], \(x) as.integer(x) - 1)),  # escore 0–27
    depr_phq9  = phq9 >= 10,                            # rastreio positivo
    orient_sexual = factor(Y008, levels = as.character(c(1:5,9)),
                        labels = c("Heterossexual","Homossexual","Bissexual",
                                   "Outra","Não sabe","Não respondeu")),
    rdpc       = as.numeric(VDF003),
    renda_dom  = as.numeric(VDF002)
  )

# recorte: morador selecionado, 18 anos ou mais, com peso válido
ds <- d |> filter(selecionado, idade >= 18, !is.na(peso_sel_cal), peso_sel_cal > 0)

# ---------- VALIDAÇÃO contra o publicado ----------
options(survey.lonely.psu = "adjust")
des    <- svydesign(ids = ~UPA_PNS, strata = ~V0024, weights = ~peso_sel_cal, data = ds, nest = TRUE)
des_sc <- svydesign(ids = ~UPA_PNS, strata = ~V0024, weights = ~peso_sel,     data = ds, nest = TRUE)

cat("Selecionados 18+:", nrow(ds), "\n")
cat("Soma do peso calibrado (≈ população 18+):", format(round(sum(ds$peso_sel_cal)), big.mark = " "), "\n\n")
cat("Autoavaliação boa/muito boa — CALIBRADO (publicado 66,1%):\n"); print(round(svymean(~saude_boa, des, na.rm = TRUE)*100, 1))
cat("Autoavaliação boa/muito boa — SEM calibração (contraste):\n");  print(round(svymean(~saude_boa, des_sc, na.rm = TRUE)*100, 1))
cat("\nIdeação suicida (N018 > nenhum dia):\n");           print(round(svymean(~ideacao_suicida, des, na.rm = TRUE)*100, 1))
cat("\nDepressão — diagnóstico (publicado: 10,2%):\n");    print(round(svymean(~depr_diag, des, na.rm = TRUE)*100, 1))
cat("\nDepressão — rastreio PHQ-9>=10:\n");                print(round(svymean(~depr_phq9, des, na.rm = TRUE)*100, 1))
cat("\nOrientação sexual (%):\n");                          print(round(svymean(~orient_sexual, des, na.rm = TRUE)*100, 1))

saveRDS(ds, "data/pns2019.rds")
cat("\nSalvo em data/pns2019.rds (", nrow(ds), "linhas x", ncol(ds), "colunas )\n")
