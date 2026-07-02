# =====================================================================
# Curadoria do subset da PNS 2013 (morador selecionado, 18+) — HARMONIZADO
# com 2019 para a comparação no tempo. Inclui só o que compara: desenho,
# autoavaliação (âncora), PHQ-9/ideação suicida (N018) e depressão (Q092).
# (Orientação sexual e o módulo de violência mudam entre edições -> ficam só em 2019.)
# Peso COM calibração: V00291. Posições do input_PNS_2013.txt.
# =====================================================================
suppressMessages({library(readr); library(dplyr); library(survey)})

txt <- "C:/Users/caios/AppData/Local/Temp/pns2013/PNS_2013.txt"

spec <- tibble::tribble(
  ~name,      ~s,   ~e,
  "V0001",      1,    2,
  "V0024",      3,    9,
  "UPA_PNS",   10,   16,   # 2013: UPA_PNS tem 7 dígitos
  "V0015",     30,   31,
  "C006",     106,  106,
  "C008",     115,  117,
  "C009",     118,  118,
  "M001",     521,  521,
  "N001",     552,  552,
  "N010",     560,  560, "N011",561,561, "N012",562,562, "N013",563,563,
  "N014",     564,  564, "N015",565,565, "N016",566,566, "N017",567,567,
  "N018",     568,  568,
  "Q002",     805,  805, "Q030",847,847, "Q092",971,971,
  "V00291",  1405, 1418,   # peso morador selecionado COM calibração
  "V00293",  1448, 1452,
  "VDD004A", 1458, 1458,
  "VDF002",  1461, 1468, "VDF003",1469,1476
)

d <- read_fwf(txt, fwf_positions(spec$s, spec$e, spec$name),
              col_types = strrep("c", nrow(spec)), na = "")

phq <- c("N010","N011","N012","N013","N014","N015","N016","N017","N018")

d <- d |>
  mutate(
    ano        = 2013L,
    idade      = as.integer(C008),
    selecionado= M001 == "1",
    peso_sel_cal = as.numeric(V00291) / 1e8,
    regiao     = factor(substr(V0001,1,1), levels = as.character(1:5),
                        labels = c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste")),
    sexo       = factor(C006, levels = c("1","2"), labels = c("Homem","Mulher")),
    raca_cor   = factor(C009, levels = as.character(1:5),
                        labels = c("Branca","Preta","Amarela","Parda","Indígena")),
    autoaval   = factor(N001, levels = as.character(1:5),
                        labels = c("Muito boa","Boa","Regular","Ruim","Muito ruim")),
    saude_boa  = N001 %in% c("1","2"),
    saude_ruim = N001 %in% c("3","4","5"),
    ideacao_suicida = N018 %in% c("2","3","4"),
    depr_diag  = Q092 == "1",
    phq9       = rowSums(sapply(d[phq], \(x) as.integer(x) - 1)),
    depr_phq9  = phq9 >= 10,
    rdpc       = as.numeric(VDF003),
    renda_dom  = as.numeric(VDF002)
  )

ds <- d |> filter(selecionado, idade >= 18, !is.na(peso_sel_cal), peso_sel_cal > 0)

options(survey.lonely.psu = "adjust")
des <- svydesign(ids = ~UPA_PNS, strata = ~V0024, weights = ~peso_sel_cal, data = ds, nest = TRUE)

cat("Selecionados 18+ (2013):", nrow(ds), "\n\n")
cat("Autoavaliação boa/muito boa (publicado 2013: 66,2%):\n"); print(round(svymean(~saude_boa, des, na.rm=TRUE)*100, 1))
cat("\nDepressão — diagnóstico (publicado 2013: 7,6%):\n");     print(round(svymean(~depr_diag, des, na.rm=TRUE)*100, 1))
cat("\nIdeação suicida (N018 > nenhum dia):\n");                print(round(svymean(~ideacao_suicida, des, na.rm=TRUE)*100, 1))
cat("\nDepressão — rastreio PHQ-9>=10:\n");                     print(round(svymean(~depr_phq9, des, na.rm=TRUE)*100, 1))

saveRDS(ds, "data/pns2013.rds")
cat("\nSalvo em data/pns2013.rds (", nrow(ds), "linhas x", ncol(ds), "colunas )\n")
