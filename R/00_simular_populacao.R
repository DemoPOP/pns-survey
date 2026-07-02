# =====================================================================
# População sintética para a "crescente" do plano amostral
# (substitui o exemplo das fazendas por unidades demográficas)
#
# Espelha a hierarquia da PNS: REGIÃO (estrato) > MUNICÍPIO (conglomerado/UPA)
# > DOMICÍLIO > PESSOA. Como geramos a população INTEIRA, a "verdade" é
# conhecida — então dá para mostrar, a cada passo do plano (AAS -> estrato ->
# conglomerado -> pesos), a estimativa batendo (ou não) na verdade.
#
# Desfecho-âncora: autoavaliação de saúde "ruim/regular" (saude_ruim = 1),
# que depende de região, idade e renda, COM efeito aleatório de município
# (correlação intraclasse) — é isso que faz o conglomerado inflar a variância.
# =====================================================================
set.seed(2026)

regioes <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")
cod       <- c(Norte = "NO", Nordeste = "NE", "Centro-Oeste" = "CO", Sudeste = "SE", Sul = "SU")
n_mun     <- c(6, 8, 5, 8, 6);                 names(n_mun)     <- regioes  # municípios por região (desigual)
ef_reg    <- c(0.60, 0.80, 0.00, -0.40, -0.55); names(ef_reg)  <- regioes  # log-odds de "saúde ruim"
renda_reg <- c(6.7, 6.6, 7.0, 7.2, 7.15);       names(renda_reg) <- regioes # meanlog da renda domiciliar

# --- municípios (conglomerados) ---
mun <- data.frame(
  regiao    = rep(regioes, n_mun),
  municipio = unlist(lapply(regioes, function(r) sprintf("%s%02d", cod[[r]], seq_len(n_mun[[r]])))),
  stringsAsFactors = FALSE
)
mun$u_mun <- rnorm(nrow(mun), 0, 0.7)                 # efeito aleatório do município (intraclasse)
mun$n_dom <- sample(40:90, nrow(mun), replace = TRUE) # domicílios por município

# --- domicílios ---
dom <- mun[rep(seq_len(nrow(mun)), mun$n_dom), c("regiao", "municipio", "u_mun")]
dom$domicilio <- sprintf("%s-D%04d", dom$municipio, ave(seq_len(nrow(dom)), dom$municipio, FUN = seq_along))
dom$n_pes     <- sample(1:5, nrow(dom), replace = TRUE, prob = c(.18, .30, .27, .15, .10))
dom$renda_dom <- round(rlnorm(nrow(dom), meanlog = renda_reg[dom$regiao], sdlog = 0.5))

# --- pessoas (18+) ---
pes <- dom[rep(seq_len(nrow(dom)), dom$n_pes), ]
pes$pessoa <- ave(seq_len(nrow(pes)), pes$domicilio, FUN = seq_along)
pes$idade  <- sample(18:85, nrow(pes), replace = TRUE)
pes$sexo   <- sample(c("Homem", "Mulher"), nrow(pes), replace = TRUE)
pes$rdpc   <- pmax(1, round(pes$renda_dom / pes$n_pes))   # renda domiciliar per capita

# autoavaliação "saúde ruim/regular" (1) — região + idade + renda + município
z_rdpc <- as.numeric(scale(log(pes$rdpc)))
lp <- -1.1 + ef_reg[pes$regiao] + pes$u_mun + 0.015 * (pes$idade - 50) - 0.45 * z_rdpc
pes$saude_ruim <- rbinom(nrow(pes), 1, plogis(lp))

populacao <- pes[, c("regiao", "municipio", "domicilio", "pessoa",
                     "sexo", "idade", "rdpc", "saude_ruim")]
rownames(populacao) <- NULL
populacao$id <- seq_len(nrow(populacao))

# --- a VERDADE conhecida (parâmetros populacionais) ---
cat("Municípios:", nrow(mun), " | Domicílios:", nrow(dom), " | Pessoas:", nrow(populacao), "\n\n")
cat("P(saúde ruim/regular) na população:", round(mean(populacao$saude_ruim), 4), "\n\n")
cat("Por região (verdade):\n"); print(round(tapply(populacao$saude_ruim, populacao$regiao, mean), 3))
cat("\nRenda domiciliar per capita média (verdade): R$", round(mean(populacao$rdpc)), "\n")

# correlação intraclasse aproximada (ANOVA 1 fator: município)
a <- anova(lm(saude_ruim ~ municipio, data = populacao))
icc <- (a["municipio", "Mean Sq"] - a["Residuals", "Mean Sq"]) /
       (a["municipio", "Mean Sq"] + (mean(table(populacao$municipio)) - 1) * a["Residuals", "Mean Sq"])
cat("Correlação intraclasse (saúde ruim por município):", round(icc, 3), "\n")

saveRDS(populacao, "data/populacao_sintetica.rds")
cat("\nSalvo em data/populacao_sintetica.rds\n")
