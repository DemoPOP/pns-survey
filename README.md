# Explorando dados da PNS com o pacote survey

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21155543.svg)](https://doi.org/10.5281/zenodo.21155543)
[![Licença: CC BY 4.0](https://img.shields.io/badge/Licen%C3%A7a-CC%20BY%204.0-blue.svg)](https://creativecommons.org/licenses/by/4.0/deed.pt-br)
[![Site](https://img.shields.io/badge/site-GitHub%20Pages-1d6e56.svg)](https://demopop.github.io/pns-survey/)

🔗 **Site:** <https://demopop.github.io/pns-survey/>

Material do workshop **Explorando dados da PNS com o pacote `survey`** (DEMOPOP 2026), por **Caio César Soares Gonçalves** — Departamento de Demografia / CEDEPLAR-UFMG.

O workshop ensina a analisar **dados de amostra complexa** da **Pesquisa Nacional de Saúde (PNS, IBGE)** — edições de **2013 e 2019** — com o pacote **`survey`** do R. Numa população sintética de verdade conhecida, monta-se o plano amostral peça por peça (AAS → estratos → conglomerados → pesos); em seguida declara-se o desenho real da PNS **sem** o pacote `PNSIBGE`, valida-se contra as tabelas publicadas e exploram-se estimativas por domínio, temas subexplorados (ideação suicida, depressão, violência por parceiro, orientação sexual), gráficos, regressão e a comparação 2013 × 2019.

## Comece aqui

[**→ Abrir o Tutorial**](https://demopop.github.io/pns-survey/tutorial.html) · [**→ Abrir os Slides**](https://demopop.github.io/pns-survey/slides.html)

## Arquivos para download

- [**Script R completo** (`tutorial.R`)](tutorial.R) — versão executável do tutorial, para rodar linha a linha no RStudio
- [`data/populacao_sintetica.rds`](data/populacao_sintetica.rds) — população sintética da Parte 1
- [`data/pns2019.rds`](data/pns2019.rds) — subconjunto da PNS 2019 (morador selecionado, 18+)
- [`data/pns2013.rds`](data/pns2013.rds) — subconjunto da PNS 2013, harmonizado

Para reproduzir, baixe o **script R** e os dados, mantendo os `.rds` numa pasta `data/` no diretório de trabalho. O primeiro bloco do script instala automaticamente os pacotes necessários (`survey`, `dplyr`, `ggplot2`); depois é só rodar de cima para baixo.

Os subconjuntos foram preparados a partir dos [microdados públicos do IBGE](https://www.ibge.gov.br/estatisticas/sociais/saude/9160-pesquisa-nacional-de-saude.html) pelos scripts da pasta [`R/`](R/) — leitura do arquivo de largura fixa **sem** o `PNSIBGE`, com validação contra as tabelas publicadas.

## Autor

**Caio César Soares Gonçalves** é professor do Departamento de Demografia do Cedeplar/UFMG, atuando em métodos estatísticos e computacionais e em ciência de dados. Doutor em População, Território e Estatísticas Públicas (ENCE/IBGE), mestre em Economia (UFRGS), especialista em Ciência de Dados e Big Data e economista (PUC Minas). Pesquisa métodos de produção de estatísticas demográficas, sociais e ambientais — combinando censos, pesquisas amostrais, registros administrativos e *big data*, com ênfase na estimação em pequenos domínios e na desagregação espaço-temporal de indicadores. Coordenou a área de indicadores sociais da Fundação João Pinheiro (IDHM e IMRS) e colaborou com IBGE, Ipea, IPEDF, UNFPA, BID e o *Office for National Statistics* (Reino Unido). Recebeu o *Young Statistician Prize* da IAOS (2021) e foi premiado no concurso de melhor tese de doutorado da ABE (2024).

[ORCID](https://orcid.org/0000-0002-3366-7560) · [Google Scholar](https://scholar.google.com/citations?user=_uOyB2AAAAAJ&hl=en) · [Lattes](http://lattes.cnpq.br/6829577347369187) · [ResearchGate](https://www.researchgate.net/profile/Caio-Cesar-Soares-Goncalves) · [LinkedIn](https://www.linkedin.com/in/caiocsg/)

## Como citar

> Gonçalves, C. C. S. (2026). *Explorando dados da PNS com o pacote survey* [material de workshop]. Workshop de Métodos Demográficos — DEMOPOP, Departamento de Demografia / CEDEPLAR-UFMG. <https://doi.org/10.5281/zenodo.21155543>

## Licença

Material distribuído sob a licença [Creative Commons Atribuição 4.0 Internacional (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/deed.pt-br) — livre para compartilhar e adaptar, inclusive para fins comerciais, desde que citada a autoria.

## Dúvidas e correções

Encontrou um erro ou tem sugestão? Abra uma *issue* no [repositório do material](https://github.com/DemoPOP/pns-survey) ou escreva para caiocsg@cedeplar.ufmg.br.

*Versão 1.0.0 · julho de 2026 · workshop realizado em 03/07/2026.*

<p style="text-align:center; margin-top:2.5rem;">
  <img src="assets/cedeplar-logo.png" height="46" style="margin:0 14px;">
  <img src="assets/log_face.png" height="46" style="margin:0 14px;">
  <img src="assets/Logo_UFMG.png" height="46" style="margin:0 14px;">
</p>
