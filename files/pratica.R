# =============================================================================
# Exercício prático: modelos de Fay-Herriot e de erros aninhados
# Introdução à Estimação para Pequenas Áreas
#
# Este script acompanha o documento pratica.qmd. Ele pode ser executado de
# forma independente: os dados vêm do pacote sae (Molina e Marhuenda, 2015),
# e nada é baixado. Execute as seções em ordem (no RStudio, Ctrl+Shift+Enter
# roda a seção atual) e responda às questões no fim de cada parte.
#
# Notação: d indexa as áreas (domínios) e i as unidades dentro de cada área.
# =============================================================================


# ---- 0. Pacotes ------------------------------------------------------------

pacotes <- c("sae", "data.table", "ggplot2", "scales")
faltando <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltando) > 0) install.packages(faltando)

library(sae)
library(data.table)
library(ggplot2)

cor_pontos <- "#1f4e79"
theme_set(theme_minimal())

# O pacote sae espera um data.frame e lê as colunas pelo nome. Por isso, os
# dados são preparados com data.table, mas passados às funções do pacote
# com as.data.frame().


# =============================================================================
# PARTE 1. Modelo de Fay-Herriot: despesa com leite
# =============================================================================

# ---- 1.1 Dados -------------------------------------------------------------
# O conjunto milk traz, para D = 43 pequenas áreas dos Estados Unidos, o
# estimador direto da despesa média com leite fresco em 1989
# (Arora e Lahiri, 1997; You e Chapman, 2006).
#   yi        : estimador direto, theta_d^DIR
#   SD        : desvio padrão estimado do direto; psi_d = SD^2, tratado
#               como conhecido
#   ni        : tamanho da amostra na área
#   MajorArea : quatro grandes regiões, usadas como covariável

data("milk", package = "sae")
milk <- as.data.table(milk)
milk[, `:=`(MajorArea = factor(MajorArea), psi = SD^2)]
milk[, .(D = .N, n_min = min(ni), n_max = max(ni),
         cv_min = min(CV), cv_max = max(CV))]

# ---- 1.2 Modelo ------------------------------------------------------------
# Modelo de Fay-Herriot (Fay e Herriot, 1979):
#
#   theta_d^DIR = x_d' beta + u_d + e_d,   d = 1, ..., D,
#
# com u_d ~ N(0, sigma_u^2) o efeito de área e e_d ~ N(0, psi_d) o erro
# amostral, todos independentes. x_d tem o intercepto e as indicadoras das
# grandes regiões 2, 3 e 4. mseFH() estima sigma_u^2 por máxima
# verossimilhança restrita (MVR) e calcula o EBLUP e seu EQM.

fh <- mseFH(yi ~ MajorArea, vardir = psi, method = "REML",
            data = as.data.frame(milk))
s2u <- fh$est$fit$refvar
fh$est$fit$estcoef
s2u
milk[, `:=`(eblup = as.vector(fh$est$eblup), mse = fh$mse)]

# ---- 1.3 Reprodução do EBLUP -----------------------------------------------
# Com s2u = estimativa de sigma_u^2:
#
#   beta_hat = ( sum_d x_d x_d' / (s2u + psi_d) )^{-1}
#              sum_d x_d theta_d^DIR / (s2u + psi_d)    [mínimos quadrados
#                                                        generalizados]
#   sintético  : x_d' beta_hat
#   gamma_d    = s2u / (s2u + psi_d)                    [fator de
#                                                        encolhimento]
#   EBLUP      = gamma_d * theta_d^DIR + (1 - gamma_d) * sintético

X <- model.matrix(~ MajorArea, milk)
w <- 1 / (s2u + milk$psi)
XtWX <- crossprod(X, w * X)
beta <- solve(XtWX, crossprod(X, w * milk$yi))
milk[, `:=`(sint = drop(X %*% beta), gamma = s2u / (s2u + psi))]
milk[, blup := gamma * yi + (1 - gamma) * sint]
cbind(manual = drop(beta), sae = fh$est$fit$estcoef$beta)
milk[, max(abs(blup - eblup))]   # deve ser praticamente zero

# ---- 1.4 Reprodução do EQM (Prasad e Rao, 1990) ----------------------------
#   g1_d = gamma_d * psi_d                         [modelo conhecido]
#   g2_d = (1 - gamma_d)^2 x_d' (X'WX)^{-1} x_d   [estimação de beta]
#   Vbar = 2 / sum_d (s2u + psi_d)^{-2}            [variância assintótica
#                                                   de s2u]
#   g3_d = psi_d^2 / (s2u + psi_d)^3 * Vbar        [estimação de sigma_u^2]
#   EQM  = g1_d + g2_d + 2 * g3_d                  [com MVR]

vbar <- 2 / sum(1 / (s2u + milk$psi)^2)
milk[, g1 := gamma * psi]
milk[, g2 := (1 - gamma)^2 * rowSums((X %*% solve(XtWX)) * X)]
milk[, g3 := psi^2 / (s2u + psi)^3 * vbar]
milk[, mse_pr := g1 + g2 + 2 * g3]
milk[, max(abs(mse_pr - mse))]   # deve ser praticamente zero
milk[, .(g1 = mean(g1), g2 = mean(g2), g3 = mean(g3))]

# ---- 1.5 Encolhimento ------------------------------------------------------
# Cada direto é puxado para o sintético da sua região com peso 1 - gamma_d.
# O encolhimento é maior onde psi_d é grande.

ggplot(milk, aes(psi, gamma)) +
  geom_point(colour = cor_pontos) +
  labs(x = expression(psi[d]), y = expression(gamma[d]))

# ---- 1.6 Diagnóstico -------------------------------------------------------
# Resíduo padronizado: r_d = (theta_d^DIR - x_d' beta_hat) / sqrt(s2u + psi_d),
# aproximadamente N(0, 1) sob o modelo.

milk[, r := (yi - sint) / sqrt(s2u + psi)]
ggplot(milk, aes(sample = r)) +
  stat_qq(colour = cor_pontos) +
  geom_abline(intercept = 0, slope = 1, colour = "grey50") +
  labs(x = "Quantis normais", y = expression(r[d]))
milk[order(-abs(r))][1:3, .(SmallArea, MajorArea, yi, sint, r)]

# Ganho de precisão: CV = raiz da variância (direto) ou do EQM (EBLUP),
# dividida pela estimativa.
milk[, `:=`(cv_dir = SD / yi, cv_eblup = sqrt(mse) / eblup)]
ggplot(milk, aes(cv_dir, cv_eblup / cv_dir)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_point(colour = cor_pontos) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "CV do direto", y = "CV do EBLUP / CV do direto")
milk[, .(cv_dir = mean(cv_dir), cv_eblup = mean(cv_eblup),
         razao = mean(cv_eblup / cv_dir))]

# ---- 1.7 Questões ----------------------------------------------------------
# 1. O EBLUP calculado à mão e o do pacote coincidem. Que valor de
#    sigma_u^2 foi usado em cada um?
#
# 2. Quais áreas recebem o maior peso do sintético? Relacione com psi_d e
#    com ni.
#
# 3. Qual dos termos g1, g2 e g3 domina o EQM? O que aconteceria com g2 e g3
#    com o dobro de áreas?
#
# 4. Ajuste o modelo por máxima verossimilhança (method = "ML") e compare
#    sigma_u^2 e os EQMs. Por que g1 + g2 + 2 g3 deixa de reproduzir
#    mseFH()?
#
# 5. A covariável MajorArea foi construída a partir das próprias estimativas
#    diretas. Que problema isso pode trazer para a avaliação do modelo?

# Espaço para as respostas:



# =============================================================================
# PARTE 2. Modelo de erros aninhados: área plantada de milho
# =============================================================================

# ---- 2.1 Dados -------------------------------------------------------------
# Dados de Battese, Harter e Fuller (1988): 12 condados de Iowa, divididos em
# segmentos de cerca de 250 hectares. Na amostra de segmentos, a área de
# milho (CornHec) foi medida por entrevista; em todos os segmentos, os pixels
# de milho (CornPix) e de soja (SoyBeansPix) vêm de imagens LANDSAT.
# cornsoybeanmeans traz, por condado, o número de segmentos na população e
# as médias populacionais dos pixels. Seguindo o artigo original, um
# segmento de Hardin com dados inconsistentes é retirado (n = 36).

data("cornsoybean", package = "sae")
data("cornsoybeanmeans", package = "sae")
cs <- as.data.table(cornsoybean)[-33]
medias <- as.data.table(cornsoybeanmeans)
Xmean <- as.data.frame(
  medias[, .(CountyIndex, MeanCornPixPerSeg, MeanSoyBeansPixPerSeg)]
)
Popn <- as.data.frame(medias[, .(CountyIndex, PopnSegments)])
cs[, .N, by = County][, .(D = .N, n = sum(N), n_min = min(N), n_max = max(N))]

# ---- 2.2 Modelo ------------------------------------------------------------
# Modelo de erros aninhados:
#
#   y_di = x_di' beta + u_d + e_di,
#
# com y_di a área de milho no segmento i do condado d, x_di o intercepto e os
# pixels de milho e de soja, u_d ~ N(0, sigma_u^2) o efeito do condado e
# e_di ~ N(0, sigma_e^2) o erro do segmento. rho = sigma_u^2 /
# (sigma_u^2 + sigma_e^2) é a correlação entre segmentos do mesmo condado.
# O parâmetro de interesse é a área média de milho por segmento no condado.

bhf <- eblupBHF(CornHec ~ CornPix + SoyBeansPix, dom = County,
                meanxpop = Xmean, popnsize = Popn, method = "REML",
                data = as.data.frame(cs))
s2u <- bhf$fit$refvar
s2e <- bhf$fit$errorvar
b <- bhf$fit$fixed
b
c(sigma2_u = s2u, sigma2_e = s2e, rho = s2u / (s2u + s2e))
# Battese, Harter e Fuller (1988) relatam sigma_u^2 ~ 140 e sigma_e^2 ~ 147,3.

# ---- 2.3 Interpretação do EBLUP --------------------------------------------
# Com n_d segmentos amostrados no condado, médias amostrais ybar_d e xbar_d,
# e média populacional Xbar_d:
#
#   gamma_d = s2u / (s2u + s2e / n_d)
#   EBLUP  ~ gamma_d * [ ybar_d + (Xbar_d - xbar_d)' beta_hat ]   [regressão
#                                                                 de pesquisa]
#            + (1 - gamma_d) * Xbar_d' beta_hat                   [sintético]
#
# A aproximação vale quando n_d / N_d é desprezível (menos de 1% aqui).

dom <- cs[, .(n_d = .N, ybar = mean(CornHec),
              x_corn = mean(CornPix), x_soy = mean(SoyBeansPix)),
          by = County]
dom <- merge(dom,
             medias[, .(County = CountyIndex, Condado = CountyName,
                        N_d = PopnSegments, X_corn = MeanCornPixPerSeg,
                        X_soy = MeanSoyBeansPixPerSeg)],
             by = "County")
dom[, gamma := s2u / (s2u + s2e / n_d)]
dom[, sint := b[1] + b[2] * X_corn + b[3] * X_soy]
dom[, regpesq := ybar + (X_corn - x_corn) * b[2] + (X_soy - x_soy) * b[3]]
dom[, aprox := gamma * regpesq + (1 - gamma) * sint]
dom <- merge(dom, as.data.table(bhf$eblup)[, .(County = domain, eblup)],
             by = "County")
dom[, .(Condado, n_d, gamma = round(gamma, 2), direto = round(ybar, 1),
        sint = round(sint, 1), regpesq = round(regpesq, 1),
        aprox = round(aprox, 1), eblup = round(eblup, 1))]

# ---- 2.4 Erro quadrático médio ---------------------------------------------
# O sae estima o EQM por bootstrap paramétrico. Para interpretar, compare com
#   g1_d = gamma_d * s2e / n_d     (termo principal)
#   s2e / n_d                      (variância da regressão de pesquisa)
# A execução leva alguns segundos. capture.output() e suppressMessages()
# escondem as mensagens de progresso de pbmseBHF().

set.seed(20261029)
invisible(capture.output(
  boot <- suppressMessages(
    pbmseBHF(CornHec ~ CornPix + SoyBeansPix, dom = County,
             meanxpop = Xmean, popnsize = Popn, B = 200,
             method = "REML", data = as.data.frame(cs))
  )
))

dom <- merge(dom, as.data.table(boot$mse)[, .(County = domain, mse)],
             by = "County")
dom[, `:=`(g1 = gamma * s2e / n_d, v_regpesq = s2e / n_d)]
eqm_long <- melt(dom[, .(n_d, `EBLUP (bootstrap)` = sqrt(mse),
                         `g1` = sqrt(g1),
                         `Regressão de pesquisa` = sqrt(v_regpesq))],
                 id.vars = "n_d", variable.name = "medida",
                 value.name = "raiz_eqm")
ggplot(eqm_long, aes(n_d, raiz_eqm, shape = medida)) +
  geom_point(colour = cor_pontos, size = 2) +
  labs(x = expression(n[d]), y = "Raiz do EQM (ha)", shape = NULL)

# ---- 2.5 Diagnóstico -------------------------------------------------------
# Duas fontes de resíduos: os efeitos de condado preditos u_hat_d e os
# resíduos de segmento e_hat_di = y_di - x_di' beta_hat - u_hat_d.
# Ambos devem ser aproximadamente normais.

res <- rbind(
  data.table(tipo = "Efeitos de condado", valor = bhf$fit$random[, 1]),
  data.table(tipo = "Resíduos de segmento", valor = bhf$fit$residuals)
)
res[, z := (valor - mean(valor)) / sd(valor), by = tipo]
ggplot(res, aes(sample = z)) +
  stat_qq(colour = cor_pontos) +
  geom_abline(intercept = 0, slope = 1, colour = "grey50") +
  facet_wrap(~ tipo) +
  labs(x = "Quantis normais", y = "Resíduo padronizado")

# ---- 2.6 Questões ----------------------------------------------------------
# 1. Compare a coluna eblup com a tabela de estimativas de Battese, Harter e
#    Fuller (1988). As diferenças são pequenas? De onde podem vir?
#
# 2. Por que Cerro Gordo, Hamilton e Worth, com um segmento cada, recebem o
#    mesmo gamma_d? E por que, ainda assim, seus EBLUPs são diferentes?
#
# 3. Em Worth, o direto é bem menor que o sintético. Para que lado o EBLUP
#    vai, e quanto?
dom[Condado == "Worth", .(direto = ybar, sint, eblup)]
#
# 4. Recalcule o ajuste sem retirar a observação 33. Como mudam sigma_u^2,
#    sigma_e^2 e o EBLUP de Hardin?
#
# 5. Ajuste o mesmo modelo para a soja (SoyBeansHec). A correlação
#    intraclasse é maior ou menor que para o milho? O que isso implica para
#    o encolhimento?

# Espaço para as respostas:


