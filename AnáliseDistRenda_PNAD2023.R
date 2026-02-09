
# Versão do R utilizada foi a versão 4.3.3 (2024-02-29 ucrt)

# Os dados foram obtidos da Pesquisa Nacional por Amostra de Domicílio 2023, o presente estudo, no entanto, para fins de simplificação
# e aprendizado, desconsidera os pesos amostrais da pesquisa.

## OBSERVAÇÃO: Além dos gráficos e tabelas apresentados, foram também foram também inseridas breves análises dos recursos construídos 
# longo do scrypt de forma a auxiliar na interpretação dos dados.


# Configurando o diretório que será utilizado.

setwd("diretórioPNAD2023")


# Checagem e instalação dos pacotes necessários para rodar o programa.

if(!require(tidyverse)) install.packages("tidyverse")

library(tidyverse)

if(!require(ineq)) install.packages("ineq")

library(ineq)


# Abrindo o arquivo da base para criação do Data Frame

read.csv("base.csv")

base = read.csv("base.csv")


# Removendo as observações nas quais a raça consta como "Ignorado" e checando ao final, para confirmar a remoção das linhas.

unique(base$raca)

base_filter1 <- filter(base, !raca ==  "Ignorado")

unique(base_filter1$raca)


# Verificando a quantidade de observações que constam como NA ou NAN no data frame.

sapply(base_filter1, function(x) sum(is.na(x)))
sapply(base_filter1, function(x) sum(is.nan(x)))


# Removendo linhas que possuem NAs em rendimento principal e total simultaneamente e checando para confirmar se as linhas foram removidas.

base_filter2 <- base_filter1 %>%
  filter(!(is.na(rendimento_todas_fontas) & is.na(rend_trabalho_principal)))

sapply(base_filter2, function(x) sum(is.na(x)))


# Observa-se que há ainda NAs na variável rend_trabalho_principal, portanto, será criado um novo Data Frame completamente livre de NAs.

base_limpa <- na.omit(base_filter2)

sapply(base_limpa, function(x) sum(is.na(x)))


# Agregando pretos e pardos em uma única categoria.

base_limpa <- base_limpa %>%
  mutate(raca_agregada = case_when (
    raca == "Parda" | raca == "Preta" ~ "Negros",
    TRUE ~ as.character(raca)
  ))

unique(base_limpa$raca_agregada)


# Criando coluna para aferir se observações possuem outra fonte de rendimento além da principal.

base_limpa <- base_limpa %>%
  mutate(mais_de_uma_renda = ifelse(rendimento_todas_fontas > rend_trabalho_principal, "Sim", "Não"))


### Criando tabela com os principais dados
# OBSEERVAÇÃO: a tabela possui algumas colunas extras, que serão utilizadas posteriormente na manipulação dos dados.

tabela <- base_limpa %>%
  group_by(raca_agregada, sexo) %>%
  summarise( media_rend_total = mean(rendimento_todas_fontas),
             media_rend_principal = mean(rend_trabalho_principal),
             mediana_rend_total = median(rendimento_todas_fontas),
             mediana_rend_principal = median(rend_trabalho_principal),
             max_rend_total = max(rendimento_todas_fontas),
             max_rend_principal = max(rend_trabalho_principal),
             desvio_rend_total = sd(rendimento_todas_fontas),
             desvio_rend_principal = sd (rend_trabalho_principal),
             variancia_rend_total = var(rendimento_todas_fontas),
             variancia_rend_principal = var(rend_trabalho_principal),
             total_pessoas = n(),
             qntd_mais_de_uma_renda = sum(mais_de_uma_renda == "Sim"),
             proporcao_mais_de_uma_renda = round(qntd_mais_de_uma_renda/total_pessoas, 3),
             gini = Gini(rendimento_todas_fontas),
             soma_renda_total = sum(rendimento_todas_fontas),
             soma_renda_principal = sum(rend_trabalho_principal),
             .groups = "drop"
  ) %>%
  select(!total_pessoas)

#### Análise: Os dados evidenciam fortes desigualdades de renda por raça e gênero: homens brancos têm a maior média de renda total,
# enquanto mulheres negras têm a menor, refletindo os efeitos combinados do racismo e do machismo. A desigualdade interna também é
# marcante, com o maior índice de Gini entre homens amarelos e o menor entre mulheres indígenas. Além disso, a proporção de pessoas 
# com mais de uma fonte de renda é maior entre mulheres indígenas, sugerindo uma necessidade maior de complementar renda.
# Por fim, a concentração da soma total da renda em grupos como homens brancos evidencia a profunda assimetria na distribuição dos
# recursos.


# Calculando o índice de Gini geral

gini_geral <- Gini(base_limpa$rendimento_todas_fontas)
print(gini_geral)

##### Análise: o índice de Gini geral observado indica um nível relativamente alto de desigualdade na sociedade brasileira, o que poderá ser observado com mais detalhes mais à frente, conforme a exploração dos dados.

# Criando coluna "grupo" para auxiliar na construção dos gráficos

tabela <- tabela %>%
  mutate(grupo = case_when (
    sexo == "Homem"& raca_agregada == "Negros" ~ "Homens Negros",
    sexo == "Homem"& raca_agregada == "Branca" ~ "Homens Brancos",
    sexo == "Homem"& raca_agregada == "Indígena" ~ "Homens Indígenas",
    sexo == "Homem"& raca_agregada == "Amarela" ~ "Homens Amarelos",
    sexo == "Mulher" & raca_agregada == "Negros" ~"Mulheres Negras",
    sexo == "Mulher" & raca_agregada == "Branca" ~"Mulheres Brancas",
    sexo == "Mulher" & raca_agregada == "Indígena" ~ "Mulheres Indígenas",
    sexo == "Mulher" & raca_agregada == "Amarela" ~ "Mulheres Amarelas",
    TRUE ~ "Outros")) 

### Criando um gráfico de barras para ilustrar a MÉDIA da RENDA TOTAL por gênero e raça.

ggplot(tabela, aes(x = reorder(grupo, media_rend_total),
                   y = media_rend_total,
                   fill = grupo)) +
  geom_col(show.legend = FALSE) + 
  labs(
    title = "Média dos Rendimentos Totais por Sexo e Raça",
    x = "Grupo",
    y = "Rendimento Médio (R$)"
  ) +
  scale_fill_manual(values = c("Homens Negros" = "#BF40BF",
                               "Mulheres Negras" = "#FFD700",
                               "Mulheres Brancas" = "#0056C2", 
                               "Homens Brancos" = "#00D25F",    
                               "Homens Indígenas" =  "#FF6969",
                               "Mulheres Amarelas" = "#FF8F33",
                               "Mulheres Indígenas" = "#666666",
                               "Homens Amarelos" = "#00B8E8"   
  ))+
  
  scale_y_continuous(labels = scales::label_number(accuracy = 1, big.mark = ".", decimal.mark = ",")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

### Criando um gráfico de barras para ilustrar a MÉDIA da RENDA PRINCIPAL por gênero e raça.

ggplot(tabela, aes(x = reorder(grupo, media_rend_principal),
                   y = media_rend_principal,
                   fill = grupo)) +
  geom_col(show.legend = FALSE) + 
  labs(
    title = "Média dos Rendimentos Principais por Sexo e Raça",
    x = "Grupo",
    y = "Rendimento Médio (R$)"
  ) +
  scale_fill_manual(values = c("Homens Negros" = "#BF40BF",
                               "Mulheres Negras" = "#FFD700",
                               "Mulheres Brancas" = "#0056C2", 
                               "Homens Brancos" = "#00D25F",    
                               "Homens Indígenas" =  "#FF6969",
                               "Mulheres Amarelas" = "#FF8F33",
                               "Mulheres Indígenas" = "#666666",
                               "Homens Amarelos" = "#00B8E8"   
  ))+
  scale_y_continuous(labels = scales::label_number(accuracy = 1, big.mark = ".", decimal.mark = ",")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

##### Análise: Observando os dois gráficos gerados, é possível concluir que a média dos rendimentos, sejam eles totais ou os 
# rendimentos habituais, segue um mesmo padrão de comportamento, no qual mulheres negras e indígenas são as que possuem as menores
# rendas, enquanto homens brancos e amarelos são os que possuem as maiores.

#################################################### EXTRAS ############################################################################

## Criando gráficos comparativos SEGMENTADOS por raça e por sexo para a MÉDIA da renda de TODAS AS FONTES.

ggplot(tabela, aes(x = sexo, y = media_rend_total, fill = sexo)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ raca_agregada) +
  labs (
    title = "Rendimento Médio Total por Sexo para cada Raça",
    x = "Sexo",
    y = "Rendimento Médio (R$)" )+
  scale_fill_manual(values = c("Homem" = "#00D25F",
                               "Mulher" = "#0056C2"))+
  scale_y_continuous(labels = scales::label_number(accuracy = 1, big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(hjust = 0.5))

## Criando gráficos comparativos SEGMENTADOS por raça e por sexo para a MÉDIA da RENDA PRINCIPAL.

ggplot(tabela, aes(x = sexo, y = media_rend_principal, fill = sexo)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ raca_agregada) +
  labs (
    title = "Rendimento Médio Principal por Sexo para cada Raça",
    x = "Sexo",
    y = "Rendimento Médio (R$)" )+
  scale_fill_manual(values = c("Homem" = "#00D25F",
                               "Mulher" = "#0056C2"))+
  scale_y_continuous(labels = scales::label_number(accuracy = 1, big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12),
            plot.title = element_text(hjust = 0.5))

##### Análise: Aqui, os gráficos também seguem comportamentos semelhantes quando se trata da renda total e da renda principal, nesse
# caso, é possível observar que a maior disparidade de rendimentos entre sexos dentro de uma mesma raça está dentre a população amarela, 
# enquanto a menor está dentre indígenas. Também ée possível observar que na maioria dos o rendimento médio de homens supera o das mulheres,
# a única exceção é dentre indígenas, raça dentro da qual os rendimentos médios são bastante semelhantes.


## Criando um gráfico de DISPERSÃO que ilustra a distribuição da população por FAIXA DE RENDIMENTO.

base_limpa <- base_limpa %>%
  mutate(faixa_renda = cut(rendimento_todas_fontas,
                           breaks = c(0, 1000, 2000, 4000, 6000, 10000, Inf),
                           labels = c("Até 1K", "1K-2K", "2K-4K", "4K-6K", "6K-10K", "Mais de 10K"),
                           include.lowest = TRUE))

resumo <- base_limpa %>%
  group_by(faixa_renda, raca_agregada, sexo) %>%
  summarise(n= n(), .groups = "drop") %>%
  mutate(grupo = paste(raca_agregada, sexo, sep = "-"))

ggplot(resumo, aes(x = faixa_renda, y = n, color = grupo)) +
  geom_point(size = 3) +
  geom_line(aes(group = grupo), linewidth = 1, alpha = 0.7) +
  labs(title = "Distribuição das Pessoas por Faixa de Rendimento e Grupo",
       x = "Faixa de Rendimento Total (R$)",
       y = "Número de Pessoas",
       color = "Raça e Sexo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

##### Análise: A partir do gráfico, é possível abstrair que a maioria da população, independentemente do recorte, está inserida na faixa
# de renda compreendida entre R$ 1K e 2K, além disso, pode-se observar que dentro das faixas de rendimento abaixo dos R$ 2K há o predomínio
# de homens e mulheres negros, enquanto nos faixas de rendimento acima dos 10K homens e mulheres brancas são a maioria. #


## Criando gráfico que indica a DISTRIBUIÇÃO das pessoas que posuem MAIS DE UM RENDIMENTO.

tabela_renda_multipla <- base_limpa %>%
  filter(mais_de_uma_renda == "Sim") %>%
  group_by(raca_agregada, sexo) %>%
  summarise(total = n(), .groups = "drop")

ggplot(tabela_renda_multipla, aes(x = raca_agregada, y = total, fill = sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Pessoas com Mais de Um Rendimento por Raça e Sexo",
       x = "Raça",
       y = "Número de Pessoas",
       fill = "Sexo") +
  scale_fill_manual(values = c("Homem" = "#00D25F",
                               "Mulher" = "#0056C2"))+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

##### Análise: No gráfico gerado, observa-se mulheres negras é o grupo que predomina com diferença substancial em relação aos demais 
# grupos na quantidade de pessoas com mais de um rendimento, superando inclusive homens negros. Quando se parte para a análise de brancos, 
# essa relação se inverte, e há mais homens com mais de uma fonte de rendimento do que mulheres.


## Para aprofundar mais a análise, é possível também a criação de um gráfico que ilustra a distribuição de rendimento dentro das FAIXAS DE RENDA.

tabela_perfil_faixa <- base_limpa %>%
  filter(mais_de_uma_renda == "Sim", !is.na(faixa_renda)) %>%
  group_by(faixa_renda, raca_agregada, sexo) %>%
  summarise(n = n(), .groups = "drop")

ggplot(tabela_perfil_faixa, aes(x = faixa_renda, y = n, fill = interaction(raca_agregada, sexo))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribuição de Pessoas com Mais de Um Rendimento por Faixa de Renda",
       x = "Faixa de Rendimento Total",
       y = "Número de Pessoas",
       fill = "Raça e Sexo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

##### Análise: Esse gráfico, apesar de trazer dados semelhantes ao anterior, permite aprofundar mais a análise iniciada. Nele, é possível
# notar que, apesar das mulheres negras serem as que mais possuem mais de uma fonte de renda, são elas que predominam nas faixas de renda
# total mais baixas, o que revela que o aumento da quantidade de fontes de renda, não necessariamente implica no correspondente aumento do
# nível de renda quando se trata desse grupo. Observa-se, ainda, que quando se trata de mulheres brancas ocorre algo semelhante. Apesar de
# dentre os brancos haver uma quantidade bem próxima de mulheres e homens com mais de uma fonte de renda, são os homens que predominam de 
# significante nas faixas mais elevadas de renda.


## Construindo gráfico que indica a APROPRIAÇÃO DE RENDA por PERCENTIS.

base_percentis <- base_limpa %>%
  filter(rendimento_todas_fontas > 0) %>%
  mutate(percentil = ntile(rendimento_todas_fontas, 100))

tabela_percentis <- base_percentis %>%
  group_by(percentil) %>%
  summarise(soma_renda = sum(rendimento_todas_fontas), .groups = "drop") %>%
  mutate(percentual_apropriado = soma_renda / sum(soma_renda) * 100)

tabela_percentis_filter <- tabela_percentis %>%
  filter(percentil %in% c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 99))

ggplot(tabela_percentis_filter, aes(x = as.factor(percentil), y = percentual_apropriado)) +
  geom_bar(stat = "identity", fill = "#00D25F") +
  labs(title = "Apropriação da Renda por Percentis",
       x = "Percentil",
       y = "Percentual Apropriado") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

##### Análise: Esse gráfico nos permite observar um dado já muito conhecido da realidade brasileira: quase 60% da renda total está 
# concentrada nas mãos do 1% mais rico da população.


## Criando Curva de Lorenz para os dados de rendimento de todas as fontes.

curva <- Lc(base_limpa$rendimento_todas_fontas)

ggplot() +
  geom_line(aes(x = curva$p, y = curva$L), color = "#00D25F", size = 2) +  
  geom_abline(slope = 1, intercept = 0, color = "deeppink", size = 1) +     
  labs(title = "Curva de Lorenz",
       x = "Quantil",
       y = "% da Renda") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

##### Análise: A construção da Curva de Lorenz nos permite observar de outro modo o dado constatado com o gráfico anterior, ao  
# facilitar a comparação da distribuição da renda com a linha representativa da perfeita igualdade, evidenciando com ainda mais 
# contraste a elevada desigualdade que permeia a realidade brasileira.















