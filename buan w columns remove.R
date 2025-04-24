---
  Title: BUAN 381 Final Project
Team: Brianna Floyd, Katie Luong, Anastasia Lomtadze, Sophia Saenger
---
  
Load libraries
library(tidyverse)
library(dplyr)

##Load data
reviews <- read_csv("https://raw.githubusercontent.com/katieluong33/BUAN-381/refs/heads/main/Avaliacoes.csv")
clients <- read_csv("https://raw.githubusercontent.com/katieluong33/BUAN-381/refs/heads/main/Clientes.csv")
operational_costs <- read_csv("https://raw.githubusercontent.com/katieluong33/BUAN-381/refs/heads/main/Custos_Operacionais.csv")
returns <- read_csv("https://raw.githubusercontent.com/katieluong33/BUAN-381/refs/heads/main/Devolucoes.csv")
stores <- read_csv("https://raw.githubusercontent.com/katieluong33/BUAN-381/refs/heads/main/Lojas.csv")
products <- read_csv("https://raw.githubusercontent.com/katieluong33/BUAN-381/refs/heads/main/Produtos.csv")
sales <- read_csv("https://raw.githubusercontent.com/katieluong33/BUAN-381/refs/heads/main/Vendas.csv")


#Remove Unnecessary colums
sales <- sales %>% select(-Colaborador_ID)
clients <- clients %>% select(-Nome, -Imagem)
operational_costs <- operational_costs %>% select(-Custo_ID,-"Tipo de Custo", -Data,)
products <- products %>% select(-Nome, -Imagem, -Custo_Aquisição, -Descrição)
returns <- returns %>% select(-"Motivo da Devolução", -"Data da Devolução")
reviews <- reviews %>% select(-Comentário)



full_data <- sales %>%
  left_join(products, by = "Produto_ID") %>%
  left_join(clients, by = "Cliente_ID") %>%
  left_join(reviews, by = "Produto_ID") %>%
  left_join(returns, by = "Produto_ID") %>%
  left_join(stores, by = "Loja_ID") %>%
  left_join(operational_costs, by = "Loja_ID")


# Create new column for order price (regression target)
full_data <- full_data %>%
  mutate(Order_Price = Quantidade.x * `Preço Unitário`)
```

