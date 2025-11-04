# 📊 Análise Exploratória e Estatística com R: Dataset Air Quality

## 🌟 Visão Geral do Projeto

Este projeto pessoal de Análise Exploratória de Dados (EDA) utiliza o dataset nativo `airquality` do RStudio para demonstrar competências em tratamento de dados, visualização estatística e testes de hipóteses.

O foco foi realizar uma análise completa dos dados de qualidade do ar, preparando o conjunto de dados para *insights* robustos e validando diferenças estatisticamente significativas entre grupos de observação.

## 🛠️ Stack Tecnológico

| Categoria | Tecnologia | Uso no Projeto |
| :--- | :--- | :--- |
| **Linguagem** | R | Linguagem principal de análise estatística e desenvolvimento do projeto. |
| **Visualização (Estática)** | `ggplot2` | Criação de gráficos estéticos e informativos (Boxplot, Histograma, Dispersão). |
| **Visualização (Interativa)** | `Plotly` | Produção de gráficos interativos para uma exploração de dados mais rica. |
| **Estatística** | ANOVA de Uma Via | Testes de hipóteses para validar diferenças significativas entre grupos (meses). |
| **Tratamento de Dados** | Pacotes Tidyverse (Implícito) | Conversões de tipo, tratamento de *outliers* e imputação de valores ausentes. |

## ⚙️ Etapas Chave do Processo de Análise

O projeto seguiu um rigoroso processo de preparação e análise de dados:

### 1. Tratamento e Imputação de Dados
* **Conversões:** Alterações necessárias nos tipos das variáveis e conversões numéricas.
* **Valores Ausentes:** Exploração da distribuição de `missmap` e aplicação de **imputação pela média** de cada variável para o tratamento de valores nulos e ausentes.
* **Outliers:** Verificação e tratamento de valores atípicos para garantir a robustez dos modelos.

### 2. Visualização Exploratória (EDA)
* Utilização de **Boxplots** para avaliar a distribuição e a presença de *outliers* por variável.
* Criação de **Histogramas** para verificar a distribuição de frequência.
* Geração de **Gráficos de Dispersão** para explorar a correlação entre as variáveis de qualidade do ar.

### 3. Análise Estatística Avançada
* **Teste ANOVA de Uma Via:** Aplicação de testes de Análise de Variância para comparar as médias das variáveis de qualidade do ar em diferentes meses.
* **Resultados:** Obtenção de **p-values significantes (p < 0.05)** para alguns meses, indicando que a diferença nas médias de qualidade do ar entre esses grupos não foi aleatória.

## 🚀 Próximos Passos (Integração com BI)

A linguagem R é poderosa pela sua riqueza de pacotes estatísticos. O próximo passo deste projeto será explorar a **integração do R no Power BI**, utilizando os *scripts* de tratamento e visualização desenvolvidos para levar análises estatísticas complexas diretamente para o ambiente de *Business Intelligence*.

---
