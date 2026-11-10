# Análise de Assimetria, Transformação e Correlação de Solubilidade Química

Este repositório contém um notebook Python voltado para a Análise Exploratória de Dados (EDA) e pré-processamento de um *dataset* de solubilidade química. O objetivo principal do estudo é examinar a distribuição das variáveis (descritores moleculares), tratar a assimetria dos dados contínuos/de contagem e analisar as correlações internas e com a variável alvo (solubilidade).

---

## 📌 Conteúdo do Notebook

O notebook está estruturado nas seguintes etapas principais:

1. **Carregamento e Inspeção dos Dados**
   - Importação dos conjuntos de treino e teste (`solTrainX.txt`, `solTestX.txt`, `solTrainY.txt`, `solTestY.txt`).
   - Verificação das dimensões dos *datasets* e confirmação de ausência de valores nulos (`NA`).

2. **Separação e Classificação dos Descritores**
   - Identificação de 208 *fingerprints* moleculares (variáveis binárias iniciando com `FP`).
   - Separação de 20 descritores físico-químicos em:
     - **Variáveis Contínuas (4):** `SurfaceArea1`, `SurfaceArea2`, `HydrophilicFactor`, `MolWeight`.
     - **Variáveis de Contagem (16):** `NumSulfur`, `NumNitrogen`, `NumChlorine`, `NumHalogen`, `NumOxygen`, `NumRotBonds`, etc.

3. **Análise de Assimetria (*Skewness*)**
   - Cálculo da assimetria para todos os grupos de variáveis.
   - Comparação da assimetria observada nas *fingerprints* com a assimetria teórica dada pela proporção de 1s ($p$).
   - Levantamento da proporção de zeros nas variáveis de contagem (ex.: `NumSulfur` com ~87% de zeros).

4. **Transformação de Dados (PowerTransformer - Yeo-Johnson)**
   - Aplicação da transformação de Yeo-Johnson (`sklearn.preprocessing.PowerTransformer`) para atenuar a assimetria dos 20 descritores não-binários.
   - Ajuste dos parâmetros ($\lambda$) no conjunto de treino e aplicação direta no conjunto de teste.
   - Comparação dos valores de *skewness* antes e depois da transformação.

5. **Análise de Correlação**
   - Matriz de correlação de Pearson entre todas as variáveis transformadas.
   - Identificação de pares com alta colinearidade ($|r| > 0.75$ e $|r| > 0.90$).
   - Geração de *heatmap* de correlação para os descritores não-binários.
   - Análise de correlação (Pearson e Spearman) entre os descritores e a variável de resposta (Solubilidade `y`).
   - Gráficos de dispersão (*scatter plots*) com linha de tendência linear para os descritores contínuos contra a solubilidade.

---

## 🛠️ Tecnologias e Bibliotecas Utilizadas

- **Python 3**
- **Pandas**: Manipulação e análise de dados estruturados.
- **NumPy**: Operações vetoriais e numéricas.
- **SciPy (`scipy.stats.skew`)**: Cálculo estatístico de assimetria.
- **Scikit-Learn (`PowerTransformer`)**: Transformação de dados.
- **Matplotlib**: Visualização de dados e gráficos.

---

## 📂 Estrutura dos Arquivos Esperados

Para executar o notebook corretamente, os seguintes arquivos de dados devem estar presentes no diretório raiz ou ambiente do Colab:
- `solTrainX.txt` (atributos de treino delimitados por tabulação)
- `solTestX.txt` (atributos de teste delimitados por tabulação)
- `solTrainY.txt` (rótulos/solubilidade de treino)
- `solTestY.txt` (rótulos/solubilidade de teste)

---

## 📊 Principais Resultados Observados

- **Assimetria Reduzida:** A transformação Yeo-Johnson reduziu significativamente o *skewness* de variáveis contínuas (ex.: `HydrophilicFactor` reduziu a assimetria de 3.41 para 0.25).
- **Falta de Ajuste em Dados Esparsos:** Variáveis com altíssima proporção de zeros (como `NumSulfur` e `NumChlorine`) mantiveram assimetria residual elevada mesmo após a transformação.
- **Correlação com a Solubilidade:** Atributos como Peso Molecular (`MolWeight`) e Número de Carbonos (`NumCarbon`) apresentaram forte correlação negativa com a solubilidade (aproximadamente $-0.66$ e $-0.61$, respectivamente).