# Dockerfile
FROM r-base:4.3.1

# Instala pacotes do sistema necessários
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Define diretório de trabalho
WORKDIR /app

# Copia arquivos da API
COPY api_andamento.R /app/api_andamento.R
COPY modelos /app/modelos

# Instala pacotes R
RUN R -e "install.packages(c('plumber','dplyr','forcats','jsonlite','caret','randomForest'), repos='https://cloud.r-project.org')"

# Expõe porta
EXPOSE 8000

# Comando para rodar a API
CMD ["R", "-e", "pr <- plumber::plumb('api_andamento.R'); pr$run(host='0.0.0.0', port=8000)"]
