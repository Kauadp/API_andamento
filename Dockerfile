# Dockerfile
FROM r-base:4.3.1

# Instala pacotes do sistema
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev

# Copia arquivos da API
COPY api_andamento.R /app/api_andamento.R
COPY modelos /app/modelos

# Instala pacotes R
RUN R -e "install.packages(c('plumber','dplyr','forcats','jsonlite'), repos='https://cloud.r-project.org')"

WORKDIR /app

# Expõe porta
EXPOSE 8000

# Comando para rodar a API
CMD ["R", "-e", "pr <- plumber::plumb('api_andamento.R'); pr$run(host='0.0.0.0', port=8000)"]
