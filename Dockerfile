# Use official Plumber image
FROM rocker/plumber:latest

# Install packages we need
RUN R -q -e 'install.packages(c("nflreadr","dplyr","stringr"), repos="https://cloud.r-project.org")'

# Copy API
WORKDIR /app
COPY api.R plumber.R /app/

EXPOSE 8000
CMD ["R", "-q", "-e", "pr <- plumber::plumb(\"/app/api.R\"); pr$run(host=\"0.0.0.0\", port=as.integer(Sys.getenv(\"PORT\", 8000)))"]
