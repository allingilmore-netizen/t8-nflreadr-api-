# Use the official Plumber image
FROM rstudio/plumber:latest

# Install system libs that help CRAN packages compile (belt & suspenders)
USER root
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev zlib1g-dev ca-certificates \
 && rm -rf /var/lib/apt/lists/*

# Install R packages we need
RUN R -q -e "install.packages(c('nflreadr','dplyr','stringr'), repos='https://cloud.r-project.org')"

# Copy API files
WORKDIR /app
COPY api.R plumber.R /app/

# Expose port (Render uses $PORT)
EXPOSE 8000

# Start the API
CMD ["R", "-q", "-e", "pr <- plumber::plumb('/app/api.R'); pr$run(host='0.0.0.0', port=as.integer(Sys.getenv('PORT', 8000)))"]
