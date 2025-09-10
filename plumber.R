# plumber.R
pr <- plumber::plumb("api.R")
port <- as.integer(Sys.getenv("PORT", "8000"))
pr$run(host = "0.0.0.0", port = port)
