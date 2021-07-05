######### general info --------
## name: r_dependency_check.R
## purpose: check all the dependent pacakges for running the follow-up R scripts
## version: 0.2.0

######### variables --------
## -- sys variables --
CRAN_PKG <- c("devtools", "BiocManager", "R.matlab", "ggrepel", "foreach", "doParallel", "parallel", "splines")
JZ_PKG <- c("RBioFS", "RBioArray")


######## R script ------
# -- check and install pacakges --
cat("CRAN package(s)\n")
for (i in CRAN_PKG) {
  cat(paste0("\t", i, "..." ))
  if (!requireNamespace(i, quietly = TRUE)) {
    cat("not found. \n\tAttempt to install from CRAN...")
    tryCatch({
      install.packages(i)
      cat("done\n\n")
    }, error = function(e){
      cat("failed\n\n")
      quit(status = 1)
    })
  } else {
    cat("ok\n")
  }
}

cat("\n")
cat("GitHub package(s)\n")
for (i in JZ_PKG) {
  cat(paste0("\t", i, "..." ))
  if (!requireNamespace(i, quietly = TRUE)) {
    cat("not found. \n\tAttempt to install from GitHub...")
    if (!requireNamespace(i)) {
      tryCatch({
        devtools::install_github(paste0("jzhangc/git_", i, "/", i), repos = BiocManager::repositories(), ref = "beta")
        cat("done\n\n")
      }, error = function(e) {
        cat("fail\n\n")
        quit(status = 1)
      })
    }
  } else {
    cat("ok\n")
  }
}


# # -- clean up the mess from pathview --
suppressMessages(rm(cpd.simtypes, gene.idtype.bods, gene.idtype.list, korg, i, CRAN_PKG, JZ_PKG)) # pathview
