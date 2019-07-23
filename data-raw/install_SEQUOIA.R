# Installation de SEQUOIA

pck_cran <- c("tcltk", "devtools")
for (a in 1:length(pck_cran)){
  if (!(pck_cran[a] %in% rownames(installed.packages()))) {
    install.packages(pck_cran[a], dependencies = TRUE)
    library(pck_cran[a])
  }
}

pck_comp <- c("svDialogs", "SEQUOIA")
for (a in 1:length(pck_comp)){
  if ((pck_comp[a] %in% rownames(installed.packages()))) {
    remove.packages(pck_comp[a], lib = .libPaths()[1])
    PCK <- tcltk::tk_choose.files(default = "~/Downloads", caption = paste0("Selectionner le package ", pck_comp[a]))
    install.packages(PCK, destdir = NULL, lib = .libPaths()[1], repos = NULL, dependencies = T)
  } else {
    PCK <- tcltk::tk_choose.files(default = "~/Downloads", caption = paste0("Selectionner le package ", pck_comp[a]))
    install.packages(PCK, destdir = NULL, lib = .libPaths()[1], repos = NULL, dependencies = T)
  }
}
