data <- GGUM::GenData.GGUM(N=1000, I=20, C=3, model="GGUM", seed=1)

stan_rsp <-  as.matrix(data$data) + matrix(1, nrow =  nrow(data$data), ncol = ncol(data$data))

stan_b <- data$delta.gen

stan_theta <- data$theta.gen

stan_tau <- data$taus.gen[,4:7]

stan_a <- data$alpha.gen

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "resp")
openxlsx::addWorksheet(wb, "b")
openxlsx::addWorksheet(wb, "tau")
openxlsx::addWorksheet(wb, "theta")
openxlsx::addWorksheet(wb, "a")

openxlsx::writeData(wb, "resp", stan_rsp, colNames=FALSE, rowNames=FALSE)
openxlsx::writeData(wb, "b", stan_b, colNames=FALSE, rowNames=FALSE)
openxlsx::writeData(wb, "tau", stan_tau, colNames=FALSE, rowNames=FALSE)
openxlsx::writeData(wb, "theta", stan_theta, colNames=FALSE, rowNames=FALSE)
openxlsx::writeData(wb, "a", stan_a, colNames=FALSE, rowNames=FALSE)

openxlsx::saveWorkbook(wb, file = "Jordan_GGUM_data.xlsx", overwrite=TRUE)
