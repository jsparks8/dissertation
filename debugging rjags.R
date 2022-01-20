df_test <- df %>% 
  dplyr::left_join(starts[["a"]] %>%
  as.data.frame() %>%
  dplyr::mutate(family = "a",
                parameter = paste0("a[", row_number(), "]")),
  by=c("family", "parameter")) %>%
dplyr::left_join(starts[["b"]] %>%
                   as.data.frame() %>%
                   dplyr::mutate(family = "b",
                                 parameter = paste0("b[", row_number(), "]")),
                 by=c("family", "parameter")) %>%
dplyr::left_join(starts[["theta"]] %>%
                   as.data.frame() %>%
                   dplyr::mutate(family = "theta",
                                 parameter = paste0("theta[", row_number(), "]")),
                 by=c("family", "parameter")) %>%
  dplyr::mutate(`..x` = as.numeric(unlist(`..x`)),
                `..y` = as.numeric(unlist(`..y`)),
                `.` = as.numeric(unlist(`.`)))

df_test2 <- df_test %>%
  unite("Initial", c(`..x`, `..y`, `.`), na.rm=TRUE) %>%
  dplyr::mutate(Initial=as.numeric(Initial))



grid.arrange(
  qplot(estimate, Initial, data = (df_test2 %>% dplyr::filter(family == "a")), main = "a"),
  qplot(estimate, Initial, data = (df_test2 %>% dplyr::filter(family == "b")), main = "b"),
  qplot(estimate, truth, data = (df_test %>% dplyr::filter(family == "tau")), main = "tau"),
  qplot(estimate, Initial, data = (df_test2 %>% dplyr::filter(family == "theta")), main = "theta")
)
