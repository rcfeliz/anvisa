test_that("pesquisa de agenda funciona", {

  # Espera retornar uma tibble com os compromissos
  d1 <- "2022-09-06" # data existente
  p1 <- "Antonio Barra Torres" # pessoa existente
  a1 <- consultar_agenda(d1, p1)
  expect_equal(nrow(a1), 4)
  expect_equal(ncol(a1), 5)

  # Espera retornar uma tibble com NAs
  d2 <- "2022-08-06" # data não existente
  p2 <- "Antonio Barra Torres" # pessoa existente
  a2 <- consultar_agenda(d2, p2)
  expect_equal(a2$horario, NA_character_)
  expect_equal(a2$compromisso, NA_character_)
  expect_equal(a2$local, NA_character_)

  # Espera dar erro
  d3 <- "2022-09-06" # data existente
  p3 <- "Julio Trecenti" # pessoa não existente
  expect_error(consultar_agenda(d3, p3))

  # Espera dar erro
  p4 <- "Antonio Barra Torres" # pessoa existente
  expect_error(consultar_agenda(p4))

  # Espera dar erro
  d5 <- "2022-09-06" # data existente
  expect_error(consultar_agenda(d5))

  # Espera dar certo
  d6 <- c("2022-09-06", "2022-09-05", "2022-08-06") # datas existentes
  p6 <- "Antonio Barra Torres" # pessoa existente
  a6 <- purrr::map_dfr(d6, consultar_agenda, p6)
  expect_error(consultar_agenda(a6))
  expect_equal(nrow(a6), 10)
  expect_equal(ncol(a6), 5)

  # Espera dar erro
  d7 <- "2022-09-06" # data existente
  p7 <- c("Antonio Barra Torres", "Lilian Nazaré Sadalla Peres Pimentel")
  expect_error(purrr::map_dfr(p7, consultar_agenda, d7))

  # Espera dr erro
  d8 <- "09/06/2022"
  p8 <-  "Antonio Barra Torres"
  expect_warning(consultar_agenda(d8, p8))

})
