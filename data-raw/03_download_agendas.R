# base de agendas

# fazendo a base ----------------------------------------------------------

map_df_progress <- function(.x, .f, ..., .id = NULL) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(total = length(.x), force = TRUE)

  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map_df(.x, f, ..., .id = .id)
}

pessoas <- anvisa::agendas |>
  dplyr::filter(
    (h1 == "Segunda Diretoria" & h2 == "-") |
    h2 == "Gerência-Geral de Alimentos (GGALI)"
  ) |>
  dplyr::pull(nome)

# dt_referencia_inicio <- as.Date("2017-12-01")  # data mais antiga que eu encontrei até agora que tem algum evento
# dt_referencia_fim <- lubridate::today()
dt_referencia_inicio <- as.Date("2017-01-01")
dt_referencia_fim <- as.Date("2020-12-31")
datas <- seq(dt_referencia_inicio, dt_referencia_fim, by="days")

# pb_pessoa = txtProgressBar(min = 0, max = length(pessoas), initial = 0)
# stepi <- 0

agendas_anvisa <- c()
for(pessoa in pessoas) {
  # setTxtProgressBar(pb_pessoa,stepi)
  # stepi <- stepi + 1
  agenda <- map_df_progress(datas, consultar_agenda, pessoa)
  agendas_anvisa <- agendas_anvisa |>
    dplyr::bind_rows(agenda)
}

# close(pb)

cargos <- anvisa::agendas |>
  dplyr::select(pessoa = nome, cargo, orgao)

agendas_ggali <- agendas_anvisa |>
  dplyr::filter(!is.na(compromisso)) |>
  dplyr::left_join(cargos) |>
  dplyr::select(pessoa, cargo, dia, horario, compromisso, local) |>
  dplyr::mutate(pessoa = forcats::fct_inorder(pessoa))

usethis::use_data(agendas_ggali, overwrite = TRUE)

