#' Consultar agenda
#'
#' Consulta a agenda de uma pessoa da direção da Anvisa em determinado dia
#'
#' @param data Data que se deseja pesquisar
#' @param pessoa Pessoa que se deseja pesquisar
#'
#' @return Retorna uma tibble com 4 colunas: dia do compromisso,
#' horário do compromisso, descrição do compromisso e local do compromisso
#'
#' @example consultar_agenda("2022-09-06", "Antonio Barra Torres")
#'
#' @export
consultar_agenda <- function(data=NULL, pessoa=NULL) {

  if(is.null(data)) {
    stop("Forneça uma ou mais datas!")
  }

  if(length(data) > 1) {
    stop("Para fornecer mais de uma data, use purrr::map_dfr({datas}, consultar_agenda, {pessoa})")
  }

  if(is.null(pessoa)) {
    stop("Indique uma pessoa!")
  }

  if(length(pessoa) > 1) {
    stop("Não indique mais do que uma pessoa!")
  }

  # deixar as letras minúsculas para comparar: menor chance de erro!

  agendas$nome_lower <- tolower(agendas$nome)

  pessoa_lower <- tolower(pessoa)


  u_pessoa <- agendas$u_agenda[agendas$nome_lower == pessoa_lower]

  if(length(u_pessoa) == 0) {
   stop("Esta pessoa não trabalha na Anvisa")
  }

  u_agenda <- glue::glue("{u_pessoa}/{data}")

  r_agenda <- httr::GET(u_agenda)

  if(r_agenda$status_code == 404) {

    todos_horarios <- NA_character_
    todos_compromissos <- NA_character_
    todos_locais <- NA_character_

  } else {

    h_agenda <- xml2::read_html(r_agenda)

    todos_horarios <- h_agenda |>
      xml2::xml_find_all("//li[@class='item-compromisso-wrapper']") |>
      xml2::xml_find_all("//div[@class='compromisso-horarios']") |>
      xml2::xml_text() |>
      stringr::str_squish()

    todos_compromissos <- h_agenda |>
      xml2::xml_find_all("//li[@class='item-compromisso-wrapper']") |>
      xml2::xml_find_all("//h2") |>
      xml2::xml_text() |>
      stringr::str_squish()

    todos_locais <- h_agenda |>
      xml2::xml_find_all("//li[@class='item-compromisso-wrapper']") |>
      xml2::xml_find_all("//div[@class='compromisso-local']") |>
      xml2::xml_text() |>
      stringr::str_squish()

    if(rlang::is_empty(todos_locais) | length(todos_locais) != length(todos_compromissos)) { # essa condicional está errada
      n <- length(todos_compromissos)
      todos_locais <- NA_character_
    }

  }

  tibble::tibble(
    pessoa = pessoa,
    dia = lubridate::ymd(data),
    horario = todos_horarios,
    compromisso = todos_compromissos,
    local = todos_locais
  )


}

