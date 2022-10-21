
agendas <- anvisa::agendas

data <- "2022-09-06" # certo
data <- "2022-09-05"
pessoa <- "Antonio Barra Torres" # certo
pessoa <- "Karin Schuck Hemesath Mendes"
# data <- "2022-08-06" # erro
# data <- "06/09/2022" # outro formato
datas <- c("2022-09-06", "2022-09-05")
pessoas <- c("Antonio Barra Torres", "Lilian Nazaré Sadalla Peres Pimentel")
# pessoa <- "Julio Trecenti" # erro

u_pessoa <- agendas$u_agenda[agendas$nome == pessoa]

u_agenda <- glue::glue("{u_pessoa}/{data}")
r_agenda <- httr::GET(u_agenda)
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

tibble::tibble(
  pessoa = pessoa,
  dia = data,
  horario = todos_horarios,
  compromisso = todos_compromissos,
  local = todos_locais
)

# fazendo a iteração de data

datas <- c("2022-09-06", "2022-09-05", "2022-08-06")
pessoa <- "Antonio Barra Torres"


consultar_agenda1 <- function(data, pessoa) {
  u_pessoa <- agendas$u_agenda[agendas$nome == pessoa]

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

  }

  tibble::tibble(
    pessoa = pessoa,
    dia = data,
    horario = todos_horarios,
    compromisso = todos_compromissos,
    local = todos_locais
  )

}

purrr::map_dfr(datas, consultar_agenda1, pessoa)

# fazendo a iteração de pessoa

data <- "2022-09-06"
pesso <- "oi"
pessoa <- "Antonio Barra Torres"
pessoas <- c("Antonio Barra Torres", "Lilian Nazaré Sadalla Peres Pimentel")

fazer_link_pessoa <- function(pessoa) {
  agendas$u_agenda[agendas$nome == pessoa]
}

talvez_fazer_link_pessoa <- purrr::possibly(fazer_link_pessoa, character(0))

consultar_agenda2 <- function(data, pessoa) {

  u_pessoa <- purrr::map(pessoa, talvez_fazer_link_pessoa) |>
    as.character()

  empty <- "character(0)"

  if(identical(empty, u_pessoa)) {

    pessoa <- NA_character_
    todos_horarios <- NA_character_
    todos_compromissos <- NA_character_
    todos_locais <- NA_character_

    tibble::tibble(
      pessoa = pessoa,
      dia = data,
      horario = todos_horarios,
      compromisso = todos_compromissos,
      local = todos_locais
    )

  } else if (length(u_pessoa) == 1) {

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

    }

    tibble::tibble(
      pessoa = pessoa,
      dia = data,
      horario = todos_horarios,
      compromisso = todos_compromissos,
      local = todos_locais
    )

  } else {

    tibble::tibble(
      pessoa = pessoa,
      dia = data,
      horario = todos_horarios,
      compromisso = todos_compromissos,
      local = todos_locais
    )

  }

}
