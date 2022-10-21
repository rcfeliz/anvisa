# scraper agendas -----------------------------------------------------------------

u_agendas <- "https://www.gov.br/anvisa/pt-br/acessoainformacao/Agendas"
r_agendas <- httr::GET(u_agendas)
h_agendas <- xml2::read_html(r_agendas)


# hierarquias e órgãos ----------------------------------------------------

todos_orgaos_h1 <- h_agendas |>
  xml2::xml_find_first("//ul[@class='lista-pessoas']") |>
  xml2::xml_find_all("./li") |>
  xml2::xml_find_first("./a") |>
  xml2::xml_text() |>
  stringr::str_squish()

todas_pessoas_h1 <- h_agendas |>
  xml2::xml_find_first("//ul[@class='lista-pessoas']") |>
  xml2::xml_find_all("./li") |>
  xml2::xml_find_first(".//p[@class='nome-autoridade']") |>
  xml2::xml_text() |>
  stringr::str_squish()

todas_pessoas_h2 <- h_agendas |>
  xml2::xml_find_first("//ul[@class='lista-pessoas']") |>
  xml2::xml_find_all("./li/a") |>
  xml2::xml_text() |>
  stringr::str_squish()

todos_h1 <- tibble::tibble(
  nome = todas_pessoas_h1,
  orgao = todos_orgaos_h1,
  hierarquia = "h1"
  ) |>
  dplyr::filter(!is.na(orgao))

todos_orgaos_h2 <- h_agendas |>
  xml2::xml_find_first("//ul[@class='lista-pessoas']") |>
  xml2::xml_find_all("./li/ul/li") |>
  xml2::xml_find_first("./a") |>
  xml2::xml_text() |>
  stringr::str_squish()

todas_pessoas_h2 <- h_agendas |>
  xml2::xml_find_first("//ul[@class='lista-pessoas']") |>
  xml2::xml_find_all("./li/ul/li") |>
  xml2::xml_find_first(".//p[@class='nome-autoridade']") |>
  xml2::xml_text() |>
  stringr::str_squish()

todos_h2 <- tibble::tibble(
  nome = todas_pessoas_h2,
  orgao = todos_orgaos_h2,
  hierarquia = "h2"
) |>
  dplyr::filter(!is.na(orgao))

todos_orgaos_h3 <- h_agendas |>
  xml2::xml_find_first("//ul[@class='lista-pessoas']") |>
  xml2::xml_find_all("./li/ul/li/ul/li") |>
  xml2::xml_find_first("./a") |>
  xml2::xml_text() |>
  stringr::str_squish()

todas_pessoas_h3 <- h_agendas |>
  xml2::xml_find_first("//ul[@class='lista-pessoas']") |>
  xml2::xml_find_all("./li/ul/li/ul/li") |>
  xml2::xml_find_first(".//p[@class='nome-autoridade']") |>
  xml2::xml_text() |>
  stringr::str_squish()

todos_h3 <- tibble::tibble(
  nome = todas_pessoas_h3,
  orgao = todos_orgaos_h3,
  hierarquia = "h3"
) |>
  dplyr::filter(!is.na(orgao))

todos_orgaos_h4 <- h_agendas |>
  xml2::xml_find_first("//ul[@class='lista-pessoas']") |>
  xml2::xml_find_all("./li/ul/li/ul/li/ul/li") |>
  xml2::xml_find_first("./a") |>
  xml2::xml_text() |>
  stringr::str_squish()

todas_pessoas_h4 <- h_agendas |>
  xml2::xml_find_first("//ul[@class='lista-pessoas']") |>
  xml2::xml_find_all("./li/ul/li/ul/li/ul/li") |>
  xml2::xml_find_first(".//p[@class='nome-autoridade']") |>
  xml2::xml_text() |>
  stringr::str_squish()

todos_h4 <- tibble::tibble(
  nome = todas_pessoas_h4,
  orgao = todos_orgaos_h4,
  hierarquia = "h4"
) |>
  dplyr::filter(!is.na(orgao))

todos_h <- todos_h1 |>
  dplyr::bind_rows(todos_h2, todos_h3, todos_h4) |>
  dplyr::mutate(
    h1 = dplyr::case_when(
      hierarquia == "h1" ~ orgao
    ),
    h2 = dplyr::case_when(
      hierarquia == "h2" ~ orgao
    ),
    h3 = dplyr::case_when(
      hierarquia == "h3" ~ orgao
    ),
    h4 = dplyr::case_when(
      hierarquia == "h4" ~ orgao
    )
  ) |>
  dplyr::group_by(nome) |>
  tidyr::fill(h1, .direction="down") |>
  dplyr::arrange(desc(hierarquia)) |>
  dplyr::slice(1) |>
  dplyr::ungroup()

# infos -------------------------------------------------------------------

todos_nomes <- h_agendas |>
  xml2::xml_find_all("//p[@class='nome-autoridade']") |>
  xml2::xml_text() |>
  stringr::str_squish() |>
  forcats::fct_inorder()

todos_cargos <- h_agendas |>
  xml2::xml_find_all("//p[@class='cargo-autoridade']") |>
  xml2::xml_text() |>
  stringr::str_squish() |>
  append(list(x="Coordenador"), 20) # isso não é muito reprodutível

todas_agendas <- h_agendas |>
  xml2::xml_find_all("//a[@title='Ver Agenda']") |>
  xml2::xml_attr("href")

agendas <- tibble::tibble(
  nome = todos_nomes,
  cargo = as.character(todos_cargos),
  u_agenda = todas_agendas
) |>
  dplyr::left_join(todos_h, by="nome") |>
  dplyr::mutate(
    h2 = dplyr::case_when(
      !is.na(h1) & is.na(h2) ~ "-",
      TRUE ~ h2
    ),
    h3 = dplyr::case_when(
      is.na(h1) & !is.na(h2) & is.na(h3) ~ "-",
      !is.na(h1) & !is.na(h2) & is.na(h3) ~ "-",
      TRUE ~ h3
    ),
    h4 = dplyr::case_when(
      is.na(h1) & !is.na(h2) & !is.na(h3) & is.na(h4) ~ "-",
      !is.na(h1) & !is.na(h2) & !is.na(h3) & is.na(h4) ~ "-",
      TRUE ~ h4
    )
  ) |>
  tidyr::fill(c(orgao, hierarquia, h1, h2, h3, h4), .direction="down") |>
  dplyr::mutate(dplyr::across(
    .cols=c(h1,h2,h3,h4),
    .fns=~ifelse(is.na(.x), "-", .x)
  )) |>
  dplyr::transmute(
    nome = forcats::fct_inorder(nome),
    cargo,
    orgao,
    hierarquia,
    h1, h2, h3, h4,
    u_agenda
  )

# readr::write_rds(agendas, "data-raw/agendas.rds")

usethis::use_data(agendas, overwrite = TRUE)

# parse agendas -----------------------------------------------------------

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
