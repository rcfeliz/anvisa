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

# nomes, cargos e links de agenda -------------------------------------------------------------------

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


# base --------------------------------------------------------------------

diretores <- tibble::tibble(
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


# save --------------------------------------------------------------------

usethis::use_data(diretores, overwrite = TRUE)

