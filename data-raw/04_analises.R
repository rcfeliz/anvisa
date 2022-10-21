# analises
agendas <- anvisa::agendas_tidy
agendas <- anvisa::agendas_2021
agendas <- anvisa::agendas_ggali

governo <- stringr::regex("anvisa|minist|secret|procurad|gerente|presidente da república|assesso|deputad|senado|embaixada", TRUE)
interno <- stringr::regex("audi[eê]ncia p[úu]blica", TRUE)
empresa <- stringr::regex("empresa|grupo|associaç|representante", TRUE)

nomes_empresas <- agendas |>
  dplyr::filter(!stringr::str_detect(compromisso, governo)) |>
  dplyr::transmute(
    compromisso,
    nome_empresa = stringr::str_to_lower(compromisso),
    nome_empresa = stringr::str_extract(nome_empresa, "(?<=empresa |grupo ).+"),
    nome_associacao = stringr::str_to_lower(compromisso),
    nome_associacao = stringr::str_extract(nome_associacao, "associação .+"),
    nome_empresa = ifelse(is.na(nome_empresa), nome_associacao, nome_empresa),
    nome = stringr::str_remove(nome_empresa, "\\.$")
  ) |>
  dplyr::filter(!is.na(nome)) |>
  dplyr::distinct(nome) |>
  dplyr::pull(nome)


# teste <- agendas |>
#   dplyr::filter(!is.na(compromisso)) |>
#   dplyr::mutate(
#     compromisso = stringr::str_to_lower(compromisso),
#     teste = any(sapply(nomes_empresas, grepl, compromisso))
#   )

agendas_empresas <- agendas |>
  dplyr::filter(!is.na(compromisso)) |>
  dplyr::mutate(
    ano = lubridate::year(dia),
    reuniao_governo = stringr::str_detect(compromisso, governo),
    reuniao_interna = stringr::str_detect(compromisso, interno),
    reuniao_empresa = dplyr::if_else(
      condition =
        !reuniao_governo &
        !reuniao_interna &
        stringr::str_detect(compromisso, empresa),
      true = TRUE,
      false = FALSE
    )
  ) |>
  dplyr::group_by(pessoa, cargo, ano, dia) |>
  dplyr::summarise(
    empresa = any(reuniao_empresa),
    governo = any(reuniao_governo)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(pessoa, ano, dia, cargo, empresa, governo)

orgaos <- anvisa::agendas |>
  dplyr::select(pessoa=nome, orgao, hierarquia)

taxa_reuniao_empresas <- agendas_empresas |>
  dplyr::left_join(orgaos) |>
  dplyr::count(pessoa, orgao, cargo, hierarquia, empresa, ano) |>
  dplyr::group_by(pessoa, ano) |>
  dplyr::mutate(
    total = sum(n),
    prop = n/sum(n),
    perc = formattable::percent(prop)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(empresa) |>
  dplyr::select(
    pessoa,
    orgao,
    cargo,
    hierarquia,
    ano,
    `Quantidade de dias com reuniões` = total,
    reunioes_empresa = prop,
    `% de reuniões com empresas` = perc
  ) |>
  dplyr::arrange(pessoa, ano)


taxa_reuniao_empresas_pessoa <- agendas_empresas |>
  dplyr::left_join(orgaos) |>
  dplyr::count(pessoa, orgao, cargo, hierarquia, empresa, ano) |>
  dplyr::group_by(pessoa, ano) |>
  dplyr::mutate(
    total = sum(n),
    prop = n/sum(n),
    perc = formattable::percent(prop)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(empresa) |>
  dplyr::select(
    pessoa,
    hierarquia,
    ano,
    total,
    prop,
    perc
  ) |>
  dplyr::arrange(pessoa, ano)

taxa_reuniao_empresas_pessoa |>
  dplyr::mutate(
    pessoa = forcats::fct_inorder(glue::glue("{pessoa} ({hierarquia})"))
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = ano, y = prop, label=perc) +
  ggplot2::geom_col(fill="#047b33") +
  ggplot2::facet_wrap(.~pessoa) +
  ggplot2::geom_label(fill="#1f4874",color="#f6c40a") +
  ggplot2::theme_minimal(10) +
  ggplot2::theme(
    strip.background=ggplot2::element_rect(fill="#1f4874"),
    strip.text=ggplot2::element_text(color="#f6c40a")
  )

taxa_reuniao_empresas_h <- agendas_empresas |>
  dplyr::left_join(orgaos) |>
  dplyr::count(hierarquia, empresa, ano) |>
  dplyr::group_by(hierarquia, ano) |>
  dplyr::mutate(
    total = sum(n),
    prop = n/sum(n),
    perc = formattable::percent(prop)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(empresa) |>
  dplyr::select(
    hierarquia,
    ano,
    `Quantidade de dias com reuniões` = total,
    reunioes_empresa = prop,
    `% de reuniões com empresas` = perc
  ) |>
  dplyr::arrange(hierarquia, ano)
