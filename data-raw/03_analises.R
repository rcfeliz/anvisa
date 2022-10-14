# analises
agendas <- anvisa::agendas_tidy
agendas <- anvisa::agendas_2021

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
  dplyr::group_by(pessoa, cargo, dia) |>
  dplyr::summarise(
    empresa = any(reuniao_empresa),
    governo = any(reuniao_governo)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(pessoa, dia, cargo, empresa, governo)

taxa_reuniao_empresas <- agendas_empresas |>
  dplyr::count(pessoa, cargo, empresa) |>
  dplyr::group_by(pessoa) |>
  dplyr::mutate(
    total = sum(n),
    prop = n/sum(n),
    perc = formattable::percent(prop)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(empresa) |>
  dplyr::select(
    pessoa,
    cargo,
    `Quantidade de dias com reuniões` = total,
    reunioes_empresa = prop,
    `% de reuniões com empresas` = perc
  ) |>
  dplyr::arrange(desc(reunioes_empresa))

