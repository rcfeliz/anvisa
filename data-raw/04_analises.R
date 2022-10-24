# analises
agendas <- anvisa::agendas_tidy
agendas <- anvisa::agendas_2021
agendas <- anvisa::agendas_ggali

governo <- stringr::regex("anvisa|minist|secret|procurad|gerente|presidente da república|assesso|deputad|senado|embaixada|aplan|dire[ ]?[12345]", TRUE)
interno <- stringr::regex("audi[eê]ncia p[úu]blica", TRUE)

empresa <- stringr::regex("empresa|ind[úu]stria|ltda|eireli|associação brasileira das indústrias da alimentação|abia$|associação brasileira das empresas de produtos nutricionais|crist[aá]lia|abbvie|bausch health|uni[aã]o qu[ií]mica|farmoqu[ií]mica|abiove|pfizer|bausch health|associação dos laboratórios farmacêuticos nacionais|alanac|vera rosas|bahiafarma|mappel indústria de embalagens s.a|hyper pharma|ems|gsk|cimed|prati donaduzzi|associação brasileira de supermercados|abras$|brasnutri|associação bras. de alimentos para fins especiais e congeneres|associação brasileira de alimentos para fins especiais e congeneres|abiad$|fbm indústria farmacêutica ltda|sindusfarma|pão de açúcar|associação brasileira das indústrias de pescados|abipesca$|associação brasileira de indústrias de óleos vegetais|abiove$|cellerafarma|chr hansen industria e comercio ltda|libbs|barral mj consultores associados ltda|sanavita indústria e comércio de alimentos funcionais eireli|cellera|cargill|abbot|amcor embalagens da amazônia s/a|integral médica|amyris biotecnologia do brasil ltda|prodiet nutri[cç][ãa]o cl[ií]nica ltda|danone|ferrero|associação brasileira de proteína animal|visanco|vetes|tate & lyle|andrade sun farms agrocomercial|4 life|nestl[eé]|brf|inove treinamentos e consultoria|abiam|abrabe|nutralider|unilever|foodstaff|united states pharmacopeia|alanac|danisco brasil ltda|prodiet|fbm indústria farmacêutica|dr ind[uú]stria|fresenius kabi brasil|sulpet plásticos ltda|atitude farmacêutica|support produtos nutricionais ltda|mondelez|recofarma|libbs farmaceutica|eurofarma|gds|gene|gabbia|recofarma|prodiet|ypy|merck|gs1|laboratórios b. braun|takeda pharma|biolab sanus|mead johnson|ambex|catalen|darnel|coca[ -]cola|food design|cmw|pharmedic pharmaceuticals|green pcr|farmoquimia|kasznar|pf. consumer|vida fortte|althaia|bmj", TRUE)

a <- agendas |>
  dplyr::mutate(
    compromisso = stringr::str_to_lower(compromisso),
    empresa = stringr::str_detect(compromisso, empresa)
  )

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

sociedade_civil <- c(
  "mães metabólicas"
)
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

orgaos <- anvisa::diretores |>
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
  ggplot2::theme_minimal(18) +
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
