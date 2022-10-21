#' Agendas de todos os cargos de direção da Anvisa
#'
#' Uma base de dados com todas as autoridades da Anvisa, seus respectivos cargos
#' e os links para as suas agendas
#'
#' @format Um data frame com 179 linhas e 9 variáveis:
#' \describe{
#'  \item{nome}{Nome da autoridade}
#'  \item{cargo}{Cargo da autoridade}
#'  \item{orgao}{Órgão ao qual está vinculado}
#'  \item{hierarquia}{Posição hierarquica dentro da Anvisa, em que "h1" é a posição mais alta e "h4", a mais baixa}
#'  \item{h1}{Órgão de nível 1 ao qual está vinculado. Se a pessoa é do nível mais alto, o h1 corresponde ao órgão}
#'  \item{h2}{Órgão de nível 2 ao qual está vinculado. Se a pessoa é do segundo nível mais alto, o h2 corresponde ao órgão. Pessoas de hierarquia superior são representadas com "-" para esta coluna}
#'  \item{h3}{Órgão de nível 3 ao qual está vinculado. Se a pessoa é do terceiro nível mais alto, o h3 corresponde ao órgão. Pessoas de hierarquia superior são representadas com "-" para esta coluna}
#'  \item{h4}{Órgão de nível 4 ao qual está vinculado. Se a pessoa é do nível mais baixo, o h4 corresponde ao órgão. Pessoas de hierarquia superior são representadas com "-" para esta coluna}
#'  \item{u_agenda}{URL da agenda da autoridade}
#'  }
"diretores"
