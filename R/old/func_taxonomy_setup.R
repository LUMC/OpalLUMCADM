#' @title Create YAML file from excel
#'
#' @description The YAML file can be uploaded into Opal as a taxonomy. The ADM template is required for correct functioning
#'
#' @param taxonomy matrix. The taxonomy, obtained from the ADM template
#' @param author string. The author of the taxonomy
#' @param license string. The license for the taxonomy
#'
#' @return The taxonomy in YAML format
#'
#' @note use writeLines({returned_object}, paste0({path}, ".yml")) to write the YAML file correctly away after running this function
#'
#' @import tidyverse yaml
#' @importFrom openxlsx read.xlsx
#'
#' @author Lars van der Burg
#'
#' @export
taxonomy_setup = function(taxonomy, author = "ADM", license = "CC BY-NC-ND 4.0"){

  all_taxonomy = list(
    name = unique(taxonomy$taxonomy),
    title = list(en = unique(taxonomy$taxonomy.title)),
    description = list(en = unique(taxonomy$taxonomy.description)),
    attributes = list(),
    keywords = list(),
    author = author,
    license = license
  )

  all_vocabularies = NULL
  vocabularies = taxonomy |> pull(vocabulary) |> unique()
  for(voc in vocabularies){
    taxonomy_voc = taxonomy |>
      filter(vocabulary == voc) |>
      select(-all_of(c("taxonomy", "taxonomy.title", "taxonomy.description")))

    vocabulary = list(
      list(name = unique(taxonomy_voc$vocabulary),
           title = list(en = unique(taxonomy_voc$vocabulary.title)),
           description = list(en = unique(taxonomy_voc$vocabulary.description)),
           attributes = list(),
           keywords = list()
           )
    )

    all_terms = NULL
    terms = taxonomy_voc |> pull(term) |> unique()
    for(trm in terms){
      taxonomy_trm = taxonomy_voc |>
        filter(term == trm) |>
        select(-all_of(c("vocabulary", "vocabulary.title", "vocabulary.description")))

      term = list(
        list(name = unique(taxonomy_trm$term),
             title = list(en = unique(taxonomy_trm$term.title)),
             description = list(en = unique(taxonomy_trm$term.description)),
             attributes = list(),
             keywords = list()
             )
        )

      all_terms = append(all_terms, term)
    }
    vocabulary[[1]]$terms = all_terms

    all_vocabularies = append(all_vocabularies, vocabulary)
  }
  all_vocabularies = list(all_vocabularies); names(all_vocabularies) = "vocabularies"

  YAML = str_replace_all(as.yaml(append(all_taxonomy, all_vocabularies)), "\\[\\]", "\\{\\}")
  cat('To save this yaml correctly, use writeLines({returned_object}, paste0({path}, ".yml"))')

  return(YAML)
}
