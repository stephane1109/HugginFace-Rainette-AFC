# ui.R

library(shiny)
library(htmltools)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #shiny-modal .modal-dialog {
        width: 96vw !important;
        max-width: 96vw !important;
      }
      #shiny-modal .modal-body {
        max-height: 88vh !important;
        overflow-y: auto !important;
      }
      .sidebar-section-title {
        font-weight: 700;
        font-size: 18px !important;
        color: #1e5aa8 !important;
        margin-top: 12px;
        margin-bottom: 6px;
      }
    "))
  ),

  tags$h2(
    style = "color: #1e5aa8;",
    "CHD - AFC avec Rainette sur corpus IRaMuTeQ"
  ),
  tags$p(
    style = "font-size: 14px;",
    "Le script est basé sur le package Rainette de Julien Barnier, l'exercice ici est de rendre fonctionnel sur un serveur distant l'analyse CHD et AFC.",
    tags$br(),
    "En test j'ai également expérimenté la recherche de NER dans le corpus s'appuyant sur la librairie Spacy (modele \"md\").",
    tags$br(),
    "Pour d'autres infos vous pouvez consulter mon site www.codeandcortex.fr",
    tags$br(),
    "version beta 0.2 - 12-02-2026"
  ),

  sidebarLayout(
    sidebarPanel(
      fileInput("fichier_corpus", "Uploader un corpus IRaMuTeQ (.txt)", accept = c(".txt")),

      tags$div(class = "sidebar-section-title", "Paramètres CHD"),

      numericInput("segment_size", "segment_size", value = 40, min = 5, step = 1),
      numericInput("k", "k (nombre de classes)", value = 3, min = 2, step = 1),
      numericInput("min_segment_size", "Nombre minimal de termes par segment (min_segment_size)", value = 10, min = 1, step = 1),
      numericInput("min_split_members", "Effectif minimal pour scinder une classe (min_split_members)", value = 10, min = 1, step = 1),
      numericInput("min_docfreq", "Fréquence minimale des termes (min_docfreq)", value = 3, min = 1, step = 1),
      numericInput("max_p", "max_p (p-value)", value = 0.05, min = 0, max = 1, step = 0.01),

      radioButtons(
        "type_classification",
        "Type de classification",
        choices = c(
          "Classification simple (rainette)" = "simple",
          "Classification double (rainette2)" = "double"
        ),
        selected = "simple",
        inline = FALSE
      ),

      conditionalPanel(
        condition = "input.type_classification == 'double'",
        numericInput("min_segment_size2", "min_segment_size (classification 2)", value = 15, min = 1, step = 1),
        numericInput("max_k_double", "max_k (rainette2)", value = 8, min = 2, step = 1)
      ),

      tags$div(class = "sidebar-section-title", "Nettoyage"),

      checkboxInput("nettoyage_caracteres", "Nettoyage caractères (regex)", value = FALSE),
      checkboxInput("supprimer_chiffres", "Supprimer les chiffres (0-9)", value = FALSE),
      checkboxInput("forcer_minuscules_avant", "Forcer les minuscules avant traitement", value = FALSE),
      checkboxInput("retirer_stopwords", "Retirer les stopwords (liste FR) – pipeline standard", value = FALSE),
      checkboxInput("spacy_utiliser_lemmes", "Lemmatisation (spaCy)", value = FALSE),
      checkboxInput("spacy_retirer_stopwords", "Retirer les stopwords (spaCy)", value = FALSE),

      tags$small("Regex appliquée quand “Nettoyage caractères (regex)” est activé :"),
      tags$pre(
        style = "white-space: pre-wrap; font-size: 11px; border: 1px solid #ddd; padding: 6px;",
        REGEX_CARACTERES_A_SUPPRIMER
      ),
      tags$small("Note : la tokenisation quanteda retire ensuite la ponctuation si remove_punct=TRUE, même si elle est autorisée par la regex ci-dessus."),

      tags$div(class = "sidebar-section-title", "Paramétrages SpaCy"),

      checkboxInput("filtrage_morpho", "Filtrage morphosyntaxique (spaCy)", value = FALSE),
      checkboxInput("activer_ner", "Activer NER (spaCy)", value = FALSE),

      conditionalPanel(
        condition = "input.filtrage_morpho == true",
        selectizeInput(
          "pos_spacy_a_conserver",
          "POS à conserver (spaCy)",
          choices = c(
            "ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN",
            "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X"
          ),
          selected = c("NOUN", "VERB"),
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        )
      ),

      tags$hr(),

      tags$div(class = "sidebar-section-title", "Paramètres AFC"),

      checkboxInput("afc_reduire_chevauchement", "Réduire les chevauchements des mots (AFC)", value = FALSE),

      radioButtons(
        "afc_taille_mots",
        "Taille des mots (AFC termes)",
        choices = c("Fréquence" = "frequency", "Chi2" = "chi2"),
        selected = "frequency",
        inline = FALSE
      ),

      tags$hr(),

      tags$div(class = "sidebar-section-title", "Cooccurrences (beta)"),

      numericInput("top_n", "top_n (wordcloud)", value = 20, min = 5, step = 1),
      numericInput("window_cooc", "window (cooccurrences)", value = 5, min = 1, step = 1),
      numericInput("top_feat", "top_feat (cooccurrences)", value = 20, min = 5, step = 1),

      tags$hr(),

      tags$div(
        style = "display: flex; gap: 8px; flex-wrap: wrap; align-items: center;",
        actionButton("lancer", "Lancer l'analyse"),
        actionButton("explor", "Explor rainette", class = "btn-primary")
      ),

      tags$hr(),

      downloadButton("dl_zip", "Télécharger exports (zip)"),
      downloadButton("dl_afc_zip", "Télécharger AFC (zip)")
    ),

    mainPanel(
      tabsetPanel(
        id = "onglets_principaux",

        tabPanel(
          "Analyse",
          tags$h3("Statut"),
          textOutput("statut"),
          tags$h3("Progression"),
          uiOutput("barre_progression"),
          tags$h3("Journal"),
          tags$pre(style = "white-space: pre-wrap;", textOutput("logs")),
          tags$h3("Répartition des classes"),
          tableOutput("table_classes")
        ),

        
        tabPanel(
          "AFC",
          tags$h3("AFC"),
          uiOutput("ui_afc_statut"),
          uiOutput("ui_afc_erreurs"),

          tags$h4("AFC des classes (Représentation des classes)"),
          plotOutput("plot_afc_classes", height = "620px"),

          tags$h4("AFC des termes"),
          tags$p("Les mots sont colorés selon la classe où ils sont le plus surreprésentés (résidus standardisés) et leur taille est proportionnelle à leur fréquence globale ou chi2 (selon le choix)."),
          plotOutput("plot_afc", height = "720px"),
          tags$h4("Table des mots projetés (fréquence, chi2, p-value, segment exemple)"),
          tableOutput("table_afc_mots"),

          tags$h4("AFC des variables étoilées"),
          plotOutput("plot_afc_vars", height = "720px"),
          tags$h4("Table des modalités projetées"),
          tableOutput("table_afc_vars"),

          tags$h4("Valeurs propres"),
          tableOutput("table_afc_eig")
        ),
        
        tabPanel(
          "NER (beta)",
          tags$h3("Détection d'entités nommées (spaCy)"),
          uiOutput("ui_ner_statut"),
          tags$h3("Résumé"),
          tableOutput("table_ner_resume"),
          tags$h3("Détails"),
          tableOutput("table_ner_details"),
          tags$h3("Nuage de mots (entités)"),
          plotOutput("plot_ner_wordcloud", height = "520px"),
          tags$h3("Nuages par classe"),
          uiOutput("ui_ner_wordcloud_par_classe")
        ),

        tabPanel(
          "Aide",
          ui_aide_huggingface()
        ),

        tabPanel(
          "Readme (Rainette.md)",
          tags$div(
            style = "padding: 12px;",
            tags$a(
              href = "https://github.com/juba/rainette/blob/main/README.md",
              target = "_blank",
              "Ouvrir le README Rainette (GitHub) dans un nouvel onglet"
            )
          )
        )
      )
    )
  )
)
