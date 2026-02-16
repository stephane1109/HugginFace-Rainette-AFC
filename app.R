###############################################################################
#                    Script CHD - version beta 0.3 - 12-02-2026               #
#      A partir d'un corpus texte formaté aux exigences IRAMUTEQ              #
#                            Stéphane Meurisse                                #
#                           wwww.codeandcortex.fr                             #          
#                                                                             #
#      1.Réalise la CHD sur le corpus, sans rainette_explor                   #
#      2.Extrait chi2, lr, freq, docprop dans un CSV                          #
#      3.AFC                                                                  #
#      4.Recherche de NER avec Spacy (md)                                     #
#      5.Génère nuages de mots et graphes de cooccurrences par classe         #
#      6.Exporte les segments de texte par classe au format text              #
#      7.Creation d'un concordancier au format html                           #
#      8.Recherche de coocurrences                                            #
###############################################################################

library(shiny)
library(rainette)
library(quanteda)
library(wordcloud)
library(RColorBrewer)
library(igraph)
library(dplyr)
library(htmltools)

options(shiny.maxRequestSize = 300 * 1024^2)
options(shinygadgets.viewer = shiny::browserViewer())

if (file.exists("help.md")) {
  ui_aide_huggingface <- function() {
    tagList(
      tags$h2("Aide"),
      includeMarkdown("help.md")
    )
  }
} else {
  ui_aide_huggingface <- function() {
    tagList(
      tags$h2("Aide"),
      tags$p("Le fichier help.md est introuvable. Ajoute help.md à la racine du projet.")
    )
  }
}

source("nettoyage.R", encoding = "UTF-8", local = TRUE)
source("concordancier.R", encoding = "UTF-8", local = TRUE)
source("afc.R", encoding = "UTF-8", local = TRUE)
source("ui.R", encoding = "UTF-8", local = TRUE)

source("R/utils_general.R", encoding = "UTF-8", local = TRUE)
source("R/utils_logging.R", encoding = "UTF-8", local = TRUE)
source("R/utils_text.R", encoding = "UTF-8", local = TRUE)

source("R/afc_helpers.R", encoding = "UTF-8", local = TRUE)

source("R/chd_afc_pipeline.R", encoding = "UTF-8", local = TRUE)
source("R/nlp_language.R", encoding = "UTF-8", local = TRUE)
source("R/nlp_spacy.R", encoding = "UTF-8", local = TRUE)
source("R/server_outputs_status.R", encoding = "UTF-8", local = TRUE)
source("R/server_events_lancer.R", encoding = "UTF-8", local = TRUE)

server <- function(input, output, session) {

  rv <- reactiveValues(
    logs = "",
    statut = "En attente.",
    progression = 0,

    base_dir = NULL,
    export_dir = NULL,
    segments_file = NULL,
    stats_file = NULL,
    html_file = NULL,
    ner_file = NULL,
    zip_file = NULL,

    res = NULL,
    res_chd = NULL,
    dfm_chd = NULL,
    dfm = NULL,
    filtered_corpus = NULL,
    res_stats_df = NULL,
    clusters = NULL,
    max_n_groups = NULL,
    max_n_groups_chd = NULL,

    res_type = "simple",

    exports_prefix = paste0("exports_", session$token),

    spacy_tokens_df = NULL,
    textes_indexation = NULL,

    ner_df = NULL,
    ner_nb_segments = NA_integer_,

    afc_obj = NULL,
    afc_erreur = NULL,

    afc_vars_obj = NULL,
    afc_vars_erreur = NULL,

    afc_dir = NULL,
    afc_table_mots = NULL,
    afc_table_vars = NULL,
    afc_plot_classes = NULL,
    afc_plot_termes = NULL,
    afc_plot_vars = NULL,

    explor_assets = NULL
  )

  register_outputs_status(input, output, session, rv)

  output$ui_afc_statut <- renderUI({
    if (!is.null(rv$afc_erreur) && nzchar(rv$afc_erreur)) {
      return(tags$p("AFC : erreur (voir ci-dessous)."))
    }
    if (is.null(rv$afc_obj) || is.null(rv$afc_obj$ca)) {
      return(tags$p("AFC non calculée. Lance une analyse pour calculer l'AFC classes × termes."))
    }
    ncl <- nrow(rv$afc_obj$table)
    nt <- ncol(rv$afc_obj$table)
    tags$p(paste0("AFC calculée sur ", ncl, " classes et ", nt, " termes (table Classes × Termes)."))
  })

  output$ui_afc_erreurs <- renderUI({
    messages <- Filter(
      nzchar,
      list(
        rv$afc_erreur,
        rv$afc_vars_erreur
      )
    )

    if (length(messages) == 0) {
      return(NULL)
    }

    tags$div(
      style = "display: flex; flex-direction: column; gap: 8px; margin-bottom: 12px;",
      lapply(messages, function(msg) {
        tags$div(
          style = "border: 1px solid #f5c2c7; background: #f8d7da; color: #842029; border-radius: 4px; padding: 10px; white-space: pre-wrap;",
          msg
        )
      })
    )
  })

  output$ui_spacy_langue_detection <- renderUI({
    if (is.null(rv$filtered_corpus)) {
      return(tags$p("Détection langue : charge et lance une analyse pour afficher une estimation."))
    }

    est <- estimer_langue_corpus(as.character(rv$filtered_corpus))
    if (is.na(est$code)) {
      return(tags$p("Détection langue : estimation indisponible."))
    }

    cfg_est <- configurer_langue_spacy(est$code)
    cfg_sel <- configurer_langue_spacy(input$spacy_langue)

    msg <- paste0(
      "Langue estimée du corpus : ", cfg_est$libelle,
      " (scores stopwords FR=", sprintf("%.3f", est$scores[["fr"]]),
      ", EN=", sprintf("%.3f", est$scores[["en"]]),
      ", ES=", sprintf("%.3f", est$scores[["es"]]), ")."
    )

    if (!identical(cfg_est$code, cfg_sel$code)) {
      return(tags$div(
        style = "border:1px solid #f5c2c7;background:#f8d7da;color:#842029;padding:10px;border-radius:4px;",
        tags$p(style = "margin:0;", paste0(msg, " Dictionnaire sélectionné : ", cfg_sel$libelle, "."))
      ))
    }

    tags$div(
      style = "border:1px solid #badbcc;background:#d1e7dd;color:#0f5132;padding:10px;border-radius:4px;",
      tags$p(style = "margin:0;", paste0(msg, " Dictionnaire sélectionné : ", cfg_sel$libelle, "."))
    )
  })

  output$ui_ner_statut <- renderUI({
    if (!isTRUE(input$activer_ner)) {
      return(tags$p("NER désactivé. Coche 'Activer NER (spaCy)' puis relance l'analyse."))
    }

    if (is.null(rv$ner_df)) {
      return(tags$p("NER activé, mais aucun résultat disponible. Relance une analyse complète."))
    }

    nb_ent <- nrow(rv$ner_df)
    nb_seg <- ifelse(is.na(rv$ner_nb_segments), 0, rv$ner_nb_segments)
    tags$p(paste0("NER calculé sur ", nb_seg, " segments. Entités détectées : ", nb_ent, "."))
  })

  output$table_ner_resume <- renderTable({
    req(rv$ner_df)
    if (nrow(rv$ner_df) == 0) return(data.frame(Message = "Aucune entité détectée.", stringsAsFactors = FALSE))

    as.data.frame(sort(table(rv$ner_df$ent_label), decreasing = TRUE), stringsAsFactors = FALSE) |>
      dplyr::rename(Type = Var1, Effectif = Freq)
  }, rownames = FALSE)

  output$table_ner_details <- renderTable({
    req(rv$ner_df)
    if (nrow(rv$ner_df) == 0) return(data.frame(Message = "Aucune entité détectée.", stringsAsFactors = FALSE))

    df <- rv$ner_df[, intersect(c("Classe", "doc_id", "ent_text", "ent_label", "start_char", "end_char"), names(rv$ner_df)), drop = FALSE]
    head(df, 200)
  }, rownames = FALSE)

  output$plot_ner_wordcloud <- renderPlot({
    req(rv$ner_df)
    if (nrow(rv$ner_df) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune entité détectée.", cex = 1.1)
      return(invisible(NULL))
    }

    freq <- sort(table(rv$ner_df$ent_text), decreasing = TRUE)
    suppressWarnings(wordcloud(
      words = names(freq),
      freq = as.numeric(freq),
      min.freq = 1,
      max.words = min(150, length(freq)),
      random.order = FALSE,
      colors = brewer.pal(8, "Dark2")
    ))
  })

  output$ui_ner_wordcloud_par_classe <- renderUI({
    req(rv$ner_df)
    if (nrow(rv$ner_df) == 0 || !"Classe" %in% names(rv$ner_df)) return(tags$p("Aucune entité à afficher par classe."))

    classes <- sort(unique(rv$ner_df$Classe))
    if (length(classes) == 0) return(tags$p("Aucune classe disponible pour l'affichage."))

    tagList(lapply(classes, function(cl) {
      nm <- paste0("plot_ner_wordcloud_cl_", cl)
      local({
        cl_local <- cl
        output[[nm]] <- renderPlot({
          df_cl <- rv$ner_df[rv$ner_df$Classe == cl_local, , drop = FALSE]
          if (nrow(df_cl) == 0) {
            plot.new()
            text(0.5, 0.5, paste0("Classe ", cl_local, " : aucune entité."), cex = 1.1)
            return(invisible(NULL))
          }

          freq <- sort(table(df_cl$ent_text), decreasing = TRUE)
          suppressWarnings(wordcloud(
            words = names(freq),
            freq = as.numeric(freq),
            min.freq = 1,
            max.words = min(120, length(freq)),
            random.order = FALSE,
            colors = brewer.pal(8, "Set2")
          ))
        })
      })

      tagList(
        tags$h4(paste0("Classe ", cl)),
        plotOutput(nm, height = "360px")
      )
    }))
  })

  output$ui_chd_statut <- renderUI({
    if (is.null(rv$res)) {
      return(tags$p("CHD non disponible. Lance une analyse."))
    }

    if (identical(rv$res_type, "double")) {
      return(tags$p("CHD disponible (classification double rainette2)."))
    }

    nb_classes <- NA_integer_
    if (!is.null(rv$clusters)) nb_classes <- length(rv$clusters)
    tags$p(paste0("CHD disponible (classification simple rainette) - classes détectées : ", nb_classes, "."))
  })

  register_events_lancer(input, output, session, rv)




  observeEvent(input$explor, {
    req(rv$export_dir, rv$html_file, rv$clusters, rv$res_stats_df)

    if (is.null(rv$exports_prefix) || !nzchar(rv$exports_prefix)) {
      showNotification("Préfixe d'export invalide.", type = "error", duration = 8)
      return(invisible(NULL))
    }
    if (!(rv$exports_prefix %in% names(shiny::resourcePaths()))) {
      shiny::addResourcePath(rv$exports_prefix, rv$export_dir)
    }

    classe_defaut <- as.character(rv$clusters[1])

    showModal(modalDialog(
      title = "Exploration (serveur)",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Fermer"),

      selectInput("classe_viz", "Classe", choices = as.character(rv$clusters), selected = classe_defaut),

      tabsetPanel(
        tabPanel(
          "CHD (rainette_plot)",
          fluidRow(
            column(
              4,
              sliderInput("k_plot", "Nombre de classes (k)", min = 2, max = rv$max_n_groups_chd, value = min(rv$max_n_groups_chd, 8), step = 1),
              selectInput(
                "measure_plot", "Statistiques",
                choices = c(
                  "Keyness - Chi-squared" = "chi2",
                  "Keyness - Likelihood ratio" = "lr",
                  "Frequency - Terms" = "frequency",
                  "Frequency - Documents proportion" = "docprop"
                ),
                selected = "chi2"
              ),
              selectInput("type_plot", "Type", choices = c("bar", "cloud"), selected = "bar"),
              numericInput("n_terms_plot", "Nombre de termes", value = 20, min = 5, max = 200, step = 1),
              conditionalPanel(
                "input.measure_plot != 'docprop'",
                checkboxInput("same_scales_plot", "Forcer les mêmes échelles", value = TRUE)
              ),
              checkboxInput("show_negative_plot", "Afficher les valeurs négatives", value = FALSE),
              numericInput("text_size_plot", "Taille du texte", value = 12, min = 6, max = 30, step = 1)
            ),
            column(
              8,
              plotOutput("plot_chd", height = "70vh")
            )
          )
        ),
        tabPanel(
          "Concordancier HTML",
          tags$iframe(
            src = paste0("/", rv$exports_prefix, "/segments_par_classe.html"),
            style = "width: 100%; height: 70vh; border: 1px solid #999;"
          )
        ),
        tabPanel(
          "Wordcloud",
          uiOutput("ui_wordcloud")
        ),
        tabPanel(
          "Cooccurrences",
          uiOutput("ui_cooc")
        ),
        tabPanel(
          "Statistiques",
          tableOutput("table_stats_classe")
        )
      )
    ))
  })

  output$plot_afc_classes <- renderPlot({
    if (!is.null(rv$afc_erreur) && nzchar(rv$afc_erreur)) {
      plot.new()
      text(0.5, 0.5, "AFC indisponible (erreur).", cex = 1.1)
      return(invisible(NULL))
    }
    if (is.null(rv$afc_obj) || is.null(rv$afc_obj$ca)) {
      plot.new()
      text(0.5, 0.5, "AFC non disponible. Lance une analyse.", cex = 1.1)
      return(invisible(NULL))
    }
    tracer_afc_classes_seules(rv$afc_obj, axes = c(1, 2), cex_labels = 1.05)
  })

  output$plot_chd <- renderPlot({
    req(rv$res_chd, rv$dfm_chd)
    req(!is.null(input$k_plot))
    req(!is.null(input$measure_plot))
    req(!is.null(input$type_plot))
    req(!is.null(input$n_terms_plot))

    same_scales <- isTRUE(input$same_scales_plot)
    show_negative <- isTRUE(input$show_negative_plot)

    rainette_plot(
      rv$res_chd,
      rv$dfm_chd,
      k = input$k_plot,
      type = input$type_plot,
      n_terms = input$n_terms_plot,
      free_scales = !same_scales,
      measure = input$measure_plot,
      show_negative = show_negative,
      text_size = input$text_size_plot
    )
  })

  output$ui_wordcloud <- renderUI({
    req(input$classe_viz, rv$exports_prefix, rv$export_dir)

    src_rel <- file.path("wordclouds", paste0("cluster_", input$classe_viz, "_wordcloud.png"))
    if (!file.exists(file.path(rv$export_dir, src_rel))) {
      return(tags$p("Aucun nuage de mots disponible pour cette classe."))
    }

    tags$div(
      style = "text-align: center;",
      tags$img(
        src = paste0("/", rv$exports_prefix, "/", src_rel),
        style = "max-width: 100%; height: auto; border: 1px solid #999; display: inline-block;"
      )
    )
  })

  output$ui_cooc <- renderUI({
    req(input$classe_viz, rv$exports_prefix, rv$export_dir)

    src_rel <- file.path("cooccurrences", paste0("cluster_", input$classe_viz, "_fcm_network.png"))
    if (!file.exists(file.path(rv$export_dir, src_rel))) {
      return(tags$p("Aucune cooccurrence disponible pour cette classe."))
    }

    tags$img(src = paste0("/", rv$exports_prefix, "/", src_rel), style = "max-width: 100%; height: auto; border: 1px solid #999;")
  })

  output$table_stats_classe <- renderTable({
    req(input$classe_viz, rv$res_stats_df)
    cl <- as.numeric(input$classe_viz)

    df <- rv$res_stats_df
    df <- df[df$Classe == cl, , drop = FALSE]

    colonnes_possibles <- intersect(
      c("Terme", "chi2", "lr", "frequency", "docprop", "p", "p_value_filter"),
      names(df)
    )
    df <- df[, colonnes_possibles, drop = FALSE]

    if ("chi2" %in% names(df)) df <- df[order(-df$chi2), , drop = FALSE]
    head(df, 50)
  }, rownames = FALSE)

  output$plot_afc <- renderPlot({
    if (!is.null(rv$afc_erreur) && nzchar(rv$afc_erreur)) {
      plot.new()
      text(0.5, 0.5, "AFC indisponible (erreur).", cex = 1.1)
      return(invisible(NULL))
    }
    if (is.null(rv$afc_obj) || is.null(rv$afc_obj$ca)) {
      plot.new()
      text(0.5, 0.5, "AFC non disponible. Lance une analyse.", cex = 1.1)
      return(invisible(NULL))
    }

    activer_repel <- TRUE
    if (!is.null(input$afc_reduire_chevauchement)) activer_repel <- isTRUE(input$afc_reduire_chevauchement)

    taille_sel <- "frequency"
    if (!is.null(input$afc_taille_mots) && nzchar(as.character(input$afc_taille_mots))) {
      taille_sel <- as.character(input$afc_taille_mots)
    }
    if (!taille_sel %in% c("frequency", "chi2")) taille_sel <- "frequency"

    top_termes <- 120
    if (!is.null(input$afc_top_termes) && is.finite(input$afc_top_termes)) top_termes <- as.integer(input$afc_top_termes)

    tracer_afc_classes_termes(rv$afc_obj, axes = c(1, 2), top_termes = top_termes, taille_sel = taille_sel, activer_repel = activer_repel)
  })

  output$ui_table_afc_mots_par_classe <- renderUI({
    if (is.null(rv$afc_table_mots)) {
      output$table_afc_mots_message <- renderTable({
        data.frame(Message = "AFC mots : non disponible.", stringsAsFactors = FALSE)
      }, rownames = FALSE)
      return(tableOutput("table_afc_mots_message"))
    }

    df <- rv$afc_table_mots
    colonnes <- intersect(c("Terme", "Classe_max", "frequency", "chi2", "p_value", "Segment_texte"), names(df))
    df <- df[, colonnes, drop = FALSE]
    if ("p_value" %in% names(df)) {
      df$p_value <- ifelse(
        is.na(df$p_value),
        NA_character_,
        formatC(df$p_value, format = "f", digits = 6)
      )
    }

    classes <- unique(as.character(df$Classe_max))
    classes <- classes[!is.na(classes) & nzchar(classes)]
    classes <- sort(classes)

    if (length(classes) == 0) {
      output$table_afc_mots_message <- renderTable({
        data.frame(Message = "AFC mots : aucune classe disponible.", stringsAsFactors = FALSE)
      }, rownames = FALSE)
      return(tableOutput("table_afc_mots_message"))
    }

    ui_tables <- lapply(seq_along(classes), function(i) {
      cl <- classes[[i]]
      id <- paste0("table_afc_mots_", i)

      output[[id]] <- renderUI({
        sous_df <- df[df$Classe_max == cl, , drop = FALSE]
        colonnes <- intersect(c("Terme", "frequency", "chi2", "p_value", "Segment_texte"), names(sous_df))
        sous_df <- sous_df[, colonnes, drop = FALSE]

        if ("p_value" %in% names(sous_df)) {
          sous_df$p_value <- ifelse(
            is.na(sous_df$p_value),
            NA_character_,
            formatC(sous_df$p_value, format = "f", digits = 6)
          )
        }

        if ("chi2" %in% names(sous_df)) {
          sous_df <- sous_df[order(-sous_df$chi2), , drop = FALSE]
          sous_df$chi2 <- ifelse(
            is.na(sous_df$chi2),
            NA_character_,
            formatC(sous_df$chi2, format = "f", digits = 6)
          )
        }

        sous_df <- head(sous_df, 100)
        generer_table_html_afc_mots(sous_df)
      })

      tagList(
        tags$h5(cl),
        uiOutput(id)
      )
    })

    do.call(tagList, ui_tables)
  })

  output$plot_afc_vars <- renderPlot({
    if (!is.null(rv$afc_vars_erreur) && nzchar(rv$afc_vars_erreur)) {
      plot.new()
      text(0.5, 0.5, "AFC variables étoilées indisponible (erreur).", cex = 1.1)
      return(invisible(NULL))
    }
    if (is.null(rv$afc_vars_obj) || is.null(rv$afc_vars_obj$ca)) {
      plot.new()
      text(0.5, 0.5, "AFC variables étoilées non disponible. Lance une analyse.", cex = 1.1)
      return(invisible(NULL))
    }

    activer_repel <- TRUE
    if (!is.null(input$afc_reduire_chevauchement)) activer_repel <- isTRUE(input$afc_reduire_chevauchement)

    top_mod <- 120
    if (!is.null(input$afc_top_modalites) && is.finite(input$afc_top_modalites)) top_mod <- as.integer(input$afc_top_modalites)

    tracer_afc_variables_etoilees(rv$afc_vars_obj, axes = c(1, 2), top_modalites = top_mod, activer_repel = activer_repel)
  })

  output$table_afc_vars <- renderTable({
    if (is.null(rv$afc_table_vars)) {
      return(data.frame(Message = "AFC variables étoilées : non disponible.", stringsAsFactors = FALSE))
    }
    df <- rv$afc_table_vars
    colonnes <- intersect(c("Modalite", "Classe_max", "frequency", "chi2", "p_value"), names(df))
    df <- df[, colonnes, drop = FALSE]
    if ("p_value" %in% names(df)) {
      p_values <- df$p_value
      df$p_value <- ifelse(
        is.na(p_values),
        NA_character_,
        ifelse(
          p_values > 0.05,
          sprintf("<span style='color:#d97706;font-weight:600;'>%s</span>", formatC(p_values, format = "f", digits = 6)),
          formatC(p_values, format = "f", digits = 6)
        )
      )
    }
    if ("chi2" %in% names(df)) df <- df[order(-df$chi2), , drop = FALSE]
    if ("chi2" %in% names(df)) {
      df$chi2 <- ifelse(
        is.na(df$chi2),
        NA_character_,
        formatC(df$chi2, format = "f", digits = 6)
      )
    }
    head(df, 200)
  }, rownames = FALSE, sanitize.text.function = function(x) x)

  output$table_afc_eig <- renderTable({
    if (!is.null(rv$afc_erreur) && nzchar(rv$afc_erreur)) {
      return(data.frame(Message = "AFC indisponible (erreur).", stringsAsFactors = FALSE))
    }
    if (is.null(rv$afc_obj) || is.null(rv$afc_obj$ca)) {
      return(data.frame(Message = "AFC non disponible.", stringsAsFactors = FALSE))
    }
    eig <- rv$afc_obj$ca$eig
    if (is.null(eig)) return(data.frame(Message = "Valeurs propres indisponibles.", stringsAsFactors = FALSE))
    df <- as.data.frame(eig)
    df$Dim <- rownames(df)
    rownames(df) <- NULL
    df <- df[, c("Dim", names(df)[1], names(df)[2], names(df)[3]), drop = FALSE]
    names(df) <- c("Dim", "Valeur_propre", "Pourcentage_inertie", "Pourcentage_cumule")
    df
  }, rownames = FALSE)

  output$dl_segments <- downloadHandler(
    filename = function() "segments_par_classe.txt",
    content = function(file) {
      req(rv$segments_file)
      file.copy(rv$segments_file, file, overwrite = TRUE)
    }
  )

  output$dl_stats <- downloadHandler(
    filename = function() "stats_par_classe.csv",
    content = function(file) {
      req(rv$stats_file)
      file.copy(rv$stats_file, file, overwrite = TRUE)
    }
  )

  output$dl_html <- downloadHandler(
    filename = function() "segments_par_classe.html",
    content = function(file) {
      req(rv$html_file)
      file.copy(rv$html_file, file, overwrite = TRUE)
    }
  )

  output$dl_zip <- downloadHandler(
    filename = function() "exports_rainette.zip",
    content = function(file) {
      req(rv$zip_file)
      file.copy(rv$zip_file, file, overwrite = TRUE)
    }
  )

  output$dl_afc_zip <- downloadHandler(
    filename = function() "afc_exports.zip",
    content = function(file) {
      req(rv$afc_dir)
      zip_tmp <- tempfile(fileext = ".zip")
      ancien <- getwd()
      on.exit(setwd(ancien), add = TRUE)
      setwd(dirname(rv$afc_dir))
      if (file.exists(zip_tmp)) unlink(zip_tmp)
      utils::zip(zipfile = zip_tmp, files = basename(rv$afc_dir))
      file.copy(zip_tmp, file, overwrite = TRUE)
    }
  )

}

shinyApp(ui = ui, server = server)
