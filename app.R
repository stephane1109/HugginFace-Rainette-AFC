###############################################################################
#                    Script CHD - version beta 0.2 - 12-02-2026               #
#      A partir d'un corpus texte formaté aux exigences IRAMUTEQ              #
#                      wwww.codeandcortex.fr                                  #          
#                                                                             #
#      1.Réalise la CHD sur le corpus, sans rainette_explor                   #
#      2.Extrait chi2, lr, freq, docprop dans un CSV                          #
#      3.AFC                                                                  #
#      4.Recherche de NER avec Spacy (md)                                     #
#      5.Génère nuages de mots et graphes de cooccurrences par classe         #
#      6.Exporte les segments de texte par classe au format text              #
#      7.Creation d'un concordancier au format html                           #
#      8.Recherche de coocurrence                                             #
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

if (file.exists("help.R")) {
  source("help.R", encoding = "UTF-8")
} else {
  ui_aide_huggingface <- function() {
    tagList(
      tags$h2("Aide"),
      tags$p("Le fichier help.R est introuvable. Ajoute help.R à la racine du projet.")
    )
  }
}

source("nettoyage.R", encoding = "UTF-8")
source("concordancier.R", encoding = "UTF-8")
source("afc.R", encoding = "UTF-8")
source("ui.R", encoding = "UTF-8")

compter_tokens <- function(tok) {
  lst <- as.list(tok)
  sum(vapply(lst, length, integer(1)))
}

md5_fichier <- function(chemin) {
  if (is.null(chemin) || !file.exists(chemin)) return(NA_character_)
  as.character(tools::md5sum(chemin))[1]
}

horodater <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

ajouter_log <- function(rv, texte) {
  rv$logs <- paste(rv$logs, paste0("[", horodater(), "] ", texte), sep = "\n")
}

normaliser_classes <- function(x) {
  y <- trimws(as.character(x))
  y[y == "" | is.na(y)] <- NA_character_
  y
}

extraire_classes_alignees <- function(corpus_obj, doc_ids, nom_colonne = "Classes") {
  dv <- docvars(corpus_obj)
  if (!(nom_colonne %in% names(dv))) return(rep(NA_character_, length(doc_ids)))

  doc_ids <- as.character(doc_ids)
  dn_corpus <- as.character(docnames(corpus_obj))
  idx <- match(doc_ids, dn_corpus)

  out <- rep(NA_character_, length(doc_ids))
  ok <- !is.na(idx)
  if (any(ok)) {
    out[ok] <- as.character(dv[[nom_colonne]][idx[ok]])
  }

  normaliser_classes(out)
}

construire_segment_source <- function(corpus_segmente) {
  dv <- docvars(corpus_segmente)

  if ("segment_source" %in% names(dv)) {
    v <- as.character(dv$segment_source)
    if (length(v) == ndoc(corpus_segmente)) return(v)
  }

  dn <- docnames(corpus_segmente)

  if (any(grepl("_seg[0-9]+$", dn))) return(gsub("_seg[0-9]+$", "", dn))
  if (any(grepl("_[0-9]+$", dn))) return(gsub("_[0-9]+$", "", dn))
  if (any(grepl("-[0-9]+$", dn))) return(gsub("-[0-9]+$", "", dn))

  dn
}

assurer_docvars_dfm_minimal <- function(dfm_obj, corpus_aligne) {
  seg_source <- construire_segment_source(corpus_aligne)
  dv <- data.frame(segment_source = seg_source, stringsAsFactors = FALSE)
  rownames(dv) <- docnames(corpus_aligne)
  docvars(dfm_obj) <- dv
  dfm_obj
}

construire_dfm_avec_fallback_stopwords <- function(tok_base, min_docfreq, retirer_stopwords, rv, libelle) {
  n_base <- compter_tokens(tok_base)
  ajouter_log(rv, paste0(libelle, " : tokens (avant stopwords) = ", n_base))

  if (isTRUE(retirer_stopwords)) {
    tok_sw <- tokens_remove(tok_base, stopwords("fr"))
    tok_sw <- tokens_tolower(tok_sw)
    n_sw <- compter_tokens(tok_sw)
    ajouter_log(rv, paste0(libelle, " : tokens (après stopwords) = ", n_sw))
    tok_final <- tok_sw
  } else {
    ajouter_log(rv, paste0(libelle, " : suppression des stopwords désactivée."))
    tok_final <- tokens_tolower(tok_base)
  }

  dfm_obj <- dfm(tok_final)
  dfm_obj <- dfm_trim(dfm_obj, min_docfreq = min_docfreq)
  ajouter_log(
    rv,
    paste0(
      libelle,
      " : DFM après trim = ", ndoc(dfm_obj), " docs ; ", nfeat(dfm_obj),
      ifelse(isTRUE(retirer_stopwords), " termes (avec stopwords retirés)", " termes (sans suppression des stopwords)")
    )
  )

  if (isTRUE(retirer_stopwords) && nfeat(dfm_obj) < 2) {
    ajouter_log(rv, paste0(libelle, " : DFM trop pauvre avec stopwords retirés. Relance automatique sans suppression des stopwords."))
    tok_final <- tokens_tolower(tok_base)
    dfm_obj <- dfm(tok_final)
    dfm_obj <- dfm_trim(dfm_obj, min_docfreq = min_docfreq)
    ajouter_log(rv, paste0(libelle, " : DFM après trim = ", ndoc(dfm_obj), " docs ; ", nfeat(dfm_obj), " termes (sans stopwords)"))
  }

  list(tok = tok_final, dfm = dfm_obj)
}

supprimer_docs_vides_dfm <- function(dfm_obj, corpus_aligne, tok_aligne, rv) {
  rs <- tryCatch(Matrix::rowSums(dfm_obj), error = function(e) NULL)

  if (is.null(rs)) {
    ajouter_log(rv, "Impossible de calculer rowSums(dfm). Aucune suppression de segments vides.")
    return(list(dfm = dfm_obj, corpus = corpus_aligne, tok = tok_aligne))
  }

  n_vides <- sum(rs == 0)
  if (n_vides > 0) {
    ajouter_log(rv, paste0("Segments vides (aucun terme) détectés : ", n_vides, ". Suppression avant CHD."))
    garder <- rs > 0
    dfm_obj <- dfm_obj[garder, ]
    noms <- docnames(dfm_obj)
    corpus_aligne <- corpus_aligne[noms]
    tok_aligne <- tok_aligne[noms]
  }

  list(dfm = dfm_obj, corpus = corpus_aligne, tok = tok_aligne)
}

verifier_dfm_avant_rainette <- function(dfm_obj, input) {
  if (ndoc(dfm_obj) < 2) {
    stop("Après filtrages, il reste moins de 2 segments utilisables. Réduis les filtrages ou augmente segment_size.")
  }
  if (nfeat(dfm_obj) < 2) {
    stop(
      "Après filtrages, il reste moins de 2 termes dans le DFM. ",
      "Même avec min_docfreq=1, cela arrive si le filtrage morphosyntaxique est trop strict et/ou si les stopwords retirent la majorité des formes. ",
      "Élargis les catégories morphosyntaxiques ou augmente segment_size."
    )
  }
  if (input$k >= ndoc(dfm_obj)) {
    stop("k est trop grand par rapport au nombre de segments restants. Diminue k ou réduis les filtrages.")
  }
}



executer_spacy_filtrage <- function(ids, textes, pos_a_conserver, utiliser_lemmes, retirer_stopwords, lower_input, rv) {
  script_spacy <- tryCatch(normalizePath("spacy_preprocess.py", mustWork = TRUE), error = function(e) NA_character_)
  if (is.na(script_spacy) || !file.exists(script_spacy)) stop("Script spaCy introuvable : spacy_preprocess.py (à la racine du projet).")

  python_cmd <- "python3"

  in_tsv <- file.path(tempdir(), paste0("spacy_in_", Sys.getpid(), ".tsv"))
  out_tsv <- file.path(tempdir(), paste0("spacy_out_", Sys.getpid(), ".tsv"))
  tok_tsv <- file.path(tempdir(), paste0("spacy_tokens_", Sys.getpid(), ".tsv"))

  df_in <- data.frame(doc_id = ids, text = textes, stringsAsFactors = FALSE)

  write.table(
    df_in, file = in_tsv, sep = "\t", quote = FALSE,
    row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8"
  )

  if (is.null(pos_a_conserver) || length(pos_a_conserver) == 0) pos_a_conserver <- c("NOUN", "ADJ")

  args <- c(
    script_spacy,
    "--input", in_tsv,
    "--output", out_tsv,
    "--modele", "fr_core_news_md",
    "--pos_keep", paste(pos_a_conserver, collapse = ","),
    "--lemmes", ifelse(isTRUE(utiliser_lemmes), "1", "0"),
    "--retirer_stopwords", ifelse(isTRUE(retirer_stopwords), "1", "0"),
    "--lower_input", ifelse(isTRUE(lower_input), "1", "0"),
    "--output_tokens", tok_tsv
  )

  ajouter_log(rv, paste0("spaCy : exécution (", python_cmd, " ", paste(args, collapse = " "), ")"))

  sortie <- tryCatch(system2(python_cmd, args = args, stdout = TRUE, stderr = TRUE), error = function(e) stop("Erreur exécution spaCy : ", e$message))
  if (!is.null(sortie) && length(sortie) > 0) ajouter_log(rv, paste(sortie, collapse = "\n"))

  if (!file.exists(out_tsv)) stop("spaCy n'a pas produit de fichier de sortie.")

  df_out <- read.delim(out_tsv, sep = "\t", stringsAsFactors = FALSE, quote = "", fileEncoding = "UTF-8")
  if (!all(c("doc_id", "text") %in% names(df_out))) stop("Sortie spaCy invalide : colonnes attendues 'doc_id' et 'text'.")

  df_tok <- NULL
  if (file.exists(tok_tsv)) {
    df_tok <- read.delim(tok_tsv, sep = "\t", stringsAsFactors = FALSE, quote = "", fileEncoding = "UTF-8")
    colonnes_attendues <- c("doc_id", "token", "lemma", "pos")
    if (!all(colonnes_attendues %in% names(df_tok))) df_tok <- NULL
  }

  res <- setNames(df_out$text, df_out$doc_id)
  list(textes = res[ids], tokens_df = df_tok)
}

executer_spacy_ner <- function(ids, textes, rv) {
  script_ner <- tryCatch(normalizePath("ner.py", mustWork = TRUE), error = function(e) NA_character_)
  if (is.na(script_ner) || !file.exists(script_ner)) stop("Script NER introuvable : ner.py (à la racine du projet).")

  python_cmd <- "python3"

  in_tsv <- file.path(tempdir(), paste0("ner_in_", Sys.getpid(), ".tsv"))
  out_tsv <- file.path(tempdir(), paste0("ner_out_", Sys.getpid(), ".tsv"))

  df_in <- data.frame(doc_id = ids, text = textes, stringsAsFactors = FALSE)

  write.table(
    df_in, file = in_tsv, sep = "\t", quote = FALSE,
    row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8"
  )

  args <- c(script_ner, "--input", in_tsv, "--output", out_tsv, "--modele", "fr_core_news_md")

  ajouter_log(rv, paste0("NER : exécution (", python_cmd, " ", paste(args, collapse = " "), ")"))

  sortie <- tryCatch(system2(python_cmd, args = args, stdout = TRUE, stderr = TRUE), error = function(e) stop("Erreur exécution NER : ", e$message))
  if (!is.null(sortie) && length(sortie) > 0) ajouter_log(rv, paste(sortie, collapse = "\n"))

  if (!file.exists(out_tsv)) stop("NER n'a pas produit de fichier de sortie.")

  df_ent <- read.delim(out_tsv, sep = "\t", stringsAsFactors = FALSE, quote = "", fileEncoding = "UTF-8")
  colonnes_attendues <- c("doc_id", "ent_text", "ent_label", "start_char", "end_char")
  if (!all(colonnes_attendues %in% names(df_ent))) stop("Sortie NER invalide : colonnes attendues 'doc_id, ent_text, ent_label, start_char, end_char'.")

  df_ent$doc_id <- trimws(as.character(df_ent$doc_id))
  df_ent$ent_text <- trimws(gsub("\\s+", " ", as.character(df_ent$ent_text), perl = TRUE))
  df_ent$ent_label <- as.character(df_ent$ent_label)
  df_ent <- df_ent[!is.na(df_ent$ent_text) & nzchar(df_ent$ent_text), , drop = FALSE]
  df_ent
}

obtenir_objet_dendrogramme <- function(res) {
  if (is.null(res)) return(NULL)

  if (inherits(res, "hclust") || inherits(res, "dendrogram")) return(res)

  if (is.list(res)) {
    candidats <- c("tree", "hc", "hclust", "dendro", "dendrogram")
    for (nm in candidats) {
      if (!is.null(res[[nm]]) && (inherits(res[[nm]], "hclust") || inherits(res[[nm]], "dendrogram"))) {
        return(res[[nm]])
      }
    }
  }

  NULL
}



construire_graphe_adjacence <- function(mat) {
  if ("graph_from_adjacency_matrix" %in% getNamespaceExports("igraph")) {
    igraph::graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE, diag = FALSE)
  } else {
    igraph::graph.adjacency(mat, mode = "undirected", weighted = TRUE, diag = FALSE)
  }
}

generer_chd_explor_si_absente <- function(rv) {
  if (is.null(rv$export_dir) || !nzchar(rv$export_dir)) return(FALSE)

  explor_dir <- file.path(rv$export_dir, "explor")
  dir.create(explor_dir, showWarnings = FALSE, recursive = TRUE)

  chd_png <- file.path(explor_dir, "chd.png")
  if (file.exists(chd_png)) return(TRUE)

  chd_obj <- rv$res_chd
  if (is.null(chd_obj)) chd_obj <- rv$res
  if (is.null(chd_obj)) return(FALSE)

  ok <- FALSE
  try({
    png(chd_png, width = 2000, height = 1500, res = 180)
    rainette_plot(chd_obj)
    dev.off()
    ok <- file.exists(chd_png)
  }, silent = TRUE)

  if (!ok) {
    try(grDevices::dev.off(), silent = TRUE)
  }

  ok
}

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

  output$logs <- renderText(rv$logs)
  output$statut <- renderText(rv$statut)

  output$barre_progression <- renderUI({
    p <- max(0, min(100, rv$progression))
    tags$div(
      style = "width: 100%; border: 1px solid #999; height: 20px; position: relative;",
      tags$div(style = paste0("width: ", p, "%; height: 100%; background-color: #4C9AFF;")),
      tags$div(
        style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; text-align: center; line-height: 20px; font-size: 12px;",
        paste0(p, "%")
      )
    )
  })

  output$table_classes <- renderTable({
    req(rv$filtered_corpus)
    tb <- table(docvars(rv$filtered_corpus)$Classes, useNA = "ifany")
    data.frame(Classe = names(tb), Effectif = as.integer(tb), stringsAsFactors = FALSE)
  }, rownames = FALSE)

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

  observeEvent(input$lancer, {
    rv$logs <- ""
    rv$statut <- "Vérification du fichier..."
    rv$progression <- 0

    rv$spacy_tokens_df <- NULL
    rv$textes_indexation <- NULL
    rv$ner_df <- NULL
    rv$ner_nb_segments <- NA_integer_
    rv$afc_obj <- NULL
    rv$afc_erreur <- NULL
    rv$afc_vars_obj <- NULL
    rv$afc_vars_erreur <- NULL

    rv$afc_dir <- NULL
    rv$afc_table_mots <- NULL
    rv$afc_table_vars <- NULL
    rv$afc_plot_classes <- NULL
    rv$afc_plot_termes <- NULL
    rv$afc_plot_vars <- NULL

    rv$segments_file <- NULL
    rv$stats_file <- NULL
    rv$html_file <- NULL
    rv$ner_file <- NULL
    rv$zip_file <- NULL

    rv$res <- NULL
    rv$res_chd <- NULL
    rv$dfm_chd <- NULL
    rv$res_type <- "simple"
    rv$max_n_groups <- NULL
    rv$max_n_groups_chd <- NULL
    rv$explor_assets <- NULL

    ajouter_log(rv, "Clic sur 'Lancer l'analyse' reçu.")

    if (is.null(input$fichier_corpus) || is.null(input$fichier_corpus$datapath) || !file.exists(input$fichier_corpus$datapath)) {
      rv$statut <- "Aucun fichier uploadé."
      rv$progression <- 0
      ajouter_log(rv, "Aucun fichier uploadé côté serveur. Sélectionne un .txt puis relance.")
      showNotification("Aucun fichier uploadé. Choisis un .txt.", type = "error", duration = 6)
      return(invisible(NULL))
    }

    withProgress(message = "Analyse Rainette en cours", value = 0, {

      p <- Progress$new(session, min = 0, max = 1)
      on.exit(try(p$close(), silent = TRUE), add = TRUE)

      avancer <- function(valeur, detail) {
        valeur <- max(0, min(1, valeur))
        p$set(value = valeur, message = "Analyse Rainette en cours", detail = detail)
        rv$progression <- round(valeur * 100)
      }

      tryCatch({

        avancer(0.02, "Préparation des répertoires")
        rv$statut <- "Préparation des répertoires..."

        rv$base_dir <- file.path(tempdir(), paste0("rainette_", session$token))
        rv$export_dir <- file.path(rv$base_dir, "exports")
        dir.create(rv$export_dir, showWarnings = FALSE, recursive = TRUE)
        ajouter_log(rv, paste0("export_dir = ", rv$export_dir))

        avancer(0.08, "Import du corpus")
        rv$statut <- "Import du corpus..."
        chemin_fichier <- input$fichier_corpus$datapath
        md5 <- md5_fichier(chemin_fichier)
        ajouter_log(rv, paste0("MD5 fichier = ", md5))

        corpus <- import_corpus_iramuteq(chemin_fichier)
        ajouter_log(rv, paste0("Nombre de documents importés : ", ndoc(corpus)))

        avancer(0.14, "Segmentation")
        rv$statut <- "Segmentation..."
        segment_size <- input$segment_size
        corpus <- split_segments(corpus, segment_size = segment_size)
        ajouter_log(rv, paste0("Nombre de segments après découpage : ", ndoc(corpus)))

        ids_corpus <- docnames(corpus)
        textes_orig <- as.character(corpus)

        avancer(0.18, "Préparation texte (nettoyage / minuscules)")
        rv$statut <- "Préparation texte..."

        textes_chd <- appliquer_nettoyage_et_minuscules(
          textes = textes_orig,
          activer_nettoyage = isTRUE(input$nettoyage_caracteres),
          forcer_minuscules = isTRUE(input$forcer_minuscules_avant),
          supprimer_chiffres = isTRUE(input$supprimer_chiffres)
        )
        names(textes_chd) <- ids_corpus

        avancer(0.22, "Prétraitement + DFM")
        rv$statut <- "Prétraitement et DFM..."

        filtrage_morpho <- isTRUE(input$filtrage_morpho)
        utiliser_lemmes <- isTRUE(input$spacy_utiliser_lemmes)
        retirer_stopwords_spacy <- isTRUE(input$spacy_retirer_stopwords)
        utiliser_pipeline_spacy <- filtrage_morpho || utiliser_lemmes || retirer_stopwords_spacy

        if (!utiliser_pipeline_spacy) {

          ajouter_log(rv, "Filtrage morphosyntaxique désactivé : pipeline standard.")
          tok_base <- tokens(textes_chd, remove_punct = TRUE, remove_numbers = isTRUE(input$supprimer_chiffres))
          tok_base <- tokens_split(tok_base, "'")

          res_dfm <- construire_dfm_avec_fallback_stopwords(
            tok_base = tok_base,
            min_docfreq = input$min_docfreq,
            retirer_stopwords = isTRUE(input$retirer_stopwords),
            rv = rv,
            libelle = "Standard"
          )
          tok <- res_dfm$tok
          dfm_obj <- res_dfm$dfm

        } else {

          pos_a_conserver <- NULL
          if (isTRUE(filtrage_morpho)) {
            pos_a_conserver <- input$pos_spacy_a_conserver
            if (is.null(pos_a_conserver) || length(pos_a_conserver) == 0) pos_a_conserver <- c("NOUN", "ADJ")
          }

          ajouter_log(
            rv,
            paste0(
              "spaCy (fr_core_news_md) | filtrage POS=", ifelse(filtrage_morpho, "1", "0"),
              ifelse(filtrage_morpho, paste0(" (", paste(pos_a_conserver, collapse = ", "), ")"), ""),
              " | lemmes=", ifelse(utiliser_lemmes, "1", "0"),
              " | stopwords_apres_spacy(R)=", ifelse(retirer_stopwords_spacy, "1", "0")
            )
          )

          avancer(0.28, "spaCy : exécution Python")
          rv$statut <- "spaCy : prétraitement..."

          sp <- executer_spacy_filtrage(
            ids = ids_corpus,
            textes = unname(textes_chd),
            pos_a_conserver = pos_a_conserver,
            utiliser_lemmes = utiliser_lemmes,
            retirer_stopwords = FALSE,
            lower_input = isTRUE(input$forcer_minuscules_avant),
            rv = rv
          )

          textes_spacy <- sp$textes
          names(textes_spacy) <- ids_corpus
          rv$spacy_tokens_df <- sp$tokens_df

          avancer(0.40, "spaCy : tokens + DFM")
          tok_base <- tokens(textes_spacy, remove_punct = TRUE, remove_numbers = isTRUE(input$supprimer_chiffres))
          tok_base <- tokens_split(tok_base, "'")

          res_dfm <- construire_dfm_avec_fallback_stopwords(
            tok_base = tok_base,
            min_docfreq = input$min_docfreq,
            retirer_stopwords = retirer_stopwords_spacy,
            rv = rv,
            libelle = "spaCy"
          )
          tok <- res_dfm$tok
          dfm_obj <- res_dfm$dfm
        }

        included_segments <- docnames(dfm_obj)
        filtered_corpus <- corpus[included_segments]
        tok <- tok[included_segments]

        dfm_obj <- assurer_docvars_dfm_minimal(dfm_obj, filtered_corpus)

        tmp <- supprimer_docs_vides_dfm(dfm_obj, filtered_corpus, tok, rv)
        dfm_obj <- tmp$dfm
        filtered_corpus <- tmp$corpus
        tok <- tmp$tok

        ajouter_log(rv, paste0("Après suppression segments vides : ", ndoc(dfm_obj), " docs ; ", nfeat(dfm_obj), " termes."))
        verifier_dfm_avant_rainette(dfm_obj, input)

        rv$textes_indexation <- vapply(as.list(tok), function(x) paste(x, collapse = " "), FUN.VALUE = character(1))
        names(rv$textes_indexation) <- docnames(dfm_obj)

        avancer(0.52, "Classification (rainette / rainette2)")
        rv$statut <- "Classification en cours..."

        type_classif <- as.character(input$type_classification)
        if (!type_classif %in% c("simple", "double")) type_classif <- "simple"

        groupes <- NULL
        res_final <- NULL

        if (type_classif == "simple") {

          rv$res_type <- "simple"
          ajouter_log(rv, "Mode : classification simple (rainette).")

          res <- rainette(
            dfm_obj,
            k = input$k,
            min_segment_size = input$min_segment_size,
            min_split_members = input$min_split_members,
            doc_id = "segment_source"
          )

          if (is.null(res) || is.null(res$group) || length(res$group) == 0) stop("Rainette n'a pas pu calculer de clusters. Diminue les filtrages, augmente segment_size, ou réduis k.")

          groupes <- res$group
          res_final <- res
          rv$res_chd <- res
          rv$dfm_chd <- dfm_obj
          rv$max_n_groups <- max(res$group, na.rm = TRUE)
          rv$max_n_groups_chd <- rv$max_n_groups

        } else {

          rv$res_type <- "double"
          ajouter_log(rv, "Mode : classification double (rainette2).")

          res1 <- rainette(dfm_obj, k = input$k, min_segment_size = input$min_segment_size, min_split_members = input$min_split_members, doc_id = "segment_source")
          if (is.null(res1) || is.null(res1$group) || length(res1$group) == 0) stop("Classification 1 (rainette) impossible.")

          res2 <- rainette(dfm_obj, k = input$k, min_segment_size = input$min_segment_size2, min_split_members = input$min_split_members, doc_id = "segment_source")
          if (is.null(res2) || is.null(res2$group) || length(res2$group) == 0) stop("Classification 2 (rainette) impossible.")

          res_d <- rainette2(res1, res2, max_k = input$max_k_double)
          groupes <- cutree(res_d, k = input$k)

          res_final <- res_d
          rv$res_chd <- res1
          rv$dfm_chd <- dfm_obj
          rv$max_n_groups <- input$max_k_double
          rv$max_n_groups_chd <- max(res1$group, na.rm = TRUE)
        }

        docvars(filtered_corpus)$Classes <- groupes

        idx_ok <- !is.na(docvars(filtered_corpus)$Classes)
        filtered_corpus_ok <- filtered_corpus[idx_ok]
        dfm_ok <- dfm_obj[idx_ok, ]
        tok_ok <- tok[idx_ok]

        if (ndoc(dfm_ok) < 2) stop("Après classification, il reste moins de 2 segments classés (hors NA).")
        if (nfeat(dfm_ok) < 2) stop("Après classification, le DFM classé est trop pauvre (moins de 2 termes).")

        rv$clusters <- sort(unique(docvars(filtered_corpus_ok)$Classes))
        rv$res <- res_final
        rv$dfm <- dfm_ok
        rv$filtered_corpus <- filtered_corpus_ok
        rv$res_stats_df <- NULL

        avancer(0.58, "NER (si activé)")
        rv$statut <- "NER (si activé)..."

        if (isTRUE(input$activer_ner)) {
          ids_ner <- docnames(filtered_corpus_ok)
          textes_ner <- as.character(filtered_corpus_ok)
          rv$ner_nb_segments <- length(textes_ner)

          df_ent <- executer_spacy_ner(ids_ner, textes_ner, rv)

          classes_vec <- as.integer(docvars(filtered_corpus_ok)$Classes)
          names(classes_vec) <- ids_ner

          df_ent$Classe <- classes_vec[df_ent$doc_id]
          df_ent$Classe <- as.integer(df_ent$Classe)
          df_ent <- df_ent[!is.na(df_ent$Classe), , drop = FALSE]

          rv$ner_df <- df_ent
        }

        avancer(0.62, "Exports + stats")
        rv$statut <- "Exports et statistiques..."

        segments_vec <- as.character(filtered_corpus_ok)
        names(segments_vec) <- docnames(filtered_corpus_ok)
        segments_by_class <- split(segments_vec, docvars(filtered_corpus_ok)$Classes)

        segments_file <- file.path(rv$export_dir, "segments_par_classe.txt")
        writeLines(unlist(lapply(names(segments_by_class), function(cl) c(paste0("Classe ", cl, ":"), unname(segments_by_class[[cl]]), ""))), segments_file)

        res_stats_list <- rainette_stats(
          dtm = dfm_ok,
          groups = docvars(filtered_corpus_ok)$Classes,
          measure = c("chi2", "lr", "frequency", "docprop"),
          n_terms = 9999,
          show_negative = TRUE,
          max_p = input$max_p
        )

        res_stats_df <- bind_rows(res_stats_list, .id = "ClusterID") %>%
          rename(Terme = feature, Classe = ClusterID) %>%
          mutate(
            Classe = as.numeric(Classe),
            p_value_filter = ifelse(p <= input$max_p, paste0("≤ ", input$max_p), paste0("> ", input$max_p))
          ) %>%
          arrange(Classe, desc(chi2))

        stats_file <- file.path(rv$export_dir, "stats_par_classe.csv")
        write.csv(res_stats_df, stats_file, row.names = FALSE)

        rv$segments_file <- segments_file
        rv$stats_file <- stats_file
        rv$res_stats_df <- res_stats_df

        avancer(0.72, "AFC (classes × termes)")
        rv$statut <- "Calcul AFC classes × termes..."

        rv$afc_obj <- NULL
        rv$afc_erreur <- NULL
        rv$afc_vars_obj <- NULL
        rv$afc_vars_erreur <- NULL
        rv$afc_dir <- file.path(rv$export_dir, "afc")
        dir.create(rv$afc_dir, showWarnings = FALSE, recursive = TRUE)

        termes_signif <- unique(subset(res_stats_df, p <= input$max_p)$Terme)
        termes_signif <- termes_signif[!is.na(termes_signif) & nzchar(termes_signif)]
        if (length(termes_signif) < 2) termes_signif <- NULL

        tryCatch({
          groupes_docs <- docvars(filtered_corpus_ok)$Classes

          obj <- executer_afc_classes(
            dfm_obj = dfm_ok,
            groupes = groupes_docs,
            termes_cibles = termes_signif,
            max_termes = 400,
            seuil_p = input$max_p,
            rv = rv
          )

          if (!is.null(obj$termes_stats) && !is.null(rv$res_stats_df)) {
            df_m <- obj$termes_stats
            df_m$Classe_num <- suppressWarnings(as.numeric(gsub("^Classe\\s+", "", as.character(df_m$Classe_max))))
            rs <- rv$res_stats_df

            rs2 <- rs[, intersect(c("Terme", "Classe", "chi2", "p", "frequency", "docprop", "lr"), names(rs)), drop = FALSE]
            rs2$Classe <- as.numeric(rs2$Classe)

            m <- merge(
              df_m,
              rs2,
              by.x = c("Terme", "Classe_num"),
              by.y = c("Terme", "Classe"),
              all.x = TRUE,
              suffixes = c("_global", "_rainette")
            )

            if ("chi2" %in% names(m)) {
              df_m$chi2 <- ifelse(is.na(m$chi2), df_m$chi2, m$chi2)
            }
            if ("p" %in% names(m)) {
              df_m$p_value <- ifelse(is.na(m$p), df_m$p_value, m$p)
            }

            df_m$Classe_num <- NULL
            obj$termes_stats <- df_m
          }

          rv$afc_obj <- obj
          ajouter_log(rv, "AFC classes × termes : calcul terminé.")

        }, error = function(e) {
          rv$afc_erreur <- paste0("AFC classes × termes : ", e$message)
          ajouter_log(rv, rv$afc_erreur)
          showNotification(rv$afc_erreur, type = "error", duration = 8)
        })

        avancer(0.74, "AFC (variables étoilées)")
        rv$statut <- "Calcul AFC variables étoilées..."

        tryCatch({
          if (!is.null(docvars(filtered_corpus_ok)$Classes)) {
            objv <- executer_afc_variables_etoilees(
              corpus_aligne = filtered_corpus_ok,
              groupes = docvars(filtered_corpus_ok)$Classes,
              max_modalites = 400,
              seuil_p = input$max_p,
              rv = rv
            )
            rv$afc_vars_obj <- objv
            ajouter_log(rv, "AFC variables étoilées : calcul terminé.")
          }
        }, error = function(e) {
          rv$afc_vars_erreur <- paste0("AFC variables étoilées : ", e$message)
          ajouter_log(rv, rv$afc_vars_erreur)
        })

        if (!is.null(rv$afc_obj) && !is.null(rv$afc_obj$ca)) {

          afc_classes_png <- file.path(rv$afc_dir, "afc_classes.png")
          afc_termes_png <- file.path(rv$afc_dir, "afc_termes.png")

          activer_repel <- TRUE
          if (!is.null(input$afc_reduire_chevauchement)) activer_repel <- isTRUE(input$afc_reduire_chevauchement)

          taille_sel <- "frequency"
          if (!is.null(input$afc_taille_mots) && nzchar(as.character(input$afc_taille_mots))) {
            taille_sel <- as.character(input$afc_taille_mots)
          }
          if (!taille_sel %in% c("frequency", "chi2")) taille_sel <- "frequency"

          top_termes <- 120
          if (!is.null(input$afc_top_termes) && is.finite(input$afc_top_termes)) top_termes <- as.integer(input$afc_top_termes)

          png(afc_classes_png, width = 1800, height = 1400, res = 180)
          try(tracer_afc_classes_seules(rv$afc_obj, axes = c(1, 2), cex_labels = 1.05), silent = TRUE)
          dev.off()

          png(afc_termes_png, width = 2000, height = 1600, res = 180)
          try(tracer_afc_classes_termes(rv$afc_obj, axes = c(1, 2), top_termes = top_termes, taille_sel = taille_sel, activer_repel = activer_repel), silent = TRUE)
          dev.off()

          rv$afc_plot_classes <- afc_classes_png
          rv$afc_plot_termes <- afc_termes_png

          write.csv(rv$afc_obj$table, file.path(rv$afc_dir, "table_classes_termes.csv"), row.names = TRUE)
          write.csv(rv$afc_obj$rowcoord, file.path(rv$afc_dir, "coords_classes.csv"), row.names = TRUE)
          write.csv(rv$afc_obj$colcoord, file.path(rv$afc_dir, "coords_termes.csv"), row.names = TRUE)
          write.csv(rv$afc_obj$termes_stats, file.path(rv$afc_dir, "stats_termes.csv"), row.names = FALSE)

          if (!is.null(rv$afc_obj$ca$eig)) {
            write.csv(as.data.frame(rv$afc_obj$ca$eig), file.path(rv$afc_dir, "valeurs_propres.csv"), row.names = TRUE)
          }

          rv$afc_table_mots <- rv$afc_obj$termes_stats
        }

        if (!is.null(rv$afc_vars_obj) && !is.null(rv$afc_vars_obj$ca)) {

          afc_vars_png <- file.path(rv$afc_dir, "afc_variables_etoilees.png")

          activer_repel2 <- TRUE
          if (!is.null(input$afc_reduire_chevauchement)) activer_repel2 <- isTRUE(input$afc_reduire_chevauchement)

          top_mod <- 120
          if (!is.null(input$afc_top_modalites) && is.finite(input$afc_top_modalites)) top_mod <- as.integer(input$afc_top_modalites)

          png(afc_vars_png, width = 2000, height = 1600, res = 180)
          try(tracer_afc_variables_etoilees(rv$afc_vars_obj, axes = c(1, 2), top_modalites = top_mod, activer_repel = activer_repel2), silent = TRUE)
          dev.off()

          rv$afc_plot_vars <- afc_vars_png

          write.csv(rv$afc_vars_obj$table, file.path(rv$afc_dir, "table_classes_variables.csv"), row.names = TRUE)
          write.csv(rv$afc_vars_obj$rowcoord, file.path(rv$afc_dir, "coords_classes_vars.csv"), row.names = TRUE)
          write.csv(rv$afc_vars_obj$colcoord, file.path(rv$afc_dir, "coords_modalites.csv"), row.names = TRUE)
          write.csv(rv$afc_vars_obj$modalites_stats, file.path(rv$afc_dir, "stats_modalites.csv"), row.names = FALSE)

          if (!is.null(rv$afc_vars_obj$ca$eig)) {
            write.csv(as.data.frame(rv$afc_vars_obj$ca$eig), file.path(rv$afc_dir, "valeurs_propres_vars.csv"), row.names = TRUE)
          }

          rv$afc_table_vars <- rv$afc_vars_obj$modalites_stats
        }

        avancer(0.76, "Concordancier HTML")
        rv$statut <- "Concordancier..."

        html_file <- file.path(rv$export_dir, "segments_par_classe.html")
        textes_index_ok <- rv$textes_indexation[docnames(dfm_ok)]
        names(textes_index_ok) <- docnames(dfm_ok)

        wordcloud_dir <- file.path(rv$export_dir, "wordclouds")
        dir.create(wordcloud_dir, showWarnings = FALSE, recursive = TRUE)
        cooc_dir <- file.path(rv$export_dir, "cooccurrences")
        dir.create(cooc_dir, showWarnings = FALSE, recursive = TRUE)

        classes_uniques <- sort(unique(as.integer(docvars(filtered_corpus_ok)$Classes)))

        for (cl in classes_uniques) {
          top_n_demande <- suppressWarnings(as.integer(input$top_n))
          if (!is.finite(top_n_demande) || is.na(top_n_demande)) top_n_demande <- 20L
          top_n_demande <- max(5L, top_n_demande)

          df_stats_cl <- subset(res_stats_df, Classe == cl & p <= input$max_p)
          if (nrow(df_stats_cl) > 0) {
            df_stats_cl <- df_stats_cl[order(-df_stats_cl$chi2), , drop = FALSE]
            df_stats_cl <- head(df_stats_cl, top_n_demande)

            wc_png <- file.path(wordcloud_dir, paste0("cluster_", cl, "_wordcloud.png"))
            try({
              png(wc_png, width = 800, height = 600)
              suppressWarnings(wordcloud(
                words = df_stats_cl$Terme,
                freq = df_stats_cl$chi2,
                scale = c(10, 0.5),
                min.freq = 0,
                max.words = nrow(df_stats_cl),
                colors = brewer.pal(8, "Dark2")
              ))
              dev.off()
            }, silent = TRUE)
          }

          tok_cl <- tok_ok[docvars(filtered_corpus_ok)$Classes == cl]
          cooc_png <- file.path(cooc_dir, paste0("cluster_", cl, "_fcm_network.png"))

          try({
            if (length(tok_cl) > 0) {
              fcm_cl <- fcm(tok_cl, context = "window", window = max(1, as.integer(input$window_cooc)), tri = FALSE)
              term_freq <- sort(colSums(fcm_cl), decreasing = TRUE)
              top_feat_demande <- suppressWarnings(as.integer(input$top_feat))
              if (!is.finite(top_feat_demande) || is.na(top_feat_demande)) top_feat_demande <- 20L
              top_feat_demande <- max(5L, top_feat_demande)

              # On borne aussi par top_n pour garder une cohérence entre nuage de mots et graphe de cooccurrences.
              top_feat_effectif <- min(top_feat_demande, top_n_demande)
              feat_sel <- names(term_freq)[seq_len(min(top_feat_effectif, length(term_freq)))]
              fcm_cl <- fcm_select(fcm_cl, feat_sel, selection = "keep")

              adj <- as.matrix(fcm_cl)
              g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE, diag = FALSE)

              num_nodes <- length(V(g))
              palette_colors <- brewer.pal(min(8, num_nodes), "Set3")
              V(g)$color <- palette_colors[seq_along(V(g))]

              png(cooc_png, width = 1600, height = 1200)
              plot(
                g,
                layout = layout_with_fr(g),
                main = paste("Cooccurrences - Classe", cl),
                vertex.size = 16,
                vertex.color = V(g)$color,
                vertex.label = V(g)$name,
                vertex.label.cex = 1,
                edge.width = E(g)$weight / 2,
                edge.color = "gray80"
              )
              dev.off()
            }
          }, silent = TRUE)
        }

        args_concordancier <- list(
          chemin_sortie = html_file,
          segments_by_class = segments_by_class,
          res_stats_df = res_stats_df,
          max_p = input$max_p,
          textes_indexation = textes_index_ok,
          spacy_tokens_df = rv$spacy_tokens_df,
          avancer = avancer,
          rv = rv
        )

        do.call(generer_concordancier_html, args_concordancier)

        rv$html_file <- html_file

        avancer(0.96, "ZIP")
        rv$statut <- "Création ZIP..."
        rv$zip_file <- file.path(rv$base_dir, "exports_rainette.zip")
        if (file.exists(rv$zip_file)) unlink(rv$zip_file)

        ancien_wd <- getwd()
        setwd(rv$base_dir)
        utils::zip(zipfile = rv$zip_file, files = "exports")
        setwd(ancien_wd)

        if (!(rv$exports_prefix %in% names(shiny::resourcePaths()))) {
          shiny::addResourcePath(rv$exports_prefix, rv$export_dir)
        }

        rv$statut <- "Analyse terminée."
        rv$progression <- 100
        ajouter_log(rv, "Analyse terminée.")
        showNotification("Analyse terminée.", type = "message", duration = 5)

      }, error = function(e) {
        rv$statut <- paste0("Erreur : ", e$message)
        ajouter_log(rv, paste0("ERREUR : ", e$message))
        showNotification(e$message, type = "error", duration = 8)
      })
    })
  })


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

  output$table_afc_mots <- renderTable({
    if (is.null(rv$afc_table_mots)) {
      return(data.frame(Message = "AFC mots : non disponible.", stringsAsFactors = FALSE))
    }
    df <- rv$afc_table_mots
    colonnes <- intersect(c("Terme", "Classe_max", "frequency", "chi2", "p_value"), names(df))
    df <- df[, colonnes, drop = FALSE]
    if ("p_value" %in% names(df)) {
      df$p_value <- ifelse(
        is.na(df$p_value),
        NA_character_,
        formatC(df$p_value, format = "f", digits = 6)
      )
    }
    if ("chi2" %in% names(df)) df <- df[order(-df$chi2), , drop = FALSE]
    head(df, 200)
  }, rownames = FALSE)

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
      df$p_value <- ifelse(
        is.na(df$p_value),
        NA_character_,
        formatC(df$p_value, format = "f", digits = 6)
      )
    }
    if ("chi2" %in% names(df)) df <- df[order(-df$chi2), , drop = FALSE]
    head(df, 200)
  }, rownames = FALSE)

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
