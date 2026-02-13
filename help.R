################################
## Rainette ## développé par Julien Barnier
# Doc Rainette : https://github.com/juba/rainette/blob/main/vignettes/introduction_usage.Rmd
# https://cran.r-project.org/web/packages/rainette/vignettes/introduction_usage.html
# https://juba.r-universe.dev/builds
################################

# install.packages(c("rainette", "quanteda", "wordcloud", "RColorBrewer", "igraph", "dplyr", "shiny"))
# install.packages("htmltools")

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

ui_aide_huggingface <- function() {
  tagList(
    tags$h2("Aide"),

    tags$h3("Pourquoi vos fichiers peuvent disparaître sur Hugging Face"),
    tags$p("Sur Hugging Face Spaces, le stockage local de ce conteneur est temporaire : si le serveur redémarre, ou si la page est rechargée après une déconnexion, les fichiers générés pendant une analyse précédente peuvent ne plus être disponibles."),
    tags$p("Conseil : télécharge l’archive ZIP des exports juste après la fin de l’analyse."),

    tags$h3("Logique générale de l’application"),
    tags$p("Uploadez un fichier texte au format IRaMuTeQ. L’app segmente, construit un DFM, lance la CHD avec rainette, calcule les statistiques, génère un HTML surligné, puis produit des images (nuages de mots et réseaux de cooccurrences). L’onglet d’exploration (rainette_explor) permet de visualiser la CHD."),

    tags$h3("Paramètres de l’analyse"),
    tags$p(tags$b("segment_size"), " : taille des segments lors du découpage du corpus. Plus petit donne plus de segments, plus grand donne des segments plus longs."),
    tags$p(tags$b("k (nombre de classes)"), " : nombre de classes demandé pour la CHD."),
    tags$p(tags$b("min_segment_size"), " : nombre minimal de termes par segment. En effet, lors de la tokenisation et du calcul de la dtm, certaines formes (mots-outils, mots trop peu fréquents) ont été supprimées, nos segments peuvent donc varier en taille (entendue comme le nombre de termes encore présents). Avec `min_segment_size = 10`, les segments comportant moins de 10 formes sont regroupés avec le segment suivant ou précédent du même document (si possible) jusqu'à atteindre la taille minimale souhaitée."),
    tags$p(tags$b("min_split_members"), " : nombre minimal de documents pour qu'une classe soit scindée en deux à l'étape suivante de la classification."),
    tags$p(tags$b("dfm_trim min_docfreq"), " : fréquence minimale en nombre de segments pour conserver un terme dans le DFM. Plus haut enlève les termes rares."),
    tags$p(tags$b("max_p (p-value)"), " : seuil de p-value pour filtrer les termes mis en avant dans les statistiques et le surlignage HTML."),
    tags$p(tags$b("top_n (wordcloud)"), " : nombre de termes affichés dans chaque nuage de mots."),
    tags$p(tags$b("window (cooccurrences)"), " : taille de la fenêtre glissante pour calculer les cooccurrences."),
    tags$p(tags$b("top_feat (cooccurrences)"), " : nombre de termes retenus pour construire le réseau de cooccurrences."),

    tags$h3("Classification double (rainette2)"),
    tags$p(tags$b("Classification double"), " : l’application combine deux classifications rainette (res1 et res2) via rainette2, puis découpe l’arbre final avec k."),

    tags$h3("Lemmatisation (option)"),
    tags$p(tags$b("Lemmatisation"), " : si activée, le texte est analysé avec Spacy."),
    tags$p(tags$b("Tokens à conserver"), " : filtre les tokens conservés selon leur catégorie grammaticale (ex. NOUN, ADJ, VERB, PROPN, ADV). Si tu ne gardes que NOUN et ADJ, tu supprimes volontairement le reste (verbes, etc.), ce qui peut modifier la CHD."),

    tags$h3("Exploration"),
    tags$p(tags$b("Classe"), " : sélection de la classe pour afficher les images et la table de statistiques associées."),
    tags$p(tags$b("CHD (rainette_plot)"), " : affichage graphique de la CHD dans l’application."),
    tags$p(tags$b("Type"), " : bar (barres) ou cloud (nuage) pour l’affichage des termes par classe."),
    tags$p(tags$b("Statistiques"), " : chi2, lr, frequency, docprop selon le critère utilisé pour classer les termes."),
    tags$p(tags$b("Nombre de termes"), " : nombre de termes affichés par classe dans la visualisation."),
    tags$p(tags$b("Forcer les mêmes échelles"), " : rend les panneaux comparables entre classes en imposant une échelle commune."),
    tags$p(tags$b("Afficher les valeurs négatives"), " : inclut les termes négativement associés à une classe."),
    tags$p(tags$b("Taille du texte"), " : taille des labels dans la visualisation.")
  )
}
