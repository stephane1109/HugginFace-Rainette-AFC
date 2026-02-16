# Vérification rapide + plan d'allègement supplémentaire de `app.R`

## 1) Vérification de l'état actuel (après étapes 1 à 3)

Constats vérifiés :
- `app.R` charge bien les modules utilitaires et NLP/CHD-AFC via `source(...)`.
- Les fonctions extraites (NLP + CHD/AFC) ne sont plus définies dans `app.R`.
- Le fichier `app.R` est descendu à ~1356 lignes, mais reste principalement volumineux à cause du bloc `server`.

Limite de vérification dans cet environnement :
- `Rscript` n'est pas disponible, donc impossibilité de lancer un parse/exécution R/Shiny ici.

## 2) Où se situe encore le volume dans `app.R`

Le poids résiduel vient quasi entièrement de `server <- function(input, output, session)`, notamment :
- nombreux `output$... <- render...`
- 2 gros `observeEvent(...)` (`input$lancer`, `input$explor`)
- logique de rendu NER/AFC/CHD mélangée dans le même bloc

## 3) Étape 4 recommandée (très concrète)

Objectif : laisser `app.R` comme orchestration minimale et déplacer le serveur dans des sous-modules.

### 3.1 Cible de structure

```text
R/
  server_main.R
  server_outputs_status.R
  server_outputs_ner.R
  server_outputs_chd.R
  server_outputs_afc.R
  server_events_lancer.R
  server_events_explor.R
```

### 3.2 Découpage proposé

1. `server_main.R`
- contient `server <- function(input, output, session)`
- initialise `rv <- reactiveValues(...)`
- appelle les fonctions d'enregistrement :
  - `register_outputs_status(input, output, session, rv)`
  - `register_outputs_ner(input, output, session, rv)`
  - `register_outputs_chd(input, output, session, rv)`
  - `register_outputs_afc(input, output, session, rv)`
  - `register_events_lancer(input, output, session, rv)`
  - `register_events_explor(input, output, session, rv)`

2. `server_outputs_status.R`
- `logs`, `statut`, `barre_progression`, tables de statut global

3. `server_outputs_ner.R`
- `ui_ner_statut`, `table_ner_resume`, `table_ner_details`, `plot_ner_wordcloud`, sorties dynamiques par classe

4. `server_outputs_chd.R`
- `ui_chd_statut`, `plot_chd`, `ui_wordcloud`, `ui_cooc`, `table_stats_classe`

5. `server_outputs_afc.R`
- `ui_afc_statut`, `ui_afc_erreurs`, `plot_afc`, `plot_afc_classes`, `plot_afc_vars`, tables AFC

6. `server_events_lancer.R`
- le gros pipeline `observeEvent(input$lancer, ...)`

7. `server_events_explor.R`
- `observeEvent(input$explor, ...)`

## 4) Règles de migration (pour zéro régression)

- Déplacer bloc par bloc **sans changer la logique**.
- Garder les mêmes noms d'objets (`rv`, `input`, `output`, `session`).
- Commencer par les outputs simples (status), puis NER/CHD/AFC, puis events.
- Après chaque extraction : vérifier que les IDs `output$...` sont inchangés.
- Finir par remplacer dans `app.R` :
  - `source("R/server_main.R", encoding = "UTF-8")`
  - et suppression du gros bloc inline.

## 5) Gains attendus

- `app.R` passe probablement sous ~250–400 lignes.
- lecture beaucoup plus simple (point d'entrée + wiring seulement).
- maintenance plus sûre : on corrige une zone fonctionnelle sans toucher tout le serveur.
