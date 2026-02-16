# Proposition de segmentation de `app.R` (1930 lignes)

## Objectif
Rendre l'application plus lisible, testable et maintenable **sans changer le comportement fonctionnel**.

## Diagnostic rapide
Le fichier `app.R` mélange actuellement plusieurs responsabilités :
- chargement de dépendances et configuration globale ;
- utilitaires génériques (texte, logs, classes, html) ;
- pipeline NLP (tokenisation, filtrage spaCy, NER) ;
- préparation CHD/AFC et génération de graphes ;
- logique Shiny serveur (reactive values, observeEvent, render*).

Cette concentration augmente le coût de maintenance : chaque correction impose de naviguer dans un gros bloc monolithique.

## Cible proposée (segmentation)

### 1) Garder `app.R` comme point d'entrée minimal
`app.R` ne devrait contenir que :
1. `library(...)`
2. `source(...)` des modules
3. initialisation `ui` + `server`
4. `shinyApp(ui, server)`

### 2) Créer des modules sous `R/`

```text
R/
  app_bootstrap.R          # options(), aide/help.md, chargements init
  utils_logging.R          # horodater(), ajouter_log(), statut/progression
  utils_text.R             # normaliser_classes(), regex_echapper(), surlignage
  nlp_language.R           # estimer_langue_corpus(), configurer_langue_spacy()
  nlp_spacy.R              # executer_spacy_filtrage(), executer_spacy_ner()
  chd_pipeline.R           # verifier_dfm..., calculer_k_effectif(), CHD
  afc_pipeline.R           # fonctions AFC, tables, projections
  visualization.R          # wordcloud, cooc, rendu CHD image/html
  server_main.R            # server <- function(input, output, session)
```

> Variante plus “Shiny modules”: remplacer certaines zones `output$...` par des modules (`mod_ner_server`, `mod_afc_server`, etc.). C'est utile mais pas obligatoire pour une première passe.

## Découpage concret recommandé (ordre de migration)

### Étape 1 — Extraction sûre des utilitaires (faible risque)
Déplacer depuis `app.R` vers `R/utils_*.R` :
- `compter_tokens`
- `md5_fichier`
- `horodater`
- `ajouter_log`
- `normaliser_classes`
- `regex_echapper`
- `surligner_terme_segment`
- `generer_table_html_afc_mots`

**Bénéfice:** baisse immédiate de taille et meilleure lisibilité sans impact runtime.

### Étape 2 — Extraction NLP
Déplacer :
- détection de langue (`estimer_langue_corpus`, cohérence dictionnaire/langue)
- configuration modèle spaCy
- exécution filtrage POS
- extraction NER

**Bénéfice:** isoler les parties dépendantes de Python/spaCy pour faciliter debug.

### Étape 3 — Extraction CHD/AFC
Déplacer :
- contrôles DFM/docvars
- calcul `k` effectif
- génération CHD (png/html)
- graphes d'adjacence/cooccurrence

**Bénéfice:** clarifier la frontière “analyse” vs “présentation Shiny”.

### Étape 4 — Allègement du `server`
Créer `R/server_main.R` puis extraire des blocs thématiques :
- `register_outputs_status()`
- `register_outputs_ner()`
- `register_outputs_afc()`
- `register_outputs_chd()`
- `register_events_main()` (`input$lancer`, `input$explor`)

**Bénéfice:** lecture top-down du serveur en quelques fonctions nommées.

## Règles de migration (pour éviter les régressions)
1. **Refactor sans changement de logique** (copier-coller puis source).
2. Après chaque extraction : démarrage Shiny + test d'un corpus minimal.
3. Conserver les signatures de fonctions (arguments/retours) au début.
4. Journaliser les erreurs dans `rv$logs` au même format.
5. Ne passer aux modules Shiny qu'après stabilisation du découpage fonctionnel.

## Proposition de jalon court (1 itération)
- Itération 1 (rapide, 1 PR): créer `R/utils_logging.R`, `R/utils_text.R`, `R/nlp_language.R` + simplifier `app.R`.
- Itération 2: isoler spaCy + NER.
- Itération 3: isoler CHD/AFC + rendre `server_main.R` lisible.

## Résultat attendu
- `app.R` descend potentiellement sous 250–350 lignes.
- Le reste est réparti en modules spécialisés, chacun testable et compréhensible indépendamment.
