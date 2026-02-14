# Analyse morphosyntaxique avec spaCy dans l'application Rainette-AFC

## À propos de la documentation spaCy en français

Il n’existe pas (à ma connaissance) de documentation officielle spaCy entièrement traduite en français. La documentation de référence est en anglais.

Dans l’application, on peut néanmoins s’appuyer sur les catégories **POS** standard de spaCy (Universal POS) et sur leur traduction française pour paramétrer le filtrage morphosyntaxique de manière fiable.

## Liens utiles spaCy

- Documentation principale : <https://spacy.io/usage>
- API Token (`token.pos_`, `token.tag_`) : <https://spacy.io/api/token>
- Linguistic Features (POS, morphology) : <https://spacy.io/usage/linguistic-features>

## Traduction FR des POS (spaCy / Universal POS)

- **ADJ** : adjectif
- **ADP** : adposition (préposition / postposition)
- **ADV** : adverbe
- **AUX** : auxiliaire
- **CCONJ** : conjonction de coordination
- **DET** : déterminant
- **INTJ** : interjection
- **NOUN** : nom commun
- **NUM** : numéral
- **PART** : particule
- **PRON** : pronom
- **PROPN** : nom propre
- **PUNCT** : ponctuation
- **SCONJ** : conjonction de subordination
- **SYM** : symbole
- **VERB** : verbe
- **X** : autre / catégorie inconnue

## Comment l’analyse morphosyntaxique fonctionne dans l’application

Dans Rainette-AFC, le prétraitement spaCy est réalisé par le script `spacy_preprocess.py` avec le modèle français `fr_core_news_md`.

### 1) Chargement du modèle et options

Le script accepte notamment :

- `--modele` : modèle spaCy (par défaut `fr_core_news_md`)
- `--pos_keep` : liste des POS à conserver, séparées par des virgules (ex. `NOUN,ADJ,VERB`)
- `--lemmes` : `1` pour utiliser les lemmes (`token.lemma_`), sinon la forme de surface
- `--lower_input` : `1` pour forcer le texte en minuscules avant traitement
- `--output_tokens` : export optionnel d’un TSV détaillant `doc_id`, `token`, `lemma`, `pos`

### 2) Filtrage des tokens

Pour chaque token, le script :

1. élimine les espaces, la ponctuation et les nombres,
2. lit l’étiquette POS via `tok.pos_`,
3. applique le filtre POS si `pos_keep` est renseigné,
4. conserve soit le lemme soit la forme token selon l’option,
5. met en minuscules la forme retenue,
6. reconstruit un texte nettoyé par document.

### 3) Résultat produit

Le script génère :

- un TSV de sortie (`doc_id`, `text`) contenant le texte reconstruit après filtrage,
- éventuellement un TSV détaillé des tokens (utile pour audit linguistique).

## Paramétrage côté interface (Shiny)

Dans l’interface, la section **Paramétrages SpaCy** permet :

- d’activer le **filtrage morphosyntaxique (spaCy)**,
- de sélectionner les POS à conserver parmi la liste Universal POS,
- de combiner ce filtrage avec la lemmatisation selon les besoins analytiques.

Par défaut, un réglage fréquent pour des analyses lexicales (thèmes, AFC, cooccurrences) est de conserver surtout les catégories porteuses de sens, par exemple `NOUN`, `VERB`, `ADJ`, éventuellement `PROPN`.

## Conseils pratiques

- Pour une analyse thématique : commencer par `NOUN,VERB,ADJ`.
- Pour préserver les noms d’organisations/personnes : ajouter `PROPN`.
- Pour éviter le bruit grammatical : exclure en général `DET`, `PRON`, `CCONJ`, `SCONJ`, `PART`.
- Toujours vérifier un échantillon de tokens exportés (`--output_tokens`) avant de figer les réglages.
