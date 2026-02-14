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
- **ADP** : adposition (préposition)
- **ADV** : adverbe
- **AUX** : auxiliaire
- **CCONJ** : conjonction de coordination
- **DET** : déterminant
- **INTJ** : interjection
- **NOUN** : nom
- **NUM** : numéral
- **PART** : particule
- **PRON** : pronom
- **PROPN** : nom propre
- **PUNCT** : ponctuation
- **SCONJ** : conjonction de subordination
- **SYM** : symbole
- **VERB** : verbe
- **X** : autre / catégorie inconnue


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
