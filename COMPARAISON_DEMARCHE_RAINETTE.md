# Comparaison de démarche : vignette rainette vs script de l'application

Ce document résume les écarts qui expliquent pourquoi les résultats peuvent diverger entre la vignette `introduction_usage` de **rainette** et ce script Shiny.

## 1) Prétraitement textuel supplémentaire (source de divergence majeure)

Le script ajoute des étapes qui ne font pas partie du flux minimal rainette :
- nettoyage regex optionnel (`nettoyage_caracteres`),
- suppression des chiffres,
- forçage en minuscules,
- découpage des apostrophes (`tokens_split("'")`),
- retrait optionnel des stopwords,
- pipeline spaCy optionnel (lemmes/POS/stopwords spaCy).

Dès qu'une de ces options est activée, le vocabulaire et les fréquences changent, donc la CHD change.

## 2) Paramètres par défaut du script différents d'un usage "vignette"

Valeurs par défaut de l'UI qui peuvent produire des résultats très différents :
- `k = 3`,
- `segment_size = 40`,
- `min_segment_size = 10`,
- `min_split_members = 10`,
- `min_docfreq = 3`.

Si ces valeurs ne sont pas strictement identiques à votre reproduction de la vignette, les classes ne seront pas les mêmes.

## 3) Filtrage implicite des segments vides

Après tokenisation et `dfm_trim`, les segments sans termes sont supprimés.

Conséquence : le corpus final classé n'est plus exactement le corpus segmenté initial.

## 4) Mode de classification (simple vs double)

Le script peut fonctionner en :
- **simple** (`rainette`),
- **double** (`rainette2`).

Si vous utilisez le mode double, la logique n'est plus celle de la démonstration simple de base.

## 5) Non-déterminisme possible

Selon l'algorithme interne et l'ordre des données, de petites variations peuvent apparaître entre exécutions/environnements (versions de packages, BLAS, etc.).

---

## Réglages recommandés pour se rapprocher d'une démarche rainette "classique"

1. Classification **simple**.
2. Désactiver : nettoyage regex, suppression chiffres, lemmatisation, filtrage morpho, retrait stopwords (quanteda + spaCy).
3. Conserver exactement les mêmes paramètres de segmentation et de seuils (`segment_size`, `min_docfreq`, `min_segment_size`, `min_split_members`, `k`) que dans votre référence.
4. Vérifier dans les logs :
   - nombre de segments après découpage,
   - dimensions DFM après trim.

Si ces deux points diffèrent, les résultats ne peuvent pas être identiques.
