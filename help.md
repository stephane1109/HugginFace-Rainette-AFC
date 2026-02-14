### Stéphane Meurisse - version beta 0.2 - 12-02-2026
- <a href="https://www.codeandcortex.fr" target="_blank" rel="noopener noreferrer">codeandcortex.fr</a>

### Rainette développé par Julien Barnier

- <a href="https://github.com/juba/rainette/blob/main/vignettes/introduction_usage.Rmd" target="_blank" rel="noopener noreferrer">Doc Rainette</a>
- <a href="https://cran.r-project.org/web/packages/rainette/vignettes/introduction_usage.html" target="_blank" rel="noopener noreferrer">Vignette CRAN</a>
- <a href="https://juba.r-universe.dev/builds" target="_blank" rel="noopener noreferrer">Builds r-universe</a>

### Pourquoi vos fichiers peuvent disparaître sur Hugging Face

Sur Hugging Face Spaces, le stockage local de ce conteneur est temporaire : si le serveur redémarre, ou si la page est rechargée après une déconnexion, les fichiers générés pendant une analyse précédente peuvent ne plus être disponibles.

Conseil : télécharge l’archive ZIP des exports juste après la fin de l’analyse.

### Logique générale de l’application

Uploadez un fichier texte au format IRaMuTeQ. L’app segmente, construit un DFM, lance la CHD avec rainette, calcule les statistiques, génère un HTML surligné, puis produit des images (nuages de mots et réseaux de cooccurrences). L’onglet d’exploration (rainette_explor) permet de visualiser la CHD.

### Paramètres de l’analyse

- **segment_size** : taille des segments lors du découpage du corpus. Plus petit donne plus de segments, plus grand donne des segments plus longs.
- **k (nombre de classes)** : nombre de classes demandé pour la CHD.
- **min_segment_size** : nombre minimal de termes par segment. En effet, lors de la tokenisation et du calcul de la dtm, certaines formes (mots-outils, mots trop peu fréquents) ont été supprimées, nos segments peuvent donc varier en taille (entendue comme le nombre de termes encore présents). Avec `min_segment_size = 10`, les segments comportant moins de 10 formes sont regroupés avec le segment suivant ou précédent du même document (si possible) jusqu'à atteindre la taille minimale souhaitée.
- **min_split_members** : nombre minimal de documents pour qu'une classe soit scindée en deux à l'étape suivante de la classification. Si cette contrainte est incompatible avec k, l'application réduit automatiquement k et l'indique dans les logs.
- **dfm_trim min_docfreq** : fréquence minimale en nombre de segments pour conserver un terme dans le DFM. Plus haut enlève les termes rares.
- **max_p (p-value)** : seuil de p-value pour filtrer les termes mis en avant dans les statistiques et le surlignage HTML.
- **top_n (wordcloud)** : nombre de termes affichés dans chaque nuage de mots.
- **window (cooccurrences)** : taille de la fenêtre glissante pour calculer les cooccurrences.
- **top_feat (cooccurrences)** : nombre de termes retenus pour construire le réseau de cooccurrences.

### Classification double (rainette2)

- **Classification double** : l’application combine deux classifications rainette (res1 et res2) via rainette2, puis découpe l’arbre final avec k.

### Lemmatisation (option)

- **Lemmatisation** : si activée, le texte est lemmatisé avec Spacy... mais la lemmatisation est plus efficace avec IRAMUTEQ.

### Filtrage Morphosyntaxique
- **Tokens à conserver** : filtre les tokens conservés selon leur catégorie grammaticale (ex. NOUN, ADJ, VERB, PROPN, ADV). Si tu ne gardes que NOUN et ADJ, tu supprimes volontairement le reste (verbes, etc.), ce qui peut modifier la CHD.

### Exploration

- **Classe** : sélection de la classe pour afficher les images et la table de statistiques associées.
- **CHD (rainette_plot)** : affichage graphique de la CHD dans l’application.
- **Type** : bar (barres) ou cloud (nuage) pour l’affichage des termes par classe.
- **Statistiques** : chi2, lr, frequency, docprop selon le critère utilisé pour classer les termes.
- **Nombre de termes** : nombre de termes affichés par classe dans la visualisation.
- **Forcer les mêmes échelles** : rend les panneaux comparables entre classes en imposant une échelle commune.
- **Afficher les valeurs négatives** : inclut les termes négativement associés à une classe.
- **Taille du texte** : taille des labels dans la visualisation.
