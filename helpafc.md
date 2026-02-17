## Aide AFC : calcul, affichage des termes et rôle de `top_termes`

### 1) Comment l’AFC est calculée dans le script

L’AFC classes × termes est calculée en 3 étapes :

1. Construction de la table de contingence **Classes × Termes** depuis le DFM.
2. Exécution de l’AFC avec `FactoMineR::CA(tab, graph = FALSE)`.
3. Récupération des coordonnées des classes (`rowcoord`) et des termes (`colcoord`) pour le tracé.

### 2) Qu’est-ce que `top_termes` ?

`top_termes` est **une limite d’affichage graphique** des mots sur le plan AFC.

- Par défaut : `top_termes = 120`.
- A développer (option) : Si un input `afc_top_termes` est fourni, il remplace 120

### 3) Sur quoi `top_termes` filtre ?

Le filtrage est fait dans la fonction de tracé des termes :

1. on part de `termes_stats`,
2. on enlève les termes vides,
3. on trie par `frequency` décroissante,
4. on garde les `top_termes` premiers.

Pourquoi top_termes est en fréquence et pas en p-value ?

Parce que top_termes est une contrainte de lisibilité graphique.
Le rôle de top_termes est de limiter le nombre de labels affichés dans `tracer_afc_classes_termes`, sinon le plot devient illisible (chevauchements). 
Le code trie les termes par `frequency` puis coupe à top_termes.
En amont, dans le pipeline serveur, on construit termes_signif avec `p <= input$max_p`, puis on passe ces termes à `executer_afc_classes`, `termes_cibles` = termes_significatifs). Donc la p-value réduit le périmètre des termes entrants AFC.

### 4) Le CSV contient-il seulement `top_termes` ?

**Non.** Le CSV `stats_termes.csv` exporte la table `rv$afc_obj$termes_stats` (jeu complet de stats AFC disponible), sans appliquer la réduction `top_termes`.

Le rapport est-il tronqué à cause de `top_termes` ?

- **Oui pour le graphe AFC des termes** (`afc_termes.png`) : il affiche au plus `top_termes` mots.
- **Non pour les CSV AFC** : `top_termes` n’y est pas appliqué.

### 5) Note sur l'afc chi2/résidu de Pearson

- Les **positions AFC** viennent de `FactoMineR::CA`.
- Les **résidus/chi2** sont des statistiques d’association utilisés pour l’interprétation.

#### Comment le résidu de Pearson est calculé

Pour chaque mot et chaque classe :

- On regarde combien de fois le mot apparaît réellement dans cette classe.
- On calcule ensuite combien on aurait “normalement” attendu pour ce mot dans cette classe, si la répartition était neutre.
- On compare les deux : réel vs attendu, et transforme cet écart en une valeur standardisée (le résidu).

Cette valeur est calculée cellule par cellule dans la table Classes × Termes.

Le résidu ne sert pas pas à recalculer l’AFC.

- le code extrait, pour chaque mot, la classe où la surreprésentation relative est la plus forte (Classe_max_resid) et sa valeur (resid_max).
- en parallèle, il garde aussi Classe_max comme classe la plus fréquente en brut (volume observé), ce qui est un autre indicateur.
- la doc du projet dit explicitement cette différence :
   - Classe_max = volume,
   - Classe_max_resid = surreprésentation relative.
 
Résidu positif / résidu négatif

Pour chaque mot dans chaque classe, on compare :

- ce qu’on voit réellement dans la classe,
- ce qu’on aurait attendu si le mot était réparti “normalement”.

Résidu positif => le mot est plus présent que prévu dans cette classe (surreprésenté).
Résidu négatif => le mot est moins présent que prévu dans cette classe (sous-représenté).
Résidu proche de 0 => présence “normale”, pas d’écart fort.


