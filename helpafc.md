## Aide AFC : calcul, affichage des termes et rôle de `top_termes`

### 1) Comment l’AFC est calculée dans le script

L’AFC classes × termes est calculée en 3 étapes :

1. Construction de la table de contingence **Classes × Termes** depuis le DFM.
   - On garde les segments avec classe non vide.
   - On peut filtrer sur une liste de termes cibles.
   - On limite éventuellement à `max_termes`.
2. Exécution de l’AFC avec `FactoMineR::CA(tab, graph = FALSE)`.
3. Récupération des coordonnées des classes (`rowcoord`) et des termes (`colcoord`) pour le tracé.

### 2) Qu’est-ce que `top_termes` ?

`top_termes` est **une limite d’affichage graphique** des mots sur le plan AFC (classes + termes).

- Par défaut : `top_termes = 120`.
- Si un input `afc_top_termes` est fourni et valide, il remplace 120 (à developper)

### 3) Sur quoi `top_termes` filtre ?

Le filtrage est fait dans la fonction de tracé des termes :

1. on part de `termes_stats`,
2. on enlève les termes vides,
3. on trie par `frequency` décroissante,
4. on garde les `top_termes` premiers.

Pourquoi top_termes est en fréquence et pas en p-value ?

Parce que top_termes est une contrainte de lisibilité graphique
Le rôle de top_termes est de limiter le nombre de labels affichés dans tracer_afc_classes_termes(...), sinon le plot devient illisible (chevauchements, bruit). 
Le code trie les termes par frequency puis coupe à top_termes.
En amont, dans le pipeline serveur, on construit termes_signif avec p <= input$max_p, puis on passe ces termes à executer_afc_classes(..., termes_cibles = termes_signif, ...). Donc la p-value a déjà réduit le périmètre des termes entrants AFC.

### 4) Le CSV contient-il seulement `top_termes` ?

**Non.** Le CSV `stats_termes.csv` exporte la table `rv$afc_obj$termes_stats` (jeu complet de stats AFC disponible), sans appliquer la coupe `top_termes`.

### 5) Le rapport est-il tronqué à cause de `top_termes` ?

- **Oui pour le graphe AFC des termes** (`afc_termes.png`) : il affiche au plus `top_termes` mots.
- **Non pour les CSV AFC** : `top_termes` n’y est pas appliqué.

Attention : le périmètre AFC peut déjà être réduit en amont :
- sélection des termes significatifs (`p <= max_p`) si disponible,
- plafond `max_termes = 400` lors de la construction de la table AFC.

Est-ce que le CSV contient top_termes ?

Non.
Le CSV stats_termes.csv exporte rv$afc_obj$termes_stats complet (dans le périmètre de l’AFC calculée), sans appliquer top_termes

### 6) Différence importante

- Les **positions AFC** viennent de `FactoMineR::CA`.
- Les **résidus/chi2** sont des statistiques d’association utiles pour l’interprétation.
- Le paramètre `top_termes` n’influence pas le calcul de l’AFC ; il limite seulement le nombre de labels affichés sur le graphe.

### Comment le résidu de Pearson est calculé

Pour chaque mot et chaque classe, le script fait ceci :

- Il regarde combien de fois le mot apparaît réellement dans cette classe.
- Il calcule ensuite combien on aurait “normalement” attendu pour ce mot dans cette classe, si la répartition était neutre.
- Il compare les deux : réel vs attendu, et transforme cet écart en une valeur standardisée (le résidu).

Cette valeur est calculée cellule par cellule dans la table Classes × Termes.

Dans ce projet, le résidu ne sert pas pas à recalculer l’AFC.

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


