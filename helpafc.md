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
- Si un input `afc_top_termes` est fourni et valide, il remplace 120.

### 3) Sur quoi `top_termes` filtre ?

Le filtrage est fait dans la fonction de tracé des termes :

1. on part de `termes_stats`,
2. on enlève les termes vides,
3. on trie par `frequency` décroissante,
4. on garde les `top_termes` premiers.

Donc `top_termes` filtre par **fréquence** (pas directement par p-value).

### 4) Le CSV contient-il seulement `top_termes` ?

**Non.** Le CSV `stats_termes.csv` exporte la table `rv$afc_obj$termes_stats` (jeu complet de stats AFC disponible), sans appliquer la coupe `top_termes`.

### 5) Le rapport est-il tronqué à cause de `top_termes` ?

- **Oui pour le graphe AFC des termes** (`afc_termes.png`) : il affiche au plus `top_termes` mots.
- **Non pour les CSV AFC** : `top_termes` n’y est pas appliqué.

Attention : le périmètre AFC peut déjà être réduit en amont :
- sélection des termes significatifs (`p <= max_p`) si disponible,
- plafond `max_termes = 400` lors de la construction de la table AFC.

### 6) Différence importante

- Les **positions AFC** viennent de `FactoMineR::CA`.
- Les **résidus/chi2** sont des statistiques d’association utiles pour l’interprétation.
- Le paramètre `top_termes` n’influence pas le calcul de l’AFC ; il limite seulement le nombre de labels affichés sur le graphe.
