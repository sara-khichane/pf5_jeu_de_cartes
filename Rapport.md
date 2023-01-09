## **Autheurs**

|   Nom   |  Prénom  |   identifiant |  N° étudiant |
|---------|----------|---------------|--------------|
|KHICHANE |  Sara    |  @khichane    |  22208952   |
|LEROY    |  Héloïse |  @leroyh      |  22200030   |

## **Types**

Pour une bonne compréhension du projet, nous avons besoin de définir les types suivants :
- Le type game qui contient les types de jeux : Freecell, Seahaven, Midnight et Baker.
- Le type mode qui est soit un Check pour la validation d'une solution ou Search pour la recherche d'une solution.
- Le type config qui contient le jeu, la graine et le mode.
- Le type plateau qui est composé de colonnes, registre, dépôt, liste de coups joués, compteur représentant le nombre de coups joués et un score représentant le nombre de cartes placées au dépôt.
- Le type dépôt qui est une liste de cartes.
- Le type partie qui est composé d'un plateau, d'une configuration et d'une liste de plateau.

## **Gestion des colonnes**

Les colonnes sont une FArray de listes de cartes. La taille de la FAarray varie selon le type du jeu. Par exemple, pour le jeu Freecell, la taille de la FAarray est 7. Pour le jeu Seahaven, la taille de la FAarray est 5. Pour le jeu Midnight, la taille de la FAarray est 3. Pour le jeu Baker, la taille de la FAarray est 4.


## **Gestion de l'historique**

L'historique est une structure de type Set dont le type est de type plateau et sa comparaison est définie.
La comparaison entre deux plateaux de parties, s'effecture en parcourant les colonnes de ces deux plateaux simultanément. Si les deux colonnes sont égales, on passe à la colonne suivante. Si les deux colonnes ne sont pas égales, on retourne la différence des éléments différents.
Si on arrive à la fin des deux colonnes, on compare le contenu des registres des deux plateaux. On parcourt les registres du premier plateau et pour chaque carte trouvée on vérifie si elle existe dans les registre du deuxième plateau. Si les plateaux sont égaux, un zéro est renvoyé.


