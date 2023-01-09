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

## **Gestion de la mise au dépôt**

#### **Déroulement**

La mise au dépôt se déroule en utilisant deux fonctions qui dépendent l'une de l'autre : la mise au dépôt au niveau des colonnes et la mise au dépôt au niveau des registres.

D'abord, on cherche les colonnes dont le bout contient des cartes qui peuvent être mise au dépôt. Si une carte trouvée, on la rajoute au dépot et on reprend la recherche à partir de la colonne 0.

Ce processus peut se répéter plusiseurs fois et c'est uniquement quand on arrive à la fin de la recherche des colonnes, qu'on commence la cherche au niveaux des registres en appelant la fonction de la lise au dépôt des registres.

Si une carte est trouvée, on la rajoute au dépot et on relance une mise au dépôt à partir des colonnes pour cette nouvelle partie générée, et cela en applelant la première fonction de mise au dépôt (dépendence mutuelle).

Si aucune carte n'est trouvée ni dans les colonnes ni dans les registres, on arrive à la fin de la mise au dépôt et on renvoie la partie.

#### **Vérification de la mise d'une carte**
Pour tester si une carte est trouvée, on utilise la fonction qui vérifie si la carte peut être mise au dépôt, qui vérifie s'il existe un bout de colonne dont le rank est exactement inférieure de 1 de la carte et le suit est le même.

#### **Ajout d'une carte au dépôt**

On note que dans une mise à jour du dépôt, les cartes trouvées sont deplacées, c'est à dire si la carte destinée à être mise au dépôt provient des colonnes elle est enlevée de sa colonne ou éventuellement des registres et ensuite mise au dépôt. Le score de la partie augmente donc de 1. La nouvelle partie conserve la même liste de coups, le même compteur de coups et la même historique.

