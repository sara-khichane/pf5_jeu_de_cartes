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

## **Gestion des registres**

Certains jeu contiennent un certain nombre de registres. Les registres sont un Parray de cartes. L'initialisation des registres se fait avec une carte défénie comme la carte de remplissage par défaut des registres : (0, Trefle). Des registres vides sont donc des registres qui ne contiennent que des cartes de types (0, Trefle). 

Il est possible d'ajouter une nouvelle carte aux registres d'un jeu si ce dernier contient une case vide. Il est également possible d'enlever une carte des registres, si elle existe en la remplaçant par la carte de remplissage par défaut.

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

## **Gestion des coups**

Un coup est un déplacement d'une carte sur une autre, c'est donc un structure composée d'une carte (à déplacer) du type carte et d'une arrivée de type carte.

#### **Validation d'un coup**

Il existe des règles de validation différentes pour chaque jeu. On vérifie d'abord avec une fonction que la carte à déplacer existe soit dans un des bouts des colonnes ou dans un des registres. Dans ce cas, la carte existe bien. 

Suite à ça, on vérifie si l'arrivée du coup est est une colonne vide, qui est représentée par une carte dont le rank est égal à 14. On vérifie donc si une colonne vide existe bien dans la partie et si c'est le cas, il est alors possible de poser la carte dans le jeu FreeCell et ne l'est pas pour les jeu Midnight et Baker. Dans le cas de Seahaven, il est uniquement possible de jouer le coup si la carte à déplacer est un Roi.

Si l'arrivée du coup a un rank de 0, ce qui est utilisé pour représenter les registres. Le coup est valide si un registre vide existe dans la partie.

Si l'arrivée est une colonne non-vide, on vérifie que la carte d'arrivée à bien un rank supérieur de exactement 1 du rank de la carte à déplacer, pour tous les jeux. Pour les jeu Freecell, il faut vérifier que les couleurs sont bien opposées, et qu'elles sont bien les mêmes pour les jeux Seahaven et Midnight.

#### **Ajout d'un coup**

Un coup est uniquement joué si ce dernier est valide, dans le cas échéant la même partie est renvoyée. Si la carte à déplacer est d'abord enlevé du registre ou du bout de colonne où elle existe. 
Elle est ensuite posée soit dans un registre soit sur un autre bout de colonne, qui peut potentiellement être une colonne vide.
