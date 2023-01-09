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


