Le rapport se compose de 5 parties principales :
1- Auteurs
2- Organisation du travail
3- Solution
4- Perspectives
5- Exécution


# **1. Auteurs**

|   Nom   |  Prénom  |   identifiant |  N° étudiant |
|---------|----------|---------------|--------------|
|KHICHANE |  Sara    |  @khichane    |  22208952   |
|LEROY    |  Héloise |  @leroyh|  22200030   |


# **2. Organisation du travail**

Le travail pour ce projet a commencé depuis la création de notre git.
Nous avons pris soin de faire plusieurs réunions pour bien comprendre le sujet, schématiser la solution et se mettre d'accord sur la manière de procéder pour chaque tâche.

La répartition des tâches fut la suivante :

KHICHANE Sara
- Création de la permutation
- Gestion des registres
- Gestion des coups
- Validation de la solution
- Recherche de coups
- Recherche de solution

LEROY Héloise
- Gestion des colonnes
- Gestion de l'historique
- Gestion de la mise au dépôt
- Initialisation de la partie
- Recherche de coups
- Recherche de solution

Chaque partie sera détaillée dans la suite du rapport.

# **3. Solution**
## **3.1. Types**

Pour une bonne compréhension du projet, nous avons besoin de définir les types suivants :
- Le type game qui contient les types de jeux : Freecell, Seahaven, Midnight et Baker.
- Le type mode qui est soit un Check pour la validation d'une solution ou Search pour la recherche d'une solution.
- Le type config qui contient le jeu, la graine et le mode.
- Le type plateau qui est composé de colonnes, registre, dépôt, liste de coups joués, compteur représentant le nombre de coups joués et un score représentant le nombre de cartes placées au dépôt.
- Le type dépôt qui est une liste de cartes.
- Le type partie qui est composé d'un plateau, d'une configuration et d'une liste de plateau.

## **3.2. Gestion des colonnes**

Les colonnes sont une FArray de listes de cartes. La taille de la FAarray varie selon le type du jeu. Par exemple, pour le jeu Freecell, la taille de la FAarray est 8. Pour le jeu Seahaven, la taille de la FAarray est 10. Pour le jeu Midnight, la taille de la FAarray est 18. Pour le jeu Baker, la taille de la FAarray est 13.

## **3.3. Gestion des registres**

Certains jeu contiennent un certain nombre de registres. Les registres sont un Parray de cartes. L'initialisation des registres se fait avec une carte défénie comme la carte de remplissage par défaut des registres : (0, Trefle). Des registres vides sont donc des registres qui ne contiennent que des cartes de types (0, Trefle). 

Il est possible d'ajouter une nouvelle carte aux registres d'un jeu si ce dernier contient une case vide. Il est également possible d'enlever une carte des registres, si elle existe en la remplaçant par la carte de remplissage par défaut.

## **3.4. Gestion de l'historique**

L'historique est une structure de type Set dont le type est de type plateau et sa comparaison est définie.
La comparaison entre deux plateaux de parties, s'effecture en parcourant les colonnes de ces deux plateaux simultanément. Si les deux colonnes sont égales, on passe à la colonne suivante. Si les deux colonnes ne sont pas égales, on retourne la différence des éléments différents.
Si on arrive à la fin des deux colonnes, on compare le contenu des registres des deux plateaux. On parcourt les registres du premier plateau et pour chaque carte trouvée on vérifie si elle existe dans les registre du deuxième plateau. Si les plateaux sont égaux, un zéro est renvoyé.

## **3.5. Gestion de la mise au dépôt**

#### **3.5.1. Déroulement**

La mise au dépôt se déroule en utilisant deux fonctions qui dépendent l'une de l'autre : la mise au dépôt au niveau des colonnes et la mise au dépôt au niveau des registres.

D'abord, on cherche les colonnes dont le bout contient des cartes qui peuvent être mise au dépôt. Si une carte trouvée, on la rajoute au dépot et on reprend la recherche à partir de la colonne 0.

Ce processus peut se répéter plusiseurs fois et c'est uniquement quand on arrive à la fin de la recherche des colonnes, qu'on commence la cherche au niveaux des registres en appelant la fonction de la lise au dépôt des registres.

Si une carte est trouvée, on la rajoute au dépot et on relance une mise au dépôt à partir des colonnes pour cette nouvelle partie générée, et cela en applelant la première fonction de mise au dépôt (dépendence mutuelle).

Si aucune carte n'est trouvée ni dans les colonnes ni dans les registres, on arrive à la fin de la mise au dépôt et on renvoie la partie.

#### **3.5.2. Vérification de la mise d'une carte**
Pour tester si une carte est trouvée, on utilise la fonction qui vérifie si la carte peut être mise au dépôt, qui vérifie s'il existe un bout de colonne dont le rank est exactement inférieure de 1 de la carte et le suit est le même.

#### **3.5.3. Ajout d'une carte au dépôt**

On note que dans une mise à jour du dépôt, les cartes trouvées sont deplacées, c'est à dire si la carte destinée à être mise au dépôt provient des colonnes elle est enlevée de sa colonne ou éventuellement des registres et ensuite mise au dépôt. Le score de la partie augmente donc de 1. La nouvelle partie conserve la même liste de coups, le même compteur de coups et la même historique.

## **3.5. Gestion des coups**

Un coup est un déplacement d'une carte sur une autre, c'est donc un structure composée d'une carte (à déplacer) du type carte et d'une arrivée de type carte.

#### **3.5.1. Validation d'un coup**

Il existe des règles de validation différentes pour chaque jeu. On vérifie d'abord avec une fonction que la carte à déplacer existe soit dans un des bouts des colonnes ou dans un des registres. Dans ce cas, la carte existe bien. 

Suite à ça, on vérifie si l'arrivée du coup est est une colonne vide, qui est représentée par une carte dont le rank est égal à 14. On vérifie donc si une colonne vide existe bien dans la partie et si c'est le cas, il est alors possible de poser la carte dans le jeu FreeCell et ne l'est pas pour les jeu Midnight et Baker. Dans le cas de Seahaven, il est uniquement possible de jouer le coup si la carte à déplacer est un Roi.

Si l'arrivée du coup a un rank de 0, ce qui est utilisé pour représenter les registres. Le coup est valide si un registre vide existe dans la partie.

Si l'arrivée est une colonne non-vide, on vérifie que la carte d'arrivée à bien un rank supérieur de exactement 1 du rank de la carte à déplacer, pour tous les jeux. Pour les jeu Freecell, il faut vérifier que les couleurs sont bien opposées, et qu'elles sont bien les mêmes pour les jeux Seahaven et Midnight.

#### **3.5.2. Ajout d'un coup**

Un coup est uniquement joué si ce dernier est valide, dans le cas échéant la même partie est renvoyée. Si la carte à déplacer est d'abord enlevé du registre ou du bout de colonne où elle existe. 
Elle est ensuite posée soit dans un registre soit sur un autre bout de colonne, qui peut potentiellement être une colonne vide.



## **3.6. Initialisation de la partie**

Pour créer une partie, il faut initialiser son plateau, c'est-à-dire remplir ses colonnes. Cela se fait à partir de la permutation, qui est une liste de cartes créée à partir de la graine. 

#### **3.6.1. Fragmentations des listes**

Pour cela, on crée une liste de cartes à partir de la permutation. On parcourt cette liste de cartes en la fragmentant en plusieurs listes. La taille de ces listes dépend du jeu, par exemple pour le jeu Freecell, la taille de ces listes est 7, c'est-à-dire qu'il y a 7 cartes dans la colonne.

Ces listes sont créées en leur ajoutant successivement les cartes de la liste de permutation de la première à la dernière. Les cartes sont rajoutées au fur et à mesure sur le début de chaque petite liste. C'est à dire que les cartes apparaîtront à l'ordre inverse de leur placement dans la liste de permutation.

#### **3.6.2. Initialisation de la partie**

Une fois les listes créées, on les ajoute dans la FAarray de colonnes, en parcourant le FArray et pour chaque colonne, la liste correspondante est parcourue et ses éléments sont copiés dans la colonne. 

On initialise la liste des coups joués est initialisée à vide, le compteur de coups à 0 et le score à 0. On initialise les registres avec la carte de remplissage par défaut. Pour le Seahaven deux cartes de la liste principale restent non placées dans les colonnes. Ces dernières sont donc ajoutées aux registres. Enfin, on ajoute le plateau à l'historique.

C'est ainsi que la partie initiale est créée.

## **3.7. Validation d'une solution**

#### **3.7.1. Transformation d'un fichier solution en une liste de coups**

Le fichier solution est lu en utilisant la fonction open_in, son contenu est récupéré via la fonction input_line.
Un fichier solution est composé d'une liste de coups, chaque coup étant composé de deux colonnes séparées par un espace. On utilise donc la fonction split pour séparer les deux colonnes et on obtient une liste de deux éléments. Les cartes sont sous leur forme numérique. 
Une carte de type V qui représente la colonne vide est représentée par le numéro 52, et une carte de type T qui représente un registre est représentée par le numéro 53.
On obtient donc deux listes différentes, celle des coups et celle des arrivées.
Les cartes de chacune de ces deux listes sont ensuite transformées en cartes de type carte, en utilisant la fonction of_num. Pour les cartes numéro 52 et 53, elles sont transformées en carte de type (14, Trefle) et (0, Trefle) respectivement.
On joint ces deux listes pour obtenir la liste des coups à jouer.

#### **3.7.2. Déroulement du jeu**

Une partie est d'abord initialisée en suivant la permutation de la graine. On obtient ainsi une liste de cartes mélangées. On utilise ensuite la fonction de création de partie pour créer une partie à partir de cette liste de cartes. On obtient ainsi une partie initialisée.
On utilise ensuite la fonction de mise au dépôt pour mettre au dépôt toutes les cartes de la partie. On obtient ainsi une partie avec un dépôt mis à jour.
On vérifie si le fichier solution est vide, si c'est le cas on finit le déroulement sur un échec suivi d'un exit 1.
On déroule ensuite la solution donnée. Cela est effectué en parcourant la liste des coups à jouer. Pour chaque coup, on vérifie que le coup est valide, et si c'est le cas, on joue le coup. 
Ce déroulement s'arrête si on tombe sur un coup non valide. On print échec et on exit 1 dans ce cas. Sinon, on déroule jusqu'à la fin de la liste des coups à jouer. 
On fait une dernière mise au dépôt et on vérifie si la solution a été bonne en vérifiant si le dépôt est rempli de carte dont le rank est 13. Si c'est le cas, on print succès et on exit 0. Sinon, on print échec et on exit 1.

## **3.8. Recherche de coups**

### **3.8.1. Recherche de coups des registres vers colonnes**

On commence par parcourir les registres et pour chaque registre on parcourt toutes les colonnes. On vérifie si la colonne est vide. Si c'est le cas, on rajoute un coup à la liste des coups en mettant la carte du registre au départ et la colonne vide comme arrivée du coup, la colonne vide est reprsentée par la carte fictive (14, Trefle).

Si la colonne n'est pas vide, on s'intéresse au bout de la colonne et on utilise la fonction du coup valide avec en entrée au départ la carte du registre et en arrivée la carte au bout de la colonne. Si le coup est valide, on le rajoute à la liste des coups.

### **3.8.2. Recherche de coups des colonnes vers registres**

On commence par parcourir les colonnes non vides, en s'intéressant au bout des colonnes. On vérifie si un des registres est vide. 
Si c'est le cas, on rajoute un coup à la liste des coups en mettant la carte du bout de la colonne au départ et le registre vide comme arrivée du coup, le registre vide est reprsenté par la carte fictive (0, Trefle).

### **3.8.3. Recherche de coups dans les colonnes**

On commence par parcourir les colonnes, et pour chaque colonne on parcourt encore une fois toutes les colonnes. 
A chaque fois, soit la colonne est vide, ce qui est représentée par la carte (14, Trefle), ou non-vide et donc son bout est pris en compte. On vérifie si le coup est valide, et si c'est le cas, on le rajoute à la liste des coups.

La liste des coups complètes est l'union des listes de coups produite à chaque types de recherche de coups.

## **3.9. Recherche de la solution**

#### **3.9.1. Optimisation de la liste de coups**

Certains coups sont considérés inutiles, c'est-à-dire qui mènent vers des états insolubles. Il serait donc plus judicieux de les supprimer pour faciliter la recherche. On applique plusieurs optimisations sur la liste de coups créée :

- On supprime les doublons que contient la liste. Par exemple, si on a plusieurs colonnes vides, pas besoin de considérer le déplacement d'une carte vers toutes ces colonnes vides, le faire vers une d'entre elles suffira.

- On supprime les coups dont le départ est une colonne qui contient un seul élément et l'arrivée est une colonne vide. On utilise une fonction qui va parcourir les bouts de colonnes pour trouver la carte du départ du coup- et vérifier que la taille de la colonne est bien 1.

#### **3.9.2. Déroulement de la recherche**

La recherche se fait en suivant la logique d'un parcours en profondeur d'un graphe dont les sommets sont les différents états que peut prendre une partie. 

On utilise une fonction récursive qui prend en entrée une partie et une liste de coups. Cette fonction va parcourir la liste des coups et pour chaque coup, on joue le coup dans une partie temporaire et on vérifie si cette partie n'existe pas dans l'historique de la partie d'entrée. 
Si c'est le cas, on appelle la recherche sur la partie temporaire.
Le cas échéant, on rappelle la fonction récursive avec la partie d'entrée et la liste des coups sans le coup qui allait être joué.

Le parcours de la liste de coups se fait par ordre de score. On utilise une fonction qui va choisir le prochain coup à jouer, qui est le coup qui va donner le meilleur score (indicateur du nombre de cartes posées dans le dépôt).

Au début de la fonction recherche, il est important de faire la mise en dépôt de la partie entrée et de vérifier si cette partie est gagnante, c'est-à-dire si le dépôt est rempli de cartes dont le rank est 13.

Au début de la fonction récursive, on vérifie si la liste de coups est vide, c'est-à-dire qu'il n'y a plus aucun coup possible à jouer. On vérifie ensuite si la partie est gagnante. Si c'est le cas, on print la solution et on exit 0. Sinon, on print insoluble et on exit 2.

#### **3.9.3. Génération du fichier solution**

La génération du fichier solution se fait en parcourant la liste des coups joués à chaque étape, qu'on trouve dans le plateau de la partie gagnante.

La liste de coups est parcourue, et à chaque coup, on utilise la fonction to_num pour transformer le départ du coup et son arrivée, de la notation rank-suit à la notation numérique. On print ces deux numéros séparés par un espace sur le fichier solution, avec la fonction output_string. 

Dans le cas des cartes (0, Trefle), et (14, Trefle), on print T ou V respectivement à la place du numéro de la carte.

# **4. Perspectives**

Il existe quelques améliorations possibles qui ont été envisagées :

- Pour la recherche aléatoire de parties de FreeCell ou Baker's Dozen qui ne seraient pas solubles dans le cadre de ce projet. On crée une fonction "depot to partie" qui met une carte du dépot vers la partie, au lieu de faire une mise au dépot automatique, et on aurait une fonction "recherche_coup_depot_partie" qui ajoute à la liste de coups les coups possibles du dépot vers la partie, ce qui enrichit fortement la liste de coups possibles.

- Pour la remontée dans le MidnightOil, on utilise dans la partie, une variable "remontee" : donne le nombre de remontée restantes (1 si on peut, 0 si deja utilisee).
    - si remontee = 1, ajout d'une fonction "remontee" qui prend une colonne et qui met en queue de celle-ci sa carte en tête (une colonne est une liste donc fonction sur une liste).
    - on ajoute dans la liste des coups : une remontée de chaque colonne si la longueur de celle-ci >=2.
- Pour les redistributions dans le MidnightOil, on ajoute un champ" redistributions" = 2 dans la partie, et on peut faire une redistribution si ce champ != 0. On crée une fonction "partie_to_liste_carte" qui ajoute les cartes dans l'ordre de la partie, puis la randomise avec shuffle. On rappelle notre fonction pour créer un plateau avec cette liste de permutation, et on continue avec notre nouveau plateau. on pourra ajouter un champ "A A" dans la liste de coup qui indique une redistribution.
Cette extension ajoute beaucoup d'états à visiter. Elle est envisageable si le jeu est bloqué à un moment.

# **5. Exécution**

Exemple d'une validation de solution :

`./run jeu.graine -check 'chemin_du_fichier_sol_à_vérifier'`

Exemple d'une génération de solution :

`./run jeu.graine -search 'chemin_du_fichier_sol_à_remplir'`
