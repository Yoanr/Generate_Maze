Partie génération et solveur carré  :

$- make square       
$- ./bin/square <hauteur> <largeur> <zoom>      (conseillé : hauteur,largeur = 20 et zoom = 10 | maximum : hauteur,largeur = 286 et zoom = 1)

Partie génération et solveur hexagonale  :

$- make hexa       
$- ./bin/hexa <nbr de couronne(s)> <zoom>      (conseillé : nbr de couronne(s) = 4, zoom = 10 | maximum : nbr de couronne(s) = 35, zoom = 1)

Partie interactive :

$- make play     
$- ./bin/play <hauteur> <largeur> <zoom>  (conseillé : hauteur = 5 largeur = 5 zoom = 40)

(ou $- make pour obtenir 3 executables différents)

Nettoyage :

$- make clean (nettoie les fichiers  et les exécutables générés)


-----------------------------
Instruction :
z = aller en haut
s = aller en bas
q = aller a gauche
d = aller a droite

-----------------------------
Dossier(s):
.     : contient le Makefile et le fichier README
./bin : contient les executables générés par la commande make
./src : contient les sources du projet
./doc : contient le rapport et sujet du projet

Fichier(s):

interactive.ml = main pour la partie interactive
display_user.ml = display pour la partie interactive

display.ml = contient le code donnée dans le sujet
labysquare.ml = fonctions pour un labyrinthe hexagonale
projet.ml = main pour la partie generation et solveur (carré)

display_hexa.ml = contient le code donnée avec un open pour la partie hexagonale
labyhexa.ml = fonctions pour un labyrinthe hexagonale
projet_hexa.ml = main pour la partie generation (hexagonale)

common.ml = contient les methodes communes entre un labyrinthe hexagonale et carré

Makefile = makefile du projet
README.md = ce fichier
rapport_projet_maze.pdf = rapport du projet
projet-suj.pdf = sujet du projet

-----------------------------