#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

// n désigne l'ordre du sudoku

// Renvoie "X_k_i_j"
char* variable(int k, int i, int j);

// Exprime la contrainte d'au moins un chiffre dans la case (i,j)
char* contrainte_une_case (int i, int j, int n);

// Exprime la contrainte d'au moins un chiffre dans chaque case
char* contrainte_toutes_cases(int n);

// Exprime la contrainte d'exactement une fois le chiffre k sur la ligne i
char* contrainte_une_ligne_un_chiffre(int k, int i, int n);

// Exprime la contrainte d'exactement une fois chaque chiffre sur chaque ligne
char* contrainte_toutes_lignes(int n);

// Exprime la contrainte d'au moins une fois le chiffre k sur la colonne i
char* contrainte_une_colonne_un_chiffre(int k, int i, int n);

// Exprime la contrainte d'au moins une fois chaque chiffre sur chaque colonne
char* contrainte_toutes_colonnes(int n);

// Exprime la contrainte d'au moins une fois k dans le carré (i,i)
char* contrainte_un_carre_un_chiffre(int k, int i, int j, int n);

// Exprime la contrainte d'au moins une fois k dans chaque carré
char* contrainte_tous_carres_un_chiffre(int k, int n);

// Exprime la contrainte d'un moins une fois chaque chiffre dans chaque carré
char* contrainte_tous_carres(int n);

// Génère la formule exprimant les cases déjà remplies au début
char* conditions_initiales(char* in);

//génère la formule modélisant le problème du sudoku de taille n dans un fichier appelé filename, avec p cases initialement remplies
void gen_formule_n_sudoku(int n, char* filename, int p);
