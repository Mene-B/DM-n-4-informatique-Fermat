
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

// Renvoie la chaîne de charactères "X_[i]_[j]"
char* variable(int i, int j);

// Renvoie la formule exprimant la contrainte de l'unique dame sur la i-ième ligne 
char* contrainte_une_ligne(int i, int n);

// Renvoie la formule exprimant la contrainte sur toutes les lignes.
char* contrainte_toutes_lignes(int n);

// Renvoie la formule exprimant la contrainte de l'unique dame sur la i-ième ligne 
char* contrainte_une_colonne(int j, int n);

// Renvoie la formule exprimant la contrainte sur toutes les colonnes.
char* contrainte_toutes_colonnes(int n);

// Renvoie la formule exprimant la contrainte sur les diagonales avec une pente positive --> "/"
char* contrainte_diag_positives(int n);

// Renvoie la formule exprimant la contrainte sur les diagonales avec une pente négative --> "\"
char* contrainte_diag_negatives(int n);

// Renvoie la formule exprimant la contrainte sur toutes les diagonales.
char* contrainte_toutes_diagonales(int n);


//génère la formule modélisant le problème des n dames dans le fichier filename.
void gen_formule_n_dames(int n, char* filename);
