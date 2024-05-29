#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "utils.h"


// Renvoie la chaîne de charactères "X_[i]_[j]"
char* variable(int i, int j){
    // Variables pour stocker les tailles des nombres i et j en base 10
    int taille_i = 0;
    int taille_j = 0;
    // Stock les valeurs de i et j pour pouvoir les modifier
    int I = i;
    int J = j;

    if(i == 0){
        taille_i = 1;
    } else while(I != 0){
        I = (int) (I/10);
        taille_i++;
    }
    if(j == 0){
        taille_j = 1;
    } else while(J != 0){
        J = (int) (J/10);
        taille_j++;
    }

    char* str = malloc((3+taille_i+taille_j)*sizeof(char));
    sprintf(str, "X_%d_%d", i, j);
    return str; 
}

// Renvoie la formule exprimant la contrainte de l'unique dame sur la i-ième ligne 
char* contrainte_une_ligne(int i, int n){
    // On va stocker toutes les variables de la forme "X_i_j" pour j entre 0 et n-1
    char** tab = malloc(n*sizeof(char*));
    for(int j = 0; j < n; j++){
        tab[j] = variable(i, j);
    }
    char* au_moins = au_moins_une(tab, n);
    char* au_plus = au_plus_une(tab, n);
    
    char* res = malloc((3+strlen(au_moins)+strlen(au_plus))*sizeof(char));
    sprintf(res, "(%s & %s)", au_moins, au_plus);
    free(au_moins);
    free(au_plus);

    return res;
}

char* contrainte_toutes_lignes(int n){
    char** tab = malloc(n*sizeof(char*));
    for(int i = 0; i < n; i++){
        tab[i] = contrainte_une_ligne(i,n);
    }
    return toutes_vraies(tab, n);
}

// Renvoie la formule exprimant la contrainte de l'unique dame sur la i-ième ligne 
char* contrainte_une_colonne(int j, int n){
    // On va stocker toutes les variables de la forme "X_i_j" pour j entre 0 et n-1
    char** tab = malloc(n*sizeof(char*));
    for(int i = 0; i < n; i++){
        tab[i] = variable(i, j);
    }
    char* au_moins = au_moins_une(tab, n);

    return au_moins;
}

char* contrainte_toutes_colonnes(int n){
    char** tab = malloc(n*sizeof(char*));
    for(int i = 0; i < n; i++){
        tab[i] = contrainte_une_colonne(i,n);
    }
    return toutes_vraies(tab, n);
}

// Renvoie la formule exprimant la contrainte sur les diagonales avec une pente positive --> "/"
char* contrainte_diag_positives(int n){
    char** formule_totale = malloc((2*n - 1)*sizeof(char*));
    for(int d = 0; d < n; d++){
        char** tab = malloc((d+1)*sizeof(char*));
        for(int j = 0; j <= d; j++){
            tab[j] = variable(d-j, j);
        }
        formule_totale[d] = au_plus_une(tab, d+1);
        free(tab);
    }
    for(int d = 1; d < n; d++){
        char** tab = malloc((n-d)*sizeof(char*));
        for(int j = d; j < n; j++){
            tab[j-d] = variable(n-1+d-j, j);
        }
        formule_totale[n-1+d] = au_plus_une(tab, n-d);
        free(tab);
    }
    char* res = toutes_vraies(formule_totale, 2*n - 1);
    for(int i = 0; i < 2*n -1; i++){
        free(formule_totale[i]);
    }
    free(formule_totale);

    return res;
}

// Renvoie la formule exprimant la contrainte sur les diagonales avec une pente négative --> "\"
char* contrainte_diag_negatives(int n){
    char** formule_totale = malloc((2*n - 1)*sizeof(char*));
    for(int d = 0; d < n; d++){
        char** tab = malloc((n-d)*sizeof(char*));
        for(int j = 0; j <= n-1-d; j++){
            tab[j] = variable(j, d+j);
        }
        formule_totale[d] = au_plus_une(tab, n-d);
        free(tab);
    }
    for(int d = 1; d < n; d++){
        char** tab = malloc((n-d)*sizeof(char*));
        for(int j = d; j < n; j++){
            tab[j-d] = variable(j, j-d);
        }
        formule_totale[n-1+d] = au_plus_une(tab, n-d);
        free(tab);
    }
    char* res = toutes_vraies(formule_totale, 2*n - 1);
    for(int i = 0; i < 2*n -1; i++){
        free(formule_totale[i]);
    }
    free(formule_totale);

    return res;
}

char* contrainte_toutes_diagonales(int n){
    char* diag_pos = contrainte_diag_positives(n);
    char* diag_neg = contrainte_diag_negatives(n);
    char** total = malloc(2*sizeof(char*));
    total[0] = diag_pos;
    total[1] = diag_neg;
    

    char* res = toutes_vraies(total, 2);
    free(diag_neg);
    free(diag_pos);
    free(total);
    return res;
}

void gen_formule_n_dames(int n, char* filename){
    FILE* file = fopen(filename,"w");
    char** l_formules = malloc(3*sizeof(char*));
    l_formules[0] = contrainte_toutes_lignes(n);
    printf("%s\n LIGNES\n", l_formules[0]);
    l_formules[1] = contrainte_toutes_colonnes(n);
    printf("%s\n COLONNES\n", l_formules[1]);
    l_formules[2] = contrainte_toutes_diagonales(n);
    printf("%s\n DIAGONALES\n", l_formules[2]);
    char* formule = toutes_vraies(l_formules,3);
    fprintf(file,"%s",formule);
}


void tests(){
    assert(strcmp("X_45_155",variable(45, 155)) == 0);
    assert(strcmp("X_33_0",variable(33, 0)) == 0);
    printf("Tous les tests ont réussi !!\n\n");
    int n = 4;
	char** l = malloc(n*sizeof(char*));
	for (int i = 0; i < n; i++){
		l[i] = malloc(5*sizeof(char));
		l[i][0] = '(';
		l[i][1] = 'a';
		l[i][2] = '&';
		l[i][3] = 'b';
		l[i][4] = ')';
	}
	char* f = au_moins_une(l, n);
	assert(strcmp(f,"((a&b)|(a&b)|(a&b)|(a&b))") == 0);
	free(f);
	for (int i = 0; i < n; i++){
		free(l[i]);
	}
	free(l);
}

int main(int argc, char** argv){
    assert(argc == 2);

    int taille_string = strlen(argv[1]);
    char* file_out = malloc((11+taille_string)*sizeof(char));
    strcat(file_out,argv[1]);
    strcat(file_out,"_dames.txt\0");

    int n=0;
    int p=1;
    for (int i=taille_string-1; i>-1; i--){
        n+=(argv[1][i]-'0')*p;
        p=10*p;
    }
    gen_formule_n_dames(n,file_out);
    printf("fichier %s créé\n", file_out);
}
