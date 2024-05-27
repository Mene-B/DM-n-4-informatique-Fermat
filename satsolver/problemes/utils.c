#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

char* au_moins_une(char** l, int n){
	int taille_somme = 0; // Somme de la taille de chaque string
	// Calcul de cette variable
	for (int i = 0; i < n; i++){
		taille_somme = taille_somme + strlen(l[i]);
	}
	// Création du string final
	char* f = malloc((taille_somme + n + 2)*sizeof(char));
	int i = 1; // position d'écriture dans le string final
	int j = 0; // position de lecture dans un des string de la liste
	int k = 0; // numéro du string de la liste
	while (i < (taille_somme + n)) {
		if (j >= strlen(l[k])){
			// Un string a été totalement parcouru (on a écrit une condition entière)
			// On passe au string suivant et on rajoute '|' pour la conjonction
			k++;
			j = 0;
			f[i] = '|';
			i++;
		}
		// On recopie dans le string final les caractères des string de la liste
		f[i] = l[k][j];
		i++;
		j++;
	}
	// On rajoute le nécessaire pour que la formule soit atomique ('(',')') et que ça soit un string ('\0')
	f[0] = '(';
	f[taille_somme + n] = ')';
	f[taille_somme + n + 1] = '\0';
	return f;
}

char* toutes_vraies(char** l, int n){
	int taille_somme = 0; // Somme de la taille de chaque string
	// Calcul de cette variable
	for (int i = 0; i < n; i++){
		taille_somme = taille_somme + strlen(l[i]);
	}
	// Création du string final
	char* f = malloc((taille_somme + n + 2)*sizeof(char));
	int i = 1; // position d'écriture dans le string final
	int j = 0; // position de lecture dans un des string de la liste
	int k = 0; // numéro du string de la liste
	while (i < (taille_somme + n)) {
		if (j >= strlen(l[k])){
			// Un string a été totalement parcouru (on a écrit une condition entière)
			// On passe au string suivant et on rajoute '|' pour la conjonction
			k++;
			j = 0;
			f[i] = '&';
			i++;
		}
		// On recopie dans le string final les caractères des string de la liste
		f[i] = l[k][j];
		i++;
		j++;
	}
	// On rajoute le nécessaire pour que la formule soit atomique ('(',')') et que ça soit un string ('\0')
	f[0] = '(';
	f[taille_somme + n] = ')';
	f[taille_somme + n + 1] = '\0';
	return f;
}

char* non(char* l){
 	int taille = strlen(l);
    char* f = malloc((taille+4)*sizeof(char));
    f[0] = '(';
    f[1] = '~';
    for (int i=0; i<taille; i++){
        f[i+2] = l[i];
    }
    f[taille+2] = ')';
    f[taille+3] = '\0';
    return f;
}

char* au_plus_une(char** l, int n){
    char** l_inter = malloc((n+1)*sizeof(char*));
    char* tout_vrai = toutes_vraies(l,n);
    l_inter[0] = non(tout_vrai);
    free(tout_vrai);
    
    char** l_une_vraie = malloc(n*sizeof(char*));
    l_une_vraie[0] = l[0];
    for (int i=1; i<n; i++){
        l_une_vraie[i] = non(l[i]);
    }
    l_inter[1] = toutes_vraies(l_une_vraie,n);

    for (int i=1; i<n; i++){
        l_une_vraie[i-1] = non(l[i-1]);
        free(l_une_vraie[i]);
        l_une_vraie[i] = l[i];
        l_inter[i+1] = toutes_vraies(l_une_vraie,n);
    }


    char* res = au_moins_une(l_inter,n+1);

    for (int i=0; i<n-1; i++){
        free(l_inter[i]);
        free(l_une_vraie[i]);
    }
    free(l_inter[n-1]);
    free(l_inter[n]);
    free(l_une_vraie);
    free(l_inter);
    return res;
}


void tests(){
	int n = 4;
	char** l = malloc(n*sizeof(char*));
	for (int i = 0; i < n; i++){
		l[i] = malloc(6*sizeof(char));
		l[i][0] = '(';
		l[i][1] = 'a';
		l[i][2] = '&';
		l[i][3] = 'b';
		l[i][4] = ')';
        l[i][5] = '\0';
	}
	char* f = au_moins_une(l, n);
	assert(strcmp(f,"((a&b)|(a&b)|(a&b)|(a&b))") == 0);

    char* g = non(f);
    assert(strcmp(g,"(~((a&b)|(a&b)|(a&b)|(a&b)))") == 0);

    char* h = toutes_vraies(l, n);
    assert(strcmp(h,"((a&b)&(a&b)&(a&b)&(a&b))") == 0);

    char* i = au_plus_une(l,n);
    printf("%s\n",i);

	free(f);
    free(g);
    free(h);
    free(i);
	for (int i = 0; i < n; i++){
		free(l[i]);
	}
	free(l);
	


}

void main(){
	tests();
	printf("Tout à l'air d'aller bien...\n");

}
