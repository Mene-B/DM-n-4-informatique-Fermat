#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

/* au_moins_une(l, n) avec l une liste de n strings de formules atomiques 
	renvoie la conjonction atomique des ces formules */
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

void tests(){
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

void main(){
	tests();
	printf("Tout à l'air d'aller bien...\n");

}