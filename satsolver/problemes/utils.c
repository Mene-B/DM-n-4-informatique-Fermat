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
			// On passe au string suivant et on rajoute '|' pour la disjonction
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
			// On passe au string suivant et on rajoute '&' pour la conjonction
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
    // Au plus une formule est vraie peut se traduire en : ∀(f1,f2) ∈ l, ~f1 v ~f2 est vraie car ainsi on n'aura jamais deux formules vraies en même temps.
    // On fait donc la conjonction de ~f1 et ~f2 pour tout f1, f2 
    char** l_inter = malloc((n*(n-1)/2)*sizeof(char*)); // Il y a 2 parmi n façon de prendre deux formules de l. 
    int k = 0; // indice dans l_inter
    int i = 0; // indice de f1
    int j = 0; // indice de f2
    if(n == 1){
        char** deux_formules = malloc(2*sizeof(char*));
        deux_formules[0]=l[0]; 
        deux_formules[1]=non(l[0]); 
        char* res = au_moins_une(deux_formules, 2);
        free(deux_formules[0]);
        free(deux_formules[1]);
        free(deux_formules);
        
        return res;
    }
    while (i<n-1){
        j=i+1;
        while(j<n){
            char** deux_formules = malloc(2*sizeof(char*));
            deux_formules[0]=non(l[i]); // ~f1
            deux_formules[1]=non(l[j]); // ~f2
            l_inter[k] = au_moins_une(deux_formules,2); // ~f1 v ~f2
            free(deux_formules[0]);
            free(deux_formules[1]);
            free(deux_formules);
            j++;
            k++;
        }
        i++;
    }
    char* res = toutes_vraies(l_inter,n*(n-1)/2); // On fait la conjonction de toutes les disjonctions
    for (int i=0; i<n*(n-1)/2; i++){
        free(l_inter[i]);
    }
    free(l_inter);
    return res;
}


