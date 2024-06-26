#include "sudoku.h"
#include "utils.h"

char* variable(int k, int i, int j){
    // Variables pour stocker les tailles des nombres i et j en base 10
    int taille_i = 0;
    int taille_j = 0;
    int taille_k = 0;
    // Stock les valeurs de i et j pour pouvoir les modifier
    int I = i;
    int J = j;
    int K = k;

    if(i == 0){
        taille_i = 1;
    } else while(I != 0){
        I = (int) (I/10);

char* variable(int k, int i, int j){
    // Variables pour stocker les tailles des nombres i et j en base 10
    int taille_i = 0;
    int taille_j = 0;
    int taille_k = 0;
    // Stock les valeurs de i et j pour pouvoir les modifier
    int I = i;
    int J = j;
    int K = k;

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
    if(k == 0){
        taille_k = 1;
    } else while(K != 0){
        K = (int) (K/10);
        taille_k++;
    }

    char* str = malloc((3+taille_i+taille_j+taille_k)*sizeof(char));
    sprintf(str, "X_%d_%d_%d",k, i, j);
    return str; 
}

char* contrainte_une_case(int i, int j, int n){
    char** tab = malloc(n*n*sizeof(char*));
    for (int l = 0; l<n*n; l++){
        tab[l] = variable(l+1,i,j);
    }
    char* res = au_moins_une(tab,n*n);
    free(tab);
    return res;
}

char* contrainte_toutes_cases(int n){
    char** tab = malloc(n*n*n*n*sizeof(char*));
    for (int i = 0; i<n*n; i++){
        for (int j = 0; j<n*n; j++){
            tab[n*n*i+j] = contrainte_une_case(i,j,n);
        }
    }
    char* res = toutes_vraies(tab,n*n*n*n);
    free(tab);
    return res;
}

char* contrainte_une_ligne_un_chiffre(int k, int i, int n){
    // On va stocker toutes les variables de la forme "X_k_i_j" pour j entre 0 et n-1
    char** tab = malloc(n*n*sizeof(char*));
    for(int j = 0; j < n*n; j++){
        tab[j] = variable(k, i, j);
    }
    char* au_moins = au_moins_une(tab, n*n);
    char* au_plus = au_plus_une(tab, n*n);
    
    char* res = malloc((1+strlen(au_moins)+strlen(au_plus))*sizeof(char));
    sprintf(res, "(%s&%s)", au_moins, au_plus);
    free(au_moins);
    free(au_plus);

    return res;
}

char* contrainte_toutes_lignes(int n){
    char** tab = malloc(n*n*n*n*sizeof(char*));
    for (int k = 0; k<n*n; k++){
        for (int i = 0; i<n*n; i++){
            tab[k*n*n+i] = contrainte_une_ligne_un_chiffre(k+1,i,n);
        }
    }
    char* res = toutes_vraies(tab,n*n*n*n);
    free(tab);
    return res;
}

char* contrainte_une_colonne_un_chiffre(int k, int j, int n){
    // On va stocker toutes les variables de la forme "X_k_i_j" pour j entre 0 et n-1
    char** tab = malloc(n*n*sizeof(char*));
    for(int i = 0; i < n*n; i++){
        tab[i] = variable(k, i, j);
    }
    char* au_moins = au_moins_une(tab, n*n);

    return au_moins;
}

char* contrainte_toutes_colonnes(int n){
    char** tab = malloc(n*n*n*n*sizeof(char*));
    for (int k = 0; k<n*n; k++){
        for(int j = 0; j < n*n; j++){
            tab[n*n*k+j] = contrainte_une_colonne_un_chiffre(k+1,j,n);
        }
    }
    char* res = toutes_vraies(tab,n*n*n*n);
    free(tab);
    return res;
}

char* contrainte_un_carre_un_chiffre(int k, int i, int j, int n){
    char** tab = malloc(n*n*sizeof(char*));
    for(int ii = 0; ii < n; ii++){
        for (int jj = 0; jj < n; jj++){
            tab[n*ii+jj] = variable(k, n*i+ii, n*j+jj);
        }
    }
    char* au_moins = au_moins_une(tab, n*n);
    free(tab);
    return au_moins;    
}

char* contrainte_tous_carres(int n){
    char** tab = malloc(n*n*n*n*sizeof(char*));
    for (int k = 0; k<n*n; k++){
        for (int i = 0; i<n; i++){
            for (int j = 0; j<n; j++){
                tab[n*n*k+n*i+j] = contrainte_un_carre_un_chiffre(k+1,i,j,n);
            }
        }
    }
    char* res = toutes_vraies(tab,n*n*n*n);
    free(tab);
    return res;
}

void gen_formule_n_sudoku(int n, char* filename){
    FILE* f = fopen(filename, "w");
    char** tab = malloc(4*sizeof(char*));
    tab[0] = contrainte_toutes_cases(n);
    tab[1] = contrainte_toutes_lignes(n);
    tab[2] = contrainte_toutes_colonnes(n);
    tab[3] = contrainte_tous_carres(n);

    char* res = toutes_vraies(tab,4);
    free(tab);
    fprintf(f,"%s",res);
}


void test(){
    // printf("%s",contrainte_une_case(0,0,6));
    // printf("%s", contrainte_toutes_cases(6));
    // printf ("%s", contrainte_une_ligne_un_chiffre(2,0,5));
    // printf("%s", contrainte_toutes_lignes(2));
    // printf("%s", contrainte_une_colonne_un_chiffre(1,2,5));
    // printf("%s", contrainte_toutes_colonnes(2));
    // printf("%s", contrainte_un_carre_un_chiffre(2,1,1,2));
    // printf("%s", contrainte_tous_carres(2));
}

int main(){
    gen_formule_n_sudoku(2,"2_sudoku.txt");
}
        taille_i++;
    }
    if(j == 0){
        taille_j = 1;
    } else while(J != 0){
        J = (int) (J/10);
        taille_j++;
    }
    if(k == 0){
        taille_k = 1;
    } else while(K != 0){
        K = (int) (K/10);
        taille_k++;
    }

    char* str = malloc((3+taille_i+taille_j+taille_k)*sizeof(char));
    sprintf(str, "X_%d_%d_%d",k, i, j);
    return str; 
}

char* contrainte_une_case(int i, int j, int n){
    char** tab = malloc(n*n*sizeof(char*));
    for (int l = 0; l<n*n; l++){
        tab[l] = variable(l+1,i,j);
    }
    char* res = au_moins_une(tab,n*n);
    free(tab);
    return res;
}

char* contrainte_toutes_cases(int n){
    char** tab = malloc(n*n*n*n*sizeof(char*));
    for (int i = 0; i<n*n; i++){
        for (int j = 0; j<n*n; j++){
            tab[n*n*i+j] = contrainte_une_case(i,j,n);
        }
    }
    char* res = toutes_vraies(tab,n*n*n*n);
    free(tab);
    return res;
}

char* contrainte_une_ligne_un_chiffre(int k, int i, int n){
    // On va stocker toutes les variables de la forme "X_k_i_j" pour j entre 0 et n-1
    char** tab = malloc(n*n*sizeof(char*));
    for(int j = 0; j < n*n; j++){
        tab[j] = variable(k, i, j);
    }
    char* au_moins = au_moins_une(tab, n*n);
    char* au_plus = au_plus_une(tab, n*n);
    
    char* res = malloc((1+strlen(au_moins)+strlen(au_plus))*sizeof(char));
    sprintf(res, "(%s&%s)", au_moins, au_plus);
    free(au_moins);
    free(au_plus);

    return res;
}

char* contrainte_toutes_lignes(int n){
    char** tab = malloc(n*n*n*n*sizeof(char*));
    for (int k = 0; k<n*n; k++){
        for (int i = 0; i<n*n; i++){
            tab[k*n*n+i] = contrainte_une_ligne_un_chiffre(k+1,i,n);
        }
    }
    char* res = toutes_vraies(tab,n*n*n*n);
    free(tab);
    return res;
}

char* contrainte_une_colonne_un_chiffre(int k, int j, int n){
    // On va stocker toutes les variables de la forme "X_k_i_j" pour j entre 0 et n-1
    char** tab = malloc(n*n*sizeof(char*));
    for(int i = 0; i < n*n; i++){
        tab[i] = variable(k, i, j);
    }
    char* au_moins = au_moins_une(tab, n*n);

    return au_moins;
}

char* contrainte_toutes_colonnes(int n){
    char** tab = malloc(n*n*n*n*sizeof(char*));
    for (int k = 0; k<n*n; k++){
        for(int j = 0; j < n*n; j++){
            tab[n*n*k+j] = contrainte_une_colonne_un_chiffre(k+1,j,n);
        }
    }
    char* res = toutes_vraies(tab,n*n*n*n);
    free(tab);
    return res;
}

char* contrainte_un_carre_un_chiffre(int k, int i, int j, int n){
    char** tab = malloc(n*n*sizeof(char*));
    for(int ii = 0; ii < n; ii++){
        for (int jj = 0; jj < n; jj++){
            tab[n*ii+jj] = variable(k, n*i+ii, n*j+jj);
        }
    }
    char* au_moins = au_moins_une(tab, n*n);
    free(tab);
    return au_moins;    
}

char* contrainte_tous_carres(int n){
    char** tab = malloc(n*n*n*n*sizeof(char*));
    for (int k = 0; k<n*n; k++){
        for (int i = 0; i<n; i++){
            for (int j = 0; j<n; j++){
                tab[n*n*k+n*i+j] = contrainte_un_carre_un_chiffre(k+1,i,j,n);
            }
        }
    }
    char* res = toutes_vraies(tab,n*n*n*n);
    free(tab);
    return res;
}

char* conditions_initiales(char* file_name, int n){
    FILE* file =  fopen(file_name, "r");
    assert (file != NULL);
    char** tab = malloc(n*sizeof(char*));
    for(int i = 0; i<n; i++ ){
        char* string = malloc(8*sizeof(char));
        fscanf(file, "%s ", string);
        tab[i] = string;
    }
    char* res = toutes_vraies(tab, n);
    fclose(file); 
    free(tab);  
    return res;
}

void gen_formule_n_sudoku(int n, char* filename, int p){
    FILE* f = fopen(filename, "w");
    char** tab = malloc(5*sizeof(char*));
    tab[0] = contrainte_toutes_cases(n);
    tab[1] = contrainte_toutes_lignes(n);
    tab[2] = contrainte_toutes_colonnes(n);
    tab[3] = contrainte_tous_carres(n);
    tab[4] = conditions_initiales("conditions.txt", p);

    char* res = toutes_vraies(tab,5);
    free(tab);
    fprintf(f,"%s",res);
    fclose(f);
}




void test(){
    // printf("%s",contrainte_une_case(0,0,6));
    // printf("%s", contrainte_toutes_cases(6));
    // printf ("%s", contrainte_une_ligne_un_chiffre(2,0,5));
    // printf("%s", contrainte_toutes_lignes(2));
    // printf("%s", contrainte_une_colonne_un_chiffre(1,2,5));
    //printf("%s", contrainte_toutes_colonnes(2));
    // printf("%s", contrainte_un_carre_un_chiffre(2,1,1,2));
    // printf("%s", contrainte_tous_carres(2));
    //printf("%s", conditions_initiales("conditions.txt", 3));
}


int main(int argc, char** argv){
    test();
    assert(argc == 3);

    int taille_string = strlen(argv[1]);
    int taille_string_2 = strlen(argv[2]);
    char* file_out = malloc((12+taille_string)*sizeof(char));
    strcat(file_out,argv[1]);
    strcat(file_out,"_sudoku.txt\0");

    int n=0;
    int p=1;
    for (int i=taille_string-1; i>-1; i--){
        n+=(argv[1][i]-'0')*p;
        p=10*p;
    }
    int n_2 = 0;
    p = 1;
    for (int i=taille_string_2-1; i>-1; i--){
        n_2+=(argv[2][i]-'0')*p;
        p=10*p;
    }
    gen_formule_n_sudoku(n,file_out,n_2);
    printf("fichier %s créé\n", file_out);
}
