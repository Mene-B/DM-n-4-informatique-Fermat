
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
    // gen_formule_n_dames(n,file_out);
    printf("fichier %s créé\n", file_out);
}
