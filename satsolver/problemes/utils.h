#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

//prend  une liste l de fromules atomiqes de taille n et fabrique la disjonction de ces formules
char* au_moins_une (char** l, int n);

//renvoie la négation de la formule atomique l.
char* non(char* l);

//prend une liste l de fromules atomiqes de taille n et fabrique la conjonction de ces formules
char* toutes_vraies(char** l, int n);

//prend une liste l de formules atomiques de taille n et renvoie une formule exprimant le fait qu'au plus une est vraie
char* au_plus_une(char** l, int n);
