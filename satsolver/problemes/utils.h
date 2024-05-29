#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

// Prend  une liste l de n formules atomiqes et renvoie la disjonction de ces formules
char* au_moins_une (char** l, int n);

// Renvoie la négation de la formule atomique l.
char* non(char* l);

// Prend une liste l de n formules atomiqes et renvoie la conjonction de ces formules
char* toutes_vraies(char** l, int n);

// Prend une liste l de n formules atomiques et renvoie une formule exprimant le fait qu'au plus une est vraie
char* au_plus_une(char** l, int n);
