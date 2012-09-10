#include <stdio.h>

void addMat(int r, int c, double m[r*c], double m2[r*c]) {
    int i;
    for (i=0; i<r*c; i++) {
	m[i] += m2[i];
    }
}

void add2Mat(int r, int c, double m[r*c], double m1[r*c], double m2[r*c]) {
    int i;
    for (i=0; i<r*c; i++) {
	m[i] = m1[i] + m2[i];
    }
}

