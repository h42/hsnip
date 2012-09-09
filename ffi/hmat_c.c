#include <stdio.h>

void xxx(int r, int c, int m[r][c]) {
    int i,j;
    for (i=0;i<r;i++) {
	for (j=0;j<c;j++) {
	    m[i][j]=0;
	}
    }
}
