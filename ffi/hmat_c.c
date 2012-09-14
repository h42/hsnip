#include <stdio.h>
#include <malloc.h>

typedef struct Matrix {
    int rows,cols;
    double *dv;
} Matrix;

void *newMat(int r, int c) {
    struct Matrix *vp = malloc(sizeof(struct Matrix));
    vp->dv = malloc(r*c*sizeof(double));
    vp->rows=r;
    vp->cols=c;
    return vp;
}

double *getMatBuf(Matrix *m) {
    return m->dv;
}

int getMatRows(Matrix *m) {
    return m->rows;
}

int getMatCols(Matrix *m) {
    return m->cols;
}

double getMatElem(Matrix *m, int i, int j) {
    return m->dv[i * m->cols + j];
}

void putMatElem(Matrix *m, int i, int j, double d) {
    m->dv[i * m->cols + j] = d;
}

void addMat(Matrix *m, Matrix *m2) {
    int i;
    for (i=0; i<m->rows * m->cols; i++) {
	m->dv[i] += m2->dv[i];
    }
}

void idMat(Matrix *m) {
    int i,j,c=m->cols,r=m->rows;
    double *dv = m->dv;
    for (i=0; i<r; i++) {
	for (j=0; j<c; j++) dv[i*c+j] = (i==j) ? 1 : 0;
    }
}

void zeroMat(Matrix *m) {
    int i,c=m->cols,r=m->rows;
    double *dv = m->dv;
    for (i=0; i<r*c; i++) dv[i]=0;
}

void _transMat(int r, int c, double m[r][c]) {
    int i,j;
    double t;
    for (i=0; i<r; i++) {
	for (j=0; j<i; j++) {
	    t=m[i][j];
	    m[i][j] = m[j][i];
	    m[j][i] = t;
	}
    }
}

void transMat(Matrix *m) {
    _transMat(m->rows, m->cols, m->dv);
}

