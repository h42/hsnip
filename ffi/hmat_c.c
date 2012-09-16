#include <stdio.h>
#include <malloc.h>

#define ij (i*c+j)
#define ji (j*c+i)

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

double *getBuf(Matrix *m) {
    return m->dv;
}

int getRows(Matrix *m) {
    return m->rows;
}

int getCols(Matrix *m) {
    return m->cols;
}

double getElem(Matrix *m, int i, int j) {
    return m->dv[i * m->cols + j];
}

void putElem(Matrix *m, int i, int j, double d) {
    m->dv[i * m->cols + j] = d;
}

void addMat(Matrix *m, Matrix *m2) {
    int i, n = m->rows * m->cols;
    double *dv=m->dv, *dv2=m2->dv;
    for (i=0; i<n; i++) dv[i] += dv2[i];
}

int multMat(Matrix *p, Matrix *m, Matrix *m2) {
    int i,j,k,a;
    int r=m->rows,c=m->cols,r2=m2->rows;
    if (c != r2 || p==m || p==m2) return -1;
    double *dv=m->dv, *dv2=m2->dv, *product=p->dv;
    for (i=0; i<r; i++) {
	for (j=0; j<c; j++) {
	    for (k=0,a=0; k<c; k++) a += dv[i*c+k] * dv2[k*c+j];
	    product[ij] = a;
	}
    }
    return 0;
}

void incMat(Matrix *m, double x) {
    int i,n=m->rows*m->cols;
    double *dv = m->dv;
    for (i=0;i<n;i++) dv[i]+=x;
}

void scaleMat(Matrix *m, double x) {
    int i,n=m->rows*m->cols;
    double *dv = m->dv;
    for (i=0;i<n;i++) dv[i]*=x;
}

void idMat(Matrix *m) {
    int i,j,c=m->cols,r=m->rows;
    double *dv = m->dv;
    for (i=0; i<r; i++) {
	for (j=0; j<c; j++) dv[ij] = (i==j) ? 1 : 0;
    }
}

void zeroMat(Matrix *m) {
    int i,c=m->cols,r=m->rows;
    double *dv = m->dv;
    for (i=0; i<r*c; i++) dv[i]=0;
}

void transMat(Matrix *m) {
    int i,j;
    int r=m->rows, c=m->cols;
    double t, *dv=m->dv;
    for (i=0; i<r; i++) {
	for (j=0; j<i; j++) {
	    t=dv[ij];
	    dv[ij] = dv[ji];
	    dv[ji] = t;
	}
    }
}

