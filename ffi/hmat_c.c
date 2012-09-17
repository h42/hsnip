#include <stdio.h>
#include <malloc.h>
#include <math.h>

#define ii (i*c+i)
#define jj (j*c+j)
#define ij (i*c+j)
#define ji (j*c+i)
#define ik (i*c+k)
#define ki (k*c+i)
#define jk (j*c+k)
#define kj (k*c+j)

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

//
// Gaussian Elimination
//
int eliminate(Matrix *m) {
    int i,j,k,maxp;
    int r=m->rows, c=m->cols;
    double t, maxv, *dv=m->dv;
    if (r>c || r<c-1) return -1;
    for (i=0; i<r; i++) {
	maxp=i;
	maxv=fabs(dv[ii]);
	for (j=i+1; j<r; j++) {
	    if ((t=fabs(dv[ji])) > maxv) {
		maxp=j;
		maxv=t;
	    }
	}
	for (k=i; k<c; k++) {
	    t = dv[ik];
	    dv[ik] = dv[maxp*c + k];
	    dv[maxp*c + k] = t;
	}
	if (dv[ii]==0) return -1; // Should we check for very low divsor
	for (j=i+1; j<r; j++) {
	    for (k=c-1; k>=i; k--) {
		dv[jk] -= dv[ik] * dv[ji]/ dv[ii];
	    }
	}
    }
    return 0;
}

int solve(Matrix *m, double *X) {
    int rc = eliminate(m);
    if (rc) return rc;
    int j,k;
    int r=m->rows, c=m->cols;
    double t, *dv=m->dv;
    for (j=r-1; j>=0; j--) {
	if (dv[jj] == 0) return -1;
	t=0;
	for (k=j+1; k<c-1; k++) t += dv[jk] * X[k];
	X[j] = (dv[j*c + (c-1)] - t) / dv[jj];
    }
    return 0;
}

