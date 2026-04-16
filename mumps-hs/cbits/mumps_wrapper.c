#include "mumps_wrapper.h"
#include <dmumps_c.h>
#include <stdlib.h>
#include <string.h>

/* MUMPS_SEQ sentinel for comm_fortran (no real MPI) */
#define MUMPS_USE_COMM_WORLD (-987654)

struct MumpsSolver {
    DMUMPS_STRUC_C id;
    int  n;
    int  nnz;
    int* irn;      /* 1-indexed copy */
    int* jcn;      /* 1-indexed copy */
    double* a;     /* matrix values  */
    double* rhs;   /* scratch for RHS / solution */
};

MumpsSolver* mumps_create(int n, int nnz, const int* irn, const int* jcn, const double* a)
{
    MumpsSolver* s = calloc(1, sizeof(MumpsSolver));
    if (!s) return NULL;

    s->n   = n;
    s->nnz = nnz;

    /* Allocate and copy matrix arrays (convert 0-indexed to 1-indexed) */
    s->irn = malloc(nnz * sizeof(int));
    s->jcn = malloc(nnz * sizeof(int));
    s->a   = malloc(nnz * sizeof(double));
    s->rhs = malloc(n * sizeof(double));
    if (!s->irn || !s->jcn || !s->a || !s->rhs) {
        free(s->irn); free(s->jcn); free(s->a); free(s->rhs);
        free(s);
        return NULL;
    }

    for (int i = 0; i < nnz; i++) {
        s->irn[i] = irn[i] + 1;  /* 0-indexed -> 1-indexed */
        s->jcn[i] = jcn[i] + 1;
    }
    memcpy(s->a, a, nnz * sizeof(double));

    /* Initialize MUMPS instance */
    s->id.job          = -1;    /* init */
    s->id.par          = 1;     /* host participates */
    s->id.sym          = 0;     /* unsymmetric */
    s->id.comm_fortran = MUMPS_USE_COMM_WORLD;
    dmumps_c(&s->id);

    if (s->id.infog[0] < 0) return s; /* caller checks via mumps_get_error */

    /* Configure ICNTL parameters (C 0-indexed: icntl[k-1] = ICNTL(k)) */
    s->id.icntl[0]  = -1;   /* ICNTL(1): no error output to stream */
    s->id.icntl[1]  = -1;   /* ICNTL(2): no diagnostic output */
    s->id.icntl[2]  = -1;   /* ICNTL(3): no global info output */
    s->id.icntl[3]  =  0;   /* ICNTL(4): verbosity: errors only */
    s->id.icntl[13] = 80;   /* ICNTL(14): memory relaxation 80% */
    s->id.icntl[23] = 1;    /* ICNTL(24): null pivot detection */

    /* Set matrix data (centralized, assembled) */
    s->id.n   = n;
    s->id.nnz = nnz;
    s->id.irn = s->irn;
    s->id.jcn = s->jcn;
    s->id.a   = s->a;

    return s;
}

int mumps_analyze(MumpsSolver* s)
{
    if (!s) return -1;
    s->id.job = 1;  /* analysis */
    dmumps_c(&s->id);
    return s->id.infog[0] < 0 ? s->id.infog[0] : 0;
}

int mumps_factorize(MumpsSolver* s)
{
    if (!s) return -1;
    s->id.job = 2;  /* factorization */
    dmumps_c(&s->id);
    return s->id.infog[0] < 0 ? s->id.infog[0] : 0;
}

int mumps_solve(MumpsSolver* s, const double* rhs, double* sol)
{
    if (!s) return -1;

    /* Copy RHS into scratch buffer (MUMPS overwrites it with solution) */
    memcpy(s->rhs, rhs, s->n * sizeof(double));
    s->id.rhs  = s->rhs;
    s->id.nrhs = 1;
    s->id.lrhs = s->n;

    s->id.job = 3;  /* solve */
    dmumps_c(&s->id);

    if (s->id.infog[0] < 0)
        return s->id.infog[0];

    /* Copy solution out */
    memcpy(sol, s->rhs, s->n * sizeof(double));
    return 0;
}

int mumps_solve_multi(MumpsSolver* s, int nrhs, const double* rhs, double* sol)
{
    if (!s || nrhs < 1) return -1;

    size_t bytes = (size_t)s->n * (size_t)nrhs * sizeof(double);
    double* scratch = malloc(bytes);
    if (!scratch) return -1;

    memcpy(scratch, rhs, bytes);
    s->id.rhs  = scratch;
    s->id.nrhs = nrhs;
    s->id.lrhs = s->n;

    s->id.job = 3;  /* solve */
    dmumps_c(&s->id);

    int rc = s->id.infog[0];
    if (rc >= 0) {
        memcpy(sol, scratch, bytes);
        rc = 0;
    }

    /* Restore scratch pointer so the single-RHS path's invariant still holds */
    s->id.rhs  = s->rhs;
    s->id.nrhs = 1;
    s->id.lrhs = s->n;

    free(scratch);
    return rc;
}

void mumps_destroy(MumpsSolver* s)
{
    if (!s) return;

    /* Tell MUMPS to release its internal memory */
    s->id.job = -2;
    dmumps_c(&s->id);

    /* Free our arrays */
    free(s->irn);
    free(s->jcn);
    free(s->a);
    free(s->rhs);
    free(s);
}

int mumps_get_error(MumpsSolver* s)
{
    if (!s) return -1;
    return s->id.infog[0];
}
