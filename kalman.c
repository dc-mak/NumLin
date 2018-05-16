#include <stdlib.h>
#include <cblas.h>
#include <lapacke.h>

void kalman(
    int k,
    int n,
    double *sigma,     /* n,n */
    double *h,         /* k,n */
    double *mu,        /* n,1 */
    double *r,         /* k,k */
    double *data,      /* k,1 */
    double **ret_mu,   /* k,1 */
    double **ret_sigma /* n,n */
) {
        double* k_by_n = (double *) malloc(k * n * sizeof(double));
/*20*/  cblas_dsymm(CblasRowMajor, CblasRight, CblasUpper, k, n, 1., sigma, n, h, n, 0., k_by_n, n);
/*21*/  cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasTrans, k, n, k, 1., k_by_n, n, h, k, 1., r, k);
/*22*/  cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, k, n, 1, 1., h, n, mu, 1, -1., data, 1);
/*23*/  cblas_dcopy(k * n, h, 1, k_by_n, 1);
        double* k_by_k = (double *) malloc(k * k * sizeof(double));
/*24*/  cblas_dcopy(k * k, r, 1, k_by_k, 1);
/*25*/  LAPACKE_dposv(LAPACK_ROW_MAJOR, 'U', k, n, k_by_k, k, k_by_n, n);
/*27*/  LAPACKE_dpotrs(LAPACK_ROW_MAJOR, 'U', k, 1, k_by_k, k, data, 1);
        free(k_by_k);
        double* n_by_n = (double *) malloc(n * n * sizeof(double));
/*28*/  cblas_dgemm(CblasRowMajor, CblasTrans, CblasNoTrans, n, k, n, 1., h, k, k_by_n, n, 0., n_by_n, n);
        free(k_by_n);
        double* n_by_1 = (double *) malloc(n * sizeof(double));
/*29*/  cblas_dgemm(CblasRowMajor, CblasTrans, CblasNoTrans, n, k, 1, 1., h, k, data, 1, 0., n_by_1, 1);
        double* new_mu = (double *) malloc(n * sizeof(double));
/*30*/  cblas_dcopy(n, mu, 1, new_mu, 1);
/*31*/  cblas_dsymm(CblasRowMajor, CblasLeft, CblasUpper, n, 1, 1., sigma, n, n_by_1, 1, 1., new_mu, 1);
        free(n_by_1);
        double* n_by_n2 = (double *) malloc(n * n * sizeof(double));
/*32*/  cblas_dsymm(CblasRowMajor, CblasRight, CblasUpper, n, n, 1., sigma, n, n_by_n, n, 0., n_by_n2, n);
/*33*/  cblas_dcopy(n*n, sigma, 1, n_by_n, 1);
/*34*/  cblas_dsymm(CblasRowMajor, CblasLeft, CblasUpper, n, n, -1., sigma, n, n_by_n2, n, 1., n_by_n, n);
        free(n_by_n2);
        *ret_sigma = n_by_n;
        *ret_mu = new_mu;
}
