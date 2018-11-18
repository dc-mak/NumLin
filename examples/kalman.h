#ifndef LT4LA_KALMAN
#define LT4LA_KALMAN

#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <cblas.h>
#include <lapacke.h>

#define N 5
#define K 3

double *new_sigma;

double const sigma[N][N] = {
    { 1.682490, 0.621964, 0.959947, 1.228820, 1.029410, } ,
    { 0.621964, 0.631446, 0.551902, 0.723342, 0.756674, } ,
    { 0.959947, 0.551902, 1.100060, 0.908402, 1.032840, } ,
    { 1.228820, 0.723342, 0.908402, 1.212400, 1.011350, } ,
    { 1.029410, 0.756674, 1.032840, 1.011350, 1.302410, } ,
};

double const h[K][N] = {
    { 0.4621110, 0.833041, 0.0395867, 0.529315, 0.241678, },
    { 0.0507828, 0.340120, 0.8726660, 0.836114, 0.571528, },
    { 0.7779080, 0.541655, 0.8691540, 0.286846, 0.265820, },
};

double mu[N][1] = {
    { 0.8015420 },
    { 0.8585870 },
    { 0.0950306 },
    { 0.8101720 },
    { 0.3491810 },
};

double r[K][K] = {
    { 0.880164, 0.676823, 0.802738, },
    { 0.676823, 0.650806, 0.958725, },
    { 0.802738, 0.958725, 1.745970, },
};

double data[K][1] = {
    { 0.551922 },
    { 0.673854 },
    { 0.259412 },
};

static void kalman(
    const int n,
    const int k,
    const double *sigma, /* n,n */
    const double *h,     /* k,n */
    double *mu,          /* n,1 */
    double *r,           /* k,k */
    double *data,        /* k,1 */
    double **ret_sigma   /* n,n */
) {
    double* n_by_k = (double *) malloc(n * k * sizeof(double));
    cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasTrans, n, k, n, 1., sigma, n, h, n, 0., n_by_k, k);
    cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, k, k, n, 1., h, n, n_by_k, k, 1., r, k);
    LAPACKE_dposv(LAPACK_COL_MAJOR, 'U', k, n, r, k, n_by_k, k);
    cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, k, 1, n, 1., h, n, mu, 1, -1., data, 1);
    cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, n, 1, k, 1., n_by_k, k, data, 1, 1., mu, 1);
    double* n_by_n = (double *) malloc(n * n * sizeof(double));
    cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, n, n, k, 1., n_by_k, k, h, n, 0., n_by_n, n);
    free(n_by_k);
    double* n_by_n2 = (double *) malloc(n * n * sizeof(double));
    cblas_dcopy(n*n, sigma, 1, n_by_n2, 1);
    cblas_dsymm(CblasRowMajor, CblasRight, CblasUpper, n, n, -1., sigma, n, n_by_n, n, 1., n_by_n2, n);
    free(n_by_n);
    *ret_sigma = n_by_n2;
}

static double measure_kalman_no_free(
    const int n,
    const int k,
    const double *sigma, /* n,n */
    const double *h,     /* k,n */
    double *mu,          /* n,1 */
    double *r,           /* k,k */
    double *data         /* k,1 */
) {
    struct rusage usage;

    getrusage(RUSAGE_SELF, &usage);
    struct timeval start = usage.ru_utime;

    kalman(n, k, sigma, h, mu, r, data, &new_sigma);

    getrusage(RUSAGE_SELF, &usage);
    struct timeval end = usage.ru_utime;

    // Execution time would have to reach about 300 years before (* 1000000) will cause problems
    return (double) ((end.tv_sec - start.tv_sec) * 1000000 + (end.tv_usec - start.tv_usec));
}

double measure_kalman(
    const int n,
    const int k,
    const double *sigma, /* n,n */
    const double *h,     /* k,n */
    double *mu,          /* n,1 */
    double *r,           /* k,k */
    double *data         /* k,1 */
) {
    const double result = measure_kalman_no_free(n, k, sigma, h, mu, r, data);
    free(new_sigma);
    return result;
}

double *result(
    const int n,
    const int k,
    const double *sigma, /* n,n */
    const double *h,     /* k,n */
    double *mu,          /* n,1 */
    double *r,           /* k,k */
    double *data         /* k,1 */
) {
    measure_kalman_no_free(n, k, sigma, h, mu, r, data);
    return new_sigma;
}

double test(int arg) {

    const double result =
        measure_kalman_no_free(N, K, &sigma[0][0], &h[0][0], &mu[0][0], &r[0][0], &data[0][0]);
    printf("Arg: %d, Result: %f\n\n", arg, result);

    // Print Matrices
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            printf("\t%f", new_sigma[i*N + j]);
        }
        printf("\n");
    }
    printf("\n");
    free(new_sigma);

    for (int i = 0; i < N; i++) {
        printf("\t%f\n", mu[i][0]);
    }
    printf("\n");

    return result;

}

#endif // LT4LA_KALMAN
