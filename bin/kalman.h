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

double sigma[N][N] = {
    { 1.682490, 0.621964, 0.959947, 1.228820, 1.029410, } ,
    { 0.621964, 0.631446, 0.551902, 0.723342, 0.756674, } ,
    { 0.959947, 0.551902, 1.100060, 0.908402, 1.032840, } ,
    { 1.228820, 0.723342, 0.908402, 1.212400, 1.011350, } ,
    { 1.029410, 0.756674, 1.032840, 1.011350, 1.302410, } ,
};

double h[K][N] = {
    { 0.4621110, 0.833041, 0.0395867, 0.529315, 0.241678, },
    { 0.0507828, 0.340120, 0.8726660, 0.836114, 0.571528, },
    { 0.7779080, 0.541655, 0.8691540, 0.286846, 0.265820, },
};

double* new_mu;

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
    const double *mu,    /* n,1 */
    double *r,           /* k,k */
    double *data,        /* k,1 */
    double **ret_mu,     /* k,1 */
    double **ret_sigma   /* n,n */
) {
        double* k_by_n = (double *) malloc(k * n * sizeof(double));
/*20*/  cblas_dsymm(CblasRowMajor, CblasRight, CblasUpper, k, n, 1., sigma, n, h, n, 0., k_by_n, n);
/*21*/  cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasTrans, k, k, n, 1., k_by_n, n, h, n, 1., r, k);
/*22*/  cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, k, 1, n, 1., h, n, mu, 1, -1., data, 1);
/*23*/  cblas_dcopy(k * n, h, 1, k_by_n, 1);
        double* k_by_k = (double *) malloc(k * k * sizeof(double));
/*24*/  cblas_dcopy(k * k, r, 1, k_by_k, 1);
/*25*/  LAPACKE_dposv(LAPACK_ROW_MAJOR, 'U', k, n, k_by_k, k, k_by_n, n);
/*27*/  LAPACKE_dpotrs(LAPACK_ROW_MAJOR, 'U', k, 1, k_by_k, k, data, 1);
        free(k_by_k);
        double* n_by_n = (double *) malloc(n * n * sizeof(double));
/*28*/  cblas_dgemm(CblasRowMajor, CblasTrans, CblasNoTrans, n, n, k, 1., h, n, k_by_n, n, 0., n_by_n, n);
        free(k_by_n);
        double* n_by_1 = (double *) malloc(n * sizeof(double));
/*29*/  cblas_dgemm(CblasRowMajor, CblasTrans, CblasNoTrans, n, 1, k, 1., h, n, data, 1, 0., n_by_1, 1);
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

static double measure_kalman_no_free(
    const int n,
    const int k,
    const double *sigma, /* n,n */
    const double *h,     /* k,n */
    const double *mu,    /* n,1 */
    double *r,           /* k,k */
    double *data         /* k,1 */
) {
    struct rusage usage;

    getrusage(RUSAGE_SELF, &usage);
    struct timeval start = usage.ru_utime;

    kalman(n, k, sigma, h, mu, r, data, &new_mu, &new_sigma);

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
    const double *mu,    /* n,1 */
    double *r,           /* k,k */
    double *data         /* k,1 */
) {
    const double result = measure_kalman_no_free(n, k, sigma, h, mu, r, data);
    free(new_sigma);
    free(new_mu);
    return result;
}

struct result {
    double *new_sigma;
    double *new_mu;
};

struct result results(
    const int n,
    const int k,
    const double *sigma, /* n,n */
    const double *h,     /* k,n */
    const double *mu,    /* n,1 */
    double *r,           /* k,k */
    double *data         /* k,1 */
) {
    measure_kalman_no_free(n, k, sigma, h, mu, r, data);
    struct result result = { .new_sigma=new_sigma, .new_mu=new_mu};
    return result;
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
        printf("\t%f\n", new_mu[i]);
    }
    printf("\n");
    free(new_mu);

    return result;

}

#endif // LT4LA_KALMAN
