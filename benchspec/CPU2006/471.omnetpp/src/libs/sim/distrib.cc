//==========================================================================
// file distrib.cc
//
// Random variate generation
//
// @author Werner Sandmann (ws), Kay Michael Masslow (kmm)
//
// @date 11/26/2002 "doxygenification" (kmm)
// @date 11/20/2002 some final comments (ws)
// @date 10/22/2002 implemented various discrete distributions (kmm)
//
//==========================================================================

#include <float.h>
#if defined(SPEC_CPU)
#include <cmath>
#else
#include <math.h>
#endif
#include "distrib.h"
#include "macros.h"
#include "cexception.h"

//
// Linux - <math.h> supplies a PI, MS does not...
//
#ifndef M_PI
#define M_PI    3.1415926535897932384626433832795
#endif

#ifndef M_E
#define M_E     2.7182818284590452353602874713527
#endif


//----------------------------------------------------------------------------
//
//  C O N T I N U O U S
//
//----------------------------------------------------------------------------

double uniform(double a, double b, int rng)
{
    return a + genk_dblrand(rng) * (b-a);
}

double exponential(double p, int rng)
{
    return -p * std::log(genk_dblrand(rng));
}

double normal(double m, double d, int rng)
{
#if defined(SPEC_CPU)
    double drand = genk_dblrand(rng);
    return m + d *  std::sqrt(-2.0*std::log(drand))*std::cos(PI*2.0*genk_dblrand(rng));
#else
    return m + d *  std::sqrt(-2.0*std::log(genk_dblrand(rng)))*std::cos(PI*2*genk_dblrand(rng));
#endif
}

double truncnormal(double m, double d, int rng)
{
    double res;
    do {
         res = normal(m,d,rng);
    } while(res<0);

    return res;
}

double gamma_d(double alpha, double beta, int rng)
{
    if (alpha<=0 || beta<=0)
        throw new cException("gamma(): alpha and beta params must be positive "
                             "(alpha=%lg, beta=%lg)", alpha, beta);

    if (std::fabs(alpha - 1.0) <= DBL_EPSILON)
    {
        return exponential(beta, rng);
    }
    else if (alpha < 1.0)
    {
        double b = (M_E + alpha) / M_E;

        double Y;
        for (;;)
        {
            double U1, U2, P;

            // step 1
            U1 = normal(0, 1, rng);
            P = b * U1;
            if (P > 1)
            {
                // step 3
                Y = -std::log((b - P) / alpha);
                U2 = normal(0, 1, rng);
                if (U2 <= std::pow(Y, alpha - 1.0))
                    break; // accept Y
            }
            else
            {
                // step 2
                Y = std::pow(P, (1 / alpha));
                U2 = normal(0, 1, rng);
                if (U2 <= std::exp(-Y))
                    break;  // accept Y
            }
        }
        return beta * Y;
    }
    else // if (alpha > 1.0)
    {
        double a = 1.0 / std::sqrt(2.0 * alpha - 1);
        double b = alpha - std::log(4.0);
        double q = alpha + 1.0 / a;
        double theta = 4.5;
        double d = 1 + std::log(theta);

        double Y;
        for (;;)
        {
            double U1, U2, V, Z, W;

            // step 1
            U1 = genk_dblrand(rng);
            U2 = genk_dblrand(rng);

            // step 2
            V = a * std::log(U1 / (1.0 - U1));
            Y = alpha * std::exp(V);
            Z = U1 * U1 * U2;
            W = b + q * V - Y;

            // step 3
            if (W + d - theta * Z >= .0)
                break;  // accept Y

            // step 4
            if (W >= std::log(Z))
                break;  // accept Y
        }
        return beta * Y;
    }
}


double beta(double alpha1, double alpha2, int rng)
{
    if (alpha1<=0 || alpha2<=0)
        throw new cException("beta(): alpha1 and alpha2 parameters must be positive "
                             "(alpha1=%lg, alpha2=%lg)", alpha1, alpha2);

    double Y1 = gamma_d(alpha1, 1.0, rng);
    double Y2 = gamma_d(alpha2, 1.0, rng);

    return Y1 / (Y1 + Y2);
}


double erlang_k(unsigned int k, double m, int rng)
{
    double U = 1.0;
    for (unsigned int i = 0; i < k; i++)
        U *= (1.0 - genk_dblrand(rng));

    return -(m / (double) k) * std::log(U);
}


double chi_square(unsigned int k, int rng)
{
    if (!(k % 2))
        return erlang_k(k >> 1, k, rng);
    else
        return gamma_d((double) k / 2.0, 2.0, rng);
}


double student_t(unsigned int i, int rng)
{
    double Z = normal(0, 1, rng);
    double W = std::sqrt(chi_square(i, rng) / (double) i);
    return Z / W;
}


double cauchy(double a, double b, int rng)
{
    if (b<=0)
        throw new cException("cauchy(): parameters must be b>0 (a=%lg, b=%lg)", a, b);

    return a + b * std::tan(M_PI * genk_dblrand(rng));
}


double triang(double a, double b, double c, int rng)
{
    if (b<a || c<b || a==c)
        throw new cException("triang(): parameters must be a<=b<=c, a<c (a=%lg, b=%lg, c=%lg)", a, b, c);

    double U, beta, T;

    U = genk_dblrand(rng);
    beta = (b - a) / (c - a);

    if (U < beta)
        T = std::sqrt(beta * U);
    else
        T = 1.0 - std::sqrt((1.0 - beta) * (1.0 - U));

    return a + (c - a) * T;
}


// lognormal() is inline


double weibull(double a, double b, int rng)
{
    if (a<=0 || b<=0)
        throw new cException("weibull(): a,b parameters must be positive (a=%lg, b=%lg)", a, b);

    return a * std::pow(-std::log(1.0 - genk_dblrand(rng)), 1.0 / b);
}


double pareto_shifted(double a, double b, double c, int rng)
{
    // FIXME: check arg values
    double u_pow = std::pow(1.0 - genk_dblrand(rng), 1.0 / a);
    return (b - c * u_pow) / u_pow;
}

//----------------------------------------------------------------------------
//
//  D I S C R E T E
//
//----------------------------------------------------------------------------

// helper function, needed for hypergeometric distribution
static double _factorial(int n)
{
    if (n<0)
        throw new cException("internal error: _factorial() called with n=%d",n);

    double fact = 1.0;
    while (n>1)
        fact *= n--;
    return fact;
}


int intuniform(int a, int b, int rng)
{
    return a + genk_intrand(rng) % (b-a+1);
}


// bernoulli() is inline


int binomial(int n, double p, int rng)
{
    int X = 0;
    // sum up n bernoulli trials
    for (int i = 0; i < n; i++)
    {
        double U = genk_dblrand(rng);
        if (p > U)
            X++;
    }
    return X;
}


int geometric(double p, int rng)
{
    double a = 1.0 / (std::log(1.0 - p));
    return (int)std::floor(a * std::log(genk_dblrand(rng)));
}


int negbinomial(int n, double p, int rng)
{
    int X = 0;
    for (int i = 0; i < n; i++)
    {
        X += geometric(p, rng);
    }
    return X;
}


/*
 * FIXME: hypergeometric() doesn't work yet
 *
int hypergeometric(int a, int b, int n, int rng)
{
    if (a<0 || b<0 || n>a+b)
        throw new cException("hypergeometric(): params must be a>=0, b>=0, n=<a+b "
                             "(a=%d, b=%d, n=%d)", a,b,n);

    double U = genk_dblrand(rng);
    double alpha = _factorial(b) / (double) _factorial(a + b - n);
    double beta = _factorial(b - n) / (double) _factorial(a + b);
    double A = alpha / beta;
    double B = A;
    int X = 0;

    while (U > A)
    {
        X++;
        B *= double(a - X) * double(n - X) / ((X + 1.0) * (b - n + X + 1.0));
        A += B;
    }

    return X;
}
*/

int poisson(double lambda, int rng)
{
    int X;
    if (lambda > 30.0)
    {
        double a = M_PI * std::sqrt(lambda / 3.0);
        double b = a / lambda;
        double c = 0.767 - 3.36 / lambda;
        double d = std::log(c) - std::log(b) - lambda;
        double U, V, Y;

        do
        {
            do
            {
                U = genk_dblrand(rng);
                Y = (a - std::log((1.0 - U) / U)) / b;
            }
            while (Y <= -0.5);

            X = (int)std::floor(Y + 0.5);
            V = genk_dblrand(rng);
        }
        while (a - b * Y + std::log(V / 1.0 + std::pow(std::exp(a - b * Y), 2.0)) > d + (double)X * std::log(lambda) - std::log((double)X));
    }
    else
    {
        double a = std::exp(-lambda);
        double p = 1.0;
        X = -1;

        while (p > a)
        {
            p *= genk_dblrand(rng);
            X++;
        }
    }
    return X;
}

//---------------------------------------------------------------------------
//
// Registration of functions for NED and omnetpp.ini
//
// They must be wrapped because MathFunc take and return all doubles,
// which isn't true for these functions.
//
//---------------------------------------------------------------------------

// continuous

static double _wrap_uniform(double a, double b)
{
    return uniform(a, b);
}

static double _wrap_exponential(double p)
{
    return exponential(p);
}

static double _wrap_normal(double m, double d)
{
    return normal(m, d);
}

static double _wrap_truncnormal(double m, double d)
{
    return truncnormal(m, d);
}

static double _wrap_gamma_d(double alpha, double beta)
{
    return gamma_d(alpha, beta);
}

static double _wrap_beta(double alpha1, double alpha2)
{
    return beta(alpha1, alpha2);
}

static double _wrap_erlang_k(double k, double m)
{
    return erlang_k( (unsigned int) k, m);
}

static double _wrap_chi_square(double k)
{
    return chi_square( (unsigned int) k);
}

static double _wrap_student_t(double i)
{
    return student_t( (unsigned int) i);
}

static double _wrap_cauchy(double a, double b)
{
    return cauchy(a, b);
}

static double _wrap_triang(double a, double b, double c)
{
    return triang(a, b, c);
}

static double _wrap_lognormal(double m, double d)
{
    return lognormal(m, d);
}

static double _wrap_weibull(double a, double b)
{
    return weibull(a, b);
}

static double _wrap_pareto_shifted(double a, double b, double c)
{
    return pareto_shifted(a, b, c);
}

// discrete

static double _wrap_intuniform(double a, double b)
{
    return (double) intuniform((int)a, (int)b);
}

static double _wrap_bernoulli(double p)
{
    return (double) bernoulli(p);
}

static double _wrap_binomial(double n, double p)
{
    return (double) binomial( (int)n, p);
}

static double _wrap_geometric(double p)
{
    return (double) geometric(p);
}

static double _wrap_negbinomial(double n, double p)
{
    return (double) negbinomial( (int)n, p);
}

/* hypergeometric doesn't work yet
static double _wrap_hypergeometric(double a, double b, double n)
{
    return (double) hypergeometric( (int)a, (int)b, (int)n);
}
*/

static double _wrap_poisson(double lambda)
{
    return (double) poisson(lambda);
}


Define_Function2(uniform, _wrap_uniform, 2);
Define_Function2(exponential, _wrap_exponential, 1);
Define_Function2(normal, _wrap_normal, 2);
Define_Function2(truncnormal, _wrap_truncnormal, 2);
Define_Function2(gamma_d, _wrap_gamma_d, 2);
Define_Function2(beta, _wrap_beta, 2);
Define_Function2(erlang_k, _wrap_erlang_k, 2);
Define_Function2(chi_square, _wrap_chi_square, 1);
Define_Function2(student_t, _wrap_student_t, 1);
Define_Function2(cauchy, _wrap_cauchy, 2);
Define_Function2(triang, _wrap_triang, 3);
Define_Function2(lognormal, _wrap_lognormal, 2);
Define_Function2(weibull, _wrap_weibull, 2);
Define_Function2(pareto_shifted, _wrap_pareto_shifted, 3);

Define_Function2(intuniform, _wrap_intuniform, 2);
Define_Function2(bernoulli, _wrap_bernoulli, 1);
Define_Function2(binomial, _wrap_binomial, 2);
Define_Function2(geometric, _wrap_geometric, 1);
Define_Function2(negbinomial, _wrap_negbinomial, 2);
/* hypergeometric doesn't work yet
Define_Function2(hypergeometric, _wrap_hypergeometric, 3);
*/
Define_Function2(poisson, _wrap_poisson, 1);


// continuous, rng versions

static double _wrap_uniform_with_rng(double a, double b, double rng)
{
    return uniform(a, b, (int)rng);
}

static double _wrap_exponential_with_rng(double p, double rng)
{
    return exponential(p, (int)rng);
}

static double _wrap_normal_with_rng(double m, double d, double rng)
{
    return normal(m, d, (int)rng);
}

static double _wrap_truncnormal_with_rng(double m, double d, double rng)
{
    return truncnormal(m, d, (int)rng);
}

static double _wrap_gamma_d_with_rng(double alpha, double beta, double rng)
{
    return gamma_d(alpha, beta, (int)rng);
}

static double _wrap_beta_with_rng(double alpha1, double alpha2, double rng)
{
    return beta(alpha1, alpha2, (int)rng);
}

static double _wrap_erlang_k_with_rng(double k, double m, double rng)
{
    return erlang_k( (unsigned int) k, m, (int)rng);
}

static double _wrap_chi_square_with_rng(double k, double rng)
{
    return chi_square( (unsigned int) k, (int)rng);
}

static double _wrap_student_t_with_rng(double i, double rng)
{
    return student_t( (unsigned int) i, (int)rng);
}

static double _wrap_cauchy_with_rng(double a, double b, double rng)
{
    return cauchy(a, b, (int)rng);
}

static double _wrap_triang_with_rng(double a, double b, double c, double rng)
{
    return triang(a, b, c, (int)rng);
}

static double _wrap_lognormal_with_rng(double m, double d, double rng)
{
    return lognormal(m, d, (int)rng);
}

static double _wrap_weibull_with_rng(double a, double b, double rng)
{
    return weibull(a, b, (int)rng);
}

static double _wrap_pareto_shifted_with_rng(double a, double b, double c, double rng)
{
    return pareto_shifted(a, b, c, (int)rng);
}

// discrete, rng versions

static double _wrap_intuniform_with_rng(double a, double b, double rng)
{
    return (double) intuniform((int)a, (int)b, (int)rng);
}

static double _wrap_bernoulli_with_rng(double p, double rng)
{
    return (double) bernoulli(p, (int)rng);
}

static double _wrap_binomial_with_rng(double n, double p, double rng)
{
    return (double) binomial( (int)n, p, (int)rng);
}

static double _wrap_geometric_with_rng(double p, double rng)
{
    return (double) geometric(p, (int)rng);
}

static double _wrap_negbinomial_with_rng(double n, double p, double rng)
{
    return (double) negbinomial( (int)n, p, (int)rng);
}

/* hypergeometric doesn't work yet
static double _wrap_hypergeometric_with_rng(double a, double b, double n, double rng)
{
    return (double) hypergeometric( (int)a, (int)b, (int)n, (int)rng);
}
*/

static double _wrap_poisson_with_rng(double lambda, double rng)
{
    return (double) poisson(lambda, (int)rng);
}


Define_Function2(uniform, _wrap_uniform_with_rng, 3);
Define_Function2(exponential, _wrap_exponential_with_rng, 2);
Define_Function2(normal, _wrap_normal_with_rng, 3);
Define_Function2(truncnormal, _wrap_truncnormal_with_rng, 3);
Define_Function2(gamma_d, _wrap_gamma_d_with_rng, 3);
Define_Function2(beta, _wrap_beta_with_rng, 3);
Define_Function2(erlang_k, _wrap_erlang_k_with_rng, 3);
Define_Function2(chi_square, _wrap_chi_square_with_rng, 2);
Define_Function2(student_t, _wrap_student_t_with_rng, 2);
Define_Function2(cauchy, _wrap_cauchy_with_rng, 3);
Define_Function2(triang, _wrap_triang_with_rng, 4);
Define_Function2(lognormal, _wrap_lognormal_with_rng, 3);
Define_Function2(weibull, _wrap_weibull_with_rng, 3);
Define_Function2(pareto_shifted, _wrap_pareto_shifted_with_rng, 4);

Define_Function2(intuniform, _wrap_intuniform_with_rng, 3);
Define_Function2(bernoulli, _wrap_bernoulli_with_rng, 2);
Define_Function2(binomial, _wrap_binomial_with_rng, 3);
Define_Function2(geometric, _wrap_geometric_with_rng, 2);
Define_Function2(negbinomial, _wrap_negbinomial_with_rng, 3);
/* hypergeometric doesn't work yet
Define_Function2(hypergeometric, _wrap_hypergeometric_with_rng, 4);
*/
Define_Function2(poisson, _wrap_poisson_with_rng, 2);


// compatibility genk_ versions:

double genk_uniform(double rng, double a, double b)
{
    return uniform(a, b, (int)rng);
}

double genk_intuniform(double rng, double a, double b)
{
    return intuniform((int)a, (int)b, (int)rng);
}

double genk_exponential(double rng, double p)
{
    return exponential(p, (int)rng);
}

double genk_normal(double rng, double m, double d)
{
    return normal(m, d, (int)rng);
}

double genk_truncnormal(double rng, double m, double d)
{
    return truncnormal(m, d, (int)rng);
}

// register functions for findFunction()
Define_Function(genk_uniform, 3);
Define_Function(genk_intuniform, 3);
Define_Function(genk_exponential, 2);
Define_Function(genk_normal, 3);
Define_Function(genk_truncnormal, 3);


