#ifndef MATHSTAT_H
#define MATHSTAT_H

#ifdef MATHSTAT_EXPORTS
#define MATHSTAT_API __declspec(dllexport)
#else
#define MATHSTAT_API __declspec(dllimport)
#endif

/* 
 * MathStatLib - Library for Statistical Data Analysis
 * Written in pure C (C99 standard or higher)
 */

/* 1. Sum of array elements */
MATHSTAT_API double ms_sum(const double* data, int n);

/* 2. Arithmetic mean */
MATHSTAT_API double ms_mean(const double* data, int n);

/* 3. Variance (sample variance) */
MATHSTAT_API double ms_variance(const double* data, int n);

/* 4. Standard deviation */
MATHSTAT_API double ms_std_dev(const double* data, int n);

/* 5. Minimum value */
MATHSTAT_API double ms_min(const double* data, int n);

/* 6. Maximum value */
MATHSTAT_API double ms_max(const double* data, int n);

/* 7. Bubble Sort (in-place) */
MATHSTAT_API void ms_sort(double* data, int n);

/* 8. Median (requires data to be sorted or will sort it internally) */
MATHSTAT_API double ms_median(double* data, int n);

#endif /* MATHSTAT_H */
