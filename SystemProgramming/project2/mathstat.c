#include "mathstat.h"
#include <math.h>
#include <float.h>

/* 1. Sum of array elements */
double ms_sum(const double* data, int n) {
    if (data == (void*)0 || n <= 0) return 0.0;
    double sum = 0.0;
    for (int i = 0; i < n; i++) {
        sum += data[i];
    }
    return sum;
}

/* 2. Arithmetic mean */
double ms_mean(const double* data, int n) {
    if (data == (void*)0 || n <= 0) return 0.0;
    return ms_sum(data, n) / (double)n;
}

/* 3. Variance (sample variance) */
double ms_variance(const double* data, int n) {
    if (data == (void*)0 || n <= 1) return 0.0;
    double mean = ms_mean(data, n);
    double var_sum = 0.0;
    for (int i = 0; i < n; i++) {
        double diff = data[i] - mean;
        var_sum += diff * diff;
    }
    return var_sum / (double)(n - 1);
}

/* 4. Standard deviation */
double ms_std_dev(const double* data, int n) {
    if (data == (void*)0 || n <= 1) return 0.0;
    return sqrt(ms_variance(data, n));
}

/* 5. Minimum value */
double ms_min(const double* data, int n) {
    if (data == (void*)0 || n <= 0) return DBL_MAX;
    double min_val = data[0];
    for (int i = 1; i < n; i++) {
        if (data[i] < min_val) min_val = data[i];
    }
    return min_val;
}

/* 6. Maximum value */
double ms_max(const double* data, int n) {
    if (data == (void*)0 || n <= 0) return -DBL_MAX;
    double max_val = data[0];
    for (int i = 1; i < n; i++) {
        if (data[i] > max_val) max_val = data[i];
    }
    return max_val;
}

/* 7. Bubble Sort (in-place) */
void ms_sort(double* data, int n) {
    if (data == (void*)0 || n <= 1) return;
    for (int i = 0; i < n - 1; i++) {
        for (int j = 0; j < n - i - 1; j++) {
            if (data[j] > data[j + 1]) {
                double temp = data[j];
                data[j] = data[j + 1];
                data[j + 1] = temp;
            }
        }
    }
}

/* 8. Median (will sort data internally) */
double ms_median(double* data, int n) {
    if (data == (void*)0 || n <= 0) return 0.0;
    ms_sort(data, n);
    if (n % 2 == 0) {
        return (data[n / 2 - 1] + data[n / 2]) / 2.0;
    } else {
        return data[n / 2];
    }
}
