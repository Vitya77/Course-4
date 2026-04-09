#include <stdio.h>
#include "mathstat.h"

int main() {
    double data[] = {10.5, 2.0, 33.4, 5.1, 12.8, 7.2, 1.0, 15.6};
    int n = sizeof(data) / sizeof(data[0]);

    printf("--- MathStatLib Demo ---\n");
    printf("Test data: ");
    for (int i = 0; i < n; i++) printf("%.1f ", data[i]);
    printf("\n\n");

    printf("1. Sum: %.2f\n", ms_sum(data, n));
    printf("2. Mean: %.2f\n", ms_mean(data, n));
    printf("3. Min: %.2f\n", ms_min(data, n));
    printf("4. Max: %.2f\n", ms_max(data, n));
    printf("5. Variance: %.2f\n", ms_variance(data, n));
    printf("6. Std Dev: %.2f\n", ms_std_dev(data, n));
    
    /* Before median we usually sort, but ms_median does it internally */
    printf("7. Median: %.2f\n", ms_median(data, n));
    
    printf("8. Sorted data: ");
    /* ms_median already sorted the data */
    for (int i = 0; i < n; i++) printf("%.1f ", data[i]);
    printf("\n");

    printf("\n--- End of Demo ---\n");

    return 0;
}
