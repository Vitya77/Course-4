import ctypes
import os

dll_path = os.path.abspath("mathstat.dll")

try:
    lib = ctypes.CDLL(dll_path)

    lib.ms_sum.argtypes = [ctypes.POINTER(ctypes.c_double), ctypes.c_int]
    lib.ms_sum.restype = ctypes.c_double

    lib.ms_mean.argtypes = [ctypes.POINTER(ctypes.c_double), ctypes.c_int]
    lib.ms_mean.restype = ctypes.c_double

    lib.ms_min.argtypes = [ctypes.POINTER(ctypes.c_double), ctypes.c_int]
    lib.ms_min.restype = ctypes.c_double

    lib.ms_max.argtypes = [ctypes.POINTER(ctypes.c_double), ctypes.c_int]
    lib.ms_max.restype = ctypes.c_double

    lib.ms_variance.argtypes = [ctypes.POINTER(ctypes.c_double), ctypes.c_int]
    lib.ms_variance.restype = ctypes.c_double

    lib.ms_std_dev.argtypes = [ctypes.POINTER(ctypes.c_double), ctypes.c_int]
    lib.ms_std_dev.restype = ctypes.c_double

    lib.ms_median.argtypes = [ctypes.POINTER(ctypes.c_double), ctypes.c_int]
    lib.ms_median.restype = ctypes.c_double

    data_list = [10.5, 2.0, 33.4, 5.1, 12.8, 7.2, 1.0, 15.6]
    n = len(data_list)

    data_array = (ctypes.c_double * n)(*data_list)

    print("--- MathStatLib Python Demo ---")
    print(f"Test data: {data_list}")
    print("-" * 31)

    print(f"1. Sum:       {lib.ms_sum(data_array, n):.2f}")
    print(f"2. Mean:      {lib.ms_mean(data_array, n):.2f}")
    print(f"3. Min:       {lib.ms_min(data_array, n):.2f}")
    print(f"4. Max:       {lib.ms_max(data_array, n):.2f}")
    print(f"5. Variance:  {lib.ms_variance(data_array, n):.2f}")
    print(f"6. Std Dev:   {lib.ms_std_dev(data_array, n):.2f}")
    print(f"7. Median:    {lib.ms_median(data_array, n):.2f}")

    sorted_data = [data_array[i] for i in range(n)]
    print(f"8. Sorted:    {sorted_data}")
    print("-" * 31)
    print("Demo completed successfully.")

except Exception as e:
    print(f"Error: {e}")
    print("Make sure mathstat.dll is in the same folder as this script.")
