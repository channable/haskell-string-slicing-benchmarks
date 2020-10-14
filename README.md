# String slicing benchmarks

While profiling one of Channable's internal tools, we found the following function was the source of a significant bottleneck:

```haskell
{-# INLINE slice #-}
-- | Substring from @offset@ to @offset + len@
slice :: Int -> Int -> Text -> Text
slice offset len = T.take len . T.drop offset
```

This repository demonstrates that this function is indeed slower and takes up more memory than one would expect, and why exactly this happens.

This repository is part of a blog post that explains this problem and its solutions in more detail. This blog post will be uploaded shortly.

## Walking through the investigation

This repository is built up such that the findings about rewrite rules can be reproduced. The initial problematic implementation can be found in `lib/Lib/NaiveSlice.hs`. The solutions can be found in the other haskell files in the `lib/Lib` directory.

## Outputting rewrite rules

Every implementation has two ghc options commented out at the top of the file. Uncommenting them will cause `stack build` to output the rewrite rules that are applied while compiling that file.

## Running the executable

The executable, defined in `app/Main.hs`, provides a simple demonstration of all implementations. The input has been chosen carefully, to show that one of the implementations has different behavior than the other ones. The executable can be run with the following command:

```
stack run
```

## Running the benchmark

The benchmark can be run as follows:

```
stack build --bench --benchmark-arguments "--regress allocated:iters +RTS -T -RTS --output slice-benchmark-output.html"
```

## Benchmark results

For ease of access, an example run of this benchmark is pasted below. Please also see `slice-benchmark-output.html` for a formatted report.

<details>

<summary>Benchmark results</summary>

```
benchmarking long-0-1000/naiveSlice
time                 33.47 μs   (33.42 μs .. 33.50 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 33.46 μs   (33.44 μs .. 33.49 μs)
std dev              94.49 ns   (79.48 ns .. 129.0 ns)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              220480.000 (220479.845 .. 220480.174)
  y                  2537.838   (2277.194 .. 2849.359)

benchmarking long-0-1000/sliceWithRule
time                 832.0 ns   (831.6 ns .. 832.6 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 833.8 ns   (833.0 ns .. 835.1 ns)
std dev              3.473 ns   (2.361 ns .. 5.475 ns)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              64.000     (63.997 .. 64.004)
  y                  2537.432   (2331.796 .. 2764.745)

benchmarking long-0-1000/sequencedSlice
time                 831.9 ns   (831.4 ns .. 832.5 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 831.7 ns   (831.3 ns .. 832.1 ns)
std dev              1.347 ns   (1.077 ns .. 1.677 ns)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              64.000     (63.997 .. 64.003)
  y                  2550.996   (2322.145 .. 2770.748)

benchmarking long-0-1000/noInlineTakeSlice
time                 633.6 ns   (632.8 ns .. 634.3 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 633.7 ns   (633.3 ns .. 634.2 ns)
std dev              1.512 ns   (1.191 ns .. 2.145 ns)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              111.999    (111.997 .. 112.002)
  y                  2562.905   (2359.615 .. 2786.755)

benchmarking long-0-1000/reimplementedSlice
time                 9.604 ns   (9.599 ns .. 9.611 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 9.593 ns   (9.586 ns .. 9.600 ns)
std dev              23.16 ps   (19.10 ps .. 28.99 ps)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              32.000     (32.000 .. 32.000)
  y                  2545.529   (2367.776 .. 2724.270)

benchmarking long-5000-1000/naiveSlice
time                 261.6 μs   (261.4 μs .. 262.0 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 262.0 μs   (261.8 μs .. 262.3 μs)
std dev              898.7 ns   (685.1 ns .. 1.276 μs)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              1380479.800 (1380478.706 .. 1380481.203)
  y                  2596.150   (2230.501 .. 2976.236)

benchmarking long-5000-1000/sliceWithRule
time                 4.944 μs   (4.941 μs .. 4.947 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.944 μs   (4.941 μs .. 4.947 μs)
std dev              9.658 ns   (7.494 ns .. 12.64 ns)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              64.002     (63.979 .. 64.027)
  y                  2545.394   (2291.716 .. 2798.238)

benchmarking long-5000-1000/sequencedSlice
time                 4.961 μs   (4.958 μs .. 4.964 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.965 μs   (4.962 μs .. 4.969 μs)
std dev              11.33 ns   (8.871 ns .. 15.81 ns)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              63.998     (63.979 .. 64.023)
  y                  2548.072   (2303.543 .. 2790.820)

benchmarking long-5000-1000/noInlineTakeSlice
time                 4.757 μs   (4.751 μs .. 4.761 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.754 μs   (4.750 μs .. 4.760 μs)
std dev              14.94 ns   (11.78 ns .. 20.47 ns)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              112.000    (111.980 .. 112.024)
  y                  2530.181   (2286.261 .. 2778.268)

benchmarking long-5000-1000/reimplementedSlice
time                 9.578 ns   (9.570 ns .. 9.585 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 9.575 ns   (9.569 ns .. 9.584 ns)
std dev              26.19 ps   (17.69 ps .. 42.94 ps)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              32.000     (32.000 .. 32.000)
  y                  2547.417   (2362.943 .. 2728.119)

benchmarking long-all/naiveSlice
time                 339.1 μs   (338.7 μs .. 339.5 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 339.2 μs   (339.0 μs .. 339.7 μs)
std dev              1.058 μs   (715.5 ns .. 1.942 μs)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              2180456.037 (2180454.350 .. 2180458.030)
  y                  2549.701   (2182.160 .. 2997.128)

benchmarking long-all/sliceWithRule
time                 7.212 ns   (7.206 ns .. 7.219 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 7.221 ns   (7.215 ns .. 7.227 ns)
std dev              20.43 ps   (15.41 ps .. 27.77 ps)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              32.000     (32.000 .. 32.000)
  y                  2543.365   (2366.879 .. 2748.933)

benchmarking long-all/sequencedSlice
time                 7.387 ns   (7.380 ns .. 7.393 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 7.384 ns   (7.378 ns .. 7.389 ns)
std dev              17.29 ps   (14.02 ps .. 21.51 ps)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              32.000     (32.000 .. 32.000)
  y                  2539.507   (2366.150 .. 2720.584)

benchmarking long-all/noInlineTakeSlice
time                 11.26 ns   (11.26 ns .. 11.28 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.30 ns   (11.29 ns .. 11.31 ns)
std dev              33.88 ps   (26.06 ps .. 45.82 ps)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              80.000     (80.000 .. 80.000)
  y                  2543.438   (2368.504 .. 2730.605)

benchmarking long-all/reimplementedSlice
time                 9.959 ns   (9.950 ns .. 9.968 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 9.971 ns   (9.964 ns .. 9.982 ns)
std dev              30.94 ps   (22.28 ps .. 43.46 ps)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              32.000     (32.000 .. 32.000)
  y                  2541.649   (2377.578 .. 2732.795)
```

</details>
