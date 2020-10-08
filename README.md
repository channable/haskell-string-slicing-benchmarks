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
stack build --bench --benchmark-arguments "--regress allocated:iters +RTS -T"
```

## Benchmark results

For ease of access, an example run of this benchmark is pasted below:

<details>

<summary>Benchmark results</summary>

```
benchmarking long-0-1000/naiveSlice
time                 33.30 μs   (33.29 μs .. 33.31 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 33.31 μs   (33.30 μs .. 33.32 μs)
std dev              24.25 ns   (17.86 ns .. 36.01 ns)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              220480.001 (220479.866 .. 220480.183)
  y                  2536.535   (2252.212 .. 2836.450)

benchmarking long-0-1000/sliceWithRule
time                 830.5 ns   (830.0 ns .. 831.1 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 830.6 ns   (830.3 ns .. 831.0 ns)
std dev              1.215 ns   (866.8 ps .. 1.600 ns)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              64.000     (63.997 .. 64.004)
  y                  2537.432   (2319.959 .. 2781.529)

benchmarking long-0-1000/sequencedSlice
time                 830.4 ns   (830.0 ns .. 830.9 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 830.2 ns   (830.1 ns .. 830.6 ns)
std dev              719.7 ps   (414.6 ps .. 1.262 ns)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              64.000     (63.996 .. 64.003)
  y                  2550.996   (2349.912 .. 2782.213)

benchmarking long-0-1000/noInlineTakeSlice
time                 630.4 ns   (630.4 ns .. 630.5 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 630.5 ns   (630.4 ns .. 630.6 ns)
std dev              247.5 ps   (190.3 ps .. 344.8 ps)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              111.999    (111.997 .. 112.002)
  y                  2562.905   (2351.828 .. 2777.875)

benchmarking long-0-1000/reimplementedSlice
time                 9.581 ns   (9.576 ns .. 9.586 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 9.581 ns   (9.576 ns .. 9.587 ns)
std dev              17.31 ps   (12.88 ps .. 26.66 ps)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              32.000     (32.000 .. 32.000)
  y                  2547.530   (2375.442 .. 2726.963)

benchmarking long-5000-1000/naiveSlice
time                 258.8 μs   (257.7 μs .. 259.7 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 260.4 μs   (260.3 μs .. 260.5 μs)
std dev              340.3 ns   (284.6 ns .. 483.1 ns)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              1380479.830 (1380478.707 .. 1380481.163)
  y                  2550.560   (2209.409 .. 2956.324)

benchmarking long-5000-1000/sliceWithRule
time                 4.941 μs   (4.937 μs .. 4.945 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.941 μs   (4.938 μs .. 4.944 μs)
std dev              9.959 ns   (8.231 ns .. 12.75 ns)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              64.000     (63.980 .. 64.025)
  y                  2556.215   (2301.293 .. 2812.799)

benchmarking long-5000-1000/sequencedSlice
time                 4.928 μs   (4.927 μs .. 4.929 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.928 μs   (4.928 μs .. 4.929 μs)
std dev              2.141 ns   (1.538 ns .. 3.274 ns)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              64.003     (63.982 .. 64.027)
  y                  2536.957   (2287.603 .. 2789.697)

benchmarking long-5000-1000/noInlineTakeSlice
time                 4.728 μs   (4.727 μs .. 4.729 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.728 μs   (4.727 μs .. 4.729 μs)
std dev              2.996 ns   (1.956 ns .. 4.959 ns)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              111.997    (111.979 .. 112.018)
  y                  2549.480   (2308.067 .. 2813.699)

benchmarking long-5000-1000/reimplementedSlice
time                 9.578 ns   (9.572 ns .. 9.584 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 9.582 ns   (9.577 ns .. 9.588 ns)
std dev              19.41 ps   (16.78 ps .. 23.28 ps)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              32.000     (32.000 .. 32.000)
  y                  2557.433   (2386.045 .. 2737.391)

benchmarking long-all/naiveSlice
time                 336.3 μs   (334.4 μs .. 338.1 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 337.2 μs   (337.0 μs .. 337.8 μs)
std dev              1.063 μs   (502.3 ns .. 2.127 μs)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              2180455.702 (2180454.223 .. 2180457.434)
  y                  2601.634   (2211.059 .. 3041.305)

benchmarking long-all/sliceWithRule
time                 7.148 ns   (7.146 ns .. 7.150 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 7.151 ns   (7.149 ns .. 7.154 ns)
std dev              8.042 ps   (6.674 ps .. 10.32 ps)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              32.000     (32.000 .. 32.000)
  y                  2539.400   (2357.504 .. 2717.880)

benchmarking long-all/sequencedSlice
time                 7.366 ns   (7.364 ns .. 7.370 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 7.364 ns   (7.362 ns .. 7.367 ns)
std dev              9.487 ps   (7.025 ps .. 13.13 ps)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              32.000     (32.000 .. 32.000)
  y                  2550.189   (2379.684 .. 2737.583)

benchmarking long-all/noInlineTakeSlice
time                 11.24 ns   (11.23 ns .. 11.24 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.25 ns   (11.25 ns .. 11.26 ns)
std dev              10.06 ps   (8.245 ps .. 14.08 ps)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              80.000     (80.000 .. 80.000)
  y                  2553.608   (2383.067 .. 2741.460)

benchmarking long-all/reimplementedSlice
time                 9.945 ns   (9.939 ns .. 9.950 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 9.959 ns   (9.956 ns .. 9.962 ns)
std dev              10.28 ps   (8.421 ps .. 13.34 ps)
allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
  iters              32.000     (32.000 .. 32.000)
  y                  2555.236   (2389.603 .. 2737.914)
```

</details>
