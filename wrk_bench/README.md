Benchmark ran via `wrk2 -t4 -c1000 --timeout 2000  -d60s --latency "http://localhost:8080" -R 60000`

Benchmark run on an Intel NUC with an i7-8559U cpu running Ubuntu 20.04 (kernel 5.8.0-53-generic)

Rust program compiled via `cargo bench --release`. Rust version `rustc 1.52.1 (9bc8c42bb 2021-05-09)`

OCaml program is location in the example folder in this repo. It was run on OCaml version 4.12.0 with flambda enabled.
