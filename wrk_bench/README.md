Benchmark ran via `wrk2 -t4 -c1000 --timeout 2000  -d60s --latency "http://localhost:8080" -R 60000`

Benchmark run on an Intel NUC with an i7-8559U cpu running Ubuntu 20.04 (kernel 5.8.0-53-generic)

Rust program compiled via `cargo bench --release`
