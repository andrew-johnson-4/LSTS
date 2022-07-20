use criterion::{criterion_group, criterion_main, Criterion};
use lsts::tlc::TLC;

pub fn big_file_10k(c: &mut Criterion) {
    c.bench_function("big file 10k loc", |b| b.iter(||
       TLC::new().import_file(None, "tests/stress/10k.tlc").expect_err("10k.tlc should fail with True undefined")
    ));
}

criterion_group!(benches, big_file_10k);
criterion_main!(benches);
