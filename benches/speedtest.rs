use criterion::{black_box, criterion_group, criterion_main, Criterion};
use diffmatchpatch::*;

pub fn speedtest_benchmark(c: &mut Criterion) {
    let text1 = include_str!("speedtest1.txt"); // .to_chars();
    let text2 = include_str!("speedtest2.txt"); //.to_chars();

    let mut dmp = DiffMatchPatch::new();
    dmp.diff_timeout = None;

    let _ = dmp.diff_main(text2, text1, false);

    c.bench_function("speedtest", |b| {
        b.iter(|| {
            let _ = dmp.diff_main(text1, text2, false);
        })
    });
}

criterion_group!(benches, speedtest_benchmark);
criterion_main!(benches);
