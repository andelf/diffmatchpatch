use criterion::{criterion_group, criterion_main, Criterion};

pub fn speedtest_this(c: &mut Criterion) {
    use diffmatchpatch::*;

    let text1 = include_str!("speedtest1.txt").to_chars();
    let text2 = include_str!("speedtest2.txt").to_chars();

    let dmp = DiffMatchPatch::new();
    // dmp.diff_timeout = Some(Duration::from_millis(100));

    let _ = dmp.diff_main(&text2, &text1, false);

    c.bench_function("speedtest_this", |b| {
        b.iter(|| {
            let n = dmp.diff_main(&text1, &text2, false);
            assert_eq!(n.len(), 2187);
        })
    });
}

/*
pub fn speedtest_dissimilar(c: &mut Criterion) {
    let text1 = include_str!("speedtest1.txt");
    let text2 = include_str!("speedtest2.txt");

    let _ = dissimilar::diff(text1, text2);

    c.bench_function("speedtest_dissimilar", |b| {
        b.iter(|| {
            let n = dissimilar::diff(text1, text2);
            //println!("=> {:#?}", &n[..]);
            assert_eq!(n.len(), 2187);
        })
    });
}

pub fn speedtest_dmp(c: &mut Criterion) {
    let text1 = include_str!("speedtest1.txt");
    let text2 = include_str!("speedtest2.txt");

    let mut dmp = dmp::Dmp::new();
    dmp.diff_timeout = None;

    let _ = dmp.diff_main(&text2, &text1, false);

    c.bench_function("speedtest_dmp", |b| {
        b.iter(|| {
            let n = dmp.diff_main(&text1, &text2, false);
            assert_eq!(n.len(), 2187);
        })
    });
}

pub fn speedtest_diff_match_patch(c: &mut Criterion) {
    let text1 = include_str!("speedtest1.txt");
    let text2 = include_str!("speedtest2.txt");

    let mut dmp = diff_match_patch::Dmp::new();

    let _ = dmp.diff_main(&text2, &text1, false);

    c.bench_function("speedtest_diff_match_patch", |b| {
        b.iter(|| {
            let n = dmp.diff_main(&text1, &text2, false);
           // println!("=> {:#?}", &n);
            assert_eq!(n.len(), 2187);
        })
    });
}
*/
criterion_group!(
    benches,
    speedtest_this,
    // speedtest_dissimilar, // has sematic Semantic Cleanup
    // speedtest_dmp,
    // speedtest_diff_match_patch
);
criterion_main!(benches);
