// return g,n,m s.t. n*a+m*b = g = gcd(a,b)
pub fn ext_gcd(a: i64, b: i64) -> (i64, i64, i64) {
    let mut r0 = a;
    let mut r1 = b;
    let mut s0 = 1;
    let mut s1 = 0;
    let mut t0 = 0;
    let mut t1 = 1;
    while r1 != 0 {
        let q = r0 / r1;
        let r = r0 % r1;
        let s = s0 - q * s1;
        let t = t0 - q * t1;
        r0 = r1;
        s0 = s1;
        t0 = t1;
        r1 = r;
        s1 = s;
        t1 = t;
    }
    (r0, s0, t0)
}
