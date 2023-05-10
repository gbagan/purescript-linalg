export function mapWithIndexImpl(m, r, c, f) {
    const m2 = new Array(r);
    for (let i = 0; i < r; i++) {
        const row = new Array(c);
        for (let j = 0; j < c; j++) {
            row[j] = f(i,j,m[i][j]);
        }
        m2[i] = row;
    }
    return m2;
}

export function _index(m, i, j) {
    return m.m[i][j]
}