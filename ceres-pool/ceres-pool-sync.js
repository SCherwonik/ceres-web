(function () {
  if (self.CeresSyncPool) return;
  const S = self.CeresSyncPool = { workers: [], ready_ctl: null, n: 0 };
  S.init = function (baseUrl, n) {
    if (S.n) return S.n;
    if (typeof SharedArrayBuffer === 'undefined' || !self.crossOriginIsolated) return 0;
    S.ready_ctl = new Int32Array(new SharedArrayBuffer(8));
    for (let i = 0; i < n; i++) {
      const w = new Worker(baseUrl + 'ceres-kernel-worker-sab.js', { type: 'module' });
      w.postMessage({ type: 'init', ready: S.ready_ctl });
      S.workers.push(w);
    }
    S.n = n;
    return n;
  };
  S.ready = () => (S.ready_ctl ? Atomics.load(S.ready_ctl, 0) : 0);

  const FS = () => self.Module.FS;
  const rd = (path, T) => { const u = FS().readFile(path); return new T(u.buffer, u.byteOffset, u.byteLength / T.BYTES_PER_ELEMENT); };
  const shared = (T, len) => new T(new SharedArrayBuffer(Math.max(len, 1) * T.BYTES_PER_ELEMENT));

  S.run = function (specJson) {
    const sp = JSON.parse(specJson);
    const nw = S.ready();
    if (!nw) return 'not ready';
    const d = sp.dir, P1 = sp.P + 1;
    const job = {
      kind: sp.kind, loglin: sp.loglin, cap: sp.cap, n: sp.n, P: sp.P, NP: sp.NP, n_tr: sp.n_tr, syy: sp.syy, yb: sp.yb,
      arrays: {
        X: rd(d + '/X.f64', Float64Array), y: rd(d + '/y.f64', Float64Array), C: rd(d + '/C.f64', Float64Array),
        thr: rd(d + '/thr.f64', Float64Array), Xp: rd(d + '/Xp.f64', Float64Array), s: rd(d + '/s.f64', Float64Array),
        xb: rd(d + '/xb.f64', Float64Array), sxy: rd(d + '/sxy.f64', Float64Array),
        okc: rd(d + '/okc.i32', Int32Array), asg: rd(d + '/asg.i32', Int32Array)
      }
    };
    const off = rd(d + '/off.i32', Int32Array), idx = rd(d + '/idx.i32', Int32Array);
    const M = off.length - 1;

    const covOff = new Int32Array(M + 1);
    for (let m = 0; m < M; m++) { const k1 = off[m + 1] - off[m] + 1; covOff[m + 1] = covOff[m] + (sp.kind === 0 ? 0 : k1 * k1); }
    const out = sp.kind === 0
      ? { ok: shared(Int32Array, M), k: shared(Int32Array, M), nsig: shared(Int32Array, M), ssr: shared(Float64Array, M),
          vif: shared(Float64Array, M), b: shared(Float64Array, M * P1), yhat: shared(Float64Array, sp.NP * M) }
      : { ok: shared(Int32Array, M), conv: shared(Int32Array, M), iter: shared(Int32Array, M), nsig: shared(Int32Array, M),
          sig_ok: shared(Int32Array, M), sigma: shared(Float64Array, M), r2: shared(Float64Array, M), adj_r2: shared(Float64Array, M),
          ll: shared(Float64Array, M), vif: shared(Float64Array, M), b: shared(Float64Array, M * P1),
          yhat: shared(Float64Array, sp.NP * M), w: shared(Float64Array, sp.n * M),
          cov: shared(Float64Array, covOff[M]), have_cov: shared(Int32Array, M) };

    const ctl = new Int32Array(new SharedArrayBuffer(16));
    const batch = sp.batch, nB = Math.ceil(M / batch);
    for (const w of S.workers) w.postMessage({ type: 'job', ctl, job, off, idx, out, covOff, batch, nB, M });

    const limit = (sp.timeout_s || 120) * 1000;
    let last = -1, since = Date.now();
    for (;;) {
      const done = Atomics.load(ctl, 1);
      if (Atomics.load(ctl, 2)) return 'error';
      if (done >= nB) break;
      if (done !== last) { last = done; since = Date.now(); }
      else if (Date.now() - since > limit) { Atomics.store(ctl, 2, 1); return 'timeout'; }
      Atomics.wait(ctl, 1, done, 1000);
    }
    for (const [k, v] of Object.entries(out)) {
      const copy = new Uint8Array(v.byteLength);
      copy.set(new Uint8Array(v.buffer, v.byteOffset, v.byteLength));
      FS().writeFile(d + '/out_' + k + '.bin', copy);
    }
    return 'ok ' + M + ' ' + nw;
  };
})();
