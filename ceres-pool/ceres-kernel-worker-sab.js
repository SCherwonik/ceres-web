import createModule from './ceres_kernels.mjs';

let Mod = null;
const ready = createModule().then(m => { Mod = m; });

function put(arr) {
  const p = Mod._malloc(Math.max(arr.byteLength, 8));
  Mod.HEAPU8.set(new Uint8Array(arr.buffer, arr.byteOffset, arr.byteLength), p);
  return p;
}
const f64 = (ptr, n) => new Float64Array(Mod.HEAPF64.buffer, ptr, n);
const i32 = (ptr, n) => new Int32Array(Mod.HEAP32.buffer, ptr, n);

self.onmessage = async (e) => {
  const msg = e.data;
  await ready;
  if (msg.type === 'init') { Atomics.add(msg.ready, 0, 1); return; }
  if (msg.type !== 'job') return;
  const { ctl, job, off, idx, out, covOff, batch, nB, M } = msg;
  const a = job.arrays, P1 = job.P + 1, ptr = {};
  try {
    for (const k of ['X', 'y', 'C', 'thr', 'okc', 'asg', 'Xp', 's', 'xb', 'sxy']) ptr[k] = put(a[k]);
    for (;;) {
      if (Atomics.load(ctl, 2)) break;
      const b = Atomics.add(ctl, 0, 1);
      if (b >= nB) break;
      const s0 = b * batch, s1 = Math.min(s0 + batch, M), m = s1 - s0;
      const base = off[s0];
      const loff = new Int32Array(m + 1);
      for (let i = 0; i <= m; i++) loff[i] = off[s0 + i] - base;
      const pOff = put(loff), pIdx = put(idx.subarray(base, off[s1]));
      Mod._ceres_run(job.kind, job.loglin, job.cap, ptr.X, job.n, ptr.y, ptr.C, job.P, ptr.thr, ptr.okc, ptr.asg,
                     pOff, pIdx, m, ptr.Xp, job.NP, ptr.s, ptr.xb, ptr.sxy, job.syy, job.yb, job.n_tr);
      Mod._free(pOff); Mod._free(pIdx);
      const o = (f) => Mod._ceres_out(f);
      if (job.kind === 0) {
        out.ok.set(i32(o(0), m), s0); out.k.set(i32(o(1), m), s0); out.nsig.set(i32(o(2), m), s0);
        out.ssr.set(f64(o(3), m), s0); out.vif.set(f64(o(4), m), s0);
        const B = f64(o(5), m * P1);
        for (let c = 0; c < P1; c++) out.b.set(B.subarray(c * m, (c + 1) * m), c * M + s0);
        out.yhat.set(f64(o(6), job.NP * m), s0 * job.NP);
      } else {
        out.ok.set(i32(o(0), m), s0); out.conv.set(i32(o(1), m), s0); out.iter.set(i32(o(2), m), s0);
        out.nsig.set(i32(o(3), m), s0); out.sig_ok.set(i32(o(4), m), s0);
        out.sigma.set(f64(o(5), m), s0); out.r2.set(f64(o(6), m), s0); out.adj_r2.set(f64(o(7), m), s0);
        out.ll.set(f64(o(8), m), s0); out.vif.set(f64(o(9), m), s0);
        const B = f64(o(10), m * P1);
        for (let c = 0; c < P1; c++) out.b.set(B.subarray(c * m, (c + 1) * m), c * M + s0);
        out.yhat.set(f64(o(11), job.NP * m), s0 * job.NP);
        out.w.set(f64(o(12), job.n * m), s0 * job.n);
        const coff = i32(o(14), m + 1), cov = f64(o(13), coff[m]);
        for (let i = 0; i < m; i++) {
          const len = coff[i + 1] - coff[i];
          out.have_cov[s0 + i] = len > 0 ? 1 : 0;
          if (len > 0) out.cov.set(cov.subarray(coff[i], coff[i + 1]), covOff[s0 + i]);
        }
      }
      Atomics.add(ctl, 1, 1);
      Atomics.notify(ctl, 1);
    }
  } catch (err) {
    Atomics.store(ctl, 2, 1);
    Atomics.notify(ctl, 1);
  } finally {
    for (const p of Object.values(ptr)) Mod._free(p);
  }
};
