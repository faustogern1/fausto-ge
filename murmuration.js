/* Light-mode background: a murmuration, in 3D.
   Each flock is a thin, curved sheet of tiny dots tumbling slowly in space.
   When a sheet passes edge-on to the viewer its thousands of dots collapse
   onto a narrow band — the stark shapes of real murmurations; face-on it
   opens into an airy cloud. An agitation wave ripples through each sheet.
   Three layers: a lead flock, a twin lagging 40s on the same orbit, and a
   wide-orbit echo lagging 80s that converges with them at one extreme of
   the swing. Simulation runs in a 1400x850 design space scaled to the
   viewport. Hidden in dark mode (the night sky takes over). */
(function () {
  var canvas = document.createElement("canvas");
  canvas.id = "murmuration";
  document.body.prepend(canvas);
  var ctx = canvas.getContext("2d");

  var DW = 1400, DH = 850, W, H, sx, sy;
  function resize() {
    var dpr = Math.min(window.devicePixelRatio || 1, 2);
    W = window.innerWidth; H = window.innerHeight;
    canvas.width = W * dpr; canvas.height = H * dpr;
    canvas.style.width = W + "px"; canvas.style.height = H + "px";
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    sx = W / DW; sy = H / DH;
  }
  resize();
  window.addEventListener("resize", resize);

  function gauss() { return (Math.random() + Math.random() + Math.random() - 1.5) * 2; }

  /* ---- seed: a wavy elliptical sheet with a dense knot, thin in z ---- */
  var N = Math.max(2500, Math.min(9000, Math.round(W * H / 130)));
  var hx = new Float32Array(N), hy = new Float32Array(N), hz = new Float32Array(N);
  var uu = new Float32Array(N), al = new Float32Array(N), ph = new Float32Array(N);

  var i, u, v;
  for (i = 0; i < N; i++) {
    if (i / N < 0.30) {                       // dense knot on the sheet
      u = 0.45 + gauss() * 0.13; v = -0.10 + gauss() * 0.17;
    } else if (i / N < 0.94) {                // body of the sheet
      do { u = (Math.random() * 2 - 1); v = (Math.random() * 2 - 1); }
      while (u * u + v * v > 1);
      u = u * (0.55 + 0.45 * Math.abs(u));    // thin the middle, feather the rim
    } else {                                  // stragglers
      u = (Math.random() * 2 - 1) * 1.25; v = (Math.random() * 2 - 1) * 1.25;
    }
    hx[i] = u * 430;
    hy[i] = v * 250 * (1 - 0.25 * u * u);
    hz[i] = 85 * Math.sin(u * 2.2) * Math.cos(v * 1.7) + gauss() * 16;  // curvature + thickness
    uu[i] = u;
    al[i] = 0.30 + Math.random() * 0.30;
    ph[i] = Math.random() * 6.283;
  }

  var TAU = Math.PI * 2, F = 1600;            // perspective distance

  /* one flock layer: tumble + orbit + agitation wave, then project */
  function drawFlock(t, bx, ax, by, ay, scMul, inkMul) {
    var cx = bx + ax * Math.sin(t * TAU / 550 + 0.8);
    var cy = by + ay * Math.sin(t * TAU / 700 + 2.1);
    var thY = 1.5708 * Math.sin(t * TAU / 430 + 0.4);     // sweeps through edge-on
    var thX = 1.35   * Math.sin(t * TAU / 610 + 1.0);
    var cb = Math.cos(thY), sb = Math.sin(thY);
    var ca = Math.cos(thX), sa = Math.sin(thX);
    var wv = t * TAU / 90, wv2 = t * TAU / 110;
    ctx.fillStyle = "#0d0f12";
    for (var i = 0; i < N; i++) {
      var z = hz[i] + 42 * Math.sin(uu[i] * 5 - wv) + 8 * Math.sin(t * TAU / 47 + ph[i]);
      var y = hy[i] + 26 * Math.sin(uu[i] * 4 + wv2);
      var x1 = hx[i] * cb + z * sb;
      var z1 = -hx[i] * sb + z * cb;
      var y2 = y * ca - z1 * sa;
      var z2 = y * sa + z1 * ca;
      var pr = F / (F + z2) * scMul;
      ctx.globalAlpha = Math.min(0.85, al[i] * inkMul * pr);
      ctx.fillRect((cx + x1 * pr) * sx, (cy + y2 * pr) * sy, 1, 1);
    }
    ctx.globalAlpha = 1;
  }

  function drawAll(t) {
    ctx.clearRect(0, 0, W, H);
    // wide-orbit echo, 80s behind; its orbit meets the others at the +1 extreme
    drawFlock(t - 80, DW * 0.5 - 210, 380, DH * 0.45 - 150, 260, 1.15, 0.7);
    // twin on the lead's orbit, 40s behind
    drawFlock(t - 40, DW * 0.5, 170, DH * 0.45, 110, 1, 0.8);
    // lead
    drawFlock(t, DW * 0.5, 170, DH * 0.45, 110, 1, 1);
  }

  var still = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  var last = 0, t = 0;
  function frame(now) {
    requestAnimationFrame(frame);
    if (now - last < 80) return;              // ~12 fps
    var dt = Math.min((now - last) / 1000, 0.25);
    last = now;
    if (document.documentElement.classList.contains("dark")) return;
    t += dt;
    drawAll(t);
  }

  drawAll(0);
  if (!still) requestAnimationFrame(frame);
})();
