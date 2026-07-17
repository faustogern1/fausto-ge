/* Light-mode background: a murmuration.
   Thousands of tiny dots seeded as a flock (dense knots, a sweeping band,
   wispy arms, sparse halo). Each dot is anchored to a "home" in that shape;
   the homes are carried through a slow global warp (drift, rotation,
   breathing, undulation) while a local swirl field adds texture. The flock
   morphs perpetually but never collapses or dissolves.
   Simulation runs in a fixed 1400x850 design space, scaled to the viewport.
   Hidden in dark mode (the night sky takes over). */
(function () {
  var canvas = document.createElement("canvas");
  canvas.id = "murmuration";
  document.body.prepend(canvas);
  var ctx = canvas.getContext("2d");

  var DW = 1400, DH = 850;                 // design space
  var W, H, sx, sy;
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

  // --- helpers ---
  function gauss() { return (Math.random() + Math.random() + Math.random() - 1.5) * 2; }
  function bell()  { return Math.min(1, Math.max(0, 0.5 + (Math.random() + Math.random() - 1) * 0.5)); }
  function bez(p0, p1, p2, p3, s) {
    var u = 1 - s;
    return [u*u*u*p0[0] + 3*u*u*s*p1[0] + 3*u*s*s*p2[0] + s*s*s*p3[0],
            u*u*u*p0[1] + 3*u*u*s*p1[1] + 3*u*s*s*p2[1] + s*s*s*p3[1]];
  }

  // --- seed the flock in design space (proportions from the tuned prototype) ---
  var N = Math.max(2500, Math.min(12000, Math.round(W * H / 125)));
  var homeX = new Float32Array(N), homeY = new Float32Array(N);
  var px = new Float32Array(N), py = new Float32Array(N);
  var spd = new Float32Array(N), al = new Float32Array(N);

  function ribbonPoint(p0, p1, p2, p3, wmax) {
    var s = bell();
    var pt = bez(p0, p1, p2, p3, s);
    var w = 8 + wmax * Math.pow(Math.sin(Math.PI * s), 1.2);
    return [pt[0] + gauss() * w * 0.6, pt[1] + gauss() * w];
  }

  var i, p, mx = 0, my = 0;
  for (i = 0; i < N; i++) {
    var u = i / N;
    if      (u < 0.30) p = [980 + gauss() * 105, 300 + gauss() * 75];          // dense knot
    else if (u < 0.42) p = [870 + gauss() * 70,  380 + gauss() * 50];          // secondary knot
    else if (u < 0.76) p = ribbonPoint([180,640],[480,560],[760,470],[1010,330], 50); // band
    else if (u < 0.92) p = ribbonPoint([330,720],[640,660],[930,590],[1150,470], 34); // arm
    else               p = [Math.random() * DW, Math.random() * DH * 0.6];     // halo
    homeX[i] = p[0]; homeY[i] = p[1];
    mx += p[0]; my += p[1];
    spd[i] = 0.5 + Math.random();
    al[i]  = 0.14 + Math.random() * 0.14;    // faint: background, not foreground
  }
  mx /= N; my /= N;
  for (i = 0; i < N; i++) {                  // center homes on origin
    homeX[i] -= mx; homeY[i] -= my;
    px[i] = homeX[i] + DW * 0.5; py[i] = homeY[i] + DH * 0.45;
  }

  // local swirl texture
  function angle(x, y, t) {
    return 1.6 * Math.sin(x * 0.0035 + t * 0.045)
         + 1.6 * Math.cos(y * 0.0042 - t * 0.038)
         + 1.3 * Math.sin(x * 0.0011 - y * 0.0013 + t * 0.025);
  }

  var SPEED = 0.3, KR = 0.012;               // px/s, 1/s (design units)
  var TAU = Math.PI * 2;

  function step(t, dt) {
    // global warp of the home shape: drift, rotation, breathing, undulation
    var cx = DW * 0.5  + 170 * Math.sin(t * TAU / 1100 + 0.8);
    var cy = DH * 0.45 + 110 * Math.sin(t * TAU / 1400 + 2.1);
    var th = 0.16 * Math.sin(t * TAU / 900);
    var sc = 1 + 0.09 * Math.sin(t * TAU / 700 + 1.3);
    var ca = Math.cos(th) * sc, sa = Math.sin(th) * sc;
    var wA = t * TAU / 300, wB = t * TAU / 360;
    for (var i = 0; i < N; i++) {
      var xr = homeX[i] * ca - homeY[i] * sa;
      var yr = homeX[i] * sa + homeY[i] * ca;
      var tx = xr + 55 * Math.sin(yr * 0.004 + wA) + cx;
      var ty = yr + 40 * Math.sin(xr * 0.003 - wB) + cy;
      var a = angle(px[i], py[i], t);
      px[i] += Math.cos(a) * SPEED * spd[i] * dt + (tx - px[i]) * KR * dt;
      py[i] += Math.sin(a) * SPEED * spd[i] * dt + (ty - py[i]) * KR * dt;
    }
  }

  function draw() {
    ctx.clearRect(0, 0, W, H);
    ctx.fillStyle = "#0d0f12";
    for (var i = 0; i < N; i++) {
      ctx.globalAlpha = al[i];
      ctx.fillRect(px[i] * sx, py[i] * sy, 1, 1);
    }
    ctx.globalAlpha = 1;
  }

  var still = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  var last = 0, t = 0;
  function frame(now) {
    requestAnimationFrame(frame);
    if (now - last < 80) return;             // ~12 fps
    var dt = Math.min((now - last) / 1000, 0.25);
    last = now;
    if (document.documentElement.classList.contains("dark")) return;
    t += dt;
    step(t, dt);
    draw();
  }

  draw();
  if (!still) requestAnimationFrame(frame);
})();
