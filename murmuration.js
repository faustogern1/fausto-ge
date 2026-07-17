/* Light-mode background: tiny dots drifting imperceptibly in
   murmuration-like ribbons, advected by a slowly evolving flow field.
   Hidden in dark mode (the night sky takes over). */
(function () {
  var canvas = document.createElement("canvas");
  canvas.id = "murmuration";
  document.body.prepend(canvas);
  var ctx = canvas.getContext("2d");

  var W, H, dots = [];
  function resize() {
    var dpr = Math.min(window.devicePixelRatio || 1, 2);
    W = window.innerWidth; H = window.innerHeight;
    canvas.width = W * dpr; canvas.height = H * dpr;
    canvas.style.width = W + "px"; canvas.style.height = H + "px";
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  }
  resize();
  window.addEventListener("resize", resize);

  // seed dots in loose clusters, so the field stretches them into ribbons
  var N = Math.min(420, Math.round(W * H / 4200));
  var centers = [];
  for (var c = 0; c < 6; c++) centers.push([Math.random() * W, Math.random() * H]);
  function spread() { return (Math.random() + Math.random() + Math.random() - 1.5) * 220; }
  for (var i = 0; i < N; i++) {
    var ce = centers[i % centers.length];
    dots.push({
      x: ce[0] + spread(), y: ce[1] + spread(),
      r: 0.6 + Math.random() * 0.6,            // tiny: 0.6-1.2px
      o: 0.15 + Math.random() * 0.3,           // faint grey
      s: 0.6 + Math.random() * 0.8             // per-dot pace
    });
  }

  // smooth pseudo-noise wind: direction varies gently over space and time
  function angle(x, y, t) {
    return Math.sin(x * 0.0016 + t * 0.05) * 1.5
         + Math.cos(y * 0.0021 - t * 0.04) * 1.5
         + Math.sin((x * 0.0007 - y * 0.0009) + t * 0.03) * 1.2;
  }

  var SPEED = 0.25;                            // px per second: imperceptible
  var still = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  var last = 0, t = 0;

  function draw() {
    ctx.clearRect(0, 0, W, H);
    ctx.fillStyle = "#0d0f12";
    for (var i = 0; i < N; i++) {
      var d = dots[i];
      ctx.globalAlpha = d.o;
      ctx.beginPath();
      ctx.arc(d.x, d.y, d.r, 0, 6.2832);
      ctx.fill();
    }
    ctx.globalAlpha = 1;
  }

  function frame(now) {
    requestAnimationFrame(frame);
    if (now - last < 80) return;               // ~12 fps is plenty at this speed
    var dt = Math.min((now - last) / 1000, 0.25);
    last = now;
    if (document.documentElement.classList.contains("dark")) return;
    t += dt;
    for (var i = 0; i < N; i++) {
      var d = dots[i];
      var a = angle(d.x, d.y, t);
      d.x += Math.cos(a) * SPEED * d.s * dt;
      d.y += Math.sin(a) * SPEED * d.s * dt;
      if (d.x < -6) d.x += W + 12; else if (d.x > W + 6) d.x -= W + 12;
      if (d.y < -6) d.y += H + 12; else if (d.y > H + 6) d.y -= H + 12;
    }
    draw();
  }

  draw();                                      // static first paint
  if (!still) requestAnimationFrame(frame);    // reduced-motion users get a still sky
})();
