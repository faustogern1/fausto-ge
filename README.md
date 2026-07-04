# fausto.ge

Personal site. Plain static HTML/CSS — no framework, no build step, no WordPress.
Everything lives in `index.html` (styles are inline in a `<style>` block).

## Edit
Open `index.html`, change the text, save. That's it. Preview by double-clicking the file.

## Deploy (free, via GitHub Pages)
1. Create a public GitHub repo (e.g. `fausto-ge`).
2. Add `index.html` and this README, commit, push.
3. Repo → Settings → Pages → Source: `main` branch, `/root`. Save.
4. Site goes live at `https://<username>.github.io/fausto-ge/`.

## Point fausto.ge at it
1. In Pages settings, set Custom domain to `fausto.ge` (creates a `CNAME` file).
2. At your domain registrar, add DNS records:
   - `A` records for the apex `fausto.ge` → GitHub Pages IPs:
     185.199.108.153, 185.199.109.153, 185.199.110.153, 185.199.111.153
   - (optional) `CNAME` for `www` → `<username>.github.io`
3. Wait for DNS to propagate, then tick "Enforce HTTPS".

Once this resolves, you can cancel the WordPress plan.
