---
title: Tessera v1.1 released, with smoother and temporally stable embeddings
description: TESSERA v1.1 is a drop-in retrained model that fixes the tiling artefacts
  of v1.0, with embeddings now being served from AWS S3 and model weights up on Hugging
  Face.
url: https://anil.recoil.org/notes/tessera-v11-out
date: 2026-06-12T00:00:00-00:00
preview_image: https://anil.recoil.org/images/videos/297de7c9-9cea-4051-8b27-041fffa90e72.webp
authors:
- Anil Madhavapeddy
source:
ignore:
---

<p><a href="https://www.cst.cam.ac.uk/people/zf281">Frank Feng</a> and I <a href="https://geotessera.org/blog/2026-06-09-tessera-v1-1">announced TESSERA v1.1</a>
on behalf of <a href="https://geotessera.org/about#:~:text=for%2520Science%2520%C2%B7%2520Isambard-,People,-Lead%2520Faculty">the team</a> earlier this week, and I wanted to follow up here with a more
visual explanation of what changed as I got quite a few questions about it!</p>
<p>v1.1 is a retrained successor to the <a href="https://anil.recoil.org/papers/2025-tessera">original v1.0 model</a> that
<a href="https://www.cst.cam.ac.uk/people/zf281">Frank Feng</a> and the team have been hammering on for months. Crucially, since we
pre-generate embedding 'map tiles', the new release is a drop-in replacement if
you just swap tiles; the basic format of 128 dimensions is unchanged.  Accuracy
of your tasks should improve in all cases (a trend which will continue as we
train better models with more data and training FLOPS).</p>
<h2><a href="https://anil.recoil.org/news.xml#fewer-artefacts-in-low-observation-areas" class="anchor" aria-hidden="true"></a>Fewer artefacts in low observation areas</h2>
<p>Tessera v1.0 could sometimes produce noisier tiles in regions with few clear
satellite observations (e.g. due to persistent cloud or satellite sensor gaps).
This exhibited as boundary-like seams in the tiles where the inferred
embeddings didn't quite align; e.g. along Sentinel-1 ascending/descending
coverage edges where one side of the line might have ~50 valid observations and
the other ~150.</p>
<p>Tessera v1.1 now handles both sparse and imbalanced observation patterns
gracefully! If your region of interest was small and didn't straddle a
problematic tile you'll see no difference, but large-scale analyses should get
cleaner.</p>
<p>The easiest way to see all this is to look at the embeddings themselves in the
<a href="https://tze.geotessera.org">TZE explorer</a>. In this video I flip between the
v1.0 and v1.1 embeddings over the same regions, visualised in false colour:</p>
<p></p><div class="video-center"><iframe title="Tessera 1.0 to 1.1 embeddings" width="100%" height="315px" src="https://crank.recoil.org/videos/embed/297de7c9-9cea-4051-8b27-041fffa90e72" frameborder="0" allowfullscreen="" sandbox="allow-same-origin allow-scripts allow-popups allow-forms"></iframe></div><p></p>
<p>What you're looking at in the v1.0 layer are the grid-like seams running
through an otherwise homogeneous landscape (Ireland doesn't really have those
jagged lines, you can confirm by visiting my lovely home).</p>
<p>What's happening is that the number of valid observations jumps across the
line, and the old v1.0 model showed that difference up into the embeddings. The
speckly patches are areas where persistent cloud left the model with too few
clean observations to produce a stable representation.</p>
<p>We then switch to the v1.1 layer, and the seams are gone and the noisy patches
resolve into a smooth structure that follows the actual land cover. It's <em>very</em>
satisfying to click around the 10m² pixels and watch embeddings that used to
flicker between years settle down into stable trajectories in <a href="https://tze.geotessera.org">the explorer</a>!</p>
<h2><a href="https://anil.recoil.org/news.xml#temporal-stability" class="anchor" aria-hidden="true"></a>Temporal stability</h2>
<p>If you're doing analysis over a long period of time, then the 128-dimensional
embeddings are now much more consistent year-on-year for the same location.
This is a big deal for tasks like change detection, trend analysis, and even
just convenience since training a classifier on one year and applying it to
another is now much more accurate.</p>
<p><img src="https://anil.recoil.org/images/tessera11-temporal-drift.webp" alt="%c" title="Differences in the same region across years with Tessera v1.0 and v1.1 (credit: Jovana Knezevic)"></p>
<p>This feature won't affect most users,
but we're pretty pleased with how well change detection now works.</p>
<h2><a href="https://anil.recoil.org/news.xml#expanded-coastal-coverage-worldwide" class="anchor" aria-hidden="true"></a>Expanded coastal coverage worldwide</h2>
<p>The v1.0 land mask we used to mask out ocean areas was too aggressive, and
dropped legitimate land pixels along coastlines or on small islands. We've
listened to our <a href="https://www.plantsci.cam.ac.uk/staff/dr-thomas-worthington">mangrove-loving friends</a> and extended the inference
buffer to 20km, which brings coastlines and remote islands properly into
coverage.</p>
<p><a href="https://tze.geotessera.org/?store=v1.1"> <img src="https://anil.recoil.org/images/tze-explorer-v1.1-ss-1.webp" alt="%c" title="The green false colour is the expanded coastal tiles, which now captures all of the UK including islands"> </a></p>
<h2><a href="https://anil.recoil.org/news.xml#our-coverage-maps-now-include-v10-and-v11" class="anchor" aria-hidden="true"></a>Our coverage maps now include v1.0 and v1.1</h2>
<p>I updated the <a href="https://ucam-eo.github.io/tessera-coverage-map/">live coverage map</a> to now
track both generations side-by-side, so you can see exactly which tiles exist
for v1.0 and v1.1 in any given year:</p>
<p></p><div class="video-center"><iframe title="Tessera v1 and v1.1 coverage map" width="100%" height="315px" src="https://crank.recoil.org/videos/embed/97d422a2-af9c-47b5-947a-c136ad7093b6" frameborder="0" allowfullscreen="" sandbox="allow-same-origin allow-scripts allow-popups allow-forms"></iframe></div><p></p>
<p>This is all updated via a <a href="https://github.com/ucam-eo/tessera-coverage-map/blob/main/.github/workflows/map.yml">GitHub Action on ucam-eo/tessera-coverage-map</a>
that also updates an index Parquet file of all available manifests.</p>
<h3><a href="https://anil.recoil.org/news.xml#getting-the-v11-embeddings" class="anchor" aria-hidden="true"></a>Getting the v1.1 embeddings</h3>
<p>To get the new embeddings, grab the <a href="https://github.com/ucam-eo/geotessera/releases/tag/v0.9.0">geotessera 0.9.0+ release</a> of the
<a href="https://anil.recoil.org/notes/geotessera-python">Python library</a> which went out alongside v1.1. It has a new
<code>--dataset-version</code> flag to pick v1.0 or v1.1, and a <code>--dataset-variant</code> flag
now that multiple parties are generating embeddings for the community:</p>
<ul>
<li><code>vultr</code> is the original <a href="https://geotessera.org/blog/2026-03-30-training-and-inference-at-scale">v1.0 global run</a></li>
<li><code>cambridge</code> is our <a href="https://www.tunbury.org/2026/05/20/processing-uk-azure-spot/">OxCaml-generated</a> v1.1 run for early adopters</li>
<li>We're working on a Zarr-native full global v1.1 with <a href="https://www.cyclops.ai/">Cyclops.ai</a>, covering 2017-2025 that will become the default once it lands.</li>
</ul>
<p>Use <a href="https://docs.astral.sh/uv/">uvx</a> to try this without any installation:</p>
<pre><code class="language-bash">uvx geotessera download \
  --country "United Kingdom" \
  --year 2024 \
  --dataset-version v1.1 \
  --dataset-variant cambridge \
  --format npy \
  --output ./uk-v1.1
</code></pre>
<p>All the embeddings (both versions) are also now in the <code>s3://tessera-embeddings</code>
public bucket on AWS Open Data, which geotessera 0.9 switches to by default.
Spare a kind thought for "okavango", our single overworked Cambridge server that served every
TESSERA embedding for the first six months without falling over (much)!
But seriously, at some point, we're going to have to turn off `okavanago' as it's
taking up a significant amount of the egress bandwidth for Cambridge, so I encourage
users to upgrade to geotessera 0.9 as soon as possible just to change the source
of your embeddings download. Let me know if you have any problems!</p>
<h3><a href="https://anil.recoil.org/news.xml#also-on-hugging-face-now" class="anchor" aria-hidden="true"></a>Also on Hugging Face now</h3>
<p>We're also now on <a href="https://huggingface.co/geotessera/TESSERA-V-1.1">Hugging Face</a>
with the full v1.1 (and <a href="https://huggingface.co/geotessera/TESSERA-V-1.0">v1.0</a>)
model weights, with checkpoints for both the Microsoft Planetary Computer and
AWS Open Data preprocessing backends. If you'd rather run inference yourself
or fine-tune on your own data, everything you need is there, all under CC0 as
usual. Do <a href="https://eeg.zulipchat.com">let us know</a> if you fine-tune a model as
we'd love to see how it goes.</p>
<p>If there's a region of the world you need for your own research urgently,
please do <a href="https://github.com/ucam-eo/geotessera/issues">request an ROI</a> on the
geotessera issue tracker and we'll prioritise it in the generation queue.
Otherwise, sit tight as we'll have full global 2017-2025 coverage within a few
months!</p>
<p>See also coverage from the <a href="https://www.meteorologicaltechnologyinternational.com/news/satellites/cambridge-ai-tool-converts-satellite-archives-into-accessible-earth-intelligence.html">Meteorological Technology trade magazine</a> about the release.</p><h1>References</h1><ul><li>Feng et al (2026). TESSERA: Temporal Embeddings of Surface Spectra for Earth Representation and Analysis. <a href="https://doi.org/10.48550/arXiv.2506.20380" target="_blank"><i>10.48550/arXiv.2506.20380</i></a></li>
<li>Madhavapeddy (2025). GeoTessera Python library released for geospatial embeddings. <a href="https://doi.org/10.59350/7hy6m-1rq76" target="_blank"><i>10.59350/7hy6m-1rq76</i></a></li></ul>
