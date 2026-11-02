const { chromium } = require("playwright");

const BASE_URL = process.env.CHECKERBOARDR_URL || "http://127.0.0.1:3847/";
const CANONICAL = "https://checkerboardr.chemgrid.org/";

(async () => {
  const browser = await chromium.launch({
    headless: true,
    executablePath: process.env.PLAYWRIGHT_CHROME_PATH || undefined,
  });
  const page = await browser.newPage();
  try {
    const response = await page.goto(BASE_URL, { waitUntil: "domcontentloaded", timeout: 120000 });
    if (!response?.ok()) throw new Error(`Application returned ${response?.status()}`);
    const metadata = await page.evaluate(() => ({
      title: document.title,
      description: document.querySelector('meta[name="description"]')?.content,
      robots: document.querySelector('meta[name="robots"]')?.content,
      canonical: document.querySelector('link[rel="canonical"]')?.href,
      ogTitle: document.querySelector('meta[property="og:title"]')?.content,
      ogImage: document.querySelector('meta[property="og:image"]')?.content,
      jsonLd: [...document.querySelectorAll('script[type="application/ld+json"]')].map(node => JSON.parse(node.textContent)),
    }));
    if (!metadata.title.includes("Drug Combination Synergy")) throw new Error("Title is not descriptive");
    if (!metadata.description || metadata.description.length < 100) throw new Error("Meta description is missing or too short");
    if (metadata.canonical !== CANONICAL) throw new Error(`Unexpected canonical: ${metadata.canonical}`);
    if (!metadata.robots?.includes("index")) throw new Error("Indexing directive is missing");
    if (!metadata.ogTitle || !metadata.ogImage) throw new Error("Open Graph metadata is incomplete");
    if (!metadata.jsonLd.some(item => item["@type"] === "WebApplication" && item.softwareVersion === "2.1.4")) {
      throw new Error("WebApplication structured data is missing");
    }
    for (const path of ["robots.txt", "sitemap.xml", "checkerboard-synergy-guide.html", "checkerboardr-synergy-heatmap.png"]) {
      const asset = await page.request.get(new URL(path, BASE_URL).href);
      if (!asset.ok()) throw new Error(`${path} returned ${asset.status()}`);
    }
    console.log("PASS: canonical, metadata, structured data, robots, sitemap, guide, and social image");
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error); process.exit(1); });
