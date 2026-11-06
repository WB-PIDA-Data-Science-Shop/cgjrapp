# CGJR writing style for the AI Report

How the AI Report's narrative should read, drawn from published Country
Growth and Jobs Reports (CGJRs). The rules below are given to the model as
rule 8 of the report's system prompt (`.STYLE_RULES` in `R/report_ai.R`);
keep the two in step. Citation style is a separate decision (the World Bank
Editorial Style Guide 2020 on Chicago; see `R/citations.R`): published CGJRs
cite inconsistently, so they are prose exemplars here, not citation
exemplars.

## Reports read

Read on 2026-11-04 from the World Bank Documents & Reports text files.

| Report | Parts read |
|---|---|
| Mauritania, *Beyond Extractives: Unlocking Mauritania's Potential for a Sustained, Inclusive and Resilient Growth*, CGJR 2025. <https://documents.worldbank.org/curated/en/099112425051530164> | Overview (business environment, volatility, fiscal management, regulatory environment, private sector, reform priorities); chapter 1, growth barriers (public investment, the hydrocarbon fund, governance), Box 1.1 |
| Mauritius, *Riding the Next Wave: Rewiring Institutions, Infrastructure, and Incentives*, CGJR 2026. <https://documents.worldbank.org/curated/en/099093026022015480> | Section 1.1 "Strong Institutional Foundations", Box 1.1 (strategy to implementation), section 1.2 (deficits and debt), Box 1.2, chapter 6 policy roadmap and Table 6.1 |
| Zimbabwe, *Country Growth and Jobs Report*, 2026. <https://documents.worldbank.org/curated/en/099090226030010924> | Overview: governance diagnosis and Figure 1.23, the three-pillar framework, the reform sequencing figure, Pillar 2 actions, the macroeconomic precondition |

Method: the passages above were read in full. The bold lead sentence was
confirmed from the Mauritania PDF's font data (the text files drop bold).
The counts below are from all three text files, prose lines only.

## Rules

**1. Each paragraph opens with one bold sentence that states its point;
2-4 sentences of evidence follow.** All three reports do this throughout.
Mauritania: "**Sound fiscal management can reduce volatility and
pro-cyclicality in public spending, ...** A stronger fiscal framework should
include ...". Mauritius: "Mauritius maintains strong institutional and
governance foundations that support economic growth." then the indicators.
In the app: the model writes the lead sentence as `**...**`, citations
inside it; the preview renders it bold, the Word report writes a bold run
(`.add_md_par()`), and the verifier checks the bold sentence and the next
one separately (`.sentences()`). Headings and bullets take no bold.

**2. Figures are set against peers, never left alone.** "Mauritania lags the
LMIC average and most peer countries in key indicators such as regulatory
quality, government effectiveness, control of corruption ..."; "Debt now
exceeds the upper-middle-income country (UMIC) average by around 26
percentage points and is more than 16 percentage points higher than
structural peers"; "domestic credit to the private sector below 10 percent of
GDP - the lowest among peers". In the app: the dashboard's band, rank and
comparators' median.

**3. Balanced: a strength is conceded before the weakness.** "Zimbabwe's
recent stabilization is genuine ... But this progress rests on a fragile
base."; "This institutional strength is mirrored in the trade and business
environment ... However, performance is weaker in areas related to fiscal
health and government integrity."

**4. Sentences of about 25 words.** Median sentence length is 25 words
(Mauritania), 24 (Mauritius) and 24 (Zimbabwe); a quarter run past 34-37.

**5. Abbreviations spelled out on first use.** "Weak public financial
management (PFM) systems"; "public investment management (PIM)";
"upper-middle-income country (UMIC)".

**6. "percent", lower-case "the government", no first person.** Prose writes
"percent" ("%" appears mostly in charts and tables); "the government" is
lower case in most uses (14 of 16, 14 of 19, 12 of 20); "we" and "our" are
rare (2 uses each in Mauritius and Zimbabwe). The verifier treats "50 percent"
and "50%" as the same figure.

**7. Reform effects are hedged.** "These measures could raise additional
revenue"; "Improving public investment management ... can greatly enhance";
"Sound fiscal management can reduce volatility". Effects are what a reform
could or can do, not what it will do.

**8. Recommendations begin with a verb and name the instrument.** Mauritius
Table 6.1: "Publish an SOE ownership policy ...", "Separate SOEs commercial
activities from public service obligations ..."; Zimbabwe: "Enforce 30-day
VAT refund timelines.", "Consolidate MDAs and shift institutional mandates
from revenue collection to service delivery." Reforms are grouped under the
WBG Jobs Strategy's three pillars, as the guidance note already asks.

## Seen but not adopted

- **Structural and aspirational peers.** Mauritius and Zimbabwe compare with
  two peer groups; the dashboard has one comparison group, so the report
  says "comparators".
- **Reform timeframes** ("short-term (1-3 years)", "medium-term (3-5
  years)"). The app gives a timeframe only where a guidance passage does:
  the dashboard cannot ground one.
- **Numbered figures and boxes** ("(Figure 1.26)", "Box 1.1"). The preview
  and Word report place each section's chart under it, unnumbered.
- **Prioritisation criteria and model-based impacts** (feasibility,
  leverage, MFMod simulations). These need analysis the app does not do.

## Open

- Reader-facing handling of the dashboard and guidance ids ([E..], [G..]) in
  the Word report, deferred from step 9b: kept for now, resolved by the
  appendices.
