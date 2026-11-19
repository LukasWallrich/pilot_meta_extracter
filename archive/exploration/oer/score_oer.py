#!/usr/bin/env python3
"""Score frozen predictions vs the human labels (development set, n=71).

Applies the u67zj archive->prereg spec flip. Reports include/text/LLM with
Wilson 95% CIs, McNemar (LLM vs text), and a paired bootstrap CI for the F1 gap.
Treats this as a DEVELOPMENT set: numbers are honest but not held-out confirmation.
"""
import csv, json, math, random
random.seed(0)
OER = "/Users/lukaswallrich/Documents/Coding/pilot_meta_extracter/exploration/oer/"
LAB = "/private/tmp/claude-501/-Users-lukaswallrich-Documents-Coding-metacheck/ec1ce03c-a76a-4646-8167-d4fe41a4fed3/scratchpad/sheet_labels.json"

d = json.load(open(LAB)); vals = d["values"]; h = vals[0]
gi, li, ti = h.index("guid"), h.index("label"), h.index("tei_file")
lab = {(r[ti], r[gi]): r[li].strip().lower() for r in vals[1:]}
lab_flip = {"u67zj": "prereg"}  # spec clarification: link-level, contains-prereg

preds = list(csv.DictReader(open(OER + "oer_predictions.csv", encoding="utf-8")))
rows = []
for p in preds:
    t = lab[(p["tei_file"], p["guid"])]
    if p["guid"] in lab_flip: t = lab_flip[p["guid"]]
    rows.append({"guid": p["guid"], "truth": t == "prereg",
                 "include": True,
                 "text": p["textgate_pred"] == "TRUE",
                 "llm": p["llm_pred"] == "TRUE"})
n = len(rows)

def wilson(k, m, z=1.96):
    if m == 0: return (float("nan"), float("nan"))
    p = k / m; d = 1 + z*z/m
    c = (p + z*z/(2*m)) / d
    hw = z*math.sqrt(p*(1-p)/m + z*z/(4*m*m)) / d
    return (max(0, c-hw), min(1, c+hw))

def confusion(pred):
    tp = sum(r[pred] and r["truth"] for r in rows)
    fp = sum(r[pred] and not r["truth"] for r in rows)
    fn = sum(not r[pred] and r["truth"] for r in rows)
    tn = sum(not r[pred] and not r["truth"] for r in rows)
    return tp, fp, fn, tn

def f1_of(tp, fp, fn):
    prec = tp/(tp+fp) if tp+fp else float("nan")
    rec = tp/(tp+fn) if tp+fn else float("nan")
    f1 = 2*prec*rec/(prec+rec) if prec and rec and (prec+rec) else float("nan")
    return prec, rec, f1

print(f"n={n}  truth: prereg={sum(r['truth'] for r in rows)} archive={sum(not r['truth'] for r in rows)}")
print("(development set; u67zj flipped archive->prereg per link-level spec)\n")
print(f"{'method':14s} {'acc [95% CI]':22s} {'prec':6s} {'rec':6s} {'F1':6s} {'spec':6s}  archives_caught")
for m in ["include", "text", "llm"]:
    tp, fp, fn, tn = confusion(m)
    prec, rec, f1 = f1_of(tp, fp, fn)
    acc = (tp+tn)/n; lo, hi = wilson(tp+tn, n)
    spec = tn/(tn+fp) if tn+fp else float("nan")
    print(f"{m:14s} {acc:.3f} [{lo:.3f},{hi:.3f}]   {prec:.3f}  {rec:.3f}  {f1:.3f}  {spec:.3f}   {tn}/{tn+fp}")

# McNemar exact (LLM vs text)
b = sum(r["llm"] == r["truth"] and r["text"] != r["truth"] for r in rows)  # llm right, text wrong
c = sum(r["text"] == r["truth"] and r["llm"] != r["truth"] for r in rows)  # text right, llm wrong
from math import comb
nn = b + c; k = min(b, c)
pmc = min(1.0, 2*sum(comb(nn, i) for i in range(k+1))/2**nn) if nn else 1.0
print(f"\nMcNemar LLM vs text: llm-right/text-wrong={b}, text-right/llm-wrong={c}, exact 2-sided p={pmc:.3f}")

# paired bootstrap CI for F1(llm) - F1(text)
def f1_for(sample, key):
    tp = sum(r[key] and r["truth"] for r in sample)
    fp = sum(r[key] and not r["truth"] for r in sample)
    fn = sum(not r[key] and r["truth"] for r in sample)
    _, _, f1 = f1_of(tp, fp, fn)
    return f1
diffs = []
for _ in range(5000):
    s = [rows[random.randrange(n)] for _ in range(n)]
    a, bb = f1_for(s, "llm"), f1_for(s, "text")
    if a == a and bb == bb: diffs.append(a - bb)
diffs.sort()
lo, hi = diffs[int(0.025*len(diffs))], diffs[int(0.975*len(diffs))]
print(f"F1(LLM) - F1(text) bootstrap: median={diffs[len(diffs)//2]:+.3f}  95% CI [{lo:+.3f}, {hi:+.3f}]")

# disagreements with truth
print("\ndisagreements (text vs llm):")
for r in rows:
    if r["text"] != r["llm"]:
        who = "text" if r["text"] == r["truth"] else "llm"
        print(f"  {r['guid']}  truth={'prereg' if r['truth'] else 'archive':7s} text={str(r['text']):5s} llm={str(r['llm']):5s} -> {who} right")
